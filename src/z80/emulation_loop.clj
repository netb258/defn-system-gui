(ns z80.emulation-loop
  (:require [z80.memory :as memory]
            [z80.display :as display]
            [quil.core :as q]))

;; --------------------------------------------------------------------------------------------------
;; --------------------------------------- Z80 Instruction Loop -------------------------------------
;; --------------------------------------------------------------------------------------------------

;; This will serve as our frame canvas. Every frame will be drawn here.
;; We will initialize this in the quil 'setup' function like so:
;; (reset! global-frame-buffer (q/create-image 256 224 :rgb))
(def ^:private global-frame-buffer (atom nil))

(defn- vblank-irq-enabled?
  "Checks the VDP's internal Register 1 state to see if the Sega Master System 
  hardware has enabled V-Blank Frame Interrupt requests."
  [^z80.vdp.VdpState vdp]
  (let [vdp-regs ^ints (:regs vdp)
        reg1 (aget vdp-regs 1)]
    ;; Bit 5 of VDP Register 1 enables the Frame Interrupt (V-Blank IRQ)
    ;; Remember the bits are counted from right to left starting at bit 0.
    (not= 0 (bit-and reg1 2r00100000))))

(defn- hblank-irq-enabled?
  "Checks Bit 4 of VDP Register 0 to see if Line Interrupts (H-Blank IRQs) are enabled.
  The bits are counted from right to left starting at bit 0."
  [^z80.vdp.VdpState vdp]
  (let [vdp-regs ^ints (:regs vdp)
        reg0 (aget vdp-regs 0)]
    (not= 0 (bit-and reg0 2r00010000))))

(defn- get-vdp-reg10
  "Retrieves the value of VDP Register 10 (Scanline target)."
  [^z80.vdp.VdpState vdp]
  (let [vdp-regs ^ints (:regs vdp)]
    (aget vdp-regs 10)))

;; NOTE: The method executeOneInstruction has a line like this: instruction = memory.readByte(reg_PC).
;; Since the program counter starts out at 0x0000, we should be starting out by reading a single byte form the ROM.

;; Notice, that we are implementing a PAL loop (313 scanlines instead of 262). This approach proved the most compatible.
;; Simply put, some games will NOT run without a PAL loop (for example: Back to the Future 3).
;; However, the opposite is not true. All non PAL games will run just fine with a PAL loop.

;; NOTE: This loop could be more accurate. However, it does run my entire library of 180+ games, so I'll leave it as is.
(defn- do-instruction-loop!
  "Executes a single PAL frame scanline-by-scanline (313 lines total).
  Runs the CPU instructions and draws the graphics."
  [^com.codingrodent.microprocessor.Z80.Z80Core cpu vdp-atom overclock-factor remove-8-sprite-limit?]
  (let [;; PAL Sega Master System metrics: 313 total scanlines per frame (0 to 312).
        total-lines-per-frame 313
        ;; Even though the system processes 313 scanlines, only 192 of them are used to draw the game.
        ;; Technically the SMS supports drawing on up to 240 lines, but this emulator only supports 192 mode.
        ;; Keep in mind, the VAST MAJORITY of games run in 192 mode (99% of games).
        visible-lines 192
        ;; Internally the Master System divides the TV screen into a matrix of 32 columns and 28 rows.
        ;; This means 28 rows × 8 tile pixels = 224 pixels. This is true even if we only support 192 mode.
        ;; The rest of the scanlines outside 192 are filled with the overscan color.
        visible-lines-plus-overscan 224
        ;; Every scanline lasts exactly 228 CPU T-states (cycles). 
        ;; Total frame footprint = 313 lines * 228 cycles = 71,364 cycles per frame (~50Hz).
        cycles-per-line (* 228 overclock-factor)
        ;; The VDP's register 10 holds a counter that is crucial the the timing of the H-BLANK.
        ;; The Master System triggers an H-BLANK interrupt only if this counter rolls over below zero.
        line-interrupt-counter (atom (get-vdp-reg10 @vdp-atom))
        ;; Extract the raw PImage canvas object out of the atom container once per frame
        frame-canvas ^processing.core.PImage @global-frame-buffer]

    ;; Open the direct 1D primitive pixel array for unchecked mutations
    (.loadPixels frame-canvas)

    (dotimes [scanline total-lines-per-frame]
      (let [start-tstates (.getTStates cpu)
            target-tstates (+ start-tstates cycles-per-line)]

        ;; Keep VDP state synchronized with the current horizontal scan-line.
        (swap! vdp-atom assoc :current-scan-line scanline)

        ;; Contrary to intuition, the interrupts need to be fired before executing the CPU instructions.
        ;; Notice that we are setting the interrupt line to true but never to false.
        ;; The games themselves must clear the interrupt line when appropriate.
        ;; The only way games can clear the interrupt line is by doing a VDP status port read.
        (let [vdp-state @vdp-atom
              vblank-asserted? (and (:vblank-active? vdp-state) (vblank-irq-enabled? vdp-state))
              hblank-asserted? (and (:hblank-active? vdp-state) (hblank-irq-enabled? vdp-state))]
          (when (or vblank-asserted? hblank-asserted?)
            (.setInterrupt cpu true)))

        ;; NOTE: If I place Step 1 and Step 2 at the very bottom of the loop, then "Chicago Syndicate" runs fine.

        ;; 1. PROCESS Z80 CPU INSTRUCTIONS FOR THIS SCANLINE
        ;; Step the Z80 processor repeatedly until it consumes exactly 228 cycle T-states.
        (loop []
          (when (< (.getTStates cpu) target-tstates)
            (.executeOneInstruction cpu)
            (recur)))

        ;; 2. RUN LINE RENDERING FUNCTIONS
        ;; Only render within the standard 224-line limit.
        (when (< scanline visible-lines-plus-overscan)
          ;; ALWAYS draw the background line. This ensures that when the system is in 192-line mode,
          ;; lines 192 to 223 automatically drop into the overscan loop to draw a clean uniform border.
          (display/draw-background-line! @vdp-atom frame-canvas scanline)
          ;; ONLY compute foreground sprites and run collision checks during active video display lines
          (when (< scanline visible-lines)
            (display/draw-sprites-line! frame-canvas vdp-atom scanline remove-8-sprite-limit?)))

        ;; 3. HANDLE H-BLANK
        ;; The VDP line counter decrements on every scanline up to 192.
        ;; NOTE: Adapted the VDP counter logic from Zenga: https://github.com/friol/zenga/blob/main/js/smsVdp.js
        ;; NOTE: If I set this to 255, then "California Games 2" runs fine.
        (if (<= scanline visible-lines)
          (let [current-count @line-interrupt-counter
                new-count (dec current-count)]
            ;; When the counter underflows below 0, reset it from VDP Register 10 and request a CPU interrupt.
            (if (< new-count 0)
              (do
                ;; Counter underflowed! Reload from VDP Register 10
                (reset! line-interrupt-counter (get-vdp-reg10 @vdp-atom))
                ;; Trigger H-BLANK Interrupt.
                ;; We have just processed a whole single scan-line with the loop above.
                ;; So, we can trigger an interrupt to let the game know there is a short time
                ;; before we snap back and process another scan-line.
                (swap! vdp-atom assoc :hblank-active? true))
              ;; Decrement counter normally
              (reset! line-interrupt-counter new-count)))
          ;; Outside the 192 window, the counter continually reloads from Register 10.
          (reset! line-interrupt-counter (get-vdp-reg10 @vdp-atom)))

        ;; 4. HANDLE V-BLANK
        ;; Trigger a VBlank on the last visible scanline, so that games have time to update
        ;; before we go back to scanline 0 and start processing a new frame. This will be a significant pause.
        ;; This is the longest, most crucial synchronization event.
        ;; During V-BLANK the VRAM is fully accessible without disrupting the display.
        ;; Games use this window to: Update sprite positions (moving characters, enemies, projectiles),
        ;; Load new tile graphics into VDP memory and more.
        ;; NOTE, that triggering v-blank on scanline 192 is actually accurate for an NTSC loop not a PAL loop (242).
        ;; We are leaving it at 192, because we also support the Game Gear and 192 seems to work for both consoles.
        (when (= scanline visible-lines)
          ;; NOTE: Reading the VDP status port is the only thing that clears :report-vblank-active?
          (swap! vdp-atom assoc :vblank-active? true :report-vblank-active? true))

        ;; 5. END OF FRAME
        ;; This is the very last scanline of the PAL cycle loop.
        (when (= scanline 312)
          (swap! vdp-atom assoc :vblank-active? false))))
    ;; This is where the finalized frame is rendered.
    (.updatePixels frame-canvas)))

;; --------------------------------------------------------------------------------------------------
;; ------------------------------------------- Quil Setup -------------------------------------------
;; --------------------------------------------------------------------------------------------------

;; This function will prepare everything needed for the instruction loop to run.
(defn make-setup-function [fps]
  (fn []
    ;; Technically we are emulating a PAL Master System and the FPS should be 50.
    ;; However, all Game Gear consoles run at 60 FPS no matter the region.
    ;; Setting the FPS to 60 covers both consoles.
    (q/frame-rate fps)
    ;; NOTE - :rgb creates a 32-bit integer per pixel (0x00RRGGBB) image.
    ;; The top 8 bits are ignored and all pixels are treated as 100% opaque.
    (reset! global-frame-buffer (q/create-image 256 224 :rgb))))

(defn set-nearest-neighbor!
  "This function forces Nearest Neighbor sampling on all images.
  That should disable any antialiasing / filtering or texture smoothing.
  We are doing this, because Quil automatically applies Bilinear filtering on all upscaed images."
  []
  (.textureSampling (q/current-graphics) 2))

(defn set-bilinear!
  "This function forces high-quality Bilinear/Trilinear sampling on all images.
  We pass 5 (Trilinear) for the highest possible rendering quality across 
  upscaling and downscaling, or 4 for strict Bilinear filtering."
  []
  (.textureSampling (q/current-graphics) 5))

(defn- show-fps!
  "Displays the current FPS in the top left corner of the screen."
  []
  (let [raw-fps (q/current-frame-rate)
        fps-text (format "FPS: %.1f" raw-fps)]
    ;; Configure the font settings
    (q/text-size 16)
    ;; Draw a subtle black drop shadow behind the text for readability
    (q/fill 0 0 0)
    (q/text fps-text 11 21) ; Offset by 1 pixel down and right
    ;; Draw the actual foreground text in bright green (or white)
    (q/fill 255 255 255)
    (q/text fps-text 10 20) ; X=10, Y=20 coordinates from the top-left corner
    ;; We're drawing the FPS counter twice, because we're trying to achieve a 'bold' text effect.
    (q/text fps-text 11 20)))

;; This function will call the instruction loop 50-60 times a second.
;; The screen-dimensions should be a vector like this: [256 192]
(defn make-draw-function [cpu-atom vdp-atom screen-dimensions config-map]
  (fn []
    (let [overclock-factor        (:overclock-factor config-map)
          sms-bilinear-filtering? (:sms-bilinear-filtering? config-map)
          gg-bilinear-filtering?  (:gg-bilinear-filtering? config-map)
          sms-shader              (:sms-shader config-map)
          gg-shader               (:gg-shader config-map)
          show-sms-border?        (:show-sms-border? config-map)
          remove-8-sprite-limit?  (:remove-8-sprite-limit? config-map)
          show-fps?               (:show-fps? config-map)
          screen-width  (first screen-dimensions)
          screen-height (second screen-dimensions)
          ;; Extract the centered Game Gear 160x144 window (X: 48 to 207, Y: 24 to 167)
          ;; We will draw this limited area only if a Game Gear rom is selected.
          gg-viewport  (.get ^processing.core.PImage @global-frame-buffer 48 24 160 144)
          sms-viewport (.get ^processing.core.PImage @global-frame-buffer 0 0 256 192)]
      ;; 1. Execute Z80 code line-by-line while filling 'global-frame-buffer'
      (do-instruction-loop! @cpu-atom vdp-atom overclock-factor remove-8-sprite-limit?)
      ;; 2. Set image filtering (if any).
      (if (or
            (and @memory/gg-rom-selected? gg-bilinear-filtering?)
            (and (not @memory/gg-rom-selected?) sms-bilinear-filtering?))
        (set-bilinear!)
        (set-nearest-neighbor!))
      ;; 3. If we are dealing with a SMS rom, we should paint the frame directly from the frame-buffer.
      ;; If we are dealing with a GG rom, we should crop the screen to 160x144 before doing the drawing.
      (if @memory/gg-rom-selected?
        (do
          (q/image gg-viewport 0 0 screen-width screen-height)
          (when gg-shader (q/shader (q/load-shader gg-shader))))
        (do
          (if show-sms-border?
            (q/image @global-frame-buffer 0 0 screen-width screen-height)
            (q/image sms-viewport 0 0 screen-width screen-height))
          (when sms-shader (q/shader (q/load-shader sms-shader)))))
      (when show-fps? (show-fps!)))))
