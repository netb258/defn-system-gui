;; NOTE: This version of the tile viewer loops through all VRAM tile in the Naming Table
;; and also all VRAM tiles in the SAT. The background tiles of the Naming table are 
;; displayed first, followed by the SAT's sprite tiles.
;; In essence the tile-viewer is showing all currently active tiles on screen.
;; So basically, we have a Tile Map and Sprite Attribute Table viewer.

(ns z80.tilemap-viewer
  (:require [z80.vdp :as vdp]
            [z80.color :as color]
            [z80.joypads :as joypads]
            [z80.memory :as memory]
            [z80.display :as display]
            [quil.core :as q]))

(defn- setup []
  (q/frame-rate 30))

(defn- key-comparator
  "Returns a function that compares keys in a map."
  [key]
  #(compare (key %1) (key %2)))

(defn- get-all-background-indexes
  "Traverses the VDP Name Table in VRAM and returns a seq of all 9-bit background pattern tile indexes.
   The tiles are returned as a seq of maps like this '({:vram-idx vram-tile-index :palette-offset 0 or 16} ... )"
  [^z80.vdp.VdpState vdp]
  (let [vram-bytes  ^bytes (:vram vdp)
        cram-ints   ^ints (:cram vdp)
        vdp-regs    ^ints (:regs vdp)
        color-palette-cache ^ints (color/get-vdp-color-palette cram-ints)
        cfg ^BackgroundData (display/parse-background-data vdp-regs color-palette-cache)
        naming-table-start (int (:naming-table-start cfg))
        max-rows           (int (:max-rows cfg))
        ;; The SMS tile map is always 32 columns wide
        total-tiles        (* max-rows 32)]
    (loop [idx 0
           acc (transient [])]
      (if (< idx total-tiles)
        (let [nte-offset (int (+ naming-table-start (* idx 2)))
              low-byte   (int (memory/signed->unsigned (aget vram-bytes nte-offset)))
              high-byte  (int (memory/signed->unsigned (aget vram-bytes (unchecked-inc nte-offset))))
              ;; NOTE: The background tiles are allowed to use both palette 0 and 1.
              use-palette-1?  (not= 0 (bit-and high-byte 2r00001000)) ;; Bit 3: Palette select (0 = Palette 0, 1 = Palette 1)
              palette-offset  (if use-palette-1? 16 0)                ;; System background colors reside in palette entries 16-31
              ;; Replicate the 9-bit calculation from the display.clj pixel loop
              vram-tile-index (int (bit-or low-byte (bit-shift-left (bit-and high-byte 2r00000001) 8)))]
          (recur (inc idx) (conj! acc {:vram-idx vram-tile-index :palette-offset palette-offset})))
        ;; (into (sorted-set-by (key-comparator :vram-idx)) (persistent! acc))
        (persistent! acc)))))

(defn- get-all-sprite-indexes
  "Traverses the Sprite Attribute Table (SAT) in VRAM and returns a seq of all tile indexes for active sprites.
   The tiles are returned as a seq of maps like this '({:vram-idx vram-tile-index :palette-offset 16} ... )"
  [^z80.vdp.VdpState vdp]
  (let [vram-bytes     ^bytes (:vram vdp)
        cram-ints      ^ints  (:cram vdp)
        color-palette-cache ^ints (color/get-vdp-color-palette cram-ints)
        ctx ^z80.display.SpriteData (display/parse-sprite-data vdp color-palette-cache nil)
        sprite-tile-base (int (.-sprite-tile-base ctx))
        sat-base-addr    (int (.-sat-base-addr ctx))
        sat-info-table   (int (.-sat-info-table ctx))]
    (loop [sprite-id 0
           acc (transient [])]
      (if (< sprite-id 64)
        (let [y-addr (int (+ sat-base-addr sprite-id))
              sat-y  (int (memory/signed->unsigned (aget vram-bytes y-addr)))
              attr-idx       (int (* sprite-id 2))
              tile-addr      (int (inc (+ sat-info-table attr-idx)))
              sat-tile-index (int (memory/signed->unsigned (aget vram-bytes tile-addr)))]
          (recur (inc sprite-id) (conj! acc {:vram-idx (+ sprite-tile-base sat-tile-index) :palette-offset 16})))
        ;; (into (sorted-set-by (key-comparator :vram-idx)) (persistent! acc))
        (persistent! acc)))))

(defn- draw-vertical-lines! [^long tiles-per-row ^long scaled-tile-size ^long grid-height]
  (dotimes [col (inc tiles-per-row)]
    (let [x (* col scaled-tile-size)]
      (q/line x 0 x grid-height))))

(defn- draw-horizontal-lines! [^long total-rows ^long scaled-tile-size ^long grid-width]
  (dotimes [row (inc total-rows)]
        (let [y (* row scaled-tile-size)]
          (q/line 0 y grid-width y))))

(defn- make-draw-function [vdp-atom scale]
  (fn []
    (let [vdp-state           @vdp-atom
          ^ints color-palette (color/get-vdp-color-palette (:cram vdp-state))
          ^bytes vram         (:vram vdp-state)
          ^ints vdp-regs      (:regs vdp-state)
          
          ;; Fetch current active mapping vectors from Name Table & SAT
          bg-indexes     (get-all-background-indexes vdp-state)
          sprite-indexes (get-all-sprite-indexes vdp-state)
          
          ;; Layout Constants
          tiles-per-row  32
          gap-rows       1  ;; Number of empty rows to insert between blocks
          
          ;; Calculate remaining spots in the last row of the background block 
          ;; so that the gap starts exactly on a fresh row.
          bg-remainder   (int (mod (count bg-indexes) tiles-per-row))
          pad-to-next-row (int (if (zero? bg-remainder) 0 (- tiles-per-row bg-remainder)))
          total-gap-tiles (int (+ pad-to-next-row (* gap-rows tiles-per-row)))
          
          ;; Combine them with an explicit gap injection
          gapped-tiles (concat sprite-indexes (repeat total-gap-tiles nil) bg-indexes)
          
          tile-size 8
          scaled-tile-size (int (* tile-size scale)) ;; 32 pixels
          total-tiles (int (count gapped-tiles))
          total-rows (int (Math/ceil (/ total-tiles tiles-per-row)))
          grid-width (int (* tiles-per-row scaled-tile-size))
          grid-height (int (* total-rows scaled-tile-size))]
      
      (q/background 0)

      ;; Loop through all dynamically discovered active tiles
      (dotimes [grid-idx total-tiles]
        (when-let [tile-meta (nth gapped-tiles grid-idx)] ;; Safely skips nil gap tiles
          (let [vram-tile-idx  (int (:vram-idx tile-meta))
                palette-offset (int (:palette-offset tile-meta))
                col (mod grid-idx tiles-per-row)
                row (quot grid-idx tiles-per-row)
                global-x (int (* col tile-size))
                global-y (int (* row tile-size))]
            
            ;; Draw Single Tile:
            (dotimes [fine-y tile-size]
              (let [fy (int fine-y)]
                (dotimes [fine-x tile-size]
                  (let [fx (int fine-x)
                        pixel-color-idx (int (color/get-sms-pixel-color-idx vram vram-tile-idx fy fx))
                        final-color-idx (+ pixel-color-idx palette-offset)
                        pixel-color     (aget color-palette final-color-idx)
                        draw-x (* (+ fx global-x) scale)
                        draw-y (* (+ fy global-y) scale)]
                    (q/fill pixel-color)
                    (q/no-stroke)
                    (q/rect draw-x draw-y scale scale))))))))

      ;; Draw Grid Overlay:
      (q/stroke 255 0 128) ;; This color should make the lines very visible.
      (q/stroke-weight 1)
      (draw-vertical-lines!   tiles-per-row scaled-tile-size grid-height)
      (draw-horizontal-lines! total-rows scaled-tile-size grid-width))))

(defn show-tilemap-viewer! [vdp-atom scale]
  (q/defsketch tilemap-viewer
      :title "DeFn System - Naming Table and SAT Viewer"
      :setup setup
      :draw (make-draw-function vdp-atom scale)
      :renderer :opengl
      ;; :features [:exit-on-close]
      :size [(* 256 scale) (* 250 scale)]))
