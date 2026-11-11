;; NOTE: This version of the tile viewer loops through all 448 tiles in VRAM and displays them.
;; This raw approach does not make any distinction between background tiles and sprites.
;; That being the case, the user might need to tweak the color palette.

(ns z80.vram-viewer 
  (:require [z80.vdp :as vdp]
            [z80.color :as color]
            [z80.joypads :as joypads]
            [z80.memory :as memory]
            [z80.display :as display]
            [quil.core :as q]))

(defn- setup []
  (q/frame-rate 30))

(def ^:private sprite-colors-first? (atom true))

(defn- handle-press []
  (let [user-input (joypads/get-key)]
    (if (= user-input \space)
      (swap! sprite-colors-first? not))))

(defn- sprites-first [^long vram-tile-index]
  (if (< vram-tile-index 256) 16 0))

(defn- sprites-last [^long vram-tile-index]
  (if (< vram-tile-index 256) 0 16))

(defn- show-color-palette-text!
  "Show the user which color palette is active."
  [scale]
  (let [text-str (str "Palette (press space): " (if @sprite-colors-first? "SPRITES -> BACKGROUND" "BACKGROUND -> SPRITES"))]
    (q/fill 0 0 0 220)
    (q/no-stroke)
    (q/rect 0 (* 160 scale) (* 133 scale) 35)
    (q/fill 0 255 0)
    (q/text-size 16)
    (q/text-align :left :center)
    (q/text text-str 15 (* 125 scale))))

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
    (let [^ints color-palette (color/get-vdp-color-palette (:cram @vdp-atom))
          ^bytes vram (:vram @vdp-atom)
          tile-size 8
          scaled-tile-size (* tile-size scale) ; 32 pixels
          tiles-per-row 32
          total-tiles 448
          total-rows (int (Math/ceil (/ total-tiles tiles-per-row)))
          grid-width (* tiles-per-row scaled-tile-size)
          grid-height (* total-rows scaled-tile-size)]
      (q/background 0)

      ;; Loop through all tiles in VRAM:
      (dotimes [tile-idx total-tiles]
        (let [col (mod tile-idx tiles-per-row)
              row (quot tile-idx tiles-per-row)
              palette-offset (if @sprite-colors-first? (sprites-first tile-idx) (sprites-last tile-idx))
              global-x (int (* col tile-size))
              global-y (int (* row tile-size))]

          ;; Draw Single Tile:
          (dotimes [fine-y tile-size]
            (let [fy (int fine-y)]
              (dotimes [fine-x tile-size]
                (let [fx (int fine-x)
                      pixel-color-idx (int (color/get-sms-pixel-color-idx vram tile-idx fy fx))
                      final-color-idx (int (+ pixel-color-idx palette-offset))
                      pixel-color (aget color-palette final-color-idx)
                      draw-x (* (+ fx global-x) scale)
                      draw-y (* (+ fy global-y) scale)]
                  (q/fill pixel-color)
                  (q/no-stroke)
                  (q/rect draw-x draw-y scale scale)))))))

      ;; Draw grid overlay:
      (q/stroke 255 0 128) ;; This color should make the lines very visible.
      (q/stroke-weight 1)
      (draw-vertical-lines!   tiles-per-row scaled-tile-size grid-height)
      (draw-horizontal-lines! total-rows scaled-tile-size grid-width)
      ;; Show the user which color palette is active:
      (show-color-palette-text! scale))))

(defn show-vram-viewer!
  "Shows a graphic window that displays all tiles currently in VRAM."
  [vdp-atom scale]
  (q/defsketch vram-viewer
      :title "DeFn System VRAM Tile Viewer"
      :setup setup
      :draw (make-draw-function vdp-atom scale)
      :key-pressed handle-press
      :renderer :opengl
      ;; :features [:exit-on-close]
      :size [(* 256 scale) (* 128 scale)]))
