;; The core module provides a nice GUI that allows the user to pick a ROM and start up the emulator.
(ns z80.core
  (:require [z80.emulator :as emu]
            [z80.memory :as memory])
  (:use [seesaw.core :only [dialog frame label menu menubar menu-item action alert show! native!]]
        [clojure.java.shell :only [sh]])
  (:use seesaw.chooser)
  (:gen-class))

(native!)

(def emulator-logo "SMS_BIOS_JP.gif")
(def config-map (memory/read-ds-from-file "./config.edn"))
(def sms-roms-dir (:sms-roms-dir config-map))
(def gg-roms-dir (:gg-roms-dir config-map))

(def controls-help 
  "  ----- Player 1 controls -----
  Z -> Button 1.
  X -> Button 2.
  Q -> Save State (for the current game).
  E  -> Load State (for the current game).
  Use the arrow keys for movement and ENTER to pause.
  ----- Player 2 controls -----
  C -> Button 1.
  V -> Button 2.
  J  -> Left.
  K -> Down.
  L -> Right.
  I  -> Up.")

(defn run-rom [rom-path]
  (emu/start-emulator rom-path))

(defn run-rom-in-new-process [rom-path]
  (sh "cmd.exe" "/c" "start" "DeFn-System Status Window" "defn-system.exe" rom-path))

(def choose-sms-rom-action
  (action :name "Load Master System ROM" :key "menu O"
          :handler 
          (fn [e]
            (if-let [f (choose-file :dir sms-roms-dir)]
              (run-rom-in-new-process (str f))))))

(def choose-gg-rom-action
  (action :name "Load Game Gear ROM" :key "menu I"
          :handler 
          (fn [e]
            (if-let [f (choose-file :dir gg-roms-dir)]
              (run-rom-in-new-process (str f))))))

(defn make-menu-bar []
  (menubar :items
           [(menu :text "File"
                  :items [choose-sms-rom-action choose-gg-rom-action (menu-item :text "Exit" :listen [:action (fn [event] (System/exit 0))])])
            (menu :text "Help"
                  :items [(menu-item :text "Controls" :listen [:action (fn [event] (alert controls-help))])])
            (menu :text "Config"
                  :items [(menu-item :text "Edit Config"
                                     :listen [:action
                                              (fn [event] (sh "cmd" "/C" "D:\\vim\\vim8.2\\gvim.exe" (str "\"" "config.edn" "\"")))])])]))

(defn make-logo-label
  [logo-img-path]
  (label :icon (clojure.java.io/file logo-img-path)))

(defn make-main-window [child-widgets]
  (let [app-icon (clojure.java.io/file "./icon/sega-icon.png")]
    (frame :title "DEFN-System launcher"
           :width 775
           :height 600
           :icon app-icon
           :resizable? false
           :content child-widgets
           :menubar (make-menu-bar)
           :on-close :exit)))

(defn -main
  [& args]
  ;; NOTE: If the user provides a ROM path, then just start the emulator with that.
  ;; Otherwise show the GUI.
  (if args
    (run-rom (first args))
    (let [main-frame (make-main-window (make-logo-label emulator-logo))]
      (-> main-frame (.setLocationRelativeTo nil)) ;; Center the main window.
      (show! main-frame)))
  (println "Done!")) ;; Signal that we have loaded the program.
