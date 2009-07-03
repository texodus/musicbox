
(ns org.musicbox.controller
  (:gen-class
   :init init)
  (:import [java.awt.event ActionListener]
           [javax.swing.event ChangeListener]
           [java.util Random]
           [javax.swing JPanel JFrame JLabel JButton JSlider JProgressBar UIManager JRootPane]
	   [org.musicbox GUI])
  (:require [org.musicbox.midi :as midi]
            [org.musicbox.composer :as composer])
  (:use [clojure.contrib.str-utils :only (str-join)]))

; Atoms for handling player state
(def run-flag (atom false))
(def progress (atom 0))
(def tempo (atom 1))

; Agents for handling UI functions
(def player-agent (agent nil))
(def progress-agent (agent nil))

(def frame nil)

(defn play-song 
  [_]
  (do
    (reset! run-flag true)
    (while @run-flag
      (-> 0
          (composer/gen-pipe 
           (vector (struct composer/grammar
                           (composer/random-seq [4] [1 1 1 1 1 2 2 2 2 3 4])
                           []
                           (composer/random-seq [4] ["A0" "D0" "E0"])
                           []
                           false
                           (composer/gen-piano))))
          first
          (midi/play tempo progress run-flag)))
    (reset! progress 0)))

(defn -main [& args] 
  (do
    (. UIManager (setLookAndFeel (. UIManager getSystemLookAndFeelClassName)))
    ; Create the application frame & utils
    (let [panel (GUI.)
          frame (doto (JFrame.)
                  (.setTitle "MusicBox")
                  (.setDefaultCloseOperation (. JFrame EXIT_ON_CLOSE))
                  (.add panel)
                  (.setResizable false)
                  .pack 
                  .show)]
      (send-off progress-agent
                (fn [_] (while true 
                          (do (.. panel getProgressBar (setValue @progress))
                              (. Thread (sleep 500))))))
    ;  (midi/set-soundbank "8RealGS20.SF2")
      ; Add ActionListeners for the various controls
      (. (. panel getPlayButton) 
         (addActionListener (proxy [ActionListener] []
                              (actionPerformed [event]
                                               (do (reset! run-flag false)
                                                   (send-off player-agent play-song))))))
      (. (. panel getStopButton)
         (addActionListener (proxy [ActionListener] []
                              (actionPerformed [event]
                                               (do (reset! run-flag false))))))       
      (. (. panel getSlider)
         (addChangeListener (proxy [ChangeListener] []
                              (stateChanged [event]
                                            (do (reset! tempo (+ 0.25 (* 1.5 (/ (.. event getSource getValue) 100))))))))))))
