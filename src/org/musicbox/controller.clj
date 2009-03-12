
(ns org.musicbox.controller
  (:gen-class
   :init init)
  (:import [javax.sound.midi MidiSystem Sequencer Sequence Synthesizer]
           [java.awt.event ActionListener]
           [javax.swing.event ChangeListener]
           [java.util Random]
           [javax.swing JPanel JFrame JLabel JButton JSlider JProgressBar UIManager JRootPane]
	   [org.musicbox GUI]
           [org.jfugue MidiRenderer MusicStringParser Pattern Player Rhythm])
  (:use [clojure.contrib.str-utils :only (str-join)]
        [org.musicbox.composer]
        [org.musicbox.midi]))

(def run-flag (atom false))
(def progress (atom 0))
(def tempo (atom 1))

(def panel nil)

(def player-agent (agent nil))

(defn play-song 
  [_]
  (do
    (reset! run-flag true)
    (while @run-flag
      (play (let [main-grammar (generate-grammar 2)]
              (struct grammar
                      [1 1 2 1]
                      [1]
                      []
                      []
                      false
                      [main-grammar]))
            tempo
            progress
            run-flag))
    (reset! progress 0)))

(defn init []
  [])

(defn -main [& args] 
  (do
    (. UIManager (setLookAndFeel (. UIManager getSystemLookAndFeelClassName)))
    ; Create the application frame & utils
    (def panel (GUI.))
    (def frame 
	 (doto (JFrame.)
	   (.setTitle "Clojure Music Box")
	   (.setDefaultCloseOperation (. JFrame EXIT_ON_CLOSE))
	   (.add panel)
	   (.setResizable false)
	   .pack 
	   .show))
    (def progress-agent (send-off (agent nil) 
                              (fn [_] (while true 
                                (do (.. panel getProgressBar (setValue @progress))
                                    (. Thread (sleep 500)))))))
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
                                          (do (reset! tempo (+ 0.25 (* 1.5 (/ (.. event getSource getValue) 100)))))))))))
