
(ns org.musicbox.djtools
  (:gen-class)
  (:import [javax.sound.midi MidiSystem Sequencer Sequence Synthesizer]
           [java.io File BufferedReader InputStreamReader]
           [java.awt.event ActionListener]
           [javax.swing.event ChangeListener]
           [java.util Random]
           [javax.swing JPanel JFrame JLabel JButton JSlider JProgressBar UIManager JRootPane]
           [org.musicbox GUI]
           [org.jfugue MidiRenderer MusicStringParser Pattern Player Rhythm])
  (:use [org.musicbox.composer]
        [org.musicbox.midi]
        [clojure.contrib.str-utils :only (str-join)]))
 
(defn bassline-string
  [voices]
  (str \
       (str "v0 I[Slap_Bass_1] " (apply str (take (quot (voice-length (longest voices)) 4) (repeat "A1qqqqa120 "))))
       (str-join \ (for [label-voice (partition 2 (interleave voices (iterate inc 1)))]
                     (str \v (second label-voice)
                          \ (voice-string (first label-voice) (second label-voice)))))))

(def synth (doto (. MidiSystem getSynthesizer) (.open)))
(def seqr (doto (. MidiSystem (getSequencer false)) 
            (.open) 
            (.setLoopCount (. Sequencer LOOP_CONTINUOUSLY))
            (.setLoopEndPoint -1)
            (.setLoopStartPoint 0)))

(defn play-loop
  [midi tempo-atom progress-atom run-flag]
      (do 
          (. seqr (setLoopCount (. Sequencer LOOP_CONTINUOUSLY)))
          (.setSequence seqr midi)
          (.start seqr)
          (while @run-flag 
            (. Thread (sleep 20))
            (reset! progress-atom (quot (* 100 (. seqr getTickPosition))
                                   (. seqr getTickLength)))
            (. seqr (setTempoFactor @tempo-atom)))
          (. seqr (setLoopCount 0))
          (while (.isRunning seqr) (. Thread (sleep 20)))
          (.println System/out "Finished")))

(defn resize-voices [voices]
  (for [voice voices] (resize-voice voice 32)))

(def master-sequencer-run-flag (atom true))

(def master-sequencer-tempo-atom (atom 2))
(def current-track-progress-atom (atom 0))
(def master-sequencer-agent (agent nil))

(defn new-phrase [] (-> (first (generate-pipe 0 (vector (struct grammar
                                                                       (random-seq [4] [1 1 1 1 1 2 2 2 2 3 4])
                                                                       []
                                                                       (random-seq [4] ["A0" "C0" "E0"])
                                                                       []
                                                                       false
                                                                       (generate-piano)))))
                               compose
                               resize-voices 
                               bassline-string
                               build-midi))

(def deck-queue-atom (atom (new-phrase)))

(defn switch [] (do
                  (reset! deck-queue-atom (new-phrase))
                  (reset! master-sequencer-run-flag false)))

(defn master-sequencer-start 
  []
  (send-off master-sequencer-agent 
            (fn [_]
              (do (.setReceiver (.getTransmitter seqr) (.getReceiver synth))
                  (while true
                    (do (play-loop  @deck-queue-atom
                                    master-sequencer-tempo-atom 
                                    current-track-progress-atom 
                                    master-sequencer-run-flag)
                        (reset! master-sequencer-run-flag true)))))))
  




