
(ns org.musicbox.djtools
  (:gen-class
   :init init)
  (:import [javax.sound.midi Sequencer Sequence Synthesizer MidiMessage]
           [java.io File BufferedReader InputStreamReader]
           [java.awt.event ActionListener]
           [javax.swing.event ChangeListener]
           [java.util Random]
           [javax.swing JPanel JFrame JLabel JButton JSlider JProgressBar UIManager JRootPane]
           [org.musicbox GUI]
           [org.jfugue MidiRenderer MusicStringParser Pattern Player Rhythm])
  (:use [org.musicbox.composer]
        [org.musicbox.instruments]
        [org.musicbox.midi]
        [clojure.contrib.str-utils :only (str-join)]))
 
(defn bassline-string
  [voices]
  (str \
       (str "v9 " (apply str (take (quot (voice-length (longest voices)) 4) (repeat "[Bass_Drum]qqqqa120 "))))
       (str-join \ (for [label-voice (partition 2 (interleave voices (iterate inc 1)))]
                     (str \v (second label-voice)
                          \ (voice-string (first label-voice) (second label-voice)))))))

(def synth)
(def seqr)

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

(def master-sequencer-run-flag (ref true))

(def master-sequencer-tempo-atom (atom 2))
(def current-track-progress-atom (atom 0))
(def master-sequencer-agent (agent nil))

(defn new-phrase [] (-> ;(first (gen-drums))
                     (first (gen-pipe 0 (vector (struct grammar
                                                             (random-seq [4] [1 1 1 1 1 2 2 2 2 3 4])
                                                             []
                                                             (random-seq [4] ["A0" "C0" "E0"])
                                                             []
                                                             false
                                                             (gen-piano)))))
                        compose
                        resize-voices 
                        bassline-string
                        build-midi))

(def deck-queue-ref (ref nil))

(defn switch 
  ([] 
     (dosync
       (ref-set deck-queue-ref (new-phrase))
       (ref-set master-sequencer-run-flag false)))
  ([& voices]
     (dosync
      (ref-set deck-queue-ref (-> voices
                              build-string
                              build-midi))
      (ref-set master-sequencer-run-flag false))))
     

(defn master-sequencer-start 
  []
  (send-off master-sequencer-agent 
            (fn [_]
              (do (.setReceiver (.getTransmitter seqr) (.getReceiver synth)) ; todo fix
                  (while true
                    (play-loop  @deck-queue-ref
                                master-sequencer-tempo-atom 
                                current-track-progress-atom 
                                master-sequencer-run-flag)
                    (dosync (ref-set master-sequencer-run-flag true)))))))
  
(defmulti init-midi identity)

(defmethod init-midi 
  :java []
  (var-set seqr
	   (doto (. javax.sound.midi.MidiSystem getSynthesizer) 
	     (.open)))
  (var-set seqr  (doto (. javax.sound.midi.MidiSystem (getSequencer false)) 
		   (.open) 
		   (.setLoopCount (. Sequencer LOOP_CONTINUOUSLY))
		   (.setLoopEndPoint -1)
		   (.setLoopStartPoint 0))))

(defmethod init-midi
  :tux-guitar []
  (var-set synth 
	   (doto (org.herac.tuxguitar.player.impl.midiport.alsa.MidiSystem.)
	     (.open)
	     (.openPort 14 0)))
  (var-set seqr  (doto (. javax.sound.midi.MidiSystem 
			  (getSequencer false)) 
		   (.open) 
		   (.setLoopCount (. Sequencer LOOP_CONTINUOUSLY))
		   (.setLoopEndPoint -1)
		   (.setLoopStartPoint 0))))

  

(defn init 
  []
  (do (dosync (ref-set deck-queue-ref (new-phrase)))
      (init-midi :java)
      (vector)))


