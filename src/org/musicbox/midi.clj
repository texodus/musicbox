
(ns org.musicbox.midi
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
        [clojure.contrib.str-utils :only (str-join)]))

(defn- note-string
  [note octave]
  (let [note-str (note :pitch)]
    (str note-str
        ; (if (!= note-str "R") (+ octave 2))
         (apply str (take (note :duration)
                          (repeat "q")))
         "a"
         (* (note :velocity) 20))))
 
(defn- voice-string
  [voice octave]
  (str "I[" (voice :instrument) "] "
       (str-join \ (map #(note-string % octave) (voice :notes)))))
 
(defn- build-string
  [voices]
  (str-join \ (for [label-voice (pair-off voices (iterate inc 1))]
                (str \v (second label-voice)
                     \ "Rww"
                     \ (voice-string (first label-voice) (second label-voice))
                     \ "Rww"))))
 
(defn- build-midi
  [string]
  (let [pattern (Pattern. string)
        parser (MusicStringParser.)
        renderer (MidiRenderer. 0 200)]
    (do
      (. System/out println string)
      (. parser (addParserListener renderer))
      (. parser (parse pattern))
      (. renderer getSequence))))
 
(defn- play-midi
  [midi tempo-atom progress-atom run-flag-atom]
  (with-open [synth (doto (. MidiSystem getSynthesizer) (.open))
              seqr (doto (. MidiSystem (getSequencer false)) (.open))]
      (do (.setReceiver (.getTransmitter seqr) (.getReceiver synth))
          (.setSequence seqr midi)
          (.start seqr)
          (while (and @run-flag-atom (.isRunning seqr))
            (. Thread (sleep 0))
            (reset! progress-atom (quot (* 100 (. seqr getTickPosition))
                                   (. seqr getTickLength)))
            (. seqr (setTempoFactor @tempo-atom)))
          (.println System/out "Finished"))))
 
(defn play
  [grammar tempo-atom progress-atom run-flag-atom]
  (-> grammar
      compose
      build-string
      build-midi
      (play-midi tempo-atom progress-atom run-flag-atom)))
 
