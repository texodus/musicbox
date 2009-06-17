
(ns org.musicbox.midi
  (:gen-class)
  (:import [javax.sound.midi Sequencer Sequence Synthesizer]
           [java.io File BufferedReader InputStreamReader]
           [java.util Random]
           [org.jfugue MidiRenderer MusicStringParser Pattern Player Rhythm])
  (:use [org.musicbox.composer]
        [clojure.contrib.duck-streams]
        [clojure.contrib.str-utils :only (str-join)]))

(defn note-string
  [note octave]
  (let [note-str (note :pitch)]
    (str note-str
         (apply str (take (note :duration)
                          (repeat "q")))
         "a"
         (* (note :velocity) 20))))
 
(defn voice-string
  [voice octave]
  (str ;"I[" (voice :instrument) "] "
       (str-join \ (map #(note-string % octave) (voice :notes)))))
 
(defn build-string
  [voices]
  (str-join \ (for [label-voice (partition 2 (interleave voices (iterate inc 1)))]
                (str \v (second label-voice)
                     \ "Rww"
                     \ (voice-string (first label-voice) (second label-voice))
                     \ "Rww"))))

(defn build-midi
  [string]
  (let [pattern (Pattern. string)
        parser (MusicStringParser.)
        renderer (MidiRenderer. 0 200)]
    (do
      (dorun (for [token (. string split " ")]
               (if (. token startsWith "I")
                 (. System/out print (str "\033[31m " token \ ))
                 (if (. token startsWith "R")
                   (. System/out print (str "\033[32m " token \ ))
                   (. System/out print (str "\033[33m " token \ ))))))
      (. parser (addParserListener renderer))
      (. parser (parse pattern))
      (. renderer getSequence))))
 
(defn play-midi
  [midi tempo-atom progress-atom run-flag]
  (with-open [synth (doto (. javax.sound.midi.MidiSystem getSynthesizer)
                      (.open))
;                      (.openPort 14 0))
              seqr (doto (. javax.sound.midi.MidiSystem (getSequencer false)) (.open))]
      (do (.setReceiver (.getTransmitter seqr) (.getReceiver synth))
          (.setSequence seqr midi)
          (.start seqr)
          (while (and @run-flag (.isRunning seqr))
            (. Thread (sleep 0))
            (reset! progress-atom (quot (* 100 (. seqr getTickPosition))
                                   (. seqr getTickLength)))
            (. seqr (setTempoFactor @tempo-atom)))
          (.println System/out "Finished"))))
 
(defn play
  [grammar tempo-atom progress-atom run-flag]
  (-> grammar
      compose
      build-string
      build-midi
      (play-midi tempo-atom progress-atom run-flag)))

(defn greatest-hits
  []
  (loop [lines (read-lines "/home/slink/Documents/greatest-hits.txt")
         name 0]
    (do (if (not (empty? (first lines)))
          (. javax.sound.midi.MidiSystem 
             (write (build-midi (first lines)) 
                    1 
                    (java.io.File. (str "/home/slink/Desktop/" name ".mid")))))
        (if (< 1 (count lines)) (recur (rest lines) (+ name 1))))))

(defn set-soundbank
  [soundbank]
  (do (. javax.sound.midi.MidiSystem (getSoundbank (java.io.File. soundbank)))))

