#!/usr/bin/env clojure

; Clojure Musicbox
; Copyright (c) 2009 Andrew Stein
; 
; Permission is hereby granted, free of charge, to any person
; obtaining a copy of this software and associated documentation
; files (the "Software"), to deal in the Software without
; restriction, including without limitation the rights to use,
; copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the
; Software is furnished to do so, subject to the following
; conditions:

; The above copyright notice and this permission notice shall be
; included in all copies or substantial portions of the Software.

; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
; OTHER DEALINGS IN THE SOFTWARE. 

(ns org.musicbox.application
  (:gen-class)
  (:import [javax.sound.midi MidiSystem Sequencer Sequence Synthesizer]
           [java.io File BufferedReader InputStreamReader]
           [java.awt.event ActionListener]
           [javax.swing.event ChangeListener]
           [java.util Random]
           [javax.swing JPanel JFrame JLabel JButton JSlider JProgressBar UIManager JRootPane]
	   [org.musicbox GUI]
           [org.jfugue MidiRenderer MusicStringParser Pattern Player Rhythm])
  (:use [clojure.contrib.str-utils :only (str-join)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

;;;; Constants 

(def note-table ["A" "A#" "B" "C"
                 "C#" "D" "D#" "E"
                 "F" "F#" "G" "G#"])

(def bass-instrument-table ["Rock_Organ" "Tubular_Bells"
                            "Electric_Bass_Pick" "Synth_Voice"
                            "Piano" "Woodblock"])

(def lead-instrument-table ["Violin" "Piano" 
                            "Alto_Sax" "Synthbrass_1"
                            "Flute" "Sawtooth"
                            "Orchestral_Strings"])

(defstruct note :duration :pitch :velocity)
(defstruct grammar :key-seq :rhyme :harmony :velocity :instrument :children)
(defstruct voice :instrument :notes)

;;;; Utility

(defn != 
  [a b] 
  (not (= a b)))

(defn pause 
  [] 
  (-> *in* BufferedReader. .readLine))

(defn pair-off
  [x y]
  (partition 2 (interleave x y)))

;;;; These are utility functions for mising voices

(defn- voice-length 
  "Determine the total duration of a vector of notes"
  [voice]
  (if voice
    (reduce + (map :duration (voice :notes)))
    0))

(defn- resize-voice 
  "Modulate a list of notes to be a specific (longer) total duration"
  [voice length] 
  (assoc voice 
    :notes (let [notes (voice :notes)
                 head (first notes) 
                 tail (take (count notes) (rest (cycle notes)))]
             (if (or (= (count notes) 1)  
                     (>= (head :duration) length))
               (vector (assoc head :duration length))               
               (cons head (:notes (resize-voice (assoc voice :notes tail) 
                                                (- length (head :duration)))))))))

(defn- longest
  "Determines the voice with the longest total duration"
  ([voices] 
     (longest nil voices))
  ([top voices] 
     (if (= (count voices) 0)
       top
       (if (< (voice-length top) 
              (voice-length (first voices)))
         (longest (first voices) (rest voices))
         (longest top (rest voices))))))

(defn- splice 
  "Join together a sequence of meters"
  [meters] 
  (reduce #(for [x (partition 2 (interleave %1 %2))] 
             (assoc (first x) 
               :notes (apply into (map :notes x)))) 
          meters))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Composition

; These Functions determine the characteristics of the composition by defining
; how nodes affect the harmony and rhythm of their children.  Change them for
; fun & profit!

(defn- synch-down 
  "Synchronize a sequence of note elements" 
  [current-key & xs] 
  (let [x (apply concat xs)]
    (take (count x)
	  (drop current-key 
		(cycle x)))))

(defn- synch-up 
  "Synchronize a sequence of voices (they must be the same length)"
  [voices]
  (let [max-voice (voice-length (longest voices))]
    (for [voice (filter #(-> % identity :notes empty? not) voices)]
      (resize-voice voice max-voice))))

; The main composition function walks the tree and calls the synchronize 
; functions when necessary

(defn compose
  "Does the actual work of composing a grammar tree into a vector of voices"
  [{:keys [key-seq children rhyme harmony velocity instrument]}]
  (splice (for [current-key key-seq] 
            (synch-up (conj (apply concat
                                   (for [child children]
                                     (compose (assoc child 
                                                :harmony (synch-down current-key harmony (:harmony child))
                                                :rhyme (synch-down current-key rhyme (:rhyme child))
                                                :velocity (synch-down current-key velocity (:velocity child))))))
                            (if instrument 
                              (struct voice 
                                      instrument
                                      (vector (struct note 
                                                      (first (drop current-key (cycle rhyme)))
                                                      (first (drop current-key (cycle harmony)))
                                                      (first (drop current-key (cycle velocity))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Style
		 
(defn generate-grammar
  "Build a semi-random grammar"
  [depth]
  (let [random (Random.)
        rnd (fn [range offset] (+ offset (. random (nextInt range))))]
    {:key-seq (take (rnd 5 2) 
                    (iterate (fn [_] (rnd 4 1)) 
                             (rnd 4 1)))
     :rhyme (if (< depth 2)
              (take (rnd 3 1)
                    (iterate (fn [_] (rnd 2 1))
                             (rnd 2 1)))
              (take (rnd 3 2)
                    (iterate (fn [_] (rnd 4 1))
                             (rnd 4 1))))
     :harmony (take (rnd 4 2)
                    (iterate (fn [_] (if (> (rnd 6 0) 1) 
                                       (nth note-table (rnd 12 0))
                                       "R"))
                             (nth note-table (rnd 12 0))))
     :velocity (take (rnd 3 2)
                     (iterate (fn [_] (rnd 5 2))
                              (rnd 5 2)))
     :instrument (if (< depth 2)
                   (if (< depth 1)
                     (lead-instrument-table (rnd (count lead-instrument-table) 0))
                     (bass-instrument-table (rnd (count bass-instrument-table) 0)))
                   false)
     :children (if (> depth 0)
                 (vector (generate-grammar (dec depth)))
                 (vector))}))
			   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; MIDI

;;;; These functions build a JFugue MusicString from note vectors
 
(defn- note-string 
  [note octave]
  (let [note-str (note :pitch)]
    (str note-str
         (if (!= note-str "R") (+ octave 2))
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

;;;; MIDI and JFugue utility functions

(def player-run-flag (atom false))
(def progress (atom 0))
(def tempo (atom 1))

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
  [midi]
  (with-open [synth (doto (. MidiSystem getSynthesizer) (.open))
              seqr (doto (. MidiSystem (getSequencer false)) (.open))]
      (do (.setReceiver (.getTransmitter seqr) (.getReceiver synth))
          (.setSequence seqr midi)
          (.start seqr)
          (while (and @player-run-flag (.isRunning seqr)) 
            (. Thread (sleep 500))
            (reset! progress (quot (* 100 (. seqr getTickPosition)) 
                                   (. seqr getTickLength)))
            (. seqr (setTempoFactor @tempo)))
          (.println System/out "Finished"))))

(defn play 
  [grammar]
  (-> grammar 
      compose 
      build-string 
      build-midi 
      play-midi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; GUI

(def panel (GUI.))

(def player-agent (agent nil))

(def progress-agent (send-off (agent nil) 
                              (fn [_] (while true 
                                (do (.. panel getJProgressBar (setValue @progress))
                                    (. Thread (sleep 500)))))))

(defn play-song 
  [_]
  (do
    (reset! player-run-flag true)
    (while @player-run-flag
      (play (let [main-grammar (generate-grammar 2)]
              (struct grammar
                      [1 1 2 1]
                      [1]
                      []
                      []
                      false
                      [main-grammar]))))
    (reset! progress 0)))


(defn -main [& args] 
  (do
    ; Create the application frame
    (def frame 
	 (doto (JFrame.)
	   (.setTitle "Clojure Music Box")
	   (.setDefaultCloseOperation (. JFrame EXIT_ON_CLOSE))
	   (.add panel)
	   (.setResizable false)
	   .pack 
	   .show))
    ; Add ActionListeners for the various controls
    (. (. panel getJButton1) 
       (addActionListener (proxy [ActionListener] []
                            (actionPerformed [event]
                                             (do (reset! player-run-flag false)
                                                 (send-off player-agent play-song))))))
    (. (. panel getJButton2)
       (addActionListener (proxy [ActionListener] []
                            (actionPerformed [event]
                                             (do (reset! player-run-flag false))))))       
    (. (. panel getJSlider)
       (addChangeListener (proxy [ChangeListener] []
                            (stateChanged [event]
                                          (do (reset! tempo (+ 0.25 (* 1.5 (/ (.. event getSource getValue) 100)))))))))      
    (. UIManager (setLookAndFeel (. UIManager getSystemLookAndFeelClassName)))))
