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

(ns musicbox
  (:import [javax.sound.midi MidiSystem Sequencer Sequence Synthesizer]
           [org.jfugue MidiRenderer MusicStringParser Pattern Player Rhythm]
           [java.io File BufferedReader InputStreamReader])
  (:use [clojure.contrib.str-utils :only (str-join)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

(defstruct note :duration :pitch)
(defstruct grammar :key-seq :rhyme :harmony :instrument :children)

(defn != 
  [a b] 
  (not (= a b)))

(defn pause 
  [] 
  (-> *in* BufferedReader. .readLine))

(defn pair-off
  [x y]
  (partition 2 (interleave x y)))

(defn- voice-length 
  "Determine the total duration of a vector of notes"
  [notes]
  (reduce + (map :duration notes)))

(defn- resize-voice 
  "Modulate a list of notes to be a specific (longer) total duration"
  [voice length] 
  (let [head (first voice) 
        tail (rest (cycle voice))]
    (if (< (head :duration) length)
      (cons head 
            (resize-voice tail (- length (head :duration))))
      (vector (struct note 
                      length 
                      (head :pitch))))))

(defn- longest
  "Determines the voice with the longest total duration"
  ([voices] 
     (longest nil voices))
  ([top voices] 
     (if (= (count voices) 0)
       top
       (if (< (voice-length top) (voice-length (first voices)))
         (longest (first voices) (rest voices))
         (longest top (rest voices))))))

(defn- splice 
  "Join together a sequence of meters"
  [meters] 
  (reduce #(for [x (partition 2 (interleave %1 %2))] 
             (apply into x)) 
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
    (for [voice (filter #(-> % empty? not) voices)]
      (resize-voice voice max-voice))))

; The main composition function walks the tree and calls the synchronize 
; functions when necessary

(defn compose
  "Does the actual work of the composing a grammar tree into a vector of voices"
  [{:keys [key-seq children rhyme harmony instrument]}]
  (splice (for [current-key key-seq] 
            (synch-up (conj (apply concat
                                   (for [child children]
                                     (compose (assoc child 
                                                :harmony (synch-down current-key harmony (:harmony child))
                                                :rhyme (synch-down current-key rhyme (:rhyme child))))))
                            (if instrument
                              (vector (struct note 
                                              (first (drop current-key (cycle rhyme)))
                                              (first (drop current-key (cycle harmony)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; MIDI

; These functions build a JFugue MusicString from note vectors
 
(defn- note-string 
  [note octave]
  (let [note-str (note :pitch)]
    (str note-str
         (if (!= note-str "R") (+ octave 2))
         (apply str (take (note :duration) 
                          (repeat "q"))))))

(defn- voice-string 
  [voice octave]
  (str-join \ 
            (map #(note-string % octave) 
                 voice)))

(defn- build-string 
  [voices]
  (str-join \ (for [voice (pair-off voices (iterate inc 1))]
                (str \v 
                     (second voice) 
                     \ 
                     (voice-string (first voice) (second voice))))))

;  MIDI and JFugue utility functions

(defn- build-midi 
  [string]
  (let [pattern (Pattern. string)
        parser (MusicStringParser.)
        renderer (MidiRenderer. 0 500)]
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
          (pause))))

(defn play 
  [grammar]
  (-> grammar 
      compose 
      build-string 
      build-midi 
      play-midi))

;;;; Sample Song 

(comment
  (load-file "musicbox-clojure.clj")
)

  (play (struct grammar 
                [1 3 2 4 1]
                [1 2 3 5] 
                ["D" "F" "A" "R" "B"]
                false
                [(struct grammar
                         [2 2 1 3 1]
                         [2 4]
                         ["E" "G" "R" "A" "F"]
                         true
                         [(struct grammar
                                  [2 1 3]
                                  [1 2 4 5]
                                  ["A" "R" "R" "B" "D" "G"]
                                  true
                                  [])])
                 (struct grammar
                         [1 1 5 1]
                         [1 2 4 5]
                         ["C" "R" "D" "E" "R" "F"]
                         true
                         [])]))






