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
  (:use [clojure.contrib.str-utils :only (str-join)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

(defstruct note :duration :pitch)
(defstruct grammar :rhyme :harmony :instrument :children)

(defn != 
  [a b] 
  (not (= a b)))

(defn xor 
  [& args]
  (-> (filter identity args) 
      count 
      odd?))

(defn pause 
  [] 
  (-> *in* BufferedReader. .readLine))

(defn if-conj 
  [pred? one all]
  (if pred?
    (conj all  one)
    all))

(defn pair-off 
  [x y]
  (partition 2 (interleave x y)))

(defn- voice-length 
  "Determine the total duration of a vector of notes"
  [notes]
  (reduce + 
	  (map :duration 
	       notes)))

(defn- resize-voice 
  "Modulate a list of notes to be a specific (longer) total duration"
  [length voice] 
  (let [head (first voice) 
	tail (rest (cycle voice))]
    (if (< (head :duration) 
	   length)
      (cons head 
	    (resize-voice (- length 
			     (head :duration)) 
			  tail))
      (vector (struct note 
		      length 
		      (head :pitch))))))

(defn- longest
  "Determines the voice with the longest total duration"
  ([voices] 
     (longest nil voices))
  ([top voices] 
     (if (= (count voices) 
	    0)
       top
       (if (< (voice-length top) 
	      (voice-length (first voices)))
	 (longest (first voices) 
		  (rest voices))
	 (longest top 
		  (rest voices))))))

(defn- splice 
  "Join together a sequence of meters"
  [meters] 
  (reduce #(for [x (pair-off %1 %2)] 
	     (apply into x)) 
	  meters))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Composition

; These Functions determine the characteristics of the composition by defining 
; operations for harmonizing (pitch) and synchronizing (rhythm) a walk of a 
; grammar tree.  Change them for fun & profit! 

(defn- harmonize-down 
  "Harmonize a sequence of harmonies"
  [& harmonies]
  (reduce #(for [pair (pair-off %1 %2)] 
	     (apply xor pair)) 
	  harmonies))

(defn- harmonize-up 
  "Harmonize a sequence of voices"
  [voices] voices)

(defn- synchronize-down 
  "Synchronize a sequence of rhymes" 
  [parent-key rhyme] 
  (nth (cycle rhyme) 
       parent-key))

(defn- synchronize-up 
  "Synchronize a sequence of voices (they must be the same length)"
  [voices]
  (map #(-> voices 
	    longest 
	    voice-length 
	    (resize-voice %))  
       (filter identity 
	       voices)))

; Determines what a new voice looks like - this is the first conversion 
; from grammar to concrete note

(defn- new-voice 
  "Compose a new voice"
  [parent-key new-key new-harmony]
  [(struct note 
	   new-key 
	   (-> (filter second 
		       (pair-off (iterate inc 0) 
				 new-harmony)) 
	       cycle 
	       (nth (+ new-key 
		       parent-key)) 
	       first))])

; The main composition function walks the tree and calls the harmonize and 
; synchronize functions when necessary

(defn compose 
  "Compose a grammar tree into a vector voices"
  ([grammar] 
     (compose (count (grammar :rhyme)) 
	      (apply vector 
		     (take 12 
			   (repeat false))) 
	      grammar))
  ([inherited-key inherited-harmony {:keys [children rhyme harmony instrument]}]
     (splice (for [current-key (take inherited-key 
				     (cycle rhyme))]
	       (let [current-harmony (harmonize-down inherited-harmony 
						     harmony) 
		     current-rhyme (synchronize-down inherited-key 
						     rhyme)
		     voices (apply concat 
				   (map #(compose current-key 
						  current-harmony 
						  %) 
					children))]
		 (-> (if instrument
		       (conj voices 
			     (new-voice current-key 
					current-rhyme 
					current-harmony)) 
		       voices) 
		     harmonize-up 
		     synchronize-up))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; MIDI

(def note-table 
     ["A" "A#" "B" "C"
      "C#" "D" "D#" "E"
      "F" "F#" "G" "G#"])
 
(defn- note-string 
  [note octave]
  (let [note-str (nth note-table 
		      (note :pitch))]
    (str note-str
	 (if (!= note-str "R") 
	   (+ octave 2))
	 (apply str (take (note :duration)
			  (repeat "q"))))))

(defn- voice-string 
  [voice octave]
  (str-join \ 
	    (map #(note-string % octave) 
		 voice)))

(defn- build-string 
  [voices]
  (str-join \ 
	    (for [voice (pair-off voices 
				  (iterate inc 1))]
		  (str \v 
		       (second voice) 
		       \ 
		       (voice-string 
			(first voice) 
			(second voice))))))

(defn- build-midi 
  [string]
  (let [pattern (Pattern. string)
	parser (MusicStringParser.)
	renderer (MidiRenderer. 0 250)]
    (do
      (. System/out println string)
      (. parser (addParserListener renderer))
      (. parser (parse pattern))
      (. renderer getSequence))))


(defn- play-midi 
  [midi]
  (with-open [synth (doto (. MidiSystem 
			     getSynthesizer) 
		      (.open))
              seqr (doto (. MidiSystem 
			    (getSequencer false)) 
		     (.open))]
      (do (.setReceiver (.getTransmitter seqr) 
			(.getReceiver synth))
          (.setSequence seqr 
			midi)
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
  (play (struct grammar 
		[1 2 2 5 2 1] 
		[true false false false 
		 true false false false 
		 false true false true]
		false
		[(struct grammar
			 [1 3 1 4 1]
			 [false true false false 
			  false true false false 
			  true false false false]
			 false
			 [(struct grammar
				  [4 3 2 1 2]
				  [false true false true 
				   true false false false 
				   true false true false]
				  true
				  [])])
		 (struct grammar
			 [1 2 1 5 1]
			 [false false true false 
			  false true false false 
			  false false true false]
			 true
			 [])]))
)




