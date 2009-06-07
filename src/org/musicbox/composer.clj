(ns org.musicbox.composer
  (:gen-class)
  (:import [java.util Random])
  (:use [clojure.contrib.test-is]
	[clojure.contrib.str-utils :only (str-join)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Common

(def note-table ["A" "A#" "B" "C"
                 "C#" "D" "D#" "E"
                 "F" "F#" "G" "G#"])

(defstruct note :duration :pitch :velocity)
(defstruct grammar :indices :rhyme :harmony :velocity :instrument :children)
(defstruct voice :instrument :notes)

(defn != 
  [a b] 
  (not (= a b)))

(defn pair-off
  [x y]
  (partition 2 (interleave x y)))

(def pitch-table
     (map #(apply str %) 
          (take 120 (pair-off (cycle note-table)
                              (map #(quot % 12) (iterate inc 0))))))

(def pitch-map 
    (reduce #(assoc %1 (second %2) (first %2)) 
            {} 
            (take 120 (pair-off (iterate inc 0)
                                pitch-table))))

;;;; These are utility functions for mixing voices

(defn voice-length 
  "Determine the total duration of a vector of notes"
  [voice]
  (if voice
    (reduce + (map :duration (voice :notes)))
    0))

(defn resize-voice 
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

(defn longest
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

(defn- harmonize
  [pitch offset]
  (if (or (= offset "R") (= pitch "R"))
    "R"
    (nth pitch-table 
	 (+ (rem (pitch-map pitch) 12)
	    (pitch-map offset)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Composition

(defmulti synch-down 
  "Manipulate a grammar node on teh way down"
  (fn [& x] (identity (first x))))

(defmethod synch-down :harmony
  [_ current-key & harmonies] 
  (reduce (fn [harmony1 harmony2]
            (let [offset (nth (cycle harmony2) current-key)]
             (for [pitch harmony1]
	       (harmonize pitch offset))))
          harmonies))
             
(defmethod synch-down :rhyme
  [_ current-key & xs]
  (let [x (apply concat xs)]
    (take (count x)
	  (drop current-key 
		(cycle x)))))

(defmethod synch-down :indices
  [_ current-key rhyme1 rhyme2]
  (map #(rem (+ (nth (cycle rhyme1) current-key) %) 12) rhyme2))

(defmethod synch-down :velocity
  [_ current-key & velocities]
  (let [x (apply concat velocities)]
    (take (count x)
	  (drop current-key 
		(cycle x)))))

(defmethod synch-down :instrument [_ _ _ x] x)

(defmethod synch-down :children [_ _ _ x] x)

(defmethod synch-down :default 
  [current-key grammar child] 
  (into {} (for [pair child]
             (vector (first pair) 
                     (apply #(synch-down %1 current-key (grammar %1) %2) pair)))))

(defn- synch-up 
  "Synchronize a sequence of voices (they must be the same length)"
  [voices]
  (let [max-voice (voice-length (longest voices))]
    (for [voice (filter #(-> % identity :notes empty? not) voices)]
      (resize-voice voice max-voice))))

; The main composition function walks the tree, generate some music bits and 
; synchronizes them.

(defn compose
  "Does the actual work of composing a grammar tree into a vector of voices"
  [{:keys [indices children rhyme harmony velocity instrument] :as grammar}]
  (splice (for [current-key indices] 
            (synch-up (conj (apply concat
                                   (for [child children]
                                     (compose (synch-down current-key grammar child))))
                            (if instrument 
                              (struct voice 
                                      instrument
                                      (vector (struct note 
                                                      (first (drop current-key (cycle rhyme)))
                                                      (first (drop current-key (cycle harmony)))
                                                      (first (drop current-key (cycle velocity))))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Composer Tools
	 
(defn random-seq 
  [length-pool element-pool]
  (let [rnd (fn [coll] (nth coll (rand-int (count coll))))]
    (take (rnd length-pool) (iterate (fn [_] (rnd element-pool)) (rnd element-pool)))))

(defn generate-pipe
  "Build a semi-random grammar"
  [depth children]
  (vector {:indices (random-seq (range 2 5) (range 1 4))
	   :rhyme  (random-seq (range 2 4) (range 1 4))
	   :harmony (random-seq (range 2 5) (take 12 pitch-table))
	   :velocity (random-seq (range 2 4) (range 4 6))
	   :instrument false
	   :children (if (> depth 0)
		       (generate-pipe (dec depth) children)
		       children)}))

(defn generate-rest-mask
  [density children]
  (vector {:indices (random-seq [1] (range 1 4))
	   :rhyme []
	   :harmony (random-seq (range 2 4) (cons "R" (take density (repeat "A0"))))
	   :velocity []
	   :instrument false
	   :children children}))

(defn generate-voice
  [instrument octave children]
  (vector {:indices (random-seq (range 2 4) (range 1 4))
	   :rhyme (random-seq (range 2 4) (range 1 3))
	   :harmony (random-seq (range 4 5)  
                                (concat (take 12 (drop (* octave 7) 
                                                       pitch-table)) 
                                        ["R" "R"]))
	   :velocity (random-seq (range 2 4) (range 2 4))
	   :instrument instrument
	   :children children}))

(defn generate-bridge
  [children]
  (vector {:indices (random-seq [1] (range 1 5))
	   :rhyme []
	   :harmony ["A0"]
	   :velocity []
	   :instrument false
	   :children children}))

(defn generate-drumline
  [children]
  (vector {:indices (random-seq (range 2 4) (range 1 4))
           :rhyme (random-seq (range 2 4) (range 1 4))
           :harmony (random-seq (range 2 4) ["[Snare]"])
           :velocity (random-seq (range 2 4) (range 5 6))
           :instrument true
           :children children}))

(defn generate-drums
  []
  (generate-bridge (generate-rest-mask 2 (generate-drumline []))))

(defn generate-piano
  []
  (generate-bridge (concat (generate-rest-mask 2 (generate-voice "Piano" 6 (concat (generate-rest-mask 3 (generate-voice "Piano" 7 []))
                                                                                   (generate-rest-mask 3 (generate-rest-mask 2 (generate-voice "Piano" 7 []))))))
                           (generate-rest-mask 2 (generate-pipe 0 (generate-rest-mask 2 (generate-voice "Piano" 5 [])))))))
                          ; (generate-rest-mask 1 (generate-pipe 0 (generate-rest-mask 2 (generate-voice "Piano" 5 [])))))))

(defn generate-song
  []
  (let [common-voice (first (generate-voice "Piano" 4 []))]
    (concat (generate-rest-mask 3 (vector (assoc common-voice
                                            :children (concat (generate-rest-mask 2 (generate-voice "Piano" 5 []))
                                                              (generate-rest-mask 2 (generate-rest-mask 2 (generate-voice "Piano" 3 []))))
                                            :instrument "Piano")))
            (generate-rest-mask 4 (vector (assoc common-voice
                                            :children (generate-voice "Piano" 4 [])
                                            :instrument false))))))

    
