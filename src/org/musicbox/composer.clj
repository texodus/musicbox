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

(def note-harmonic-table ["A" "B" "C" "D" "E" "F" "G"])

(defstruct note :duration :pitch :velocity)
(defstruct grammar :theme :rhyme :harmony :velocity :instrument :children)
(defstruct voice :instrument :notes)

(defn pair-off
  [x y]
  (map #(apply vector %)
       (partition 2 (interleave x y))))

(def pitch-table
     (map #(apply str %) 
          (take 120 (pair-off (cycle note-table)
                              (map #(quot % 12) (iterate inc 0))))))

(def harmonic-table
     (map #(apply str %) 
          (take 70 (pair-off (cycle note-harmonic-table)
                              (map #(quot % 12) (iterate inc 0))))))

(def pitch-map 
    (into {} 
          (take 120 (pair-off pitch-table 
                              (iterate inc 0)))))
                              

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

; Composition Notes
;
; verify identity synch-downs
; verify some simple transformations

; synch-down manipulates data of child nodes based on the parent node

(defmulti vary 
  "Manipulate a grammar node on the way down, dispatching on key from the grammar struct"
  (fn [_ _ [k _]] k))

(defmethod vary :harmony
  [modulus {harmony1 :harmony} [_ harmony2]] 
  [:harmony (reduce (fn [harmony1 harmony2]
                      (let [offset (nth (cycle harmony2) modulus)]
                        (for [pitch harmony1]
                          (harmonize pitch offset))))
                    [harmony1 harmony2])])
             
(defmethod vary :rhyme
  [modulus {rhyme1 :rhyme} [_ rhyme2]]
  [:rhyme (let [x (concat rhyme2 rhyme1)]
            (take (count x)
                  (drop modulus (cycle x))))])

(defmethod vary :theme
  [modulus {theme1 :theme} [_ theme2]]
  [:theme (map #(rem (+ (nth (cycle theme1) modulus) %) 12) theme2)])

(defmethod vary :emphasis
  [modulus {emphasis1 :emphasis} [_ emphasis2]]
  [:emphasis (let [x (concat emphasis2 emphasis1)]
               (take (count x)
                     (drop modulus (cycle x))))])

(defmethod vary :default [_ _ pair] pair)

(defn- synchronize
  "Synchronize a sequence of voices (they must be the same length)"
  [voices]
  (let [max-voice (voice-length (longest voices))]
    (for [voice (filter #(-> % identity :notes empty? not) voices)]
      (resize-voice voice max-voice))))

; The main composition function walks the tree, generate some music bits and 
; synchronizes them.

(defn compose
  "Does the actual work of composing a grammar tree into a vector of voices"
  [{:keys [theme children rhyme harmony emphasis instrument] :as grammar}]
  (splice (for [modulus theme] 
            (synchronize (conj (apply concat
                                      (for [child children]
                                        (compose (into {} (map (partial vary modulus grammar) child)))))
                               (if instrument 
                                 (struct voice 
                                         instrument
                                         (vector (struct note 
                                                         (first (drop modulus (cycle rhyme)))
                                                         (first (drop modulus (cycle harmony)))
                                                         (first (drop modulus (cycle emphasis))))))))))))

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
  (vector {:theme (random-seq (range 2 5) (range 1 4))
	   :rhyme  (random-seq (range 2 4) (range 1 4))
	   :harmony (random-seq (range 2 5) (take 12 pitch-table))
	   :emphasis (random-seq (range 2 4) (range 4 6))
	   :instrument false
	   :children (if (> depth 0)
		       (generate-pipe (dec depth) children)
		       children)}))

(defn generate-rest-mask
  [density children]
  (vector {:theme (random-seq [1] (range 1 4))
	   :rhyme []
	   :harmony (random-seq (range 2 4) (cons "R" (take density (repeat "A0"))))
	   :emphasis []
	   :instrument false
	   :children children}))

(defn generate-voice
  "Generate a random voice, or partially random"
  ([instrument octave children]
     (vector {:theme (random-seq (range 2 4) (range 1 6)) ; 6 -> 5
              :rhyme (random-seq (range 2 4) (range 1 3))
              :harmony (random-seq (range 4 5)  
                                   (concat (take 12 (drop (* octave 7) 
                                                          harmonic-table)) 
                                           ["R" "R"]))
              :emphasis (random-seq (range 2 4) (range 3 5))
              :instrument instrument
              :children children}))
  ([instrument octave {harmony :harmony rhyme :rhyme} children]
     (vector {:theme (random-seq (range 2 4) (range 1 4))
              :rhyme rhyme
              :harmony harmony
              :emphasis (random-seq (range 2 4) (range 2 4))
              :instrument instrument
              :children children})))

(defn generate-bridge
  [children]
  (vector {:theme (random-seq [1] (range 1 5))
	   :rhyme []
	   :harmony ["A0"]
	   :emphasis []
	   :instrument false
	   :children children}))

(defn generate-drumline
  [children]
  (vector {:theme (random-seq (range 2 4) (range 1 4))
           :rhyme (random-seq (range 2 4) (range 1 4))
           :harmony (random-seq (range 2 4) ["[Snare]"])
           :emphasis (random-seq (range 2 4) (range 5 6))
           :instrument true
           :children children}))

(defn generate-drums
  []
  (generate-bridge (generate-rest-mask 2 (generate-drumline []))))

(defn generate-piano
  ([]
     (generate-bridge (concat (generate-rest-mask 2 (generate-voice "Synth_Bass_2" 6 (concat (generate-rest-mask 3 (generate-voice "Synth_Bass_2" 7 [])))))
                                                                                      (generate-rest-mask 3 (generate-rest-mask 2 (generate-voice "Synth_Bass_2" 7 []))))))
                              (generate-rest-mask 2 (generate-pipe 0 (generate-rest-mask 2 (generate-voice "Synth_Bass_2" 5 [])))))))
  ([bass soprano tenor alto]
     (generate-bridge (concat (generate-rest-mask 2 (generate-voice "Piano" 6 bass (concat (generate-rest-mask 3 (generate-voice "Piano" 7 tenor []))
                                                                                      (generate-rest-mask 3 (generate-rest-mask 2 (generate-voice "Piano" 7 alto []))))))
                              (generate-rest-mask 2 (generate-pipe 0 (generate-rest-mask 2 (generate-voice "Piano" soprano []))))))))
     
  

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

