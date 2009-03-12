
(ns org.musicbox.composer
  (:gen-class
   :init init)
  (:import [java.util Random]))

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

(defn- synch-harmony-down
  [current-key & harmonies] 
  (reduce (fn [harmony1 harmony2]
            (let [offset (nth (cycle harmony1) current-key)]
             (for [pitch harmony2]
               (if (or (= offset "R") (= pitch "R"))
                 "R"
                 (nth pitch-table 
                      (+ (rem (pitch-map pitch) 12)
                         (pitch-map offset)))))))
          harmonies))
             
(defn- synch-rhythm-down
  [current-key & xs]
  (let [x (apply concat xs)]
    (take (count x)
	  (drop current-key 
		(cycle x)))))

(defn- synch-velocity-down
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

; The main composition function walks the tree, generate some music bits and 
; synchronizes them.

(defn compose
  "Does the actual work of composing a grammar tree into a vector of voices"
  [{:keys [key-seq children rhyme harmony velocity instrument]}]
  (splice (for [current-key key-seq] 
            (synch-up (conj (apply concat
                                   (for [child children]
                                     (compose (assoc child 
                                                :harmony (synch-harmony-down current-key harmony (:harmony child))
                                                :rhyme (synch-rhythm-down current-key rhyme (:rhyme child))
                                                :velocity (synch-velocity-down current-key velocity (:velocity child))))))
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
                    (iterate (fn [_] (if (> (rnd 30 0) 1) 
                                       (nth pitch-table (rnd 12 36))
                                       "R"))
                             (nth pitch-table (rnd 12 24))))
     :velocity (take (rnd 3 2)
                     (iterate (fn [_] (rnd 3 4))
                              (rnd 3 4)))
     :instrument (if (< depth 2)
                   (if (< depth 1)
                     (lead-instrument-table (rnd (count lead-instrument-table) 0))
                     (bass-instrument-table (rnd (count bass-instrument-table) 0)))
                   false)
     :children (if (> depth 0)
                 (vector (generate-grammar (dec depth)))
                 (vector))}))
			   

