(ns org.musicbox.genetic
  (:gen-class)
  (:import [java.util Random])
  (:use [clojure.contrib.test-is]
	[clojure.contrib.str-utils :only (str-join)])
  (:require [org.musicbox.composer :as composer]))

(def harmony-store (ref nil))
(def rhyme-store (ref nil))

(defstruct store-record :rating :gene)

(defn random-harmony 
  []
  (composer/random-seq (range 4 5)  
                       (concat (take 15 (drop 14 composer/pitch-table)) 
                               ["R" "R"])))

(defn random-rhyme
  []
  (composer/random-seq (range 2 4) (range 1 3)))

(defn populate
  [n]
  (do
    (dosync (ref-set harmony-store
                     (take n (repeatedly random-harmony))))
    (dosync (ref-set rhyme-store
                     (take n (repeatedly random-rhyme))))))

(defn evolve
  "Discard some genes in the store and add some new ones"
  [min-rating]
  (do
    (dosync (ref-set harmony-store
                     (let [new-store (filter #(> (:rating %) min-rating) @harmony-store)
                           dropped (- (count @harmony-store) (count new-store))]
                       (concat new-store
                               (take dropped (repeatedly random-harmony))))))
    (dosync (ref-set rhyme-store
                     (let [new-store (filter #(> (:rating %) min-rating) @rhyme-store)
                           dropped (- (count @rhyme-store) (count new-store))]
                       (concat new-store
                               (take dropped (repeatedly random-rhyme))))))))

(defn generate-pipe
  "Build a semi-random grammar"
  [depth children]
  (vector {:indices (composer/random-seq (range 2 5) (range 1 4))
	   :rhyme  (nth @rhyme-store (rand-int (count @rhyme-store)))
	   :harmony (nth @harmony-store (rand-int (count @harmony-store)))
	   :velocity (composer/random-seq (range 2 4) (range 4 6))
	   :instrument false
	   :children (if (> depth 0)
		       (generate-pipe (dec depth) children)
		       children)}))

(defn generate-rest-mask
  [density children]
  (vector {:indices (composer/random-seq [1] (range 1 4))
	   :rhyme []
	   :harmony (composer/random-seq (range 2 4) (cons "R" (take density (repeat "A0"))))
	   :velocity []
	   :instrument false
	   :children children}))

(defn generate-voice
  "Generate a random voice, or partially random"
  [instrument octave children]
  (vector {:indices (composer/random-seq (range 2 4) (range 1 4))
           :rhyme (nth @rhyme-store (rand-int (count @rhyme-store)))
           :harmony (nth @harmony-store (rand-int (count @harmony-store)))
           :velocity (composer/random-seq (range 2 4) (range 2 4))
           :instrument instrument
           :children children}))

(defn generate-bridge
  [children]
  (vector {:indices (composer/random-seq [1] (range 1 5))
	   :rhyme []
	   :harmony ["A0"]
	   :velocity []
	   :instrument false
	   :children children}))

(defn generate-song
  []
  (generate-pipe 0
   (vector (struct composer/grammar
                   (composer/random-seq [4] [1 1 1 1 1 2 2 2 2 3 4])
                   []
                   (composer/random-seq [4] ["A0" "C0" "E0"])
                   []
                   false
                   (generate-bridge 
                    (concat (generate-rest-mask 2 
                             (generate-voice "Piano" 6 
                              (concat (generate-rest-mask 3 
                                       (generate-voice "Piano" 7 []))
                                      (generate-rest-mask 3 
                                       (generate-rest-mask 2 
                                        (generate-voice "Piano" 7 []))))))
                            (generate-rest-mask 2 
                             (generate-pipe 0 
                              (generate-rest-mask 2 
                               (generate-voice "Piano" 5 []))))))))))
