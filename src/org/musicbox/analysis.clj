(ns org.musicbox.analysis
  (:gen-class)
  (:import [java.util Random])
  (:use [clojure.contrib.str-utils :only (str-join)])
  (:require [org.musicbox.composer :as composer]
            [org.musicbox.midi :as midi]))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Parsing - Pitch

(defn find-largest
  [m]
  (loop [pairs m, max 0]
    (if (empty? pairs)
      max
      (if (> (-> pairs first second) max)
        (recur (drop 1 pairs)
               (-> pairs first second))
        (recur (drop 1 pairs) max)))))

(defn- count-pattern
  [pitches pattern]
  (loop [pitches* pitches, total 0]
    (if (< (count pitches*) (count pattern))
      total
      (recur (drop 1 pitches*)
             (if (= (take (count pattern) pitches*)
                    pattern)
               (+ 1 total)
               total)))))

(defn analyze-pitches
  [pitches min-length max-length]
  (let [total (* (- max-length min-length) (count pitches) 0.01)
        counter (memoize count-pattern)
        log (agent 0)]
    (apply concat (for [pattern-length (range min-length max-length)]
                    (into {}
                          (pmap #(do (send-off log (fn [a] (do (print (str (/ a total) "%\r")) (+ 1 a))))
                                     (vector % (counter pitches %)))
                                (partition pattern-length 1 pitches)))))))

(defn extract-pitches
  "Takes a JFugue musicstring and extracts just the pitches"
  [string]
  (map #(first (. % (split "/")))
       (filter #(some #{\/} %) (. string (split " ")))))

(defn get-pitches
  "Parses the supplied midi filename, and returns the c most common pitch sequences that contain
   between min and max pitches"
  [file-name max min c]
  (let [pitch-count-map (analyze-pitches (extract-pitches (midi/parse-midi file-name)) min max)]
    (loop [return [] greatest (find-largest pitch-count-map)]
      (let [extracted (concat return (loop [return [], pairs pitch-count-map]
                                       (if (zero? (count pairs))
                                         return
                                         (if (= (-> pairs first second) greatest)
                                           (recur (cons (-> pairs first first) return) (drop 1 pairs))
                                           (recur return (drop 1 pairs))))))]
        (if (>= (count extracted) c)
          extracted
          (recur extracted (- greatest 1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Parsing - Rhyme

(defn extract-rhymes
  [string]
  (map #(first (. (second (. % (split "/"))) (split "(a|b|c|d|e|f|g|h)")))
       (filter #(some #{\/} %) (. string (split " ")))))

(defn analyze-rhymes
  [string]
  (map #(. Float (parseFloat (apply str (take 4 %)))) 
       (sort (into #{} (extract-rhymes string)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Parsing - Main

(defn parse
  [filename]
  (let [midistring (midi/parse-midi filename)]
    ; The structure of the first parsing round will be a map of (position -> [notes]) by eating @ strings
    (loop [[token & tail] (. midistring (split " "))
           result (sorted-map)
           position 0]
      (if (nil? token)
        result
        (condp = (first token)
          ; Time token; modify the position pointer
          \@ (recur tail result (Integer/parseInt (apply str (drop 1 token))))
          ; velocity signal; do nothing
          \X (recur tail result position)
          ; voice; there is only one of these for now TODO
          \V (recur tail result position)
          ; empty; do nothing
          nil (recur tail result position)
          ; note; add it to the map
          (recur tail 
                 (assoc result 
                   position (let [notes (result position)]
                              (if (nil? notes) 
                                (vector token)
                                (conj notes token))))
                 position))))))
          
      
    

