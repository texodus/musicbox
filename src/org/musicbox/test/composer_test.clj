(ns org.musicbox.test.composer-test
  (:gen-class)
  (:use [org.musicbox.composer]
	[clojure.contrib.test-is]))

(deftest test-common
  (testing "Common - utility functions"
	   (is (= (pair-off [1 2 3 4] [:a :b :c :d]) [[1 :a] [2 :b] [3 :c] [4 :d]]))
	   (is (= (pair-off [] []) []))
	   (is (= (pair-off [1 2 3] []) []))
	   (is (= (pair-off [] [1 2 3]) []))
	   (is (= (nth pitch-table 10) "A#0"))
	   (is (= (nth pitch-table 0) "C0"))
	   (is (thrown? IndexOutOfBoundsException (nth pitch-table 120)))
	   (is (= (nth harmonic-table 0) "C0"))
	   (is (= (nth harmonic-table 20) "B1"))
	   (is (thrown? IndexOutOfBoundsException (nth harmonic-table 70)))
	   (is (= (pitch-map "C0") 0))
	   (is (= (pitch-map "A#4") 58))
	   (is (= (pitch-map "B9") 119))
	   (is (nil? (pitch-map "C10"))))
  (testing "Common - mixing functions"
	   (let [voice1 (struct voice "Piano" [(struct note 2 "A0" 3) (struct note 4 "B3" 2) (struct note 1 "C4" 1)])
		 voice2 (struct voice "Piano" [(struct note 0 "A1" 1)])
		 voice3 (struct voice "Guitar" [])]
	     (is (= (voice-length voice1) 7))
	     (is (= (voice-length voice2) 0))
	     (is (= (voice-length voice3) 0))
	     (is (= (resize-voice voice1 10) 
		    (struct voice "Piano" [(struct note 2 "A0" 3) (struct note 4 "B3" 2) (struct note 1 "C4" 1) (struct note 2 "A0" 3) (struct note 1 "B3" 2)])))
	     (is (= (resize-voice voice1 5)
		    (struct voice "Piano" [(struct note 2 "A0" 3) (struct note 3 "B3" 2)])))
	     (is (= (resize-voice voice3 1) 
		    (struct voice "Guitar" [(struct note 1 "R" 1)])))
	     (is (= (resize-voice voice2 0) 
		    (struct voice "Piano" [])))
	     (is (= (longest [voice1 voice2 voice3]) voice1))
	     (is (= (longest [voice1 (resize-voice voice2 11) voice3]) 
		    (resize-voice voice2 11))))))

(defn -main
  []
  (run-tests 'org.musicbox.test.composer-test))