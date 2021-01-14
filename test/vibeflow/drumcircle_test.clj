(ns vibeflow.drumcircle-test
  (:require [vibeflow.drumcircle :as drumcircle]
            [clojure.test :refer [deftest testing is are use-fixtures run-tests join-fixtures]]))

(def timing {:bpm 120
             :sig-count 4
             :sig-type 4
             :frame-rate 44100
             :ticks-per-beat 1920})

(deftest sample-at-bbt-test
  (is (= (drumcircle/sample-at-bbt timing 1 1 0) 0))
  (is (= (drumcircle/sample-at-bbt timing 1 2 0) 22050))
  (is (= (drumcircle/sample-at-bbt timing 1 3 0) 44100))
  (is (= (drumcircle/sample-at-bbt timing 2 1 0) 88200))
  (is (= (drumcircle/sample-at-bbt timing 1 1 240) 11025/4)))

(drumcircle/bbt-at-sample timing 22050)
[1 2 0]

(drumcircle/bbt-at-sample timing 11025/4)
[1 1 240]
