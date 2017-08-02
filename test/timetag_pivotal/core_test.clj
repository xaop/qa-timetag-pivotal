(ns timetag-pivotal.core-test
  (:require [clojure.test :refer :all]
            [timetag-pivotal.core :refer :all]))

(def entry-no-pivotal {entry-key "foobar sldkghdsklh [ slkghsl ] sdkghs dskgl"})
(def entry-pivotal-1 {entry-key "ldkshg [ ] ldskghds [[ ]] kdslg [123]"})
(def entry-pivotal-2 {entry-key "dslkhgds [123,456,789]"})
(def entry-pivotal-3 {entry-key "dslghdslh [123, 456  789 01234]"})

(deftest entry-parsing
  (is (empty? (entry-get-pivotals entry-no-pivotal)))
  (is (= (set (entry-get-pivotals entry-pivotal-1)) #{"123"}))
  (is (= (set (entry-get-pivotals entry-pivotal-2)) #{"123" "456" "789"}))
  (is (= (set (entry-get-pivotals entry-pivotal-3)) #{"123" "456" "789" "01234"})))
