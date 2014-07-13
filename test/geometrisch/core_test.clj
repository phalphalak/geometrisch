(ns geometrisch.core-test
  (:require [clojure.test :refer :all]
            [geometrisch.core :refer :all]))

(deftest point-test
  (testing "triangle-area"
    (is (== 2.5 (triangle-area (point 1 1)
                               (point 3 2)
                               (point 2 4))))))

(deftest polygon-test
  (testing "area"
    (is (= 5 (area (polygon [[1 1]
                             [3 2]
                             [2 4]
                             [0 3]]))))))
