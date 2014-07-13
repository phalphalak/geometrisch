(ns geometrisch.core-test
  (:require [clojure.test :refer :all]
            [geometrisch.core :refer :all]))

(def ^:priate p1v1 (point 1 1))
(def ^:priate p1v2 (point 3 2))
(def ^:priate p1v3 (point 2 4))
(def ^:priate p1v4 (point 0 3))

(def ^:private polygon1 (polygon [[1 1]
                                  [3 2]
                                  [2 4]
                                  [0 3]]))

(deftest point-test
  (testing "triangle-area"
    (is (== 2.5 (triangle-area (point 1 1)
                               (point 3 2)
                               (point 2 4))))))

(deftest polygon-test
  (testing "vertices"
    (is (= [p1v1 p1v2 p1v3 p1v4]
           (vertices polygon1))))
  (testing "vertex"
    (is (= p1v1 (vertex polygon1 0)))
    (is (= p1v2 (vertex polygon1 1)))
    (is (= p1v3 (vertex polygon1 2)))
    (is (= p1v4 (vertex polygon1 3))))
  (testing "edges"
    (is (= [[p1v1 p1v2]
            [p1v2 p1v3]
            [p1v3 p1v4]
            [p1v4 p1v1]] (edges polygon1))))
  (testing "area"
    (is (= 5 (area polygon1)))))
