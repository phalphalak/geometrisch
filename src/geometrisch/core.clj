(ns geometrisch.core)

(defprotocol TRANSFORMABLE
  (transform [this transformation]))

(defprotocol VECTOR
  (cross-product-length
    [this other-vector]
    [v1 v2 v3])
  (relation
    [v1 v2]
    [v1 v2 v3])
  (colinear?
    [v1 v2]
    [v1 v2 v3])
  (left?
    [v1 v2]
    [v1 v2 v3])
  (right?
    [v1 v2]
    [v1 v2 v3])
  (add [this other])
  (negative [this])
  (subtract [this other])
  (triangle-area [v1 v2 v3]))

(defprotocol AREA
  (area [this]))

(defprotocol GRAPH
  (vertices [this])
  (vertex [index])
  (edges [this])
  (outbound-edges [this index])
  (inbound-edges [this index]))

(defrecord Point [x y]
  VECTOR
  (cross-product-length [_ [x2 y2]]
    (- (* x y2)
       (* x2 y)))
  (cross-product-length [this v2 v3]
    (cross-product-length (subtract v2 this)
                          (subtract v3 this)))
  (relation [this other]
    (let [cpl (cross-product-length this other)]
      (case (int (Math/signum (float cpl)))
        0 :colinear
        1 :left
        -1 :right)))
  (relation [this v2 v3]
    (relation (subtract v2 this)
              (subtract v3 this)))
  (colinear? [this other]
    (= :colinear (relation this other)))
  (colinear? [v1 v2 v3]
    (colinear? (subtract v2 v1)
               (subtract v3 v1)))
  (left? [this other]
    (= :left (relation this other)))
  (left? [v1 v2 v3]
    (left? (subtract v2 v1)
           (subtract v3 v1)))
  (right? [this other]
    (= :right (relation this other)))
  (right? [v1 v2 v3]
    (right? (subtract v2 v1)
            (subtract v3 v1)))
  (negative [_]
    (->Point (- x) (- y)))
  (add [this [x2 y2]]
    (->Point (+ x x2)
             (+ y y2)))
  (subtract [this other]
    (add this (negative other)))
  (triangle-area [v1 v2 v3]
    (/ (cross-product-length v1 v2 v3) 2))
  clojure.lang.Indexed
  (nth [_ i] (case i 0 x 1 y
                   (throw (IndexOutOfBoundsException.))))
  (nth [_ i default]
    (case i 0 x 1 y
          default))
  TRANSFORMABLE)

(defn- mod-nth [col i]
  (nth col (mod i (count col))))

(defrecord Polygon [points holes]
  AREA
  (area [this]
    (let [fixed (mod-nth points 0)]
      (reduce (fn [acc index]
                (+ acc (triangle-area fixed
                                      (mod-nth points index)
                                      (mod-nth points (inc index)))))
              0
              (range (count points)))))
  GRAPH
  (vertices [_] points)
  (vertex [index]
    (get points index))
  (edges [_]
    (let [size (count points)]
      (for [i size]
        [(vertex i) (vertex (mod (inc i) size))])))
  TRANSFORMABLE)

(defn point
  ([o] (cond
        (satisfies? VECTOR o) o
        (and (sequential? o)
             (= 2 (count o))) (->Point (first o)
                                       (last o))
        :else (assert false "invalid point format")))
  ([x y] (->Point x y)))

(defn polygon
  ([points] (polygon points nil))
  ([points holes]
     (->Polygon (map point points) holes)))
