(ns ikhilko.clojure.first.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

; TODO marco from intenet, remove it later
(defmacro dbg [x] `(let [x# ~x] (println "dbg:" '~x "=" x#) x#))

; define algorithm constants
(def ^:private r-a 3)
(def ^:private alpha (/ 4 (Math/pow r-a 2)))
(def ^:private r-b (* 1.5 r-a))
(def ^:private betta (/ 4 (Math/pow r-b 2)))
(def ^:private distance-types #{"euclidean" "hamming"})
(def ^:private e_max 0.5)
(def ^:private e_min 0.15)

(defn- calc-exponent
  [param distance]
  (Math/exp (- (* param distance))))

(defn- set-point-potential
  [distance-func points point]
  (->> (pmap #(distance-func (:vals point) (:vals %)) points)
       (pmap (partial calc-exponent alpha))
       (reduce + 0)
       (assoc point :potential)))

(defn- set-points-potentials
  [distance-func points]
  (pmap (partial set-point-potential distance-func points) points))

(defn- revise-point-potential
  [distance-func kernel point]
  (->> (distance-func (:vals point) (:vals kernel))
       (calc-exponent betta)
       (* (:potential kernel))
       (- (:potential point))
       (assoc point :potential)))

(defn- revise-points-potentials
  [distance-func kernel points]
  (pmap (partial revise-point-potential distance-func kernel) points))

(defn- find-max-potential-point
  [points]
  (->> (sort-by :potential > points)
       (first)))

(defn- find-shortest-distance
  [distance-func kernel kernels]
  (->> (pmap #(distance-func (:vals kernel) (:vals %)) kernels)
       (apply min)))

; entry point of Cluster Estimation algorithm
(defn- clusterize
  ([distance-func points]                                   ; init recursion call
    (let [potentials (->> (set-points-potentials distance-func points)
                          (sort-by :potential >))
          first-kernel (first potentials)]
      (clusterize distance-func
                  potentials
                  first-kernel
                  [first-kernel])))
  ([distance-func points first-kernel kernels]
    (println "Kernels: \n\n\n" kernels)
    (let [revised-points (revise-points-potentials distance-func (last kernels) points)
          next-kernel (find-max-potential-point revised-points)
          first-potential (:potential next-kernel)
          next-potential (:potential first-kernel)]
      (if (> next-potential (* first-potential e_max))
        (recur distance-func
               revised-points
               first-kernel
               (conj kernels next-kernel))
        (if (< next-potential (* first-potential e_min))
          kernels
          (if (>= (->> (map /
                            [(find-shortest-distance distance-func next-kernel kernels) next-potential]
                            [r-a first-potential])
                       (reduce +)), 1)
            (recur distance-func
                   revised-points
                   first-kernel
                   (conj kernels next-kernel))
            (recur distance-func
                   (conj (rest revised-points) (assoc next-kernel :potential 0))
                   first-kernel
                   (conj kernels (find-max-potential-point revised-points)))))))))

(defn- euclidean-distance
  [point1 point2]
  (->> (map (comp (fn [x] (* x x)) -) point1 point2)
       (reduce + 0)))

(defn- hamming-distance
  [point1 point2]
  (->> (map not= point1 point2)
       (filter true?)
       (count)))

(defn- get-distance-func
  [distance-type]
  (case distance-type "euclidean" euclidean-distance
                      "hamming" hamming-distance))

; string to hash-map (excluded last value in line)
(defn- line->point
  [line]
  (->> (str/split line #",")
       (pmap #(Double/parseDouble (str/trim %)))
       (butlast)
       (vec)
       (hash-map :vals)))

; read file to list of hash-maps
(defn- file->points
  [filename]
  (->> (io/reader filename)
       (line-seq)
       (pmap line->point)))

; check, is file exist?
(defn- file-exist? [filename]
  (-> filename io/file (.exists)))

; helper throwing Error with "message" if condition falsy
(defn- if-false-throw-error
  [condition message]
  (if-not condition (throw (IllegalArgumentException. message)))
  condition)

; check arguments and throw exception if some check doen't valid
(defn- check-argumets
  [distance-type filename]
  (dorun
    (map #(apply if-false-throw-error %)
         [[(some? distance-type) "Distance-type must be provided as first argument"]
          [(some? filename) "Distance-type must be provided as second argument"]
          [(contains? distance-types distance-type) (str "Unknown distance type: \"" distance-type "\", please use "
                                                         (->> distance-types (pmap #(str "\"" % "\"")) (str/join " or ")))]
          [(file-exist? filename) (str "File with name " filename " doesn't exist!")]])))

; entry point
(defn -main
  [distance-type filename]
  (try
    (do
      (check-argumets distance-type filename)               ; if invalid will throw an IllegalArgumentException that catched below
      (println "Distance type: " distance-type)
      (println "Source file: " filename)
      (let [points (file->points filename)
            distance-func (get-distance-func distance-type)]
        (clusterize distance-func points)))
    ;(catch IllegalArgumentException e (->> e (.getMessage) (println "Invalid argument: ")))
    )
  (shutdown-agents))

;(->> (file->points filename)
;           (map println)
;           (dorun))

;(-main "euclidean" "./samples/glass.txt")