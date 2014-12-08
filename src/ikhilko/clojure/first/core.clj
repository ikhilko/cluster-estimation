(ns ikhilko.clojure.first.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

; define algorithm constants
(def ^:private r-a 3)
(def ^:private alpha (/ 4 (Math/pow r-a 2)))
(def ^:private r-b (* 1.5 r-a))
(def ^:private betta (/ 4 (Math/pow r-b 2)))
(def ^:private distance-types #{"euclidean" "hamming"})
(def ^:private e_max 0.5)
(def ^:private e_min 0.15)

; helper, like regular reduce but add index param
(defn reduce-indexed
  "Reduce while adding an index as the second argument to the function"
  ([f coll]
    (reduce-indexed f (first coll) 0 (rest coll)))

  ([f init coll]
    (reduce-indexed f init 0 coll))

  ([f init i coll]
    (if (empty? coll)
      init
      (let [v (first coll)
            fv (f init i v)]
        (recur f fv (inc i) (rest coll))))))

(defn- calc-exponent
  [param distance]
  (Math/exp (- (* param distance))))

(defn- set-point-potential
  [distance-func points point]
  (->> (reduce
         (fn [memo another-point]
           (+ memo (->> (distance-func (:vals point)
                                       (:vals another-point))
                        (calc-exponent alpha))))
         0 points)
       (assoc point :potential)))

(defn- set-points-potentials
  [distance-func points]
  (map (partial set-point-potential distance-func points) points))

(defn- revise-point-potential
  [distance-func kernel point]
  (->> (distance-func (:vals point) (:vals kernel))
       (calc-exponent betta)
       (* (:potential kernel))
       (- (:potential point))
       (assoc point :potential)))

(defn- revise-points-potentials
  [distance-func kernel points]
  (->> (map (partial revise-point-potential distance-func kernel) points)
       (sort-by :potential >)))

(defn- find-max-potential-point
  [points]
  (->> (sort-by :potential > points)
       (first)))

(defn- find-shortest-distance
  [distance-func kernel kernels]
  (reduce
    (fn [memo another-kernel]
      (min memo (distance-func (:vals kernel)
                               (:vals another-kernel))))
    Double/POSITIVE_INFINITY kernels))

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
    (let [revised-points (revise-points-potentials distance-func ((comp first reverse) kernels) points)
          next-kernel (first revised-points)
          first-potential (:potential first-kernel)
          next-potential (:potential next-kernel)]
      (if (> next-potential (* first-potential e_max))
        (recur distance-func
               revised-points
               first-kernel
               (conj kernels next-kernel))
        (if (< next-potential (* first-potential e_min))
          kernels
          (let [shortest-distance (find-shortest-distance distance-func next-kernel kernels)]
            (if (<= 1 (+ (/ shortest-distance r-a) (/ next-potential first-potential)))
              (recur distance-func
                     revised-points
                     first-kernel
                     (conj kernels next-kernel))
              (let [revised-points (rest revised-points)]
                (recur distance-func
                       (conj revised-points (assoc next-kernel :potential 0))
                       first-kernel
                       (conj kernels (find-max-potential-point revised-points)))))))))))

(defn- sqr [x] (* x x))

(defn- euclidean-distance
  [point1 point2]
  (reduce-indexed (fn [memo i value]
                    (+ memo (sqr (- value (get point2 i)))))
                  0 point1))

(defn- hamming-distance
  [point1 point2]
  (reduce-indexed (fn [memo i value]
                    (if (not= value (get point2 i))
                      (inc memo)
                      memo))
                  0 point1))

(defn- get-distance-func
  [distance-type]
  (case distance-type "euclidean" euclidean-distance
                      "hamming" hamming-distance))

; string to hash-map (excluded last value in line)
(defn- line->point
  [i line]
  (->> (str/split line #",")
       (butlast)
       (reduce #(conj %1 (Double/parseDouble (str/trim %2))) [])
       (hash-map :index (inc i) :vals)))

; read file to list of hash-maps
(defn- file->points
  [filename]
  (->> (io/reader filename)
       (line-seq)
       (reduce-indexed #(if (not (str/blank? %3)) (conj %1 (line->point %2 %3)) %1) [])))

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
         [[(not (str/blank? distance-type)) "Not blank \"distance-type\" must be provided as first argument"]
          [(not (str/blank? filename)) "Not blank \"filename\" must be provided as second argument"]
          [(contains? distance-types distance-type) (str "Unknown distance type: \"" distance-type "\", please use "
                                                         (->> distance-types (map #(str "\"" % "\"")) (str/join " or ")))]
          [(file-exist? filename) (str "File with name " filename " doesn't exist!")]])))

; entry point
(defn -main
  [distance-type filename]
  (try
    (do (check-argumets distance-type filename)             ; if invalid will throw an IllegalArgumentException that catched below
        (println "Distance type:" distance-type)
        (println "Source file:" filename)
        (let [points (file->points filename)
              distance-func (get-distance-func distance-type)
              kernels (clusterize distance-func points)]
          (println "Kernels finded: " (count kernels))
          (->> kernels
               (map #(printf "line: %3d, potential: %.4f, data: %s \n" (:index %) (:potential %) (:vals %)))
               (dorun))))
    (catch IllegalArgumentException e (->> e (.getMessage) (println "Invalid argument:")))
    (finally (shutdown-agents))))