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

; string to hash-map (excluded last value in line)
(defn- line->point
  [line]
  (->> (str/split line #",")
       (pmap str/trim)
       (butlast)
       (hash-map :values)))

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
  (every? true?
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
      (->> (file->points filename)
           (map println)
           (doall)))
    (catch IllegalArgumentException e (->> e (.getMessage) (println "Invalid argument: "))))
  (shutdown-agents))

;(-main "euclidean" "./samples/glass.txt")