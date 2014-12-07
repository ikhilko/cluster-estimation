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

(defn- read-file-lines
  [filename]
  (println "Opening file: " filename))

; check, is file exist?
(defn- file-exist? [filename]
  (-> filename io/file (.exists)))

; helper throwing Error with "message" if condition falsy
(defn- if-false-throw-error
  [condition message]
  (if-not condition (throw (Exception. message)))
  condition)

; check arguments and throw exception if some check doen't valid
(defn- arguments-valid?
  [distance-type filename]
  (every? true?
          (map #(apply if-false-throw-error %)
               [[(some? distance-type) "Distance-type must be provided as first argument"]
                [(some? filename) "Distance-type must be provided as second argument"]
                [(contains? distance-types distance-type) (str "Unknown distance type: \"" distance-type "\", please use "
                                                               (->> distance-types (map #(str "\"" % "\"")) (str/join " or ")))]
                [(file-exist? filename) (str "File with name " filename " doesn't exist!")]])))

(defn -main
  [distance-type filename]
  {:pre [(arguments-valid? distance-type filename)]}
  (println "Distance type: " distance-type)
  (println "Source file: " filename)
  (read-file-lines filename))

;(-main "euclidean" "./samples/glass.txt")