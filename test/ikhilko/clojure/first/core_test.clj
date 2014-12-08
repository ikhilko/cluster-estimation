(ns ikhilko.clojure.first.core-test
  (:require [clojure.test :refer :all]
            [ikhilko.clojure.first.core :refer :all :as first]))

(deftest test-check-argumets
  (testing "Function 'check-argumets'"
    (testing "thrown Exception on blank 'distance-type'"
      (is (thrown-with-msg? IllegalArgumentException #"Not blank \"distance-type\" must be provided as first argument"
                            (#'first/check-argumets "   " "./samples/irises.txt"))))
    (testing "thrown Exception on blank 'filename'"
      (is (thrown-with-msg? IllegalArgumentException #"Not blank \"filename\" must be provided as second argument"
                            (#'first/check-argumets "euclidean" "   "))))
    (testing "thrown Exception on unknown 'distance-type'"
      (is (thrown-with-msg? IllegalArgumentException #"Unknown distance type: \"euclidaaa\""
                            (#'first/check-argumets "euclidaaa" "./samples/irises.txt"))))
    (testing "thrown Exception when file doesn't exist"
      (is (thrown-with-msg? IllegalArgumentException #"File with name ./bad-file-name doesn't exist!"
                            (#'first/check-argumets "euclidean" "./bad-file-name"))))))

(deftest test-file-exist?
  (testing "Function 'file-exist?'"
    (testing "returns true if file exist"
      (is (true? (#'first/file-exist? "./samples/irises.txt"))))
    (testing "returns false if file doen't exist"
      (is (false? (#'first/file-exist? "./bad-file-name"))))))

(deftest test-file->points
  (testing "Function 'file->points'"
    (testing "returns correct value"
      (let [filename "./test/ikhilko/clojure/first/fixtures/mock.txt"]
        (is (= (#'first/file->points filename)
               [{:index 1, :vals [1.0 5.0]} {:index 2, :vals [3.0 4.0]}]))))
    (testing "returns correct value even file contains empty lines"
      (let [filename "./test/ikhilko/clojure/first/fixtures/mock-with-empty-lines.txt"]
        (is (= (#'first/file->points filename)
               [{:index 2, :vals [1.0 5.0]} {:index 4, :vals [3.0 4.0]}]))))))

(deftest test-distance-funcs
  (testing "Distance function"
    (testing "'euclidean-distance' returns correct value"
      (is (= (#'first/euclidean-distance [6 23.1] [3 19.1])
             25.0)))
    (testing "'hamming-distance' returns correct value"
      (is (= (#'first/hamming-distance [3 5 9 1 7] [2 5 3 1 8.5])
             3)))))

(deftest test-calculation-points-potentials
  (testing "Correct calculated and setted :potential value in function"
    (testing "'set-point-potential'"
      (let [distance-func #'first/hamming-distance
            point {:index 2, :vals [3.0 4.0]}
            points [{:index 1, :vals [1.0 5.0]} point]]
        (is (= (-> (#'first/set-point-potential distance-func points point) :potential)
               1.4111122905071873))))
    (testing "'revise-point-potential'"
      (let [distance-func #'first/hamming-distance
            kernel {:index 2, :vals [3.0 4.0]}
            point {:index 1, :vals [1.0 5.0]}]
        (is (= (-> (#'first/set-point-potential distance-func kernel point) :potential)
               0.8222245810143749))))))


(deftest test-find-point
  (testing "Correct result when find"
    (testing "max potentian if function 'find-max-potential-point'"
      (let [points [{:index 1, :vals [1.0 5.0], :potential 1.4111122905071873}
                    {:index 2, :vals [3.0 4.0], :potential 1.4111522905071873}
                    {:index 3, :vals [3.0 4.0], :potential 0.8222245810143749}]]
        (is (= (-> (#'first/find-max-potential-point points) :potential)
               1.4111522905071873))))
    (testing "shortest distance if function 'find-shortest-distance'"
      (let [distance-func #'first/euclidean-distance
            kernel {:index 3, :vals [2.4 4.8], :potential 2.9222245810146749}
            kernels [{:index 1, :vals [1.0 5.0], :potential 1.4111122905071873}
                     {:index 2, :vals [3.0 4.0], :potential 1.4111522905071873}
                     {:index 3, :vals [3.0 4.0], :potential 0.8222245810143749}]]
        (is (= (#'first/find-shortest-distance distance-func kernel kernels)
               0.9999999999999998))))))