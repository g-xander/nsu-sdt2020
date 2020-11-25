(ns lab4.atoms-test
  (:require [clojure.test :refer :all]
            [lab4.atoms :as atoms]))

(deftest variable-test
  (testing "variable"
    (let [foo (atoms/variable :foo)
          bar (atoms/variable :bar)]
      
      (is (atoms/variable? foo))
      (is (not (atoms/variable? "iamvar")))
      (is (= (atoms/variable-name foo) :foo))
      (is (not (atoms/same-variable? foo bar))))))

(deftest constant-test
  (testing "constant"
    (let [foo (atoms/constant true)
          bar (atoms/constant false)]

      (is (atoms/constant? foo))
      (is (not (atoms/constant? "iamconstant")))
      (is (= (atoms/constant-value bar) false)))))
