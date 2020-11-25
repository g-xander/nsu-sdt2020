(ns lab4.utils-test
  (:require [clojure.test :refer :all]
            [lab4.atoms :as atoms]
            [lab4.utils :as utils]
            [lab4.operations :as operations]))

(deftest constant-or-variable
  (testing "constant or variable"
    (let [foo (atoms/variable :foo)
          bar (operations/negation foo)]
      
      (is (utils/atom? foo))
      (is (not (utils/atom? bar))))))

(deftest primitive
  (testing "primitive"
    (let [foo (atoms/variable :foo)
          bar (operations/negation foo)
          baz (operations/conjunction (atoms/constant true) foo)]

      (is (utils/primitive? foo))
      (is (utils/primitive? bar))
      (is (not (utils/primitive? baz))))))

(deftest primitive-or-conj-of-primitives?
  (testing "basic conjunction primiteve?"
    (let [foo (atoms/variable :foo)
          bar (operations/negation foo)
          baz (operations/conjunction (atoms/constant true) foo)
          qux (operations/disjunction (atoms/constant true) foo)]

      (is (utils/primitive-or-conj-of-primitives? foo))
      (is (utils/primitive-or-conj-of-primitives? bar))
      (is (not (utils/primitive-or-conj-of-primitives? qux)))
      (is (utils/primitive-or-conj-of-primitives? baz)))))
