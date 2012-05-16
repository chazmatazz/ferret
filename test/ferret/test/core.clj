
(ns ferret.test.core
  (:use [ferret.core] :reload)
  (:use [clojure.test]
        [clojure.java.shell]))

(defn compile-run-solution []
  (with-sh-dir "solution/"
    (sh "g++" "solution.cpp")
    (let [r (sh "./a.out")]
      (sh "rm" "a.out")
      r)))

(deftest processing-test
  (is (seq? (vector->list [1 2 [2 [5 4] 3]])))
  (is (= (symbol-conversion '(make-adder 2)) '(make_adder 2)))
  (is (= (symbol-conversion '(make-adder* 2)) '(make_adder_star_ 2)))

  (let [form (closure-conversion '((def make-adder (fn [n] (fn [x] (+ x n))))))]
    (is (= (ffirst form) 'define-lambda))
    (is (= (last (first form)) '(+ x n)))
    (is (= (second (last form)) 'make-adder))
    (is (= (first (last form)) 'def))))

(deftest arithmetic-test
  (is (= "0 1 10 10.000000 -1 0 0.000000 1 8 8.000000 1 0 1 1.000000 "
         (do (compile->cpp '((print (+ )
                                    (+ 1)
                                    (+ 1 2 3 4)
                                    (+ 1 2.0 3 4)

                                    (- 1)
                                    (- 4 2 2)
                                    (- 4 2 2.0)

                                    (* )
                                    (* 2 2 2)
                                    (* 2.0 2 2)

                                    (/ 1)
                                    (/ 2)
                                    (/ 4 2 2)
                                    (/ 4 2 2.0))))
             (:out (compile-run-solution)))))

  (is (= "true true false false true true false "
         (do (compile->cpp '((print (pos? 1)
                                    (pos? 0.2)
                                    (pos? 0)
                                    (neg? 1)
                                    (neg? -1)
                                    (zero? 0)
                                    (zero? 10))))
             (:out (compile-run-solution))))))

(deftest comparison-test
  (is (= "true true false true false true true true false true true false true false true true "
         (do (compile->cpp '((print (< 2)
                                    (< 2 3 4 5)
                                    (< 2 3 6 5)
                                    (> 2)
                                    (> 2 3 4 5)
                                    (> 6 5 4 3)
                                    (>= 2)
                                    (>= 5 4 3 2 2 2)
                                    (>= 5 1 3 2 2 2)
                                    (<= 2)
                                    (<= 2 2 3 4 5)
                                    (<= 2 2 1 3 4)
                                    (= 2)
                                    (= 2 3)
                                    (= 2 2 2 2)
                                    (= 2 2.0 2))))
             (:out (compile-run-solution)))))

  (is (= "false true false true false false "
         (do (compile->cpp '((print (= 2 2 2 2 3 5)
                                    (= (list 1 2) (list 1 2))
                                    (= (list 1 2) (list 1 3))
                                    (= true true)
                                    (not (= true true))
                                    (not 1))))
             (:out (compile-run-solution))))))


(deftest macro-test
  (is (= "1 1 1 true false true true true 0 1 2 3 4 "
         (do (compile->cpp '((defmacro my-when [test & body]
                               (list 'if test (cons 'do body)))

                             (print (my-when (< 2 3) 1)

                                    (when (< 2 3) 1)

                                    (when (< 2 3) 1)

                                    (let [a 1]
                                      (and (> a 0)
                                           (< a 10)))

                                    (let [a 11]
                                      (and (> a 0)
                                           (< a 10)))

                                    (and true true)

                                    (or true false)

                                    (let [a 11]
                                      (or (> a 0)
                                          (< a 10))))

                             (dotimes [i 5] (print i))))
             (:out (compile-run-solution))))))

(deftest runtime-test
  (is (= "( 1 2 3 4 ) 1 ( 2 3 4 ) ( 3 4 ) ( 3 3 4 ) 3 4 ( 4 3 2 1 1 2 ) ( 4 3 2 1 ) 21 21 "
         (do (compile->cpp '((print (list 1 2 3 4)
                                    (first (list 1 2 3 4))
                                    (rest (list 1 2 3 4))
                                    (rest (rest (list 1 2 3 4)))
                                    (cons 3 (rest (rest (list 1 2 3 4))))
                                    (first (cons 3 (rest (rest (list 1 2 3 4)))))
                                    (count (list 1 2 3 4))
                                    (conj (list 1 2) 1 2 3 4)
                                    (conj nil 1 2 3 4)
                                    (reduce + (list 1 2 3 4 5 6))
                                    (apply + (list 1 2 3 4 5 6)))))
             (:out (compile-run-solution)))))

  (is (= "( 6 5 4 3 2 1 ) ( 6 5 4 3 2 ) ( 4 3 2 1 0 ) c ( H e l l o . ) ( . o l l e H ) "
         (do (compile->cpp '((print (reverse (list 1 2 3 4 5 6))
                                    (reduce (fn [h v]
                                              (conj h (inc v))) (list) (list 1 2 3 4 5))
                                    (reduce (fn [h v]
                                              (conj h (dec v))) (list) (list 1 2 3 4 5))
                                    \c
                                    "Hello."
                                    (reduce (fn [h v]
                                              (conj h v)) (list) "Hello.")
                                    )))
             (:out (compile-run-solution))))))

(deftest special-forms-test
  (is (= "10 89 11 3 1 5 5 1 1 1 1 1 1 1 1 1 1 "
         (do (compile->cpp '((def make-adder
                                  (fn [n] (fn [x] (+ x n))))
                             (def adder
                                  (make-adder 1))

                             (def fibo (fn [n]
                                         (if (< n 2)
                                           1
                                           (+ (fibo (- n 1))
                                              (fibo (- n 2))))))

                             (def adder-let (let [a 1
                                                  b 2]
                                              (fn [n] (+ a b n))))

                             (def adder-let-2 (fn [n]
                                                (let [a 1
                                                      b 2]
                                                  (+ a b n))))

                             (native-declare "int i = 0;")
                             (defn inc-int [] "return i++;")

                             (print (adder 9)

                                    (fibo 10)

                                    ((fn [n] (+ n 1)) 10)

                                    (((fn [n] (fn [n] n)) 3) 3)

                                    (if (< 2 3 4 5 6)
                                      (do 1)
                                      (do 2))

                                    (adder-let 2)

                                    (adder-let-2 2))

                             (while (< (inc-int) 10)
                               (print 1))))
             (:out (compile-run-solution))))))
