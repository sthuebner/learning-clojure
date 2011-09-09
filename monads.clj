(ns monads
  (:use clojure.contrib.monads
        clojure.test
        [clojure.repl :only [source]]))

;;;; following http://onclojure.com/2009/03/05/a-monad-tutorial-for-clojure-programmers-part-1/


;;; first example

(let [a 1
      b (inc a)]
  (* a b))
;; 2

;; … it's the same as:
((fn [a]
   ((fn [b]
      (* a b))
    (inc a))
   )
 1)
;; 2


(defn m-bind-identity [value function]
  (function value))

(defn m-result-identity [value]
  value)

;;; now with 'clojure.contrib.monads/m-bind
(m-bind-identity 1 (fn [a]
                     (m-bind-identity (inc a) (fn [b]
                                                (m-result-identity (* a b))))))

(let [m '(domonad identity-m
                  [a 1
                   b (inc a)]
                  (* a b))]
  (println (eval m))
  (println (macroexpand-1 m)))
;; 2
;; (clojure.contrib.monads/with-monad identity-m (m-bind 1 (fn [a] (m-bind (inc a) (fn [b] (m-result (* a b)))))))
;; nil

;; m-bind actually is a place holder that means different things for different monads.
;; here is the identity-m monad:
(source identity-m)
;; (defmonad identity-m
;;    "Monad describing plain computations. This monad does in fact nothing
;;     at all. It is useful for testing, for combination with monad
;;     transformers, and for code that is parameterized with a monad."
;;   [m-result identity
;;    m-bind   (fn m-result-id [mv f]
;; 	      (f mv))
;;   ])
;; nil


;;; second example

(defn f [x]
  (let [a x
        b (inc a)]
    (* a b)))

;; using the maybe-m monad, only executing the next step, if the previous one didn't eval to nil
(defn f [x]
  (domonad maybe-m
           [a x
            b (inc a)]
           (* a b)))

(f 1)
;; 2

(f nil)
;; nil

(macroexpand-1 '(domonad maybe-m
                         [a x
                          b (inc a)]
                         (* a b)))
;; (clojure.contrib.monads/with-monad maybe-m (m-bind x (fn [a] (m-bind (inc a) (fn [b] (m-result (* a b)))))))

(source maybe-m)
;; (defmonad maybe-m
;;    "Monad describing computations with possible failures. Failure is
;;     represented by nil, any other value is considered valid. As soon as
;;     a step returns nil, the whole computation will yield nil as well."
;;    [m-zero   nil
;;     m-result (fn m-result-maybe [v] v)
;;     m-bind   (fn m-bind-maybe [mv f]
;;                (if (nil? mv) nil (f mv)))    ; this is the "maybe"
;;     m-plus   (fn m-plus-maybe [& mvs]
;; 	       (first (drop-while nil? mvs)))
;;     ])
;; nil




;;;; following http://onclojure.com/2009/03/06/a-monad-tutorial-for-clojure-programmers-part-2/

(for [a (range 5)
      b (range a)]
  (* a b))
;; (0 0 2 0 3 6 0 4 8 12)

;; can be expressed as
(mapcat (fn [a]
          (mapcat (fn [b]
                    (list (* a b)))
                  (range a)))
        (range 5))
;; (0 0 2 0 3 6 0 4 8 12)

;; translated into m-bind and m-result
(defn m-bind-sequence [sequence function]
  (mapcat function sequence))

(defn m-result-sequence [value]
  (list value))

(m-bind-sequence (range 5) (fn [a]
                             (m-bind-sequence (range a) (fn [b]
                                                          (m-result-sequence (* a b))))))
;; (0 0 2 0 3 6 0 4 8 12)

(domonad sequence-m
         [a (range 5)
          b (range a)]
         (* a b))
;; (0 0 2 0 3 6 0 4 8 12)

(source sequence-m)
;; (defmonad sequence-m
;;    "Monad describing multi-valued computations, i.e. computations
;;     that can yield multiple values. Any object implementing the seq
;;     protocol can be used as a monadic value."
;;    [m-result (fn m-result-sequence [v]
;; 	       (list v))
;;     m-bind   (fn m-bind-sequence [mv f]
;;                (flatten* (map f mv)))
;;     m-zero   (list)
;;     m-plus   (fn m-plus-sequence [& mvs]
;;                (flatten* mvs))
;;     ])
;; nil

(macroexpand-1 '(domonad sequence-m
                         [a (range 5)
                          b (range a)]
                         (* a b)))
;; (clojure.contrib.monads/with-monad sequence-m (m-bind (range 5) (fn [a] (m-bind (range a) (fn [b] (m-result (* a b)))))))



;;;; TODO study the Three Laws of Monads



;;;; Operations used with Monads

;;; m-lift
(def nil-respecting-addition
  (with-monad maybe-m
    (m-lift 2 +)))

(is (= (nil-respecting-addition 2 3)
       5))
;; 5

(is (= (nil-respecting-addition 2 nil)
       nil))
;; nil

(is (thrown? NullPointerException (+ 2 nil)))
;; throws NPE

;;; what's happening here?
;(macroexpand-1 '(m-lift 2 +))
;; (clojure.core/fn [mv_4649 mv_4650] (m-bind mv_4649 (fn [x_4651] (m-bind mv_4650 (fn [x_4652] (m-result (+ x_4651 x_4652)))))))

;; m-lift creates a function of N monadic expressions that returns a monadic expression


;; no the same with domonad
(defn nil-respecting-addition
  [x y]
  (domonad maybe-m
          [a x
           b y]
          (+ a b)))

(is (= (nil-respecting-addition 2 3)
       5))
;; true

(is (= (nil-respecting-addition 2 nil)
       nil))
;; true


;;; "Exercice: The following function is equivalent to a well-known built-in Clojure function. Which one?"
(with-monad sequence-m
  (defn mystery
    [f xs]
    ( (m-lift 1 f) xs )))

;; Answer: it's the map function
(is (= (mystery inc [1 2 3])
       [2 3 4]))
;; true



;;;; m-seq operation
(with-monad sequence-m
  (defn ntuples [n xs]
    (m-seq (replicate n xs))))

(is (= (ntuples 2 [1 2 3])
       '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))))
;; true

;; trying to translate m-seq
(defn m-seq* [ms]
  (reduce (fn [q p]
            (m-bind-sequence p (fn [x]
                                 (m-bind-sequence q (fn [y]
                                                      (m-result-sequence (cons x y)))) )))
          (m-result-sequence '())
          (reverse ms)))

(defn ntuples [n xs]
  (m-seq* (replicate n xs)))

(is (= (ntuples 2 [1 2 3])
       '((1 1) (1 2) (1 3) (2 1) (2 2) (2 3) (3 1) (3 2) (3 3))))
;; true



;;;; m-chain operation
;;; example: find the n-th generation ascendants of a class
(with-monad sequence-m
  (defn n-th-generation
    [n cls]
    ( (m-chain (replicate n parents)) cls )))

(n-th-generation 0 (class []))
;; (clojure.lang.PersistentVector)

(n-th-generation 1 (class []))
;; (clojure.lang.APersistentVector clojure.lang.IObj clojure.lang.IEditableCollection)

(n-th-generation 2 (class []))
;; (java.lang.Iterable clojure.lang.AFn java.lang.Comparable java.io.Serializable java.util.RandomAccess clojure.lang.IPersistentVector java.util.List clojure.lang.IMeta)






;;;; following http://onclojure.com/2009/03/23/a-monad-tutorial-for-clojure-programmers-part-3/


;;; :when clause in 'for
(for [a (range 5)
      :when (odd? a)]
  (* 2 a))
;; (2 6)


;;; monadic
(domonad sequence-m
         [a (range 5)
          :when (odd? a)]
         (* 2 a))
;; (2 6)
