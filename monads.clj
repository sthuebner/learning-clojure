(ns monads
  (:use clojure.contrib.monads
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

