"
Convenience things for ndarrays, matrices and numerical data.
"

(require hyrule [unless ncut
                 -> ->> as->])

(import functools [reduce]
        cytoolz [last])

(import numpy)
(import random [randint])
(import shutil)
(import operator)


;; * Matrices
;; ----------------------------------------------------

(defn ppa [a [precision 3] [digits None] [thou-sep ","] [suppress-small True] [formatter None]]
  "Pretty-print a numpy ndarray."
  (let [digits (+ 4
                  (or digits (int (numpy.log10 (numpy.max (abs a))))))
        formatter (or formatter {"float_kind" (fn [x] f" {x :=+{(+ precision digits)}_.{precision}f} ")
                                 "int_kind" (fn [x] f" {x :=+{digits}_d} ")
                                 "complex_kind" (fn [x] f" {x :=+{(+ precision digits)}_.{precision}f} ")})
        desc f" {(.join "×" (map str a.shape))}  ({(prod a.shape)})  {a.ndim}D  {a.dtype}  {a.nbytes}B"
        ts (shutil.get-terminal-size)
        width (- ts.columns 8)
        s (numpy.array2string a :formatter formatter
                                :suppress-small suppress-small
                                :separator "  "
                                :max-line-width width)
        s-width (len (last (.split s "\n")))]
    (print)
    (print desc)
    (print "╭──\n│" :end "")
    (-> s
        (.replace "_" thou-sep)
        (.replace "[" "")
        (.replace "]" "")
        (print :end "│\n"))
    (print f"{"──╯" :>{(- s-width 2)}}")))

;; * Numeric
;; ----------------------------------------------------

(defn dice [n]
  "True 1/n of the time."
  (not (randint 0 (- n 1))))

(defn prod [l]
  (reduce operator.mul l))
