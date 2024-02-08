
(defmacro delmacro [m]
  "Delete a macro."
  `(eval-when-compile
     (del (get _hy_macros (hy.mangle m)))))

(defmacro .. [a b [step 1]]
  `(list (range ~a ~b :step ~step)))

(defmacro prepend [x l]
  "Prepend x to a list."
  `(+ [~x] ~l))

(defmacro append [x l]
  "Append x to a list."
  `(+ ~l [~x]))
