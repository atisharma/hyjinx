"Convenience macros."

(defmacro delmacro [m]
  "Delete a macro."
  `(eval-when-compile
     (del (get _hy_macros (hy.mangle m)))))
  
(defmacro .. [a b [step 1]]
  "A realised range."
  `(list (range ~a ~b ~step)))

(defmacro prepend [x l]
  "Prepend x to a list."
  `(+ [~x] ~l))

(defmacro append [x l]
  "Append x to a list."
  `(+ ~l [~x]))

(defmacro defmethod [f #* body]
  "Define a multimethod.
`(defmethod f [#^ int x #^ float y] (// x (int y)))`
will compile to
`@multimethod
 def f(x: int, y: float):
     return(x // int(y))`
You have to import multimethod where it is used:
`(import multimethod [multimethod])`"
  `(defn [multimethod] ~f
     ~@body))

(defmacro help-macro [m]
  "Get help for a macro."
  `(help (get-macro ~m)))
