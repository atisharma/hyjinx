"Convenience macros."

;; * macros about macros
;; ----------------------------------------------------

(defmacro delmacro [m]
  "Delete a macro."
  `(eval-when-compile
     (del (get _hy_macros (hy.mangle m)))))
  
(defmacro help-macro [m]
  "Get help for a macro.
  Use like (help-macro 'help-macro)."
  `(help (get-macro ~m)))
  
;; * macros for sequences
;; ----------------------------------------------------

(defmacro rest [xs]
  "A slice of all but the last of xs."
  `(cut ~xs 1 None))
  
(defmacro .. [a b [step 1]]
  "A realised (eager) range, a to b inclusive."
  `(list (range ~a ~b ~step)))

(defmacro prepend [x l]
  "Return a new list with x prepended."
  `(+ [~x] ~l))

(defmacro append [x l]
  "Return a new list with x appended."
  `(+ ~l [~x]))

;; * macros for functions
;; ----------------------------------------------------

(defmacro defmethod [f #* body]
  "Define a multimethod (using multimethod.multimethod).
  For example, the Hy code

  (defmethod f [#^ int x #^ float y]
    (// x (int y)))

  will compile to the following python code:

  @multimethod
  def f(x: int, y: float):
      return(x // int(y))`
  "
  `(defn [hy.I.multimethod.multimethod] ~f
     ~@body))

;; * macros for data structures
;; ----------------------------------------------------

(defmacro defstruct [d #* body]
  "Define a basic immutable dataclass.
  For example, the Hy code

  (defdataclass D [#^ int x #^ float y]

  will compile to

  @dataclass
  class D:
      x: int
      y: float`
  "
  `(defclass [hy.I.dataclass.dataclass] ~d []
     ~@body))

;; * macros for flow control
;; ----------------------------------------------------

(defmacro do-while [condition #* body]
  "C-style do-while loop, which executes the body
  while condition is true, but testing at the end."
  `(do
    ~@body
    (while ~condition
      ~@body)))
