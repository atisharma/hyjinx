"
This module, hyjinx.macros, provides a collection of convenient
macros for various use cases in Hy programming.

Macros about Macros

- `delmacro`: Delete or unregister a macro.
- `help-macro`: Retrieve help or documentation for a macro.

Macros for sequences

- `rest`: Return a slice of all but the first element in a sequence.
- `butlast`: Return a slice of all but the last element in a sequence.
- `..`: Generate a realised range for a sequence.
- `prepend`: Prepend an element to a sequence.
- `append`: Append an element to a sequence.

Macros for Functions

- `defmethod`: Dispatch on type using multimethod.multimethod.
- `defproperty`: Define a class method using the property decorator.

Macros for Data Structures

- `defstruct`: Define a basic immutable dataclass.

Macros for Flow Control

- `do-while`: A C-style do-while loop.

"

(import toolz [first second])


;; * macros about macros
;; ----------------------------------------------------

(defmacro delmacro [macro]
  "Delete / unregister a macro."
  `(eval-when-compile
     (del (get _hy_macros (hy.mangle macro)))))
  
(defmacro help-macro [macro]
  "Get help for a macro.
  Use like (help-macro 'help-macro)."
  `(help (get-macro ~macro)))
  

;; * macros for sequences
;; ----------------------------------------------------

(defmacro rest [xs]
  "A slice of all but the first element of a sequence."
  ;; consider toolz.itertoolz.rest
  `(cut ~xs 1 None))
  
(defmacro butlast [xs]
  "A slice of all but the last element of a sequence."
  `(cut ~xs 0 -1))
  
(defmacro .. [start end [step 1]]
  "A realised (eager) range, [start end) (i.e. excluding right boundary)."
  `(list (range ~start ~end ~step)))

(defmacro prepend [x l]
  "Return a new list with x prepended."
  `(+ [~x] ~l))

(defmacro append [x l]
  "Return a new list with x appended."
  `(+ ~l [~x]))

(defmacro lmap [#* args]
  "Eager map, realised as a list."
  `(list (map ~@args)))


;; * macros for functions
;; ----------------------------------------------------

(defmacro defmethod [#* args]
  "Define a multimethod (using multimethod.multimethod).
  For example, the Hy code

  `(defmethod f [#^ int x #^ float y]
    (// x (int y)))`

  is equivalent to the following Python code:

  `@multimethod
  def f(x: int, y: float):
      return await x // int(y)`

  You can also define an asynchronous multimethod:

  `(defmethod :async f [#* args #** kwargs]
    (await some-async-function #* args #** kwargs))`
  "
  (if (= :async (first args))

    (let [f (second args)
          body (cut args 2 None)]
      `(defn :async [hy.I.multimethod.multimethod] ~f
         ~@body))

    (let [f (first args)
          body (cut args 1 None)]
      `(defn [hy.I.multimethod.multimethod] ~f
         ~@body))))

(defmacro defproperty [f #* body]
  "Class method definition using the property decorator.

  `(defproperty p 2)`

  is equivalent to the following Python code:

  `@property
  def p(self):
    2`
  "
  `(defn [property] ~f [self] ~@body))
  

;; * macros for pytest
;; ----------------------------------------------------

(defmacro fixture [f #* body]
  "Function definition using pytest fixture decorator.

  `(fixture p 2)`

  is equivalent to the following Python code:

  `@pytest.fixture
  def p():
    2`
  "
  `(defn [hy.I.pytest.fixture] ~f [] ~@body))
  
(defmacro def-numeric-test [test-name f x ans]
  "Return a function that makes a numeric comparison (using
  `numpy.isclose`) of function f applied to x to ans, where the result
  is an ndarray. The result of (f x) should be numerically close to
  ans for all elements. This is convenient for writing tests with
  pytest."
  `(defn ~test-name [~x]
     (assert (hy.I.numpy.isclose (~f ~x) ~ans)))) 


;; * macros for data structures
;; ----------------------------------------------------

(defmacro defstruct [d #* body]
  "Define an immutable dataclass using the dataclass decorator.
  For example, the Hy code

  `(defdataclass D [#^ int x #^ float y])`

  is equivalent to the following Python code:

  `@dataclass
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

(defmacro when-let [bindings #* body]
  "Bind a value to a variable and execute code if the value is truthy.
  For example,

  `(when-let [x (get-dict my-dict :key)]
     (print x))`"
  (let [var (get bindings 0)
        expr (get bindings 1)]
    `(let [~var ~expr]
       (when ~var
         (do ~@body)))))
