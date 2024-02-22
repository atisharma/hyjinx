;; data structure visualization

(require hyjinx [defmethod])

(import curses)
(import hyrule [inc dec])
(import hyjinx.screen [Screen])


(defmethod vis [#^ dict d])
  

(defmethod vis [#^ (| list set) l])
  

(defmethod vis [x])
