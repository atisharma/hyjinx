"A simple curses display class with context manager.
You'll need to start the main using (.wrapper curses ...)."

(require hyrule [unless -> ->>])

(import curses)
(import logging)


(defclass Screen []
  "Manage a display."

  (defn __init__ [self stdscr]
    (setv self.warnings []
          self.errors []
          self.infos []
          self.debugs []
          self.msgs [])
    (.nodelay stdscr True)
    (setv self.stdscr stdscr)
    (.curs_set curses False)
    (.start_color curses)
    ;; if TERMCAP is set wrong in gnu screen, this will fail.
    (try
      (.use_default_colors curses)
      (except [Exception]))

    ;; FIXME: check this is the right colour pairing
    (for [i (range 0 curses.COLORS)]
      (.init-pair curses (+ i 1) i -1))
    (setv self.window (.newwin curses curses.LINES curses.COLS))
    (self.clear))

  (defn __del__ [self]
    (.clear self.stdscr)
    (.curs_set curses True))

  (defn __enter__ [self]
    self)

  (defn __exit__ [self exc-type exc-val exc-tb])

  (defn clear [self]
    ;;(.cbreak curses)
    ;;(.noecho curses)
    (.clear self.stdscr)
    (.refresh self.stdscr)
    (.update_lines_cols curses)
    (.resize self.window curses.LINES curses.COLS)
    (setv self.warnings [])
    (setv self.errors [])
    (setv self.infos [])
    (setv self.debugs []))

  (defn clear_line [self y]
    (try
      (.addstr self.window y 0 " ")
      (.clrtoeol self.window)
      (except [curses.error])))

  (defn refresh [self]
    (.refresh self.window)
    (.erase self.window))

  (defn getkey [self]
    (try
      (.getkey self.stdscr)
      (except [curses.error] None)))

  (defn put [self y x s * [col 0] [style 0]]
    (setv #(h w) (.getmaxyx self.stdscr))
    (when (< y (- h 1))
      (try
        (.addnstr self.window y x f"{s}" (- w x) (| (.color_pair curses col) style))
        (except [curses.error]))))

  (defn input [self [prompt ""]]
    "Get input in a text box."
    (let [y (- curses.LINES 2)
          x (+ 1 (len prompt))
          tw (.newwin curses 1 (- curses.COLS 1) y 1)]
      (.put self y 1 prompt) 
      (.refresh self.window)
      (.bkgdset tw (| (.color_pair curses 191) curses.A_REVERSE))
      (let [tb (.Textbox curses.textpad tw :insert-mode True)]
        (.edit tb)
        (setv instr (.strip (tb.gather)))
        (.clear tw)
        instr)))

  (defn centre [self s]
    (setv #(y x) (.getmaxyx self.stdscr))
    (int (/ (- x (len s)) 2)))

  (defn right [self * [s " "]]
    (setv [y x] (.getmaxyx self.stdscr))
    (int (- x (len s) 1)))

  (defn bottom [self]
    (setv [y x] (.getmaxyx self.stdscr))
    (- y 2))

  (defn warning [self s]
    (unless (in s self.warnings)
      (.append self.warnings s)))

  (defn error [self s]
    (unless (in s self.errors)
      (.append self.errors s)))

  (defn info [self s]
    (unless (in s self.infos)
      (.append self.infos s)))
  
  (defn debug[self s]
    (unless (in s self.debugs)
      (.append self.debugs s)))

  (defn isEnabledFor [self x]
    (not (= x logging.DEBUG))))
