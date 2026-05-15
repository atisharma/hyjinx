#!/usr/bin/env hy
"
CLI for Hy/Python code introspection.

Commands:
    hyjinx where <symbol>  - Print file:line for a symbol
    hyjinx source <symbol> - Print source code with highlighting
    hyjinx doc <symbol>    - Print docstring
"

(require hyrule [-> ->>])

(import click)
(import importlib)
(import pathlib [Path])
(import functools [reduce])
(import hy [mangle])
(import toolz [last])
(import inspect [ismodule])
(import pygments [highlight])
(import pygments.lexers [get-lexer-by-name])
(import pygments.formatters [TerminalFormatter])
(import multimethod [multimethod])
(import hyjinx.source [print-source get-source-details _get-lang-from-filename])
(import hyjinx.hjx-inspect [getsource getsourcefile])
(import hyjinx.api [api-surface resolve-module-path format-surface])


(defn get-source-details-safe [obj]
  "Get source details, handling modules and multimethods specially."
  (try
    (get-source-details obj)
    (except [TypeError]
      ;; Module, multimethod, or other object without source details
      (cond
        ;; Module
        (ismodule obj)
        (if (hasattr obj "__file__")
          {"line" 1
           "module" (getattr obj "__name__" "unknown")
           "file" obj.__file__
           "language" (_get-lang-from-filename obj.__file__)
           "extension" (last (.split obj.__file__ "."))}
          None)

        ;; Multimethod
        (isinstance obj multimethod)
        (let [module-name (getattr obj "__module__" None)
              obj-name (getattr obj "__name__" "unknown")]
          (if module-name
            (let [module (importlib.import-module module-name)]
              {"line" 1
               "module" module-name
               "file" module.__file__
               "language" (_get-lang-from-filename module.__file__)
               "extension" (last (.split module.__file__ "."))})
            None))

        ;; Other object with __file__
        (hasattr obj "__file__")
        {"line" 1
         "module" (getattr obj "__name__" "unknown")
         "file" obj.__file__
         "language" (_get-lang-from-filename obj.__file__)
         "extension" (last (.split obj.__file__ "."))}

        ;; Give up
        True
        None))))

(defn resolve-symbol [symbol-path]
  "Resolve a dotted symbol path to an object.

  Examples:
    'hyjinx.macros.defmethod'
    -> import hyjinx.macros, return defmethod macro"
  (let [parts (.split symbol-path ".")]
    (if (= (len parts) 1)
      ;; Just a module name
      (importlib.import-module symbol-path)
      ;; Module + attributes
      (do
        ;; Try progressively longer module paths
        ;; e.g. for 'a.b.c.d', try 'a', 'a.b', 'a.b.c' as modules
        (setv module-idx None)
        (for [i (range (len parts) 0 -1)]
          (let [module-path (.join "." (cut parts 0 i))]
            (try
              (importlib.import-module module-path)
              (setv module-idx i)
              (break)
              (except [ImportError]))))

        (when (is module-idx None)
          (raise (ImportError f"Cannot import any module from {symbol-path}")))

        ;; Import the module and walk the attribute chain
        (let [module-path (.join "." (cut parts 0 module-idx))
              module (importlib.import-module module-path)
              attrs (list (map mangle (cut parts module-idx None)))]
          ;; First try regular attributes
          (setv obj module)
          (for [attr attrs]
            (setv obj (getattr obj attr None)))
          ;; If not found, try macros
          (if (is obj None)
            (do
              (setv macros (getattr module "_hy_macros" {}))
              (if (in (last attrs) macros)
                (get macros (last attrs))
                (raise (AttributeError f"{module-path} has no attribute {(last attrs)}"))))
            obj))))))


(defn get-docstring [obj]
  "Get the docstring for an object."
  (or (getattr obj "__doc__" None) "No docstring available."))


;; --- CLI Commands ---

(defn [(click.group)]
  cli [])


(defn [(click.command)
       (click.argument "symbol")
       (click.option "--json" :is-flag True :help "Output as JSON")]
  where [symbol json]
  "Print the file and line number where SYMBOL is defined."
  (try
    (let [obj (resolve-symbol symbol)
          details (get-source-details-safe obj)]
      (if (is details None)
        (do
          (click.echo f"No source details available for: {symbol}" :err True)
          (raise (SystemExit 1)))
        (if json
          (click.echo (hy.I.json.dumps details :indent 2))
          (click.echo f"{(:file details)}:{(:line details)}"))))
    (except [ImportError]
      (click.echo f"Symbol not found: {symbol}" :err True)
      (raise (SystemExit 1)))
    (except [AttributeError]
      (click.echo f"Attribute not found: {symbol}" :err True)
      (raise (SystemExit 1)))
    (except [[OSError TypeError]]
      (click.echo f"No source available for: {symbol}" :err True)
      (raise (SystemExit 1)))))

(cli.add-command where)


(defn [(click.command)
       (click.argument "symbol")
       (click.option "--no-highlight" :is-flag True :help "Disable syntax highlighting")
       (click.option "--line-numbers" :is-flag True :default True :help "Show line numbers")
       (click.option "--reformat" :is-flag True :help "Reformat with beautifhy")]
  source [symbol no-highlight line-numbers reformat]
  "Print the source code for SYMBOL."
  (try
    (let [obj (resolve-symbol symbol)]
      (if no-highlight
        ;; Just print raw source
        (click.echo (getsource obj))
        ;; Check if it's a module
        (if (ismodule obj)
          ;; For modules, print with basic highlighting
          (let [file (getsourcefile obj)
                lang (_get-lang-from-filename file)
                source (getsource obj)
                lexer (get-lexer-by-name lang)
                formatter (TerminalFormatter :bg "dark" :stripall True)]
            (click.echo f"{obj}")
            (click.echo f"File {file}")
            (click.echo)
            (click.echo (highlight source lexer formatter)))
          ;; For other objects, use print-source with options
          (print-source obj 
                        :linenos line-numbers 
                        :reformat reformat))))
    (except [ImportError]
      (click.echo f"Symbol not found: {symbol}" :err True)
      (raise (SystemExit 1)))
    (except [AttributeError]
      (click.echo f"Attribute not found: {symbol}" :err True)
      (raise (SystemExit 1)))
    (except [[OSError TypeError]]
      (click.echo f"No source available for: {symbol}" :err True)
      (raise (SystemExit 1)))))

(cli.add-command source)


(defn [(click.command)
       (click.argument "symbol")
       (click.option "--all" :is-flag True :help "Include function signature")]
  doc [symbol all]
  "Print the docstring for SYMBOL."
  (try
    (let [obj (resolve-symbol symbol)
          docstring (get-docstring obj)]
      (if all
        (do
          (click.echo f"{obj}")
          (click.echo)
          (click.echo docstring))
        (click.echo docstring)))
    (except [ImportError]
      (click.echo f"Symbol not found: {symbol}" :err True)
      (raise (SystemExit 1)))
    (except [AttributeError]
      (click.echo f"Attribute not found: {symbol}" :err True)
      (raise (SystemExit 1)))
    (except [[OSError TypeError]]
      (click.echo f"No docstring available for: {symbol}" :err True)
      (raise (SystemExit 1)))))

(cli.add-command doc)


(defn [(click.command)
       (click.argument "target")
       (click.option "--params" :is-flag True :default True :help "Show parameter names")
       (click.option "--line" :is-flag True :default False :help "Show line numbers")
       (click.option "--kind" :type (click.Choice ["defn" "defclass" "defmacro" "setv" "def" "class"]) :multiple True :help "Filter by definition kind")]
  dir-cmd [target params line kind]
  "List the API surface of a module or file — no imports, no side effects.\n\nLike Python's dir(), but static. TARGET can be a file path (preferred)\nor a dotted module name. File paths are safest as they never trigger\nmodule side effects. Dotted names resolve via sys.path or importlib\n(may trigger imports)."
  (let [path (if (.startswith target "/")
                (Path target)
                (resolve-module-path target))]
    (when (is path None)
      (click.echo f"Cannot resolve: {target}" :err True)
      (raise (SystemExit 1)))
    (when (not path.exists)
      (click.echo f"File not found: {path}" :err True)
      (raise (SystemExit 1)))
    (let [defs (api-surface path)]
      (when kind
        (setv defs (lfor d defs :if (in (:kind d) kind) d)))
      (if (not defs)
        (click.echo f"No definitions found in {path}")
        (do
          (click.echo f"{(str path)}  [{(len defs)} definitions]")
          (click.echo (format-surface defs :show-params params :show-line line)))))))

(cli.add-command dir-cmd :name "dir")


(defn main []
  "Entry point for the CLI."
  (cli))


(when (= __name__ "__main__")
  (main))
