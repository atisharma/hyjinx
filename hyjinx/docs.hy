"Browse the official Hy / Hyrule docs (with syntax highlighting)."

(import hy hyrule)
(import os
        tarfile
        shutil
        urllib.request)
(import pydoc [pager])
(import platformdirs [user-cache-dir])

(import pygments [highlight])
(import pygments.lexers [RstLexer])
(import pygments.formatters [TerminalFormatter])

(import hyjinx.lib [mkdir slurp])


(defn install []
  "Download a local copy of the Hy documentation."
  (let [hy-url f"https://github.com/hylang/hy/archive/refs/tags/{hy.__version__}.tar.gz"
        hyrule-url f"https://github.com/hylang/hyrule/archive/refs/tags/{hyrule.__version__}.tar.gz"
        cachedir (user-cache-dir __package__ __name__)]
    (mkdir f"{cachedir}/docs/hy")
    ;; download docs to cache
    (urllib.request.urlretrieve hy-url :filename f"{cachedir}/{hy.__version__}.tar.gz")
    (with [tf (tarfile.open f"{cachedir}/{hy.__version__}.tar.gz" "r")]
      (for [f (tf.getmembers)]
        (when (f.path.endswith ".rst")
          (setv f.name (os.path.basename f.name))
          ;; unpack and copy docs/*.rst to cache (incl. version name)
          (tf.extract :member f :path f"{cachedir}/docs/hy"))))
    (mkdir f"{cachedir}/docs/hyrule")
    (urllib.request.urlretrieve hyrule-url :filename f"{cachedir}/{hyrule.__version__}.tar.gz")
    (with [tf (tarfile.open f"{cachedir}/{hyrule.__version__}.tar.gz" "r")]
      (for [f (tf.getmembers)]
        (when (f.path.endswith ".rst")
          (setv f.name (os.path.basename f.name))
          (tf.extract :member f :path f"{cachedir}/docs/hyrule"))))))


(defn hy-doc [[page "index"] * [package "hy"] [use-pager True] [bg "dark"]]
  "Show hy docs (or another package). For example, `(hy-doc \"whyhy\")` or `(hy-doc \"NEWS\")."
  (try
    (let [formatter (TerminalFormatter :bg bg :stripall True)
          term (shutil.get-terminal-size)
          cachedir (user-cache-dir __package__ __name__)
          fname f"{cachedir}/docs/{package}/{page}.rst"
          text (highlight (slurp fname) (RstLexer) formatter)]
      (if use-pager
          (pager text)
          (print text)))
    (except [FileNotFoundError]
      (raise (FileNotFoundError f"Could not find '{page}' - did you correctly type the page name and install the documentation with (doc.install)?")))))

(defn hyrule-doc [#* args #** kwargs]
  "Show hyrule docs."
  (hy-docs #* args #** kwargs :package "hyrule"))
