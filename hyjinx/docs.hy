"Browse the official hy, hyrule and toolz docs (with syntax highlighting)."

(require hyrule [defmain])

(import hy hyrule toolz)
(import toolz.itertoolz [rest])
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

;; TODO search docs for terms


(defn _install_docs [tarball-url package * [extension ".rst"]]
  "Download a local copy of some specific documentation."
  (let [cachedir (user-cache-dir __package__ __name__)
        tarball f"{cachedir}/{package}.tar.gz"]
    (mkdir f"{cachedir}/docs/{package}")
    ;; download docs to cache
    (urllib.request.urlretrieve tarball-url :filename tarball) 
    (with [tf (tarfile.open tarball "r")]
      (for [f (tf.getmembers)]
        (when (f.path.endswith extension)
          (setv f.name (os.path.basename f.name))
          ;; unpack and copy docs/*.rst to cache
          (tf.extract :member f :path f"{cachedir}/docs/{package}"))))
    (os.remove tarball)))

(defn install []
  "Download a local copy of the Hy, Hyrule and toolz documentation."
  (_install-docs f"https://github.com/hylang/hy/archive/refs/tags/{hy.__version__}.tar.gz" f"hy_{hy.__version__}")
  (_install-docs f"https://github.com/hylang/hyrule/archive/refs/tags/{hyrule.__version__}.tar.gz" f"hyrule_{hyrule.__version__}")
  (_install-docs f"https://github.com/pytoolz/toolz/archive/refs/tags/{toolz.__version__}.tar.gz" f"toolz_{toolz.__version__}"))

(defn _show-doc [[page "index"] * package [use-pager True] [bg "dark"]]
  "Show a package's documentation."
  (try
    (let [formatter (TerminalFormatter :bg bg :stripall True)
          term (shutil.get-terminal-size)
          cachedir (user-cache-dir __package__ __name__)
          fname f"{cachedir}/docs/{package}/{(str page)}.rst"
          text (highlight (slurp fname) (RstLexer) formatter)]
      (if use-pager
          (pager text)
          (print text)))
    (except [FileNotFoundError]
      (raise (FileNotFoundError f"Could not find '{(str page)}' - did you correctly type the page name and install the documentation with (hyjinx.docs.install)?")))))

(defn doc-hy [#* args #** kwargs]
  "Show hy's documentation. For example, (doc-hy \"whyhy\") or (doc-hy \"NEWS\")."
  (_show-doc #* args #** kwargs :package f"hy_{hy.__version__}"))

(defn doc-hyrule [#* args #** kwargs]
  "Show hyrule's docs."
  (_show-doc #* args #** kwargs :package f"hyrule_{hyrule.__version__}"))

(defn doc-toolz [#* args #** kwargs]
  "Show toolz's documentation. For example, (doc-toolz \"curry\")."
  (_show-doc #* args #** kwargs :package f"toolz_{toolz.__version__}"))

(defn doc [package #* args #** kwargs]
  "Show documentation page for hy, hyrule or toolz."
  (match (str package)
    "hy" (doc-hy #* args #** kwargs)
    "hyrule" (doc-hyrule #* args #** kwargs)
    "toolz" (doc-toolz #* args #** kwargs)
    _ (raise (FileNotFoundError f"Could not find docs for '{(str package)}' - did you install the documentation with (hyjinx.docs.install)?"))))

(defmain [args]
  "If running from the command line, either install the docs, or show the help."
  (if args
    (doc #* (rest args))
    (install)))
