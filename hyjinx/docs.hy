"Browse the official Hy / Hyrule docs (with syntax highlighting)."

(require hyrule [defmain])

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
  "Download a local copy of the Hy and Hyrule documentation."
  (_install-docs f"https://github.com/hylang/hy/archive/refs/tags/{hy.__version__}.tar.gz" f"hy_{hy.__version__}")
  (_install-docs f"https://github.com/hylang/hyrule/archive/refs/tags/{hyrule.__version__}.tar.gz" f"hyrule_{hyrule.__version__}"))

(defn _show-doc [[page "index"] * package [use-pager True] [bg "dark"]]
  "Show a package's documentation. For example, (hy-doc \"whyhy\") or (hy-doc \"NEWS\")."
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
      (raise (FileNotFoundError f"Could not find '{page}' - did you correctly type the page name and install the documentation with (hyjinx.docs.install)?")))))

(defn hyrule-doc [#* args #** kwargs]
  "Show hyrule's docs."
  (_show-doc #* args #** kwargs :package f"hyrule_{hyrule.__version__}"))

(defn hy-doc [#* args #** kwargs]
  "Show Hy's documentation. For example, (hy-doc \"whyhy\") or (hy-doc \"NEWS\")."
  (_show-doc #* args #** kwargs :package f"hy_{hy.__version__}"))

(defmain []
  "If running from the command line, install the docs."
  (install))
