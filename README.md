# cljdoc_specter_support

The repository contains an synthetic example of `cljs.analyzer.api/analyze-file`
failure over `specter.cljc`, a file of the specter package.

This investigation is motivated by the project cljdoc-analyzer. Currently the
project is configured to analyse specter package only in clojure environment.
If you disable this rule (from `resources/config.edn`) and you run this command
from the root of cljdoc-analyzer project repo:
``` shell
clojure -m cljdoc-analyzer.main analyze \
  --project com.rpl/specter --version "1.1.3" \
  --output-filename "com-rpl-specter-1.1.3.edn"
```
You'll get the origin of this investigation.

This is related to the [cljdoc issue #261](https://github.com/cljdoc/cljdoc/issues/261).

The generated crash report is available in `resources/clojure-3143760720018079960.edn`.

## Installation

You need leiningen and usual stuff of clojure development environment.

## Usage

``` shell
lein run
```

You should get an error message containing:
```
Syntax error (ExceptionInfo) compiling at (/tmp/form-init5983654348168614256.clj:1:73).
Unable to resolve var: coll? in this context at line 21 src/main/essential.cljc
```
