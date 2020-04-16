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

The generated crash report is available in `resources/clojure-7761295136030698780.edn`.

## Installation

You need leiningen and usual stuff of clojure development environment.
NB.: the specter package v1.1.3 is available under the `resources` directory.

## Usage

``` shell
lein run
```

You should get an error message containing:
```
Unable to resolve var: coll? in this context at line 1450 resources/specter-1.1.3/com/rpl/specter.cljc
```

## Observation

If the `recursive-path` macro is used in the `specter.cljc` file, `analyze-file` fails.
This macro is only used in the definition of `walker` and `codewalker` near to the
end of `specter.cljc`.
