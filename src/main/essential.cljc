(ns essential
  #?(:cljs (:require-macros
            [essential
              :refer
             [varialization]]))

  ;; The following line makes the difference. If this line is uncomment
  ;; the cljs.analyzer.api/analyze-file function will execute successfully
  ;; on essential.cljc
  ;; #?(:cljs (:use [cljs.core :only [coll?]]))
  )

#?(:clj
   (do
     (defmacro varialization
       [arg]
       `(var ~arg))
     ))

(defn simple []
  (varialization coll?))

;; (prn (macroexpand '(varialization coll?)))
;; (prn ((simple) [42]))
