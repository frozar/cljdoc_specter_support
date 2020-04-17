(ns com.rpl.specter
  #?(:cljs (:require-macros
            [com.rpl.specter
              :refer
              [defdynamicnav
               recursive-path]]
            )))

#?(:clj
   (do
     (defmacro recursive-path [params self-sym path]
       (if (empty? params)
         `(let [~self-sym (i/local-declarepath)]
            (providepath ~self-sym ~path)
            ~self-sym)
         `(i/direct-nav-obj
            (fn ~params
              (let [~self-sym (i/local-declarepath)]
                (providepath ~self-sym ~path)
                ~self-sym)))))

     (defmacro dynamicnav [& args]
       `(vary-meta (wrap-dynamic-nav (fn ~@args)) assoc :dynamicnav true))

     (defmacro defdynamicnav
       "Defines a function that can choose what navigator to use at runtime based on
        the dynamic context. The arguments will either be static values or
        objects satisfying `dynamic-param?`. Use `late-bound-nav` to produce a runtime
        navigator that uses the values of the dynamic params. See `selected?` for
        an illustrative example of dynamic navs."
       [name & args]
       (let [[name args] (name-with-attributes name args)]
         `(def ~name (dynamicnav ~@args))))
    ))

(def STAY :STAY)
(def pred :pred)

(defdynamicnav cond-path
  [& conds]
  42)

(def
  ^{:doc "Navigate the data structure until reaching
          a value for which `afn` returns truthy. Has
          same semantics as clojure.walk."}
  walker
  (recursive-path [afn] p
    (cond-path (pred afn) STAY
               coll? [ALL p]
               )))

(comment
  (macroexpand '(recursive-path [afn] p
                                (cond-path (pred afn) STAY
                                           coll? [ALL p]
                                           )))
  ;;(com.rpl.specter.impl/direct-nav-obj (clojure.core/fn [afn] (clojure.core/let [p (com.rpl.specter.impl/local-declarepath)] (com.rpl.specter/providepath p (cond-path (pred afn) STAY coll? [ALL p])) p)))
  )
