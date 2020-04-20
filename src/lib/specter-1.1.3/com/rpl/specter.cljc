(ns com.rpl.specter
  #?(:cljs (:require-macros
            [com.rpl.specter
              :refer
              [late-bound-nav
               late-bound-richnav
               late-bound-collector
               defcollector
               defnav
               defdynamicnav
               dynamicnav
               richnav
               defrichnav
               recursive-path]]))
  #?(:cljs (:refer-clojure :exclude [NONE]))

  ;; #?(:cljs (:use [cljs.core :only [coll?]]))
  (:require [com.rpl.specter.impl :as i]
            [com.rpl.specter.navs :as n]
            #?(:clj [clojure.walk :as cljwalk])
            #?(:clj [com.rpl.specter.macros :as macros])))

(defn- static-path? [path]
  (if (sequential? path)
   (every? static-path? path)
   (-> path i/dynamic-param? not)
   ))

(defn wrap-dynamic-nav [f]
  (fn [& args]
    (let [ret (apply f args)]
      (cond (and (sequential? ret) (static-path? ret))
            (i/comp-paths* ret)

            (and (sequential? ret) (= 1 (count ret)))
            (first ret)

            :else
            ret
            ))))

#?(:clj
   (do

     (defmacro defmacroalias [name target]
       `(do
          (def ~name (var ~target))
          (alter-meta! (var ~name) merge {:macro true})))

     (defmacroalias richnav macros/richnav)
     (defmacroalias defnav macros/defnav)
     (defmacroalias defrichnav macros/defrichnav)

     (defmacro collector [params [_ [_ structure-sym] & body]]
       `(richnav ~params
          (~'select* [this# vals# ~structure-sym next-fn#]
           (next-fn# (conj vals# (do ~@body)) ~structure-sym))
          (~'transform* [this# vals# ~structure-sym next-fn#]
            (next-fn# (conj vals# (do ~@body)) ~structure-sym))))

     (defmacro defcollector [name & body]
       `(def ~name (collector ~@body)))


     (defn- late-bound-operation [bindings builder-op impls]
       (let [bindings (partition 2 bindings)
             params (map first bindings)
             curr-params (map second bindings)]
         `(let [builder# (~builder-op [~@params] ~@impls)
                curr-params# [~@curr-params]]
            (if (every? (complement i/dynamic-param?) curr-params#)
              (apply builder# curr-params#)
              (com.rpl.specter.impl/->DynamicFunction builder# curr-params# nil)))))

     (defmacro late-bound-nav [bindings & impls]
       (late-bound-operation bindings `nav impls))

     (defmacro late-bound-collector [bindings impl]
       (late-bound-operation bindings `collector [impl]))

     (defmacro late-bound-richnav [bindings & impls]
       (late-bound-operation bindings `richnav impls))

     (defmacro providepath [name apath]
       `(i/providepath* ~name (path ~apath)))

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

     ;; (defmacro investigation-macro [body]
     ;;   `(if ~body
     ;;      "that's a collection"
     ;;      "NOT A COLLECTION"))
     (defmacro investigation-macro [body]
       `(~body))

     (defmacro investigation-macro-fn-1 [& args]
       `(vary-meta (fn ~@args) assoc :depth 1))

     (defmacro investigation-macro-fn [fn-name args body]
       ;; `(defn ~fn-name ~args ~body)
       `(def ~fn-name (investigation-macro-fn-1 ~args ~body))
       )

     ;; copied from tools.macro to avoid the dependency
     (defn- name-with-attributes
       "To be used in macro definitions.
       Handles optional docstrings and attribute maps for a name to be defined
       in a list of macro arguments. If the first macro argument is a string,
       it is added as a docstring to name and removed from the macro argument
       list. If afterwards the first macro argument is a map, its entries are
       added to the name's metadata map and the map is removed from the
       macro argument list. The return value is a vector containing the name
       with its extended metadata map and the list of unprocessed macro
       arguments."
       [name macro-args]
       (let [[docstring macro-args] (if (string? (first macro-args))
                                      [(first macro-args) (next macro-args)]
                                      [nil macro-args])
             [attr macro-args]          (if (map? (first macro-args))
                                          [(first macro-args) (next macro-args)]
                                          [{} macro-args])
             attr                       (if docstring
                                          (assoc attr :doc docstring)
                                          attr)
             attr                       (if (meta name)
                                          (conj (meta name) attr)
                                          attr)]
         [(with-meta name attr) macro-args]))

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


     (defn- ic-prepare-path [locals-set path]
       (cond
         (vector? path)
         (mapv #(ic-prepare-path locals-set %) path)

         (symbol? path)
         (if (contains? locals-set path)
           (let [s (get locals-set path)
                 embed (i/maybe-direct-nav path (-> s meta :direct-nav))]
             `(com.rpl.specter.impl/->LocalSym ~path (quote ~embed)))
           ;; var-get doesn't work in cljs, so capture the val in the macro instead
           `(com.rpl.specter.impl/->VarUse
              ~path
              ~(if-not (instance? Class (resolve path)) `(var ~path))
              (quote ~path)))


         (i/fn-invocation? path)
         (let [[op & params] path]
           ;; need special case for 'fn since macroexpand does NOT
           ;; expand fn when run on cljs code, but it's also not considered a special symbol
           (if (or (= 'fn op) (special-symbol? op))
             `(com.rpl.specter.impl/->SpecialFormUse ~path (quote ~path))
             `(com.rpl.specter.impl/->FnInvocation
               ~(ic-prepare-path locals-set op)
               ~(mapv #(ic-prepare-path locals-set %) params)
               (quote ~path))))


         :else
         (if (empty? (i/used-locals locals-set path))
           path
           `(com.rpl.specter.impl/->DynamicVal (quote ~path)))))


     (defn- ic-possible-params [path]
       (do
         (mapcat
          (fn [e]
            (cond (or (set? e)
                      (map? e)
                      (symbol? e)
                      (and (i/fn-invocation? e)
                           (or (contains? #{'fn* 'fn} (first e))
                               (special-symbol? (first e)))))
                  [e]

                  (sequential? e)
                  (concat (if (vector? e) [e]) (ic-possible-params e))))


          path)))


     (defn- cljs-macroexpand [env form]
       (let [expand-fn (i/cljs-analyzer-macroexpand-1)
             mform (expand-fn env form)]
         (cond (identical? form mform) mform
               (and (seq? mform) (#{'js*} (first mform))) form
               :else (cljs-macroexpand env mform))))

     (defn- cljs-macroexpand-all* [env form]
       (if (and (seq? form)
                (#{'fn 'fn* 'cljs.core/fn} (first form)))
         form
         (let [expanded (if (seq? form) (cljs-macroexpand env form) form)]
           (cljwalk/walk #(cljs-macroexpand-all* env %) identity expanded))))


     (defn- cljs-macroexpand-all [env form]
       (let [ret (cljs-macroexpand-all* env form)]
         ret))


     (defmacro path
       "Same as calling comp-paths, except it caches the composition of the static parts
       of the path for later re-use (when possible). For almost all idiomatic uses
       of Specter provides huge speedup. This macro is automatically used by the
       select/transform/setval/replace-in/etc. macros."
       [& path]
       (let [;;this is a hack, but the composition of &env is considered stable for cljs
             platform (if (contains? &env :locals) :cljs :clj)
             local-syms (if (= platform :cljs)
                          (-> &env :locals keys set) ;cljs
                          (-> &env keys set)) ;clj

             used-locals (i/used-locals local-syms path)

             ;; note: very important to use riddley's macroexpand-all here, so that
             ;; &env is preserved in any potential nested calls to select (like via
             ;; a view function)
             expanded (if (= platform :clj)
                        (i/clj-macroexpand-all (vec path))
                        (cljs-macroexpand-all &env (vec path)))

             prepared-path (ic-prepare-path local-syms expanded)
             possible-params (vec (ic-possible-params expanded))

             cache-sym (vary-meta
                        (gensym "pathcache")
                        merge {:cljs.analyzer/no-resolve true :no-doc true :private true})

             info-sym (gensym "info")

             get-cache-code (if (= platform :clj)
                              `(try (i/get-cell ~cache-sym)
                                    (catch ClassCastException e#
                                      ;; With AOT compilation it's possible for:
                                      ;; Thread 1: unbound, so throw exception
                                      ;; Thread 2: unbound, so throw exception
                                      ;; Thread 1: do alter-var-root
                                      ;; Thread 2: it's bound, so retrieve the current value
                                      (if (bound? (var ~cache-sym))
                                        (i/get-cell ~cache-sym)
                                        (do
                                          (alter-var-root
                                           (var ~cache-sym)
                                           (fn [_#] (i/mutable-cell)))
                                          nil))))
                              cache-sym)

             add-cache-code (if (= platform :clj)
                              `(i/set-cell! ~cache-sym ~info-sym)
                              `(def ~cache-sym ~info-sym))

             precompiled-sym (gensym "precompiled")

             handle-params-code
             (if (= platform :clj)
               `(~precompiled-sym ~@used-locals)
               `(~precompiled-sym ~possible-params))]
         (if (= platform :clj)
           (i/intern* *ns* cache-sym (i/mutable-cell)))
         `(let [info# ~get-cache-code

                info#
                (if (nil? info#)
                  (let [~info-sym (i/magic-precompilation
                                   ~prepared-path
                                   ~(str *ns*)
                                   (quote ~used-locals)
                                   (quote ~possible-params))]
                    ~add-cache-code
                    ~info-sym)
                  info#)

                ~precompiled-sym (i/cached-path-info-precompiled info#)
                dynamic?# (i/cached-path-info-dynamic? info#)]
            (if dynamic?#
              ~handle-params-code
              ~precompiled-sym))))


     (defmacro select
       "Navigates to and returns a sequence of all the elements specified by the path.
       This macro will do inline caching of the path."
       [apath structure]
       `(i/compiled-select* (path ~apath) ~structure))

     (defmacro select-any
       "Returns any element found or [[NONE]] if nothing selected. This is the most
       efficient of the various selection operations.
       This macro will do inline caching of the path."
       [apath structure]
       `(i/compiled-select-any* (path ~apath) ~structure))

     (defmacro transform
       "Navigates to each value specified by the path and replaces it by the result of running
       the transform-fn on it.
       This macro will do inline caching of the path."
       [apath transform-fn structure]
       `(i/compiled-transform* (path ~apath) ~transform-fn ~structure))

     (defmacro setval
       "Navigates to each value specified by the path and replaces it by `aval`.
       This macro will do inline caching of the path."
       [apath aval structure]
       `(i/compiled-setval* (path ~apath) ~aval ~structure))))

(def ^{:doc "Global value used to indicate no elements selected during
             [[select-any]]."}
  NONE i/NONE)

;; ;; Helper for making late-bound navs

(def late-path i/late-path)

;; ;; Built-in pathing and context operations

(defnav
  ^{:doc "Stops navigation at this point. For selection returns nothing and for
          transformation returns the structure unchanged"}
  STOP
  []
  (select* [this structure next-fn]
    NONE)
  (transform* [this structure next-fn]
    structure))

(def
  ^{:doc "Stays navigated at the current point. Essentially a no-op navigator."}
  STAY
  i/STAY*)

(defnav
  ^{:doc "Navigate to every element of the collection. For maps navigates to
          a vector of `[key value]`."}
  ALL
  []
  (select* [this structure next-fn]
    (n/all-select structure next-fn))
  (transform* [this structure next-fn]
    (n/all-transform structure next-fn)))

(def
  ^{:doc "Keeps the element only if it matches the supplied predicate. Functions in paths
          implicitly convert to this navigator."
    :direct-nav true}
  pred
  i/pred*)


(defdynamicnav if-path
  "Like cond-path, but with if semantics."
  ([cond-p then-path]
   (if-path cond-p then-path STOP))
  ([cond-p then-path else-path]
   (if-let [afn (n/extract-basic-filter-fn cond-p)]
    (late-bound-richnav [late-then (late-path then-path)
                         late-else (late-path else-path)]
      (select* [this vals structure next-fn]
        (n/if-select
          vals
          structure
          next-fn
          afn
          late-then
          late-else))
      (transform* [this vals structure next-fn]
        (n/if-transform
          vals
          structure
          next-fn
          afn
          late-then
          late-else)))
    (late-bound-richnav [late-cond (late-path cond-p)
                         late-then (late-path then-path)
                         late-else (late-path else-path)]
      (select* [this vals structure next-fn]
         (n/if-select
          vals
          structure
          next-fn
          #(n/selected?* late-cond vals %)
          late-then
          late-else))
      (transform* [this vals structure next-fn]
         (n/if-transform
          vals
          structure
          next-fn
          #(n/selected?* late-cond vals %)
          late-then
          late-else))))))


(defdynamicnav cond-path
  "Takes in alternating cond-path path cond-path path...
   Tests the structure if selecting with cond-path returns anything.
   If so, it uses the following path for this portion of the navigation.
   Otherwise, it tries the next cond-path. If nothing matches, then the structure
   is not selected."
  [& conds]
  (let [pairs (reverse (partition 2 conds))]
    (reduce
      (fn [p [tester apath]]
        (if-path tester apath p))
      STOP
      pairs)))

(comment
  ;; (def
  ;;   cond-path
  ;;   (com.rpl.specter/dynamicnav
  ;;    [& conds]
  ;;    (let
  ;;        [pairs (reverse (partition 2 conds))]
  ;;      (reduce
  ;;       (fn [p [tester apath]] (if-path tester apath p))
  ;;       STOP
  ;;       pairs))))
  (macroexpand '(com.rpl.specter/dynamicnav
                 [& conds]
                 (let
                     [pairs (reverse (partition 2 conds))]
                   (reduce
                    (fn [p [tester apath]] (if-path tester apath p))
                    STOP
                    pairs))))
  ;; (clojure.core/vary-meta
  ;;  (com.rpl.specter/wrap-dynamic-nav
  ;;   (clojure.core/fn
  ;;     [& conds]
  ;;     (let
  ;;         [pairs (reverse (partition 2 conds))]
  ;;       (reduce
  ;;        (fn [p [tester apath]] (if-path tester apath p))
  ;;        STOP
  ;;        pairs))))
  ;;  clojure.core/assoc
  ;;  :dynamicnav
  ;;  true)
  )

;; (def
;;   ^{:doc "Navigate the data structure until reaching
;;           a value for which `afn` returns truthy. Has
;;           same semantics as clojure.walk."}
;;   walker
;;   (recursive-path [afn] p
;;     (cond-path (pred afn) STAY
;;                coll? [ALL p]
;;                )))

;; (def investigation-fn
;;   (fn [predicat maybe-coll]
;;     (if (predicat maybe-coll)
;;       "that's a collection"
;;       "NOT A COLLECTION")))

(investigation-macro-fn investigation-fn [predicat maybe-coll]
                        (if (predicat maybe-coll)
                          "that's a collection"
                          "NOT A COLLECTION"))

(comment
  (macroexpand '(investigation-macro-fn investigation-fn [predicat maybe-coll]
                                        (if (predicat maybe-coll)
                                          "that's a collection"
                                          "NOT A COLLECTION"))))

(def investigation-str
  (investigation-macro (investigation-fn coll? (range 3))))

(comment
  (macroexpand '(recursive-path [afn] p
                                (cond-path (pred afn) STAY
                                           coll? [ALL p]
                                           )))
  ;; (com.rpl.specter.impl/direct-nav-obj
  ;;  (clojure.core/fn
  ;;    [afn]
  ;;    (clojure.core/let
  ;;        [p (com.rpl.specter.impl/local-declarepath)]
  ;;      (com.rpl.specter/providepath
  ;;       p
  ;;       (cond-path (pred afn) STAY coll? [ALL p]))
  ;;      p)))

  (macroexpand '(investigation-fn (coll? (range 3))))
  )
