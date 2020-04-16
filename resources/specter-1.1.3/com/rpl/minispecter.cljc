(ns com.rpl.specter
  #?(:cljs (:require-macros
            [com.rpl.specter
              :refer
              [defdynamicnav
               dynamicnav
               providepath
               recursive-path]]
            ))
  (:require [com.rpl.specter.impl :as i]))

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

    ))

(def
  ^{:doc "Stays navigated at the current point. Essentially a no-op navigator."}
  STAY
  i/STAY*)

(def
  ^{:doc "Keeps the element only if it matches the supplied predicate. Functions in paths
          implicitly convert to this navigator."
    :direct-nav true}
  pred
  i/pred*)

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
  ;; (com.rpl.specter.impl/direct-nav-obj (clojure.core/fn [afn] (clojure.core/let [p (com.rpl.specter.impl/local-declarepath)] (com.rpl.specter/providepath p (cond-path (pred afn) STAY coll? [ALL p])) p)))
  )
