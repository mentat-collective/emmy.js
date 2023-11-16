#_"SPDX-License-Identifier: MIT"

;; useful to see what's exported:
;; npx shadow-cljs clj-eval "(use 'emmy.js.main) (sort (keys (emmy-exports)))"

(ns emmy.js.main
  "This namespace exists to give shadow-cljs something local to compile
   while pulling in the rest of Emmy."
  (:require [emmy.env]
            [emmy.operator]
            #?(:cljs [emmy.structure :as s])
            [emmy.abstract.function]
            [emmy.mechanics.lagrange]
            #?(:cljs [emmy.util :as u])
            [emmy.value :as v]
            [clojure.string :as string]))

(let [name-mappings [#"^->" "to_"
                     #"->" "_to_"
                     #"^\+$" "add"
                     #"^-$" "sub"
                     #"^\*$" "mul"
                     #"^/$" "div"
                     #"^=$" "eq"
                     #"\?$" "_p"
                     #"\$\$$" "_display"
                     #"!$" "_now"
                     #"^-" "minus_"
                     #"[-:]" "_"]]
  (defn fn-name-for-js
    "Replaces characters allowed in Clojure function names with substitutions in the
     narrower set of characters allowed in JavaScript function names."
    [fn-name]
    (reduce (fn [s [re sub]] (string/replace s re sub)) fn-name (partition 2 name-mappings))))

(defn emmy-exports
  []
  (into {:count count
         :flatten flatten
         :literal_function emmy.abstract.function/literal-function
         :nth nth
         :symbol symbol
         :type type
         :vector_p vector?}
        (for [[fn-sym export] (ns-publics 'emmy.env)]
          [(keyword (fn-name-for-js (str fn-sym))) @export])))


    ;; withSymbols(symbols: string, f: (...a: any[]) => any) {
    ;;     const args = symbols.trim().split(/[\s,]+/).map(x => {
    ;;         if (x.endsWith('()')) {
    ;;             return this.literalFunction(x.substring(0, x.length - 2))
    ;;         } else {
    ;;             return this.symbol(x)
    ;;         }
    ;;     })
    ;;     return f(...args)
    ;; }

#?(:cljs
   (defn js-literal-function
    "Takes a string (instead of a symbol, for convenience) and produces
     a literal function which is JS-callable."
    [s]
    (v/make-es6-callable (emmy.env/literal-function (symbol s)) identity)))

#?(:cljs
   (defn ^:private with-symbols
     [sym-string f]
     (let [->sym (fn [s] (if (string/ends-with? s "()")
                           (js-literal-function (subs s 0 (- (count s) 2)))
                           (symbol s)))
           sym-seq  (-> sym-string
                        string/trim
                        (string/split #"[\s,]+"))]
       (apply f (map ->sym sym-seq)))))

#?(:cljs
   #_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
   (defn emmy-js-exports
     "This function is invoked by the shadow-cljs build system (see deps.edn) to
      generate a map of exported-function-name -> implementation. (functions absent
      from this map are subject to having their names minified in a release build)."
     []
     (letfn [(es-promote-structure
              ;; Takes a structure and equips with with callability and indexability in
              ;; the JavaScript senses of those terms.
              [s]
              (s/make-es6-indexable (v/make-es6-callable s s/make-es6-indexable)))
             (js-literal-function
              ;; Takes a string (instead of a symbol, for convenience) and produces
              ;; a literal function which is JS-callable
              [s]
              (v/make-es6-callable (emmy.env/literal-function (symbol s)) identity))
             (callable-wrapper
              ;; There are things in ClojureScript which are callable within the language
              ;; which are implemented by JavaScript objects which are not callable in
              ;; JavaScript. Two examples are MultiFns and Emmy operators. MultiFns
              ;; are implemented as objects with a `call` member, and operators implement
              ;; IFn, but that is not sufficient to make the backing JS object callable.
              ;; This function wraps such objects in a pass-through function which will
              ;; become a Function object in JavaScript. Other objects are returned
              ;; unmodified.
              [mf]
              (if (or (instance? MultiFn mf)
                      (emmy.operator/operator? mf))
                (fn [& xs] (apply mf xs))
                mf))
             ]
       (clj->js
        (u/map-vals
         callable-wrapper
         (into (emmy-exports)
               {:make_es6_indexable emmy.structure/make-es6-indexable
                :make_es6_callable emmy.value/make-es6-callable
                :clj_to_js clj->js
                ;; functions returning structures are wrapped to promote those structures
                ;; with JavaScript native index and call capability
                :up #(es-promote-structure (apply s/up %&))
                :down #(es-promote-structure (apply s/down %&))
                :cross_product #(es-promote-structure (emmy.env/cross-product %1 %2))
                :literal_function js-literal-function
                :partial (fn [& xs] (v/make-es6-callable (apply emmy.env/partial xs) identity))
                :make_path (fn [& xs] (v/make-es6-callable (apply emmy.mechanics.lagrange/make-path xs) identity))
                ;; functions which return things that would be easier to work with in
                ;; native JavaScript form
                :minimize (comp clj->js emmy.env/minimize)
                :multidimensional_minimize  (comp clj->js emmy.env/multidimensional-minimize)
                :with_symbols with-symbols}))))))
