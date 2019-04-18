(ns axel-f.runtime
  (:refer-clojure :exclude [eval type])
  (:require [axel-f.parser :as parser]
            [axel-f.lexer :as lexer]
            [clojure.string :as string]))

(declare eval*)

(defn flexy-get [m k]
  (cond
    (string? k)
    (or (get m k)
        (get m (keyword k)))

    (keyword? k)
    (or (get m k)
        (get m (string/join "/" (filter identity ((juxt namespace name) k)))))))

(defn env-lookup [env path]
  (if (empty? path)
    env
    (let [[p & path] path
          [index? p'] (when (sequential? p) p)]
      (if index?
        (cond
          (number? p') (env-lookup (let [env' (or (get env "_")
                                                  (get env ::context)
                                                  env)]
                                     (when (sequential? env')
                                       (nth env' p' nil)))
                                   path)
          (= ::select-all p') (mapv #(env-lookup % path) (or (get env "_")
                                                             (get env ::context)
                                                             env))
          (sequential? p') (map #(env-lookup % path) (map #(env-lookup env [[::index-ref %]]) p'))
          :otherwise (env-lookup (flexy-get env p') path))
        (when-let [env' (or (flexy-get env p)
                            (flexy-get (get env "_") p)
                            (flexy-get (get env ::context) p))]
          (env-lookup env' path))))))

(defmulti eval* (fn [env {::parser/keys [type]}] type))

(defmethod eval* ::parser/constant [_ {::lexer/keys [value type]}]
  (if (= ::lexer/symbol type)
    (case (string/lower-case value)
      "true" true
      "false" false
      "null" nil)
    value))

(defmethod eval* ::parser/var [env {::parser/keys [parts]}]
  (env-lookup env (mapv (partial eval* env) parts)))

(defmethod eval* ::parser/symbol [env {::lexer/keys [value]}]
  value)

(defn- wildcart-operator? [{::parser/keys [type parts]}]
  (and (= type ::parser/var)
       (= 1 (count parts))
       (= "*" (-> parts first ::lexer/value))))

(defmethod eval* ::parser/list-ref [env {::parser/keys [expr]}]
  (let [ref (eval* env expr)]
    (cond
      (or (number? ref)
          (sequential? ref)) [::index-ref ref]
      (wildcart-operator? expr) [::index-ref ::select-all]
      (or (string? ref)
          (keyword? ref)) ref)))

(defmethod eval* ::parser/list [env {::parser/keys [entries]}]
  (map (partial eval* env) entries))

(defmethod eval* ::parser/binary [env {::parser/keys [left right operator]}]
  ;; TODO throw when no operator
  ((eval* env operator)
   (eval* env left)
   (eval* env right)))

(defmethod eval* ::parser/prefix [env {::parser/keys [operator operand]}]
  ;; TODO throw when no operator
  ((eval* env operator)
   (eval* env operand)))

(defmethod eval* ::parser/postfix [env {::parser/keys [operator operand]}]
  ;; TODO throw when no operator
  ((eval* env operator)
   (eval* env operand)))

(defmethod eval* ::parser/application [env {::parser/keys [function args]}]
  (let [f (eval* env function)
        {:keys [special? arglists]} (meta f)]
    ;; TODO check arity and presense of f
    (if special?
      (apply (partial f env) args)
      (apply f (map (partial eval* env) args)))))

(defmethod eval* ::parser/formula [env {::parser/keys [body]}]
  (let [f (gensym)]
    (fn f
      ([] (f nil nil))
      ([context] (f context nil))
      ([context extra-env]
       (loop [result nil
              env (assoc (merge env extra-env)
                         ::context context
                         "_" context)
              exprs body]
         (if (empty? exprs)
           result
           (let [result' (eval* env (first exprs))]
             (recur result' env (next exprs)))))))))
