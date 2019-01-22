(ns axel-f.v2.runtime
  (:require [axel-f.v2.parser :as parser]
            [axel-f.functions :as impl])
  (:refer-clojure :exclude [eval]))

(declare eval)

(def eval-node nil)

(defmulti eval-node (fn [{:keys [f]} _ _] f))

;; Constants

(defmethod eval-node ::parser/const [{:keys [arg]} _ _] arg)

(defmethod eval-node ::parser/keyword [{:keys [arg]} _ _] arg)

(defmethod eval-node ::parser/vector [{:keys [args]} global-context local-context]
  (mapv #(eval-node % global-context local-context) args))

;; Operators

(defn- boolean?->number [x]
  (case x
    true 1
    false 0
    x))

(defmethod eval-node "+" [{:keys [args]} global-context local-context]
  (reduce (fn [acc i]
            (let [arg (eval-node i global-context local-context)]
              (if ((some-fn boolean? number?) arg)
                (+ acc (boolean?->number arg))
                (throw (ex-info (str "Argument for `+` operator can be number or boolean, got `" arg "`")
                                {:position [(:begin i) (:end i)]})))))
          0
          args))

(defmethod eval-node "-" [{:keys [args]} global-context local-context]
  (let [farg (eval-node (first args) global-context local-context)]
    (if ((some-fn boolean? number?) farg)
      (if (= 1 (count args))
        (* -1 (boolean?->number farg))
        (reduce (fn [acc i]
                  (let [arg (eval-node i global-context local-context)]
                    (if ((some-fn number? boolean?) arg)
                      (- acc (boolean?->number (eval-node i global-context local-context)))
                      (throw (ex-info (str "Argument for `-` operator can be number or boolean, got `" arg "`")
                                      {:position [(:begin i) (:end i)]})))))
                (boolean?->number farg)
                (rest args)))
      (throw (ex-info (str "Argument for `-` operator can be number or boolean, got `" farg "`")
                      {:position [(:begin (first args)) (:end (first args))]})))))

(defmethod eval-node "*" [{:keys [args]} global-context local-context]
  (reduce (fn [acc i]
            (* acc (boolean?->number (eval-node i global-context local-context))))
          1
          args))

(defmethod eval-node "/" [{:keys [args]} global-context local-context]
  (reduce (fn [acc i]
            (/ acc (boolean?->number (eval-node i global-context local-context))))
          (boolean?->number (eval-node (first args) global-context local-context))
          (rest args)))

(defmethod eval-node "!" [{:keys [args]} global-context local-context]
  (not (eval-node (first args) global-context local-context)))

(defmethod eval-node "%" [{:keys [args]} global-context local-context]
  (* 0.01 (eval-node (first args) global-context local-context)))

(defmethod eval-node "&" [{:keys [args]} global-context local-context]
  (reduce (fn [acc i]
            (str acc (eval-node i global-context local-context)))
          ""
          args))

(defmethod eval-node "<" [{:keys [args]} global-context local-context]
  (apply < (map #(eval-node % global-context local-context) args)))

(defmethod eval-node ">" [{:keys [args]} global-context local-context]
  (apply > (map #(eval-node % global-context local-context) args)))

(defmethod eval-node "<=" [{:keys [args]} global-context local-context]
  (apply <= (map #(eval-node % global-context local-context) args)))

(defmethod eval-node ">=" [{:keys [args]} global-context local-context]
  (apply >= (map #(eval-node % global-context local-context) args)))

(defmethod eval-node "<>" [{:keys [args]} global-context local-context]
  (apply not= (map #(eval-node % global-context local-context) args)))

(defmethod eval-node "=" [{:keys [args]} global-context local-context]
  (apply = (map #(eval-node % global-context local-context) args)))

(defmethod eval-node "^" [{:keys [args]} global-context local-context]
  (Math/pow (eval-node (first args) global-context local-context)
            (eval-node (second args) global-context local-context)))

;; Reference

(defn- ->keyword [k]
  (cond
    (string? k) (keyword k)
    (keyword? k) k))

(defn- ->string [k]
  (cond
    (string? k) k
    (keyword? k) (name k)))

(defn- get* [ctx k]
  (or (get ctx (->keyword k))
      (get ctx (->string k))))

(defn- get-in* [ctx [p & path]]
  (if p
    (if (vector? p)
      (if (= ::ALL (first p))
        (mapv #(get-in* % path) ctx)
        (recur (nth ctx (second p) nil) path))
      (recur (get* ctx p) path))
    ctx))

(defn- build-reference-path
  ([path gc lc] (build-reference-path path [] gc lc))
  ([[p & path] acc gc lc]
   (if p
     (cond
       (= ::parser/keyword (:f p))
       (recur path (conj acc (:arg p)) gc lc)

       (= ::parser/symbol (:f p))
       (recur path (conj acc (:arg p)) gc lc)

       (= ::parser/reference (:f p))
       (recur path (conj acc (-> p :args first :arg)) gc lc)

       (= ::parser/nth (:f p))
       (recur path (conj acc (if (= ::parser/ALL (-> p :arg :f))
                               [::ALL]
                               [::NTH (eval-node (:arg p) gc lc)]))
              gc lc)

       (= ::parser/const (:f p))
       (recur path (conj acc (:arg p)) gc lc))
     acc)))

(defmethod eval-node ::parser/reference [{:keys [args]} global-context local-context]
  (let [local-context? (= "_" (-> args first :arg))
        context (if local-context?
                  (or local-context global-context)
                  (get* global-context (-> args first :arg)))]
    (let [path (build-reference-path (rest args) global-context local-context)]
      (get-in* context path))))

;; Special functions

(defmethod eval-node "MAP" [{:keys [args]} global-context local-context]
  (let [f (eval (first args))]
    (mapv #(f global-context %) (eval-node (second args) global-context local-context))))

(defmethod eval-node "FILTER" [{:keys [args]} global-context local-context]
  (let [f (eval (first args))]
    (filterv #(f global-context %)
             (eval-node (second args) global-context local-context))))

(defmethod eval-node "SORT" [{:keys [args]} global-context local-context]
  (let [f (eval (first args))]
    (sort-by #(f global-context %)
             (eval-node (second args) global-context local-context))))

(defmethod eval-node "IF" [{:keys [args]} global-context local-context]
  (if (eval-node (first args) global-context local-context)
    (eval-node (second args) global-context local-context)
    (when-let [else (nth args 2 nil)]
      (eval-node else global-context local-context))))

(defmethod eval-node "IFS" [{:keys [args]} global-context local-context]
  (loop [[[test expr] & clauses] (partition 2 2 args)]
    (when (some? test)
      (if (eval-node test global-context local-context)
        (eval-node expr global-context local-context)
        (recur clauses)))))

(defmethod eval-node :default [{:keys [f args]} global-context local-context]
  (let [f' (impl/find-impl f)]
    (try
      (apply f' (map #(eval-node % global-context local-context) args))
      (catch #?(:clj Throwable
                :cljs js/Error) e
        (let [data (ex-data e)]
          (case (:type data)
            :argument-type
            (throw (ex-info (str "Invalid argument type passed to the function `" f "`")
                            {:position [(-> args first :begin) (-> args last :end)]}))))))))

(defn eval [ast]
  (fn [& [ctx local-ctx]]
    (eval-node ast ctx local-ctx)))
