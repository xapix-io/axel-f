(ns axel-f.analyzer
  #?(:clj (:import clojure.lang.ExceptionInfo))
  (:require [axel-f.runtime :as runtime]))

(defmulti analyze runtime/type)

(defmethod analyze :default [_ & _])

(defmethod analyze ::runtime/unary-expr [{::runtime/keys [expr]} & [*acc*]]
  (analyze expr *acc*))

(defmethod analyze ::runtime/binary-expr [{::runtime/keys [left-expr right-expr]} & [*acc*]]
  (doseq [expr [left-expr right-expr]]
    (analyze expr *acc*)))

(defmethod analyze ::runtime/root-reference-expr [{::runtime/keys [field-expr]} & [*acc*]]
  (let [field (runtime/eval field-expr)]
    (swap! *acc* update :refs conj (reverse (conj (:proc @*acc*) field)))
    (swap! *acc* assoc :proc [])))

(defmethod analyze ::runtime/reference-expr [{::runtime/keys [ctx-expr field-expr]} & [*acc*]]
  (let [field (runtime/eval field-expr nil nil)]
    (swap! *acc* update :proc conj field)
    (analyze ctx-expr *acc*)))

(defmethod analyze ::runtime/index-expr [{::runtime/keys [ctx-expr ref-expr]} & [*acc*]]
  (if ctx-expr
    (do
      (swap! *acc* update :proc conj "*")
      (let [nacc (atom {:proc []
                        :refs []})]
        (analyze ref-expr nacc)
        (when-let [nrefs (not-empty (:refs @nacc))]
          (swap! *acc* update :refs concat nrefs))
        (analyze ctx-expr *acc*)))
    (do
      (swap! *acc* update :refs conj (reverse (conj (:proc @*acc*) "*")))
      (swap! *acc* assoc :proc []))))

(defmethod analyze ::runtime/list-expr [{::runtime/keys [exprs]} & [*acc*]]
  (doseq [expr exprs]
    (analyze expr *acc*)))

(defmethod analyze ::runtime/application-expr [{::runtime/keys [arg-list]} & [*acc*]]
  (analyze arg-list *acc*))

(defmethod analyze ::runtime/formula [{::runtime/keys [expr]}]
  (let [acc (atom {:proc []
                   :refs []})]
    (analyze expr acc)
    @acc))

(defn report [ast]
  (let [{:keys [refs]} (analyze ast)]
    {:used-references refs}))
