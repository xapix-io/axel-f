(ns axel-f.v2.type-system
  (:require [axel-f.v2.utils :as utils]
            [axel-f.v2.functions :as functions]
            [axel-f.v2.types :as t]))

(defn type-fn [e]
  (if (utils/constant? e)
    (fn [_ _ _]
      (t/extract-type e))
    (case (first e)
      :FORMULA (let [base-registry (merge functions/default-registry
                                          @functions/*registry*)]
                 (fn [ctx & {:keys [registry]}]
                   (last (map #((type-fn %) ctx nil (merge registry
                                                           base-registry))
                              (rest e)))))
      :REF_OP (fn [ctx local-ctx registry]
                (t/extract-type (get-in (case (second e)
                                          :GLOBAL_ROOT ctx
                                          :LOCAL_ROOT local-ctx)
                                        (rest (rest e)))))
      (fn [ctx local-ctx registry]
        (if-let [args-pred (get-in registry [(first e) :args])]
          (let [arg-types (map #((type-fn %) ctx local-ctx registry) (rest e))]
            (try
              (mapv #(when (isa? % clojure.lang.ExceptionInfo)
                       (throw %))
                    (apply args-pred arg-types))
              (catch clojure.lang.ExceptionInfo e
                (let [{:keys [expected actual argument]} (ex-data e)]
                  (throw (ex-info (str "arg[" argument "] of " (first e) " must be of type " expected " got " actual))))))
            (if-let [res-pred (get-in registry [(first e) :res])]
              (apply res-pred arg-types)
              #{Object}))
          (throw (ex-info (str "Function " (first e) " is not implemented.") {})))))))
