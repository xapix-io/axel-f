(ns axel-f.v2.eval
  (:require [axel-f.v2.utils :as utils]
            [axel-f.v2.functions :as functions]))

(defn exec-fn [e]
  (if (utils/constant? e)
    (fn [_ _ _] e)
    (case (first e)
      :FORMULA (let [base-registry (merge functions/default-registry
                                          @functions/*registry*)]
                 (fn [ctx & {:keys [registry]}]
                   (last (map #((exec-fn %) ctx nil (merge registry
                                                           base-registry))
                              (rest e)))))

      ;; Reference
      :REF_OP (fn [ctx local-ctx registry]
                ;; TODO implement get-in for symbols
                (get-in (case (second e)
                          :GLOBAL_ROOT ctx
                          :LOCAL_ROOT local-ctx)
                        (rest (rest e))))

      ;; Object
      :OBJECT_OP (fn [ctx local-ctx registry]
                   (->> (rest e)
                        (map (fn [[k v]]
                               [k ((exec-fn v) ctx local-ctx registry)]))
                        (into {})))

      ;; Collection
      :ARRAY_OP (fn [ctx local-ctx registry]
                  (->> (rest e)
                       (map (fn [e]
                              ((exec-fn e) ctx local-ctx registry)))
                       (into [])))

      ;; Special functions
      ;; Those functions requires some extra work for its arguments
      ;; Like 'do not evaluate branches before condition' in IF
      "IF" (fn [ctx local-ctx registry]
            (if ((exec-fn (nth e 1 nil)) ctx local-ctx registry)
              ((exec-fn (nth e 2 nil)) ctx local-ctx registry)
              ((exec-fn (nth e 3 nil)) ctx local-ctx registry)))

      "MAP" (fn [ctx local-ctx registry]
              (map #((exec-fn (second e)) ctx % registry)
                   ((exec-fn (nth e 2)) ctx local-ctx registry)))

      "SORT" (fn [ctx local-ctx registry]
               (let [keyfn? (> 2 (count e))
                     keyfn (if (not keyfn?)
                             identity
                             #((exec-fn (second e)) ctx % registry))]
                 (sort-by keyfn ((exec-fn (nth e (if (not keyfn?) 2 1))) ctx local-ctx registry))))

      "FILTER" (fn [ctx local-ctx registry]
                 (let [no-pred? (= 2 (count e))
                       pred (if no-pred?
                              identity
                              #((exec-fn (second e)) ctx % registry))]
                   (filter pred ((exec-fn (nth e (if no-pred? 1 2))) ctx local-ctx registry))))

      ;; Default handler
      (fn [ctx local-ctx registry]
        (if-let [f (get-in registry [(first e) :impl])]
          (apply f (map #((exec-fn %) ctx local-ctx registry) (rest e)))
          (throw (ex-info (str "Function " (first e) " is not implemented.") {})))))))
