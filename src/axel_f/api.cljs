(ns axel-f.api
  (:require [axel-f.core :as axel-f]
            axel-f.functions
            axel-f.base64
            axel-f.json
            axel-f.geo))

(defn- fix-regex-in-exception [exception-data]
  (update exception-data
          :reason
          (fn [reasons]
            (mapv (fn [{:keys [tag] :as reason}]
                    (if (= tag :regexp)
                      (update reason :expecting str)
                      reason))
                  reasons))))

(defn ^:export compile [formula-str]
  (try
    (clj->js (axel-f/compile formula-str))
    (catch ExceptionInfo e
      (throw (js/Error. (js/JSON.stringify (clj->js (fix-regex-in-exception
                                                     (ex-data e)))))))))

(defn ^:export run
  ([formula] (run formula (clj->js {})))
  ([formula context]
   (let [context (js->clj context)]
     (try
       (let [formula (if (string? formula)
                       formula
                       (js->clj formula))]
         (clj->js (axel-f/run formula context)))
       (catch ExceptionInfo e
         (throw (js/Error. (js/JSON.stringify (clj->js (fix-regex-in-exception
                                                        (ex-data e)))))))))))

(defn ^:export autocomplete
  ([incomplete-formula] (autocomplete incomplete-formula {}))
  ([incomplete-formula context]
   (let [context (js->clj context)]
     (clj->js
      (try
        (autocomplete/autocomplete incomplete-formula context)
        (catch ExceptionInfo e
          []))))))
