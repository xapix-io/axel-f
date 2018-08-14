(ns axel-f.api
  (:require [axel-f.core :as axel-f]))

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
       (clj->js (axel-f/run formula context))
       (catch ExceptionInfo e
         (throw (js/Error. (js/JSON.stringify (clj->js (fix-regex-in-exception
                                                        (ex-data e)))))))))))
