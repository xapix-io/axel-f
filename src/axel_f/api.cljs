(ns axel-f.api
  (:require [axel-f.excel :as excel]))

(defn ^:export compile [formula-str]
  (try
    (let [f (excel/compile formula-str)]
      (fn [ctx]
        (try
          (clj->js (f (js->clj ctx)))
          (catch ExceptionInfo e
            (throw (js/Error. (js/JSON.stringify (clj->js
                                                  {:message (.-message e)
                                                   :data (ex-data e)}))))))))
    (catch ExceptionInfo e
      (throw (js/Error. (js/JSON.stringify (clj->js
                                            {:message (.-message e)
                                             :data (ex-data e)})))))))

(defn ^:export context [formula]
  (try
    (let [{:keys [free-variables]} (meta (if (fn? formula)
                                           formula
                                           (excel/compile formula)))]
      (clj->js free-variables))
    (catch ExceptionInfo e
      (throw (js/Error. (js/JSON.stringify (clj->js
                                            {:message (.-message e)
                                             :data (ex-data e)})))))))

(defn ^:export autocomplete [context]
  (let [context (js->clj context)]
    (axel-f/suggest-fn context)))
