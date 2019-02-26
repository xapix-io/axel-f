(ns axel-f.api
  (:require [axel-f.core :as axel-f]))

(defn ^:export compile [formula-str]
  (try
    (let [f (axel-f/compile formula-str)]
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

(defn ^:export context [formula-str]
  (try
    (let [{:keys [vars]} (axel-f/analyze formula-str)]
      (clj->js vars))
    (catch ExceptionInfo e
      (throw (js/Error. (js/JSON.stringify (clj->js
                                            {:message (.-message e)
                                             :data (ex-data e)})))))))

(defn ^:export autocomplete [incomplete-formula context]
  (let [context (js->clj context)]
    (clj->js (axel-f/suggestions incomplete-formula context))))
