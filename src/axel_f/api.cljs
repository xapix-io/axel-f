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
    (let [{:keys [used-references]} (axel-f/analyze formula-str)]
      (clj->js used-references))
    (catch ExceptionInfo e
      (throw (js/Error. (js/JSON.stringify (clj->js
                                            {:message (.-message e)
                                             :data (ex-data e)})))))))
