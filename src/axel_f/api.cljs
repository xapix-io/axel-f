(ns axel-f.api
  (:require [axel-f.core :as axel-f]))

(defn ^:export compile [formula-str]
  (try
    (let [f (axel-f/compile formula-str)]
      (fn [ctx]
        (clj->js (f (js->clj ctx)))))
    (catch ExceptionInfo e
      (throw (js/Error. (js/JSON.stringify (clj->js (ex-data e))))))))
