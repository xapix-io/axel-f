(ns axel-f.buddy.codecs.json
  #?(:clj (:require [cheshire.core :as json]
                    [cheshire.exact :as exact-json])))

(defn- update-keys [m f]
  (cond
    (map? m)
    (reduce-kv
     (fn [a k v]
       (assoc a (f k) (update-keys v f)))
     m
     m)

    (sequential? m)
    (reduce
     (fn [a v]
       (conj a (update-keys v f)))
     []
     m)

    :else m))

(defn generate-string [data]
  #?(:clj (json/generate-string data)
     :cljs (js/JSON.stringify (clj->js data))))

(defn parse-string
  ([s] (parse-string s identity))
  ([s key-fn]
   #?(:clj (exact-json/parse-string s key-fn)
      :cljs (let [key-fn (or (if (and (boolean? key-fn) key-fn) ::keywordize-keys key-fn) identity)
                  data (js->clj (js/JSON.parse s) :keywordize-keys (= key-fn ::keywordize-keys))]
              (if (= key-fn ::keywordize-keys)
                data
                (update-keys data key-fn))))))
