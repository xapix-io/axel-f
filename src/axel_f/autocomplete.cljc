(ns axel-f.autocomplete
  (:require [clojure.string :as string]
            [axel-f.core :as axel-f]
            [axel-f.functions :as functions]))

(defn- valid-reference? [ref]
  (try
    (let [ref (axel-f/compile ref)]
      (and (seqable? ref) (= (first ref) :OBJREF)))
    (catch clojure.lang.ExceptionInfo _
      false)))

(defn- fix-up-fncall [fncall]
  (let [fncall (string/replace fncall #",[ ]*$" "")]
    (str fncall
         (when-not (string/ends-with? fncall ")")
           ")"))))

(defn- valid-fncall? [fncall]
  (try
    (let [fncall (axel-f/compile (fix-up-fncall fncall))]
      (and (seqable? fncall) (= (first fncall) :FNCALL)))
    (catch #?(:clj clojure.lang.ExceptionInfo
              :cljs js/Error) _
      false)))

(defn- build-suggestion [type item & [args]]
  (merge {:type type}
         (case type
           :OBJREF {:value (axel-f/->string item)
                    :description "Field in the context"}
           :FN (merge {:value item}
                      (select-keys (get functions/functions-map item)
                                   [:description :args]))
           :FNCALL (merge {:value item
                           :current-arg (dec (count args))}
                          (select-keys (get functions/functions-map item)
                                       [:description :args])))))

(defn- build-suggestions-for-objref [objref context]
  (concat (->> functions/functions-map
              keys
              (filter #(string/starts-with? % objref))
              (map #(build-suggestion :FN %)))
          (let [[_ & fields] (axel-f/compile objref)]
            (if (> (count fields) 1)
              (let [known-path (butlast fields)
                    maybe-path (last fields)]
                (cond
                  (= maybe-path "*") (->> (axel-f/run (string/join "." known-path)
                                           context)
                                         (filter map?)
                                         (apply merge)
                                         keys
                                         (map #(build-suggestion :OBJREF %)))
                  :otherwise (let [new-context (axel-f/run (string/join "." known-path)
                                                 context)]
                               (cond
                                 (map? new-context) (->> new-context
                                                        keys
                                                        (map axel-f/->string)
                                                        (filter #(string/starts-with? % maybe-path))
                                                        (map #(build-suggestion :OBJREF %)))))))
              (->> context
                  keys
                  (map axel-f/->string)
                  (filter #(string/starts-with? % (first fields)))
                  (map #(build-suggestion :OBJREF %)))))))

(defn- build-suggestions-for-fncall [fncall]
  (let [[_ f args] (axel-f/compile (fix-up-fncall fncall))]
    (->> functions/functions-map
        keys
        (filter #(string/starts-with? % f))
        (map #(build-suggestion :FNCALL % args)))))

(defn get-last-part [incomplete-formula]
  (let [chx (string/split incomplete-formula #"")]
    (loop [ch (last chx) chx (butlast chx) acc [] terms {:open-round 0
                                                         :close-round 0
                                                         :pre-terminate false
                                                         :terminate false}]
      (if (or (not ch) (:terminate terms))
        (apply str acc)
        (let [terms (cond-> terms
                      (= ")" ch) (update :close-round inc)
                      (= "(" ch) (update :open-round inc)
                      (= "," ch) (#(if (valid-reference? (apply str acc))
                                     (assoc % :terminate true)
                                     %))
                      (re-matches #"[A-Z]" ch) (#(if (>= (:open-round %) (:close-round %))
                                                   (assoc % :pre-terminate true)
                                                   %))
                      (not (re-matches #"[A-Z]" ch)) (#(if (:pre-terminate %)
                                                         (assoc % :terminate true)
                                                         %)))]
          (if (:terminate terms)
            (apply str acc)
            (recur (last chx) (butlast chx) (cons ch acc) terms)))))))

(defn autocomplete
  ([incomplete-formula] (autocomplete incomplete-formula {}))
  ([incomplete-formula context]
   (let [last-part (get-last-part incomplete-formula)]
     (cond
       (valid-reference? last-part) (build-suggestions-for-objref last-part context)
       (valid-fncall? last-part)    (build-suggestions-for-fncall last-part)))))

(comment

  (autocomplete "SU.bar[*]" {:SU {:bar [{:baz 1} {:buz 2}]}})

  (autocomplete "S" {:SU {:bar [{:baz 1} {:buz 2}]}})

  (autocomplete "SUM(1,2" {:SU {:bar [{:baz 1} {:buz 2}]}})

  )
