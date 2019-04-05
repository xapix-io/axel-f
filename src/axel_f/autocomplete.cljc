(ns axel-f.autocomplete
  (:require [axel-f.analyzer :as analyzer]
            [axel-f.lexer :as lexer]
            [axel-f.parser :as parser]
            [axel-f.runtime :as runtime]
            [axel-f.functions.core :as functions]
            [clojure.string :as string]
            [clj-fuzzy.metrics :as fuzzy])
  #?(:clj (:import [clojure.lang ExceptionInfo])))

(defn get-base-context [ref context]
  (if ref
    (case (runtime/type ref)
      ::runtime/root-reference-expr context
      ::runtime/reference-expr (runtime/eval* (::runtime/ctx-expr ref) context nil nil))
    context))

(defn get-last-field [ref context]
  (if ref
    (runtime/eval* (::runtime/field-expr ref) context nil nil)
    ""))

(defn next-fields [context]
  (cond
    (map? context)
    (keys context)

    (sequential? context)
    (distinct (mapcat keys context))))

(defn get-function-name [ref]
  (try
    (runtime/function-name ref)
    (catch ExceptionInfo e
      nil)))

(defn- ->string [x]
  (if (keyword? x)
    (str ":" (string/join "/" [(namespace x) (name x)]))
    x))

(defn- fuzzy-match? [ex dict]
  (if ex
    (let [ex (->string ex)
          dict (map #(->string %) dict)]
      (->> dict
           (map #(hash-map :value %
                           :distance (if (string/starts-with? (string/lower-case %)
                                                              (string/lower-case ex))
                                       0
                                       (fuzzy/jaccard ex %))))
           (filter #(< (:distance %) 0.6))
           (sort-by :distance)
           (map :value)))
    dict))

(defn get-similar-functions [fnname]
  (map (fn [f]
         (merge {:type :FN
                 :value f}
                (get-in @functions/*functions-store* [f :meta])))
       (fuzzy-match? fnname (->> @functions/*functions-store*
                                 (filter (fn [[_ {:keys [meta]}]] (not-empty meta)))
                                 (map first)))))

(defn get-similar-fields [field context]
  (map (fn [f]
         {:type :REF
          :value f
          :desc "Field in the context"})
       (fuzzy-match? field (next-fields context))))

(defn analyze-ref [ref context]
  (let [context (get-base-context ref context)
        field (get-last-field ref context)
        fnname (get-function-name ref)
        position (runtime/position (::runtime/field-expr ref))]
    (map #(assoc % :position position)
         (concat
          (when-not (and (sequential? context)
                         (nil? fnname))
            (get-similar-functions fnname))
          (get-similar-fields field context)))))

(defn subs-ref [tokens depth]
  (loop [acc [] [t & tokens'] tokens]
    (if (and t
             (or (lexer/punctuation-literal? t ["."])
                 (lexer/symbol-literal? t)
                 (lexer/bracket-literal? t ["[" "]"])
                 (lexer/operator-literal? t [":" "/"])
                 (> (::lexer/depth t) depth)
                 (lexer/end-of-input? t)))
      (recur (cons t acc) tokens')
      (let [acc' (if (lexer/end-of-input? (last acc))
                   (butlast acc)
                   acc)
            acc' (if (or (nil? (last acc'))
                         (lexer/punctuation-literal? (last acc') ["."]))
                   (concat acc' [#::lexer{:type ::lexer/symbol
                                          :value ""
                                          :depth (::lexer/depth (last acc))
                                          :begin (::lexer/end (last acc))
                                          :end (::lexer/end (last acc))}])
                   acc')]
        (first (parser/parse-primary acc'))))))

(defn analyze-fncall [f current-arg]
  (when-let [fnname (not-empty (get-function-name f))]
    (merge {:current-arg current-arg
            :type :FNCALL
            :value fnname}
           (get-in @functions/*functions-store* [fnname :meta]))))

(defn subs-fncall [tokens depth]
  (let [[args tokens'] (loop [acc [] [t & tokens'] tokens]
                         (if (or (not t)
                                 (and (lexer/bracket-literal? t ["("])
                                      (= (::lexer/depth t) (dec depth))))
                           [acc (drop (count acc) tokens)]
                           (recur (cons t acc) tokens')))
        current-arg (count (filter #(and (lexer/punctuation-literal? % [","])
                                         (= (::lexer/depth %) depth))
                                   args))
        fn-ref (subs-ref (next tokens') (dec depth))]
    [fn-ref current-arg]))

(defn suggestions [term context]
  (let [tokens (reverse (lexer/read-formula term false))
        depth (::lexer/depth (first tokens))]
    (let [ref (subs-ref tokens depth)
          fncall (subs-fncall tokens depth)]
      {:suggestions (analyze-ref ref context)
       :context (apply analyze-fncall fncall)})))
