(ns axel-f.lexer
  (:refer-clojure :exclude [read])
  (:require [clojure.edn :as edn]
            [clojure.string :as string]
            [clojure.set :as set]))

(defn- whitespace? [{::keys [v]}]
  (contains? #{\space \tab \newline} v))

(defn- newline? [{::keys [v] :as e}]
  (= \newline (or v e)))

(defn- number-literal? [{::keys [v]}]
  (contains? (set "0123456789") v))

(defn- string-literal? [{::keys [v]}]
  (contains? (set "\"'#") v))

(defn- comment-literal? [{::keys [v]}]
  (= \; v))

(defn- punctuation-literal? [{::keys [v]}]
  (contains? (set ".,()[]{}") v))

(defn- operator-literal? [{::keys [v]}]
  (contains? (set ":+-*/&=<>^!%") v))

(defn- eof? [{::keys [v]}]
  (nil? v))

(defn- escape-char? [{::keys [v type]}]
  (= type ::escaped))

(defn- escape-str [args]
  (string/replace (apply str args) #"\\(.)" "$1"))

(defmulti read-next* (fn [[e & ex]]
                       (cond
                         (whitespace? e) ::whitespace
                         (number-literal? e) ::number-literal
                         (string-literal? e) ::string-literal
                         (punctuation-literal? e) ::punctuation-literal
                         (operator-literal? e) ::operator-literal
                         (comment-literal? e) ::comment
                         (eof? e) ::eof
                         :otherwise ::symbol-literal)))

(defmethod read-next* ::whitespace [ex]
  (loop [e (first ex) ex ex]
    (if (whitespace? e)
      (let [ex (next ex)]
        (recur (first ex) ex))
      [nil ex])))

(defn- read-natural [[e & ex]]
  (loop [acc [] e' e ex' ex ex'' ex]
    (if (number-literal? e')
      (recur (conj acc e') (first ex') (next ex') ex')
      [acc ex''])))

(defmethod read-next* ::number-literal [ex]
  (let [{::keys [l c]} (first ex)]
    (loop [acc [] ex ex part ::natural]
      (let [[acc' ex']
            (update-in
             (case part

               ::natural
               (read-natural ex)

               ::fract
               (if (= \. (::v (first ex)))
                 (update-in (read-natural (next ex)) [0] (partial cons (first ex)))
                 ['() ex])

               ::scientific
               (if (contains? (set "eE") (::v (first ex)))
                 (let [signed? (contains? (set "+-") (::v (second ex)))]
                   (update-in (read-natural (if signed? (nnext ex) (next ex)))
                              [0]
                              (partial concat (if signed? (list (first ex) (second ex)) (list (first ex))))))
                 ['() ex]))
             [0] (partial concat acc))
            next-number-part ({::natural ::fract
                               ::fract ::scientific}
                              part)]
        (if next-number-part
          (recur acc' ex' next-number-part)
          (let [{ec ::c el ::l} (last acc')]
            [{::type ::number
              ::value (edn/read-string (->> acc' (map ::v) (apply str)))
              ::begin {::line l
                       ::col c}
              ::end {::line el
                     ::col ec}}
             ex']))))))

(defmethod read-next* ::string-literal [ex]
  (let [ex (if (= \# (-> ex first ::v)) (next ex) ex)
        {literal-t ::v
         line ::l
         col ::c} (first ex)]
    (loop [acc [] {::keys [v l c] :as e} (second ex) ex (nnext ex)]
      (if (and (= v literal-t)
               (not (escape-char? (last acc))))
        [{::type ::string
          ::value (->> acc (map ::v) escape-str)
          ::begin {::line line
                   ::col col}
          ::end {::line l
                 ::col c}}
         ex]
        (if (eof? e)
          (throw (ex-info "Unexpected end of string" {:begin (set/rename-keys (select-keys e [::l ::c])
                                                                              {::l ::line
                                                                               ::c ::col})}))
          (recur (conj acc (if (and (= \\ (::v e))
                                    (not (escape-char? (last acc))))
                             (assoc e ::type ::escaped)
                             e))
                 (first ex)
                 (next ex)))))))

(defmethod read-next* ::punctuation-literal [[{::keys [v l c]} & ex]]
  [{::type ::punct
    ::value (str v)
    ::begin {::line l
             ::col c}
    ::end {::line l
           ::col c}}
   ex])

(defn wrap-keyword [kw-parts begin]
  (let [[ns [_ & n]] (split-with #(not= "/" (::value %))
                                 kw-parts)]
    (merge {::type ::symbol
            ::end (::end (last kw-parts))
            ::begin begin}
           {::value (apply keyword
                           (filter not-empty
                                   (map (fn [n]
                                          (string/join
                                           "."
                                           (sequence
                                            (comp
                                             (filter #(not= "." (::value %)))
                                             (map ::value))
                                            n)))
                                        [ns n])))})))

(defn read-keyword [[{::keys [v l c] :as e} & _ :as ex] begin]
  (let [read-symbol (get-method read-next* ::symbol-literal)]
    (loop [acc [] state [::namespace ::begin] ex ex]
      (case state
        [::namespace ::begin]
        (let [[s ex'] (read-next* ex)]
          (if (= ::symbol (::type s))
            (recur (conj acc s) [::namespace ::sep] ex')
            [nil ex]))

        [::namespace ::symbol]
        (let [[s ex'] (read-next* ex)]
          (recur (conj acc s) [::namespace ::sep] ex'))

        [::namespace ::sep]
        (let [[p ex'] (read-next* ex)]
          (cond
            (and (= ::punct (::type p))
                 (= "." (::value p)))
            (recur (conj acc p) [::namespace ::symbol] ex')

            (and (= ::operator (::type p))
                 (= "/" (::value p)))
            (recur (conj acc p) [::name ::symbol] ex')

            :else
            [(wrap-keyword acc begin) ex]))

        [::name ::symbol]
        (let [[s ex'] (read-next* ex)]
          (if (= ::symbol (::type s))
            [(wrap-keyword (conj acc s) begin) ex']
            (throw (ex-info "Namespaced keyword must have a name" {:begin begin
                                                                   :end (::end (last acc))}))))))))

(defmethod read-next* ::operator-literal [[{::keys [v l c]} & ex]]
  (if (= v \:)
    (let [[kw ex'] (read-keyword ex {::line l ::col c})]
      (if (some? kw)
        [kw ex']
        [{::type ::operator
          ::value (str v)
          ::begin {::line l
                   ::col c}
          ::end {::line l
                 ::col c}}
         ex]))
    (let [compound? (contains? (set ["<=" ">=" "<>"]) (str v (::v (first ex))))
          {v' ::v l' ::l c' ::c} (first ex)
          op (str v (when compound? v'))]
      [{::type ::operator
        ::value op
        ::begin {::line l
                 ::col c}
        ::end {::line (if compound? l' l)
               ::col (if compound? c' c)}}
       (if compound? (next ex) ex)])))

(defmethod read-next* ::symbol-literal [ex]
  (let [{::keys [l c]} (first ex)]
    (loop [acc [] ex ex]
      (let [{::keys [v] :as e'} (first ex)]
        (if (and (or (empty? ex)
                     (whitespace? e')
                     (contains? (set ",.[](){}/") v)
                     (eof? e'))
                 (not (escape-char? (last acc))))
          (let [{l' ::l c' ::c} (last acc)]
            [{::type ::symbol
              ::value (->> acc (map ::v) escape-str)
              ::begin {::line l
                       ::col c}
              ::end {::line l'
                     ::col c'}}
             ex])
          (recur (conj acc (if (and (= \\ (::v e'))
                                    (not (escape-char? (last acc))))
                             (assoc e' ::type ::escaped)
                             e'))
                 (next ex)))))))

(defn- drop-inline-comment [ex line]
  (if (= line (::l (first ex)))
    (recur (next ex) line)
    ex))

(defn- drop-block-comment [ex]
  (let [f (::v (first ex))
        s (::v (second ex))]
    (cond
      (= "~;" (str f s))
      (nnext ex)

      (not s)
      (throw (ex-info "Unclosed comment block"
                      {:begin (select-keys
                               (set/rename-keys (first ex)
                                                {::l ::line
                                                 ::c ::col})
                               [::line ::col])}))

      :otherwise
      (recur (next ex)))))

(defmethod read-next* ::comment [[{::keys [l]} & ex]]
  (let [{::keys [v]} (first ex)]
    [nil (if (= \~ v)
           (drop-block-comment (next ex))
           (drop-inline-comment (next ex) l))]))

(defmethod read-next* ::eof [[{::keys [l c]}]]
  [{::type ::eof
    ::begin {::line l
             ::col c}
    ::end {::line l
           ::col c}} nil])

(defn- str->stream
  ([s] (str->stream s 0 1))
  ([s col line]
   (lazy-seq
    (if-let [e (first s)]
      (cons {::v e
             ::l line
             ::c (inc col)}
            (str->stream (next s)
                         (if (newline? e) 0 (inc col))
                         (if (newline? e) (inc line) line)))
      (list {::l line
             ::c (inc col)})))))

(defn- read-next [tokens]
  (lazy-seq
   (let [[token tokens] (read-next* tokens)]
     (cond
       (= (::type token) ::eof)
       (list token)

       token
       (cons token (read-next tokens))

       :otherwise
       (read-next tokens)))))

(defn read [s]
  (read-next (str->stream s)))
