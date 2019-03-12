(ns axel-f.lexer
  (:require [axel-f.reader :as reader]
            #?(:clj [clojure.edn :as edn])
            [clojure.string :as string]))

(defn get-position [rdr]
  {::line (reader/get-line-number rdr)
   ::column (reader/get-column-number rdr)})

(defn end-of-input? [{::keys [type] :as t}]
  (or (= type ::eoi)
      (nil? t)))

(defn whitespace? [{::keys [type] :as t}]
  (or (= type ::whitespace)
      (contains? #{\space \tab} t)))

(defn newline? [{::keys [type] :as t}]
  (or (= type ::newline)
      (contains? #{\newline} t)))

(defn text-literal? [{::keys [type] :as t}]
  (or (= type ::text)
      (contains? #{\' \"} t)))

(defn number-literal? [{::keys [type] :as t}]
  (or (= type ::number)
      (contains? (set "0123456789") t)))

(defn symbol-literal? [{::keys [type] :as t}]
  (or (= type ::symbol)
      (contains? #{\#} t)))

(defn punctuation-literal?
  ([t] (punctuation-literal? t ["," "."]))
  ([{::keys [type value] :as t} val]
   (or (and (= type ::punctuation)
            (contains? (set val) value))
       (contains? (set val) (str t)))))

(defn bracket-literal?
  ([t] (bracket-literal? t ["(" ")" "[" "]" "{" "}"]))
  ([{::keys [type value] :as t} val]
   (or (and (= type ::bracket)
            (contains? (set val) value))
       (contains? (set val) (str t)))))

(def prefix-operators [":" "+" "-" "!"])
(def infix-operators ["+" "-" "*" "/" "&" "=" "<" ">" "<=" ">=" "<>" "^"])
(def postfix-operators ["%"])
(def operators (distinct (concat prefix-operators infix-operators postfix-operators)))

(defn operator-literal?
  ([t] (operator-literal? t operators))
  ([{::keys [type value] :as t} val]
   (or (and (= type ::operator)
            (contains? (set val) value))
       (contains? (set val) (str t)))))

(defn prefix-operator? [{::keys [value] :as el}]
  (operator-literal? el prefix-operators))

(defn infix-operator? [{::keys [value] :as el}]
  (operator-literal? el infix-operators))

(defn postfix-operator? [{::keys [value] :as el}]
  (operator-literal? el postfix-operators))

(defn append-bracket [brackets {::keys [value begin] :as bracket} throw?]
  (if-let [bracket' (last brackets)]
    (let [value' (::value bracket')
          begin' (::begin bracket')]
      (if (contains? #{"()" "[]" "{}"} (str value' value))
        (vec (butlast brackets))
        (conj brackets bracket)))
    (if (and throw? (bracket-literal? value [")" "}" "]"]))
      (throw (ex-info "Unexpected closing bracket"
                      {:position {:begin begin
                                  :end begin}}))
      (conj brackets bracket))))

(defn clean-escaped-string [s]
  (string/replace s #"\\(.)" "$1"))

(defmulti read-token! (fn [rdr]
                        (let [ch (reader/peek-elem rdr)]
                          (cond
                            (end-of-input? ch) ::eoi
                            (whitespace? ch) ::whitespace
                            (newline? ch) ::newline
                            (text-literal? ch) ::text
                            (number-literal? ch) ::number
                            (punctuation-literal? ch) ::punctuation
                            (bracket-literal? ch) ::bracket
                            (operator-literal? ch) ::operator
                            :otherwise ::symbol))))

(defmethod read-token! ::eoi [rdr]
  {::type ::eoi
   ::begin (get-position rdr)
   ::end (get-position rdr)})

(defmethod read-token! ::whitespace [rdr]
  (let [begin (get-position rdr)]
    (loop [acc []]
      (if (whitespace? (reader/peek-elem rdr))
        (recur (conj acc (reader/read-elem rdr)))
        {::type ::whitespace
         ::value (apply str acc)
         ::begin begin
         ::end (get-position rdr)}))))

(defmethod read-token! ::newline [rdr]
  (let [begin (get-position rdr)]
    (loop []
      (if (newline? (reader/peek-elem rdr))
        (do (reader/read-elem rdr)
            (recur))
        {::type ::newline
         ::begin begin
         ::end (get-position rdr)}))))

(defmethod read-token! ::text [rdr]
  (let [begin (get-position rdr)
        text-literal (reader/peek-elem rdr)]
    (loop [acc []]
      (let [fc (reader/peek-elem rdr)
            end (get-position rdr)]
        (cond
          (and (empty? acc)
               (text-literal? fc))
          (recur (conj acc (reader/read-elem rdr)))

          (= \\ fc)
          (let [_ (reader/read-elem rdr)
                fc (reader/peek-elem rdr)]
            (if (end-of-input? fc)
              (throw (ex-info "Unexpected end of string"
                              {:position {:begin (get-position rdr)
                                          :end (get-position rdr)}}))
              (recur (conj acc (reader/read-elem rdr)))))

          (= text-literal fc)
          (do (reader/read-elem rdr)
              {::value (apply str (rest acc))
               ::type ::text
               ::begin begin
               ::end end})

          (end-of-input? fc)
          (throw (ex-info "Unexpected end of string"
                          {:position {:begin (get-position rdr)
                                      :end (get-position rdr)}}))

          :otherwise
          (recur (conj acc (reader/read-elem rdr))))))))

(defmethod read-token! ::number [rdr]
  (let [begin (get-position rdr)]
    (loop [acc [] end begin number-pattern #"[0-9]+"]
      (let [ch (reader/peek-elem rdr)
            end' (get-position rdr)]
        (cond
          (punctuation-literal? ch ["."])
          (recur (conj acc (reader/read-elem rdr)) end' #"[0-9]+\.[0-9]+")

          (contains? (set "eE") ch)
          (let [acc (conj acc (reader/read-elem rdr))
                end' (get-position rdr)
                nch (reader/read-elem rdr)]
            (recur (conj acc nch) end'
                   (if (contains? (set "+-") nch)
                     #"[0-9]+\.?[0-9]*(e|E)(\+|-)[0-9]+"
                     #"[0-9]+\.?[0-9]*(e|E)[0-9]+")))

          (and (number-literal? ch)
               (re-matches number-pattern (apply str (conj acc ch))))
          (recur (conj acc (reader/read-elem rdr)) end' number-pattern)

          :otherwise
          (let [n (apply str acc)]
            (cond
              (and (re-matches number-pattern n)
                   (or (not ch)
                       (operator-literal? ch)
                       (punctuation-literal? ch)
                       (bracket-literal? ch)
                       (whitespace? ch)))
              {::value (#?(:clj edn/read-string
                           :cljs js/parseFloat) n)
               ::type ::number
               ::begin begin
               ::end end}

              (re-matches #"[0-9]+\." n)
              (do
                (reader/unread-elem rdr (last acc))
                {::value (apply str (butlast acc))
                 ::type ::symbol
                 ::begin begin
                 ::end (update (get-position rdr) ::column dec)})

              :otherwise
              (do
                (doseq [c (reverse acc)]
                  (reader/unread-elem rdr c))
                ((#?(:clj .getMethod
                     :cljs get-method) read-token! ::symbol) rdr)))))))))

(defmethod read-token! ::punctuation [rdr]
  (let [begin (get-position rdr)]
    {::value (str (reader/read-elem rdr))
     ::type ::punctuation
     ::begin begin
     ::end begin}))

(defmethod read-token! ::bracket [rdr]
  (let [begin (get-position rdr)]
    {::value (str (reader/read-elem rdr))
     ::type ::bracket
     ::begin begin
     ::end begin}))

(defmethod read-token! ::operator [rdr]
  (let [begin (get-position rdr)
        op1 (reader/read-elem rdr)
        end (get-position rdr)
        op2 (when (contains? #{"<=" ">=" "<>"} (str op1 (reader/peek-elem rdr)))
              (reader/read-elem rdr))]
    {::value (str op1 op2)
     ::type ::operator
     ::begin begin
     ::end (if op2 end begin)}))

(defmethod read-token! ::symbol [rdr]
  (let [begin (get-position rdr)]
    (loop [acc [] escaped? false end (get-position rdr)]
      (let [ch (reader/peek-elem rdr)]
        (cond
          (= \# ch)
          (let [_ (reader/read-elem rdr)
                s ((get-method read-token! ::text) rdr)]
            (assoc s ::type ::symbol ::begin begin))

          (and escaped? (end-of-input? ch))
          (throw (ex-info "Unexpected end of token"
                          {:position {:begin end
                                      :end end}}))

          (and (not escaped?)
               (or (nil? ch)
                   (whitespace? ch)
                   (newline? ch)
                   (text-literal? ch)
                   (punctuation-literal? ch)
                   (bracket-literal? ch)
                   (operator-literal? ch ["/"])))
          {::value (clean-escaped-string (apply str acc))
           ::type ::symbol
           ::begin begin
           ::end end}

          :otherwise
          (let [end' (get-position rdr)]
            (let [ch (reader/read-elem rdr)]
              (recur (conj acc ch)
                     (if (= \\ ch)
                       (not escaped?)
                       false)
                     end'))))))))

(defn read-formula* [rdr tokens brackets-heap throw?]
  (let [init-position {::line 0 ::column 0}
        token (read-token! rdr)
        brackets-heap' (if (bracket-literal? token)
                         (append-bracket brackets-heap token throw?)
                         brackets-heap)
        old-depth (count brackets-heap)
        new-depth (count brackets-heap')
        depth (if (<= old-depth new-depth) old-depth new-depth)
        token (assoc token ::depth depth)]
    (if (end-of-input? token)
      (if (and throw? (not-empty brackets-heap))
        (throw (ex-info "Unbalanced brackets"
                        {:position {:begin (::begin (last brackets-heap))
                                    :end (get-position rdr)}}))
        (conj tokens token))
      (recur rdr (conj tokens token) brackets-heap' throw?))))

(defn read-formula
  ([s] (read-formula s true))
  ([s throw?]
   (let [rdr (-> s reader/reader reader/push-back-reader reader/indexing-push-back-reader)]
     (read-formula* rdr [] [] throw?))))
