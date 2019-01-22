(ns axel-f.v2.lexer
  (:require #?(:clj [clojure.edn :as edn])
            [clojure.string :as string]
            [axel-f.v2.reader :as reader]))

(defn get-position [rdr]
  {:line (reader/get-line-number rdr)
   :column (reader/get-column-number rdr)})

(defn closing? [bracket]
  (contains? #{")" "]" "}" \) \] \}} bracket))

(defn opening? [bracket]
  (contains? #{"(" "[" "{" \( \[ \{} bracket))

(defn append-bracket [brackets {:keys [value begin] :as bracket}]
  (if-let [bracket' (last brackets)]
    (let [value' (:value bracket')
          begin' (:begin bracket')]
      (if (contains? #{"()" "[]" "{}"} (str value' value))
        (vec (butlast brackets))
        (if (closing? value)
          (throw (ex-info "Unbalanced brackets"
                          {:position [begin' begin]}))
          (conj brackets bracket))))
    (if (closing? value)
      (throw (ex-info "Unexpected closing bracket"
                      {:position begin}))
      (conj brackets bracket))))

(defn end-of-input? [{:keys [kind] :as t}]
  (or (= kind ::eoi)
      (nil? t)))

(defn whitespace? [{:keys [kind] :as t}]
  (or (= kind ::whitespace)
      (contains? #{\space \tab \return \newline} t)))

(defn newline? [{:keys [kind] :as t}]
  (or (= kind ::newline)
      (contains? #{\return \newline} t)))

(defn text-literal? [{:keys [kind] :as t}]
  (or (= kind ::text)
      (contains? #{\' \"} t)))

(defn number-literal? [{:keys [kind] :as t}]
  (or (= kind ::number)
      (contains? (set "0123456789") t)))

(defn symbol-literal? [{:keys [kind]}]
  (= kind ::symbol))

(defn punctuation-literal?
  ([t] (punctuation-literal? t ["." ","]))
  ([{:keys [kind value] :as t} val]
   (or (and (= kind ::punctuation)
            (contains? (set val) value))
       (contains? (set val) (str t)))))

(defn bracket-literal?
  ([t] (bracket-literal? t ["(" ")" "[" "]" "{" "}"]))
  ([{:keys [kind value] :as t} val]
   (or (and (= kind ::bracket)
            (contains? (set val) value))
       (contains? (set val) (str t)))))

(defn operator-literal?
  ([t] (operator-literal? t [":" "+" "-" "*" "/" "&" "<" "<=" ">" ">=" "<>" "=" "!" "%" "^"]))
  ([{:keys [kind value] :as t} val]
   (or (and (= kind ::operator)
            (contains? (set val) value))
       (contains? (set val) (str t)))))

(defn prefix-operator? [{:keys [value] :as el}]
  (operator-literal? el [":" "+" "-" "!"]))

(defn infix-operator? [{:keys [value] :as el}]
  (operator-literal? el ["+" "-" "*" "/" "&" "=" "<" ">" "<=" ">=" "<>" "^"]))

(defn postfix-operator? [{:keys [value] :as el}]
  (operator-literal? el ["%"]))

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

(defmethod read-token! ::eoi [_]
  {:kind ::eoi})

(defmethod read-token! ::whitespace [rdr]
  (loop []
    (if (whitespace? (reader/peek-elem rdr))
      (do (reader/read-elem rdr)
          (recur))
      {:kind ::whitespace})))

(defmethod read-token! ::newline [rdr]
  (loop []
    (if (newline? (reader/peek-elem rdr))
      (do (reader/read-elem rdr)
          (recur))
      {:kind ::newline})))

(defmethod read-token! ::text [rdr]
  (let [begin (get-position rdr)
        text-literal (reader/read-elem rdr)]
    (loop [acc [] quote? false]
      (let [end (get-position rdr)
            ch (reader/read-elem rdr)]
        (cond
          (end-of-input? ch)
          (throw (ex-info "Unexpected end of string"
                          {:position (get-position rdr)}))

          (= \\ ch)
          (recur (conj acc ch) (not quote?))

          (and (= text-literal ch)
               (not quote?))
          {:value (clean-escaped-string (apply str acc))
           :kind ::text
           :begin begin
           :end end}

          :otherwise
          (recur (conj acc ch) false))))))

(defmethod read-token! ::integer [rdr]
  (loop [acc [] end nil]
    (if (number-literal? (reader/peek-elem rdr))
      (let [end (get-position rdr)]
        (recur (conj acc (reader/read-elem rdr)) end))
      [(apply str acc) end])))

(defmethod read-token! ::signed-integer [rdr]
  (let [sign (when (contains? (set "+-") (reader/peek-elem rdr))
               (reader/read-elem rdr))
        [int-part end] (if (number-literal? (reader/peek-elem rdr))
                         ((get-method read-token! ::integer) rdr)
                         (do (reader/unread-elem rdr sign)
                             nil))]
    (when int-part
      [(str sign int-part) end])))

(defmethod read-token! ::number [rdr]
  (let [begin (get-position rdr)
        [int-part end] ((get-method read-token! ::integer) rdr)
        [float-part end'] (when (= \. (reader/peek-elem rdr))
                            (do (reader/read-elem rdr)
                                ((get-method read-token! ::integer) rdr)))
        [exp-part end''] (when (contains? (set "eE") (reader/peek-elem rdr))
                           (do (reader/read-elem rdr)
                               (or ((get-method read-token! ::signed-integer) rdr)
                                   (throw (ex-info "Invalid number format"
                                                   {:position (get-position rdr)})))))]
    {:value (#?(:clj edn/read-string
                :cljs js/parseFloat)
             (str int-part
                  (when float-part (str "." float-part))
                  (when exp-part (str "e" exp-part))))
     :kind ::number
     :begin begin
     :end (or end'' end' end)}))

(defmethod read-token! ::punctuation [rdr]
  (let [begin (get-position rdr)]
    {:value (str (reader/read-elem rdr))
     :kind ::punctuation
     :begin begin
     :end begin}))

(defmethod read-token! ::bracket [rdr]
  (let [begin (get-position rdr)]
    {:value (str (reader/read-elem rdr))
     :kind ::bracket
     :begin begin
     :end begin}))

(defmethod read-token! ::operator [rdr]
  (let [begin (get-position rdr)
        op1 (reader/read-elem rdr)
        end (get-position rdr)
        op2 (when (contains? #{"<=" ">=" "<>"} (str op1 (reader/peek-elem rdr)))
              (reader/read-elem rdr))]
    {:value (str op1 op2)
     :kind ::operator
     :begin begin
     :end (if op2 end begin)}))

(defmethod read-token! ::symbol [rdr]
  (let [begin (get-position rdr)]
    (loop [acc [] escaped? false end (get-position rdr)]
      (let [ch (reader/peek-elem rdr)]
        (cond
          (and escaped? (end-of-input? ch))
          (throw (ex-info "Unexpected end of token"
                          {:position end}))

          (and (not escaped?)
               (or (nil? ch)
                   (whitespace? ch)
                   (newline? ch)
                   (text-literal? ch)
                   (punctuation-literal? ch)
                   (bracket-literal? ch)
                   (operator-literal? ch)))
          {:value (clean-escaped-string (apply str acc))
           :kind ::symbol
           :begin begin
           :end end}

          :otherwise
          (let [end' (get-position rdr)]
            (let [ch (reader/read-elem rdr)]
              (recur (conj acc ch)
                     (if (= \\ ch)
                       (not escaped?)
                       false)
                     end'))))))))

(defn read-formula* [rdr tokens brackets-heap]
  (let [token (read-token! rdr)
        brackets-heap' (if (bracket-literal? token)
                         (append-bracket brackets-heap token)
                         brackets-heap)
        old-depth (count brackets-heap)
        new-depth (count brackets-heap')
        depth (if (<= old-depth new-depth) old-depth new-depth)]
    (if (end-of-input? token)
      (if (empty? brackets-heap)
        tokens
        (throw (ex-info "Unbalanced brackets"
                        {:position [(:begin (last brackets-heap))
                                    (get-position rdr)]})))
      (recur rdr (if (whitespace? token)
                   tokens
                   (conj tokens (assoc token :depth depth))) brackets-heap'))))

(defn read-formula [s]
  (let [rdr (-> s reader/reader reader/push-back-reader reader/indexing-push-back-reader)]
    (read-formula* rdr [] [])))

(comment

  (read-formula "foo[* ].bar[1] = 1")

  (read-formula "F(1,2,3)")

  (parse-formula "foo[*].bar[1 ].'bar' = 2
bar.baz = 4")

  (read-formula ":f\\+.b/b +")

  (read-formula "foo.bar.baz1 = 1
foo.bar.baz2 = 3")

  (read-formula ":foo.bar/baz.booz")

  (parse-expression (read-formula "1"))

  (let [r (reader/indexing-push-back-reader "qwe\r\newq")]
    (loop [c (reader/read-elem r)]
      (when c
        (prn c)
        (recur (reader/read-elem r)))))

  )
