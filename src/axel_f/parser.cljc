(ns axel-f.parser
  (:refer-clojure :exclude [memoize])
  (:require [axel-f.lexer :as lexer]
            [clojure.string :as string]))

(declare ^:dynamic *store*)
(declare parse-expression
         parse-square-block
         parse-atom
         parse-binary
         maybe-expression
         maybe-square-block)

(defn memoize [f store-key]
  (fn [& args]
    (if-let [res (get-in @*store* [store-key (cons (ffirst args) (next args))])]
      res
      (let [res (apply f args)]
        (vswap! *store* assoc-in [store-key (cons (ffirst args) (next args))] res)
        res))))

(defn- eof? [{::lexer/keys [type] :as token}]
  (= type ::lexer/eof))

(defn- postfix? [{::lexer/keys [value] ::keys [type parts] :as token}]
  (let [value (or value (and (= type ::var) (= 1 (count parts)) (::lexer/value (first parts))))]
    (= "%" value)))

(defn- prefix? [{::lexer/keys [value] ::keys [type parts] :as op}]
  (let [value (or value (and (= type ::var) (= 1 (count parts)) (::lexer/value (first parts))))]
    (contains? (set ["!" "+" "-"]) value)))

(defn- punctuation?
  "Check if token matches desired symbol and has punctuation type
  Possible values: `[\".\" \",\" \"(\" \")\" \"[\" \"]\" \"{\" \"}\"]`"
  [{::lexer/keys [type value]} desired]
  (let [desired (if (set? desired) desired (set [desired]))]
    (and (= type ::lexer/punct)
         (contains? desired value))))

(defn- operator?
  "Check if token matches desired symbol and has operator type
  Possible values: `[\":\" \"+\" \"-\" \"!\" \"*\" \"/\" \"&\" \"=\" \"<\" \">\" \"<=\" \">=\" \"<>\" \"^\" \"%\"]`"
  [{::lexer/keys [type value]} desired]
  (let [desired (if (set? desired) desired (set [desired]))]
    (and (= type ::lexer/operator)
         (contains? desired value))))

(defn- symbol*? [{parser-type ::type
                  lexer-type ::lexer/type}]
  (or (= ::symbol parser-type)
      (= ::lexer/symbol lexer-type)))

(defn- precedence [{::lexer/keys [value] ::keys [parts]}]
  ({"^" 30
    "*" 20 "/" 20
    "+" 10 "-" 10
    "<" 7 ">" 7 "<=" 7 ">=" 7 "<>" 7 "=" 7
    "&" 1
    ":" 0}
   (or value (-> parts first ::lexer/value))))

(defn- formula [exprs]
  {::type ::formula
   ::body exprs})

(defn- binary [left right op]
  {::type ::binary
   ::left left
   ::right right
   ::operator op})

(defn- postfix [operand op]
  {::type ::postfix
   ::operand operand
   ::operator op})

(defn- prefix [{::lexer/keys [value length] :as operand} {::lexer/keys [line col] :as op}]
  {::type ::prefix
   ::operand operand
   ::operator op})

(defn- var* [var-parts]
  {::type ::var
   ::parts var-parts})

(defn- constant [const]
  (assoc const ::type ::constant))

(defn- symbol* [token]
  (assoc token
         ::type ::symbol
         ::lexer/type ::lexer/symbol))

(defn- list-ref [ref]
  {::type ::list-ref
   ::expr ref})

(defn parse-constant* [tokens]
  (let [[{::lexer/keys [type] :as token} & tokens'] tokens]
    (if (contains? #{::lexer/number ::lexer/string} type)
      [(constant token) tokens']
      [nil tokens])))

(def parse-constant (memoize parse-constant* ::constant))

(defn maybe-constant [tokens]
  (let [res (parse-constant tokens)
        [const _] res]
    (when const res)))

(defn parse-application* [tokens fn-var]
  (letfn [(parse-arguments [acc tokens]
            (let [tokens (if (punctuation? (first tokens) #{"," "("})
                           (next tokens) tokens)]
              (if (punctuation? (first tokens) ")")
                [acc (next tokens)]
                (let [[arg-expr tokens'] (parse-expression tokens)]
                  (recur (conj acc arg-expr) tokens')))))]
    (let [[args tokens] (parse-arguments [] tokens)]
      [{::type ::application
        ::function fn-var
        ::args args}
       tokens])))

(def parse-application (memoize parse-application* ::application))

(defn parse-symbol* [tokens]
  (let [[{::lexer/keys [type] :as token} & tokens'] tokens]
    (if (= ::lexer/symbol type)
      [(symbol* token) tokens']
      [nil tokens])))

(def parse-symbol (memoize parse-symbol* ::symbol))

(defn maybe-symbol [tokens]
  (let [res (parse-symbol tokens)
        [sym _] res]
    (when sym res)))

(defn parse-operator* [tokens]
  (let [[{::lexer/keys [type] :as token} & tokens'] tokens]
    (if (= ::lexer/operator type)
      [(var* [(assoc token ::type ::symbol)]) tokens']
      [nil tokens])))

(def parse-operator (memoize parse-operator* ::operator))

(defn maybe-operator [tokens]
  (let [res (parse-operator tokens)
        [op _] res]
    (when op res)))

(defn parse-kw* [tokens]
  (let [{::lexer/keys [line col] :as op} (first tokens)
        {::lexer/keys [value length]} (second tokens)]
    (if (operator? op ":")
      (let [kw (string/split value #"/" 2)]
        [{::lexer/type ::keyword
          ::lexer/value (apply keyword (filter identity kw))
          ::lexer/line line
          ::lexer/col col
          ::lexer/length (inc length)
          ::type ::symbol}
         (nnext tokens)])
      [nil tokens])))

(def parse-kw (memoize parse-kw* ::kw))

(defn maybe-kw [tokens]
  (let [res (parse-kw tokens)
        [kw _] res]
    (when kw res)))

(defn parse-var* [tokens]
  (let [[root & tokens'] tokens]
    (loop [acc [root] tokens' tokens']
      (if (punctuation? (first tokens') "(")
        (parse-application tokens' (var* acc))
        (let [[var-part tokens'']
              (cond
                (punctuation? (first tokens') ".")
                (or (maybe-symbol (next tokens'))
                    (maybe-square-block (next tokens'))
                    (maybe-kw (next tokens'))
                    (maybe-constant (next tokens')))

                (punctuation? (first tokens') "[")
                (parse-square-block tokens'))]
          (if var-part
            (recur (conj acc var-part) tokens'')
            [(var* acc) tokens']))))))

(def parse-var (memoize parse-var* ::var))

(defn parse-block* [tokens]
  (let [[_ & tokens'] tokens
        [expr tokens''] (parse-expression tokens')]
    (if (and expr (punctuation? (first tokens'') ")"))
      [expr (next tokens'')]
      [nil tokens])))

(def parse-block (memoize parse-block* ::block))

(defn maybe-block [tokens]
  (when (punctuation? (first tokens) "(")
    (let [res (parse-block tokens)
          [block _] res]
      (when block res))))

(defn parse-square-block* [tokens]
  (let [[parsed tokens'] (or (let [[_ tokens' :as kw] (maybe-kw (next tokens))]
                               (when kw
                                 (if-not (punctuation? (first tokens') "]")
                                   (maybe-expression (next tokens))
                                   kw)))
                             (maybe-operator (next tokens))
                             (maybe-expression (next tokens))
                             [nil tokens])]
    (if parsed
      (if-not (punctuation? (first tokens') "]")
        (throw (ex-info "Unexpected token" (first tokens')))
        [(list-ref parsed) (next tokens')])
      [nil tokens])))

(def parse-square-block (memoize parse-square-block* ::square-block))

(defn maybe-square-block [tokens]
  (when (punctuation? (first tokens) "[")
    (let [res (parse-square-block tokens)
          [block _] res]
      (when block res))))

(defn parse-curly-block* [tokens]
  (letfn [(parse-multiple [acc tokens]
            (let [tokens (if (punctuation? (first tokens) #{"," "{"})
                           (next tokens) tokens)]
              (if (punctuation? (first tokens) "}")
                [acc (next tokens)]
                (let [[entry-expr tokens'] (parse-expression tokens)]
                  (recur (conj acc entry-expr) tokens')))))]
    (let [[entries tokens] (parse-multiple [] tokens)]
      [{::type ::list
        ::entries entries}
       tokens])))

(def parse-curly-block (memoize parse-curly-block* ::curly-block))

(defn maybe-curly-block [tokens]
  (when (punctuation? (first tokens) "{")
    (let [res (parse-curly-block tokens)
          [block _] res]
      (when block res))))

(defn parse-atom* [tokens]
  (or (when-let [res (maybe-constant tokens)]
        (if (and (= ::lexer/string (::lexer/type (first res)))
                 (punctuation? (first (second res)) #{"[" "." "("}))
          (parse-atom (apply cons (update res 0 symbol*)))
          res))
      (when-let [res (maybe-kw tokens)]
        (parse-var (apply cons res)))
      (when-let [res (maybe-symbol tokens)]
        (cond
          (punctuation? (first (second res)) #{"[" "." "("})
          (parse-var (apply cons res))

          (contains? #{"TRUE" "True" "true" "FALSE" "False" "false" "NULL" "Null" "null"}
                     (::lexer/value (first res)))
          (update res 0 constant)

          :otherwise
          (update-in res [0] (fn [x]
                               (var* (cond
                                       (map? x) [x]
                                       (sequential? x) x))))))
      (maybe-block tokens)
      (maybe-curly-block tokens)
      (when-let [res (maybe-square-block tokens)]
        (parse-var (apply cons res)))
      [nil tokens]))

(def parse-atom (memoize parse-atom* ::atom))

(defn parse-binary*
  ([tokens lexpr] (parse-binary* tokens lexpr 0))
  ([tokens lexpr prec]
   (let [[operator tokens'] (parse-operator tokens)]
     (if (or (empty? operator) (< (precedence operator) prec))
       [lexpr tokens]
       (let [[rexpr tokens''] (parse-atom tokens')
             [rexpr tokens''] (parse-binary tokens'' rexpr (precedence operator))]
         (parse-binary tokens'' (binary lexpr rexpr operator) prec))))))

(def parse-binary (memoize parse-binary* ::binary))

(defn maybe-binary [tokens lexpr]
  (when lexpr
    (let [[b _ :as res] (parse-binary tokens lexpr)]
      (when b res))))

(defn parse-postfix* [tokens]
  (let [[operand tokens'] (parse-atom tokens)
        [operator tokens'] (parse-operator tokens')]
    (if (and operand operator (postfix? operator))
      [(postfix operand operator) tokens']
      [nil tokens])))

(def parse-postfix (memoize parse-postfix* ::postfix))

(defn maybe-postfix [tokens]
  (let [res (parse-postfix tokens)
        [post _] res]
    (when post res)))

(defn parse-prefix* [tokens]
  (let [[operator tokens'] (parse-operator tokens)
        [operand tokens'] (when (and operator (prefix? operator)) (maybe-expression tokens'))]
    (if operand
      (let [parsed (prefix operand operator)]
        (if (symbol*? parsed)
          (parse-var (cons parsed tokens'))
          [parsed tokens']))
      [nil tokens])))

(def parse-prefix (memoize parse-prefix* ::prefix))

(defn maybe-prefix [tokens]
  (let [res (parse-prefix tokens)
        [pref _] res]
    (when pref res)))

(defn parse-expression* [tokens]
  (let [[binary tokens']
        (let [[atom tokens'] (or (maybe-prefix tokens)
                                 (maybe-postfix tokens)
                                 (parse-atom tokens))]
          (maybe-binary tokens' atom))]
    (if (not-empty binary)
      [binary tokens']
      [nil tokens])))

(def parse-expression (memoize parse-expression* ::expression))

(defn maybe-expression [tokens]
  (let [res (parse-expression tokens)
        [expr _] res]
    (when expr res)))

(defn parse [tokens]
  ;; TODO parse exactly one expression. throw an expression when non-eof token(s)
  (binding [*store* (volatile! {})]
    (loop [acc [] tokens tokens]
      (if (or (empty? tokens) (eof? (first tokens)))
        (formula acc)
        (let [[expr tokens'] (parse-expression tokens)]
          (recur (conj acc expr) tokens'))))))
