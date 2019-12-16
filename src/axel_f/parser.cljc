(ns axel-f.parser
  (:refer-clojure :exclude [memoize])
  (:require [axel-f.lexer :as lexer]))

(defn- eof? [{::lexer/keys [type]}]
  (= type ::lexer/eof))

(defn- postfix? [{::lexer/keys [value] ::keys [type parts]}]
  (let [value (or value (and (= type ::var) (= 1 (count parts)) (::lexer/value (first parts))))]
    (= "%" value)))

(defn- prefix? [{::lexer/keys [value] ::keys [type parts]}]
  (let [value (or value (and (= type ::var) (= 1 (count parts)) (::lexer/value (first parts))))]
    (contains? (set ["!" "+" "-"]) value)))

(defn- punctuation?
  "Check if token matches desired symbol and has punctuation type
  Possible values: `[\".\" \",\" \"(\" \")\" \"[\" \"]\" \"{\" \"}\"]`"
  [{::lexer/keys [type value]} desired]
  (let [desired (if (set? desired) desired (set [desired]))]
    (and (= type ::lexer/punct)
         (contains? desired value))))

(defn- precedence [{::lexer/keys [value] ::keys [parts]}]
  ({"^" 30
    "*" 20 "/" 20
    "+" 10 "-" 10
    "<" 7 ">" 7 "<=" 7 ">=" 7 "<>" 7 "=" 7
    "&" 1
    ":" 0}
   (or value (-> parts first ::lexer/value))))

(defn- formula [expr]
  {::type ::formula
   ::body expr})

(defn- primary [t op args]
  (merge
   {::type ::primary
    ::operator op
    ::args args}
   (case t
     ::prefix {::lexer/begin (::lexer/begin op)
               ::lexer/end (::lexer/end (first args))}
     ::postfix {::lexer/begin (::lexer/begin (first args))
                ::lexer/end (::lexer/end op)}
     ::infix {::lexer/begin (::lexer/begin (first args))
              ::lexer/end (::lexer/end (second args))})))

(defn- var-part [t]
  (case [(::lexer/type t) (::type t)]
    [::lexer/symbol ::symbol] (if (= "_" (::lexer/value t))
                                :axel-f.runtime/context
                                (::lexer/value t))
    [nil ::list-ref] [::list-ref (::expr t)]
    t))

(defn- var*
  ([var-parts] (var* var-parts nil))
  ([var-parts var-cb]
   (let [parts (map var-part var-parts)]
     (when (fn? var-cb) (var-cb parts (select-keys (last var-parts) [::lexer/begin ::lexer/end])))
     {::type ::var
      ::parts parts
      ::lexer/begin (::lexer/begin (first var-parts))
      ::lexer/end (::lexer/end (last var-parts))})))

(defn- constant [const]
  (assoc const ::type ::constant))

(defn- symbol* [token]
  (assoc token
         ::type ::symbol
         ::lexer/type ::lexer/symbol))

(defn- list-ref [{::keys [type] ::lexer/keys [value] :as ref} begin end]
  (let [expr (case type
               ::constant value
               ::operator (if (= value "*")
                            ::select-all
                            (throw (ex-info "Invalid operator inside array reference expression."
                                            {:begin (::lexer/begin ref)})))
               ref)]
    {::type ::list-ref
     ::expr expr
     ::lexer/begin begin
     ::lexer/end end}))

(defn memoize [f]
  (let [store (atom {})]
    (fn [& args]
      (let [[token & _ :as tokens] (first args)]
        (if-let [res (get @store (cons token (rest args)))]
          res
          (let [res (apply f (cons tokens (rest args)))]
            (swap! store assoc (cons token (rest args)) res)
            res))))))

(defn first-of [& fs]
  (fn [tokens]
    (reduce (fn [_ f]
              (let [res (f tokens)]
                (when (first res)
                  (reduced res))))
            nil fs)))

(defn constant-parser []
  (memoize
   (fn [tokens]
     (let [[{::lexer/keys [type] :as token} & tokens'] tokens]
       (if (contains? #{::lexer/number ::lexer/string} type)
         [(constant token) tokens']
         [nil tokens])))))

(defn operator-parser []
  (memoize
   (fn [tokens]
     (let [[{::lexer/keys [type] :as token} & tokens'] tokens]
       (if (= ::lexer/operator type)
         [(assoc token ::type ::operator) tokens']
         [nil tokens])))))

(defn symbol-parser []
  (memoize
   (fn [tokens]
     (let [[{::lexer/keys [type] :as token} & tokens'] tokens]
       (if (= ::lexer/symbol type)
         [(symbol* token) tokens']
         [nil tokens])))))

(defn application-parser [parsers fncall-cb]
  (memoize
   (fn [tokens]
     (let [[{::keys [parts] :as fn-var} & tokens] tokens
           {parse-expression :expression
            parse-var :var} @parsers]
       (letfn [(parse-arguments [acc tokens]
                 (let [tokens (if (punctuation? (first tokens) #{"," "("})
                                (next tokens) tokens)]
                   (when (eof? (first tokens))
                     (fncall-cb parts (count acc))
                     (throw (ex-info "Unexpected end of input." {:begin (::lexer/begin (first tokens))})))
                   (if (punctuation? (first tokens) ")")
                     [acc (next tokens) (first tokens)]
                     (let [[arg-expr tokens'] (parse-expression tokens)]
                       (if arg-expr
                         (recur (conj acc arg-expr) tokens')
                         (do
                           (fncall-cb parts (count acc))
                           (throw (ex-info "Can not extract expression."
                                           {:begin (::lexer/begin (first tokens'))}))))))))]
         (let [[args tokens' end] (parse-arguments [] tokens)
               fn-call {::type ::application
                        ::function fn-var
                        ::args args
                        ::lexer/begin (::lexer/begin fn-var)
                        ::lexer/end (::lexer/end end)}]
           (if (punctuation? (first tokens') #{"." "["})
             (parse-var (cons fn-call tokens'))
             [fn-call tokens'])))))))

(defn var-parser [parsers var-cb]
  (memoize
   (fn [tokens]
     (let [{parse-application :application
            parse-symbol :symbol
            parse-square-block :square-block
            parse-constant :constant} @parsers
           [root & tokens'] tokens]
       (loop [acc [root] tokens' tokens']
         (if (punctuation? (first tokens') "(")
           (parse-application (cons (var* acc) tokens'))
           (let [[var-part tokens'']
                 (cond
                   (punctuation? (first tokens') ".")
                   ((first-of parse-symbol
                              parse-square-block
                              parse-constant)
                    (next tokens'))

                   (punctuation? (first tokens') "[")
                   (parse-square-block tokens'))]
             (if var-part
               (recur (conj acc var-part) tokens'')
               [(var*
                 (if (punctuation? (first tokens') ".")
                   (conj acc
                         {::lexer/type ::lexer/symbol
                          ::lexer/value ""
                          ::lexer/begin (::lexer/begin (second tokens'))
                          ::lexer/end (::lexer/begin (second tokens'))
                          ::type ::symbol})
                   acc)
                 (when (or (eof? (first tokens'))
                           (empty? (first tokens'))
                           (punctuation? (first tokens') "."))
                   var-cb))
                tokens']))))))))

(defn block-parser [parsers]
  (memoize
   (fn [tokens]
     (let [{parse-expression :expression} @parsers
           [token & tokens'] tokens]
       (if (punctuation? token "(")
         (let [[expr tokens''] (parse-expression tokens')
               begin (::lexer/begin (first tokens))
               end (::lexer/end (first tokens''))]
           (if (and expr (punctuation? (first tokens'') ")"))
             [(assoc expr ::lexer/begin begin ::lexer/end end)
              (next tokens'')]
             (cond
               (empty? expr)
               (throw (ex-info "Empty expression inside block." {:begin begin
                                                                 :end end}))

               (not (punctuation? (first tokens'') ")"))
               (throw (ex-info "Unclosed round bracket." {:begin begin
                                                          :end end})))))
         [nil tokens])))))

(defn square-block-parser [parsers]
  (memoize
   (fn [tokens]
     (let [{parse-expression :expression
            parse-operator :operator} @parsers]
       (if (punctuation? (first tokens) "[")
         (let [[parsed tokens'] (or (when (punctuation? (second tokens) "]")
                                      [nil (next tokens)])
                                    ((first-of parse-operator
                                               parse-expression)
                                     (next tokens)))
               begin (::lexer/begin (first tokens))
               end (::lexer/end (first tokens'))]
           (if (and parsed (punctuation? (first tokens') "]"))
             [(list-ref parsed begin end)
              (next tokens')]
             (cond
               (empty? parsed)
               [(list-ref {::lexer/value "*" ::type ::operator} begin end)
                (next tokens')]

               (eof? (first tokens'))
               (throw (ex-info "Unclosed square bracket." {:begin begin
                                                           :end end}))

               (not (punctuation? (first tokens') "]"))
               (throw (ex-info "Multiple expressions detected." {:begin begin
                                                                 :end end})))))
         [nil tokens])))))

(defn curly-block-parser [parsers]
  (memoize
   (fn [tokens]
     (let [{parse-expression :expression} @parsers]
       (if (punctuation? (first tokens) "{")
         (letfn [(parse-multiple [acc tokens]
                   (let [tokens (if (punctuation? (first tokens) #{"," "{"})
                                  (next tokens) tokens)]
                     (cond
                       (punctuation? (first tokens) "}")
                       [acc tokens]

                       (eof? (first tokens))
                       (throw (ex-info "Unexpected end of input" {:begin (::lexer/begin (first tokens))}))

                       :else
                       (let [[entry-expr tokens'] (parse-expression tokens)]
                         (if (empty? entry-expr)
                           (throw (ex-info "Unexpected token" {:begin (::lexer/begin (first tokens'))}))
                           (recur (conj acc entry-expr) tokens'))))))]
           (let [[entries tokens'] (parse-multiple [] tokens)
                 begin (::lexer/begin (first tokens))
                 end (::lexer/end (first tokens'))]
             [{::type ::list
               ::entries entries
               ::lexer/begin begin
               ::lexer/end end}
              (next tokens')]))
         [nil tokens])))))

(defn atom-parser [parsers var-cb]
  (memoize
   (fn [tokens]
     (let [{parse-constant :constant
            parse-atom :atom
            parse-var :var
            parse-symbol :symbol
            parse-block :block
            parse-curly-block :curly-block
            parse-square-block :square-block} @parsers]
       (or ((first-of (fn [tokens]
                        (let [res (parse-constant tokens)]
                          (when (first res)
                            (if (and (= ::lexer/string (::lexer/type (first res)))
                                     (punctuation? (first (second res)) #{"[" "." "("}))
                              (parse-atom (apply cons (update res 0 symbol*)))
                              res))))
                      (fn [[token & tokens']]
                        (when (punctuation? token #{"."})
                          (let [res (parse-atom tokens')]
                            (when (first res)
                              (case (::type (first res))
                                ::constant [(first (parse-var (update (vector (first res)) 0 symbol*)))
                                            (second res)]
                                ::var res
                                (parse-var (apply cons res)))))))
                      (fn [tokens]
                        (let [res (parse-symbol tokens)]
                          (when (first res)
                            (cond
                              (punctuation? (first (second res)) #{"[" "." "("})
                              (parse-var (apply cons res))

                              (contains? #{"TRUE" "True" "true" "FALSE" "False" "false" "NULL" "Null" "null"}
                                         (::lexer/value (first res)))
                              (update res 0 constant)

                              :else
                              (update-in res [0] (fn [x]
                                                   (var* [x] (when (let [t (-> res second first)]
                                                                     (or (eof? t)
                                                                         (empty? t)))
                                                               var-cb))))))))
                      parse-block
                      parse-curly-block
                      (fn [tokens]
                        (let [res (parse-square-block tokens)]
                          (when (first res)
                            (parse-var (apply cons res))))))
            tokens)
           [nil tokens])))))

(defn binary-parser [parsers]
  (memoize
   (fn
     ([tokens lexpr]
      (let [{parse-binary :binary} @parsers]
        (parse-binary tokens lexpr 0)))
     ([tokens lexpr prec]
      (let [{parse-operator :operator
             parse-atom :atom
             parse-binary :binary
             parse-prefix :prefix
             parse-postfix :postfix} @parsers
            [operator tokens'] (parse-operator tokens)]
        (if (or (empty? operator) (< (precedence operator) prec))
          [lexpr tokens]
          (let [[rexpr tokens''] ((first-of parse-prefix
                                            parse-postfix
                                            parse-atom)
                                  tokens')
                _ (when (empty? rexpr)
                    (throw (ex-info "Second argument for binary operator can not be parsed"
                                    {:begin (or (::lexer/begin (first tokens''))
                                                (::lexer/end operator))})))
                [rexpr tokens''] (parse-binary tokens'' rexpr (precedence operator))]
            (parse-binary tokens'' (primary ::infix operator [lexpr rexpr]) prec))))))))

(defn postfix-parser [parsers]
  (memoize
   (fn [tokens]
     (let [{parse-atom :atom
            parse-operator :operator} @parsers
           [operand tokens'] (parse-atom tokens)
           [operator tokens'] (parse-operator tokens')]
       (if (and operand operator (postfix? operator))
         [(primary ::postfix operator [operand]) tokens']
         [nil tokens])))))

(defn prefix-parser [parsers]
  (memoize
   (fn [tokens]
     (let [{parse-operator :operator
            parse-prefix :prefix
            parse-postfix :postfix
            parse-atom :atom} @parsers
           [operator tokens'] (parse-operator tokens)
           [operand tokens'] (when (and operator (prefix? operator))
                               (or ((first-of parse-prefix
                                              parse-postfix
                                              parse-atom)
                                    tokens')
                                   [nil tokens']))]
       (if operand
         [(primary ::prefix operator [operand]) tokens']
         [nil tokens])))))

(defn expression-parser [parsers]
  (memoize
   (fn [tokens]
     (let [{parse-prefix :prefix
            parse-postfix :postfix
            parse-atom :atom
            parse-binary :binary} @parsers
           [binary tokens']
           (let [[atom tokens']
                 ((first-of parse-prefix
                            parse-postfix
                            parse-atom)
                  tokens)]
             (parse-binary tokens' atom))]
       (if (not-empty binary)
         [binary tokens']
         [nil tokens])))))

(defn parser [{:keys [var-cb
                      fncall-cb]
               :or {var-cb (constantly nil)
                    fncall-cb (constantly nil)}}]
  (let [parsers (atom {})
        {parse-expression :expression}
        (swap! parsers assoc
               :constant (constant-parser)
               :symbol (symbol-parser)
               :operator (operator-parser)
               :var (var-parser parsers var-cb)
               :application (application-parser parsers fncall-cb)
               :block (block-parser parsers)
               :square-block (square-block-parser parsers)
               :curly-block (curly-block-parser parsers)
               :atom (atom-parser parsers var-cb)
               :binary (binary-parser parsers)
               :prefix (prefix-parser parsers)
               :postfix (postfix-parser parsers)
               :expression (expression-parser parsers))]
    (fn [tokens]
      (let [[expr tokens'] (parse-expression tokens)]
        (if (or (empty? tokens') (eof? (first tokens')))
          (formula expr)
          (throw (ex-info "Unexpected token" {:begin (::lexer/begin (first tokens'))})))))))

(defn parse [tokens & {:as opts}]
  ((parser opts) tokens))
