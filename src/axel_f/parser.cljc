(ns axel-f.parser
  (:require [axel-f.lexer :as lexer]
            [axel-f.runtime :as runtime]
            [clojure.string :as string]))

(defn triml-whitespaces [tokens]
  (if (lexer/whitespace? (first tokens))
    (recur (rest tokens))
    tokens))

(defn trimr-whitespaces [tokens]
  (if (lexer/whitespace? (last tokens))
    (recur (butlast tokens))
    tokens))

(defn trim-whitespaces [tokens]
  (-> tokens
      trimr-whitespaces
      triml-whitespaces))

(declare parse-expression parse-primary)

(defn parse-keyword [tokens]
  (loop [acc [] [token & tokens'] tokens]
    (cond
      (or (lexer/operator-literal? token [":" "/"])
          (lexer/punctuation-literal? token ["."]))
      (recur (conj acc token) tokens')

      (lexer/symbol-literal? token)
      (if (lexer/operator-literal? (last acc) ["/"])
        (cons (runtime/root-reference-expr
               (runtime/constant-expr
                {::lexer/value (keyword (when-let [ns-part (not-empty (map ::lexer/value (butlast (rest acc))))]
                                          (apply str ns-part))
                                        (::lexer/value token))}))
              tokens')
        (recur (conj acc token) tokens')))))

(defn parse-group [[token & tokens]]
  (let [tokens' (take-while (fn [t]
                              (not (and (lexer/bracket-literal? t [")"])
                                        (= (::lexer/depth t)
                                           (::lexer/depth token)))))
                            tokens)
        [expr _] (parse-expression tokens')]
    (cons expr (drop (inc (count tokens')) tokens))))

(defn parse-array [[token & tokens] close-symbol]
  (let [tokens' (take-while (fn [t]
                              (not (and (lexer/bracket-literal? t [close-symbol])
                                        (= (::lexer/depth t)
                                           (::lexer/depth token)))))
                            tokens)
        delimeter-depth (inc (::lexer/depth token))]
    (cons (runtime/list-expr (->> tokens'
                                  (partition-by (fn [t]
                                                  (and (lexer/punctuation-literal? t [","])
                                                       (= delimeter-depth (::lexer/depth t)))))
                                  (filter (fn [t]
                                            (not (and (= 1 (count t))
                                                      (lexer/punctuation-literal? (first t) [","])))))
                                  (map parse-expression)
                                  (map first))
                             token
                             (first (drop (count tokens') tokens)))
          (drop (inc (count tokens')) tokens))))

(defn parse-symbol [[token & tokens]]
  (cond
    (or (lexer/punctuation-literal? (first tokens) ["."])
        (lexer/bracket-literal? (first tokens) ["["]))
    (parse-primary (cons (runtime/root-reference-expr (runtime/constant-expr token))
                         tokens))

    (and (contains? #{"true" "false"} (string/lower-case (::lexer/value token)))
         (not (or (lexer/punctuation-literal? (first tokens) ["."])
                  (lexer/bracket-literal? (first tokens) ["["]))))
    (cons (runtime/constant-expr
           {::lexer/value (let [s (string/lower-case (::lexer/value token))]
                            (case s
                              "true" true
                              "false" false))})
          tokens)

    :otherwise
    (cons (runtime/root-reference-expr (runtime/constant-expr token))
          tokens)))

(defn parse-reference* [root-expr [token & tokens]]
  (if token
    (cond
      (lexer/punctuation-literal? token ["."])
      (let [token' (first tokens)]
        (cond
          (or (lexer/symbol-literal? token')
              (lexer/text-literal? token')
              (lexer/number-literal? token'))
          (parse-reference*
           (runtime/reference-expr root-expr
                                   (runtime/constant-expr token'))
           (rest tokens))

          (lexer/bracket-literal? token' ["["])
          (parse-reference* root-expr
                            tokens)

          (nil? token')
          (throw (ex-info "Unexpected end of reference expression"
                          {:position {:begin (:begin (runtime/position root-expr))
                                      :end (::lexer/end token)}}))))

      (lexer/bracket-literal? token ["["])
      (let [d (::lexer/depth token)
            tokens' (take-while #(< d (::lexer/depth %)) tokens)
            tokens'' (trim-whitespaces tokens')]
        (if (and (lexer/operator-literal? (first tokens'') ["*"])
                 (= 1 (count tokens'')))
          (parse-reference* (runtime/index-expr root-expr
                                                (runtime/operator-expr (first tokens''))
                                                token
                                                (first (drop (count tokens') tokens)))
                            (drop (inc (count tokens')) tokens))
          (let [[expr rest-tokens] (parse-expression tokens')]
            ;; TODO rest-tokens must be `empty`
            (parse-reference* (runtime/index-expr root-expr
                                                  expr
                                                  token
                                                  (first (drop (count tokens') tokens)))
                              (drop (inc (count tokens')) tokens))))))
    root-expr))

(defn parse-reference [[root-reference & tokens]]
  (let [d (::lexer/depth (first tokens))]
    (loop [acc [] [token & tokens'] tokens]
      (if (or (lexer/end-of-input? token)
              (and (= d (::lexer/depth token))
                   (or (lexer/whitespace? token)
                       (lexer/bracket-literal? token ["("])
                       (lexer/operator-literal? token)
                       (lexer/punctuation-literal? token [","]))))
        (cons (parse-reference* root-reference acc) (cons token tokens'))
        (recur (conj acc token) tokens')))))

(defn parse-function-call [ref-expr tokens]
  (let [[args & tokens'] (parse-array tokens ")")]
    (cons (runtime/application-expr ref-expr
                                    args)
          tokens')))

(defn parse-primary [tokens]
  (let [tokens (triml-whitespaces tokens)
        [token & tokens'] tokens]
    (cond
      (lexer/operator-literal? token [":"])
      (let [[root-reference-expr & rest-tokens] (parse-keyword tokens)]
        (parse-primary (cons root-reference-expr rest-tokens)))

      (lexer/prefix-operator? token)
      (let [op (runtime/operator-expr token)
            [expr' & tokens''] (parse-primary tokens')]
        (cons (runtime/unary-expr op expr') tokens''))

      (lexer/bracket-literal? token ["{"])
      (parse-array tokens "}")

      (lexer/bracket-literal? token ["("])
      (parse-group tokens)

      (lexer/bracket-literal? token ["["])
      (parse-reference (cons nil tokens))

      (lexer/symbol-literal? token)
      (parse-primary (parse-symbol tokens))

      (and (runtime/expr? token)
           (= (runtime/type token) ::runtime/root-reference-expr))
      (let [tokens (parse-reference tokens)
            [ref-expr & tokens'] tokens]
        (if (lexer/bracket-literal? (first tokens') ["("])
          (parse-function-call ref-expr tokens')
          tokens))

      (or (lexer/number-literal? token)
          (lexer/text-literal? token))
      (cons (runtime/constant-expr token) tokens')

      (runtime/expr? token)
      tokens)))

(defn parse-expression [tokens]
  (let [[expr & tokens'] (parse-primary tokens)
        tokens' (triml-whitespaces tokens')]
    (cond
      (lexer/infix-operator? (first tokens'))
      (let [op (runtime/operator-expr (first tokens'))
            [expr' tokens''] (parse-expression (rest tokens'))]
        [(if (and (runtime/binary? expr')
                  (< (runtime/precedence (runtime/operator expr'))
                     (runtime/precedence op)))
           (runtime/binary-expr (runtime/operator expr')
                                (runtime/binary-expr op expr (runtime/left expr'))
                                (runtime/right expr'))
           (runtime/binary-expr op expr expr'))
         tokens''])

      (lexer/postfix-operator? (first tokens'))
      [(runtime/unary-expr (runtime/operator-expr (first tokens')) expr)
       (rest tokens')]

      :otherwise
      [expr tokens'])))

(defn parse [tokens]
  (let [[expr [token & _]] (parse-expression tokens)]
    (if (not (lexer/end-of-input? token))
      (throw (ex-info "Unexpected token"
                      {:position (select-keys token [::lexer/begin ::lexer/end])}))
      (runtime/formula-expr expr))))
