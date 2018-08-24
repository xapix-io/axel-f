(ns axel-f.functions
  (:require [clojure.string :as string]))


(defn sum-fn [& items]
  (reduce #?(:clj +' :cljs +) items))

(defn min-fn [& items]
  (reduce min items))

(defn max-fn [& items]
  (reduce max items))

(defn- excel-str [item]
  (case item
    true "TRUE"
    false "FALSE"
    (str item)))

(defn concatenate-fn [& items]
  (->> items
       (map excel-str)
       (apply str)))

(defn average-fn [& items]
  (let [len (count items)]
    (when-not (zero? len)
      (/ (apply sum-fn items)
         len))))

(defn round-fn
  ([d] (round-fn d 0))
  ([d precision]
   (let [precision (or precision 0)
         factor (Math/pow 10 precision)
         res (/ (Math/round (* d factor)) factor)]
     (if (> precision 0)
       res
       (int res)))))

(defn and-fn [& args]
  (every? identity args))

(defn or-fn [& args]
  (boolean (some identity args)))

(defn clean-fn
  "removes all nonprintable characters from text.

  Use CLEAN on text imported from other applications that contains
  characters that may not print with your operating system. For
  example, you can use CLEAN to remove some low-level computer code
  that is frequently at the beginning and end of data files and cannot
  be printed."
  [text]
  (string/replace text #"[0-x1F]" ""))

(defn code-fn
  "Returns a numeric code for the first character in a text string."
  [text]
  #?(:clj (-> text first int)
     :cljs (.charCodeAt text 0)))

(defn exact-fn
  "Compares two text strings and returns TRUE if they are exactly the same,
   FALSE otherwise. EXACT is case-sensitive but ignores formatting differences."
  [str1 str2]
  (= str1 str2))

(defn find-fn
  "FIND locate one text string within a second text string, and return the number
   of the starting position of the first text string from the first character of the
   second text string."
  ([substr str] (find-fn substr str 0))
  ([substr str from-index]
   (some-> str
           (string/index-of substr from-index)
           (inc))))

(defn left-fn
  "LEFT returns the first character or characters in a text string, based on the number
   of characters you specify."
  ([text] (left-fn text 1))
  ([text number] (subs text 0 number)))

(defn len-fn
  "LEN returns the number of characters in a text string."
  [text]
  (count text))

(defn lower-fn
  "Converts all uppercase letters in a text string to lowercase."
  [text]
  (string/lower-case text))

(defn mid-fn
  "MID returns a specific number of characters from a text string,
  starting at the position you specify, based on the number of characters you specify."
  [text start number]
  (let [text-end (count text)
        params-start (dec start)
        params-end (+ (dec start) number)
        start (if (> params-start text-end)
                text-end
                params-start)
        end (if (> params-end text-end)
              text-end
              params-end)]
    (subs text start end)))

(defn numbervalue-fn
  "Converts text to a number"
  [text]
  #?(:clj  (read-string text)
     :cljs (js/parseFloat text)))

(defn proper-fn
  "Capitalizes the first letter in a text string and any other letters
   in text that follow any character other than a letter.
   Converts all other letters to lowercase letters."
  [text]
  (string/replace (str text) #"\w*" string/capitalize))

(defn replace-fn
  "REPLACE replaces part of a text string, based on the number of characters
   you specify, with a different text string."
  [text position length new-text]
  (str (subs text 0 (dec position))
       new-text
       (subs text (+ (dec position) length))))

;; TODO: REGEX fns

(defn rept-fn
  "Repeats text a given number of times."
  [text number]
  (->> (constantly text)
       (repeatedly number)
       (apply str)))

(defn right-fn
  "RIGHT returns the last character or characters in a text string,
   based on the number of characters you specify."
  ([text] (right-fn text 1))
  ([text number]
   (subs text (- (count text)
                number))))

(defn search-fn
  "SEARCH returns the number of the character at which a specific character
   or text string is first found, beginning with start_num. Use SEARCH to determine
   the location of a character or text string within another text string so that
   you can use the MID or REPLACE functions to change the text."
  ([find-text within-text] (search-fn find-text within-text 0))
  ([find-text within-text position]
   (inc
    (string/index-of (string/lower-case within-text)
                     (string/lower-case find-text)
                     position))))

;; TODO: | seaprator doesn't work
(defn split-fn [text separator]
  (vec (.split text separator)))

(defn substitute-fn
  "Substitutes new_text for old_text in a text string.
   Use SUBSTITUTE when you want to replace specific text in a text string;
   use REPLACE when you want to replace any text that occurs in a specific location
   in a text string."
  ([text old-text new-text]
   (if (empty? old-text)
     text
     (string/replace text (re-pattern old-text) new-text)))
  ([text old-text new-text occurrence]
   (if (every? string? [text old-text new-text])
     (if (empty? old-text)
       text
       (loop [i 1
              index (string/index-of text old-text)]
         (if index
           (if (= i occurrence)
             (str (subs text 0 index)
                  new-text
                  (subs text (+ index (count old-text))))
             (recur (inc i)
                    (string/index-of text old-text (inc index))))
           text)))
     ;; TODO: emit error
     {:error "text should be string"})))


(defn trim-fn
  "Removes all spaces from text except for single spaces between words.
   Use TRIM on text that you have received from another application that may
   have irregular spacing."
  [text]
  (string/trim
   (string/replace text #"\ +" " ")))

(defn upper-fn
  "Converts text to uppercase."
  [text]
  (string/upper-case text))

(defn count-fn [& args]
  (count (flatten args)))


(def functions-map
  {"SUM"         {:impl #'sum-fn}
   "MIN"         {:impl #'min-fn}
   "MAX"         {:impl #'max-fn}
   "CONCATENATE" {:impl #'concatenate-fn}
   "AVERAGE"     {:impl #'average-fn}
   "ROUND"       {:impl #'round-fn}
   "AND"         {:impl #'and-fn}
   "OR"          {:impl #'or-fn}
   "CLEAN"       {:impl #'clean-fn}
   "CODE"        {:impl #'code-fn}
   "EXACT"       {:impl #'exact-fn}
   "FIND"        {:impl #'find-fn}
   "LEFT"        {:impl #'left-fn}
   "LEN"         {:impl #'len-fn}
   "LOWER"       {:impl #'lower-fn}
   "MID"         {:impl #'mid-fn}
   "NUMBERVALUE" {:impl #'numbervalue-fn}
   "PROPER"      {:impl #'proper-fn}
   "REPLACE"     {:impl #'replace-fn}
   "REPT"        {:impl #'rept-fn}
   "RIGHT"       {:impl #'right-fn}
   "SEARCH"      {:impl #'search-fn}
   "SPLIT"       {:impl #'split-fn}
   "SUBSTITUTE"  {:impl #'substitute-fn}
   "TRIM"        {:impl #'trim-fn}
   "UPPER"       {:impl #'upper-fn}
   "COUNT"       {:impl #'count-fn}
   "IF"          {:impl :special-form}
   "OBJREF"      {:impl :special-form}
   })
