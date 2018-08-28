(ns axel-f.functions.text
  (:require [axel-f.error :as error]
            [clojure.string :as string]
            [axel-f.functions.coercion :as coercion]
            [axel-f.functions.math :as math]
            #?@(:cljs
                [[goog.string :as gstring]
                 [goog.string.format]])))

(defn- regex-escape [pattern]
  (let [cmap {\. "\\."
              \+ "\\+"
              \| "\\|"
              \* "\\*"
              \^ "\\^"
              \[ "\\["
              \] "\\]"
              \( "\\("
              \) "\\)"}]
    (if (char? pattern)
      (or (get cmap pattern) pattern)
      (string/escape pattern cmap))))

;; TODO
;; (defn arabic-fn [roman-numeral])

;; TODO
;; (defn asc-fn [])

(defn char-fn [number]
  (if (number? number)
    (if (< 0 number 65536)
      #?(:clj (-> number char str)
         :cljs (js/String.fromCharCode number))
      (throw (error/error "#NUM!" (str "Function CHAR parameter 1 value " number " is out of range."))))
    (throw (error/error "#VALUE!" (str "Function CHAR parameter 1 expects number values. But '" number "' is a text.")))))

(defn code-fn [text]
  #?(:clj (-> text first int)
     :cljs (.charCodeAt text 0)))

(defn dollar-fn
  ([number] (dollar-fn number 2))
  ([number number-of-places]
   (cond
     (not (number? number))
     (throw (error/error "#VALUE!" "Function DOLLAR parameter 1 expects number values."))

     (not (number? number-of-places))
     (throw (error/error "#VALUE!" "Function DOLLAR parameter 2 expects number values."))

     :otherwise
     (let [number (double (math/round-fn number number-of-places))
           fmt (if (< number-of-places 0)
                 "%.0f"
                 (str "%." number-of-places "f"))]
       (str "$" #?(:clj (format fmt number)
                   :cljs (gstring/format fmt number)))))))

(defn exact-fn [str1 str2]
  (= str1 str2))

(defn find-fn
  ([substr str] (find-fn substr str 0))
  ([substr str from-index]
   (some-> str
           (string/index-of substr from-index)
           (inc))))

;; TODO
;; (defn fixed-fn [])

(defn join-fn [delimeter & items]
  (->> items
      flatten
      (map coercion/excel-str)
      (string/join delimeter)))

(defn left-fn
  ([text] (left-fn text 1))
  ([text number]
   (if (> number (count text))
     text
     (subs text 0 number))))

(defn len-fn [text]
  (let [text (cond
               (string? text) text
               (seqable? text) (first text))]
    (count (coercion/excel-str text))))

(defn lower-fn [text]
  (string/lower-case (coercion/excel-str text)))

(defn mid-fn [text start number]
  (if (and (number? start) (number? number))
    (let [text (coercion/excel-str text)
          text-end (count text)
          params-start (dec start)
          params-end (+ (dec start) number)
          start (if (> params-start text-end)
                  text-end
                  params-start)
          end (if (> params-end text-end)
                text-end
                params-end)]
      (subs text start end))
    (throw (error/error "#VALUE!" "Function MID parameter 1 and 2 expects number values."))))

(defn proper-fn [text]
  (string/replace (coercion/excel-str text) #"\w*" string/capitalize))

;; TODO
;; (defn regexextract-fn [])

;; TODO
;; (defn regexmatch-fn [])

;; TODO
;; (defn regexreplace-fn [])

(defn replace-fn
  [text position length new-text]
  (if (and (number? position)
           (number? length))
    (str (subs text 0 (dec position))
         (coercion/excel-str new-text)
         (subs text (+ (dec position) length)))
    (throw (error/error "#VALUE!" "Function REPLACE parameters 2 and 3 expects number values."))))

(defn rept-fn
  [text number]
  (->> (constantly text)
      (repeatedly number)
      (apply str)))

(defn right-fn
  ([text] (right-fn text 1))
  ([text number]
   (if (<= (count text) number)
     text
     (subs text (- (count text)
                   number)))))

(defn roman-fn [n]
  (if (and (number? n)
           (<= 0 n 3999))
    (let [n (int n)
          alphabet (sort-by val >
                            {\I   1   \V   5   \X   10   \L   50
                             \C   100 \D   500 \M   1000 "IV" 4
                             "IX" 9   "XL" 40  "XC" 90   "CD" 400
                             "CM" 900})]
      (loop [res "" n n]
        (if (zero? n) res
            (let [[rom arab] (some #(when (<= (val %) n) %) alphabet)]
              (recur (str res rom) (- n arab))))))
    (throw (error/error "#VALUE!" (str "Function ROMAN parameter 1 value is " n ". Valid values are between 1 and 3999 inclusive.")))))

(defn search-fn
  ([find-text within-text] (search-fn find-text within-text 0))
  ([find-text within-text position]
   (inc
    (string/index-of (string/lower-case within-text)
                     (string/lower-case find-text)
                     position))))

(defn split-fn [text delimeter & [split-by-each & [remove-empty-text]]]
  (keep
   (if (or (nil? remove-empty-text)
           remove-empty-text)
     not-empty
     identity)
   (string/split (coercion/excel-str text)
                 (re-pattern (if split-by-each
                               (string/join "|" (map regex-escape
                                                     (vec (coercion/excel-str delimeter))))
                               (regex-escape (coercion/excel-str delimeter))))
                 -1)))

(defn- substitute-fn* [text old-text new-text occurrence]
  (if (empty? old-text)
    text
    (if (nil? occurrence)
      (string/replace text (re-pattern old-text) new-text)
      (loop [i 1
             index (string/index-of text old-text)]
        (if index
          (if (= i occurrence)
            (str (subs text 0 index)
                 new-text
                 (subs text (+ index (count old-text))))
            (recur (inc i) (string/index-of text old-text (inc index))))
          text)))))

(defn substitute-fn [& args]
  (cond

    (and (nth args 3 nil)
         (not (integer? (nth args 3))))
    (throw (error/error "#VALUE!" (str "Function SUBSTITUTE parameter 4 expects number values. But '" (nth args 3) "' is a text and cannot be coerced to a number.")))

    :otherwise
    (let [[text old-text new-text & [occurrence]] args]
      (substitute-fn*
       (coercion/excel-str text)
       (coercion/excel-str old-text)
       (coercion/excel-str new-text)
       occurrence))))

;; TODO
;; (defn t-fn [])

;; TODO
;; (defn text-fn [])

(defn trim-fn [& args]
  (string/trim
   (string/replace (-> args first coercion/excel-str) #"\ +" " ")))

(defn upper-fn [& args]
  (-> args
      first
      coercion/excel-str
      string/upper-case))

;; TODO
;; (defn value-fn [])

;; TODO
;; (defn textjoin-fn [])
