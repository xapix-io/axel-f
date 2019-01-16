(ns axel-f.v2.reader)

(defmacro ^:private update! [what f]
  (list 'set! what (list f what)))

(defmulti newline? type)

(defmethod newline? Character [c]
  (contains? #{\return \newline} c))

(defmethod newline? :default [el]
  (throw (ex-info (str "Unknown element type " (type el))
                  {:element el})))

(defprotocol IReader
  (read-elem [reader])
  (read-until [reader pred])
  (peek-elem [reader])
  (analyze-tail [reader pred]))

(defprotocol IPushbackReader
  (unread-elem [reader elem]))

(defprotocol IIndexingReader
  (get-line-number [reader])
  (get-column-number [reader]))

(deftype Reader
    [col ^long col-len ^:unsynchronized-mutable ^long col-pos]
  IReader
  (read-elem [reader]
    (let [r (nth col col-pos ::default)]
      (when-not (= ::default r)
        (update! col-pos inc)
        r)))
  (peek-elem [reader]
    (let [r (nth col col-pos ::default)]
      (when-not (= ::default r)
        r)))
  (analyze-tail [reader pred]
    (let [begin (peek-elem reader)]
      (when-not (= begin ::default)
        (loop [pos col-pos el begin]
          (cond
            (= ::default el) true
            (not (pred el)) false
            :otherwise (recur (inc pos) (nth col (inc pos) ::default))))))))

(deftype PushbackReader
    [^Reader rdr ^"[Ljava.lang.Object;" buf ^long buf-len ^:unsynchronized-mutable ^long buf-pos]
  IReader
  (read-elem [reader]
    (if (< buf-pos buf-len)
      (let [r (aget buf buf-pos)]
        (update! buf-pos inc)
        r)
      (read-elem rdr)))
  (peek-elem [reader]
    (if (< buf-pos buf-len)
      (aget buf buf-pos)
      (peek-elem rdr)))
  (analyze-tail [reader pred]
    (analyze-tail rdr pred))

  IPushbackReader
  (unread-elem [reader elem]
    (when elem
      (if (zero? buf-pos) (throw (RuntimeException. "Pushback buffer is full")))
      (update! buf-pos dec)
      (aset buf buf-pos elem))))

(deftype IndexingPushbackReader
    [rdr ^:unsynchronized-mutable ^long line ^:unsynchronized-mutable ^long column
     ^:unsynchronized-mutable line-start?
     ^:unsynchronized-mutable prev ^:unsynchronized-mutable ^long prev-column]
  IReader
  (read-elem [reader]
    (when-let [el (read-elem rdr)]
      (set! prev line-start?)
      (set! line-start? (newline? el))
      (when line-start?
        (set! prev-column column)
        (set! column 0)
        (update! line inc))
      (update! column inc)
      el))

  (peek-elem [reader]
    (peek-elem rdr))

  IPushbackReader
  (unread-elem [reader ch]
    (if line-start?
      (do (update! line dec)
          (set! column prev-column))
      (update! column dec))
    (set! line-start? prev)
    (unread-elem rdr ch))

  IIndexingReader
  (get-line-number [reader] (int line))
  (get-column-number [reader] (int column)))

(defn read-until [reader pred]
  (loop [acc [] r (peek-elem reader)]
    (if (pred r)
      acc
      (recur (conj acc (read-elem reader)) (peek-elem reader)))))

(defn reader [s-or-coll]
  (Reader. s-or-coll (count s-or-coll) 0))

(defn push-back-reader [rdr]
  (PushbackReader. rdr (object-array 1000) 1000 1000))

(defn indexing-push-back-reader [^PushbackReader rdr]
  (IndexingPushbackReader. rdr 1 1 true nil 0))

(comment

  (let [r (-> "qwe\newq" reader push-back-reader indexing-push-back-reader)]
    (prn (peek-elem r) " - " (get-line-number r) ":" (get-column-number r))
    (loop [c (read-elem r)]
      (when c
        (prn c " - " (get-line-number r) ":" (get-column-number r))
        (when-not (newline? c)
          (unread-elem r \v))
        (prn (read-elem r) " - " (get-line-number r) ":" (get-column-number r))
        (recur (read-elem r)))))



  )
