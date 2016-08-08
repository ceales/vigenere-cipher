(ns vigenere-cipher.core)

(def alphabet-size 27)

(defn encode-char [c]
  (let [ascii (int c)]
    (if (and (>= ascii (int \a))
             (<= ascii (int \z)))
      (- ascii (int \a))
      (if (= c \space)
        26))))

(defn decode-char [e]
  (if (and (>= e 0)
           (<= e 25))
    (char (+ e (int \a)))
    (if (= e 26)
      \space)))

(defn encode-string [s]
  (map encode-char s))
(encode-string "hello")

(defn decode-string [s]
  (apply str (map decode-char s)))
(decode-string (encode-string "she sells sea shells on the sea shore"))

(defn encode-key [key]
  (cycle (encode-string key)))

(defn infinite-encrypt [s k]
  (map (fn [x y] (mod (+ x y) alphabet-size)) (encode-string s) (encode-key k))) 

(defn encrypt [s k]
  (decode-string (take (count s) (infinite-encrypt s k))))

(defn inverse-key [k]
  (decode-string (map (fn [x] (mod (- alphabet-size x) alphabet-size)) (encode-string k)))
  )

(defn decrypt [s k]
  (encrypt s (inverse-key k)))

(let [k "a b"
      s "she sells sea shell on the sea shore"]
  (decrypt (encrypt s k) k))
