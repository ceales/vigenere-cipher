(ns vigenere-cipher.core)

(def alphabet "abcdefghijklmnopqrstuvwxyz 0123456789")
(def alphabet-size (count alphabet))

(def encode-map
  (into {}
        (map vector alphabet (range))))

(def decode-map (clojure.set/map-invert encode-map))

(defn encode-char [c] (encode-map c))

(defn decode-char [e] (decode-map e))

(defn encode-string [s]
  (map encode-char s))

(defn decode-string [s]
  (apply str (map decode-char s)))

(defn encode-key [key]
  (cycle (encode-string key)))

(defn infinite-encrypt [s k]
  (map (fn [x y] (mod (+ x y) alphabet-size))
       (encode-string s) (encode-key k)))

(defn encrypt
  "Encrypt the given string s with the key k using vigenere cipher.
  Assuming \\a maps to 0, then
  (encrypt \"hello\" \"b\") => \"ifmmp\""
  [s k] (decode-string (take (count s) (infinite-encrypt s k))))

(defn inverse-key [k]
  (decode-string (map (fn [x] (mod (* -1 x) alphabet-size))
                      (encode-string k))))

(defn decrypt
  "Decrypt the passed encrypted string s using key k"
  [s k] (encrypt s (inverse-key k)))
