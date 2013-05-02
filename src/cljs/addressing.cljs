(ns cljs-6502.addressing
  (:use [cljs-6502.cpu :only [get-byte get-word
                              wrap-byte wrap-word
                              maybe-update-cycle-count]]))

(defprotocol AddressingMode
  (getter [mode] "Return a fn that gets the value at MODE for CPU.")
  (setter [mode] "Return a fn that sets the value at MODE for CPU.")
  (reader [mode] "Return a Regular Expression for parsing MODE.")
  (writer [mode] "Return a Format Expression for printing MODE."))

(defmacro defaddress
  [name {:keys [reader writer]} & body]
  (let [getter `(fn [cpu] ~@body)
        setter `(fn [cpu val] ~@body)]
    `(do
       (deftype ~name [])
       (extend-protocol AddressingMode
         ~name
         (getter [mode] ~getter)
         (setter [mode] ~setter)
         (reader [mode] ~reader)
         (writer [mode] ~writer)))))

(defaddress Implied
  {:reader #"^$"
   :writer ""}
  nil)

(defaddress Accumulator
  {:reader #"^[aA]$"
   :writer "A"}
  (:ar cpu))

(defaddress Immediate
  {:reader #"^#\\$[0-9a-fA-F]{2}$"
   :writer "~{#$~2,'0x~}"}
  (:pc cpu))

(defaddress ZeroPage
  {:reader #"^\\$[0-9a-fA-F]{2}$"
   :writer "~{$~2,'0x~}"}
  (get-byte (:pc cpu)))

(defaddress ZeroPageX
  {:reader #"^\\$[0-9a-fA-F]{2},[xX]$"
   :writer "$~{~2,'0x~}, X"}
  (wrap-byte (+ (:pc cpu) (:xr cpu))))

(defaddress ZeroPageY
  {:reader #"^\\$[0-9a-fA-F]{2},[yY]$"
   :writer "$~{~2,'0x~}, Y"}
  (wrap-byte (+ (:pc cpu) (:xr cpu))))

(defaddress Absolute
  {:reader #"^\\$[0-9a-fA-F]{4}$"
   :writer "$~{~2,'0x~}"}
  (get-word (:pc cpu)))

(defaddress AbsoluteX
  {:reader #"^\\$[0-9a-fA-F]{4},[xX]$"
   :writer "$~{~2,'0x~}, X"}
  ;; TODO: Either have M-U-C-C return result or use a threading macro.
  (let [result (wrap-word (+ (get-word (:pc cpu)) (:xr cpu)))]
    (maybe-update-cycle-count cpu result)))

(defaddress AbsoluteY
  {:reader #"^\\$[0-9a-fA-F]{4},[yY]$"
   :writer "$~{~2,'0x~}, Y"}
    (let [result (wrap-word (+ (get-word (:pc cpu)) (:yr cpu)))]
    (maybe-update-cycle-count cpu result)))

(defaddress Indirect
  {:reader #"^\\(\\$[0-9a-fA-F]{4}\\)$"
   :writer "($~{~2,'0x~})"}
  (get-word (get-word (:pc cpu)) t))

(defaddress IndirectX
  {:reader #"^\\(\\$[0-9a-fA-F]{2}\\),[xX]$"
   :writer "($~{~2,'0x~}), X"}
  (get-word (wrap-byte (+ (get-byte (:pc cpu) (:xr cpu)))) t))

(defaddress IndirectY
  {:reader #"^\\(\\$[0-9a-fA-F]{2}\\),[yY]$"
   :writer "($~{~2,'0x~}), Y"}
  (let [addr (get-word (get-byte (:pc cpu)) t)
        result (wrap-word (+ addr (:yr cpu)))]
    (maybe-update-cycle-count cpu result)))

(defaddress Relative
  {:reader #"^&[0-9a-fA-F]{2}$"
   :writer "&~{~2,'0x~}"}
  ;; TODO: (inc (:cc cpu))
  (let [offset (get-byte (:pc cpu))
        result (if (bit-test offset 7)
                 (wrap-word (- (:pc cpu) (- 0xff offset)))
                 (wrap-word (+ (:pc cpu) (1+ offset))))]
    (maybe-update-cycle-count cpu result (1+ (:pc cpu)))))
