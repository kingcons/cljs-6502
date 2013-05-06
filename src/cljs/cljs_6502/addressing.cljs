(ns cljs-6502.addressing
  (:use [cljs-6502.cpu :only [cpu get-byte get-word
                              wrap-byte wrap-word
                              maybe-update-cycle-count]])
  (:use-macros [clj-6502.addressing :only [defaddress]]))

(defprotocol AddressingMode
  (getter [mode] "Return a fn that gets the value at MODE for CPU.")
  (setter [mode] "Return a fn that sets the value at MODE for CPU.")
  (reader [mode] "Return a Regular Expression for parsing MODE.")
  (writer [mode] "Return a Format Expression for printing MODE."))

(defaddress Implied
  {:reader #"^$"
   :writer ""}
  nil)

(defaddress Accumulator
  {:reader #"^[aA]$"
   :writer "A"}
  (:ar @cpu))

(defaddress Immediate
  {:reader #"^#\\$[0-9a-fA-F]{2}$"
   :writer "#$%02x"}
  (:pc @cpu))

(defaddress ZeroPage
  {:reader #"^\\$[0-9a-fA-F]{2}$"
   :writer "$%02x"}
  (get-byte (:pc @cpu)))

(defaddress ZeroPageX
  {:reader #"^\\$[0-9a-fA-F]{2},[xX]$"
   :writer "$%02x, X"}
  (wrap-byte (+ (:pc @cpu) (:xr @cpu))))

(defaddress ZeroPageY
  {:reader #"^\\$[0-9a-fA-F]{2},[yY]$"
   :writer "$%02x, Y"}
  (wrap-byte (+ (:pc @cpu) (:xr @cpu))))

(defaddress Absolute
  {:reader #"^\\$[0-9a-fA-F]{4}$"
   :writer "$%02x%02x"}
  (get-word (:pc @cpu)))

(defaddress AbsoluteX
  {:reader #"^\\$[0-9a-fA-F]{4},[xX]$"
   :writer "$%02x%02x, X"}
  ;; TODO: Either have M-U-C-C return result or use a threading macro.
  (let [result (wrap-word (+ (get-word (:pc @cpu)) (:xr @cpu)))]
    (maybe-update-cycle-count cpu result)))

(defaddress AbsoluteY
  {:reader #"^\\$[0-9a-fA-F]{4},[yY]$"
   :writer "$%02x%02x, Y"}
  (let [result (wrap-word (+ (get-word (:pc @cpu)) (:yr @cpu)))]
    (maybe-update-cycle-count cpu result)))

(defaddress Indirect
  {:reader #"^\\(\\$[0-9a-fA-F]{4}\\)$"
   :writer "($%02x%02x)"}
  (get-word (get-word (:pc @cpu)) t))

(defaddress IndirectX
  {:reader #"^\\(\\$[0-9a-fA-F]{2}\\),[xX]$"
   :writer "($%02x), X"}
  (get-word (wrap-byte (+ (get-byte (:pc @cpu) (:xr @cpu)))) t))

(defaddress IndirectY
  {:reader #"^\\(\\$[0-9a-fA-F]{2}\\),[yY]$"
   :writer "($%02x), Y"}
  (let [addr (get-word (get-byte (:pc @cpu)) t)
        result (wrap-word (+ addr (:yr @cpu)))]
    (maybe-update-cycle-count cpu result)))

(defaddress Relative
  {:reader #"^&[0-9a-fA-F]{2}$"
   :writer "&%02x"}
  (swap! cpu update-in [:cc] inc)
  (let [pc (:pc @cpu)
        offset (get-byte pc)
        result (if (bit-test offset 7)
                 (wrap-word (- pc (- 0xff offset)))
                 (wrap-word (+ pc (inc offset))))]
    (maybe-update-cycle-count cpu result (inc pc))))
