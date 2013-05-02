(ns cljs-6502.cpu)

(defrecord CPU [pc sp sr xr yr ar cc])

(defn make-cpu []
  (CPU. 0xfffc 0xfd 0x24 0 0 0 0))

(defn make-ram []
  (vec (repeat 0x10000 0)))

(def *opcodes* (vec (repeat 0x100 [nil nil nil nil])))
(def *cpu* (make-cpu))
(def *ram* (make-ram))

(defn get-byte [address]
  )

(defn set-byte [address value]
  )

(defn get-word [address]
  )

(defn set-word [address value]
  )

(defn wrap-byte [value] (bit-and value 0xff))

(defn wrap-word [value] (bit-and value 0xffff))

(defn maybe-update-cycle-count
  ([cpu address])
  ([cpu address start]))

(defmacro branch-if [cpu predicate]
  `(if ~predicate
     (set! (:pc cpu) (Relative.))
     (inc! (:pc cpu))))

;; (defmacro set-flags-if [cpu & flag-preds]
;;   )

;; (defn set-flags-nz [cpu value]
;;   (set-flags-if cpu :zero (zero? value) :negative (bit-test 7 value)))

;; (defmacro defopcode [name [{:keys [docs addr-style track-pc]}]
;;                      modes & body]
;;   `(do
;;      (defmulti ~name)
;;      ))
