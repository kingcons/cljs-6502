(ns cljs-6502.cpu)

(defrecord CPU [pc sp sr xr yr ar cc])

(defn make-cpu []
  (CPU. 0xfffc 0xfd 0x24 0 0 0 0))

(defn make-ram []
  (vec (repeat 0x10000 0)))

(def cpu "The 6502 instance used by opcodes in the package."
  (atom (make-cpu)))

(def ram "A lovely hunk of bytes."
  (atom (make-ram)))

(def opcodes "A mapping of opcodes to instruction metadata vectors."
  (atom (vec (repeat 0x100 [nil nil nil nil]))))

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
