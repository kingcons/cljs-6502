(ns cljs-6502.cpu
  (:use-macros [clj-6502.macros :only [defenum]]))

(defprotocol Resettable
  (reset [obj] "Reset the given object to an initial state."))

(defrecord CPU [pc sp sr xr yr ar cc])

(defn make-cpu
  []
  (CPU. 0xfffc 0xfd 0x24 0 0 0 0))

(def cpu "The 6502 instance used by opcodes in the package."
  (atom (make-cpu)))

(def ram "A lovely hunk of bytes."
  (atom (vec (repeat 0x10000 0))))

(def opcodes "A mapping of opcodes to instruction metadata vectors."
  (atom (vec (repeat 0x100 [nil nil nil nil]))))

(defn get-register
  "Get the value of REGISTER."
  [register]
  (get @cpu register))

(defn set-register
  "Set REGISTER to VALUE."
  [register value]
  (swap! cpu update-in [register] (fn [x] value))
  value)

(defn get-byte
  "Get a byte from ram at the given ADDRESS."
  [address]
  (get @ram address))

(defn set-byte
  "Set ADDRESS in ram to VALUE."
  [address value]
  (swap! ram update-in [address] (fn [x] value))
  value)

(defn get-word
  "Get the word starting at ADDRESS."
  [address]
  (+ (get-byte address)
     (bit-shift-left (get-byte (inc address)) 8)))

(defn get-range
  "Get the range of bytes from START to END from ram."
  [start end]
  (subvec @ram start end))

(defn wrap-byte
  "Wrap VALUE to ensure it fits in a single byte."
  [value]
  (bit-and value 0xff))

(defn wrap-word
  "Wrap VALUE to ensure it fits in a single word."
  [value]
  (bit-and value 0xffff))

(defn wrap-page
  "Wrap VALUE to ensure it doesn't cross a page boundary."
  [value]
  (+ (bit-and value 0xff00)
     (bit-and (inc value) 0xff)))

(defn maybe-update-cycle-count
  "If ADDRESS crosses a page boundary, add an extra cycle to CPU's count. If
START is provided, test that against ADDRESS. Otherwise, use absolute address."
  [address start]
  (when (not (= (bit-and (or start (get-word (:pc @cpu))) 0xff00)
                (bit-and address 0xff00)))
    (swap! cpu update-in [:cc] inc)))

(defn stack-push
  "Push the byte VALUE on the stack, decrementing the stack pointer."
  [value]
  (let [address (+ (:sp @cpu) 0x100)]
    (swap! ram update-in [address] (fn [x] (wrap-byte value)))
    (swap! cpu update-in [:sp] (fn [x] (wrap-byte (dec x))))))

(defn stack-push-word
  "Push the 16-bit word VALUE onto the stack, decrementing the stack pointer."
  [value]
  (stack-push (wrap-byte (bit-shift-right value 8)))
  (stack-push (wrap-byte value)))

(defn stack-pop
  "Pop the top byte off the stack, incrementing the stack pointer."
  []
  (swap! cpu update-in [:sp] (fn [x] (wrap-byte (inc x))))
  (get-byte (+ (:sp @cpu) 0x100)))

(defn stack-pop-word
  "Pop the top word off the stack, incrementing the stack pointer."
  []
  (+ (stack-pop) (bit-shift-left (stack-pop) 8)))

(defenum status-bit [:carry :zero :interrupt :decimal
                     :break :unused :overflow :negative])

(defn status-bit
  "Retrieve the bit corresponding to keyword NAME from the CPU status register."
  [name]
  (bit-and (:sr @cpu) (Math/pow 2 (%status-bit name))))

(defn set-status-bit
  "Set the bit corresponding to NAME from the CPU status register to VALUE."
  [name value]
  (let [setter (if (zero? value) bit-clear bit-set)
        index (%status-bit name)]
    (swap! cpu update-in [:sr] (fn [x] (apply setter [x index])))))

(defn set-flags-if
  "Takes any even number of arguments where the first is a keyword denoting a
status bit and the second is a predicate function that takes no arguments.
It will set each flag to 1 if its predicate is true, otherwise 0."
  [& flag-preds]
  (doseq [[flag predicate] (apply hash-map flag-preds)]
    (set-status-bit flag (if (predicate) 1 0))))

(defn set-flags-nz
  "Set the zero and negative bits of CPU's status-register based on VALUE."
  [value]
  (set-flags-if :zero #(zero? value) :negative #(bit-test value 7)))

(defn overflow?
  "Checks whether the sign of RESULT is found in the signs of REG or MEM."
  [result reg mem]
  (not (or (= (bit-test result 7) (bit-test reg 7))
           (= (bit-test result 7) (bit-test mem 7)))))

(defn rotate-byte
  "Rotate the bits of INTEGER by COUNT. If COUNT is negative, rotate right."
  [integer count]
  (let [shifter (if (pos? count) bit-shift-left bit-shift-right)
        result (apply shifter [integer count])]
    (cond
     (not (pos? (status-bit :carry))) result
     (= 01 count) (bit-or result 0x01)
     (= -1 count) (bit-or result 0x80))))

(defn nmi
  "Generate a non-maskable interrupt. Used for vblanking in NES."
  []
  (stack-push-word (:pc @cpu))
  (stack-push (:sr @cpu))
  (set-register :pc (get-word 0xfffa)))

(extend-protocol Resettable
  CPU
  (reset [obj] (swap! cpu (fn [x] (make-cpu)))))
