(ns cljs-6502.core
  (:use [cljs-6502.cpu :only [get-byte cpu opcodes]]
        ;[cljs-6502.opcodes :only []]
        ))

(defn execute
  "Step the CPU until a BRK."
  []
  (loop [opcode (get-byte (:pc @cpu))]
    (let [result (cpu-step opcode)]
      (if (= result :done)
        @cpu
        (recur (get-byte (:pc @cpu)))))))

(defn cpu-step
  "Step the CPU through the next instruction, returning the CPU or :done."
  [opcode]
  (let [[multimethod _ _ mode] (get @opcodes opcode)
        result (multimethod opcode mode)]
    (if (zero? opcode)
      :done
      result)))
