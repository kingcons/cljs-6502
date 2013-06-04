(ns cljs-6502.opcodes
  (:refer-clojure :exclude [and inc dec])
  (:use-macros [clj-6502.macros :only [defasm branch-if getter-mixed]])
  (:use [cljs-6502.addressing :only [Implied Immediate Accumulator
                                     ZeroPage ZeroPageX ZeroPageY
                                     Absolute AbsoluteX AbsoluteY
                                     Indirect IndirectX IndirectY
                                     Relative getter setter]]
        [cljs-6502.cpu :only [get-register set-register get-byte set-byte
                              get-word wrap-byte wrap-word rotate-byte
                              stack-push stack-pop stack-push-word stack-pop-word
                              status-bit set-status-bit set-flags-if set-flags-nz
                              overflow?]]))

(defasm adc {:docs "Add to Accumulator with Carry"}
    [[0x61 6 2 IndirectX]
     [0x65 3 2 ZeroPage]
     [0x69 2 2 Immediate]
     [0x6d 4 3 Absolute]
     [0x71 5 2 IndirectY]
     [0x75 4 2 ZeroPageX]
     [0x79 4 3 AbsoluteY]
     [0x7d 4 3 AbsoluteX]]
  (let [value (getter mode raw?)
        result (+ (get-register :ar) value (status-bit :carry))]
    (set-flags-if :carry #(> result 0xff)
                  :overflow #(overflow? result (get-register :ar) value)
                  :negative #(bit-test result 7)
                  :zero #(zero? (wrap-byte result)))
    (set-register :ar (wrap-byte result))))

(defasm and {:docs "And with Accumulator"}
    [[0x21 6 2 IndirectX]
     [0x25 3 2 ZeroPage]
     [0x29 2 2 Immediate]
     [0x2d 4 3 Absolute]
     [0x31 5 2 IndirectY]
     [0x35 4 2 ZeroPageX]
     [0x39 4 3 AbsoluteY]
     [0x3d 4 3 AbsoluteX]]
  (let [value (getter mode raw?)
        result (set-register :ar (bit-and (get-register :ar) value))]
    (set-flags-nz result)))

(defasm asl {:docs "Arithmetic Shift Left"}
    [[0x06 5 2 ZeroPage]
     [0x0a 2 1 Accumulator]
     [0x0e 6 3 Absolute]
     [0x16 6 2 ZeroPageX]
     [0x1e 7 3 AbsoluteX]]
  (let [value (getter-mixed)
        result (wrap-byte (bit-shift-left value 1))]
    (set-flags-if :carry #(bit-test value 7))
    (set-flags-nz result)
    (setter mode result)))

(defasm bcc {:docs "Branch on Carry Clear" :track-pc? nil}
    [[0x90 2 2 Relative]]
  (branch-if #(zero? (status-bit :carry))))

(defasm bcs {:docs "Branch on Carry Set" :track-pc? nil}
    [[0xb0 2 2 Relative]]
  (branch-if #(pos? (status-bit :carry))))

(defasm beq {:docs "Branch if Equal" :track-pc? nil}
    [[0xf0 2 2 Relative]]
  (branch-if #(pos? (status-bit :zero))))

(defasm bit {:docs "Test Bits in Memory with Accumulator"}
    [[0x24 3 2 ZeroPage]
     [0x2c 4 3 Absolute]]
  (let [result (getter mode raw?)]
    (set-flags-if :zero #(zero? (bit-and (get-register :ar) result))
                  :negative #(bit-test result 7)
                  :overflow #(bit-test result 6))))

(defasm bmi {:docs "Branch on Negative Result" :track-pc? nil}
    [[0x30 2 2 Relative]]
  (branch-if #(pos? (status-bit :negative))))

(defasm bne {:docs "Branch if Not Equal" :track-pc? nil}
    [[0xd0 2 2 Relative]]
  (branch-if #(zero? (status-bit :zero))))

(defasm bpl {:docs "Branch on Positive Result" :track-pc? nil}
    [[0x10 2 2 Relative]]
  (branch-if #(zero? (status-bit :negative))))

(defasm brk {:docs "Force Break"}
    [[0x00 7 1 Implied]]
  (let [pc (wrap-word (+ 1 (get-register :pc)))]
    (stack-push-word pc)
    (set-status-bit :break 1)
    (stack-push (get-register :sr))
    (set-status-bit :interrupt 1)
    (set-register :pc (get-word 0xfffe))))

(defasm bvc {:docs "Branch on Overflow Clear" :track-pc? nil}
    [[0x50 2 2 Relative]]
  (branch-if #(zero? (status-bit :overflow))))

(defasm bvs {:docs "Branch on Overflow Set" :track-pc? nil}
    [[0x70 2 2 Relative]]
  (branch-if #(pos? (status-bit :overflow))))

(defasm clc {:docs "Clear Carry Flag"}
    [[0x18 2 1 Implied]]
  (set-status-bit :carry 0))

(defasm cld {:docs "Clear Decimal Flag"}
    [[0xd8 2 1 Implied]]
  (set-status-bit :decimal 0))

(defasm cli {:docs "Clear Interrupt Flag"}
    [[0x58 2 1 Implied]]
  (set-status-bit :interrupt 0))

(defasm clv {:docs "Clear Overflow Flag"}
    [[0xb8 2 1 Implied]]
  (set-status-bit :overflow 0))

(defasm cmp {:docs "Compare Memory with Accumulator"}
    [[0xc1 6 2 IndirectX]
     [0xc5 3 2 ZeroPage]
     [0xc9 2 2 Immediate]
     [0xcd 4 3 Absolute]
     [0xd1 5 2 IndirectY]
     [0xd5 4 2 ZeroPageX]
     [0xd9 4 3 AbsoluteY]
     [0xdd 4 3 AbsoluteX]]
  (let [result (- (get-register :ar) (getter mode raw?))]
    (set-flags-if :carry (not (neg? result)))
    (set-flags-nz result)))

(defasm cpx {:docs "Compare Memory with X register"}
    [[0xe0 2 2 Immediate]
     [0xe4 3 2 ZeroPage]
     [0xec 4 3 Absolute]]
  (let [result (- (get-register :xr) (getter mode raw?))]
    (set-flags-if :carry #(not (neg? result)))
    (set-flags-nz result)))

(defasm cpy {:docs "Compare Memory with Y register"}
    [[0xc0 2 2 Immediate]
     [0xc4 3 2 ZeroPage]
     [0xcc 4 3 Absolute]]
  (let [result (- (get-register :yr) (getter mode raw?))]
    (set-flags-if :carry #(not (neg? result)))
    (set-flags-nz result)))

(defasm dec {:docs "Decrement Memory"}
    [[0xc6 5 2 ZeroPage]
     [0xce 6 3 Absolute]
     [0xd6 6 2 ZeroPageX]
     [0xde 7 3 AbsoluteX]]
  (let [result (wrap-byte (- (getter mode raw?) 1))]
    (setter mode result)
    (set-flags-nz result)))

(defasm dex {:docs "Decrement X register"}
    [[0xca 2 1 Implied]]
  (let [result (set-register :xr (wrap-byte (- (get-register :xr) 1)))]
    (set-flags-nz result)))

(defasm dey {:docs "Decrement Y register"}
    [[0x88 2 1 Implied]]
  (let [result (set-register :yr (wrap-byte (- (get-register :yr) 1)))]
    (set-flags-nz result)))

(defasm eor {:docs "Exclusive OR with Accumulator"}
    [[0x41 6 2 IndirectX]
     [0x45 3 2 ZeroPage]
     [0x49 2 2 Immediate]
     [0x4d 4 3 Absolute]
     [0x51 5 2 IndirectY]
     [0x55 4 2 ZeroPageX]
     [0x59 4 3 AbsoluteY]
     [0x5d 4 3 AbsoluteX]]
  (let [result (set-register :ar (bit-xor (getter mode raw?) (get-register :ar)))]
    (set-flags-nz result)))

(defasm inc {:docs "Increment Memory"}
    [[0xe6 5 2 ZeroPage]
     [0xee 6 3 Absolute]
     [0xf6 6 2 ZeroPageX]
     [0xfe 7 3 AbsoluteX]]
  (let [result (wrap-byte (+ 1 (getter mode raw?)))]
    (setter mode result)
    (set-flags-nz result)))

(defasm inx {:docs "Increment X register"}
    [[0xe8 2 1 Implied]]
  (let [result (set-register :xr (wrap-byte (+ 1 (get-register :xr))))]
    (set-flags-nz result)))

(defasm iny {:docs "Increment Y register"}
    [[0xc8 2 1 Implied]]
  (let [result (set-register :yr (wrap-byte (+ 1 (get-register :yr))))]
    (set-flags-nz result)))

(defasm jmp {:docs "Jump Unconditionally" :raw? t :track-pc? nil}
    [[0x4c 3 3 Absolute]
     [0x6c 5 3 Indirect]]
  (set-register :pc (getter mode raw?)))

(defasm jsr {:docs "Jump to Subroutine" :raw? t :track-pc? nil}
    [[0x20 6 3 Absolute]]
  (stack-push-word (wrap-word (+ 1 (get-register :pc))))
  (set-register :pc (getter mode raw?)))

(defasm lda {:docs "Load Accumulator from Memory"}
    [[0xa1 6 2 IndirectX]
     [0xa5 3 2 ZeroPage]
     [0xa9 2 2 Immediate]
     [0xad 4 3 Absolute]
     [0xb1 5 2 IndirectY]
     [0xb5 4 2 ZeroPageX]
     [0xb9 4 3 AbsoluteY]
     [0xbd 4 3 AbsoluteX]]
  (let [result (set-register :ar (getter mode raw?))]
    (set-flags-nz result)))

(defasm ldx {:docs "Load X register from Memory"}
    [[0xa2 2 2 Immediate]
     [0xa6 3 2 ZeroPage]
     [0xae 4 3 Absolute]
     [0xb6 4 2 ZeroPageY]
     [0xbe 4 3 AbsoluteY]]
  (let [result (set-register :xr (getter mode raw?))]
    (set-flags-nz result)))

(defasm ldy {:docs "Load Y register from Memory"}
    [[0xa0 2 2 Immediate]
     [0xa4 3 2 ZeroPage]
     [0xac 4 3 Absolute]
     [0xbc 4 3 AbsoluteX]
     [0xb4 4 2 ZeroPageX]]
  (let [result (set-register :yr (getter mode raw?))]
    (set-flags-nz result)))

(defasm lsr {:docs "Logical Shift Right"}
    [[0x46 5 2 ZeroPage]
     [0x4a 2 1 Accumulator]
     [0x4e 6 3 Absolute]
     [0x56 6 2 ZeroPageX]
     [0x5e 7 3 AbsoluteX]]
  (let [value (getter-mixed)
        result (bit-shift-right value 1)]
    (set-flags-if :carry #(bit-test value 0))
    (setter mode result)
    (set-flags-nz result)))

(defasm nop {:docs "No Operation"}
    [[0xea 2 1 Implied]]
  nil)

(defasm ora {:docs "Bitwise OR with Accumulator"}
    [[0x01 6 2 IndirectX]
     [0x05 3 2 ZeroPage]
     [0x09 2 2 Immediate]
     [0x0d 4 3 Absolute]
     [0x11 5 2 IndirectY]
     [0x15 4 2 ZeroPageX]
     [0x19 4 3 AbsoluteY]
     [0x1d 4 3 AbsoluteX]]
  (let [result (set-register :ar (bit-or (get-register :ar) (getter mode raw?)))]
    (set-flags-nz result)))

(defasm pha {:docs "Push Accumulator"}
    [[0x48 3 1 Implied]]
  (stack-push (get-register :ar)))

(defasm php {:docs "Push Processor Status"}
    [[0x08 3 1 Implied]]
  (stack-push (bit-or (get-register :sp) 0x10)))

(defasm pla {:docs "Pull Accumulator from Stack"}
    [[0x68 4 1 Implied]]
  (let [result (set-register :ar (stack-pop))]
    (set-flags-nz result)))

(defasm plp {:docs "Pull Processor Status from Stack"}
    [[0x28 4 1 Implied]]
  (let [result (bit-or (stack-pop) 0x20)]
    (set-register :sr result)
    (set-status-bit :break 0)))

(defasm rol {:docs "Rotate Left"}
    [[0x2a 2 1 Accumulator]
     [0x26 5 2 ZeroPage]
     [0x2e 6 3 Absolute]
     [0x36 6 2 ZeroPageX]
     [0x3e 7 3 AbsoluteX]]
  (let [value (getter-mixed)
        result (wrap-byte (rotate-byte value 1))]
    (setter mode result)
    (set-flags-if :carry #(bit-test value 7))
    (set-flags-nz result)))

(defasm ror {:docs "Rotate Right"}
    [[0x66 5 2 ZeroPage]
     [0x6a 2 1 Accumulator]
     [0x6e 6 3 Absolute]
     [0x76 6 2 ZeroPageX]
     [0x7e 7 3 AbsoluteX]]
  (let [value (getter-mixed)
        result (wrap-byte (rotate-byte value -1))]
    (setter mode result)
    (set-flags-if :carry #(bit-test value 0))
    (set-flags-nz result)))

(defasm rti {:docs "Return from Interrupt"}
    [[0x40 6 1 Implied]]
  (set-register :sr (bit-or (stack-pop) 0x20))
  (set-register :pc (stack-pop-word)))

(defasm rts {:docs "Return from Subroutine" :track-pc? nil}
    [[0x60 6 1 Implied]]
  (set-register :pc (+ 1 (stack-pop-word))))

; TODO: Add support for Decimal mode. (not supported on NES)
(defasm sbc {:docs "Subtract from Accumulator with Carry"}
    [[0xe1 6 2 IndirectX]
     [0xe5 3 2 ZeroPage]
     [0xe9 2 2 Immediate]
     [0xed 4 3 Absolute]
     [0xf1 5 2 IndirectY]
     [0xf5 4 2 ZeroPageX]
     [0xf9 4 3 AbsoluteY]
     [0xfd 4 3 AbsoluteX]]
  (let [value (getter mode raw?)
        result (- (get-register :ar) value
                  (bit-flip (status-bit :carry) 0))]
    (set-flags-if :zero #(zero? (wrap-byte result))
                  :overflow #(overflow? result (bit-flip value 7)
                                        (get-register :ar))
                  :negative #(bit-test result 7)
                  :carry #(not (neg? result)))
    (set-register :ar (wrap-byte result))))

(defasm sec {:docs "Set Carry Flag"}
    [[0x38 2 1 Implied]]
  (set-status-bit :carry 1))

(defasm sed {:docs "Set Decimal Flag"}
    [[0xf8 2 1 Implied]]
  (set-status-bit :decimal 1))

(defasm sei {:docs "Set Interrupt Flag"}
    [[0x78 2 1 Implied]]
  (set-status-bit :interrupt 1))

(defasm sta {:docs "Store Accumulator" :raw? t}
    [[0x81 6 2 IndirectX]
     [0x85 3 2 ZeroPage]
     [0x8d 4 3 Absolute]
     [0x91 6 2 IndirectY]
     [0x95 4 2 ZeroPageX]
     [0x99 5 3 AbsoluteY]
     [0x9d 5 3 AbsoluteX]]
  (setter mode (get-register :ar)))

(defasm stx {:docs "Store X register" :raw? t}
    [[0x86 3 2 ZeroPage]
     [0x8e 4 3 Absolute]
     [0x96 4 2 ZeroPageY]]
  (setter mode (get-register :xr)))

(defasm sty {:docs "Store Y register" :raw? t}
    [[0x84 3 2 ZeroPage]
     [0x8c 4 3 Absolute]
     [0x94 4 2 ZeroPageX]]
  (setter mode (get-register :yr)))

(defasm tax {:docs "Transfer Accumulator to X register"}
    [[0xaa 2 1 Implied]]
  (let [result (set-register :xr (get-register :ar))]
    (set-flags-nz result)))

(defasm tay {:docs "Transfer Accumulator to Y register"}
    [[0xa8 2 1 Implied]]
  (let [result (set-register :yr (get-register :ar))]
    (set-flags-nz result)))

(defasm tsx {:docs "Transfer Stack Pointer to X register"}
    [[0xba 2 1 Implied]]
  (let [result (set-register :xr (get-register :sp))]
    (set-flags-nz result)))

(defasm txa {:docs "Transfer X register to Accumulator"}
    [[0x8a 2 1 Implied]]
  (let [result (set-register :ar (get-register :xr))]
    (set-flags-nz result)))

(defasm txs {:docs "Transfer X register to Stack Pointer"}
    [[0x9a 2 1 Implied]]
  (set-register :sp (get-register :xr)))

(defasm tya {:docs "Transfer Y register to Accumulator"}
    [[0x98 2 1 Implied]]
  (let [result (set-register :ar (get-register :yr))]
    (set-flags-nz result)))
