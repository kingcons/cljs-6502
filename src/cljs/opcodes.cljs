(ns cljs-6502.opcodes
  (:use-macros [cljs-6502.cpu :only [defopcode]])
  (:use [cljs-6502.addressing :only [Implied Immediate Accumulator
                                     ZeroPage ZeroPageX ZeroPageY
                                     Absolute AbsoluteX AbsoluteY
                                     Indirect IndirectX IndirectY
                                     Relative]]))

(defopcode adc (:docs "Add to Accumulator with Carry")
    [[0x61 6 2 IndirectX]
     [0x65 3 2 ZeroPage]
     [0x69 2 2 Immediate]
     [0x6d 4 3 Absolute]
     [0x71 5 2 IndirectY]
     [0x75 4 2 ZeroPageX]
     [0x79 4 3 AbsoluteY]
     [0x7d 4 3 AbsoluteX]]
  (let [result (+ (cpu-ar cpu) (funcall mode cpu) (status-bit :carry cpu))]
    (set-flags-if cpu :carry (> result 0xff)
                  :overflow (overflow-p result (cpu-ar cpu) (funcall mode cpu))
                  :negative (logbitp 7 result)
                  :zero (zerop (wrap-byte result)))
    (setf (cpu-ar cpu) (wrap-byte result))))

(defopcode and (:docs "And with Accumulator")
    [[0x21 6 2 IndirectX]
     [0x25 3 2 ZeroPage]
     [0x29 2 2 Immediate]
     [0x2d 4 3 Absolute]
     [0x31 5 2 IndirectY]
     [0x35 4 2 ZeroPageX]
     [0x39 4 3 AbsoluteY]
     [0x3d 4 3 AbsoluteX]]
  (let [result (setf (cpu-ar cpu) (logand (cpu-ar cpu) (funcall mode cpu)))]
    (set-flags-nz cpu result)))

(defopcode asl (:docs "Arithmetic Shift Left" :addr-style :mixed)
    [[0x06 5 2 ZeroPage]
     [0x0a 2 1 Accumulator]
     [0x0e 6 3 Absolute]
     [0x16 6 2 ZeroPageX]
     [0x1e 7 3 AbsoluteX]]
  (set-flags-if cpu :carry (logbitp 7 (funcall mode cpu)))
  (let [result (wrap-byte (ash (funcall mode cpu) 1))]
    (set-flags-nz cpu result)
    (funcall setf-form result)))

(defopcode bcc (:docs "Branch on Carry Clear" :track-pc nil)
    [[0x90 2 2 Relative]]
  (branch-if (zerop (status-bit :carry cpu)) cpu))

(defopcode bcs (:docs "Branch on Carry Set" :track-pc nil)
    [[0xb0 2 2 Relative]]
  (branch-if (plusp (status-bit :carry cpu)) cpu))

(defopcode beq (:docs "Branch if Equal" :track-pc nil)
    [[0xf0 2 2 Relative]]
  (branch-if (plusp (status-bit :zero cpu)) cpu))

(defopcode bit (:docs "Test Bits in Memory with Accumulator")
    [[0x24 3 2 ZeroPage]
     [0x2c 4 3 Absolute]]
  (let [result (funcall mode cpu)]
    (set-flags-if cpu :zero (zerop (logand (cpu-ar cpu) result))
                  :negative (logbitp 7 result)
                  :overflow (logbitp 6 result))))

(defopcode bmi (:docs "Branch on Negative Result" :track-pc nil)
    [[0x30 2 2 Relative]]
  (branch-if (plusp (status-bit :negative cpu)) cpu))

(defopcode bne (:docs "Branch if Not Equal" :track-pc nil)
    [[0xd0 2 2 Relative]]
  (branch-if (zerop (status-bit :zero cpu)) cpu))

(defopcode bpl (:docs "Branch on Positive Result" :track-pc nil)
    [[0x10 2 2 Relative]]
  (branch-if (zerop (status-bit :negative cpu)) cpu))

(defopcode brk (:docs "Force Break")
    [[0x00 7 1 Implied]]
  (let [pc (wrap-word (1+ (cpu-pc cpu)))]
    (stack-push-word pc cpu)
    (setf (status-bit :break cpu) 1)
    (stack-push (cpu-sr cpu) cpu)
    (setf (status-bit :interrupt cpu) 1)
    (setf (cpu-pc cpu) (get-word 0xfffe))))

(defopcode bvc (:docs "Branch on Overflow Clear" :track-pc nil)
    [[0x50 2 2 Relative]]
  (branch-if (zerop (status-bit :overflow cpu)) cpu))

(defopcode bvs (:docs "Branch on Overflow Set" :track-pc nil)
    [[0x70 2 2 Relative]]
  (branch-if (plusp (status-bit :overflow cpu)) cpu))

(defopcode clc (:docs "Clear Carry Flag")
    [[0x18 2 1 Implied]]
  (setf (status-bit :carry cpu) 0))

(defopcode cld (:docs "Clear Decimal Flag")
    [[0xd8 2 1 Implied]]
  (setf (status-bit :decimal cpu) 0))

(defopcode cli (:docs "Clear Interrupt Flag")
    [[0x58 2 1 Implied]]
  (setf (status-bit :interrupt cpu) 0))

(defopcode clv (:docs "Clear Overflow Flag")
    [[0xb8 2 1 Implied]]
  (setf (status-bit :overflow cpu) 0))

(defopcode cmp (:docs "Compare Memory with Accumulator")
    [[0xc1 6 2 IndirectX]
     [0xc5 3 2 ZeroPage]
     [0xc9 2 2 Immediate]
     [0xcd 4 3 Absolute]
     [0xd1 5 2 IndirectY]
     [0xd5 4 2 ZeroPageX]
     [0xd9 4 3 AbsoluteY]
     [0xdd 4 3 AbsoluteX]]
  (let [result (- (cpu-ar cpu) (funcall mode cpu))]
    (set-flags-if cpu :carry (not (minusp result)))
    (set-flags-nz cpu result)))

(defopcode cpx (:docs "Compare Memory with X register")
    [[0xe0 2 2 Immediate]
     [0xe4 3 2 ZeroPage]
     [0xec 4 3 Absolute]]
  (let [result (- (cpu-xr cpu) (funcall mode cpu))]
    (set-flags-if cpu :carry (not (minusp result)))
    (set-flags-nz cpu result)))

(defopcode cpy (:docs "Compare Memory with Y register")
    [[0xc0 2 2 Immediate]
     [0xc4 3 2 ZeroPage]
     [0xcc 4 3 Absolute]]
  (let [result (- (cpu-yr cpu) (funcall mode cpu))]
    (set-flags-if cpu :carry (not (minusp result)))
    (set-flags-nz cpu result)))

(defopcode dec (:docs "Decrement Memory")
    [[0xc6 5 2 ZeroPage]
     [0xce 6 3 Absolute]
     [0xd6 6 2 ZeroPageX]
     [0xde 7 3 AbsoluteX]]
  (let [result (wrap-byte (1- (funcall mode cpu)))]
    (funcall setf-form result)
    (set-flags-nz cpu result)))

(defopcode dex (:docs "Decrement X register")
    [[0xca 2 1 Implied]]
  (let [result (setf (cpu-xr cpu) (wrap-byte (1- (cpu-xr cpu))))]
    (set-flags-nz cpu result)))

(defopcode dey (:docs "Decrement Y register")
    [[0x88 2 1 Implied]]
  (let [result (setf (cpu-yr cpu) (wrap-byte (1- (cpu-yr cpu))))]
    (set-flags-nz cpu result)))

(defopcode eor (:docs "Exclusive OR with Accumulator")
    [[0x41 6 2 IndirectX]
     [0x45 3 2 ZeroPage]
     [0x49 2 2 Immediate]
     [0x4d 4 3 Absolute]
     [0x51 5 2 IndirectY]
     [0x55 4 2 ZeroPageX]
     [0x59 4 3 AbsoluteY]
     [0x5d 4 3 AbsoluteX]]
  (let [result (setf (cpu-ar cpu) (logxor (funcall mode cpu) (cpu-ar cpu)))]
    (set-flags-nz cpu result)))

(defopcode inc (:docs "Increment Memory")
    [[0xe6 5 2 ZeroPage]
     [0xee 6 3 Absolute]
     [0xf6 6 2 ZeroPageX]
     [0xfe 7 3 AbsoluteX]]
  (let [result (wrap-byte (1+ (funcall mode cpu)))]
    (funcall setf-form result)
    (set-flags-nz cpu result)))

(defopcode inx (:docs "Increment X register")
    [[0xe8 2 1 Implied]]
  (let [result (setf (cpu-xr cpu) (wrap-byte (1+ (cpu-xr cpu))))]
    (set-flags-nz cpu result)))

(defopcode iny (:docs "Increment Y register")
    [[0xc8 2 1 Implied]]
  (let [result (setf (cpu-yr cpu) (wrap-byte (1+ (cpu-yr cpu))))]
    (set-flags-nz cpu result)))

(defopcode jmp (:docs "Jump Unconditionally" :addr-style :raw :track-pc nil)
    [[0x4c 3 3 Absolute]
     [0x6c 5 3 indirect]]
  (setf (cpu-pc cpu) (funcall mode cpu)))

(defopcode jsr (:docs "Jump to Subroutine" :addr-style :raw :track-pc nil)
    [[0x20 6 3 Absolute]]
  (stack-push-word (wrap-word (1+ (cpu-pc cpu))) cpu)
  (setf (cpu-pc cpu) (funcall mode cpu)))

(defopcode lda (:docs "Load Accumulator from Memory")
    [[0xa1 6 2 IndirectX]
     [0xa5 3 2 ZeroPage]
     [0xa9 2 2 Immediate]
     [0xad 4 3 Absolute]
     [0xb1 5 2 IndirectY]
     [0xb5 4 2 ZeroPageX]
     [0xb9 4 3 AbsoluteY]
     [0xbd 4 3 AbsoluteX]]
  (let [result (setf (cpu-ar cpu) (funcall mode cpu))]
    (set-flags-nz cpu result)))

(defopcode ldx (:docs "Load X register from Memory")
    [[0xa2 2 2 Immediate]
     [0xa6 3 2 ZeroPage]
     [0xae 4 3 Absolute]
     [0xb6 4 2 ZeroPageY]
     [0xbe 4 3 AbsoluteY]]
  (let [result (setf (cpu-xr cpu) (funcall mode cpu))]
    (set-flags-nz cpu result)))

(defopcode ldy (:docs "Load Y register from Memory")
    [[0xa0 2 2 Immediate]
     [0xa4 3 2 ZeroPage]
     [0xac 4 3 Absolute]
     [0xbc 4 3 AbsoluteX]
     [0xb4 4 2 ZeroPageX]]
  (let [result (setf (cpu-yr cpu) (funcall mode cpu))]
    (set-flags-nz cpu result)))

(defopcode lsr (:docs "Logical Shift Right" :addr-style :mixed)
    [[0x46 5 2 ZeroPage]
     [0x4a 2 1 Accumulator]
     [0x4e 6 3 Absolute]
     [0x56 6 2 ZeroPageX]
     [0x5e 7 3 AbsoluteX]]
  (set-flags-if cpu :carry (logbitp 0 (funcall mode cpu)))
  (let [result (ash (funcall mode cpu) -1)]
    (funcall setf-form result)
    (set-flags-nz cpu result)))

(defopcode nop (:docs "No Operation")
    [[0xea 2 1 Implied]]
  nil)

(defopcode ora (:docs "Bitwise OR with Accumulator")
    [[0x01 6 2 IndirectX]
     [0x05 3 2 ZeroPage]
     [0x09 2 2 Immediate]
     [0x0d 4 3 Absolute]
     [0x11 5 2 IndirectY]
     [0x15 4 2 ZeroPageX]
     [0x19 4 3 AbsoluteY]
     [0x1d 4 3 AbsoluteX]]
  (let [result (setf (cpu-ar cpu) (logior (cpu-ar cpu) (funcall mode cpu)))]
    (set-flags-nz cpu result)))

(defopcode pha (:docs "Push Accumulator")
    [[0x48 3 1 Implied]]
  (stack-push (cpu-ar cpu) cpu))

(defopcode php (:docs "Push Processor Status")
    [[0x08 3 1 Implied]]
  (stack-push (logior (cpu-sr cpu) 0x10) cpu))

(defopcode pla (:docs "Pull Accumulator from Stack")
    [[0x68 4 1 Implied]]
  (let [result (setf (cpu-ar cpu) (stack-pop cpu))]
    (set-flags-nz cpu result)))

(defopcode plp (:docs "Pull Processor Status from Stack")
    [[0x28 4 1 Implied]]
  (let [result (logior (stack-pop cpu) 0x20)]
    (setf (cpu-sr cpu) (logandc2 result 0x10))))

(defopcode rol (:docs "Rotate Left" :addr-style :mixed)
    [[0x2a 2 1 Accumulator]
     [0x26 5 2 ZeroPage]
     [0x2e 6 3 Absolute]
     [0x36 6 2 ZeroPageX]
     [0x3e 7 3 AbsoluteX]]
  (let [val (funcall mode cpu)
        result (wrap-byte (rotate-byte val 1 cpu))]
    (funcall setf-form result)
    (set-flags-if cpu :carry (logbitp 7 val))
    (set-flags-nz cpu result)))

(defopcode ror (:docs "Rotate Right" :addr-style :mixed)
    [[0x66 5 2 ZeroPage]
     [0x6a 2 1 Accumulator]
     [0x6e 6 3 Absolute]
     [0x76 6 2 ZeroPageX]
     [0x7e 7 3 AbsoluteX]]
  (let [val (funcall mode cpu)
        result (wrap-byte (rotate-byte val -1 cpu))]
    (funcall setf-form result)
    (set-flags-if cpu :carry (logbitp 0 val))
    (set-flags-nz cpu result)))

(defopcode rti (:docs "Return from Interrupt")
    [[0x40 6 1 Implied]]
  (setf (cpu-sr cpu) (logior (stack-pop cpu) 0x20))
  (setf (cpu-pc cpu) (stack-pop-word cpu)))

(defopcode rts (:docs "Return from Subroutine" :track-pc nil)
    [[0x60 6 1 Implied]]
  (setf (cpu-pc cpu) (1+ (stack-pop-word cpu))))

; TODO: Add support for Decimal mode. (not supported on NES)
(defopcode sbc (:docs "Subtract from Accumulator with Carry")
    [[0xe1 6 2 IndirectX]
     [0xe5 3 2 ZeroPage]
     [0xe9 2 2 Immediate]
     [0xed 4 3 Absolute]
     [0xf1 5 2 IndirectY]
     [0xf5 4 2 ZeroPageX]
     [0xf9 4 3 AbsoluteY]
     [0xfd 4 3 AbsoluteX]]
  (flet [flip-bit (position x) (logxor (expt 2 position) x)))
    (let [val (funcall mode cpu)
          result (- (cpu-ar cpu) val (flip-bit 0 (status-bit :carry cpu)))]
      (set-flags-if cpu :zero (zerop (wrap-byte result))
                    :overflow (overflow-p result (cpu-ar cpu) (flip-bit 7 val))
                    :negative (logbitp 7 result)
                    :carry (not (minusp result)))
      (setf (cpu-ar cpu) (wrap-byte result)))))

(defopcode sec (:docs "Set Carry Flag")
    [[0x38 2 1 Implied]]
  (setf (status-bit :carry cpu) 1))

(defopcode sed (:docs "Set Decimal Flag")
    [[0xf8 2 1 Implied]]
  (setf (status-bit :decimal cpu) 1))

(defopcode sei (:docs "Set Interrupt Flag")
    [[0x78 2 1 Implied]]
  (setf (status-bit :interrupt cpu) 1))

(defopcode sta (:docs "Store Accumulator" :addr-style :raw)
    [[0x81 6 2 IndirectX]
     [0x85 3 2 ZeroPage]
     [0x8d 4 3 Absolute]
     [0x91 6 2 IndirectY]
     [0x95 4 2 ZeroPageX]
     [0x99 5 3 AbsoluteY]
     [0x9d 5 3 AbsoluteX]]
  (funcall setf-form (cpu-ar cpu)))

(defopcode stx (:docs "Store X register" :addr-style :raw)
    [[0x86 3 2 ZeroPage]
     [0x8e 4 3 Absolute]
     [0x96 4 2 ZeroPageY]]
  (funcall setf-form (cpu-xr cpu)))

(defopcode sty (:docs "Store Y register" :addr-style :raw)
    [[0x84 3 2 ZeroPage]
     [0x8c 4 3 Absolute]
     [0x94 4 2 ZeroPageX]]
  (funcall setf-form (cpu-yr cpu)))

(defopcode tax (:docs "Transfer Accumulator to X register")
    [[0xaa 2 1 Implied]]
  (let [result (setf (cpu-xr cpu) (cpu-ar cpu))]
    (set-flags-nz cpu result)))

(defopcode tay (:docs "Transfer Accumulator to Y register")
    [[0xa8 2 1 Implied]]
  (let [result (setf (cpu-yr cpu) (cpu-ar cpu))]
    (set-flags-nz cpu result)))

(defopcode tsx (:docs "Transfer Stack Pointer to X register")
    [[0xba 2 1 Implied]]
  (let [result (setf (cpu-xr cpu) (cpu-sp cpu))]
    (set-flags-nz cpu result)))

(defopcode txa (:docs "Transfer X register to Accumulator")
    [[0x8a 2 1 Implied]]
  (let [result (setf (cpu-ar cpu) (cpu-xr cpu))]
    (set-flags-nz cpu result)))

(defopcode txs (:docs "Transfer X register to Stack Pointer")
    [[0x9a 2 1 Implied]]
  (setf (cpu-sp cpu) (cpu-xr cpu)))

(defopcode tya (:docs "Transfer Y register to Accumulator")
    [[0x98 2 1 Implied]]
  (let [result (setf (cpu-ar cpu) (cpu-yr cpu))]
    (set-flags-nz cpu result)))
