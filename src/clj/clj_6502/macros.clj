(ns clj-6502.macros
  (:refer-clojure :exclude [bytes]))

(defmacro defenum
  "Define an Enum: a function %NAME that can be called with a keyword
to retrieve the corresponding index."
  [name keys]
  `(defn ~(symbol (str "%" name)) [key]
     (let [enum# ~(zipmap keys (range))]
       (get enum# key))))

(defmacro branch-if
  "Take a Relative branch if PREDICATE is true, otherwise increment the PC."
  [predicate]
  `(if ~predicate
     (set-register :pc (getter (Relative.)))
     (swap! cpu update-in [:pc] inc)))

(defmacro defaddress
  "Define an Addressing Mode: a type NAME and an implementation of the
AddressingMode protocol for that type. BODY should compute an address
and will be used by getter and setter. If CPU-REG is non-nil, BODY will be
wrapped in a get-byte call for setting. READER should be a regex that
can read assembly in the mode. PRINTER should be the format string
desired for disassembly of the mode."
  [name {:keys [reader writer cpu-reg]} & body]
  (let [getter `(fn [] ~@(if cpu-reg
                           `((cljs-6502.cpu/get-register ~@body))
                           body))
        setter `(fn [val] ~(if cpu-reg
                             `(cljs-6502.cpu/set-register ~@body val)
                             `(cljs-6502.cpu/set-byte ~@body val)))]
    `(do
       (deftype ~name [])
       (extend-protocol cljs-6502.addressing/AddressingMode
         ~name
         (getter [mode#] ~getter)
         (setter [mode#] ~setter)
         (reader [mode#] ~reader)
         (writer [mode#] ~writer)))))

(defmacro defopcode
  "Define an Opcode: a method NAME that takes an OPCODE and AdressingMode,
executes BODY, and updates the cpu's program counter and cycle count."
  [name {:keys [mode track-pc]} & body]
  (let [[op cycles bytes addr-mode] mode]
    `(defmethod ~name ~op
       [opcode mode]
       ~@body
       ~@(when (and track-pc (> bytes 1))
           `((swap! cpu update-in [:pc] (fn [x] (+ x ~(dec bytes))))))
       (swap! cpu update-in [:cc] (fn [x] (+ x ~cycles))))))

(defmacro defasm
  "Define an Assembly Mnemonic: a multimethod NAME with specializations
for each of the provided MODES that executes BODY. ADDR-STYLE should be one
of :raw, :mixed, or nil. TRACK-PC can be passed nil to short-circuit
PC incrementing for opcodes that modify the PC."
  [name {:keys [docs addr-style track-pc]} modes & body]
  `(do
     (doseq [[op cycles bytes mode] ~modes]
       (swap! cljs-6502.cpu/opcodes update-in [op]
              (fn [x] [~name cycles bytes mode])))
     (defmulti ~name ~docs (fn [opcode mode] opcode))
     ~@(for [mode modes]
         `(defopcode ~name {:mode ~mode
                            :track-pc ~track-pc}
            (swap! cljs-6502.cpu/cpu update-in [:pc] inc)
            ~@body))))
