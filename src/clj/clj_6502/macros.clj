(ns clj-6502.macros
  (:refer-clojure :exclude [bytes]))

(defmacro defenum
  "Define an Enum: a function %NAME that can be called with a keyword
to retrieve the corresponding index."
  [name keys]
  `(defn ~(symbol (str "%" name)) [~'key]
     (let [enum# ~(zipmap keys (range))]
       (get enum# ~'key))))

(defmacro branch-if
  "Take a Relative branch if PREDICATE is true, otherwise increment the PC."
  [predicate]
  `(if ~predicate
     (set-register :pc (getter (Relative.)))
     (swap! cpu update-in [:pc] inc)))

(defmacro getter-mixed
  "Special-case the handling of accumulator mode in ASL/LSR/ROL/ROR."
  []
  `(if (= (type mode) ~'accumulator)
     (getter mode raw?)
     (get-byte (getter mode raw?))))

(defmacro defaddress
  "Define an Addressing Mode: a type NAME and an implementation of the
AddressingMode protocol for that type. BODY should compute an address
and will be used by getter and setter. If CPU-REG is non-nil, BODY will be
wrapped in a get-byte call for setting. READER should be a regex that
can read assembly in the mode. PRINTER should be the format string
desired for disassembly of the mode."
  [name {:keys [reader writer cpu-reg]} & body]
  (let [getter-body (if cpu-reg
                      `((cljs-6502.cpu/get-register ~@body))
                      body)])
  `(do
     (deftype ~name [])
     (extend-protocol cljs-6502.addressing/AddressingMode
       ~name
       (~'getter [mode# raw?#] `(if raw?#
                                  ~@getter-body
                                  (get-byte ~@getter-body)))
       (~'setter [mode# val#] ~(if cpu-reg
                                 `(cljs-6502.cpu/set-register ~@body val#)
                                 `(cljs-6502.cpu/set-byte ~@body val#)))
       (~'reader [mode#] ~reader)
       (~'writer [mode#] ~writer))))

(defmacro defasm
  "Define an Assembly Mnemonic: a function NAME that takes an array of
opcode metadata and executes BODY. DOCS is the documentation for the
opcode. TRACK-PC? can be passed nil to short-circuit PC incrementing
for opcodes that modify the PC. RAW? controls whether the addressing mode's
GETTER returns an address directly or the byte at that address."
  [name {:keys [docs raw? track-pc?]} modes & body]
  `(do
     `(defn ~name
        ~docs
        [~'cycles ~'bytes ~'mode ~'raw?]
        (swap! cljs-6502.cpu/cpu update-in [:pc] inc)
        ~@body
        (when (clojure.core/and ~track-pc? (> bytes 1))
          (swap! cljs-6502.cpu/cpu update-in [:pc] #(+ % (dec bytes))))
        (swap! cljs-6502.cpu/cpu update-in [:cc] #(+ % ~'cycles)))
     (doseq [[op# cycles# bytes# mode#] ~modes]
       (swap! cljs-6502.cpu/opcodes update-in [op#]
              (fn [x#] [~name cycles# bytes# (new mode#) ~raw?])))))
