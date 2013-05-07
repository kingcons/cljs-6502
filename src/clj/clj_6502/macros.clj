(ns clj-6502.macros)

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
        setter `(fn [~'val] ~(if cpu-reg
                               `(cljs-6502.cpu/set-register ~@body ~'val)
                               `(cljs-6502.cpu/set-byte ~@body ~'val)))]
    `(do
       (deftype ~name [])
       (extend-protocol cljs-6502.addressing/AddressingMode
         ~name
         (getter [mode#] ~getter)
         (setter [mode#] ~setter)
         (reader [mode#] ~reader)
         (writer [mode#] ~writer)))))

(defmacro defenum
  "Define an Enum: a function %NAME that can be called with a keyword
to retrieve the corresponding index."
  [name keys]
  `(defn ~(symbol (str "%" name)) [~'key]
     (let [enum# ~(zipmap keys (range))]
       (get enum# ~'key))))
