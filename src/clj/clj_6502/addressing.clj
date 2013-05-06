(ns clj-6502.addressing)

(defmacro defaddress
  [name {:keys [reader writer]} & body]
  (let [getter `(fn [] (get @ram ~@body))
        setter `(fn [~'val]
                  (let [~'address ~@body]
                    (swap! ram update-in [~'address] ~'val)))]
    `(do
       (deftype ~name [])
       (extend-protocol cljs-6502.addressing/AddressingMode
         ~name
         (getter [mode#] ~getter)
         (setter [mode#] ~setter)
         (reader [mode#] ~reader)
         (writer [mode#] ~writer)))))
