(ns cljs-6502.assembler)

(defprotocol AssemblyCode
  "A Protocol for objects that can be Assembled into 6502 machine code."
  (asm [source] "Assemble SOURCE into a bytevector of 6502 machine code."))

;; ;; TODO
;; (extend-type string
;;   AssemblyCode
;;   (asm [source]
;;     (let [tokenized (map tokenize-statement (string/split "\\n" source))]
;;       (asm (remove nil tokenized)))))

;; (extend-type clojure.lang.PersistentVector
;;   AssemblyCode
;;   (asm [source]
;;     (if (seq? (first source))
;;       (apply concatvec (map process-statement source))
;;       (apply vec (process-statement source)))))

(defn process-statement
  "Given a symbolic assembly STATEMENT, convert it to a list of bytes."
  [statement]
  (let [[op & args] (map str statement)
        mode (match-mode args)]
    (if (> (length args) 2)
      (throw 'invalid-syntax statement)
      (list* (find-opcode op mode) (process-args args mode)))))

(defn match-mode
  "Given a list of args, TOKENS, match them to an addressing mode or return nil."
  [tokens]
  (let [line (apply str (transform-sexp-syntax tokens))
        modes (keys (:impls AddressingMode))]
    (filter #(re-find (reader (new %)) line) modes)))

(defn find-opcode
  "Find an opcode matching NAME and MODE, raising ILLEGAL-OPCODE otherwise."
  [name mode]
  (let [insts (filter #(= mode (nth % 3 nil)) opcodes)
        match ]
    (if match
      (contains? match opcodes)
      (throw 'illegal-opcode (list name mode)))))

(defn process-args
  "Take a list of args TOKENS and produce an appropriate 6502 bytevector."
  [tokens mode]
  (remove nil (flatten (map #(extract-num % mode) tokens))))

(defn transform-sexp-syntax
  "Given a SEXP-token using an indirect, *.x or *.y addressing mode, transform
it to use the classic string assembly syntax."
  [tokens]
  (letfn [(munge-indirect [x]
            (string/replace x #"\!([^.]*)(.*)?" "($\\1)\\2"))]
    (map #(apply str (replace {\. \,} (munge-indirect %))) tokens))

(defn extract-num
  "Extract a hex number from its containing string, X."
  [x {:keys [mode]}]
  (let [token (re-scan "[0-9a-fA-F]{2,4}" x)]
    (when token
      (if (member mode '(absolute abs-x abs-y indirect)) ; TODO
        (map read-string [(subs token 2) (subs token 0 2)])
        (read-string token)))))

(defn tokenize-statement [statement]
  (let [results (string/split (string/trim statement) #" ")]
    (map keyword results)))
