(ns cljs-6502.disassembler
  (:require [clojure.string :only [cl-format]]))

(defmacro with-disasm
  "Loop from START to END, passing each instruction to OP and execute BODY.
OP is PRINT-INSTRUCTION by default. Within BODY, the return value of OP is
bound to RESULT and the length of the instruction in bytes is bound to STEP."
  [[start end op] & body]
  )

(defn disasm
  "Disassemble memory from START to END."
  [start end]
  (with-disasm (start end print-instruction)))

(defn disasm-to-list
  "Disassemble a given region of memory into a sexp-based format."
  [start end]
  (with-disasm (start end sexpify-instruction)))

(defn disasm-to-str
  "Call DISASM with the provided args and return its output as a string."
  [start end]
  (with-output-to-string (*standard-output*) (disasm start end)))

(defn disasm-ins
  "Lookup the metadata for the instruction at INDEX and pass it to
DISASM-OP for formatting and display, returning the instruction length."
  [index disasm-op]
  (let [[name cycles size mode] (nth opcodes (get-byte index))
        code-block (get-range index (+ index size))]
    (list size (disasm-op code-block index name mode))))

(defn print-instruction
  "Format the instruction at INDEX and its operands for display."
  [bytes index name mode]
  (let [byte-str (format nil "~{~2,'0x ~}" bytes)
        args-str (format nil "~A ~A" name (arg-formatter (rest bytes) mode))
        docs (documentation name 'function)]
    (printf "$~4,'0x   ~9A  ;; ~14A ~A~%" index byte-str args-str docs)))

(defn sexpify-instruction
  "Given BYTES and metadata, return a sexp-format representation of it."
  [bytes index name mode]
  ;; TODO: clean this up
  (if-let [args (rest bytes)
           args-str (transform-str-syntax bytes mode)]
    (map keyword (list name args-str))
    (map keyword (list name))))

;; TODO: finish arg-formatter and transform-str-syntax. TESTS!
(defn arg-formatter
  "Given an instruction's ARG, format it for display using the MODE's PRINTER."
  [arg mode]
  (if (member mode '(absolute absolute-x absolute-y indirect))
      (format nil (printer mode) (reverse arg))
      (format nil (printer mode) arg)))

(defn transform-str-syntax
  "TODO: docstring"
  [bytes mode]
  (let ((result (arg-formatter (rest bytes) mode)))
    (flet ((munge-indirect (str)
             (cl-ppcre:regex-replace "\\(\\$(.*)\\)(.*)?" str "!\\1\\2")))
      (cl-ppcre:regex-replace ", " (munge-indirect result) "."))))

(defn current-instruction
  "Return a list representing the current instruction. If PRINT-P is non-nil,
print the current address and instruction and return NIL."
  [cpu print?]
  (let [fn (if print? print-instruction sexpify-instruction)]
    (second (disasm-ins (:pc cpu) fn))))
