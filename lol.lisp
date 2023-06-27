(defun group (source n)
  ;; group list of values by n-length sublists
  (when (plusp n)
    (labels ((core (source acc)
               (let ((rest (nthcdr n source)))
                 (if (consp rest)
                     (core rest
                           (cons (subseq source 0 n)
                                 acc))
                     (nreverse (cons source acc))))))
      (if source (core source nil) nil))))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

  (defun flatten (x)
    (labels ((rec (x acc)
               (cond ((null x) acc)
                     ;#+(and sbcl (not lol::old-sbcl))
                     ((typep x 'sb-impl::comma) (rec (sb-impl::comma-expr x) acc))
                     ((atom x) (cons x acc))
                     (t (rec
                         (car x)
                         (rec (cdr x) acc))))))
      (rec x nil)))

  (defun g!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "g!"
                  :start1 0
                  :end1 2)))

  (defun o!-symbol-p (s)
    (and (symbolp s)
         (> (length (symbol-name s)) 2)
         (string= (symbol-name s)
                  "o!"
                  :start1 0
                  :end1 2)))

  (defun o!-symbol-to-g!-symbol (s)
    (symb "g!"
          (subseq (symbol-name s) 2))))


(defun register-allocated-fixnum ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((acc 0))
    (loop for i from 1 to 100 do
          (incf (the fixnum acc)
                (the fixnum i)))
    acc))

;; (defmacro lambda (&whole form &rest bvl-decls-and-body)
;;   (declare (ignore bvl-decls-and-body))
;;   `#',form)

(defun lambda-returner ()
  (lambda (x) (* x x)))

(defun let-over-lambda-returner ()
  (let ((y 2))
    (lambda (x)
      (incf y x))))


(defun block-scanner (trigger-string)
  (let ((trig (coerce trigger-string 'list))
        curr-states)
    (lambda (data-string)
      (loop for c across data-string do
        (let (new-states)
          (if (char= c (car trig))
              (push (cdr trig) new-states))
          (loop for s in curr-states do
            (if (char= c (car s))
                (push (cdr s) new-states)))
          (setf curr-states new-states))
        (if (remove-if-not 'null curr-states)
            (return t))))))


(let ((direction t))
  (defun toogle-counter-direction ()
    (setf direction (if direction nil t)))

  (defun counter-class ()
    (let ((counter 0))
      (lambda ()
        (incf counter (if direction 1 -1))))))


;;; chapter 3 starts
(defun sleep-units% (value unit)
  (sleep
   (* value
      (case unit
        ((s) 1)
        ((m) 60)
        ((h) 3600)
        ((d) 86400)
        ((ms) 1/1000)
        ((us) 1/1000000)
        (t 1)))))


(defmacro sleep-units (value unit)
  `(sleep
    (* ,value
       ,(case unit
          ((s) 1)
          ((m) 60)
          ((h) 3600)
          ((d) 86400)
          ((ms) 1/1000)
          ((us) 1/1000000)
          (t 1)))))

(defmacro unit-of-time (value unit)
  `(* ,value
      ,(case unit
         ((s) 1)
         ((m) 60)
         ((h) 3600)
         ((d) 86400)
         ((ms) 1/1000)
         ((us) 1/1000000)
         (t 1))))


(defmacro nlet (n letargs &rest body)
  `(labels ((,n ,(mapcar #'car letargs)
              ,@body))
     (,n ,@ (mapcar #'cadr letargs))))

(defun nfact (n)
  (nlet fact ((n n))
        (if (zerop n)
            1
            (* n (fact (1- n))))))

(defmacro nif-buggy (expr pos zero neg)
  ;; first edition
  `(let ((obscure-name ,expr))
     (cond ((plusp obscure-name)
            ,pos)
           ((zerop obscure-name)
            ,zero)
           (t ,neg))))

(defmacro nif% (expr pos zero neg)
  ;; second edition
  (let ((g (gensym)))
    `(let ((,g ,expr))
       (cond ((plusp ,g)
              ,pos)
             ((zerop ,g)
              ,zero)
             (t ,neg)))))

(defmacro defmacro/g! (name args &rest body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    `(defmacro ,name ,args
       (let ,(mapcar
              (lambda (s)
                `(,s (gensym ,(subseq
                               (symbol-name s)
                               2))))
              syms)
         ,@body))))

(defmacro defmacro! (name args &rest body)
  (let* ((os (remove-if-not #'o!-symbol-p (flatten args)))
         (gs (mapcar #'o!-symbol-to-g!-symbol os)))
    `(defmacro/g! ,name ,args
       `(let ,(mapcar #'list (list ,@gs) (list ,@os))
          ,(progn ,@body)))))


(defmacro! nif (o!expr pos zero neg)
  `(cond ((plusp ,g!expr) ,pos)
         ((zerop ,g!expr) ,zero)
         (t ,neg)))

(defmacro defun! (name args &body body)
  (let ((syms (remove-duplicates
               (remove-if-not #'g!-symbol-p
                              (flatten body)))))
    `(defun ,name ,args
       (let ,(mapcar (lambda (s)
                       `(,s (gensym ,(subseq (symbol-name s)
                                             2))))
              syms)
         ,@body))))

;;; chapter 4 starts
;; (defun dangerous-use-of-bq ()
;;   `(a ,.'(b c d) e))

(defun safer-use-of-bq ()
  `(a ,.(mapcar #'identity '(b c d)) e))

(defun print-form-read ()
  (let ((*print-pretty*))
    (print '`(football-game
              (game-started-at
               ,(get-internal-real-time))
              (coin-flip
               ,(if (zerop (random 2))
                    'heads
                    'tails))))
    t))

;; the result
;; (sb-int:quasiquote
;;  (football-game
;;   (game-started-at
;;    #s(sb-impl::comma :expr (get-internal-real-time) :kind 0))
;;   (coin-flip
;;    #s(sb-impl::comma :expr
;;                      (if (zerop (random 2))
;;                          (quote heads)
;;                          (quote tails))
;;                      :kind 0))))

;; nestable suggestion from daniel herring
(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun |#"-reader| (stream sub-char numarg)
   (declare (ignore sub-char numarg))
   (let (chars (state 'normal) (depth 1))
     (loop do
       (let ((curr (read-char stream)))
         (cond ((eq state 'normal)
                (cond ((char= curr #\#)
                       (push #\# chars)
                       (setq state 'read-sharp))
                      ((char= curr #\")
                       (setq state 'read-quote))
                      (t
                       (push curr chars))))
               ((eq state 'read-sharp)
                (cond ((char= curr #\")
                       (push #\" chars)
                       (incf depth)
                       (setq state 'normal))
                      (t
                       (push curr chars)
                       (setq state 'normal))))
               ((eq state 'read-quote)
                (cond ((char= curr #\#)
                       (decf depth)
                       (if (zerop depth) (return))
                       (push #\" chars)
                       (push #\# chars)
                       (setq state 'normal))
                      (t
                       (push #\" chars)
                       (if (char= curr #\")
                           (setq state 'read-quote)
                           (progn
                             (push curr chars)
                             (setq state 'normal)))))))))
     (coerce (nreverse chars) 'string))))

;(set-dispatch-macro-character #\# #\" #'|#"-reader|)

;;; this version is from martin dirichs
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun |#>-reader| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let (chars)
      (do ((curr (read-char stream)
                 (read-char stream)))
          ((char= #\newline curr))
        (push curr chars))
      (let ((pattern (nreverse chars))
            output)
        (labels ((match (pos chars)
                   (if (null chars)
                       pos
                       (if (char= (nth pos pattern) (car chars))
                           (match (1+ pos) (cdr chars))
                           (match 0 (cdr (append (subseq pattern 0 pos) chars)))))))
          (do (curr
               (pos 0))
              ((= pos (length pattern)))
            (setf curr (read-char stream)
                  pos (match pos (list curr)))
            (push curr output))
          (coerce
           (nreverse
            (nthcdr (length pattern) output))
           'string))))))

;(set-dispatch-macro-character #\# #\> #'|#>-reader|)


(defun segment-reader (stream ch n)
  (if (> n 0)
      (let ((chars))
        (do ((curr (read-char stream)
                   (read-char stream)))
            ((char= ch curr))
          (push curr chars))
        (cons (coerce (nreverse chars) 'string)
              (segment-reader stream ch (- n 1))))))

#+cl-ppcre
(defmacro! match-mode-ppcre-lambda-form (o!args o!mods)
 ``(lambda (,',g!str)
     (ppcre:scan-to-strings
       ,(if (zerop (length ,g!mods))
          (car ,g!args)
          (format nil "(?~a)~a" ,g!mods (car ,g!args)))
       ,',g!str)))

#+cl-ppcre
(defmacro! subst-mode-ppcre-lambda-form (o!args)
 ``(lambda (,',g!str)
     (cl-ppcre:regex-replace-all
       ,(car ,g!args)
       ,',g!str
       ,(cadr ,g!args))))


#+cl-ppcre
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun |#~-reader| (stream sub-char numarg)
    (declare (ignore sub-char numarg))
    (let ((mode-char (read-char stream)))
      (cond
        ((char= mode-char #\m)
         (match-mode-ppcre-lambda-form
          (segment-reader stream
                          (read-char stream)
                          1)
          (coerce (loop for c = (read-char stream)
                     while (alpha-char-p c)
                     collect c
                     finally (unread-char c stream))
                  'string)))
        ((char= mode-char #\s)
         (subst-mode-ppcre-lambda-form
          (segment-reader stream
                          (read-char stream)
                          2)))
        (t (error "unknown #~~ mode character"))))))

#+cl-ppcre
;(set-dispatch-macro-character #\# #\~ #'|#~-reader|)


(defun cyclic-p (l)
  (cyclic-p-aux l (make-hash-table)))

(defun cyclic-p-aux (l seen)
  (if (consp l)
      (or (gethash l seen)
          (progn
            (setf (gethash l seen) t)
            (or (cyclic-p-aux (car l) seen)
                (cyclic-p-aux (cdr l) seen))))))

(defparameter safe-read-from-string-blacklist
  '(#\# #\: #\|))

(let ((rt (copy-readtable nil)))
  (defun safe-reader-error (stream closech)
    (declare (ignore stream closech))
    (error "safe-read-from-string failure"))

  (dolist (c safe-read-from-string-blacklist)
    (set-macro-character
     c #'safe-reader-error nil rt))

  (defun safe-read-from-string (s &optional fail)
    (if (stringp s)
        (let ((*readtable* rt)
              *read-eval*)
          (handler-bind
              ((error (lambda (condition)
                        (declare (ignore condition))
                        (return-from
                         safe-read-from-string fail))))
            (read-from-string s)))
        fail)))


;;; chapter 5
(defun defunits-changing (u units prev)
  (when (member u prev)
    (error "~{ ~a~^ depends on~}"
           (cons u prev)))
  (let ((spec (find u units :key #'car)))
    (if (null spec)
        (error "unknown unit ~a" u)
        (let ((chain (cadr spec)))
          (if (listp chain)
              (* (car chain)
                 (defunits-changing
                     (cadr chain)
                   units
                   (cons u prev)))
              chain)))))



;; (defmacro! defunits (quantity base-unit &rest units)
;;   `(defmacro ,(symb 'unit-of- quantity)
;;        (,g!val ,g!un)
;;      `(* ,,g!val
;;          ,(case ,g!un
;;             ((,base-unit) 1)
;;             ,@(mapcar (lambda (x)
;;                         `((,(car x))
;;                           ,(defunits-changing
;;                                (car x)
;;                                (cons
;;                                 `(,base-unit 1)
;;                                 (group units 2))
;;                              nil)))
;;                (group units 2))))))

(defun predicate-slitter (orderp splitp)
  (lambda (a b)
    (let ((s (funcall splitp a)))
      (if (eq s (funcall splitp b))
          (funcall orderp a b)
          s))))

(defun tree-leaves%% (tree test result)
  (if tree
      (if (listp tree)
          (cons
           (tree-leaves%% (car tree) test result)
           (tree-leaves%% (cdr tree) test result))
          (if (funcall test tree)
              (funcall result tree)
              tree))))

(defmacro tree-leaves (tree test result)
  `(tree-leaves%%
    ,tree
    (lambda (x)
      (declare (ignorable x))
      ,test)
    (lambda (x)
      (declare (ignorable x))
      ,result)))

;; (defmacro! nlet-tail (n letargs &rest body)
;;   (let ((gs (loop for i in letargs
;;                   collect (gensym))))
;;     `(macrolet
;;          ((,n ,gs
;;             `(progn
;;                (psetq ,@ (apply #'nconc
;;                                 (mapcar
;;                                  #'list
;;                                  ',(mapcar #'car letargs)
;;                                  (list ,@gs))))
;;                (go ,',g!n))))
;;        (block ,g!b
;;          (let ,letargs
;;            (tagbody
;;               ,g!n (return-from ,g!b (progn ,@body))))))))

(defmacro! nlet-tail (n letargs &rest body)
  (let ((gs (loop for i in letargs
                  collect (gensym))))
    `(macrolet
         ((,n ,gs
            `(progn
               (psetq
                ,@(apply #'nconc
                         (mapcar
                          #'list
                          ',(mapcar #'car letargs)
                          (list ,@gs))))
               (go ,',g!n))))
       (block ,g!b
         (let ,letargs
           (tagbody
              ,g!n (return-from
                    ,g!b (progn ,@body))))))))



(defparameter *cxr-inline-thresh* 10)

;; (defmacro! cxr (x tree)
;;   (if (null x)
;;       tree
;;       (let ((op (cond
;;                   ((eq 'a (cadr x)) 'car)
;;                   ((eq 'd (cadr x)) 'cdr)
;;                   (t (error "non a/d symbol")))))
;;         (if (and (integerp (car x))
;;                  (<= 1 (car x) *cxr-inline-thresh*))
;;             (if (= 1 (car x))
;;                 `(,op (cxr ,(cddr x) ,tree))
;;                 `(,op (cxr ,(cons (- (car x) 1) (cdr x))
;;                            ,tree)))
;;             `(nlet-tail
;;               ,g!name ((,g!count ,(car x))
;;                        (,g!val (cxr ,(cddr x) ,tree)))
;;               (if (>= 0 ,g!count)
;;                   ,g!val
;;                   (,g!name (- ,g!count 1)
;;                            (,op ,g!val))))))))
(defmacro! cxr (x tree)
  (if (null x)
      tree
      (let ((op (cond
                  ((eq 'a (cadr x)) 'car)
                  ((eq 'd (cadr x)) 'cdr)
                  (t (error "non a/d symbol")))))
        (if (and (integerp (car x))
                 (<= 1 (car x) *cxr-inline-thresh*))
            (if (= 1 (car x))
                `(,op (cxr ,(cddr x) ,tree))
                `(,op (cxr ,(cons (- (car x) 1) (cdr x))
                           ,tree)))
            `(nlet-tail
              ,g!name ((,g!count ,(car x))
                       (,g!val (cxr ,(cddr x) ,tree)))
              (if (>= 0 ,g!count)
                  ,g!val
                  (,g!name (- ,g!count 1)
                           (,op ,g!val))))))))


(defmacro def-english-list-accessors (start end)
  (if (not (<= 1 start end))
      (error "bad start/end range"))
  `(progn
     ,@(loop for i from start to end
             collect `(defun
                          ,(symb
                            (map 'string
                                 (lambda (c)
                                   (if (alpha-char-p c)
                                       (char-upcase c)
                                       #\-))
                                 (format nil "~:r" i)))
                          (arg)
                        (cxr (1 a ,(- i 1) d) arg)))))


(defmacro! dlambda (&rest ds)
  `(lambda (&rest ,g!args)
     (case (car ,g!args)
       ,@(mapcar
          (lambda (d)
            `(,(if (eq t (car d))
                   t
                   (list (car d)))
              (apply (lambda ,@(cdr d))
                     ,(if (eq t (car d))
                          g!args
                          `(cdr ,g!args)))))
          ds))))


(let ((count 0))
  (defun set-counter ()
    (setf (symbol-function 'count-test)
          (dlambda
           (:reset () (setf count 0))
           (:inc (n) (incf count n))
           (:dec (n) (decf count n))
           (:bound (lo hi)
                   (setf count
                         (min hi (max lo count))))))))

;;; chapter 6
(defmacro alambda (params &body body)
  `(labels ((self ,params ,@body))
     #'self))

(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))

(eval-when (:compile-toplevel :execute :load-toplevel)
  (defun |#`-reader| (stream sub-char numarg)
    (declare (ignore sub-char))
    (unless numarg (setq numarg 1))
    `(lambda ,(loop for i from 1 to numarg
                 collect (symb 'a i))
       ,(funcall
         (get-macro-character #\`) stream nil)))

  (defun |#f-reader| (stream sub-char numarg)
    (declare (ignore stream sub-char))
    (setq numarg (or numarg 3))
    (unless (<= numarg 3)
      (error "bad value for #f: ~a" numarg))
    `(declare (optimize (speed ,numarg)
                        (safety ,(- 3 numarg)))))

  ;; (defreadtable lol-syntax
  ;;   (:merge :standard)
  ;;   (:dispatch-macro-char #\# #\" #'|#"-reader|)
  ;;   (:dispatch-macro-char #\# #\> #'|#>-reader|)
  ;;   #+cl-ppcre
  ;;   (:dispatch-macro-char #\# #\~ #'|#~-reader|)
  ;;   (:dispatch-macro-char #\# #\` #'|#`-reader|)
  ;;   (:dispatch-macro-char #\# #\f #'|#f-reader|));
)


(defmacro alet% (letargs &rest body)
  `(let ((this) ,@letargs)
     (setf this ,@(last body))
     ,@(butlast body)
     this))

(defmacro alet (letargs &rest body)
  `(let ((this) ,@letargs)
     (setq this ,@(last body))
     ,@(butlast body)
     (lambda (&rest params)
       (apply this params))))


(defmacro! ichain-before (&rest body)
  `(let ((,g!indir-env this))
     (setq this
           (lambda (&rest ,g!temp-args)
             ,@body
             (apply ,g!indir-env
                    ,g!temp-args)))))

(defmacro! ichain-after (&rest body)
  `(let ((,g!indir-env this))
     (setq this
           (lambda (&rest ,g!temp-args)
             (prog1
                 (apply ,g!indir-env
                        ,g!temp-args
                        ,@body))))))


