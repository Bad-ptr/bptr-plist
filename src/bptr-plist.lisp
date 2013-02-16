(in-package :cl-user)

(defpackage :bptr-plist
  (:nicknames :bpplist :cl-bptr-plist)
  (:documentation "Functions that operate on property lists.
The lists of form (:a 1 :b 2 :c :d 3 e 3 4) etc.
Allow 'empty' keys. Keywords can't be the values of keys.")
  (:use :cl)
  (:export #:for-kv

           #:plist-strict-p
           #:plist-p
           #:plist
           #:plist-strict
           
           #:full-copy
           #:key-value-pairs
           #:not-paired

           #:list-keys
           #:list-keys-that-not-keywords
           #:list-keys-with-nonkeywords
           #:list-empty-keys

           #:list-values
           #:list-values-that-keyless
           #:list-values-with-keyless

           #:what-keys-exists
           #:whata-keys-exists
           #:if-key-exists

           #:get-keys-values
           #:geta-keys-values
           #:get-key-value

           #:include-keys
           #:includea-keys
           #:include-key

           #:exclude-keys
           #:excludea-keys
           #:exclude-key

           #:set-keys-values
           #:seta-keys-values
           #:set-key-value
           
           #:append-keys-values
           #:appenda-keys-values
           #:append-key-value

           #:append-or-set-keys-values
           #:append-or-seta-keys-values
           #:append-or-set-key-value

           #:remf-keys
           #:remfa-keys
           #:remf-key
           
           #:setf-keys
           #:setfa-keys
           #:setf-key

           #:remf-keyless
           #:remf-empty-keys))

(in-package :bptr-plist)

#|
Functions to work with property lists.
 (The lists of form '(:a 1 :b 2 :c :d 3 e 34) etc.)
|#

(defmacro for-kv (plist
                  (skey sval
                        &key
                        ((:initially initially-form))
                        ((:finally finally-form))
                        ((:while while-form))
                        ((:until until-form))
                        (empty-value t)
                        loopvars)
                  &body body)

  "`for' macro adopted to loop over keys and values of plist.
Example:
 (for-kv '(:a 1 :b 2 :c :d 3) (k v)
        when (keywordp k)
        do (format t \"~s : ~s~%\" k v)
        end)"

  (let ((cname (gensym)))
    `(let (,skey ,sval)
       (loop :for
          ,@(and loopvars (nconc loopvars '(:and)))
          ,cname on ,plist
          :by #'(lambda (plst)
                  (if (keywordp (first plst))
                      (if (keywordp (second plst))
                          (rest plst)
                          (cddr plst))
                      (rest plst)))
          ,@(and initially-form (list :initially initially-form))
          ,@(and while-form (list :while while-form))
          ,@(and until-form (list :until until-form))
          :do (setq ,skey (first ,cname))
          :if (keywordp ,skey)
            :if (rest ,cname)
              :do (setq ,sval (second ,cname)) :and
              :when (keywordp ,sval)
                :do (setq ,sval ,empty-value)
              :end
              :and ,@body
            :else :do (setq ,sval ,empty-value) :and
              ,@body
          :else :do (setq ,sval ,empty-value) :and
            ,@body
          ,@(and finally-form (list :finally finally-form))))))



(defun plist-strict-p (object &optional (element-type '*))

  "Returns true if OBJECT is a 'clean' property list.
  Examples:
  (plist-strict-p 1) => NIL ;;
  (plist-strict-p '(1 2 3) 'integer) => NIL ;;
  (plist-strict-p '(foo)) => NIL ;;
  (plist-strict-p nil) => T ;;
  (plist-strict-p '(foo 1)) => NIL ;;
  (plist-strict-p '(:a 1 :b 2) 'integer) => T"
  
  (typecase object
    (null t)
    (cons
     (for-kv object (key val :finally (return t))
       :if (keywordp key)
         :unless (or (eq element-type '*) (typep val element-type))
           :return nil
         :end
       :else :return nil))
    (t nil)))

(defun plist-p (object &optional (element-type '*))

  "Returns true if OBJECT is a property list valid for functions in this file.
  Examples:
  (plist-p 1) => NIL ;;
  (plist-p '(1 2 3) 'integer) => T ;;
  (plist-p '(foo)) => T ;;
  (plist-p nil) => T ;;
  (plist-p '(foo 1) 'integer) => NIL ;;
  (plist-p '(:a 1 :b 2)) => T"
  
  (typecase object
    (null t)
    (cons (if (eq element-type '*)
              t
              (loop :for el :in object
                 :finally (return t)
                 :unless (or (typep el 'keyword) (typep el element-type))
                 :return nil
                 :end)))
    (t nil)))

(deftype plist (&optional (element-type '*))

  "Property lists type."

  (let ((plistp (gensym)))
    (setf (symbol-function plistp)
          #'(lambda (obj) (plist-p obj element-type)))
    `(and list (satisfies ,plistp))))

(deftype plist-strict (&optional (element-type '*))

  "Type for 'clean' property lists."

  (let ((pstrictp (gensym)))
    (setf (symbol-function pstrictp)
          #'(lambda (obj) (plist-strict-p obj element-type)))
    `(and list (satisfies ,pstrictp))))


(defun full-copy (plist &key (empty-value t ev-supplied-p))

  "Make full copy of list.
Example:
 (full-copy '(:a 1 :b 2 :c :d 2)) => (:a 1 :b 2 :c :d 2)"

  (for-kv plist (k v :empty-value :+-123empty)
    :if (keywordp k)
      :collect k :and
      :if (eq v :+-123empty)
        :if ev-supplied-p
          :collect empty-value
        :end
      :else :collect v
    :else :collect k))


(defun key-value-pairs (plist &key (empty-value t))

  "List only correct :key-value pairs.
Example:
 (key-value-pairs '(:a 1 :b 2 :c :d 2 e 3 4)) => (:a 1 :b 2 :c t :d 2)"

  (for-kv plist (k v :empty-value empty-value)
    :when (keywordp k)
      :collect k :and :collect v
    :end))


(defun not-paired (plist)

  "List only keyless things.
Example:
 (not-paired '(:a 1 :b 2 :c :d 2 e 3 4)) => (e 3 4)"

  (for-kv plist (k v)
    :unless (keywordp k)
      :collect k
    :end))


(defun list-keys (plist)

  "List :keys.
Example:
 (list-keys '(:a 1 :b 2 :c :d 2 e 3 4)) => (:a :b :c :d)"

  (for-kv plist (k v)
    :when (keywordp k)
      :collect k
    :end))


(defun list-keys-that-not-keywords (plist)

  "List something that lies on place of key, but not of :keyword type.
Example:
 (list-keys-that-not-keywords '(:a 1 :b 2 :c :d 2 e 3 4)) => (e 3 4)"

  (for-kv plist (k v)
    :unless (keywordp k)
      :collect k
    :end))


(defun list-keys-with-nonkeywords (plist)
  
  "List keys and 'not keyword keys'.
Example:
 (list-keys-with-nonkeywords '(:a 1 :b 2 :c :d 2 e 3 4))
    => (:a :b :c :d e 3 4)"

  (for-kv plist (k v)
    :collect k))


(defun list-empty-keys (plist)

  "List keys that have not values.
Example:
 (list-empty-keys '(:a 1 :b 2 :c :d 2 e 3 4)) => (:c)"
  
  (for-kv plist (k v :empty-value :+-123empty)
      :when (and (keywordp k) (eq v :+-123empty))
        :collect k
      :end))


(defun list-values (plist &key (empty-value t))
  
  "List values of keys.
Note that it collect `empty-value' for 'empty' keys.
Example:
 (list-values '(:a 1 :b 2 :c :d 2 e 3 4)) => (1 2 t 2)"

  (for-kv plist (k v :empty-value empty-value)
    :when (keywordp k)
      :collect v
    :end))


(defun list-values-that-keyless (plist)
  
  "List values that havn't keys.
Example:
 (list-values-that-keyless '(:a 1 :b 2 :c :d 2 e 3 4)) => (e 3 4)"
  
  (for-kv plist (k v)
    :when (not (keywordp k))
      :collect k
    :end))


(defun list-values-with-keyless (plist &key (empty-value t))
  
  "List values and 'values that havn't keys'.
Note that it collect `empty-value' for 'empty' keys.
Example:
 (list-values-with-keyless '(:a 1 :b 2 :c :d 2 e 3 4))
    => (1 2 t 2 e 3 4)"
  
  (for-kv plist (k v :empty-value empty-value)
    :if (keywordp k)
      :collect v
    :else :collect k))



(defun what-keys-exists (plist keys)

  "Check all keys for existance. Return list of keys that exists.
Destructively removing keys from `KEYS' (it's for efficiency), so be warned.
Example:
 (what-keys-exists '(:a 1 :b 2 :c :d 2 e 3 4) (list :c :f :b)) => (:c :b)"

  (when plist
    (let ((plkeys (list-keys plist)))
      (loop :for key :in keys
         :while keys
         :when (find key plkeys)
           :collect key :into ret
           :and :do (setq keys (delete key keys))
         :end
         :finally (return ret)))))

(defun whata-keys-exists (plist &rest keys)

  "Alias for what-keys-exists
Example:
 (whata-keys-exists '(:a 1 :b 2 :c :d 2 e 3 4) :c :f :b) => (:c :b)"

  (what-keys-exists plist (copy-list keys)))

(defun if-key-exists (plist key)
  
  "Alias for whata-keys-exists.
Check if one key exists. Return this key or nil.
Example:
 (if-key-exists '(:a 1 :b 2 :c :d 2 e 3 4) :c) => :c"
  
  (first (whata-keys-exists plist key)))



(defun get-keys-values (plist keys &key (empty-value t) nokey-value)
  
  "Return values of keys.
Example:
 (get-keys-values '(:a 1 :b 2 :c :d 2 e 3 4) '(:c :f :b)) => (t nil 2)"
  
  (let ((ret (make-array (length keys) :initial-element nokey-value)))
    (for-kv plist (k v :while keys :empty-value empty-value)
      :do (loop :for n = 0 :then (1+ n) :and kk :in keys
             :when (eq kk k)
             :do (setf (aref ret n) v)
             :end))
    (map 'list #'identity ret)))

(defmacro geta-keys-values ((plist &rest keys) &key (empty-value t) nokey-value)

  "Alias for get-keys-values
Example:
 (geta-keys-values ('(:a 1 :b 2 :c :d 2 e 3 4) :c :f :b)) => (t nil 2)"
  
  `(get-keys-values ,plist ',keys :empty-value ,empty-value :nokey-value ,nokey-value))

(defun get-key-value (plist key &key (empty-value t) nokey-value)

  "Alias for get-keys-values for one key.
Example:
 (get-key-value '(:a 1 :b 2 :c :d 2 e 3 4) :b) => 2"

  (first (get-keys-values plist (list key)
                          :empty-value empty-value :nokey-value nokey-value)))



(defun include-keys (plist keys &key (empty-value t))

  "Return list of keys-values pairs for keys.
Destructively deleting keys from `KEYS' for efficiency, so be warned.
Example:
 (include-keys '(:a 1 :b 2 :c :d 2 e 3 4) (list :c :f :b)) => (:b 2 :c t)"
  
  (for-kv plist (k v :while keys :empty-value empty-value)
    :when (find k keys)
      :collect k :and :collect v
      :and :do (setq keys (delete k keys))
    :end))

(defmacro includea-keys ((plist &rest keys) &key (empty-value t))

  "Alias for include-keys
Example:
 (includea-keys ('(:a 1 :b 2 :c :d 2 e 3 4) :c :f :b)) => (:b 2 :c t)"
  
  `(include-keys ,plist (list ,@keys) :empty-value ,empty-value))

(defun include-key (plist key &key (empty-value t))

  "Alias for include-keys.
Example:
 (include-key '(:a 1 :b 2 :c :d 2 e 3 4) :b) => (:b 2)"

  (include-keys plist (list key) :empty-value empty-value))



(defun exclude-keys (plist keys)
  
  "Return copy of list excluding some key-value pairs.
Destructively deleting keys from `KEYS' for efficiency, so be warned.
Example:
 (exclude-keys '(:a 1 :b 2 :c :d 2 e 3 4) (list :c :f :b))
    => (:a 1 :d 2 e 3 4)"

  (for-kv plist (k v :empty-value :+-123empty)
    :if (keywordp k)
      :unless (find k keys)
        :collect k
        :and :unless (eq v :+-123empty) :collect v :end
        :and :do (setq keys (delete k keys))
      :end
    :else :collect k))

(defun excludea-keys (plist &rest keys)

  "Alias for exclude-keys.
Example:
 (excludea-keys '(:a 1 :b 2 :c :d 2 e 3 4) :c :f :b)
    => (:a 1 :d 2 e 3 4)"

  (exclude-keys plist (copy-list keys)))

(defun exclude-key (plist key)

  "Alias for excludea-keys for one key.
Example:
 (exclude-key '(:a 1 :b 2 :c :d 2 e 3 4) :b) => (:a 1 :c :d 2 e 3 4)"

  (excludea-keys plist key))


(defun set-keys-values (plist keysvals)
  
  "Return copy of list with values replaced for keys, that is in the keysvals.
Example:
 (set-keys-values '(:a 1 :b 2 :c :d 2 e 3 4) (list :c 11 :f :b 10))
    => (:a 1 :b 10 :c 11 :d 2 e 3 4)"
  (let ((lk (list-keys keysvals))
        ret)
    (for-kv plist (k v :empty-value :+-123empty)
      :if (keywordp k)
      :do (loop :named inner :for key :in lk
             :when (eq k key)
               :do (push k ret)
               :and :do (let ((gk (get-key-value keysvals k :empty-value :+-123empty)))
                          (unless (eq gk :+-123empty)
                            (push gk ret)))
               :and :do (setq lk (delete k lk))
               :and :do (return-from inner)
             :end
             :finally (push k ret) (unless (eq v :+-123empty) (push v ret)))
      :else :do (push k ret))
    (nreverse ret)))

(defun seta-keys-values (plist &rest keysvals)

  "Alias for set-keys-values.
Example:
 (seta-keys-values '(:a 1 :b 2 :c :d 2 e 3 4) :c 11 :f :b 10)
   => (:a 1 :b 10 :c 11 :d 2 e 3 4)"

  (set-keys-values plist keysvals))

(defun set-key-value (plist key &optional (val nil val-p))

  "Alias for seta-keys-values for one key-value pair.
Example:
 (set-key-value '(:a 1 :b 2 :c :d 2 e 3 4) :b 10) => (:a 1 :b 10 :c :d 2 e 3 4)"

  (if val-p (seta-keys-values plist key val) (seta-keys-values plist key)))



(defun append-keys-values (plist keysvals &key (empty-value t ev-supplied-p))
  
  "Append keysvals to list.
Example:
 (append-keys-values '(:a 1 :b 2 :c :d 2 e 3 4) '(:c 2 :f :b 1))
    => (:a 1 :b 2 :c :d 2 e 3 4 :f)"

  (let ((to-append
         (for-kv keysvals (k v :empty-value :+-123empty)
           :unless (if-key-exists plist k)
             :collect k
             :and :if (eq v :+-123empty)
               :when ev-supplied-p
                 :collect empty-value
               :end
             :else :collect v
           :end)))
    (append plist to-append)))

(defmacro appenda-keys-values ((plist &rest keysvals) &key (empty-value t ev-supplied-p))

  "Alias for append-keys-values.
Example:
 (appenda-keys-values ('(:a 1 :b 2 :c :d 2 e 3 4) :c 2 :f :b 1))
   => (:a 1 :b 2 :c :d 2 e 3 4 :f)"

  `(append-keys-values ,plist ',keysvals ,@(when ev-supplied-p
                                                 (list :empty-value empty-value))))

(defun append-key-value (plist key &optional (val nil val-p))

  "Alias for appenda-key-values for one key-value pair.
Example:
 (append-key-value '(:a 1 :b 2 :c :d 2 e 3 4) :b 1)
   => (:a 1 :b 2 :c :d 2 e 3 4)"

  (append-keys-values plist (if val-p (list key val) (list key))))



(defun append-or-set-keys-values (plist keysvals &key (empty-value t ev-supplied-p))
  
  "Append or replace values of keys in list.
Example:
 (append-or-set-keys-values '(:a 1 :b 2 :c :d 2 e 3 4) '(:c :f :b))
    => (:a 1 :b :c :d 2 e 3 4 :f) ;;
 (append-or-set-keys-values '(:a 1 :b 2 :c :d 4 e 34) '(:a 2 :c :d))
    => (:a 2 :b 2 :c :d e 34) ;;
 (append-or-set-keys-values nil '(:a 1 b)) => (:a 1 b)"
  
  (let* (to-set (to-append
                 (for-kv keysvals (k v :empty-value :+-123empty)
                   :if (if-key-exists plist k)
                     :do (push k to-set)
                     :and :if (eq v :+-123empty)
                       :when ev-supplied-p
                         :do (push empty-value to-set)
                       :end
                     :else :do (push v to-set)
                   :else :collect k
                     :and :if (eq v :+-123empty)
                       :when ev-supplied-p
                         :collect empty-value
                       :end
                     :else :collect v)))
    (nconc (set-keys-values plist (nreverse to-set)) to-append)))

(defmacro append-or-seta-keys-values ((plist &rest keysvals)
                                      &key (empty-value t ev-supplied-p))

  "Alias for append-or-set-keys-values.
Example:
 (append-or-seta-keys-values ('(:a 1 :b 2 :c :d 2 e 3 4) :c :f :b))
    => (:a 1 :b :c :d 2 e 3 4 :f)"

  `(append-or-set-keys-values ,plist ',keysvals ,@(when ev-supplied-p
                                                        (list :empty-value empty-value))))

(defun append-or-set-key-value (plist key &optional (val nil val-p))

  "Alias for append-or-set-keys-values for one key-value pair.
Example:
 (append-or-set-key-value '(:a 1 :b 2 :c :d 2 e 3 4) :f 2)
    => (:a 1 :b 2 :c :d 2 e 3 4 :f 2)"

  (append-or-set-keys-values plist (if val-p (list key val) (list key))))



(defun remf-keys (plist keys)

  "Destructively remove key-value pairs from list.
Destructively deleting keys from `KEYS' for efficiency, so be warned.
Do not discard it's return value.
Return 2 values: first -- new list;
 second -- list of removed values.
Example:
 (bpplist:remf-keys
    (list :A 1 :B 10 :C 11 :D 3 'E 4 5 :F) (list :b :c))
   => (:A 1 :D 3 E 4 5 :F) (:b 10 :c 11) ;;
 (remf-keys (list :d) (list :d)) => NIL (:d) ;;
 (remf-keys (list :a) (list :c)) => (:a) NIL"

  (labels ((recur-remf-keys (acc plist keys)
             (if keys
                 (if plist
                     (let ((h (first plist)))
                       (typecase h
                         (keyword
                          (let ((hx (second plist)))
                            (typecase hx
                              (keyword
                               (if (find h keys)
                                   (progn
                                     (setf (rest acc) (cons h nil))
                                     (recur-remf-keys (rest acc) (rest plist) (delete h keys)))
                                   (let ((rl (recur-remf-keys acc (rest plist) keys)))
                                     (setf (rest plist) rl)
                                     plist)))
                              (t (if (find h keys)
                                     (if (rest plist)
                                         (progn
                                           (setf (rest acc) (cons h (cons hx nil)))
                                           (recur-remf-keys (cddr acc) (cddr plist) (delete h keys)))
                                         (progn
                                           (setf (rest acc) (cons h nil))
                                           nil))
                                     (if (cddr plist)
                                         (let ((rl (recur-remf-keys acc (cddr plist) keys)))
                                           (setf (cddr plist) rl)
                                           plist)
                                         plist))))))
                         (t (let ((rl (recur-remf-keys acc (rest plist) keys)))
                              (setf (rest plist) rl)
                              plist))))
                     nil)
                 plist)))
    (let* ((rms (list nil))
           (rl (recur-remf-keys rms plist keys)))
      (values rl (cdr rms)))))

(defmacro remfa-keys (plist &rest keys)

  "Alias for remf-keys.
Example:
 (bpplist:remfa-keys
    (list :A 1 :B 10 :C 11 :D 3 'E 4 5 :F) :b :c)
   => (:A 1 :D 3 E 4 5 :F) (:b 10 :c 11)"

  `(remf-keys ,plist (list ,@keys)))

(defun remf-key (plist key)

  "Alias for remf-keys for one key.
Example:
 (bpplist:remf-key
    (list :A 1 :B 10 :C 11 :D 3 'E 4 5 :F) :b)
   => (:A 1 :C 11 :D 3 E 4 5 :F) (:b 10) ;;
 (remf-key (list :a :c :d) :d) => (:a :c) (:d) ;;
 (remf-key (list :a) :c) => (:a) nil"

  (remf-keys plist (list key)))



(defun setf-keys (plist keysvals)

  "Destructively set or append key-value pairs to list.
Destructively deleting keys from `KEYSVALS' for efficiency, so be warned.
Return 2 values: first -- new list;
 second -- list of old values of keys that was set.
Example:
  (bpplist:setf-keys
    (list :A 1 :B 10 :C 11 :D 3 'E 4 5) (list :b 11 :c 10 :f))
   => (:A 1 :B 11 :C 10 :D 3 E 4 5 :F) (:b 10 :c 11) ;;
  (setf-keys (list :a 1 :b 2 :c :d 4 'e 34) (list :a 2 :c :d))
   => (:a 2 :b 2 :c :d e 34) (:a 1 :c :d 4)"

  (labels ((recur-setf-keys (acc plist keysvals)
             (if keysvals
                 (if plist
                     (let ((h (first plist)))
                       (typecase h
                         (keyword
                          (let ((hx (second plist)))
                            (typecase hx
                              (keyword
                               (if (find h keysvals)
                                   (multiple-value-bind (kl ka) (remf-key keysvals h)
                                     (setf (rest acc) (cons h nil))
                                     (setf (rest plist)
                                           (if (rest ka)
                                               (cons (second ka) (recur-setf-keys (rest acc) (rest plist) kl))
                                               (recur-setf-keys (rest acc) (rest plist) kl)))
                                     plist)
                                   (progn
                                     (setf (rest plist)
                                           (recur-setf-keys acc (rest plist) keysvals))
                                     plist)))
                              (t (if (find h keysvals)
                                     (multiple-value-bind (kl ka) (remf-key keysvals h)
                                       (setf (rest acc)
                                             (cons h (cons hx nil)))
                                       (setf (rest plist)
                                             (if (rest ka)
                                                 (cons (second ka) (recur-setf-keys (cddr acc) (cddr plist) kl))
                                                 (recur-setf-keys (cddr acc) (cddr plist) kl)))
                                       plist)
                                     (progn
                                       (setf (cddr plist)
                                             (recur-setf-keys acc (cddr plist) keysvals))
                                       plist))))))
                         (t (setf (rest plist) (recur-setf-keys acc (rest plist) keysvals))
                            plist)))
                     keysvals)
                 plist)))
    (let* ((setl (list nil))
           (rl (recur-setf-keys setl plist keysvals)))
      (values rl (cdr setl)))))

(defmacro setfa-keys (plist &rest keysvals)

  "Alias for setf-keys.
Example:
  (bpplist:setfa-keys
    (list :A 1 :B 10 :C 11 :D 3 'E 4 5) :b 11 :c 10 :f)
   => (:A 1 :B 11 :C 10 :D 3 E 4 5 :F) (:b 10 :c 11)"

  `(setf-keys ,plist (list ,@keysvals)))

(defun setf-key (plist key &optional (val nil val-p))

  "Alias for setf-keys for one key.
Example:
  (bpplist:setf-key
    (list :A 1 :B 10 :C 11 :D 3 'E 4 5) :b 11)
   => (:A 1 :B 11 :C 11 :D 3 E 4 5) (:b 10)"

  (setf-keys plist (if val-p (list key val) (list key))))


(defun remf-keyless (plist)

  "Destructively remove values that have not keys from list.
Do not discard return value of this function.
Return 2 values: first -- new list;
second -- list of removed values.
Example:
 (remf-keyless (list :a 1 :b 2 23 43 :c :d 4 'e 2 2))
   =--> (:a 1 :b 2 :c :d 4) (23 43 e 2 2)"

  (labels ((recur-remf-keyless (acc plist)
             (if plist
                 (let ((h (first plist)))
                   (typecase h
                     (keyword
                      (let ((hx (second plist)))
                        (typecase hx
                          (keyword
                           (setf (rest plist) (recur-remf-keyless acc (rest plist)))
                           plist)
                          (t (setf (cddr plist) (recur-remf-keyless acc (cddr plist)))
                             plist))))
                     (t (setf (rest acc) (cons h nil))
                        (recur-remf-keyless (rest acc) (rest plist)))))
                 nil)))
    (let* ((rms (list nil))
           (rl (recur-remf-keyless rms plist)))
      (values rl (cdr rms)))))

(defun remf-empty-keys (plist &key (empty-value t ev-supplied-p))

  "Destructively remove keys that have not values from list.
Do not discard return value of this function.
Return 2 values: first -- new list;
second -- list of removed keys.
Example:
 (remf-empty-keys (list :a 1 :b 2 23 43 :c :d 4 'e 2 2))
   --> (:a 1 :b 2 23 43 :d 4 e 2 2) (:c) ;;
 (remf-empty-keys (list :a)) --> nil (:a)"

  (labels ((recur-remf-empty-keys (acc plist)
             (if plist
                 (let ((h (first plist)))
                   (typecase h
                     (keyword
                      (let ((hx (second plist)))
                        (typecase hx
                          (keyword
                           (if ev-supplied-p
                               (progn
                                 (setf (rest acc) (cons h (cons empty-value nil)))
                                 (recur-remf-empty-keys (cddr acc) (rest plist)))
                               (progn
                                 (setf (rest acc) (cons h nil))
                                 (recur-remf-empty-keys (rest acc) (rest plist)))))
                          (t (if (rest plist)
                                 (progn
                                   (setf (cddr plist) (recur-remf-empty-keys acc (cddr plist)))
                                   plist)
                                 (progn
                                   (if ev-supplied-p
                                       (setf (rest acc) (cons h (cons empty-value nil)))
                                       (setf (rest acc) (cons h nil)))
                                   (rest plist)))))))
                     (t (setf (rest plist)
                              (recur-remf-empty-keys acc (rest plist)))
                        plist)))
                 nil)))
    (let* ((rms (list nil))
           (rl (recur-remf-empty-keys rms plist)))
      (values rl (cdr rms)))))
