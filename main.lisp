(in-package #:evaled-when)

(defun evaled-when:parse-situations (situations)
  (let (ct lt e)
    (dolist (situation situations)
      (ecase situation
        ((:compile-toplevel compile)
         (setf ct t))
        ((:load-toplevel load)
         (setf lt t))
        ((:execute eval)
         (setf e t))))
    (values ct lt e)))

(defun %extract-declarations (declarations-then-body)
  (let ((body (member-if-not (lambda (form)
                               (typep form '(cons (eql declare))))
                             declarations-then-body)))
    (values (mapcan (lambda (declare-expression)
                      (copy-seq (cdr declare-expression)))
                    (ldiff declarations-then-body body))
            body)))

(defun %process-body (body mode env)
  (mapcan (lambda (form)
            (%process form mode env))
          body))

(defun %process (top-level-form mode env)
  (check-type mode (member :compile-time-too :not-compile-time))
  (let ((form (macroexpand top-level-form env)))
    (flet ((not-special-case ()
             (when (eq mode :compile-time-too)
               (list form))))
      (if (consp form)
          (case (car form)
            (progn
              (%process-body (cdr form) mode env))
            (locally
              (multiple-value-bind (declarations body) (%extract-declarations (cdr form))
                (let ((body (%process-body body mode (if declarations
                                                         (cltl2:augment-environment
                                                          env :declare declarations)
                                                         env))))
                  (if declarations
                      (list `(locally (declare ,@declarations)
                               ,@body))
                      body))))
            ((macrolet symbol-macrolet)
             (flet ((local-macros-form (operator binding-type parse-bindings)
                      (destructuring-bind (unparsed-bindings &body body) (cdr form)
                        (multiple-value-bind (declarations body) (%extract-declarations body)
                          (let ((bindings (funcall parse-bindings unparsed-bindings)))
                            `(,operator ,unparsed-bindings
                               ,@(when declarations (list `(declare ,@declarations)))
                               ,@(%process-body body mode (cltl2:augment-environment
                                                           env binding-type bindings :declare declarations))))))))
               (list (ecase (car form)
                       (macrolet
                         (local-macros-form
                          'macrolet :macro
                          (lambda (unparsed-bindings)
                            (mapcar (lambda (binding)
                                      (destructuring-bind (name lambda-list &body body) binding
                                        (list name (compile nil (cltl2:parse-macro name lambda-list body env)))))
                                    unparsed-bindings))))
                       (symbol-macrolet
                         (local-macros-form 'symbol-macrolet :symbol-macro #'identity))))))
            (eval-when
              (destructuring-bind ((&rest situations) &body body) (cdr form)
                (multiple-value-bind (ct lt e) (evaled-when:parse-situations situations)
                  (if lt
                      (%process-body body
                                     (if ct
                                         :compile-time-too
                                         (if e
                                             mode
                                             :not-compile-time))
                                     env)
                      (when (or ct (and e (eq mode :compile-time-too)))
                        (copy-seq body))))))
            (t (not-special-case)))
          (not-special-case)))))

(defun evaled-when:extract-compile-toplevel-forms (body &optional env (processing-mode :not-compile-time))
  (%process-body body processing-mode env))

(defmacro evaled-when:evaled-when ((&rest situations) &body body &environment env)
  (unless (equal situations '(:compile-toplevel))
    (error "~S situations must be ~S." 'evaled-when:evaled-when '(:compile-toplevel)))
  (let ((forms (evaled-when:extract-compile-toplevel-forms body env)))
    (when forms
      `(eval-when (:compile-toplevel)
         ,@forms))))
