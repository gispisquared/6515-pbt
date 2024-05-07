(let ((env (make-top-level-environment)))
  (with-working-directory-pathname
   (directory-pathname (current-load-pathname))
   (lambda ()
     (load '("ps00") env)))
  (environment-define system-global-environment 'problem-set-environment env)
  (ge env))
