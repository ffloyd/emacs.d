;; ffe-config-test.el --- ERT tests for ffe-config.el

(require 'ert)
(require 'cl-lib)

(require 'ffe-config)

;;
;; Helpers
;;

(defmacro with-sandbox (&rest body)
  "Evaluate BODY with sandboxed ffe-config vars."
  
  `(let (ffe-config-alist
         ffe-config-loaded-list
         ffe-config-failed-list)
     ,@body))

;;
;; Tests
;;

(ert-deftest ffe-config-message/standart-case ()
  (let (ffe-test/message-calls)
    (cl-flet ((message (&rest args)
                       (push (apply #'format-message args) ffe-test/message-calls)))
      (ffe-config-message 'core "test msg")
      (should (equal
               '("ffe-config[core]: test msg")
               ffe-test/message-calls)))))

(ert-deftest ffe-config/minimum-args ()
  (with-sandbox
   (ffe-config some-config "Some description.")
   (should (ffe-config-p 'some-config))))

(ert-deftest ffe-config/full-args()
  (with-sandbox
   (ffe-config some-config "Some description."
               :deps ()
               :init ignore
               :packages ()
               :config ignore)
   (should (ffe-config-p 'some-config))))

(ert-deftest ffe-config-p/when-nil-config-alist ()
  (with-sandbox
   (should (equal nil (ffe-config-p 'some-config)))))

(ert-deftest ffe-config-p/when-config-present()
  (with-sandbox
   (ffe-config some-config "Some description" :init (ignore))
   (should (equal t (ffe-config-p 'some-config)))))

(ert-deftest ffe-config-loaded-p/when-nil-loaded-list ()
  (with-sandbox
   (should (equal nil (ffe-config-loaded-p 'some-config)))))

(ert-deftest ffe-config-loaded-p/when-config-loaded()
  (with-sandbox
   (ffe-config some-config "Some description" :init (ignore))
   (ffe-config-load 'some-config)
   (should (ffe-config-loaded-p 'some-config))))

(ert-deftest ffe-config-load/undefined-config ()
  (with-sandbox
   (should-error (ffe-config-load 'some-config))))

(ert-deftest ffe-config-load/empty-config ()
  (with-sandbox
   (ffe-config some-config "Some description")
   (should (ffe-config-load 'some-config))))

(ert-deftest ffe-config-load/full-config ()
  (with-sandbox
    (ffe-config dep-config "Some dependency")
    (ffe-config some-config "Some description"
                :deps (dep-config)
                :init (ignore)
                :packages (cl-lib) ;; we need working straight.el to pass test
                :config (ignore))
    (should (ffe-config-load 'some-config))
    (should (ffe-config-loaded-p 'dep-config))
    (should (ffe-config-loaded-p 'some-config))))

(ert-deftest ffe-config-load/undefined-dependency ()
  (with-sandbox
   (ffe-config some-config "Some description"
               :deps (dep-config))
   (should-not (ffe-config-load 'some-config))))

(ert-deftest ffe-config-load/error-when-execution ()
  (with-sandbox
   (ffe-config incorrect-config "Some incorrect config"
               :init (error "Unfortunately, fuck off"))
   (should-not (ffe-config-load 'incorrect-config))
   (should (equal '(incorrect-config) ffe-config-failed-list))))

(ert-deftest ffe-config-load-all/no-defined-configs ()
  (with-sandbox
   (ffe-config-load-all)
   (should-not ffe-config-loaded-list)))

(ert-deftest ffe-config-load-all/several-defined-configs-and-one-incorrect ()
  (with-sandbox
   (ffe-config example1 "Example config 1")
   (ffe-config example2 "Example config 2")
   (ffe-config incorrect "Incorrect config" :init (error "Unfortunately, fuck off"))
   (ffe-config-load-all)

   (should (equal '(example2 example1) ffe-config-loaded-list))
   (should (equal '(incorrect) ffe-config-failed-list))))
   
