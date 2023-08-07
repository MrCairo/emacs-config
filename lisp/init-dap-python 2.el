;;; init-dap-python.el -- Things that need to be done first.
;;
;;; Commentary:
;;; ------------------------------------------------------------------------
;;; This is required for dap to work with Python properly:
;;;     pip install "debugpy"
;;;

;;; Code:
(require 'dap-mode)

(use-package dap-python
  :ensure nil
  :config
  (dap-register-debug-template "Python :: Run file (buffer)"
                               (list :type "python"
                                     :args ""
                                     :cwd nil
                                     :module nil 
                                     :program nil
                                     :request "launch"
                                     :name "Python :: Run file (buffer)"))

  (dap-register-debug-template "Python :: Run file from project directory"
                               (list :type "python"
                                     :args ""
                                     :cwd nil
                                     :module nil
                                     :program nil
                                     :request "launch"))

  (dap-register-debug-template "Python :: Run pytest (buffer)"
                               (list :type "python"
                                     :args ""
                                     :cwd nil
                                     :program nil
                                     :module "pytest"
                                     :request "launch"
                                     :name "Python :: Run pytest (buffer)")))

(provide 'init-dap-python)

;;; init-dap-python.el ends here.

