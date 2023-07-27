;;; init-python-keybind.el -- Dap and realgud Python Keybinding.

;;; Commentary:
;;; ------------------------------------------------------------------------
;;; This is required for dap to work with Python properly:
;;;     pip install "debugpy"

;;;
;;; Code:
;; (if (package-installed-p 'dap-mode)
   (general-def python-mode-map
      "C-. /" 'dap-step-in
      "C-. ." 'dap-next
      "C-. ," 'dap-step-out
      "C-. ?" 'dap-breakpoint-condition
      "C-. C-b" 'dap-ui-breakpoints
      "C-. C-b" 'dap-ui-breakpoints
      "C-. C-c" 'dap-ui-controls-mode
      "C-. C-e" 'dap-ui-expressions
      "C-. C-l" 'dap-ui-locals
      "C-. C-r" 'dap-ui-repl-mode
      "C-. b" 'dap-breakpoint-toggle
      "C-. c" 'dap-continue
      "C-. d"  'dap-debug
      "C-. i" 'dap-step-in
      "C-. n" 'dap-next
      "C-. o" 'dap-step-out
      "C-. r" 'dap-debug-restart
      "C-. t" 'dap-breakpoint-toggle
      "C-. x" 'dap-disconnect
      "C-c }" 'indent-region) ;;)

;;; =========================================================================
(if (package-installed-p 'realgud)
   (general-def python-mode-map
      "M-p" 'python-nav-backward-defun
      "M-n" 'python-nav-forward-defun
      "C-c p" 'elpy-goto-definition
      "C-c h" 'elpy-doc
      "C-q j" 'realgud:cmd-jump
      "C-q k" 'realgud:cmd-kill
      "C-q s" 'realgud:cmd-step
      "C-q n" 'realgud:cmd-next
      "C-q q" 'realgud:cmd-quit
      "C-q F" 'realgud:window-bt
      "C-q U" 'realgud:cmd-until
      "C-q X" 'realgud:cmd-clear
      "C-q !" 'realgud:cmd-shell
      "C-q b" 'realgud:cmd-break
      "C-q f" 'realgud:cmd-finish
      "C-q D" 'realgud:cmd-delete
      "C-q +" 'realgud:cmd-enable
      "C-q R" 'realgud:cmd-restart
      "C-q -" 'realgud:cmd-disable
      "C-q B" 'realgud:window-brkpt
      "C-q c" 'realgud:cmd-continue
      "C-q e" 'realgud:cmd-eval-dwim
      "C-q Q" 'realgud:cmd-terminate
      "C-q T" 'realgud:cmd-backtrace
      "C-q h" 'realgud:cmd-until-here
      "C-q u" 'realgud:cmd-older-frame
      "C-q 4" 'realgud:cmd-goto-loc-hist-4
      "C-q 5" 'realgud:cmd-goto-loc-hist-5
      "C-q 6" 'realgud:cmd-goto-loc-hist-6
      "C-q 7" 'realgud:cmd-goto-loc-hist-7
      "C-q 8" 'realgud:cmd-goto-loc-hist-8
      "C-q 9" 'realgud:cmd-goto-loc-hist-9
      "C-q d" 'realgud:cmd-newer-frame
      "C-q RET" 'realgud:cmd-repeat-last
      "C-q E" 'realgud:cmd-eval-at-point
      "C-q I" 'realgud:cmdbuf-info-describe
      "C-q C-d" 'realgud:pdb
      "C-q C-f" 'realgud:flake8-goto-msg-line
      "C-q C-i" 'realgud:cmd-info-breakpoints))

(provide 'init-python-keybind)

;;; init-python-keybind.el ends here.

