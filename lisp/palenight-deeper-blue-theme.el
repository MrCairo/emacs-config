;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(deftheme palenight-deeper-blue
  "Doom palenight and deeper-blue")

(custom-theme-set-variables
 'palenight-deeper-blue
 '(ansi-color-names-vector ["#292D3E" "#ff5370" "#c3e88d" "#ffcb6b" "#82aaff" "#c792ea" "#89DDFF" "#EEFFFF"])
 '(rustic-ansi-faces ["#292D3E" "#ff5370" "#c3e88d" "#ffcb6b" "#82aaff" "#c792ea" "#89DDFF" "#EEFFFF"])
 '(exwm-floating-border-color "#232635")
 '(fci-rule-color "#676E95")
 '(jdee-db-spec-breakpoint-face-colors '("#1c1f2b" . "#676E95"))
 '(jdee-db-requested-breakpoint-face-colors '("#1c1f2b" . "#c3e88d"))
 '(jdee-db-active-breakpoint-face-colors '("#1c1f2b" . "#c792ea")))
(custom-theme-set-faces
   'palenight-deeper-blue
   '(Info-title-1-face ((((class color) (min-colors 89)) (:family "helv" :weight bold :height 1.728)))) ;;; deeper-blue-theme
   '(Info-title-2-face ((((class color) (min-colors 89)) (:family "helv" :weight bold :height 1.44)))) ;;; deeper-blue-theme
   '(Info-title-3-face ((((class color) (min-colors 89)) (:family "helv" :weight bold :height 1.2)))) ;;; deeper-blue-theme
   '(Info-title-4-face ((((class color) (min-colors 89)) (:family "helv" :weight bold)))) ;;; deeper-blue-theme
   '(TeX-error-description-error ((t (:inherit error :weight bold))))
   '(TeX-error-description-tex-said ((t (:inherit success :weight bold))))
   '(TeX-error-description-warning ((t (:inherit warning :weight bold))))
   '(agda2-highlight-bound-variable-face ((t (:inherit font-lock-variable-name-face))))
   '(agda2-highlight-coinductive-constructor-face ((t (:inherit font-lock-type-face))))
   '(agda2-highlight-datatype-face ((t (:inherit font-lock-type-face))))
   '(agda2-highlight-dotted-face ((t (:inherit font-lock-variable-name-face))))
   '(agda2-highlight-error-face ((t (:inherit font-lock-warning-face))))
   '(agda2-highlight-field-face ((t (:inherit font-lock-type-face))))
   '(agda2-highlight-function-face ((t (:inherit font-lock-function-name-face))))
   '(agda2-highlight-incomplete-pattern-face ((t (:inherit font-lock-warning-face))))
   '(agda2-highlight-inductive-constructor-face ((t (:inherit font-lock-type-face))))
   '(agda2-highlight-keyword-face ((t (:inherit font-lock-keyword-face))))
   '(agda2-highlight-macro-face ((t (:inherit font-lock-function-name-face))))
   '(agda2-highlight-module-face ((t (:inherit font-lock-variable-name-face))))
   '(agda2-highlight-number-face ((t (:inherit font-lock-string-face))))
   '(agda2-highlight-positivity-problem-face ((t (:inherit font-lock-warning-face))))
   '(agda2-highlight-postulate-face ((t (:inherit font-lock-type-face))))
   '(agda2-highlight-primitive-face ((t (:inherit font-lock-type-face))))
   '(agda2-highlight-primitive-type-face ((t (:inherit font-lock-type-face))))
   '(agda2-highlight-record-face ((t (:inherit font-lock-type-face))))
   '(agda2-highlight-string-face ((t (:inherit font-lock-string-face))))
   '(agda2-highlight-symbol-face ((t (:inherit font-lock-variable-name-face))))
   '(agda2-highlight-termination-problem-face ((t (:inherit font-lock-warning-face))))
   '(agda2-highlight-typechecks-face ((t (:inherit font-lock-warning-face))))
   '(agda2-highlight-unsolved-constraint-face ((t (:inherit font-lock-warning-face))))
   '(agda2-highlight-unsolved-meta-face ((t (:inherit font-lock-warning-face))))
   '(alert-high-face ((((class color) (min-colors 257)) (:inherit t :foreground "#ffcb6b")) (((class color) (min-colors 256)) (:inherit t :foreground "#ffd700")) (((class color) (min-colors 16)) (:inherit t :foreground "brightyellow"))))
   '(alert-low-face ((((class color) (min-colors 257)) (:foreground "#676E95")) (((class color) (min-colors 256)) (:foreground "#585858")) (((class color) (min-colors 16)) (:foreground "brightblack"))))
   '(alert-moderate-face ((((class color) (min-colors 257)) (:inherit t :foreground "#BFC7D5")) (((class color) (min-colors 256)) (:inherit t :foreground "#bcbcbc")) (((class color) (min-colors 16)) (:inherit t :foreground "white"))))
   '(alert-trivial-face ((((class color) (min-colors 257)) (:foreground "#8d92af")) (((class color) (min-colors 256)) (:foreground "#818181")) (((class color) (min-colors 16)) (:foreground "brightblack"))))
   '(alert-urgent-face ((((class color) (min-colors 257)) (:inherit t :foreground "#ff5370")) (((class color) (min-colors 256)) (:inherit t :foreground "#ff0000")) (((class color) (min-colors 16)) (:inherit t :foreground "red"))))
   '(all-the-icons-blue ((((class color) (min-colors 257)) (:foreground "#82aaff")) (((class color) (min-colors 256)) (:foreground "#5fafff")) (((class color) (min-colors 16)) (:foreground "brightblue"))))
   '(all-the-icons-blue-alt ((((class color) (min-colors 257)) (:foreground "#44b9b1")) (((class color) (min-colors 256)) (:foreground "#00d7af")) (((class color) (min-colors 16)) (:foreground "brightgreen"))))
   '(all-the-icons-cyan ((((class color) (min-colors 257)) (:foreground "#89DDFF")) (((class color) (min-colors 256)) (:foreground "#5fd7ff")) (((class color) (min-colors 16)) (:foreground "brightcyan"))))
   '(all-the-icons-cyan-alt ((((class color) (min-colors 257)) (:foreground "#89DDFF")) (((class color) (min-colors 256)) (:foreground "#5fd7ff")) (((class color) (min-colors 16)) (:foreground "brightcyan"))))
   '(all-the-icons-dblue ((((class color) (min-colors 257)) (:foreground "#7986E7")) (((class color) (min-colors 256)) (:foreground "#d7ffff")) (((class color) (min-colors 16)) (:foreground "blue"))))
   '(all-the-icons-dcyan ((((class color) (min-colors 257)) (:foreground "#80cbc4")) (((class color) (min-colors 256)) (:foreground "#00d7af")) (((class color) (min-colors 16)) (:foreground "cyan"))))
   '(all-the-icons-dgreen ((((class color) (min-colors 257)) (:foreground "#88a262")) (((class color) (min-colors 256)) (:foreground "#7ab200")) (((class color) (min-colors 16)) (:foreground "green"))))
   '(all-the-icons-dired-dir-face ((((class color) (min-colors 257)) (:foreground "#8d92af")) (((class color) (min-colors 256)) (:foreground "#818181")) (((class color) (min-colors 16)) (:foreground "brightblack"))))
   '(all-the-icons-dmaroon ((((class color) (min-colors 257)) (:foreground "#8b66a3")) (((class color) (min-colors 256)) (:foreground "#965e96")) (((class color) (min-colors 16)) (:foreground "brightmagenta"))))
   '(all-the-icons-dorange ((((class color) (min-colors 257)) (:foreground "#ac624b")) (((class color) (min-colors 256)) (:foreground "#b24200")) (((class color) (min-colors 16)) (:foreground "brightred"))))
   '(all-the-icons-dpink ((((class color) (min-colors 257)) (:foreground "#ff6c85")) (((class color) (min-colors 256)) (:foreground "#ff2626")) (((class color) (min-colors 16)) (:foreground "red"))))
   '(all-the-icons-dpurple ((((class color) (min-colors 257)) (:foreground "#82597d")) (((class color) (min-colors 256)) (:foreground "#965e7a")) (((class color) (min-colors 16)) (:foreground "magenta"))))
   '(all-the-icons-dred ((((class color) (min-colors 257)) (:foreground "#b23a4e")) (((class color) (min-colors 256)) (:foreground "#b20000")) (((class color) (min-colors 16)) (:foreground "red"))))
   '(all-the-icons-dsilver ((((class color) (min-colors 257)) (:foreground "#767c9f")) (((class color) (min-colors 256)) (:foreground "#686868")) (((class color) (min-colors 16)) (:foreground "brightblack"))))
   '(all-the-icons-dyellow ((((class color) (min-colors 257)) (:foreground "#b28e4a")) (((class color) (min-colors 256)) (:foreground "#b29600")) (((class color) (min-colors 16)) (:foreground "brightyellow"))))
   '(all-the-icons-green ((((class color) (min-colors 257)) (:foreground "#c3e88d")) (((class color) (min-colors 256)) (:foreground "#afff00")) (((class color) (min-colors 16)) (:foreground "green"))))
   '(all-the-icons-lblue ((((class color) (min-colors 257)) (:foreground "#a7c3ff")) (((class color) (min-colors 256)) (:foreground "#8fc7ff")) (((class color) (min-colors 16)) (:foreground "brightblue"))))
   '(all-the-icons-lcyan ((((class color) (min-colors 257)) (:foreground "#ace7ff")) (((class color) (min-colors 256)) (:foreground "#8fe3ff")) (((class color) (min-colors 16)) (:foreground "brightcyan"))))
   '(all-the-icons-lgreen ((((class color) (min-colors 257)) (:foreground "#d4eeaf")) (((class color) (min-colors 256)) (:foreground "#c7ff4c")) (((class color) (min-colors 16)) (:foreground "green"))))
   '(all-the-icons-lmaroon ((((class color) (min-colors 257)) (:foreground "#d7b2f0")) (((class color) (min-colors 256)) (:foreground "#e3abe3")) (((class color) (min-colors 16)) (:foreground "brightmagenta"))))
   '(all-the-icons-lorange ((((class color) (min-colors 257)) (:foreground "#f9ae98")) (((class color) (min-colors 256)) (:foreground "#ff8f4c")) (((class color) (min-colors 16)) (:foreground "brightred"))))
   '(all-the-icons-lpink ((((class color) (min-colors 257)) (:foreground "#ffb1be")) (((class color) (min-colors 256)) (:foreground "#ff8c8c")) (((class color) (min-colors 16)) (:foreground "red"))))
   '(all-the-icons-lpurple ((((class color) (min-colors 257)) (:foreground "#cfa6c9")) (((class color) (min-colors 256)) (:foreground "#e3abc7")) (((class color) (min-colors 16)) (:foreground "magenta"))))
   '(all-the-icons-lred ((((class color) (min-colors 257)) (:foreground "#ff869a")) (((class color) (min-colors 256)) (:foreground "#ff4c4c")) (((class color) (min-colors 16)) (:foreground "red"))))
   '(all-the-icons-lsilver ((((class color) (min-colors 257)) (:foreground "#d1d3df")) (((class color) (min-colors 256)) (:foreground "#cccccc")) (((class color) (min-colors 16)) (:foreground "brightblack"))))
   '(all-the-icons-lyellow ((((class color) (min-colors 257)) (:foreground "#ffda97")) (((class color) (min-colors 256)) (:foreground "#ffe34c")) (((class color) (min-colors 16)) (:foreground "brightyellow"))))
   '(all-the-icons-maroon ((((class color) (min-colors 257)) (:foreground "#c792ea")) (((class color) (min-colors 256)) (:foreground "#d787d7")) (((class color) (min-colors 16)) (:foreground "brightmagenta"))))
   '(all-the-icons-orange ((((class color) (min-colors 257)) (:foreground "#f78c6c")) (((class color) (min-colors 256)) (:foreground "#ff5f00")) (((class color) (min-colors 16)) (:foreground "brightred"))))
   '(all-the-icons-pink ((((class color) (min-colors 257)) (:foreground "#ff8fa2")) (((class color) (min-colors 256)) (:foreground "#ff5959")) (((class color) (min-colors 16)) (:foreground "red"))))
   '(all-the-icons-purple ((((class color) (min-colors 257)) (:foreground "#bb80b3")) (((class color) (min-colors 256)) (:foreground "#d787af")) (((class color) (min-colors 16)) (:foreground "magenta"))))
   '(all-the-icons-purple-alt ((((class color) (min-colors 257)) (:foreground "#737099")) (((class color) (min-colors 256)) (:foreground "#6b5f65")) (((class color) (min-colors 16)) (:foreground "magenta"))))
   '(all-the-icons-red ((((class color) (min-colors 257)) (:foreground "#ff5370")) (((class color) (min-colors 256)) (:foreground "#ff0000")) (((class color) (min-colors 16)) (:foreground "red"))))
   '(all-the-icons-red-alt ((((class color) (min-colors 257)) (:foreground "#7d698f")) (((class color) (min-colors 256)) (:foreground "#714a4a")) (((class color) (min-colors 16)) (:foreground "red"))))
   '(all-the-icons-silver ((((class color) (min-colors 257)) (:foreground "#abafc4")) (((class color) (min-colors 256)) (:foreground "#a3a3a3")) (((class color) (min-colors 16)) (:foreground "brightblack"))))
   '(all-the-icons-yellow ((((class color) (min-colors 257)) (:foreground "#ffcb6b")) (((class color) (min-colors 256)) (:foreground "#ffd700")) (((class color) (min-colors 16)) (:foreground "brightyellow"))))
   '(annotate-annotation ((((class color) (min-colors 257)) (:background "#38374f" :foreground "#8d92af")) (((class color) (min-colors 256)) (:background nil :foreground "#818181")) (((class color) (min-colors 16)) (:background nil :foreground "brightblack"))))
   '(annotate-annotation-secondary ((((class color) (min-colors 257)) (:background "#383f45" :foreground "#8d92af")) (((class color) (min-colors 256)) (:background nil :foreground "#818181")) (((class color) (min-colors 16)) (:background nil :foreground "brightblack"))))
   '(annotate-highlight ((((class color) (min-colors 257)) (:background "#38374f" :underline "#c792ea")) (((class color) (min-colors 256)) (:background nil :underline "#d787d7")) (((class color) (min-colors 16)) (:background nil :underline "brightmagenta"))))
   '(annotate-highlight-secondary ((((class color) (min-colors 257)) (:background "#383f45" :underline "#c3e88d")) (((class color) (min-colors 256)) (:background nil :underline "#afff00")) (((class color) (min-colors 16)) (:background nil :underline "green"))))
   '(ansi-color-black ((((class color) (min-colors 257)) (:foreground "#292D3E" :background "#292D3E")) (((class color) (min-colors 256)) (:foreground nil :background nil)) (((class color) (min-colors 16)) (:foreground nil :background nil))))
   '(ansi-color-blue ((((class color) (min-colors 257)) (:foreground "#82aaff" :background "#82aaff")) (((class color) (min-colors 256)) (:foreground "#5fafff" :background "#5fafff")) (((class color) (min-colors 16)) (:foreground "brightblue" :background "brightblue"))))
   '(ansi-color-bright-black ((((class color) (min-colors 257)) (:foreground "#1c1f2b" :background "#232635")) (((class color) (min-colors 256)) (:foreground "black" :background "#303030")) (((class color) (min-colors 16)) (:foreground "black" :background "brightblack"))))
   '(ansi-color-bright-blue ((((class color) (min-colors 257)) (:foreground "#94b6ff" :background "#94b6ff")) (((class color) (min-colors 256)) (:foreground "#77bbff" :background "#77bbff")) (((class color) (min-colors 16)) (:foreground "brightblue" :background "brightblue"))))
   '(ansi-color-bright-cyan ((((class color) (min-colors 257)) (:foreground "#9ae2ff" :background "#9ae2ff")) (((class color) (min-colors 256)) (:foreground "#77ddff" :background "#77ddff")) (((class color) (min-colors 16)) (:foreground "brightcyan" :background "brightcyan"))))
   '(ansi-color-bright-green ((((class color) (min-colors 257)) (:foreground "#cbeb9e" :background "#cbeb9e")) (((class color) (min-colors 256)) (:foreground "#bbff26" :background "#bbff26")) (((class color) (min-colors 16)) (:foreground "green" :background "green"))))
   '(ansi-color-bright-magenta ((((class color) (min-colors 257)) (:foreground "#cfa2ed" :background "#cfa2ed")) (((class color) (min-colors 256)) (:foreground "#dd99dd" :background "#dd99dd")) (((class color) (min-colors 16)) (:foreground "brightmagenta" :background "brightmagenta"))))
   '(ansi-color-bright-red ((((class color) (min-colors 257)) (:foreground "#ff6c85" :background "#ff6c85")) (((class color) (min-colors 256)) (:foreground "#ff2626" :background "#ff2626")) (((class color) (min-colors 16)) (:foreground "red" :background "red"))))
   '(ansi-color-bright-white ((((class color) (min-colors 257)) (:foreground "#A6Accd" :background "#A6Accd")) (((class color) (min-colors 256)) (:foreground "#a8a8a8" :background "#a8a8a8")) (((class color) (min-colors 16)) (:foreground "white" :background "white"))))
   '(ansi-color-bright-yellow ((((class color) (min-colors 257)) (:foreground "#ffd281" :background "#ffd281")) (((class color) (min-colors 256)) (:foreground "#ffdd26" :background "#ffdd26")) (((class color) (min-colors 16)) (:foreground "brightyellow" :background "brightyellow"))))
   '(ansi-color-cyan ((((class color) (min-colors 257)) (:foreground "#89DDFF" :background "#89DDFF")) (((class color) (min-colors 256)) (:foreground "#5fd7ff" :background "#5fd7ff")) (((class color) (min-colors 16)) (:foreground "brightcyan" :background "brightcyan"))))
   '(ansi-color-green ((((class color) (min-colors 257)) (:foreground "#c3e88d" :background "#c3e88d")) (((class color) (min-colors 256)) (:foreground "#afff00" :background "#afff00")) (((class color) (min-colors 16)) (:foreground "green" :background "green"))))
   '(ansi-color-magenta ((((class color) (min-colors 257)) (:foreground "#c792ea" :background "#c792ea")) (((class color) (min-colors 256)) (:foreground "#d787d7" :background "#d787d7")) (((class color) (min-colors 16)) (:foreground "brightmagenta" :background "brightmagenta"))))
   '(ansi-color-red ((((class color) (min-colors 257)) (:foreground "#ff5370" :background "#ff5370")) (((class color) (min-colors 256)) (:foreground "#ff0000" :background "#ff0000")) (((class color) (min-colors 16)) (:foreground "red" :background "red"))))
   '(ansi-color-white ((((class color) (min-colors 257)) (:foreground "#EEFFFF" :background "#EEFFFF")) (((class color) (min-colors 256)) (:foreground "#e4e4e4" :background "#e4e4e4")) (((class color) (min-colors 16)) (:foreground "brightwhite" :background "brightwhite"))))
   '(ansi-color-yellow ((((class color) (min-colors 257)) (:foreground "#ffcb6b" :background "#ffcb6b")) (((class color) (min-colors 256)) (:foreground "#ffd700" :background "#ffd700")) (((class color) (min-colors 16)) (:foreground "brightyellow" :background "brightyellow"))))
   '(anzu-replace-highlight ((((class color) (min-colors 257)) (:background "#1c1f2b" :foreground "#ff5370" :weight bold :strike-through t)) (((class color) (min-colors 256)) (:background "black" :foreground "#ff0000" :weight bold :strike-through t)) (((class color) (min-colors 16)) (:background "black" :foreground "red" :weight bold :strike-through t))))
   '(anzu-replace-to ((((class color) (min-colors 257)) (:background "#1c1f2b" :foreground "#c3e88d" :weight bold)) (((class color) (min-colors 256)) (:background "black" :foreground "#afff00" :weight bold)) (((class color) (min-colors 16)) (:background "black" :foreground "green" :weight bold))))
   '(avy-background-face ((((class color) (min-colors 257)) (:foreground "#676E95")) (((class color) (min-colors 256)) (:foreground "#585858")) (((class color) (min-colors 16)) (:foreground "brightblack"))))
   '(avy-lead-face ((((class color) (min-colors 257)) (:background "#c792ea" :foreground "#292D3E" :distant-foreground "#EEFFFF" :weight bold)) (((class color) (min-colors 256)) (:background "#d787d7" :foreground nil :distant-foreground "#e4e4e4" :weight bold)) (((class color) (min-colors 16)) (:background "brightmagenta" :foreground nil :distant-foreground "brightwhite" :weight bold))))
   '(avy-lead-face-0 ((((class color) (min-colors 257) (background dark)) (:inherit avy-lead-face :background "#d7b2f0")) (((class color) (min-colors 256) (background dark)) (:inherit avy-lead-face :background "#e3abe3")) (((class color) (min-colors 16) (background dark)) (:inherit avy-lead-face :background "brightmagenta")) (((class color) (min-colors 257) (background light)) (:inherit avy-lead-face :background "#8b66a3")) (((class color) (min-colors 256) (background light)) (:inherit avy-lead-face :background "#965e96")) (((class color) (min-colors 16) (background light)) (:inherit avy-lead-face :background "brightmagenta"))))
   '(avy-lead-face-1 ((((class color) (min-colors 257) (background dark)) (:inherit avy-lead-face :background "#e8d3f6")) (((class color) (min-colors 256) (background dark)) (:inherit avy-lead-face :background "#efcfef")) (((class color) (min-colors 16) (background dark)) (:inherit avy-lead-face :background "brightmagenta")) (((class color) (min-colors 257) (background light)) (:inherit avy-lead-face :background "#4f3a5d")) (((class color) (min-colors 256) (background light)) (:inherit avy-lead-face :background "#563656")) (((class color) (min-colors 16) (background light)) (:inherit avy-lead-face :background "brightmagenta"))))
   '(avy-lead-face-2 ((((class color) (min-colors 257) (background dark)) (:inherit avy-lead-face :background "#f9f4fc")) (((class color) (min-colors 256) (background dark)) (:inherit avy-lead-face :background "#fbf3fb")) (((class color) (min-colors 16) (background dark)) (:inherit avy-lead-face :background "brightmagenta")) (((class color) (min-colors 257) (background light)) (:inherit avy-lead-face :background "#130e17")) (((class color) (min-colors 256) (background light)) (:inherit avy-lead-face :background "#150d15")) (((class color) (min-colors 16) (background light)) (:inherit avy-lead-face :background "brightmagenta"))))
   '(bmkp-*-mark ((((class color) (min-colors 257)) (:foreground "#292D3E" :background "#ffcb6b")) (((class color) (min-colors 256)) (:foreground nil :background "#ffd700")) (((class color) (min-colors 16)) (:foreground nil :background "brightyellow"))))
   '(bmkp->-mark ((((class color) (min-colors 257)) (:foreground "#ffcb6b")) (((class color) (min-colors 256)) (:foreground "#ffd700")) (((class color) (min-colors 16)) (:foreground "brightyellow"))))
   '(bmkp-D-mark ((((class color) (min-colors 257)) (:foreground "#292D3E" :background "#ff5370")) (((class color) (min-colors 256)) (:foreground nil :background "#ff0000")) (((class color) (min-colors 16)) (:foreground nil :background "red"))))
   '(bmkp-X-mark ((((class color) (min-colors 257)) (:foreground "#ff5370")) (((class color) (min-colors 256)) (:foreground "#ff0000")) (((class color) (min-colors 16)) (:foreground "red"))))
   '(bmkp-a-mark ((((class color) (min-colors 257)) (:background "#ff5370")) (((class color) (min-colors 256)) (:background "#ff0000")) (((class color) (min-colors 16)) (:background "red"))))
   '(bmkp-bad-bookmark ((((class color) (min-colors 257)) (:foreground "#292D3E" :background "#ffcb6b")) (((class color) (min-colors 256)) (:foreground nil :background "#ffd700")) (((class color) (min-colors 16)) (:foreground nil :background "brightyellow"))))
   '(bmkp-bookmark-file ((((class color) (min-colors 257)) (:foreground "#bb80b3" :background "#242837")) (((class color) (min-colors 256)) (:foreground "#d787af" :background nil)) (((class color) (min-colors 16)) (:foreground "magenta" :background nil))))
   '(bmkp-bookmark-list ((((class color) (min-colors 257)) (:background "#242837")) (((class color) (min-colors 256)) (:background nil)) (((class color) (min-colors 16)) (:background nil))))
   '(bmkp-buffer ((((class color) (min-colors 257)) (:foreground "#82aaff")) (((class color) (min-colors 256)) (:foreground "#5fafff")) (((class color) (min-colors 16)) (:foreground "brightblue"))))
   '(bmkp-desktop ((((class color) (min-colors 257)) (:foreground "#292D3E" :background "#bb80b3")) (((class color) (min-colors 256)) (:foreground nil :background "#d787af")) (((class color) (min-colors 16)) (:foreground nil :background "magenta"))))
   '(bmkp-file-handler ((((class color) (min-colors 257)) (:background "#ff5370")) (((class color) (min-colors 256)) (:background "#ff0000")) (((class color) (min-colors 16)) (:background "red"))))
   '(bmkp-function ((((class color) (min-colors 257)) (:foreground "#c3e88d")) (((class color) (min-colors 256)) (:foreground "#afff00")) (((class color) (min-colors 16)) (:foreground "green"))))
   '(bmkp-gnus ((((class color) (min-colors 257)) (:foreground "#f78c6c")) (((class color) (min-colors 256)) (:foreground "#ff5f00")) (((class color) (min-colors 16)) (:foreground "brightred"))))
   '(bmkp-heading ((((class color) (min-colors 257)) (:foreground "#ffcb6b")) (((class color) (min-colors 256)) (:foreground "#ffd700")) (((class color) (min-colors 16)) (:foreground "brightyellow"))))
   '(bmkp-info ((((class color) (min-colors 257)) (:foreground "#89DDFF")) (((class color) (min-colors 256)) (:foreground "#5fd7ff")) (((class color) (min-colors 16)) (:foreground "brightcyan"))))
   '(bmkp-light-autonamed ((((class color) (min-colors 257)) (:foreground "#242837" :background "#89DDFF")) (((class color) (min-colors 256)) (:foreground nil :background "#5fd7ff")) (((class color) (min-colors 16)) (:foreground nil :background "brightcyan"))))
   '(bmkp-light-autonamed-region ((((class color) (min-colors 257)) (:foreground "#242837" :background "#ff5370")) (((class color) (min-colors 256)) (:foreground nil :background "#ff0000")) (((class color) (min-colors 16)) (:foreground nil :background "red"))))
   '(bmkp-light-fringe-autonamed ((((class color) (min-colors 257)) (:foreground "#242837" :background "#bb80b3")) (((class color) (min-colors 256)) (:foreground nil :background "#d787af")) (((class color) (min-colors 16)) (:foreground nil :background "magenta"))))
   '(bmkp-light-fringe-non-autonamed ((((class color) (min-colors 257)) (:foreground "#242837" :background "#c3e88d")) (((class color) (min-colors 256)) (:foreground nil :background "#afff00")) (((class color) (min-colors 16)) (:foreground nil :background "green"))))
   '(bmkp-light-mark ((((class color) (min-colors 257)) (:foreground "#292D3E" :background "#89DDFF")) (((class color) (min-colors 256)) (:foreground nil :background "#5fd7ff")) (((class color) (min-colors 16)) (:foreground nil :background "brightcyan"))))
   '(bmkp-light-non-autonamed ((((class color) (min-colors 257)) (:foreground "#292D3E" :background "#bb80b3")) (((class color) (min-colors 256)) (:foreground nil :background "#d787af")) (((class color) (min-colors 16)) (:foreground nil :background "magenta"))))
   '(bmkp-light-non-autonamed-region ((((class color) (min-colors 257)) (:foreground "#292D3E" :background "#ff5370")) (((class color) (min-colors 256)) (:foreground nil :background "#ff0000")) (((class color) (min-colors 16)) (:foreground nil :background "red"))))
   '(bmkp-local-directory ((((class color) (min-colors 257)) (:foreground "#292D3E" :background "#bb80b3")) (((class color) (min-colors 256)) (:foreground nil :background "#d787af")) (((class color) (min-colors 16)) (:foreground nil :background "magenta"))))
   '(bmkp-local-file-with-region ((((class color) (min-colors 257)) (:foreground "#ffcb6b")) (((class color) (min-colors 256)) (:foreground "#ffd700")) (((class color) (min-colors 16)) (:foreground "brightyellow"))))
   '(bmkp-local-file-without-region ((((class color) (min-colors 257)) (:foreground "#676E95")) (((class color) (min-colors 256)) (:foreground "#585858")) (((class color) (min-colors 16)) (:foreground "brightblack"))))
   '(bmkp-man ((((class color) (min-colors 257)) (:foreground "#bb80b3")) (((class color) (min-colors 256)) (:foreground "#d787af")) (((class color) (min-colors 16)) (:foreground "magenta"))))
   '(bmkp-no-jump ((((class color) (min-colors 257)) (:foreground "#676E95")) (((class color) (min-colors 256)) (:foreground "#585858")) (((class color) (min-colors 16)) (:foreground "brightblack"))))
   '(bmkp-no-local ((((class color) (min-colors 257)) (:foreground "#ffcb6b")) (((class color) (min-colors 256)) (:foreground "#ffd700")) (((class color) (min-colors 16)) (:foreground "brightyellow"))))
   '(bmkp-non-file ((((class color) (min-colors 257)) (:foreground "#c3e88d")) (((class color) (min-colors 256)) (:foreground "#afff00")) (((class color) (min-colors 16)) (:foreground "green"))))
   '(bmkp-remote-file ((((class color) (min-colors 257)) (:foreground "#f78c6c")) (((class color) (min-colors 256)) (:foreground "#ff5f00")) (((class color) (min-colors 16)) (:foreground "brightred"))))
   '(bmkp-sequence ((((class color) (min-colors 257)) (:foreground "#82aaff")) (((class color) (min-colors 256)) (:foreground "#5fafff")) (((class color) (min-colors 16)) (:foreground "brightblue"))))
   '(bmkp-su-or-sudo ((((class color) (min-colors 257)) (:foreground "#ff5370")) (((class color) (min-colors 256)) (:foreground "#ff0000")) (((class color) (min-colors 16)) (:foreground "red"))))
   '(bmkp-t-mark ((((class color) (min-colors 257)) (:foreground "#bb80b3")) (((class color) (min-colors 256)) (:foreground "#d787af")) (((class color) (min-colors 16)) (:foreground "magenta"))))
   '(bmkp-url ((((class color) (min-colors 257)) (:foreground "#82aaff" :underline t)) (((class color) (min-colors 256)) (:foreground "#5fafff" :underline t)) (((class color) (min-colors 16)) (:foreground "brightblue" :underline t))))
   '(bmkp-variable-list ((((class color) (min-colors 257)) (:foreground "#c3e88d")) (((class color) (min-colors 256)) (:foreground "#afff00")) (((class color) (min-colors 16)) (:foreground "green"))))
   '(bold ((((class color) (min-colors 257)) (:weight bold :foreground unspecified)) (((class color) (min-colors 256)) (:weight bold :foreground unspecified)) (((class color) (min-colors 16)) (:weight bold :foreground unspecified))))
   '(bold-italic ((t (:inherit (bold italic)))))
   '(bookmark-face ((((class color) (min-colors 257)) (:background "#38374f" :extend t)) (((class color) (min-colors 256)) (:background nil :extend t)) (((class color) (min-colors 16)) (:background nil :extend t))))
   '(cfw:face-grid ((((class color) (min-colors 257)) (:foreground "#232635")) (((class color) (min-colors 256)) (:foreground "#303030")) (((class color) (min-colors 16)) (:foreground "brightblack"))))
   '(cfw:face-header ((((class color) (min-colors 257)) (:foreground "#7091d8" :weight bold)) (((class color) (min-colors 256)) (:foreground nil :weight bold)) (((class color) (min-colors 16)) (:foreground nil :weight bold))))
   '(cfw:face-holiday ((((class color) (min-colors 257)) (:background "#242837" :weight bold)) (((class color) (min-colors 256)) (:background nil :weight bold)) (((class color) (min-colors 16)) (:background nil :weight bold))))
   '(cfw:face-periods ((((class color) (min-colors 257)) (:foreground "#ffcb6b")) (((class color) (min-colors 256)) (:foreground "#ffd700")) (((class color) (min-colors 16)) (:foreground "brightyellow"))))
   '(cfw:face-saturday ((((class color) (min-colors 257)) (:foreground "#d44b66" :weight bold)) (((class color) (min-colors 256)) (:foreground nil :weight bold)) (((class color) (min-colors 16)) (:foreground nil :weight bold))))
   '(cfw:face-sunday ((((class color) (min-colors 257)) (:foreground "#d44b66" :weight bold)) (((class color) (min-colors 256)) (:foreground nil :weight bold)) (((class color) (min-colors 16)) (:foreground nil :weight bold))))
   '(cfw:face-title ((((class color) (min-colors 257)) (:foreground "#82aaff" :weight bold :height 2.0 :inherit variable-pitch)) (((class color) (min-colors 256)) (:foreground "#5fafff" :weight bold :height 2.0 :inherit variable-pitch)) (((class color) (min-colors 16)) (:foreground "brightblue" :weight bold :height 2.0 :inherit variable-pitch))))
   '(compilation-column-number ((((class color) (min-colors 89)) (:foreground "LightGreen")))) ;;; deeper-blue-theme
   '(compilation-error ((((class color) (min-colors 89)) (:foreground "Red1")))) ;;; deeper-blue-theme
   '(compilation-info ((((class color) (min-colors 89)) (:weight normal :foreground "LightSkyBlue")))) ;;; deeper-blue-theme
   '(compilation-line-number ((((class color) (min-colors 89)) (:foreground "LightGreen")))) ;;; deeper-blue-theme
   '(compilation-mode-line-exit ((((class color) (min-colors 89)) (:foreground "blue4")))) ;;; deeper-blue-theme
   '(cperl-array-face ((((class color) (min-colors 89)) (:foreground "yellow2")))) ;;; deeper-blue-theme
   '(cperl-hash-face ((((class color) (min-colors 89)) (:foreground "coral1")))) ;;; deeper-blue-theme
   '(cursor ((((class color) (min-colors 89)) (:background "green")))) ;;; deeper-blue-theme
   '(default ((((class color) (min-colors 89)) (:background "#181a26" :foreground "gray80")))) ;;; deeper-blue-theme
   '(diff-context ((((class color) (min-colors 89)) (:foreground "seashell4")))) ;;; deeper-blue-theme
   '(diff-file-header ((((class color) (min-colors 89)) (:background "grey60")))) ;;; deeper-blue-theme
   '(diff-function ((((class color) (min-colors 89)) (:inherit diff-header)))) ;;; deeper-blue-theme
   '(diff-header ((((class color) (min-colors 89)) (:background "grey45")))) ;;; deeper-blue-theme
   '(diff-hunk-header ((((class color) (min-colors 89)) (:inherit diff-header)))) ;;; deeper-blue-theme
   '(diff-index ((((class color) (min-colors 89)) (:inherit diff-file-header)))) ;;; deeper-blue-theme
   '(diff-indicator-added ((((class color) (min-colors 89)) (:foreground "white" :background "darkolivegreen")))) ;;; deeper-blue-theme
   '(diff-indicator-changed ((((class color) (min-colors 89)) (:foreground "white" :background "dodgerblue4")))) ;;; deeper-blue-theme
   '(diff-indicator-removed ((((class color) (min-colors 89)) (:foreground "white" :background "indianred4")))) ;;; deeper-blue-theme
   '(diff-refine-change ((((class color) (min-colors 89)) (:background "skyblue4")))) ;;; deeper-blue-theme
   '(dired-marked ((((class color) (min-colors 89)) (:background "dodgerblue3" :foreground "white")))) ;;; deeper-blue-theme
   '(ediff-current-diff-A ((((class color) (min-colors 89)) (:background "green4" :foreground "white")))) ;;; deeper-blue-theme
   '(ediff-current-diff-B ((((class color) (min-colors 89)) (:background "darkorange3" :foreground "white")))) ;;; deeper-blue-theme
   '(ediff-even-diff-B ((((class color) (min-colors 89)) (:background "Grey50" :foreground "White")))) ;;; deeper-blue-theme
   '(ediff-fine-diff-A ((((class color) (min-colors 89)) (:background "skyblue4" :foreground "white")))) ;;; deeper-blue-theme
   '(ediff-fine-diff-B ((((class color) (min-colors 89)) (:background "cyan4" :foreground "white")))) ;;; deeper-blue-theme
   '(ediff-odd-diff-A ((((class color) (min-colors 89)) (:background "Grey50" :foreground "White")))) ;;; deeper-blue-theme
   '(error ((((class color) (min-colors 89)) (:foreground "red")))) ;;; deeper-blue-theme
   '(escape-glyph ((((class color) (min-colors 257)) (:foreground "#89DDFF")) (((class color) (min-colors 256)) (:foreground "#5fd7ff")) (((class color) (min-colors 16)) (:foreground "brightcyan"))))
   '(flymake-errline ((((class color) (min-colors 89)) (:background nil :underline "red")))) ;;; deeper-blue-theme
   '(flymake-warnline ((((class color) (min-colors 89)) (:background nil :underline "magenta3")))) ;;; deeper-blue-theme
   '(font-latex-bold-face ((t (:inherit bold))))
   '(font-latex-italic-face ((t (:inherit italic))))
   '(font-latex-math-face ((((class color) (min-colors 257)) (:foreground "#82aaff")) (((class color) (min-colors 256)) (:foreground "#5fafff")) (((class color) (min-colors 16)) (:foreground "brightblue"))))
   '(font-latex-script-char-face ((((class color) (min-colors 257)) (:foreground "#7986E7")) (((class color) (min-colors 256)) (:foreground "#d7ffff")) (((class color) (min-colors 16)) (:foreground "blue"))))
   '(font-latex-sectioning-0-face ((((class color) (min-colors 257)) (:foreground "#82aaff" :weight ultra-bold)) (((class color) (min-colors 256)) (:foreground "#5fafff" :weight ultra-bold)) (((class color) (min-colors 16)) (:foreground "brightblue" :weight ultra-bold))))
   '(font-latex-sectioning-1-face ((((class color) (min-colors 257)) (:foreground "#c792ea" :weight semi-bold)) (((class color) (min-colors 256)) (:foreground "#d787d7" :weight semi-bold)) (((class color) (min-colors 16)) (:foreground "brightmagenta" :weight semi-bold))))
   '(font-latex-sectioning-2-face ((((class color) (min-colors 257)) (:foreground "#bb80b3" :weight semi-bold)) (((class color) (min-colors 256)) (:foreground "#d787af" :weight semi-bold)) (((class color) (min-colors 16)) (:foreground "magenta" :weight semi-bold))))
   '(font-latex-sectioning-3-face ((((class color) (min-colors 257)) (:foreground "#a7c3ff" :weight semi-bold)) (((class color) (min-colors 256)) (:foreground "#8fc7ff" :weight semi-bold)) (((class color) (min-colors 16)) (:foreground "brightblue" :weight semi-bold))))
   '(font-latex-sectioning-4-face ((((class color) (min-colors 257)) (:foreground "#d7b2f0" :weight semi-bold)) (((class color) (min-colors 256)) (:foreground "#e3abe3" :weight semi-bold)) (((class color) (min-colors 16)) (:foreground "brightmagenta" :weight semi-bold))))
   '(font-latex-sectioning-5-face ((((class color) (min-colors 257)) (:foreground "#cfa6c9" :weight semi-bold)) (((class color) (min-colors 256)) (:foreground "#e3abc7" :weight semi-bold)) (((class color) (min-colors 16)) (:foreground "magenta" :weight semi-bold))))
   '(font-latex-string-face ((t (:inherit font-lock-string-face))))
   '(font-latex-verbatim-face ((((class color) (min-colors 257)) (:inherit fixed-pitch :foreground "#bb80b3" :slant italic)) (((class color) (min-colors 256)) (:inherit fixed-pitch :foreground "#d787af" :slant italic)) (((class color) (min-colors 16)) (:inherit fixed-pitch :foreground "magenta" :slant italic))))
   '(font-latex-warning-face ((t (:inherit font-lock-warning-face))))
   '(font-lock-builtin-face ((((class color) (min-colors 89)) (:foreground "LightCoral")))) ;;; deeper-blue-theme
   '(font-lock-comment-delimiter-face ((((class color) (min-colors 89)) (:foreground "gray50")))) ;;; deeper-blue-theme
   '(font-lock-comment-face ((((class color) (min-colors 89)) (:foreground "gray50")))) ;;; deeper-blue-theme
   '(font-lock-constant-face ((((class color) (min-colors 89)) (:foreground "DarkOliveGreen3")))) ;;; deeper-blue-theme
   '(font-lock-doc-face ((((class color) (min-colors 89)) (:foreground "moccasin")))) ;;; deeper-blue-theme
   '(font-lock-doc-string-face ((((class color) (min-colors 89)) (:foreground "moccasin")))) ;;; deeper-blue-theme
   '(font-lock-function-name-face ((((class color) (min-colors 89)) (:foreground "goldenrod")))) ;;; deeper-blue-theme
   '(font-lock-keyword-face ((((class color) (min-colors 89)) (:foreground "DeepSkyBlue1")))) ;;; deeper-blue-theme
   '(font-lock-negation-char-face ((((class color) (min-colors 257)) (:inherit bold :foreground "#89DDFF")) (((class color) (min-colors 256)) (:inherit bold :foreground "#5fd7ff")) (((class color) (min-colors 16)) (:inherit bold :foreground "brightcyan"))))
   '(font-lock-number-face ((((class color) (min-colors 257)) (:foreground "#f78c6c")) (((class color) (min-colors 256)) (:foreground "#ff5f00")) (((class color) (min-colors 16)) (:foreground "brightred"))))
   '(font-lock-preprocessor-char-face ((((class color) (min-colors 257)) (:inherit bold :foreground "#89DDFF")) (((class color) (min-colors 256)) (:inherit bold :foreground "#5fd7ff")) (((class color) (min-colors 16)) (:inherit bold :foreground "brightcyan"))))
   '(font-lock-preprocessor-face ((((class color) (min-colors 89)) (:foreground "gold")))) ;;; deeper-blue-theme
   '(font-lock-reference-face ((((class color) (min-colors 89)) (:foreground "LightCoral")))) ;;; deeper-blue-theme
   '(font-lock-regexp-grouping-backslash ((((class color) (min-colors 89)) (:weight bold)))) ;;; deeper-blue-theme
   '(font-lock-regexp-grouping-construct ((((class color) (min-colors 89)) (:weight bold)))) ;;; deeper-blue-theme
   '(font-lock-string-face ((((class color) (min-colors 89)) (:foreground "burlywood")))) ;;; deeper-blue-theme
   '(font-lock-type-face ((((class color) (min-colors 89)) (:foreground "CadetBlue1")))) ;;; deeper-blue-theme
   '(font-lock-variable-name-face ((((class color) (min-colors 89)) (:foreground "SeaGreen2")))) ;;; deeper-blue-theme
   '(font-lock-warning-face ((t (:inherit warning))))
   '(fringe ((((class color) (min-colors 89)) (:background "black")))) ;;; deeper-blue-theme
   '(header-line ((t (:inherit mode-line))))
   '(header-line-highlight ((t (:inherit mode-line-highlight))))
   '(highlight ((((class color) (min-colors 89)) (:background "DodgerBlue4")))) ;;; deeper-blue-theme
   '(ido-first-match ((((class color) (min-colors 89)) (:weight normal :foreground "orange")))) ;;; deeper-blue-theme
   '(ido-only-match ((((class color) (min-colors 89)) (:foreground "green")))) ;;; deeper-blue-theme
   '(ido-subdir ((((class color) (min-colors 89)) (:foreground nil :inherit font-lock-keyword-face)))) ;;; deeper-blue-theme
   '(info-header-node ((((class color) (min-colors 89)) (:foreground "DeepSkyBlue1")))) ;;; deeper-blue-theme
   '(info-header-xref ((((class color) (min-colors 89)) (:foreground "SeaGreen2")))) ;;; deeper-blue-theme
   '(info-menu-header ((((class color) (min-colors 89)) (:family "helv" :weight bold)))) ;;; deeper-blue-theme
   '(info-node ((((class color) (min-colors 89)) (:foreground "DeepSkyBlue1")))) ;;; deeper-blue-theme
   '(info-xref ((((class color) (min-colors 89)) (:foreground "SeaGreen2")))) ;;; deeper-blue-theme
   '(isearch ((((class color) (min-colors 89)) (:background "coral2" :foreground "white")))) ;;; deeper-blue-theme
   '(isearch-lazy-highlight-face ((((class color) (min-colors 89)) (:background "coral4" :foreground "white")))) ;;; deeper-blue-theme
   '(italic ((t (:slant italic))))
   '(lazy-highlight ((((class color) (min-colors 89)) (:background "cadetblue" :foreground "white")))) ;;; deeper-blue-theme
   '(line-number ((((class color) (min-colors 257)) (:inherit default :foreground "#676E95" :distant-foreground unspecified :weight normal :italic unspecified :underline unspecified :strike-through unspecified)) (((class color) (min-colors 256)) (:inherit default :foreground "#585858" :distant-foreground unspecified :weight normal :italic unspecified :underline unspecified :strike-through unspecified)) (((class color) (min-colors 16)) (:inherit default :foreground "brightblack" :distant-foreground unspecified :weight normal :italic unspecified :underline unspecified :strike-through unspecified))))
   '(line-number-current-line ((((class color) (min-colors 257)) (:inherit (hl-line default) :foreground "#EEFFFF" :distant-foreground unspecified :weight normal :italic unspecified :underline unspecified :strike-through unspecified)) (((class color) (min-colors 256)) (:inherit (hl-line default) :foreground "#e4e4e4" :distant-foreground unspecified :weight normal :italic unspecified :underline unspecified :strike-through unspecified)) (((class color) (min-colors 16)) (:inherit (hl-line default) :foreground "brightwhite" :distant-foreground unspecified :weight normal :italic unspecified :underline unspecified :strike-through unspecified))))
   '(link ((((class color) (min-colors 257)) (:foreground "#c792ea" :underline t :weight bold)) (((class color) (min-colors 256)) (:foreground "#d787d7" :underline t :weight bold)) (((class color) (min-colors 16)) (:foreground "brightmagenta" :underline t :weight bold))))
   '(match ((((class color) (min-colors 89)) (:background "DeepPink4")))) ;;; deeper-blue-theme
   '(minibuffer-prompt ((((class color) (min-colors 89)) (:foreground "CadetBlue1")))) ;;; deeper-blue-theme
   '(mode-line ((((class color) (min-colors 89)) (:background "gray75" :foreground "black" :box (:line-width 1 :style released-button))))) ;;; deeper-blue-theme
   '(mode-line-active ((((class color) (min-colors 257)) (:background "DodgerBlue3")))) ;; 00bfff
   ;; '(mode-line-active ((t (:inherit mode-line))))
   '(mode-line-buffer-id ((((class color) (min-colors 89)) (:weight bold :background nil :foreground "blue4")))) ;;; deeper-blue-theme
   '(mode-line-emphasis ((((class color) (min-colors 257)) (:foreground "#c792ea" :distant-foreground "#292D3E")) (((class color) (min-colors 256)) (:foreground "#d787d7" :distant-foreground nil)) (((class color) (min-colors 16)) (:foreground "brightmagenta" :distant-foreground nil))))
   '(mode-line-highlight ((((class color) (min-colors 257)) (:inherit highlight :distant-foreground "#292D3E")) (((class color) (min-colors 256)) (:inherit highlight :distant-foreground nil)) (((class color) (min-colors 16)) (:inherit highlight :distant-foreground nil))))
   '(mode-line-inactive ((((class color) (min-colors 89)) (:background "gray40" :foreground "black" :box (:line-width 1 :color "gray40" :style nil))))) ;;; deeper-blue-theme
   '(nobreak-space ((t (:inherit escape-glyph :underline t))))
   '(outline-1 ((((class color) (min-colors 89)) (:foreground "SkyBlue1")))) ;;; deeper-blue-theme
   '(outline-2 ((((class color) (min-colors 89)) (:foreground "CadetBlue1")))) ;;; deeper-blue-theme
   '(outline-3 ((((class color) (min-colors 89)) (:foreground "LightSteelBlue1")))) ;;; deeper-blue-theme
   '(outline-4 ((((class color) (min-colors 89)) (:foreground "turquoise2")))) ;;; deeper-blue-theme
   '(outline-5 ((((class color) (min-colors 89)) (:foreground "aquamarine1")))) ;;; deeper-blue-theme
   '(primary-selection ((((class color) (min-colors 89)) (:background "blue3")))) ;;; deeper-blue-theme
   '(region ((((class color) (min-colors 89)) (:background "#103050")))) ;;; deeper-blue-theme
   '(bg-region ((((class color) (min-colors 89)) (:background "#2f2630")))) ;;; deeper-blue-theme
   ;; '(org-block-begin-line ((((class color) (min-colors 89)) (:underline "#1D2C39" :foreground "#676E95" :background "#1D2C39"))))
   ;; '(org-block-end-line ((((class color) (min-colors 89)) (:overline "#1D2C39" :foreground "#676E95" :background "#1D2C39"))))
   ;; '(org-block ((((class color) (min-colors 89)) (:background "#242635")))) ;;; deeper-blue-theme
   ;; '(org-block-background ((((class color) (min-colors 257)) (:background "#FFFFEA" :extend t)))) ;;; deeper-blue-theme
   '(secondary-selection ((((class color) (min-colors 257)) (:background "#676E95" :extend t)) (((class color) (min-colors 256)) (:background "#585858" :extend t)) (((class color) (min-colors 16)) (:background "brightblack" :extend t))))
   '(shadow ((((class color) (min-colors 257)) (:foreground "#676E95")) (((class color) (min-colors 256)) (:foreground "#585858")) (((class color) (min-colors 16)) (:foreground "brightblack"))))
   '(show-paren-match-face ((((class color) (min-colors 89)) (:background "dodgerblue1" :foreground "white")))) ;;; deeper-blue-theme
   '(show-paren-mismatch-face ((((class color) (min-colors 89)) (:background "red1" :foreground "white")))) ;;; deeper-blue-theme
   '(success ((((class color) (min-colors 89)) (:foreground "SeaGreen2")))) ;;; deeper-blue-theme
   '(tab-line ((((class color) (min-colors 257)) (:background "#242837" :foreground "#242837" :box nil)) (((class color) (min-colors 256)) (:background nil :foreground nil :box nil)) (((class color) (min-colors 16)) (:background nil :foreground nil :box nil))))
   '(tab-line-close-highlight ((((class color) (min-colors 257)) (:foreground "#c792ea")) (((class color) (min-colors 256)) (:foreground "#d787d7")) (((class color) (min-colors 16)) (:foreground "brightmagenta"))))
   '(tab-line-highlight ((t (:inherit tab-line-tab))))
   '(tab-line-tab ((((class color) (min-colors 257)) (:background "#292D3E" :foreground "#EEFFFF" :box nil)) (((class color) (min-colors 256)) (:background nil :foreground "#e4e4e4" :box nil)) (((class color) (min-colors 16)) (:background nil :foreground "brightwhite" :box nil))))
   '(tab-line-tab-current ((((class color) (min-colors 257)) (:background "#292D3E" :foreground "#EEFFFF")) (((class color) (min-colors 256)) (:background nil :foreground "#e4e4e4")) (((class color) (min-colors 16)) (:background nil :foreground "brightwhite"))))
   '(tab-line-tab-inactive ((((class color) (min-colors 257)) (:inherit tab-line-tab :background "#242837" :foreground "#BFC7D5" :box nil)) (((class color) (min-colors 256)) (:inherit tab-line-tab :background nil :foreground "#bcbcbc" :box nil)) (((class color) (min-colors 16)) (:inherit tab-line-tab :background nil :foreground "white" :box nil))))
   '(tab-line-tab-inactive-alternate ((t (:inherit tab-line-tab-inactive))))
   '(tooltip ((((class color) (min-colors 257)) (:background "#1c202c" :foreground "#EEFFFF")) (((class color) (min-colors 256)) (:background nil :foreground "#e4e4e4")) (((class color) (min-colors 16)) (:background nil :foreground "brightwhite"))))
   '(trailing-whitespace ((((class color) (min-colors 257)) (:background "#ff5370")) (((class color) (min-colors 256)) (:background "#ff0000")) (((class color) (min-colors 16)) (:background "red"))))
   '(vertical-border ((((class color) (min-colors 257)) (:background "#232635" :foreground "#232635")) (((class color) (min-colors 256)) (:background "#303030" :foreground "#303030")) (((class color) (min-colors 16)) (:background "brightblack" :foreground "brightblack"))))
   '(warning ((((class color) (min-colors 89)) (:foreground "Yellow"))))) ;;; deeper-blue-theme
   ;; '(diff-added ((((class color) (min-colors 89)) (nil)))) ;;; deeper-blue-theme
   ;; '(diff-changed ((((class color) (min-colors 89)) (nil)))) ;;; deeper-blue-theme
   ;; '(diff-removed ((((class color) (min-colors 89)) (nil)))) ;;; deeper-blue-theme

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'palenight-deeper-blue)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; palenight-deeper-blue-theme.el ends here.
