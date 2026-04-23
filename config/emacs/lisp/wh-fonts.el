;;; wh-fonts.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package fontaine
  :ensure t
  :config (fontaine-set-preset 'regular)
  :custom
  (fontaine-presets
   '((regular
      :default-family "Berkeley Mono Variable"
      :default-height 160
      :default-weight regular
      :default-width regular
      :fixed-pitch-family nil
      :fixed-pitch-weight nil
      :fixed-pitch-height 1.0
      :variable-pitch-family "Input Sans Condensed"
      :variable-pitch-height 150
      :variable-pitch-weight regular
      :bold-weight bold)
     (narrow
      :default-family "Aporetic Sans Mono"
      :default-height 160
      :default-weight regular
      :fixed-pitch-family nil
      :fixed-pitch-weight nil
      :variable-pitch-family "Aporetic Sans"
      :variable-pitch-height 160
      :variable-pitch-weight regular
      :bold-weight bold
      :line-spacing 1)
     (legible
      :default-family "CommitMono"
      :default-height 170
      :default-weight regular
      :fixed-pitch-family nil
      :fixed-pitch-weight nil
      :variable-pitch-family "Atkinson Hyperlegible Next"
      :variable-pitch-height 170
      :variable-pitch-weight regular
      :bold-weight bold
      :line-spacing 0.001)
     (classic
      :default-family "JetBrains Mono NL"
      :default-height 160
      :default-weight semilight
      :fixed-pitch-family nil
      :fixed-pitch-weight nil
      :fixed-pitch-height 1.0
      :variable-pitch-family "Input Sans Condensed"
      :variable-pitch-height 150
      :variable-pitch-weight regular
      :bold-weight bold
      :line-spacing 0.001))))

(set-fontset-font t nil "Font Awesome 7 Free" nil 'append)
(set-fontset-font t nil "Symbols Nerd Font" nil 'append)

(provide 'wh-fonts)
;;; wh-fonts.el ends here
