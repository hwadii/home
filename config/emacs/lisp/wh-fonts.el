;;; wh-fonts.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package fontaine
  :ensure t
  :init (fontaine-mode 1)
  :config (fontaine-set-preset 'classic)
  :custom
  (fontaine-presets
   '((regular
      :default-family "Berkeley Mono Variable"
      :default-height 160
      :default-weight semilight
      :fixed-pitch-family "Berkeley Mono Variable"
      :fixed-pitch-weight semilight
      :variable-pitch-family "Miriam Libre"
      :variable-pitch-height 150
      :variable-pitch-weight regular
      :bold-weight bold
      :line-spacing 0.001)
     (narrow
      :default-family "Aporetic Sans Mono"
      :default-height 170
      :default-weight regular
      :fixed-pitch-family "Aporetic Sans Mono"
      :fixed-pitch-weight regular
      :variable-pitch-family "Aporetic Sans"
      :variable-pitch-height 170
      :variable-pitch-weight regular
      :bold-weight medium
      :line-spacing 0.001)
     (legible
      :default-family "Geist Mono"
      :default-height 160
      :default-weight regular
      :fixed-pitch-family "Geist Mono"
      :fixed-pitch-weight regular
      :variable-pitch-family "Geist"
      :variable-pitch-height 150
      :variable-pitch-weight regular
      :bold-weight semibold
      :line-spacing 1)
     (classic
      :default-family "Source Code Pro"
      :default-height 160
      :default-weight regular
      :fixed-pitch-family "Source Code Pro"
      :fixed-pitch-weight regular
      :variable-pitch-family "Source Sans Pro"
      :variable-pitch-height 160
      :variable-pitch-weight regular
      :bold-weight bold))))

(provide 'wh-fonts)
;;; wh-fonts.el ends here
