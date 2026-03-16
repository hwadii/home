;;; wh-fonts.el --- -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(use-package fontaine
  :ensure t
  :init (fontaine-mode 1)
  :config (fontaine-set-preset 'regular)
  :custom
  (fontaine-presets
   '((regular
      :default-family "Berkeley Mono Variable"
      :default-height 160
      :default-weight regular
      :fixed-pitch-family "Berkeley Mono Variable"
      :fixed-pitch-weight regular
      :variable-pitch-family "Input Sans Condensed"
      :variable-pitch-height 160
      :variable-pitch-weight regular
      :bold-weight bold)
     (narrow
      :default-family "Iosevka SS05"
      :default-height 160
      :default-weight regular
      :default-width expanded
      :fixed-pitch-family "Iosevka SS05"
      :fixed-pitch-weight regular
      :fixed-pitch-width expanded
      :variable-pitch-family "Input Sans Condensed"
      :variable-pitch-height 160
      :variable-pitch-weight regular
      :bold-weight bold
      :line-spacing 1)
     (legible
      :default-family "Geist Mono"
      :default-height 160
      :default-weight regular
      :fixed-pitch-family "Geist Mono"
      :fixed-pitch-weight regular
      :variable-pitch-family "Geist"
      :variable-pitch-height 160
      :variable-pitch-weight regular
      :bold-weight semibold
      :line-spacing 0.001)
     (classic
      :default-family "Lilex"
      :default-height 160
      :default-weight regular
      :fixed-pitch-family "Lilex"
      :fixed-pitch-weight regular
      :variable-pitch-family "Input Sans Condensed"
      :variable-pitch-height 150
      :variable-pitch-weight regular
      :bold-weight medium
      :line-spacing 0.0001))))

(provide 'wh-fonts)
;;; wh-fonts.el ends here
