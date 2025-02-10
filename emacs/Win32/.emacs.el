;; ===============================================================================================
;; .emacs.el - Init lisp script for emacs
;;
;; Init lisp script for emacs; This script setup my emacs lisp environment for fixing all my
;; settings in the emacs integrated development environment.
;; It setup & starts the emacs server allowing to use emacs-client. It also loads & sets up all
;; themes and i18n tricks, initialize/configure the emacs (M)ELPA packages manager and its sources
;; urls. Finally it defines all my custom bindings including fonts faces & colors.
;;
;; Copyright © 2024 Ampère Software Technology - Rémi COHEN SCALI <remi.cohen-scali@ampere.cars>
;; ===============================================================================================

;; Force delete server instance. This delete the EMACS_SERVER_FILE=C:/Users/a047461/EMACS~1.D/server/server
(server-force-delete)
;; Then restart server (and create EMACS_SERVER_FILE)
(server-start)

;; ====================================================================================
;; Use emacs packages
(require 'package)
(setq package-enable-at-startup nil)

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
			 user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))

;;; Standard package repositories
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Official MELPA Mirror, in case necessary.
  ;;(add-to-list 'package-archives (cons "melpa-mirror" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/")) t)
  (if (< emacs-major-version 24)
      ;; For important compatibility libraries like cl-lib
      (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))
    (unless no-ssl
      ;; Force SSL for GNU ELPA
      (setcdr (assoc "gnu" package-archives) "https://elpa.gnu.org/packages/"))))

;;; On-demand installation of packages
(require 'cl-lib)

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.If NO-REFRESH is non-nil, the available package lists will not bere-downloaded in order to locate PACKAGE."
  (or (package-installed-p package min-version)
      (let* ((known (cdr (assoc package package-archive-contents)))
	     (versions (mapcar #'package-desc-version known)))
	(if (cl-find-if (lambda (v) (version-list-<= min-version v)) versions)
	    (package-install package)
	  (if no-refresh (error "No version of %s >= %S is available" package min-version)
	    (package-refresh-contents)
	    (require-package package min-version t))))))

(defun maybe-require-package (package &optional min-version no-refresh)
  "Try to install PACKAGE, and return non-nil if successful.In the event of failure, return nil and print a warning message.Optionally require MIN-VERSION. If NO-REFRESH is non-nil, theavailable package lists will not be re-downloaded in order tolocate PACKAGE."
  (condition-case err (require-package package min-version no-refresh)
    (error (message "Couldn't install optional package `%s': %S" package err) nil)))

;;; Fire up package.el
(package-initialize)

;; package.el updates the saved version of package-selected-packages correctly only
;; after custom-file has been loaded, which is a bug. We work around this by adding;; the required packages to package-selected-packages after startup is complete.
(defvar sanityinc/required-packages nil)

(defun sanityinc/note-selected-package (oldfun package &rest args)
  "If OLDFUN reports PACKAGE was successfully installed, note it in `sanityinc/required-packages'."
  (let ((available (apply oldfun package args)))
    (prog1 available
      (when (and available (boundp 'package-selected-packages))
	(add-to-list 'sanityinc/required-packages package)))))

(advice-add 'require-package :around 'sanityinc/note-selected-package)
(when (fboundp 'package--save-selected-packages)
  (require-package 'seq)
  (add-hook 'after-init-hook (lambda ()
			       (package--save-selected-packages
				(seq-uniq (append sanityinc/required-packages package-selected-packages))))))

(require-package 'fullframe)
(fullframe list-packages quit-window)

(defun sanityinc/set-tabulated-list-column-width (col-name width)
  "Set any column with name COL-NAME to the given WIDTH."
  (when (> width (length col-name))
    (cl-loop for column across tabulated-list-format when (string= col-name (car column))
	     do (setf (elt column 1) width))))

(defun sanityinc/maybe-widen-package-menu-columns ()
  "Widen some columns of the package menu table to avoid truncation."
  (when (boundp 'tabulated-list-format)
    (sanityinc/set-tabulated-list-column-width "Version" 13)
    (let ((longest-archive-name
	   (apply 'max (mapcar 'length (mapcar 'car package-archives)))))
      (sanityinc/set-tabulated-list-column-width "Archive" longest-archive-name))))
(add-hook 'package-menu-mode-hook 'sanityinc/maybe-widen-package-menu-columns)

;; Provides ELPA
(provide 'init-elpa)

;; Global bindings
(global-set-key "\C-x\C-l" 'goto-line)
(global-set-key (kbd "<f9>") 'shell)

(add-to-list 'auto-mode-alist '("\\.ts\\'" . javascript-mode))

;; Activate column number display
(column-number-mode)

;; Font naming scheme:
;;  -$1-$2-$3-$4-$5-$6-$7-$8-$9-$10-$11-$12-$13 with:
;;    $1: font family          -outline
;;    $2: font name            -Lucida Console
;;    $3: font weight          -normal
;;    $4: font width           -normal
;;    $5: font variant         -normal
;;    $6: font charset         -mono
;;    $7: font size            -*
;;    $8: font size            -*
;;    $9: font size            -*
;;    $10: font size           -*
;;    $11: font size           -c
;;    $12: font size           -*
;;    $13: font size           -iso10646-1 
;;
;; "-outline-Lucida Console-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
;; "-outline-MS Gothic-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
;; "-outline-NSimSun-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
;; "-outline-SimSun-ExtB-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
;;  
;; "-outline-Anonymice Powerline-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
;; "-outline-Anonymice Powerline-bold-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
;; "-outline-Anonymice Powerline-normal-italic-normal-mono-*-*-*-*-c-*-iso10646-1"
;; "-outline-NSimSun-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1"
;;

;;(set-face-font 'default "-outline-Consolas-normal-normal-normal-mono-24-*-*-*-c-*-iso10646-1")
;;(set-face-font 'bold "-outline-Consolas-bold-normal-normal-mono-24-*-*-*-c-*-iso10646-1")
;;(set-face-font 'italic "-outline-Consolas-normal-italic-normal-mono-*-*-*-*-c-*-iso10646-1")
;;(set-face-font 'link "-outline-SimSun-ExtB-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1")
(set-face-font 'default "-outline-Anonymice Powerline-normal-normal-normal-mono-18-*-*-*-c-*-iso10646-1")
(set-face-font 'bold "-outline-Anonymice Powerline-bold-normal-normal-mono-18-*-*-*-c-*-iso10646-1")
(set-face-font 'italic "-outline-Anonymice Powerline-normal-italic-normal-mono-18-*-*-*-c-*-iso10646-1")
(set-face-font 'link "-outline-NSimSun-normal-normal-normal-mono-18-*-*-*-c-*-iso10646-1")
(set-face-font 'minibuffer-prompt            "-outline-IBM 3270 Narrow-normal-italic-normal-mono-18-*-*-*-c-*-iso10646-1")
(set-face-font 'completions-annotations      "-outline-IBM 3270 Narrow-normal-italic-normal-mono-18-*-*-*-c-*-iso10646-1")
(set-face-font 'completions-common-part      "-outline-IBM 3270 Narrow-bold-italic-normal-mono-18-*-*-*-c-*-iso10646-1")
(set-face-font 'completions-first-difference "-outline-IBM 3270 Narrow-normal-italic-normal-mono-18-*-*-*-c-*-iso10646-1")
(set-face-font 'completions-group-separator  "-outline-IBM 3270 Narrow-normal-italic-normal-mono-18-*-*-*-c-*-iso10646-1")
;;(set-face-font 'completions-group-title      "-outline-IBM 3270 Narrow-normal-italic-normal-mono-27-*-*-*-c-*-iso10646-1")

;; ====================================================================================
;; Load theme from installed ones
;; ====================================================================================

;; Load Abyss theme
;;(load-theme 'abyss t)

;; Load Acme theme
;;(load-theme 'acme t)
;; Turn foreground into full black instead of grey for Acme theme 
;;(setq acme-theme-black-fg t)

;; Load Afternoon theme
;;(load-theme 'afternoon t)

;; Load Ahungry theme
;; Only set this if you wish to retain your own font settings
;; otherwise, leave it out.
;;(setq ahungry-theme-font-settings nil)
;;(load-theme 'ahungry t)

;; Load Aircon theme
;;(load-theme 'aircon)

;; Load airline themes ----------------------------------------------------------------
(require 'powerline)
(powerline-default-theme)
(require 'airline-themes)

;; Customize airline theme
(setq
 ;; Hide Evil and buffer state on inactive buffers.
 ;; Valid Values: t (hidden), nil (shown)
 airline-hide-state-on-inactive-buffers t
 
 ;; "Hide eyebrowse indicator on inactive buffers.
 ;; Valid Values: t (hidden), nil (shown)"
 airline-hide-eyebrowse-on-inactive-buffers nil
 
 ;; Hide vc branch on inactive buffers:
 ;; Valid Values: t (hidden), nil (shown)
 airline-hide-vc-branch-on-inactive-buffers nil
 
 ;; Set eshell prompt colors to match the airline theme.
 ;; Valid Values: t (enabled), nil (disabled)
 airline-eshell-colors t
 
 ;; Set helm colors to match the airline theme.
 ;; Valid Values: t (enabled), nil (disabled)
 airline-helm-colors t
 
 ;; Set the cursor color based on the current evil state.
 ;; Valid Values: t (enabled), nil (disabled)
 airline-cursor-colors t
 
 ;; Display the currend directory along with the filename.
 ;; Valid Values: 'airline-directory-full
 ;;               'airline-directory-shortened
 ;;               nil (disabled)
 airline-display-directory 'airline-directory-shortened
 
 ;; Max directory length to display when using 'airline-directory-shortened
 airline-shortened-directory-length 48
 
 ;; Unicode character choices
 airline-utf-glyph-separator-left #xe0b0
 airline-utf-glyph-separator-right #xe0b2
 airline-utf-glyph-subseparator-left #xe0b1
 airline-utf-glyph-subseparator-right #xe0b3
 airline-utf-glyph-branch #xe0a0
 airline-utf-glyph-readonly #xe0a2
 airline-utf-glyph-linenumber #x2630
 
 ;; You may also wish to force powerline to use utf8 character separators
 powerline-default-separator 'utf-8
 powerline-utf-8-separator-left  #xe0b0
 powerline-utf-8-separator-right #xe0b2
 )

;; (load-theme 'airline-dark t)
;; (load-theme 'airline-base16-gui-dark t)
;; (load-theme 'airline-atomic t)
;; (load-theme 'airline-badwolf t)
;; (load-theme 'airline-base16_black_metal t)
;; (load-theme 'airline-base16_classic_dark t)
;; (load-theme 'airline-base16_espresso t)
;; (load-theme 'airline-base16_cupertino t)
;; (load-theme 'airline-base16_google_dark t)
;; (load-theme 'airline-ravenpower t)
;; (load-theme 'airline-alduin t)
;; (load-theme 'airline-angr t)
;; (load-theme 'airline-apprentice t)
;; (load-theme 'airline-ayu_dark t)
;; (load-theme 'airline-ayu_light t)
;; (load-theme 'airline-ayu_mirage t)
;; (load-theme 'airline-base16 t)
;; (load-theme 'airline-base16-gui-light t)
;; (load-theme 'airline-base16-shell-dark t)
;; (load-theme 'airline-base16_3024 t)
(load-theme 'airline-base16_adwaita t)
;; (load-theme 'airline-base16_apathy t)
;; (load-theme 'airline-base16_ashes t)
;; (load-theme 'airline-base16_atelier_cave t)
;; (load-theme 'airline-base16_atelier_cave_light t)
;; (load-theme 'airline-base16_atelier_dune t)
;; (load-theme 'airline-base16_atelier_dune_light t)
;; (load-theme 'airline-base16_atelier_estuary t)
;; (load-theme 'airline-base16_atelier_estuary_light t)
;; (load-theme 'airline-base16_atelier_forest t)
;; (load-theme 'airline-base16_atelier_forest_light t)
;; (load-theme 'airline-base16_atelier_heath t)
;; (load-theme 'airline-base16_atelier_heath_light t)
;; (load-theme 'airline-base16_atelier_lakeside t)
;; (load-theme 'airline-base16_atelier_lakeside_light t)
;; (load-theme 'airline-base16_atelier_plateau t)
;; (load-theme 'airline-base16_atelier_plateau_light t)
;; (load-theme 'airline-base16_atelier_savanna t)
;; (load-theme 'airline-base16_atelier_savanna_light t)
;; (load-theme 'airline-base16_atelier_seaside t)
;; (load-theme 'airline-base16_atelier_seaside_light t)
;; (load-theme 'airline-base16_atelier_sulphurpool t)
;; (load-theme 'airline-base16_atelier_sulphurpool_light t)
;; (load-theme 'airline-base16_atlas t)
;; (load-theme 'airline-base16_bespin t)
;; (load-theme 'airline-base16_black_metal_bathory t)
;; (load-theme 'airline-base16_black_metal_burzum t)
;; (load-theme 'airline-base16_black_metal_dark_funeral t)
;; (load-theme 'airline-base16_black_metal_gorgoroth t)
;; (load-theme 'airline-base16_black_metal_immortal t)
;; (load-theme 'airline-base16_black_metal_khold t)
;; (load-theme 'airline-base16_black_metal_marduk t)
;; (load-theme 'airline-base16_black_metal_mayhem t)
;; (load-theme 'airline-base16_black_metal_nile t)
;; (load-theme 'airline-base16_black_metal_venom t)
;; (load-theme 'airline-base16_brewer t)
;; (load-theme 'airline-base16_bright t)
;; (load-theme 'airline-base16_brogrammer t)
;; (load-theme 'airline-base16_brushtrees t)
;; (load-theme 'airline-base16_brushtrees_dark t)
;; (load-theme 'airline-base16_chalk t)
;; (load-theme 'airline-base16_circus t)
;; (load-theme 'airline-base16_classic t)
;; (load-theme 'airline-base16_classic_dark t)
;; (load-theme 'airline-base16_classic_light t)
;; (load-theme 'airline-base16_codeschool t)
;; (load-theme 'airline-base16_colors t)
;; (load-theme 'airline-base16_cupcake t)
;; (load-theme 'airline-base16_darktooth t)
;; (load-theme 'airline-base16_decaf t)
;; (load-theme 'airline-base16_default t)
;; (load-theme 'airline-base16_default_dark t)
;; (load-theme 'airline-base16_default_light t)
;; (load-theme 'airline-base16_dracula t)
;; (load-theme 'airline-base16_edge_dark t)
;; (load-theme 'airline-base16_edge_light t)
;; (load-theme 'airline-base16_eighties t)
;; (load-theme 'airline-base16_embers t)
;; (load-theme 'airline-base16_flat t)
;; (load-theme 'airline-base16_framer t)
;; (load-theme 'airline-base16_fruit_soda t)
;; (load-theme 'airline-base16_gigavolt t)
;; (load-theme 'airline-base16_github t)
;; (load-theme 'airline-base16_google t)
;; (load-theme 'airline-base16_google_light t)
;; (load-theme 'airline-base16_grayscale t)
;; (load-theme 'airline-base16_grayscale_dark t)
;; (load-theme 'airline-base16_grayscale_light t)
;; (load-theme 'airline-base16_greenscreen t)
;; (load-theme 'airline-base16_gruvbox_dark_hard t)
;; (load-theme 'airline-base16_gruvbox_dark_medium t)
;; (load-theme 'airline-base16_gruvbox_dark_pale t)
;; (load-theme 'airline-base16_gruvbox_dark_soft t)
;; (load-theme 'airline-base16_gruvbox_light_hard t)
;; (load-theme 'airline-base16_gruvbox_light_medium t)
;; (load-theme 'airline-base16_gruvbox_light_soft t)
;; (load-theme 'airline-base16_harmonic16 t)
;; (load-theme 'airline-base16_harmonic_dark t)
;; (load-theme 'airline-base16_harmonic_light t)
;; (load-theme 'airline-base16_heetch t)
;; (load-theme 'airline-base16_heetch_light t)
;; (load-theme 'airline-base16_helios t)
;; (load-theme 'airline-base16_hopscotch t)
;; (load-theme 'airline-base16_horizon_dark t)
;; (load-theme 'airline-base16_horizon_light t)
;; (load-theme 'airline-base16_horizon_terminal_dark t)
;; (load-theme 'airline-base16_horizon_terminal_light t)
;; (load-theme 'airline-base16_ia_dark t)
;; (load-theme 'airline-base16_ia_light t)
;; (load-theme 'airline-base16_icy t)
;; (load-theme 'airline-base16_irblack t)
;; (load-theme 'airline-base16_isotope t)
;; (load-theme 'airline-base16_londontube t)
;; (load-theme 'airline-base16_macintosh t)
;; (load-theme 'airline-base16_marrakesh t)
;; (load-theme 'airline-base16_materia t)
;; (load-theme 'airline-base16_material t)
;; (load-theme 'airline-base16_material_darker t)
;; (load-theme 'airline-base16_material_lighter t)
;; (load-theme 'airline-base16_material_palenight t)
;; (load-theme 'airline-base16_material_vivid t)
;; (load-theme 'airline-base16_mellow_purple t)
;; (load-theme 'airline-base16_mexico_light t)
;; (load-theme 'airline-base16_mocha t)
;; (load-theme 'airline-base16_monokai t)
;; (load-theme 'airline-base16_nord t)
;; (load-theme 'airline-base16_nova t)
;; (load-theme 'airline-base16_ocean t)
;; (load-theme 'airline-base16_oceanicnext t)
;; (load-theme 'airline-base16_one_light t)
;; (load-theme 'airline-base16_onedark t)
;; (load-theme 'airline-base16_outrun_dark t)
;; (load-theme 'airline-base16_papercolor_dark t)
;; (load-theme 'airline-base16_papercolor_light t)
;; (load-theme 'airline-base16_paraiso t)
;; (load-theme 'airline-base16_phd t)
;; (load-theme 'airline-base16_pico t)
;; (load-theme 'airline-base16_pop t)
;; (load-theme 'airline-base16_porple t)
;; (load-theme 'airline-base16_railscasts t)
;; (load-theme 'airline-base16_rebecca t)
;; (load-theme 'airline-base16_sandcastle t)
;; (load-theme 'airline-base16_seti t)
;; (load-theme 'airline-base16_shapeshifter t)
;; (load-theme 'airline-base16_shell t)
;; (load-theme 'airline-base16_snazzy t)
;; (load-theme 'airline-base16_solarflare t)
;; (load-theme 'airline-base16_solarized t)
;; (load-theme 'airline-base16_solarized_dark t)
;; (load-theme 'airline-base16_solarized_light t)
;; (load-theme 'airline-base16_spacemacs t)
;; (load-theme 'airline-base16_summerfruit t)
;; (load-theme 'airline-base16_summerfruit_dark t)
;; (load-theme 'airline-base16_summerfruit_light t)
;; (load-theme 'airline-base16_synth_midnight_dark t)
;; (load-theme 'airline-base16_tomorrow t)
;; (load-theme 'airline-base16_tomorrow_night t)
;; (load-theme 'airline-base16_tomorrow_night_eighties t)
;; (load-theme 'airline-base16_tube t)
;; (load-theme 'airline-base16_twilight t)
;; (load-theme 'airline-base16_unikitty_dark t)
;; (load-theme 'airline-base16_unikitty_light t)
;; (load-theme 'airline-base16_vim t)
;; (load-theme 'airline-base16_woodland t)
;; (load-theme 'airline-base16_xcode_dusk t)
;; (load-theme 'airline-base16_zenburn t)
;; (load-theme 'airline-base16color t)
;; (load-theme 'airline-behelit t)
;; (load-theme 'airline-biogoo t)
;; (load-theme 'airline-bubblegum t)
;; (load-theme 'airline-cobalt2 t)
;; (load-theme 'airline-cool t)
;; (load-theme 'airline-dark t)
;; (load-theme 'airline-dark_minimal t)
;; (load-theme 'airline-desertink t)
;; (load-theme 'airline-deus t)
;; (load-theme 'airline-distinguished t)
;; (load-theme 'airline-doom-molokai t)
;; (load-theme 'airline-doom-one t)
;; (load-theme 'airline-durant t)
;; (load-theme 'airline-fairyfloss t)
;; (load-theme 'airline-fruit_punch t)
;; (load-theme 'airline-google_dark t)
;; (load-theme 'airline-google_light t)
;; (load-theme 'airline-gruvbox-dark t)
;; (load-theme 'airline-hybrid t)
;; (load-theme 'airline-hybridline t)
;; (load-theme 'airline-jellybeans t)
;; (load-theme 'airline-jet t)
;; (load-theme 'airline-kalisi t)
;; (load-theme 'airline-kolor t)
;; (load-theme 'airline-laederon t)
;; (load-theme 'airline-lessnoise t)
;; (load-theme 'airline-light t)
;; (load-theme 'airline-lighthaus t)
;; (load-theme 'airline-lucius t)
;; (load-theme 'airline-luna t)
;; (load-theme 'airline-minimalist t)
;; (load-theme 'airline-molokai t)
;; (load-theme 'airline-monochrome t)
;; (load-theme 'airline-murmur t)
;; (load-theme 'airline-night_owl t)
;; (load-theme 'airline-nord_minimal t)
;; (load-theme 'airline-onedark t)
;; (load-theme 'airline-ouo t)
;; (load-theme 'airline-owo t)
;; (load-theme 'airline-papercolor t)
;; (load-theme 'airline-peaksea t)
;; (load-theme 'airline-powerlineish t)
;; (load-theme 'airline-qwq t)
;; (load-theme 'airline-raven t)
;; (load-theme 'airline-seagull t)
;; (load-theme 'airline-selenized t)
;; (load-theme 'airline-selenized_bw t)
;; (load-theme 'airline-seoul256 t)
;; (load-theme 'airline-serene t)
;; (load-theme 'airline-sierra t)
;; (load-theme 'airline-silver t)
;; (load-theme 'airline-simple t)
;; (load-theme 'airline-soda t)
;; (load-theme 'airline-sol t)
;; (load-theme 'airline-solarized t)
;; (load-theme 'airline-solarized-alternate-gui t)
;; (load-theme 'airline-solarized-gui t)
;; (load-theme 'airline-solarized_flood t)
;; (load-theme 'airline-supernova t)
;; (load-theme 'airline-term t)
;; (load-theme 'airline-term_light t)
;; (load-theme 'airline-tomorrow t)
;; (load-theme 'airline-transparent t)
;; (load-theme 'airline-ubaryd t)
;; (load-theme 'airline-understated t)
;; (load-theme 'airline-violet t)
;; (load-theme 'airline-wombat t)
;; (load-theme 'airline-xtermlight t)
;; (load-theme 'airline-zenburn t)

;; Load Ample theme
;(load-theme 'ample t)
;(load-theme 'ample-flat t)
;(load-theme 'ample-light t)

;; Load Alect theme
;(load-theme 'alect-black t)
;(load-theme 'alect-black-alt t)
;(load-theme 'alect-dark t)
;(load-theme 'alect-dark-alt t)
;(load-theme 'alect-light t)
;(load-theme 'alect-light-alt t)

;; Load Ample-zen theme
;;(load-theme 'ample-zen t)

;; Load Calmer-forest theme
(load-theme 'calmer-forest t)
(load-theme 'base16-darkviolet t)

;; Fix fore/background colors for some theme
;(set-foreground-color '"#000000")
;(set-background-color '"#707080")

;; Set Frame (window)  WidthxHeight
(set-frame-width nil 192)
(set-frame-height nil 72)

;; Fix dired directory face & face foreground color
;(set-face-font 			'dired-directory "-outline-Anonymice Powerline-bold-normal-normal-mono-*-*-*-*-c-*-iso10646-1")
;(set-face-foreground 	'dired-directory "#A040FF")
;(set-face-font			'dired-ignored   "-outline-Anonymice Powerline-normal-normal-normal-mono-*-*-*-*-c-*-iso10646-1")
;(set-face-foreground 	'dired-ignored   "#5020FF")
;(set-face-foreground 	'dired-broken-symlink	 "#5020FF")
;(set-face-foreground 	'dired-directory	 	 "#5020FF")
;(set-face-foreground 	'dired-flagged			 "#5020FF")
;(set-face-foreground 	'dired-header			 "#5020FF")
;(set-face-foreground 	'dired-ignored			 "#5020FF")
;(set-face-foreground 	'dired-mark				 "#5020FF")
;(set-face-foreground 	'dired-marked			 "#5020FF")
;(set-face-foreground 	'dired-perm-write		 "#5020FF")
;(set-face-foreground 	'dired-set-id			 "#5020FF")
;(set-face-foreground 	'dired-special			 "#5020FF")
;(set-face-foreground 	'dired-symlink			 "#5020FF")
;(set-face-foreground 	'dired-warning			 "#5020FF")


;; Add hook for rust mode for cargo minor mode. Setup scroll on output
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;; Add c-mode for .cin files
(add-to-list 'auto-mode-alist '("\\.cin\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.c.in\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.h.in\\'" . c-mode))
(add-to-list 'auto-mode-alist '("\\.c++\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.C\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.H\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . javascript-mode))
(add-to-list 'auto-mode-alist '("\\.xml\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xsd\\'" . nxml-mode))
(add-to-list 'auto-mode-alist '("Makefile.inc\\'" . makefile-mode))

(setq compilation-scroll-output t)

(defun my-revert-buffer-with-coding-system ()
  ""
  (interactive)
  (revert-buffer-with-coding-system 'utf-8-unix 't)
  )
(global-set-key (kbd "C-x RET r") 'my-revert-buffer-with-coding-system)

;; ====================================================================================
;; Custom config of emacs through 'Custom' conf variables
;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-default-style
   '((c-mode . "")
	 (c++-mode . "")
	 (java-mode . "java")
	 (awk-mode . "awk")
	 (other . "gnu")))
 '(c-label-minimum-indentation 2)
 '(custom-enabled-themes '(base16-darkviolet))
 '(custom-safe-themes
   '("aa3fcb18cad987feb30f7c840fdce709c47ad862eec4331d014dd40474fbbe10" "71f01d164fbc76d7186e6f93e5820166cbecb5b98b41db6a480a9d202a0b1b6a" "427cc3110288c2aa479309dc649bd81a88000ded78aad301cc04a4ede196b824" "a07c53c3c821494eaf093a3a822a392cab173e26f9918a27c4198d18d94a9114" "f700bc979515153bef7a52ca46a62c0aa519950cc06d539df4f3d38828944a2c" "711df73c2d73a038f3e4f9327fa96244f6b081a0ef631fafdc5d3a859d7257b7" "6e14d67edb9336686b4824223320cc7fa5e88fdf15fc030bd6c92ebaca9db963" "ecd76ed4cdfc0534b90b663fb18afdd18bd29d7f40761f658d995c9a82f6490b" "91db2df9490180a006964179f3aa4fcbc6bbf63cdcba189b41ea1ff5a606df33" "c7b8dbc62bf969295d0068d8dcb47bd1832d9c466bd76ddc6ac325b93cbdf7c6" "dbd6f98cd5f7d9692fcdcb5b6739b2c81b61fdc011b0a68ceee1ebadca531367" "bf9c9221f31fdf5605440286adc1a7fa1430d108da14487dcde1d8d42124fc2d" "83cdf477d72d640269bec7e42751570f99a4d6af23b8f2e8f797efe762bc28de" "943409d89579978aa5fc093e52a121aa3c1b5ff0b135d6fcfe741878a315750b" "2020c8be45db49182ef51ee5cd648ac4cd9caaebb19dac174f4bdd09cf42ad1d" "32300c2d3e3289dfcbdd757b5e5631a0028c50a4f1714a77dc331fc7114e66b6" "b0e53b10f0dfa8e8d64646929d913b24bf59460e00ffa16059863461cb855e19" "721bb3cb432bb6be7c58be27d583814e9c56806c06b4077797074b009f322509" "e57ad9eb8465dcb6b85eeb2f1be11a37aee7b1f24e5f99155b39ff0679e664d5" "5d67552ed2e841039034dc8245ee1746ab4f00614366ca7018386041f9b0a96f" "4491de9b215f405a52d03c084664efdc50131f31225651f68d79dcf308c75278" "adb567dc2208b9e95ce3bc1930bf0e84a3eced91e674b4c35777e54650a60763" "a12f585b1ff1b35f4000eab71f9f20a784b94f797296de13d467f9b3021b9a8b" "882d6a5981fd85d9f987d31623e25e69b8d5635a53ab442f1a51e7c252790320" "7fb25688a73234177e66a36b5c8178feed7da7a970d4464f51a0165ffcbf1286" "1576495a064188b7b3017d16763288a6e9583a01f02773144b246ef56e64ec76" "3b27421334802fec606d358062b519291fab80c520979b0834694a8ab939a4f1" "fd098e42a2f446c5d852ae074d37210252e2f3a72ba994d8f2c123d02da80385" "f8d9de5d4ed5c1b20835a59d13891869239b62c8e64d7e0d09a060ad4e653fb4" "a38d7a034d1c4aafd9831437fa983ce3c95de8ed20f2523f5a5a27259680ff28" "94256a2952c7a6a77b4f290e6a16d1b2ded393591e7e7fc03edf8cfd34a9b55e" "2f1157e7e30f85eb730a2d2c122b66491d48d32f6c92068970824d05b117ecad" "c3da4bc9704db159e601c727fe9c33945147749205bb4536ad11fb883faa96f9" "2902694c7ef5d2a757146f0a7ce67976c8d896ea0a61bd21d3259378add434c4" "9c204e727c8a05fb0aa9b065f0b15f483fb0a083e2b2f8b8ac5d89bf514def1b" "33c4f1e69f2f266a0b8e006858039298e6ff00868048cbfb9e20d7e0e4d410c3" "74797b7cbcc4356101a62037316149faa4935522775adaaa68972f7488361c0d" "c7eb06356fd16a1f552cfc40d900fe7326ae17ae7578f0ef5ba1edd4fdd09e58" "da19739a11a46ef7c252d9132d0c2c08c269ef2840bb19b136d8d79e370ab1a5" "aab08128c97e08edaad475d07151a6d2371d90ecc4e8935d2cdb519674a00117" "d678ec420b0ede7ace7adb0fa9f448329e132de2f868b20773e282eb29fb1498" "df85955fd38ee2dae7476a5fa93e58e594df96132871c10ecaf4de95bdae932a" "ecc077ef834d36aa9839ec7997aad035f4586df7271dd492ec75a3b71f0559b3" "c07daf599a7d07a18faaf354d269c48d4462ff7dbdbcc3773950a4b37b003d80" "00b463c48742afe509ae7d1dcfce09471f7203e13a118f1256b208017a978b4e" "5ed25f51c2ed06fc63ada02d3af8ed860d62707e96efc826f4a88fd511f45a1d" "de1f10725856538a8c373b3a314d41b450b8eba21d653c4a4498d52bb801ecd2" "4af38f1ae483eb9335402775b02e93a69f31558f73c869af0d2403f1c72d1d33" "45feb1f130c54e0fc116faa71c784562b41009ffc908cf5cef06b6df4bb60a9a" "7177e3299cffdcbe26f633ab07033b5f646c536dcddd9cd2d1859b4eb0c64f79" "437cd756e079901ccdecd9c397662a3ee4da646417d7469a1c35aa8e246562fe" "1711947b59ea934e396f616b81f8be8ab98e7d57ecab649a97632339db3a3d19" "e7ce09ff7426c9a290d06531edc4934dd05d9ea29713f9aabff834217dbb08e4" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "7153b82e50b6f7452b4519097f880d968a6eaf6f6ef38cc45a144958e553fbc6" "5a0eee1070a4fc64268f008a4c7abfda32d912118e080e18c3c865ef864d1bea" "c3e6b52caa77cb09c049d3c973798bc64b5c43cc437d449eacf35b3e776bf85c" "70f5a47eb08fe7a4ccb88e2550d377ce085fedce81cf30c56e3077f95a2909f2" "d6c5b14073abc649dad816750ef1ac7d5defdf1630d4e9938e06c38ad547d6da" "06a2eef27703cd3c8b017c90d9025d766ade307971826362c487a5273e14cc5a" "92cfd42cedb42fdd3ea0d84d518825a94f29b30be20f65978dab7d7c8fa30c6a" "7f954d5bee47a27cc6cc83d1d6b80f7a32d82f744a725375d6e22b65758f9a5e" "3eb4031719479655814b5db031492570cdc7c82e37f34d7707515590c926980f" "b87f0a7cc94fc07f1417f95de2382a7c1c853a6822d987af45b3b3c5e95e3abb" "df069ec238487ceab1cec64809a3c1dcef1393123ecdf430bdb7b94537ca2c6a" "0f964c8dbc5a668cc2ba7aa1003792fbbf3000a6ed69c0e53b1eeb2c1afc25cb" "b8720a6ec85bee63542f0b202763e0a40606863e9ca7ebd94b7fcd7744234312" "43b78a08f245bc198dadf35b324f445472c92dda3f1b6d1746cefee9f2ade177" "8d68cd652ab405df5db91acc66be58307e008bfac3ddc4beef7d7f814d37026c" "04b856ef48419963ad1b22dfe0e2b388e4bb9ee126d84fc0300352afc18732bd" "2b8dff32b9018d88e24044eb60d8f3829bd6bbeab754e70799b78593af1c3aba" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "9b88b8c64dc30188514f19d1be732ee71cc905b04b0c2c7eb1194528fcebbea4" default))
 '(indent-tabs-mode t)
 '(inhibit-startup-screen t)
 '(list-matching-lines-default-context-lines 1)
 '(list-matching-lines-jump-to-current-line t)
 '(package-selected-packages
   '(fullframe seq flycheck-buf-lint protobuf-mode lsp-mode rustic demangle-mode flycheck-rust rust-mode git-blamed git-command git-commit-insert-issue git-dwim git-grep git-identity git-walktree git-msg-prefix gerrit gerrit-download ggo-mode ggtags gh gh-md genrnc dired-filetype-face dired-icon dired-k dired-launch dired-quick-sort dired-rainbow dired-sidebar dired-subtree dired-toggle diff-ansi diff-at-point diffview dired-git dired-hacks-utils dired-imenu flimenu gtags-mode imenu-anywhere imenu-extra imenu-list imenus popup-imenu yaml-imenu cargo cargo-mode cargo-transient vterm-toggle vterm vscode-dark-plus-theme vscode-icon vscdark-theme vertico vertico-prescient orderless hide-lines hidepw map-regexp ample-regexps pug-mode cakecrumbs jade-mode bbdb csv-mode sql-indent emacsql emacsql-mysql emacsql-pg emacsql-psql emacsql-sqlite3 color-theme-buffer-local clang-capf clang-format clang-format+ challenger-deep-theme change-inner chapel-mode call-graph captain broadcast browse-kill-ring browse-url-dwim bshell buffer-env bookmark-in-project bookmark-view aggressive-fill-paragraph bibtex-completion bibtex-utils binclock bnf-mode asm-blox ascii-table ascii-art-to-unicode arduino-cli-mode apparmor-mode annotate android-env android-mode all all-ext all-the-icons all-the-icons-completion all-the-icons-dired all-the-icons-ibuffer all-the-icons-ivy all-the-icons-ivy-rich aes ack ack-menu achievements powerline abyss-theme acme-theme ahungry-theme airline-themes ample-zen-theme ancient-one-dark-theme anti-zenburn-theme arjen-grey-theme avk-emacs-themes ayu-theme badger-theme badwolf-theme base16-theme basic-theme berrys-theme birds-of-paradise-plus-theme blackboard-theme bliss-theme borland-blue-theme boron-theme brutalist-theme bubbleberry-theme busybee-theme calmer-forest-theme camcorder caroline-theme world-time-mode alarm-clock aa-edit-mode sweeprolog exercism flymake-sqlfluff hcel atl-markup atom-dark-theme atom-one-dark-theme bbcode-mode jira-markup-mode markup markup-faces seml-mode json-mode adjust-parens afternoon-theme aggressive-completion aggressive-indent aircon-theme alect-themes apropospriate-theme arduino-mode async autothemer avy beacon bison-mode bluetooth brief caml chess cl-lib cl-print clojure-mode comint-mime compat corfu-terminal coterm cpio-mode cycle-quotes devdocs dired-du dired-git-info disk-usage djvu ef-themes git-commit git-modes gited gnome-c-style gnu-elpa gnu-elpa-keyring-update html5-schema htmlize load-dir load-relative lua-mode nasm-mode nhexl-mode oauth2 perl-doc php-mode phps-mode pinentry psgml python undo-tree vcard vdiff visual-fill visual-fill-column web-server websocket yaml-mode zones))
 '(tab-always-indent t)
 '(tab-first-completion nil)
 '(tab-stop-list
   '(0 4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80))
 '(tab-width 4)
 '(tree-widget-theme nil)
 '(warning-suppress-log-types '((comp) (auto-save) (auto-save) (auto-save)))
 '(warning-suppress-types '((auto-save) (auto-save) (auto-save))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Fix comment & comment delimiter face foreground color
(set-face-foreground 	'font-lock-comment-delimiter-face "OrangeRed2")
(set-face-foreground 	'font-lock-comment-face "orange3")

