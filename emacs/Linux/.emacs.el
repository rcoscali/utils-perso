;; ====================================================================================
;; .emacs.el
;;
;; ====================================================================================

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
;;(set-face-font 'default "-mlss-Anonymice Powerline PNFT-regular-normal-normal-*-*-*-*-*-*-0-iso10646-1")
(set-face-font 'default "-    -NovaMono for Powerline-regular-normal-normal-*-*-*-*-*-m-0-iso10646-1")
(set-face-font 'bold "-mlss-Anonymous Pro-bold-normal-normal-*-*-*-*-*-m-0-iso10646-1")
(set-face-font 'italic "-mlss-Anonymous Pro-regular-italic-normal-*-*-*-*-*-m-0-iso10646-1")
;(set-face-font 'link "-outline-NSimSun-normal-normal-normal-mono-18-*-*-*-c-*-iso10646-1")
(set-face-font 'minibuffer-prompt            "-3270-IBM 3270-regular-normal-normal-*-*-*-*-*-m-0-iso10646-1")
;(set-face-font 'completions-annotations      "-outline-IBM 3270 Narrow-normal-italic-normal-mono-18-*-*-*-c-*-iso10646-1")
;(set-face-font 'completions-common-part      "-outline-IBM 3270 Narrow-bold-italic-normal-mono-18-*-*-*-c-*-iso10646-1")
;(set-face-font 'completions-first-difference "-outline-IBM 3270 Narrow-normal-italic-normal-mono-18-*-*-*-c-*-iso10646-1")
;(set-face-font 'completions-group-separator  "-outline-IBM 3270 Narrow-normal-italic-normal-mono-18-*-*-*-c-*-iso10646-1")
;;(set-face-font 'completions-group-title      "-outline-IBM 3270 Narrow-normal-italic-normal-mono-27-*-*-*-c-*-iso10646-1")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(ef-winter))
 '(custom-safe-themes
   '("b216e9b72dc8c2b702e4fcfd3c0af2d73c87eba46fd4db824ddb50863447d6a9" "abd7719fd9255fcd64f631664390e2eb89768a290ee082a9f0520c5f12a660a8" "6b4f7bde1ce64ea4604819fe56ff12cda2a8c803703b677fdfdb603e8b1f8bcb" "cb39485fd94dabefc5f2b729b963cbd0bac9461000c57eae454131ed4954a8ac" "9a3c51c59edfefd53e5de64c9da248c24b628d4e78cc808611abd15b3e58858f" "09feeb867d1ca5c1a33050d857ad6a5d62ad888f4b9136ec42002d6cdf310235" "b4fd44f653c69fb95d3f34f071b223ae705bb691fb9abaf2ffca3351e92aa374" "3e200d49451ec4b8baa068c989e7fba2a97646091fd555eca0ee5a1386d56077" "d89e15a34261019eec9072575d8a924185c27d3da64899905f8548cbd9491a36" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "72ed8b6bffe0bfa8d097810649fd57d2b598deef47c992920aef8b5d9599eefe" "ca47f7b222eb380e3035fb594d52032acd89dae0a49eac3da722a5cd3f485e3b" "ed1b7b4db911724b2767d4b6ad240f5f238a6c07e98fff8823debcfb2f7d820a" "e8480a7c0fcd73c430111858550dfb91326df654ccbe038485ba87158320595d" "702d0136433ca65a7aaf7cc8366bd75e983fe02f6e572233230a528f25516f7e" "058ba0ed929f801fc4077617e816797654c7775382943520875642d5507d8696" "fad2824e94f48ac33be713d287da005f7009bb5af9a41c214d660dced190e7db" default))
 '(package-selected-packages
   '(fullframe seq bison-mode cargo-mode demangle-mode flexoki-themes flycheck flycheck-buf-lint flycheck-clolyze flycheck-cstyle flycheck-dtrace flycheck-google-cpplint flycheck-guile flycheck-hdevtools flycheck-indent flycheck-jest flycheck-kotlin flycheck-objc-clang flycheck-ocaml flycheck-pkg-config flycheck-rust font-lock-studio font-utils forest-blue-theme form-feed fortune-cookie frame-tabs frameshot lsp-mode protobuf-mode protobuf-ts-mode rust-auto-use rust-mode rust-playground rustic term+ term+mux term-alert term-cmd term-manager term-run terminal-here)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
