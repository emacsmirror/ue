;;; ue.el --- Minor mode for Unreal Engine projects -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Oleksandr Manenko

;; Author:    Oleksandr Manenko <seidfzehsd@use.startmail.com>
;; URL:       https://gitlab.com/unrealemacs/ue.el
;; Version:   0.0.1
;; Created:   26 August 2021
;; Keywords:  unreal engine, languages, tools
;; Package-Requires: ((emacs "25.1") (projectile "0.12.0"))

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; To start it for Unreal Engine projects:
;;    (ue-global-mode)
;;

;;; Code:

(require 'projectile)

;; TODO: *A*ctors
;;       *I*nterfaces
;;       *U*Objects
;;       *F*Classes
;; TODO: Set current target and save it? Then use that for a compile command.
;; TODO: Recommend to switch from projectile alien mode.
;; TODO: Recommend installing ag Emacs package.
;; TODO: Run the editor.
;; https://docs.unrealengine.com/4.26/en-US/ProductionPipelines/CommandLineArguments/
;; TODO: Build configuration
;; https://docs.unrealengine.com/4.26/en-US/ProductionPipelines/DevelopmentSetup/BuildConfigurations/
;; TODO: Class wizards

;; Functions used to sort Unreal C++ keywords by length which is used in font locking
(eval-and-compile
  (defun ue--c++-string-length< (a b) (< (length a) (length b)))
  (defun ue--c++-string-length> (a b) (not (ue--c++-string-length< a b))))

(defgroup ue nil
  "A minor mode for Unreal Engine projects."
  :prefix "ue-"
  :group  'projectile)

(defcustom ue-expand-snippets t
  "Enable Unreal Engine C++ yasnippet snippets."
  :group 'ue
  :type  'boolean)

(defcustom ue-attributes
  (eval-when-compile
    (sort '("UCLASS"
	    "UDELEGATE"
	    "UENUM"
	    "UFUNCTION"
	    "UINTERFACE"
	    "UMETA"
	    "UPARAM"
	    "UPROPERTY"
	    "USTRUCT")
	  #'ue--c++-string-length>))
  "List of Unreal C++ attributes."
  :type  '(choice (const :tag "Disabled" nil)
		  (repeat string))
  :group 'ue)

(defcustom ue-generated-body-macro
  (eval-when-compile
    (sort '("GENERATED_BODY"
	    "GENERATED_IINTERFACE_BODY"
	    "GENERATED_UCLASS_BODY"
	    "GENERATED_UINTERFACE_BODY"
	    "GENERATED_USTRUCT_BODY")
	  #'ue--c++-string-length>))
  "List of Unreal C++ GENERATED_*_BODY macros."
  :type  '(choice (const :tag "Disabled" nil)
		  (repeat string))
  :group 'ue)

(defcustom ue-attribute-face 'font-lock-preprocessor-face
  "Face for displaying Unreal attributes (UCLASS, UFUNCTION and the like)."
  :type  'symbol
  :group 'ue)

(defcustom ue-generated-body-macro-face 'font-lock-preprocessor-face
  "Face for displaying Unreal GENERATED_*_BODY macros."
  :type  'symbol
  :group 'ue)

(defvar ue--font-lock-attributes nil)
(defvar ue--font-lock-generated-body-macro nil)

(defun ue--generate-font-lock-attributes ()
  "Generate font-lock config for Unreal attributes."
  (let ((attributes-regexp (regexp-opt ue-attributes 'words)))
    (setq ue--font-lock-attributes
	  `((,attributes-regexp (0 ue-attribute-face))))))

(defun ue--generate-font-lock-generated-body-macro ()
  "Generate font-lock config for GENERATED_BODY macro and friends."
  (let ((generated-body-macro-regexp (regexp-opt ue-generated-body-macro 'words)))
    (setq ue--font-lock-generated-body-macro
	  `((,generated-body-macro-regexp (0 ue-generated-body-macro-face))))))

(defun ue-font-lock-add-keywords (&optional mode)
  "Add Unreal Engine keywords into major MODE or current buffer if nil."
  (font-lock-add-keywords mode (ue--generate-font-lock-attributes) nil)
  (font-lock-add-keywords mode (ue--generate-font-lock-generated-body-macro) nil))

(defun ue-font-lock-remove-keywords (&optional mode)
  "Remove Unreal Engine keywords from major MODE or current buffer if nil."
  (font-lock-remove-keywords mode ue--font-lock-attributes)
  (font-lock-remove-keywords mode ue--font-lock-generated-body-macro))

(defconst ue-root-directory ".uemacs"
  "The directory that is used to indentify Unreal Emacs project root.")

(defvar ue-cache-data (make-hash-table :test 'equal)
  "A hash table used for caching information about the current project.")

(defun ue-cache-key (key)
  "Generate a cache key based on the current directory and the given KEY."
  (format "%s-%s" default-directory key))

(defun ue-project-root ()
  "Return Unreal Emacs root directory if this file is part of the Unreal Emacs project else nil."
  (let* ((cache-key   (ue-cache-key "root"))
	 (cache-value (gethash cache-key ue-cache-data)))
    (or cache-value
	(ignore-errors
	  (let ((root (projectile-locate-dominating-file
		       default-directory
		       ue-root-directory)))
	    (puthash cache-key root ue-cache-data)
	    root)))))

;; Copied from yasnippet-snippets
(defconst ue-snippets-dir
  (expand-file-name
   "snippets"
   (file-name-directory
    (cond
     (load-in-progress load-file-name)
     ((and (boundp 'byte-compile-current-file) byte-compile-current-file)
      byte-compile-current-file)
     (:else (buffer-file-name))))))

(defun ue--register-snippets ()
  "Add Unreal Engine C++ snippets to yasnippet if it is available."
  (when (and ue-expand-snippets
	 (require 'yasnippet nil t))
    (add-to-list 'yas-snippet-dirs 'ue-snippets-dir t)
    (yas-load-directory ue-snippets-dir t)))

(defun ue--activate-snippets ()
  "Instruct yasnippet to consider Unreal Engine C++ snippets for expansion."
  (when (and ue-expand-snippets
	     (require 'yasnippet nil t))
    (yas-activate-extra-mode 'ue-mode)))

(defun ue-mode-init ()
  "Configure 'ue-mode'."
  (when (derived-mode-p 'c++-mode)
    (ue-font-lock-add-keywords)
    (font-lock-flush))
  (ue--activate-snippets))

(defun ue-mode-deinit ()
  "Cleanup change made by 'ue-mode."
  (when (derived-mode-p 'c++-mode)
    (ue-font-lock-remove-keywords)
    (font-lock-flush)))

;;;###autoload
(define-minor-mode ue-mode
  "Minor mode for Unreal Engine projects based on projectile-mode."
  :init-value nil
  :lighter    " ue"
  (if ue-mode
      (ue-mode-init)
    (ue-mode-deinit)))

;;;###autoload
(defun ue-on ()
  "Enable 'ue-mode' minor mode if this is an Unreal Engine based project."
  (when (and (projectile-project-p)
	     (ue-project-root))
    (ue-mode +1)))

;;;###autoload
(define-globalized-minor-mode ue-global-mode
  ue-mode
  ue-on)

(defun ue-off ()
  "Disable 'ue-mode' minor mode."
  (ue-mode -1))

;; Teach projectile how to recognize ue.el projects
(projectile-register-project-type 'ue           (list ue-root-directory)
				  :project-file ue-root-directory)

;; Add Unreal Engine C++ snippets
(with-eval-after-load "yasnippet"
  (ue--register-snippets))

(provide 'ue)

;;; ue.el ends here
