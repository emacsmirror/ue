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
(require 'json)

;; TODO: *A*ctors
;;       *I*nterfaces
;;       *U*Objects
;;       *F*Classes
;; TODO: Recommend to switch away from projectile alien mode.
;; TODO: Recommend installing ag Emacs package.
;; TODO: Class wizards
;; TODO: Debugging (lsp?)
;; TODO: Add run commands
;;       Editor configurations run the editor with the name of the project. The solution configuration affects the name
;;       of the Engine executable (UE4Editor-Mac-DebugGame, etc).
;;       Game configurations run the binary from the Binary project directory.
;; TODO: .NET projects?
;; TODO: Project.Target.cs files
;; TODO: Config files (*.ini)

;; Functions used to sort Unreal C++ keywords by length which is used in font locking
(eval-and-compile
  (defun ue--c++-string-length< (a b) (< (length a) (length b)))
  (defun ue--c++-string-length> (a b) (not (ue--c++-string-length< a b))))

(defun ue--alist-keys (alist)
  "Return keys of the given ALIST."
  (mapcar 'car alist))

(defgroup ue nil
  "A minor mode for Unreal Engine projects."
  :prefix "ue-"
  :group  'projectile)

(defcustom ue-expand-snippets t
  "Enable Unreal Engine C++ yasnippet snippets."
  :group 'ue
  :type  'boolean)

(defcustom ue-mode-line-prefix " ue"
  "Mode line lighter prefix for ue-mode.")

(defcustom ue-globally-ignored-files
  '("compile_commands.json")
  "A list of files globally ignored by projectile.
Note that files aren't filtered if `projectile-indexing-method' is set to 'alien'."
  :group 'ue
  :type '(repeat string))

(defcustom ue-globally-ignored-directories
    '("*Binaries"
      "*Content"
      "*DerivedDataCache"
      "*Intermediate"
      "*Saved"
      "*Script"
      "*.uemacs")
  "A list of directories globally ignored by projectile.
Regular expressions can be used.

Strings that don't start with * are only ignored at the top level
of the project. Strings that start with * are ignored everywhere
in the project, as if there was no *.  So note that * when used as
a prefix is not a wildcard; it is an indicator that the directory
should be ignored at all levels, not just root.

Examples: \"tmp\" ignores only ./tmp at the top level of the
project, but not ./src/tmp. \"*tmp\" will ignore both ./tmp and
./src/tmp, but not ./not-a-tmp or ./src/not-a-tmp.

Note that files aren't filtered if `projectile-indexing-method'
is set to 'alien'."
  :group 'ue
  :type '(repeat string))

(defcustom ue-globally-ignored-file-suffixes
  '(".uplugin"
    ".uproject"
    ".tiff"
    ".png"
    ".bmp"
    ".jpg"
    ".wav"
    ".mp3"
    ".fbx"
    ".3ds"
    ".psd"
    ".xcf"
    ".icns"
    ".uasset"
    ".umap")
  "A list of file suffixes globally ignored by projectile.
Note that files aren't filtered if `projectile-indexing-method'
is set to 'alien'."
  :group 'ue
  :type '(repeat string))

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

(defconst ue--uemacs-dir ".uemacs"
  "The directory name that is used to indentify Unreal Emacs project root.")

(defconst ue--project-file "project.json"
  "The name of the file that contains Unreal Emacs project metadata.")

(defconst ue--project-target-file "target"
  "The name of the file that contains current build/run target name.")

(defvar-local ue--mode-line ue-mode-line-prefix)

(defvar ue--cache-data (make-hash-table :test 'equal)
  "A hash table used for caching information about the current project.")

(defun ue--cache-key (key)
  "Generate a cache key based on the current directory and the given KEY."
  (format "%s-%s" default-directory key))

(defun ue-project-root ()
  "Return Unreal Emacs root directory if this file is part of the Unreal Emacs project else nil."
  (let* ((cache-key   (ue--cache-key "root"))
	 (cache-value (gethash cache-key ue--cache-data)))
    (or cache-value
	(ignore-errors
	  (let ((root (projectile-locate-dominating-file
		       default-directory
		       ue--uemacs-dir)))
	    (puthash cache-key root ue--cache-data)
	    root)))))

(defun ue--uemacs-dir ()
  "Return absolute path '.uemacs' directory if this is Unreal Emacs project, nil otherwise."
  (when-let ((root (ue-project-root)))
    (expand-file-name ue--uemacs-dir root)))

(defun ue--uemacs-expand-file-name (file-name)
  "Return absolute path to FILE-NAME relative to '.uemacs' directory."
  (when-let ((uemacs-dir (ue--uemacs-dir)))
    (expand-file-name file-name uemacs-dir)))

(defun ue--uemacs-project-file ()
  "Return absolute path to the 'project.json' file."
  (ue--uemacs-expand-file-name ue--project-file))

(defun ue--uemacs-project-target-file ()
  "Return absolute path to the project's current run/build target file."
  (ue--uemacs-expand-file-name ue--project-target-file))

(defun ue-project-metadata ()
  "Return project metadata alist."
  (let* ((cache-key   (ue--cache-key "project-meta"))
	 (cache-value (gethash cache-key ue--cache-data)))
    (or cache-value
	(ignore-errors
	  (when-let* ((file (ue--uemacs-project-file))
		      (json (json-read-file file)))
	    (puthash cache-key json ue--cache-data)
	    json)))))

(defun ue-project-targets ()
  "Return a list of the run/build targets for the current project."
  (let-alist (ue-project-metadata) .Project.Targets))

(defun ue-project-target-ids ()
  "Return a list of the run/build target identifiers for the current project.
A target id is $TargetName$-$Platform$-$Configuration$."
  (ue--alist-keys (ue-project-targets)))

(defun ue-project-target-id-valid-p (target-id)
  "Check if the given project run/build TARGET-ID is valid."
  (and (symbolp target-id)
       (member target-id (ue-project-target-ids))))

(defun ue--project-current-target-id-read ()
  "Return saved run/build target id as a symbol.
We cannot cache it because a user can switch to another target
in other buffers."
  (when-let ((target-file (ue--uemacs-project-target-file)))
    (when (file-exists-p target-file)
      (with-temp-buffer
	(insert-file-contents target-file)
	(when-let ((target-id (buffer-string)))
	  (intern target-id))))))

(defun ue--update-mode-line ()
  "Update ue-mode mode-line for all project buffers."
  (let* ((id              (ue--project-current-target-id-read))
	 (id              (when (and id (ue-project-target-id-valid-p id)) id))
	 (project         (projectile-acquire-root))
         (project-name    (projectile-project-name project))
         (project-buffers (projectile-project-buffers project))
	 (mode-line       (format "%s[%s]" ue-mode-line-prefix (or id "?"))))
    (dolist (buf project-buffers)
      (setf (buffer-local-value 'ue--mode-line buf) mode-line)))
  (force-mode-line-update))

(defun ue--project-current-target-id-write (id)
  "Save the given run/build target ID to the current target file."
  (write-region (symbol-name id) nil (ue--uemacs-project-target-file))
  (ue--update-mode-line)
  id)

(defun ue-select-project-target ()
  "Prompt a user to pick a default run/build target from the list."
  (interactive)
  (when-let ((targets   (mapcar #'symbol-name (ue-project-target-ids)))
	     (target-id (completing-read
			 "Run/Build Target: "
			 targets
			 nil
			 t
			 nil
			 nil
			 targets)))
    (ue--project-current-target-id-write (intern target-id))))

(defun ue-current-project-target ()
  "Return current project target if set and valid or ask user to set it."
  (let ((saved-target (ue--project-current-target-id-read)))
    (if (and saved-target
	     (ue-project-target-id-valid-p saved-target))
	saved-target
      (ue-select-project-target))))

(defun ue-project-target-get (id)
  "Return alist for the given run/build target ID."
  (alist-get id (ue-project-targets)))

(defun ue-project-target-build-command (id)
  "Return build command for the given run/build target ID."
  (let-alist (ue-project-target-get id) .Tasks.Build))

(defun ue-project-target-run-command (id)
  "Return run command for the given run/build target ID."
  (let-alist (ue-project-target-get id) .Tasks.Run))

(defun ue-project-build-command (&optional target)
  "Return build command for the given run/build TARGET id."
  (ue-project-target-build-command (or target
				       (ue-current-project-target))))

(defun ue-project-run-command (&optional target)
  "Return build command for the given run/build TARGET id."
  (ue-project-target-run-command (or target
				     (ue-current-project-target))))

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

(defun ue--setup-ignore-lists ()
  "Setup Projectile to ignore Unreal Engine specific directories and files."
  (setq projectile-globally-ignored-directories
	(append projectile-globally-ignored-directories ue-globally-ignored-directories)

	projectile-globally-ignored-files
	(append projectile-globally-ignored-files ue-globally-ignored-files)

	projectile-globally-ignored-file-suffixes
	(append projectile-globally-ignored-file-suffixes ue-globally-ignored-file-suffixes)))

(defun ue--register-keywords ()
  "Enable colouring of Unreal Engine keywords."
  (ue-font-lock-add-keywords 'c++-mode)
  (font-lock-flush)
  (setf ue--keywords-registered t))

(defun ue--unregister-keywords ()
  "Disable colouring of Unreal Engine keywords."
  (ue-font-lock-remove-keywords 'c++-mode)
  (font-lock-flush))

(defun ue--ignore-buffer-p ()
  "Return t if `ue-mode' should not be enabled for the current buffer."
  (string-match-p
   "\\*\\(Minibuf-[0-9]+\\|helm mini\\|helm projectile\\|scratch\\|Messages\\|clang*\\|lsp*\\)\\*"
   (buffer-name)))

(define-minor-mode ue-mode
  "Minor mode for Unreal Engine projects based on projectile-mode."
  :init-value nil
  :lighter    ue--mode-line
  (when ue-mode
    (ue--register-keywords)
    (ue--activate-snippets)
    (ue--setup-ignore-lists)
    (ue--update-mode-line)))

(defun ue-on ()
  "Enable 'ue-mode' minor mode if this is an Unreal Engine based project."
  (when (and (not (ue--ignore-buffer-p))
	     (projectile-project-p)
	     (ue-project-root))
    (ue-mode +1)))

(define-globalized-minor-mode ue-global-mode ue-mode ue-on)

(defun ue-off ()
  "Disable 'ue-mode' minor mode."
  (ue-mode -1))

;; Teach projectile how to recognize ue.el projects
(projectile-register-project-type 'ue           (list ue--uemacs-dir)
				  :project-file ue--uemacs-dir
				  :compile      #'ue-project-build-command
				  :run          #'ue-project-run-command)

;; Add Unreal Engine C++ snippets
(with-eval-after-load "yasnippet"
  (ue--register-snippets))

(provide 'ue)

;;; ue.el ends here
