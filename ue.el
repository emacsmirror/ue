;;; ue.el --- Minor mode for Unreal Engine projects -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Oleksandr Manenko

;; Author:    Oleksandr Manenko <seidfzehsd@use.startmail.com>
;; URL:       https://gitlab.com/unrealemacs/ue.el
;; Version:   1.0.2
;; Created:   26 August 2021
;; Keywords:  unreal engine, languages, tools
;; Package-Requires: ((emacs "26.1") (projectile "2.5.0"))

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

(defvar yas-snippet-dirs)

(declare-function yas-load-directory      "ext:yasnippet")
(declare-function yas-activate-extra-mode "ext:yasnippet")

;; TODO: Class wizards
;; TODO: Debugging (lsp?)
;; TODO: .NET projects?
;; TODO: Project.Target.cs files
;; TODO: Config files (*.ini)

;; Functions used to sort Unreal C++ keywords by length which is used in font locking
(eval-and-compile
  (defun ue--c++-string-length< (a b) (< (length a) (length b)))
  (defun ue--c++-string-length> (a b) (not (ue--c++-string-length< a b))))

(defun ue--alist-keys (alist)
  "Return keys of the given ALIST."
  (mapcar #'car alist))

(defgroup ue nil
  "A minor mode for Unreal Engine projects."
  :prefix "ue-"
  :group  'projectile)

(defcustom ue-expand-snippets t
  "Enable Unreal Engine C++ `yasnippet' snippets."
  :group 'ue
  :type  'boolean)

(defcustom ue-mode-line-prefix " ue"
  "Mode line lighter prefix for command `ue-mode'."
  :group 'ue
  :type  'string)

(defcustom ue-globally-ignored-files
  '("compile_commands.json")
  "A list of files globally ignored by command `ue-mode'.

Note that files aren't filtered if `projectile-indexing-method' is set to `alien'."
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
  "A list of directories globally ignored by command `ue-mode'.

Regular expressions can be used.

Strings that don't start with * are only ignored at the top level
of the project.

Strings that start with `*' are ignored everywhere in the
project, as if there was no `*'.

When `*' used as a prefix is not a wildcard; it is an indicator
that the directory should be ignored at all levels, not just
root.

Note that files aren't filtered if `projectile-indexing-method'
is set to `alien'."
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
  "A list of file suffixes globally ignored by command `ue-mode'.

Note that files aren't filtered if `projectile-indexing-method'
is set to `alien'."
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
  "List of Unreal C++ `GENERATED_*_BODY' macros."
  :type  '(choice (const :tag "Disabled" nil)
		  (repeat string))
  :group 'ue)

(defcustom ue-attribute-face 'font-lock-preprocessor-face
  "Face for displaying Unreal attributes (`UCLASS', `UFUNCTION' and the like)."
  :type  'symbol
  :group 'ue)

(defcustom ue-generated-body-macro-face 'font-lock-preprocessor-face
  "Face for displaying Unreal `GENERATED_*_BODY' macros."
  :type  'symbol
  :group 'ue)

(defcustom ue-keymap-prefix nil
  "Keymap prefix for command `ue-mode'."
  :type  'string
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
  "The name of the file that contains current build target id.")

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
  "Return absolute path to `.uemacs' directory if this is Unreal Emacs project, nil otherwise."
  (when-let ((root (ue-project-root)))
    (expand-file-name ue--uemacs-dir root)))

(defun ue--uemacs-expand-file-name (file-name)
  "Return absolute path to FILE-NAME relative to `.uemacs' directory."
  (when-let ((uemacs-dir (ue--uemacs-dir)))
    (expand-file-name file-name uemacs-dir)))

(defun ue--uemacs-project-file ()
  "Return absolute path to the `project.json' file."
  (ue--uemacs-expand-file-name ue--project-file))

(defun ue--uemacs-project-target-file ()
  "Return absolute path to a file that store the project's current build target id."
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
  "Return a list of the build targets for the current project."
  (let-alist (ue-project-metadata) .Project.Targets))

(defun ue-project-target-ids ()
  "Return a list of the build target identifiers for the current project.

A target id has the following format: `$TargetName$-$Platform$-$Configuration$'."
  (ue--alist-keys (ue-project-targets)))

(defun ue-project-target-id-valid-p (id)
  "Check if the given project build target ID is valid."
  (and (symbolp id)
       (member  id (ue-project-target-ids))))

(defun ue--project-current-target-id-read ()
  "Return previously saved build target id as a symbol.

We cannot cache it because a user can switch to another target
in other buffers."
  (when-let ((target-file (ue--uemacs-project-target-file)))
    (when (file-exists-p target-file)
      (with-temp-buffer
	(insert-file-contents target-file)
	(when-let ((target-id (buffer-string)))
	  (intern target-id))))))

(defun ue--update-mode-line ()
  "Update ue-mode's mode-line for all project buffers."
  (let* ((id              (ue--project-current-target-id-read))
	 (id              (when (and id (ue-project-target-id-valid-p id)) id))
	 (project         (projectile-acquire-root))
         (project-buffers (projectile-project-buffers project))
	 (mode-line       (format "%s[%s]" ue-mode-line-prefix (or id "?"))))
    (dolist (buf project-buffers)
      (setf (buffer-local-value 'ue--mode-line buf) mode-line)))
  (force-mode-line-update))

(defun ue--project-current-target-id-write (id)
  "Save the given build target ID to the file."
  (write-region (symbol-name id) nil (ue--uemacs-project-target-file))
  (ue--update-mode-line)
  id)

(defun ue--select-project-build-target ()
  "Prompt a user to pick a default build target id from the list."
  (when-let ((targets   (mapcar #'symbol-name (ue-project-target-ids)))
	     (target-id (completing-read
			 "Build target: "
			 targets
			 nil
			 t
			 nil
			 nil
			 targets)))
    (ue--project-current-target-id-write (intern target-id))
    ;; Need to clear projectile command caches
    ;; otherwise it will use the old commands.
    (clrhash projectile-compilation-cmd-map)
    (clrhash projectile-run-cmd-map)))

(defun ue-current-project-build-target ()
  "Return the project's build target if set and valid or prompts user to set it."
  (let ((saved-target (ue--project-current-target-id-read)))
    (if (and saved-target
	     (ue-project-target-id-valid-p saved-target))
	saved-target
      (ue--select-project-build-target))))

(defun ue-project-target-get (id)
  "Return alist for the given build target ID."
  (alist-get id (ue-project-targets)))

(defun ue-project-target-build-command (id)
  "Return build command for the given build target ID."
  (let-alist (ue-project-target-get id) .Tasks.Build))

(defun ue-project-target-run-command (id)
  "Return run command for the given build target ID."
  (let-alist (ue-project-target-get id) .Tasks.Run))

(defun ue-project-target-uht-command (id)
  "Return command that run UnrealHeaderTool aka UHT for the given build target ID."
  (when-let ((build-command (ue-project-target-build-command id)))
    (concat build-command " -SkipBuild")))

(defun ue-project-build-command (&optional id)
  "Return build command for the given build target ID.

Return current target if ID is falsy."
  (ue-project-target-build-command
   (or id
       (ue-current-project-build-target))))

(defun ue-project-run-command (&optional id)
  "Return build command for the given build target ID."
  (ue-project-target-run-command
   (or id
       (ue-current-project-build-target))))

(defun ue-project-uht-command (&optional id)
  "Return command that run UnrealHeaderTool aka UHT for the given build target ID."
  (ue-project-target-uht-command
   (or id
       (ue-current-project-build-target))))

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

(defvar ue--snippets-installed nil
  "A flag that indicates whether UE snippets are installed.")

(defun ue--register-snippets ()
  "Add Unreal Engine C++ snippets to yasnippet if it is available."
  (when (and ue-expand-snippets
	     (not ue--snippets-installed)
	     (require 'yasnippet nil t))
    (add-to-list 'yas-snippet-dirs 'ue-snippets-dir t)
    (yas-load-directory ue-snippets-dir t)
    (setq ue--snippets-installed t)))

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
  (font-lock-flush))

(defun ue--unregister-keywords ()
  "Disable colouring of Unreal Engine keywords."
  (ue-font-lock-remove-keywords 'c++-mode)
  (font-lock-flush))

(defun ue--ignore-buffer-p ()
  "Return t if command `ue-mode' should not be enabled for the current buffer."
  (string-match-p
   "\\*\\(Minibuf-[0-9]+\\|helm mini\\|helm projectile\\|scratch\\|Messages\\|clang*\\|lsp*\\)\\*"
   (buffer-name)))

(defun ue-jump-between-header-and-implementation ()
  "Jump between header and source files in the project."
  (interactive)
  (projectile-find-other-file))

(defun ue-switch-to-buffer ()
  "Display a list of all project buffers currently open."
  (interactive)
  (projectile-switch-to-buffer))

(defun ue-find-dir (&optional invalidate-cache)
  "Display a list of all directories in the project.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile-find-dir invalidate-cache))

(defun ue-dired ()
  "Open `dired' at the root of the project."
  (interactive)
  (projectile-dired))

(defun ue-recentf ()
  "Show a list of recently visited files in the project."
  (interactive)
  (projectile-recentf))

(defun ue-edit-dir-locals ()
  "Edit or create a .dir-locals.el file of the project."
  (interactive)
  (projectile-edit-dir-locals))

(defun ue-find-file (&optional invalidate-cache)
  "Jump to the project's file using completion.

With a prefix arg INVALIDATE-CACHE invalidates the cache first."
  (interactive "P")
  (projectile-find-file invalidate-cache))

(defun ue-find-file-dwim (&optional invalidate-cache)
  "Jump to a project's files using completion based on context.

With a prefix arg INVALIDATE-CACHE invalidates the cache first.

If point is on a filename, `ue.el' first tries to search for that
file in project:

- If it finds just a file, it switches to that file instantly.
This works even if the filename is incomplete, but there's only a
single file in the current project that matches the filename at
point.

- If it finds a list of files, the list is displayed for
selecting.  A list of files is displayed when a filename appears
more than one in the project or the filename at point is a prefix
of more than two files in the project.

- If it finds nothing, the list of all files in the project is
displayed for selecting."
  (interactive "P")
  (projectile-find-file-dwim invalidate-cache))

(defun ue-invalidate-cache ()
  "Remove the current project's files from the cache."
  (interactive)
  (projectile-invalidate-cache nil))

(defun ue-find-file-in-directory (&optional directory)
  "Jump to a file in a (maybe regular) DIRECTORY.

This command will first prompt for the directory the file is in."
  (interactive "DFind file in directory: ")
  (projectile-find-file-in-directory directory))

(defun ue-multi-occur (&optional nlines)
  "Do a `multi-occur' in the project's buffers.

With a prefix argument, show NLINES of context."
  (interactive "P")
  (projectile-multi-occur nlines))

(defun ue-grep (&optional regexp arg)
  "Perform `rgrep' in the project.

With a prefix ARG asks for files (globbing-aware) which to grep in.
With prefix ARG of `-' (such as `M--'), default the files (without prompt),
to `projectile-grep-default-files'.

With REGEXP given, don't query the user for a regexp."
  (interactive "i\nP")
  (projectile-grep regexp arg))

(defun ue-ripgrep (search-term &optional arg)
  "Run a `ripgrep' (`rg') search with SEARCH-TERM at current project root.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression.

This command depends on of the Emacs packages `ripgrep' or `rg' being
installed to work."
  (interactive
   (list (projectile--read-search-string-with-default
          (format "Ripgrep %ssearch for" (if current-prefix-arg "regexp " "")))
         current-prefix-arg))
  (projectile-ripgrep search-term arg))

(defun ue-ag (search-term &optional arg)
  "Run an `ag' search with SEARCH-TERM in the project.

With an optional prefix argument ARG SEARCH-TERM is interpreted as a
regular expression."
  (interactive
   (list (projectile--read-search-string-with-default
          (format "Ag %ssearch for" (if current-prefix-arg "regexp " "")))
         current-prefix-arg))
  (projectile-ag search-term arg))

(defun ue-save-project-buffers ()
  "Save all project buffers."
  (interactive)
  (projectile-save-project-buffers))

(defun ue-switch-build-target ()
  "Prompt a user for a build target to switch to and switch to it if the user selected any."
  (interactive)
  (ue--select-project-build-target))

(defun ue-version-control-status ()
  "Open version control status window at the root of the project.

For git projects `magit-status-internal' is used if available."
  (interactive)
  (projectile-vc nil))

(defun ue-compile-project ()
  "Compile project for the current build target.

If there is no target set, prompt user to choose it and then compile."
  (interactive)
  (let ((compilation-read-command nil))
    (projectile-compile-project nil)))

(defun ue-run-project ()
  "Run project for the current build target.

If there is no target set, prompt user to choose it and then run."
  (interactive)
  (let ((compilation-read-command nil))
    (projectile-run-project nil)))

(defun ue-uht-project ()
    "Run UnrealHeaderTool aka UHT on the project for the current build target.

If there is no target set, prompt user to choose it and then run UHT."
    (interactive)
    (let ((compilation-read-command nil)
	  (uht-command              (ue-project-uht-command)))
      (ue-save-project-buffers)
      (compile uht-command projectile-run-use-comint-mode)))

(defun ue-previous-project-buffer ()
  "In selected window switch to the previous project buffer.

If the current buffer does not belong to a project, call `previous-buffer'."
  (interactive)
  (projectile-previous-project-buffer))

(defun ue-next-project-buffer ()
  "In selected window switch to the next project buffer.

If the current buffer does not belong to a project, call `next-buffer'."
  (interactive)
  (projectile-next-project-buffer))

(defvar ue-command-map
  (let ((map (make-sparse-keymap)))
    ;; Switch between files with the same name but different extensions.
    ;; Use this to switch between header and source files.
    (define-key map (kbd "a") #'ue-jump-between-header-and-implementation)
    ;; Display a list of all project buffers currently open.
    (define-key map (kbd "b") #'ue-switch-to-buffer)
    ;; Compile the project for current build target.
    ;; If there is no target set, prompt a user to select one
    ;; and then compile the project.
    (define-key map (kbd "c") #'ue-compile-project)
    ;; Display a list of all directories in the project.
    ;; With a prefix argument it will clear the cache first.
    (define-key map (kbd "d") #'ue-find-dir)
    ;; Open the root of the project in dired.
    (define-key map (kbd "D") #'ue-dired)
    ;; Show a list of recently visited project files.
    (define-key map (kbd "e") #'ue-recentf)
    ;; Open the root dir-locals-file of the project.
    (define-key map (kbd "E") #'ue-edit-dir-locals)
    ;; Display a list of all files in the project.
    ;; With a prefix argument it will clear the cache first.
    (define-key map (kbd "f") #'ue-find-file)
    ;; Jump to a project's files using completion based on context.
    ;; With a prefix argument invalidates the cache first.
    (define-key map (kbd "g") #'ue-find-file-dwim)
    ;; Invalidate the project cache (if existing).
    (define-key map (kbd "i") #'ue-invalidate-cache)
    ;; Display a list of all files in a directory (that’s not necessarily a project).
    (define-key map (kbd "l") #'ue-find-file-in-directory)
    ;; Run `multi-occur' on all project buffers currently open.
    (define-key map (kbd "o") #'ue-multi-occur)
    ;; Run `UnrealHeaderTool' for the project to generate header and source files.
    (define-key map (kbd "R") #'ue-uht-project)
    ;; Run grep on the files in the project.
    (define-key map (kbd "s g") #'ue-grep)
    ;; Run `ripgrep' on the project, performing a literal search.
    ;; Requires the presence of `rg.el'.
    ;; With a prefix argument it will perform a regex search.
    (define-key map (kbd "s r") #'ue-ripgrep)
    ;; Run `ag' on the project, performing a literal search.
    ;; Requires the presence of `ag.el'.
    ;; With a prefix argument it will perform a regex search.
    (define-key map (kbd "s s") #'ue-ag)
    ;; Save all project buffers.
    (define-key map (kbd "S") #'ue-save-project-buffers)
    ;; Select a new build target for the current project.
    ;; This affects run and compile commands.
    (define-key map (kbd "t") #'ue-switch-build-target)
    ;; Run the project using the current build target.
    ;; If there is no target set, prompt a user to select one
    ;; and then run the project.
    (define-key map (kbd "u") #'ue-run-project)
    ;; Open version control status window at the root of the project.
    ;; For git projects `magit-status-internal' is used if available.
    (define-key map (kbd "v") #'ue-version-control-status)
    ;; Switch to the previous project buffer.
    (define-key map (kbd "<left>") #'ue-previous-project-buffer)
    ;; Switch to the next project buffer.
    (define-key map (kbd "<right>") #'ue-next-project-buffer)
    map)
  "Keymap after `ue-keymap-prefix'.")
(fset 'ue-command-map ue-command-map)

(defvar ue-mode-map
  (let ((map (make-sparse-keymap)))
    (when ue-keymap-prefix
      (define-key map ue-keymap-prefix 'ue-command-map))
    (easy-menu-define ue-mode-menu map
      "Menu for ue-mode"
      '("UE"
	["Find file"                      ue-find-file]
	["Find directory"                 ue-find-dir]
        ["Find file in directory"         ue-find-file-in-directory]
	"--"
        ["Jump between header and source" ue-jump-between-header-and-implementation]
	["Previous buffer"                ue-previous-project-buffer]
        ["Next buffer"                    ue-next-project-buffer]
	["Save project buffers"           ue-save-project-buffers]
	"--"
	["Switch build target"            ue-switch-build-target]
	["Invalidate cache"               ue-invalidate-cache]
	["Run UnrealHeaderTool"           ue-uht-project]
	"--"
        ["Search in project using grep"   ue-grep]
        ["Search in project using ag"     ue-ag]
        ["Replace in project"             ue-replace]
        ["Multi-occur in project"         ue-multi-occur]
	"--"
	["Compile project"                ue-compile-project]
	["Run project"                    ue-run-project]))
    map)
  "Keymap for command `ue-mode'.")

(define-minor-mode ue-mode
  "Minor mode for Unreal Engine projects based on `projectile-mode'.

\\{ue-mode-map}"
  :init-value nil
  :lighter    ue--mode-line
  :keymap     ue-mode-map
  (when ue-mode
    (ue--register-keywords)
    (ue--register-snippets)
    (ue--activate-snippets)
    (ue--setup-ignore-lists)
    (ue--update-mode-line)))

(defun ue-on ()
  "Enable command `ue-mode' if this is an Unreal Engine based project."
  (when (and (not (ue--ignore-buffer-p))
	     (projectile-project-p)
	     (ue-project-root))
    (ue-mode +1)))

;;;###autoload
(define-globalized-minor-mode ue-global-mode ue-mode ue-on)

(defun ue-off ()
  "Disable command `ue-mode'."
  (ue-mode -1))

;; Teach projectile how to recognize `ue.el' projects
(projectile-register-project-type 'ue           (list ue--uemacs-dir)
				  :project-file ue--uemacs-dir
				  :compile      #'ue-project-build-command
				  :run          #'ue-project-run-command)

(provide 'ue)

;;; ue.el ends here
