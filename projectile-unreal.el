;;; projectile-unreal.el --- Minor mode for Unreal Engine projects based on projectile-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Oleksandr Manenko

;; Author:    Oleksandr Manenko <seidfzehsd@use.startmail.com>
;; URL:       https://gitlab.com/manenko/projectile-unreal
;; Version:   0.0.1
;; Created:   26 August 2021
;; Keywords:  unreal engine, projectile
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
;;    (projectile-unreal-global-mode)

;;; Code:

(require 'projectile)
(require 'json)
(require 'seq)

(defgroup projectile-unreal nil
  "Unreal Engine mode based on projectile"
  :prefix "projectile-unreal-"
  :group  'projectile)

(defcustom projectile-unreal-attributes
  '("UCLASS"
    "UDELEGATE"
    "UENUM"
    "UFUNCTION"
    "UINTERFACE"
    "UMETA"
    "UPARAM"
    "UPROPERTY"
    "USTRUCT")
  "List of Unreal C++ attributes."
  :group 'projectile-unreal
  :type  '(repeat string))

(defcustom projectile-unreal-class-specifiers
  '("Abstract"
    "AdvancedClassDisplay"
    "AutoCollapseCategories"
    "AutoExpandCategories"
    "Blueprintable"
    "BlueprintType"
    "ClassGroup"
    "CollapseCategories"
    "Config"
    "Const"
    "ConversionRoot"
    "CustomConstructor"
    "DefaultToInstanced"
    "DependsOn"
    "Deprecated"
    "DontAutoCollapseCategories"
    "DontCollapseCategories"
    "EditInlineNew"
    "HideCategories"
    "HideDropdown"
    "HideFunctions"
    "Intrinsic"
    "MinimalAPI"
    "NoExport"
    "NonTransient"
    "NotBlueprintable"
    "NotPlaceable"
    "PerObjectConfig"
    "Placeable"
    "ShowCategories"
    "ShowFunctions"
    "Transient"
    "Within"
    "meta")
  "List of Unreal C++ UCLASS specifiers."
  :group 'projectile-unreal
  :type  '(repeat string))

(defcustom projectile-unreal-class-metadata-specifiers
  '("BlueprintSpawnableComponent"
    "BlueprintThreadSafe"
    "ChildCannotTick"
    "ChildCanTick"
    "DeprecatedNode"
    "DeprecationMessage"
    "DisplayName"
    "DontUseGenericSpawnObject"
    "ExposedAsyncProxy"
    "IgnoreCategoryKeywordsInSubclasses"
    "IsBlueprintBase"
    "KismetHideOverrides"
    "ProhibitedInterfaces"
    "RestrictedToClasses"
    "ShortToolTip"
    "ShowWorldContextPin"
    "UsesHierarchy"
    "ToolTip")
  "List of Unreal C++ UCLASS metadata specifiers."
  :group 'projectile-unreal
  :type  '(repeat string))

(defcustom projectile-unreal-enum-specifiers
  '("meta")
  "List of Unreal C++ UENUM specifiers."
  :group 'projectile-unreal
  :type  '(repeat string))

(defcustom projectile-unreal-enum-metadata-specifiers
  '("Bitflags"
    "Experimental"
    "ScriptName"
    "ToolTip")
  "List of Unreal C++ UENUM metadata specifiers."
  :group 'projectile-unreal
  :type  '(repeat string))

(defcustom projectile-unreal-enum-enumerator-specifiers
  '("DisplayName"
    "Hidden"
    "ToolTip")
  "List of Unreal C++ UMETA specifiers used for UENUM enumerators."
  :group 'projectile-unreal
  :type  '(repeat string))

(defcustom projectile-unreal-function-specifiers
  '("BlueprintAuthorityOnly"
    "BlueprintCallable"
    "BlueprintCosmetic"
    "BlueprintImplementableEvent"
    "BlueprintNativeEvent"
    "BlueprintPure"
    "CallInEditor"
    "Category"
    "Client"
    "CustomThunk"
    "Exec"
    "NetMulticast"
    "Reliable"
    "SealedEvent"
    "ServiceRequest"
    "ServiceResponse"
    "Server"
    "Unreliable"
    "WithValidation"
    "meta")
  "List of Unreal C++ UFUNCTION specifiers."
  :group 'projectile-unreal
  :type  '(repeat string))

(defcustom projectile-unreal-function-metadata-specifiers
  '("AdvancedDisplay"
    "ArrayParm"
    "ArrayTypeDependentParams"
    "AutoCreateRefTerm"
    "BlueprintAutocast"
    "BlueprintInternalUseOnly"
    "BlueprintProtected"
    "CallableWithoutWorldContext"
    "CommutativeAssociativeBinaryOperator"
    "CompactNodeTitle"
    "CustomStructureParam"
    "DefaultToSelf"
    "DeprecatedFunction"
    "DeprecationMessage"
    "DeterminesOutputType"
    "DevelopmentOnly"
    "DisplayName"
    "ExpandEnumAsExecs"
    "HidePin"
    "HideSelfPin"
    "InternalUseParam"
    "Keywords"
    "Latent"
    "LatentInfo"
    "MaterialParameterCollectionFunction"
    "NativeBreakFunc"
    "NotBlueprintThreadSafe"
    "ShortToolTip"
    "ToolTip"
    "UnsafeDuringActorConstruction"
    "WorldContext")
  "List of Unreal C++ UFUNCTION metadata specifiers."
  :group 'projectile-unreal
  :type  '(repeat string))

(defcustom projectile-unreal-delegate-specifiers
  '("BlueprintAuthorityOnly"
    "BlueprintCallable"
    "BlueprintCosmetic"
    "BlueprintImplementableEvent"
    "BlueprintNativeEvent"
    "BlueprintPure"
    "CallInEditor"
    "Category"
    "Client"
    "CustomThunk"
    "Exec"
    "NetMulticast"
    "Reliable"
    "SealedEvent"
    "ServiceRequest"
    "ServiceResponse"
    "Server"
    "Unreliable"
    "WithValidation"
    "meta")
  "List of Unreal C++ UDELEGATE specifiers."
  :group 'projectile-unreal
  :type  '(repeat string))

(defcustom projectile-unreal-delegate-metadata-specifiers
  '("AdvancedDisplay"
    "ArrayParm"
    "ArrayTypeDependentParams"
    "AutoCreateRefTerm"
    "BlueprintAutocast"
    "BlueprintInternalUseOnly"
    "BlueprintProtected"
    "CallableWithoutWorldContext"
    "CommutativeAssociativeBinaryOperator"
    "CompactNodeTitle"
    "CustomStructureParam"
    "DefaultToSelf"
    "DeprecatedFunction"
    "DeprecationMessage"
    "DeterminesOutputType"
    "DevelopmentOnly"
    "DisplayName"
    "ExpandEnumAsExecs"
    "HidePin"
    "HideSelfPin"
    "InternalUseParam"
    "Keywords"
    "Latent"
    "LatentInfo"
    "MaterialParameterCollectionFunction"
    "NativeBreakFunc"
    "NotBlueprintThreadSafe"
    "ShortToolTip"
    "ToolTip"
    "UnsafeDuringActorConstruction"
    "WorldContext")
  "List of Unreal C++ UDELEGATE metadata specifiers."
  :group 'projectile-unreal
  :type  '(repeat string))

(defcustom projectile-unreal-uparam-specifiers
  '("DisplayName"
    "ref")
  "List of Unreal C++ UPARAM specifiers."
  :group 'projectile-unreal
  :type  '(repeat string))

(defcustom projectile-unreal-interface-specifiers
  '("BlueprintType"
    "DependsOn"
    "MinimalAPI"
    "meta")
  "List of Unreal C++ UINTERFACE specifiers."
  :group 'projectile-unreal
  :type  '(repeat string))

(defcustom projectile-unreal-interface-metadata-specifiers
  '("CannotImplementInterfaceInBlueprint")
  "List of Unreal C++ UINTERFACE metadata specifiers."
  :group 'projectile-unreal
  :type  '(repeat string))

(defcustom projectile-unreal-property-specifiers
  '("AdvancedDisplay"
    "AssetRegistrySearchable"
    "BlueprintAssignable"
    "BlueprintAuthorityOnly"
    "BlueprintCallable"
    "BlueprintGetter"
    "BlueprintReadOnly"
    "BlueprintReadWrite"
    "BlueprintSetter"
    "Category"
    "Config"
    "DuplicateTransient"
    "EditAnywhere"
    "EditDefaultsOnly"
    "EditFixedSize"
    "EditInline"
    "EditInstanceOnly"
    "Export"
    "GlobalConfig"
    "Instanced"
    "Interp"
    "Localized"
    "Native"
    "NoClear"
    "NoExport"
    "NonPIEDuplicateTransient"
    "NonTransactional"
    "NotReplicated"
    "Replicated"
    "ReplicatedUsing"
    "RepRetry"
    "SaveGame"
    "SerializeText"
    "SkipSerialization"
    "SimpleDisplay"
    "TextExportTransient"
    "Transient"
    "VisibleAnywhere"
    "VisibleDefaultsOnly"
    "VisibleInstanceOnly"
    "meta")
  "List of Unreal C++ UPROPERTY specifiers."
  :group 'projectile-unreal
  :type  '(repeat string))

(defcustom projectile-unreal-property-metadata-specifiers
  '("AllowAbstract"
    "AllowedClasses"
    "AllowPreserveRatio"
    "ArrayClamp"
    "AssetBundles"
    "BlueprintBaseOnly"
    "BlueprintCompilerGeneratedDefaults"
    "ClampMin"
    "ClampMax"
    "ConfigHierarchyEditable"
    "ContentDir"
    "DisplayAfter"
    "DisplayName"
    "DisplayPriority"
    "DisplayThumbnail"
    "EditCondition"
    "EditFixedOrder"
    "ExactClass"
    "ExposeFunctionCategories"
    "ExposeOnSpawn"
    "FilePathFilter"
    "GetByRef"
    "HideAlphaChannel"
    "HideViewOptions"
    "InlineEditConditionToggle"
    "LongPackageName"
    "MakeEditWidget"
    "NoGetter")
  "List of Unreal C++ UPROPERTY metadata specifiers."
  :group 'projectile-unreal
  :type  '(repeat string))

(defcustom projectile-unreal-struct-specifiers
  '("Atomic"
    "BlueprintType"
    "Immutable"
    "NoExport"
    "meta")
  "List of Unreal C++ USTRUCT specifiers."
  :group 'projectile-unreal
  :type  '(repeat string))

(defcustom projectile-unreal-struct-metadata-specifiers
  '("BlueprintSpawnableComponent"
    "BlueprintThreadSafe"
    "ChildCannotTick"
    "ChildCanTick"
    "DeprecatedNode"
    "DeprecationMessage"
    "DisplayName"
    "DontUseGenericSpawnObject"
    "ExposedAsyncProxy"
    "IgnoreCategoryKeywordsInSubclasses"
    "IsBlueprintBase"
    "KismetHideOverrides"
    "ProhibitedInterfaces"
    "ShortToolTip"
    "ShowWorldContextPin"
    "UsesHierarchy"
    "ToolTip")
  "List of Unreal C++ USTRUCT metadata specifiers."
  :group 'projectile-unreal
  :type  '(repeat string))

(defcustom projectile-unreal-generated-body-macro
  '("GENERATED_BODY"
    "GENERATED_IINTERFACE_BODY"
    "GENERATED_UCLASS_BODY"
    "GENERATED_UINTERFACE_BODY"
    "GENERATED_USTRUCT_BODY")
  "List of Unreal C++ GENERATED_*_BODY macros."
  :group 'projectile-unreal
  :type  '(repeat string))

(defcustom projectile-unreal-epic-games-root-dir
  nil
  "A root directory that contains Unreal Engine installations.

The 'projectile-unreal' will use the directory to construct path to the Unreal Engine of the project specific version.

If the value of this variable is nil, the 'projectile-unreal' will try to use a default location, depending on your platform:
- macOS:     /Users/Shared/Epic Games
- GNU/Linux: TBD
- windows:   TBD"
  :group 'projectile-unreal
  :type  'string)

;; TODO: *A*ctors
;;       *I*nterfaces
;;       *U*Objects
;;       *F*Classes

(defcustom projectile-unreal-project-file-extension "uproject"
  "Unreal Engine project file extension."
  :group 'projectile-unreal
  :type  'string)

(defvar projectile-unreal-cache-data
  (make-hash-table :test 'equal)
  "A hash table used for caching information about the current project.")

(defun projectile-unreal-cache-key (key)
  "Generate a cache key based on the current directory and the given KEY."
  (format "%s-%s" default-directory key))

(defun projectile-unreal-project-file-wildcard ()
  "Return wildcard pattern used to match Unreal Engine project files."
  (concat "?*." projectile-unreal-project-file-extension))

(defun projectile-unreal-locate-project-file (&optional dir)
  "Return Unreal Engine project file name in the given directory DIR, return nil if there is none or more than one."
  (let ((files (file-expand-wildcards
		 (expand-file-name (projectile-unreal-project-file-wildcard)
				   dir))))
    (when (= 1 (length files))
      (nth 0 files))))

(defun projectile-unreal-project-p (&optional dir)
  "Check if the given directory DIR contains an Unreal Engine project file."
  (projectile-unreal-locate-project-file dir))

(defun projectile-unreal-root ()
  "Return Unreal Engine based project root directory if this file is part of the project, else nil."
  (let* ((cache-key   (projectile-unreal-cache-key "root"))
	 (cache-value (gethash cache-key projectile-unreal-cache-data)))
    (or cache-value
	(ignore-errors
	  (let ((root (projectile-locate-dominating-file
		       default-directory
		       #'projectile-unreal-project-p)))
	    (when root
	      (puthash cache-key root projectile-unreal-cache-data)
	      root))))))

(defun projectile-unreal-non-source-directories ()
  (seq-filter
   (lambda (dir)
     (and (file-directory-p (expand-file-name dir (projectile-project-root)))
	  (not (string= "Source" dir))))
   (directory-files (projectile-unreal-root))))

(defun projectile-unreal-project ()
  "Return Unreal Engine project file name."
  (let* ((cache-key   (projectile-unreal-cache-key "project-file"))
	 (cache-value (gethash cache-key projectile-unreal-cache-data)))
    (or cache-value
	(ignore-errors
	  (when (projectile-unreal-root)
	    (let ((project-file (projectile-unreal-locate-project-file (projectile-unreal-root))))
	      (when project-file
		(puthash cache-key project-file projectile-unreal-cache-data)
		project-file)))))))

(defun projectile-unreal-parse-project (&optional file)
  "Return alist of the Unreal Engine project FILE."
  (when-let* ((project-file (or file (projectile-unreal-project))))
    (let ((json (json-read-file project-file)))
      `((file-version     . ,(alist-get 'FileVersion       json))
	(engine-version   . ,(alist-get 'EngineAssociation json))
	(target-platforms . ,(alist-get 'TargetPlatforms   json))
	(modules          . ,(mapcar
			      (lambda (module)
				`((name . ,(alist-get 'Name module))
				  (type . ,(alist-get 'Type module))
				  (deps . ,(alist-get 'AdditionalDependencies module))))
			      (alist-get 'Modules json)))))))

(defun projectile-unreal-project-name ()
  "Get the name of the current Unreal Engine project"
  (when-let* ((project (projectile-unreal-project)))
    (file-name-base project)))

(defun projectile-unreal-epic-games-root-dir ()
  "Return the name of directory that contains various versions of Unreal Engine."
  (or projectile-unreal-epic-games-root-dir)
  (pcase system-type
    ('darwin "/Users/Shared/Epic Games")
    (type (error "Please set Epic Games root directory using projectile-unreal-ue-installation-root-dir"))))

(defun projectile-unreal-engine-root (version)
  "Get root directory of the Unreal Engine of the given VERSION."
  (expand-file-name (concat "UE_" version)
		    (projectile-unreal-epic-games-root-dir)))

(defun projectile-unreal-engine-build-script (engine-version)
  "Get path to the Unreal Engine build script specific to the current platform."
  (let ((engine-root (projectile-unreal-engine-root engine-version)))
    (pcase system-type
      ('darwin (expand-file-name "Engine/Build/BatchFiles/Mac/Build.sh" engine-root))
      (type (error "To be implemented")))))

(defun projectile-unreal-project-compilation-cmd ()
  "Get the compilation command for the current Unreal Engine project."
  ;; TODO: Extend to GNU/Linux and windows
  (let* ((project (projectile-unreal-parse-project))
	 (engine-version (alist-get 'engine-version project)))
    (concat "cd "
	    (shell-quote-argument (projectile-unreal-engine-root engine-version))
	    " && bash "
	    (shell-quote-argument (projectile-unreal-engine-build-script engine-version))
	    " "
	    (projectile-unreal-project-name)
	    "Editor"
	    " Mac"
	    " Development"
	    " -project="
	    (shell-quote-argument (projectile-unreal-project))
	    " -game"
	    " -progress"
	    " -buildscw")))

(defun projectile-unreal-project-configure-cmd ()
  ;; TODO: Extend to GNU/Linux and windows
  (let* ((project (projectile-unreal-parse-project))
	 (engine-version (alist-get 'engine-version project)))
    (concat "cd "
	    (shell-quote-argument (projectile-unreal-engine-root engine-version))
	    " && bash "
	    (shell-quote-argument (projectile-unreal-engine-build-script engine-version))
	    " -project="
	    (shell-quote-argument (projectile-unreal-project))
	    " -ProjectFiles"
	    " -ProjectFileFormat"
	    " -CMakefile"
	    " && cd "
	    (shell-quote-argument (projectile-unreal-root))
	    " && mkdir -p .projectile-unreal-build" ;; TODO: make configurable
	    " && cd .projectile-unreal-build"
	    " && cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=YES -DCMAKE_BUILD_TYPE=Debug" ;; TODO: make cmake command configurable
	    " && cd .."
	    " && ln -fs .projectile-unreal-build/compile_commands.json .")))

;; TODO: Set current target and save it? Then use that for a compile command.

;; Need a path to unreal engine root directory, then the version could be used to construct the path

;; Configure command will generate cmake and compile-commands json

;; Add everything except Source to the ignore list automatically, i.e. +/Source
;; The file is in (projectile-dirconfig-file)

;; TODO: Recommend to switch from alien mode
;; Or. Maybe. Set project root to the Source dir?

(defun projectile-unreal-mode-setup ()
  "Configure 'projectile-unreal-mode'."
  (setq projectile-globally-ignored-directories
	(projectile-unreal-non-source-directories)

	projectile-project-name
	(projectile-unreal-project-name)

	projectile-project-compilation-cmd
	(projectile-unreal-project-compilation-cmd)

	projectile-project-configure-cmd
	(projectile-unreal-project-configure-cmd)))

;;;###autoload
(define-minor-mode projectile-unreal-mode
  "Unreal Engine mode based on projectile."
  :init-value nil
  :lighter    " UE"
  (when projectile-unreal-mode
    (projectile-unreal-mode-setup)))

;;;###autoload
(defun projectile-unreal-on ()
  "Enable 'projectile-unreal-mode' minor mode if this is an Unreal Engine based project."
  (when (and (projectile-unreal-project-p)
	     (projectile-unreal-root))
    (projectile-unreal-mode +1)))

;;;###autoload
(define-globalized-minor-mode projectile-unreal-global-mode
  projectile-unreal-mode
  projectile-unreal-on)

(defun projectile-unreal-off ()
  "Disable 'projectile-unreal-mode' minor mode."
  (projectile-unreal-mode -1))

(provide 'projectile-unreal)

;;; projectile-unreal.el ends here
