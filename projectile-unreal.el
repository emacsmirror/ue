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
;;
;; TODO: Set current target and save it? Then use that for a compile command.
;; TODO: Recommend to switch from alien mode. Or set project root to the Source dir?
;; TODO: Recommend installing ag Emacs package
;; TODO: Run the editor. We have -game and -server switches
;; https://docs.unrealengine.com/4.26/en-US/ProductionPipelines/CommandLineArguments/
;; TODO: Build configuration
;; https://docs.unrealengine.com/4.26/en-US/ProductionPipelines/DevelopmentSetup/BuildConfigurations/
;; TODO: Class wizards

;;; Code:

(require 'projectile)
(require 'json)
(require 'seq)

(defgroup projectile-unreal nil
  "Unreal Engine mode based on projectile."
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

(defcustom projectile-unreal-epic-games-dir
  nil
  "The directory where Unreal Engine(s) are installed.

The 'projectile-unreal-mode' uses this variable to construct
the path to the Unreal Engine of the project specific version.

If this variable is nil, the 'projectile-unreal-mode' uses
the current platform's 'default' location:

platform        default location
--------        ----------------
macOS           /Users/Shared/Epic Games
GNU/Linux       /opt/Epic Games
Windows         C:/Program Files/Epic Games"
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
  "Return Unreal Engine project file name in the given directory DIR,
return nil if there is none or more than one."
  (let ((files (file-expand-wildcards
		 (expand-file-name (projectile-unreal-project-file-wildcard)
				   dir))))
    (when (= 1 (length files))
      (nth 0 files))))

(defun projectile-unreal-project-p (&optional dir)
  "Check if the given directory DIR contains an Unreal Engine project file."
  (projectile-unreal-locate-project-file dir))

(defun projectile-unreal-root ()
  "Return an absolute path to the project root directory if this file is part
of the project, else nil."
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
  "Return all project directories except 'Source/'.

The function is used during the mode activation to add non-source directories
to the projectile ignore list."
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
  "Return alist of project properties parsed from the given project FILE.

The project properties returned:
key                 description
---                 -----------
file-version        The project file schema version.
engine-version      The engine version used in the project.
target-platforms    The list of platforms the project supports.
modules             The list of Unreal modules.

The module properties returned:
key                 description
---                 -----------
name                The module name.
type                The module type.
deps                The modules this one depends on.
"
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
  "Get the name of the current Unreal Engine project."
  (when-let* ((project (projectile-unreal-project)))
    (file-name-base project)))

(defun projectile-unreal-epic-games-dir-platform-default ()
  "Return the platform default location of the 'Epic Games' directory."
  (pcase system-type
    ('darwin     "/Users/Shared/Epic Games")
    ('gnu/linux  "/opt/Epic Games")
    ('windows-nt "C:/Program Files/Epic Games")
    (_           (error "Unsupported platform."))))

(defun projectile-unreal-epic-games-dir ()
  "Return the directory where Unreal Engine(s) are installed.

The function uses 'projectile-unreal-epic-games-dir'
variable unless it is nil."
  (or projectile-unreal-epic-games-dir
      (projectile-unreal-epic-games-dir-platform-default)))

(defun projectile-unreal-engine-dir (engine-version)
  "Return the installation directory of Unreal Engine of the given ENGINE-VERSION."
  (expand-file-name (concat "UE_" engine-version)
		    (projectile-unreal-epic-games-dir)))

(defun projectile-unreal-engine-build-script-rel ()
  "Return platform specific path to the build script relative to the engine directory."
  (pcase system-type
    ('darwin     "Engine/Build/BatchFiles/Mac/Build.sh")
    ('gnu/linux  "Engine/Build/BatchFiles/Linux/Build.sh")
    ('windows-nt "Engine/Build/BatchFiles/Build.bat")
    (_           (error "Unsupported platform."))))

(defun projectile-unreal-engine-build-script (engine-version)
  "Return location of the Unreal Engine build script specific to the current platform and ENGINE-VERSION."
  (expand-file-name (projectile-unreal-engine-build-script-rel)
		    (projectile-unreal-engine-dir engine-version)))

(defun projectile-unreal-build-platform-default ()
  "Return the build plaform name for the current system."
  (pcase system-type
    ('darwin     "Mac")
    ('gnu/linux  "Linux")
    ('windows-nt "Win64")
    (_           (error "Unsupported platform."))))

(defun projectile-unreal-shell-script-invoker ()
  "Return string used to invoke a shell script on the current platform."
  (pcase system-type
    ('darwin     "bash")
    ('gnu/linux  "bash")
    ('windows-nt "call")
    (_           (error "Unsupported platform."))))

(defun projectile-unreal-project-build-cmd (&rest command-args)
  "Return the terminal command that uses Build.sh/Build.bat to operate on the current project."
  (let* ((project        (projectile-unreal-parse-project))
	 (engine-version (alist-get 'engine-version project))
	 (build-script   (projectile-unreal-engine-build-script engine-version))
	 (project-file   (projectile-unreal-project)))
    (string-join (append (list (projectile-unreal-shell-script-invoker)
			       (shell-quote-argument build-script) 
			       (concat "-project=" (shell-quote-argument project-file)))
			 command-args)
		 " ")))

(defun projectile-unreal-project-compilation-cmd ()
  "Return the compilation command for the current Unreal Engine project."
  (projectile-unreal-project-build-cmd
   (concat (projectile-unreal-project-name) "Editor")
   "Development"
   (projectile-unreal-build-platform-default)
   "-game"
   "-progress"
   "-buildscw"))

(defun projectile-unreal-mode-setup ()
  "Configure 'projectile-unreal-mode'."
  (setq projectile-globally-ignored-directories
	(projectile-unreal-non-source-directories)

	projectile-project-name
	(projectile-unreal-project-name)

	projectile-project-compilation-cmd
	(projectile-unreal-project-compilation-cmd)))

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
