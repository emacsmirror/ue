;;; ue.el --- Minor mode for Unreal Engine projects based on projectile-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Oleksandr Manenko

;; Author:    Oleksandr Manenko <seidfzehsd@use.startmail.com>
;; URL:       https://gitlab.com/unrealemacs/ue.el
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
;;    (ue-global-mode)
;;

;;; Code:

(require 'projectile)
(require 'json)
(require 'seq)

;; TODO: *A*ctors
;;       *I*nterfaces
;;       *U*Objects
;;       *F*Classes
;; TODO: Set current target and save it? Then use that for a compile command.
;; TODO: Recommend to switch from alien mode. Or set project root to the Source dir?
;; TODO: Recommend installing ag Emacs package
;; TODO: Run the editor. We have -game and -server switches
;; https://docs.unrealengine.com/4.26/en-US/ProductionPipelines/CommandLineArguments/
;; TODO: Build configuration
;; https://docs.unrealengine.com/4.26/en-US/ProductionPipelines/DevelopmentSetup/BuildConfigurations/
;; TODO: Class wizards


(defgroup ue nil
  "A minor mode for Unreal Engine projects based on projectile-mode."
  :prefix "ue-"
  :group  'projectile)

(defcustom ue-attributes
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
  :group 'ue
  :type  '(repeat string))

(defcustom ue-class-specifiers
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
  :group 'ue
  :type  '(repeat string))

(defcustom ue-class-metadata-specifiers
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
  :group 'ue
  :type  '(repeat string))

(defcustom ue-enum-specifiers
  '("meta")
  "List of Unreal C++ UENUM specifiers."
  :group 'ue
  :type  '(repeat string))

(defcustom ue-enum-metadata-specifiers
  '("Bitflags"
    "Experimental"
    "ScriptName"
    "ToolTip")
  "List of Unreal C++ UENUM metadata specifiers."
  :group 'ue
  :type  '(repeat string))

(defcustom ue-enum-enumerator-specifiers
  '("DisplayName"
    "Hidden"
    "ToolTip")
  "List of Unreal C++ UMETA specifiers used for UENUM enumerators."
  :group 'ue
  :type  '(repeat string))

(defcustom ue-function-specifiers
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
  :group 'ue
  :type  '(repeat string))

(defcustom ue-function-metadata-specifiers
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
  :group 'ue
  :type  '(repeat string))

(defcustom ue-delegate-specifiers
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
  :group 'ue
  :type  '(repeat string))

(defcustom ue-delegate-metadata-specifiers
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
  :group 'ue
  :type  '(repeat string))

(defcustom ue-uparam-specifiers
  '("DisplayName"
    "ref")
  "List of Unreal C++ UPARAM specifiers."
  :group 'ue
  :type  '(repeat string))

(defcustom ue-interface-specifiers
  '("BlueprintType"
    "DependsOn"
    "MinimalAPI"
    "meta")
  "List of Unreal C++ UINTERFACE specifiers."
  :group 'ue
  :type  '(repeat string))

(defcustom ue-interface-metadata-specifiers
  '("CannotImplementInterfaceInBlueprint")
  "List of Unreal C++ UINTERFACE metadata specifiers."
  :group 'ue
  :type  '(repeat string))

(defcustom ue-property-specifiers
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
  :group 'ue
  :type  '(repeat string))

(defcustom ue-property-metadata-specifiers
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
  :group 'ue
  :type  '(repeat string))

(defcustom ue-struct-specifiers
  '("Atomic"
    "BlueprintType"
    "Immutable"
    "NoExport"
    "meta")
  "List of Unreal C++ USTRUCT specifiers."
  :group 'ue
  :type  '(repeat string))

(defcustom ue-struct-metadata-specifiers
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
  :group 'ue
  :type  '(repeat string))

(defcustom ue-generated-body-macro
  '("GENERATED_BODY"
    "GENERATED_IINTERFACE_BODY"
    "GENERATED_UCLASS_BODY"
    "GENERATED_UINTERFACE_BODY"
    "GENERATED_USTRUCT_BODY")
  "List of Unreal C++ GENERATED_*_BODY macros."
  :group 'ue
  :type  '(repeat string))

(defvar ue-root-directory ".uemacs"
  "The directory that is used to indentify Unreal Emacs project root.")

(defvar ue-cache-data (make-hash-table :test 'equal)
  "A hash table used for caching information about the current project.")

(defun ue-cache-key (key)
  "Generate a cache key based on the current directory and the given KEY."
  (format "%s-%s" default-directory key))

(defun ue-project-root ()
  "Return Unreal Emacs root directory if this file is part
of the Unreal Emacs project else nil."
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

(defun ue-maybe-enable-snippets ()
  "Enable Unreal Engine snippets if yasnippet is available."
  (when (require 'yasnippet nil t)
    (add-to-list 'yas-snippet-dirs 'ue-snippets-dir t)
    (yas-load-directory ue-snippets-dir t)))

(defun ue-mode-setup ()
  "Configure 'ue-mode'.")

;;;###autoload
(define-minor-mode ue-mode
  "Minor mode for Unreal Engine projects based on projectile-mode."
  :init-value nil
  :lighter    " ue"
  (when ue-mode
    (ue-mode-setup)))

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
  "Disable 'unreal-emacs-mode' minor mode."
  (ue-mode -1))

(projectile-register-project-type 'ue           (list ue-root-directory)
				  :project-file ue-root-directory)

(provide 'ue)

;;; ue.el ends here
