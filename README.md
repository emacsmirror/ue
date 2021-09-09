[[_TOC_]]

## Synopsis

**ue.el** provides a minor mode for working with [Unreal Engine](https://www.unrealengine.com/) projects in GNU Emacs.
It complements [Unreal Emacs plug-in](https://gitlab.com/unrealemacs/emacs-sourcecode-access) and uses project files it
generates.

Internally it is based on [Projectile](https://github.com/bbatsov/projectile).
It means that you can use Projectile's commands for greping (or acking) files, switching between alternating files, etc.

Compilation and running is not available yet but will be added in the future. 
This is a work-in-progress project.

## Features

### Supported Projectile Features

The project extends Projectile with Unreal Engine support and supports most of its features.
Please, refer to [Projectile usage documentation](https://docs.projectile.mx/projectile/usage.html) for the list of things you can do.

The following Projectile features are not supported yet:

- Test-related commands
- Running Project/UnrealEditor executables (coming soon!)
- Debugging (will be added at some point)

#### Building the Project

Unreal Engine projects could be built for different platforms and configurations.

The `ue.el` uses the same key bindings to build a project as Projectile `s-p c`.
If you haven't set project run configuration yet, the `ue.el` will ask you to choose one next time you build the project.
It will not ask for it again if it is set.


#### Mode Line

You can see the current run configuration in the mode-line:

```
ue[MyBigProjectEditor-Mac-DebugGame]
```

If the configuration is unknown, the mode-line looks as follows:

```
ue[?]
```

### Syntax Highlighting

There are a few Unreal Engine keywords and macro highlighted by default.

#### Keywords

- `UCLASS`
- `UDELEGATE`
- `UENUM`
- `UFUNCTION`
- `UINTERFACE`
- `UMETA`
- `UPARAM`
- `UPROPERTY`
- `USTRUCT`

Keywords faces are configured by altering `ue-attribute-face` variable which is set to `font-lock-preprocessor-face` by default.

#### Macro

- `GENERATED_BODY`
- `GENERATED_IINTERFACE_BODY`
- `GENERATED_UCLASS_BODY`
- `GENERATED_UINTERFACE_BODY`
- `GENERATED_USTRUCT_BODY`

Macro faces could be configured by altering `ue-generated-body-macro-face` variable which is set to `font-lock-preprocessor-face` by default.

### Snippets

The package activates a few Unreal Engine snippets if you have [yasnippet](https://github.com/joaotavora/yasnippet) installed.
More snippets will be added in the future.

#### Logging

| Key      | Description                    |
| ---      | ---                            |
| `ulh`    | Declares a custom log category |
| `ulc`    | Defines a custom log category  |
| `ull`    | Writes a message to the log    |

#### UPROPERTY

| Key      | Description                    |
| ---      | ---                            |
| `upc`    | Component `UPROPERTY`          |
| `upe`    | `UPROPERTY(EditDefaultsOnly)`  |
| `ups`    | `TSubclassOf UPROPERTY`        |
| `upv`    | `UPROPERTY(VisibleEverywhere)` |

#### UFUNCTION

| Key      | Description                    |
| ---      | ---                            |
| `uff`    | `UFUNCTION(BlueprintCallable)` |
| `ufp`    | `UFUNCTION(BlueprintPure)`     |

#### Events

All event snippets use the same mnemonic.
They start with `u` like all Unreal Engine snippets.
Then either `c` (**c**pp, i.e. declare event handler) or `h` (**h**eader, i.e. define event handler) or `s` (**s**ubscribe to event) follows.
Then follows the lowercase event name.

| Key      | Description                    |
| ---      | ---                            |
| `uconcomponentbeginoverlap` | Define `OnComponentBeginOverlap` event handler  |
| `uhoncomponentbeginoverlap` | Declare `OnComponentBeginOverlap` event handler |
| `usoncomponentbeginoverlap` | Subscribe to `OnComponentBeginOverlap` event    |

#### Misc

| Key      | Description                    |
| ---      | ---                            |
| `ucds`   | `CreateDefaultSubobject`       |
| `utext`  | `TEXT()`                       |

## Setup

### Installation

The project is not released yet and is not available on any of Emacs repositories, which means you have to install it manually.

#### Manual

Clone the project:

```shell
$ mkdir -p ~/Documents/Projects/UnrealEmacs
$ cd ~/Documents/Projects/UnrealEmacs
$ git clone git@gitlab.com:unrealemacs/ue.el.git ue
```

Add the cloned directory to Emacs `load-path` in your `init.el` file:

```elisp
(add-to-list 'load-path "~/Documents/Projects/UnrealEmacs/ue")
```

## Usage

### The global mode

Use the package as a global mode:

```elisp
(require 'ue)

(ue-global-mode)
```

That will turn on the local `ue-mode` for the buffers which belong to an Unreal Engine projects generated by .

Probably you should read Projectile's [README](https://github.com/bbatsov/projectile) on setting up the completion system,
caching and indexing files.

## Caveats

Project still has to be tested on GNU/Linux and Windows.

There is no default key binding for switching the run configuration yet :) 
You can do that manually via `M-x ue-select-project-target`.
