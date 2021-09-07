# ue.el
## Synopsis

**ue** is a minor mode for working with [Unreal Engine](https://www.unrealengine.com/) projects in GNU Emacs.
Internally it is based on [Projectile](https://github.com/bbatsov/projectile).

It means that you can use Projectile's commands for greping (or acking) files, compile projects, etc.

This is a work-in-progress project, which means there are a lot of things to do.

## Setup

### Installation

The project is not yet released so it is not available on any of Emacs repositories, which means you have to install it
manually.

#### Manual

TBD (In-Progress)

## Usage

### The global mode

Use the package as a global mode:

```el
(require 'ue)

(ue-global-mode)
```

That will turn on the local `ue-mode` for the buffers which belong to an Unreal Engine project.

Probably you should read Projectile's [README](https://github.com/bbatsov/projectile) on setting up the completion system,
caching and indexing files.

## Caveats

TBD
