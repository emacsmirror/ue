# Projectile Unreal

## Synopsis

**Projectile Unreal** is a minor mode for working with [Unreal Engine](https://www.unrealengine.com/) projects in GNU Emacs.
Internally it is based on [Projectile](https://github.com/bbatsov/projectile).

It means that you can use Projectile's commands for greping (or acking) files, compile projects, etc.

This is a work-in-progress project, which means there are a lot of things to do.

## Setup

### Installation

The project is not yet released so it is not available on any of Emacs repositories, which means you have to install it
manually.

#### Manual

Download the [projectile-unreal.el](https://gitlab.com/manenko/projectile-unreal/-/raw/master/projectile-unreal.el) and
put it to a directory in your `load-path`, e.g. `~/emacs.d`.

To add a directory to the load path, add the following to your initialization file 
(probably `~/.emacs` or `~/.emacs.d/init.el`, depends on your preferrable setup):

```el
(add-to-list 'load-path "mypath")
```

## Usage

### The global mode

Use the package as a global mode:

```el
(require 'projectile-unreal)

(projectile-unreal-global-mode)
```

That will turn on the local `projectile-unreal-mode` for the buffers which belong to an Unreal Engine project.

Probably you should read Projectile's [README](https://github.com/bbatsov/projectile) on setting up the completion system,
caching and indexing files.

## Caveats

I haven't tested the project on GNU/Linux or Windows yet.
