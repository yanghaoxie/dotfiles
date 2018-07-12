# My dotfiles

This is a collection of dotfiles I use daily.

They are managed with [GNU Stow](https://www.gnu.org/software/stow/).

## Usage

* Install Stow

On Archlinux systems:

```console
sudo pacman -S stow
```


* Checkout this repo in your $HOME

```console
git clone https://github.com/yanghaoxie/dotfiles.git
```
* Symlink modules

For example, if you want to use my emacs dotfiles:

```console
cd dotfiles
stow i3
```

And that is all. Stow will take care of symlinking the files to your $HOME.

# Author

* Yanghao Xie <yhaoxie@gmail.com>