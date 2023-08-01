# emacs-configuration

To use:
* Clone this repo into your `.emacs.d` directory (or wherever Emacs looks for it's `init.el` file.)
* Edit the `Configuration.org` file.
* Save the file.
  * The `init.el` file is created!

Of course, don't edit the `init.el` file as any changes will be lost the next time the `Configure.org` file is saved.

Note: I've had cases where saving the Org file doesn't re-generate the `init.el`. This generally means that there might be an issue with one or more emacs-lisp blocks. You can always force Org mode to write the `init.el` by calling `M-x org-babel-tangle`.

My latest Emacs Configuration based upon an ORG mode file. I give credit to [System Crafters](https://systemcrafters.net) for showing me the way of using Org mode to manage the Emacs configuration.

There are some specific packages in the `site-lisp` that are either not in MELPA or couldn't be found with the correct verion needed. Also, the `use-package` package is there because it's needed on early startup. With the advent of Emacs 29, `use-package` is now part of Emacs so will eventually be removed from here.

This emacs configuration consists of a standard .org file that, when saved, will automatically create the init.el script.
I hope that at least one person finds this helpful!

- mitch
