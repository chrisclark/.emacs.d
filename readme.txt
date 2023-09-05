1. Install emacs from: https://emacsformacosx.com/

2. Symlink to this .emacs.d. E.g.
> ln -s ~/Dropbox/.emacs.d ~/.emacs.d

3. Ensure homebrew is installed (googleable)

4. Install Cask (emacs package management)
> brew install cask

5. Install emacs packages. In the .emacs.d folder:
> cask install

6. You may need to update misc.el's gimme-org command to refer to the correct Dropbox path. It's hard-coded.

7. Emacs glory is mine once again!
