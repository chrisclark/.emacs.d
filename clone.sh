# Get iterm
https://www.iterm2.com/downloads.html

# Install Homebrew and deps
/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew install emacs
brew install cask
brew install git

# OS X ships with an ancient version of emacs. Alias to the Homebrew version.
alias emacs="/usr/local/Cellar/emacs/24.5/bin/emacs -nw"

git clone https://github.com/chrisclark/.emacs.d.git
cask install --path ~/.emacs.d/

# Then download and install OSX emacs from: https://emacsformacosx.com
