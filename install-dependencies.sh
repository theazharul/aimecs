#!/bin/bash

# Script to install dependencies for AIM Emacs Configuration on Debian-based systems
# Run with sudo if necessary for package installation

set -e

echo "Updating package lists..."
sudo apt update

echo "Installing core dependencies..."
sudo apt install -y \
    emacs \
    git \
    curl \
    wget \
    zsh \
    ripgrep \
    silversearcher-ag \
    unzip \
    make \
    gcc \
    g++ \
    libgccjit0 \
    libgccjit-12-dev \
    libjansson4 \
    libjansson-dev \
    libtree-sitter0 \
    libtree-sitter-dev

# Install fonts
echo "Installing fonts..."
# Create a fonts directory in the user's home
mkdir -p ~/.local/share/fonts

# Navigate to a temporary directory
cd /tmp

# Download the Source Code Pro font
wget https://github.com/adobe-fonts/source-code-pro/archive/refs/heads/release.zip -O source-code-pro.zip
# Unzip the downloaded file
unzip source-code-pro.zip

# Copy the OTF font files to the fonts directory
cp source-code-pro-release/OTF/*.otf ~/.local/share/fonts/

# Update the font cache
fc-cache -f -v

# Verify the font is installed
fc-list | grep "Source Code Pro"

# Clean up
rm -rf source-code-pro.zip source-code-pro-release

# Install language-specific dependencies
echo "Installing Python dependencies..."
sudo apt install -y python3 python3-pip
pip3 install --user pyright black --break-system-packages

echo "Installing Node.js and related tools..."
sudo apt install -y nodejs
sudo npm install -g prettier \
    typescript-language-server \
    vscode-langservers-extracted \
    intelephense

echo "Installing Elixir and language server..."
sudo apt install -y elixir
# Install elixir-ls (Elixir Language Server)
git clone git@github.com:elixir-lsp/elixir-ls.git ~/.elixir-ls
cd ~/.elixir-ls
mix deps.get
mix compile
mix elixir_ls.release2 -o release
cd -

echo "Installing Go and gopls..."
sudo apt install -y golang
# go install golang.org/x/tools/gopls@latest

echo "Installing Dart..."
sudo apt install -y apt-transport-https
wget -qO- https://dl-ssl.google.com/linux/linux_signing_key.pub | sudo gpg --dearmor -o /usr/share/keyrings/dart.gpg
echo 'deb [signed-by=/usr/share/keyrings/dart.gpg arch=amd64] https://storage.googleapis.com/download.dartlang.org/linux/debian stable main' | sudo tee /etc/apt/sources.list.d/dart_stable.list
sudo apt update
sudo apt install -y dart

echo "Installing PHP dependencies..."
sudo apt install -y php-cli
# Install pint for PHP formatting
sudo curl -sSL https://github.com/laravel/pint/releases/latest/download/pint.phar -o /usr/local/bin/pint
sudo chmod +x /usr/local/bin/pint

echo "Installing LaTeX dependencies..."
sudo apt install -y texlive-full latexmk

echo "Installing PDF Tools dependencies..."
sudo apt install -y libpoppler-glib-dev libpng-dev zlib1g-dev

echo "Installing email-related dependencies..."
sudo apt install -y mu4e isync msmtp

# Ensure temporary directories for Emacs
echo "Creating Emacs temporary directories..."
mkdir -p ~/.config/emacs/tmp/{backups,auto-save,undo-tree}

echo "Installing additional utilities..."
sudo apt install -y aspell aspell-en # For flyspell
sudo apt install -y editorconfig # For editorconfig support

echo "Installation complete! Please verify language servers and tools are in PATH."
echo "You may need to restart your shell or system for some changes to take effect."
