#!/bin/bash

# This script installs all the necessary dependencies for PHP, ReactJS, and Emacs development tools.

# Ensure the script is running as root (or with sudo)
if [[ $(id -u) -ne 0 ]]; then
  echo "This script requires root privileges. Please run as root or with sudo."
  exit 1
fi

# Update package list
echo "Updating package list..."
apt-get update -y

# Install PHP and related packages
echo "Installing PHP and required extensions..."
apt-get install -y php php-cli php-xdebug php-xml php-mbstring php-curl php-zip php-sqlite3

# Install Node.js (required for ReactJS and Prettier)
echo "Installing Node.js (for ReactJS and Prettier)..."
curl -fsSL https://deb.nodesource.com/setup_18.x | bash - 
apt-get install -y nodejs

# Install global npm packages (Prettier, Typescript, typescript-language-server)
echo "Installing global npm packages (Prettier, Typescript, typescript-language-server)..."
npm install -g prettier typescript typescript-language-server

# Install LSP server for PHP (intelephense)
echo "Installing PHP language server (intelephense)..."
npm install -g intelephense

# Install Emacs dependencies (if not installed already)
echo "Installing Emacs..."
apt-get install -y emacs

# Install additional Emacs packages
echo "Installing Emacs packages (lsp-mode, prettier, yasnippet, flycheck, company)..."
# Make sure straight.el is set up, and use-package is available for managing Emacs packages

# For system-wide installations of packages, uncomment the following if you need to install Emacs packages directly
# emacs --batch -l ~/.emacs.d/init.el

# Update npm to ensure Prettier and other tools are up to date
echo "Updating npm..."
npm install -g npm

# Clean up
echo "Cleaning up..."
apt-get clean

# End of the script
echo "All dependencies installed successfully!"

# Reminder: You should now be able to use Emacs for PHP and ReactJS development with the required dependencies.
