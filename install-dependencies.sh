#!/bin/bash

# Update package list
echo "Updating package list..."
sudo apt-get update -y

# Install PHP, Node.js, and required extensions
echo "Installing PHP, Node.js, and required extensions..."
sudo apt-get install -y \
    php php-cli php-xdebug php-xml php-mbstring php-curl php-zip php-sqlite3 \
    nodejs emacs

# Install global npm packages (Prettier, Typescript, typescript-language-server, intelephense)
echo "Installing global npm packages..."
sudo npm install -g prettier typescript typescript-language-server intelephense npm

# Clean up
echo "Cleaning up..."
sudo apt-get clean

# Install ElixirLS
echo "Installing ElixirLS..."
if [[ ! -d "$HOME/.config/emacs/elixir-ls" ]]; then
  git clone https://github.com/elixir-lsp/elixir-ls.git "$HOME/.config/emacs/elixir-ls"
fi

cd "$HOME/.config/emacs/elixir-ls"
echo "Running mix deps.get..."
mix deps.get
echo "Running mix elixir_ls.release2..."
mix elixir_ls.release2

# Dictionary
sudo apt install dictd dict-gcide
sudo systemctl start dictd
sudo systemctl enable dictd


# End of the script
echo "All dependencies installed successfully!"
