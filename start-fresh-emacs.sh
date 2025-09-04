#!/bin/bash

echo "Starting Emacs with fresh package installation..."
echo "This will take 5-10 minutes to download and install all packages."
echo ""
echo "Starting Emacs now. Check the *Messages* buffer for progress."
echo "Press Ctrl-C to cancel if needed."
echo ""

# Start Emacs with minimal startup to let Spacemacs install packages
emacs --eval "(setq dotspacemacs-verbose-loading t)" &

echo "Emacs is starting (PID: $!)"
echo ""
echo "If Emacs doesn't open within 30 seconds, try:"
echo "  1. Check if packages are downloading: ls ~/.emacs.d/elpa/"
echo "  2. Kill and restart: pkill emacs && emacs --debug-init"
echo "  3. Check errors: cat ~/.emacs.d/spacemacs-buffer-error.txt"