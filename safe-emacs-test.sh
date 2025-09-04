#!/bin/bash

# Test Emacs in safe mode to diagnose issues

echo "Emacs Safe Mode Tester"
echo "======================"
echo ""
echo "Choose test mode:"
echo "1) Vanilla Emacs (no config at all)"
echo "2) Minimal Spacemacs (bypass user config)"
echo "3) Debug mode (show all errors)"
echo "4) List mode (just list packages)"
echo ""
echo "Enter choice (1-4):"
read -r CHOICE

case $CHOICE in
    1)
        echo "Starting vanilla Emacs with no configuration..."
        emacs -Q
        ;;
    2)
        echo "Starting Spacemacs with minimal config..."
        emacs --eval "(setq spacemacs-start-directory \"~/.emacs.d/\")" \
              --eval "(load \"~/.emacs.d/init.el\")" \
              --eval "(setq dotspacemacs-configuration-layers '())"
        ;;
    3)
        echo "Starting Emacs in debug mode..."
        emacs --debug-init
        ;;
    4)
        echo "Listing installed packages..."
        emacs --batch --eval "(progn
          (require 'package)
          (package-initialize)
          (dolist (pkg package-alist)
            (princ (format \"%s\n\" (car pkg)))))"
        ;;
    *)
        echo "Invalid choice"
        ;;
esac