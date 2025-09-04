#!/bin/bash

# Spacemacs Package Recovery Script
# This script helps recover from a broken Spacemacs update

echo "Spacemacs Package Recovery Script"
echo "=================================="
echo ""

# Check if Emacs directory exists
if [ ! -d "$HOME/.emacs.d" ]; then
    echo "Error: ~/.emacs.d directory not found"
    exit 1
fi

# List available rollback points
echo "Available rollback points:"
ls -1 ~/.emacs.d/.cache/.rollback/develop/ | sort -r
echo ""

# Ask user to select a rollback point
echo "Enter the rollback directory name (e.g., 25-08-27_01.02.46) or 'manual' for manual recovery:"
read -r ROLLBACK_DIR

if [ "$ROLLBACK_DIR" = "manual" ]; then
    # Manual recovery - just clear current elpa
    echo "Manual recovery selected."
    echo "This will:"
    echo "1. Backup current elpa directory"
    echo "2. Clear elpa directory"
    echo "3. Let Spacemacs reinstall packages on next startup"
    echo ""
    echo "Continue? (y/n)"
    read -r CONFIRM
    
    if [ "$CONFIRM" = "y" ]; then
        # Create backup
        BACKUP_DIR="$HOME/.emacs.d/elpa-backup-$(date +%Y%m%d-%H%M%S)"
        echo "Creating backup at $BACKUP_DIR..."
        cp -r ~/.emacs.d/elpa "$BACKUP_DIR"
        
        # Clear elpa but keep version directories
        echo "Clearing elpa packages..."
        find ~/.emacs.d/elpa -maxdepth 1 -type d ! -name 'elpa' ! -name '29.*' -exec rm -rf {} +
        
        echo "Recovery complete. Start Emacs with:"
        echo "  emacs --debug-init"
        echo "Spacemacs will reinstall packages automatically."
    else
        echo "Cancelled."
    fi
else
    # Rollback to specific point
    ROLLBACK_PATH="$HOME/.emacs.d/.cache/.rollback/develop/$ROLLBACK_DIR"
    
    if [ ! -d "$ROLLBACK_PATH" ]; then
        echo "Error: Rollback directory not found: $ROLLBACK_PATH"
        exit 1
    fi
    
    echo "This will restore packages from: $ROLLBACK_DIR"
    echo "Continue? (y/n)"
    read -r CONFIRM
    
    if [ "$CONFIRM" = "y" ]; then
        # Create backup of current state
        BACKUP_DIR="$HOME/.emacs.d/elpa-backup-$(date +%Y%m%d-%H%M%S)"
        echo "Creating backup at $BACKUP_DIR..."
        cp -r ~/.emacs.d/elpa "$BACKUP_DIR"
        
        # Clear current packages (but keep version directories)
        echo "Clearing current packages..."
        find ~/.emacs.d/elpa -maxdepth 1 -type d ! -name 'elpa' ! -name '29.*' -exec rm -rf {} +
        
        # Copy packages from rollback point
        echo "Restoring packages from rollback point..."
        cp -r "$ROLLBACK_PATH"/* ~/.emacs.d/elpa/ 2>/dev/null || true
        
        echo "Recovery complete!"
        echo ""
        echo "Start Emacs with:"
        echo "  emacs --debug-init"
        echo ""
        echo "If it still doesn't work, your backup is at:"
        echo "  $BACKUP_DIR"
    else
        echo "Cancelled."
    fi
fi

echo ""
echo "Additional troubleshooting tips:"
echo "1. Try: emacs -Q (starts without any configuration)"
echo "2. Try: emacs --debug-init (shows error details)"
echo "3. Check ~/.emacs.d/spacemacs-buffer-error.txt for errors"
echo "4. Delete ~/.emacs.d/.cache/recentf if it's corrupted"