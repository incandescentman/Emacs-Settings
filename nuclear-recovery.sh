#!/bin/bash

# Nuclear Spacemacs Recovery - Complete Reset
echo "NUCLEAR SPACEMACS RECOVERY"
echo "=========================="
echo ""
echo "This will completely reset Spacemacs packages and cache."
echo "Your configuration files will be preserved."
echo ""
echo "This will:"
echo "1. Backup current state"
echo "2. Delete ALL packages and cache"
echo "3. Force Spacemacs to reinstall everything fresh"
echo ""
echo "Continue? (yes/no)"
read -r CONFIRM

if [ "$CONFIRM" != "yes" ]; then
    echo "Cancelled."
    exit 0
fi

# Create comprehensive backup
BACKUP_TIME=$(date +%Y%m%d-%H%M%S)
BACKUP_DIR="$HOME/.emacs.d-backup-$BACKUP_TIME"

echo "Creating full backup at $BACKUP_DIR..."
cp -r ~/.emacs.d "$BACKUP_DIR"
echo "Backup complete."

echo ""
echo "Starting nuclear recovery..."

# 1. Remove all package directories
echo "1. Removing elpa directory..."
rm -rf ~/.emacs.d/elpa
mkdir -p ~/.emacs.d/elpa

# 2. Clear all cache directories
echo "2. Clearing cache directories..."
rm -rf ~/.emacs.d/.cache/auto-compile
rm -rf ~/.emacs.d/.cache/packages
rm -rf ~/.emacs.d/.cache/quelpa
rm -rf ~/.emacs.d/.cache/.rollback
rm -f ~/.emacs.d/.cache/recentf
rm -f ~/.emacs.d/.cache/.lsp_session*
rm -f ~/.emacs.d/.cache/.mc-lists.el

# 3. Clear auto-save-list
echo "3. Clearing auto-save-list..."
rm -rf ~/.emacs.d/auto-save-list/*

# 4. Remove native compilation cache
echo "4. Clearing native compilation cache..."
rm -rf ~/.emacs.d/eln-cache

# 5. Remove quelpa builds
echo "5. Clearing quelpa builds..."
rm -rf ~/.emacs.d/quelpa/build
rm -rf ~/.emacs.d/quelpa/melpa

# 6. Remove any lock files
echo "6. Removing lock files..."
find ~/.emacs.d -name "*.lock" -delete 2>/dev/null
find ~/.emacs.d -name ".#*" -delete 2>/dev/null

# 7. Clear transient directory
echo "7. Clearing transient..."
rm -rf ~/.emacs.d/transient

echo ""
echo "Nuclear recovery complete!"
echo ""
echo "Now start Emacs with:"
echo "  emacs --debug-init"
echo ""
echo "Spacemacs will download and install all packages fresh."
echo "This may take 5-10 minutes on first launch."
echo ""
echo "Your backup is saved at:"
echo "  $BACKUP_DIR"
echo ""
echo "If you need to restore the backup:"
echo "  rm -rf ~/.emacs.d && mv $BACKUP_DIR ~/.emacs.d"