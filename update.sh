#!/bin/bash
# Update kiro.el

set -e

INSTALL_DIR="${HOME}/.emacs.d/lisp/kiro"

if [ ! -d "$INSTALL_DIR/.git" ]; then
    echo "❌ kiro.el not installed. Run install.sh first."
    exit 1
fi

echo "🔄 Updating kiro.el..."
cd "$INSTALL_DIR"
git pull

echo "✅ Update complete!"
echo "Restart Emacs or run: M-x load-file RET ~/.emacs.d/lisp/kiro/kiro-osm-save.el"
