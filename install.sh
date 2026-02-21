#!/bin/bash
# Install kiro.el

set -e

INSTALL_DIR="${HOME}/.emacs.d/lisp/kiro"

echo "📦 Installing kiro.el..."

# Create directory
mkdir -p "$INSTALL_DIR"

# Clone or update
if [ -d "$INSTALL_DIR/.git" ]; then
    echo "🔄 Updating existing installation..."
    cd "$INSTALL_DIR"
    git pull
else
    echo "📥 Cloning kiro.el..."
    git clone https://github.com/meta-introspector/kiro.el.git "$INSTALL_DIR"
fi

# Add to init.el if not present
INIT_FILE="${HOME}/.emacs.d/init.el"
if ! grep -q "kiro-osm-save" "$INIT_FILE" 2>/dev/null; then
    echo "📝 Adding to init.el..."
    cat >> "$INIT_FILE" << 'EOF'

;; Kiro.el integration
(add-to-list 'load-path "~/.emacs.d/lisp/kiro")
(require 'kiro-osm-save)
EOF
fi

echo "✅ Installation complete!"
echo ""
echo "Keybindings:"
echo "  C-c k c - Save, compact, and save compacted"
echo "  C-c k s - Quick save"
echo "  C-c k d - Toggle dry-run mode"
