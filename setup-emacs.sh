#!/usr/bin/env bash
# Setup Kiro for Emacs M-x commands

EMACS_INIT="${HOME}/.emacs"
KIRO_SETUP="
;; Kiro Setup
(add-to-list 'load-path \"${HOME}/.emacs.d/kiro.el\")
(load-file \"${HOME}/.emacs.d/kiro.el/kiro.el\")
(global-set-key (kbd \"C-c k d\") 'kiro-dashboard)
(global-set-key (kbd \"C-c k q\") 'kiro-query)
"

if [ -f "$EMACS_INIT" ]; then
    if grep -q "Kiro Setup" "$EMACS_INIT"; then
        echo "✅ Kiro already configured in $EMACS_INIT"
    else
        echo "$KIRO_SETUP" >> "$EMACS_INIT"
        echo "✅ Added Kiro to $EMACS_INIT"
    fi
else
    echo "$KIRO_SETUP" > "$EMACS_INIT"
    echo "✅ Created $EMACS_INIT with Kiro setup"
fi

echo ""
echo "Kiro commands available:"
echo "  M-x kiro-dashboard  - Open dashboard"
echo "  M-x kiro-query      - Create query"
echo "  M-x kiro-spool-start - Start spool"
echo ""
echo "Keybindings:"
echo "  C-c k d - Dashboard"
echo "  C-c k q - Query"
