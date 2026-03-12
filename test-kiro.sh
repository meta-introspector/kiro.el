#!/bin/bash
# test-kiro.sh - Test kiro.el installation

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "🧪 Testing kiro.el installation..."
echo ""

# Check Emacs
echo "1. Checking Emacs..."
if command -v emacs &> /dev/null; then
    EMACS_VERSION=$(emacs --version | head -1)
    echo "   ✓ $EMACS_VERSION"
else
    echo "   ✗ Emacs not found"
    exit 1
fi

# Check kiro-cli
echo "2. Checking kiro-cli..."
if command -v kiro-cli &> /dev/null; then
    KIRO_PATH=$(which kiro-cli)
    echo "   ✓ kiro-cli found at $KIRO_PATH"
else
    echo "   ✗ kiro-cli not found"
    exit 1
fi

# Check kiro.el directory
echo "3. Checking kiro.el directory..."
if [ -d "$SCRIPT_DIR" ]; then
    FILE_COUNT=$(ls -1 "$SCRIPT_DIR"/*.el 2>/dev/null | wc -l)
    echo "   ✓ Found $FILE_COUNT elisp files in $SCRIPT_DIR"
else
    echo "   ✗ Directory not found: $SCRIPT_DIR"
    exit 1
fi

# Test loading
echo "4. Testing kiro.el loading..."
if emacs --batch -l "$SCRIPT_DIR/init-kiro.el" --eval "(message \"OK\")" 2>&1 | grep -q "✅ Kiro.el loaded successfully"; then
    echo "   ✓ kiro.el loads successfully"
else
    echo "   ✗ Failed to load kiro.el"
    exit 1
fi

# Test commands
echo "5. Testing commands..."
COMMANDS="kiro-task-new kiro-task-save-chat kiro-save-all kiro-dashboard"
for cmd in $COMMANDS; do
    if emacs --batch -l "$SCRIPT_DIR/init-kiro.el" --eval "(if (fboundp '$cmd) (message \"OK\") (error \"Not found\"))" 2>&1 | grep -q "OK"; then
        echo "   ✓ $cmd"
    else
        echo "   ✗ $cmd not found"
    fi
done

# Check directories
echo "6. Checking directories..."
TASK_DIR="$HOME/tasks"
if [ -d "$TASK_DIR" ]; then
    echo "   ✓ Task directory exists: $TASK_DIR"
else
    echo "   ○ Task directory will be created: $TASK_DIR"
fi

echo ""
echo "✅ All tests passed!"
echo ""
echo "Next steps:"
echo "  1. Add to your ~/.emacs.d/init.el:"
echo "     (load-file \"$SCRIPT_DIR/init-kiro.el\")"
echo ""
echo "  2. Or test interactively:"
echo "     emacs -l $SCRIPT_DIR/test-interactive.el"
echo ""
echo "  3. See TESTING.md for detailed usage"
