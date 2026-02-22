#!/usr/bin/env bash
# Test dashboard navigation

echo "Testing dashboard navigation..."

# Open dashboard
emacsclient --eval '(kiro-dashboard)'

echo ""
echo "Dashboard commands:"
echo "  RET - Jump to buffer at cursor"
echo "  v   - View buffer"
echo "  g   - Refresh"
echo ""
echo "Navigate to any shell and press RET to jump to it!"
echo ""
echo "Try:"
echo "  1. Move cursor to a shell line"
echo "  2. Press RET"
echo "  3. You'll jump to that shell buffer"
