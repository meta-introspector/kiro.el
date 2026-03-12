#!/bin/bash
# demo-kiro.sh - Interactive demo of kiro.el

cat << 'EOF'
╔═══════════════════════════════════════════════════════════╗
║                    Kiro.el Demo                           ║
║                                                           ║
║  Complete Emacs integration for Kiro CLI                  ║
╚═══════════════════════════════════════════════════════════╝

This demo will show you how to use kiro.el.

Press Enter to continue...
EOF
read

echo ""
echo "📦 Step 1: Load kiro.el in Emacs"
echo ""
echo "Run this command:"
echo "  emacs --eval \"(load-file \\\"$HOME/.emacs.d/kiro.el/init-kiro.el\\\")\""
echo ""
echo "Or add to your ~/.emacs.d/init.el:"
echo "  (load-file \"$HOME/.emacs.d/kiro.el/init-kiro.el\")"
echo ""
read -p "Press Enter to continue..."

echo ""
echo "🎯 Step 2: Create a new task"
echo ""
echo "In Emacs, run:"
echo "  M-x kiro-task-new RET my-first-task RET"
echo ""
echo "This creates ~/tasks/task-my-first-task.kiro.org"
echo ""
read -p "Press Enter to continue..."

echo ""
echo "🐚 Step 3: Test shell integration"
echo ""
echo "1. In Emacs: M-x shell RET"
echo "2. Start kiro-cli: kiro-cli chat"
echo "3. Have a conversation..."
echo "4. Press: C-c k s"
echo ""
echo "This saves the chat to chats/YYYY/MM/DD/taskname-TIMESTAMP.json"
echo ""
read -p "Press Enter to continue..."

echo ""
echo "🔄 Step 4: Switch between buffers"
echo ""
echo "In Emacs, run:"
echo "  M-x kiro-switch-buffer RET 2 RET"
echo ""
echo "This switches to 'kiro-binary' (prime 2 = Energy)"
echo ""
echo "Try other primes:"
echo "  3 = kiro-ternary (Time/Triples)"
echo "  5 = kiro-pentagonal (Space)"
echo "  31 = kiro-communication (Chat)"
echo ""
read -p "Press Enter to continue..."

echo ""
echo "💾 Step 5: Save everything"
echo ""
echo "In Emacs, run:"
echo "  M-x kiro-save-all RET"
echo ""
echo "This will:"
echo "  - Rename all shells"
echo "  - Rename all buffers"
echo "  - Save all modified buffers"
echo "  - Compact buffers (DASL)"
echo "  - Save all CLI sessions"
echo "  - Rename terminal windows"
echo "  - Compact sessions"
echo ""
read -p "Press Enter to continue..."

echo ""
echo "📋 Step 6: View all tasks"
echo ""
echo "In Emacs, run:"
echo "  M-x kiro-task-list RET"
echo ""
echo "This shows all tasks in ~/tasks/ with org-mode links"
echo ""
read -p "Press Enter to continue..."

echo ""
echo "🎛️ Step 7: Interactive managers"
echo ""
echo "Try these commands:"
echo "  M-x kiro-buffers    - Manage all buffers"
echo "  M-x kiro-sessions   - Manage CLI sessions"
echo "  M-x kiro-dashboard  - Open dashboard"
echo ""
read -p "Press Enter to continue..."

echo ""
echo "⌨️  Quick Reference - Keybindings"
echo ""
echo "Global:"
echo "  C-c k r  - Rename buffer as task"
echo "  C-c k n  - New task"
echo "  C-c k l  - List tasks"
echo "  C-c k b  - Switch buffer"
echo ""
echo "In shell-mode:"
echo "  C-c k s  - Save chat"
echo "  C-c k c  - Save, compact, save"
echo ""
read -p "Press Enter to continue..."

echo ""
echo "📚 Documentation"
echo ""
echo "  README.md        - Full documentation"
echo "  QUICKREF.md      - Quick reference card"
echo "  TESTING.md       - Testing guide"
echo "  TEST-SUMMARY.md  - Test results"
echo ""
echo "All files in: ~/.emacs.d/kiro.el/"
echo ""
read -p "Press Enter to continue..."

echo ""
echo "🧪 Run Tests"
echo ""
echo "Automated test:"
echo "  ~/.emacs.d/kiro.el/test-kiro.sh"
echo ""
echo "Interactive test:"
echo "  emacs -l ~/.emacs.d/kiro.el/test-interactive.el"
echo ""
read -p "Press Enter to finish..."

cat << 'EOF'

╔═══════════════════════════════════════════════════════════╗
║                    Demo Complete!                         ║
║                                                           ║
║  Kiro.el is ready to use. Happy hacking! 🚀               ║
╚═══════════════════════════════════════════════════════════╝

Next steps:
  1. Add to init.el: (load-file "~/.emacs.d/kiro.el/init-kiro.el")
  2. Restart Emacs or eval the line
  3. Try: M-x kiro-task-new RET test RET
  4. See QUICKREF.md for all commands

EOF
