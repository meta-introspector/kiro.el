#!/bin/bash
# test-dashboard.sh - Test kiro-dashboard functions

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "=== Testing Kiro Dashboard ==="
echo

echo "1. Opening dashboard..."
emacsclient --eval "(progn (add-to-list (quote load-path) \"$SCRIPT_DIR\") (load-file \"$SCRIPT_DIR/kiro-dashboard.el\") (kiro-dashboard))" 2>&1 | head -1

echo "2. Checking shell count..."
emacsclient --eval '(length (seq-filter (lambda (buf) (with-current-buffer buf (or (eq major-mode (quote shell-mode)) (eq major-mode (quote eshell-mode))))) (buffer-list)))' 2>&1

echo "3. Testing refresh..."
emacsclient --eval '(with-current-buffer "*Kiro Dashboard*" (kiro-dashboard-refresh) "Refreshed")' 2>&1

echo "4. Checking service status..."
emacsclient --eval '(with-current-buffer "*Kiro Dashboard*" (goto-char (point-min)) (search-forward "Service Status" nil t) (buffer-substring-no-properties (line-beginning-position) (+ (point) 200)))' 2>&1 | sed 's/^"//; s/"$//; s/\\n/\n/g'

echo
echo "5. Testing view-job navigation..."
emacsclient --eval '(with-current-buffer "*Kiro Dashboard*" (goto-char (point-min)) (search-forward "• *shell*" nil t) (beginning-of-line) (looking-at "• \\\\([^ ]+\\\\)"))' 2>&1

echo
echo "✅ Dashboard tests complete"
