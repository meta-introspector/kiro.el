#!/usr/bin/env bash
# Demo Kiro Dashboard

set -euo pipefail

echo "=== Kiro Dashboard Demo ==="

# Create some test queries
echo -e "\n1. Creating test queries..."
~/.emacs.d/kiro.el/kiro-emacs.py query-plan "Monster prime 71" > /dev/null
~/.emacs.d/kiro.el/kiro-emacs.py query-plan "tau function" > /dev/null
~/.emacs.d/kiro.el/kiro-emacs.py query-plan "Ramanujan" > /dev/null

echo "✅ Created 3 test queries"

# Open dashboard
echo -e "\n2. Opening dashboard..."
~/.emacs.d/kiro.el/kiro-emacs.py dashboard

echo -e "\n3. Dashboard commands:"
echo "  g - Refresh"
echo "  n - New query"
echo "  e - Execute job at point"
echo "  v - View job buffer"
echo "  c - Clear cache"
echo "  q - Quit"

echo -e "\n✅ Dashboard ready in Emacs"
echo "   Attach with: tmux attach -t kiro-emacs"
