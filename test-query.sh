#!/usr/bin/env bash
# Test Kiro query planner integration

set -euo pipefail

echo "=== Testing Kiro Query Planner ==="

# Plan query
echo -e "\n1. Planning query..."
PLAN=$(~/.emacs.d/kiro.el/kiro-emacs.py query-plan "Monster prime")
echo "$PLAN"

# Get job ID
echo -e "\n2. Getting job ID..."
JOB_ID=$(~/.emacs.d/kiro.el/kiro-emacs.py eval '(progn (load-file "~/.emacs.d/kiro.el/kiro-query-planner.el") (kiro-query-plan "Monster prime") (car (hash-table-keys kiro-query-jobs)))' | tr -d '"')
echo "Job ID: $JOB_ID"

# Execute
echo -e "\n3. Executing query..."
~/.emacs.d/kiro.el/kiro-emacs.py query-execute "$JOB_ID"

# Get results
echo -e "\n4. Results:"
~/.emacs.d/kiro.el/kiro-emacs.py query-results "$JOB_ID"

echo -e "\n=== Test Complete ==="
