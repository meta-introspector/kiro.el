#!/usr/bin/env bash
# Test Kiro CLI -> Emacs integration via spool

SPOOL_DIR=~/kiro-spool
mkdir -p "$SPOOL_DIR"

# Start Emacs in tmux with spool enabled
tmux new-session -d -s kiro-test "emacs -nw -l ~/.emacs.d/kiro.el/kiro-spool.el --eval '(kiro-spool-start)'"

sleep 2

# Send test request
REQUEST_ID=$(date +%s)
cat > "$SPOOL_DIR/request-$REQUEST_ID.json" <<EOF
{
  "id": "$REQUEST_ID",
  "action": "eval",
  "data": {
    "expr": "(+ 2 3)"
  }
}
EOF

# Wait for response
for i in {1..10}; do
  if [ -f "$SPOOL_DIR/response-$REQUEST_ID.json" ]; then
    echo "✅ Response received:"
    cat "$SPOOL_DIR/response-$REQUEST_ID.json"
    rm "$SPOOL_DIR/response-$REQUEST_ID.json"
    exit 0
  fi
  sleep 1
done

echo "❌ Timeout waiting for response"
exit 1
