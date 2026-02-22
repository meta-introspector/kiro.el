#!/usr/bin/env bash
# Send Kiro announcement to local UUCP spool

ANNOUNCEMENT="$HOME/.emacs.d/kiro.el/ANNOUNCEMENT.txt"
UUCP_SPOOL="/mnt/data1/spool/uucp"

echo "Sending Kiro announcement to UUCP spool..."

# Create UUCP message
TIMESTAMP=$(date +%Y%m%d%H%M%S)
MSG_FILE="$UUCP_SPOOL/kiro-announcement-$TIMESTAMP.txt"

cat > "$MSG_FILE" <<EOF
From: kiro-dev@localhost
Subject: Kiro Emacs System - Central Prompt Queue & Multi-Instance Dashboard
Date: $(date -R)

EOF

cat "$ANNOUNCEMENT" >> "$MSG_FILE"

echo "✅ Announcement sent to UUCP spool: $MSG_FILE"
echo ""
echo "Repository: https://github.com/meta-introspector/kiro.el"
echo "Branch: monster-integration"
echo "Commit: 56fe083"
