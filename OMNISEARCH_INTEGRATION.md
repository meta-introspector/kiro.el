# Omnisearch Integration with Emacs Search Service

**Updated**: 2026-03-10

## Overview

The `kiro-query-planner.el` now uses the emacs-search service (port 8093) for omnisearch queries.

## Changes Made

### 1. Updated `kiro-query-omnisearch`
Now calls `http://localhost:8093/search/buffers` instead of external binary.

### 2. Added New Search Tools
- `emacs-buffers` - Search all Emacs buffers
- `emacs-kiro` - Search kiro-cli sessions
- `emacs-files` - Search files via Emacs

### 3. Updated Query Planner
Smart routing based on pattern:
- "buffer|emacs" → emacs-buffers + emacs-kiro
- "prime|Monster|group" → omnisearch + plocate + emacs-buffers
- "def|class|function" → emacs-buffers + ripgrep
- Default → emacs-buffers + omnisearch + plocate

### 4. Added Interactive Commands
- `M-x kiro-omnisearch` - Search everything
- `M-x kiro-search-emacs-buffers` - Search buffers only
- `M-x kiro-search-kiro-sessions` - Search kiro sessions only

## Usage

### From Emacs

```elisp
;; Reload
(load-file "~/.emacs.d/kiro.el/kiro-query-planner.el")

;; Search everything
M-x kiro-omnisearch RET pattern RET

;; Search buffers
M-x kiro-search-emacs-buffers RET pattern RET

;; Search kiro sessions
M-x kiro-search-kiro-sessions RET pattern RET
```

### Programmatic

```elisp
;; Plan and execute
(let ((job-id (kiro-query-plan "Monster")))
  (kiro-query-execute job-id))

;; Direct API calls
(kiro-query-emacs-buffers "test")
(kiro-query-emacs-kiro "kiro")
(kiro-query-emacs-files "*.el")
```

## Requirements

1. emacs-search service running on port 8093
2. curl available in PATH
3. json.el loaded (built-in)

## Testing

```bash
# Start service
systemctl --user start emacs-search

# Test from Emacs
emacs --eval '(progn
  (load-file "~/.emacs.d/kiro.el/kiro-query-planner.el")
  (message "%S" (kiro-query-emacs-buffers "test")))'
```

## Architecture

```
kiro-query-planner.el
  ↓
kiro-query-omnisearch (pattern)
  ↓
curl POST http://localhost:8093/search/buffers
  ↓
emacs_search.py (Flask)
  ↓
emacsclient --eval (search-forward pattern)
  ↓
JSON results [buffer, line, text]
  ↓
Format as "buffer:line: text"
```

## Benefits

1. **Unified Search** - All Emacs content searchable via REST API
2. **Live Results** - Searches current buffer state, not saved files
3. **Integration** - Works with existing query planner
4. **Extensible** - Easy to add new search endpoints

## Next Steps

- [ ] Add caching for repeated queries
- [ ] Add result ranking/scoring
- [ ] Add fuzzy matching
- [ ] Add search history
- [ ] Add result filtering
