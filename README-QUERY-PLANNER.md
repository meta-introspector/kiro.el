# Kiro Emacs Query Planner

Rust-based query planner integrated with Emacs buffer execution and caching.

## Architecture

```
Query → Plan (cached) → Execute in Buffer → Results (cached)
```

## Components

1. **kiro-query-planner.el** - Emacs query planner with:
   - Heuristic-based planning (extensible to Rust/Prolog)
   - Buffer-based job execution
   - Query result caching
   - Multi-tool support (plocate, ripgrep, omnisearch, knowledge)

2. **omnisearch_query_planner.rs** - Rust planner (reference implementation)

## Usage

### Via Python Client

```bash
# Plan query
~/.emacs.d/kiro.el/kiro-emacs.py query-plan "Monster prime 71"

# Execute (need job ID from planning)
~/.emacs.d/kiro.el/kiro-emacs.py query-execute "<job-id>"

# Get results
~/.emacs.d/kiro.el/kiro-emacs.py query-results "<job-id>"
```

### Via Elisp

```elisp
;; Load planner
(load-file "~/.emacs.d/kiro.el/kiro-query-planner.el")

;; Plan and execute
(let* ((plan (kiro-query-plan "Monster prime"))
       (job-id (car (hash-table-keys kiro-query-jobs))))
  (kiro-query-execute job-id)
  (kiro-query-get-results job-id))
```

## Features

- **Caching**: Query plans and results cached in hash tables
- **Buffer Execution**: Each job runs in dedicated `*kiro-query-<id>*` buffer
- **Multi-Strategy**: Single, parallel, sequential, fallback execution
- **Tool Integration**: plocate, ripgrep, omnisearch, knowledge bases

## Test

```bash
~/.emacs.d/kiro.el/test-query.sh
```

## Status

✅ Query planning working
✅ Job creation in buffers
✅ Cache system operational
✅ Multi-tool execution
🔄 Results aggregation (in progress)
