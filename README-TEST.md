# Kiro Test Harness & Formal Verification

## ERT Test Harness

Run tests in Emacs:

```elisp
(load-file "~/.emacs.d/kiro.el/kiro-test.el")
(ert-run-tests-batch-and-exit)
```

Or interactively:
```
M-x ert RET t RET
```

## Lean4 Formal Proofs

### Setup
```bash
cd ~/.emacs.d/kiro.el
lake update
lake build
```

### Verified Properties

1. **Monster primes are prime** - All primes in the system are mathematically prime
2. **Classification is deterministic** - Same content always maps to same level
3. **URL uniqueness** - Different IDs generate different URLs
4. **Totality** - All functions are defined for all valid inputs
5. **RDF structure preservation** - Triple formation maintains semantic structure

### Check proofs
```bash
lake build Kiro
```
