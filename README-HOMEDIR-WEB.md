# Kiro Homedir Web Convention

Monster Symmetry Index for ~/projects/ with 71×59×47 door system.

## Overview

Organize projects by Monster symmetries with automatic discovery and nginx routing.

## Structure

```
~/projects/{project}/
├── public_html/          # Static HTML (nginx serves)
├── datasets/             # Parquet/JSON data
├── wasm/                 # Rust WASM door games
├── services/             # Microservices (systemd)
├── .monster/             # Monster symmetry metadata
│   ├── symmetries.json   # Input/output/invariant symmetries
│   ├── complexity.zkp    # ZKP complexity proofs
│   └── tests/            # Property tests
└── flake.nix             # Nix build
```

## Symmetries Format

```json
{
  "project": "monster-osm-quest",
  "symmetries": {
    "input": [71, 59, 47],
    "output": [17, 23, 59],
    "invariants": ["hyperbolic", "10-fold"]
  },
  "complexity": {
    "lines": 187,
    "cyclomatic": 12
  }
}
```

## Door System (71×59×47)

Global index: 71 layers × 59 sectors × 47 zones = 196,883 addresses

Address format: `{layer}.{sector}.{zone}`

Examples:
- `71.59.47` = Omega Terminal (full Monster)
- `17.23.59` = Sacred Shards (quest path)
- `2.3.5` = Prime Foundation (bootstrap)

## Commands

### List Projects
```elisp
M-x kiro-homedir-projects
```

Shows all projects with Monster symmetry indicators.

### Create Monster Directory
```elisp
M-x kiro-homedir-create-monster-dir RET project-name RET
```

Creates `.monster/symmetries.json` template.

### Find by Symmetry
```elisp
M-x kiro-homedir-find-by-symmetry RET 71 RET 59 RET
```

Find all projects with 71→59 transformation.

### Open Project
```elisp
M-x kiro-homedir-open-project RET project-name RET
```

### Generate Nginx Config
```elisp
M-x kiro-homedir-serve-project RET project-name RET
```

## Service Ports

Base: 10000 + (layer × 100) + sector

Examples:
- `71.59.*` → 17159 + zone
- `17.23.*` → 11723 + zone
- `2.3.*` → 10203 + zone

## Example Output

```
╔═══════════════════════════════════════════════════════════╗
║           🌐 HOMEDIR WEB PROJECTS 🌐                     ║
╚═══════════════════════════════════════════════════════════╝

• monster-osm-quest-standalone ✓MONSTER [71,59,47→17,23,59]
• osm-planet-torrent ✓MONSTER [71,59,47→17,23,59]
• kiro
• introspector
```

## Integration

Add to dashboard or use standalone:

```elisp
(global-set-key (kbd "C-c k h") 'kiro-homedir-projects)
```

## Convention Source

Based on UUCP message: `/mnt/data1/zones/42/uucp/spool/HOMEDIR-WEB-CONVENTION-20260222.txt`
