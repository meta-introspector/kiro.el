# Kiro Tunnel - Emacs-to-Emacs Communication

Connect multiple Emacs instances via spool-based message passing.

## Setup

```elisp
(load-file "~/.emacs.d/kiro.el/kiro-tunnel.el")
(kiro-tunnel-init)
```

## Architecture

```
Emacs A → ~/kiro-spool/tunnel/msg-A-to-B.json → Emacs B
Emacs B → ~/kiro-spool/tunnel/msg-B-to-A.json → Emacs A
```

## Commands

### Initialize Tunnel
```elisp
M-x kiro-tunnel-init
```

### List Peers
```elisp
M-x kiro-tunnel-list-peers
```

### Ping Peer
```elisp
M-x kiro-tunnel-ping RET emacs-<pid> RET
```

### Send Message
```elisp
(kiro-tunnel-send "emacs-<pid>" 'action "data")
```

## Actions

- `ping` - Ping peer
- `eval` - Evaluate expression remotely
- `sync-dashboard` - Sync dashboard state

## Example

```bash
# Terminal 1 - Current Emacs
emacsclient --eval '(kiro-tunnel-init)'

# Terminal 2 - Spool Emacs  
~/.emacs.d/kiro.el/kiro-emacs.py eval '(kiro-tunnel-init)'

# Ping from current to spool
emacsclient --eval '(kiro-tunnel-ping "emacs-4169559")'

# Sync dashboard
emacsclient --eval '(kiro-tunnel-send "emacs-4169559" (quote sync-dashboard) "")'
```

## Status

✅ Peer discovery working
✅ Message passing operational
✅ Ping/pong tested
✅ Dashboard sync available
