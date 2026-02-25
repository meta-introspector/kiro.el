#!/usr/bin/env python3
"""Kiro CLI helper to send commands to Emacs via spool"""

import json
import time
import sys
from pathlib import Path
from datetime import datetime

SPOOL_DIR = Path.home() / "kiro-spool"

def send_request(action, data, timeout=10):
    SPOOL_DIR.mkdir(exist_ok=True)
    req_id = str(int(time.time() * 1000))
    req_file = SPOOL_DIR / f"request-{req_id}.json"
    resp_file = SPOOL_DIR / f"response-{req_id}.json"
    
    req_file.write_text(json.dumps({"id": req_id, "action": action, "data": data}))
    
    for _ in range(timeout * 10):
        if resp_file.exists():
            result = json.loads(resp_file.read_text())
            resp_file.unlink()
            return result["result"]
        time.sleep(0.1)
    
    raise TimeoutError("No response from Emacs")

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print("Usage: kiro-emacs.py <action> [args...]")
        sys.exit(1)
    
    action = sys.argv[1]
    
    if action == "eval":
        result = send_request("eval", {"expr": sys.argv[2]})
        print(result)
    elif action == "find-file":
        send_request("find-file", {"path": sys.argv[2]})
        print(f"Opened {sys.argv[2]}")
    elif action == "get-content":
        content = send_request("get-buffer-content", {})
        print(content)
    elif action == "query-plan":
        plan = send_request("query-plan", {"pattern": sys.argv[2]})
        print(plan)
    elif action == "query-execute":
        send_request("query-execute", {"job_id": sys.argv[2]})
        print(f"Executing job {sys.argv[2]}")
    elif action == "query-results":
        results = send_request("query-results", {"job_id": sys.argv[2]})
        print(results)
    elif action == "dashboard":
        send_request("dashboard", {})
        print("Dashboard opened in Emacs")
