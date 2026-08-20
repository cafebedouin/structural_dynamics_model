#!/bin/bash
# Stratum-2 enumeration (OQ-310). Printed integrity lines in executable artifacts under audits/.
# An "integrity line" = an emitted line (print(/echo) whose payload reports the outcome of a
# comparison or an assertion verdict.
cd "$(git rev-parse --show-toplevel)"
find audits -type f \( -name '*.sh' -o -name '*.py' \) -print0 \
 | xargs -0 /usr/bin/grep -nE '^[[:space:]]*(print\(|echo )' 2>/dev/null \
 | /usr/bin/grep -E '(PASS|FAIL|MISMATCH|mismatch|GREEN|RED|✓|✗|\bOK\b|expected|EXPECT|==|!=)' \
 | sort
