#!/usr/bin/env bash
# Enumeration scan for OQ-302 Phase 0: reachable bound-`false` CALL sites of
# epistemic_access_check/2 in live engine code.
#
# Selection rule (stated so it is checkable, not a convention):
#   - .pl files under prolog/, excluding prolog/archives/
#   - line contains a call `epistemic_access_check(<arg>, false)`
#   - the line is INDENTED (>=1 leading space): a clause HEAD sits at column 0,
#     a body goal never does. This is what separates the catch-all DEFINITION
#     `epistemic_access_check(_, false).` from a bound-`false` CALL.
#   - the line is not a comment (`%`).
# /usr/bin/grep pinned, not `grep` (build_discipline: a consistency check is not a
# discrimination check -> pin the binary in any script computing a reported count).
set -u
ROOT="${1:-prolog}"
find "$ROOT" -name '*.pl' -not -path '*/archives/*' -print0 \
  | xargs -0 /usr/bin/grep -n '^[[:space:]]\+[^%]*epistemic_access_check([^,]*,[[:space:]]*false[[:space:]]*)' \
  | sort
