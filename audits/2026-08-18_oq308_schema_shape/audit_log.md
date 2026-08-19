# OQ-308 audit log

**HEAD at OPEN:**  d0caef57  (2026-08-18)
**HEAD at CLOSE:** 6d0e020feb3e7f199faf3ddf093965e89b80553b

No PREREGISTRATION.md: the plan handed to this instance carried the step-0 prediction, and
step 0 was executed and committed (`c2aa6a67`) BEFORE any authoring. That commit is the
freeze — its message states the prediction and the measured result together, and it sits
physically before every other commit in this pass.

## Head-stamp comparison

The open stamp is the session-start commit; every commit between open and close is this
pass's own. Concurrent-writer check:

```
  6d0e020f OQ-308 commit 2: schema_shape.txt (63 rows) + arms E-H, all riding --check
  1b2f9d9c OQ-308: a goal in a clause body is not a clause head (arm F prerequisite)
  1db3ba01 OQ-308: fix the arity scanner's bracket blindness (17 phantom /0 entries)
  3d9c221c OQ-308 commit 1: one parse per file, output-preserving
  c2aa6a67 OQ-308 step 0: the pass's only falsifier, run before authoring
```

All authored by this instance. `git diff --stat` over the read-set (the checker, the
allowlist, the five corpus legs) shows no third-party change; the corpus legs were
md5-fingerprinted around the arm F decoy and were byte-identical either side.

## Substrate drift found at open

The plan's evidence was recorded at HEAD `6f7e859e`; this pass opened 42 commits later at
`d0caef57`. One of those commits (`18c31a2c`, OQ-296) added a `ROLE=helper-static` row to
the allowlist, moving the row count 116 -> 117. Checked before relying on any plan number.
