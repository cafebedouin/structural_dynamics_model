# audit_log — OQ-120 Phase 0

Append-only. Everything above the first result line was recorded BEFORE any Step B code ran.

## OPEN stamp

- **OPEN HEAD:** `f88c8c3cf4040e5994c8c1af11e40dcb6c484cf0`
- **OPEN date:** 2026-08-23
- **PREREGISTRATION.md md5:** `b181e1a2a9cd42b86d190be09f61d400` (frozen before any sweep code was written)
- **Tree at OPEN:** clean, 0 tracked modifications, in sync with `origin/main`.

## Gate baseline (S8) — OBSERVED, not predicted

**S8 as written asserts the baseline is GREEN. That is TRUE of the pristine tree and FALSE of the
tree after this audit's own Step A**, and the distinction was settled with a two-sided control
rather than assumed.

- **Pristine tree at `f88c8c3c` (before any write by this session): `GATE: GREEN`**, 28 rows,
  0 problems. Full transcript: `gate_open_baseline.txt` (captured mid-Step-A, so its two red
  rows are already the self-caused ones below).
- **After Step A's writes: 2 red rows, both self-caused and transient:**
  - `audit writeup` RED — `PROBLEM: 2026-08-21_oq120_epsilon_boundary: no WRITEUP.md entry
    point`. This dir has a PREREGISTRATION.md and no WRITEUP.md yet, by design; Step E clears it.
  - `apparatus` RED — `PROBLEM: 2026-08-21_oq120_epsilon_boundary: WRITEUP.md missing its
    **Fired:** line`. Same cause, same clearance.
  - **Control (the reason this is attribution and not a guess):** with `INVESTIGATIONS.md`
    restored to HEAD and this dir moved aside, `audit_writeup_gate --check` → `OK (204 dirs,
    36 enforced, 0 problems)` and `apparatus_instrument --check` → `GREEN`. Restoring both
    turns them red again. The reds track this audit's files, not the concurrent leg generation.
- **NOT mine, and pre-existing — surfaced, not attributed to this run:** the
  `OQ-276 WAKE: \`Fired: no\` recorded ... the deferred value ruling is RIPE; surface to the
  operator` line prints in BOTH states (6 ledger bits pristine, 7 after this audit's retro
  registration). It is a standing operator-facing signal in the pristine tree.
- The `apparatus` line's `NO DECLINE EVER RECORDED` clause also prints in the GREEN state — it
  is not the RED cause. Reading it as one would have misattributed a pre-existing condition.

## Assumed-substrate verification (S1–S13) — executed before Step B

See `substrate_check.md`.

---

## Results (nothing above this line was written after a sweep ran)

