# CORRECTION (2026-08-20, post-review) — the `target` column was classified by the INVOCATION FLAG, not the code

**Status: the `target` column of `classification.tsv` is RETRACTED and restated below. The `shape`
column — the one the falsifier turns on — is UNAFFECTED and stands.**

`classification.tsv` is a frozen artifact (md5 `c6604cc6bd0625253788045179a6035e`) and is **not
edited**. This file is the correction record; the frozen file keeps the wrong values so the error
is recoverable.

## What was claimed

> Six of 26 rows assert nothing about the substrate: `python env st`, `omega selftest`,
> `claim cites st`, `axis boundary`, `cli selftest`, `tripwire hook`. The `axis boundary` case is
> the one worth a tripwire: the gate row runs `--selftest` only, so the live reachability sweep is
> **not in the gate**.

## What is true

**One** of 26 asserts nothing about the substrate. Five of the six named rows run a **live-substrate
negative case inside their selftest**, verified by reading the code:

| row | live-substrate arm | verified at |
|---|---|---|
| `axis boundary` | `cases[0] = ("negative (clean corpus)", PROBE, 0)` — runs `_gate(PROBE)` against the **live** allowlist and requires exit 0, i.e. **zero un-allowlisted edges on the real substrate** | `check_axis_boundary.py:84` |
| `claim cites st` | first control: *"declines on the real repository (68 live citations, all resolving)"* | `claim_cite_check.py` selftest, pasted output |
| `python env st` | `hits, _ = scan()` over the real repo; asserts `len(hits) >= 5`, `"anthropic" in hits or "numpy" in hits`, stdlib/local excluded | `python_env_check.py:178-190` |
| `cli selftest` | `build_index()` over the real `python/` tree; N>0 per physical group; `run_pipeline` resolves to its real path | `cli.py:241-251` |
| `tripwire hook` | `kss.scan()` over the **real `KNOWN_STATE.md`**; asserts `live_ticks == 0` | `pretooluse_tripwires.py:202-209` |
| `omega selftest` | **none** — `parse_entries(FIXTURE)`, fixture only | `omega_resolver.py:528` |

**Corrected targets: substrate 18, both 7, instrument-only 1.** (Was: 18 / 2 / 6.)

**And the live arm was run directly, as the check that should have preceded the claim:**

```
$ .venv/bin/python python/check_axis_boundary.py
[AXIS-GATE] 9 boundary edges: 9 allowlisted, 0 unexpected
EXIT=0
```

`AGENTS.md:652-654` and the design comment at `run_pipeline.py:1760-1769` — *"--selftest asserts
BOTH planted violations still fire AND the clean corpus passes … a new un-allowlisted cross-axis
read … HALTS the run"* — were **correct as written**. The retracted tripwire told future editors to
distrust a row that works.

## The error, named at the right altitude

This was not a slip on one row. **Every row invoked with `--selftest` was assigned
`target = instrument` from the flag, without reading whether the selftest carried a live case** —
in a pass whose own frozen criterion is *"source decides; the docstring is a separate column."*
The rule was applied to the `shape` column and not to the column added at Phase 2.

**It is a false-absence** — *"the live sweep is not in the gate"* — shipped without the positive
control the repo's own discipline requires: reading what `--selftest` actually does. The reading
that was done (`main()`'s dispatch) is one level short of the reading that decides.

**And it shipped into the worst possible channel.** It was published as a KNOWN_STATE **tripwire**,
the tier that routes to every future editor of `check_axis_boundary.py` and `scripts/gate.sh`, and
into `build_discipline.md`. A wrong assertion is checkable the moment someone looks; **a wrong
dismissal suppresses the signal before it reaches a read site** — the exact shape
`build_discipline.md` names in *A verification section may not pre-authorize dismissal of a signal
it has not re-witnessed*, committed by the pass that cites that rule.

## The finding that replaces it — which runs the other way

**This repo's selftest rows are two-sided by construction: a live-substrate negative case plus
planted positives.** Five of six do it; only `omega selftest` is fixture-only. That is §7.3's
discipline implemented, and it is the opposite of the caution originally published.

**The one real residue** is `omega selftest`: fixture-only, asserting nothing about live
`ISSUES.md`. Carried as **OQ-333** — numbered rather than left in prose; not ruled here.

## Downstream numbers corrected

- Instrument-only exposure: **67 of 695 row-days (9.6%)**, not 195 of 695 (28%). Only
  `omega selftest`'s 67 days.
- Effective substrate-watching exposure: **628 row-days over 25 rows**, not 500 over 20.
- **Falsifier B's declared power is therefore reduced only marginally by this axis**, not by the
  28% previously published. The other two limits on B — median row-exposure 7 days, and
  jurisdiction being cleanly partitioned — are unaffected and remain the operative ones.

*(For the record, the 195/28% figure was itself the second draft: the first said 207/30%, written
before the sum was run. Three successive numbers about the same quantity, two of them wrong, in the
pass's own power analysis. The invariant that would have caught all three on the first pass is the
one this walk promoted: derive the number, do not assert it.)*
