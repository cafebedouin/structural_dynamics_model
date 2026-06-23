# OQ-112 Round 2 — the indexed completion assert (the highest-risk write), witnessed both arms

**Date:** 2026-06-22  **Corpus:** live `testsets`, LIVE=92 (pinned, see `WRITEUP.md`).
**Scope:** ONE engine write, landed ALONE with a clean revert and its own witnessed
boundary (separated-pass discipline applied to the riskiest line — operator, 2026-06-22),
before the attempt marker, the `verdict_join` wire-in, item 7, AGENTS.md, or ISSUES.md.

## Why this is the highest-risk line in Round 2

The Round-0 witness-truth control found the indexed stage's witness-truth verdict **OPEN**:
`maxent_indexed_run` (`maxent_classifier.pl:870–904`) asserted **no** completion fact, so
the control was unrunnable (nothing to control). Fixing that means *writing* a completion
witness rather than *reading* one — the single place the premature-assert Pattern 6 the
gate exists to catch can be introduced **by this fix**. So the two-arm control is not
verification-after; it is the thing that decides whether the line is correct.

## The write

`maxent_classifier.pl`, four lines, all confined to one predicate's stage:
- `:- dynamic maxent_indexed_run_info/3.` — a **DISTINCT** fact, NOT shared with
  `maxent_run_info/3`. Indexed requires a prior `maxent_run` (priors), so the classical
  witness is always present here; a shared fact could not distinguish "indexed completed"
  from "classical completed, indexed voided" — the exact Pattern 6 being gated.
- `retractall(maxent_indexed_run_info(_,_,_))` in `maxent_cleanup` and
  `retractall(maxent_indexed_run_info(Context,_,_))` in the indexed-local cleanup block —
  matching where its `maxent_indexed_dist`/`maxent_indexed_profile` siblings are cleaned.
- `get_time(TIdx), assertz(maxent_indexed_run_info(Context, NTotal, TIdx))` placed
  **strictly after** `maxent_classify_all_indexed/2` (genuine completion), mirroring
  `:555`/`:734`.

## Two-arm control — `indexed_assert_control.txt` (probe: `probe_indexed_assert_control.pl`)

```
CLEAN: maxent_run -> success ; maxent_indexed_run -> success
CLEAN: indexed_run_info PRESENT? ctx==input=...->yes  N=86  count=1   (expect yes / 86 / 1)
THROW: priors run -> success ; discovery N=87 ; THROWER at K=52
THROW: indexed_run -> error(type_error(evaluable,unknown/0)) ; indexed_run_info ABSENT, count=0  <-- GOOD
THROW why-distinct: classical maxent_run_info count=1 (PRESENT) while indexed ABSENT
THROW why-distinct: a SHARED fact would read PRESENT here -> indexed void undetectable
PHASE A compute_profiles_indexed(incl thrower) -> success
PHASE B classify_all_indexed([thrower], profiles present) -> error(type_error(evaluable,unknown/0))  (throw IS in the loop)
POST: polaris_document_status_contradictions claim-less again -- cleanup OK
```

**Both arms, same rigor as `:555`/`:734`:**
- **CLEAN (positive arm)** — assert fires on genuine completion **with the correct
  Context** (`ctx==input -> yes`), N=86, count=1. This is the arm the forced-throw cannot
  see: a correctly-positioned-but-wrongly-guarded or wrong-Context assert would pass the
  throw arm yet fail here. It passes.
- **THROW (negative arm)** — a mid-loop throw at literal index **K=52 of 87** leaves
  `maxent_indexed_run_info` **ABSENT, count=0**. So the witness cannot be asserted ahead of
  the failure it catches.
- **PHASE** — `compute_profiles_indexed` succeeds; `classify_all_indexed([thrower])` throws
  → the throw is genuinely **in the per-constraint loop**, and the assert is after it.
- **WHY-DISTINCT** — in the throw arm the classical `maxent_run_info` is PRESENT (count=1)
  while indexed is ABSENT; a shared fact would read clean. This is the empirical
  justification for the distinct-fact design.

Verdict: indexed witness-truth **PASSED** — the OPEN site from Round 0 is now closed for
`maxent_indexed_run`. All three maxent assert sites (`:555`, `:734`, indexed) now carry a
both-arms-witnessed completion fact.

## Output-neutrality (the assert is NOT yet read — "halt before the gate reads it")

```
grep -rn maxent_indexed_run_info prolog/ (excl. testsets):
  ONLY maxent_classifier.pl  (decl :78, retract :117, retract :896, assert :905)
  -> NO reader outside the producer. Gate wire-in is the next step, not this commit.
```

So pipeline output is unchanged (an unconsumed fact); no before/after `pipeline_output.json`
diff is owed. Regression smoke: `diagnostic_selftest` → **SELFTEST_PASS**; module loads
clean (`LOAD_OK`); the CLEAN arm exercised full `maxent_run` + `maxent_indexed_run` at the
normal N=86.

**Deliberate producer-lands-alone (operator-ruled).** Build Discipline Pattern 1 says wire
the consumer in the same change; here the operator ruled the opposite *for this one line* —
isolate the highest-risk write with its own clean revert and two-arm witness, then build the
gate that reads it on top of a trusted assert. The two-arm control IS the loud check.

## Next (downstream of this paste, not in this commit)

Attempt marker (`json_report.pl:72/76`, `trajectory_mining.pl:910/912`) → `verdict_join`
fail-closed wire-in (`diagnostic_summary.pl:632`, must now read all three completion facts:
`maxent_run_info` ×2 + `maxent_indexed_run_info`) → item 7 → regression test → AGENTS.md
invariant → ISSUES.md OQ-112 sync (supersede Round-1's single-falsifier wording **in the
diff**, both old and new visible — the why, W2's claim-less→claim-bearing transition being
invisible to W3, is the load-bearing finding and must not ride in as a clean overwrite).
