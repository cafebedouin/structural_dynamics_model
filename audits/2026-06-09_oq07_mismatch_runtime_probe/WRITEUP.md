# OQ-07 runtime probe — hand-traced mismatch candidate verified at runtime

**Date:** 2026-06-09. **Question (ISSUES.md OQ-07):** does
`cs_drift_mismatch('72c8aa61-6909-40a1-83ef-a460510f3b82', Source)` actually succeed at
runtime, making the hand-traced `conceptual_emergence_reading` UID a live mismatch case?

## Method

- The live corpus (post-2026-06-05 rebuild) does not contain the testset. A repo-wide grep
  (`grep -rln "72c8aa61" prolog/ --include="*.pl"`) found the UID in exactly ONE file:
  `prolog/archives/datasets/kernel_test/conceptual_emergence_reading.pl` — other archive
  copies of the same-named testset carry different per-generation story_uids (UIDs are
  per-generation-event surrogates, not stable identities).
- `probe.pl` (this dir): `[stack]`, overlay `corpus_path` →
  `archives/datasets/kernel_test` (retract+asserta idiom), `load_all_testsets`
  (229 testsets), then (1) load-witness, (2) corpus-wide positive control, (3) the query,
  (4) per-conjunct decomposition on silence.

## Result (raw output: `probe_output.txt`)

```
[corpus] Loaded 229 testsets successfully.
LOADED: reading conceptual_emergence_reading carries UID 72c8aa61-6909-40a1-83ef-a460510f3b82
POSITIVE-CONTROL: cs_drift_mismatch fires on 11 UID(s) corpus-wide
OQ07-VERDICT: SILENT for 72c8aa61-6909-40a1-83ef-a460510f3b82
  foreclosure-half: trajectory=axiom_foreclosure_trajectory axiom=none
  metric-stable-half: FAILS
```

## Reading

1. **The probe is live** (positive control: 11 corpus-wide firings — the OQ-15-era audit
   reported 12 on its then-corpus; same order of magnitude on this archive).
2. **The candidate does NOT fire.** The hand-trace was half right: the foreclosure half
   holds at runtime (`cs_drift_trajectory → axiom_foreclosure`; the `cs_axiom_foreclosed`
   path does not also fire). The mismatch fails on the OTHER conjunct: `cs_is_metric_stable`
   FAILS — DR's network machinery detects drift (or above-threshold drift velocity) for
   this constraint at the default context, so it is not "metric-stable while CS-foreclosed."
3. **OQ-07 verdict: architecturally-possible-but-not-this-case.** The predicate is
   exercised end-to-end on real corpus data (11 live cases on the same load), and the
   candidate is a witnessed off-case where exactly one named conjunct blocks. The
   hand-trace's unverified assumption — that DR sees the constraint as metric-stable —
   is the part runtime falsified.

## Scope

Measured on the `kernel_test` archive (the only corpus carrying this UID), engine at the
working tree of 2026-06-09. The verdict is about THIS UID on THIS archive; the 11 positive
firings are not analyzed here beyond their role as positive control.
