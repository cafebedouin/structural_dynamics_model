# Python-side sweep + R4 coverage line (2026-07-23)

## Truthiness sweep (pasted in session; positive control: grep counts 31 purity hits in
## enhanced_report.py, the known consumer)

`grep purity_score|purity_band|effective_purity|intrinsic_purity` over `python/` (excl.
audits/archive), filtered to truthiness/`or 0`/`.get(...,0)` shapes:

- `enhanced_report.py:675` — `if purity_score is not None:` — explicit, safe.
- `query.py:276` — `if args.purity_band:` — CLI-arg truthiness, not a purity value; safe.
- `classification_confidence.py:293` — `.get("purity_band") in ("degraded","contaminated")`
  — None fails membership (existential filter); safe.
- `meta_reporter.py:84` — `if purity_scores:` — list-emptiness, built with
  `if score is not None` (`:70-73`); safe. Its `purity_avg` is over scorable rows; the
  0b `purity_n_scored/n_total` siblings it re-emits carry the denominator.
- **No `if purity_score:` value-truthiness, no `.get("purity_score", 0)`, no `or 0` found
  anywhere in live python/.**
- `query.py:292-293` — `df["purity_score"] >= args.min_purity`: NaN rows are silently
  excluded by mask semantics (correct exclusion, UNLABELED drop) — routed to the
  print-what-you-dropped follow-up OQ at close, not fixed here.

## Where null actually bites (witnessed per leg, post-C-FLOOR dumps)

`purity_score` loads as **float64** on every leg (no object-dtype column — the flag
condition is clean); JSON null → NaN; `.mean()` skipna silently computes over scorable —
now labeled by the R4 line:

```
[testsets]  n=199  NaN=46  -1.0=0 numeric=153  mean(skipna)=0.545003
[haiku]     n=960  NaN=468 -1.0=0 numeric=492  mean(skipna)=0.491555
[flash]     n=960  NaN=292 -1.0=0 numeric=668  mean(skipna)=0.571126
[kernel_v1] n=1106 NaN=4   -1.0=0 numeric=1102 mean(skipna)=0.481251
```

`-1.0` never reaches JSON (emitter routes sentinel AND unknown to null); NaN counts
decompose exactly as census sentinel+flip per leg (35+11 / 466+2 / 212+80 / 2+2).
skipna means equal the census-predicted scorable means.

## R4 coverage line (enhanced_report.py `build_header`)

Unconditional `Purity coverage: n/N scorable, M unscored (gate-fail sentinel or no-data)`
in CORPUS CONTEXT; prefers the 0b diagnostic siblings, falls back to per_constraint
counting for pre-0b outputs. Witnessed on BOTH paths (pasted):

```
0b (keys present) -> Purity coverage: 153/199 scorable, 46 unscored (...)
pre-0b (fallback) -> Purity coverage: 153/199 scorable, 46 unscored (...)
```
