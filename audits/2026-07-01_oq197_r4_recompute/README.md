# R4 recompute on the fixed detector — read-only, 2026-07-01

**Why (unblocking a circular hold):** R4 was "held on the detector_calibration proposal ruling,"
but that ruling was starved for exactly what R4 produces — the net-new figure on the FIXED
three-valued detector. The earlier "~3× inflation (43/53 vs 14/12 genuine)" was measured under
the PRE-fix conflation and had been retracted; the corrected number did not exist. So this run
computes it **read-only** (loading the untracked `detector_calibration.pl` purely to measure —
no wiring, no commit), inverting the sequence: recompute R4 → then the proposal ruling has a real
number. Raw: `r4_recompute_output.txt`.

**Method.** For each `detector_calibration_fires(C)` (the net-new set — `detector_calibration_due`
already excludes `already_covered` = extraction_blindness gap), split by the `gap_status` of the
extraction_blindness question under the ruled source (a, stakeholder): `gap` / `no_gap` /
`undetermined`. "Guarding correctly on undetermined" = exclude the undetermined bucket.

## Numbers

```
                net-new   gap   no_gap   undetermined   DETERMINABLE(guard)   OPERATIVE(gap-only)   INFLATION(undet)
testsets_haiku     43      14      25          4                39                   14                    4
testsets_flash     53      12      29         12                41                   12                   12
```

## Findings

1. **The proposal's "14/12 genuine" = the OPERATIVE (gap-only) bar** — net-new where
   extraction_blindness itself fires a (different-pattern, general_type_mismatch) gap. Reproduced
   exactly (14/12). It was labeled "operative" but is really "extraction_blindness FIRES," not
   "extraction_blindness could examine."

2. **The real undetermined-inflation is only 4/12, NOT ~3×.** The genuine Pattern-6 artifact — where
   extraction_blindness was *inexaminable* so "net-new" couldn't be confirmed — is just 4 haiku / 12
   flash. Guarding on undetermined, net-new = **39 / 41** (43−4, 53−12). Inflation ratio 43/39 ≈ 1.1×,
   53/41 ≈ 1.3× — **the "~3×" claim is RETRACTED.**

3. **The ~3× came from mislabeling the `no_gap` bucket (25/29) as artifact.** Those are net-new
   firings where extraction_blindness genuinely EXAMINED the constraint (≥2 seats spanning ≥2 power
   positions) and found the engine's types AGREE (no power-gradient cover-story) — yet
   detector_calibration still fired on an AUTHOR-vs-ENGINE seat disagreement (a distinct axis from
   extraction_blindness's engine-cross-power view). Extraction_blindness examined-and-cleared them;
   detector_calibration adds a different axis. That is **genuine net-new**, not inflation. The pre-fix
   analysis conflated `no_gap` (examined, agree) with `undetermined` (couldn't examine) — the exact
   collapse OQ-197 fixed — and counted both as artifact.

## Implication for the detector_calibration proposal ruling (does NOT make the ruling)

The basis shifts. The not-wire case can no longer rest on "3× inflated / mostly redundant" — that
was the conflation. On the fixed detector, net-new is **substantial (39/41 determinable)**, so per the
"contribution not clutter" test the net-new bar is met on real evidence. The remaining question is no
longer redundancy or inflation; it is the module's OWN open axes: is the detector *calibrated* (Ω_E —
external, no corpus ground truth) and what false-positive rate is *acceptable* (Ω_P — value decision).
Those are the proposal's declared open seats. The recompute removes the retracted-inflation confound so
the ruling turns on calibration, not on a bad number.

## Caveats

- `detector_calibration.pl` loaded READ-ONLY to measure; not wired, not committed. The wire/no-wire
  ruling remains the operator's, now fed a witnessed figure.
- Twins are correlated (same-seed, different-backend) — one correlated pair, not two independent
  corpora; the 39/41 should be re-measured on an independent corpus before treated as general.
- "Genuine net-new" here means "extraction_blindness examinable AND detector_calibration fires beyond
  it." Whether that author-vs-engine axis is RELIABLE (vs authoring noise) is the detector's own
  calibration Ω_E — unaddressed here, same shape as OQ-199 for the gap omega.
