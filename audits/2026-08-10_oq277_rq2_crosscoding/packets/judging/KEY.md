# KEY — packet composition (do not open until the completed verdict file is committed)

| item | stratum | unit file | source_dir | incident_location |
|---|---|---|---|---|
| 1 | primary | `packets/our_units/02_blocking_gate.json` | `2026-04-14_blocking_gate` | `subject` |
| 2 | escape | `packets/escape_units/02_authoring_closure_fabricated_defaults.json` | `2026-05-30_authoring_closure_fabricated_defaults` | `subject` |
| 3 | primary | `packets/our_units/08_oq131_six_observer.json` | `2026-06-15_oq131_six_observer` | `self_audit_subsection` |
| 4 | escape | `packets/escape_units/01_spectral_laplacian.json` | `2026-02-25_spectral_laplacian` | `self_audit_subsection` |

## Seeds and method

- **primary-side draw:** seed `20260811` — random.Random(SEED); sorted pool per incident_location; r.choice(subject) then r.choice(self_audit_subsection); pool sizes {'subject': 17, 'self_audit_subsection': 5}; location match: EXACT — both escape-side incident_location values present in the primary stratum; no substitution
- **escape-side draw:** seed `20260811`, executed and recorded in `PREREGISTRATION_threshold_calibration.md` before this assembly; not re-rolled here
- **presentation order:** seed `20260811` — `random.Random(ORDER_SEED).shuffle(pool)` over the canonical (stratum, path) sort

## Scoring, pre-committed

The outcome table in `PREREGISTRATION_threshold_calibration.md` governs. The two `primary` rows are the calibration arm (known-positives: a `no-extract` on either is instrument failure and discards all four verdicts). The two `escape` rows are the candidates. Amendment 3's fifth item and its recognition flags are RETIRED — there are no recognition flags to score.
