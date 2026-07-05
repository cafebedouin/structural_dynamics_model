# Twin-model cross-classification — RESULTS

Pre-registration: `audits/2026-06-13_twin_comparison/PRE_REGISTRATION.md` (committed before this run).

## Inputs

| label | commit | n_constraints | corpus_path |
|---|---|---|---|
| haiku | bbf5c92 | 960 | testsets_haiku |
| flash | bbf5c92 | 960 | testsets_flash |

**Matched intersection: n = 960** (both twins classified at commit bbf5c92).

Permutations: 1000; seed 20260613.

## H1 — structural type model-invariance (per-field; NOT aggregated)

agreement-rate over both-populated pairs; H1 holds iff Wilson-95% lower bound > permute band (95th pct). verdict & perspectives are CORRELATED (verdict folds perspectives) — not independent confirmations.

| field | both-pop | agree | disp | one-sided | rate | Wilson-95 lo | band95 | H1 |
|---|---|---|---|---|---|---|---|---|
| `verdict` | 817 | 619 | 198 | 124 | 0.758 | 0.727 | 0.660 | HOLDS |
| `persp:powerless` | 960 | 378 | 582 | 0 | 0.394 | 0.363 | 0.232 | HOLDS |
| `persp:moderate` | 960 | 566 | 394 | 0 | 0.590 | 0.558 | 0.314 | HOLDS |
| `persp:institutional` | 960 | 664 | 296 | 0 | 0.692 | 0.662 | 0.619 | HOLDS |
| `persp:analytical` | 960 | 546 | 414 | 0 | 0.569 | 0.537 | 0.330 | HOLDS |
| `signature` | 960 | 693 | 267 | 0 | 0.722 | 0.693 | 0.474 | HOLDS |
| `claimed_type` | 960 | 692 | 268 | 0 | 0.721 | 0.692 | 0.356 | HOLDS |

### Disparity exemplars (both populated, values differ)

- `verdict`:
  - `acceptable_risk_for_energy__expected_value_dominant`: haiku='green' vs flash='yellow'
  - `ai_dignity_safeguarding__posthuman_continuity_reading`: haiku='yellow' vs flash='red'
  - `ai_governance_legitimacy__technocratic_optimization_reading`: haiku='yellow' vs flash='green'
  - `ai_human_relationship__technocratic_optimization`: haiku='red' vs flash='yellow'
  - `ai_risk_governance_priority__existential_risk_reading`: haiku='yellow' vs flash='green'
- `persp:powerless`:
  - `abrahamic_covenant__isaac_covenant_reading`: haiku='tangled_rope' vs flash='snare'
  - `abrahamic_covenant__ishmael_covenant_reading`: haiku='tangled_rope' vs flash='unknown'
  - `abrahamic_covenant__land_promise_constraint`: haiku='naturalized' vs flash='snare'
  - `acceptable_risk_energy__catastrophic_tail_dominant`: haiku='tangled_rope' vs flash='snare'
  - `acceptable_risk_energy__expected_value_dominant`: haiku='tangled_rope' vs flash='naturalized'
- `persp:moderate`:
  - `abrahamic_covenant__ishmael_covenant_reading`: haiku='tangled_rope' vs flash='unknown'
  - `acceptable_risk_energy__option_value_preserving`: haiku='unknown' vs flash='tangled_rope'
  - `acceptable_risk_for_energy__catastrophic_tail_dominant`: haiku='tangled_rope' vs flash='snare'
  - `acceptable_risk_for_energy__comparative_risk_dominant`: haiku='snare' vs flash='unknown'
  - `acceptable_risk_for_energy__expected_value_dominant`: haiku='tangled_rope' vs flash='scaffold'
- `persp:institutional`:
  - `acceptable_risk_energy__expected_value_dominant`: haiku='naturalized' vs flash='rope'
  - `acceptable_risk_energy__option_value_preserving`: haiku='scaffold' vs flash='rope'
  - `acceptable_risk_for_energy__comparative_risk_dominant`: haiku='naturalized' vs flash='rope'
  - `acceptable_risk_for_energy__expected_value_dominant`: haiku='rope' vs flash='scaffold'
  - `ai_alignment_commitment__integrated_reading`: haiku='naturalized' vs flash='rope'
- `persp:analytical`:
  - `abrahamic_covenant__ishmael_covenant_reading`: haiku='snare' vs flash='unknown'
  - `acceptable_risk_energy__option_value_preserving`: haiku='unknown' vs flash='tangled_rope'
  - `acceptable_risk_for_energy__comparative_risk_dominant`: haiku='snare' vs flash='unknown'
  - `acceptable_risk_for_energy__expected_value_dominant`: haiku='tangled_rope' vs flash='scaffold'
  - `ai_alignment_commitment__ethics_justice_reading`: haiku='snare' vs flash='tangled_rope'
- `signature`:
  - `abrahamic_covenant__ishmael_covenant_reading`: haiku='constructed_high_extraction' vs flash='false_ci_rope'
  - `acceptable_risk_for_energy__comparative_risk_dominant`: haiku='constructed_high_extraction' vs flash='false_ci_rope'
  - `acceptable_risk_for_energy__expected_value_dominant`: haiku='constructed_high_extraction' vs flash='false_ci_rope'
  - `ai_alignment_priority__integrated_reading`: haiku='constructed_high_extraction' vs flash='false_ci_rope'
  - `ai_dignity_safeguarding__imago_dei_reading`: haiku='false_ci_rope' vs flash='constructed_high_extraction'
- `claimed_type`:
  - `abrahamic_covenant__isaac_covenant_reading`: haiku='tangled_rope' vs flash='snare'
  - `acceptable_risk_for_energy__comparative_risk_dominant`: haiku='tangled_rope' vs flash='rope'
  - `acceptable_risk_for_energy__expected_value_dominant`: haiku='tangled_rope' vs flash='rope'
  - `ai_alignment_commitment__safety_control_reading`: haiku='tangled_rope' vs flash='snare'
  - `ai_alignment_priority__integrated_reading`: haiku='tangled_rope' vs flash='rope'

## H2 — continuous drift (per-field)

observed mean|Δ| (haiku−flash, paired) vs permuted-Δ band; pre-registered literal: H2 holds iff observed > band95 (true pairs MORE dispersed than chance). 'below' = natural invariance tail (more similar than chance).

| field | both-numeric | obs mean\|Δ\| | band5 | band95 | tail | status |
|---|---|---|---|---|---|---|
| `theater_ratio` | 960 | 0.1984 | 0.2444 | 0.2538 | below | no |
| `chi:powerless` | 960 | 0.1755 | 0.2575 | 0.2747 | below | no |
| `chi:moderate` | 960 | 0.1219 | 0.2939 | 0.3127 | below | no |
| `chi:institutional` | 960 | 0.0746 | 0.0793 | 0.0802 | below | no |
| `chi:analytical` | 960 | 0.1479 | 0.3554 | 0.3767 | below | no |

## Validity notes

- verdict_join.verdict is the only headline verdict (OQ-98).
- verdict & perspectives are CORRELATED, not independent confirmations (verdict folds perspectives via compute_verdict/4).
- signature agreement is STRUCTURAL-coding, not detection (OQ-70).
- Per-field adjudication only; no aggregate H1 claim.
