# Twin-model cross-classification — RESULTS

Pre-registration: `audits/2026-06-13_twin_comparison/PRE_REGISTRATION.md` (committed before this run).

## Inputs

| label | commit | n_constraints | corpus_path |
|---|---|---|---|
| haiku | 1169170 | 960 | testsets_haiku |
| flash | 1169170 | 960 | testsets_flash |
| sonnet | 1169170 | 1001 | testsets_sonnet |

**Common matched intersection (all 3 legs): n = 957** (all legs classified at commit 1169170).

Permutations: 1000; seed 20260613. Pairs crossed: flash × haiku, flash × sonnet, haiku × sonnet.

## H1 — structural type model-invariance (per-field, per-pair; NOT aggregated)

agreement-rate over both-populated pairs; H1 holds iff Wilson-95% lower bound > permute band (95th pct). Each pair runs on the COMMON intersection, so pairwise rates are directly comparable. verdict & perspectives are CORRELATED (verdict folds perspectives) — not independent confirmations.


### Pair: flash × haiku

| field | both-pop | agree | disp | one-sided | rate | Wilson-95 lo | band95 | H1 |
|---|---|---|---|---|---|---|---|---|
| `verdict` | 814 | 607 | 207 | 124 | 0.746 | 0.715 | 0.662 | HOLDS |
| `persp:powerless` | 957 | 377 | 580 | 0 | 0.394 | 0.363 | 0.229 | HOLDS |
| `persp:moderate` | 957 | 570 | 387 | 0 | 0.596 | 0.564 | 0.317 | HOLDS |
| `persp:institutional` | 957 | 670 | 287 | 0 | 0.700 | 0.670 | 0.628 | HOLDS |
| `persp:analytical` | 957 | 551 | 406 | 0 | 0.576 | 0.544 | 0.332 | HOLDS |
| `signature` | 957 | 691 | 266 | 0 | 0.722 | 0.693 | 0.473 | HOLDS |
| `claimed_type` | 957 | 689 | 268 | 0 | 0.720 | 0.691 | 0.356 | HOLDS |

_Disparity exemplars (flash × haiku):_
- `verdict`:
  - `acceptable_risk_for_energy__expected_value_dominant`: flash='yellow' vs haiku='green'
  - `ai_dignity_safeguarding__posthuman_continuity_reading`: flash='red' vs haiku='yellow'
  - `ai_governance_legitimacy__technocratic_optimization_reading`: flash='green' vs haiku='yellow'
  - `ai_human_relationship__technocratic_optimization`: flash='yellow' vs haiku='red'
  - `ai_risk_governance_priority__existential_risk_reading`: flash='green' vs haiku='yellow'
- `persp:powerless`:
  - `abrahamic_covenant__isaac_covenant_reading`: flash='snare' vs haiku='tangled_rope'
  - `abrahamic_covenant__ishmael_covenant_reading`: flash='unknown' vs haiku='tangled_rope'
  - `abrahamic_covenant__land_promise_constraint`: flash='snare' vs haiku='naturalized'
  - `acceptable_risk_energy__catastrophic_tail_dominant`: flash='snare' vs haiku='tangled_rope'
  - `acceptable_risk_energy__expected_value_dominant`: flash='naturalized' vs haiku='tangled_rope'
- `persp:moderate`:
  - `abrahamic_covenant__ishmael_covenant_reading`: flash='unknown' vs haiku='tangled_rope'
  - `acceptable_risk_energy__option_value_preserving`: flash='tangled_rope' vs haiku='unknown'
  - `acceptable_risk_for_energy__catastrophic_tail_dominant`: flash='snare' vs haiku='tangled_rope'
  - `acceptable_risk_for_energy__comparative_risk_dominant`: flash='unknown' vs haiku='snare'
  - `acceptable_risk_for_energy__expected_value_dominant`: flash='scaffold' vs haiku='tangled_rope'
- `persp:institutional`:
  - `acceptable_risk_energy__expected_value_dominant`: flash='rope' vs haiku='naturalized'
  - `acceptable_risk_energy__option_value_preserving`: flash='rope' vs haiku='scaffold'
  - `acceptable_risk_for_energy__comparative_risk_dominant`: flash='rope' vs haiku='naturalized'
  - `acceptable_risk_for_energy__expected_value_dominant`: flash='scaffold' vs haiku='rope'
  - `ai_alignment_commitment__integrated_reading`: flash='rope' vs haiku='naturalized'
- `persp:analytical`:
  - `abrahamic_covenant__ishmael_covenant_reading`: flash='unknown' vs haiku='snare'
  - `acceptable_risk_energy__option_value_preserving`: flash='tangled_rope' vs haiku='unknown'
  - `acceptable_risk_for_energy__comparative_risk_dominant`: flash='unknown' vs haiku='snare'
  - `acceptable_risk_for_energy__expected_value_dominant`: flash='scaffold' vs haiku='tangled_rope'
  - `ai_alignment_commitment__ethics_justice_reading`: flash='tangled_rope' vs haiku='snare'
- `signature`:
  - `abrahamic_covenant__ishmael_covenant_reading`: flash='false_ci_rope' vs haiku='constructed_high_extraction'
  - `acceptable_risk_for_energy__comparative_risk_dominant`: flash='false_ci_rope' vs haiku='constructed_high_extraction'
  - `acceptable_risk_for_energy__expected_value_dominant`: flash='false_ci_rope' vs haiku='constructed_high_extraction'
  - `ai_alignment_priority__integrated_reading`: flash='false_ci_rope' vs haiku='constructed_high_extraction'
  - `ai_dignity_safeguarding__imago_dei_reading`: flash='constructed_high_extraction' vs haiku='false_ci_rope'
- `claimed_type`:
  - `abrahamic_covenant__isaac_covenant_reading`: flash='snare' vs haiku='tangled_rope'
  - `acceptable_risk_for_energy__comparative_risk_dominant`: flash='rope' vs haiku='tangled_rope'
  - `acceptable_risk_for_energy__expected_value_dominant`: flash='rope' vs haiku='tangled_rope'
  - `ai_alignment_commitment__safety_control_reading`: flash='snare' vs haiku='tangled_rope'
  - `ai_alignment_priority__integrated_reading`: flash='rope' vs haiku='tangled_rope'

### Pair: flash × sonnet

| field | both-pop | agree | disp | one-sided | rate | Wilson-95 lo | band95 | H1 |
|---|---|---|---|---|---|---|---|---|
| `verdict` | 792 | 552 | 240 | 126 | 0.697 | 0.664 | 0.646 | HOLDS |
| `persp:powerless` | 957 | 362 | 595 | 0 | 0.378 | 0.348 | 0.205 | HOLDS |
| `persp:moderate` | 957 | 416 | 541 | 0 | 0.435 | 0.404 | 0.202 | HOLDS |
| `persp:institutional` | 957 | 722 | 235 | 0 | 0.754 | 0.726 | 0.666 | HOLDS |
| `persp:analytical` | 957 | 472 | 485 | 0 | 0.493 | 0.462 | 0.235 | HOLDS |
| `signature` | 957 | 767 | 190 | 0 | 0.801 | 0.775 | 0.457 | HOLDS |
| `claimed_type` | 957 | 633 | 324 | 0 | 0.661 | 0.631 | 0.376 | HOLDS |

_Disparity exemplars (flash × sonnet):_
- `verdict`:
  - `acceptable_risk_energy__option_value_preserving`: flash='green' vs sonnet='yellow'
  - `ai_alignment_commitment__ethics_justice_reading`: flash='yellow' vs sonnet='green'
  - `ai_alignment_priority__existential_risk_reading`: flash='yellow' vs sonnet='green'
  - `ai_dignity_safeguarding__posthuman_continuity_reading`: flash='red' vs sonnet='yellow'
  - `ai_risk_governance_priority__near_term_harms_reading`: flash='yellow' vs sonnet='green'
- `persp:powerless`:
  - `abrahamic_covenant__isaac_covenant_reading`: flash='snare' vs sonnet='tangled_rope'
  - `acceptable_risk_energy__catastrophic_tail_dominant`: flash='snare' vs sonnet='tangled_rope'
  - `acceptable_risk_energy__expected_value_dominant`: flash='naturalized' vs sonnet='piton'
  - `acceptable_risk_energy__option_value_preserving`: flash='tangled_rope' vs sonnet='piton'
  - `acceptable_risk_for_energy__catastrophic_tail_dominant`: flash='snare' vs sonnet='naturalized'
- `persp:moderate`:
  - `abrahamic_covenant__isaac_covenant_reading`: flash='snare' vs sonnet='tangled_rope'
  - `acceptable_risk_energy__expected_value_dominant`: flash='snare' vs sonnet='piton'
  - `acceptable_risk_energy__option_value_preserving`: flash='tangled_rope' vs sonnet='piton'
  - `acceptable_risk_for_energy__catastrophic_tail_dominant`: flash='snare' vs sonnet='tangled_rope'
  - `acceptable_risk_for_energy__comparative_risk_dominant`: flash='unknown' vs sonnet='piton'
- `persp:institutional`:
  - `acceptable_risk_energy__expected_value_dominant`: flash='rope' vs sonnet='piton'
  - `acceptable_risk_energy__option_value_preserving`: flash='rope' vs sonnet='piton'
  - `acceptable_risk_for_energy__comparative_risk_dominant`: flash='rope' vs sonnet='piton'
  - `acceptable_risk_for_energy__expected_value_dominant`: flash='scaffold' vs sonnet='piton'
  - `ai_alignment_priority__integrated_reading`: flash='rope' vs sonnet='piton'
- `persp:analytical`:
  - `abrahamic_covenant__isaac_covenant_reading`: flash='snare' vs sonnet='unknown'
  - `acceptable_risk_energy__expected_value_dominant`: flash='snare' vs sonnet='piton'
  - `acceptable_risk_energy__option_value_preserving`: flash='tangled_rope' vs sonnet='piton'
  - `acceptable_risk_for_energy__comparative_risk_dominant`: flash='unknown' vs sonnet='piton'
  - `acceptable_risk_for_energy__expected_value_dominant`: flash='scaffold' vs sonnet='piton'
- `signature`:
  - `acceptable_risk_energy__expected_value_dominant`: flash='constructed_high_extraction' vs sonnet='false_ci_rope'
  - `ai_dignity_safeguarding__imago_dei_reading`: flash='constructed_high_extraction' vs sonnet='false_ci_rope'
  - `ai_dignity_safeguarding__posthuman_continuity_reading`: flash='false_summit_mountain' vs sonnet='false_ci_rope'
  - `ai_governance_legitimacy__market_libertarian_reading`: flash='coupling_invariant_rope' vs sonnet='false_ci_rope'
  - `ai_human_relationship__incarnational_humanism`: flash='false_summit_mountain' vs sonnet='false_ci_rope'
- `claimed_type`:
  - `abrahamic_covenant__isaac_covenant_reading`: flash='snare' vs sonnet='tangled_rope'
  - `acceptable_risk_energy__option_value_preserving`: flash='rope' vs sonnet='tangled_rope'
  - `acceptable_risk_for_energy__comparative_risk_dominant`: flash='rope' vs sonnet='tangled_rope'
  - `ai_alignment_commitment__safety_control_reading`: flash='snare' vs sonnet='tangled_rope'
  - `ai_alignment_priority__integrated_reading`: flash='rope' vs sonnet='tangled_rope'

### Pair: haiku × sonnet

| field | both-pop | agree | disp | one-sided | rate | Wilson-95 lo | band95 | H1 |
|---|---|---|---|---|---|---|---|---|
| `verdict` | 792 | 556 | 236 | 146 | 0.702 | 0.669 | 0.665 | HOLDS |
| `persp:powerless` | 957 | 372 | 585 | 0 | 0.389 | 0.358 | 0.260 | HOLDS |
| `persp:moderate` | 957 | 388 | 569 | 0 | 0.405 | 0.375 | 0.231 | HOLDS |
| `persp:institutional` | 957 | 610 | 347 | 0 | 0.637 | 0.606 | 0.577 | HOLDS |
| `persp:analytical` | 957 | 430 | 527 | 0 | 0.449 | 0.418 | 0.259 | HOLDS |
| `signature` | 957 | 721 | 236 | 0 | 0.753 | 0.725 | 0.493 | HOLDS |
| `claimed_type` | 957 | 795 | 162 | 0 | 0.831 | 0.806 | 0.485 | HOLDS |

_Disparity exemplars (haiku × sonnet):_
- `verdict`:
  - `acceptable_risk_for_energy__expected_value_dominant`: haiku='green' vs sonnet='yellow'
  - `ai_alignment_commitment__ethics_justice_reading`: haiku='yellow' vs sonnet='green'
  - `ai_alignment_priority__existential_risk_reading`: haiku='yellow' vs sonnet='green'
  - `ai_governance_legitimacy__magisterial_subsidiarity_reading`: haiku='green' vs sonnet='yellow'
  - `ai_governance_legitimacy__technocratic_optimization_reading`: haiku='yellow' vs sonnet='green'
- `persp:powerless`:
  - `abrahamic_covenant__ishmael_covenant_reading`: haiku='tangled_rope' vs sonnet='unknown'
  - `abrahamic_covenant__land_promise_constraint`: haiku='naturalized' vs sonnet='snare'
  - `acceptable_risk_energy__expected_value_dominant`: haiku='tangled_rope' vs sonnet='piton'
  - `acceptable_risk_energy__option_value_preserving`: haiku='unknown' vs sonnet='piton'
  - `acceptable_risk_for_energy__catastrophic_tail_dominant`: haiku='tangled_rope' vs sonnet='naturalized'
- `persp:moderate`:
  - `abrahamic_covenant__isaac_covenant_reading`: haiku='snare' vs sonnet='tangled_rope'
  - `abrahamic_covenant__ishmael_covenant_reading`: haiku='tangled_rope' vs sonnet='unknown'
  - `acceptable_risk_energy__expected_value_dominant`: haiku='snare' vs sonnet='piton'
  - `acceptable_risk_energy__option_value_preserving`: haiku='unknown' vs sonnet='piton'
  - `acceptable_risk_for_energy__comparative_risk_dominant`: haiku='snare' vs sonnet='piton'
- `persp:institutional`:
  - `acceptable_risk_energy__expected_value_dominant`: haiku='naturalized' vs sonnet='piton'
  - `acceptable_risk_energy__option_value_preserving`: haiku='scaffold' vs sonnet='piton'
  - `acceptable_risk_for_energy__comparative_risk_dominant`: haiku='naturalized' vs sonnet='piton'
  - `acceptable_risk_for_energy__expected_value_dominant`: haiku='rope' vs sonnet='piton'
  - `ai_alignment_commitment__integrated_reading`: haiku='naturalized' vs sonnet='rope'
- `persp:analytical`:
  - `abrahamic_covenant__isaac_covenant_reading`: haiku='snare' vs sonnet='unknown'
  - `abrahamic_covenant__ishmael_covenant_reading`: haiku='snare' vs sonnet='unknown'
  - `acceptable_risk_energy__expected_value_dominant`: haiku='snare' vs sonnet='piton'
  - `acceptable_risk_energy__option_value_preserving`: haiku='unknown' vs sonnet='piton'
  - `acceptable_risk_for_energy__comparative_risk_dominant`: haiku='snare' vs sonnet='piton'
- `signature`:
  - `abrahamic_covenant__ishmael_covenant_reading`: haiku='constructed_high_extraction' vs sonnet='false_ci_rope'
  - `acceptable_risk_energy__expected_value_dominant`: haiku='constructed_high_extraction' vs sonnet='false_ci_rope'
  - `acceptable_risk_for_energy__comparative_risk_dominant`: haiku='constructed_high_extraction' vs sonnet='false_ci_rope'
  - `acceptable_risk_for_energy__expected_value_dominant`: haiku='constructed_high_extraction' vs sonnet='false_ci_rope'
  - `ai_alignment_priority__integrated_reading`: haiku='constructed_high_extraction' vs sonnet='false_ci_rope'
- `claimed_type`:
  - `acceptable_risk_energy__option_value_preserving`: haiku='rope' vs sonnet='tangled_rope'
  - `acceptable_risk_for_energy__expected_value_dominant`: haiku='tangled_rope' vs sonnet='rope'
  - `aneyoshi_land_use_prohibition__behavioral_competence_reading`: haiku='mountain' vs sonnet='rope'
  - `article17_erasure_right__censorship_mechanism_reading`: haiku='snare' vs sonnet='tangled_rope'
  - `article_51_self_defense__expansive_preventive_reading`: haiku='snare' vs sonnet='tangled_rope'

## N-way agreement partition (odd-leg localisation; missingness carried)

Over ids where EVERY leg is field-populated: unanimous / odd-one-out (exactly one leg a minority-of-one, N≥3) / split-multi (>1 leg differs). odd_leg tally is now-decidable INPUT to OQ-123/124 — NOT a localization verdict. Missingness complement carried so a low odd count is not misread as agreement when a leg dropped the field (residual (b)).

| field | in∩ | all-pop | unanim | split-multi | odd-total | odd-leg tally | missing≥1 | closure |
|---|---|---|---|---|---|---|---|---|
| `verdict` | 957 | 753 | 454 | 16 | 283 | sonnet=116, flash=87, haiku=80 | 204 | ok |
| `persp:powerless` | 957 | 957 | 187 | 220 | 550 | sonnet=190, flash=185, haiku=175 | 0 | ok |
| `persp:moderate` | 957 | 957 | 286 | 155 | 516 | sonnet=284, haiku=130, flash=102 | 0 | ok |
| `persp:institutional` | 957 | 957 | 544 | 43 | 370 | haiku=178, sonnet=126, flash=66 | 0 | ok |
| `persp:analytical` | 957 | 957 | 331 | 166 | 460 | sonnet=220, haiku=141, flash=99 | 0 | ok |
| `signature` | 957 | 957 | 632 | 42 | 283 | haiku=135, flash=89, sonnet=59 | 0 | ok |
| `claimed_type` | 957 | 957 | 590 | 20 | 347 | flash=205, sonnet=99, haiku=43 | 0 | ok |

_Missingness detail (which legs drop the field):_
- `verdict`: {'sonnet': 113, 'flash': 91, 'haiku': 71}

## H2 — continuous drift (per-field, per-pair)

observed mean|Δ| (paired) vs permuted-Δ band; pre-registered literal: H2 holds iff observed > band95 (true pairs MORE dispersed than chance). 'below' = natural invariance tail (more similar than chance).


### Pair: flash × haiku

| field | both-numeric | obs mean\|Δ\| | band5 | band95 | tail | status |
|---|---|---|---|---|---|---|
| `theater_ratio` | 957 | 0.1983 | 0.2441 | 0.2536 | below | no |
| `chi:powerless` | 957 | 0.1757 | 0.2579 | 0.2741 | below | no |
| `chi:moderate` | 957 | 0.1220 | 0.2937 | 0.3121 | below | no |
| `chi:institutional` | 957 | 0.0748 | 0.0795 | 0.0804 | below | no |
| `chi:analytical` | 957 | 0.1479 | 0.3544 | 0.3776 | below | no |

### Pair: flash × sonnet

| field | both-numeric | obs mean\|Δ\| | band5 | band95 | tail | status |
|---|---|---|---|---|---|---|
| `theater_ratio` | 957 | 0.1606 | 0.2109 | 0.2193 | below | no |
| `chi:powerless` | 957 | 0.1554 | 0.2390 | 0.2534 | below | no |
| `chi:moderate` | 957 | 0.1005 | 0.2854 | 0.3030 | below | no |
| `chi:institutional` | 957 | 0.0114 | 0.0183 | 0.0190 | below | no |
| `chi:analytical` | 957 | 0.1237 | 0.3515 | 0.3743 | below | no |

### Pair: haiku × sonnet

| field | both-numeric | obs mean\|Δ\| | band5 | band95 | tail | status |
|---|---|---|---|---|---|---|
| `theater_ratio` | 957 | 0.0945 | 0.1655 | 0.1759 | below | no |
| `chi:powerless` | 957 | 0.1579 | 0.2206 | 0.2337 | below | no |
| `chi:moderate` | 957 | 0.1112 | 0.2597 | 0.2760 | below | no |
| `chi:institutional` | 957 | 0.0750 | 0.0824 | 0.0858 | below | no |
| `chi:analytical` | 957 | 0.1311 | 0.3113 | 0.3297 | below | no |

## Validity notes

- verdict_join.verdict is the only headline verdict (OQ-98).
- verdict & perspectives are CORRELATED, not independent confirmations (verdict folds perspectives via compute_verdict/4).
- signature agreement is STRUCTURAL-coding, not detection (OQ-70).
- Per-field, per-pair adjudication only; no aggregate cross-field H1 claim.
- odd_leg tally is now-decidable INPUT to OQ-123/124, not a localization verdict — one triple does not earn 'model X is the odd one out'.
