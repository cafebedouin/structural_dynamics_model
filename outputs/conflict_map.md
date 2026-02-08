# Conflict Map: Perspectival Gap Analysis by Domain

*Where do analytical and powerless perspectives diverge most?*

## Global Summary

- **Total constraints**: 717
- **Consensus** (same type both perspectives): 210
- **Coordination-washing** (analyst sees benign, powerless sees extractive): 10
- **Severity amplification** (both see extraction, powerless sees worse): 44
- **Protective framing** (analyst sees worse than powerless): 157
- **Analytical blind** (analyst can't classify, powerless can): 33
- **Powerless blind** (powerless can't classify, analyst can): 263
- **Both unknown**: 0

## Data Source Summary

- **Fingerprint (engine-computed)**: 636 constraints — from shift patterns in fingerprint report, respects all metric gates
- **Corpus static (testset annotations)**: 81 constraints — from hardcoded `constraint_classification/3` facts in testset `.pl` files

> **Note:** Corpus static entries are hand-authored narrative annotations encoding how a constraint
> is *experienced* from a given perspective. They do not pass through the metric engine's gate logic
> (e.g., snare requires epsilon >= 0.46). Low-epsilon snare classifications from this source reflect
> authorial perspectival analysis, not engine-computed measurement.

## Most Common Shift Pairs (excluding consensus)

| Shift | Count | Class |
|-------|-------|-------|
| snare -> unknown | 195 | powerless_blind |
| snare -> tangled_rope | 149 | protective_framing |
| rope -> unknown | 67 | powerless_blind |
| tangled_rope -> snare | 22 | severity_amplification |
| piton -> snare | 22 | severity_amplification |
| unknown -> tangled_rope | 20 | analytical_blind |
| unknown -> mountain | 11 | analytical_blind |
| scaffold -> snare | 4 | coordination_washing |
| mountain -> snare | 3 | coordination_washing |
| tangled_rope -> mountain | 3 | protective_framing |
| scaffold -> mountain | 2 | protective_framing |
| rope -> scaffold | 1 | coordination_washing |
| mountain -> unknown | 1 | powerless_blind |
| unknown -> rope | 1 | analytical_blind |
| rope -> piton | 1 | coordination_washing |

## Domain Rankings by Perspectival Gap

Domains ranked by mean severity delta (higher = more divergent perspectives).

| Rank | Domain | Constraints | Mean Gap | Coord-Wash | Sev-Amp | Consensus |
|------|--------|-------------|----------|------------|---------|-----------|
| 1 | psychology | 1 | 5.00 | 1 | 0 | 0 |
| 2 | socio_political | 1 | 2.00 | 0 | 1 | 0 |
| 3 | astrophysical | 1 | 2.00 | 0 | 0 | 0 |
| 4 | institutional | 3 | 1.67 | 0 | 0 | 0 |
| 5 | unknown | 1 | 1.00 | 0 | 0 | 0 |
| 6 | corporate_governance | 1 | 1.00 | 0 | 0 | 0 |
| 7 | bio_industrial | 1 | 1.00 | 0 | 0 | 0 |
| 8 | psychological | 8 | 0.88 | 0 | 1 | 1 |
| 9 | philosophical | 11 | 0.82 | 0 | 1 | 2 |
| 10 | health | 5 | 0.80 | 1 | 1 | 2 |
| 11 | medical | 4 | 0.75 | 0 | 1 | 1 |
| 12 | political | 110 | 0.58 | 1 | 10 | 17 |
| 13 | social | 118 | 0.50 | 0 | 13 | 33 |
| 14 | geopolitical | 8 | 0.50 | 0 | 0 | 0 |
| 15 | economic | 117 | 0.33 | 1 | 6 | 39 |
| 16 | technological | 203 | 0.28 | 6 | 8 | 65 |
| 17 | religious | 9 | 0.22 | 0 | 1 | 3 |
| 18 | biological | 15 | 0.20 | 0 | 1 | 5 |
| 19 | organizational | 9 | 0.11 | 0 | 0 | 1 |
| 20 | scientific | 9 | 0.11 | 0 | 0 | 6 |
| 21 | legal | 8 | 0.00 | 0 | 0 | 5 |
| 22 | environmental | 5 | 0.00 | 0 | 0 | 1 |
| 23 | mathematical | 42 | 0.00 | 0 | 0 | 19 |
| 24 | logical | 2 | 0.00 | 0 | 0 | 2 |
| 25 | physics | 2 | 0.00 | 0 | 0 | 1 |
| 26 | mathematics | 2 | 0.00 | 0 | 0 | 0 |
| 27 | cognitive | 3 | 0.00 | 0 | 0 | 0 |
| 28 | logistics | 1 | 0.00 | 0 | 0 | 0 |
| 29 | informational | 2 | 0.00 | 0 | 0 | 0 |
| 30 | logistical | 1 | 0.00 | 0 | 0 | 0 |
| 31 | technology | 1 | 0.00 | 0 | 0 | 1 |
| 32 | analytical | 2 | 0.00 | 0 | 0 | 1 |
| 33 | systems_engineering | 1 | 0.00 | 0 | 0 | 0 |
| 34 | military | 2 | 0.00 | 0 | 0 | 0 |
| 35 | artistic | 2 | 0.00 | 0 | 0 | 2 |
| 36 | linguistic | 2 | 0.00 | 0 | 0 | 1 |
| 37 | infrastructure | 1 | 0.00 | 0 | 0 | 0 |
| 38 | ecological | 1 | 0.00 | 0 | 0 | 1 |
| 39 | physical | 1 | 0.00 | 0 | 0 | 0 |
| 40 | atmospheric_science | 1 | 0.00 | 0 | 0 | 1 |

## Domain Profiles (Top 10 by Gap)

### psychology (n=1, mean gap=5.00)

**Shift classes:**
- coordination_washing: 1 (100%)

**Characteristic shifts:**
- mountain -> snare: 1

**Most divergent constraints:**
- `emotional_cycles_of_change`: mountain -> snare (delta=5, eps=0.55) *

*\* = corpus_static source (testset annotation, not engine-computed)*

### socio_political (n=1, mean gap=2.00)

**Shift classes:**
- severity_amplification: 1 (100%)

**Characteristic shifts:**
- piton -> snare: 1

**Most divergent constraints:**
- `harry_potter_liberalism`: piton -> snare (delta=2, eps=0.52) *

*\* = corpus_static source (testset annotation, not engine-computed)*

### astrophysical (n=1, mean gap=2.00)

**Shift classes:**
- protective_framing: 1 (100%)

**Characteristic shifts:**
- scaffold -> mountain: 1

**Most divergent constraints:**
- `mom_z14_galaxy_2026`: scaffold -> mountain (delta=2, eps=0.05) *

*\* = corpus_static source (testset annotation, not engine-computed)*

### institutional (n=3, mean gap=1.67)

**Shift classes:**
- protective_framing: 2 (67%)
- powerless_blind: 1 (33%)

**Characteristic shifts:**
- snare -> unknown: 1
- tangled_rope -> mountain: 1
- snare -> tangled_rope: 1

**Most divergent constraints:**
- `mandatrophic_margin_collapse_diagnostic`: tangled_rope -> mountain (delta=4, eps=0.90) *
- `procedural_compliance_theater`: snare -> tangled_rope (delta=1, eps=0.58)
- `mandatrophic_margin_collapse`: snare -> unknown (delta=0, eps=0.85)

*\* = corpus_static source (testset annotation, not engine-computed)*

### unknown (n=1, mean gap=1.00)

**Shift classes:**
- protective_framing: 1 (100%)

**Characteristic shifts:**
- snare -> tangled_rope: 1

**Most divergent constraints:**
- `ai_evaluators_matching`: snare -> tangled_rope (delta=1, eps=0.75)

### corporate_governance (n=1, mean gap=1.00)

**Shift classes:**
- protective_framing: 1 (100%)

**Characteristic shifts:**
- snare -> tangled_rope: 1

**Most divergent constraints:**
- `blackstone_conflicts_of_interest`: snare -> tangled_rope (delta=1, eps=0.75)

### bio_industrial (n=1, mean gap=1.00)

**Shift classes:**
- protective_framing: 1 (100%)

**Characteristic shifts:**
- snare -> tangled_rope: 1

**Most divergent constraints:**
- `circadian_decoupling_arbitrage`: snare -> tangled_rope (delta=1, eps=0.62)

### psychological (n=8, mean gap=0.88)

**Shift classes:**
- protective_framing: 4 (50%)
- powerless_blind: 2 (25%)
- consensus: 1 (12%)
- severity_amplification: 1 (12%)

**Characteristic shifts:**
- snare -> tangled_rope: 3
- snare -> unknown: 2
- piton -> snare: 1
- scaffold -> mountain: 1

**Most divergent constraints:**
- `mirror_of_erised_expectation`: piton -> snare (delta=2, eps=0.65) *
- `self_enforced_boundary_protocol`: scaffold -> mountain (delta=2, eps=0.00) *
- `cognitive_induction_gap`: snare -> tangled_rope (delta=1, eps=0.70)
- `finite_pool_of_worry`: snare -> tangled_rope (delta=1, eps=0.75)
- `hedonic_adaptation_baseline`: snare -> tangled_rope (delta=1, eps=0.48)

*\* = corpus_static source (testset annotation, not engine-computed)*

### philosophical (n=11, mean gap=0.82)

**Shift classes:**
- powerless_blind: 4 (36%)
- protective_framing: 4 (36%)
- consensus: 2 (18%)
- severity_amplification: 1 (9%)

**Characteristic shifts:**
- snare -> unknown: 3
- snare -> tangled_rope: 3
- rope -> unknown: 1
- tangled_rope -> mountain: 1
- piton -> snare: 1

**Most divergent constraints:**
- `self_surpassing_superman`: tangled_rope -> mountain (delta=4, eps=0.80) *
- `ulysses_chp03`: piton -> snare (delta=2, eps=0.52) *
- `parable_fish_turtle`: snare -> tangled_rope (delta=1, eps=0.70)
- `stoic_logos_governance`: snare -> tangled_rope (delta=1, eps=0.75)
- `tractarian_logic_limit`: snare -> tangled_rope (delta=1, eps=0.70)

*\* = corpus_static source (testset annotation, not engine-computed)*

### health (n=5, mean gap=0.80)

**Shift classes:**
- consensus: 2 (40%)
- severity_amplification: 1 (20%)
- coordination_washing: 1 (20%)
- powerless_blind: 1 (20%)

**Characteristic shifts:**
- tangled_rope -> snare: 1
- scaffold -> snare: 1
- rope -> unknown: 1

**Most divergent constraints:**
- `fmt_oncology_realignment_2026`: scaffold -> snare (delta=3, eps=0.18) *
- `aging_well_assessment`: tangled_rope -> snare (delta=1, eps=0.52) *
- `med_diet_consensus_2026`: rope -> unknown (delta=0, eps=0.20)

*\* = corpus_static source (testset annotation, not engine-computed)*

## Coordination-Washing Hotlist

Constraints where analytical perspective sees rope/scaffold but powerless sees tangled_rope/snare.
These are the strongest candidates for "extraction narrated as coordination."

| Constraint | Domain | Analytical | Powerless | Delta | Epsilon | Source |
|------------|--------|------------|-----------|-------|---------|--------|
| emotional_cycles_of_change | psychology | mountain | snare | 5 | 0.55 | corpus_static |
| quantam_decryption_risk_2026 | technological | mountain | snare | 5 | 0.80 | corpus_static |
| trumps_second_term_authoritarianism_2026 | political | mountain | snare | 5 | 0.85 | corpus_static |
| ai_superpowers_race_2026 | technological | scaffold | snare | 3 | 0.64 | corpus_static |
| fmt_oncology_realignment_2026 | health | scaffold | snare | 3 | 0.18 | corpus_static |
| mit_tfus_consciousness_2026 | technological | scaffold | snare | 3 | 0.52 | corpus_static |
| rfc9293_interoperability | technological | scaffold | snare | 3 | 0.20 | corpus_static |
| gold_piton_2026 | economic | rope | piton | 2 | 0.20 | fingerprint |
| alternative_sovereignty_scaffold | technological | rope | scaffold | 1 | 0.22 | fingerprint |
| new_civilizational_rope | technological | mountain | rope | 1 | 0.08 | corpus_static |

> **Caveat:** 8 of the above entries are from `corpus_static` (testset annotations).
> These encode the author's perspectival analysis of how a constraint is *experienced*,
> not engine-computed gaps. Low-epsilon snare classifications from this source bypass metric gate logic.

## Severity Amplification Hotlist

Constraints where both perspectives see extraction but powerless sees it as worse.
These are domains where enforcement asymmetry is highest.

| Constraint | Domain | Analytical | Powerless | Delta | Epsilon | Source |
|------------|--------|------------|-----------|-------|---------|--------|
| agentive_optimism_2026 | political | piton | snare | 2 | 0.70 | corpus_static |
| epstein_espionage_crisis_2026 | political | piton | snare | 2 | 0.68 | corpus_static |
| epstein_kgb_honeytrap | political | piton | snare | 2 | 0.92 | corpus_static |
| harry_potter_liberalism | socio_political | piton | snare | 2 | 0.52 | corpus_static |
| mirror_of_erised_expectation | psychological | piton | snare | 2 | 0.65 | corpus_static |
| ulysses_chp01 | social | piton | snare | 2 | 0.48 | corpus_static |
| ulysses_chp02 | economic | piton | snare | 2 | 0.49 | corpus_static |
| ulysses_chp03 | philosophical | piton | snare | 2 | 0.52 | corpus_static |
| ulysses_chp04 | social | piton | snare | 2 | 0.47 | corpus_static |
| ulysses_chp05 | social | piton | snare | 2 | 0.48 | corpus_static |
| ulysses_chp07 | technological | piton | snare | 2 | 0.51 | corpus_static |
| ulysses_chp08 | social | piton | snare | 2 | 0.50 | corpus_static |
| ulysses_chp09 | social | piton | snare | 2 | 0.50 | corpus_static |
| ulysses_chp10 | social | piton | snare | 2 | 0.48 | corpus_static |
| ulysses_chp11 | social | piton | snare | 2 | 0.53 | corpus_static |
| ulysses_chp12 | social | piton | snare | 2 | 0.54 | corpus_static |
| ulysses_chp13 | social | piton | snare | 2 | 0.49 | corpus_static |
| ulysses_chp14 | biological | piton | snare | 2 | 0.55 | corpus_static |
| ulysses_chp15 | social | piton | snare | 2 | 0.58 | corpus_static |
| ulysses_chp16 | social | piton | snare | 2 | 0.47 | corpus_static |
| ulysses_chp17 | technological | piton | snare | 2 | 0.46 | corpus_static |
| ulysses_chp18 | social | piton | snare | 2 | 0.50 | corpus_static |
| CG_IsraelGaza_20231012 | political | tangled_rope | snare | 1 | 0.65 | corpus_static |
| MOLTBOT_RELIGION | technological | tangled_rope | snare | 1 | 0.60 | corpus_static |
| aging_well_assessment | health | tangled_rope | snare | 1 | 0.52 | corpus_static |
| blackstone_tax_receiveable_agreement | economic | tangled_rope | snare | 1 | 0.85 | corpus_static |
| cognitive_mimicry_arbitrage | technological | tangled_rope | snare | 1 | 0.82 | corpus_static |
| conversational_dogmas_interuption | social | tangled_rope | snare | 1 | 0.55 | corpus_static |
| dionysaic_frenzy | religious | tangled_rope | snare | 1 | 0.80 | corpus_static |
| emrgency_medicine_clinical_guidelines | medical | tangled_rope | snare | 1 | 0.60 | corpus_static |

> **Caveat:** 30 of the above entries are from `corpus_static` (testset annotations).

## Engine-Computed Gaps Only

This section filters to fingerprint-sourced data only (engine-computed shift patterns
that respect all metric gates). Compare with the global summary above to see what
testset annotations add.

### Engine-Only Shift Class Counts

- **Total constraints**: 636
- **Consensus**: 205
- **Coordination-washing**: 2
- **Severity amplification**: 0
- **Protective framing**: 149
- **Analytical blind**: 20
- **Powerless blind**: 260
- **Both unknown**: 0

### Engine-Only Domain Rankings

| Rank | Domain | Constraints | Mean Gap | Coord-Wash | Sev-Amp | Consensus |
|------|--------|-------------|----------|------------|---------|-----------|
| 1 | unknown | 1 | 1.00 | 0 | 0 | 0 |
| 2 | corporate_governance | 1 | 1.00 | 0 | 0 | 0 |
| 3 | bio_industrial | 1 | 1.00 | 0 | 0 | 0 |
| 4 | medical | 3 | 0.67 | 0 | 0 | 1 |
| 5 | geopolitical | 8 | 0.50 | 0 | 0 | 0 |
| 6 | psychological | 6 | 0.50 | 0 | 0 | 1 |
| 7 | institutional | 2 | 0.50 | 0 | 0 | 0 |
| 8 | political | 98 | 0.47 | 0 | 0 | 17 |
| 9 | philosophical | 9 | 0.33 | 0 | 0 | 2 |
| 10 | economic | 107 | 0.30 | 1 | 0 | 38 |
| 11 | social | 100 | 0.22 | 0 | 0 | 32 |
| 12 | technological | 181 | 0.18 | 1 | 0 | 64 |
| 13 | religious | 8 | 0.12 | 0 | 0 | 3 |
| 14 | organizational | 9 | 0.11 | 0 | 0 | 1 |
| 15 | scientific | 9 | 0.11 | 0 | 0 | 6 |
| 16 | biological | 13 | 0.08 | 0 | 0 | 4 |
| 17 | legal | 8 | 0.00 | 0 | 0 | 5 |
| 18 | environmental | 5 | 0.00 | 0 | 0 | 1 |
| 19 | mathematical | 38 | 0.00 | 0 | 0 | 19 |
| 20 | logical | 2 | 0.00 | 0 | 0 | 2 |
| 21 | physics | 2 | 0.00 | 0 | 0 | 1 |
| 22 | mathematics | 2 | 0.00 | 0 | 0 | 0 |
| 23 | health | 3 | 0.00 | 0 | 0 | 2 |
| 24 | cognitive | 3 | 0.00 | 0 | 0 | 0 |
| 25 | logistics | 1 | 0.00 | 0 | 0 | 0 |
| 26 | informational | 2 | 0.00 | 0 | 0 | 0 |
| 27 | logistical | 1 | 0.00 | 0 | 0 | 0 |
| 28 | technology | 1 | 0.00 | 0 | 0 | 1 |
| 29 | analytical | 2 | 0.00 | 0 | 0 | 1 |
| 30 | systems_engineering | 1 | 0.00 | 0 | 0 | 0 |
| 31 | military | 2 | 0.00 | 0 | 0 | 0 |
| 32 | artistic | 2 | 0.00 | 0 | 0 | 2 |
| 33 | linguistic | 2 | 0.00 | 0 | 0 | 1 |
| 34 | infrastructure | 1 | 0.00 | 0 | 0 | 0 |
| 35 | ecological | 1 | 0.00 | 0 | 0 | 1 |
| 36 | physical | 1 | 0.00 | 0 | 0 | 0 |

### Engine-Only Coordination-Washing Hotlist

| Constraint | Domain | Analytical | Powerless | Delta | Epsilon |
|------------|--------|------------|-----------|-------|---------|
| gold_piton_2026 | economic | rope | piton | 2 | 0.20 |
| alternative_sovereignty_scaffold | technological | rope | scaffold | 1 | 0.22 |

### Engine-Only Severity Amplification Hotlist

*None detected.*
