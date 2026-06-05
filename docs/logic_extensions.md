# Deferential Realism: Logic Extensions — The Structural Physics

**Version 4.0**  
**Stages 7-9:** Boltzmann Compliance, Purity Propagation, Network Dynamics  
**Purpose:** Mathematical foundation for natural law detection and structural health measurement  
**Last Updated:** February 2026

---

## Overview

This document extends the core indexed constraint logic (Stages 1-6) with three major theoretical advances:

- **Stage 7 (Boltzmann Engine)**: Natural laws must factorize across index dimensions — independence test for NL claims
- **Stage 8 (Purity & Network)**: Structural integrity measurement and contamination propagation
- **Stage 9 (Network Drift)**: How degradation spreads through constraint networks over time

**Key Innovation:** We moved from classification ("what type is this?") to structural physics ("is this structurally sound?" and "how does contamination propagate?").

---

## Reading This Document

### Triple-Layer Format
Each concept is presented in three layers:

1. **Conceptual Overview** — Clear prose explaining the idea (your cereal choice shouldn't depend on your soap choice)
2. **Formal Specification** — Mathematical/logical definitions
3. **Implementation Reference** — Exact Prolog predicate signatures

### Prerequisites
- Read logic.md (Stages 1-6) first for core concepts
- Understand indexical relativity, power scaling, seven constraint types
- Familiarity with logic_thresholds.md for canonical parameter values

### Shadow Mode
Stages 7-9 run **alongside** the core classification system without modifying `classify_from_metrics/6`. This architectural discipline ensures:
- No regressions in core logic
- Calibration before deployment
- Backward compatibility

---

## Table of Contents

### §1. The Boltzmann Axiom: Independence as Natural Law Test
1.1 The Problem: "Physics-Washing"  
1.2 The Boltzmann Distribution & Independence  
1.3 Boltzmann Compliance Test  
1.4 False Natural Law (FNL) Detection  
1.5 Coupling-Invariant Rope (CI_Rope) Certification  
1.6 False CI_Rope (FCR) Detection  
1.7 Nonsensical Coupling  

### §2. Structural Purity: Coordination Health Measurement
2.1 The Purity Concept  
2.2 Purity Score Formula  
2.3 Purity Zones (pristine → degraded)  
2.4 Subscores & Component Analysis  
2.5 Calibration & Sensitivity  

### §3. Network Dynamics: Contamination Propagation
3.1 Why Networks Matter  
3.2 Network Discovery  
3.3 Contamination Rules  
3.4 Network Drift Dynamics (Stage 9)  
3.5 Cascade Prediction  
3.6 Network Stability Assessment  

### §4. Lifecycle Extensions: Drift Types 8-11
4.1 Type 8: Coupling Drift  
4.2 Type 9: Boltzmann Floor Drift  
4.3 Type 10: Purity Drift  
4.4 Type 11: Network Drift  

### §5. Action Layer Extensions
5.1 Purity-Qualified Actions  
5.2 Energy Scaling by Purity  
5.3 Composition Gates  
5.4 Network-Qualified Actions  

### §6. Integration Architecture
6.1 Module Dependency Graph  
6.2 Two-Regime System  
6.3 Shadow Mode Design  
6.4 Implementation Cross-Reference  
6.5 Known Limitations  

---

## §1. The Boltzmann Axiom: Independence as Natural Law Test

### 1.1 The Problem: "Physics-Washing"

**Conceptual Overview:**

Your cereal choice shouldn't depend on your soap choice. If someone tells you "It's a law of nature that Lucky Charms eaters must use Dove soap," you'd smell a rat. Why? Because breakfast and hygiene are **independent dimensions** of human life. A constraint that couples them is constructed, not natural.

This intuition generalizes. Natural laws don't "senselessly couple" independent variables. If a constraint's classification depends on the product of two independent index dimensions in a non-factorizable way, it's not a natural law — it's a constructed constraint masquerading as one.

We call this **physics-washing**: claiming unchangeability by invoking nature, when the coupling topology reveals human construction.

**The Core Insight:**

Mountains (■) claim to be natural laws. But if:
```
Classification(C, Power=powerless, Scope=local) = Mountain
Classification(C, Power=powerless, Scope=global) = Snare
Classification(C, Power=institutional, Scope=local) = Rope
```

...then **C couples Power and Scope** in a way that natural laws can't. This is the signature of a constructed constraint with scope-dependent extraction.

**Examples:**
- **True Mountain**: Thermodynamics. Entropy increases at all power levels, all scopes. Factorizes perfectly.
- **Physics-washed Snare**: "Natural market forces" that somehow only constrain the powerless at global scale. Coupling reveals construction.

---

### 1.2 The Boltzmann Distribution & Independence

**Formal Foundation:**

In statistical mechanics, the Boltzmann distribution is **the only distribution** that correctly describes systems of independent particles. Tamuz & Sandomirskiy (2025) proved this uniqueness theorem.

**Translation to DR:**

If a constraint is a genuine natural law, its classification across index dimensions must behave like independent particles in a gas:
- Changing **Power** should have the same effect at all **Scope** levels
- Changing **Scope** should have the same effect at all **Power** levels
- The joint distribution should **factorize**: χ(P,S) ≈ f(P) × g(S)

**Formal Definition:**

```
Boltzmann_Compliant(C) ≡ 
    ∀(P₁, P₂, S₁, S₂) ∈ Index_Grid:
        Type(C, P₁, S₁) = Type(C, P₁, S₂)
        ∧ Type(C, P₁, S₁) = Type(C, P₂, S₁)
        ∧ χ(C, P, S) ≈ f_P(P) × f_S(S)  [within tolerance ε]
```

Where:
- `Index_Grid` = test points across Power × Scope space
- `f_P`, `f_S` are functions of single variables (factorization)
- `ε` = `boltzmann_factorization_tolerance` = 0.10

**Implementation:**
```prolog
% boltzmann_compliance.pl
boltzmann_compliant(C, Result) :-
    (   epistemic_access_check(C, false)
    ->  Result = inconclusive(insufficient_classifications)
    ;   cross_index_coupling(C, CouplingScore),
        complexity_adjusted_threshold(C, Threshold),
        (   CouplingScore =< Threshold
        ->  Result = compliant(CouplingScore)
        ;   Result = non_compliant(CouplingScore, Threshold)
        )
    ).
```

---

### 1.3 Boltzmann Compliance Test

**The "Sicherman Dice" Test:**

Named after the only pair of dice that produce the same distribution as standard dice but with different faces. Just as Sicherman dice reveal when a distribution factorizes, our test reveals when classification factorizes.

**Algorithm:**

1. **Build Grid**: Classify C at all combinations of (Power, Scope)
   - Powers: `[powerless, moderate, institutional, analytical]`
   - Scopes: `[local, national, global]`
   - Grid size: 4 × 3 = 12 classifications

2. **Count Violations**: For each dimension pair (P₁, P₂) at different scopes:
   - If `Type(C, P₁, S₁) ≠ Type(C, P₁, S₂)` but `Type(C, P₂, S₁) = Type(C, P₂, S₂)`
   - This is a **coupling violation** (one power level sees scope-dependence, another doesn't)

3. **Compute Coupling Score**:
   ```
   CouplingScore = Violations / MaxPossibleViolations
   ```
   - Range: [0.0, 1.0]
   - 0.0 = perfectly factorized (independent)
   - 1.0 = maximally coupled

4. **Apply Threshold**:
   - If `CouplingScore ≤ boltzmann_coupling_threshold` (0.25) → **compliant**
   - If `CouplingScore > boltzmann_coupling_threshold` → **non_compliant**

**Edge Case: Complexity Offset**

Not all coupling is extractive. A global power grid **must** couple more dimensions than "drive on right" convention. We adjust the threshold for inherently complex coordination:

```
effective_threshold = base_threshold + complexity_offset(coordination_type)
```

**Canonical `coordination_type/2` values (the authoritative six; schema
`$defs.CoordinationType.enum` and `linter.py` rule 15 must match this list):**

| Value | Definition | complexity_offset | boltzmann_floor |
|---|---|---|---|
| `information_standard` | Naming conventions, encoding standards, measurement units, protocols — minimal complexity, minimal inherent cost | +0.00 | 0.02 |
| `attachment_coordination` | Emotional bonds, caregiving norms, kinship obligations, alliance commitments — structurally simple (dyadic/small-group) but needing continuous maintenance | +0.04 | 0.08 |
| `resource_allocation` | Markets, distribution mechanisms, multi-party resource sharing — moderate complexity, significant inherent transaction costs | +0.05 | 0.15 |
| `identity_coordination` | Group membership, licensing, national identity, reputation systems — boundary maintenance and membership claims against evolving criteria | +0.04 | 0.08 |
| `enforcement_mechanism` | Legal systems, regulatory frameworks, governance structures — requires dedicated enforcement infrastructure | +0.08 | 0.10 |
| `global_infrastructure` | Planetary-scale coordination: power grids, internet protocols, global supply chains — maximum complexity, maximum inherent cost | +0.15 | 0.20 |

(The generator prompt's coordination-type selection table is derived from this one; one
constraint declares exactly ONE coordination_type — decompose multi-function constraints.)

Values live in `config.pl:362-383` (`complexity_offset_*`, `boltzmann_floor_*`); floors are
provisional pending calibration, overridable per-testset via `boltzmann_floor_override/2`.

**Classification-activity asymmetry (witnessed, OQ-30):** the two levers are NOT
equivalent. The **offset is signature-layer-ACTIVE** — it adds into
`complexity_adjusted_threshold` and can flip `boltzmann_compliant` and hence the final
`dr_type` (48 witnessed flips). The **floor is classification-INACTIVE** — it only shifts
the `excess_extraction` metric and leaves `dr_type` unchanged (witnessed: `abolition_reading`
holds `tangled_rope` across floor 0.0→0.99). When coordination_type is absent, the engine
silently falls back to `complexity_offset_default = 0.00` / `boltzmann_floor_default = 0.05`
(fabricated-default pattern; open as OQ-41).

**Edge Case: Epistemic Access**

If fewer than `boltzmann_min_classifications` (3) exist, the test returns `inconclusive(insufficient_classifications)` rather than failing. Can't test factorization without data.

**Implementation:**
```prolog
% boltzmann_compliance.pl
cross_index_coupling(C, CouplingScore) :-
    coupling_test_powers(Powers),
    coupling_test_scopes(Scopes),
    findall(classified(P, S, Type),
            (member(P, Powers), member(S, Scopes),
             coupling_test_context(P, S, Ctx),
             classify_at_context(C, Ctx, Type)),
            Grid),
    count_coupling_violations(Grid, Powers, Scopes, Violations),
    % ... compute score from violations ...
```

**Parameters (logic_thresholds.md §5a):**
- `boltzmann_coupling_threshold` = 0.25 *(Updated February 2026 to match calibrated config.pl value; see §4.1 for calibration rationale.)*
- `boltzmann_coupling_strong_threshold` = 0.50
- `boltzmann_factorization_tolerance` = 0.10
- `boltzmann_min_classifications` = 3

---

### 1.4 False Natural Law (FNL) Detection

**Conceptual Overview:**

A **False Natural Law** is a constraint that:
1. Claims to be a Mountain (unchangeable natural law)
2. But fails the Boltzmann independence test

This is the formal signature of **physics-washing**: using nature rhetoric to hide constructed constraints.

**Formal Definition:**

```
FNL(C) ≡
    Claimed(■C)
    ∧ boltzmann_compliant(C, non_compliant(Score, Threshold))
    ∧ Score > Threshold
```

**Note:** The formal definition does not gate on `ε(C)`. Any constraint claiming naturality that fails Boltzmann compliance is flagged as FNL regardless of extraction level. Excess extraction is gathered as diagnostic evidence (in `ExcessExtraction` field of the evidence package) but is not a gating condition.

**Detection Logic:**

The FNL detector checks:
1. Is C claimed as Mountain (either explicit claim or indexed classification)?
2. Does C fail Boltzmann compliance (coupling score > threshold)?

If both: **physics-washed constraint detected**.

**Evidence Package:**

When FNL fires, it returns:
```
fnl_evidence(
    ClaimType,           % mountain | indexed_mountain
    BoltzmannResult,     % non_compliant(Score, Threshold)
    CouplingScore,       % 0.0-1.0
    CoupledPairs,        % [(P₁,S₁), (P₂,S₂), ...] 
    ExcessExtraction     % ε - BoltzmannFloor
)
```

**Override Behavior:**

FNL overrides metric-based classification:
- **Before**: `classify_from_metrics/6` → Mountain
- **FNL fires**: Signature override
- **After**: Final type → Tangled Rope (conservative: retains coordination possibility)

**Why Tangled Rope?**

We don't jump straight to Snare because the constraint might have genuine coordination function. Tangled Rope is the safe fallback: "this coordinates something but extracts asymmetrically."

**Implementation:**
```prolog
% signature_detection.pl
false_natural_law(C, fnl_evidence(Claim, BoltzmannResult, CouplingScore,
                                   CoupledPairs, ExcessExtraction)) :-
    claimed_natural(C, Claim),
    boltzmann_compliant(C, BoltzmannResult),
    BoltzmannResult = non_compliant(_, _),
    cross_index_coupling(C, CouplingScore),
    (   detect_nonsensical_coupling(C, CoupledPairs, _)
    ->  true
    ;   CoupledPairs = []
    ),
    (   excess_extraction(C, ExcessExtraction)
    ->  true
    ;   ExcessExtraction = unknown
    ).

% Priority in constraint_signature/2
constraint_signature(C, false_natural_law) :-
    false_natural_law(C, _), !.
```

**Key implementation details:**
- CouplingScore is obtained via a separate `cross_index_coupling/2` call, not extracted from the BoltzmannResult term
- `detect_nonsensical_coupling/3` and `excess_extraction/2` use soft fallbacks (`-> true ; default`): if either is unavailable (e.g., insufficient data), FNL still fires with default values (`CoupledPairs = []`, `ExcessExtraction = unknown`) rather than failing

**Real-World Pattern:**

"It's just human nature to compete" — claimed as Mountain, but:
- Cooperation levels vary by scope (local communes vs global markets)
- Power position dramatically changes whether "competition" is opportunity or trap
- Coupling topology reveals construction, not natural law

**Parameters:**
- Detection relies on `boltzmann_coupling_threshold` (0.25)
- Excess extraction uses `boltzmann_floor_*` values

---

### 1.5 Coupling-Invariant Rope (CI_Rope) Certification

**Conceptual Overview:**

A **Coupling-Invariant Rope** is the positive mirror of FNL. It's a constraint that:
1. Provides genuine coordination
2. Passes all four Boltzmann invariance tests
3. Has zero or minimal excess extraction

This is **certified true coordination** — not just low-extraction construction, but structurally pure mechanism.

**The Four Tests:**

```
CI_Rope(C) ≡ 
    boltzmann_compliant(C, compliant(_))          [Test 1: Factorization]
    ∧ scale_invariant(C)                           [Test 2: Scope invariance]
    ∧ excess_extraction(C, X) ∧ X ≈ 0             [Test 3: At Boltzmann floor]
    ∧ Coord(C)                                     [Test 4: Coordination function]
```

**Test 1: Factorization**
- Classification factorizes across Power × Scope
- No nonsensical coupling
- Coupling score ≤ 0.15

**Test 2: Scope Invariance**
- Constraint type stable across scope changes
- If Rope at local, also Rope at global
- Natural coordination doesn't become extractive at scale

**Test 3: Excess Extraction**
- ε(C) ≤ BoltzmannFloor(coordination_type) + tolerance
- Any extraction is "necessary cost" not extractive overhead
- Price of Anarchy at theoretical minimum

**Test 4: Coordination Function**
- Has genuine coordination benefit
- Not just absence of extraction
- Provides value to participants

**Evidence Package:**

```
ci_rope_evidence(
    ComplianceResult,    % compliant(Score)
    ScopeInvariance,     % invariant | variant(UniqueTypes)
    ExcessExtraction,    % ε - Floor
    CoordFunction        % true | false
)
```

**Override Behavior:**

CI_Rope **promotes** constraints:
- **Before**: Tangled Rope (moderate extraction detected)
- **CI_Rope fires**: All four tests pass
- **After**: Rope (certified pure coordination)

**Why This Matters:**

Distinguishes:
- **Low-extraction construction** (metrics look good, but structurally suspect)
- **True coordination** (mathematically certified structural purity)

The difference between "nudge architecture that happens to benefit you" and "genuinely symmetric standard."

**Implementation:**
```prolog
% signature_detection.pl
coupling_invariant_rope(C, ci_rope_evidence(Compliance, ScopeResult,
                                             ExcessEps, CoordFn)) :-
    boltzmann_compliant(C, Compliance),
    Compliance = compliant(_),
    scope_invariance_test(C, ScopeResult),
    ScopeResult = invariant,
    (   excess_extraction(C, ExcessEps)
    ->  ExcessEps =< 0.05  % Within noise floor of Boltzmann floor
    ;   ExcessEps = 0.0
    ),
    narrative_ontology:has_coordination_function(C),
    CoordFn = true.

% Priority in constraint_signature/2 (after FNL, before FCR)
constraint_signature(C, coupling_invariant_rope) :-
    coupling_invariant_rope(C, _), !.
```

**Note:** The excess extraction tolerance is **0.05** (matching the implementation in `signature_detection.pl`), not 0.10 as previously documented.

**Canonical Example:**

**UTF-8 Encoding Standard**
- Factorizes: Same benefits at all power levels, all scopes
- Scope-invariant: Works identically at local and global scale
- Minimal excess: ε ≈ 0.02 (learning cost only, no extractive overhead)
- Coordinates: Enables universal text representation

→ **Certified CI_Rope**

**Parameters:**
- Uses `boltzmann_coupling_threshold` (0.25)
- Excess tolerance: 0.05 (within noise floor of Boltzmann floor)
- Boltzmann floor varies by coordination type

---

### 1.6 False CI_Rope (FCR) Detection

**Conceptual Overview:**

A **False CI_Rope** is a constraint that:
1. **Appears** to be a Rope from metrics (low χ, low ε)
2. But **fails** Boltzmann structural tests

This is **coordination-washing**: hiding extraction behind:
- Distributed enforcement (no single point of suppression)
- Behavioral defaults (nudge architecture)
- Metric manipulation (keeping ε artificially low)
- Clever coupling (extraction hidden in scope interactions)

**Formal Definition:**

```
FCR(C) ≡ 
    appears_as_rope(C)
    ∧ (¬boltzmann_compliant(C)
       ∨ ¬scale_invariant(C)
       ∨ excess_extraction(C, X) ∧ X > threshold
       ∨ detect_nonsensical_coupling(C, _, _))
```

**The Four Failure Modes:**

1. **Boltzmann non-compliance**: Coupling score > 0.15
2. **Scope variance**: Type changes across scopes (Rope → Tangled → Snare)
3. **Excess extraction**: ε significantly above Boltzmann floor
4. **Nonsensical coupling**: Couples dimensions without functional justification

**Detection Logic:**

```
appears_as_rope(C) ← 
    explicit_claim(rope)
    ∨ indexed_classification(rope)
    ∨ (low_extraction_profile(C) ∧ ¬only_mountain_classifications(C))
```

Then collect failures:
```
collect_fcr_failures(C) → [FailedTests]
```

If any test fails → **coordination-washed constraint detected**.

**Evidence Package:**

```
fcr_evidence(
    AppearanceType,      % claimed_rope | indexed_rope | low_extraction
    FailedTests,         % [boltzmann | scope | excess | coupling]
    CouplingScore,       % If Boltzmann failed
    ScopeViolations,     % [(S₁, Type₁), (S₂, Type₂)]
    ExcessExtraction     % ε - Floor
)
```

**Override Behavior:**

FCR **downgrades** constraints:
- **Before**: Rope (metrics look clean)
- **FCR fires**: Structural tests fail
- **After**: Tangled Rope (hidden extraction detected)

**Why This Matters:**

Catches modern extraction techniques:
- **Behavioral defaults** (opt-out instead of opt-in)
- **Privacy dark patterns** (low ε because no single suppression point)
- **Scope-dependent extraction** (free at local scale, extractive at global)
- **Asymmetric network effects** (benefits early adopters, traps late joiners)

**Implementation:**
```prolog
% signature_detection.pl
false_ci_rope(C, fcr_evidence(AppType, FailedTests, CouplingScore,
                               ScopeViolations, ExcessEps)) :-
    appears_as_rope(C, AppType),
    collect_fcr_failures(C, FailedTests),
    FailedTests \= [],  % At least one failure
    % ... collect evidence ...

% Priority in constraint_signature/2 (after FNL, before natural_law v6.1)
constraint_signature(C, false_ci_rope) :-
    false_ci_rope(C, _), !.

% Natural Law via Emergence (v6.1) — after FCR, before CI_Rope
% Intercepts natural laws with incidental beneficiaries (e.g., P!=NP)
% that would otherwise be caught by CI_Rope due to constraint_beneficiary
% declarations used for perspectival analysis.
constraint_signature(C, natural_law) :-
    domain_priors:emerges_naturally(C),
    get_constraint_profile(C, Profile),
    natural_law_signature(Profile), !.
```

> **v6.1 Addition:** The `natural_law` clause in the priority chain prevents natural laws with incidental beneficiaries from being intercepted by CI_Rope. A constraint that `emerges_naturally` AND passes the full NL profile check is certified as `natural_law` regardless of coordination-function status. Without this, constraints like P!=NP (which have `constraint_beneficiary` declarations for perspectival analysis) would be misclassified as rope.

**Real-World Pattern:**

**"Free" Social Media Platform**
- Appears as Rope: Low monetary cost, voluntary adoption, no suppression
- But fails:
  - **Boltzmann**: Couples user power with scope (harmless locally, extractive globally)
  - **Scope invariance**: Rope at small scale, Snare at billion-user scale
  - **Excess extraction**: Attention harvesting >> coordination value
  - **Nonsensical coupling**: Why does your messaging app need your location history?

→ **Coordination-washed Snare**

**Parameters:**
- Uses same Boltzmann thresholds as CI_Rope
- `appears_as_rope` checks `rope_chi_ceiling` (0.35)
- Excess extraction relative to `boltzmann_floor_*`

---

### 1.7 Nonsensical Coupling

**Conceptual Overview:**

Some coupling is **functional** (global power grid must coordinate local + regional + national grids). Some coupling is **nonsensical** (breakfast cereal shouldn't determine soap brand).

Nonsensical coupling is the signature of extraction hiding behind complexity.

**Formal Definition:**

```
NonsensicalCoupling(C, (D₁, D₂)) ≡ 
    Coupled(C, D₁, D₂)
    ∧ ¬FunctionalJustification(C, D₁, D₂)
    ∧ Strength(Coupling) > threshold
```

**Detection Logic:**

For each coupled pair `(P, S)` from Boltzmann test:
1. Check if coordination function justifies coupling
2. Check if coupling strength exceeds threshold
3. If strong coupling without justification → nonsensical

**Functional Justifications:**

- **Global infrastructure**: Scope naturally couples with power (scale matters)
- **Resource allocation**: Scope couples with power (distribution logistics)
- **Enforcement mechanism**: Power couples with scope (jurisdictional range)

**Nonsensical Patterns:**

- **Power × Time**: Why does your power position change the time horizon relevant to a constraint?
- **Scope × Exit**: Why does global scope eliminate exit options that existed locally?
- **Power × Suppression (unusual)**: Why does suppression vary by power for a "natural law"?

**Implementation:**
```prolog
% boltzmann_compliance.pl
detect_nonsensical_coupling(C, CoupledPairs, Strength) :-
    coupling_test_powers(Powers),
    coupling_test_scopes(Scopes),
    findall(
        classified(P, S, Type),
        (   member(P, Powers), member(S, Scopes),
            coupling_test_context(P, S, Ctx),
            classify_at_context(C, Ctx, Type)
        ),
        Grid
    ),
    findall(
        coupled(power_scope, P, ScopePair, Score),
        (   member(P, Powers),
            member(S1, Scopes), member(S2, Scopes),
            S1 @< S2,
            member(classified(P, S1, T1), Grid),
            member(classified(P, S2, T2), Grid),
            T1 \= T2,
            ScopePair = S1-S2,
            Score = 1.0
        ),
        CoupledPairs
    ),
    (   CoupledPairs = []
    ->  Strength = 0.0
    ;   length(CoupledPairs, N),
        length(Powers, NP), length(Scopes, NS),
        MaxPairs is NP * (NS * (NS - 1)) // 2,
        (MaxPairs > 0 -> Strength is min(1.0, N / MaxPairs) ; Strength = 0.0)
    ).

% Used in FNL and FCR evidence
```

**Note:** The actual implementation builds a Power × Scope classification grid and finds scope-variance pairs — power levels where classification changes across scopes. Strength is computed as the ratio of coupled pairs to maximum possible pairs. There is no separate "nonsensical vs functional" filtering step; all scope-variant pairs are reported.

**Why This Matters:**

Nonsensical coupling is **extractive complexity**. It's how constraints hide:
- Scope-dependent extraction
- Power-dependent traps
- Coupling that serves extraction, not coordination

If your cereal choice determines your soap brand, someone's making money off that coupling.

**Parameters:**
- Strong coupling threshold: 0.50
- Functional justification checked via coordination_type/2

---

## §2. Structural Purity: Coordination Health Measurement

### 2.1 The Purity Concept

**Conceptual Overview:**

Imagine a river. A **pure** river flows clear, supports life, and maintains its course. A **contaminated** river carries pollutants, kills fish, and erodes unpredictably.

Constraints are the same. **Structural purity** measures how "clean" a constraint is:
- Does it factorize (Boltzmann compliance)?
- Does it work the same at all scales (scope invariance)?
- Does it couple only what must be coupled (coupling cleanliness)?
- Does it extract only what coordination requires (excess extraction)?

Purity is a **continuous health metric**: [0, 1] where:
- **1.0 (pristine)**: Perfect coordination, zero extractive overhead
- **0.7 (sound)**: Good coordination, minor imperfections
- **0.5 (borderline)**: Coordination exists but extraction rising
- **0.3 (contaminated)**: Extraction dominant, coordination degrading
- **0.0 (degraded)**: Pure extraction, no coordination value

**Why Continuous?**

The seven types (Mountain, Rope, Tangled Rope, Snare, Scaffold, Piton, Naturalized) are **categorical** — you're either in or out. Purity is **continuous** — you can track degradation over time.

This enables:
- **Early warning**: Detect drift before type flips
- **Precise comparison**: "Constraint A is healthier than B"
- **Reform targeting**: "We need to improve purity by 0.2 to make this reformable"

**The Core Insight:**

Purity measures **how close to theoretical ideal** a constraint operates. A Rope at purity 0.9 is near-ideal coordination. A Rope at purity 0.4 is degrading toward Tangled Rope — still coordinates, but extraction rising.

---

### 2.2 Purity Score Formula

**Formal Definition:**

```
purity_score(C) = w₁·F(C) + w₂·S(C) + w₃·K(C) + w₄·E(C)
```

Where:
- **F(C)** = Factorization score = `1.0 - coupling_score(C)`
- **S(C)** = Scope invariance score = `1.0 - scope_variance_penalty(C)`
- **K(C)** = Coupling cleanliness = `1.0 - nonsensical_coupling_strength(C)`
- **E(C)** = Excess extraction decay = `max(0.0, 1.0 - min(1.0, excess_extraction(C) × 2.0))`

**Weights** (from theoretical considerations):
- w₁ = 0.30 (Factorization — most fundamental)
- w₂ = 0.25 (Scope invariance — scale stability)
- w₃ = 0.25 (Coupling cleanliness — structural integrity)
- w₄ = 0.20 (Excess extraction — Price of Anarchy)

**Subscore Details:**

**F(C): Factorization** (30%)
```
F(C) = 1.0 - cross_index_coupling(C)
```
- 1.0 = perfectly factorized (independent dimensions)
- 0.0 = maximally coupled

**S(C): Scope Invariance** (25%)
```
S(C) = 1.0 - (N_types - 1) · penalty
```
Where:
- N_types = distinct classifications across scopes
- penalty = 0.25 per extra type
- If Rope everywhere: N_types=1, S=1.0
- If 3 types (Rope/Tangled/Snare): N_types=3, S=0.5

**K(C): Coupling Cleanliness** (25%)
```
K(C) = 1.0 - nonsensical_coupling_strength(C)
```
- Filters out functional coupling (justified by coordination type)
- Only penalizes coupling without justification
- 1.0 = zero nonsensical coupling
- 0.0 = maximal nonsensical coupling

**E(C): Excess Extraction Decay** (20%)
```
E(C) = max(0.0, 1.0 - min(1.0, excess_extraction(C) × 2.0))
```
Where:
- excess_extraction(C) = ε(C) - BoltzmannFloor(coordination_type)
- This is a **clamped linear decay**, not exponential
- At floor (0): E = 1.0
- At 0.25 excess: E = 0.50
- At 0.5 excess: E = 0.0 (fully penalized)
- Linear decay reaches zero at excess = 0.5, rather than the asymptotic approach of an exponential

**Sentinel Value:**

`purity_score(C) = -1.0` indicates **insufficient epistemic data**:
- < 3 indexed classifications (can't test Boltzmann)
- Missing coordination_type (can't compute Boltzmann floor)
- Missing structural predicates

**Implementation:**
```prolog
% purity_scoring.pl (actual clause shape: positive access check + cut,
% sentinel as fallback clause; result clamped to [0,1])
purity_score(C, Score) :-
    epistemic_access_check(C, true),
    !,
    factorization_subscore(C, F),
    scope_invariance_subscore(C, S),
    coupling_cleanliness_subscore(C, K),
    excess_extraction_subscore(C, E),
    RawScore is (0.30 * F) + (0.25 * S) + (0.25 * K) + (0.20 * E),
    Score is min(1.0, max(0.0, RawScore)).
purity_score(_, -1.0).   % insufficient epistemic data
```

**Parameters:**
- Weights: 30/25/25/20 (theoretical, not fitted)
- Decay rate λ = 2.0
- Scope penalty = 0.25 per type

---

### 2.3 Purity Zones

**Conceptual Overview:**

Like water quality standards, purity scores map to named zones that guide action:

| Zone | Range | Status | Action Implication |
|------|-------|--------|-------------------|
| **Pristine** | ≥ 0.9 | Exemplary coordination | Maintain, study, replicate |
| **Sound** | ≥ 0.7 | Healthy coordination | Monitor, minor tuning |
| **Borderline** | ≥ 0.5 | Acceptable but watch | Increased monitoring, consider reform |
| **Contaminated** | ≥ 0.3 | Degrading coordination | Reform urgently or prepare exit |
| **Degraded** | < 0.3 | Extraction-dominant | **Reform blocked** — cut/exit only |

**The 0.3 Threshold (Critical):**

Purity < 0.3 is the **composition gate** for surgical reform:
```
purity_surgical_reform_gate = 0.30
```

Below this, the constraint is too corrupted for targeted reform. Attempting reform:
- Wastes energy (high cost, low success probability)
- Risks making it worse (partial reforms entrench extraction)
- Better to cut or exit and build fresh

**Why This Matters:**

Answers the strategic question: **"Can this be fixed, or should we abandon it?"**

Purity provides the quantitative answer. At 0.35, maybe. At 0.25, no.

**Implementation:**
```prolog
% signature_detection.pl
% NOTE: epistemic_access_check/2 must be called with an UNBOUND second argument —
% calling it with `false` bound always succeeds via its catch-all clause
% (bound-probe bypasses clause-order; see audits/2026-06-03_purity/purity_audit_20260603.md §2).
structural_purity(C, inconclusive) :-
    epistemic_access_check(C, Access),
    Access == false, !.

structural_purity(C, PurityClass) :-
    purity_test_factorization(C, T1),
    purity_test_scope_invariance(C, T2),
    purity_test_coupling(C, T3),
    purity_test_excess(C, T4),
    Tests = [T1, T2, T3, T4],
    include(is_failure, Tests, Failures),
    (   Failures = []
    ->  determine_pure_subtype(C, PurityClass)
    ;   PurityClass = contaminated(Failures)
    ).

% PurityClass is one of:
%   pure_natural_law      — NL signature + all four tests pass
%   pure_coordination     — CI_Rope signature + all four tests pass
%   pure_scaffold         — has sunset clause + all four tests pass
%   contaminated(Reasons) — one or more tests fail (Reasons = list of failures)
%   inconclusive          — insufficient data for reliable test
```

**Note:** `structural_purity/2` returns structural subtypes (not numeric purity zones). The numeric purity zones (pristine, sound, borderline, contaminated, degraded) are derived from `purity_score/2` values in `purity_scoring.pl`. The two are complementary: `purity_score/2` gives a continuous [0,1] health metric, while `structural_purity/2` gives a categorical structural diagnosis.

---

### 2.4 Subscores & Component Analysis

**[EXPAND: Detailed breakdown of each subscore component, how to compute, diagnostic value]**

**Implementation:**
```prolog
% purity_scoring.pl (v5.1 — exposed for reform recommendations)
factorization_subscore/2
scope_invariance_subscore/2
coupling_cleanliness_subscore/2
excess_extraction_subscore/2
```

---

### 2.5 Calibration & Sensitivity

**[EXPAND: Known limitations, sensitivity to weight changes, calibration needs]**

**Current Status:**
- Weights (30/25/25/20) are **theoretical**, not empirically fitted
- Need sensitivity analysis: how much does ±5% weight change affect zone classification?
- Need corpus validation: do pristine/degraded zones match human judgment?

**Open Questions:**
- Should decay rate λ vary by coordination type?
- Should scope penalty be symmetric (same for 2→3 types as 1→2)?
- Can we validate against long-term constraint health outcomes?

---

## §3. Network Dynamics: Contamination Propagation

### 3.1 Why Networks Matter

**Conceptual Overview:**

No constraint is an island. They connect, influence, and contaminate each other.

Imagine three constraints:
- **A**: Snare (purity 0.25, degraded)
- **B**: Tangled Rope (purity 0.55, borderline)
- **C**: Rope (purity 0.85, sound)

If A and B share agents (beneficiaries/victims), A's degradation can **contaminate** B:
- B's effective purity drops from 0.55 → 0.45
- B crosses from borderline → contaminated
- B's action recommendation changes from "monitor" → "reform urgently"

This is **network contamination**: low-purity constraints drag down their neighbors.

**Why This Matters:**

1. **Context-dependent health**: Constraint quality depends on neighborhood
2. **Cascade prediction**: One constraint's drift can trigger network-wide degradation
3. **Targeted intervention**: Fixing the worst node improves entire cluster
4. **Realistic modeling**: Real constraints don't exist in isolation

---

### 3.2 Network Discovery

**[EXPAND: How edges are inferred, three discovery methods, network topology]**

**Three Edge Types:**

1. **Explicit**: `affects_constraint(C1, C2)` declared in ontology
2. **Inferred coupling**: High correlation in coupling topology (> 0.50)
3. **Shared agents**: Common beneficiaries or victims

**Implementation:**
```prolog
% drl_purity_network.pl (Stage 8)
constraint_neighbors(C, Context, Neighbors) :-
    findall(neighbor(Other, Strength, Source),
            discover_edge(C, Other, Context, Strength, Source),
            Neighbors).
```

**Neighbor structure:** `neighbor(Other, Strength, Source)` where:
- `Other` = neighboring constraint identifier
- `Strength` = edge strength (numeric, used in contamination attenuation)
- `Source` = edge discovery method (explicit, inferred_coupling, shared_agents)

---

### 3.3 Contamination Rules

**[EXPAND: Full contamination propagation algorithm, one-hop only, downward flow]**

**Core Rule: Downward-Only Flow**

```
Contamination flows: Snare → Rope
Contamination does NOT flow: Rope → Snare
```

Low purity contaminates high purity. High purity does not purify low.

**Asymmetry is fundamental**: Entropy increases. Coordination degrades. Extraction spreads.

**Contamination Strength by Type:**

| Source Type | Strength |
|-------------|----------|
| Snare | 1.0 (maximum) |
| Piton | 0.8 (high) |
| Tangled Rope | 0.5 (moderate) |
| Naturalized | 0.3 (moderate-low) |
| Scaffold | 0.2 (low) |
| Rope | 0.1 (minimal) |
| Mountain | 0.0 (none) |

**Mountains don't contaminate**: Natural laws don't spread extraction.

**Type Immunity (Target Susceptibility):**

Different constraint types have different susceptibility to contamination from neighbors:

| Target Type | Immunity | Effect |
|-------------|----------|--------|
| Mountain | 0.0 | Immune — natural laws unaffected by neighbor contamination |
| Piton | 0.3 | Low susceptibility — degraded state already resistant |
| Snare | 0.5 | Moderate susceptibility |
| Naturalized | 0.7 | High susceptibility |
| Tangled Rope | 0.8 | High susceptibility — hybrid state easily influenced |
| Scaffold | 0.9 | Very high susceptibility — temporary structures fragile |
| Rope | 1.0 | Fully susceptible — pure coordination most affected |

Immunity scales the total contamination before subtraction from intrinsic purity. At 0.0 (Mountain), no contamination passes through. At 1.0 (Rope), all contamination applies.

**Formula:**

Contamination uses a **delta-based** model: contamination flows from lower-purity neighbors to higher-purity targets, proportional to the purity difference (not absolute low purity).

```
effective_purity(C, Context) =
    max(0.0, intrinsic_purity(C) - TotalContam × type_immunity(MyType))
```

Where `TotalContam = Σ edge_contamination(Neighbor)` and per edge:

```
Delta = max(0.0, MyPurity - OtherPurity)    [purity difference, not absolute]
Attenuation = EdgeStrength × attenuation_factor
RawContam = Delta × Attenuation × type_contamination_strength(OtherType)
edge_contamination(N) = min(cap, RawContam)
```

**Three key design choices:**
1. **Delta-based driver**: Uses `max(0, MyPurity - OtherPurity)` — contamination depends on the *relative* purity difference, not on the neighbor's absolute low purity. This means high-purity targets get more contamination pressure from the same neighbor than low-purity targets do.
2. **Edge strength attenuation**: `EdgeStrength` (from network discovery) is included in the attenuation calculation, so stronger edges propagate more contamination.
3. **Immunity scaling**: Total contamination is multiplied by `type_immunity(MyType)` before subtraction. Mountains (immunity 0.0) are completely immune; Ropes (immunity 1.0) receive the full contamination pressure.

**Parameters:**
- `purity_contamination_cap` = 0.30 (max reduction per edge)
- `purity_attenuation_factor` = 0.50 (loses 50% per hop)

**One-Hop Only:**

Contamination is **local** (one edge away), not transitive. Prevents:
- Infinite recursion
- Over-counting (A→B→C shouldn't double-count A's effect on C)
- Computational explosion

**Implementation:**
```prolog
% drl_purity_network.pl
effective_purity(C, Context, EffPurity, purity_components(Intrinsic, TotalContam, Detail)) :-
    purity_score(C, Intrinsic),
    constraint_neighbors(C, Context, Neighbors),
    (   Neighbors = []
    ->  TotalContam = 0.0, Detail = no_neighbors, EffPurity = Intrinsic
    ;   Intrinsic < 0.0
    ->  TotalContam = 0.0, Detail = no_neighbors, EffPurity = Intrinsic
    ;   dr_type(C, Context, MyType),
        type_immunity(MyType, Immunity),
        compute_total_contamination(C, Intrinsic, Neighbors, Context, TotalContam, NeighborDetails),
        Detail = contamination_detail(NeighborDetails),
        RawEff is Intrinsic - TotalContam * Immunity,
        EffPurity is max(0.0, RawEff)
    ).
```

**Return structure:** The 4th argument is `purity_components(Intrinsic, TotalContam, Detail)` where:
- `Intrinsic` = raw purity score from `purity_score/2`
- `TotalContam` = total contamination pressure from all neighbors
- `Detail` = `no_neighbors` | `contamination_detail(NeighborList)` with per-edge breakdown

---

### 3.4 Network Drift Dynamics (Stage 9)

**[EXPAND: How drift propagates through networks over time, velocity calculations]**

**The Problem:**

Constraint B might have:
- **Intrinsic purity stable**: B's own metrics haven't changed
- **Effective purity declining**: Neighbor A is drifting, increasing contamination

This is **induced degradation** — B is getting worse because A is getting worse, even though B itself is stable.

**Network Drift Velocity:**

```
dEP/dt = Σ (dP_neighbor/dt · contamination_strength · attenuation)
```

Where:
- EP = effective purity
- dP_neighbor/dt = neighbor's drift velocity (from lifecycle analysis)

**Detection:**

```
network_drift(C) ← 
    |dEP/dt| > network_drift_velocity_threshold (0.01/year)
    ∧ has_drifting_neighbor(C)
```

**Implementation:**
```prolog
% network_dynamics.pl (via drl_lifecycle.pl facade)
detect_network_drift/3
network_drift_velocity/4
cascade_prediction/3
```

---

### 3.5 Cascade Prediction

**[EXPAND: Given source drift, predict which neighbors will cross purity thresholds and when]**

**Algorithm:**

1. Identify constraint C with drift velocity dP/dt < 0
2. For each neighbor N connected to C:
   - Compute contamination pressure increase: ΔP_contam
   - Compute effective purity trajectory: EP_N(t)
   - Find time when EP_N crosses threshold (sound→borderline, etc.)
3. Return predictions: `will_cross(N, Threshold, ETA)`

**Usage:**

**Early warning system** for network-wide degradation:
"If copyright enforcement continues drifting at current rate, 3 related IP constraints will cross into contaminated zone within 18 months."

**Implementation:**
```prolog
% network_dynamics.pl (via drl_lifecycle.pl facade)
cascade_prediction(SourceConstraint, Context, Predictions) :-
    % ... find neighbors, compute trajectories, predict crossings ...
```

---

### 3.6 Network Stability Assessment

**[EXPAND: Classify entire network as stable/degrading/cascading]**

**Three States:**

1. **Stable**: < 10% of constraints drifting, no cascade predictions
2. **Degrading**: 10-30% drifting, some neighbors at risk
3. **Cascading**: > 30% drifting OR cascade count > threshold (3)

**Implementation:**
```prolog
% network_dynamics.pl (via drl_lifecycle.pl facade)
network_stability_assessment(Context, Classification) :-
    findall(C, has_network_drift(C, Context), Drifting),
    % ... compute percentages, check cascade threshold ...
```

---

## §4. Lifecycle Extensions: Drift Types 8-11

### 4.1 Type 8: Coupling Drift (CD)

**Conceptual Overview:**

Remember: independent dimensions should stay independent. **Coupling Drift** is when they start entangling over time.

**Example:**

Year 1: "Use this app for messaging" — simple tool, no coupling
Year 5: "Use this app for messaging... and it needs your location, contacts, calendar, photos, and microphone access"

The constraint **coupled** independent dimensions (communication + privacy + data access). This is drift toward extraction.

**Formal Definition:**

```
CD(C, t_drift) ≡
    CouplingTopology(C, t < t_drift) = independent
    ∧ CouplingTopology(C, t ≥ t_drift) = coupled
    ∧ coupling_score(C, t_drift) - coupling_score(C, t₀) ≥ threshold
    ∧ ε(C, t ≥ t_drift) > ε(C, t < t_drift)
```

**Detection:**

Coupling drift detection uses `boltzmann_coupling_threshold` (0.25) as the actual gate for whether coupling is significant. The `detect_coupling_drift` predicate in `drl_lifecycle.pl` compares coupling scores across time intervals against this threshold.

> **Note (February 2026):** The original spec referenced a dedicated `coupling_drift_threshold` parameter (0.10) for delta-based drift detection. This parameter was never wired into any code — `detect_coupling_drift` uses `boltzmann_coupling_threshold` instead. The orphan parameter has been removed from `config.pl`. If delta-based coupling drift detection is wanted in the future, it should be designed fresh with proper temporal coupling history.

**Why Extraction Matters:**

Not all coupling is extractive (complexity can increase legitimately). We only flag coupling drift when **accompanied by extraction increase**.

**Implementation:**
```prolog
% drift_events.pl (via drl_lifecycle.pl facade)
% Uses boltzmann_coupling_threshold (0.25) as the coupling gate
detect_coupling_drift(C)
```

**Note:** `detect_coupling_drift/1` takes only a ConstraintID. Context is not a parameter — the predicate checks current coupling against `boltzmann_coupling_threshold` and requires `metric_trend(C, base_extractiveness, increasing)`.

**Parameters:**
- `boltzmann_coupling_threshold` = 0.25 (actual gate used by detect_coupling_drift)

**Severity:**
- **critical**: coupling > 0.50 (strongly coupled) AND extraction rising fast
- **warning**: coupling 0.25-0.50
- **watch**: coupling 0.15-0.25

---

### 4.2 Type 9: Boltzmann Floor Drift (BFD)

**Conceptual Overview:**

Sometimes complexity **must** increase. A global pandemic coordination mechanism genuinely needs more coupling than a local health clinic.

**Boltzmann Floor Drift** is when the **minimum necessary extraction** rises — not because of extractive overhead, but because the coordination problem got harder.

**Formal Definition:**

```
BFD(C, t_drift) ≡ 
    BoltzmannFloor(C, t_drift) > BoltzmannFloor(C, t₀)
    ∧ ε(C, t_drift) tracks new floor
    ∧ ε(C) - Floor remains constant
```

**The Key Distinction:**

- **Extractive drift**: ε rises, floor stays constant → excess extraction increasing
- **Boltzmann floor drift**: ε rises, floor rises, excess stays constant → necessary complexity increasing

**Example:**

- **Year 1**: Local power grid, Floor = 0.05, ε = 0.08, Excess = 0.03
- **Year 10**: Regional grid, Floor = 0.12, ε = 0.15, Excess = 0.03

The grid got more complex (floor rose), but it's not more extractive (excess unchanged).

**Detection:**

1. Measure Boltzmann floor over time
2. If increase ≥ `boltzmann_floor_drift_threshold` (0.05)
3. AND ε tracks floor (excess stays ~constant)
4. → **Boltzmann Floor Drift detected**

**Implementation:**
```prolog
% drift_events.pl (via drl_lifecycle.pl facade)
detect_boltzmann_floor_drift(C) :-
    boltzmann_compliance:excess_extraction(C, Excess),
    boltzmann_compliance:boltzmann_floor_for(C, Floor),
    safe_metric(C, extractiveness, CurrentEps),
    metric_trend(C, base_extractiveness, increasing),
    Excess < Floor * 0.5,
    % ... format drift report ...
```

**Parameters:**
- `boltzmann_floor_drift_threshold` = 0.05

**Severity:**
- Based on floor increase magnitude
- Typically "watch" (legitimate complexity increase)

---

### 4.3 Type 10: Purity Drift (PD)

**[EXPAND: Structural decay despite stable metrics — the entropy increase of coordination]**

**Conceptual Overview:**

Your car's metrics look fine: engine runs, transmission shifts, no warning lights. But the mechanic says "this needs work" because they see:
- Oil pressure declining
- Bearing wear increasing
- Gasket degradation starting

**Purity Drift** is the same: structural health declining even when χ and ε appear stable.

**The Four Signals:**

1. **Factorization declining**: Coupling score rising (extraction_rising signal)
2. **Coupling above threshold**: Nonsensical coupling detected
3. **Theater rising**: IB noise increasing (coordination signal fading)
4. **Excess above floor**: Extraction drifting above Boltzmann floor

**Detection:**

Any **one** of the four signals triggers the event. Severity based on:
- Purity level (< 0.30 = critical, < 0.50 = warning, else watch)
- Number of signals (≥3 = critical, ≥2 = warning, 1 = watch)

**Implementation:**
```prolog
% drift_events.pl (via drl_lifecycle.pl facade)
detect_purity_drift(C) :-
    purity_score(C, Purity),
    collect_purity_decline_signals(C, Signals),
    Signals \= [],
    classify_purity_drift_severity(Purity, Signals, Severity).
```

**Why This Matters:**

Early warning before type flip. A Rope at purity 0.45 is drifting toward Tangled Rope. Catch it now, before it crosses the threshold.

---

### 4.4 Type 11: Network Drift (ND)

**[EXPAND: Induced degradation from neighbors — constraint gets worse because its context gets worse]**

**Formal Definition:**

```
ND(C, t_drift) ≡ 
    intrinsic_purity(C, t) stable
    ∧ effective_purity(C, t) declining
    ∧ has_drifting_neighbor(C)
    ∧ network_drift_velocity(C) > threshold
```

**Implementation:**
```prolog
% network_dynamics.pl (via drl_lifecycle.pl facade)
detect_network_drift/3
network_drift_contagion/3
network_drift_velocity/4
```

**Parameters:**
- `network_drift_velocity_threshold` = 0.01/year

---

## §5. Action Layer Extensions

### 5.1 Purity-Qualified Actions

**[EXPAND: How purity modifies action recommendations]**

**Core Insight:**

The action layer (logic.md §VI) says:
- Mountain → Accept
- Rope → Maintain
- Tangled Rope → Reform
- Snare → Cut

But **purity** adds qualifiers:
- Tangled Rope at purity 0.65 → "Reform (viable)"
- Tangled Rope at purity 0.25 → "Reform **blocked** → Cut/Exit"

**The Qualifier System:**

```
purity_qualified_action(C, Context, QAction, Rationale)
```

Where `QAction = qualified_action(BaseAction, Qualifier, Priority)` is a compound return term.

Qualifiers:
- `stable` — purity ≥ 0.70, continue base action
- `monitor` — purity 0.50-0.70, watch for decline
- `escalate_reform` — purity 0.30-0.50, reform urgently
- `escalate_cut` — purity < 0.30 for snare, switch to cut
- `accelerate_sunset` — purity < 0.30 for scaffold
- `degraded` — purity < 0.30, base action blocked
- `inconclusive` — purity = -1.0 (sentinel), insufficient epistemic data for qualification

**Implementation:**
```prolog
% drl_boltzmann_analysis.pl (via drl_modal_logic.pl facade)
purity_qualified_action(C, Context, QAction, Rationale) :-
    constraint_indexing:valid_context(Context),
    drl_core:dr_action(C, Context, BaseAction),
    purity_scoring:purity_score(C, Purity),
    qualify_action(BaseAction, Purity, C, QAction, Rationale).

% QAction = qualified_action(BaseAction, Qualifier, Priority)
% Rationale = human-readable atom explaining the qualification
```

---

### 5.2 Energy Scaling by Purity

**[EXPAND: Reform cost multiplier based on structural health]**

**Formula:**

```
energy_cost(action, C) = base_cost(action) × purity_multiplier(C)
```

**Purity Multiplier:**

```
M(p) = 1 + (purity_energy_max_multiplier - 1) · (1 - p)²
```

Where:
- p = purity score [0, 1]
- max_multiplier = 3.0

**Examples:**
- Purity 0.90 → M = 1.01× (almost no overhead)
- Purity 0.70 → M = 1.18× (slight overhead)
- Purity 0.50 → M = 1.50× (moderate overhead)
- Purity 0.31 → M = 2.16× (expensive!)
- Purity 0.10 → M = 2.89× (near cap)

**Implementation:**
```prolog
% drl_modal_logic.pl
purity_adjusted_energy(Action, C, Context, AdjustedCost) :-
    base_energy_cost(Action, BaseCost),
    purity_score(C, Purity),
    (   Purity < 0
    ->  AdjustedCost = BaseCost  % Sentinel, no adjustment
    ;   config:param(purity_energy_max_multiplier, MaxMult),
        Multiplier is 1 + (MaxMult - 1) * (1 - Purity) * (1 - Purity),
        AdjustedCost is BaseCost * Multiplier
    ).
```

**Parameters:**
- `purity_energy_max_multiplier` = 3.0

---

### 5.3 Composition Gates

**[EXPAND: Safety checks preventing harmful actions on degraded constraints]**

**The Problem:**

Attempting surgical reform on a degraded constraint (purity < 0.30):
- **Wastes energy** (2.5× cost multiplier)
- **Low success rate** (too corrupted to fix)
- **Risk of entrenchment** (partial reform can make it worse)

**Solution: Composition Gates**

Block certain actions when purity too low:

```
can_perform(surgical_reform, C) ← 
    purity(C) ≥ purity_surgical_reform_gate (0.30)
```

```
can_perform(safe_transition, C) ← 
    purity(C) ≥ purity_scaffold_health_gate (0.50)
```

```
can_perform(efficient_coordination, C) ← 
    purity(C) ≥ purity_action_sound_floor (0.70)
```

**Implementation:**
```prolog
% drl_modal_logic.pl
action_composition_gate(surgical_reform, C, pass) :-
    purity_score(C, P),
    config:param(purity_surgical_reform_gate, Gate),
    P >= Gate, !.
action_composition_gate(surgical_reform, _, block_degraded).
```

**Parameters:**
- `purity_surgical_reform_gate` = 0.30
- `purity_scaffold_health_gate` = 0.50
- `purity_action_sound_floor` = 0.70

---

### 5.4 Network-Qualified Actions

**[EXPAND: Context-aware decisions based on effective purity vs intrinsic purity]**

**The Insight:**

Constraint C might have:
- **Intrinsic purity**: 0.75 (sound)
- **Effective purity**: 0.55 (borderline, due to contamination)

Which should guide action?

**Answer: Effective purity** (includes network context)

**Network-Qualified Action:**

```
network_qualified_action(C, Context, BaseAction, NetworkQualifier, Priority)
```

Where NetworkQualifier adds:
```
network_contaminated(Rationale, Drop)
```

If effective purity drops ≥ 0.05 from intrinsic, annotate the action with network warning.

**Implementation:**
```prolog
% drl_modal_logic.pl (Stage 8)
network_qualified_action(C, Context, BaseAction, Qualifier, Priority) :-
    purity_qualified_action(C, Context, BaseAction, PurityQual, BasePriority),
    effective_purity(C, Context, EP, IP),
    Drop is IP - EP,
    (   Drop >= 0.05
    ->  Qualifier = network_contaminated(PurityQual, Drop),
        Priority is BasePriority + 1  % Escalate
    ;   Qualifier = PurityQual,
        Priority = BasePriority
    ).
```

---

## §6. Integration Architecture

### 6.1 Module Dependency Graph

**[EXPAND: How the 30 Prolog modules connect, data flow, call hierarchy]**

```
config.pl (thresholds)
    ↓
drl_core.pl (canonical classify_from_metrics/6)
    ↓
boltzmann_compliance.pl (Boltzmann test, coupling, excess extraction)
    ↓
signature_detection.pl (FNL, CI_Rope, FCR, structural_purity)
    ↓
purity_scoring.pl (purity_score/2, subscores)
    ↓
drl_purity_network.pl (effective_purity, contamination, neighbors)
    ↓
drl_lifecycle.pl (11 drift types, purity drift, network drift)
    ↓
drl_modal_logic.pl (Stages 7-9, action layer)
    ↓
logical_fingerprint.pl (7D fingerprints + coupling)
```

**Note:** `structural_signatures.pl` still exists as a wrapper module that re-exports from `boltzmann_compliance.pl`, `signature_detection.pl`, and `purity_scoring.pl`. The split occurred to improve modularity, but the original module interface is preserved for backward compatibility.

**Critical Interfaces:**

1. **config.pl → all modules**: Single source of truth for thresholds
2. **drl_core.pl → structural_signatures.pl**: Two-regime handoff (metrics → signature)
3. **structural_signatures.pl → drl_modal_logic.pl**: Purity score consumption
4. **drl_lifecycle.pl → drl_modal_logic.pl**: Drift detection → action escalation

---

### 6.2 Two-Regime System

**[EXPAND: Metrics-first vs signature-override, priority ordering, resolution logic]**

**Regime 1: Metrics**

`drl_core.pl` / `classify_from_metrics/6`:
- Uses ε, χ, Supp thresholds
- Priority: Mountain > Snare > Scaffold > Rope > Tangled Rope > Piton > Naturalized > unknown
- Fast, deterministic

**Regime 2: Signatures**

`signature_detection.pl` / `constraint_signature/2`:
- Can override metric classification
- Priority: FNL > FCR > natural_law(v6.1) > CI_Rope > NL > CS > CC > Piton > ambiguous
- Slower (requires Boltzmann test), but catches structural properties

**Resolution:**

```
final_type(C, Context) = 
    integrate_signature_with_modal(C, metric_type(C), signature_type(C))
```

Override rules:
- FNL → Tangled Rope (override Mountain)
- CI_Rope → Rope (certify true coordination)
- FCR → Tangled Rope (downgrade Rope)
- NL + Mountain → keep Mountain
- CS + Mountain → downgrade to Rope

---

### 6.3 Shadow Mode Design

**[EXPAND: Why shadow mode, how to activate gates, calibration process]**

**Current Status:**

Stages 7-9 are in **shadow mode**:
- All predicates functional
- All tests run
- Results logged
- **BUT**: No classification overrides (except FNL/CI_Rope/FCR)

**Reason:**

Boltzmann thresholds are **provisional**:
- Calibrated on 691-constraint corpus
- Western-biased
- Need sensitivity analysis
- Need cross-cultural validation

**Activation Path:**

1. Run corpus through shadow audit
2. Check for false positives/negatives
3. Tune thresholds (`boltzmann_coupling_threshold`, etc.)
4. Validate on hold-out set
5. Remove shadow mode flag
6. Activate gates

**How to Activate:**

Currently, FNL/CI_Rope/FCR are already active (they fire in `constraint_signature/2`). To fully activate Boltzmann:

1. In `natural_law_signature/1`, add:
   ```prolog
   boltzmann_invariant_mountain(C, true)
   ```
2. This makes NL signature require full Boltzmann compliance

---

### 6.4 Implementation Cross-Reference

**[EXPAND: Predicate → file → line number mapping, for implementers]**

**Key Predicates:**

| Predicate | Module | Purpose |
|-----------|--------|---------|
| `classify_from_metrics/6` | drl_core.pl | Canonical classifier |
| `boltzmann_compliant/2` | boltzmann_compliance.pl | Boltzmann test |
| `cross_index_coupling/2` | boltzmann_compliance.pl | Coupling score |
| `excess_extraction/2` | boltzmann_compliance.pl | Excess extraction |
| `clear_classification_cache/0` | boltzmann_compliance.pl | Retracts all cached classification and coupling data |
| `ib_adjusted_threshold/2` | boltzmann_compliance.pl | Coupling threshold refined by theater ratio (IB signal loss) |
| `coupling_test_powers/1` | boltzmann_compliance.pl | Power levels used in Boltzmann grid |
| `coupling_test_scopes/1` | boltzmann_compliance.pl | Scope levels used in Boltzmann grid |
| `coupling_test_context/3` | boltzmann_compliance.pl | Builds context term for grid cell |
| `classify_at_context/3` | boltzmann_compliance.pl | Classifies constraint at specific grid context |
| `purity_score/2` | purity_scoring.pl | Purity calculation |
| `structural_purity/2` | signature_detection.pl | Structural purity classification |
| `effective_purity/4` | drl_purity_network.pl | Network contamination |
| `constraint_neighbors/3` | drl_purity_network.pl | Network discovery |
| `type_contamination_strength/2` | drl_purity_network.pl | Type contamination potency |
| `purity_contamination_pressure/4` | drl_purity_network.pl | Per-edge contamination |
| `network_purity_metrics/2` | drl_purity_network.pl | Network-level metrics |
| `network_drift_velocity/4` | network_dynamics.pl | Induced drift rate |
| `purity_qualified_action/4` | drl_boltzmann_analysis.pl | Action + purity |

**Full Module List (core pipeline):**

1. config.pl — All thresholds
2. drl_core.pl — Core logic (classify_from_metrics/6)
3. boltzmann_compliance.pl — Boltzmann test, coupling, excess extraction
4. signature_detection.pl — FNL, CI_Rope, FCR, structural_purity
5. purity_scoring.pl — Purity score, subscores
6. drl_purity_network.pl — Network contamination, effective_purity
7. structural_signatures.pl — Wrapper (re-exports from 3-5)
8. drl_lifecycle.pl — Facade; re-exports from:
   - drift_events.pl — Event detection, severity, velocity
   - transition_paths.pl — Degradation path detection & terminal states
   - network_dynamics.pl — Network drift, contagion, cascades
   - drift_report.pl — Unified scan & report generation
9. drl_modal_logic.pl — Facade; re-exports from drl_composition.pl, drl_counterfactual.pl, drl_boltzmann_analysis.pl, drl_purity_network.pl, drl_fpn.pl
10. logical_fingerprint.pl — Fingerprints
11. [+20 more modules for supporting infrastructure]

---

### 6.5 Known Limitations

**[EXPAND: What we know doesn't work yet, calibration needs, open research questions]**

**Issue 1: Threshold Calibration**

- **Problem**: Boltzmann thresholds (0.15 coupling, etc.) are educated guesses
- **Impact**: May have false positives/negatives
- **Need**: Sensitivity analysis, corpus validation
- **Status**: Shadow mode until calibrated

**Issue 2: Weight Sensitivity**

- **Problem**: Purity weights (30/25/25/20) are theoretical, not fitted
- **Impact**: Unknown — could be robust or fragile
- **Need**: Perturbation testing (±10% weight changes)
- **Status**: Open research question

**Issue 3: Cultural Bias**

- **Problem**: Power modifiers calibrated on Western corpus
- **Impact**: Indexical relativity claims may not hold cross-culturally
- **Need**: Non-WEIRD validation
- **Status**: Known limitation flagged

**Issue 4: Boltzmann Floor Estimates**

- **Problem**: Floor values (0.02-0.20) are provisional
- **Impact**: Excess extraction calculations uncertain
- **Need**: Per-coordination-type calibration
- **Status**: Testsets can override via `boltzmann_floor_override/2`

**Issue 5: Network Propagation Assumptions**

- **Problem**: One-hop only, downward-only
- **Impact**: May miss long-range cascade effects
- **Need**: Validation that one-hop is sufficient
- **Status**: Architectural simplification, may need revision

**Issue 6: Temporal Interpolation**

- **Problem**: Network drift requires historical purity data
- **Impact**: Can't detect induced drift without time series
- **Need**: More testsets with multi-year measurement/5 data
- **Status**: Works when data available, otherwise inconclusive

---

## §7. Cross-Domain Bridges & Diagnostic Extensions

### 7.1 Psychological Domain Bridge (psych_bridge.pl)

The psychological bridge maps DR constraint types to clinical/developmental vocabulary without modifying classification logic. Each exported predicate is a named alias that delegates to the corresponding `drl_core` classifier after performing a metric swap.

**Metric swap mechanism:** `with_psych_metric(+C, +Goal)` (unexported helper) temporarily asserts `constraint_metric(C, suppression_requirement, Depth)` from any existing `internalization_depth` metric via `setup_call_cleanup/3`, then calls the Goal. This allows psychological data (internalization depth) to substitute for structural data (suppression requirement) when running the standard classifier.

**Alias mapping:**

| Predicate | DR Type | Output Atom | Clinical Interpretation |
|-----------|---------|-------------|------------------------|
| `is_substrate/3` | mountain | `substrate` | Biological/neurological limits |
| `is_negotiable_pattern/3` | rope | `negotiable_pattern` | Healthy, functional habits |
| `is_hybrid_pattern/3` | tangled_rope | `hybrid_pattern` | Habits that both serve and harm |
| `is_self_extraction/3` | snare | `self_extraction` | Self-destructive patterns (e.g., addiction) |
| `is_developmental_support/3` | scaffold | `developmental_support` | Temporary supports (e.g., therapy) |
| `is_abandoned_camp/3` | piton | `abandoned_camp` | Obsolete coping mechanisms |

All predicates take `(+Constraint, +Context, -Type)`. No external callers currently exist — the module is exported for domain-specific psychology analysis.

**Implementation:** `psych_bridge.pl`, 6 exported predicates + 1 helper.

---

### 7.2 UKE Status Bridge (uke_dr_bridge.pl)

The UKE (Urgency-Knowledge-Energy) bridge maps DR classifications and feasibility assessments to actionable recommendation statuses.

**Primary predicate:** `uke_status(+RecID, -Status, -Reasons)` — takes a recommendation ID, resolves its affected constraint via `affects_constraint/2`, retrieves the DR type and feasibility, then routes through a priority-ordered status determination.

**Status mapping (priority order):**

| Priority | Condition | Status | Meaning |
|----------|-----------|--------|---------|
| 1 | Type = mountain | `fantasy` | Cannot modify a natural constraint |
| 2 | Type = snare, load-bearing, no scaffold | `blocked` | Load-bearing snare removal without safety net |
| 3 | Type ∈ {rope, snare, piton}, viable, no vetoes | `viable` | No structural or political obstacles |
| 4 | Type = tangled_rope | `aspirational` | Requires systemic reform, not immediate action |
| 5 | Type = unknown | `investigate` | Constraint metrics incomplete |
| 6 | Feasibility = blocked_by_veto | `blocked` | Political actors vetoing change |

**Load-bearing detection:** `is_load_bearing(+C)` checks `dr_type(C, snare)` AND `constraint_metric(C, extractiveness, X)` AND `X >` `snare_load_bearing_threshold` (config value: 0.70). A load-bearing snare is one where removal without a scaffold would cause structural collapse.

**Caller:** `report_generator.pl:79`

**Limitation:** Assumes single affected constraint per recommendation (no backtracking on `affects_constraint/2`).

**Implementation:** `uke_dr_bridge.pl`, 1 exported predicate + 2 helpers.

---

### 7.3 Post-Synthesis Divergence Detection (post_synthesis.pl)

Post-synthesis checks detect divergence between diagnostic verdicts and abductive trigger counts — a canary signal that the diagnostic and anomaly-detection subsystems disagree.

**Primary predicate:** `post_synthesis_check(+C, +Summary, -Flags)` — takes a constraint and its `diagnostic_summary/7` term, queries abductive triggers, and returns a list of `flag/2` terms.

**Two divergence cases:**

1. **`red_without_triggers`** — Red diagnostic verdict + zero genuine abductive triggers. The diagnosis says "problematic" but no anomaly explanation was found. Flag fields: `verdict-red`, `genuine_trigger_count-0`, `total_trigger_count-N`.

2. **`green_with_triggers`** — Green diagnostic verdict + ≥ threshold genuine triggers. The diagnosis says "clean" despite multiple anomalies detected. Threshold: `config:param(post_synthesis_green_trigger_threshold)` (default: 2). Flag fields: `verdict-green`, `genuine_trigger_count-N`, `trigger_classes-[...]`.

**Trigger filtering:** `is_genuine_trigger/1` filters triggers by `Classification = genuine` (vs `artifact`). Only genuine triggers count toward divergence.

**Yellow verdict:** Explicitly ignored — yellow is already ambiguous and does not warrant a divergence flag.

**Self-test:** `post_synthesis_selftest/0` runs 5 hardcoded test cases covering both divergence types and expected non-divergence.

**Caller:** `json_report.pl:407`

**Implementation:** `post_synthesis.pl`, 161 lines, 6 predicates.

---

### 7.4 Inferred Coupling Protocol (inferred_coupling_protocol.pl)

The inferred coupling protocol validates temporal coupling inference by correcting a bug in the gradient computation and testing the corrected version against designed pairs with known expected results.

**The bug:** `drl_modal_logic:dr_gradient_at/3` uses a bare `!` (cut) inside `findall/3`. When `findall` backtracks over timepoints, the cut stops all choice points — returning only 1 gradient instead of N-1 for N timepoints. This cascades to `infer_structural_coupling/3`, which relies on complete gradient data.

**The fix:** `gradient_at/3` (`inferred_coupling_protocol.pl:115-123`) replaces the bare cut with `once/1`, providing local cut semantics that preserve the outer `findall` backtracking. The gradient metric is hardcoded to `extractiveness`.

**5-phase validation protocol:**

1. **Discovery** — Find constraints with `measurement/5` data
2. **Gradient verification** — Confirm each measured constraint produces ≥2 gradients
3. **Designed pair testing** — Test 4 known constraint pairs:
   - `pair_1_tech_ecosystem`: quantum_decryption_risk_2026 ↔ smartphone_ubiquity (expected 1.0)
   - `pair_2_institutional_erosion`: regulatory_capture ↔ institutional_trust_decay (expected 1.0)
   - `pair_3_commons_degradation`: tragedy_of_the_commons ↔ pareto_principle (expected 1.0)
   - `pair_4_negative_control`: hawthorne_effect ↔ rotation_seven_black_soil (expected 0.0)
4. **Network impact** — Measure component merging when inferred edges are added to baseline (explicit + shared_agent)
5. **Bridge analysis** — Identify cross-domain bridge edges and type contamination

**Coupling computation:** `compute_coupling/3` computes a sign-agreement ratio: `Strength = Matches / L`, where Matches counts same-sign gradient pairs. Threshold: `config:param(network_coupling_threshold)`.

**Implementation:** `inferred_coupling_protocol.pl`, 510 lines, ~15 core predicates + BFS graph utilities.

---

### 7.5 Giant Component Analysis (giant_component_analysis.pl)

Analyzes the constraint network's percolation structure — whether a giant component exists, at what coupling threshold it emerges, and how contamination propagates through it.

**4 phases:**

1. **Network Topology at Default Threshold** — Maps the existing network using the current `network_coupling_threshold`. Reports degree distribution, component sizes, type/purity landscape, and top 20 super-spreaders. Pre-computes all edges at threshold 0.01 (near-zero capture) and caches node properties.

2. **Threshold Sweep (Erdős–Rényi Phase Transition)** — Sweeps coupling threshold from **0.10 to 0.90 in steps of 0.05** (17 points). At each threshold, `explicit` and `shared_agent` edges always survive; only `inferred_coupling` edges are filtered. Reports edge count, component count, and largest component size at each step. Detects the percolation threshold where a giant component emerges.

3. **Contamination Propagation** — Runs fixed-point iteration to steady state. Maps type contamination paths through the network. Reports immunity scores and cross-type contamination vulnerability.

4. **Context Comparison** — Repeats the analysis in 3 power contexts: `institutional`, `moderate`, `analytical`. Compares component structures across perspectives to identify context-dependent giant components.

**Key predicates:** `run_giant_component_analysis/0` (orchestrator), `run_phase1/0` through `run_phase4/0`, `precompute_all_edges/2`, `sweep_thresholds/3`, `edges_at_threshold/2`, `compute_components/2`, `purity_zone/2`.

**Implementation:** `giant_component_analysis.pl`, 1279 lines, 70+ predicates.

---

## Conclusion

**What We've Built:**

Stages 7-9 extend Deferential Realism from **classification logic** to **structural physics**:

- **Stage 7**: Mathematical test for natural law claims (Boltzmann compliance)
- **Stage 8**: Continuous health metric (purity) + network contamination
- **Stage 9**: Temporal dynamics (how degradation spreads over time)

**What This Enables:**

1. **Catch physics-washing**: FNL detects false Mountain claims
2. **Certify true coordination**: CI_Rope proves structural purity
3. **Detect hidden extraction**: FCR catches coordination-washing
4. **Early warning**: Purity drift before type flip
5. **Network-aware decisions**: Effective purity includes context
6. **Cascade prediction**: Anticipate network-wide degradation

**What's Still Open:**

- Threshold calibration (shadow mode until validated)
- Weight sensitivity analysis (purity score robustness)
- Cross-cultural validation (power modifiers, Boltzmann floors)
- Long-range network effects (beyond one-hop)

**The Core Achievement:**

We moved from asking "What type is this constraint?" to asking:

- Is it structurally sound?
- Is it degrading?
- Is its context contaminating it?
- Can it be reformed, or should we abandon it?

That's the difference between taxonomy and physics.

---

**"Natural laws don't couple independent dimensions. If your classification depends on products of unrelated variables, someone's hiding extraction behind complexity."**
