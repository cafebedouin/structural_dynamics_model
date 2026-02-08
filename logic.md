# Deferential Realism: A Logic of Indexed Constraints

## I. Foundation: Why Indexed Constraint-Logic?

Traditional logic asks: **Is proposition P true?**

Deferential Realism asks: **What constraint-type is C from index I, and what does that imply?**

This requires different logical machinery:
- **Not truth-preservation** → **Constraint-type preservation under transformation**
- **Not validity** → **Classification coherence across indexed evidence**
- **Not soundness** → **Action-consequence alignment relative to power position**

The goal: **Formal system for reasoning about what binds us, from where we stand.**

**Key innovation:** Truth is index-relative but index-objective. The same constraint can be Mountain from one index and Snare from another—both classifications are objectively true.

---

## II. Basic Syntax: Indexed Constraint Operators

### A. Index Structure

**Index Notation:**
```
I ::= (P, T, E, S)
where:
  P ∈ {powerless, moderate, powerful, organized, institutional, analytical}
  T ∈ {immediate, biographical, generational, historical, civilizational}
  E ∈ {trapped, constrained, mobile, arbitrage, analytical}
  S ∈ {local, regional, national, continental, global, universal}
```

**Indexed Classification:**
```
C[I] denotes "constraint C as experienced from index I"
Type(C[I]) ∈ {■, ⊞, ⊠, ⊞⊠, ⊡, ⊟}  (Mountain, Rope, Snare, Tangled Rope, Scaffold, Piton)
```

All six types are full indexed classifications produced by the same canonical pipeline (`classify_from_metrics/6`).

### B. Core Modal Operators (Indexed)

**Mountain Operator: `■C[I]`**
- "C is a Mountain from index I" = C has zero degrees of freedom from I's perspective
- Formal: `■C[I] ↔ ε(C) ≤ 0.15 ∧ Supp(C) ≤ 0.05 ∧ Immutable(C, I.T, I.E)`
- Semantics: From index I, C appears unchangeable and requires no enforcement
- Power-scaling: Mountains don't check χ — they use raw ε and suppression directly
- Immutability gate: Requires `effective_immutability(I.T, I.E) = mountain`

**Rope Operator: `⊞C[I]`**
- "C is a Rope from index I" = C provides coordination with minimal extraction for agent at I
- Formal: `⊞C[I] ↔ χ(C, I.P, I.S) ≤ 0.35 ∧ ε(C) ≤ 0.15 ∧ Changeable(C, I.T, I.E)`
- Semantics: From index I, C coordinates effectively without significant extraction
- Power-scaling: `χ(C, I.P, I.S)` applies power and scope modifiers to base extraction
- Immutability gate: Requires `effective_immutability(I.T, I.E) = rope`
- Note: No coordination function gate — coordination is a structural expectation for ropes but not a metric prerequisite

**Snare Operator: `⊠C[I]`**
- "C is a Snare from index I" = C extracts heavily from agent at I
- Formal: `⊠C[I] ↔ χ(C, I.P, I.S) ≥ 0.66 ∧ ε(C) ≥ 0.46 ∧ Supp(C) ≥ 0.60 ∧ Changeable(C, I.T, I.E)`
- Semantics: From index I, C appears as active extraction requiring resistance
- Power-scaling: High extraction for powerless, may be negative for institutional
- Immutability gate: Requires `effective_immutability(I.T, I.E) = rope`
- Note: No ¬Coord gate — snares may have vestigial coordination functions

**Tangled Rope Operator: `⊞⊠C[I]`**
- "C is a Tangled Rope from index I" = C both coordinates AND extracts from agent at I
- Formal: `⊞⊠C[I] ↔ 0.40 ≤ χ(C, I.P, I.S) ≤ 0.90 ∧ ε(C) ≥ 0.50 ∧ Supp(C) ≥ 0.40 ∧ Enforce(C) ∧ Coord(C) ∧ Asymmetric(C)`
- Semantics: From index I, C provides genuine coordination while extracting asymmetrically
- Power-scaling: May appear as Rope to powerful, Snare to powerless, Tangled to moderate
- Requires three structural properties: active enforcement, coordination function, and asymmetric extraction
- **Empirically validated:** ~36% of analyzed constraints show irreducible hybrid pattern

**Scaffold Operator: `⊡C[I]`**
- "C is a Scaffold from index I" = C provides temporary coordination with built-in expiration
- Formal: `⊡C[I] ↔ χ(C, I.P, I.S) ≤ 0.30 ∧ Coord(C) ∧ Sunset(C) ∧ Theater(C) ≤ 0.70`
- Semantics: From index I, C is a transitional support structure designed for dissolution
- No immutability gate — scaffolds are inherently temporary
- Theater ceiling ensures the constraint is doing real work, not mere performance

**Piton Operator: `⊟C[I]`**
- "C is a Piton from index I" = C persists without function, imposing cost without benefit
- Formal: `⊟C[I] ↔ χ(C, I.P, I.S) ≤ 0.10 ∧ ε(C) > 0.10 ∧ Theater(C) ≥ 0.70`
- Semantics: Degradation state — extraction dried up but structure persists as theater
- Theater floor: High theater ratio (≥ 0.70) distinguishes pitons from low-extraction ropes
- Pitons are detected by their combination of minimal effective extraction, measurable base extraction, and high theater ratio

### C. Detection Pattern Operators (Non-Indexed)

**False Mountain Detector: `FM(C)`**
- "C is misclassified as Mountain" = Claimed natural but actually constructed
- Formal: `FM(C) ↔ Claimed(■C) ∧ ∃I(¬■C[I]) ∧ ε(C) > 0.70`
- Semantics: Structural signature shows construction, not natural emergence
- Not index-relative: Fraud detection operates on objective structure

### D. Power-Scaling Function

**Power Modifier: `π(P) → ℝ`**

Calibrated from 691-constraint corpus to minimize classification errors:

```
π(powerless)     = 1.5    (experience MORE extraction)
π(moderate)      = 1.0    (baseline)
π(powerful)      = 0.6    (experience LESS extraction)
π(organized)     = 0.4    (shared burden)
π(institutional) = -0.2   (NET BENEFICIARY)
π(analytical)    = 1.15   (analytical clarity: detects extraction moderate agents normalize)
```

**Design note on π(analytical) = 1.15:** The analytical modifier was changed from 1.0 to 1.15 to break the moderate-analytical degeneracy. At π = 1.0, analytical and moderate agents produced identical χ values, making the analytical perspective unable to distinguish constraints that moderate agents normalize. At 1.15, 93 constraints in the corpus show the "only analyst catches snare" pattern — constraints that appear as tangled rope from moderate perspective but snare from analytical perspective.

**Scope Modifier: `σ(S) → ℝ`**

Scope affects verification difficulty and thus effective extraction. Larger scope = harder verification = more extraction hidden behind complexity:

```
σ(local)        = 0.8    (easy verification, extraction dampened)
σ(regional)     = 0.9    (easier verification)
σ(national)     = 1.0    (baseline)
σ(continental)  = 1.1    (harder verification)
σ(global)       = 1.2    (hardest verification, extraction amplified)
σ(universal)    = 1.0    (neutral — natural laws)
```

**Effective Extractiveness: `χ(C, P, S)`**

```
χ(C, P, S) = ε(C) × π(P) × σ(S)
```

Where:
- `ε(C)` is the base extraction (structural property of the constraint)
- `π(P)` is the power modifier (how much extraction the agent experiences)
- `σ(S)` is the scope modifier (how scope affects verification difficulty)

**Example:**
```
ε(carbon_credits) = 0.40

χ(carbon_credits, powerless, local)      = 0.40 × 1.5  × 0.8  = 0.48 → Tangled Rope
χ(carbon_credits, moderate, national)    = 0.40 × 1.0  × 1.0  = 0.40 → Tangled threshold
χ(carbon_credits, institutional, global) = 0.40 × -0.2 × 1.2  = -0.096 → Rope (net benefit)
χ(carbon_credits, analytical, global)    = 0.40 × 1.15 × 1.2  = 0.552 → Tangled Rope
```

### E. Structural Signature Predicates

Signatures detect constraint ORIGIN (natural law vs coordination scaffold vs constructed constraint) rather than just constraint METRICS. They operate in a two-regime architecture with metric-based classification (see §II.F).

**Natural Law Signature: `NL(C)`**
```
NL(C) ↔ Emerges_Naturally(C) ∧ ε(C) ≈ 0 ∧ Universal_Scope(C)
       ∧ No_Alternatives(C) ∧ Zero_Beneficiaries(C) ∧ Temporally_Stable(C)
```
Examples: Gravity, thermodynamics, logical necessity

**Coordination Scaffold Signature: `CS(C)`**
```
CS(C) ↔ Designed(C) ∧ Solves_Collective_Problem(C) ∧ ε(C) ≤ 0.15 ∧ Benefits_Distributed(C)
       ∧ Has_Alternatives(C)
```
Examples: TCP/IP, traffic conventions, metric system

**Constructed Constraint Signature: `CC(C)` — Three Extraction-Aware Sub-Signatures**

The constructed constraint signature is split into three sub-signatures based on extraction level. This enables the signature regime to provide extraction-aware classification when metrics fail (unknown) or conflict (mountain override):

```
CC_low(C) ↔ Imposed(C) ∧ ε(C) ≤ 0.35 ∧ (Enforcement(C) ∨ Resistance(C) ∨ Asymmetric_Benefits(C))
```
Low extraction construct → rope when metrics fail. These are rule-based coordination structures, not extraction mechanisms.

```
CC_high(C) ↔ Imposed(C) ∧ ε(C) ≥ 0.46 ∧ (Enforcement(C) ∨ Resistance(C) ∨ Asymmetric_Benefits(C))
```
High extraction construct → snare when metrics fail. These are extraction mechanisms that metrics failed to classify.

```
CC_mid(C) ↔ Imposed(C) ∧ 0.35 < ε(C) < 0.46 ∧ (Enforcement(C) ∨ Resistance(C) ∨ Asymmetric_Benefits(C))
```
Mid extraction construct → tangled_rope (genuine middle). These constraints sit in the extraction gap between rope and snare thresholds.

**Piton Signature: `PS(C)`**
```
PS(C) ↔ Low_Suppression(C) ∧ High_Resistance(C) ∧ Has_Alternatives(C) ∧ Temporally_Evolving(C)
```
Persists through inertia, not force. Once useful, now ossified.

**Signature Override Rules:**
```
NL(C) → ■C[I] for all I           (natural laws are Mountains from ALL indices — strongest override)
CS(C) ∧ ■C[I] → ⊞C[I]            (coordination scaffolds misclassified as mountain → rope)
CC_low(C) ∧ ■C[I] → ⊞C[I]        (low-extraction constructs misclassified as mountain → rope)
CC_high(C) ∧ ■C[I] → ⊞⊠C[I]      (high-extraction constructs misclassified as mountain → tangled rope)
CC_mid(C) ∧ ■C[I] → ⊞⊠C[I]       (mid-extraction constructs misclassified as mountain → tangled rope)

When metrics produce unknown:
CS(C) → ⊞C[I]                     (coordination scaffold → rope)
CC_low(C) → ⊞C[I]                 (low extraction → rope)
CC_high(C) → ⊠C[I]                (high extraction → snare)
CC_mid(C) → ⊞⊠C[I]               (mid extraction → tangled rope)
PS(C) → ⊟C[I]                     (piton signature → piton)
```

### F. Classification Priority and Architecture

**Classification Priority Order:**

When multiple type predicates could match, the system applies a strict priority ordering (implemented as Prolog clause order in `classify_from_metrics/6`):

```
Mountain > Snare > Scaffold > Rope > Tangled Rope > Piton > unknown
```

This ordering reflects structural logic:
- Mountain checked first: immutability gate prevents false mountains from low-extraction constraints
- Snare before Rope: prevents high-extraction constraints from falling into rope range when χ happens to be low
- Scaffold before Rope: sunset clause is a stronger structural signal than generic low-extraction
- Tangled Rope late: requires the most structural predicates (enforcement + coordination + asymmetry), so it's a fallback when simpler types don't match
- Piton last: catch-all for degraded constraints

**Two-Regime Architecture:**

Classification operates in two sequential regimes:

```
Regime 1: Metric-Based Classification
  classify_from_metrics(C, BaseEps, Chi, Supp, Context, MetricType)
  → Uses threshold gates from config.pl
  → Produces: mountain | snare | scaffold | rope | tangled_rope | piton | unknown

Regime 2: Signature Override
  integrate_signature_with_modal(C, MetricType, FinalType)
  → Detects structural signature (NL, CS, CC_low, CC_mid, CC_high, PS)
  → Overrides MetricType when signature conflicts (see §II.E override rules)
  → Most classifications pass through unchanged
```

**Canonical Predicate — Single Source of Truth:**

All classification modules delegate threshold logic to a single canonical predicate:

```
classify_from_metrics(C, BaseEps, Chi, Supp, Context, Type)
```

Four modules delegate to this predicate:
- `drl_core:metric_based_type_indexed/3` — primary classification
- `drl_modal_logic:dr_type_at/4` — modal logic (possible/necessary worlds)
- `drl_lifecycle:classify_snapshot/3` — temporal snapshot classification
- `data_validation:infer_expected_type/2` — data completeness validation

This ensures that threshold changes in `config.pl` propagate uniformly across all classification pathways.

### G. Immutability Function

**Effective Immutability: `Immutability(T, E) → mountain | rope`**

The immutability function gates Mountain and Snare/Rope classification. It determines whether a constraint appears changeable from the agent's temporal and exit perspective:

```
Immutability(T, E) → mountain:
  (T = immediate ∨ T = biographical) ∧ (E = trapped ∨ E = constrained)
  T = generational ∧ E = trapped
  T = civilizational ∧ E = analytical    (analytical can perceive both mountain and rope)

Immutability(T, E) → rope:
  T = historical ∧ ∀E
  T = civilizational ∧ E ≠ analytical
  (T = immediate ∨ T = biographical) ∧ (E = mobile ∨ E = arbitrage)
  T = generational ∧ E ≠ trapped
```

**Design rationale:** The immutability function implements the WHEN × WHERE coupling discovered in the invariant analysis:
- Trapped agents with short time horizons perceive constraints as immutable (mountain gate passes)
- Mobile agents or those with long time horizons perceive constraints as changeable (rope gate passes)
- The analytical perspective at civilizational scope can perceive both — this is why the metric gates (ε, χ) determine which fires first in the priority ordering

**Mountain gate:** `effective_immutability(I.T, I.E) = mountain` — required for Mountain classification
**Rope gate:** `effective_immutability(I.T, I.E) = rope` — required for Snare and Rope classification

### H. Logical Fingerprint Engine

The Logical Fingerprint Engine extracts structural "logical fingerprints" from constraints — the qualitative shape of a constraint's logic, abstracted away from domain-specific content.

**Six Dimensions:**

```
Fingerprint(C) = (Shift, Properties, Voids, Actors, Drift, Zone)
```

1. **Shift** (perspectival): How classification transforms across power levels. The most powerful discriminator. Two constraints with the same shift pattern are governed by the same underlying power dynamic regardless of subject matter.
   ```
   Shift(C) = (Type_powerless, Type_moderate, Type_institutional, Type_analytical)
   ```

2. **Properties** (skeleton): Which structural predicates hold — enforcement, natural, sunset, coordination, asymmetric, has_beneficiaries, has_victims, has_temporal_data, has_theater.

3. **Voids** (negative space): Diagnostic absences — properties that SHOULD exist given the constraint's profile but DON'T. Examples: `unaccountable_extraction` (high extraction, no sunset), `coercion_without_coordination` (enforcement but no coordination function).

4. **Actors** (topology): Not WHO the actors are, but the STRUCTURE of actor relationships — concentrated vs distributed beneficiaries/victims.

5. **Drift** (temporal): Direction the constraint is moving — rising, stable, falling, or unknown — across extraction, suppression, and theater metrics.

6. **Zone** (metric region): Where the constraint sits in metric space — categorized using config.pl thresholds (negligible, low, moderate, high, extreme) for both extraction and suppression.

**Isomorphism:** Two constraints with unifiable fingerprints are logically isomorphic — they operate through the same mechanism regardless of domain. This enables cross-domain pattern discovery: a tax loophole and a feudal tithe may share identical fingerprints despite having no surface-level similarity.

**Corpus results:** 691+ constraints, 16 distinct shift patterns across the corpus.

---

## IIa. Temporal Logic: Lifecycle Operators and State Transitions

**Temporal extension:** Constraints have lifecycle trajectories (genesis, maturation, degradation, terminal states). Temporal operators track state transitions and temporal properties.

### A. Temporal Operators

**Always (Universal Temporal Quantifier): `□C(t)`**
- "C holds at all times in scope"
- Formal: `□C(t) ≡ ∀t ∈ T: C(t)`
- Example: `□▲(gravity)` - gravity is always a Mountain
- **Mountains only:** Only Mountains satisfy universal temporal invariance

**Eventually (Existential Temporal Quantifier): `◊C(t)`**
- "C holds at some time in scope"
- Formal: `◊C(t) ≡ ∃t ∈ T: C(t)`
- Example: `◊☠(C)` - C will eventually become Piton (piton)

**Next (Successor State): `○C(t)`**
- "C holds at next discrete time step"
- Formal: `○C(t) ≡ C(t+1)`
- Example: `☒C(t) → ○☠C(t)` - Scaffold at t implies Piton at t+1 (if sunset violated)

**Until (Bounded Temporal): `C₁ U C₂`**
- "C₁ holds until C₂ becomes true"
- Formal: `C₁ U C₂ ≡ ∃t': (C₂(t') ∧ ∀t < t': C₁(t))`
- Example: `☒C(t) U Purpose_Complete(C)` - Scaffold persists until purpose complete

**Since (Historical Temporal): `C₁ S C₂`**
- "C₁ has held since C₂ was true"
- Formal: `C₁ S C₂ ≡ ∃t': (C₂(t') ∧ ∀t > t': C₁(t))`
- Example: `☠C S Function_Lost(C)` - Piton since function lost

### B. Lifecycle State Predicates

**Genesis Predicate: `Genesis(C, t₀, type)`**
```
Genesis(C, t₀, Mountain) ≡ false                    [Mountains have no genesis]
Genesis(C, t₀, Rope) ≡ Created(C, t₀) ∧ ε(C) ≤ 0.15
Genesis(C, t₀, Tangled) ≡ Created(C, t₀) ∧ ε(C) ≥ 0.40 ∧ Coord(C)
Genesis(C, t₀, Snare) ≡ Created(C, t₀) ∧ ε(C) ≥ 0.46 ∧ Supp(C) ≥ 0.60
Genesis(C, t₀, Scaffold) ≡ Created(C, t₀) ∧ ∃t_end > t₀: Sunset(C, t_end)
Genesis(C, t₀, Piton) ≡ false                       [Pitons result from degradation]
```

**Maturation Predicate: `Mature(C, t, type)`**
```
Mature(C, t, type) ≡ Genesis(C, t₀, type) ∧ t₀ < t < t_degrade
                     ∧ Type(C[I], t) = type
                     ∧ Stable_Function(C, t)
```
Constraint operates in designed type without degradation.

**Degradation Event Predicate: `Degrade(C, t_drift, type_before, type_after)`**
```
Degrade(C, t_drift, type_before, type_after) ≡
    Type(C[I], t_drift - Δt) = type_before
    ∧ Type(C[I], t_drift) = type_after
    ∧ type_before ≠ type_after
    ∧ Drift_Event(C, t_drift)
```

**Terminal State Predicate: `Terminal(C, t, state)`**
```
Terminal(C, t, state) ≡ Type(C[I], t) = state
                        ∧ ∀t' > t: (Type(C[I], t') = state ∨ ¬Exists(C, t'))
```
Constraint has reached endpoint (no further transitions expected or possible).

### C. Drift Event Operators (Eleven Types — see also §IIb.H, §IIc.G, §IId.F)

**Type 1 - Metric Substitution: `MS(C, t_drift, V, M)`**
```
MS(C, t_drift, V, M) ≡
    Original_Value(C, V) ∧ Proxy_Metric(C, M, V) at t < t_drift
    ∧ Optimization_Shift(C, V → M) at t_drift
    ∧ Value_Harm(V, M, t > t_drift)
    ∧ ε(C, t_drift) > ε(C, t₀) + 0.30
```
Example: `MS(social_media, 2012, connection, engagement)`
- Connection → engagement optimization, extraction increased

**Type 2 - Extraction Accumulation: `EA(C, t_start, t_drift)`**
```
EA(C, t_start, t_drift) ≡
    ☰C[I] at t_start                         [was Rope]
    ∧ ε(C, t_start) ≤ 0.15
    ∧ ε(C, t_drift) ≥ 0.40
    ∧ ∀t ∈ [t_start, t_drift]: ∂ε/∂t > 0    [monotonic extraction increase]
    ∧ ☰☙C[I] at t_drift                      [now Tangled Rope]
```
Example: `EA(academic_publishing, 1970, 1995)`
- Rope → Tangled Rope via commercial capture

**Type 3 - Coordination Loss: `CL(C, t_drift)`**
```
CL(C, t_drift) ≡
    ☰☙C[I] at t < t_drift                    [was Tangled Rope]
    ∧ Coord(C, t < t_drift) = true
    ∧ Coord(C, t ≥ t_drift) = false
    ∧ ε(C, t_drift) ≥ ε(C, t₀)               [extraction persists or increases]
    ∧ ☙C[I] at t_drift                       [now Snare]
```
Example: `CL(cable_TV, 2015)`
- Coordination obsolete (streaming exists), extraction persists

**Type 4 - Function Obsolescence: `FO(C, t_drift, E)`**
```
FO(C, t_drift, E) ≡
    Environment_Shift(E, t_drift)
    ∧ Function_Necessary(C, E_old) = true
    ∧ Function_Necessary(C, E_new) = false
    ∧ Structure_Persists(C, t > t_drift)
    ∧ ☠C at t_drift                          [becomes Piton]
```
Example: `FO(fax_requirements, 2005, email_adoption)`
- Environment changed, function obsolete, structure persists

**Type 5 - Sunset Violation: `SV(C, t_sunset, t_actual)`**
```
SV(C, t_sunset, t_actual) ≡
    ☒C(t < t_sunset)                         [was Scaffold]
    ∧ Sunset_Clause(C, t_sunset)
    ∧ Purpose_Complete(C, t_sunset)
    ∧ t_actual > t_sunset
    ∧ ¬Dismantled(C, t_sunset)
    ∧ ☠C at t_actual                         [becomes Piton]
```
Example: `SV(thailand_senate_veto, 2024, 2025)`
- PM power expired 2024, structure persists as piton

**Type 6 - Extraction Collapse: `EC(C, t_drift)`**
```
EC(C, t_drift) ≡
    (☰☙C[I] ∨ ☙C[I]) at t < t_drift          [was Tangled or Snare]
    ∧ ε(C, t < t_drift) ≥ 0.40
    ∧ Extraction_Source_Eliminated(C, t_drift)
    ∧ ε(C, t ≥ t_drift) < 0.20
    ∧ ☠C at t_drift                          [becomes Piton]
```
Example: `EC(soviet_committees, 1991)`
- Regime collapse → extraction mechanism gone → piton remains

**Type 7 - Algorithm Shutdown → Internalized Piton: `AS_IP(C, t_shutdown)`**
```
AS_IP(C, t_shutdown) ≡
    ☙C[I] at t < t_shutdown                  [was Snare]
    ∧ Extraction_Mechanism(C, M, t < t_shutdown)
    ∧ Mechanism_Removed(C, M, t_shutdown)
    ∧ Behavior_Persists(C, t > t_shutdown)   [habits internalized]
    ∧ Population_Unaware_Mechanism_Gone(C, t > t_shutdown)
    ∧ I-Piton(C) at t > t_shutdown           [Internalized Piton]
```
Example: `AS_IP(social_media_doomscroll, 2025_hypothetical)`
- Algorithm disabled, doom-scrolling habit persists

### D. State Transition Rules

**Rule T1 (Rope Degradation):**
```
☰C[I](t₀) ∧ EA(C, t₀, t₁)
──────────────────────────
    ☰☙C[I](t₁)
```
Rope + Extraction Accumulation → Tangled Rope

**Rule T2 (Tangled Rope Degradation to Snare):**
```
☰☙C[I](t₀) ∧ CL(C, t₁)
──────────────────────────
    ☙C[I](t₁)
```
Tangled Rope + Coordination Loss → Snare

**Rule T3 (Any → Piton via Function Loss):**
```
Type(C[I], t₀) ∈ {☰, ☰☙} ∧ FO(C, t₁, E)
──────────────────────────────────────────
    ☠C(t₁)
```
Rope or Tangled Rope + Function Obsolescence → Piton

**Rule T4 (Scaffold → Piton via Sunset Violation):**
```
☒C(t₀) ∧ SV(C, t_sunset, t₁)
────────────────────────────
    ☠C(t₁)
```
Scaffold + Sunset Violation → Piton

**Rule T5 (Snare → I-Piton via Algorithm Shutdown):**
```
☙C[I](t₀) ∧ AS_IP(C, t_shutdown)
────────────────────────────────
    I-Piton(C)(t₁)
```
Snare + Algorithm Shutdown → Internalized Piton

**Rule T6 (Successful Scaffold Dissolution):**
```
☒C(t₀) ∧ Purpose_Complete(C, t_sunset) ∧ Dismantled(C, t_sunset)
─────────────────────────────────────────────────────────────────
    ¬Exists(C, t > t_sunset)
```
Scaffold + Purpose Complete + Actually Dismantled → Successful dissolution (no residue)

**Rule T7 (Tangled Rope Reform → Rope):**
```
☰☙C[I](t₀) ∧ Reform(C, t₁) ∧ Extraction_Excised(C, t₁) ∧ Coord_Preserved(C, t₁)
─────────────────────────────────────────────────────────────────────────────────
    ☰C[I](t₁)
```
Tangled Rope + Successful Reform → Rope (rare but possible)

### E. Temporal Invariants and Properties

**Temporal Invariant TI1 (Mountain Permanence):**
```
▲C[I](t₀) → □▲C[I](t)
```
If Mountain at any time, Mountain at all times (within civilizational scope).

**Temporal Invariant TI2 (Piton Irreversibility):**
```
☠C(t₁) → ∀t > t₁: (☠C(t) ∨ ¬Exists(C, t))
```
Once Piton, either stays Piton or gets eliminated (no resurrection to functional state).

**Temporal Property TP1 (Scaffold Finite Lifetime):**
```
☒C(t₀) → ∃t_end: (¬Exists(C, t > t_end) ∨ Degrade(C, t_end, Scaffold, Piton))
```
All Scaffolds either dissolve successfully or degrade to Piton (cannot persist indefinitely in designed state).

**Temporal Property TP2 (Extraction Monotonicity - Unchecked):**
```
Type(C[I], t) ∈ {☰, ☰☙} ∧ ¬Reform(C) → ∂ε/∂t ≥ 0
```
Without active reform, extraction tends to accumulate (entropy increases).

**Temporal Property TP3 (Degradation Asymmetry):**
```
Rate(Rope → Piton) >> Rate(Piton → Rope)
```
Degradation much faster than repair (easier to break than fix).

### F. Lifecycle Velocity Parameters

**Fast Lifecycle (months to years):**
```
Velocity(C) = "fast" ≡ (t_terminal - t₀) < 10 years
```
Example: Tech startups, social media platforms, market trends

**Medium Lifecycle (decades to generations):**
```
Velocity(C) = "medium" ≡ 10 years ≤ (t_terminal - t₀) < 100 years
```
Example: Professional norms, institutional practices, social conventions

**Slow Lifecycle (generations to centuries):**
```
Velocity(C) = "slow" ≡ (t_terminal - t₀) ≥ 100 years
```
Example: Constitutional structures, civilizational norms, religious institutions

**Velocity affects intervention timing:**
```
Velocity(C) = "fast" → Monitor_Frequently(C, monthly)
Velocity(C) = "medium" → Monitor_Periodically(C, annually)
Velocity(C) = "slow" → Monitor_Generationally(C, decade)
```

### G. Predictive Temporal Formulas

**Degradation Prediction: `Prob_Degrade(C, t_current, t_future, type_after)`**
```
Prob_Degrade(C, t_current, t_future, Piton) =
    α · Age(C) + β · ε_trend(C) + γ · Maintenance_Level(C)

where:
    Age(C) = t_current - t₀
    ε_trend(C) = ∂ε/∂t (extraction accumulation rate)
    Maintenance_Level(C) ∈ [0, 1] (active reform/monitoring effort)
    α, β, γ calibrated from corpus
```

**Reform Success Prediction: `Prob_Reform_Success(C, t)`**
```
Prob_Reform_Success(☰☙C, t) =
    f(Separability(Coord, Extract), Power_Coalition(Reform), Velocity(C))

where:
    Separability ∈ [0, 1]     (can functions be separated?)
    Power_Coalition ∈ [0, 1]  (reform coalition strength)
    Velocity = "fast" → Higher success (easier to change)
    Velocity = "slow" → Lower success (inertia dominates)
```

**Piton Persistence Prediction: `Expected_Persistence(☠C, t)`**
```
Expected_Persistence(☠C, t) =
    Identity_Fusion(C) · Sunk_Cost(C) · (1 - Harm_Level(C))

where:
    Identity_Fusion ∈ [0, 1]  (is C part of "who we are"?)
    Sunk_Cost ∈ [0, 1]       (investment in C's history)
    Harm_Level ∈ [0, 1]      (active harm from C)

High persistence = low probability of elimination
```

### H. Temporal Axioms

**Axiom TA1 (Directionality of Time):**
```
t₁ < t₂ → ¬(t₂ < t₁)
```
Time is linearly ordered (no time travel, no loops).

**Axiom TA2 (Causal Precedence):**
```
Degrade(C, t_drift, type₁, type₂) → ∃E: Event(E, t ≤ t_drift) ∧ Causes(E, Degrade)
```
All degradation events have causal precedents (drift events).

**Axiom TA3 (State Persistence):**
```
Type(C[I], t₁) = τ ∧ ¬Degrade(C, t ∈ (t₁, t₂)) → Type(C[I], t₂) = τ
```
Without degradation event, type persists (inertia).

**Axiom TA4 (Lifecycle Completeness):**
```
Created(C, t₀) → (∃t_terminal: Terminal(C, t_terminal, state)) ∨ □Exists(C, t)
```
All created constraints either reach terminal state or persist indefinitely (Mountains).

---


## IIb. Boltzmann Compliance and Coupling Topology (v5.0)

**Structural extension:** Mountains claim to be natural laws — constraints that emerge from physics, logic, or mathematical necessity rather than human design. The Boltzmann compliance framework provides a formal test for this claim, drawing on Tamuz & Sandomirskiy (2025): the Boltzmann distribution is the ONLY probability law describing uncoupled systems. If a constraint claimed as natural shows cross-index coupling, it fails the independence test that real natural laws satisfy.

### A. Cross-Index Coupling Detection

**Coupling Score: `CouplingScore(C) ∈ [0,1]`**

Tests whether a constraint's classification factorizes across Power × Scope dimensions. If changing power level has *different effects* at different scope levels, the constraint exhibits coupling — a signature of constructed, not natural, origin.

```
CouplingScore(C) = min(1.0, Violations / MaxViolations)

where:
  Grid = {(P, S) | P ∈ {powerless, moderate, analytical}, S ∈ {local, national, global}}
  Violations = |{(P, S₁, S₂) | Type(C[P,S₁]) ≠ Type(C[P,S₂]) for S₁ ≠ S₂}|
  MaxViolations = |Grid| × (|Scopes| - 1)
```

Three coupling regimes:
- `independent` (Score = 0.0): Classification factorizes perfectly — consistent with natural law
- `weakly_coupled` (0 < Score ≤ 0.40): Minor coupling — may reflect measurement noise or edge cases
- `strongly_coupled` (Score > 0.40): Significant coupling — inconsistent with natural law claim

### B. Complexity-Adjusted Threshold

Not all coupling indicates fraud. Complex coordination mechanisms (global infrastructure, enforcement systems) inherently exhibit some coupling because their effects genuinely vary by context. The threshold adjusts by coordination type:

```
Threshold(C) = BaseThreshold + Offset(coordination_type(C))

BaseThreshold = 0.15

Offsets:
  information_standard    = 0.00  (pure information — no expected coupling)
  resource_allocation     = 0.05  (allocation effects vary somewhat by context)
  enforcement_mechanism   = 0.08  (enforcement inherently context-dependent)
  global_infrastructure   = 0.15  (global systems have legitimate scope effects)
```

`coordination_type/2` is declared per-constraint in testset files. If undeclared, the default threshold (0.15) applies.

### C. Boltzmann Floor (Price of Anarchy)

Every coordination type has a minimum extraction inherent to its function — the "price of anarchy" that would exist even in an ideal implementation. Extraction below this floor is not exploitative; it's the structural cost of coordination.

```
Floor(coordination_type) → ℝ

  information_standard    = 0.02  (near-zero overhead)
  resource_allocation     = 0.15  (allocation requires infrastructure)
  enforcement_mechanism   = 0.10  (enforcement has inherent costs)
  global_infrastructure   = 0.20  (global coordination is expensive)
  default                 = 0.05  (when type unknown)
```

**Excess Extraction:**
```
ExcessExtraction(C) = max(0, ε(C) - Floor(coordination_type(C)))
```

Excess extraction measures extraction *beyond* what the coordination type structurally requires. A constraint with ε = 0.15 and Floor = 0.15 has zero excess — it extracts exactly what coordination costs.

### D. Boltzmann Invariant Mountain Test

A constraint passes the Boltzmann invariant test if it satisfies four conditions:

```
BoltzmannInvariant(C) = invariant iff:
  1. Factorization:      CouplingScore(C) ≤ Threshold(C)
  2. Scope Invariance:   ∀S₁,S₂: Type(C[P,S₁]) = Type(C[P,S₂]) for fixed P
  3. No Excess:          ExcessExtraction(C) ≤ 0.02
  4. Natural Signature:  NL(C) holds (from §II.E)

BoltzmannInvariant(C) = variant iff any condition fails with sufficient data
BoltzmannInvariant(C) = inconclusive iff insufficient classifications (< 3)
```

**Relationship to Mountain classification:** A Mountain that fails the Boltzmann invariant test is not necessarily misclassified — it may be a Mountain from the metric perspective that exhibits non-natural structural properties. The test provides diagnostic evidence, not classification override.

### E. Coupling-Aware Reformability

Reform difficulty depends on how deeply coupled a constraint's extraction is with its coordination function:

```
ReformScore(C) = 0.30 × Separability(C) + 0.40 × (1 - CouplingScore(C)) + 0.30 × ExcessFactor(C)

where:
  Separability(C) ∈ [0,1]  (can coordination be separated from extraction?)
  ExcessFactor(C) = min(1.0, ExcessExtraction(C) / ε(C))  (fraction that is excess)
```

Range [0,1] where 1.0 = easily reformable, 0.0 = extraction deeply entangled with coordination.

### F. Boltzmann-Derived Signatures

Three new structural signatures extend the detection pattern operators from §II.C:

**False Natural Law: `FNL(C)`**
```
FNL(C) ↔ Claimed_Natural(C) ∧ BoltzmannInvariant(C) = variant
         ∧ CouplingScore(C) > Threshold(C)
```
Constraint claims to be a natural law (Mountain) but fails the Boltzmann independence test. The coupling reveals constructed origin disguised as physics ("physics-washed").

**Coupling-Invariant Rope: `CI_Rope(C)`**
```
CI_Rope(C) ↔ BoltzmannInvariant(C) = compliant
             ∧ Scope_Invariant(C)
             ∧ ExcessExtraction(C) ≤ 0.02
             ∧ Coord(C)
```
Certified coordination mechanism. Passes all Boltzmann structural tests — extraction is minimal and consistent with coordination costs. This is what good ropes look like under structural scrutiny.

**False CI-Rope: `FCR(C)`**
```
FCR(C) ↔ Appears_As_Rope(C) ∧ ∃Test ∈ {Factorization, Scope, Excess, Coupling}: Fails(C, Test)
```
Constraint appears to be a rope by metrics but fails one or more Boltzmann structural tests. "Coordination-washed" — hides extraction behind low metrics, distributed enforcement, or behavioral defaults.

### G. Drift Events 8-9

**Type 8 - Coupling Drift: `CD(C, t_drift)`**
```
CD(C, t_drift) ≡
    CouplingScore(C, t < t_drift) ≤ Threshold(C)
    ∧ CouplingScore(C, t ≥ t_drift) > Threshold(C)
```
Constraint that was Boltzmann-compliant develops cross-index coupling over time — coordination mechanism is being captured.

**Type 9 - Boltzmann Floor Drift: `BFD(C, t_drift)`**
```
BFD(C, t_drift) ≡
    ExcessExtraction(C, t < t_drift) ≤ 0.02
    ∧ ExcessExtraction(C, t ≥ t_drift) > 0.02
```
Extraction rises above the coordination-type floor — the constraint is extracting more than its coordination function requires.

---

## IIc. Structural Purity (v5.1)

**Structural extension:** While Boltzmann compliance tests whether a constraint *claims* to be natural, purity scoring measures how *cleanly* it operates regardless of type. A high-purity snare extracts efficiently without coupling artifacts; a low-purity rope has structural contamination that undermines its coordination function.

### A. Purity Score

**Purity: `Purity(C) ∈ [0,1]`**

```
Purity(C) = 0.30 × F + 0.25 × SI + 0.25 × CC + 0.20 × EX

where:
  F  = Factorization subscore    = 1 - CouplingScore(C)
  SI = Scope Invariance subscore = 1.0 if scope-invariant, penalized by 0.25 per extra type
  CC = Coupling Cleanliness      = 1.0 - max(nonsensical_coupling_strength)
  EX = Excess Extraction subscore = 1 - min(1.0, ExcessExtraction(C) × 2)
```

Returns -1.0 (sentinel) when epistemic data is insufficient for computation.

### B. Structural Purity Classification

```
Purity(C) ≥ 0.85              → pure_natural_law    (if NL(C))
                                  pure_coordination   (if CS(C) or CI_Rope(C))
                                  pure_scaffold       (if Scaffold(C))
0.70 ≤ Purity(C) < 0.85       → sound               (structurally clean)
0.50 ≤ Purity(C) < 0.70       → borderline           (monitoring recommended)
0.30 ≤ Purity(C) < 0.50       → contaminated(Tests)  (failed structural tests listed)
Purity(C) < 0.30              → degraded             (significant structural problems)
Purity(C) = -1.0              → inconclusive         (insufficient data)
```

### C. Purity-Qualified Action Algebra

The action algebra from §VI.B is extended with purity qualifiers:

```
Qualifier(C) =
  stable          if Purity(C) ≥ 0.70  (sound floor)
  monitor         if 0.50 ≤ Purity(C) < 0.70  (escalation floor)
  escalate_reform if 0.30 ≤ Purity(C) < 0.50 ∧ Type(C) ∈ {tangled_rope}
  escalate_cut    if 0.30 ≤ Purity(C) < 0.50 ∧ Type(C) ∈ {snare}
  accelerate_sunset if 0.30 ≤ Purity(C) < 0.50 ∧ Type(C) ∈ {scaffold, piton}
  degraded        if Purity(C) < 0.30
```

**Three Purity Floors:**
- `sound` (0.70): Constraint operates cleanly — standard action algebra applies
- `escalation` (0.50): Structural contamination detected — actions should be more aggressive
- `degraded` (0.30): Severe contamination — constraint may not function as classified

### D. Purity-Adjusted Energy Costs

Purity affects the energy cost of actions. Contaminated constraints are harder to reform and more expensive to maintain:

```
E_reform(C, I) = E_base_reform(C, I) × ReformMultiplier(C)
  ReformMultiplier(C) = 1.0 + 2.0 × (1 - Purity(C))  [up to 3× for degraded]

E_cut(C, I) = E_base_cut(C, I) × CutMultiplier(C)
  CutMultiplier(C) = 1.0 + 0.75 × (1 - Purity(C))    [up to 1.75× for degraded]

E_maintain(C, I) = E_base_maintain(C, I) × MaintainMultiplier(C)
  MaintainMultiplier(C) = 1.0 - 0.2 × Purity(C)       [down to 0.8× for pristine]
```

### E. Action Composition Gates

Purity prerequisites for composed actions:

```
Surgical_Reform(⊞⊠C[I])  requires Purity(C) ≥ 0.30
  [Below 0.30, coordination and extraction are too entangled for surgical separation]

Safe_Transition(⊡C → ¬C)  requires Purity(⊡C) ≥ 0.50
  [Below 0.50, scaffold has too much structural contamination for clean dissolution]

Efficient_Coordination(⊞C[I])  requires Purity(⊞C) ≥ 0.50
  [Below 0.50, rope's coordination function is compromised by structural contamination]
```

### F. Purity Reform Recommendations

When purity is below target, the system identifies which subscores need improvement:

```
Target(C) = max(Current_Purity(C), 0.85)
Deficit(subscore) = Target_contribution - Actual_contribution

Urgency:
  critical  if Purity(C) < 0.30
  high      if 0.30 ≤ Purity(C) < 0.50
  moderate  if 0.50 ≤ Purity(C) < 0.70
  low       if 0.70 ≤ Purity(C) < 0.85
  none      if Purity(C) ≥ 0.85
```

### G. Drift Event 10

**Type 10 - Purity Drift: `PD(C, t_drift)`**
```
PD(C, t_drift) ≡
    Purity(C, t < t_drift) ≥ Floor
    ∧ Purity(C, t ≥ t_drift) < Floor
    ∧ ∃Signal ∈ {extraction_rising, coupling_above_threshold,
                  theater_rising, excess_above_floor}
```
Purity drops below a floor threshold with identifiable decline signals. Decline signals indicate the *source* of purity loss.

---

## IId. Constraint Network Dynamics (v5.2)

**Structural extension:** Constraints do not exist in isolation. A constraint's effective purity depends not only on its intrinsic properties but on the purity of its neighbors. Network dynamics model how contamination propagates through constraint clusters and predict cascade failures.

### A. Network Topology

Three sources of edges between constraints:

```
Edge(C₁, C₂) ←
  1. Explicit declaration:  affects_constraint(C₁, C₂)
  2. Inferred coupling:     shared regulatory domain or causal dependency
  3. Shared agents:         same beneficiary/victim groups
```

`constraint_neighbors(C, Context, Neighbors)` returns the set of constraints connected to C with edge strength and source type.

### B. Contamination Model

Contamination propagates one hop, downward only (from lower-purity to higher-purity neighbors):

```
EdgeContam(C₁ → C₂) = min(Cap, Delta × Attenuation × TypeStrength(C₁))

where:
  Cap = 0.30                    (contamination_cap — maximum single-edge effect)
  Delta = max(0, Purity(C₂) - Purity(C₁))  (purity differential)
  Attenuation = 0.50            (purity_attenuation_factor)
  TypeStrength = emission strength by constraint type:
    snare         = 1.0   (maximum contamination emission)
    piton         = 0.8
    tangled_rope  = 0.5
    scaffold      = 0.2
    rope          = 0.1
    mountain      = 0.0   (zero emission — natural laws don't contaminate)
```

**Type Immunity** (receiving side):
```
Immunity(type) = 1.0 - TypeStrength(type)
  mountain      = 1.0   (fully immune)
  rope          = 0.9
  scaffold      = 0.8
  tangled_rope  = 0.5
  piton         = 0.2
  snare         = 0.0   (fully susceptible — already contaminated)
```

**Effective Purity:**
```
EffectivePurity(C) = max(0, IntrinsicPurity(C) - TotalContam(C) × Immunity(Type(C)))

where TotalContam(C) = Σ EdgeContam(Neighbor → C) for all neighbors
```

### C. Network Metrics

```
WeakestLink(Network) = min(EffectivePurity(C)) for C ∈ Network
AveragePurity(Network) = mean(EffectivePurity(C)) for C ∈ Network
AtRiskCount(Network) = |{C | EffectivePurity(C) < 0.50}|
ClusterPurity(Cluster) = mean(EffectivePurity(C)) for C ∈ Cluster
```

### D. Network-Qualified Actions

When network contamination significantly reduces effective purity:

```
If EffectivePurity(C) - IntrinsicPurity(C) ≤ -0.05:
  Use EffectivePurity(C) for action qualification (§IIc.C)
  instead of IntrinsicPurity(C)
```

This means a constraint with intrinsic purity of 0.72 (sound) but effective purity of 0.55 (due to contaminated neighbors) would be qualified as `monitor` rather than `stable`.

### E. Network Drift Dynamics

**Induced Drift:** When a neighbor is drifting, it can induce drift in connected constraints:

```
InducedVelocity(C) = Σ(Rate_i × Sensitivity_i) for drifting neighbors i

where:
  Rate_i = drift rate of neighbor i (from metric trend analysis)
  Sensitivity_i = EdgeStrength(i, C) × TypeStrength(i)
```

**Cascade Prediction:** Time to cross purity thresholds:

```
TimeToCross(C, Floor) = (EffectivePurity(C) - Floor) / InducedVelocity(C)
  if InducedVelocity(C) > 0
```

**Network Stability Assessment:**
```
NetworkStability =
  stable     if no constraints have induced velocity > threshold (0.01)
  degrading  if 1-2 constraints above threshold
  cascading  if ≥ 3 constraints above threshold (cascade_count_threshold)
```

**Severity Escalation:**
- Hub escalation: If a drifting constraint has degree ≥ hub_degree_threshold (3), severity increases by 1 level
- Multi-source escalation: If drift has multiple contributing neighbors, severity increases by 1 level

### F. Drift Event 11

**Type 11 - Network Drift: `ND(C, t_drift)`**
```
ND(C, t_drift) ≡
    ∃Neighbor: Drifting(Neighbor, t_drift)
    ∧ EdgeContam(Neighbor → C) > 0
    ∧ EffectivePurity(C, t_drift) < IntrinsicPurity(C) - 0.05
```
Purity decline induced by contamination from drifting neighbors, not from the constraint's own metric changes.

---


## III. Inference Rules

### A. Indexed Classification Rules

**Rule M (Mountain Identification)**
```
ε(C) ≤ 0.15 ∧ Supp(C) ≤ 0.05 ∧ Immutable(C, I.T, I.E)
──────────────────────────────────────────────────────
            ∀I(■C[I])  [where immutability gate passes]
```
If base extraction minimal, suppression negligible, and immutability gate passes from I, then Mountain from I. Mountains do not check χ — they test structural properties (raw ε, suppression) and the immutability gate.

**Rule R (Rope Identification)**
```
χ(C, I.P, I.S) ≤ 0.35 ∧ ε(C) ≤ 0.15 ∧ Changeable(C, I.T, I.E)
─────────────────────────────────────────────────────────────────
                      ⊞C[I]
```
If power-and-scope-scaled extraction low, base extraction minimal, and changeable from I, then Rope at I.

**Rule N (Snare Identification)**
```
χ(C, I.P, I.S) ≥ 0.66 ∧ ε(C) ≥ 0.46 ∧ Supp(C) ≥ 0.60 ∧ Changeable(C, I.T, I.E)
─────────────────────────────────────────────────────────────────────────────────
                      ⊠C[I]
```
If power-and-scope-scaled extraction high, base extraction high, suppression high, and changeable from I, then Snare at I.

**Rule TR (Tangled Rope Identification)**
```
0.40 ≤ χ(C, I.P, I.S) ≤ 0.90 ∧ ε(C) ≥ 0.50 ∧ Supp(C) ≥ 0.40
∧ Enforce(C) ∧ Coord(C) ∧ Asymmetric(C)
─────────────────────────────────────────────────────────────────
                      ⊞⊠C[I]
```
If power-and-scope-scaled extraction in hybrid range, base extraction high, suppression moderate, AND has enforcement, coordination function, and asymmetric extraction, then Tangled Rope at I.

**Rule SC (Scaffold Identification)**
```
χ(C, I.P, I.S) ≤ 0.30 ∧ Coord(C) ∧ Sunset(C) ∧ Theater(C) ≤ 0.70
──────────────────────────────────────────────────────────────────
                      ⊡C[I]
```
If power-scaled extraction low, has coordination function, has sunset clause, and theater ratio not excessive, then Scaffold at I.

**Rule Z (Piton Identification)**
```
χ(C, I.P, I.S) ≤ 0.10 ∧ ε(C) > 0.10 ∧ Theater(C) ≥ 0.70
────────────────────────────────────────────────────────────
                      ⊟C[I]
```
If power-scaled extraction negligible, but base extraction still measurable, and theater ratio high (mostly performance), then Piton at I.

**Rule FM (False Mountain Detection)**
```
Claimed(■C) ∧ (ε(C) > 0.70 ∨ ∃I(χ(C, I.P, I.S) > 0.40)) ∧ CC(C)
──────────────────────────────────────────────────────────────────
                    FM(C)
```
If claimed as Mountain but requires high enforcement OR extracts significantly from some index AND has constructed signature, then False Mountain detected.

**Classification priority:** When multiple rules could match, the priority order from §II.F applies: M > N > SC > R > TR > Z > unknown.

### B. Indexical Relativity Rules

**Rule IR1 (Simultaneous Truth)**
```
Type(C[I₁]) = T₁ ∧ Type(C[I₂]) = T₂ ∧ I₁ ≠ I₂
───────────────────────────────────────────────
         T₁ ≠ T₂ is permissible
```
Different types from different indices are not contradictory—both can be objectively true.

**Rule IR2 (Power-Scope-Scaling Explains Variance)**
```
ε(C) = x ∧ (π(I₁.P) ≠ π(I₂.P) ∨ σ(I₁.S) ≠ σ(I₂.S))
───────────────────────────────────────────────────────
    χ(C, I₁.P, I₁.S) ≠ χ(C, I₂.P, I₂.S)
```
Same base extraction experienced differently due to power position and/or scope.

**Rule IR3 (Index Collision Prohibition)**
```
I₁ = I₂ ∧ Type(C[I₁]) ≠ Type(C[I₂])
────────────────────────────────────
           ⊥ (contradiction)
```
Same index must produce same type (0% collision rate empirically validated).

**Rule IR4 (Analytical Perspective Privilege)**
```
I.P = analytical ∧ I.T = civilizational
─────────────────────────────────────────
  Type(C[I]) most likely structural truth
```
Analytical perspective with long time horizon provides most accurate structural classification (but is still indexed, not absolute). Note: π(analytical) = 1.15 provides slight extraction amplification, enabling detection of extraction that moderate agents normalize.

### C. Transformation Rules (Temporal)

**Rule Capture (Rope → Tangled Rope → Snare)**
```
⊞C[I](t₁) ∧ Capture(C, t₁, t₂) ∧ ε(C, t₂) > ε(C, t₁) ∧ Asymmetric(C, t₂)
─────────────────────────────────────────────────────────────────────────
                    ⊞⊠C[I](t₂) [or ⊠C[I](t₂)]
```
A Rope becomes Tangled Rope or Snare through capture as enforcement increases and benefits concentrate.

**Rule Degradation (Tangled Rope → Piton)**
```
⊞⊠C[I](t₁) ∧ EnvChange(t₁, t₂) ∧ ∀I(χ(C, I.P, I.S, t₂) < 0.10) ∧ ε(C, t₂) > 0.10
──────────────────────────────────────────────────────────────────────────────────────
                              ⊟C(t₂)
```
A Tangled Rope becomes Piton when extraction dries up but enforcement persists.

**Rule Calcification (Scaffold → Snare)**
```
⊡C(t) ∧ t > t_end ∧ ¬Terminated(C) ∧ ∃S(Capture(C, S))
──────────────────────────────────────────────────────
                    ⊠C[I](t)
```
A Scaffold becomes Snare if it persists past sunset and develops concentrated beneficiaries.

**Rule Discovery (False Mountain → Actual Type)**
```
FM(C) ∧ Investigate(C) → Reveal(Type(C[I]))
where Type(C[I]) ∈ {⊞, ⊞⊠, ⊠} depending on χ(C, I.P, I.S) and structural properties
```
False Mountain detection reveals actual constraint type through investigation.

### D. Modal Composition Rules

**Necessity Inheritance (Mountains constrain all indices)**
```
∀I(■C₁[I]) ∧ (C₁ → C₂) ∧ NL(C₂)
───────────────────────────────
        ∀I(■C₂[I])
```
If C₁ is necessary from all indices and logically implies C₂ which has natural law signature, then C₂ is necessary from all indices.

**Power-Position Divergence (Same C, Different Types)**
```
χ(C, powerless, S) = x₁ ∧ χ(C, institutional, S) = x₂ ∧ x₁ ≥ 0.66 ∧ x₂ ≤ 0.35
──────────────────────────────────────────────────────────────────────────────────
  ⊠C[I_powerless] ∧ ⊞C[I_institutional] (Snare for some, Rope for others)
```
Same constraint can be Snare for powerless and Rope for institutional due to power-scaling.

**Tangled Rope Decomposition (Hybrid Analysis)**
```
⊞⊠C[I]
────────────────────────────────────────────────
∃C_coord, C_extract: C = C_coord ⊕ C_extract ∧
                      ⊞C_coord[I] ∧ ⊠C_extract[I]
```
Tangled Ropes can be analyzed into coordination core + extraction mechanism (for reform purposes).

---

## IV. Error Logic: Indexed Misclassification Consequences

### A. Type I Error: False Mountain (Misclassification)

**Formal:**
```
Believe(■C[I]) ∧ ¬■C[I] → Wasted(Agency) ∧ Suffered(Unnecessarily)
```

**Important Distinction:**
This error usually stems from:
1. **Genuine confusion** (most common) - lack of indexical framework
2. **Motivated reasoning** - position benefits from belief
3. **Intentional misrepresentation** (rare) - deliberate false claims

**Consequence Chain:**
```
■C[I] (false belief from index I)
  → Accept(C) (incorrect action)
  → ¬Resist(C) (forgone resistance)
  → Continue(C) (unnecessary constraint persists)
  → Energy(wasted) ∧ Freedom(lost)
```

**Error Cost: Severe**
- Treat changeable constraint as unchangeable
- Surrender agency unnecessarily
- Enable extractive structures to persist
- Waste life accepting artificial limits

**Example:**
```
Claimed: ∀I(■("Humans naturally form hierarchies"))
Actual: ⊠("Hierarchy"[I_powerless]) ∧ ⊞("Hierarchy"[I_institutional])
Error: Accept hierarchy as natural → Enable extraction from powerless
Cost: Freedom sacrificed to false necessity
```

### B. Type II Error: Mountain Denial (Fighting Physics)

**Formal:**
```
Believe(⊞C[I] ∨ ⊠C[I] ∨ ⊞⊠C[I]) ∧ ∀I(■C[I]) → Wasted(Energy) ∧ Failed(Attempt)
```

**Consequence Chain:**
```
¬■C (false belief for all indices)
  → Attempt(Change(C)) (doomed effort)
  → Reality(resists) (inevitable failure)
  → Energy(depleted) ∧ Morale(damaged)
  → Possible: System(collapse) if critical Mountain
```

**Error Cost: Variable**
- Low cost: Waste energy on impossible fight (thermodynamics denial)
- High cost: Catastrophic failure (denying structural limits)
- Opportunity cost: Energy could address actual changeable problems

**Example:**
```
Claimed: ⊞("Eliminate scarcity through policy"[I_institutional])
Actual: ∀I(■("Thermodynamic scarcity"))
Error: Fight Mountain → Waste energy → Policy failure
Cost: Resources spent on impossible, real problems neglected
```

### C. Type III Error: Snare Misclassified as Rope

**Formal:**
```
Believe(⊞C[I]) ∧ ⊠C[I] → Maintained(Extraction) ∧ Normalized(Oppression)
```

**Consequence Chain:**
```
⊠C[I] (actual Snare from index I)
  → Believe(⊞C[I]) (misclassified as Rope)
  → Maintain(C) (preserve extraction)
  → Enable(χ(C, oppressor.P, oppressor.S) < 0) (extraction continues, beneficiaries profit)
  → Justice(denied)
```

**Error Cost: Severe (Justice)**
- Legitimize extraction as coordination
- Preserve unnecessary suffering
- Block resistance by claiming functionality
- Enable power to hide as necessity

**Example:**
```
Claimed: ⊞("Copyright protects creators"[I_moderate])
Actual: ⊠("Copyright monopolies"[I_powerless]) ∧ ⊞("Copyright"[I_institutional])
Error: Defend as universal Rope → Maintain extraction from powerless creators
Cost: Access denied, innovation blocked, extraction normalized
```

### D. Type IV Error: Rope Misclassified as Snare

**Formal:**
```
Believe(⊠C[I]) ∧ ⊞C[I] → Destroyed(Coordination) ∧ Lost(Collective_Benefit)
```

**Consequence Chain:**
```
⊞C[I] (actual Rope from index I)
  → Believe(⊠C[I]) (misclassified as Snare)
  → Cut(C) (destroy coordination)
  → Lose(Collective_Function) (coordination benefit lost)
  → Worse_Outcome (coordination problem resurfaces)
```

**Error Cost: Moderate to Severe**
- Destroy functional coordination
- Lose collective benefits
- Create coordination vacuum
- May enable actual Snare to fill gap

**Example:**
```
Claimed: ⊠("Building codes"[I_developer])
Actual: ⊞("Building codes"[I_resident])
Error: Cut as Snare → Buildings collapse → Deaths
Cost: Safety coordination destroyed
```

### E. Type V Error: Piton Misclassified as Active Constraint

**Formal:**
```
⊟C ∧ Believe(⊞C[I] ∨ ⊠C[I]) → Wasted(Political_Capital) ∧ Opportunity(Lost)
```

**Consequence Chain:**
```
⊟C (actual Piton)
  → Believe(⊠C[I]) (misclassified as active Snare)
  → Fight(C) (waste political capital)
  → Win(Bypass_Available) (could have bypassed easily)
  → Exhausted(Resources) (opportunity cost high)
```

**Error Cost: Moderate (Efficiency)**
- Waste political capital on non-threat
- Miss opportunity for easy bypass
- Drain organizing energy
- Neglect actual active Snares

**Example:**
```
⊟("Fax requirement") ∧ Believe(⊠("Fax requirement"[I_powerless]))
Error: Organize campaign against dead requirement → Waste energy
Better: Bypass (use email, ignore fax)
Cost: Political capital spent on piton, not living constraints
```

### F. Type VI Error: Tangled Rope Mishandled

**Formal (Two sub-errors):**
```
⊞⊠C[I] ∧ Believe(⊞C[I]) → Maintained(Extraction)  [Sub-error A]
⊞⊠C[I] ∧ Believe(⊠C[I]) → Destroyed(Coordination) [Sub-error B]
```

**Error A: Treat as Pure Rope**
- Maintain entire system → Preserve extraction mechanism
- Defend coordination function → Legitimize extraction
- Miss reform opportunity

**Error B: Treat as Pure Snare**
- Cut entire system → Destroy coordination benefit
- Fight extraction → Lose collective function
- Create worse outcome

**Correct Response:**
```
⊞⊠C[I] → Reform(C):
  Preserve(C_coord) ∧ Excise(C_extract)
```

**Example:**
```
⊞⊠("Carbon credits"[I_moderate])

Error A: "It's all good coordination" → Maintain financial intermediation
Error B: "It's all extraction" → Destroy price signal mechanism
Correct: "Hybrid - preserve price signal, cut financial rents"
```

---

## V. Theorems

### Theorem 1: Indexical Relativity (Core)
```
∀C ∃I₁, I₂ (I₁ ≠ I₂ ∧ Type(C[I₁]) ≠ Type(C[I₂]))
```
**Empirical validation:** >99% of constraints in the 691-constraint corpus show high variance across indices.

**Proof sketch:** Power-scaling function π(P) and scope-scaling function σ(S) create differential extraction. Combined with immutability perception varying by time horizon, same constraint appears as different types from different power-time-scope positions.

**Implication:** "Is X a Mountain or Snare?" is ill-formed without specifying index.

### Theorem 2: Index Sufficiency
```
∀C ∀I₁, I₂ (I₁ = I₂ → Type(C[I₁]) = Type(C[I₂]))
```
**Empirical validation:** 0% collision rate within formal system across 691 constraints.

**Proof sketch:** Four indices (WHO, WHEN, WHERE, HOW MUCH) fully determine classification without hidden variables. Same index configuration always produces same type.

**Nuance from invariant analysis:** Scope (HOW MUCH) now affects classification via σ(S) in the χ formula. 19 of 691 constraints are scope-sensitive — they change classification when scope varies while other indices remain fixed. This does not violate index sufficiency (the full 4-tuple still determines type uniquely) but demonstrates that scope is not inert.

**Implication:** No fifth index needed for disambiguation within the formal model.

### Theorem 3: Power-Scope-Scaling Explains Variance
```
Without π(P) and σ(S): Collision_Rate(C, I₁, I₂) = 8.2%
With π(P) only:         Collision_Rate(C, I₁, I₂) ≈ 0.4%
With π(P) and σ(S):     Collision_Rate(C, I₁, I₂) = 0%
```
**Empirical validation:** Corpus analysis shows power-and-scope-scaling essential for collision-free classification.

**Proof sketch:** Without power modifiers, agents with different power at same structural position would need hidden variable to explain classification differences. Power-scaling makes differences explicit and measurable. Scope-scaling captures verification difficulty at different spatial scales — global constraints hide more extraction behind complexity.

**Implication:** Power and scope are not hidden variables but observable structural properties that modulate extraction.

### Theorem 4: Tangled Rope Necessity
```
∃C: ⊞⊠C ∧ ¬Decomposable_To(⊞C ∨ ⊠C)
```
**Empirical validation:** ~36% of constraints show irreducible hybrid pattern.

**Proof sketch:** Many constraints designed from inception to both coordinate AND extract. Decomposition into pure Rope + pure Snare loses explanatory power. Hybrid is primitive, not emergent.

**Implication:** Six categories (not three or four) are empirically necessary — Mountain, Rope, Tangled Rope, Snare, Scaffold, and Piton each capture distinct structural patterns that cannot be reduced to the others.

### Theorem 5: Load-Bearing Constraint Theorem
```
Critical(C) ∧ Cut(C) ∧ ¬Build(⊡C') → Collapse
```
**Proof sketch:** If C bears critical load (prevents worse outcome) and is cut without scaffold replacement, system collapses to worse state.

**Implication:** Some Snares and Tangled Ropes are load-bearing. Cutting requires scaffolding.

### Theorem 6: Error Asymmetry (Indexed)
```
Cost(Type_I, I) ≈ Cost(Type_III, I) > Cost(Type_VI, I) > Cost(Type_V) > Cost(Type_II, I) > Cost(Type_IV, I)
```

**Proof sketch:**
- Type I (false Mountain): Surrender agency → severe loss from I's position
- Type III (Snare as Rope): Maintain extraction → severe injustice from I's position
- Type VI (Tangled Rope mishandled): Lose coordination OR maintain extraction → moderate to severe
- Type V (Piton as active): Waste capital → moderate inefficiency
- Type II (false Rope/Snare): Waste energy → variable cost
- Type IV (Rope as Snare): Destroy coordination → moderate to severe

**Implication:** When uncertain, err toward avoiding Type I and Type III errors from your index position.

### Theorem 7: Power's Indexical Move
```
Power(X, Y) ↔ Control(X, Belief(Y, Type(C[I_Y])))
```
**Proof sketch:** Power operates by controlling others' indexical beliefs. Make them believe their Snares are Mountains (false necessity), or that Mountains are negotiable (dangerous hubris).

**Key insight:** Power operates indexically—controlling what type Y believes C is from Y's position.

**Implication:** Political struggle is often about indexical classification, not truth per se.

---

## VI. Decision Logic

### A. Indexed Classification Decision Tree

```
Evidence(C, I) → Measure → Classify → Act

Measurement:
  ε(C)          [Base extraction — structural property]
  χ(C, I.P, I.S) [Power-and-scope-scaled extraction from index I]
  Supp(C)       [Suppression requirement — structural property]
  Theater(C)    [Theater ratio — performance vs substance]
  Coord(C)      [Coordination function presence]
  Asymmetric(C) [Beneficiary distribution]
  Enforce(C)    [Active enforcement requirement]
  Sunset(C)     [Sunset clause presence]

Classification (priority order):
  ■C[I]   iff ε(C) ≤ 0.15 ∧ Supp(C) ≤ 0.05 ∧ Immutable(C, I.T, I.E)
  ⊠C[I]   iff χ(C, I.P, I.S) ≥ 0.66 ∧ ε(C) ≥ 0.46 ∧ Supp(C) ≥ 0.60 ∧ Changeable(C, I.T, I.E)
  ⊡C[I]   iff χ(C, I.P, I.S) ≤ 0.30 ∧ Coord(C) ∧ Sunset(C) ∧ Theater(C) ≤ 0.70
  ⊞C[I]   iff χ(C, I.P, I.S) ≤ 0.35 ∧ ε(C) ≤ 0.15 ∧ Changeable(C, I.T, I.E)
  ⊞⊠C[I]  iff 0.40 ≤ χ ≤ 0.90 ∧ ε ≥ 0.50 ∧ Supp ≥ 0.40 ∧ Enforce(C) ∧ Coord(C) ∧ Asymmetric(C)
  ⊟C[I]   iff χ(C, I.P, I.S) ≤ 0.10 ∧ ε(C) > 0.10 ∧ Theater(C) ≥ 0.70

Detection:
  FM(C)   iff Claimed(■C) ∧ ε(C) > 0.70

Action(Type, I):
  ■C[I]   → Accept(C) ∧ Navigate(C)
  ⊞C[I]   → Maintain(C) ∨ Careful_Reform(C)
  ⊞⊠C[I]  → Surgical_Reform(C): Preserve(Coord) ∧ Excise(Extract)
  ⊠C[I]   → Cut(C) ∨ Exit(C) [with Scaffold if load-bearing]
  ⊡C[I]   → Monitor_Sunset(C) ∧ Prepare_Dissolution(C)
  ⊟C      → Bypass(C) ∧ ¬Fight(C)
```

### B. Action Algebra (Indexed)

**Operator Precedence:**
```
Accept ≫ Surgical_Reform ≫ (Cut ∨ Exit) ≫ Maintain ≫ Monitor_Sunset ≫ Bypass ≫ Careful_Reform
```

**Composition Rules:**
```
Accept(■C[I]) ⊕ Cut(⊠C'[I]) = Navigate(■C) ∧ Resist(⊠C')
  [Can accept Mountains while cutting Snares from same index]

Cut(⊠C₁[I]) ⊕ Build(⊡C₂) = Safe_Transition(C₁ → ¬C₁)
  [Cutting Snare requires Scaffold if load-bearing]

Surgical_Reform(⊞⊠C[I]) = Preserve(C_coord) ⊕ Excise(C_extract)
  [Tangled Rope requires decomposition, not simple cut or maintain]

Maintain(⊞C[I]) ⊕ Bypass(⊟C') = Efficient_Coordination
  [Keep functional Ropes, ignore Pitons]

Monitor_Sunset(⊡C[I]) ⊕ Prepare_Dissolution(⊡C[I]) = Responsible_Scaffolding
  [Track sunset clause, prepare for clean dissolution]

Accept(■C[I₁]) ∧ Cut(■C[I₂]) ≠ ⊥ when I₁ ≠ I₂
  [Can accept from one index while recognizing changeability from another]
```

### C. Energy Accounting (Indexed)

**Energy Conservation Law:**
```
E_total(I) = E_accept(I) + E_build(I) + E_cut(I) + E_maintain(I) + E_bypass + E_reform(I) + E_monitor(I)

Optimal: Minimize(E_total(I)) subject to Maximize(Agency(I))
```

**Energy Allocation (varies by index):**
```
E(■C[I]) = 0            [Mountains need no energy to accept from any index]
E(⊞C[I]) = O(log n)     [Ropes need occasional maintenance]
E(⊞⊠C[I]) = O(n²)       [Tangled Ropes need complex surgical reform]
E(⊠C[I]) = O(n)         [Snares need active resistance, varies by power]
E(⊡C[I]) = O(n)         [Scaffolds need monitoring and dissolution preparation]
E(⊟C) = O(1)            [Pitons need simple bypass]
```

**Power-Dependent Resistance Costs:**
```
E(⊠C[I_powerless]) > E(⊠C[I_moderate]) > E(⊠C[I_powerful])
```
Same Snare costs more to resist from powerless position.

**Implication:**
```
Fight(■C[I]) → E = ∞         [Infinite energy for zero success]
Fight(⊟C) → E = n, Value ≈ 0 [Linear energy for near-zero value]
Cut(⊠C[I]) → E ∝ π(I.P)      [Energy scales with power position]
Reform(⊞⊠C[I]) → E = n²      [Complex due to decomposition requirement]
```

---

## VII. Meta-Logic: Self-Application

### A. The Framework's Own Indexed Status

**Classification of DR itself:**
```
DR ≝ This_Framework

From analytical index I_analytical:
  Type(DR[I_analytical]) = ⊡  [Scaffold]

Reasoning:
  - ε(DR) > 0  [Requires cognitive effort to use]
  - ∃t_end(Sunset(DR, t_end))  [Designed for obsolescence]
  - Transitional(DR)  [Moves from opaque to legible power]
  - Coord(DR) [Solves indexical disambiguation problem]
```

**From user index I_user:**
```
May vary: ⊞DR[I_novice] (helpful coordination)
          ⊞⊠DR[I_skeptic] (coordinates but seems extractive)
          ⊟DR[I_expert] (once useful, now internalized → bypass)
```

**Sunset Condition:**
```
∀agents ∀I (Automatic(Classify(C[I])) ∧ Automatic(Ask("What type is C from my position?")))
→ Unnecessary(DR) → Should(Dissolve(DR))
```

When constraint-literacy becomes automatic and indexical awareness is universal, the framework should dissolve.

### B. Gödel Limitation (Indexed)

**Incompleteness Applied:**
```
DR ⊬ Consistent(DR) [Cannot prove own consistency]
DR ⊬ Complete(Index_Set) [Cannot prove indices are complete]
```

The framework cannot prove its own consistency or that the four indices are the only possible ones without circular reasoning.

**Response:**
```
Accept(■(Gödel)) ∧ Classify(DR, ⊡) ∧ Empirical_Test(DR)
```

Accept Gödel's theorem as Mountain (applies to all formal systems, including this one), classify DR as Scaffold (temporary tool), test through outcomes rather than proof.

### C. Calibration Loop (Indexed)

**Update Rule:**
```
Believe(Type(C[I], t)) ∧ Evidence(¬Type(C[I]), t+1) → Update(Belief(Type(C[I], t+1)))
```

**Meta-Rule:**
```
¬Update(When_Evidence_Contradicts) → ¬Practicing(DR)
```

If you don't update when reality contradicts your indexed classification, you're not practicing Deferential Realism—you're practicing theology.

**Index-Specific Calibration:**
```
Evidence(Type(C[I₁])) may contradict Belief(Type(C[I₂]))
→ Check: I₁ = I₂?
  If yes: Update(Belief)
  If no: Both may be true (indexical relativity)
```

### D. Known Limitations

**Explicit Uncertainties:**
```
1. Real-world epistemic access differs (same structural position, different information)
   → May create practical collisions not predicted by formal model

2. Power modifiers calibrated from Western economic contexts
   → May need adjustment for different cultural/economic structures

3. Threshold values derived from analyzed corpus (691+ constraints)
   → May need refinement as corpus expands

4. Temporal dynamics of transitions (Rope → Tangled → Snare → Piton)
   → Implemented in drl_lifecycle.pl, predictions uncertain

5. Scope modifier values (σ) are theoretically motivated but empirically untested
   → The verification-difficulty justification is sound but calibration is provisional

6. Boltzmann floor values are provisionally calibrated
   → Floor values for coordination types (§IIb.C) are theoretically motivated but
     based on limited empirical data. The default floor (0.05) and type-specific
     floors may need adjustment as more constraints with coordination_type
     declarations enter the corpus.
```

**Honest Framing:**
```
Claim: Internal consistency (strong)
Claim: Corpus validation (medium to strong)
Claim: Universal applicability (uncertain, requires external validation)
```

---

## VIII. Applications: Proofs Using Indexed Constraint-Logic

**Note on "proofs":** These are formalized arguments using empirically-derived thresholds (χ, ε values from the 691-constraint corpus). They demonstrate the logical structure of the system and show how inference rules operate, but depend on threshold values that may require refinement as more data becomes available. These are logical derivations within the formal system, not mathematical proofs of universal truth.

### Proof 1: Why Revolutions Often Fail (Indexed)

**Claim:** Cutting load-bearing Snare without Scaffold leads to worse outcome from revolutionary index.

**Proof:**
```
1. ⊠C₁[I_revolutionary] ∧ Load_Bearing(C₁)    [Premise: C₁ is Snare from revolutionary perspective]
2. ⊞C₁[I_institutional] ∨ ⊞⊠C₁[I_moderate]    [Same C₁ may coordinate from other indices]
3. Cut(C₁) ∧ ¬Build(⊡C₂)                       [Premise: Cut without Scaffold]
4. Cut(C₁) → ¬Exists(C₁)                       [Definition of Cut]
5. Load_Bearing(C₁) ∧ ¬Exists(C₁) → Collapse   [Theorem 5]
6. Collapse → Worse(I_revolutionary)           [Revolutionaries suffer from collapse]
7. ∴ Cut(⊠C₁[I_rev]) ∧ ¬Build(⊡C₂) → Worse(I_revolutionary) [1-6, modus ponens]
```

**Key insight:** Constraint may be ⊠C from revolutionary index but ⊞C or ⊞⊠C from other indices. Cutting eliminates coordination function others depend on.

**Historical parallels (requiring deeper analysis):** French Revolution → Terror, Russian Revolution → Stalin, Arab Spring → ISIS. These cases suggest the pattern of cutting load-bearing structures without adequate transition support, but each involves complex historical factors beyond the framework's formal scope.

### Proof 2: Why "Just Work Harder" Fails (Index-Blind Error)

**Claim:** Treating Mountain as Rope leads to burnout (index-independent).

**Proof:**
```
1. ∀I(■("Human_Energy_Finite"))                    [Mountain: Biological limit from all indices]
2. Believe(⊞("Human_Energy_Finite"[I_employee]))   [False belief: Think it's negotiable]
3. Believe(⊞C[I]) → Attempt(Increase(C))           [If Rope, try to modify]
4. Attempt(Increase(■C[I])) → Fail ∀I              [Mountains don't yield from any index]
5. Persist(Attempt(Fail)) → Deplete(E)             [Repeated failure depletes energy]
6. Deplete(E) → Burnout                             [Definition]
7. ∴ Believe(⊞(■C[I])) → Burnout                   [2-6, chain]
```

**Implication:** Hustle culture treats biological limits as negotiable Ropes, leading to systemic burnout. Mountains are rare cases where indexical variance doesn't apply—constraints are unchangeable from all positions.

### Proof 3: Why Power Naturalizes Itself (Indexical Deception)

**Claim:** Snares benefit from being misclassified as Mountains.

**Proof:**
```
1. ⊠C[I_powerless] ∧ ⊞C[I_powerful]                    [C is Snare for powerless, Rope for powerful]
2. ⊠C[I_powerless] → Should(Cut(C) ∨ Exit(C))         [Ethical response from powerless index]
3. ∀I(■C) → Should(Accept(C))                          [Ethical response if Mountain from all indices]
4. Make_Believe(∀I(■C)) → Accept(C) ∧ ¬Cut(C)         [If believe Mountain, accept it]
5. ¬Cut(C) → Continue(⊞C[I_powerful])                  [If not cut, powerful keep coordination benefits]
6. ∴ Benefit(Powerful, Make_Powerless_Believe(■C))    [4-5, powerful benefit from false belief]
7. Rational(Powerful) → Promote(Claim(■C))             [Powerful rationally promote false universality]
```

**Key insight:** Power benefits from universalizing its own index. Claiming "this is a Mountain for everyone" when it's actually ⊠C[I_powerless] ∧ ⊞C[I_powerful].

**Implication:** Political struggle often centers on indexical claims: is this constraint unchangeable for everyone, or just from your position?

### Proof 4: Carbon Credits as Indexical Case Study

**Claim:** Carbon credits are simultaneously Mountain, Rope, Tangled Rope, and Snare from different indices.

**Proof:**
```
Given: carbon_credits with ε = 0.40, Supp = 0.60, Coord = true, Asymmetric = true

Index I₁ = (powerless, biographical, trapped, local):
  χ(C, powerless, local) = 0.40 × 1.5 × 0.8 = 0.48
  Immutable(C, biographical, trapped) = true [mountain gate passes]
  BUT ε = 0.40 > 0.15 [mountain epsilon gate fails]
  ∴ ⊞⊠C[I₁] (Tangled Rope — meets hybrid thresholds)
  [From trapped powerless local perspective: entangled coordination/extraction]

Index I₂ = (moderate, biographical, constrained, national):
  χ(C, moderate, national) = 0.40 × 1.0 × 1.0 = 0.40
  0.40 ∈ [0.40, 0.90] ∧ ε = 0.40 ... BUT ε = 0.40 < 0.50 [tangled ε gate fails]
  χ ≤ 0.35? No (0.40 > 0.35) [rope gate fails]
  ∴ Classification depends on structural properties and priority ordering

Index I₃ = (institutional, generational, arbitrage, global):
  χ(C, institutional, global) = 0.40 × -0.2 × 1.2 = -0.096 (net benefit)
  -0.096 ≤ 0.35 ∧ ε = 0.40 ... BUT ε = 0.40 > 0.15 [rope ε gate fails]
  ∴ Signature override may apply — institutional perspective sees net benefit

Index I₄ = (analytical, civilizational, analytical, global):
  χ(C, analytical, global) = 0.40 × 1.15 × 1.2 = 0.552
  0.552 ∈ [0.40, 0.90] ∧ ε = 0.40 ... tangled ε requires ≥ 0.50
  ∴ Classification reveals the extraction the moderate agent normalizes
```

**The analytical perspective (π = 1.15) amplifies extraction just enough to reveal structural extraction that the moderate agent (π = 1.0) would normalize.** This is the design rationale for the moderate-analytical degeneracy break.

---

## IX. Conclusion: What This Indexed Logic Achieves

### A. Precision With Perspectivalism

Traditional logic: Truth-values of propositions (binary: true/false)

**Indexed Constraint-Logic:** Classification-values relative to indices (sextary: ■, ⊞, ⊞⊠, ⊠, ⊡, ⊟) — six full types, each with metric thresholds and structural predicates

**Achievement:** Formal reasoning about **what binds us from where we stand**, not just what's abstractly true.

### B. Modality With Power-Scope-Scaling

Traditional modal logic: Possible/necessary as abstract operators

**Indexed Constraint-Logic:** Degrees of freedom scaled by power position and spatial scope

**Achievement:** Modal operators grounded in **empirical measurement** (decay rates, enforcement requirements) and **power-scope differentials** (extraction modifiers via χ = ε × π(P) × σ(S)).

### C. Error-Awareness With Intentionality Gradation

Traditional logic: Valid/invalid inference

**Indexed Constraint-Logic:** Misclassification types, costs, and intentionality levels (confusion/motivated reasoning/deception)

**Achievement:** Built-in **error taxonomy** showing what goes wrong when classification fails, without assuming malice.

### D. Action-Routing With Index-Sensitivity

Traditional logic: Preserves truth

**Indexed Constraint-Logic:** Routes to index-appropriate strategy

**Achievement:** Formal system that **tells you what to do from your position** based on constraint-type.

### E. Self-Awareness With Limitation Acknowledgment

Traditional logic: Assumes own consistency

**Indexed Constraint-Logic:** Classifies itself as Scaffold, acknowledges Gödel limits, lists known uncertainties

**Achievement:** Meta-logical honesty—**admits its own limitations, temporality, and provisional status**.

### F. Indexical Realism

Traditional logic: Universal truth or pure relativism

**Indexed Constraint-Logic:** Index-relative truth with objective classification within each index

**Achievement:** Bridge between absolutism and relativism—**truth varies by position but is objective within position**.

### G. Architectural Coherence

Traditional formal systems: Specification and implementation drift apart

**Indexed Constraint-Logic:** Single canonical predicate (`classify_from_metrics/6`) ensures all classification pathways — primary, modal, lifecycle, and validation — apply identical threshold logic from `config.pl`. Two-regime architecture (metrics-first, signature-override) provides both precision and structural awareness.

**Achievement:** Formal spec and implementation remain **aligned through architectural discipline**, not manual synchronization.

### H. Structural Integrity

Traditional formal systems: Classify objects in isolation

**Indexed Constraint-Logic:** Purity scoring (§IIc) measures how cleanly each constraint operates across four structural dimensions. Network dynamics (§IId) propagate contamination between connected constraints, enabling cascade prediction. Boltzmann compliance (§IIb) provides a physics-grounded test for natural law claims.

**Achievement:** The system reasons not only about **what a constraint is** but about **how structurally sound it is** and **how its neighbors affect it** — moving from static classification to dynamic structural health monitoring.

---

**What This Achieves:**

A logical system where:
- **Operators track real structure AND perspectival position** (degrees of freedom, decay rates, power-scope scaling)
- **Inference rules produce index-appropriate action-guidance** (classify from your position → respond appropriately)
- **Errors have consequences and explanations** (misclassification → wasted energy or lost freedom, usually from confusion not malice)
- **Self-application is honest** (framework is Scaffold, not eternal truth; has known limitations; requires external validation)
- **Same constraint can have different types from different indices** (indexical relativity without contradiction)
- **Six types capture the full structural space** (Mountain, Rope, Tangled Rope, Snare, Scaffold, Piton — each irreducible)

This is **operational indexed modal logic**—reasoning about necessity and contingency that **routes directly to practice from your structural position**.

---

**"Formal systems should track real structure, acknowledge power differentials, and guide action from where you actually stand. This is that system."**
