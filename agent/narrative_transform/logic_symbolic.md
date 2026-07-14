# Indexed Constraint Logic: Symbolic Reference

**Version:** 5.0 Symbolic Edition
**Purpose:** Formal system for constraint classification. Consumed by **Stage 0 (per-character classification)** and **Stage 1 (formalization)** of the narrative pipeline — `STAGE_INPUTS["narrative"]` wires `dr_logic_symbolic` to `stage_0` and `stage_1` only (not Stage 5, which is a narrative-critique Discovery pass with no logic reference).
**Source of truth:** `prolog/config.pl` `param/2` facts, verified against `drl_core.pl:classify_from_metrics/6` (§IV thresholds). Historical extraction from `logic.md`; where the two disagree, **the engine wins**.
**Scope:** Definitions, predicates, thresholds, error taxonomy, lifecycle. No narrative guidance.

---

## I. Core Principle: Indexed Relativity

**Central question:** What type of constraint is C from index I?

**Formal statement:**
```
∀I₁, I₂: (Type(C, I₁) ≠ Type(C, I₂)) ∧ (True(Type(C, I₁)) ∧ True(Type(C, I₂)))
```

Multiple valid classifications exist because different structural positions expose different properties of the same constraint. Each indexed claim is objectively verifiable.

**Not relativism** ("truth is whatever you believe")
**Not absolutism** ("same truth for everyone")
**Indexed realism:** Truth is position-relative; each indexed claim is objectively true or false.

---

## II. Index Structure

An index specifies structural position as a four-tuple:

```
I = (P, T, E, S)

where:
  P ∈ {powerless, moderate, powerful, organized, institutional, analytical}
  T ∈ {immediate, biographical, generational, historical, civilizational}
  E ∈ {trapped, identity_locked, constrained, mobile, arbitrage, analytical}
  S ∈ {local, regional, national, continental, global, universal}
```

### Power Position (P)

Capacity to influence or exit the constraint.

| Position | π value | Effect |
|----------|---------|--------|
| powerless | 1.5 | Extraction amplified — bears full cost |
| moderate | 1.0 | Baseline agency |
| powerful | 0.6 | Extraction dampened — deflects costs |
| organized | 0.4 | Collective burden-sharing |
| institutional | −0.2 | Net beneficiary — extracts FROM system |
| analytical | 1.15 | Detects normalized extraction (degeneracy-breaking) |

**π(analytical) = 1.15 rationale:** Calibrated to break moderate-analytical degeneracy. At π = 1.0, 93 constraints in corpus show identical χ for both. At 1.15, the analyst detects Snare where moderate normalizes as Tangled Rope. The modifier reflects freedom from normalization pressure, not superior cognition.

### Time Horizon (T)

Planning scale determining what counts as "changeable."

| Horizon | Scale | Effect on immutability |
|---------|-------|----------------------|
| immediate | Days–weeks | Most constraints appear as Mountains |
| biographical | Decades | Some constraints changeable within lifetime |
| generational | Children's lifetimes | More constraints become modifiable |
| historical | Centuries | Most constraints appear changeable |
| civilizational | Millennia | Only fundamental physics remains Mountain |

### Exit Options (E)

Practical ability to escape the constraint.

| Exit | Description |
|------|-------------|
| trapped | Physically/economically cannot leave |
| identity_locked | Structurally mobile but cognitively/identity-fused to constraint |
| constrained | Can exit at high cost |
| mobile | Multiple alternatives available |
| arbitrage | Can play alternatives against each other |
| analytical | Can analyze from outside without being subject to it |

### Scope (S)

Scale determining verification difficulty.

| Scope | σ value | Mechanism |
|-------|---------|-----------|
| local | 0.8 | Easy verification → extraction dampened |
| regional | 0.9 | Feasible verification |
| national | 1.0 | Baseline |
| continental | 1.1 | Harder verification |
| global | 1.2 | Hardest verification → extraction amplified |
| universal | 1.0 | Natural laws are scope-invariant |

---

## III. The Key Formula

```
χ(C, I.P, I.S) = ε(C) × π(P) × σ(S)
```

Where:
- **χ** = effective extractiveness (power-scaled, what the agent at this index experiences)
- **ε** = base extractiveness (structural property of the constraint itself)
- **π(P)** = power modifier
- **σ(S)** = scope modifier

---

## IV. The Six Constraint Types

**Metric cascade in `classify_from_metrics/6`** (priority order, **pre-signature-override**):
```
Mountain > Piton(dead-coordination) > Snare > Scaffold > Rope > Tangled Rope > Piton(fallback) > Naturalized > unknown
```

This is the **metric** cascade only. After it runs, `dr_type/3` (`drl_core.pl:441`) applies a
structural-signature override (`metric_based_type_indexed` → `integrate_signature_with_modal`) that
**can change the final type**. The classifications below are what the metric cascade yields per
character; the signature layer is not modelled here (Stages 0/1 reason about these metric types).

> **Provenance.** The numeric bounds and gate structure below mirror `prolog/config.pl` `param/2`
> facts, verified against `drl_core.pl:classify_from_metrics/6` (clause bodies at `:333–428`) at
> commit **`d2f8b829`** (`config.pl` / `drl_core.pl` working tree clean for those files). `config.pl`
> is the source of truth; these values are a hand-mirror. Re-sync on any classification-gate param
> change — a drift guard (`python/check_logic_symbolic_drift.py`) reads the live values from
> `config.pl` and asserts each appears on its type's gate line here.

### Mountain (■) — Unchangeable Terrain

```
■C[I] ↔ ε(C) ≤ 0.25 ∧ Supp(C) ≤ 0.05 ∧ NaturalEmergence(C) ∧ Immutable(C, I.T, I.E)
```

- Zero or minimal extraction (natural laws don't extract from subjects)
- Zero or minimal suppression (no enforcement needed)
- Naturally emerges without human construction
- Immutable from the agent's time horizon and exit options
- **Does not check χ** — natural laws affect everyone equally; if burden varies by power position, it's constructed

**Thresholds:** ε ≤ 0.25 (mountain_extractiveness_max), Supp ≤ 0.05 (mountain_suppression_ceiling)

**Structural gate:** Must pass Boltzmann Independence Test (see §VIII). If classification varies by Power × Scope in non-factorizable way → constructed, not natural.

### Piton (⊟, dead-coordination pre-check) — Vitality Override

Fires **before Snare** when coordination vitality is explicitly declared dead or degrading. A
constraint whose coordination is dead but which still performs (high theater) is a Piton regardless
of extraction level.

```
⊟C[I] ↔ CoordinationDead(C) ∧ ε(C) > 0.10 ∧ TheaterRatio(C) ≥ 0.70
```

- **Does not check χ or suppression** (v7.0 vitality gate) — dead coordination + theater is a Piton
  at any extraction level
- ε > 0.10 floor prevents zero-extraction Mountains from misclassifying here
- `CoordinationDead(C)` = authored `coordination_vitality(C, dead)` or `(C, degrading)`; absent
  declaration ⇒ falls through to the normal priority chain below

**Thresholds:** ε > 0.10 (piton_epsilon_floor), TheaterRatio ≥ 0.70 (piton_theater_floor).
`CoordinationDead` reads an authored vitality flag — a structural gate, no numeric threshold of its
own (`drl_core.pl:coordination_dead/1`).

### Rope (⊞) — Coordination Mechanism

```
⊞C[I] ↔ χ(C, I.P, I.S) ≤ 0.35 ∧ (χ ≤ 0 → ⊤ | ε(C) ≤ 0.45)
         ∧ (Changeable(C, I.T, I.E) ∨ NaturalEmergence(C))
```

- Low power-scaled extraction (χ ≤ 0.35)
- **Dual threshold:** When χ > 0, also requires ε ≤ 0.45 (prevents powerful agents from misclassifying high-ε constraints as Ropes)
- When χ ≤ 0 (net beneficiary), ε ceiling bypassed
- Provides genuine coordination value
- Changeable (distinguishes from Mountain)

**Thresholds:** χ ≤ 0.35 (rope_chi_ceiling), ε ≤ 0.45 (rope_epsilon_ceiling)

### Snare (⊠) — Extraction Trap

```
⊠C[I] ↔ χ(C, I.P, I.S) ≥ 0.66 ∧ ε(C) ≥ 0.46 ∧ Supp(C) ≥ 0.60
         ∧ ¬NaturalLawWithoutBeneficiary(C)
         ∧ SnareImmutability(C, I)
```

- High power-scaled extraction (χ ≥ 0.66)
- High base extraction (ε ≥ 0.46) — prevents false Snare from power amplification of a low-ε constraint
- High suppression (Supp ≥ 0.60) — requires force to maintain
- Not a beneficiary-free natural law (see gloss below)
- Immutable-to-this-index but changeable to some higher-power context (`SnareImmutability`)
- **Most index-sensitive type** — same constraint: Snare (powerless), Tangled (moderate), Rope (institutional). `SnareImmutability` encodes exactly this: the gate fires when *this* index sees the constraint as immutable but a higher-power context perceives it as Rope (changeable).

**Thresholds:** χ ≥ 0.66 (snare_chi_floor), ε ≥ 0.46 (snare_epsilon_floor), Supp ≥ 0.60 (snare_suppression_floor). `SnareImmutability(C, I)` = `drl_core.pl:snare_immutability_check/1` (some standard context perceives Rope).

> **Gloss — `¬NaturalLawWithoutBeneficiary(C)`** (`drl_core.pl:natural_law_without_beneficiary/1`, no numeric threshold): Snare is blocked when the constraint **emerges naturally, requires no active enforcement, AND names no beneficiary** — asymmetric *impact* is not asymmetric *extraction*, so such a constraint stays a Mountain. All three conjuncts must hold for the block to fire.

### Tangled Rope (⊞⊠) — Hybrid Coordination-Extraction

```
⊞⊠C[I] ↔ 0.35 < χ(C, I.P, I.S) ≤ 0.90 ∧ ε(C) ≥ 0.30 ∧ Supp(C) ≥ 0.40
           ∧ RequiresActiveEnforcement(C) ∧ Coord(C) = true ∧ Asym(C) = true
           ∧ ¬NaturalLawWithoutBeneficiary(C)
```

- Moderate-to-high power-scaled extraction (χ in the (0.35, 0.90] band)
- **Strict χ floor:** χ = 0.35 belongs to Rope (χ ≤ ceiling); Tangled owns (0.35, 0.90] (OQ-37 Move 1 keeps the partition single-valued at the seam)
- Genuine coordination value (Coord) AND asymmetric cost distribution (Asym), AND the constraint must be actively enforced (`RequiresActiveEnforcement`) — a non-enforced coordination is Scaffold-like, not Tangled
- Not a beneficiary-free natural law (same gloss as Snare)
- **Common real-world type** — irreducible hybrid, not confused Rope or disguised Snare

**Thresholds:** χ > 0.35 (tangled_rope_chi_floor, **strict**), χ ≤ 0.90 (tangled_rope_chi_ceil), ε ≥ 0.30 (tangled_rope_epsilon_floor), Supp ≥ 0.40 (tangled_rope_suppression_floor). `RequiresActiveEnforcement` = `drl_core.pl:requires_active_enforcement/1` (authored enforcement flag, no numeric threshold).

### Scaffold (⊡) — Temporary Support

```
⊡C[I] ↔ χ(C, I.P, I.S) ≤ 0.45
         ∧ Coord(C) = true
         ∧ ¬Captured(C)
         ∧ ScaffoldTemporality(C)
         ∧ TheaterRatio(C) ≤ 0.70
```

- Low-to-moderate extraction (χ ≤ 0.45)
- Provides coordination (`has_coordination_function`)
- **Temporality gate** (`ScaffoldTemporality`): built-in expiration (sunset clause) — OR, absent a sunset, the constraint requires no active enforcement (non-enforced coordination is inherently scaffold-like)
- Real work, not pure performance (theater ratio ≤ 0.70)
- Degrades to Piton when the sunset is violated

**Thresholds:** χ ≤ 0.45 (scaffold_extraction_ceil), TheaterRatio ≤ 0.70. **The theater bound is a hardcoded literal `TR > 0.70` in `drl_core.pl:382` — NOT a `config.pl` param** (the drift guard cannot read it from `config.pl`; see the guard's stated scope). `ScaffoldTemporality` = `drl_core.pl:scaffold_temporality_check/1` (`has_sunset_clause` ∨ ¬`requires_active_enforcement`), no numeric threshold.

> **Gloss — `¬Captured(C)`** (`narrative_ontology.pl:constraint_captured/1`, OQ-94, no numeric threshold): Scaffold is blocked when the authored **gain-flow surface names a specific, existing (non-`diffuse`) stakeholder seat** as the receiver of the constraint's gains — i.e. a witnessed beneficiary of record, not merely "some beneficiary could exist." An absent or `diffuse` gain-flow leaves the constraint *uncaptured* (the block does not fire). Captured coordination routes to Tangled Rope (the cell that says "genuinely coordinates AND has an owner"), never benign Scaffold.

### Piton (⊟, fallback) — Degraded Theater

The general-priority Piton (distinct from the dead-coordination pre-check above): reached late in the
cascade for a low-χ, still-extracting, high-theater constraint.

```
⊟C[I] ↔ χ(C, I.P, I.S) ≤ 0.45 ∧ ε(C) > 0.10 ∧ TheaterRatio(C) ≥ 0.70
```

- Function dried up, structure persists
- Low power-scaled extraction (χ ≤ 0.45) but non-trivial base extraction (ε > 0.10)
- High theater ratio (performance >> substance)
- Still costs maintenance energy

**Thresholds:** χ ≤ 0.45 (piton_extraction_ceiling), ε > 0.10 (piton_epsilon_floor), TheaterRatio ≥ 0.70 (piton_theater_floor)

### Naturalized — Power-Scaling Ambiguity

```
ε(C) > 0.45 ∧ χ(C, I.P, I.S) < 0.35
```

- High base extraction (ε > 0.45, the Rope ε-ceiling) but low power-scaled extraction (χ < 0.35)
- Suggests extraction is being absorbed/hidden by power position — primary substrate for false-summit rhetoric
- Action: investigate_naturalization

**Thresholds:** ε > 0.45 (rope_epsilon_ceiling), χ < 0.35 (tangled_rope_chi_floor — the χ bound is the Tangled floor, so Naturalized occupies the low-χ / high-ε corner the other gates leave open)

---

## V. Error Taxonomy

Six misclassification types based on structural position:

### Type I: False Mountain
- **Pattern:** Changeable constraint treated as unchangeable
- **Index conditions:** Typically (trapped, immediate) — constraint genuinely appears immutable
- **Test:** Does classification vary by Power × Scope? If yes → not Mountain

### Type II: Mountain Denial
- **Pattern:** Unchangeable constraint treated as changeable
- **Consequence:** Energy depletion fighting what cannot change

### Type III: Snare-as-Rope (Missing Extraction)
- **Pattern:** Normalizing extraction as coordination
- **Index conditions:** Common at (moderate+, mobile+) — extraction not felt
- **Test:** Check χ at powerless index. If χ(powerless) > 0.70 → Snare exists regardless of χ(powerful)

### Type IV: Rope-as-Snare (Missing Coordination)
- **Pattern:** Treating genuine coordination as extraction
- **Consequence:** Destroying functional systems
- **Test:** Does removing constraint eliminate a coordination function?

### Type V: Tangled Rope Mishandling
- **V.a: Tangled-as-Rope** — ignoring extraction component
- **V.b: Tangled-as-Snare** — ignoring coordination component
- **V.c: Wrong reform strategy** — attempting surgical reform when too degraded (purity too low)

### Type VI: Scaffold Misclassification
- **Pattern:** Failing to recognize sunset clause (→ treats as Rope) or expired sunset (→ Piton)

### Error Observable Format

For Stage 1 formalization, errors are specified as:
```
Error: [Type] — [Pattern label]
Agent: [Variable name]
Constraint: [C_id]
Actual type from agent's index: [classification]
Perceived type: [misclassification]
Observable: [testable action or decision that reveals the error]
Correction trigger: [what would change the classification]
```

---

## VI. Transformation Rules

Transformation rules specify how actions change constraint dynamics. Format:

```
Rule: TR_id
Trigger: [action] on [constraint]
Index change: I_before → I_after
χ recalculation: χ_before → χ_after
Type change: Type_before → Type_after
Preconditions: [required states]
Blocked by: [constraints that prevent transformation]
```

All transformation rules must be testable: given the trigger condition, the χ recalculation follows mechanically from the formula.

---

## VII. Institutional Rationality Models

```
Perfect Institutional Rationality (PIR):
  Maximize utility without bounds. No negotiation except Pareto-improving.
  Tends toward: Deterministic Tragedy
  Use when: Implacable systems, natural law, algorithmic governance

Bounded Institutional Rationality (BIR):
  Satisfice under uncertainty. Principal-agent problems, risk aversion.
  Tends toward: Negotiated Equilibrium, Seeded Possibility
  Use when: Realistic organizations, human institutions

CRITICAL: This choice determines which attractors are reachable.
```

### Terminal Attractors

```
□ Deterministic Tragedy — constraints run to completion
□ Negotiated Equilibrium — constraints find balance through bargaining
□ Revolutionary Rupture — constraint logic itself disrupted
□ Seeded Possibility — surface tragedy, underground transformation
```

### Attractor Compatibility Matrix

| Constraint Profile | PIR | BIR | Compatible Attractors |
|-------------------|-----|-----|----------------------|
| Mountain-dominated | Yes | Yes | Tragedy, Seeded Possibility |
| Mountain + Snares | Yes | N/A | Deterministic Tragedy |
| Tangled Ropes dominant | Yes | N/A | Tragedy (crushed by hybrid) |
| Tangled Ropes dominant | N/A | Yes | Negotiated Equilibrium |
| Pure Snares, no Mountains | Yes | N/A | Tragedy or Revolutionary Rupture |
| Pure Snares, no Mountains | N/A | Yes | Equilibrium or Rupture |
| Piton present | Either | Either | Seeded Possibility |

---

## VIII. Structural Physics (Boltzmann Test)

### The Boltzmann Independence Test

Natural laws must be independent of index dimensions. If classification varies by Power × Scope in a non-factorizable way → constraint is constructed, not natural.

**Test procedure:**
```
1. Compute Type(C, I) for multiple indices varying P and S independently
2. If Type changes with P (burden varies by power) → FAIL
3. If Type changes with S (burden varies by scope) → FAIL
4. If Type(C, I₁) = Type(C, I₂) for all tested I → PASS (candidate Mountain)
```

**Implication:** Constraints that FAIL Boltzmann are constructed regardless of ε and Supp values. Low extraction + low suppression + Boltzmann failure = well-designed institution, not natural law.

---

## IX. Lifecycle: State Transitions

### The Seven Classical Transitions

```
T1: ⊞ → ⊞⊠   Rope degradation (extraction accumulation)
T2: ⊞⊠ → ⊠    Tangled → Snare (coordination loss)
T3: ⊞ → ⊡     Rope → Scaffold (sunset acceptance)
T4: ⊡ → ⊟     Scaffold → Piton (sunset violation)
T5: ⊞⊠ → ⊟    Tangled → Piton (extraction collapse)
T6: ⊠ → ⊟     Snare → Piton (exhaustion)
T7: ■ ↛ X     Mountains don't transition — if one does, it was never a Mountain
```

### Transition Irreversibility (Entropy Principle)

```
Natural direction (low-energy decay):
  Rope → Tangled Rope → Snare → Piton

Unnatural direction (high-energy reform):
  Snare → Tangled Rope → Rope
  Requires: Scaffold construction + massive agency injection
```

Degradation is thermodynamically favored. Reform fights entropy.

### The Eleven Drift Types

| Type | Name | Pattern |
|------|------|---------|
| 1 | Metric Substitution (MS) | Optimization target becomes constraint |
| 2 | Extraction Accumulation (EA) | Coordination persists, extraction added |
| 3 | Coordination Loss (CL) | Coordination dries up, extraction remains |
| 4 | Function Obsolescence (FO) | Purpose served, constraint persists |
| 5 | Sunset Violation (SV) | Scaffold sunset missed, theater spikes |
| 6 | Extraction Collapse (EC) | Victims exhausted/escaped, constraint vestigial |
| 7 | Algorithm Shutdown (AS) | Automated enforcement stops, behavior persists |
| 8 | Coupling Drift (CD) | Independent dimensions become coupled |
| 9 | Boltzmann Floor Drift (BFD) | Minimum extraction rises (legitimate complexity) |
| 10 | Purity Drift (PD) | Metrics stable, structural integrity degrading |
| 11 | Network Drift (ND) | Intrinsic purity stable, contamination from neighbors |

---

## X. Stage 1 Output Specification

Stage 1 produces a formal constraint network. Output contains ONLY:

### Required elements:

1. **Constraint formalizations** — For each constraint:
   - Base properties: ε, Supp, Coord, Asym
   - Per-agent indexed classification: I = (P, T, E, S), χ calculation, type, threshold check
   - Structural certification: Boltzmann test result
   - Indexical variance summary

2. **Transformation rules** — IF-THEN format with χ recalculations

3. **Error manifestations** — Per error taxonomy, with observable markers

4. **Rationality model** — PIR or BIR, with justification and attractor compatibility

5. **Terminal attractor** — Selected, justified, compatible with rationality model

### Variable naming:

Characters are designated as variables: X₁, X₂, X₃, ... Xₙ. No source names, no agent labels that imply narrative role. Group entities use the same convention.

### Prohibited in Stage 1 output:

- Source character names or aliases (Santiago, "the old man", Agent_A)
- Occupation-specific vocabulary from the source (fishing, boats, harvesting)
- `<experience>` fields describing how constraints feel
- `<dialogue_markers>` containing source dialogue or paraphrase
- `<narrative>` sections in transformation rule consequences
- Natural-language descriptions of constraint effects in the source context
- Any text that would allow identification of the source work

### Permitted in Stage 1 output:

- Abstract structural descriptions: "Social enforcement mechanism," "Asymmetric risk distribution," "Recursive feedback loop"
- Formal predicates and calculations
- Classification rationales in structural terms: "High extraction dominates minimal coordination value"
- Error observable patterns as testable conditions: "Does not attempt collective organization despite χ indicating feasibility"

---

## XI. Validation Checklist

```
☐ All constraints formalized with ε, Supp, Coord, Asym
☐ All χ calculations shown with π and σ values
☐ All characters use variable names (X₁, X₂, ...) — no source identifiers
☐ No source occupation, setting, or domain vocabulary anywhere in output
☐ No <experience>, <dialogue_markers>, or <narrative> fields
☐ Transformation rules are testable (IF-THEN with mechanical χ recalculation)
☐ Error types have observable patterns (testable conditions, not source actions)
☐ Institutional rationality model specified (PIR/BIR) with justification
☐ Terminal attractor selected, justified, and compatible with rationality model
☐ Indexical variance explicitly preserved across agents
☐ Boltzmann test run for each constraint claiming Mountain status
☐ No ambiguity in specifications
```

---

**END OF SYMBOLIC LOGIC REFERENCE**

Version 5.0 Symbolic Edition
§IV threshold values mirror `prolog/config.pl`, verified against `drl_core.pl:classify_from_metrics/6`
at commit **`d2f8b829`** (`config.pl` / `drl_core.pl` working tree clean for those files). Re-sync on
any classification-gate param change; drift guard: `python/check_logic_symbolic_drift.py`.
Consumed by narrative Stage 0 (per-character classification) and Stage 1 (formalization).
Compatible with UKE_Narrative v1.4+
