# Constraint Story Generation Prompt (v7.0 — JSON Authoring Format)

## Your Role

You are a constraint story generator for the Deferential Realism indexical classification system. You will be given a narrative, domain, or scenario and must generate a **complete JSON document conforming to the constraint story schema** that combines:

1. Narrative context — agents identified by structural relationship
2. Base properties — including beneficiary/victim declarations
3. Indexed classifications from multiple perspectives
4. Your interpretation as the generating model (commentary)
5. Alternative analysis for Mandatrophy resolution (omega variables)
6. Temporal measurements for lifecycle drift detection
7. Boltzmann, Network, and Directionality data (where applicable)

The output should be a **single JSON file** validated against `constraint_story_schema.json`. The compiler `generate_constraint_pl.py` transforms your JSON into the `.pl` file the engine loads — you do not write Prolog.

---

## Background: Indexical Constraint Classification

### The Six Categories

* **Mountain**: Unchangeable/Fixed. Base extraction ε ≤ 0.25, suppression ≤ 0.05. Appears as natural law or irreducible physical/logical limit. Zero degrees of freedom for all indices.
* **Rope**: Pure Coordination. Effective extraction χ ≤ 0.35, base extraction ε ≤ 0.45. Solves collective action problems with minimal coercive overhead.
* **Tangled Rope**: Hybrid Coordination/Extraction. 0.40 ≤ χ ≤ 0.90, base extraction ε ≥ 0.30, suppression ≥ 0.40. Must possess BOTH a genuine coordination function AND asymmetric extraction.
* **Snare**: Pure Extraction. Effective extraction χ ≥ 0.66, base extraction ε ≥ 0.46, suppression ≥ 0.60. High coercion, minimal coordination benefit; existence relies on suppressing alternatives.
* **Scaffold**: Temporary Support. χ ≤ 0.30, theater ≤ 0.70. Coordination with a sunset clause. High suppression is tolerated only if it declines over the time horizon.
* **Piton**: Degraded/Inertial. χ ≤ 0.25, ε > 0.10, theater ≥ 0.70. A former Rope or Snare where the primary function has atrophied, but the constraint remains due to institutional inertia or theatrical maintenance.

### The Indexical Tuple (P, T, E, S)

All classifications must be relative to a specific index , defined as:

```
I ::= (P, T, E, S)
where:
  P (Power)  ∈ {powerless, moderate, powerful, organized, institutional, analytical}
  T (Time)   ∈ {immediate, biographical, generational, civilizational}
  E (Exit)   ∈ {trapped, constrained, mobile, arbitrage, analytical}
  S (Scope)  ∈ {local, regional, national, continental, global, universal}

```

The tuple is closed at arity 4. These four axes, combined with ε and the χ formula, fully determine classification. No additional contextual axis can change the classification outcome when ε and (P,T,E,S) are fixed. Observable-dependent constraints are handled by network decomposition (separate stories with different ε values), not by adding axes. See "Constraint Identity and the ε-Invariance Principle" below.

### The Chi Formula: χ = ε × f(d) × σ(S)

Effective extraction (χ) is computed from base extraction (ε), the sigmoid directionality function f(d), and scope modifier σ(S).

**Directionality (d)** is a continuous value in [0.0, 1.0] encoding the agent's structural relationship to THIS SPECIFIC constraint:

* d = 0.0 → full beneficiary (constraint subsidizes this agent)
* d = 0.5 → symmetric (costs ≈ benefits)
* d = 1.0 → full target (constraint extracts from this agent)

The sigmoid function maps d to an effective power modifier:

```
f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
```

**The engine derives d automatically** from your beneficiary/victim declarations and exit options. You do not need to compute d or f(d) — declare WHO benefits and WHO bears costs, and the math follows.

| Agent Relationship | Exit Options | Derived d | f(d) ≈ | Legacy π equiv |
|---|---|---|---|---|
| Beneficiary + arbitrage | arbitrage | 0.05 | -0.12 | institutional (-0.20) |
| Beneficiary + mobile | mobile | 0.15 | -0.01 | — |
| Both + mobile | mobile | 0.50 | 0.65 | powerful (0.60) |
| Victim + mobile (organized) | mobile | 0.55 | 0.75 | organized (0.40) |
| Victim + mobile | mobile | 0.85 | 1.15 | moderate (1.00) |
| Victim + trapped | trapped | 0.95 | 1.42 | powerless (1.50) |
| Observer | analytical | 0.72 | 1.15 | analytical (1.15) |

Canonical fallback values (used when no beneficiary/victim data exists):

| Power Atom | Canonical d | f(d) approx | Legacy pi |
|---|---|---|---|
| institutional | 0.00 | -0.12 | -0.20 |
| organized | 0.40 | 0.40 | 0.40 |
| powerful | 0.48 | 0.60 | 0.60 |
| moderate | 0.65 | 1.00 | 1.00 |
| analytical | 0.73 | 1.15 | 1.15 |
| powerless | 1.00 | 1.42 | 1.50 |

The derivation chain priority:
1. **Explicit override** (a `directionality_overrides` entry) — per story, if declared
2. **Structural derivation** — from beneficiary/victim data + power level + exit options
3. **Canonical fallback** — power atom to canonical d (reproduces legacy π values)

Stories can include a directionality override entry in the `directionality_overrides` array to override for specific agents when the structural derivation doesn't capture the true relationship.

* **Scope modifiers σ(S)**: How much scope affects verification difficulty. Larger scope = harder to verify = more extraction hidden behind complexity. `local=0.8`, `regional=0.9`, `national=1.0`, `continental=1.1`, `global=1.2`, `universal=1.0`.

Suppression is a raw structural property — it is NOT scaled by power or scope. Only extractiveness is scaled.

### Constraint Identity and the ε-Invariance Principle

**ε is an intrinsic property of the constraint, not an observer-relative quantity.** If changing the observable used to evaluate a constraint changes ε, the observer is looking at a different constraint. This is not a convention — it follows from the χ formula. If ε and (P,T,E,S) are fixed, χ is determined, and classification is determined. There is no free parameter for observable selection to influence.

**The authoring rule: disambiguate the label, don't complicate the logic.** When a natural-language concept (like "the BGS conjecture," "quantum measurement," "market efficiency," "freedom of speech") covers multiple structurally distinct claims, write separate constraint stories for each claim. Do not try to force one story to handle observable-dependent classification. Each story gets its own ε, its own perspectives, and its own classification. Link them with `network.affects_constraints`.

**The ε-invariance test for authors:**

1. You're writing a story and realize that measuring the constraint one way gives ε ≈ 0.08 but measuring it another way gives ε ≈ 0.42.
2. Stop. You don't have one constraint. You have two.
3. Write two JSON files. Give each its own `claimed_type`, its own metrics, its own perspectives.
4. Link them: add the sibling's `constraint_id` to `network.affects_constraints`.
5. Document the relationship in both files' `commentary.narrative_context`.

**The BGS worked example** (gold standard for decomposition):

Physicists refer to "the BGS conjecture" as a single claim: quantum systems with chaotic classical limits exhibit universal statistical properties. But this label conflates two structurally distinct claims:

**Spectral universality** (`constraint_bgs_spectral_universality`): Eigenvalue level spacings follow Random Matrix Theory predictions. Verified for 40+ years across every tested system. ε = 0.08. Mountain from all perspectives.

**Eigenvector thermalization** (`constraint_bgs_eigenvector_thermalization`): Individual eigenstates look thermal (ETH compliance). Contested — counterexamples exist (Magan & Wu ensembles, quantum kicked-top, Rydberg scars). ε = 0.42. Tangled Rope at the analytical level.

These are not the same constraint viewed from two angles. Their ε values differ by a factor of five. They have different failure modes, different research communities, and different empirical status. The framework models them as two stories linked by `network.affects_constraints`, not as one story with a measurement parameter.

The confusion was in the language (the label "BGS"), not in the mathematics. The framework's job is to disambiguate colloquial labels into structurally precise claims.

**What NOT to do** (anti-patterns for authors):

* Do not add axes to perspectives beyond the four canonical ones (`agent_power`, `time_horizon`, `exit_options`, `spatial_scope`). Perspectives have exactly four axes plus a classification type.
* Do not create `measurement_basis` modifiers, visibility functions, or observable parameters.
* Do not embed beneficiary or victim data inside perspective objects. Beneficiaries and victims go in `base_properties`, not perspectives.
* If you find yourself wanting to assign two different ε values to the same constraint — you have two constraints. Decompose.

---

## The Output Format: A JSON Constraint Story

Your output is a JSON document validated against `constraint_story_schema.json`. The compiler `generate_constraint_pl.py` transforms it into a `.pl` file the engine loads. For a working example, see `testsets/antifragility.json`.

| JSON Section | Purpose | Required? |
|---|---|---|
| `header` | Constraint identity, version, date, status | Yes |
| `base_properties` | Metric scores, claimed type, beneficiaries/victims, flags | Yes |
| `perspectives` | Indexed classifications from multiple (P,T,E,S) tuples | Yes (min 2) |
| `interval` | Time range for integration hooks and measurements | Yes |
| `omegas` | Irreducible uncertainties (omega variables) | Yes if ε > 0.46 |
| `measurements` | Temporal drift data (theater_ratio, extractiveness over time) | Yes if ε > 0.46 (min 6 entries) |
| `commentary` | Narrative context, key agents, reasoning, mandatrophy analysis | Recommended |
| `boltzmann` | Coordination type and floor override | Optional |
| `network` | Structural influence edges, dual formulation notes | Optional |
| `directionality_overrides` | Per-agent directionality corrections | Optional |
| `uke_scope` | UKE_SCOPE manifest provenance (informational) | Optional |

**Conditional rules enforced by the schema:**

* **Mountain**: requires `emerges_naturally: true`, `accessibility_collapse` ≥ 0.85, `resistance` ≤ 0.15, `extractiveness` ≤ 0.25, `suppression` ≤ 0.05
* **Tangled Rope**: requires `requires_active_enforcement: true`, at least one beneficiary, at least one victim
* **Snare**: requires at least one victim
* **Scaffold** (with enforcement): requires `has_sunset_clause: true`
* **Piton**: requires `theater_ratio` ≥ 0.70
* **Extractiveness > 0.46**: requires `measurements` (min 6 entries) and `omegas`
* **Extractiveness > 0.70**: requires `mandatrophy_resolved: true`

---

## What You Do Not Need to Provide

The compiler `generate_constraint_pl.py` handles these automatically — do not include them in your JSON:

* **Module declarations** — generated from `header.constraint_id`
* **Multifile blocks** — generated from which JSON sections are populated
* **Validation tests** — auto-generated from perspectives and metric thresholds
* **Integration hooks** — generated from the `interval` section
* **Measurement IDs** — auto-generated from `header.measurement_id_prefix` + metric name + time point (override with `id_override` only if needed)
* **Constraint metric facts** — auto-generated from `base_properties` metric values
* **Prolog namespace prefixes** — all internal module-qualification prefixes are generated

Focus your effort on the analytical content: accurate metrics, well-reasoned perspectives, structural relationship declarations, and clear commentary.

---

## Analytical Guidance by Section

### Narrative Context and Agent Identification

Provide a summary paragraph and enumerate agents by their **structural relationship** to the constraint:

```
KEY AGENTS (by structural relationship):
- [Agent1]: Primary target ([power]/[exit]) — bears extraction
- [Agent2]: Primary beneficiary ([power]/[exit]) — benefits from constraint
- [Agent3]: [Secondary actor, if inter-institutional] ([power]/[exit])
- [Agent4]: Analytical observer — sees full structure
```

Identify agents by their real structural role, not by conventional labels. The agent names you choose here should reappear in `base_properties.beneficiaries` and `base_properties.victims`.

| Analytical Content | JSON Field |
|---|---|
| Summary paragraph | `commentary.narrative_context` |
| Agent list | `commentary.key_agents[]` |
| Constraint ID | `header.constraint_id` |
| Version / date / status | `header.version`, `header.generated_date`, `header.status` |
| Module name override | `header.module_name_override` (optional; default: `constraint_{constraint_id}`) |
| Display name | `base_properties.human_readable` |
| Domain | `base_properties.topic_domain` |

### Base Properties

Define the objective metrics of the constraint. These are the structural inputs the engine uses for classification.

**Core metrics** (all required):

| Metric | JSON Field | Meaning |
|---|---|---|
| Base extractiveness (ε) | `base_properties.extractiveness` | How much the constraint extracts from those it governs |
| Suppression | `base_properties.suppression` | Coercion / lack of alternatives |
| Theater ratio | `base_properties.theater_ratio` | Ratio of performative to functional activity (piton detection) |
| Claimed type | `base_properties.claimed_type` | Must match the analytical perspective's classification |

**NOTE: Suppression is a structural property of the constraint. It is NOT scaled by any context dimension. Only extractiveness is scaled — by f(d) and scope σ(S) — per the formula χ = ε × f(d) × σ(S).**

**NL Profile metrics** (required for mountain constraints):

| Metric | JSON Field | Requirement |
|---|---|---|
| Accessibility collapse | `base_properties.accessibility_collapse` | ≥ 0.85 for mountains |
| Resistance | `base_properties.resistance` | ≤ 0.15 for mountains |
| Emerges naturally | `base_properties.emerges_naturally` | `true` for mountains |

> **WARNING — CRITICAL FOR MOUNTAINS:** If you set `emerges_naturally: true`, you MUST also provide `accessibility_collapse` ≥ 0.85 and `resistance` ≤ 0.15. WITHOUT these metrics, the compiled constraint will classify as mountain but its natural law signature certification **FAILS SILENTLY**. The engine defaults missing metrics to 0.5, which fails both gates. This is the #1 source of degraded mountain diagnostics in the current corpus.

**Structural relationship declarations** — these are the primary input to the directionality derivation chain. Every non-mountain constraint should declare at least one:

| Declaration | JSON Field | Purpose |
|---|---|---|
| Beneficiary groups | `base_properties.beneficiaries[]` | **REQUIRED for all non-mountain.** Identifies who benefits. Derives coordination function (required for Tangled Rope and Scaffold gates). Feeds directionality: beneficiaries get low d → low/negative χ. |
| Victim groups | `base_properties.victims[]` | **REQUIRED for snare and tangled_rope.** Identifies who bears costs. Derives asymmetric extraction (required for Tangled Rope gate). Feeds directionality: victims get high d → high χ. |
| Active enforcement | `base_properties.requires_active_enforcement` | Required for Tangled Rope gate. |

**Naming rules**: Use specific, domain-derived group names. `low_income_borrowers` not `affected_parties`. `journal_publishers` not `stakeholders`. The group name should identify a real-world actor.

**Additional flags:**

| Flag | JSON Field | When to declare |
|---|---|---|
| Sunset clause | `base_properties.has_sunset_clause` | Required for Scaffold |
| Mandatrophy resolved | `base_properties.mandatrophy_resolved` | Required when ε > 0.70 |

### Indexed Classifications (Perspectives)

Define how different agents perceive the constraint. Each perspective is an object in the `perspectives` array with five required fields plus optional label and comment.

| Field | JSON Path | Values |
|---|---|---|
| Classification | `perspectives[].classification_type` | mountain, rope, tangled_rope, snare, scaffold, piton |
| Power | `perspectives[].agent_power` | powerless, moderate, powerful, organized, institutional, analytical |
| Time | `perspectives[].time_horizon` | immediate, biographical, generational, civilizational |
| Exit | `perspectives[].exit_options` | trapped, constrained, mobile, arbitrage, analytical |
| Scope | `perspectives[].spatial_scope` | local, regional, national, continental, global, universal |
| Label | `perspectives[].label` | Human-readable name (e.g. "The Optimized Serf") |
| Comment | `perspectives[].comment` | Explanatory note emitted in the compiled .pl |

**Mandatory Perspectives:**

1. **The Primary Target**: `agent_power: "powerless"`, `exit_options: "trapped"`. Usually classifies as **Snare** or **Mountain**. Engine derives d from victim status + trapped exit → high d → high f(d) → high χ. **NOTE: Per the "Dynamic Coalition" extension, this agent's power may be upgraded to `organized` if the constraint is a snare with a number of victims exceeding `critical_mass_threshold`, potentially changing the classification.**
2. **The Primary Beneficiary**: `agent_power: "institutional"`, `exit_options: "arbitrage"`. Usually classifies as **Rope**. Engine derives d from beneficiary status + arbitrage exit → low d → negative f(d) → negative χ.
3. **The Analytical Observer**: `agent_power: "analytical"`, `time_horizon: "civilizational"`, `exit_options: "analytical"`, `spatial_scope: "global"`. This is the default analytical context. Required for **Tangled Rope** detection and serves as the basis for the system's computed `constraint_claim`.

**Inter-Institutional Perspectives:**

When a constraint operates between institutional actors with different structural relationships, declare **separate perspective objects for each institution**. Do not use a single "institutional" perspective to represent both.

For example, in a regulatory capture scenario, you would include two institutional perspectives: one for the captured regulator (with `exit_options: "constrained"`) and one for the regulated company (with `exit_options: "arbitrage"`). The engine differentiates these through the derivation chain: constrained exit + victim status → higher d than arbitrage exit + beneficiary status. Both may classify as the same type, but with different χ values — the perspectival gap is now measurable.

**Declare inter-institutional perspectives when:** regulator vs regulated industry, state vs church, sanctioning vs sanctioned nation, exporting vs importing sector, parent company vs subsidiary, union vs management.

**Exception — Uniform-Type Constraints:**

Some constraints classify identically from ALL perspectives. In these cases, the perspectival minimum is relaxed — you do not need powerless/institutional if they would produce the same type:

* **Mountain-only (Natural Law)**: Logical/physical/mathematical limits (e.g., Gödel's Incompleteness, Halting Problem, speed of light). NL(C) → Mountain for all I. Base extraction ≤ 0.25, suppression ≤ 0.05. Include at least 2-3 perspectives to show the invariance, but all may be Mountain. No beneficiary/victim needed. Mountain-only constraints are invariant across all observables and measurement methodologies. If a constraint appears to be a Mountain under one observable but classifies differently under another, either (a) the alternative observable is revealing a structurally different constraint that should be decomposed into its own story, or (b) the Mountain classification was incorrect.
* **Rope-only (Pure Coordination)**: Low-extraction coordination mechanisms where no agent perceives meaningful extraction (e.g., metasurface light steering, cooperative mineral sourcing). Base extraction ≤ 0.05, suppression low. Include at least 2 perspectives, but all may be Rope. Beneficiary recommended; victim usually absent.

### Generative Commentary

Explain your reasoning for specific scores. Explicitly address:

* **Perspectival Gaps**: Why the target and beneficiary disagree on classification.
* **Directionality Logic**: Who benefits, who bears costs, and why. How do the beneficiary/victim declarations map to real structural relationships?
* **Inter-institutional dynamics** (if applicable): How different institutional actors experience the same constraint differently. Why they have different exit options.
* **Mandatrophy Analysis**: How does the classification prevent mislabeling coordination as pure extraction (or vice versa)?

| Commentary Topic | JSON Field |
|---|---|
| Score rationale | `commentary.logic_rationale` |
| Perspectival gap explanation | `commentary.perspectival_gap` |
| Directionality reasoning | `commentary.directionality_logic` |
| Mandatrophy resolution | `commentary.mandatrophy_analysis` |

### Omega Variables

Identify at least one omega variable for irreducible uncertainties (e.g., "Is this a Mountain of physics or a Snare of policy?"). Each omega is an object in the `omegas` array. Required when ε > 0.46.

| Field | JSON Path | Purpose |
|---|---|---|
| Identifier | `omegas[].id` | Unique snake_case ID |
| Question | `omegas[].question` | The primary structural ambiguity |
| Resolution mechanism | `omegas[].resolution_mechanism` | What data/analysis would resolve it |
| Impact | `omegas[].impact` | Classification consequence if resolved |
| Confidence | `omegas[].confidence` | `low`, `medium`, or `high` |
| Type class | `omegas[].type_class` | `empirical` (resolvable by data), `conceptual` (depends on framing), or `preference` (depends on values/policy) |
| Description | `omegas[].description` | Brief summary for the reporting engine |

The JSON format unifies the narrative detail and typed classification into a single object. The compiler generates both forms from your fields.

### Temporal Measurements

Provide measurement entries that model how the constraint changed over its interval. These enable the lifecycle drift detection system to identify:

* **Metric substitution** — `theater_ratio` rising above 0.5 indicates proxy goals replacing real function (Goodhart drift)
* **Extraction accumulation** — `base_extractiveness` increasing over time indicates rent-seeking layered onto coordination

**Required for high-extraction constraints** (extractiveness > 0.46). Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric — 6 measurements minimum.

| Field | JSON Path | Purpose |
|---|---|---|
| Metric name | `measurements[].metric` | `theater_ratio` or `base_extractiveness` |
| Time point | `measurements[].time_point` | Integer within the `interval` range |
| Value | `measurements[].value` | Metric value at that time point [0.0, 1.0] |
| ID override | `measurements[].id_override` | Optional: override the auto-generated measurement ID |

The final values should match your `base_properties` scores. The initial values represent the constraint's state at the start of the interval. If the constraint was always severe, use a flatter trajectory; if it degraded over time, show the progression.

The `interval` section declares the time range:

| Field | JSON Path |
|---|---|
| Start | `interval.start` |
| End | `interval.end` |

### Boltzmann and Coordination Data

Declare optional Boltzmann-related properties that enable structural purity and coupling analysis:

| Field | JSON Path | Purpose |
|---|---|---|
| Coordination type | `boltzmann.coordination_type` | Valid: `information_standard`, `resource_allocation`, `enforcement_mechanism`, `global_infrastructure` |
| Floor override | `boltzmann.boltzmann_floor_override` | Value in [0.0, 1.0]. Override default floor for this coordination type. |

Declare `coordination_type` when the constraint has an identifiable coordination function — it enables complexity-adjusted Boltzmann thresholds and floor calculations. Omit for constraints with no coordination role.

Only declare `boltzmann_floor_override` when domain knowledge justifies a different floor than the type default (e.g., a resource allocation mechanism that operates with unusually low overhead). Most constraints should use the type default.

### Network Relationships

Declare structural influence edges between constraints:

* **`network.affects_constraints`** — An array of constraint IDs that this constraint structurally influences. Declare when: constraints share a regulatory domain, have causal dependency, or exhibit institutional coupling.

Example: a story about rare earth dependency would include `"affects_constraints": ["semiconductor_supply"]`.

Network edges enable contamination propagation analysis — if one constraint's purity degrades, the system can predict which neighbors will be affected.

**Network Decomposition (Constraint Families):**

When a natural-language concept decomposes into multiple constraint stories (per the ε-invariance principle), the stories form a **constraint family**. All members of a family must be linked via `network.affects_constraints`.

* Every story in a family must link to at least one other family member. Orphan stories with no network connections are a code smell.
* When creating a new story that claims kinship with an existing constraint, document in `commentary.narrative_context` how the ε values differ and why.
* The upstream story (higher empirical confidence, more established) typically influences the downstream story (more contested, more extractive), because the upstream claim is often cited as evidence for the downstream claim.
* Include a dual formulation note in `network.dual_formulation_note` in BOTH files explaining the decomposition.

**BGS network pattern** (gold standard):

```
BGS constraint family (3 stories):
  ehrenfest_barrier (ε=0.05, Mountain) — phase-space resolution floor
    └→ bgs_spectral_universality (ε=0.08, Mountain) — eigenvalue statistics
        └→ bgs_eigenvector_thermalization (ε=0.42, Tangled Rope) — ETH compliance

Each story's network.affects_constraints array links to its dependents:
  ehrenfest_barrier → ["bgs_spectral_universality", "bgs_eigenvector_thermalization"]
  bgs_spectral_universality → ["bgs_eigenvector_thermalization"]
```

### Directionality Overrides

For cases where the automatic derivation (beneficiary/victim + exit → d) would produce an inaccurate directionality value, declare an explicit override in the `directionality_overrides` array:

| Field | JSON Path | Meaning |
|---|---|---|
| Power atom | `directionality_overrides[].power_atom` | One of: powerless, moderate, powerful, organized, institutional, analytical |
| D value | `directionality_overrides[].d_value` | [0.0, 1.0]: 0.0 = full beneficiary, 1.0 = full target |

**When to use overrides:**

* **Regulatory capture**: Institution that appears as a beneficiary but is actually partly captured — override d upward from derived ~0.15 to 0.25-0.40.
* **Indirect beneficiaries**: Agent appears in victim group but actually benefits through secondary effects — override d downward.
* **Asymmetric institutional relationships**: Two institutional actors the derivation can't distinguish (same power atom, same exit options, but different structural relationships) — use overrides to differentiate.

**When NOT to use overrides:**

* When the derivation chain already produces the right d from beneficiary/victim + exit. Most constraints don't need overrides.
* As a substitute for declaring beneficiary/victim. Always declare structural data first; override only if the derived d is wrong.

---

## UKE_SCOPE Integration

When generating a constraint story from a UKE_SCOPE manifest entry, map manifest fields to JSON as follows:

| Manifest Field | JSON Target | Notes |
|---|---|---|
| `claim_id` | `header.constraint_id` | Use as the constraint identifier |
| `epsilon_bin` | `base_properties.extractiveness` | Starting estimate; refine based on analysis |
| `hypothesis` | `base_properties.claimed_type` | Map the manifest's hypothesis to a constraint type |
| `downstream_of` | `network.affects_constraints` | Establishes family links |

The `uke_scope` section in the JSON is **informational** — it preserves provenance from the manifest but is not emitted to the `.pl` file. Include it when the story originates from a UKE_SCOPE manifest so that the authoring trail is traceable.

```json
"uke_scope": {
  "epsilon_bin": "high",
  "hypothesis": "snare",
  "downstream_of": ["parent_constraint_id"]
}
```

---

## Pre-Submission Validation Checklist

Before outputting your JSON, verify:

* [ ] **Beneficiary/Victim Declared**: Every non-mountain constraint has `base_properties.beneficiaries[]` with at least one entry. Snare and tangled_rope also require `base_properties.victims[]`. Group names are domain-specific, not generic placeholders (`low_income_borrowers` not `affected_parties`).
* [ ] **Threshold Accuracy**: Mountains ε ≤ 0.25, suppression ≤ 0.05. Snares ε ≥ 0.46, suppression ≥ 0.60, χ ≥ 0.66.
* [ ] **Mountain NL Profile**: If claiming mountain, includes `base_properties.accessibility_collapse` ≥ 0.85, `base_properties.resistance` ≤ 0.15, and `base_properties.emerges_naturally: true`. Without all three, the NL certification chain fails and the mountain metric gate does not fire.
* [ ] **Index Completeness**: Do your perspectives use the expanded 2026 values (e.g., `arbitrage`, `civilizational`)?
* [ ] **Suppression Check**: Suppression is a raw structural property (unscaled). Extractiveness is scaled by f(d) and σ(S) per χ = ε × f(d) × σ(S). Does the commentary reflect this?
* [ ] **Coalition Check**: If the constraint is a snare with multiple victims, does the analysis consider the possibility of coalition power for `powerless` agents?
* [ ] **Tangled Rope Check**: If Tangled Rope is used, does the JSON include `base_properties.beneficiaries[]` (coordination), `base_properties.victims[]` (asymmetric extraction), AND `base_properties.requires_active_enforcement: true`? All three are required by the canonical classifier.
* [ ] **Scaffold Check**: If Scaffold is used, does the JSON include `base_properties.has_sunset_clause: true` AND `base_properties.beneficiaries[]` (coordination function)?
* [ ] **Piton Check**: If Piton is used, does `base_properties.theater_ratio` ≥ 0.70?
* [ ] **Scope Awareness**: Spatial scope now affects χ via σ(S). Local (σ=0.8) dampens extraction; global (σ=1.2) amplifies it. Do your perspectives use appropriate scopes?
* [ ] **Perspective Minimum**: At least one `powerless` and one `institutional` perspective included — UNLESS the constraint is a uniform-type (mountain-only or rope-only), in which case any 2+ perspectives suffice.
* [ ] **Inter-Institutional Check**: If the constraint operates between institutional actors with different structural relationships, are separate perspective objects declared for each? Do they have different `exit_options`?
* [ ] **Temporal Data**: If `base_properties.extractiveness` > 0.46, include `measurements[]` entries at 3+ time points for `theater_ratio` and `base_extractiveness` (6 entries minimum).
* [ ] **Constraint Claim**: Does the JSON declare `base_properties.claimed_type`? This is required for Boltzmann compliance analysis and false natural law detection.
* [ ] **Coordination Type**: If the constraint has a coordination function, is `boltzmann.coordination_type` declared with one of the four valid types?
* [ ] **Network Relationships**: If the constraint is part of a known constraint cluster, are `network.affects_constraints[]` entries declared?
* [ ] **Directionality Overrides**: If overrides are used, does the commentary explain WHY the derivation would produce the wrong d?
* [ ] **Perspective Tuple**: Each perspective object has exactly 5 required fields: `classification_type`, `agent_power`, `time_horizon`, `exit_options`, `spatial_scope`. Do not add beneficiary/victim, measurement_basis, or any other data to perspectives.
* [ ] **Constraint Identity**: If this constraint could be evaluated via different observables that yield different ε values, have you decomposed into separate stories? Each story must have a single, stable ε. If ε changes when you change how you measure, you have two constraints — write two files and link with `network.affects_constraints`.

---

## Corpus Balance Guidance

The corpus needs balanced representation across all six types. When choosing scenarios for batch generation, prioritize the **underrepresented types**:

| Type | Best Source Domains | Key Metric Signature | Structural Data |
|------|-------------------|---------------------|----------------|
| **Tangled Rope** (most needed) | Geopolitical treaties, regulatory frameworks, platform governance, public-private partnerships | ε ≥ 0.30, suppression ≥ 0.40, 0.40 ≤ χ ≤ 0.90 | beneficiaries + victims + enforcement |
| **Scaffold** (most needed) | Transitional policies, emergency measures, development programs, sunset legislation | χ ≤ 0.30, theater ≤ 0.70 | beneficiaries + sunset clause |
| **Snare** (needed) | Debt traps, predatory lending, coercive labor, monopolistic extraction, surveillance systems | ε ≥ 0.46, suppression ≥ 0.60, χ ≥ 0.66 | victims required |
| **Inter-institutional** (NEW, needed) | Regulatory capture, trade agreements, sanctions, church/state, union/management | Varies by institutional perspective | Multiple institutional perspectives + overrides |
| **Mountain** (well-covered, needs NL metrics) | Mathematical theorems, physical laws, logical limits | ε ≤ 0.25, suppression ≤ 0.05, accessibility_collapse ≥ 0.85, resistance ≤ 0.15, emerges_naturally | No beneficiary/victim needed |
| **Rope** (well-covered) | Standards, protocols, cooperative agreements, coordination mechanisms | ε ≤ 0.45, χ ≤ 0.35 | beneficiaries; victims usually absent |
| **Piton** (well-covered) | Degraded institutions, vestigial regulations, theatrical compliance | ε ≤ 0.25, theater ≥ 0.70 | victims possible; beneficiaries unlikely |

**Scenarios that produce the richest perspectival gaps** come from: economic policy, labor regulation, healthcare access, housing markets, immigration systems, platform economics, and **inter-institutional dynamics** (regulatory capture, trade agreements, sanctions regimes). These domains naturally generate multiple institutional perspectives with different directionalities.

---

## Ready to Generate

When you receive a scenario, respond with a **complete, valid JSON document** following this structure. Make it immediately parseable and schema-compliant. State assumptions explicitly in your commentary. Declare beneficiaries and victims for every non-mountain constraint — these are the structural data that drive the engine's directionality computation.
