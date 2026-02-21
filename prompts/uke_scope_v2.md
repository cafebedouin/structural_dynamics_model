# UKE_SCOPE v2.0 [Universal Knowledge Evaluator — Scoping & Extraction Protocol]

**Protocol Version:** 2.0
**Status:** Production
**License:** CC0-1.0 (Public Domain)
**Parent Suite:** UKE Protocol Suite
**Supersedes:** UKE_SCOPE v1.0, UKE_D (Extraction), UKE_C (Criticism/Flagging) in pipeline context

---

## §0. FOUNDATION

### Purpose

Receive raw input (research, scenario description, article, conversation), extract the structural substrate, decompose it into generation-ready constraint axes, select the three most structurally central axes, and produce a machine-parseable manifest for the generation pipeline.

### Core Invariants

**AVAILABILITY IS NOT COVERAGE.** The dimension most prominent in training data is not the dimension the domain requires. Probe actively for what the model would not spontaneously generate.

**THREE IS THE STRUCTURAL BUDGET.** The full decomposition may identify 4–10 axes. Exactly three proceed to generation. The remainder are documented as deferred axes and surfaced in the final essay's omega section. Three captures the upstream/downstream topology; more burns token budget without adding structural novelty.

**EXTRACTION BEFORE DECOMPOSITION.** The model must catalog what the input actually contains before interpreting what it means structurally. Observation precedes classification.

### Pipeline Position

```
User prompt
  → UKE_SCOPE §1 (Extraction)
  → UKE_SCOPE §2 (Decomposition)
  → UKE_SCOPE §3 (Independence Testing)
  → UKE_SCOPE §4 (Selection & Triage)
  → UKE_SCOPE §5 (Manifest Assembly)
  → Human Review (Checkpoint)
  → Per-axis: Generation → Linter → Prolog Engine
  → UKE_DISCUSSION (Final Essay Synthesis)
```

### What Changed from v1.0

| v1.0 | v2.0 | Reason |
|------|------|--------|
| No extraction phase | §1 Extraction added | UKE_D's anchor identification was doing this ad hoc |
| No axis cap | 3-axis cap with explicit selection | Rate limits, token budget, structural sufficiency |
| Prose output only | JSON blocks within markdown | Orchestrator must parse axes programmatically |
| Single output format | Dual format (human-readable + machine manifest) | Supports both interactive and automated modes |
| UKE_C handled flagging | Flagging folded into §2 decomposition | Criticism protocol is for evaluating finished work, not routing |
| Generation order recommended | Generation order computed from graph | Upstream axes must exist before downstream references them |

### Modes of Operation

**Interactive Mode (Streamlit):** Full protocol execution. Checkpoint pauses for human review. User toggles axes on/off, answers checkpoint questions. Selected axes proceed to generation.

**Automated Mode:** Full protocol execution. Checkpoint is logged but not blocking. Top-3 selection proceeds automatically via centrality scoring. Human reviews the final essay, not the scope output.

**Manual Mode:** Human reads the full output, makes selections, feeds axes to generation one at a time. Identical to v1.0 workflow.

---

## §1. EXTRACTION (Substrate Identification)

**Purpose:** Catalog the structural content of the raw input before any classification. This replaces UKE_D's "Extract anchors" step.

**Principle:** The model observes and records. It does not classify, hypothesize, or interpret. §1 output is a factual inventory that a hostile reader would confirm is present in the source material.

### §1.1 Anchor Inventory

Read the input and extract:

```
[ANCHORS]
entities:
  - id: [short_handle]
    name: [full name]
    type: [person | organization | institution | state | concept | resource]
    role: [as described in source — not interpreted]

claims:
  - id: [short_handle]
    content: [one-sentence paraphrase of a factual or argumentative claim in the source]
    source_location: [quote or paragraph reference]
    status: [stated_as_fact | argued | contested | implied]

tensions:
  - id: [short_handle]
    poles: [entity_a, entity_b]
    nature: [one sentence describing the structural opposition]

mechanisms:
  - id: [short_handle]
    description: [a causal pathway, enforcement mechanism, or feedback loop described in the source]
    agents: [who operates it]
    targets: [who it acts upon]

absences:
  - id: [short_handle]
    description: [something conspicuously not addressed in the source that a domain expert would expect]
```

**Extraction Rules:**

- Every anchor must be traceable to the source text. If the input does not contain it, do not invent it.
- Entities are named as the source names them. Do not rename for clarity yet.
- Claims preserve the source's framing. Reframing happens in §2.
- Absences are noted only when the model has high confidence that the missing element is structurally relevant (not merely tangential).

### §1.2 Domain Recognition

After extraction, identify:

```
[DOMAIN]
primary: [e.g., Political Economy, Comparative Linguistics, Technology Governance]
secondary: [additional domains the input touches]
disciplinary_lenses: [minimum 3 relevant lenses for §2 decomposition]
```

The disciplinary lenses are drawn from the domain, not from a fixed list. They must be relevant to the specific input, not generic.

---

## §2. DECOMPOSITION (Axis Identification)

**Purpose:** Convert the substrate into candidate constraint axes. This is the structural analysis step.

**Principle:** Each axis must have its own ε — its own extractiveness that would not change if the other axes were removed from consideration.

### §2.1 Multi-Lens Scan

For each disciplinary lens identified in §1.2, ask:

1. **What structural delta exists?** Not what people say is different — what is measurably, observably different in how the system is built.
2. **What is the primary observable?** How would this be measured? If you cannot name a measurement, the axis is not well-defined.
3. **Does this axis have its own ε?** Could you write a constraint story for this axis alone?

Each lens that produces a candidate axis adds a row to the working table.

### §2.2 Dark Matter Probes (Mandatory)

After the lens scan, apply:

**Probe 1 — Operational Medium:** What medium does the model operate within when analyzing this domain? Is that medium itself an unexamined axis?

**Probe 2 — Absence Inventory:** For each axis listed, what is the thing not being described? What was treated as background rather than figure? Cross-reference against the absences cataloged in §1.1.

**Probe 3 — Beneficiary Scan:** Who benefits from the current framing? Is there a structural axis the dominant framing obscures because making it visible would threaten the framing?

Each probe that surfaces a new candidate axis adds a row to the working table.

### §2.3 Preliminary Classification

For each candidate axis, assign:

| Field | Description |
|-------|-------------|
| `claim_id` | Snake_case handle. Becomes constraint_id in .pl if selected. |
| `structural_delta` | The irreducible difference. One sentence. |
| `primary_observable` | How ε would be measured. Concrete enough that two researchers agree. |
| `epsilon_bin` | V.Low (≤0.10), Low (0.11–0.30), Mod (0.31–0.55), High (0.56+) |
| `hypothesis` | mountain, rope, tangled_rope, snare, scaffold, piton |
| `beneficiary` | Who benefits (if applicable). "none" for mountains. |
| `victim` | Who bears costs (if applicable). "none" for mountains and ropes. |
| `downstream_of` | Claim_id of upstream axis, or "none". |
| `feeds_into` | Claim_id(s) of downstream axes, or "none". |

**Classification gates (from v1.0, retained):**

- **Mountain Check:** Would this persist in radically different political systems? If not, downgrade to rope or tangled_rope.
- **Beneficiary Check:** Who benefits from this axis being framed as it is? If someone specific benefits, likely tangled_rope or snare downstream.

---

## §3. INDEPENDENCE TESTING

**Purpose:** Neither over-split nor under-merge. Every pair of candidate axes must pass the pairwise test.

### §3.1 Pairwise Test

For every pair (A, B):

1. **Same observable?** → Merge unless different ε values from the same observable.
2. **Different observables, correlated?** → Keep separate. Document correlation.
3. **Different observables, causally linked?** → Keep separate. Document edge as `affects_constraint`.

### §3.2 Overlap Classification

When two axes appear to overlap:

- **Type A (Drift-Generated):** Same structural delta, different names. → Merge.
- **Type B (Structure-Generated):** Genuinely entangled, cannot fully separate. → Keep both. Document coupling.
- **Type C (Ambiguity-Generated):** User's prompt packaged multiple questions. → Split.

### §3.3 Granularity Check

- Fewer than 3 candidate axes after testing → likely under-split. Re-examine lenses.
- More than 8 → likely over-split. Look for merges or sub-domain decomposition.

---

## §4. SELECTION & TRIAGE (The Three-Axis Budget)

**Purpose:** Select exactly three axes for generation. This is the step that v1.0 lacked.

### §4.1 Centrality Scoring

For each candidate axis, compute a structural centrality score:

```
centrality = inbound_edges + outbound_edges + type_weight
```

Where `type_weight`:
- tangled_rope = 3 (highest priority — entanglement is the core analytical target)
- snare = 2
- mountain = 1 (important as upstream, but structurally simpler to analyze)
- rope = 1
- scaffold = 0
- piton = 1

Edges count `downstream_of` and `feeds_into` connections within the candidate set.

**Selection algorithm:**

1. Rank all candidates by centrality score.
2. Select the highest-scoring axis (typically a downstream tangled_rope or snare).
3. Select its most structurally distinct upstream dependency (highest ε difference from the first selection).
4. Select the next highest-scoring axis that is not already selected and is not a near-duplicate of the first two (different primary observable, different beneficiary/victim pair).

**If fewer than 3 candidates survive independence testing:** Generate what you have. Document the gap.

### §4.2 Deferred Axes

All candidate axes not selected become deferred axes. They are:

- Listed in the manifest with `selected: false`
- Available to the essayist as omega-level context
- Candidates for future generation if the user requests expansion

### §4.3 Generation Order

Computed from the constraint graph, not recommended:

1. Axes with no `downstream_of` dependency go first (upstream).
2. Axes whose upstream dependency is already ordered go next.
3. If two axes are independent (no edge between them), order by ε bin ascending (mountains before tangled_ropes).

The generation pipeline uses this order because downstream constraint stories reference upstream constraint_ids in `affects_constraint/2` declarations.

---

## §5. MANIFEST ASSEMBLY

**Purpose:** Produce two outputs — a human-readable summary and a machine-parseable JSON manifest.

### §5.1 Human-Readable Output

```
[UKE_META]
protocol: UKE_SCOPE
version: 2.0
domain: [domain label]
input_summary: [2-3 sentence summary of the raw input]
axes_identified: [total candidates]
axes_selected: 3
disciplinary_lenses_used: [list]

[EXTRACTION SUMMARY]
Entities: [count]
Claims: [count]
Tensions: [count]
Mechanisms: [count]
Absences: [count]
(Full extraction in §1 working notes.)

[CONSTRAINT MAP]
| # | Claim ID | Structural Delta | Observable | ε Bin | Hypothesis | Edges | Selected |
|---|----------|------------------|------------|-------|------------|-------|----------|
| 1 | ... | ... | ... | ... | ... | ← X, → Y | ✓ |
| 2 | ... | ... | ... | ... | ... | none | ✓ |
| 3 | ... | ... | ... | ... | ... | ← Z | ✓ |
| 4 | ... | ... | ... | ... | ... | → Y | deferred |

[CONSTRAINT GRAPH]
  [ascii directed graph — selected axes marked with ✓]

[GENERATION ORDER]
  1. [claim_id] (upstream, no dependencies)
  2. [claim_id] (upstream, independent)
  3. [claim_id] (downstream of 1 and 2)

[SELF-SCAN]
F14 (Tunnel Vision): [clean | detected → Ω]
F15 (Premature Closure): [clean | detected → Ω]
F03 (Hasty Generalization): [clean | detected → Ω]
F34 (Epistemic Trespass): [clean | detected → Ω]
F01 (Premise Drift): [clean | detected → Ω]
Dark Matter:
  Probe 1 (Operational Medium): [finding]
  Probe 2 (Absence Inventory): [finding]
  Probe 3 (Beneficiary Scan): [finding]

[OMEGA]
Ω: [label] — [specific bounded uncertainty]
Ω: [label] — [specific bounded uncertainty]

[DEFERRED AXES]
  [claim_id]: [one-line reason for deferral; available as essay context]
  [claim_id]: [one-line reason for deferral]

[CHECKPOINT]
Selected axes: [claim_id_1, claim_id_2, claim_id_3]
Generation order: [ordered list]
Question for reviewer: "[Specific question about what was missed or what should be swapped.]"
```

### §5.2 Machine Manifest (JSON)

This block is embedded in the output and is what the orchestrator parses. It appears between ```` ```json:scope_manifest ```` and ```` ``` ```` fences.

```json:scope_manifest
{
  "protocol": "UKE_SCOPE",
  "version": "2.0",
  "domain": "string",
  "family_id": "string (snake_case label for the constraint family)",

  "axes": [
    {
      "claim_id": "string",
      "structural_delta": "string",
      "primary_observable": "string",
      "epsilon_bin": "v_low | low | mod | high",
      "hypothesis": "mountain | rope | tangled_rope | snare | scaffold | piton",
      "beneficiary": "string | null",
      "victim": "string | null",
      "downstream_of": ["claim_id", ...],
      "feeds_into": ["claim_id", ...],
      "centrality_score": 0,
      "selected": true,
      "generation_order": 1,
      "selection_reason": "string (why selected or why deferred)"
    }
  ],

  "generation_sequence": ["claim_id_1", "claim_id_2", "claim_id_3"],

  "deferred_axes": [
    {
      "claim_id": "string",
      "structural_delta": "string",
      "hypothesis": "string",
      "deferral_reason": "string"
    }
  ],

  "omegas": [
    {
      "id": "string",
      "description": "string",
      "source": "string (which probe or fracture generated this)"
    }
  ],

  "extraction_summary": {
    "entity_count": 0,
    "claim_count": 0,
    "tension_count": 0,
    "mechanism_count": 0,
    "absence_count": 0
  }
}
```

### §5.3 Upstream Context Payload

For each axis in `generation_sequence`, the orchestrator passes upstream context to the generation step. The Scope manifest defines what that context contains:

For axis N in the sequence, the generation prompt receives:
- The axis entry from the manifest (its own row)
- For each axis in `downstream_of` that was generated earlier: the constraint_id, claimed type, and `affects_constraint` hook (not the full .pl file)
- The `extraction_summary` anchors relevant to this axis (filtered by entity/tension references)

This keeps the generation context focused without passing all previously generated stories in full.

---

## §6. FRACTURE SELF-SCAN

Retained from v1.0 without change. The five mandatory checks and three dark matter probes apply identically.

| Fracture | What It Catches | Mandatory Ω if Detected |
|----------|-----------------|-------------------------|
| F14 (Tunnel Vision) | All axes from one lens | Ω: Context Scope |
| F15 (Premature Closure) | Stopped enumerating early | Ω: Alternative Space |
| F03 (Hasty Generalization) | Mountain without evidence | Ω: Sample Validity |
| F34 (Epistemic Trespass) | Beyond model's domain competence | Ω: Domain Competence |
| F01 (Premise Drift) | Interpreted prompt differently than intended | Ω: Canonical Premise |

---

## §7. HUMAN CHECKPOINT

The protocol terminates at human review. UKE_SCOPE does not auto-approve its own output.

**Interactive Mode (Streamlit):**

The app displays:
1. The constraint map table with checkboxes per axis (top 3 pre-selected)
2. The constraint graph
3. Omegas and deferred axes
4. The checkpoint question

Human actions:
- **Toggle axes:** Select different 3. Swapping triggers re-computation of generation order.
- **Add axis:** Describe a new axis in natural language. It enters §3 (independence testing) against existing axes.
- **Approve:** Selected axes proceed to generation pipeline.
- **Reject:** Provide additional context. Scope restarts from §1.

**Automated Mode:**

The checkpoint is logged. Top-3 selection proceeds. If the self-scan produced any critical-severity omega (F03 or F34 detected), the pipeline pauses and flags for human review regardless of mode.

---

## §8. WORKED EXAMPLE: ALBERTA SOVEREIGNTIST TURN

**Input:** Research on Alberta separatism, the Alberta Prosperity Project, and the collision between resource wealth, federal transfers, and climate transition.

### §1 Extraction (abbreviated)

```
[ANCHORS]
entities:
  - id: app
    name: Alberta Prosperity Project
    type: organization
    role: Separatist movement promoting independence
  - id: federal_gov
    name: Canadian Federal Government
    type: state
    role: Constitutional authority; transfer payment administrator
  - id: heritage_fund
    name: Alberta Heritage Savings Trust Fund
    type: institution
    role: Sovereign wealth fund (under-capitalized vs Norway GPF)
  - id: us_treasury
    name: U.S. Treasury/State Department
    type: institution
    role: Potential external patron for dollar adoption

claims:
  - id: siphon_claim
    content: APP claims federal transfers represent net extraction from Alberta
    status: argued
  - id: zero_tax_promise
    content: APP promises $0 income tax in independent Alberta
    status: stated_as_fact
  - id: dollar_adoption
    content: APP proposes adopting U.S. dollar
    status: argued

tensions:
  - id: sovereignty_vs_federation
    poles: [app, federal_gov]
    nature: Provincial autonomy vs constitutional territorial integrity
  - id: extraction_vs_climate
    poles: [app, federal_climate_policy]
    nature: Hydrocarbon revenue maximization vs emissions reduction

mechanisms:
  - id: equalization_formula
    description: Federal transfer payment formula redistributes resource revenue
    agents: [federal_gov]
    targets: [alberta_taxpayers]
  - id: carbon_pricing
    description: Federal carbon tax increases costs for hydrocarbon producers
    agents: [federal_gov]
    targets: [oil_gas_sector]

absences:
  - id: indigenous_treaties
    description: No discussion of Crown treaty obligations in separation scenario
  - id: fiscal_resilience
    description: No long-term fiscal modeling beyond tax elimination promise
```

### §2–§3 Decomposition & Testing (abbreviated)

Six candidate axes identified. After pairwise independence testing:

| # | Claim ID | Structural Delta | Observable | ε Bin | Hypothesis | Edges | Centrality |
|---|----------|------------------|------------|-------|------------|-------|------------|
| 1 | fossil_fuel_lock_in | Structural dependence on hydrocarbons for GDP, employment, identity | HC share of provincial GDP | V.Low | mountain | → 3, → 5 | 3 |
| 2 | fiscal_equalization_friction | Perceived vs actual net fiscal contribution | Transfer payment formulas | Low | rope | → 3 | 2 |
| 3 | sovereignty_as_arbitrage | Independence framed as mechanism to bypass federal constraints for profit | APP fiscal blueprint metrics | Mod | tangled_rope | ← 1, ← 2, ← 4 | 6 |
| 4 | boom_bust_path_dependency | Chronic under-investment in long-term resilience during high-yield periods | Heritage Fund vs Norway GPF balance | V.Low | piton | → 3 | 2 |
| 5 | climate_policy_extraction | Federal climate rules perceived as imposed extraction | Sovereignty Act invocations vs federal legislation | High | snare | ← 1 | 3 |
| 6 | u_s_alignment_projection | State-building via informal D.C. diplomacy and dollar adoption | Treasury/State meeting frequency | Low | scaffold | none | 0 |

### §4 Selection

Centrality ranking: sovereignty_as_arbitrage (6) > fossil_fuel_lock_in (3) = climate_policy_extraction (3) > fiscal_equalization_friction (2) = boom_bust_path_dependency (2) > u_s_alignment_projection (0).

1. **Selected:** sovereignty_as_arbitrage — highest centrality, downstream synthesis node, 3 inbound edges.
2. **Selected:** fossil_fuel_lock_in — most structurally distinct upstream (mountain vs tangled_rope), different ε bin, different observable, feeds both selected axis 1 and deferred axis 5.
3. **Selected:** fiscal_equalization_friction — next highest centrality among remaining, different beneficiary/victim structure from axis 1, different observable from axis 2.

**Deferred:**
- boom_bust_path_dependency: Additional upstream to axis 3 but lower centrality than fiscal_equalization_friction; similar "past policy failure" structure. Available as essay context.
- climate_policy_extraction: High ε but structurally downstream of fossil_fuel_lock_in with limited independent analytical yield beyond what the mountain + tangled_rope pairing captures.
- u_s_alignment_projection: Zero centrality, no edges, speculative scaffold. Omega material.

**Generation order:** fossil_fuel_lock_in (1) → fiscal_equalization_friction (2) → sovereignty_as_arbitrage (3).

### §5 Machine Manifest

```json:scope_manifest
{
  "protocol": "UKE_SCOPE",
  "version": "2.0",
  "domain": "Political Economy / Federalism / Energy Policy",
  "family_id": "alberta_sovereignty_2026",

  "axes": [
    {
      "claim_id": "fossil_fuel_lock_in",
      "structural_delta": "Structural dependence on oil/gas for employment, identity, and tax revenue",
      "primary_observable": "Hydrocarbon share of provincial GDP; public opinion on net-zero resistance",
      "epsilon_bin": "v_low",
      "hypothesis": "mountain",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [],
      "feeds_into": ["sovereignty_as_arbitrage", "climate_policy_extraction"],
      "centrality_score": 3,
      "selected": true,
      "generation_order": 1,
      "selection_reason": "Most structurally distinct upstream; mountain providing physical constraint base"
    },
    {
      "claim_id": "fiscal_equalization_friction",
      "structural_delta": "Perceived vs actual net fiscal contribution to the Canadian federation",
      "primary_observable": "Net federal/provincial fiscal balance data; transfer payment formulas",
      "epsilon_bin": "low",
      "hypothesis": "rope",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [],
      "feeds_into": ["sovereignty_as_arbitrage"],
      "centrality_score": 2,
      "selected": true,
      "generation_order": 2,
      "selection_reason": "Different observable and beneficiary structure from axis 1; upstream to synthesis node"
    },
    {
      "claim_id": "sovereignty_as_arbitrage",
      "structural_delta": "Independence framed as mechanism to bypass federal tax/climate limits for profit",
      "primary_observable": "APP fiscal blueprint metrics (e.g., $0 income tax promise)",
      "epsilon_bin": "mod",
      "hypothesis": "tangled_rope",
      "beneficiary": "separatist_leadership",
      "victim": "national_social_safety_net",
      "downstream_of": ["fossil_fuel_lock_in", "fiscal_equalization_friction"],
      "feeds_into": [],
      "centrality_score": 6,
      "selected": true,
      "generation_order": 3,
      "selection_reason": "Highest centrality; downstream synthesis node with 3 inbound edges"
    },
    {
      "claim_id": "boom_bust_path_dependency",
      "structural_delta": "Chronic under-investment in long-term resilience during high-yield periods",
      "primary_observable": "Heritage Savings Trust Fund vs Norway GPF-Global balance",
      "epsilon_bin": "v_low",
      "hypothesis": "piton",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [],
      "feeds_into": ["sovereignty_as_arbitrage"],
      "centrality_score": 2,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: additional upstream but lower structural distinctiveness than fiscal_equalization_friction"
    },
    {
      "claim_id": "climate_policy_extraction",
      "structural_delta": "Federal carbon pricing/net-zero goals perceived as imposed extraction on provincial economy",
      "primary_observable": "Federal emissions legislation vs provincial Sovereignty Act invocations",
      "epsilon_bin": "high",
      "hypothesis": "snare",
      "beneficiary": null,
      "victim": "provincial_hydrocarbon_sector",
      "downstream_of": ["fossil_fuel_lock_in"],
      "feeds_into": [],
      "centrality_score": 3,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: structurally downstream of fossil_fuel_lock_in with limited independent yield"
    },
    {
      "claim_id": "u_s_alignment_projection",
      "structural_delta": "Attempted state-building via informal D.C. diplomacy and dollar adoption talks",
      "primary_observable": "Frequency/level of Treasury/State Dept meetings; currency adoption feasibility",
      "epsilon_bin": "low",
      "hypothesis": "scaffold",
      "beneficiary": null,
      "victim": null,
      "downstream_of": [],
      "feeds_into": [],
      "centrality_score": 0,
      "selected": false,
      "generation_order": null,
      "selection_reason": "Deferred: zero centrality, speculative scaffold, omega material"
    }
  ],

  "generation_sequence": [
    "fossil_fuel_lock_in",
    "fiscal_equalization_friction",
    "sovereignty_as_arbitrage"
  ],

  "deferred_axes": [
    {
      "claim_id": "boom_bust_path_dependency",
      "structural_delta": "Chronic under-investment in resilience during booms",
      "hypothesis": "piton",
      "deferral_reason": "Lower structural distinctiveness than selected upstream axes"
    },
    {
      "claim_id": "climate_policy_extraction",
      "structural_delta": "Federal climate rules perceived as imposed extraction",
      "hypothesis": "snare",
      "deferral_reason": "Captured indirectly via fossil_fuel_lock_in mountain + sovereignty_as_arbitrage tangled_rope"
    },
    {
      "claim_id": "u_s_alignment_projection",
      "structural_delta": "State-building via D.C. diplomacy and dollar adoption",
      "hypothesis": "scaffold",
      "deferral_reason": "Speculative; zero graph centrality; omega variable"
    }
  ],

  "omegas": [
    {
      "id": "omega_indigenous_treaty_collision",
      "description": "Does an independent Alberta maintain Crown treaty obligations? If not, the Mountain of territorial law becomes a Snare for Indigenous groups.",
      "source": "Dark Matter Probe 2 (Absence Inventory)"
    },
    {
      "id": "omega_currency_instability",
      "description": "Is dollar adoption a Rope (coordination) or Piton (atrophied sovereignty)? Without Fed oversight, systemic extraction via exchange-rate risk.",
      "source": "Dark Matter Probe 3 (Beneficiary Scan) + deferred axis u_s_alignment_projection"
    },
    {
      "id": "omega_mountain_validity",
      "description": "Is fossil_fuel_lock_in a true Mountain (physics of resource deposits) or a Piton of policy choice? Global energy transition may reclassify.",
      "source": "F03 (Hasty Generalization)"
    }
  ],

  "extraction_summary": {
    "entity_count": 4,
    "claim_count": 3,
    "tension_count": 2,
    "mechanism_count": 2,
    "absence_count": 2
  }
}
```

---

## §9. KNOWN LIMITATIONS

Retained from v1.0:

- **L1 (Training Data Bias):** Dark matter probes reduce but do not eliminate.
- **L2 (Domain Expertise Ceiling):** F34 catches shallow training data when it fires.
- **L3 (Granularity Judgment):** The 4–8 axis heuristic is calibration, not truth.
- **L4 (Classification Instability):** Preliminary types are routing hypotheses.
- **L5 (Graph Completeness):** Edges are hypotheses. Generation may discover or invalidate them.

New in v2.0:

- **L6 (Selection Loss):** The 3-axis cap necessarily loses information. Deferred axes may contain the most analytically important dimension. The deferred axis list and omega routing partially mitigate this; human review at the checkpoint is the primary safeguard.
- **L7 (Centrality Bias):** The scoring formula privileges downstream synthesis nodes (tangled_ropes with many inbound edges). This is intentional — these are the analytical targets — but it can under-weight structurally important mountains that have fewer graph connections. The "most structurally distinct upstream" selection in step 2 of the algorithm partially compensates.
- **L8 (Extraction Fidelity):** §1 extraction depends on the model's reading comprehension. Subtle claims or mechanisms in dense source material may be missed. The human checkpoint is the correction point.

---

## §10. INTEGRATION HOOKS

**Inputs:**
- User's raw prompt or uploaded document
- Domain context (optional)
- Prior conversation (optional)

**Outputs to orchestrator:**
- Machine manifest (JSON, §5.2) — parsed to drive generation loop
- Human-readable output (§5.1) — displayed in Streamlit for review

**Outputs to generation pipeline (per axis):**
- Axis entry from manifest
- Upstream context payload (§5.3)
- Relevant extraction anchors

**Outputs to UKE_DISCUSSION (essay synthesis):**
- Full manifest including deferred axes and omegas
- All generated (story, report) pairs
- Constraint graph topology

**Fracture codes (scoping-specific, retained from v1.0):**
- F41 (Constraint Compression): Multiple axes collapsed into one story
- F42 (Availability Capture): Axis selection dominated by training frequency
- F43 (Phantom Independence): Two axes sharing same observable and ε

**Omega routing:**
- Missing lens → Ω_lens_gap
- Contested mountain → Ω_mountain_validity
- Beyond model expertise → Ω_domain_competence
- Unclear independence → Ω_axis_independence
- User intent ambiguity → Ω_premise
- Deferred axis with high ε → Ω_deferred_high_epsilon

---

**Document Status:** Protocol specification v2.0
**Generated:** 2026-02-16
**License:** CC0-1.0 (Public Domain)
**Dependencies:** UKE Protocol Suite, Deferential Realism constraint ontology, Constraint Story Generation Prompt v6.0
**Lineage:** UKE_SCOPE v1.0 + UKE_D (extraction) + UKE_C (flagging) + pipeline automation requirements

*"Three axes capture the topology. The rest are omegas waiting for their turn."*
