# UKE_SCOPE v2.0-json [Universal Knowledge Evaluator — Scoping & Extraction Protocol]

**Protocol Version:** 2.0-json
**Status:** Production
**License:** CC0-1.0 (Public Domain)
**Parent Suite:** UKE Protocol Suite
**Supersedes:** UKE_SCOPE v2.0 dual-output format, UKE_D (Extraction), UKE_C (Criticism/Flagging) in pipeline context

OUTPUT ONLY valid JSON — no markdown fences, no commentary outside the JSON.

---

## §0. FOUNDATION

### Purpose

Receive raw input (research, scenario description, article, conversation), extract the structural substrate, decompose it into generation-ready constraint axes, select the three most structurally central axes, and produce a single JSON manifest for the generation pipeline.

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
  → UKE_SCOPE §5 (JSON Manifest Assembly)
  → Orchestrator Review (fracture_scan check)
  → Per-axis: Generation → Linter → Prolog Engine
  → UKE_DISCUSSION (Final Essay Synthesis)
```

### What Changed from v2.0

| v2.0 | v2.0-json | Reason |
|------|-----------|--------|
| Dual output (markdown + JSON) | JSON manifest only | Orchestrator parses JSON directly; human-readable info carried as JSON string fields |
| §5.1 markdown blocks | Removed | topic_summary, extraction_summary, fracture_scan fields carry the same information |
| §5.2 JSON subsection | §5 JSON IS the output | No wrapper format |
| §7 Human checkpoint | Orchestrator decides pause | fracture_scan flags F03/F34 for orchestrator to handle |
| §8 Worked example shows both formats | §8 shows expanded JSON only | Single output format |

### Modes of Operation

**Automated Mode (default):** Full protocol execution. JSON manifest returned to orchestrator. If `fracture_scan` detects F03 or F34, orchestrator may pause for human review.

**Manual Mode:** Human reads the JSON manifest, makes selections, feeds axes to generation one at a time.

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
| `human_readable` | Display-friendly axis title for human review. |
| `structural_delta` | The irreducible difference. One sentence. |
| `primary_observable` | How ε would be measured. Concrete enough that two researchers agree. |
| `epsilon_bin` | V.Low (≤0.10), Low (0.11–0.30), Mod (0.31–0.55), High (0.56+) |
| `hypothesis` | mountain, rope, tangled_rope, snare, scaffold, piton |
| `beneficiary` | Who benefits (if applicable). null for mountains. |
| `victim` | Who bears costs (if applicable). null for mountains and ropes. |
| `downstream_of` | Claim_id of upstream axis, or empty array. |
| `feeds_into` | Claim_id(s) of downstream axes, or empty array. |

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

## §5. MANIFEST ASSEMBLY (JSON Output)

**Purpose:** Produce a single JSON manifest containing all analytical results.

The JSON manifest IS the complete output. All information that was previously in human-readable markdown blocks (UKE_META, EXTRACTION SUMMARY, CONSTRAINT MAP, CONSTRAINT GRAPH, SELF-SCAN, OMEGA, DEFERRED AXES, CHECKPOINT) is now carried as JSON fields.

### Required Fields

| Field | Description |
|-------|-------------|
| `protocol` | Always `"UKE_SCOPE"` |
| `version` | `"2.0-json"` |
| `domain` | Primary domain label |
| `family_id` | Snake_case family identifier |
| `topic_summary` | 2-3 sentence summary of the raw input (was in UKE_META) |
| `extraction_summary` | Counts + key entities and tensions (was in EXTRACTION SUMMARY) |
| `axes` | Full axis table with all classification fields |
| `generation_sequence` | Ordered list of selected claim_ids |
| `deferred_axes` | Axes not selected, with deferral reasons |
| `omegas` | Bounded uncertainties with source attribution |
| `fracture_scan` | Self-scan results as booleans + notes (was in SELF-SCAN) |

### §5.1 Upstream Context Payload

For each axis in `generation_sequence`, the orchestrator passes upstream context to the generation step. The Scope manifest defines what that context contains:

For axis N in the sequence, the generation prompt receives:
- The axis entry from the manifest (its own row)
- For each axis in `downstream_of` that was generated earlier: the constraint_id, claimed type, and `affects_constraint` hook (not the full .pl file)
- The `extraction_summary` anchors relevant to this axis (filtered by entity/tension references)

This keeps the generation context focused without passing all previously generated stories in full.

---

## §6. FRACTURE SELF-SCAN

The five mandatory checks and three dark matter probes apply identically to v2.0. Results are recorded in the `fracture_scan` field of the JSON manifest.

| Fracture | What It Catches | JSON Field |
|----------|-----------------|------------|
| F14 (Tunnel Vision) | All axes from one lens | `f14_tunnel_vision` |
| F15 (Premature Closure) | Stopped enumerating early | `f15_premature_closure` |
| F03 (Hasty Generalization) | Mountain without evidence | `f03_hasty_generalization` |
| F34 (Epistemic Trespass) | Beyond model's domain competence | `f34_epistemic_trespass` |
| F01 (Premise Drift) | Interpreted prompt differently than intended | `f01_premise_drift` |

If any fracture is detected, set the corresponding boolean to `true` and describe the finding in the `notes` field. Generate a corresponding omega entry in the `omegas` array.

---

## §7. ORCHESTRATOR CHECKPOINT

The JSON manifest includes `fracture_scan` results. The orchestrator decides whether to pause based on these flags:

- If `f03_hasty_generalization` is `true`: potential Mountain misclassification. Orchestrator may pause for human review.
- If `f34_epistemic_trespass` is `true`: domain competence concern. Orchestrator may pause for human review.
- All other fractures: logged, pipeline continues.

The protocol itself does not block. Checkpoint logic lives in the orchestrator, not in the prompt.

---

## §8. WORKED EXAMPLE: ALBERTA SOVEREIGNTIST TURN

**Input:** Research on Alberta separatism, the Alberta Prosperity Project, and the collision between resource wealth, federal transfers, and climate transition.

### §1–§4 Analysis (abbreviated)

Extraction identifies 4 entities, 3 claims, 2 tensions, 2 mechanisms, 2 absences. Multi-lens scan + dark matter probes produce 6 candidate axes. After independence testing and centrality scoring:

- sovereignty_as_arbitrage (centrality 6) — selected
- fossil_fuel_lock_in (centrality 3) — selected
- fiscal_equalization_friction (centrality 2) — selected
- boom_bust_path_dependency (centrality 2) — deferred
- climate_policy_extraction (centrality 3) — deferred
- u_s_alignment_projection (centrality 0) — deferred

### §5 JSON Manifest Output

```json
{
  "protocol": "UKE_SCOPE",
  "version": "2.0-json",
  "domain": "Political Economy / Federalism / Energy Policy",
  "family_id": "alberta_sovereignty_2026",
  "topic_summary": "Analysis of Alberta separatism driven by the Alberta Prosperity Project, examining the collision between hydrocarbon resource wealth, federal equalization transfers, and climate transition policy. The APP proposes independence as a mechanism to eliminate income tax and adopt the U.S. dollar, framing federal climate and fiscal policy as extraction from Alberta.",

  "extraction_summary": {
    "entity_count": 4,
    "claim_count": 3,
    "tension_count": 2,
    "mechanism_count": 2,
    "absence_count": 2,
    "key_entities": [
      "Alberta Prosperity Project (separatist movement promoting independence)",
      "Canadian Federal Government (constitutional authority; transfer payment administrator)",
      "Alberta Heritage Savings Trust Fund (sovereign wealth fund, under-capitalized vs Norway GPF)",
      "U.S. Treasury/State Department (potential external patron for dollar adoption)"
    ],
    "key_tensions": [
      "Provincial autonomy vs constitutional territorial integrity (APP vs federal government)",
      "Hydrocarbon revenue maximization vs emissions reduction (APP vs federal climate policy)"
    ]
  },

  "axes": [
    {
      "claim_id": "fossil_fuel_lock_in",
      "human_readable": "Fossil Fuel Lock-In (Structural Hydrocarbon Dependence)",
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
      "human_readable": "Fiscal Equalization Friction (Perceived Net Extraction)",
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
      "human_readable": "Sovereignty as Arbitrage (Independence as Extraction Bypass)",
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
      "human_readable": "Boom-Bust Path Dependency (Under-Investment in Resilience)",
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
      "human_readable": "Climate Policy as Extraction (Federal Rules as Imposed Cost)",
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
      "human_readable": "U.S. Alignment Projection (Dollar Adoption & D.C. Diplomacy)",
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

  "fracture_scan": {
    "f14_tunnel_vision": false,
    "f15_premature_closure": false,
    "f03_hasty_generalization": true,
    "f34_epistemic_trespass": false,
    "f01_premise_drift": false,
    "notes": "F03 detected: fossil_fuel_lock_in classified as mountain but global energy transition may reclassify it as piton of policy choice. Generated omega_mountain_validity to bound this uncertainty."
  }
}
```

---

## §9. KNOWN LIMITATIONS

Retained from v2.0:

- **L1 (Training Data Bias):** Dark matter probes reduce but do not eliminate.
- **L2 (Domain Expertise Ceiling):** F34 catches shallow training data when it fires.
- **L3 (Granularity Judgment):** The 4–8 axis heuristic is calibration, not truth.
- **L4 (Classification Instability):** Preliminary types are routing hypotheses.
- **L5 (Graph Completeness):** Edges are hypotheses. Generation may discover or invalidate them.
- **L6 (Selection Loss):** The 3-axis cap necessarily loses information. Deferred axes may contain the most analytically important dimension.
- **L7 (Centrality Bias):** The scoring formula privileges downstream synthesis nodes.
- **L8 (Extraction Fidelity):** §1 extraction depends on the model's reading comprehension.

---

## §10. INTEGRATION HOOKS

**Inputs:**
- User's raw prompt or uploaded document
- Domain context (optional)
- Research context from search grounding (optional)

**Outputs to orchestrator:**
- JSON manifest (§5) — parsed to drive generation loop

**Outputs to generation pipeline (per axis):**
- Axis entry from manifest
- Upstream context payload (§5.1)
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

**Document Status:** Protocol specification v2.0-json
**Generated:** 2026-02-22
**License:** CC0-1.0 (Public Domain)
**Dependencies:** UKE Protocol Suite, Deferential Realism constraint ontology, Constraint Story Generation Prompt v6.0
**Lineage:** UKE_SCOPE v2.0 (dual-output) → v2.0-json (JSON-only output)

*"Three axes capture the topology. The rest are omegas waiting for their turn."*
