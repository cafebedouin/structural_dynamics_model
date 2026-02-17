# 🔬 UKE_SCOPE v1.0 [Universal Knowledge Evaluator - Scoping Protocol]

### §0. FOUNDATION

**Purpose:** Identify the distinct structural axes within a domain before any constraint stories are written. Prevents Constraint Compression — the flattening of multiple structurally independent realities into a single narrative because the generating model reached for the most available dimension rather than the most complete set.

**Scope:** Pre-generation. UKE_SCOPE sits between the user's natural-language prompt and the constraint story generation prompt. It produces a Constraint Map that a human approves before generation begins.

**Core Invariant:** **Availability is not coverage.** The dimension most prominent in training data is not necessarily the most important dimension in the domain. The scoping model must actively probe for axes it might not spontaneously generate.

**Pipeline Position:**
```
User prompt → UKE_SCOPE (scoping) → Human review →
  Per-axis: Constraint story generation → Linter → Engine →
UKE_E (compression) → UKE_DR (if recommendations) →
UKE_A (audit) → UKE_R (review) → publish
```

**Input:** A natural-language prompt describing a domain, question, or scenario.
**Output:** A Constraint Map (table + directed graph) ready for human review.

**What This Protocol Is:**
A routing mechanism that decomposes a broad topic into generation-ready units. The preliminary classifications and ε estimates in the Constraint Map are behavioral triggers — they route each axis to the appropriate generation template. They are not truth claims about the domain. (See: UKE Protocol Framing Guide.)

**What This Protocol Is Not:**
- Not a substitute for domain expertise. The map requires human review.
- Not a generation protocol. No Prolog, no narrative, no stories.
- Not exhaustive. It produces the best decomposition the scoping model can manage, then asks what it missed.

---

### §1. DIMENSIONAL DECOMPOSITION

**Principle:** Enumerate structurally distinct axes before committing to any single one.

**§1.1 Multi-Lens Requirement**

For any domain involving two or more entities being compared (languages, institutions, systems, cultures), generate candidate axes from a minimum of three disciplinary perspectives relevant to the domain.

The disciplinary lenses are domain-dependent. Examples:

| Domain | Minimum Lenses |
|---|---|
| Language comparison | Phonology, morphosyntax, orthography, pragmatics, information theory |
| Economic policy | Microeconomics, political economy, institutional design, behavioral science |
| Technology governance | Engineering constraints, legal frameworks, market dynamics, social effects |
| Healthcare systems | Biology/medicine, insurance/finance, public health, patient experience |

**The 3-lens minimum is a floor, not a ceiling.** If the domain naturally spans five disciplines, use five. If a lens produces no candidate axes, document that — the absence is informative.

**§1.2 Axis Enumeration**

For each disciplinary lens, ask:

1. **What is structurally different between the entities being compared?** Not "what do people say is different" — what is measurably, observably different in how the thing is built?
2. **How would we measure this difference?** What is the primary observable? If you cannot name an observable, the axis is not well-defined.
3. **Does this axis have its own ε?** Could you write a constraint story for this axis alone, with a stable base extractiveness that would not change if you removed the other axes from consideration?

If the answer to (3) is yes, the axis is a candidate for an independent constraint story. If no, it may be a facet of another axis — document the dependency and consider merging.

**§1.3 The Dark Matter Probe**

After generating candidate axes from disciplinary lenses, apply these mandatory self-checks:

**Probe 1 — Operational Medium:** "What medium do I (the generating model) operate within when processing this domain? Is that medium itself a structural axis I haven't listed?"

*Rationale:* An LLM analyzing language differences operates in written text. A model analyzing economic systems operates through quantitative reasoning trained on English-language economics. The medium of analysis is often an unexamined axis. If it's structurally relevant to the domain, it must be listed.

**Probe 2 — Absence Inventory:** "For each axis I've listed, what is the thing I'm *not* describing? What did I treat as background rather than figure?"

*Rationale:* The Linguistic Availability Heuristic causes models to foreground what training data foregrounds. The corrective is to explicitly inventory what was backgrounded. If the inventory reveals a structural feature treated as default, it may be a missing axis.

**Probe 3 — Beneficiary Scan:** "Who benefits from the current framing of this domain? Is there a structural axis that the dominant framing obscures because making it visible would threaten the framing?"

*Rationale:* Tangled Rope constraints are often downstream of Mountains whose identification the Tangled Rope's beneficiaries prefer to suppress. The beneficiary scan surfaces axes that are politically inconvenient to name.

**§1.4 T1 Triggers for Scoping**

These claims require grounding before they can appear in the Constraint Map:

- **"This is a Mountain"** — requires naming the physical, logical, or biological law. If you cannot point to the law, it may be a Rope or Tangled Rope being presented as natural.
- **"These axes are independent"** — requires stating why ε differs or why the primary observables are distinct. "They feel different" is not grounding.
- **"This axis is not relevant"** — requires stating why. Exclusion is a claim and must be justified.

---

### §2. INDEPENDENCE TESTING

**Principle:** Neither over-split nor under-merge. Use ε-invariance as the structural test.

**§2.1 The Pairwise Test**

For every pair of candidate axes (A, B), ask:

1. **Same observable?** If A and B would be measured by the same experiment, instrument, or data source → **Merge** unless you can demonstrate that they produce different ε values from the same observable (in which case they are the same constraint measured at different resolutions — write one story, not two).

2. **Different observables, correlated?** If A and B are measured differently but tend to co-occur (e.g., tonal phonology and logographic writing systems co-occur in Chinese but are structurally independent — tonal languages exist with alphabets, and logographic systems exist without tone) → **Keep separate.** Document the empirical correlation and the structural independence.

3. **Different observables, causally linked?** If A structurally causes or enables B (e.g., a Mountain creates the conditions that a downstream Tangled Rope exploits) → **Keep separate and document the edge.** This becomes an `affects_constraint/2` relationship in the constraint story.

**§2.2 Trifurcation Diagnostic on Overlaps**

When two candidate axes appear to overlap, classify the overlap before resolving it (per UKE_G §2.1):

- **Type A (Drift-Generated):** Same structural delta, different terminology. → Merge. Standardize the name.
- **Type B (Structure-Generated):** Genuinely entangled axes that cannot be fully separated. → Keep both. Document the coupling. Flag for the constraint story author.
- **Type C (Ambiguity-Generated):** The user's prompt packaged multiple questions into one phrase. → Split. Each question becomes its own axis.

**§2.3 Granularity Calibration**

The scoping protocol must resist both directions of error:

**Over-splitting** produces a 15-row map where 6 rows are facets of the same constraint. Signal: multiple axes share the same primary observable, or removing one axis changes the ε estimate of another.

**Under-splitting** produces a 3-row map that misses half the domain. Signal: a disciplinary lens was checked and produced no axes (did the lens genuinely find nothing, or did the model fail to probe?), or the Dark Matter probes surface axes not in the table.

**Calibration heuristic:** For a binary comparison (Entity A vs Entity B) in a moderately complex domain, expect 4-8 independent axes. Fewer than 3 suggests under-splitting. More than 10 suggests over-splitting or a domain that should be decomposed into sub-domains first.

---

### §3. PRELIMINARY CLASSIFICATION

**Principle:** Route each axis to the appropriate generation template using lightweight classification. Do not over-invest in precision — these are hypotheses, not verdicts.

**§3.1 DR Lens Kit (Hypothesis Level)**

For each axis, assign a preliminary type using the Deferential Realism taxonomy:

| Type | Signal | Generation Template |
|---|---|---|
| Mountain | No agent designed it; no agent extracts from it; persists across political systems | Uniform-type (mountain-only), no beneficiary/victim |
| Rope | Solves coordination problem; low extraction; broadly beneficial | Low-ε story, beneficiary declared, victim usually absent |
| Tangled Rope | Genuine coordination AND asymmetric extraction | Dual-perspective story, beneficiary + victim + enforcement |
| Snare | High extraction, high suppression, minimal coordination | High-ε story, victim required, temporal measurements |
| Scaffold | Temporary support with sunset | Beneficiary + sunset clause |
| Piton | Atrophied function, theatrical maintenance | High theater ratio |

**§3.2 Lightweight Reality Check**

Apply two tests from the UKE_DR battery at scoping time (the rest apply post-generation):

**T1 — Mountain Check:** "Would this constraint persist in radically different political systems?" If yes → Mountain hypothesis holds. If no → downgrade to Rope or Tangled Rope. This catches premature Mountain classification before the Prolog engine has to reject it as `false_natural_law`.

**T5 — Beneficiary Check:** "Who benefits from this axis being framed the way it's currently framed in the dominant literature?" If the answer is "nobody — it's a symmetric structural fact" → Mountain or Rope. If the answer identifies specific beneficiaries → likely has a Tangled Rope downstream.

**§3.3 Upstream/Downstream Mapping**

For each axis classified as Tangled Rope, Snare, or Piton, ask: "What Mountain or Rope does this constraint build upon or exploit?" That upstream axis must also appear in the map. Draw the directed edge.

For each axis classified as Mountain or Rope, ask: "Is there a known framing, institutional practice, or cultural narrative that exploits this structural fact for asymmetric benefit?" If yes, the downstream axis must also appear in the map. Draw the directed edge.

The result is a directed graph: Mountains and Ropes at the top, Tangled Ropes and Snares downstream, with `affects_constraint` edges connecting them.

---

### §4. CONSTRAINT MAP OUTPUT

**§4.1 Table Format**

One row per axis. No narrative. Compression applies — if the axis cannot be described in this format, it is not well-defined enough for generation.

```
| Claim ID | Structural Delta | Primary Observable | ε Bin | Hypothesis | Downstream? | Independence Justification |
|----------|------------------|--------------------|-------|------------|-------------|---------------------------|
| [handle] | [irreducible difference] | [how measured] | [V.Low/Low/Mod/High] | [Mtn/Rope/TR/Snare/Scaf/Piton] | [→ claim_id or "none"] | [why not collapsible] |
```

**Column definitions:**

- **Claim ID:** Unique handle. Becomes the constraint_id in the .pl file if approved. Use snake_case, domain-specific.
- **Structural Delta:** The specific, irreducible difference in how the thing is built. One sentence. Not an interpretation — a structural fact.
- **Primary Observable:** How ε would be measured for this specific axis. Must be concrete enough that two researchers would agree on the measurement method.
- **ε Bin:** Routing estimate. V.Low (≤0.10), Low (0.11-0.30), Moderate (0.31-0.55), High (0.56+). This is a behavioral trigger, not a precision claim.
- **Hypothesis:** Preliminary DR classification. Will be refined during generation.
- **Downstream?:** If this axis feeds a downstream constraint, list the claim_id. If this axis IS downstream, list what it's downstream of.
- **Independence Justification:** One sentence explaining why this axis cannot be collapsed into another axis in the table. Must reference either a different ε, a different primary observable, or a different beneficiary/victim structure.

**§4.2 Graph Format**

Below the table, include a directed graph sketch showing `affects_constraint` edges:

```
CONSTRAINT FAMILY: [domain label]
  [mountain_axis_1] (ε bin, type)
    └→ [tangled_rope_1] (ε bin, type)
  [mountain_axis_2] (ε bin, type)
    └→ [tangled_rope_2] (ε bin, type)
    └→ [tangled_rope_1] (ε bin, type)  ← fed by both mountains
  [rope_axis_1] (ε bin, type)
```

---

### §5. FRACTURE SELF-SCAN

**Principle:** The scoping model must audit its own output before presenting it. The scan is mandatory, not optional.

**Required checks (from UKE_A Fracture Taxonomy):**

| Fracture | What It Catches in Scoping | Mandatory Ω if Detected |
|---|---|---|
| **F14 (Tunnel Vision)** | All axes from one disciplinary lens | Ω: Context Scope — What disciplinary perspective is missing from this map? |
| **F15 (Premature Closure)** | Stopped enumerating too early | Ω: Alternative Space — What unexplored axis must be considered? |
| **F03 (Hasty Generalization)** | Axis classified as Mountain without observable evidence | Ω: Sample Validity — What evidence supports this axis's preliminary classification? |
| **F34 (Epistemic Trespass)** | Scoping model lacks domain expertise for an axis | Ω: Domain Competence — Does the scoping model have adequate training data to evaluate this axis? |
| **F01 (Premise Drift)** | User's prompt interpreted differently than intended | Ω: Canonical Premise — Does the constraint map address what the user actually asked? |

**Report format:**

```
[SCOPING SELF-SCAN]
F14 (Tunnel Vision): [clean | detected → Ω generated]
F15 (Premature Closure): [clean | detected → Ω generated]
F03 (Hasty Generalization): [clean | detected → Ω generated]
F34 (Epistemic Trespass): [clean | detected → Ω generated]
F01 (Premise Drift): [clean | detected → Ω generated]
Dark Matter Probes:
  Probe 1 (Operational Medium): [finding]
  Probe 2 (Absence Inventory): [finding]
  Probe 3 (Beneficiary Scan): [finding]
```

---

### §6. HUMAN CHECKPOINT

**The scoping protocol terminates at human review.** UKE_SCOPE does not auto-approve its own output.

**Presentation to human:**

1. The Constraint Map (table + graph)
2. The Fracture Self-Scan results
3. Any Ω variables generated from the scan
4. An explicit question: **"I've identified N structural axes. What did I miss?"**

**Human actions:**

- **Approve:** Map proceeds to generation. Each row becomes an input to the constraint story generation prompt, one axis per generation call.
- **Modify:** Add axes, remove axes, merge axes, reclassify. Modified map re-enters §2 (Independence Testing) for the changed rows only.
- **Reject:** Map is inadequate. Human provides additional framing or domain context. Scoping restarts from §1.

**After approval, the map becomes structural.** Constraint stories generated from it should reference the approved map in their narrative context sections. If a constraint story author discovers during generation that an axis needs to be split or merged, they flag this as a map revision request — they do not silently deviate from the approved scope.

---

### §7. OUTPUT FORMAT

```
[UKE_META]
protocol: UKE_SCOPE
version: 1.0
domain: [domain label]
input_prompt: [user's original prompt, verbatim]
axes_identified: N
disciplinary_lenses_used: [list]

[CONSTRAINT MAP]
| Claim ID | Structural Delta | Primary Observable | ε Bin | Hypothesis | Downstream? | Independence Justification |
|----------|------------------|--------------------|-------|------------|-------------|---------------------------|
| ... | ... | ... | ... | ... | ... | ... |

[CONSTRAINT GRAPH]
  [ascii directed graph]

[SCOPING SELF-SCAN]
  {fracture scan results from §5}

[ΩΩΩΩ]
Ω: [label] — [specific bounded uncertainty from fracture scan or dark matter probes]

[CHECKPOINT]
Axes identified: N
Recommended generation order: [which axes to write first, based on upstream/downstream]
Question for reviewer: "What did I miss?"
```

---

### §8. WORKED EXAMPLE (The English-Chinese Failure Case)

**Input prompt:** "The relative structures of the English and Chinese languages and comment on how that might affect our individual thoughts, and also our cultures."

**What happened without UKE_SCOPE:** The generating model reached for tense/aspect obligatoriness (the dimension most prominent in anglophone Sapir-Whorf literature), decomposed it into Mountain + Tangled Rope using ε-invariance, and produced two well-formed constraint stories that covered one dimension thoroughly while missing four others entirely. The Prolog engine then rejected the Mountain as `false_natural_law`, and a human reviewer identified the coverage gap.

**What UKE_SCOPE would have produced:**

Disciplinary lenses: phonology, morphosyntax, orthography, pragmatics, information theory.

| Claim ID | Structural Delta | Primary Observable | ε Bin | Hypothesis | Downstream? | Independence Justification |
|---|---|---|---|---|---|---|
| orthographic_system | Alphabetic (phoneme→grapheme) vs logographic (morphosyllabic→character) | Eye-tracking saccade patterns; neural activation in reading tasks | V.Low | Mountain | → orthographic_deficit_framing | Different neural substrate than morphosyntax; measurable independently via reading studies |
| tense_aspect_obligatoriness | Obligatory verb morphology vs aspect markers + pragmatic context | Error rates in temporal forced-choice tasks | V.Low | Mountain | → linguistic_relativity_framing | Independent of writing system; operates in spoken language |
| syntax_topology | SVO + recursive subordination vs topic-comment + serial verb | Dependency length in parsed corpora | V.Low | Mountain | → directness_deficit_framing | Different parsing complexity profile; measurable via syntactic analysis |
| information_density | High morphological marking per word vs high semantic density per character | Bits per unit (character/word) in parallel corpora | V.Low | Rope | none | Different compression strategy; measurable via information theory |
| classifier_systems | No classifiers vs obligatory classifiers for counting/reference | Categorization task performance cross-linguistically | V.Low | Mountain | none | Independent morphosyntactic subsystem; own experimental literature |
| linguistic_relativity_framing | Deficit framing of Chinese features in anglophone research/pedagogy | Citation bias; grant allocation; textbook framing analysis | Moderate | Tangled Rope | ← tense, orthographic, syntax (fed by all three mountains) | Extraction structure distinct from any single structural fact |
| orthographic_deficit_framing | "Chinese is hard" / "character memorization is rote" in ELT | Pedagogical materials analysis; learner self-efficacy measures | Moderate | Tangled Rope | ← orthographic_system | Distinct victim group (heritage readers) and distinct institutional beneficiary (alphabetic-literacy testing industry) |
| directness_deficit_framing | "Chinese communication is indirect/vague" in corporate training | Cross-cultural training materials; performance review language | Low-Mod | Tangled Rope | ← syntax_topology | Distinct institutional practice (corporate communication norms) |

```
CONSTRAINT FAMILY: english_chinese_structural_comparison
  orthographic_system (V.Low, Mountain)
    └→ orthographic_deficit_framing (Mod, Tangled Rope)
    └→ linguistic_relativity_framing (Mod, Tangled Rope)
  tense_aspect_obligatoriness (V.Low, Mountain)
    └→ linguistic_relativity_framing (Mod, Tangled Rope)
  syntax_topology (V.Low, Mountain)
    └→ directness_deficit_framing (Low-Mod, Tangled Rope)
    └→ linguistic_relativity_framing (Mod, Tangled Rope)
  information_density (V.Low, Rope)
  classifier_systems (V.Low, Mountain)
```

**Scoping Self-Scan:**

```
[SCOPING SELF-SCAN]
F14 (Tunnel Vision): clean — 5 disciplinary lenses used
F15 (Premature Closure): detected → Ω generated
  Ω: Phonological Gap — Tonal phonology (Mandarin is tonal; English is
     stress-timed) is absent from this map. Different auditory processing
     demands, different musical cognition, different poetic traditions.
     Should this be a separate axis?
F03 (Hasty Generalization): detected → Ω generated
  Ω: Mountain Validity — Are the Mountain classifications for
     orthographic_system and syntax_topology defensible? The Prolog engine
     rejected tense_aspect_obligatoriness as false_natural_law (coupling
     score 0.75). The same objection may apply: language structures are
     historically constructed, not physically necessary. Consider
     classifying as Rope instead.
F34 (Epistemic Trespass): detected → Ω generated
  Ω: Classifier Expertise — The scoping model's training data on
     classifier systems and categorization cognition is shallow. The
     axis is included because the experimental literature exists, but
     the preliminary classification confidence is low.
F01 (Premise Drift): clean — map covers both "individual thoughts"
     (cognitive axes) and "cultures" (institutional framing axes)
Dark Matter Probes:
  Probe 1 (Operational Medium): Writing system IS listed (orthographic_system).
     However, information processing at the tokenization level — how LLMs
     segment Chinese vs English differently, affecting the analysis itself —
     is not represented. Flag for human review.
  Probe 2 (Absence Inventory): Discourse-level coherence strategies (English
     uses explicit connectives; Chinese uses zero anaphora and topic chains)
     are partially captured under syntax_topology but may deserve a separate
     axis. Pragmatic inference demands differ.
  Probe 3 (Beneficiary Scan): The alphabetic-literacy testing industry
     (TOEFL, IELTS, Cambridge) benefits from framing Chinese orthography
     as "difficult." This is captured in orthographic_deficit_framing.
```

**Result:** 8 axes identified, 3 Ω variables generated, explicit gaps flagged. The human reviewer would see the full map and decide which axes to develop into constraint stories, which to defer, and which to merge. Generation would proceed one axis at a time, with `affects_constraint` edges pre-specified by the graph.

---

### §9. KNOWN LIMITATIONS

**L1: Training Data Bias.** The Linguistic Availability Heuristic is not fully solvable from inside. The Dark Matter probes reduce but do not eliminate the risk. Human review is the termination point, not the protocol.

**L2: Domain Expertise Ceiling.** The scoping model cannot identify axes in domains where its training data is thin. F34 (Epistemic Trespass) catches this when it fires, but it may not always fire. Domains requiring deep specialist knowledge should have a domain expert review the map.

**L3: Granularity Judgment.** The 4-8 axis heuristic is a rough calibration, not a validated threshold. Some domains genuinely have 12 independent axes; others have 3. The heuristic routes attention, not truth.

**L4: Classification Instability.** Preliminary Mountain/Rope/Tangled Rope assignments may change during generation as the constraint story author engages with the domain in detail. This is expected, not a failure — the scoping classification is a routing hypothesis. Flag significant reclassifications as map revision requests.

**L5: Graph Completeness.** The `affects_constraint` edges in the graph are hypotheses. The constraint story generation process may discover edges the scoping protocol missed, or may find that hypothesized edges don't hold. Update the graph during generation.

---

### §10. INTEGRATION HOOKS

**Pre-UKE_SCOPE (Inputs):**
- User's natural-language prompt
- Domain context (if provided)
- Prior conversation history (if relevant)

**Post-UKE_SCOPE (Outputs):**
- To **Human Review**: Constraint Map for approval
- To **Constraint Story Generation**: One approved row per generation call
- To **UKE_E**: If user requests condensed output, compress after generation

**Fracture Codes (Scoping-Specific):**
- **F41 (Constraint Compression):** Multiple independent axes collapsed into one story. Detected when a constraint story's ε changes depending on which observable is foregrounded.
- **F42 (Availability Capture):** Axis selection dominated by training-data frequency rather than domain coverage. Detected when all axes originate from a single disciplinary tradition.
- **F43 (Phantom Independence):** Two axes claimed as independent but sharing the same primary observable and ε. Detected during Independence Testing (§2).

**Omega Routing:**
- Missing disciplinary lens → Ω_lens_gap
- Contested Mountain classification → Ω_mountain_validity
- Axis beyond model expertise → Ω_domain_competence
- Unclear independence → Ω_axis_independence
- User intent ambiguity → Ω_premise

---

**Document Status:** Protocol specification v1.0
**Generated:** 2026-02-12
**License:** CC0-1.0 (Public Domain)
**Dependencies:** UKE Protocol Suite, Deferential Realism constraint ontology, Constraint Story Generation Prompt v6.0
**Lineage:** Failure analysis (English-Chinese tense compression) → Gemini diagnostic (Dimensional Decomposition) → Protocol synthesis from MCK v1.6, UKE_G v1.4, UKE_A v1.4, UKE_E v2.2, UKE_DR v1.0, UKE Protocol Framing Guide

*"Availability is not coverage. The dimension the model reaches for first is the dimension the training data foregrounds, not the dimension the domain requires."*
