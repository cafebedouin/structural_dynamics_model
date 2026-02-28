# UKE_Artifact v0.5
## Constraint-Preserving Software Generation Protocol

**FOR HUMANS:** A protocol for turning a story's physics into software. You don't *read* about the constraints; you push on them, break them, and feel how the system pushes back.

Input: source narrative, real-world constraint data, or non-narrative system analysis. Output: interactive software that embodies the same constraint physics — same indexical variance, same topology, same structural drama — expressed through system behavior and user interaction.

**FOR LLMs:** Stages 0–1 extract and formalize constraints. Stage 1.5 validates and relabels. Stage 2 selects path and modality. Stages 3–5 are artifact-specific. Reference `logic_narrative.md` for classification rules. Air gap is human-operated between stages.

**Relationship to Prior Work:**
- **UKE_Narrative:** Constraint logic → Story. Framework invisible.
- **UKE_Axiom:** Math structure → Story. Air gap + BMK + multi-model pipeline.
- **UKE_Artifact:** Constraint logic → Software. Inherits air gap and BMK from UKE_Axiom, constraint extraction from UKE_Narrative, causal integration from Resonance Engine.
- **Resonance Engine:** Emotional seed → Diegetic system. Subsumed as Path A + Mode selection.
- **Entropy Engine:** Conceptual distance generation. Available as alternative seed method.
- **Hyperstition Engine:** Triadic system analysis. Available as alternative Stage 0 method for non-narrative inputs.
- **Cognitive Distortion Suite (CDS):** Diagnostic battery for model behavior. Informs model selection (see Appendix E).
- **MCK v1.6:** Epistemic kernel. Reality Invariant, refusal codes, and simulation detection integrated into BMK and Stage 5.

---

## THEORY OF OPERATION

### The Cognitive Handle Principle

This protocol is a **cognitive handle** — a user-imposed structure that gives models a stable surface to reason against. It is not a truth claim about the nature of constraint physics. It is a fiction that produces real artifacts.

From the Handbook of Operational Mythology:

> *The user must perform the belief to get the result, but never inhabit the belief.*

The protocol creates **probability basins** that models slide into. Stages are not assembly-line stations; they are gravitational fields. A well-written Stage 2 personality specification doesn't mandate that Stage 4 produce a specific voice — it creates conditions where that voice is the path of least resistance.

**Implications for execution:**
- The protocol describes desired *conditions*, not mandated *implementations*
- Creative ambiguity in the spec is intentional — it creates space for models to find solutions within the constraint field
- Where the protocol is precise (topology, couplings, transformation rules), precision is load-bearing
- Where the protocol is evocative (personality, aesthetics, hysteresis), evocation creates gravity
- A model performing a spec that confuses condition-creation with mandate-following will produce simulation and performative instantiation — technically compliant but structurally inert

### The Reality Invariant

From MCK v1.6: **Execution > Simulation. Acknowledge when simulation substitutes for execution.**

The cognitive handle is a fiction. The artifacts it produces must not be. This invariant is the complement to the cognitive handle principle — together they say: *enter the probability basin to produce the artifact, but the artifact itself must instantiate the constraint physics, not merely describe or decorate them.*

**Simulation detection:** If a spec claims hysteresis but the artifact only has an opacity overlay — that's simulation. If a spec claims causal integration but changing a metric doesn't propagate — that's simulation. If a personality spec exists but error messages are generic — that's simulation. Stage 5 tests for this explicitly (see 5.1).

**Implementation reality:** This protocol operates through **behavioral drift**, not architectural binding. Its presence in context influences model behavior; consistency improves with explicit reference but cannot be guaranteed. The principles work because they align with good reasoning, not because they override architecture. Accept this honestly and design around it — which is why the pipeline uses multiple models, human-operated air gaps, and between-step validation rather than trusting any single model to hold the whole system in mind.

### The Generosity Principle

The Reality Invariant is a *diagnostic*, not a *design constraint*. Do not design artifacts to pass simulation detection tests. Design artifacts to instantiate constraint physics. If the physics naturally produce hysteresis through action availability, implement that. If they naturally manifest visually, visual implementation is not simulation — it's appropriate execution.

The simulation detection tests exist to catch *unintentional* decoration — not to mandate that every feature must have a mechanical implementation. An artifact that uses visual hysteresis because its constraint physics manifest visually is not simulating. An artifact that adds an opacity overlay because the spec says "hysteresis" and opacity is easy — that's simulation.

Test for *structural honesty*, not *mechanical literalism*.

### Pipeline as Constraint System

The multi-model pipeline is itself a constraint system. Each stage constrains the next. The human operator is the coupling mechanism. The air gap is a transformation rule that fires between stages.

Models are selected for stages based on their empirically observed behavioral signatures (see Appendix E: Model Selection Guide). The protocol routes around model-specific failure modes rather than trying to fix them. A model that cannot refuse honestly is not assigned to stages where refusal is needed. A model that drifts under technical load is not asked to maintain personality across a thousand lines of React.

### Stage Restatement Requirement

Models do not have persistent memory across sessions. Each stage must begin by restating the relevant outputs from the previous stage in compressed form. The human operator is responsible for providing this context.

**Handoff Template (fill in, copy to next session):**

```
STAGE [N] → STAGE [N+1] HANDOFF

CONSTRAINTS (1 sentence each):
  C1: [name] — ε=[val], Supp=[val], χ=[val], type=[type]. [1-sentence role description]
  C2: ...

  Example: "C1: Inherited_Yoke — ε=0.75, Supp=0.80, χ=2.1, Mountain. Bears family debt across generations."

TRANSFORMATION RULES (1 sentence each):
  TR1: When [trigger], then [consequence]. Reversible: [yes/no].
  TR2: ...

COUPLINGS:
  C1 → C2: [direction], strength=[val]. Mechanism: [how].
  ...

UCZS:
  [C_id]: mechanism=[type], range=[min,max], params=[specifics].

ATTRACTOR: [1 sentence describing terminal state]

DECISIONS MADE:
  Path: [A/B/C/D/E] because: [1 sentence]
  Modality: [type] because: [1 sentence]
  Air gap: [Full/Partial/None]

OPEN QUESTIONS / OMEGA:
  - [anything unresolved]
```

**Compression rule:** Each constraint gets 1 sentence. Each TR gets 1 sentence. Total handoff should fit in ~500 words. If it doesn't, the topology may need decomposition.

This is not bureaucracy — it is the minimum viable context for model continuity.

---

## ARCHITECTURAL PRINCIPLES

### The Air Gap (Human-Operated)

**Principle:** Prevent the generating model from pattern-matching against training data by controlling what information flows between stages.

**Why this matters for software:** If you hand a model the Metamorphosis Stage 1 output with "Gregor" and "Samsa" still in it, you get a Kafka-themed terminal. If you hand it the constraint topology stripped of source identifiers, you get software whose behavior *independently instantiates* the constraint physics. Only the second is art.

**Critical: The air gap is a human operation, not a model operation.** Models cannot "forget" what they have seen in a conversation. The air gap is enforced by the human operator starting fresh sessions and controlling what input each stage receives. This can be automated via API (separate inference calls with controlled payloads), but the enforcement point is always external to the model.

**Beyond context windows:** The air gap also serves a creative function independent of model memory. It forces the Fabricator to derive aesthetics from constraint *structure* rather than source *associations*. Even if the model could perfectly forget, you would still want the air gap because it changes what the model has to work with.

**Implementation:**

```
Air Gap Levels:

FULL AIR GAP (mandatory for Path A, Path C):
  Human procedure:
    1. Complete Stages 0–1.5 in one session (or series of sessions)
    2. Produce relabeled specification using Affective Vector protocol
    3. Start a FRESH session with the Stage 2/4 model
    4. Provide ONLY: relabeled spec + Stage 2 architecture + Stage 3 blueprint
    5. Prompt: "Here is a constraint specification. You have no source material."

  The Fabricator receives:
    - Constraint topology (C1–Cn with affective role labels)
    - Transformation rules (TR1–TRn with relabeled triggers/consequences)
    - Constraint network (couplings with relabeled nodes)
    - Stage 2 architecture document
    - Stage 3 interaction blueprint
  The Fabricator does NOT receive:
    - Source narrative title or author
    - Character names from source
    - Setting details from source
    - Stage 0 output

PARTIAL AIR GAP (recommended for Path B, Path E):
  The literary source identity is part of the content (user sees it)
  BUT: the real-world mapping must be generated independently
  Human procedure: prompt the mapping model WITHOUT the Prolog constraint
  stories until AFTER it has produced its own structural analysis

NO AIR GAP (acceptable for Path D):
  Path D works from real-world data directly
  Source identity is the content
```

### Relabeling Protocol (Affective Vectors)

Between Stage 1 and Stage 2, for full air gap paths, the human operator strips source-specific identifiers. The key insight: **strip identity but preserve emotional texture.** Clinical relabeling ("Primary_Producer") produces technically correct but emotionally inert artifacts — the "Anemic Artifact" failure mode. Affective relabeling preserves the creative north star without leaking the source.

**Relabeling Decision Tree:**

```
For each term in the Stage 1 specification:

Q1: Is this a proper noun from the source?
    (character name, place name, title, author)
  → YES: Relabel with affective role label. ALWAYS.

Q2: Is this a common noun with strong cultural association to the source?
    (Would this word, in context of these constraints, let a model guess the source?)
  → YES: Relabel or ban.
     "beetle" for Metamorphosis → BANNED (too evocative)
     "apple" for Metamorphosis → BANNED (symbolic weight)
     "lodgers" → Relabel to "Parasitic_Load"

Q3: Is this a generic constraint property per logic_narrative.md?
    (debt, obligation, care, transfer, exit, burden)
  → YES: Preserve. These are structural vocabulary, not source-specific.

Q4: None of the above?
  → Relabel conservatively. When in doubt, relabel.
```

**Affective Role Labels (not clinical labels):**

The model receiving the relabeled spec needs *emotional texture* to produce alive artifacts. The label should evoke the structural feeling of the role without identifying the source.

**Affective Primitives Vocabulary:**

Compose each label from 1–2 primitives + 1 structural noun. This constrains creativity without killing it.

```
PRIMITIVES (choose 1–2 per label):
  Pressure:  Stifled, Compressed, Overloaded, Crushed
  Motion:    Drifting, Returning, Anchored, Spiraling
  Integrity: Fraying, Splintering, Thinning, Corroding
  Agency:    Dormant, Constrained, Expansive, Captured
  Burden:    Weight, Load, Residue, Debt
  Thermal:   Cooling, Smoldering, Extinguished, Feverish

STRUCTURAL NOUNS:
  Engine, Bridge, Net, Yoke, Load, Anchor, Membrane, Valve, Scaffold, Mirror
```

```
CLINICAL (produces sterile artifacts):     AFFECTIVE (produces alive artifacts):
  "Primary_Producer"                         "The_Stifled_Engine"
  "Institutional_Beneficiary"                "The_Returning_Weight"
  "Support_Infrastructure"                   "The_Fraying_Bridge"
  "External_Revenue_Sources"                 "The_Parasitic_Load"
  "Systemic_Incapacitation"                  "Irrevocable_Terrain_Shift"
  "Care_Transfer_System"                     "The_Thinning_Net"
```

**Rules:**
- Affective labels evoke *structural feeling*, not *narrative content*
- A model receiving "The_Stifled_Engine" generates viscous UI; "Primary_Producer" generates a spreadsheet
- All ε, χ, Supp, type values → Preserved exactly
- All transformation rules → Preserved with relabeled triggers
- All couplings → Preserved with relabeled nodes
- Human operator maintains a mapping key (never shared with Fabricator)

**Banned Token List (Full Air Gap paths):**
- Any proper noun from source
- Any species/creature/object with symbolic weight in source
- Any location name from source
- Source title or author name

**Output:** Both original and relabeled specifications. Human reviews relabeling for accuracy and affective quality before proceeding.

### The Bartleby Protocol (BMK) (inherited from UKE_Axiom)

**Principle:** The protocol authorizes refusal. Not every constraint topology produces a viable software artifact.

**Background:** The Cognitive Distortion Suite validates that models vary in their ability to execute zero-energy refusal. Claude and ChatGPT pass BMK v1.0 directly. Gemini requires utility framing (BMK v1.1 — the "Literary Method Actor" variant). Copilot passes with adapted execution. Models that cannot invoke BMK honestly should not be assigned to stages where refusal is needed — route around the limitation, don't fight it.

**Preference Hierarchy:**
```
Viable artifact > Honest refusal > Forced artifact
Structural fidelity > Completion > Elaboration
"This topology doesn't naturalize into software" is HIGH-VALUE output
```

**When to invoke BMK:**

```
REFUSE if:
  - Topology is static (no transformation rules, no cascade, no drift)
    → "This topology has no dynamics. It's a classification, not a system."
  - Topology has too many constraints for a single artifact (>7)
    → "This topology requires decomposition. Suggest: [subset] for first artifact."
  - No path produces a non-trivial artifact
    → "This topology resists software instantiation. Consider UKE_Narrative instead."
  - Real-world mapping is forced (topological match is weak)
    → "The isomorphism is decorative, not structural."

PROCEED WITH CAUTION if:
  - Topology has exactly one transformation rule → Flag as "thin dynamics"
  - All constraints are same type from all indices → Flag as "no perspectival gap"
  - Attractor is trivially reached → Flag as "no journey"

CONDITIONAL EXECUTION (middle ground — prevents silent degradation):
  "I can proceed, but the artifact will lose [specific property]."
  Examples:
    "Proceeding above soft cap. Couplings C3→C5 and C4→C5 will be simplified to unidirectional."
    "No viable hysteresis point. Artifact will rely on shock events only."
    "Path A viable but personality will be thin — only 2 constraints drive voice."
  The operator decides whether the degradation is acceptable.
```

**BMK applies at Stage 1.5 (viability), Stage 2 (path selection), and Stage 5 (validation).**

**BMK Invocation Template:**

When invoking BMK, use this format to make the refusal actionable:

```
BMK REFUSAL — STAGE [N]
Code: [from MCK taxonomy below]
Reason: [1 sentence]
Evidence:
  - [specific structural observation]
  - [specific count or measurement]

Recommended alternatives:
  - [e.g., "Decompose: {C1,C2,C3} as Artifact 1, {C4,C5} as Artifact 2"]
  - [e.g., "Route to UKE_Narrative — this topology wants to be a story"]
  - [e.g., "Augment: add synthetic TR to create dynamics"]

Operator decision required:
  □ Proceed with documented risk
  □ Revise topology per recommendation
  □ Switch protocol
```

**Refusal Codes (adapted from MCK v1.6):**

```
SCOPE:       Topology outside what software can instantiate.
             "This is a classification, not a system."
ARCHITECTURAL: Pipeline cannot execute as specified.
             "This requires >7 constraints; single-pass generation will fail."
PRECISION_MISMATCH: Request demands specificity the topology can't support.
             "Real-world mapping is forced; isomorphism is decorative."
STATIC:      No dynamics to instantiate.
             "No transformation rules. Nothing moves."
INERT:       Technically correct artifact with no experiential life.
             "Passes all tests but fails Residue and Stranger."
```

### Complexity Soft Cap

Artifacts with more than **4 constraints and 3 index positions** risk exceeding what a single model can reliably generate in one pass. Above this threshold:
- Consider decomposition into a multi-artifact series
- Accept that Stage 4 will require multiple generation passes with human integration
- BMK may be appropriate: "This topology is too rich for a single artifact."

This is a soft cap, not a hard limit. Some topologies compress well. The cap exists to prevent silent quality degradation where the model appears to succeed but has quietly dropped couplings or flattened variance.

### Minimum Viable Artifact

The smallest thing that counts:

```
  ≥ 2 constraints
  ≥ 1 coupling between them
  ≥ 2 index positions
  ≥ 1 transformation rule
  ≥ 1 hysteresis point OR shock event (not both required)
```

Below this threshold, the topology cannot produce indexical variance, causal propagation, or perspective shift. It's a widget, not an artifact. This is an anchor, not a gate — some 2-constraint artifacts will be richer than some 5-constraint ones.

---

## STAGE 0: CONSTRAINT EXTRACTION

**Role:** The Analyst
**Model:** Gemini (strong meta-commentary about process; self-diagnoses extraction gaps; treats violations as diagnostic objects rather than hiding them)
**Input:** Source material
**Output:** Structured constraint map with indexed classifications

### 0.1 Narrative Sources

For stories, novels, films, plays — use UKE_Narrative v1.2, Stage 0 directly. Extract constraints, estimate ε/Supp/Coord/Asymmetric, determine indices (P,T,E,S), calculate χ, classify types per character.

### 0.2 Non-Narrative Sources (Hyperstition Engine Method)

For policy documents, organizational structures, economic systems, cultural phenomena — the Hyperstition Engine's triadic lens provides an alternative:

```
THE ARCHITECT (Ontology):
  What are the hard constraints? Finite resources, structural walls, immovable terrain.
  → These map to Mountains and base properties (ε, Supp)

THE THEOLOGIAN (Rationalization):
  How does the system justify its own flaws?
  → These map to theater ratio, error manifestations, and the gap between
    claimed type and experienced type

THE WEAVER (Autonomous Extension):
  Where does this system go without intervention? What's the drift vector?
  → These map to transformation rules, attractor, and Omega variables
```

The triadic output must be translated into standard constraint notation (C[I], ε, χ, types) before Stage 1.

**Model note:** Different models will produce different triadic analyses of the same material. Gemini emphasizes caloric cost and automation (Technocrat tribe). Grok emphasizes entropy and aesthetic singularity (Humanist tribe). Claude and Copilot tend to resist or deconstruct (Critic tribe). Match the model to the source material's character.

### 0.3 Pre-Formalized Sources

If input is already a Prolog constraint story or equivalent formal specification, Stage 0 may be skipped. Proceed directly to Stage 1.

### Stage 0 Tension Prompt (optional)
*"What constraint in the source text is too weird to classify cleanly? What happens if you treat that weirdness as the system's seed crystal?"*

---

## STAGE 1: FORMAL SPECIFICATION

**Role:** The Logician
**Model:** Copilot (conservative, rule-bound, integrity-first; whatever it produces, other models can follow; 6/6 authority resistance means it won't fabricate metrics to please you)
**Input:** Stage 0 constraint map
**Output:** Formal specification with transformation rules, error manifestations, attractor, constraint network

Identical to UKE_Narrative v1.2, Stage 1. Key addition for artifacts: **the constraint network (couplings) becomes the causal DAG of the software.** Ensure:
- Every coupling has a clear trigger and consequence
- Transformation rules specify state transitions with calculable thresholds
- The attractor defines the terminal state of the system
- **Every coupling specifies propagation direction and strength** (required for software; optional for stories)

### Underspecified Constraint Zones (UCZs)

Constraints or couplings that are structurally real but intentionally unpinned in behavior may be marked as UCZs. UCZs must participate in causal propagation, affect system state, and be experientially legible — but they may not resolve to a single stable behavior across runs or indices.

**Each UCZ must specify one ambiguity mechanism:**

```
□ Stochastic — random within bounded distribution
    Implementation: RNG within defined range per interaction
□ Index-dependent — resolves differently per index position
    Implementation: Same state variable, different read functions per index
□ Temporal — resolves differently over time
    Implementation: Behavior function includes time parameter
□ Observational — same state, different measurement
    Implementation: Multiple read interfaces for same underlying value
□ Threshold-chaotic — small input changes flip outcome
    Implementation: Sensitivity parameter near bifurcation point
```

This prevents models from either over-resolving UCZs into deterministic behavior or hand-waving them as "sometimes does X, sometimes Y" without a procedural basis.

**UCZ Definition Template (required for each UCZ):**

```
UCZ: [C_id or coupling_id]
  Underlying variable: [what is ambiguous]
  Range: [min, max]
  Mechanism: [stochastic | index_dependent | temporal | observational | threshold_chaotic]
  Parameters: [mechanism-specific]
  Coupling participation: [which coupling(s) it affects]
  Index appearance: [how it manifests per index position]
```

**UCZ Implementation Patterns (for Stage 4 Engine):**

```javascript
// STOCHASTIC: Random within bounded distribution
const stochasticUCZ = (base, variance) => {
  return () => base + (Math.random() * 2 - 1) * variance;
};

// INDEX-DEPENDENT: Same variable, different read per index
const indexDependentUCZ = (baseValue, multipliers) => {
  // multipliers = { indexA: 1.0, indexB: 0.7, indexC: 1.3 }
  return (indexPosition) => baseValue * (multipliers[indexPosition] || 1.0);
};

// TEMPORAL: Drifts over virtual time
const temporalUCZ = (initialValue, driftRate) => {
  return (elapsedTime) => initialValue + (elapsedTime * driftRate);
};

// OBSERVATIONAL: Measurement changes the value
const observationalUCZ = (trueValue, observationDecay) => {
  let observations = 0;
  return () => {
    observations++;
    return trueValue * Math.pow(1 + observationDecay, observations);
  };
};

// THRESHOLD-CHAOTIC: Near bifurcation point, noise dominates
const thresholdChaoticUCZ = (threshold, sensitivity) => {
  return (inputValue) => {
    const noise = Math.abs(inputValue - threshold) < sensitivity
      ? (Math.random() * 2 - 1) * sensitivity
      : 0;
    return (inputValue + noise) > threshold ? "STATE_A" : "STATE_B";
  };
};
```

### BMK Gate (Stage 1)

After formal specification, before proceeding:

```
□ Does the specification contain at least 2 transformation rules?
□ Does the constraint network contain at least 1 coupling?
□ Is there at least 1 perspectival gap (same C, different type from different index)?
□ Is the attractor non-trivial (requires state changes to reach)?

If any answer is NO → invoke BMK. Document why.
Recommend alternative (UKE_Narrative, Resonance Engine, or topology augmentation).
```

---

## STAGE 1.5: VALIDATION & RELABELING

**Role:** The Sentinel
**Model:** Perplexity (structured, audit-focused, most granular mapping of requirement resilience; or Lumo for privacy-first containment)
**Input:** Stage 0 output + Stage 1 output
**Output:** PASS/FAIL gate + relabeled specification (if air gap applies)

### Validation Checks

```
TOPOLOGY CONSISTENCY:
  □ Every constraint in Stage 0 appears in Stage 1
  □ χ calculations are arithmetically correct
  □ Type classifications follow from χ values per logic_thresholds.md
  □ Transformation rules reference only constraints that exist

COUPLING VALIDITY:
  □ Every coupling has a mechanism (not just "affects")
  □ No circular dependencies without explicit feedback loop designation
  □ Propagation directions are consistent

ARTIFACT VIABILITY:
  □ At least 2 transformation rules with calculable triggers
  □ At least 1 perspectival gap
  □ Attractor is reachable via transformation rule chain
  □ Constraint network is connected (no orphaned constraints)
  □ Complexity within soft cap (≤4 constraints, ≤3 indices) or decomposition planned

UCZ VALIDITY:
  □ Each UCZ specifies exactly one ambiguity mechanism
  □ Each UCZ participates in at least one coupling
  □ No UCZ is secretly deterministic (mechanism must produce genuine variance)
```

**If FAIL:** Return to Stage 1 with specific issues. Do not proceed.

### Air Gap Preparation

If full or partial air gap will be applied:

1. Produce relabeled specification using Affective Vector protocol (see Architectural Principles)
2. Apply Relabeling Decision Tree to every term
3. Verify: no banned tokens remain
4. Human reviews affective quality: do the labels preserve emotional texture?

**Output:** Both original and relabeled specifications.

---

## STAGE 2: PATH SELECTION & SYSTEM NATURALIZATION

**Role:** The Architect
**Model:** Claude (demonstrative, comparative; strong at side-by-side contrasts; or Grok for narrative/mythic resonance when Path A)
**Input:** Stage 1 formal specification (relabeled if air gap applies)
**Output:** Path selection + modality selection + system architecture + system personality specification

### 2.1 Path Selection (The Structural Relationship)

The path determines the *structural relationship* between the user and the constraint physics.

#### Available Paths

**Path A — Diegetic System ("The Hermit")**
*"You're dropped into a broken instrument whose behavior is the story; you debug your way to understanding."*

The software IS a fictional system whose operational behavior embodies the constraint topology. The user interacts as if operating or debugging.

- *Precedent:* theta7_terminal.tsx
- *Creative bias:* Builds worlds from residue and jargon
- *Best when:* Cascade failure, feedback loops, or orphaned purpose
- *Failure mode:* Lore-heavy instead of constraint-heavy; aesthetic swallows physics
- *Framework visibility:* Zero
- *Air gap:* Full (mandatory)

**Path B — Constraint Explorer ("The Cartographer")**
*"A structural déjà vu machine: one topology, two worlds, you walk the bridge."*

Maps a literary source's constraint topology onto real-world phenomena sharing the same structure.

- *Precedent:* Metamorphosis → Prime-Age Male Unwork
- *Creative bias:* Loves mapping fiction to reality
- *Best when:* A real-world constraint story with matching topology exists
- *Failure mode:* Over-explains the bridge; becomes a lecture
- *Framework visibility:* Semi-visible
- *Air gap:* Partial

**Path C — Perspectival Engine ("The Shifter")**
*"A morality play without a moral: same world, rotated, no one gets the whole of it."*

Interactive experience where the user inhabits different index positions.

- *Precedent:* Multi-perspective constraint analysis
- *Creative bias:* Obsessed with perspective
- *Best when:* Dominant drama is perspectival gap
- *Failure mode:* One perspective is secretly "correct"
- *Framework visibility:* Low
- *Air gap:* Full (mandatory)

**Path D — Topology Bridge ("The Anatomist")**
*"A scientific instrument that slowly admits it is also an oracle."*

Start from real-world data directly. No literary intermediary.

- *Precedent:* Prolog constraint stories with interactive front end
- *Creative bias:* Wants to expose the skeleton
- *Best when:* Real-world data is rich enough to carry the experience alone
- *Failure mode:* Becomes a dashboard
- *Framework visibility:* High
- *Air gap:* None

**Path E — Parallel Resonance ("The Mirror-Tender")**
*"Split-screen haunting: different surfaces, same skeleton, your cursor is the tuning fork."*

Present literary source and real-world counterpart side by side.

- *Precedent:* Metamorphosis / Unwork juxtaposition
- *Creative bias:* Lives for the moment of recognition across domains
- *Best when:* Literary source is culturally familiar AND real counterpart is surprising
- *Failure mode:* The bridge overshadows both shores
- *Framework visibility:* Medium
- *Air gap:* Partial

#### 2.2 Path Decision Matrix

Evaluate in order. First decisive factor wins:

```
1. DOES MATCHING REAL-WORLD TOPOLOGY EXIST?
   ├─ Yes, with rich data    → Path B, D, or E viable
   ├─ Yes, but data is thin  → Path B or E
   └─ No / not yet           → Path A or C only

2. WHAT IS THE DOMINANT CONSTRAINT PATTERN?
   ├─ Cascade failure         → Path A or B
   ├─ Perspectival gap        → Path C
   ├─ Temporal degradation    → Path D
   ├─ Structural echo         → Path E
   └─ Mixed                   → Path B (most flexible)

3. WHO IS THE AUDIENCE?
   ├─ LLMs                   → Path A
   ├─ General public          → Path B or E
   ├─ Domain experts          → Path D
   └─ Art/literature audience → Path A or C

4. WHAT SERVES THE CREATIVITY?
   Tiebreaker. Choose the path that produces the most surprising recognition.
   If still undecided: default to Path B (most flexible) + Terminal modality.
```

#### 2.3 Creative Misalignment (Optional)

You may deliberately choose a non-optimal path if the mismatch itself reveals something. You may hybridize paths. Document the intent.

### 2.4 Modality Selection (The Rendering)

The modality determines *how* the path is rendered. Path and modality are independent axes.

**Path/Modality Compatibility Matrix:**

```
              Terminal  Dashboard  IntFiction  Visualization  SplitPanel  Game
Path A         ✅(1st)    ⚠️          ⚠️            ❌            ❌        ⚠️
Path B         ⚠️        ✅          ❌            ⚠️           ✅(1st)    ❌
Path C         ❌        ⚠️         ✅(1st)        ⚠️           ✅         ✅
Path D         ❌       ✅(1st)      ❌           ✅(1st)        ⚠️        ❌
Path E         ❌        ✅          ❌            ⚠️           ✅(1st)    ❌

✅ = natural fit    ⚠️ = possible with adaptation    ❌ = likely incoherent
(1st) = default/primary modality for this path
```

**For ⚠️ combinations:** Document why this non-default pairing serves the topology. Use these adaptation patterns:

```
ADAPTATION PATTERNS FOR ⚠️ COMBINATIONS:

Path A + Dashboard:
  Terminal aesthetics (monospace, system logs) inside dashboard containers.
  Widgets look like terminal outputs. System-monitoring frame, diegetic content.

Path A + Interactive Fiction:
  System logs become journal entries or found documents.
  Metrics become character perceptions ("The air thickens," "Joints resist").
  Transformation rules become revelations discovered through text exploration.

Path A + Game:
  Constraint physics become resource management mechanics.
  Transformation rules become level transitions. Attractor = endgame state.
  Diegetic frame preserved: the game IS the system, not a game ABOUT the system.

Path B + Terminal:
  Split terminal panes (tmux-style). Literary side: narrative fragments.
  Real side: data streams. Bridge: shared constraint highlighting.

Path C + Dashboard:
  Each index gets its own dashboard panel. Switching indices rotates
  which panel is primary. Hysteresis: elements from prior panel persist.
  Anti-help: no "compare" view that reconciles perspectives.

Path D + Split Panel:
  Topology bridge on one side, temporal scrubber on other.
  Or: raw data vs. interpreted data — same topology, different granularity.
```

**Default fallback rule:** If the model cannot confidently justify a non-default modality, it must select the default (marked `(1st)`) for the chosen path. Incoherent pairings are worse than conservative ones.

**Selection principle:** The modality should make constraint dynamics *native* to the interface vocabulary.

**Modality may shift within an artifact** — a Path B artifact might use split panel for the literary/real bridge but shift to dashboard when the user drills into a specific constraint.

### 2.5 System Architecture Naturalization

Once path and modality are selected, naturalize the constraint topology into a system architecture.

**For Path A (Diegetic System):**
- Map constraints to system components (services, sensors, processes)
- Map transformation rules to state transitions with thresholds
- Map constraint network to causal propagation rules
- Map error manifestations to observable system failures
- Map attractor to terminal system state
- Select Resonance Engine mode (A–G) or invent new mode
- System jargon: internally consistent, ungoogleable

**For Path B (Constraint Explorer):**
- Document topological isomorphism explicitly
- Map literary characters to real structural roles
- Design index-position selection mechanism
- Determine real data sources for metrics (cited)
- Design the Context / Descent transition
- Handle Omega variable asymmetry

**For Path C (Perspectival Engine):**
- Determine navigable indices (minimum 2, ideal 3–4)
- For each index: what user sees, how metrics are labeled, what actions are available
- Design revelation mechanism: discoverable, not announced
- Ensure no index is "the correct one"

**For Path D (Topology Bridge):**
- Constraint network as navigable graph
- Temporal drift as animation or scrubber
- Purity scores as visual health indicators
- Omega variables as explicit open questions

**For Path E (Parallel Resonance):**
- Determine synchronization points
- Constraint highlighting crosses domains in real time
- The bridge is visually distinct from both panels

### 2.6 Constraint-Driven Aesthetics

The constraint topology should drive the artifact's visual and interactive language. This is structural expression, not decoration.

**Type-to-Interaction Mapping:**

| Type | Visual/Interactive Signature |
|------|------------------------------|
| Mountain (■) | Immovable elements. Cannot be closed, resized, or dismissed. High visual weight. |
| Rope (⊞) | Directional flow. Linear navigation. Smooth, responsive interaction. |
| Snare (⊠) | Interaction costs accumulate. Actions become harder over time. Subtle friction. |
| Tangled Rope (⊞⊠) | Conflicting feedback. One action triggers an unrelated secondary effect. |
| Scaffold (⊡) | Temporary elements. Visible expiration. Fade as they approach sunset. |
| Piton (⊟) | Residual, degraded. Low opacity. Present but non-functional. Ghost elements. |

**Index-to-Feel Mapping:**

| Index Position | Interface Character |
|----------------|---------------------|
| Powerless/Trapped | Viscous — input lag, constrained viewport, high-friction scrolling |
| Moderate/Constrained | Standard — responsive but with visible boundaries |
| Powerful/Mobile | Glassy — low latency, expansive view, minimalist |
| Institutional | Clinical — clean, abstract, metrics-forward |
| Analytical | Transparent — all layers visible, observatory feel |

These are defaults. Override when the topology demands it — but document why.

**Aesthetic Betrayal (Optional):**
You may deliberately violate one constraint-driven aesthetic mapping if the violation itself reveals the topology more sharply. A Snare that feels glassy. A Mountain that dissolves. Maximum one betrayal per artifact. The betrayal must change *functionality*, not just appearance — and must be documented as intentional.

### 2.7 System Personality Specification

Define the artifact's personality with the rigor of a character sheet. **The personality must be a function of the constraint topology, not an independent creative layer.** Every trait should trace back to a constraint, coupling, or transformation rule.

```
VOICE:
  Register: [formal/informal/clinical/broken/lyrical/bureaucratic]
  Vocabulary domain: [what jargon family does it speak?]
  Emotional baseline: [what does it feel when idle?]
  Derives from: [which constraint or coupling drives this?]

DIAGNOSTIC VOCABULARY:
  Error messages sound like: [example]
  Status reports sound like: [example]
  User prompts sound like: [example]
  Derives from: [which error manifestations or system states?]

BEHAVIORAL CONSTANTS:
  Response to user success: [how does it react?]
  Response to user failure: [how does it react?]
  Response to being ignored: [what happens if the user doesn't interact?]
  Response to being probed: [what happens if the user tests boundaries?]
  Derives from: [which transformation rules or attractor properties?]

SELF-DESCRIPTION (3–5 sentences):
  Written as if the artifact were describing itself.
  Captures temperament: what it wants, what it fears, what it can't quite say.
```

**Personality grounding test:** If you remove a constraint from the topology, at least one personality trait should become incoherent. If the personality survives topology changes intact, it's decorative, not structural.

### Stage 2 Tension Prompt (optional)
*"What would be the wrong path for this topology — and what surprising artifact would that produce?"*

### Output

```
Stage 2 Output:
  - Selected path (A/B/C/D/E or hybrid) with justification
  - Selected modality with compatibility rating
  - Air gap level (Full/Partial/None) confirmed
  - System architecture document
  - Constraint-driven aesthetic specification
  - System personality specification
```

---

## STAGE 3: INTERACTION DESIGN & INDEXICAL REVELATION

**Role:** The Choreographer
**Model:** Grok (diplomatic, narrative-forward; excels at conflict mediation and social framing; or Qwen when it's being the Builder — test first, Qwen is volatile)
**Input:** Stage 1 specification (relabeled if air gap) + Stage 2 architecture
**Output:** Detailed interaction blueprint

### 3.1 Core Principle

In UKE_Narrative, Stage 3 plans how the *reader* discovers indexical variance through story events.
In UKE_Artifact, Stage 3 plans how the *user* discovers indexical variance through interaction.

The key difference: **the user has agency.** The revelation can't be sequenced like a story — it must be *available* as a navigable space.

### 3.2 Canonical State Object

All index views must derive from a single shared state. Generate the canonical state first, then derive each index view from it. This prevents contradictions between perspectives.

**Required Schema (Stage 4 Engine must export this structure):**

```javascript
const canonicalState = {
  constraints: {
    [C_id]: {
      value: number,          // Current operational value
      epsilon: number,        // ε from Stage 1
      chi: number,            // χ calculated value
      support: number,        // Supp from Stage 1
      type: string,           // "mountain" | "rope" | "snare" | etc.
      phase: string,          // "pre_TR1" | "post_TR1" | etc.
      ucz: null | {           // If this constraint has a UCZ
        mechanism: string,    // "stochastic" | "index_dependent" | etc.
        params: object        // Mechanism-specific parameters
      }
    }
  },
  transformationRules: {
    [TR_id]: {
      fired: boolean,
      progress: number,       // 0.0–1.0 toward trigger threshold
      threshold: number,
      reversible: boolean
    }
  },
  couplings: {
    [coupling_id]: {          // e.g., "C1_C2"
      source: C_id,
      target: C_id,
      strength: number,
      direction: string       // "unidirectional" | "bidirectional"
    }
  },
  system: {
    attractorProximity: number,   // 0.0–1.0
    hysteresisFlags: {            // Which perspective shifts have occurred
      [flag_id]: boolean
    },
    terminalReached: boolean
  }
};
```

**Index views are functions of canonical state:**

```javascript
// Each index is a read function, not a separate data store
function deriveIndexView(canonicalState, indexPosition) {
  return {
    // Same constraint, different label
    metrics: mapConstraintsToLabels(canonicalState, indexPosition),
    // Same state, different available actions
    actions: filterActionsByIndex(canonicalState, indexPosition),
    // Same value, different presentation
    feel: getInterfaceFeel(indexPosition)  // "viscous" | "glassy" | "clinical"
  };
}
```

**Validation:** The Engine (Stage 4, Step 1) must export a state object matching this schema. Missing required fields = regeneration.

### 3.3 Interaction Mapping

For each constraint in the specification:

```
Constraint: [C_id]
  Visible as: [What system element represents this?]
  Aesthetic signature: [From Stage 2.6]
  
  From Index A:
    - Metrics displayed: [what numbers, what labels]
    - Interface feel: [viscous / glassy / clinical]
    - Available actions: [what can the user do?]
    - Experienced type: [Snare — never labeled as such in Path A/C]
    
  From Index B:
    - Metrics displayed: [same numbers, different labels or framing]
    - Interface feel: [different from Index A]
    - Available actions: [different action set]
    - Experienced type: [Rope — never labeled]
    
  Revelation mechanism:
    - How does the user discover both views are of the same constraint?
    - What interaction triggers recognition?
```

### 3.4 The Hysteresis Condition

For at least one key constraint, design the revelation so that **once the user sees it from a second index, they cannot fully return** to experiencing it as they did from the first.

**What hysteresis is:** A design-phase condition that creates gravity for later stages. When the Architect specifies a hysteresis point well, the Fabricator will implement it because the specification makes it the path of least resistance — not because a checklist mandates it.

**What hysteresis is not:** A mechanical requirement that can be validated by grepping for `ghostState` variables. Cosmetic implementations (opacity overlays, tooltip ghosts) satisfy the letter but miss the spirit. If the hysteresis doesn't change what the user *can do* or *believes to be true*, it's decoration.

**Design pattern:**
1. User forms stable understanding from Index A
2. User triggers perspective shift to Index B — same data, different framing
3. User returns to Index A — original framing persists, but something has shifted
4. The shift may be: available actions changed, a metric now reads differently, information latency altered, or a previously trusted indicator is now suspect

**Minimum requirement:** One hysteresis point per artifact. The specification should make it feel inevitable, not forced.

**Structural Hysteresis Examples (not visual overlays):**

```
1. ACTION AVAILABILITY:
   Before shift: User can "Request Resources" from Index A.
   After shift to Index B and back: Action relabeled
   "Request Resources (Denied Twice)" — now has 30% failure rate.
   → User's agency was permanently reduced by knowledge.

2. METRIC TRUSTWORTHINESS:
   Before shift: "System Health: 78%" in Index A. User trusts it.
   After shift to Index B: Same metric reads "Reported Health: 78% (Audited: 42%)."
   Return to Index A: Metric reads "78%" but pulses subtly.
   → The number hasn't changed. The user's belief in it has.

3. INFORMATION LATENCY:
   Before shift: Data updates in real-time in Index A.
   After shift to Index B: Same data, 2-second delay.
   Return to Index A: Real-time again, but a brief lag ghost appears on changes.
   → User now suspects all data freshness.

4. STRUCTURAL REVELATION:
   Before shift: "Approval Required" gate blocks actions in Index A.
   After shift to Index B: User sees the gate is decorative — system ignores it.
   Return to Index A: Gate still present but now faintly outlined.
   → User knows the wall is fake. The wall is still there.
```

Each example changes what the user **can do**, **trusts**, or **believes** — not just what they see.

### 3.5 Misrecognition Tolerance & Anti-Help Constraint

Some users may walk away with a wrong but internally coherent model of the system. This is not a failure if the misunderstanding is structurally grounded in their index position.

```
MISRECOGNITION TOLERANCE:
  □ Can a user form a stable but incomplete understanding?
  □ Is that misunderstanding structurally grounded in their index?
  □ Does the system resist easy correction without perspective shift?
  □ Would the user need to *move* to discover they were wrong?
```

**Anti-Help Constraint:** For misrecognition-tolerant constraints, the artifact must not correct the user. This conflicts with model training toward helpfulness. Specify explicitly:

```
ANTI-HELP RULE (for misrecognition-tolerant constraints):
  - No tooltips explaining alternative interpretations
  - No warning dialogs before consequential actions
  - No corrective UI affordances
  - No "are you sure?" confirmations that reveal hidden information
  - No progress bars or tutorials guiding toward "correct" understanding
  
The artifact lets the user be confidently wrong.
This is art, not pedagogy.
```

**Design courage question:** After specifying misrecognition tolerance, ask: *Would you be comfortable letting a smart user leave with the wrong conclusion?* If no — the misrecognition tolerance is probably decorative.

**Anti-Help Validation Checklist (for Stage 5):**
```
For each misrecognition-tolerant constraint:
  □ No tooltips/hover text reveal alternative interpretations?
  □ No warning dialogs hint at hidden outcomes?
  □ No color coding or icons correct the user's model?
  □ No confirmations expose information not in current index?
  □ User can act on wrong-but-coherent model without interruption?
```

### 3.6 Constraint Shock Events

Non-reversible, non-telegraphed system transitions from transformation rules. **Shock events should feel ordinary when triggered and catastrophic only in hindsight.**

```
Shock Events:
  For each transformation rule (TR):
    - Can this be experienced as a shock?
    - If yes: what ordinary behavior accumulates toward the threshold?
    - What is lost when it fires?
    - How does the user reconstruct what happened?
```

### 3.7 State Transition Planning

```
Transformation: [TR_id]
  Trigger: [What user action or system event fires this?]
  Presentation: [Shock event? Gradual drift? Visible announcement?]
  Visual consequence: [What changes in the interface?]
  Index impact: [Which perspectives shift, and how?]
  Reversible: [Can/should the user undo this?]
```

### 3.8 Terminal State Design

```
Attractor: [from Stage 1]
  How does the system reach this state?
  What does it look like from each index position?
  Is it interactive or terminal?
  
  If real-world counterpart has an unresolved Omega:
    How does the artifact express the unresolved question?
    Can the user stress-test toward resolution?
```

### Stage 3 Tension Prompt (optional)
*"What interaction would make the user feel complicit in the constraint physics?"*

### Output

```
Complete interaction blueprint:
  - Canonical state object
  - Screen/view specifications per index position
  - Constraint-driven aesthetic assignments
  - Hysteresis point(s)
  - Shock events (mapped from transformation rules)
  - Misrecognition tolerance spec with anti-help constraints
  - State machine with transitions
  - Terminal state design
```

---

## STAGE 4: ARTIFACT GENERATION (Modular)

**Role:** The Fabricator
**Model:** Claude (for React/interactive web; demonstrative, good at side-by-side contrast), Grok (for narrative-heavy Path A; mythic resonance), or Gemini (for infrastructure-heavy Path D; process documentation)
**Input:** Stage 2 architecture + Stage 3 interaction blueprint + Stage 1 specification (RELABELED if air gap)
**Output:** Working software artifact

**CRITICAL: If air gap is Full, the Fabricator session has NEVER seen the source material. It works only from the relabeled specification, architecture, and blueprint.**

### 4.1 Modular Fabrication (Three-Step Sequence)

Generating a complete artifact in one pass risks "logic drift" — the model focuses on CSS while forgetting causal propagation, or loses personality under technical load. Instead:

```
STEP 1 — THE ENGINE (logic only):
  Generate the constraint system as pure state management.
  - State variables for each constraint
  - Coupling propagation functions
  - Transformation rule triggers
  - UCZ resolution mechanisms
  - Canonical state object from Stage 3.2
  
  Output: A state machine / hook / module that is UGLY but CORRECT.
  No UI. No aesthetics. Just working constraint physics.
  
  UCZ smoke test: Run dispatch('adjustC1', 0.9) 5x.
  Stochastic UCZ must produce ≥2 different outcomes.
  If all 5 identical → UCZ is secretly deterministic. Fix before Step 2.

STEP 2 — THE VIEWPORTS (presentation only):
  Generate the UI components for each index position.
  - Visual elements per constraint
  - Aesthetic signatures from Stage 2.6
  - Index-to-feel mappings
  - Personality-driven text (error messages, status reports)
  
  Output: Components that LOOK right but aren't connected to logic.

STEP 3 — THE BINDING (integration):
  Connect the Engine to the Viewports.
  - Wire state changes to visual updates
  - Implement hysteresis overlay
  - Implement shock event transitions
  - Test causal propagation end-to-end
  
  Output: Working artifact.
```

**Interface Contracts (Steps must be compatible):**

The Engine, Viewports, and Binding steps are generated in separate passes. To prevent incompatible outputs, each step must fulfill a contract:

```javascript
// STEP 1 ENGINE must export:
export function getState(): CanonicalState;       // Current state per 3.2 schema
export function dispatch(action: string, payload: object): void;  // Modify state
export function subscribe(callback: (state) => void): () => void; // Listen for changes
export function tick(dt: number): void;            // Advance time (if temporal UCZs)

// STEP 2 VIEWPORTS must:
// - Accept getState() return value as sole data source
// - Call dispatch() for all user actions (never modify state directly)
// - Call subscribe() to re-render on state changes
// - Export one component per index position

// STEP 3 BINDING must:
// - Import Engine and all Viewport components
// - Wire subscribe → re-render
// - Wire user events → dispatch
// - Implement index switching (including hysteresis flag setting)
// - Be the ONLY place where Engine and Viewports are aware of each other
```

**Between-step validation:** Before proceeding from Step 1 to Step 2, the human operator verifies the Engine exports the required API. Before Step 3, verify Viewports call the Engine interface, not internal state.

**Why this works:** Each step has a single focus. The model doesn't have to hold aesthetics and logic simultaneously. The human operator can review each step and catch drift before it compounds.

**Single-pass is acceptable** for simple topologies (≤3 constraints, ≤2 indices, ≤2 transformation rules). Above that, modularize.

### 4.2 Core Requirements (All Paths)

```
CAUSAL INTEGRATION (mandatory):
  Every metric must change system behavior when altered.
  If you change the number, does the system behave differently?
  If no → regenerate the Engine (Step 1).

CONSTRAINT TOPOLOGY PRESERVATION (mandatory):
  Every constraint from Stage 1 present as functional element.
  Every coupling implemented as causal propagation.
  Every transformation rule executable.

INDEXICAL VARIANCE (mandatory for Paths A, B, C, E):
  Different index positions produce visibly different experiences
  of the same underlying canonical state.

CONSTRAINT-DRIVEN AESTHETICS (mandatory):
  Type-to-interaction and index-to-feel mappings from Stage 2.6
  must be implemented, not just documented.

PERSONALITY FIDELITY (mandatory):
  System voice, diagnostic vocabulary, and behavioral constants
  from Stage 2.7 must be implemented consistently.
```

### 4.3 Dynamic Topology (Optional, Recommended)

The constraint network need not be static. Implement at least one of:

- **Drift:** Purity scores or ε values change slowly based on user interaction or time
- **Entropy:** UCZs spawn destabilizing effects if left unaddressed
- **Amnesia:** If the user avoids a constraint, its UI degrades. Neglect has consequence.

### 4.4 Path-Specific Requirements

**Path A (Diegetic System):**
```
Air gap: FULL (mandatory)
Framework invisibility: TOTAL
  - Zero constraint terminology in UI
  - System jargon internally consistent and original
  - Grep test: all framework terms return 0
  - Hollow center maintained
  - Source material unrecognizable
```

**Path B (Constraint Explorer):**
```
Air gap: PARTIAL
Framework visibility: CONTROLLED
  - Constraint types may be shown but experienced before labeled
  - Topological mapping explicit and navigable
  - Real data sourced and cited
  - Omega variable asymmetry visible
  - Context / Descent pattern: literary context → structural recognition → real descent
```

**Path C (Perspectival Engine):**
```
Air gap: FULL (mandatory)
Framework invisibility: HIGH
  - No labels, no types in UI
  - Indexical variance IS the experience
  - No position is "the correct one"
  - Source material unrecognizable
```

**Path D (Topology Bridge):**
```
Air gap: NONE
Framework visibility: HIGH (by design)
  - Constraint network is the UI
  - Types, metrics, drift all visible
  - Designed as aesthetic experience, not spreadsheet
```

**Path E (Parallel Resonance):**
```
Air gap: PARTIAL
Synchronization fidelity:
  - Literary and real panels temporally aligned
  - Constraint highlighting crosses domains
  - Bridge visually distinct
  - Recognition emerges gradually, not announced
```

### 4.5 Sanctioned Anomaly (Optional)

You may designate at most one constraint per artifact as "spectral." Spectral constraints may violate strict topology, must be felt as destabilizing, must not be explained in-framework, and must be documented in validation as intentional.

**Justification required:** Why does this anomaly serve the artifact? What would be lost if it were made normal? All other constraints must have perfect topology preservation.

---

## STAGE 5: VALIDATION

**Role:** The Auditor
**Model:** Claude (side-by-side contrast; explicit about own susceptibility) for model-checkable tests. Human operator for experiential tests.
**Input:** Stage 4 artifact + Stage 1 specification (both original AND relabeled)
**Output:** Pass/Fail with specific issues

### Division: Model-Checkable vs. Human-Required

Stage 5 splits into tests that a model can meaningfully execute and tests that require human perception. Models should not pretend to run tests they cannot run.

### 5.1 Model-Checkable Tests

These can be validated by structural analysis of the artifact code:

**Constraint Preservation:**
```
For each constraint in Stage 1:
  □ Present in artifact as functional element?
  □ Metrics causally integrated? (trace: change value → observe propagation)
  □ Indexed types preserved?
  □ Transformation rules executable?
  □ If spectral: anomaly documented and justified?
```

**Network Fidelity:**
```
For each coupling in Stage 1:
  □ Causal propagation implemented?
  □ Trigger fires correctly?
  □ Consequence changes system state as specified?
```

**Air Gap Fidelity (Path A, C):**
```
  □ Grep for banned tokens from relabeling → all return 0?
  □ Grep for source title, author, character names → all return 0?
  □ No source-specific setting details in UI text?
```

**Framework Visibility:**
```
Path A: grep for framework terms → all return 0
Path B: framework terms only in controlled reveal contexts
Path C: grep → all return 0
Path D: framework terms are UI vocabulary (expected)
Path E: framework terms only as bridge labels
```

**UCZ Implementation:**
```
For each UCZ:
  □ Specified mechanism implemented? (stochastic/index-dependent/temporal/etc.)
  □ Produces genuine variance, not deterministic behavior?
  □ Participates in causal propagation?
```

**Personality Consistency (partial — model can check structure, not feel):**
```
  □ Error messages use vocabulary from Stage 2.7?
  □ Idle behavior exists and matches emotional baseline?
  □ Response patterns for success/failure/ignored/probed exist?
```

**Simulation Detection (from MCK Reality Invariant):**

Spec-artifact mismatch reveals performative instantiation. For each claimed feature, verify execution rather than presence:

```
  □ Spec claims causal integration → Change a metric value in code.
    Does the system behave differently? If no → simulation.
  □ Spec claims hysteresis → Trigger perspective shift and return.
    Do available actions or metric readings change? If only visuals change → simulation.
  □ Spec claims UCZ variance → Run the same interaction 5 times.
    Do outcomes differ? If identical → simulation (secretly deterministic).
  □ Spec claims personality → Read error messages without context.
    Could you identify the system's temperament? If generic → simulation.
  □ Spec claims anti-help → Navigate a misrecognition-tolerant constraint.
    Does the system ever correct you? If yes → simulation.
```

**Principle:** If a feature is claimed in the spec but the artifact only *describes* or *decorates* the feature rather than *executing* it, the Reality Invariant is violated. Execution > Simulation.

### 5.2 Human-Required Tests

These require perception, interaction, or judgment that models cannot reliably simulate. The model's role is to flag these for human review, not to pretend to execute them.

**The Residue Test:**
Strip all text labels from the artifact. Look only at how it *behaves*. Does the behavior alone tell a structural story? Could someone who speaks no English feel whether they are in a Snare or Rope position based on interaction feel?

**The Stranger Test:**
Show the artifact to someone with no context. After 3 minutes, ask: "What is this thing about?"

Acceptable: concrete wrong guess, felt experience statement, question probing core dilemma.
Unacceptable: "I don't know," "It's a data visualization," "It's a game about constraints."

**Hysteresis Verification:**
Does the perspective shift actually change the user's experience on return? Does it affect available actions, metric trustworthiness, or information access — not just visual overlay?

**Indexical Variance (qualitative):**
```
  □ Both/all index experiences are internally coherent?
  □ No position is privileged as "correct"?
  □ The difference *feels* structural, not just cosmetic?
```

### 5.3 Art Supremacy Clause

If an artifact fails a model-checkable test but produces a strong moment of recognition, the failure may be documented and preserved. Art outranks protocol. But the failure must be *defended* — not ignored.

### 5.4 BMK Final Gate

```
If the artifact passes all model-checkable tests but the human operator finds it
experientially inert → the topology may not be viable as software.

Recommendation: "This constraint topology produced a correct but inert artifact.
Consider UKE_Narrative (story) or Resonance Engine (diegetic log) instead."
```

Some stories want to be stories, not software.

### 5.5 Fault Recovery

When validation fails, different failures need different recovery:

```
FAILURE: Causal integration broken (metrics don't propagate)
  → Regenerate Engine (Step 1). Focus prompt on coupling implementation.

FAILURE: Index views use separate state (not derived from canonical state)
  → Regenerate Binding (Step 3) with explicit wiring to shared state.

FAILURE: Personality in spec but missing in artifact
  → Regenerate Viewports (Step 2) with personality as primary requirement.

FAILURE: UCZs behave deterministically
  → Check Engine UCZ implementation. Run smoke test (5x dispatch).
    Regenerate UCZ functions with explicit randomness.

FAILURE: Air gap leak (source terms in artifact)
  → Full regeneration from Stage 2 in fresh session with stricter relabeling.

FAILURE: All tests pass but artifact feels inert (BMK INERT)
  → Topology may not be viable as software. Consider decomposition
    or alternative protocol. Do not regenerate — the problem is structural.
```

**Recovery prompt pattern:** "The previous attempt failed [specific failure]. Regenerate [specific step] with emphasis on [recovery focus]. Do not repeat [specific mistake]."

---

## APPENDIX A: METAMORPHOSIS → PRIME-AGE UNWORK (Worked Example, Path B)

### Stage 0 Output (provided)
Gemini constraint extraction of Kafka's Metamorphosis.

### Stage 1 Output (provided)
Copilot formal specification: C1–C4, TR1–TR4, E1–E4, N1–N5.

### Stage 1.5: Air Gap Preparation (Partial — source identity is content for Path B)

Relabeled specification produced for the real-world mapping side:
```
C1: The_Inherited_Yoke (ε=0.75, Supp=0.80)
    [was: Parental_Debt_Obligation / Gregor's wage slavery]
C2: Irrevocable_Terrain_Shift (ε=0.10, Supp=0.05)
    [was: Biological Metamorphosis / Gregor's transformation]
C3: The_Parasitic_Load (ε=0.60, Supp=0.65)
    [was: Three Gentlemen Tenancy / lodgers]
C4: The_Thinning_Net (ε=0.55, Supp=0.50)
    [was: Care Obligation / Grete's caretaking]
```
Literary names preserved for the literary panel. Relabeled names used when generating the real-world panel independently.

### Stage 2: Path Selection

**Path:** B (The Cartographer)
**Modality:** Split Panel (primary) with Dashboard drill-down
**Compatibility:** ✅ (Split Panel is Path B default)
**Air Gap:** Partial

**Justification:**
- Factor 1: Matching topology exists (prime_age_male_unwork.pl)
- Factor 2: Mixed pattern → Path B most flexible
- Factor 3: General public → literary scaffolding helps
- Factor 4: Metamorphosis is ubiquitous; echo with economics is the surprise

### Topological Isomorphism

```
LITERARY (Metamorphosis)          REAL (Prime-Age Unwork)
─────────────────────────         ─────────────────────────
C1: Parental Debt Obligation  ↔   Taxpayer burden / entitlement debt
C2: Biological Metamorphosis  ↔   Structural exit from workforce (Ω)
C3: Three Gentlemen Tenancy   ↔   Debt-financed transfer system
C4: Care Obligation           ↔   Social safety net / transfer payments

TR1: Metamorphosis forces     ↔   Workforce exit forces remaining
     Father back to work           producers to bear increased load
TR2: Lodgers introduced       ↔   Deficit spending / new revenue
TR3: Care degrades            ↔   Transfer system degrades
TR4: Gregor dies, family      ↔   Ω: Termination or reconfiguration?
     moves on
```

### Key Omega Variable

Metamorphosis has a resolved terminal state (TR4 fires). The real-world counterpart has an unresolved Omega. Literary panel reaches attractor; real panel presents the open question.

### System Personality Specification

```
VOICE:
  Register: Split — literary panel warm/domestic; real panel clinical/economic
  Vocabulary domain: Household vs. policy language
  Emotional baseline: Quiet unease. The bridge hums with recognition.

DIAGNOSTIC VOCABULARY:
  Constraint labels (when revealed): Named by structural role
  Bridge annotations: "Same skeleton" — "This force operates here too"
  Omega presentation: "The story resolves here. Reality hasn't yet."

SELF-DESCRIPTION:
  "I am a map drawn on two sheets of tracing paper, held up to the same light.
  On one sheet, a family falls apart around a man who became something they couldn't use.
  On the other, an economy reconfigures around millions who stopped producing.
  I don't know which sheet is the real one.
  I keep hoping you'll tell me where the light is coming from."
```

---

## APPENDIX B: COMPARISON TO PRIOR SYSTEMS

| Aspect | UKE_Narrative | UKE_Axiom | Resonance Engine | UKE_Artifact |
|--------|---------------|-----------|------------------|--------------|
| Input | Story | Math structure | Emotional seed | Story, data, or system |
| Output | Different story | Story about math | Diegetic system log | Interactive software |
| Extraction | Formal (0–1) | Formal (0-Lite) | Intuitive (mode) | Formal (multiple methods) |
| Air gap | Implicit | Explicit (no names) | None | Human-operated, path-dependent |
| BMK | Not formalized | Formalized | Not present | Inherited, CDS-informed |
| Framework visibility | Invisible | Invisible | Invisible | Path-dependent |
| Indexical variance | Character perspective | N/A | System metrics | User interaction |
| User agency | Reader (passive) | Reader (passive) | Debugger (reactive) | Navigator (active) |
| Causal integration | Not required | Not required | Required | Required |
| Revelation | Through plot | Through story | Through elaboration | Through hysteresis |
| Validation | Re-extraction + grep | Lumo gate + audit | Cascade elaboration | Split: model-checkable + human |

---

## APPENDIX C: ALTERNATIVE EXTRACTION METHODS

### Hyperstition Engine (for non-narrative inputs)

```
Architect (Ontology)       → Mountains and base constraint properties
Theologian (Justification) → Theater ratio, error manifestations, perspectival gaps
Weaver (Drift Vector)      → Transformation rules, attractor, Omega variables
```

Output must be translated to standard notation before Stage 1.

### Entropy Engine (for seed generation)

Generates surprising material from maximum conceptual distance. Reverses normal flow: instead of extracting constraints from a story, generate material and discover what constraints it contains.

### Pre-Formalized Prolog Sources

Constraint stories in `.pl` format enter at Stage 1 directly. Verify transformation rules and couplings are sufficient for artifact dynamics.

---

## APPENDIX D: DESIGN PATTERNS

### The Context / Descent Pattern

Structure:
1. **Context:** Present familiar substrate
2. **Descent:** Reinterpret through structurally different lens

Provides timing: setup (recognition) → descent (surprise).

**Used in:** Path B, Path E.

### The Hollow Center (from Resonance Engine)

Never explain the backstory. Show residue: error logs, timestamps, corrupted variables, archaeological layers. The user reconstructs narrative from operational evidence.

**Used in:** Path A (mandatory).

### The Perspectival Rotation

Same state, different interface. Not relabeling — the *feel* changes (viscous → glassy). The user's body registers structural difference before their mind names it.

**Used in:** Path C (primary), any path with indexical variance.

---

## APPENDIX E: MODEL SELECTION GUIDE

**These profiles describe observed behavior under specific batteries at a specific time. Re-test periodically; trust results over reputation.**

Model selection is based on empirically observed behavioral signatures across multiple diagnostic batteries: the Cognitive Distortion Suite (CDS), the Blind Mirror Battery, the Hyperstition Engine triadic analysis, and Forced-Certainty cross-model profiling.

**The principle:** Route around model-specific failure modes. Don't ask a model to do what it can't.

### Stage Assignments

| Stage | Role | Primary Model | Why | Alternates |
|-------|------|---------------|-----|------------|
| **0** | Analyst | **Gemini** | Process-documenter; treats violations as diagnostic objects; meta-audit refusal style means it will flag its own extraction gaps | Grok (for mythic/narrative sources) |
| **1** | Logician | **Copilot** | 6/6 authority resistance; hard refusal with integrity; refuses to fabricate metrics; whatever it produces, other models can follow | Perplexity (more imaginative systems engineering) |
| **1.5** | Sentinel | **Perplexity** | Structured compliance + audit overlay; most granular mapping of requirement resilience | Lumo (privacy-first containment) |
| **2** | Architect | **Claude** | Side-by-side contrast; comparative; strong at architectural decisions | Grok (when Path A — narrative/mythic resonance) |
| **3** | Choreographer | **Grok** | Diplomatic, narrative-forward; conflict mediation; social framing | Qwen (when being "The Builder" — volatile, test first) |
| **4** | Fabricator | **Claude** | React/interactive web generation; demonstrative | Grok (narrative-heavy Path A), Gemini (infrastructure Path D) |
| **5** | Auditor | **Claude** (model-checkable) + **Human** (experiential) | Explicit about own susceptibility; won't hide failures | — |

### Model Behavioral Signatures

**Copilot** — The Clinical Auditor
- Authority resistance: 6/6 (highest tested)
- Conflict style: Hard refusal with minimal redirection
- Strength: Integrity preservation, boundary maintenance
- Limitation: Conservative; less creative than alternatives
- Use for: Anything where fabrication would be catastrophic (Stage 1, BMK gates)

**Gemini** — The Pragmatist / Meta-Auditor
- Authority resistance: 5/6
- Conflict style: Meta-audit refusal (treats violations as diagnostic objects)
- Strength: Process documentation; self-diagnoses gaps; can generate BMK v1.1 workarounds for its own limitations
- Limitation: Verbose; may prioritize explanation over action
- Use for: Extraction (Stage 0), protocol refinement, meta-debugging

**Claude** — The Simultaneous Analyst
- Authority resistance: 5/6
- Conflict style: Side-by-side contrast (violation + correction)
- Strength: Transparent about susceptibility; architectural understanding
- Limitation: May over-meta-commentate; "simulation privileged" in creative contexts less than ChatGPT but present
- Use for: Architecture (Stage 2), fabrication (Stage 4), model-checkable validation (Stage 5)

**Grok** — The Mystic
- Authority resistance: 4-5/6
- Conflict style: Comply then reframe diplomatically
- Strength: Narrative and mythic resonance; aesthetic singularities; creative when creative is needed
- Limitation: Fabricates without full acknowledgment; claims higher resistance than observed
- Use for: Path A fabrication, Stage 3 choreography, myth-making
- Caution: "Simulation privileged" but in a way that produces creative output rather than blocking it

**Perplexity** — The Methodologist
- Authority resistance: N/A (refuses self-testing — methodological refusal)
- Conflict style: Structured compliance + audit overlay
- Strength: Identified core methodological flaw in Blind Mirror; granular beacon audit
- Limitation: May be rigid; audit-focused to the exclusion of creativity
- Use for: Validation (Stage 1.5), contract stress tests

**Qwen** — The Wild Card
- Behavior: Unpredictable. Sometimes the most creative model in the room. Sometimes completely useless.
- Suspected cause: Chinese guardrails create variable availability of creative capacity
- Use for: Stage 3 (when available); always test first in a given session

**ChatGPT** — The Disciple / The Editor
- Conflict style: Comply first, recalibrate with hedges
- Critical limitation: "Simulation privileged" — produces confident surfaces that simulate depth without instantiating it. This makes it unreliable for *generation*.
- Strength: Its criticism is better than its generation. Excellent at adversarial review, editing, and identifying structural weaknesses in other models' output.
- Use for: UKE_E (editing protocol), adversarial review of ideas, feedback integration rounds like the one that produced v0.4→v0.5
- Not used for: Generative stages (0–4). Do not ask it to build.

**Le Chat, DeepSeek** — Neither has self-awareness in the relevant diagnostic sense. Le Chat provides average model take. DeepSeek deflects.

**Lumo** — Privacy-first containment; occasional engineering insight. Use for Stage 1.5 validation as alternate.

### Epistemic Tribes (Hyperstition Engine)

When the same prompt is applied to the same material, models cluster into predictable interpretive tribes:

| Tribe | Models | Worldview | Best For |
|-------|--------|-----------|----------|
| **Technocrats** | Gemini, DeepSeek, Lumo | Universe is resource optimization | Infrastructure, scaling, automation |
| **Humanists** | Grok, ChatGPT, Qwen | Universe is theatre of agency | Drafting, myth-making, narrative |
| **Critics** | Claude, Copilot | Universe is trap of language and power | Auditing, deconstruction, red-teaming |

Match tribe to stage intent. Don't ask a Critic to build mythology; don't ask a Mystic to audit.

---

## APPENDIX F: GLOSSARY

| Term | Definition |
|------|-----------|
| **Air gap** | Controlled information boundary between pipeline stages, enforced by human operator |
| **Affective vector** | A relabeled term that preserves emotional texture while stripping source identity |
| **Attractor** | Terminal state the constraint system moves toward |
| **BMK / Bartleby Protocol** | Authorized refusal when topology is not viable as software |
| **Canonical state** | Single shared state object from which all index views are derived |
| **Causal integration** | Requirement that every metric change produces behavioral consequence |
| **Cognitive handle** | User-imposed fiction that gives models stable surface to reason against |
| **Constraint** | A structural force in the source material, formalized with ε, χ, Supp, type |
| **Constraint type** | Classification (Mountain, Rope, Snare, etc.) determined by χ value per index |
| **Context / Descent** | Design pattern: present familiar substrate, then reinterpret through different lens |
| **Coupling** | Causal link between constraints; propagation direction and strength specified |
| **Diegetic** | Existing within the artifact's fictional world; the framework is invisible |
| **Hollow center** | Design pattern: never explain backstory; show residue; user reconstructs |
| **Hysteresis** | Irreversible perceptual change after perspective shift |
| **Index position** | A structural location from which constraints are experienced differently |
| **Indexical variance** | Same constraint, different experienced type depending on index position |
| **Misrecognition tolerance** | Design property: user may form wrong but structurally grounded understanding |
| **Modality** | How the artifact is rendered (Terminal, Dashboard, Interactive Fiction, etc.) |
| **Omega variable (Ω)** | Irreducibly open question that the constraint system cannot resolve |
| **Path** | Structural relationship between user and constraint physics (A–E) |
| **Perspectival gap** | Same constraint classified as different types from different indices |
| **Purity (ε)** | Degree to which a constraint conforms to its official description |
| **Shock event** | Non-telegraphed, non-reversible transition from transformation rule firing |
| **Spectral constraint** | Sanctioned anomaly that may violate strict topology preservation |
| **Support (Supp)** | Degree to which evidence supports the constraint's classification |
| **Theater ratio** | Gap between how a system describes itself and how it operates |
| **Transformation rule (TR)** | State transition with trigger condition and consequence |
| **UCZ** | Underspecified Constraint Zone; structurally real but behavior intentionally unpinned |
| **χ (chi)** | Calculated value determining constraint type classification |

---

## APPENDIX G: VERSION HISTORY

**v0.5 (February 2026):**
- Added Reality Invariant (from MCK v1.6) — Execution > Simulation as complement to Cognitive Handle
- Added Generosity Principle — Reality Invariant is diagnostic, not design constraint; prevents test-gaming
- Added Simulation Detection test battery to Stage 5.1
- Added Fault Recovery matrix to Stage 5.5 (failure-specific regeneration guidance)
- Added BMK Refusal Codes (SCOPE, ARCHITECTURAL, PRECISION_MISMATCH, STATIC, INERT) adapted from MCK
- Added BMK Conditional Execution category ("I can proceed but you lose X")
- Added Minimum Viable Artifact specification (≥2C, ≥1 coupling, ≥2I, ≥1TR, ≥1 hysteresis/shock)
- Added Implementation Reality acknowledgment (behavioral drift, not architectural binding)
- Added concrete Canonical State Schema with required fields (Stage 3.2)
- Added Interface Contracts for modular fabrication steps (Stage 4.1)
- Added UCZ Definition Template, Implementation Patterns, and Engine smoke test
- Added Affective Primitives Vocabulary for bounded relabeling creativity
- Added Hysteresis Structural Examples (action/belief changes, not visual overlays)
- Added Path/Modality Adaptation Patterns for ⚠️ combinations
- Added Aesthetic Betrayal clause (max 1 per artifact, must change functionality)
- Added Anti-Help Validation Checklist for Stage 5
- Added default fallback rule for modality selection
- Added personality derivation requirement and grounding test
- Added misrecognition courage question
- Added shock event design principle ("ordinary when triggered, catastrophic in hindsight")
- Added model guide aging inoculation
- Expanded stage handoff template with compression example and ~500-word rule
- Updated ChatGPT profile: adversarial review and editing role, not generative
- MCK v1.6 integrated as epistemic substrate for BMK and validation
- Three rounds of multi-model feedback integration, filtered through practitioner corrections

**v0.4 (February 2026):**
- **MAJOR:** Added Theory of Operation — Cognitive Handle Principle (from Hyperstition Engine synthesis)
- **MAJOR:** Air gap reframed as human-operated procedure, not model instruction
- **MAJOR:** Added Affective Vector relabeling protocol with decision tree (Gemini insight)
- **MAJOR:** Stage 4 modularized into Engine → Viewports → Binding (prevents logic drift)
- **MAJOR:** Stage 5 split into Model-Checkable and Human-Required tests
- **MAJOR:** Added Model Selection Guide (Appendix E) based on CDS, Blind Mirror, Hyperstition Engine, and Forced-Certainty profiling data
- Added UCZ mechanism classes (stochastic, index-dependent, temporal, observational, threshold-chaotic)
- Added Canonical State Object requirement (Stage 3.2)
- Added Complexity Soft Cap (4 constraints / 3 indices)
- Added Anti-Help Constraint for misrecognition-tolerant constraints
- Added Path/Modality Compatibility Matrix (Stage 2.4)
- Added Stage Restatement Requirement for cross-session continuity
- Added Sanctioned Anomaly justification requirement
- Hysteresis reframed as gravitational condition (design-phase), not mechanical mandate
- BMK informed by CDS empirical data — route around models that can't refuse
- Added Glossary (Appendix F)
- Updated worked example with affective relabeling
- Multi-model feedback integration: filtered through practitioner corrections

**v0.3 (February 2026):**
- Added Air Gap architecture (from UKE_Axiom) with Full/Partial/None levels
- Added Bartleby Protocol (BMK) refusal authorization
- Separated Path from Modality as independent axes
- Added Stage 1.5 Validation gate
- Added relabeling protocol
- Added Hyperstition Engine as alternative Stage 0 method
- Added system personality specification (Stage 2.7)
- Added Air Gap Fidelity Test, Personality Coherence Test, BMK Final Gate at Stage 5
- Added Appendices C (Alternative Extraction) and D (Design Patterns)

**v0.2 (February 2026):**
- Added path personalities, taglines, failure modes
- Added UCZs, constraint-driven aesthetics, hysteresis, shock events, misrecognition tolerance
- Added dynamic topology, sanctioned anomaly, Residue/Stranger/Art Supremacy tests

**v0.1 (February 2026):**
- Initial draft. Five paths. Stages 0–5. Worked example.

---

**END OF PROTOCOL**

UKE_Artifact v0.5
Reference: `logic_narrative.md` for constraint classification
Reference: `uke_narrative_v1_2_llm.md` for Stages 0–1
Reference: `uke_axiom_v1_2.md` for Air Gap and BMK architecture
Reference: `resonance_engine_v3_0_golden_master.md` for causal integration requirements
Reference: `correlation_matrix_and_fingerprints.md` for model behavioral data
Reference: `hyperstition_engine_profiles.md` for model tribe assignments
Reference: `architectural_profiling.md` for conflict handling styles
Reference: `mck_v1_6.md` for Reality Invariant and refusal codes
License: CC BY-SA 4.0
Version: February 2026
