# Deferential Realism: A Logic of Indexed Constraints

**Version 4.0**  
**Stages 1-6:** Core Indexed Constraint Logic  
**Purpose:** Formal system for reasoning about what binds us, from where we stand  
**Last Updated:** February 2026

---

## Navigation

**This document establishes:** The core indexed constraint logic (Stages 1-6)  

**For navigation by role:** See [logic_index.md](logic_index.md)  
**For canonical threshold values:** See [logic_thresholds.md](logic_thresholds.md)  
**For Stages 7-9 (Boltzmann, Purity, Network):** See [logic_extensions.md](logic_extensions.md)

**Implementation:** Prolog modules (drl_core.pl, signature_detection.pl, drl_lifecycle.pl)  
**Corpus:** ~999 constraints across 35+ domains (expanded from original 691-constraint calibration corpus)

---

## I. Foundation: Why Indexed Constraint-Logic?

### 1. The Central Problem

Traditional logic asks: **Is proposition P true?**

This question assumes truth is universal and context-free. But consider a carbon credit trading system. Four people examine the same constraint:

**The Corporation** views it from index (institutional, generational, arbitrage, global):
- They possess rule-making power and can trade credits across borders
- From their position, the system is a **coordination mechanism** that efficiently allocates emissions reduction
- It costs them less than they extract from the market (χ = -0.132, negative value indicates net benefit)
- Classification: **Rope** (⊞)
- **Note:** Negative χ values indicate the constraint extracts from others while providing advantage to this index

**The Small Business** views it from index (moderate, biographical, constrained, national):
- They have moderate agency but face real compliance costs
- The system both helps (provides framework) and hurts (imposes costs)
- They can't avoid it but it's manageable
- Classification: **Tangled Rope** (⊞⊠)

**The Consumer** views it from index (powerless, biographical, trapped, local):
- They have no control over the system or its implementation
- Prices simply rise; they cannot opt out or influence the mechanism
- They pay but receive no direct benefit
- Classification: **Snare** (⊠)

**The Climate Activist** views it from index (analytical, civilizational, analytical, global):
- They can analyze the system from outside its immediate constraints
- They see the mechanism delays real climate action while creating profit opportunities
- The extraction exceeds the coordination value at civilization scale
- Classification: **Tangled Rope** trending toward **Snare** (⊞⊠ → ⊠)

Which classification is correct?

**All four.**

They're not disagreeing about facts. They're reporting different structural properties of the same constraint, visible from different positions in the system. The corporation genuinely experiences net benefit (extraction < 0 from their power position). The consumer genuinely experiences pure extraction (no exit options). The small business genuinely experiences both coordination and extraction. The activist genuinely sees the long-term extractive tendency.

Traditional logic would demand: pick one truth. But that's the wrong demand. The question isn't "What is the carbon credit system?" The question is "What type of constraint is the carbon credit system **from this index**?"

This is **Indexed Constraint Logic**: a formal system where truth is position-relative but each indexed claim remains objectively verifiable.

### 2. What Indexical Relativity Means

**Not relativism.** Not "truth is whatever you believe."

**Indexed realism:** Truth is position-relative, but each indexed claim is objectively true or false.

Formally:
```
∀I₁, I₂: (Type(C, I₁) ≠ Type(C, I₂)) ∧ (True(Type(C, I₁)) ∧ True(Type(C, I₂)))
```

Multiple valid answers exist because different positions expose different structural properties of the same constraint.

**The analogy:** Four people examine an elephant in darkness:
- One touches the trunk: "It's a snake"
- One touches the leg: "It's a tree"
- One touches the ear: "It's a fan"
- One steps back with a flashlight: "It's an elephant"

In traditional epistemology, three people are wrong. In indexed constraint logic, all four are reporting true structural features from their positions. The person with the flashlight has more information (analytical index), but they're not contradicting the others—they're synthesizing.

**The critical difference:** We're not asking "What does this person believe?" We're asking "What structural properties are accessible from this position?"

Position determines **epistemic access**, not belief. The consumer doesn't merely believe they're trapped—they are trapped (no exit options, no power to modify the system). The corporation doesn't merely believe they benefit—they genuinely extract value (negative χ, net beneficiary position).

### 3. The Core Innovation

Different logical machinery is required:

**Traditional Logic:**
- Preserves: Truth across inference steps
- Validates: Arguments from premises to conclusions
- Evaluates: Soundness (valid + true premises)

**Indexed Constraint Logic:**
- Preserves: **Constraint-type across indexed transformations**
- Validates: **Classification coherence across indexed evidence**
- Evaluates: **Action-consequence alignment relative to power position**

The goal: **Formal system for reasoning about what binds us, from where we stand.**

### 4. Why "Constraint" Not "Proposition"

Constraints are structural features of reality that restrict agency. They have:
- **Extractiveness** (ε): How much they take from participants
- **Suppression** (Supp): Enforcement energy required
- **Coordination function**: Whether they provide genuine value
- **Asymmetry**: Whether costs/benefits distribute unevenly

These properties exist independent of belief. You can believe a constraint doesn't bind you while being objectively trapped by it. You can believe you're trapped while having genuine exit options.

Constraints are **structural**, not propositional. The logic must reflect that.

### 4.5. Why These Four Dimensions?

The index structure I = (P, T, E, S) captures the **minimal sufficient** dimensions for constraint classification:

**Power (P):** Determines your capacity to modify or exit the constraint. A law binds differently if you can lobby to change it (powerful) versus if you're subject to it with no recourse (powerless). Power position directly affects effective extractiveness via π(P).

**Time Horizon (T):** Determines what counts as "changeable." A constitution is a Mountain on biographical timescale but potentially a Rope on civilizational timescale. Without temporal indexing, immutability becomes absolute rather than relative to your planning horizon.

**Exit Options (E):** Determines whether you're genuinely trapped or can route around. Same employment contract: Snare if you're trapped in a company town with no alternatives, Tangled Rope if you have negotiating power from multiple offers. Exit options break the false binary of "free choice" vs "no choice."

**Scope Awareness (S):** Determines verification difficulty and thus hidden extraction. A scam operates differently at local scale (easily verified) versus global scale (Dunbar number prevents verification). Scope scaling via σ(S) captures how constraints change with participant count.

These four dimensions are **not arbitrary**—they're the structural features that empirically determine constraint type. Adding more dimensions (wealth, education, network position) would provide finer resolution but the current four capture 90%+ of classification variance in the 691-constraint corpus.

### 5. The Seven Types: Terrain Metaphor

Constraint-space is terrain you navigate with finite energy. From your position, constraints appear as:

**Mountains (■)**: Unchangeable terrain, route around
- Examples: Thermodynamics, logical necessity, death
- Action: Accept

**Ropes (⊞)**: Coordination tools, maintain
- Examples: Language, UTF-8, traffic signals
- Action: Maintain

**Snares (⊠)**: Traps, cut or exit
- Examples: Debt spirals, predatory contracts
- Action: Cut/Exit

**Tangled Ropes (⊞⊠)**: Coordination with extraction, surgical reform
- Examples: Employment, carbon credits, social media
- Action: Reform (preserve coordination, excise extraction)

**Scaffolds (⊡)**: Temporary supports, sunset when done
- Examples: Training wheels, emergency powers
- Action: Monitor sunset

**Pitons (⊟)**: Failed anchors, bypass
- Examples: QWERTY keyboards, legacy systems
- Action: Bypass

**Naturalized**: Power-scaling ambiguity, investigate_naturalization
- High base extraction (ε > 0.45) but low power-scaled extraction (χ < 0.40)
- Action: `investigate_naturalization` (distinct from generic fallback `investigate` for unknown type)

Same constraint can be different types from different indices. Carbon credits: Rope (institutional) + Tangled (moderate) + Snare (powerless). All true.

### 6. Scope and Methodology

This document establishes:

**§I (This Section):** Why indexed logic is necessary

**§II Basic Syntax:**
- Index structure (Power, Time, Exit, Scope)
- Seven constraint operators with formal definitions
- Power-scaling function π(P)
- Scope modifier σ(S)
- Structural signature predicates

**§III Temporal Logic:**
- Temporal operators (□, ◊, ○, U, S)
- Lifecycle states and transitions
- Eleven drift types (degradation patterns)

**§IV Inference Rules:**
- Priority ordering: Mountain > Snare > Scaffold > Rope > Tangled > Piton > Naturalized
- Classification algorithm
- Two-regime architecture (metrics → signature override)

**§V Error Taxonomy:**
- Six misclassification types
- Consequences and detection

**§VI Decision Logic:**
- Action routing from classification
- Energy accounting
- Purity-qualified extensions (→ logic_extensions.md)

**§VII Containment Logic:**
- Irreducible structural paradoxes
- When resolution impossible, containment required

**§VIII Self-Application:**
- Framework as Scaffold (designed for dissolution)
- Canonical examples (carbon credits, Article 112, UTF-8)
- Known limitations

**§IX Conclusion:**
- What this logic achieves
- Integration with Stages 7-9

### 7. Critical Clarifications

**This framework is:**

**Diagnostic, not prescriptive:** We classify constraints, we don't dictate normative responses. Identifying a Snare doesn't automatically justify cutting it (some are load-bearing). Classification informs action, doesn't determine it.

**Incomplete, not comprehensive:** The seven types cover most constraint-space but not all of it. Edge cases exist. Unknown classification is valid output.

**Provisional, not eternal:** Thresholds are empirically calibrated but subject to revision. The framework itself is a Scaffold—built-in expiration, designed to be replaced when better systems emerge.

**Complementary, not competitive:** Works alongside democratic theory, economics, psychology, sociology. We're adding formal machinery for constraint classification, not replacing existing frameworks.

**Empirically grounded:** 691-constraint corpus, 35+ domains, electoral systems analysis, power modifier calibration from observed extraction patterns. Not armchair philosophy—tested against real constraints.

**Implementation-first:** This is a formal specification document with canonical Prolog implementation. Thresholds are in config.pl, not this document (see logic_thresholds.md for values). Changes flow spec → registry → implementation, never backward.

### 8. What This Enables

**Precision with perspectivalism:** Same constraint, multiple types, all objectively true from their indices.

**Modality with power-scaling:** χ = ε × π(P) × σ(S) grounds classification in empirical measurement while preserving indexical relativity.

**Error-awareness with intentionality:** Built-in error taxonomy distinguishes confusion from deception, system bugs from fraud.

**Action-routing with index-sensitivity:** From classification flows action recommendation, but indexed to your actual structural position, not abstracted universal agent.

**Self-awareness with limitation acknowledgment:** Framework admits its own limits, tracks known issues (logic_thresholds.md §Known Issues), designed for falsification.

**Architectural coherence:** Spec → thresholds → implementation discipline. Single canonical predicate (classify_from_metrics/6). Two-regime system (metrics-first, signature-override). Shadow mode for extensions.

**Structural integrity measurement:** Stages 7-9 add Boltzmann compliance (natural law test), purity scoring (coordination health), network dynamics (contamination propagation). From static classification → dynamic structural physics.

### 9. Reading This Document

**Triple-layer format:** Each operator presented as:
1. **Conceptual overview** (intuitive explanation, examples)
2. **Formal definition** (mathematical/logical specification)
3. **Implementation** (Prolog predicate reference)

**Structural gates:** At key definitions (Mountain, Rope, Snare), **mathematical tests prevent constraints from masquerading as natural laws or pure coordination**. These gates use Boltzmann compliance (Stage 7) to detect physics-washing and coordination-washing. See callout boxes in §II.B and details in logic_extensions.md.

**Canonical thresholds:** All parameter values reference logic_thresholds.md. No hardcoded numbers in this document.

**Cross-references:** Clear pointers to related concepts, extensions, implementation details.

**Stakeholder paths:** See logic_index.md for reading recommendations by role (theorist, implementer, pipeline consumer, calibration team).

### 10. A Note on Terminology

**"Deferential" Realism:**

Not "differential" (though we do differentiate by index). **Deferential** as in "showing deference."

The framework defers to:
- **Structural position**: Your index determines what's accessible
- **Empirical measurement**: Metrics ground classification
- **Lived experience**: Powerless agents know their constraints
- **Analytical distance**: But also honors outside perspective
- **Reality's structure**: Constraints exist independent of belief

We defer to what **is**, indexed by **where you stand**.

Not relativism (anything goes). Not absolutism (one truth for all). **Indexed realism:** Position-relative objectivity.

---

**With foundation established, we proceed to formal definitions.**

---

## II. Basic Syntax: Indexed Constraint Operators

### A. Index Structure

An **index** specifies the structural position from which a constraint is experienced. It is a four-tuple:

**Formal Definition:**
```
I ::= (P, T, E, S)

where:
  P ∈ {powerless, moderate, powerful, organized, institutional, analytical}
  T ∈ {immediate, biographical, generational, historical, civilizational}
  E ∈ {trapped, constrained, mobile, arbitrage, analytical}
  S ∈ {local, regional, national, continental, global, universal}
```

Each dimension captures a distinct structural feature:

---

#### **P: Power Position**

Your capacity to influence, exit, or modify the constraint.

**powerless** — No control over entry, exit, or enforcement
- Example: Minimum wage worker facing non-compete clause
- Cannot negotiate, cannot exit without losing livelihood
- Bears full extraction (π = 1.5, amplification)

**moderate** — Average agency, some choices available
- Example: Mid-level employee with transferable skills
- Can negotiate within limits, can exit at moderate cost
- Baseline extraction (π = 1.0)

**powerful** — Can deflect costs, influence rule-making
- Example: Executive with board seat
- Shapes policy, exits without penalty, arbitrage opportunities
- Reduced extraction (π = 0.6)

**organized** — Collective action, shared burden
- Example: Union members with collective bargaining
- Coordinated resistance distributes costs
- Shared burden (π = 0.4)

**institutional** — Rule-making position, net beneficiary
- Example: Legislature member, regulatory capture beneficiary
- Creates rules that extract from others
- Net extraction FROM system (π = -0.2, negative = benefits exceed costs)

**analytical** — Analytical distance, sees through normalization
- Example: Academic researcher, systems analyst
- Not subject to everyday pressures that normalize extraction
- Detects extraction moderate agents normalize (π = 1.15, degeneracy-breaking)

**Key insight on π(analytical) = 1.15:**

This value was calibrated to break moderate-analytical degeneracy. At π = 1.0, analytical and moderate perspectives produced identical χ values, making them indistinguishable. At π = 1.15, **93 constraints in the corpus** show the critical pattern: "only analyst catches snare." These are constraints where moderate agents have normalized the extraction (χ = 0.55, Tangled Rope) but analytical perspective detects it as extractive (χ = 0.63, near Snare threshold).

The analytical modifier is not about "smarter people see more." It's about **freedom from normalization pressure**. The analyst isn't subject to the daily constraints that make moderate agents rationalize extraction as coordination.

---

#### **T: Time Horizon**

The temporal scale over which you can plan and act.

**immediate** — Days to weeks
- Example: Gig worker living paycheck to paycheck
- Only immediate survival matters
- Long-term constraints appear as Mountains (no time to change them)

**biographical** — Lifetime scale (decades)
- Example: Professional planning career trajectory
- Retirement, children's education, legacy
- Some constraints changeable within lifetime

**generational** — Children's and grandchildren's lifetimes
- Example: Parent concerned with climate, institutions
- Sees constraints as potentially changeable across generations
- More constraints become Ropes (modifiable with sustained effort)

**historical** — Centuries
- Example: Historian studying institutional evolution
- Sees constitutions, empires rise and fall
- Most constraints appear changeable

**civilizational** — Millennia
- Example: Anthropologist, philosopher of deep time
- Even "human nature" becomes contingent
- Only fundamental physics remains Mountain

**Critical effect:** Time horizon determines **immutability gate**. A constitutional constraint is Mountain from biographical perspective (can't change it in your lifetime) but Rope from civilizational perspective (constitutions are revised, replaced, transcended).

---

#### **E: Exit Options**

Your practical ability to route around or escape the constraint.

**trapped** — Physically or economically cannot leave
- Example: Prisoner, indentured worker, resident of company town
- No alternatives, no negotiating power
- Full constraint force

**identity_locked** — Structurally mobile but cognitively/identity-fused
- Example: Captured regulator fused with industry identity, trauma-bonded partner, ideologically committed state
- Exit would require becoming a different person, not just paying a cost
- Binding is cognitive (identity fusion, epistemic closure) rather than material
- Hub 2: mountain at immediate, rope at biographical and beyond (perceptual filter, not structural immobility)
- Phase transition at biographical horizon — see effective_immutability table below

**Effective Immutability by Exit Option × Time Horizon:**

| Exit Option | Immediate | Biographical | Generational | Historical | Civilizational |
|-------------|-----------|--------------|--------------|------------|----------------|
| trapped | mountain | mountain | mountain | rope | rope |
| **identity_locked** | **mountain** | **rope** | **rope** | **rope** | **rope** |
| constrained | mountain | mountain | rope | rope | rope |
| mobile | rope | rope | rope | rope | rope |
| arbitrage | rope | rope | rope | rope | rope |
| analytical | — | — | — | — | mountain ∧ rope |

`identity_locked` is the only exit option that flips at the biographical boundary (1–50 years). The comment in `constraint_indexing.pl:179` explains: "Perceptual filter, not structural immobility." Identity fusion prevents exit within 1 year (mountain), but identity can be reconstructed over a lifetime (rope). Compare `trapped`, which remains mountain through generational timescales — physical/economic barriers resist individual lifespans. The `analytical` row at civilizational is non-deterministic by design (both mountain and rope succeed; metric gates determine which fires first).

**Implementation:** `effective_immutability/3` in `constraint_indexing.pl:172-205`; wrapper `effective_immutability_for_context/2` at line 207.

**constrained** — Can exit but at high cost
- Example: Professional with golden handcuffs, homeowner in declining market
- Exit possible but destroys accumulated capital
- Partial constraint force

**mobile** — Multiple alternatives available
- Example: In-demand specialist, remote worker
- Can switch without major penalty
- Reduced constraint force

**arbitrage** — Can play alternatives against each other
- Example: Recruited executive with competing offers
- Uses alternatives as leverage
- Minimal constraint force, possibly net benefit

**analytical** — Can analyze from outside the constraint
- Example: Researcher studying the system without being subject to it
- Epistemic but not practical exit
- Sees extraction others normalize

**Interaction with Power:** Exit and Power often correlate but diverge. A PhD student has analytical exit (can critique the system) but may be practically trapped (years invested, limited outside options). A wealthy retiree has mobile exit but powerless position (can leave but can't change the system).

---

#### **S: Scope Awareness**

The scale at which you experience or can verify the constraint.

**local** — Neighborhood, town, face-to-face community
- Example: Small town resident
- Direct verification possible (know the players, see the effects)
- Extraction dampened by transparency (σ = 0.8)

**regional** — State, province, known cultural region
- Example: State-level political engagement
- Verification harder but still feasible
- Slight amplification (σ = 0.9)

**national** — Country-wide
- Example: National citizen with media access
- Verification requires institutional mediators
- Baseline (σ = 1.0)

**continental** — Multi-country region
- Example: EU policy observer
- Verification difficult, multiple languages/systems
- Amplification (σ = 1.1)

**global** — Planetary scale
- Example: Multinational corporation, global activist
- Dunbar number makes verification nearly impossible
- Maximum amplification (σ = 1.2)

**universal** — Abstract, mathematical, scope-independent
- Example: Mathematician, physicist
- Natural laws don't change with scale
- Neutral (σ = 1.0)

**Why σ(universal) = 1.0:**

Natural laws are scope-invariant. Thermodynamics operates identically at local and global scales. The universal modifier returns to baseline because genuine natural laws don't hide extraction behind verification difficulty—there's no extraction to hide.

**Why σ(global) = 1.2:**

Global scope maximally amplifies extraction through verification difficulty. With 8 billion participants, direct verification is impossible. Claims about "global markets," "universal standards," or "planetary systems" cannot be individually verified. This verification gap creates space for hidden extraction. The 1.2 multiplier reflects empirically observed extraction amplification in global-scale constraints.

---

#### **Indexed Classification Notation**

We write:
```
C[I] — constraint C experienced from index I
Type(C[I]) ∈ {■, ⊞, ⊠, ⊞⊠, ⊡, ⊟} — classification from index I
```

**Examples:**
```
carbon_credits[(powerless, biographical, trapped, local)] = ⊠ (Snare)
carbon_credits[(institutional, generational, arbitrage, global)] = ⊞ (Rope)
carbon_credits[(moderate, biographical, constrained, national)] = ⊞⊠ (Tangled Rope)

All three classifications are simultaneously true.
```

---

#### **Valid Index Combinations**

Not all 6×5×5×6 = 900 combinations are coherent. Some constraints:

**Mutual entailment:**
- `analytical` power tends to imply `analytical` exit (epistemic freedom)
- `trapped` exit tends to imply `powerless` or at most `moderate` power
- `universal` scope tends to imply `civilizational` time horizon

**Incoherent combinations:**
- `(institutional, immediate, trapped, local)` — institutional power + trapped exit is contradictory
- `(powerless, civilizational, arbitrage, local)` — powerless position with arbitrage options is unstable

**Implementation:**

The `valid_context/1` predicate in constraint_indexing.pl validates dimension values:
```prolog
% constraint_indexing.pl
valid_context(context(
    agent_power(P),
    time_horizon(T),
    exit_options(E),
    spatial_scope(S)
)) :-
    agent_power(P),
    time_horizon(T),
    exit_options(E),
    spatial_scope(S).
```

**Note:** Coherence checks (e.g., `incoherent_power_exit/2`, `incoherent_time_scope/2`) are not currently enforced. Any valid dimension combination is accepted. Context terms use tagged wrappers (`agent_power(P)`, etc.) rather than bare atoms. The `identity_locked` exit option is coherent with all power levels including `analytical` (an analyst can recognize their own identity lock while being unable to break it — this instantiates the Classical Oracle Gap, Theorem 4).

In practice, most real indices cluster around:
- `(powerless/moderate, biographical, constrained, national)` — typical citizen
- `(powerful, generational, mobile, global)` — elite perspective
- `(analytical, civilizational, analytical, universal)` — researcher perspective

---

#### **Why Four Dimensions Are Sufficient**

Could we add wealth, education, network position, race, gender?

**Yes, but diminishing returns.** The current four dimensions capture **~90% of classification variance** in the 691-constraint corpus. Additional dimensions would provide finer resolution but at cost of:
- Combinatorial explosion (already 900 possible indices)
- Decreased interpretability
- Higher calibration requirements

The four dimensions (P, T, E, S) represent **empirically validated minimum sufficient set** for constraint classification. They're not the only possible dimensions, but they're the ones that matter most.

---

**[§II.A COMPLETE]**

**Integration check:**
- ✅ Full four-dimensional index defined
- ✅ Each dimension explained with examples
- ✅ π(analytical) = 1.15 rationale provided
- ✅ σ values justified (verification difficulty)
- ✅ Index notation established
- ✅ Coherence constraints noted
- ✅ Implementation reference included

**Next:** §II.B Core Modal Operators (Mountain, Rope, Snare with full triple-layer + structural gates)

---

### B. Core Modal Operators (Indexed)

The seven constraint types form the core classification system. Each type has distinct structural properties and action implications.

**Priority ordering in classify_from_metrics/6:**
```
Mountain > Snare > Scaffold > Rope > Tangled Rope > Piton > Naturalized > unknown
```

This ordering is implemented as a sequence of guards in drl_core.pl. The first type whose conditions are satisfied becomes the classification.

---

#### B.1 Mountain Operator: ■C[I]

##### Conceptual Overview

A **Mountain** is unchangeable terrain. Not "hard to change" but genuinely immutable from your structural position and time horizon.

The name comes from mountaineering: you don't move a mountain, you route around it. Mountains are features of reality that constrain all possible paths forward. You must accept their existence and plan accordingly.

**Examples of true Mountains:**
- **Second Law of Thermodynamics**: Entropy increases. True from all indices, all scopes, all time horizons.
- **Computational complexity bounds**: P ≠ NP (assuming conjecture true). No amount of power changes this.
- **Dunbar Number (~150)**: Cognitive limit on stable relationships. Scales across human populations.
- **Death**: From most biographical indices, genuinely unchangeable (though from civilizational + analytical index, aging may become modifiable).

**Examples of False Mountains (physics-washing):**
- **"Market forces are natural laws"**: Claims inevitability, but classification varies by Power×Scope (only binds powerless at global scale).
- **"Human nature is competitive"**: Claims universality, but cooperation levels vary dramatically by scope and cultural context.
- **"Scarcity is fundamental"**: Claims natural necessity, but often reflects constructed property systems rather than physical limits.

**The critical distinction:** True Mountains have **zero or minimal extraction** (ε ≤ 0.25) and **zero or minimal suppression** (Supp ≤ 0.05). If a constraint requires active enforcement or extracts heavily from participants, it's not a natural law—it's a constructed constraint claiming inevitability.

**Why such strict thresholds?**

Because calling something a Mountain has massive consequences. It shifts from "reform or resist" to "accept and route around." The classification must be bulletproof. We tolerate no false positives—better to misclassify a Mountain as Rope than to let a Snare masquerade as natural law.

##### Formal Definition

```
■C[I] ↔ ε(C) ≤ 0.25 ∧ Supp(C) ≤ 0.05 ∧ NaturalEmergence(C) ∧ Immutable(C, I.T, I.E)
```

**Components:**

- **ε(C) ≤ 0.25**: Base extractiveness at coordination floor
  - Natural laws don't extract—they simply constrain what's possible
  - The 0.25 threshold allows for minimal necessary coordination overhead
  - Reference: logic_thresholds.md §3a, `mountain_extractiveness_max`

- **Supp(C) ≤ 0.05**: Suppression at noise floor
  - Natural laws don't require enforcement—they're inescapable
  - The 0.05 threshold is noise floor (measurement error, edge cases)
  - Reference: logic_thresholds.md §3a, `mountain_suppression_ceiling`

- **NaturalEmergence(C)**: Constraint emerges without active human construction
  - Checked via `emerges_naturally(C)` predicate (delegated to `domain_priors`)
  - Mountains must arise from natural processes, not deliberate design
  - Without this gate, well-designed low-extraction immutable institutions could be misclassified as natural laws
  - This is a silent exclusion gate: a constraint satisfying all metric thresholds *and* immutability will still fail Mountain classification if it was actively constructed

- **Immutable(C, I.T, I.E)**: Unchangeable from time horizon T and exit options E
  - Checked via `effective_immutability_for_context(Context, mountain)` predicate
  - Time horizon matters: constitutions are Mountains from biographical perspective but Ropes from civilizational
  - Exit options matter: analytical perspective can sometimes transcend constraints that bind others

**Immutability Gate Logic:**

```
Immutable(C, T, E) ≡ effective_immutability_for_context(Context, mountain)
```

Truth conditions (from constraint_indexing.pl):
- **(civilizational, analytical)**: Can classify as Mountain OR Rope (analytical perspective sees potential change)
- **(generational+, mobile+)**: Likely Rope (time + exit options enable change)
- **(biographical, trapped)**: Mountain if other criteria met
- **Otherwise**: Mountain classification possible

**Power-Scaling Note:**

Mountains **don't check χ** (power-scaled extraction). They use raw ε and Supp directly. Why? Because natural laws affect everyone equally. Thermodynamics doesn't care about your power position. If a constraint's burden varies dramatically by power, it's not a Mountain—it's constructed.

##### Implementation

```prolog
% drl_core.pl, classify_from_metrics/6
classify_from_metrics(C, BaseEps, _Chi, Supp, Context, mountain) :-
    config:param(mountain_suppression_ceiling, SuppCeil),
    Supp =< SuppCeil,
    config:param(mountain_extractiveness_max, MaxX),
    BaseEps =< MaxX,
    emerges_naturally(C),
    constraint_indexing:effective_immutability_for_context(Context, mountain), !.
```

**Key observation:** The cut (!) means if Mountain classification succeeds, we don't check other types. Mountain has highest priority.

**Canonical Thresholds** (from logic_thresholds.md §3a):
- `mountain_extractiveness_max` = **0.25**
- `mountain_suppression_ceiling` = **0.05**
- `mountain_extractiveness_min` = **0.0** (theoretical minimum, unused in practice)

**Priority:** Mountain > all other types (checked first in classify_from_metrics/6)

---

**📌 STRUCTURAL GATE — Boltzmann Compliance for Natural Law:**

> **Shadow Mode Reminder:** This gate runs AFTER metric-based classification in `classify_from_metrics/6`. It does not modify the core classification logic—it operates in the signature override layer (`signature_detection.pl`) and can demote a claimed Mountain to Constructed Constraint based on coupling topology.
>
> **Before a constraint is accepted as a Mountain (■ signature), it must pass the Boltzmann Independence Test.** 
>
> Natural laws do not "senselessly couple" unrelated index dimensions. If a constraint's classification depends on the product of Power and Scope in a non-factorizable way, it is **constructed**, not natural—regardless of how low its ε and Supp values are.
>
> **Note:** Boltzmann compliance is **necessary but not sufficient** for Natural Law classification. A constraint must ALSO meet the Mountain thresholds (ε ≤ 0.25, Supp ≤ 0.05) AND show temporal stability AND have zero beneficiaries. Boltzmann compliance alone doesn't make something a Mountain—it just prevents false Mountains from passing as natural laws.
>
> **The Test:**
>
> Does classification factorize across Power × Scope dimensions?
> ```
> χ(P, S) ≈ f(P) × g(S)  [within tolerance 0.10]
> ```
>
> If the effective extractiveness can be separated into independent power and scope functions, the constraint passes. If Power and Scope couple nonsensically (changing scope flips classification at one power level but not others), the constraint **fails Boltzmann compliance** and is reclassified as **Constructed**.
>
> **This is the formal test for "physics-washing"**: claiming unchangeability by invoking nature, when the coupling topology reveals human construction.
>
> **Critical: FNL Detection is Signature Override, Not Audit.**
>
> When False Natural Law (FNL) is detected, it's not merely flagged for review—it **overrides the metric-based classification**. A constraint that passes Mountain thresholds (ε ≤ 0.25, Supp ≤ 0.05) but fails Boltzmann compliance is **demoted** from "Universal Necessity" (Mountain) to "Asymmetric Construction" (Tangled Rope). This happens in signature_detection.pl, which runs after drl_core.pl's metric classification.
>
> **Note: Boltzmann compliance is necessary but not sufficient for Natural Law (NL) signature.** A constraint can factorize perfectly and still not be natural—it could be a well-designed coordination standard (CS). The full NL signature requires: Boltzmann compliance AND no viable alternatives AND zero beneficiaries AND temporal stability. See logic_extensions.md §1.2 for complete NL signature requirements.
>
> **Shadow Mode Reminder:** These gates do NOT modify `classify_from_metrics/6` (the canonical predicate in drl_core.pl). They operate in the signature override layer (signature_detection.pl) which runs afterward. The core classification logic remains unchanged; signatures can only enhance or override, not replace.
>
> **Example of Boltzmann failure:**
> ```
> Claimed Mountain: "Competition is human nature"
> 
> Test at (powerless, local): ε = 0.12, Supp = 0.04 → Mountain ✓
> Test at (powerless, global): ε = 0.68, Supp = 0.65 → Snare ✗
> Test at (institutional, local): ε = 0.08, Supp = 0.03 → Mountain ✓
> Test at (institutional, global): ε = 0.02, Supp = 0.01 → Mountain ✓
> 
> Coupling detected: Power×Scope interaction.
> Only powerless agents at global scale experience high extraction.
> This is not a natural law—it's a constructed system.
> 
> Boltzmann compliance: FAIL
> Reclassification: False Natural Law (FNL) → Tangled Rope
> ```
>
> **Why this matters:**
>
> Physics-washing is a common rhetorical move: "This constraint is just how things are, like gravity." The Boltzmann test mathematically refutes this claim. Natural laws factorize cleanly across independent dimensions. Constructed constraints couple dimensions to hide extraction.
>
> **See:**
> - logic_extensions.md §1.3: Boltzmann Compliance Test (full algorithm)
> - logic_extensions.md §1.4: False Natural Law (FNL) Detection
> - logic_extensions.md §1.7: Nonsensical Coupling
>
> **Implementation:** signature_detection.pl + boltzmann_compliance.pl, `boltzmann_compliant/2`, `false_natural_law/2`

---

**Detection Pattern: False Mountain (FM)**

Even without full Boltzmann testing, we can detect obvious False Mountains:

```
FM(C) ↔ Claimed(■C) ∧ ∃I(¬■C[I]) ∧ ε(C) > 0.70
```

If someone claims a constraint is a Mountain but:
- It has ε > 0.70 (high extraction), AND
- From some index it's clearly NOT a Mountain (e.g., Snare from powerless perspective)

Then we have a False Mountain claim. The high enforcement contradicts natural emergence.

**Examples:**
- "Poverty is inevitable" (ε = 0.75, Supp = 0.80) — high enforcement reveals construction
- "Markets naturally produce inequality" (ε varies 0.15-0.85 by power position) — coupling reveals construction

**Action Implication:**

**Accept** → Route strategy around what cannot be changed.

When facing a true Mountain:
- Don't waste energy trying to move it
- Find paths around or over it
- Adapt strategy to terrain constraints
- Accept what genuinely cannot be changed

**Critical distinction: Accept ≠ Resignation.**

Accepting a Mountain is a **strategic move to conserve energy** for changeability elsewhere. It's not defeatism—it's tactical resource allocation. You acknowledge unchangeable constraints so you can focus energy on changeable ones (Ropes, Tangled Ropes, Snares).

A mountaineer doesn't "resign" to the mountain's existence—they route intelligently around terrain. Similarly, you don't "resign" to thermodynamics—you work within physical constraints to achieve goals.

**But verify it's truly a Mountain first.** Most constraints claiming Mountain status are False Summits—constructed constraints with good PR.

---

**Examples in Practice:**

**True Mountain: Second Law of Thermodynamics**
```
ε(entropy_increase) = 0.0 (no extraction—just physical constraint)
Supp(entropy_increase) = 0.0 (no enforcement needed)
Immutable(entropy_increase, all_T, all_E) = true

Boltzmann test:
  All indices → Mountain
  All scopes → Mountain  
  Factorizes perfectly (no coupling)
  
Classification: ■ from all indices ✓
```

**False Mountain: "Natural monopolies in utilities"**
```
ε(utility_monopoly) = 0.45 (moderate extraction)
Supp(utility_monopoly) = 0.55 (active regulatory protection)

From (powerless, biographical, trapped, local):
  χ = 0.45 × 1.5 × 0.8 = 0.54 → Tangled Rope

From (institutional, generational, arbitrage, national):
  χ = 0.45 × -0.2 × 1.0 = -0.09 → Rope (net benefit)

Boltzmann test:
  Coupling detected (Power×Scope interaction)
  Classification varies by index
  Fails factorization test
  
FNL detected: Claimed natural but constructed ✗
Reclassification: Tangled Rope (conservative fallback)
```

**Edge Case: Dunbar Number (~150 stable relationships)**
```
ε(dunbar_limit) = 0.08 (minimal coordination overhead)
Supp(dunbar_limit) = 0.02 (no enforcement—just cognitive limit)

From (powerless, biographical, trapped, local):
  Cannot maintain >150 stable relationships
  → Mountain ✓

From (institutional, generational, mobile, global):
  Still cannot maintain >150 stable relationships
  (Power doesn't change cognitive architecture)
  → Mountain ✓

From (analytical, civilizational, analytical, universal):
  Could potentially be transcended via technology (AI-assisted relationships)
  But on civilizational timescale, not biographical
  → Mountain at biographical scale, Rope at civilizational ✓

Boltzmann test:
  Factorizes cleanly (no Power×Scope coupling)
  Same limit across all power positions
  Scope-invariant (local and global both ~150)
  
Classification: ■ from most indices ✓
Note: May become Rope from (analytical, civilizational) as technology evolves
```

---

**Summary: Mountain Operator**

**Use when:** Constraint genuinely unchangeable from your index (ε ≤ 0.25, Supp ≤ 0.05, immutable)

**Verify with:** Boltzmann compliance test (factorization across Power×Scope)

**Beware:** Physics-washing (constructed constraints claiming natural inevitability)

**Action:** Accept and route around—don't waste energy fighting what cannot change

**Next operator:** Rope (⊞) — genuine coordination mechanisms

---

#### B.2 Rope Operator: ⊞C[I]

**Conceptual Overview:**

A **Rope** is a coordination tool. It binds people together for mutual benefit. Language. Standards. Contracts. Shared protocols.

The metaphor: climbers rope together for safety. Each person benefits from the connection. The rope constrains individual freedom (can't wander off alone) but provides collective benefit (don't fall to death).

From your index I, a Rope provides **genuine coordination value while extracting minimally**. You benefit more than you pay. The constraint helps you navigate terrain more safely.

**Examples of true Ropes:**
- **UTF-8 encoding**: Universal text representation. Minimal learning cost (ε ≈ 0.02), massive coordination benefit.
- **Metric system**: Shared measurement. Learning cost exists but dwarfed by interoperability gains.
- **Traffic signals**: Red/yellow/green. Small time cost (ε ≈ 0.05), prevents collisions.
- **Handshake protocol**: Social coordination. Nearly zero extraction, genuine signaling value.

**Examples of Rope-like constraints that fail structural tests:**
- **"Free" social media platforms**: Appear as Rope (low monetary cost, voluntary), but fail Boltzmann compliance (couple User Power × Network Scope nonsensically).
- **Opt-out privacy defaults**: Low ε but nonsensical coupling (why does privacy level depend on user sophistication?).
- **Behavioral defaults with high switching costs**: Coordination-washing—extraction hidden in friction.

**The critical distinction:** True Ropes have **dual threshold protection**:
1. Low power-scaled extraction: χ ≤ 0.35
2. Low base extraction: ε ≤ 0.45

**Why dual thresholds?**

We check BOTH χ (power-scaled) AND ε (base). This prevents powerful agents from misclassifying high-ε constraints as Ropes just because THEY don't feel the extraction.

**Example:**
```
Constraint X has ε = 0.60 (high base extraction)

From (institutional, generational, arbitrage, global):
  χ = 0.60 × -0.2 × 1.2 = -0.144
  → Net benefit! Appears as Rope from institutional view
  
BUT ε = 0.60 fails rope_epsilon_ceiling (0.45)
→ NOT a Rope, even from institutional index
→ Prevents powerful from reclassifying extraction as coordination
```

The dual threshold ensures: "If it extracts heavily from ANYONE, it's not a Rope."

**Formal Definition:**

```
⊞C[I] ↔ χ(C, I.P, I.S) ≤ 0.35 ∧ (χ ≤ 0 → ⤠| ε(C) ≤ 0.45)
         ∧ (Changeable(C, I.T, I.E) ∨ NaturalEmergence(C))
```

**Components:**

- **χ(C, I.P, I.S) ≤ 0.35**: Power-scaled extraction below ceiling
  - χ = ε × π(P) × σ(S)
  - Accounts for how extraction scales with power position and scope
  - Reference: logic_thresholds.md §3b, `rope_chi_ceiling`

- **χ ≤ 0 → ⤠| ε(C) ≤ 0.45**: Negative-chi epsilon bypass (v6.0)
  - When χ ≤ 0 (net beneficiary from institutional perspective), the ε ≤ 0.45 check is skipped
  - Rationale: negative effective extraction means the constraint is experienced as coordination infrastructure regardless of how high the raw base extraction is
  - When χ > 0, the dual-threshold check applies: ε(C) ≤ 0.45 must also hold
  - Reference: logic_thresholds.md §3b, `rope_epsilon_ceiling`

- **Changeable(C, I.T, I.E) ∨ NaturalEmergence(C)**: Changeability or natural emergence
  - Primary path: `effective_immutability_for_context(Context, rope)` checks changeability
  - Alternative path: `emerges_naturally(C)` — domain-invariant bypass for naturally emerging constraints that may not satisfy power-indexed immutability checks
  - Distinguishes Ropes (voluntary standards or natural coordination) from Mountains (natural laws)
  - Checked via `effective_immutability_for_context(Context, rope)`
  - Distinguishes Ropes (voluntary standards) from Mountains (natural laws)
  - Ropes can be revised, replaced, or transcended; Mountains cannot

**Changeability vs Immutability:**

The key distinction between Rope and Mountain at low ε:
- **Mountain**: ε ≤ 0.25, Supp ≤ 0.05, **Immutable**
- **Rope**: χ ≤ 0.35, ε ≤ 0.45, **Changeable**

UTF-8 is a Rope (could be replaced by different encoding standard, unlikely but possible). Thermodynamics is a Mountain (cannot be replaced—it's how reality works).

**Implementation:**

```prolog
% drl_core.pl, classify_from_metrics/6
classify_from_metrics(C, BaseEps, Chi, _Supp, Context, rope) :-
    config:param(rope_chi_ceiling, ChiCeil),
    Chi =< ChiCeil,
    % v6.0: When Chi =< 0, agent is a net beneficiary — skip base extraction gate.
    (Chi =< 0 -> true ; config:param(rope_epsilon_ceiling, EpsCeil), BaseEps =< EpsCeil),
    (   constraint_indexing:effective_immutability_for_context(Context, rope)
    ;   emerges_naturally(C)  % Domain-invariant: bypass power-indexed immutability
    ), !.
```

**Note:** Unlike Tangled Rope, current implementation doesn't explicitly check `has_coordination_function(C)`. Coordination is a **structural expectation** for Ropes but not a metric prerequisite. This is a known simplification—in practice, low ε + low χ strongly implies coordination value.

**Canonical Thresholds** (from logic_thresholds.md §3b):
- `rope_chi_ceiling` = **0.35**
- `rope_epsilon_ceiling` = **0.45**
- `rope_suppression_ceiling` = **0.16** (base suppression, used in other predicates)
- `rope_extractiveness_min` = **0.0** (theoretical minimum)

**Priority:** Rope > Tangled Rope > Piton (checked after Mountain, Snare, Scaffold)

**Power Scaling Example:**

```
UTF-8 encoding standard:
  ε = 0.12 (learning cost)

From (powerless, biographical, trapped, global):
  π(powerless) = 1.5
  σ(global) = 1.2
  χ = 0.12 × 1.5 × 1.2 = 0.216
  
  Check: χ = 0.216 ≤ 0.35 ✓
  Check: ε = 0.12 ≤ 0.45 ✔
  Check: Changeable (could adopt different encoding) ✓
  → Rope from powerless/global ✓

From (institutional, generational, arbitrage, global):
  π(institutional) = -0.2
  σ(global) = 1.2  
  χ = 0.12 × -0.2 × 1.2 = -0.029
  
  Check: χ = -0.029 ≤ 0.35 ✓ (net benefit!)
  Check: ε = 0.12 ≤ 0.45 ✔
  Check: Changeable ✓
  → Rope from institutional/global ✓

Same base structure, both perspectives experience genuine coordination.
Negative χ for institutional means they benefit MORE than powerless (can leverage encoding ecosystem).
But base ε low → genuinely symmetric coordination, not extraction mechanism.
```

---

**📌 STRUCTURAL GATE — Coupling-Invariant Rope Certification:**

> **Shadow Mode Reminder:** CI_Rope certification runs AFTER metric-based classification. A constraint classified as Rope by `classify_from_metrics/6` can be promoted to CI_Rope (certified coordination) if it passes all four Boltzmann invariance tests. This certification operates in `signature_detection.pl`.
>
> **A Rope can be certified as a CI_Rope (Coupling-Invariant Rope) if it maintains high structural purity (≥ 0.7) and shows no nonsensical coupling.**
>
> Certification distinguishes **true coordination mechanisms** from **low-extraction construction** (nudge architecture, behavioral defaults, coordination-washing).
>
> **The Four Tests:**
>
> ```
> CI_Rope(C) ≡ 
>   boltzmann_compliant(C, compliant(_))          [Test 1: Factorization]
>   ∧ scale_invariant(C)                           [Test 2: Scope invariance]
>   ∧ excess_extraction(C, X) ∧ X ≈ 0             [Test 3: At Boltzmann floor]
>   ∧ Coord(C)                                     [Test 4: Coordination function]
> ```
>
> **Test 1: Boltzmann Compliance (Factorization)**
> - Classification factorizes across Power × Scope
> - No nonsensical coupling
> - Coupling score ≤ 0.25
> - Changing Power has same effect at all Scope levels
>
> **Test 2: Scope Invariance**
> - Constraint type stable across scope changes
> - If Rope at local, also Rope at global
> - Natural coordination doesn't become extractive at scale
>
> **Test 3: Excess Extraction**
> - ε(C) ≤ BoltzmannFloor(coordination_type) + tolerance
> - Any extraction is "necessary cost" not extractive overhead
> - Price of Anarchy at theoretical minimum
>
> **Test 4: Coordination Function**
> - Has genuine coordination benefit
> - Not just absence of extraction
> - Provides value to participants
>
> **Editorial Note:** While the metric-based Rope classification in `classify_from_metrics/6` doesn't explicitly check `has_coordination_function(C)` (it's inferred from low ε + low χ), a constraint seeking **CI_Rope certification** MUST satisfy the `Coord(C)` predicate to pass Test 4. This is the distinction between "appears to coordinate from metrics" and "provably coordinates via structural test."
>
> **What Certification Means:**
>
> - **Rope** (from metrics): χ ≤ 0.35, ε ≤ 0.45 — passes thresholds
>   - Could be: true coordination OR low-extraction nudge OR behavioral default
>
> - **CI_Rope** (Boltzmann-certified): Passes all four tests
>   - Mathematically verified: genuine coordination, not coordination-washing
>   - Factorizes cleanly, scope-invariant, at theoretical minimum extraction
>
> **You want UTF-8, not cookie consent dialogs.** Both may pass Rope thresholds. Only UTF-8 passes Boltzmann certification.
>
> **Example: UTF-8 encoding (certified CI_Rope)**
> ```
> Test 1 (Factorization):
>   (powerless, local): Rope
>   (powerless, global): Rope  
>   (institutional, local): Rope
>   (institutional, global): Rope
>   → No coupling, factorizes perfectly ✓
>
> Test 2 (Scope invariance):
>   Same classification (Rope) across all scopes ✓
>
> Test 3 (Excess extraction):
>   ε = 0.12
>   BoltzmannFloor(information_standard) = 0.02
>   Excess = 0.12 - 0.02 = 0.10
>   Within tolerance (≤ 0.10) ✓
>
> Test 4 (Coordination):
>   Enables universal text representation ✓
>
> Certification: CI_Rope ✓✓✓✓
> ```
>
> **Example: "Opt-out" privacy default (fails CI_Rope certification)**
> ```
> Metrics: ε = 0.18, χ = 0.22 (appears as Rope)
>
> Test 1 (Factorization):
>   (powerless, local): Rope (default = private)
>   (powerless, global): Tangled Rope (default = share)
>   → Coupling detected: scope-dependent privacy ✗
>
> Test 2 (Scope invariance):
>   Classification changes: Rope → Tangled Rope ✗
>
> Test 3 (Excess extraction):
>   ε = 0.18
>   BoltzmannFloor(information_standard) = 0.02
>   Excess = 0.16 (above tolerance 0.10) ✗
>
> Test 4 (Coordination):
>   Debatable—mostly benefits data collectors ✗
>
> Certification: FAIL
> Detection: False CI_Rope (FCR) — coordination-washed extraction
> Reclassification: Tangled Rope (downgraded)
> ```
>
> **Why This Matters:**
>
> Modern extraction techniques hide behind low ε metrics:
> - Behavioral defaults (opt-out instead of opt-in)
> - Distributed enforcement (no single suppression point)
> - Scope-dependent extraction (free locally, extractive globally)
> - Asymmetric network effects (benefits early adopters, traps late joiners)
>
> CI_Rope certification catches these patterns. It's the mathematical test for **coordination-washing**: claiming genuine coordination while hiding extraction behind metric manipulation.
>
> **See:**
> - logic_extensions.md §1.5: CI_Rope Certification (full algorithm)
> - logic_extensions.md §1.6: False CI_Rope (FCR) Detection
> - logic_extensions.md §2.3: Purity Scoring (structural health measurement)
>
> **Implementation:** signature_detection.pl, `coupling_invariant_rope/2`, `false_ci_rope/2`

---

**Action Implication:**

**Maintain** → Preserve coordination benefits, avoid disruption.

When facing a true Rope:
- Don't "improve" it hastily (Chesterton's fence)
- Maintain stability—coordination has value
- Protect from extraction (watch for drift to Tangled)
- Document why it exists (institutional memory)

**But verify it's genuinely coordinating first.** Run CI_Rope tests if purity is questionable.

---

**Examples in Practice:**

**Certified CI_Rope: Metric System (SI Units)**
```
ε(metric_system) = 0.08 (learning cost)
Supp(metric_system) = 0.10 (minimal enforcement—mostly voluntary)

From all power positions:
  χ varies 0.05-0.10 (minimal extraction for all)
  
From all scopes:
  Works identically (local and global both benefit)

Boltzmann tests:
  ✓ Factorizes (no Power×Scope coupling)
  ✓ Scope-invariant (same benefits at all scales)
  ✓ At floor (excess ≈ 0.06, within tolerance)
  ✓ Coordinates (universal measurement)

Certification: CI_Rope ✓
Action: Maintain, protect from replacement by inferior alternatives
```

**Rope (not certified): Traffic signal conventions**
```
ε(traffic_lights) = 0.05 (time waiting at red)
χ varies 0.04-0.08 across indices

Appears as Rope from all perspectives, but:
  - Hasn't been tested for Boltzmann compliance (not in corpus)
  - Likely would pass (factorizes, scope-invariant)
  - But certification not yet confirmed

Status: Rope ✓ (from metrics)
CI_Rope: Unknown (needs Boltzmann testing)
Action: Maintain (treat as coordination pending full analysis)
```

**False CI_Rope: Social media "connection" features**
```
ε(social_feed) = 0.20 (attention cost)
Supp = 0.15 (algorithmic nudges, not legal enforcement)

Appears as Rope from some perspectives (low monetary cost)

But Boltzmann tests reveal:
  ✗ Factorization FAIL (powerless×global = Snare, organized×local = Rope)
  ✗ Scope variance (becomes extractive at scale)
  ✗ Excess extraction (ε = 0.20 vs floor = 0.02, excess = 0.18)
  ✗ Coordination questionable (optimizes for engagement, not connection)

Detection: FCR (False CI_Rope)
True classification: Tangled Rope → Snare (scope-dependent)
Action: Reform or exit (not genuine coordination)
```

---

**Summary: Rope Operator**

**Use when:** Genuine coordination with bounded extraction (χ ≤ 0.35, ε ≤ 0.45, changeable)

**Verify with:** CI_Rope certification tests (four Boltzmann invariance tests)

**Beware:** Coordination-washing (low ε hiding extraction via behavioral defaults, coupling)

**Action:** Maintain—preserve coordination value, watch for drift

**Distinction from Mountain:** Ropes are changeable voluntary standards; Mountains are unchangeable natural laws

**Next operator:** Snare (⊠) — pure extraction mechanisms

---

#### B.3 Snare Operator: ⊠C[I]

**Conceptual Overview:**

A **Snare** is a trap. It extracts heavily with minimal or no return. Debt spirals. Predatory contracts. Rent-seeking institutions.

The metaphor: hunter's snare—once caught, struggling makes it tighter. The constraint actively harms those bound by it. Energy flows one direction: from participant to mechanism.

From your index I, a Snare appears as **active extraction requiring resistance**. You pay far more than you get. The constraint damages you.

**Examples of Snares:**
- **Payday loans**: Extract wealth via interest spiral, provide minimal liquidity
- **Non-compete clauses** (powerless perspective): Block mobility, suppress wages, no reciprocal benefit
- **Mandatory arbitration** (consumer perspective): Remove legal recourse, favor corporations
- **Company scrip systems**: Pay in currency only usable at company store (historical)
- **Visa overstay traps**: Immigration status used to extract labor below market rates

**Examples that appear as Snares from some indices but not others:**
- **Employment contracts**: Snare for trapped workers, Tangled Rope for moderates, Rope for executives
- **Credit cards**: Snare for financially illiterate (spiral), Rope for sophisticated users (rewards arbitrage)
- **Rent control**: Snare for landlords (powerless to evict), potential Rope for tenants (depends on implementation)

**The critical insight:** Snares are **the most index-sensitive** type. Same constraint can be:
- **Snare** from (powerless, biographical, trapped, local)
- **Tangled Rope** from (moderate, biographical, constrained, national)  
- **Rope** from (institutional, generational, arbitrage, global)

All three classifications are simultaneously objectively true.

**Why the strictest thresholds?**

Snares require **triple gate**—all three conditions must hold:
1. **High power-scaled extraction**: χ ≥ 0.66
2. **High base extraction**: ε ≥ 0.46
3. **High suppression**: Supp ≥ 0.60

**All three must pass.** This prevents false positives:

- **High χ alone**: Could be powerless perspective on legitimate coordination (ε might be low)
- **High ε alone**: Could be complex but fair system (χ might vary by power position)
- **High Supp alone**: Could be necessary enforcement for coordination (ε might be low)

**Together: extractive trap requiring force.**

**Why ε ≥ 0.46 (not just χ)?**

The ε threshold prevents powerless agents from misclassifying moderate-extraction constraints as Snares just because THEY feel high χ.

**Example:**
```
Constraint Y has ε = 0.30 (moderate extraction)

From (powerless, biographical, trapped, global):
  χ = 0.30 × 1.5 × 1.2 = 0.54
  
  BUT ε = 0.30 < 0.46 (snare_epsilon_floor)
  → NOT a Snare, despite χ feeling high to powerless
  → Prevents powerless-only classification
  → Likely Tangled Rope instead
```

The dual threshold (χ AND ε) ensures: "If it's going to be called a Snare, it must extract heavily from EVERYONE, not just the powerless."

**Formal Definition:**

```
⊠C[I] ↔ χ(C, I.P, I.S) ≥ 0.66 ∧ ε(C) ≥ 0.46 ∧ Supp(C) ≥ 0.60 ∧ Changeable(C, I.T, I.E)
```

**Note:** The implementation additionally requires `¬NaturalLawWithoutBeneficiary(C)` — natural laws without identifiable human beneficiaries are blocked from Snare classification regardless of metric values (asymmetric impact is not asymmetric extraction). See `natural_law_without_beneficiary/1` in `drl_core.pl`.

**Components:**

- **χ(C, I.P, I.S) ≥ 0.66**: Power-scaled extraction high
  - χ = ε × π(P) × σ(S)
  - From your power position, extraction exceeds coordination
  - Reference: logic_thresholds.md §3c, `snare_chi_floor`

- **ε(C) ≥ 0.46**: Base extraction high
  - Prevents powerless-only misclassification
  - Ensures constraint extracts heavily from most positions
  - Reference: logic_thresholds.md §3c, `snare_epsilon_floor`

- **Supp(C) ≥ 0.60**: Requires active enforcement
  - Snares need force to maintain
  - People would exit if they could (high suppression prevents exit)
  - Reference: logic_thresholds.md §3c, `snare_suppression_floor`

- **Changeable(C, I.T, I.E)**: Not a natural law
  - Snares are constructed, not inevitable
  - This prevents classifying death as Snare (it's Mountain—unchangeable)

**Load-Bearing Snare (Theorem 3):**

Special category when suppression exceeds threshold:

```
LoadBearing(C) ↔ ⊠C[I] ∧ Supp(C) ≥ 0.70
```

If Supp > 0.70, the snare is **load-bearing**: cutting it immediately causes system collapse because other structures depend on it.

**Theorem 3 (Catastrophic Collapse Avoidance):** Load-bearing constraints with Supp ≥ 0.70 require **Scaffold construction before removal**. Direct cutting triggers cascade failure.

**Formal statement:**
```
∀C: (⊠C[I] ∧ Supp(C) ≥ 0.70) → (Cut(C) → SystemCollapse)
∴ Build(⊡Replacement[I]) before Cut(C)
```

**Examples:**
- **Abusive welfare system**: Extractive and demeaning, but people literally die if you cut it without replacement
- **Corrupt police force**: Protection racket, but removing it before alternative security exists creates chaos
- **Predatory healthcare financing**: Extraction mechanism, but cutting it before universal alternative kills patients

**Action for load-bearing Snares:** Build **Scaffold** first, then cut Snare. Don't just cut and hope.

**No ¬Coord Gate:**

Current implementation doesn't require `¬has_coordination_function(C)` for Snares. Why?

Many Snares have **vestigial coordination functions**. They started as Ropes, degraded to Tangled Ropes, degraded further to Snares. The coordination function dried up but structural memory remains.

Example: Company scrip was initially a coordination tool (no external currency available). It degraded to pure extraction (company monopoly + inflated prices). Coordination function gone, but historical rationale persists.

**Implementation:**

```prolog
% drl_core.pl, classify_from_metrics/6
classify_from_metrics(C, BaseEps, Chi, Supp, Context, snare) :-
    \+ natural_law_without_beneficiary(C),    % Block snare for natural laws
    config:param(snare_chi_floor, ChiFloor),
    Chi >= ChiFloor,
    config:param(snare_epsilon_floor, EpsFloor),
    BaseEps >= EpsFloor,
    config:param(snare_suppression_floor, SuppFloor),
    Supp >= SuppFloor,
    snare_immutability_check(Context), !.
```

**Note:** The `natural_law_without_beneficiary(C)` guard blocks constraints that emerge naturally, require no enforcement, and have no identifiable human beneficiary from being classified as Snares. The `snare_immutability_check(Context)` ensures changeability.

**Canonical Thresholds** (from logic_thresholds.md §3c):
- `snare_chi_floor` = **0.66**
- `snare_epsilon_floor` = **0.46**
- `snare_suppression_floor` = **0.60**
- `snare_extraction_ceil` = **1.00** (maximum possible extraction)
- `snare_load_bearing_threshold` = **0.70** (above → load-bearing snare)

**Priority:** Snare > Scaffold > Rope > Tangled Rope (checked second, immediately after Mountain)

**Power Scaling Example:**

```
Payday loan system:
  ε = 0.70 (high base extraction—interest rates, fees, spiral)
  Supp = 0.75 (active legal enforcement via debt collection)

From (powerless, biographical, trapped, local):
  π(powerless) = 1.5
  σ(local) = 0.8
  χ = 0.70 × 1.5 × 0.8 = 0.84
  
  Check: χ = 0.84 ≥ 0.66 ✓
  Check: ε = 0.70 ≥ 0.46 ✓
  Check: Supp = 0.75 ≥ 0.60 ✓
  → Snare from powerless/local ✓
  
  Note: Supp = 0.75 ≥ 0.70 → Load-bearing Snare
  (Cutting without alternative creates crisis for trapped borrowers)

From (powerful, biographical, mobile, national):
  π(powerful) = 0.6
  σ(national) = 1.0
  χ = 0.70 × 0.6 × 1.0 = 0.42
  
  Check: χ = 0.42 < 0.66 ✗
  → NOT Snare from powerful/national
  → Likely Tangled Rope (they see it as extractive but manageable)

Same mechanism, different experience.
Powerless agent: trapped in debt spiral (Snare)
Powerful agent: avoids mechanism entirely or uses strategically (Tangled/Rope)
```

---

**📌 STRUCTURAL GATE — Nonsensical Coupling as Extraction Evidence:**

> **Shadow Mode Reminder:** Nonsensical coupling detection runs AFTER metric-based classification. A constraint classified as Snare by `classify_from_metrics/6` can have this classification confirmed (or a Mountain/Rope can be demoted to Tangled Rope) if nonsensical coupling is detected. This operates in `signature_detection.pl`.
>
> **Extraction is often hidden.** If a constraint fails the Boltzmann Factorization test (coupling score > 0.25), it couples independent dimensions in ways that natural laws cannot—this is mathematical evidence of construction, even if it mimics a natural law or claims symmetric benefits.
>
> **The Core Insight:**
>
> Some coupling is **functional** (necessary for coordination):
> - Global power grid must coordinate local + regional + national infrastructure
> - Resource allocation requires scope awareness
> - Enforcement mechanisms have natural jurisdictional ranges
>
> Some coupling is **nonsensical** (serves extraction, not coordination):
> - Why does breakfast cereal choice determine soap brand?
> - Why does scope change classification at one power level but not another?
> - Why does a "coordination mechanism" only bind powerless at large scale?
>
> **Nonsensical coupling is the signature of extractive complexity**: coupling that hides extraction behind false necessity.
>
> **Detection Pattern:**
>
> For each coupled pair (D₁, D₂) from Boltzmann test:
> 1. Check if coordination function justifies coupling
> 2. Check if coupling strength exceeds threshold (> 0.50)
> 3. If strong coupling without justification → nonsensical
>
> **Functional Justifications:**
> - **Global infrastructure**: Scope naturally couples with power (scale matters for coordination)
> - **Resource allocation**: Scope couples with power (distribution logistics)
> - **Enforcement mechanism**: Power couples with scope (jurisdictional range)
>
> **Nonsensical Patterns:**
> - **Power × Time**: Why does your power position change the time horizon relevant to a constraint?
> - **Scope × Exit**: Why does global scope eliminate exit options that existed locally?
> - **Power × Suppression (unusual)**: Why does suppression vary by power for a claimed "natural law"?
>
> **Example: "Competition is human nature" (physics-washed Snare)**
> ```
> Claimed: Mountain (universal natural law)
> 
> Boltzmann test grid:
>   (powerless, local):  ε=0.12, Supp=0.04 → Mountain ✓
>   (powerless, global): ε=0.68, Supp=0.65 → Snare ✗
>   (powerful, local):   ε=0.08, Supp=0.03 → Mountain ✓
>   (powerful, global):  ε=0.02, Supp=0.01 → Mountain ✓
>
> Nonsensical coupling detected:
>   Power × Scope interaction
>   Only powerless agents at global scale experience high extraction
>   
>   Question: Why does "natural competition" only trap the powerless globally?
>   Answer: Because it's not natural—it's a constructed market system
>           that extracts from those without bargaining power at scale.
>
> Coupling strength: 0.52 (strong)
> Functional justification: NONE (competition doesn't require scope-dependent extraction)
> 
> Verdict: Nonsensical coupling → Extractive complexity
> Classification: False Natural Law (FNL) → Constructed Constraint
> Reclassification from claimed Mountain → Tangled Rope (conservative fallback)
> ```
>
> **Example: Carbon pricing mechanism**
> ```
> Coupling test:
>   (powerless, local):  χ=0.66 → Snare (borderline)
>   (powerless, global): χ=0.90 → Snare ✓
>   (institutional, local): χ=-0.10 → Rope ✓
>   (institutional, global): χ=-0.13 → Rope ✓
>
> Coupling detected: Power × Scope
>   Powerless: Extraction increases with scope (0.66 → 0.90)
>   Institutional: Net benefit regardless of scope (-0.10 → -0.13)
>
> Functional justification check:
>   Does carbon pricing require differential extraction by power?
>   Theoretically: NO (carbon has same climate impact regardless of who emits)
>   Practically: YES (compliance costs scale differently)
>   
> Verdict: Functionally justified coupling (represents real cost asymmetry)
>         NOT nonsensical (reflects power asymmetry in implementation)
>         
> Classification: Structurally valid Tangled Rope
> (Coordination exists but extraction distributed asymmetrically)
> ```
>
> **Why This Matters:**
>
> Nonsensical coupling is **how extraction hides**. By coupling independent dimensions without justification:
> - Claims appear universal but bind selectively
> - "Natural laws" only constrain the powerless
> - "Coordination" becomes extraction at scale
> - Complexity serves to obscure, not coordinate
>
> If your cereal choice determines your soap brand, someone's extracting via that coupling. If "market competition" only traps the powerless at global scale, it's not a natural law—it's a constructed extraction mechanism.
>
> **See:**
> - logic_extensions.md §1.7: Nonsensical Coupling Detection
> - logic_extensions.md §1.4: False Natural Law (FNL) — physics-washed Snares
> - logic_extensions.md §1.6: False CI_Rope (FCR) — coordination-washed Snares
>
> **Implementation:** boltzmann_compliance.pl + signature_detection.pl, `detect_nonsensical_coupling/3`, `false_natural_law/2`

---

**Action Implication:**

**Cut/Exit** → Escape extraction or resist actively if trapped.

When facing a Snare:

**If you have exit options (mobile, arbitrage):**
- Cut immediately—don't waste time reforming traps
- Route around via alternatives
- Warn others about the extraction mechanism

**If you're trapped (trapped, constrained):**
- Resist where possible
- Build alternative systems (mutual aid, collective action)
- Organize (trapped → organized shifts π from 1.5 to 0.4)
- If load-bearing, build Scaffold first before cutting

**Never normalize a Snare.** The fact that "everyone does it" or "that's how things work" doesn't make extraction legitimate. Snares persist through normalization.

---

**Examples in Practice:**

**Clear Snare: Payday loan industry**
```
ε = 0.70 (APR often >300%, debt spiral design)
Supp = 0.75 (legal enforcement, wage garnishment)
χ(powerless, local) = 0.84

Triple gate: ✓✓✓ → Snare
Load-bearing: Supp > 0.70 → YES

Action: Build alternative (credit union, mutual aid) THEN cut
Don't just ban payday loans—trapped borrowers need alternatives first
```

**Index-sensitive: Employment contract**
```
ε = 0.50 (alienation, time extraction, power asymmetry)
Supp = 0.60 (legal enforcement, economic coercion)

From (powerless, biographical, trapped, local):
  χ = 0.50 × 1.5 × 0.8 = 0.60
  Fails χ ≥ 0.66 (just below threshold)
  → Tangled Rope (extraction exists but below Snare threshold)

From (moderate, biographical, constrained, national):
  χ = 0.50 × 1.0 × 1.0 = 0.50
  → Tangled Rope ✓

From (powerful, generational, mobile, global):
  χ = 0.50 × 0.6 × 1.2 = 0.36
  Fails χ ≥ 0.66
  → Rope (net benefit from executive compensation, autonomy)

Same employment system, three different types.
All objectively true from their indices.
```

**Physics-washed Snare: "Natural market dynamics"**
```
Claimed: Mountain (inevitable, natural)

Actual metrics: ε varies 0.15-0.75 by power position
                Supp varies 0.05-0.70 by power position

Boltzmann test:
  Coupling score = 0.58 (strong coupling)
  Power × Scope interaction detected
  Only binds powerless at global scale
  
  Fails factorization test ✗

FNL detected: Physics-washed construction
True classification: Tangled Rope → Snare (index-dependent)
```

---

**Summary: Snare Operator**

**Use when:** High extraction from your index (χ ≥ 0.66, ε ≥ 0.46, Supp ≥ 0.60)

**Verify with:** Nonsensical coupling check (is extraction hidden in complexity?)

**Beware:** Index sensitivity (Snare for powerless, Rope for powerful—both true)

**Load-bearing check:** If Supp ≥ 0.70, build Scaffold before cutting

**Action:** Cut/Exit if mobile; Resist/Organize if trapped; Build alternatives before destroying load-bearing Snares

**Distinction from Tangled Rope:** Snares extract heavily (ε ≥ 0.46); Tangled Ropes have genuine coordination (ε typically 0.30-0.60, requires Coord predicate)

**Next operator:** Tangled Rope (⊞⊠) — the hybrid coordination-extraction type

---

#### B.4 Tangled Rope Operator: ⊞⊠C[I]

##### Conceptual Overview

A **Tangled Rope** both coordinates AND extracts. This is not a mistake, edge case, or failed Rope—it's the **most common constraint type** in real systems (~36% of analyzed constraints).

The metaphor: ropes tangled with extraction mechanisms. The rope genuinely helps you climb (coordination) but snags constantly, requiring energy to manage (extraction). You benefit from being roped together, but you also pay asymmetric costs.

**Critical insight:** Tangled Ropes are **irreducible hybrids**. They are NOT "Ropes that went bad" or "Snares with benefits." They are genuinely BOTH:
- Provide coordination value (you'd be worse off without them)
- Extract asymmetrically (some benefit more than they pay, others pay more than they benefit)

**Examples:**
- **Employment**: Provides income, skill-building, structure (coordination) while extracting time, agency, surplus value (extraction)
- **Social media**: Enables communication, organization (coordination) while harvesting attention, data, behavioral manipulation (extraction)
- **Carbon credits** (ε = 0.55): Market mechanism for emissions reduction (coordination) with asymmetric compliance costs (extraction)
- **Mortgages**: Enable homeownership (coordination) with interest extraction and foreclosure risk (extraction)
- **Public education**: Provides knowledge, credentialing (coordination) with standardization pressure and opportunity costs (extraction)

##### Formal Definition

```
⊞⊠C[I] ↔ ¬NaturalLawWithoutBeneficiary(C)
         ∧ 0.40 ≤ χ(C, I.P, I.S) ≤ 0.90
         ∧ ε(C) ≥ 0.30
         ∧ Supp(C) ≥ 0.40
         ∧ Enforce(C)
         ∧ Coord(C)
         ∧ Asymmetric(C)
```

**Note:** The `¬NaturalLawWithoutBeneficiary(C)` guard prevents natural laws without identifiable beneficiaries from being classified as Tangled Rope, same as for Snare.

**Components:**

- **0.40 ≤ χ ≤ 0.90**: Moderate to high power-scaled extraction
  - Overlaps with Snare range (Snare starts at χ ≥ 0.66)
  - Same constraint can be Tangled from one index, Snare from another
  - Reference: logic_thresholds.md §3d, `tangled_rope_chi_floor`, `tangled_rope_chi_ceil`

- **ε(C) ≥ 0.30**: Moderate-to-high base extraction
  - Distinguishes from Rope (ε ≤ 0.45)
  - Ensures genuine extraction, not just coordination overhead
  - Reference: logic_thresholds.md §3d, `tangled_rope_epsilon_floor`

- **Supp(C) ≥ 0.40**: Active enforcement required
  - Not voluntary (unlike Rope)
  - Requires institutional force or social pressure
  - Reference: logic_thresholds.md §3d, `tangled_rope_suppression_floor`

- **Enforce(C)**: Structural predicate—requires active enforcement mechanism
  - Checked via `requires_active_enforcement(C)`
  - Distinguishes from low-enforcement Ropes

- **Coord(C)**: Structural predicate—has genuine coordination function
  - Checked via `has_coordination_function(C)`
  - **Critical:** This distinguishes Tangled from Snare
  - If Coord(C) false → likely Snare, not Tangled

- **Asymmetric(C)**: Structural predicate—extraction distributed unevenly
  - Checked via `has_asymmetric_extraction(C)`
  - Some agents benefit more than they pay (π < 0 or low π)
  - Others pay more than they benefit (high π)

##### Implementation

```prolog
% drl_core.pl, classify_from_metrics/6
classify_from_metrics(C, BaseEps, Chi, Supp, _Context, tangled_rope) :-
    \+ natural_law_without_beneficiary(C),    % Block tangled_rope for natural laws
    config:param(tangled_rope_chi_floor, ChiFloor),
    config:param(tangled_rope_chi_ceil, ChiCeil),
    Chi >= ChiFloor,
    Chi =< ChiCeil,
    config:param(tangled_rope_epsilon_floor, EpsFloor),
    BaseEps >= EpsFloor,
    config:param(tangled_rope_suppression_floor, MinS),
    Supp >= MinS,
    requires_active_enforcement(C),
    narrative_ontology:has_coordination_function(C),
    narrative_ontology:has_asymmetric_extraction(C), !.
```

**Note:** Unlike other types, Tangled Rope requires **three structural predicates** to fire. Metrics alone insufficient—must confirm enforcement, coordination, and asymmetry.

##### Canonical Thresholds

From logic_thresholds.md §3d:
- `tangled_rope_chi_floor` = **0.40**
- `tangled_rope_chi_ceil` = **0.90**
- `tangled_rope_epsilon_floor` = **0.30**
- `tangled_rope_suppression_floor` = **0.40**

**Priority:** Tangled Rope > Piton (checked after Mountain, Snare, Scaffold, Rope)

##### Cross-Index Classification Table

**Example: Carbon Credits System (ε = 0.55, Supp = 0.60)**

*Note: This table shows a representative slice of the index grid. The full system evaluates constraints across all combinations of (P, T, E, S), not just these four indices. These four were chosen to illustrate the range of indexical variation.*

| Index | Power | Scope | π | σ | χ Calculation | χ Value | Type | Interpretation |
|-------|-------|-------|---|---|---------------|---------|------|----------------|
| I₁ | powerless | local | 1.5 | 0.8 | 0.55 × 1.5 × 0.8 | **0.66** | **Tangled** ⊞⊠ | Benefits exist but costs high |
| I₂ | moderate | national | 1.0 | 1.0 | 0.55 × 1.0 × 1.0 | **0.55** | **Tangled** ⊞⊠ | Clear hybrid (mid-range) |
| I₃ | institutional | global | -0.2 | 1.2 | 0.55 × -0.2 × 1.2 | **-0.13** | **Rope** ⊞ | Net beneficiary (extracts from system) |
| I₄ | analytical | global | 1.15 | 1.2 | 0.55 × 1.15 × 1.2 | **0.76** | **Tangled** ⊞⊠ | Sees extraction moderate normalizes |

**Key observations:**
- Same ε (0.55), same Supp (0.60), same constraint
- Four different experienced types (3× Tangled, 1× Rope)
- All four classifications simultaneously objectively true
- Institutional index experiences net benefit (χ < 0)
- Powerless at Snare threshold (0.66 = borderline)
- Analytical detects extraction moderate agents miss (π = 1.15)

##### Overlap with Snare

Tangled Rope overlaps with Snare in χ range:
- Tangled Rope: 0.40 ≤ χ ≤ 0.90
- Snare: χ ≥ 0.66

**How same constraint can be both:**

```
Carbon credits from (powerless, biographical, trapped, global):
  χ = 0.55 × 1.5 × 1.2 = 0.99

Check Tangled Rope:
  χ = 0.99 within [0.40, 0.90]? NO (exceeds ceiling) ✗
  
Check Snare:
  χ = 0.99 ≥ 0.66? YES ✓
  ε = 0.55 ≥ 0.46? YES ✓
  Supp = 0.60 ≥ 0.60? YES ✓
  → Snare from powerless/global ✓

Same constraint:
  - Tangled from moderate/national (χ = 0.55)
  - Snare from powerless/global (χ = 0.99)
  
Both true. The constraint genuinely coordinates for moderates while trapping powerless at global scale.
```

##### Action Implication: Surgical Reform

**Reform (Surgical)** → Preserve coordination, excise extraction.

The goal is NOT to destroy the Tangled Rope—it provides genuine value. The goal is to **untangle** it: remove the extraction mechanism while preserving the coordination function.

**Surgical reform process:**
1. **Identify coordination function**: What genuine value does this provide?
2. **Identify extraction mechanism**: Where does asymmetric cost come from?
3. **Separate the two**: Can coordination exist without extraction?
4. **Preserve coordination, cut extraction**: Reform specifically targets extractive component
5. **Monitor for re-tangling**: Watch for extraction creeping back in

**Example: Employment contract reform**
```
Coordination function: 
  - Income provision
  - Skill development
  - Social structure
  
Extraction mechanism:
  - Surplus value capture
  - Agency restriction
  - Exit costs (health insurance tied to job)
  
Surgical reform:
  - Preserve: employment relationship, income, skill-building
  - Cut: surplus extraction (worker ownership, profit-sharing)
  - Cut: exit costs (decouple healthcare from employment)
  - Result: Rope (pure coordination) or lower-ε Tangled (reduced extraction)
```

**When surgical reform fails:**

If ε too high (> 0.70) or purity too low (< 0.30), reform may be blocked. At that point:
- Transition from Reform → Cut/Exit
- Build alternative Scaffold first
- Then abandon degraded Tangled Rope

**Stage 7 Extension:** Purity scoring (logic_extensions.md §2) provides a **reformability subscore** that mathematically predicts surgical reform success probability. Tangled Ropes with purity ≥ 0.50 are reform candidates; those below 0.30 have reform blocked by composition gates. This moves from qualitative judgment ("seems reformable") to quantitative prediction ("78% success probability given purity 0.62").

See logic_extensions.md §5.3 for composition gates (surgical reform requires purity ≥ 0.30).

##### Empirical Prevalence

**Most common constraint type:** ~36% of 691-constraint corpus

**Why so common?**

Real-world coordination is messy. Pure coordination (Rope, ε ≤ 0.45) is rare. Pure extraction (Snare, ε ≥ 0.46, no coordination) is also rare. Most constraints fall in the middle: they coordinate something while extracting asymmetrically.

**Distribution:**
- Mountains: ~8%
- Ropes: ~12%
- Snares: ~18%
- **Tangled Ropes: ~36%** ← Most common
- Scaffolds: ~15%
- Pitons: ~11%

##### Summary: Tangled Rope Operator

**Use when:** Genuine coordination exists but extraction present (0.40 ≤ χ ≤ 0.90, ε ≥ 0.30, both Coord and Asymmetric)

**Key insight:** Not failed Ropes—irreducible hybrids providing value AND extracting

**Action:** Surgical reform—preserve coordination, excise extraction

**Beware:** Can appear as Snare from powerless index, Rope from institutional index (both true)

**Reform limit:** If purity < 0.30 or ε > 0.70, surgical reform may be blocked → transition to Cut/Exit

**Prevalence:** Most common type (~36% of constraints)

**Next operator:** Scaffold (⊡) — temporary support structures

---

#### B.5 Scaffold Operator: ⊡C[I]

##### Conceptual Overview

A **Scaffold** is temporary support. Built-in expiration, designed for dissolution.

The metaphor: construction scaffolding. Essential while building, dangerous if left up permanently. The structure is meant to support something else, then disappear.

From your index I, a Scaffold provides **time-limited coordination** with explicit sunset conditions. It's not permanent infrastructure—it's transitional support.

**Examples:**
- **Training wheels**: Support while learning to bike, remove when skilled
- **Pandemic emergency powers**: Temporary authority expansion with explicit end date
- **Startup funding rounds**: Staged capital with exit/IPO expectation
- **Construction permits**: Time-limited authorization
- **Transition governments**: Post-revolution temporary authority until elections

**Non-examples (failed sunsets):**
- **"Temporary" income tax** (1913 in US): No sunset, became permanent
- **Emergency surveillance powers** (post-9/11): Sunset violated, became institutional

These degraded from Scaffolds to Pitons (sunset violation).

##### The Distinguishing Feature: Sunset Clause

**Formal operator:**
```
Sunset(C) ≡ ∃t_end, ∃conditions: (t ≥ t_end ∨ Met(conditions)) → Dissolve(C)
```

A Scaffold MUST have built-in expiration—either:
- **Temporal**: Fixed end date
- **Conditional**: Triggered by achievement
- **Milestone**: Triggered by progress markers

**Without sunset → NOT a Scaffold.** A "permanent Scaffold" is incoherent. A "permanent emergency power" is a Piton.

##### Formal Definition

```
⊡C[I] ↔ χ(C, I.P, I.S) ≤ 0.30 ∧ Coord(C) ∧ (Sunset(C) ∨ ¬Enforce(C)) ∧ Theater(C) ≤ 0.70
```

**Note on temporality:** The implementation uses a soft temporality gate: an explicit sunset clause is the strongest signal, but absence of active enforcement also qualifies. Non-enforced coordination is inherently scaffold-like (temporary support that dissolves on its own) rather than tangled-rope-like (enforced extraction). See `scaffold_temporality_check/1` in `drl_core.pl`.

**Components:**
- **χ ≤ 0.30**: Low extraction (enabling, not extractive)
- **Coord(C)**: Has genuine coordination function
- **Sunset(C)**: Built-in expiration clause ← **Critical distinguisher**
- **Theater ≤ 0.70**: Ensures real work, not performance

##### Implementation

```prolog
% drl_core.pl, classify_from_metrics/6, lines 2962-2968
classify_from_metrics(C, _BaseEps, Chi, _Supp, _Context, scaffold) :-
    config:param(scaffold_extraction_ceil, MaxX),
    Chi =< MaxX,
    narrative_ontology:has_coordination_function(C),
    scaffold_temporality_check(C),        % Sunset clause OR no active enforcement
    config:param(theater_metric_name, TheaterMetricName),
    \+ (narrative_ontology:constraint_metric(C, TheaterMetricName, TR), TR > 0.70), !.
```

**Canonical Thresholds** (logic_thresholds.md §3e):
- `scaffold_extraction_ceil` = **0.45**

**Priority:** Scaffold > Rope (checked after Mountain, Snare, before Rope)

##### Lifecycle: Sunset Violation → Piton

**Healthy:**
```
Build → Serve purpose → Sunset triggers → Dissolve
```

**Degraded (Type 5 drift):**
```
Build → Serve purpose → Sunset violated → Piton
```

**Sunset Drift:** A Scaffold without a functioning sunset mechanism is structurally indistinguishable from a decaying Rope or proto-Piton. The sunset clause is not merely administrative detail—it's the core structural feature that defines Scaffold type. When sunset drifts (conditions become ambiguous, timeline extends indefinitely, triggers never evaluated), the constraint transitions from temporary support (Scaffold) to permanent theater (Piton). This is why "Monitor Sunset" is the highest-priority action for Scaffolds.

##### Action Implication

**Monitor Sunset** → Ensure timely dissolution.

- Track progress toward sunset conditions
- **Resist sunset extension** (unless conditions genuinely unmet)
- If extension needed, rebuild as new Scaffold with new sunset
- If violated → flag as Piton

##### Summary

**Use when:** Temporary support with sunset (χ ≤ 0.30, Coord, Sunset, Theater ≤ 0.70)

**Distinguisher:** Sunset clause—built-in expiration

**Action:** Monitor sunset, resist extension

**Degradation:** Sunset violation → Piton

**Next operator:** Piton (⊟) — degraded constraints

---

#### B.6 Piton Operator: ⊟C[I]

##### Conceptual Overview

A **Piton** is a degradation state. Function dried up, structure persists as theater.

The metaphor: climbing piton left in rock after route abandoned. Once essential, now vestigial. Provides minimal function but costs energy to maintain.

From your index I, a Piton appears as **theater without substance**. Performance ratio dominates actual work.

**Examples:**
- **QWERTY keyboard**: Vestigial typewriter constraint (no mechanical jams to avoid)
- **Legacy software monoliths**: Minimal value, high maintenance cost
- **Obsolete regulations**: No longer serve purpose but remain enforced
- **Expired Scaffolds**: Sunset violated, now just bureaucratic theater

##### The Distinguishing Feature: Theater Ratio ≥ 0.70

**Theater Ratio:**
```
Theater(C) = Performance / Substance
```

When Performance >> Substance, constraint is "coordination in name only."

**Pitons are characterized by:**
- **χ ≤ 0.25**: Low effective extraction (inactive or near-inactive)
- **ε > 0.10**: Still costs energy (maintenance burden)
- **Theater ≥ 0.70**: High performance/substance ratio

The theater floor (≥ 0.70) distinguishes Pitons from low-extraction Ropes:
- **Rope**: χ ≤ 0.35, ε ≤ 0.45, Theater < 0.70 (real coordination)
- **Piton**: χ ≤ 0.45, ε > 0.10, Theater ≥ 0.70 (theater dominates)

##### Formal Definition

```
⊟C[I] ↔ χ(C, I.P, I.S) ≤ 0.25 ∧ ε(C) > 0.10 ∧ Theater(C) ≥ 0.70
```

**Components:**
- **χ ≤ 0.25**: Low effective extraction
- **ε > 0.10**: Still requires maintenance
- **Theater ≥ 0.70**: Performance >> Substance

##### Implementation

```prolog
% drl_core.pl, classify_from_metrics/6, lines 2990-2997
classify_from_metrics(C, BaseEps, Chi, _Supp, _Context, piton) :-
    config:param(piton_extraction_ceiling, XCeil),
    Chi =< XCeil,
    config:param(piton_epsilon_floor, EpsFloor),
    BaseEps > EpsFloor,
    config:param(theater_metric_name, TheaterMetricName),
    narrative_ontology:constraint_metric(C, TheaterMetricName, TR),
    config:param(piton_theater_floor, TRFloor),
    TR >= TRFloor, !.
```

**Canonical Thresholds** (logic_thresholds.md §3f):
- `piton_extraction_ceiling` = **0.45**
- `piton_epsilon_floor` = **0.10**
- `piton_theater_floor` = **0.70**

**Priority:** Piton > Naturalized > unknown (checked after Tangled Rope, before Naturalized)

##### Action Implication

**Bypass** → Route around obsolete structure.

**Critical distinction: Bypass ≠ Cut.**

- **Cut (Snare action)**: Active removal of harmful extraction mechanism. Snares are **active threats** requiring resistance.
- **Bypass (Piton action)**: Route around inert theater. Pitons are **inactive remnants** not worth the energy of destruction.

The energy-accounting logic differs:
- **Snare**: High extraction (ε ≥ 0.46), high suppression (Supp ≥ 0.60) → **active harm** → justify energy cost of cutting
- **Piton**: Low extraction (χ ≤ 0.45), mostly theater (≥ 0.70) → **inert decay** → energy better spent building alternatives

Don't waste energy maintaining or fighting Pitons:
- Ignore where possible
- Build alternatives that make Piton irrelevant
- Don't lobby for removal (low priority—it's already inactive)
- Let natural obsolescence complete

**Example:** QWERTY keyboard is a Piton. You could campaign to replace it (high energy cost, resistance from installed base), or you could route around it (voice input, alternative layouts for new contexts). Bypass > Cut for inert constraints.

##### Summary

**Use when:** Degraded constraint with high theater (χ ≤ 0.25, ε > 0.10, Theater ≥ 0.70)

**Distinguisher:** Theater ratio ≥ 0.70

**Action:** Bypass—route around, don't engage

**Origin:** Often degraded Scaffolds (sunset violation) or obsolete Ropes

---

#### B.6a Coordination Vitality Gates (v7.0)

The v7.0 revision added a **piton pre-check** that fires before the snare gate when coordination vitality is explicitly declared dead or degrading.

**Predicates:**

- `coordination_vitality(+C, +Status)` — Multifile/dynamic fact in `narrative_ontology.pl:337`. Status is `dead`, `degrading`, or `active`. Default clause fails (coordination presumed alive unless declared in testset files).
- `coordination_dead(+C)` — Succeeds if `coordination_vitality(C, dead)` or `coordination_vitality(C, degrading)`. Defined in `drl_core.pl:295-298`.

**Classification effect:** When `coordination_dead(C)` succeeds, the pre-check clause (`classify_from_metrics/6` at `drl_core.pl:314-321`) fires before the snare gate:

```
Piton_precheck(C) ↔ coordination_dead(C) ∧ ε(C) > 0.10 ∧ theater(C) ≥ 0.70
```

This clause does **not** check χ or suppression. A dead-coordination constraint with high theater is a piton regardless of extraction level. The `piton_epsilon_floor` (0.10) floor prevents zero-extraction mountains from piton misclassification.

**Diagnostic evidence:** Dead and degrading pitons show mean ε = 0.66 (consistent with terminal pitons), while transitional pitons with live coordination show mean ε ≈ 0.39.

**Limitation:** Coordination vitality must be manually declared in testset files — there is no auto-detection mechanism. Only three status values exist (dead, degrading, active) with no temporal modeling of when coordination died or degradation rate.

---

#### B.7 Naturalized Operator

##### Conceptual Overview

An **Naturalized** constraint is one where power scaling dramatically changes the experienced extraction, making the constraint's "true nature" structurally ambiguous. It has high base extraction (ε > 0.45) but low power-scaled extraction (χ < 0.40), meaning the raw cost is high but the effective experienced cost is low from the evaluating index.

This catches a specific structural gap: constraints that fall through all other gates because their base extraction is too high for Rope but their power-scaled extraction is too low for Tangled Rope or Snare. The disconnect between raw and effective extraction is itself informative — it reveals that power position is doing significant work in mediating the constraint's impact.

**Examples:**
- **Professional licensing** (from institutional perspective): High ε (training costs, compliance burden) but institutional power dramatically reduces effective extraction — the institution benefits from the barrier to entry
- **Complex tax codes** (from organized perspective): High raw complexity/cost but organized actors reduce effective burden through professional management
- **International trade regulations** (from institutional perspective): High base extraction but institutional actors extract net benefit through regulatory capture

##### The Distinguishing Feature: Power Scaling Naturalization

The constraint's classification is "naturalized" because the same structural mechanism appears fundamentally different depending on your power index. The base extraction is genuinely high (above Rope ceiling), but power scaling compresses it below the Tangled Rope floor.

##### Formal Definition

```
Naturalized(C) ↔ ε(C) > 0.45 ∧ χ(C, I.P, I.S) < 0.40
```

**Components:**
- **ε(C) > 0.45**: Base extraction exceeds Rope ceiling (`rope_epsilon_ceiling`)
  - Constraint imposes real costs in absolute terms
- **χ(C, I.P, I.S) < 0.40**: Power-scaled extraction below Tangled Rope floor (`tangled_rope_chi_floor`)
  - From the evaluating index, those costs are dramatically compressed by power position

**Gate Position:** Between Piton and unknown — catches constraints that fall through all six primary gates.

##### Implementation

```prolog
% drl_core.pl, classify_from_metrics/6
classify_from_metrics(_C, BaseEps, Chi, _Supp, _Context, naturalized) :-
    config:param(rope_epsilon_ceiling, EpsCeil),
    BaseEps > EpsCeil,
    config:param(tangled_rope_chi_floor, ChiFloor),
    Chi < ChiFloor, !.
```

**Priority:** Naturalized > unknown (checked after Piton, before unknown)

##### Network Participation

Naturalized constraints participate in purity contamination propagation with specific parameters:

| Property | Value | Rationale |
|----------|-------|-----------|
| `type_contamination_strength` | 0.3 | Moderate-low — structural ambiguity limits contagion |
| `type_immunity` | 0.7 | High susceptibility — ambiguous structures easily influenced |

##### Action Implication

**`investigate_naturalization`** → The power-scaling naturalization itself is the primary finding.

When facing a Naturalized constraint:
- Investigate why power scaling is doing so much work
- Re-evaluate from multiple indices (powerless, moderate, institutional)
- The naturalization often reveals that the constraint is a Tangled Rope or Snare from lower-power indices
- Consider whether the base extraction (ε > 0.45) represents real structural cost that power is masking

##### Summary

**Use when:** High base extraction with low power-scaled extraction (ε > 0.45, χ < 0.40)

**Distinguisher:** Power scaling dramatically compresses experienced extraction

**Action:** Investigate — re-evaluate from multiple indices

**Diagnostic value:** Reveals constraints where power position does the most structural work in mediating experience

---

**[§II.B COMPLETE - Seven Core Modal Operators]**

All seven constraint types now defined with full triple-layer format:
- ✅ Mountain (■): Unchangeable terrain
- ✅ Rope (⊞): Coordination mechanisms
- ✅ Snare (⊠): Extraction traps
- ✅ Tangled Rope (⊞⊠): Hybrid coordination-extraction
- ✅ Scaffold (⊡): Temporary support
- ✅ Piton (⊟): Degraded theater
- ✅ Naturalized: Power-scaling ambiguity

**Next:** §II.C Detection Pattern Operators (FNL, CI_Rope, FCR)

---

### C. Detection Pattern Operators (Non-Indexed)

Beyond the seven indexed types, the system includes **detection patterns** that identify misclassification or structural fraud. These operate as **signature overrides** in the two-regime architecture.

**Architecture note:** Detection patterns run in `signature_detection.pl` (via wrapper `structural_signatures.pl`) AFTER metric-based classification in `drl_core.pl`. They can override or certify the metric-based type but do not modify the core classification logic.

---

#### C.1 False Mountain (FM)

**Conceptual Overview:**

The simplest detection pattern. If someone claims a constraint is a Mountain (unchangeable natural law) but evidence shows high extraction and enforcement, the claim is false.

**Formal Definition:**
```
FM(C) ↔ Claimed(■C) ∧ ∃I(¬■C[I]) ∧ ε(C) > 0.70
```

**Detection logic:**
- Constraint is explicitly claimed as Mountain, OR
- Indexed classification produces Mountain from some perspectives
- BUT ε > 0.70 (high extraction contradicts natural emergence)
- AND from at least one index it's clearly NOT a Mountain

**Why ε > 0.70?**

High enforcement contradicts the claim of natural inevitability. If it takes 70%+ energy to maintain, it's not a natural law—it's a constructed constraint requiring force.

**Example:**
```
Claimed: "Poverty is inevitable" (Mountain claim)
Actual: ε = 0.75, Supp = 0.80
Detection: High enforcement reveals construction → FM ✓
Reclassification: Tangled Rope or Snare (depending on power position)
```

**Implementation:**
```prolog
% signature_detection.pl
false_mountain(C) :-
    (claimed_type(C, mountain) ; indexed_as_mountain_somewhere(C)),
    extractiveness(C, Eps),
    Eps > 0.70.
```

---

#### C.2 False Natural Law (FNL) — Stage 7 Extension

**Conceptual Overview:**

Physics-washing detector. Uses Boltzmann compliance test to detect constructed constraints masquerading as natural laws.

**Formal Definition:**
```
FNL(C) ↔ Claimed(■C) 
         ∧ boltzmann_compliant(C, non_compliant(Score, Threshold))
         ∧ Score > Threshold
         ∧ ε(C) > 0.70
```

**The critical test:**

Natural laws **factorize** across index dimensions:
```
χ(P, S) ≈ f(P) × g(S)
```

If classification depends on nonsensical coupling of Power × Scope (e.g., only binds powerless at global scale), it's constructed, not natural.

**Example:**
```
Claimed: "Market forces are natural laws"

Boltzmann test:
  (powerless, global): Snare (high extraction)
  (institutional, global): Rope (net benefit)
  
Coupling detected: Power × Scope interaction
Factorization fails: χ(P,S) ≠ f(P) × g(S)

FNL detected → Reclassification: Constructed Constraint
```

**Why this matters:**

Catches modern physics-washing. "It's just how things are" rhetoric often hides extractive construction. Boltzmann test mathematically refutes the claim.

**See:** logic_extensions.md §1.4 for full FNL detection algorithm

**Implementation:** `signature_detection.pl`, `false_natural_law/2`

---

#### C.3 Coupling-Invariant Rope (CI_Rope) — Stage 7 Extension

**Conceptual Overview:**

Certification that a Rope is genuinely coordinating, not coordination-washing.

**Formal Definition:**
```
CI_Rope(C) ↔ boltzmann_compliant(C, compliant(_))
             ∧ scale_invariant(C)
             ∧ excess_extraction(C, X) ∧ X ≈ 0
             ∧ Coord(C)
```

**The four tests:**

1. **Factorization** (Boltzmann compliance)
2. **Scope invariance** (same type at all scopes)
3. **At Boltzmann floor** (ε ≈ theoretical minimum)
4. **Coordination function** (genuine value provision)

**Example:**
```
UTF-8 encoding:
  ✓ Factorizes (no Power × Scope coupling)
  ✓ Scope-invariant (works identically at local and global)
  ✓ At floor (ε = 0.12 vs floor = 0.02, excess within tolerance)
  ✓ Coordinates (universal text representation)
  
Certification: CI_Rope ✓
```

**Why this matters:**

Distinguishes true coordination (UTF-8) from coordination-washing (opt-out privacy defaults, behavioral nudges).

**See:** logic_extensions.md §1.5 for full CI_Rope certification process

**Implementation:** `signature_detection.pl`, `coupling_invariant_rope/2`

---

#### C.4 False CI_Rope (FCR) — Stage 7 Extension

**Conceptual Overview:**

Coordination-washing detector. Catches constraints that appear as Ropes from metrics but fail structural tests.

**Formal Definition:**
```
FCR(C) ↔ appears_as_rope(C)
         ∧ (¬boltzmann_compliant(C)
            ∨ ¬scale_invariant(C)
            ∨ excess_extraction(C, X) > threshold
            ∨ nonsensical_coupling(C))
```

**Detection logic:**

If a constraint:
- Passes Rope thresholds (χ ≤ 0.35, ε ≤ 0.45), BUT
- Fails any of the four CI_Rope tests

Then it's coordination-washing—extraction hidden behind low metrics.

**Example:**
```
Social media "free" platform:
  Appears: ε = 0.20, χ = 0.25 (looks like Rope)
  
  Tests:
    ✗ Factorization (couples Power × Network Scope)
    ✗ Scope variance (Rope locally, Snare globally)
    ✗ Excess extraction (ε = 0.20 vs floor = 0.02)
    ✗ Questionable coordination (optimizes engagement, not connection)
    
FCR detected → Reclassification: Tangled Rope
```

**Why this matters:**

Modern extraction techniques hide behind low ε:
- Behavioral defaults (opt-out, not opt-in)
- Distributed enforcement (no single suppression point)
- Asymmetric network effects

FCR catches these patterns.

**See:** logic_extensions.md §1.6 for full FCR detection

**Implementation:** `signature_detection.pl`, `false_ci_rope/2`

---

**[§II.C COMPLETE - Detection Pattern Operators]**

Four detection patterns defined:
- ✅ False Mountain (FM): High extraction contradicts natural claim
- ✅ False Natural Law (FNL): Boltzmann coupling reveals construction
- ✅ Coupling-Invariant Rope (CI_Rope): Certified true coordination
- ✅ False CI_Rope (FCR): Coordination-washing detector

**Next:** §II.D Power-Scaling Function (π values and rationale)

---

### D. Power-Scaling Function

The power-scaling function adjusts base extractiveness ε(C) based on structural power position via sigmoid directionality.

**Formula (implementation):**
```
χ(C, P, S) = ε(C) × f(d(P)) × σ(S)
```

Where f is a sigmoid function and d(P) is the derived directionality value for power position P (see constraint_indexing.pl:extractiveness_for_agent/3). The directionality d is resolved via: explicit override > beneficiary/victim structural derivation > canonical fallback.

**Simplified (for intuition):**
```
χ(C, P, S) ≈ ε(C) × π(P) × σ(S)
```

Where:
- **χ** = effective extractiveness (what you experience)
- **ε** = base extractiveness (structural property of constraint)
- **f(d(P))** = sigmoid-scaled directionality (actual implementation)
- **π(P)** ≈ f(d_canonical(P)) = power modifier at canonical directionality (approximation)
- **σ(S)** = scope modifier (verification difficulty)

---

#### Power Position Modifiers

| Position | π Value | Rationale |
|----------|---------|-----------|
| **powerless** | **1.5** | Extraction amplified—bear full cost, no deflection |
| **moderate** | **1.0** | Baseline—average agency |
| **powerful** | **0.6** | Extraction reduced—can deflect costs, influence rules |
| **organized** | **0.4** | Shared burden—collective action distributes costs |
| **institutional** | **-0.2** | Net beneficiary—extract MORE than pay |
| **analytical** | **1.15** | Analytical clarity—detects normalized extraction |

---

#### Rationale by Position

**π(powerless) = 1.5:**

Powerless agents bear amplified extraction because they cannot:
- Deflect costs to others
- Negotiate for better terms
- Exit without catastrophic loss
- Influence rule-making

**Example:** Payday loan (ε = 0.70)
- Powerless experiences: χ = 0.70 × 1.5 = 1.05 (extraction exceeds coordination)
- The 1.5 multiplier captures how lack of options amplifies harm

**π(moderate) = 1.0:**

Baseline. Moderate agents have:
- Some choices (but limited)
- Some negotiating power (but constrained)
- Some exit options (but costly)

This is the reference point for ε calibration.

**π(powerful) = 0.6:**

Powerful agents deflect ~40% of extraction via:
- Rule influence (shape constraints to benefit themselves)
- Cost transfer (push burden to others)
- Exit arbitrage (credible threat of leaving)

**Example:** Employment contract (ε = 0.50)
- Executive experiences: χ = 0.50 × 0.6 = 0.30 (manageable, even beneficial)
- Same contract that traps others becomes opportunity for powerful

**π(organized) = 0.4:**

Collective action distributes burden. Union members share:
- Legal costs (collective bargaining)
- Strike funds (mutual aid)
- Information (solidarity networks)

The 0.4 modifier reflects ~60% cost reduction from organization.

**π(institutional) = -0.2:**

**Negative value = net extraction FROM system.**

Institutional agents don't just pay less—they extract more than they contribute. The -0.2 captures:
- Rule-making privilege (create constraints that benefit you)
- Regulatory capture (shape enforcement to your advantage)
- Rent extraction (extract from system without providing value)

**Critical implementation note:** Handle sign carefully. Negative χ means net benefit, not impossibility.

**π(analytical) = 1.15:**

**The degeneracy-breaking value.**

At π = 1.0, analytical and moderate perspectives produced identical χ values, making them computationally indistinguishable.

At π = 1.15, we observe a critical pattern in **93 corpus constraints**:
- Moderate agents: χ = 0.50-0.60 (Tangled Rope, normalized as "just how things work")
- Analytical agents: χ = 0.58-0.69 (Snare threshold crossed, extraction detected)

**This is not "smarter people see more."** It's **freedom from normalization pressure.**

Analytical agents aren't subject to daily constraints that force rationalization:
- Don't need the job (can critique employment without risk)
- Don't need the platform (can analyze social media without using it)
- Don't need the system (can study markets without participating)

The 1.15 modifier captures detection of extraction that embedded agents must normalize to function.

**Example:** Credit card rewards system (ε = 0.55)
- Moderate: χ = 0.55 × 1.0 = 0.55 (Tangled—"I get rewards, but...")
- Analytical: χ = 0.55 × 1.15 = 0.63 (near Snare—"transfer from unsophisticated to sophisticated users")

Both true. Moderate agents can't see the full extraction (survival requires participation). Analytical agents detect the transfer mechanism.

---

#### Calibration and Limitations

**Empirical basis:** 691-constraint corpus (2024-2026)

**Known limitations:**

1. **Western bias**: Power modifiers calibrated primarily on US/European systems
   - Non-WEIRD validation needed
   - Cultural power dynamics may differ

2. **Temporal stability**: Modifiers may drift as power structures evolve
   - Requires periodic recalibration
   - Watch for organized → institutional transitions (labor movements, etc.)

3. **Granularity**: Six positions are coarse-grained
   - Finer distinctions possible (petit bourgeois vs haute bourgeois)
   - Current set captures 90%+ of variance

4. **Intersectionality**: Single power position may miss compounding effects
   - Analytical + powerless (academic precariat)
   - Institutional + organized (regulatory capture via industry groups)
   - Future versions may model power as vector, not scalar

**Implementation:**

```prolog
% config.pl, lines 130-136
param(power_modifier_powerless, 1.5).
param(power_modifier_moderate, 1.0).
param(power_modifier_powerful, 0.6).
param(power_modifier_organized, 0.4).
param(power_modifier_institutional, -0.2).
param(power_modifier_analytical, 1.15).
```

**Reference:** logic_thresholds.md §1 for complete power modifier registry

---

#### Coalition Power Resolution

When a `powerless` agent faces a snare-like constraint with enough identified victims, the system upgrades their effective power to `organized` — modeling the emergence of collective action capacity from shared structural harm.

**Predicate:** `resolve_coalition_power(+Power, +Constraint, -ResolvedPower)` in `constraint_indexing.pl:348-370`

**Preconditions (all must hold for upgrade):**
1. Input power = `powerless`
2. Base extraction ≥ `snare_epsilon_floor` (0.46) — constraint extracts enough to motivate collective action
3. Suppression ≥ `snare_suppression_floor` — active suppression creates shared grievance
4. Constraint has identified victims (`narrative_ontology:constraint_victim/2`)
5. Victim count ≥ `critical_mass_threshold` (config value: 3) — minimum coalition size

**Result:** `powerless` → `organized`. All other power levels pass through unchanged (fallthrough clause at line 370).

**Circularity avoidance:** The preconditions use snare-property heuristics (raw metric checks) rather than calling `dr_type/3`, which would create a circular dependency since coalition resolution feeds into `extractiveness_for_agent/3`, which feeds into `classify_from_metrics/6`.

**Effect on classification:** The upgrade changes the power input to the sigmoid directionality function, which changes χ. A constraint that classifies as snare from a powerless perspective may classify as rope or tangled_rope from the upgraded organized perspective — reflecting the real structural shift that collective action produces.

**Limitations:** Only upgrades from powerless (no downgrade mechanism). No time-horizon sensitivity — coalition strength is constant across all temporal perspectives. No modeling of coalition formation dynamics or fragility.

**Implementation:** Called by `extractiveness_for_agent/3` at `constraint_indexing.pl:381`.

---

#### Observer Accessibility & the Restriction Operator

The restriction operator formalizes the epistemic trap: a powerless observer experiences extraction (χ) but cannot decompose it into structural components. This is the mechanism that makes snares look like "just how things are."

**Predicate:** `observer_accessible(+C, +Context, -RestrictedView)` in `constraint_indexing.pl:663-681`

**Output structure:** `view(Chi, VisibleEps, VisibleSupp, VisibleTheater, KnownBeneficiaries, PerceivedMutability)` — where inaccessible features return `unknown` and felt-only features return the Chi proxy.

**Feature Accessibility Table:**

| Power Level   | raw_extraction | suppression | beneficiaries | alternatives | theater_ratio | cross_context |
|---------------|----------------|-------------|---------------|--------------|---------------|---------------|
| powerless     | none           | felt_only   | none          | none         | none          | none          |
| moderate      | partial        | partial     | partial       | partial      | partial       | none          |
| powerful      | partial        | partial     | partial       | full         | partial       | none          |
| organized     | partial        | full        | full          | full         | partial       | none          |
| institutional | full           | full        | full          | full         | full          | none          |
| analytical    | full           | full        | full          | full         | full          | full          |

**Access level semantics:**
- `full` — true metric value returned
- `partial` — true metric value returned (same data as full; precision difference is conceptual, not quantitative in current implementation)
- `felt_only` — Chi proxy returned (observer experiences the effect but cannot separate components)
- `none` — `unknown` returned (feature not accessible from this position)

**Beneficiary restriction:** At `partial` access, returns the first `max(1, N//2)` beneficiaries (half-knowledge). At `none`, returns empty list.

**Classification from restricted data:** `classify_from_restricted(+C, +Context, -RestrictedType)` (`constraint_indexing.pl:734-758`) applies a simplified threshold cascade using only observer-accessible features:

| Type          | Conditions                                              |
|---------------|---------------------------------------------------------|
| mountain      | supp ≤ 0.05 ∧ ε ≤ 0.25 ∧ mutability = mountain        |
| snare         | χ ≥ 0.66 ∧ ε ≥ 0.46 ∧ supp ≥ 0.60                     |
| rope          | χ ≤ 0.35 ∧ ε ≤ 0.45                                    |
| piton         | theater ≥ 0.70 ∧ χ ≤ 0.25 ∧ ε > 0.10                  |
| indeterminate | fallback                                                |

These thresholds are hardcoded (not config-driven), unlike the full `classify_from_metrics/6`.

**Testable prediction** (documented at `constraint_indexing.pl:589-590`): The set of constraints where `classify_from_restricted/3` differs from `dr_type/3` should match the set with `gauge_fixed = true`. The gap between restricted and full classification measures the epistemic cost of the observer's position.

**Called from:** `diagnostic_summary.pl:301`, `abductive_triggers.pl:884` (Trigger 15: epistemic trap detection), `quantum_verification_report.pl:345`.

---

**[§II.D COMPLETE]**

Power-scaling function defined with full rationale for all six modifiers, including:
- ✅ Amplification (powerless 1.5×)
- ✅ Deflection (powerful 0.6×)
- ✅ Collective burden-sharing (organized 0.4×)
- ✅ Net extraction (institutional -0.2×)
- ✅ Degeneracy-breaking (analytical 1.15×)

**Next:** §II.E Scope Modifiers (σ values and verification difficulty)

---

### E. Scope Modifiers

The scope modifier σ(S) adjusts extraction based on verification difficulty at different scales.

**Core insight:** Larger scope = more participants = harder to verify claims = easier to hide extraction.

---

#### Scope Modifiers Table

| Scope | σ Value | Rationale |
|-------|---------|-----------|
| **local** | **0.8** | Easy verification → extraction dampened |
| **regional** | **0.9** | Easier verification |
| **national** | **1.0** | Baseline |
| **continental** | **1.1** | Harder verification |
| **global** | **1.2** | Hardest verification → extraction amplified |
| **universal** | **1.0** | Neutral—natural laws are scope-invariant |

---

#### Rationale by Scope

**σ(local) = 0.8:**

Local scope enables direct verification:
- Know the players (Dunbar number not exceeded)
- See the effects firsthand (direct observation)
- Reputation mechanisms work (repeat interactions)
- Scams get caught quickly (word of mouth)

The 0.8 dampening reflects ~20% reduction in effective extraction due to transparency.

**Example:** Local farmers market
- Vendor overcharges once → everyone knows → vendor excluded
- ε might be 0.40 (base extraction from market structure)
- χ = 0.40 × 0.8 = 0.32 (dampened by local accountability)

**σ(regional) = 0.9:**

Verification harder but still feasible:
- State-level media coverage
- Regional reputation networks
- Regulatory oversight possible
- Travel enables personal verification

Slight dampening (10% reduction).

**σ(national) = 1.0:**

Baseline. National scope is the reference point for most calibration:
- Institutional mediators required (media, regulators)
- Direct verification impossible
- Reputation mechanisms weakened
- But still manageable scale

**σ(continental) = 1.1:**

Verification difficulty increases:
- Multiple languages and legal systems
- Cross-border enforcement gaps
- Information fragmentation
- Regulatory arbitrage possible

Amplification (10% increase).

**σ(global) = 1.2:**

**Maximum amplification from Dunbar number constraints.**

At 8 billion participants:
- Direct verification impossible
- Institutional mediators themselves unverifiable
- Claims about "global markets" or "planetary systems" cannot be individually checked
- Extraction hides in complexity and scale

The 1.2 multiplier (20% amplification) reflects empirically observed extraction increase at global scale.

**Example:** Global supply chain
- ε = 0.50 (base extraction—labor conditions, environmental costs)
- χ(powerless, global) = 0.50 × 1.5 × 1.2 = 0.90 (near maximum extraction)
- χ(institutional, global) = 0.50 × -0.2 × 1.2 = -0.12 (net extraction FROM system)

Same supply chain: Snare for workers, Rope for institutions.

**σ(universal) = 1.0:**

**Special case: Natural laws are scope-invariant.**

Universal scope applies to:
- Mathematical truths
- Physical laws
- Logical necessities

These constraints don't change with number of participants. Thermodynamics operates identically at local and global scales.

The 1.0 modifier (neutral) reflects zero verification difficulty—natural laws are self-verifying through observation.

**Example:** Second Law of Thermodynamics
- ε ≈ 0 (no extraction—just constraint on possibility)
- σ(universal) = 1.0
- χ same at all scopes (entropy increases regardless of scale)

---

#### The Dunbar Number Connection

**Why σ(global) = 1.2 specifically?**

Based on Dunbar number (~150 stable relationships):
- **Local** (< 150): All participants knowable → verification easy
- **Regional** (~1,000-10,000): Second-order networks → verification feasible
- **National** (~millions): Institutional mediation required → baseline difficulty
- **Global** (~billions): No direct verification path → maximum difficulty

The 1.2 value represents the empirically observed ceiling on verification-driven extraction amplification. Above this, additional scale doesn't further increase extraction (already at verification limit).

---

#### Calibration and Limitations

**Empirical basis:** 691-constraint corpus verification difficulty analysis

**Known limitations:**

1. **Information technology**: Digital verification may reduce σ values
   - Blockchain, transparency platforms
   - But also: deep fakes, information overload
   - Net effect unclear—requires recalibration

2. **Scope transitions**: Abrupt jumps between discrete levels
   - Reality is continuous (local → regional transition is gradual)
   - Six levels are practical compromise

3. **Cultural variation**: Verification difficulty varies by context
   - High-trust societies: lower σ values
   - Low-trust societies: higher σ values
   - Current values reflect Western moderate-trust baseline

4. **Temporal change**: Scope perception evolves
   - "Global" meant different thing in 1800 vs 2025
   - Requires periodic recalibration as communication technology changes

**Implementation:**

```prolog
% config.pl, lines 153-158
param(scope_modifier_local, 0.8).
param(scope_modifier_regional, 0.9).
param(scope_modifier_national, 1.0).
param(scope_modifier_continental, 1.1).
param(scope_modifier_global, 1.2).
param(scope_modifier_universal, 1.0).
```

**Reference:** logic_thresholds.md §2 for complete scope modifier registry

---

**[§II.E COMPLETE]**

Scope modifiers defined with verification difficulty rationale:
- ✅ Local dampening (0.8×, easy verification)
- ✅ Regional/national baseline (0.9×, 1.0×)
- ✅ Continental/global amplification (1.1×, 1.2×)
- ✅ Universal neutrality (1.0×, scope-invariant natural laws)
- ✅ Dunbar number connection explained

---

**[§II COMPLETE - Basic Syntax]**

All components of indexed constraint operators now defined:
- ✅ §II.A: Index structure (P, T, E, S)
- ✅ §II.B: Six core modal operators (■, ⊞, ⊠, ⊞⊠, ⊡, ⊟)
- ✅ §II.C: Four detection patterns (FM, FNL, CI_Rope, FCR)
- ✅ §II.D: Power-scaling function (π modifiers)
- ✅ §II.E: Scope modifiers (σ modifiers)

**Total §II word count:** ~17,000 words (comprehensive reference)

**Next section:** §III Temporal Logic & Lifecycle States

---

## III. Temporal Logic & Lifecycle States

### A. Temporal Operators (Indexed)

Standard temporal logic operators apply to indexed constraints. **Critical:** These operators are **not universal**—they depend on your temporal index (I.T) and exit options (I.E).

---

#### Always (□)

```
□φ[I] ≡ ∀t: φ(t)[I]
```

"Always φ from index I" means φ holds at all times **accessible from your index**.

**Indexical variation:**

```
□⊠C[(powerless, immediate, trapped, local)]
```
"This constraint is always a Snare from my perspective."

- Time horizon: immediate (can't plan beyond next paycheck)
- Exit options: trapped (no alternatives)
- Result: constraint appears immutable and extractive

**But the same constraint:**

```
◊⊞C[(institutional, historical, mobile, global)]
```
"This constraint is eventually a Rope from institutional perspective."

- Time horizon: historical (centuries-scale planning)
- Exit options: mobile (can exit or restructure)
- Result: constraint appears reformable

**Both true.** "Always" and "Eventually" depend on temporal position, not just constraint.

**This resolves the paradox of hope:** Hope is a function of your temporal index. From trapped + immediate, hope may be mathematically unjustified (□⊠ is objectively true). From mobile + generational, hope is rational (◊⊞ is achievable).

---

#### Eventually (◊)

```
◊φ[I] ≡ ∃t: φ(t)[I]
```

"Eventually φ from index I" means φ holds at some future time **accessible from your trajectory**.

**Critical guard condition:**

```
If E = trapped ∧ ¬Transition(C) → ◊Exit is FALSE
```

If you're trapped and the constraint type doesn't transition (e.g., Snare remains Snare), eventual exit is **mathematically impossible** from your index. ◊ is not wish—it's reachable trajectory.

**Example:**

```
Powerless worker in company town:
  E = trapped (no jobs elsewhere, can't relocate)
  C = employment_contract (Snare from this index)
  
  ◊Exit? Only if:
    1. Constraint transitions (Snare → Rope via reform), OR
    2. Index transitions (trapped → mobile via collective action)
    
  Without transition: ◊Exit = FALSE (objectively, not pessimism)
```

This is why collective action matters. It's not just psychological—it's the only path that makes ◊Exit true.

---

#### Next (○)

```
○φ[I] ≡ φ(t+1)[I]
```

"Next step, φ holds from index I."

Useful for state transition modeling (see §III.B).

---

#### Until (U) and Since (S)

```
φ U ψ[I] ≡ ∃t': ψ(t')[I] ∧ ∀t < t': φ(t)[I]
```

"φ holds until ψ occurs from index I."

```
φ S ψ[I] ≡ ∃t': ψ(t')[I] ∧ ∀t > t': φ(t)[I]
```

"φ holds since ψ occurred from index I."

**Example:**

```
Employment Snare Until Collective Bargaining:
  ⊠(employment) U ⊞(employment)[(powerless, biographical, trapped, local)]
  
  Means: Employment is Snare until unionization succeeds,
         after which it becomes Rope (from this index)
```

---

### B. Lifecycle States & Drift Types

Constraints are not static. They **degrade over time** through drift. The lifecycle model tracks state transitions between the seven types.

**Metaphor:** Drift is the **weather** of constraint-space. You don't just find a Piton on the mountain—you watch a Rope rot into one. Entropy is real.

---

#### State Transition Rules

**The Seven Classical Transitions:**

```
T1: ⊞ → ⊞⊠  (Rope degradation via extraction accumulation)
T2: ⊞⊠ → ⊠   (Tangled → Snare via coordination loss)
T3: ⊞ → ⊡    (Rope → Scaffold via sunset acceptance)
T4: ⊡ → ⊟    (Scaffold → Piton via sunset violation)
T5: ⊞⊠ → ⊟   (Tangled → Piton via extraction collapse)
T6: ⊠ → ⊟    (Snare → Piton after extraction dried up)
T7: ■ ↛ *    (Mountains don't transition—unchangeable by definition)
```

**Transition Irreversibility (Entropic Direction):**

```
Rope → Snare: Low-energy natural decay (entropy)
Snare → Rope: Massive agency injection + Scaffold required
```

Degradation is **thermodynamically favored**. Reform fights entropy. This is why:
- Drift happens passively (just wait, constraints degrade)
- Reform requires active energy (must fight natural decay)
- Scaffold required for Snare → Rope (can't jump directly)

**Visual state diagram:**

```
         ┌──────┐
         │  ■   │ (Mountains: unchangeable)
         └──────┘
              
    ┌─────┐  T1   ┌────┐  T2   ┌────┐  T6   ┌────┐
    │  ⊞  │ ───→  │ ⊞⊠ │ ───→  │ ⊠  │ ───→  │ ⊟  │
    └─────┘       └────┘       └────┘       └────┘
       │  T3         │  T5                      ▲
       ▼             ▼                          │
    ┌─────┐  T4   ┌────────────────────────────┘
    │  ⊡  │ ───→  │ (Theater increases)
    └─────┘       
```

---

#### The Eleven Drift Types

**Classical Drift (Types 1-7):** Metric-based transitions

**Type 1: Metric Substitution (MS)**
- Good metrics replaced with proxies
- Example: "Test scores" replace "learning"
- Result: Goodhart's Law (gaming the metric)

**Type 2: Extraction Accumulation (EA)**
- ε increases over time (T1: ⊞ → ⊞⊠)
- Example: Free platform adds ads, then tracking, then manipulation
- Mechanism: Ratchet effect (extraction easier to add than remove)

**Type 3: Coordination Loss (CL)**
- Coord(C) degrades (T2: ⊞⊠ → ⊠)
- Example: Professional association becomes lobbying group
- Original purpose lost, extraction remains

**Type 4: Function Obsolescence (FO)**
- Constraint's purpose gone but structure persists
- Example: Regulations for technology that no longer exists
- Theater ratio increases

**Type 5: Sunset Violation (SV)**
- Scaffold sunset clause ignored (T4: ⊡ → ⊟)
- Example: "Temporary" powers become permanent
- **Critical observation:** Theater ratio spikes exactly when sunset missed
- Mechanism: Function gone, but bureaucratic inertia maintains form

**Type 6: Extraction Collapse (EC)**
- ε drops rapidly (T5: ⊞⊠ → ⊟ or T6: ⊠ → ⊟)
- Constraint no longer profitable to enforce
- Example: Prohibition after repeal (still on books, no enforcement)

**Type 7: Algorithm Shutdown → Internalized Piton (AS)**
- Algorithmic constraint becomes habitual
- Example: Factory rhythm internalized by workers
- Mechanism: External constraint removed, behavior persists

---

**Structural Physics Drift (Types 8-11):** Boltzmann/Purity/Network pathologies

**Type 8: Coupling Drift (CD)**

Independent dimensions start entangling over time.

```
CD(C, t_drift) ≡
    cross_index_coupling(C, CurrentCoupling)
    ∧ CurrentCoupling > boltzmann_coupling_threshold (0.25)
    ∧ metric_trend(C, base_extractiveness, increasing)
```

> **Note (February 2026):** The implementation uses an absolute coupling threshold (`boltzmann_coupling_threshold` = 0.25), not the delta-based 0.10 threshold from the original spec. See logic_extensions.md §4.1 for details.

**Example:** App starts as messaging tool (ε = 0.10), gradually couples: messaging + location + contacts + camera + microphone (ε = 0.45). Coupling serves extraction, not coordination.

**See:** logic_extensions.md §4.1

---

**Type 9: Boltzmann Floor Drift (BFD)**

Minimum necessary extraction rises (legitimate complexity increase, not extractive drift).

```
BFD(C, t_drift) ≡ 
    BoltzmannFloor(C, t_drift) > BoltzmannFloor(C, t₀)
    ∧ ε(C, t_drift) tracks new floor
    ∧ (ε(C) - Floor) remains constant
```

**Example:** Local power grid → regional grid. Floor rises from 0.05 to 0.12, but ε tracks floor (excess extraction stays constant). Complexity increased, not extractiveness.

**See:** logic_extensions.md §4.2

---

**Type 10: Purity Drift (PD) — Pre-Symptomatic Decay**

**Most critical v4.0 addition.** Structural health declines even when χ and ε appear stable.

```
PD(C) ≡ 
    purity_score(C, t) declining
    ∧ ¬(ε(C, t) rising significantly)
    ∧ ¬(χ(C, t) rising significantly)
```

**The danger:** Metrics look fine, but Boltzmann compliance is failing. This is **pre-symptomatic**—decay is happening before metric-based detection.

**Four signals:**
1. Factorization declining (coupling score rising)
2. Nonsensical coupling detected
3. Theater ratio increasing
4. Excess extraction above Boltzmann floor

**Any one signal triggers purity drift detection.**

**Example:** 
```
Year 1: Carbon credits
  ε = 0.55, χ = 0.60 (moderate, national)
  Purity = 0.72 (sound)
  Coupling = 0.12 (clean factorization)

Year 5: Same metrics externally
  ε = 0.55, χ = 0.60 (unchanged!)
  Purity = 0.42 (contaminated)
  Coupling = 0.28 (nonsensical coupling emerging)
  
Purity drift detected ← Early warning before type flip
```

**See:** logic_extensions.md §4.3 for full purity drift detection

---

**Type 11: Network Drift (ND) — Contagion**

**Induced degradation from neighbors.** Constraint gets worse because its context gets worse.

```
ND(C, t_drift) ≡ 
    intrinsic_purity(C, t) stable
    ∧ effective_purity(C, t) declining
    ∧ has_drifting_neighbor(C)
```

**The mechanism:** Low-purity constraints contaminate neighbors. Contamination flows **downward only** (Snare → Rope, never Rope → Snare). Entropy increases.

**Example:**
```
Constraint B (employment contract):
  Intrinsic purity = 0.65 (stable)
  
Constraint A (housing market) drifts:
  Purity drops 0.70 → 0.25 (becomes Snare)
  
Network effect on B:
  Effective purity = 0.65 - contamination_pressure(A)
  Effective purity = 0.65 - 0.20 = 0.45
  
B crosses from borderline → contaminated
Action recommendation escalates: monitor → reform urgently
```

**Predictive modeling (Stage 9):** Network drift velocity allows **cascade prediction**. If A continues drifting at current rate, we can predict when B will cross purity thresholds.

**See:** logic_extensions.md §4.4 for network drift velocity calculation

---

**Drift Summary Table:**

| Type | Name | Mechanism | Detection |
|------|------|-----------|-----------|
| 1 | Metric Substitution | Proxy replacement | Goodhart's Law |
| 2 | Extraction Accumulation | ε increases | T1: ⊞ → ⊞⊠ |
| 3 | Coordination Loss | Coord(C) fails | T2: ⊞⊠ → ⊠ |
| 4 | Function Obsolescence | Purpose gone | Theater rises |
| 5 | Sunset Violation | Scaffold persists | T4: ⊡ → ⊟ |
| 6 | Extraction Collapse | ε drops rapidly | T5/T6 → ⊟ |
| 7 | Algorithm Shutdown | External → internal | Habit formation |
| **8** | **Coupling Drift** | Dimensions entangle | Coupling score ≥ 0.10 |
| **9** | **Boltzmann Floor Drift** | Complexity rises | Floor rises, excess constant |
| **10** | **Purity Drift** | Pre-symptomatic decay | Purity declining, metrics stable |
| **11** | **Network Drift** | Neighbor contagion | Effective purity < intrinsic |

---

**[§III COMPLETE - Temporal Logic & Lifecycle]**

Temporal operators and drift types defined:
- ✅ Indexed temporal operators (□, ◊, ○, U, S)
- ✅ Hope as function of temporal index
- ✅ E=trapped guards on ◊Exit
- ✅ Seven classical transitions (T1-T7)
- ✅ Transition irreversibility (entropy favors degradation)
- ✅ Eleven drift types (1-7 classical, 8-11 structural physics)
- ✅ Purity drift as pre-symptomatic decay
- ✅ Network drift as contagion
- ✅ Cross-references to logic_extensions.md

**Next section:** §IV Inference Rules & Priority Ordering

---

## IV. Inference Rules & Priority Ordering

### A. The Canonical Predicate

**Single source of truth:** `classify_from_metrics/6` in drl_core.pl (lines 2936-3000)

```prolog
classify_from_metrics(C, BaseEps, Chi, Supp, Context, Type)
```

**Parameters:**
- `C`: Constraint identifier
- `BaseEps`: Base extractiveness ε(C)
- `Chi`: Power-scaled extractiveness χ(C, I.P, I.S)
- `Supp`: Suppression requirement
- `Context`: Index tuple (Power, TimeHorizon, Exit, Scope)
- `Type`: Classified type (output)

**Discipline:** All threshold logic flows through this predicate. No hardcoded thresholds elsewhere. All values from config.pl → logic_thresholds.md.

---

### B. Priority Ordering

Classification checks types in strict order (first match wins):

**┌─────────────────────────────────────────────────────────┐**
**│ PRIORITY ORDERING (First Match Wins):                  │**
**│ Mountain > Snare > Scaffold > Rope > Tangled > Piton > Naturalized   │**
**└─────────────────────────────────────────────────────────┘**

**Why this ordering?**

1. **Mountain first:** Most restrictive gate (ε ≤ 0.25, Supp ≤ 0.05, immutable). Must rule out before considering changeable types.

2. **Snare before Scaffold:** High extraction should be flagged before low-extraction temporaries. Safety-critical to catch Snares.

3. **Scaffold before Rope:** Explicit sunset takes priority. If something has built-in expiration, classify it as temporary even if it looks like permanent Rope.

4. **Rope before Tangled:** Pure coordination preferred over hybrid. If passes strict Rope thresholds, don't check weaker Tangled thresholds.

5. **Tangled before Piton:** Active hybrid beats degraded state. Tangled Ropes still coordinate (despite extraction); Pitons are theater.

6. **Piton before unknown:** Explicitly degraded beats unclassifiable. If theater ratio high, it's a known degradation, not mysterious failure.

**Implementation:**

Each type checks its conditions with cut (`!`) at end. First successful check terminates search. This is why order matters.

---

### C. The Two-Regime Architecture

**Regime 1: Metrics (drl_core.pl)**

`classify_from_metrics/6` uses threshold logic:
- Compares ε, χ, Supp against configured thresholds
- Checks immutability gates
- Fast, deterministic classification
- Priority: Mountain > Snare > Scaffold > Rope > Tangled > Piton > Naturalized

**Regime 2: Signatures (signature_detection.pl)**

`constraint_signature/2` can override metric classification:
- Runs Boltzmann compliance tests
- Detects False Natural Laws (FNL)
- Certifies Coupling-Invariant Ropes (CI_Rope)
- Detects False CI_Ropes (FCR)
- Slower (requires full index grid evaluation), but catches structural properties

**Resolution:**

```
final_type(C, Context) = 
    integrate_signature_with_modal(
        C, 
        metric_type(C), 
        signature_type(C)
    )
```

**Override rules:**
- **FNL fires**: Mountain → Tangled Rope (physics-washed)
- **CI_Rope fires**: Rope → Rope (certified, no change in type but flagged)
- **FCR fires**: Rope → Tangled Rope (coordination-washed)
- **NL signature + Mountain**: Keep Mountain (natural law confirmed)
- **CS signature + Rope**: Keep Rope (coordination scaffold confirmed)

**Shadow Mode Discipline:**

Signatures do NOT modify `classify_from_metrics/6`. They operate afterward, providing enhancement or override. Core classification logic remains stable and unchanged.

**See:** logic_extensions.md §6.2 for full two-regime architecture details

---

### D. Inference Rule Examples

**Rule M (Mountain):**
```
ε(C) ≤ 0.25 ∧ Supp(C) ≤ 0.05 ∧ Immutable(C, I.T, I.E)
─────────────────────────────────────────────────────
                    ■C[I]
```

**Rule R (Rope):**
```
χ(C, I.P, I.S) ≤ 0.35 ∧ ε(C) ≤ 0.45 ∧ Changeable(C, I.T, I.E)
──────────────────────────────────────────────────────────────
                         ⊞C[I]
```

**Rule N (Snare):**
```
χ(C, I.P, I.S) ≥ 0.66 ∧ ε(C) ≥ 0.46 ∧ Supp(C) ≥ 0.60 ∧ Changeable(C, I.T, I.E)
────────────────────────────────────────────────────────────────────────────────
                                 ⊠C[I]
```

**Rule TR (Tangled Rope):**
```
0.40 ≤ χ(C, I.P, I.S) ≤ 0.90 ∧ ε(C) ≥ 0.30 ∧ Supp(C) ≥ 0.40
∧ Enforce(C) ∧ Coord(C) ∧ Asymmetric(C)
───────────────────────────────────────────────────────────
                      ⊞⊠C[I]
```

**Rule SC (Scaffold):**
```
χ(C, I.P, I.S) ≤ 0.30 ∧ Coord(C) ∧ Sunset(C) ∧ Theater(C) ≤ 0.70
──────────────────────────────────────────────────────────────────
                           ⊡C[I]
```

**Rule Z (Piton):**
```
χ(C, I.P, I.S) ≤ 0.25 ∧ ε(C) > 0.10 ∧ Theater(C) ≥ 0.70
─────────────────────────────────────────────────────────
                        ⊟C[I]
```

---

**[§IV COMPLETE - Inference Rules]**

Priority ordering and two-regime architecture defined:
- ✅ Canonical predicate: classify_from_metrics/6
- ✅ Priority: Mountain > Snare > Scaffold > Rope > Tangled > Piton > Naturalized
- ✅ Two-regime: Metrics (fast) → Signatures (structural)
- ✅ Shadow mode discipline maintained
- ✅ Override rules specified

**Next section:** §V Error Taxonomy & Misclassification

---

## V. Error Taxonomy & Misclassification

Misclassification has consequences. The six error types:

### Type I: False Mountain (Treating Changeable as Unchangeable)

**Pattern:** Accepting constructed constraints as natural laws.

**Consequence:** Wasted energy accepting reformable constraints. Premature surrender.

**Example:** "Income inequality is inevitable" (claimed Mountain, actually Snare from powerless index, Rope from institutional). Accepting as Mountain blocks reform efforts.

**Detection:** FNL signature (physics-washing), high ε > 0.70

**Indexical note:** From some indices, constraint genuinely IS a Mountain (biographical + trapped = unchangeable within lifetime). Error is claiming universal unchangeability.

**Empirical findings (corpus audit, February 2026):** Type I is the primary empirically confirmed failure mode. 252 of 594 files flagged by the Prolog engine's False Mountain report; 96 with base extractiveness > 0.25 directly violating Mountain definition. Three threshold rules formalize detection:
- ε > 0.25 incompatible with Mountain (Mountains have near-zero extraction)
- Theater ratio > 0.50 incompatible with Mountain (natural laws don't need theater)
- `requires_active_enforcement` incompatible with Mountain (natural laws enforce themselves)

Confirmed worst cases: AI professional displacement classified as "structural market law" (Mountain) by an LLM with ε=0.71, TR=0.55, requires_active_enforcement=true (self-referential blind spot). Xi Jinping's ideological centralization classified as Mountain with ε=0.75, TR=0.82 (dominant cultural narrative). Golden handcuffs (negotiable employment contract) classified as Mountain from powerless perspective with ε=0.40 (complexity collapse). All three caught by threshold rules above. See `limitations.md` for full audit narrative.

---

### Type II: Mountain Denial (Treating Unchangeable as Changeable)

**Pattern:** Attempting to reform genuine natural laws.

**Consequence:** Wasted energy fighting entropy, thermodynamics, logical necessity.

**Example:** Perpetual motion machines (deny Second Law), flat earth (deny geometry), Ponzi schemes (deny mathematical limits).

**Detection:** Boltzmann compliance + temporal stability + zero beneficiaries confirm true Mountain

**Energy cost:** Extremely high (infinite if genuinely impossible)

---

### Type III: Snare-as-Rope (Missing Extraction)

**Pattern:** Normalizing extraction as coordination.

**Consequence:** Voluntary participation in own exploitation. Stockholm syndrome at systemic level.

**Example:** "Company culture" masking wage theft, "market efficiency" hiding rent extraction, "innovation" justifying precarity.

**Detection:** Check ε (base extraction) even when χ appears low from your index

**Why dangerous:** Most common error for moderate/powerful agents (they don't feel the extraction, classify as Rope)

---

### Type IV: Rope-as-Snare (Missing Coordination)

**Pattern:** Treating genuine coordination as extraction.

**Consequence:** Destroying beneficial constraints. Throwing out baby with bathwater.

**Example:** Anti-vaxxers (treat public health coordination as control Snare), libertarians destroying public goods (treat taxes as pure extraction).

**Detection:** Check for Coord(C) function. CI_Rope certification prevents this error.

**Energy cost:** High (rebuilding coordination is expensive)

---

### Type V: Piton-as-Active (Maintaining Dead Constraints)

**Pattern:** Treating degraded theater as functional coordination or active threat.

**Consequence:** Wasted energy maintaining or fighting irrelevant structures.

**Example:** Enforcing obsolete regulations (costs energy, provides no value), campaigning against dead letter laws.

**Detection:** Theater ratio ≥ 0.70, χ ≤ 0.10 (inactive)

**Action:** Bypass, not maintain or cut

---

### Type VI: Tangled Rope Mishandling

**Three sub-errors:**

**VI.a: Tangled-as-Rope (Ignoring Extraction)**
- Consequence: Accepting asymmetric costs as symmetric coordination
- Example: "We all benefit from growth" (ignoring who captures gains)

**VI.b: Tangled-as-Snare (Ignoring Coordination)**
- Consequence: Cutting beneficial constraints because extraction exists
- Example: Abolishing flawed but functional institutions without replacement

**VI.c: Wrong Reform Strategy**
- Consequence: Attempting surgical reform when purity < 0.30 (composition gate blocks it)
- Should transition to Cut/Exit instead
- See logic_extensions.md §5.3 for composition gates

---

**Error Consequence Table:**

| Type | Pattern | Energy Sink | Cascade Risk |
|------|---------|-------------|--------------|
| **I: False Mountain** | Accept changeable as unchangeable | Normalization → Drift T2 (EA) | High |
| **II: Mountain Denial** | Reform unchangeable | Infinite energy loss | Catastrophic |
| **III: Snare-as-Rope** | Normalize extraction | Voluntary exploitation | Medium |
| **IV: Rope-as-Snare** | Destroy coordination | Coordination rebuild cost | High |
| **V: Piton-as-Active** | Maintain dead theater | Agency heat sink | Low (but wasteful) |
| **VI: Tangled Mishandling** | Wrong reform strategy | Reform failure → disillusionment | Variable |

**Energy sink explanation:**
- **Infinite loss** (Type II): Attempting impossible (perpetual motion) → unbounded waste
- **Agency heat sink** (Type V): Energy absorbed by entropy, produces no work
- **Cascade risk**: Probability error triggers further errors (Type I → T2 drift → Type III)

---

**[§V COMPLETE - Error Taxonomy]**

Six misclassification types with consequences:
- ✅ Type I: False Mountain (acceptance error) — **primary empirical failure mode, 252 files flagged**
- ✅ Type II: Mountain Denial (reform futility)
- ✅ Type III: Snare-as-Rope (normalization)
- ✅ Type IV: Rope-as-Snare (coordination destruction)
- ✅ Type V: Piton-as-Active (energy waste)
- ✅ Type VI: Tangled mishandling (reform strategy error)

### Verification Architecture for Error Detection

The Prolog implementation operates as a three-layer verification system:

**Layer 1 — Structural Linter (pre-Prolog):** Gates whether files load. Checks syntax, required fields, namespace validation (`narrative_ontology:` prefix on beneficiary/victim declarations), claim value membership in {natural_law, coordination, constructed, enforcement}. Mechanical. Complete within its scope. Cannot catch classification errors because it does not evaluate content.

**Layer 2 — Classification Engine (Prolog):** Runs indexed classification across perspectives, computes perspectival gaps (χ divergence), generates reports (false_mountain_report, gap_report, snare_report). Deterministic. Catches what rules encode. The False Mountain report is Layer 2's primary error-detection output — it flags all cases where perspectives disagree about Mountain status.

**Layer 3 — Meta-Engine (second Prolog system):** Operates on Layer 2's outputs as its own fact base. Different ontology, different rules, different update cycle. Partitions Layer 2's findings into actionable categories:
- **Category A (Naturalization Errors):** Mountain + ε > 0.25 + requires_active_enforcement → auto-queue for regeneration
- **Category B (Theater-Mountain Conflicts):** Mountain + TR > 0.50 → flag for review
- **Category C (Legitimate Perspectival Gaps):** Mountain + ε ≤ 0.25 + no enforcement → no action needed (e.g., mathematical theorems experienced differently by power level)
- **Category D (WHO Assignment Suspects):** High extraction + declared beneficiary/victim but no powerless/institutional gap → human review
- **Category E (Structural Defects):** Illegal claim values, missing theater ratio → fixable without regeneration
- **Category F (Corpus-Wide Bias):** Domain naturalization rates, model naturalization rates → research findings

**Design principle:** Layer 2 classifies what the generating LLM gave it without judging. Layer 3 judges whether Layer 2's inputs made sense. This preserves the generating LLM's narrative biases as observable data rather than filtering them at generation time.

**Next section:** §VI Decision Logic & Action Routing

---

## VI. Decision Logic & Action Routing

Action flows from classification. Base routing:

| Type | Action | Energy | Priority |
|------|--------|--------|----------|
| **Mountain (■)** | Accept | Zero (don't fight) | Conserve for changeable |
| **Rope (⊞)** | Maintain | Low (preserve) | Protect from drift |
| **Snare (⊠)** | Cut/Exit | High (if trapped) | Escape extraction |
| **Tangled (⊞⊠)** | Surgical Reform | Medium-High | Preserve Coord, cut extraction |
| **Scaffold (⊡)** | Monitor Sunset | Low (watch) | Ensure dissolution |
| **Piton (⊟)** | Bypass | Minimal (ignore) | Route around |

---

### A. Action Qualifiers (Stage 7 Extensions)

**Base actions modified by structural health:**

**Purity-Qualified Actions:**

```
purity_qualified_action(C, Context, BaseAction, Qualifier, Priority)
```

Qualifiers:
- **stable** (purity ≥ 0.70): Continue base action
- **monitor** (0.50-0.70): Watch for decline
- **escalate_reform** (0.30-0.50): Reform urgently
- **degraded** (< 0.30): Base action blocked, switch to Cut/Exit

**Example:**
```
Tangled Rope with purity 0.65: Reform (viable)
Tangled Rope with purity 0.25: Reform blocked → Cut/Exit
```

**See:** logic_extensions.md §5.1 for purity-qualified action details

---

### B. Energy Scaling by Purity

Reform cost increases as structural health declines:

```
cost(surgical_reform, C) = base_cost × purity_multiplier(purity_score(C))
```

**Purity multiplier:**
```
M(p) = 1 + (3.0 - 1) × (1 - p)²
```

**Examples:**
- Purity 0.90 → M = 1.01× (almost no overhead)
- Purity 0.50 → M = 1.50× (moderate overhead)
- Purity 0.31 → M = 2.16× (expensive!)

**Agency Heat Sink (p < 0.30):**

When purity drops below 0.30, the constraint becomes an **agency heat sink**—a degraded system that absorbs reform energy without producing coordination work. The system's entropy consumes the work intended for structural improvement.

This is why composition gates block surgical reform at p < 0.30. Not because reform is "hard"—because the degraded structure literally radiates away the intervention energy as waste heat (bureaucratic friction, factional conflicts, extraction recapture).

**Implication:** At purity < 0.30, reform becomes prohibitively expensive → composition gate blocks surgical reform.

**See:** logic_extensions.md §5.2 for energy scaling formula

---

### C. Composition Gates

Safety checks preventing harmful actions:

```
can_perform(surgical_reform, C) ← purity(C) ≥ 0.30
can_perform(safe_scaffold_transition, C) ← purity(C) ≥ 0.50
can_perform(efficient_coordination, C) ← purity(C) ≥ 0.70
```

If purity too low, action blocked. Must transition to different strategy (Cut/Exit instead of Reform).

**See:** logic_extensions.md §5.3 for composition gates

---

### D. Network-Qualified Actions

Context-aware decisions based on effective purity (includes neighbor contamination):

```
network_qualified_action(C, Context, BaseAction, NetworkQualifier, Priority)
```

If effective_purity significantly < intrinsic_purity (drop ≥ 0.05), escalate priority due to network contamination.

**Example:**
```
Constraint B:
  Intrinsic purity: 0.65 (sound)
  Effective purity: 0.48 (borderline, due to neighbor A degradation)
  
Action: Escalate from "monitor" to "reform urgently"
Rationale: Network contamination pushing toward threshold
```

**See:** logic_extensions.md §5.4 for network-qualified actions

---

**[§VI COMPLETE - Decision Logic]**

Action routing with Stage 7-9 extensions:
- ✅ Base action table (Mountain → Accept, Rope → Maintain, etc.)
- ✅ Purity-qualified actions (degraded → blocked)
- ✅ Energy scaling (purity < 0.30 → 2×+ cost)
- ✅ Composition gates (surgical reform requires purity ≥ 0.30)
- ✅ Network-qualified actions (effective vs intrinsic purity)

**Next section:** §VII Containment Logic (Structural Paradoxes)

---

## VII. Containment Logic (Structural Paradoxes)

Some tensions are **irreducible**. They cannot be resolved, only contained.

### A. The Containment Protocol

**Formal statement:**

```
Contain(C) ← Irreducible(C) ∧ Paradoxical(C) ∧ ¬Resolvable(C)
```

When facing structural paradoxes:
1. **Acknowledge** the paradox exists (don't deny)
2. **Map** the trade-offs explicitly
3. **Oscillate** between poles as context shifts
4. **Don't force resolution** (attempting to "solve" causes collapse)

---

### B. Examples of Structural Paradoxes

**Arrow's Impossibility Theorem (Social Choice)**
- No voting system satisfies all fairness criteria simultaneously
- Must choose which criteria to prioritize
- Different contexts → different trade-offs
- **Containment**: Accept that "perfect democracy" is impossible, optimize for context

**Gödel's Incompleteness (Formal Systems)**
- Sufficiently powerful formal systems can't be both complete and consistent
- Must choose: completeness OR consistency, not both
- **Containment**: Accept limitation, use appropriate system for context

**Heisenberg Uncertainty (Quantum Measurement)**
- Position and momentum cannot both be precisely known
- Measurement itself changes system
- **Containment**: Accept fundamental limit, choose measurement based on need

---

### C. Tangled Ropes as Irreducible Hybrids

Some Tangled Ropes cannot be "untangled":

```
∃C: ⊞⊠C[I] ∧ ¬Separable(Coord(C), Extract(C))
```

**Example:** Employment under capitalism
- Coordination (income, skill-building, structure)
- Extraction (surplus value, agency restriction)
- **Cannot separate**: Employment IS the hybrid

**Containment strategy: Strategic Oscillation**

For irreducible hybrids, the goal is NOT surgical reform (separation impossible) but **strategic oscillation**:
- Accept hybrid nature (coordination AND extraction intertwined)
- Oscillate between poles as context shifts (emphasize coordination when possible, resist extraction when necessary)
- Optimize ratios over time (increase coordination percentage, reduce extraction percentage)
- Don't expect pure Rope (would require different economic system)

**Example:** Labor organizing in Tangled employment
- Recognize employment as irreducible hybrid
- Oscillate: Defend coordination value (job security, benefits) while resisting extraction (wage theft, speedup)
- Long-term: Shift ratio toward coordination (worker ownership, profit-sharing)
- Don't wait for "perfect" pure Rope—work within hybrid constraint

This differs from surgical reform (which assumes separability). Strategic oscillation accepts inseparability but refuses stasis.

---

### D. Omega Variables (Systemic Uncertainties)

When analysis hits irreducible uncertainty, route to **Omega variables**:

```
Ω₁: Unknown unknowns (structural blind spots)
Ω₂: Measurement limits (can't observe without changing)
Ω₃: Computational intractability (knowable but not computable)
Ω₄: Fundamental paradoxes (logical impossibilities)
```

**Usage:** Acknowledge limit, flag for meta-analysis, don't pretend certainty.

**See:** Omega variables track what we CAN'T know, preventing false confidence.

#### Mandatrophy Reconciliation

A **mandatrophy** is a specialized omega: a Mountain (natural fact) that functions as a Snare (extraction trap). The paradox arises when a constraint is classified as mountain (immutable) but has extractiveness > 0.7 — extraction levels normally associated with snares.

**Detection:** `detect_omega(+Name, -mandatrophy)` in `narrative_ontology.pl:303-310` succeeds when:
1. Constraint classified as mountain via `constraint_classification/3`
2. Extractiveness > 0.7 (hardcoded, not config-parameterized)
3. NOT `is_mandatrophy_resolved(Name)`

**Resolution:** Two hardcoded resolutions exist (`narrative_ontology.pl:298-299`):
- `is_mandatrophy_resolved(gale_shapley)` — "The Algorithm is the Mandate." The matching algorithm imposes costs (ε > 0.7) but is structurally immutable in the same way gravity is — no agent chose this mechanism.
- `is_mandatrophy_resolved(planetary_boundaries)` — "The Biological Limit is the Mandate." Carrying capacity extracts from those who exceed it, but the limit is a natural fact.

**Two resolution paths:** The system checks for mandatrophy resolution via two independent attribute lookups:
1. `has_mandatrophy_declaration/1` — checks `attribute(C, lifecycle, mandatrophy)` (used by `check_indexical_relativity/1`)
2. `is_indexical_resolution_declared/1` — checks `attribute(ID, indexical_resolution, resolved)` (used by `detect_mandatrophy_omega/1`)

**Aggregation:** `count_unresolved_omegas(-Count)` uses `aggregate_all/3` to count system-wide unresolved mandatrophy omegas.

**Implementation:** `narrative_ontology.pl:94-327`. The 0.7 threshold appears in three places (lines 277, 305, 320) and is not parameterized in `config.pl`.

---

**[§VII COMPLETE - Containment Logic]**

Structural paradoxes and containment protocol:
- ✅ Containment vs resolution distinction
- ✅ Arrow, Gödel, Heisenberg as examples
- ✅ Tangled Ropes as irreducible hybrids
- ✅ Omega variables for systemic uncertainty

**Next section:** §VIII Self-Application & Meta-Logic

---

## VIII. Self-Application & Meta-Logic

### A. The Framework as Scaffold

Deferential Realism classifies **itself**:

```
⊡(Deferential_Realism)[(analytical, civilizational, analytical, universal)]
```

**Why Scaffold, not Rope or Mountain?**

- **Not Mountain**: Framework is changeable (subject to falsification, revision)
- **Not Rope**: Framework is temporary (designed for dissolution when better systems emerge)
- **Scaffold**: Built-in expiration, supports theory-building, explicit sunset

**Sunset clause:**

```
Sunset(DR) ≡ 
    (Falsified(core_claims) ∨ Better_Framework_Exists)
    → Dissolve(DR)
```

This framework has **designed obsolescence**. It's meant to be replaced.

---

### B. Gödel Limits & Known Limitations

**What this framework CANNOT do:**

1. **Prove its own consistency** (Gödel's Second Incompleteness)
   - We cannot use DR logic to prove DR logic is sound
   - Requires external validation (empirical testing, peer review)

2. **Guarantee universal correctness** (indexical relativity is fundamental)
   - Classifications are index-objective, not universally true
   - Different indices → different types (both correct)

3. **Predict all drift** (complexity limits)
   - Can detect patterns, can't predict specific trajectories
   - Network dynamics especially unpredictable (emergence, cascades)

4. **Resolve all paradoxes** (containment, not resolution)
   - Some tensions are structurally irreducible
   - Omega variables track what we can't know

---

### C. Empirical Validation (v4.0)

**Corpus statistics:**

- **691 constraints** analyzed across 35+ domains
- **Empirical distribution:**
  - Mountains: ~8%
  - Ropes: ~12%
  - Snares: ~18%
  - **Tangled Ropes: ~36%** (most common)
  - Scaffolds: ~15%
  - Pitons: ~11%

**Domain coverage:**
- Electoral systems (30+ countries)
- Economic structures (markets, labor, finance)
- Social institutions (education, healthcare, justice)
- Technical standards (encoding, protocols, interfaces)
- Environmental constraints (climate, resources)

**Power-scaling collision elimination:**
- Before indexing: 8.2% of constraints had power-position-dependent classification ambiguity
- After full (P, T, E, S) indexing: 0% ambiguity
- Resolution: Same constraint, different indices → both classifications true

---

### D. Canonical Example: Carbon Credits (ε = 0.55)

**Full formal analysis:**

```
carbon_credits:
  ε = 0.55 (base extractiveness)
  Supp = 0.60 (active enforcement via regulation)
  coordination_type = resource_allocation
  Coord(C) = true (emissions reduction mechanism)
  Asymmetric(C) = true (compliance costs vary)
```

**Classification by index:**

**I₁ = (powerless, biographical, trapped, local):**
```
π = 1.5, σ = 0.8
χ = 0.55 × 1.5 × 0.8 = 0.66

Tangled Rope threshold check:
  0.40 ≤ χ=0.66 ≤ 0.90 ✓
  ε=0.55 ≥ 0.30 ✔
  Supp=0.60 ≥ 0.40 ✓
  
Classification: ⊞⊠ (Tangled Rope)
Experience: Benefits exist (cleaner air) but costs high (price increases)
```

**I₂ = (moderate, biographical, constrained, national):**
```
π = 1.0, σ = 1.0
χ = 0.55 × 1.0 × 1.0 = 0.55

Classification: ⊞⊠ (Tangled Rope, clear mid-range)
Experience: Balanced—coordination value matches extraction cost
```

**I₃ = (institutional, generational, arbitrage, global):**
```
π = -0.2, σ = 1.2
χ = 0.55 × -0.2 × 1.2 = -0.132

Rope threshold check:
  χ=-0.132 ≤ 0.35 ✓
  ε=0.55 ≰ 0.45 ✗

FAILS rope_epsilon_ceiling (dual threshold protection)

Tangled Rope check:
  χ=-0.132 < 0.40 ✗
  
Falls below Tangled floor. But χ negative indicates net benefit.

Classification: ⊞ (Rope from institutional perspective)
Experience: Net extraction FROM system (profit from carbon trading)
```

**I₄ = (analytical, civilizational, analytical, global):**
```
π = 1.15, σ = 1.2
χ = 0.55 × 1.15 × 1.2 = 0.759

Snare threshold check:
  χ=0.759 ≥ 0.66 ✓
  ε=0.55 ≥ 0.46 ✓
  Supp=0.60 ≥ 0.60 ✓
  
Classification: ⊠ (Snare from analytical perspective)
Experience: Extraction exceeds coordination (delays real climate action)
```

**Indexical tension resolved:**

All four classifications are simultaneously objectively true:
- Powerless/local: Tangled (0.66) — benefits but high cost
- Moderate/national: Tangled (0.55) — balanced
- Institutional/global: Rope (-0.13) — **net beneficiary**
- Analytical/global: Snare (0.76) — sees extraction moderate normalizes

Not disagreement about facts—different structural properties visible from different indices.

---

### E. Known Limitations (Explicit)

**1. Calibration bias:**
- Power modifiers Western-biased (US/EU corpus)
- Non-WEIRD validation needed
- π(institutional) may vary across cultures

**2. Threshold uncertainty:**
- Many values provisional (Boltzmann thresholds especially)
- Require sensitivity analysis
- Subject to recalibration as corpus grows

**3. Temporal dynamics underspecified:**
- Can observe drift, lack predictive model for individual constraints
- Network drift velocity helps but not comprehensive
- Cascade prediction limited to 1-2 steps ahead

**4. Complexity ceiling:**
- Framework itself becomes unwieldy at large scale
- 691-constraint corpus pushes cognitive limits
- Need automated tooling for larger analyses

**5. Intersectionality gaps:**
- Single power position may miss compounding effects
- Race, gender, disability not explicitly modeled
- Future versions may use power vector, not scalar

**6. Purity scoring calibration:**
- Weights (30/25/25/20) theoretical, not empirically fitted
- Need sensitivity testing (±10% weight perturbation)
- Cross-corpus validation required

---

### F. Falsification Criteria

**The framework claims falsifiability. How would we know if DR is wrong?**

**Falsification conditions:**

1. **Indexical collision persistence:**
   - If full (P, T, E, S) indexing still produces contradictory classifications
   - Currently: 0% collision rate after full indexing
   - Would indicate: Index space incomplete

2. **Power-scaling failure:**
   - If π modifiers don't predict extraction amplification/reduction
   - Test: Controlled experiments with power position variation
   - Would indicate: Power position irrelevant (contradicts core claim)

3. **Boltzmann test invalid:**
   - If natural laws fail factorization test
   - Test: Thermodynamics, mathematics, logic should factorize perfectly
   - Would indicate: Coupling test detects noise, not structure

4. **Purity-drift disconnect:**
   - If purity score doesn't predict classification transitions
   - Test: Longitudinal studies tracking purity → type drift
   - Would indicate: Purity is epiphenomenal, not causal

5. **Action-consequence mismatch:**
   - If recommended actions consistently fail
   - Test: Compare outcomes of Accept vs Reform vs Cut
   - Would indicate: Classification doesn't guide effective strategy

**Current status:** Framework has survived initial corpus testing. Awaits:
- Non-WEIRD validation
- Longitudinal drift tracking
- Action-outcome correlation studies

---

**[§VIII COMPLETE - Self-Application]**

Framework analyzed with own tools:
- ✅ Classified as Scaffold (built-in obsolescence)
- ✅ Gödel limits acknowledged
- ✅ v4.0 corpus statistics (691 constraints, 11 drift types)
- ✅ Carbon credits full formal analysis (4 indices, 4 types)
- ✅ Six known limitations explicit
- ✅ Falsification criteria defined

**Final section:** §IX Conclusion

---

## IX. Conclusion: What This Logic Achieves

### A. From Truth-Preservation to Type-Preservation

Traditional logic asks: "Is this inference valid?"
- Preserves **truth** across inference steps
- Evaluates **soundness** (valid + true premises)
- Goal: Derive true conclusions from true premises

Indexed constraint logic asks: "What type is this constraint from this index?"
- Preserves **constraint-type** across indexed transformations
- Evaluates **classification coherence** across indexed evidence
- Goal: Route action appropriately from structural position

**The shift:** From abstract universal truth to positioned structural navigation.

**Meta-Invariants (System Guardrails):**

Whatever extensions or modifications we make to this system, these invariants MUST hold:

1. **Metrics-first classification** — `classify_from_metrics/6` remains canonical predicate in drl_core.pl
2. **Detection patterns only refine or demote** — Signatures in signature_detection.pl can override but never modify core logic
3. **Action flows from final type** — Recommendations derive from integrated classification, not raw metrics
4. **Thresholds are calibration points** — Not moral boundaries, not absolute truths, but measurement regime parameters subject to empirical validation

**On Thresholds:** The values in logic_thresholds.md (ε ≤ 0.45 for Rope, χ ≥ 0.66 for Snare, etc.) are **calibration points in a noisy measurement regime**, not Platonic ideals. They represent empirically observed boundaries in the 691-constraint corpus but remain subject to:
- Sensitivity testing (perturbation analysis)
- Cross-corpus validation (non-Western contexts)
- Temporal recalibration (as power structures evolve)

A constraint at χ = 0.65 vs χ = 0.67 is not categorically different—the threshold exists because discrete classification requires boundaries, not because reality has sharp edges.

---

### B. From Static Classification to Dynamic Structural Physics

**Version 1-3 (The Realist Core):**
- Six constraint types
- Power-scope scaling
- Metric-based classification
- **Static snapshot**: "What type is C?"

**Version 4.0 (+ The Structural Physics):**
- Boltzmann compliance (natural law test)
- Purity scoring (coordination health)
- Network dynamics (contamination propagation)
- Lifecycle drift (11 degradation patterns)
- **Dynamic system**: "Is C degrading? How fast? Will it cascade?"

**The achievement:** From taxonomy → physics. From naming things → predicting their evolution.

---

### C. Precision With Perspectivalism

**Not relativism:** Truth isn't "whatever you believe."

**Not absolutism:** Truth isn't "same for everyone."

**Indexed realism:** Truth is position-relative but each indexed claim is objectively verifiable.

```
∀I₁, I₂: (Type(C, I₁) ≠ Type(C, I₂)) ∧ (True(Type(C, I₁)) ∧ True(Type(C, I₂)))
```

Multiple valid answers exist because different positions expose different structural properties.

**Example:** Carbon credits are simultaneously Rope (institutional), Tangled (moderate), and Snare (powerless). All three objectively true.

---

### D. Modality With Power-Scope-Scaling

Modal logic traditionally treats constraints as universal (Mountain for everyone, or no one).

Indexed modal logic treats constraints as **power-scope-dependent**:

```
χ(C, P, S) = ε(C) × π(P) × σ(S)
```

Same base structure (ε = 0.55), different experienced types:
- χ(powerless, global) = 0.90 → Snare
- χ(moderate, national) = 0.55 → Tangled
- χ(institutional, global) = -0.13 → Rope

**Grounded in empirical measurement**, not abstract possibility.

---

### E. Error-Awareness With Intentionality Gradation

Built-in error taxonomy distinguishes:
- **Confusion** (Type I: False Mountain acceptance)
- **Deception** (physics-washing via FNL)
- **System bugs** (threshold miscalibration)
- **Fraud** (coordination-washing via FCR)

**Error ≠ Bad Faith.** The framework tracks both honest mistakes and deliberate misrepresentation.

---

### F. Action-Routing With Index-Sensitivity

From classification flows action recommendation:
- Mountain → Accept (conserve energy for changeable constraints)
- Rope → Maintain (preserve coordination)
- Snare → Cut/Exit (escape extraction)
- Tangled → Surgical Reform (preserve Coord, excise extraction)
- Scaffold → Monitor Sunset (ensure dissolution)
- Piton → Bypass (route around theater)

**But actions are indexed to your structural position:**
- If E = trapped, Cut/Exit may be impossible → Organize first
- If purity < 0.30, Surgical Reform blocked → Cut/Exit instead
- If T = immediate, Accept may be only option → long-term change invisible

**Strategy adapts to terrain AND your position on it.**

---

### G. Self-Awareness With Limitation Acknowledgment

The framework admits:
- Gödel limits (can't prove own consistency)
- Calibration bias (Western corpus)
- Threshold uncertainty (provisional values)
- Complexity ceiling (becomes unwieldy at scale)
- Falsification criteria (how to know if wrong)

**Meta-logical honesty:** We track what we DON'T know (Omega variables), not just what we claim to know.

---

### H. Architectural Coherence

**Spec → Implementation discipline:**
- Single canonical predicate: `classify_from_metrics/6`
- Single threshold registry: logic_thresholds.md
- Single signature override: signature_detection.pl (via structural_signatures.pl facade)
- Shadow mode: Extensions don't modify core

**No regressions.** v4.0 adds Stages 7-9 WITHOUT changing v1-6 classification logic.

---

### I. Integration: The Six Stages + Three Extensions

**Stages 1-6 (The Realist Core):**
- Indexed constraint operators (■, ⊞, ⊠, ⊞⊠, ⊡, ⊟)
- Power-scope scaling (χ = ε × π × σ)
- Lifecycle states & temporal logic
- Error taxonomy & action routing

**Stages 7-9 (The Structural Physics):**
- **Stage 7 (Boltzmann)**: Natural law test via factorization
- **Stage 8 (Purity & Network)**: Coordination health + contamination
- **Stage 9 (Drift Dynamics)**: Cascade prediction, network velocity

Together: **Operational indexed modal logic with structural physics.**

---

### J. What We Can Now Do

1. **Classify constraints** from any structural position (691-constraint corpus tested)
2. **Detect physics-washing** (FNL via Boltzmann compliance)
3. **Detect coordination-washing** (FCR via purity testing)
4. **Measure structural health** (purity score, 0-1 scale)
5. **Predict degradation** (11 drift types, network cascade modeling)
6. **Route action strategically** (index-sensitive recommendations)
7. **Track uncertainty** (Omega variables for unknowable limits)
8. **Falsify claims** (explicit criteria, empirical validation)

---

### K. The Core Achievement

We moved from asking:

**"What type is this constraint?"**

To asking:

**"What type is it FROM WHERE?"**
**"Is it structurally sound?"**
**"Is it degrading?"**
**"Can it be reformed?"**
**"Should we abandon it?"**

That's the difference between **taxonomy and physics**.

---

**"Formal systems should track real structure, acknowledge power differentials, and guide action from where you actually stand. This is that system."**

---

### L. Remaining Edges & Future Work

**This system is complete but not finished.** Four critical edges remain:

**1. Field Protocol Gap (Exportability)**

**Problem:** Implementation requires Prolog + 691-constraint corpus. Non-technical users lack accessible approximation methods.

**Future work:**
- Develop simplified field estimation protocol for ε, χ, Supp
- Create decision tree for index selection without code
- Build "pocket guide" with 2-3 canonical examples per type
- Minimal questions to test action appropriateness (e.g., "Is this Snare load-bearing?")

**Example protocol sketch:**
```
Q1: Can powerful agents exit easily? → Estimate π
Q2: How many participants can verify claims? → Estimate σ  
Q3: What % of cost exceeds coordination value? → Estimate ε
Q4: Would people comply without enforcement? → Estimate Supp
```

**2. Intersectionality Gap (Multi-Index Agents)**

**Problem:** Power modeled as scalar (6 positions), missing compounding structures.

**Known case:** Analytical + powerless (academic precariat)
- Analytical index: π = 1.15 (detects extraction)
- Powerless index: π = 1.5 (can't escape)
- **Compound effect not captured**

**Future work:**
- Model power as **vector** or **distribution** over nearby indices
- Weight by time spent in each index position
- Aggregate classification across inhabited indices
- Example: Academic precariat = 70% powerless + 30% analytical → mixed classification

**3. Normative Entanglement (Auditable Assumptions)**

**Problem:** System claims "structural not subjective" but value-laden choices exist:
- Which metrics count as "extraction" (ε measurement)
- Purity scoring weights (30/25/25/20) encode priorities
- Which coordination functions "count" (Coord(C) predicate)

**Future work:**
- Explicit subsection: "Where Normative Assumptions Enter"
- Document assumption→consequence chains
- Make value choices **auditable**, not hidden
- Example: "We weight Factorization at 30% because we prioritize natural-law detection over other purity signals"

**4. Action Validation Gap (Outcome Tracking)**

**Problem:** Framework routes action (Accept/Maintain/Cut/Reform) but doesn't validate whether recommendations improve conditions.

**Future work:**
- Longitudinal outcome studies
- Compare recommended vs alternative actions
- Measure: Did Accept (Mountain) conserve energy? Did Reform (Tangled) improve purity?
- Build feedback loop: Outcome → Threshold adjustment

**Critical questions requiring empirical validation:**

1. **Threshold sensitivity:** Do purity weight perturbations (±10%) cause mass reclassification?
2. **Temporal granularity:** Does discrete scope (local→regional→national) miss continuous transitions?
3. **Cross-cultural robustness:** Do π modifiers hold in non-WEIRD contexts?
4. **Cascade prediction accuracy:** Can network drift velocity actually predict transitions?

**Next steps (priority order):**

1. **Non-WEIRD corpus validation** (Critical: Western bias flagged in limitations)
2. **Sensitivity analysis** (Test threshold robustness)
3. **Field protocol development** (Make system exportable)
4. **Longitudinal drift tracking** (Validate predictive claims)
5. **Action-outcome correlation studies** (Close validation loop)

---

**[DOCUMENT COMPLETE]**

**Version 4.0 — Deferential Realism: A Logic of Indexed Constraints**

**Total document:** ~26,000 words

**Sections complete:**
- ✅ §I Foundation (indexed realism vs relativism)
- ✅ §II Basic Syntax (index structure, seven types, detection patterns, π/σ modifiers)
- ✅ §III Temporal Logic (indexed operators, 11 drift types)
- ✅ §IV Inference Rules (priority ordering, two-regime architecture)
- ✅ §V Error Taxonomy (six misclassification types)
- ✅ §VI Decision Logic (action routing + Stage 7-9 extensions)
- ✅ §VII Containment Logic (structural paradoxes)
- ✅ §VIII Self-Application (framework as Scaffold, v4.0 stats, limitations)
- ✅ §IX Conclusion (achievements, integration, core shift)

**Integration complete** with:
- logic_index.md (navigation)
- logic_thresholds.md (canonical values)
- logic_extensions.md (Stages 7-9 structural physics)

---

## III. Temporal Logic & Lifecycle States

Constraints don't just exist in static types—they evolve, degrade, and transition over time. Section III extends the core operators with temporal dynamics and lifecycle pathologies.

**Key insight:** Temporal operators are **indexed**, not universal. A constraint can be □Snare (Always a Snare) from one index but ◊Rope (Eventually a Rope) from another.

---

### A. Temporal Operators (Indexed)

Standard temporal logic operators, but **indexed to (P, T, E, S)**:

**Always (□):**
```
□φ[I] ≡ ∀t: φ(t)[I]
```
From index I, φ holds at all times.

**Example:**
```
□Snare[I_powerless] — constraint is Always a Snare for powerless agents
□Mountain[I_analytical] — constraint is Always a Mountain from analytical perspective
```

**Eventually (◊):**
```
◊φ[I] ≡ ∃t: φ(t)[I]
```
From index I, φ holds at some future time.

**Example:**
```
◊Rope[I_institutional] — Eventually becomes Rope for institutional agents
◊Exit[I_mobile] — Eventually can exit (have mobile exit options)
```

**Next (○):**
```
○φ[I] ≡ φ(t+1)[I]
```
From index I, φ holds in the next time step.

**Until (U):**
```
φ U ψ[I] ≡ ∃t': ψ(t')[I] ∧ ∀t < t': φ(t)[I]
```
φ holds Until ψ becomes true, from index I.

**Example:**
```
Scaffold U Dissolved[I] — Scaffold holds until dissolution, from index I
```

**Since (S):**
```
φ S ψ[I] ≡ ∃t': ψ(t')[I] ∧ ∀t > t': φ(t)[I]
```
φ has held Since ψ was true, from index I.

---

#### The Paradox of Hope (Resolved by Indexing)

**Classical problem:** How can a constraint be both "hopeless" and "changeable"?

**Indexed resolution:**

```
□Snare[I_powerless] ∧ ◊Rope[I_institutional]
```

From powerless perspective (E=trapped, T=immediate):
- Always a Snare (no escape visible within time horizon)
- Hope appears irrational

From institutional perspective (E=arbitrage, T=generational):
- Eventually a Rope (system reformable with time and resources)
- Change appears feasible

**Both are objectively true.** Hope is not delusion—it's indexical. What's "hopeless" from one structural position is "changeable" from another.

**Forensic Tip:** When an agent's index shows □⊠ (Always Snare) but the analytical index shows ◊⊞ (Eventually Rope), the delta between them measures the **energy required for collective action**. The gap from powerless to organized (π: 1.5 → 0.4) quantifies the coordination cost needed to make hope rational from the trapped position.

**Critical constraint on Eventually (◊) when E=trapped:**

```
If E = trapped, then ◊Exit → ∃t: Type(C, t) ≠ Type(C, t₀)
```

If you're truly trapped (E=trapped), eventual exit is mathematically impossible **unless the constraint itself transitions**. You can't have ◊Exit without either:
- Constraint type changes (Snare → Tangled → Rope)
- Your index changes (trapped → constrained via external help)

This formalizes the difference between "eventual escape" (possible if mobile) and "eventual liberation" (requires constraint transformation).

---

### B. Lifecycle States & Transition Dynamics

Constraints have **entropy**. Like physical systems, they naturally degrade from ordered (coordinating) to disordered (extractive) states.

**Metaphor:** Drift is the **weather** of constraint-space. You don't just find a Piton lying around—you watch a Rope rot into one. Scaffolds don't spontaneously appear as Pitons—they decay through sunset violation.

---

#### State Transitions (T1-T7)

**T1: Rope → Tangled Rope (Extraction Accumulation)**
```
⊞C → ⊞⊠C
```
Natural decay: Extraction creeps into coordination mechanism.
- Example: Open-source project → venture-funded company (coordination persists but extraction added)
- Drift type: Type 2 (EA — Extraction Accumulation)

**T2: Tangled Rope → Snare (Coordination Loss)**
```
⊞⊠C → ⊠C
```
Coordination function dries up, extraction dominates.
- Example: Employment → gig economy (income provision decays, extraction intensifies)
- Drift type: Type 3 (CL — Coordination Loss)

**T3: Rope → Scaffold (Sunset Acceptance)**
```
⊞C → ⊡C
```
Intentional transition: Add sunset clause to permanent coordination.
- Example: Permanent welfare → time-limited assistance with job training
- Drift type: Intentional (not pathological)

**T4: Scaffold → Piton (Sunset Violation)**
```
⊡C → ⊟C
```
**Most common Scaffold degradation.** Sunset clause violated, function dried up, theater persists.
- Example: "Temporary" emergency powers → permanent but unused authority
- Drift type: Type 5 (SV — Sunset Violation)
- **Theater spike:** When sunset missed, Theater ratio usually jumps from ~0.40 → 0.75+ as performance replaces substance

**T5: Tangled Rope → Piton (Extraction Collapse)**
```
⊞⊠C → ⊟C
```
Rare: Both coordination AND extraction dry up simultaneously.
- Example: Obsolete guild restrictions (coordinated nothing, extracted nothing, pure bureaucracy)
- Drift type: Type 6 (EC — Extraction Collapse)

**T6: Snare → Piton (Exhaustion)**
```
⊠C → ⊟C
```
Extraction mechanism depleted, victims escaped or died, constraint becomes vestigial.
- Example: Collapsed pyramid scheme → empty legal shell
- Drift type: Type 6 (EC — Extraction Collapse variant)

**T7: Mountain Stability (No Transitions)**
```
■C ↛ X
```
Mountains don't transition. Natural laws don't degrade.
- If a "Mountain" transitions → it was never a Mountain (False Mountain detection)

---

#### Transition Irreversibility (Entropy Principle)

**Natural direction: Coordination → Extraction**

```
Rope → Tangled Rope → Snare → Piton
```

This is **low-energy decay** (entropy increase). Extraction accumulates naturally. Coordination degrades naturally.

**Unnatural direction: Extraction → Coordination**

```
Snare → Tangled Rope → Rope
```

This is **high-energy reform** requiring:
- Massive agency energy
- Usually requires Scaffold construction first
- Often fails (purity too low, composition gates blocked)

**Key asymmetry:**

- **Decay** (→ extraction): Natural, low-energy, constant pressure
- **Reform** (→ coordination): Unnatural, high-energy, requires intervention

This is why Snares persist. Not because they're optimal—because reversing entropy requires work.

**Stage 7 extension:** Purity scoring (logic_extensions.md §2) quantifies this asymmetry. Reform energy scales with purity:
```
energy_cost(reform) = base_cost × purity_multiplier
```
At purity 0.31: multiplier ≈ 2.16× (expensive)
At purity 0.10: multiplier ≈ 2.89× (near-impossible)

---

#### The Eleven Drift Types

**Classical Types (1-7):**

**Type 1: Metric Substitution (MS)** — Goodhart's Law
- Optimization target becomes constraint itself
- Example: Test scores replace learning

**Type 2: Extraction Accumulation (EA)** — Rope → Tangled
- Coordination persists but extraction added
- Example: Ads added to free service

**Type 3: Coordination Loss (CL)** — Tangled → Snare
- Coordination function dries up
- Example: HOA stops maintaining, keeps extracting fees

**Type 4: Function Obsolescence (FO)** — Rope → Piton
- Purpose served, constraint persists
- Example: Typewriter-era regulations

**Type 5: Sunset Violation (SV)** — Scaffold → Piton
- **Primary Scaffold degradation path**
- Sunset missed, theater spikes
- Example: Temporary tax becomes permanent

**Type 6: Extraction Collapse (EC)** — Snare → Piton
- Victims exhausted or escaped
- Example: Depleted pyramid scheme

**Type 7: Algorithm Shutdown → Internalized Piton (AS)**
- Automated enforcement stops but behavioral pattern persists
- Example: Recommendation algorithm removed but users still behave as if present

**Stage 7-9 Extensions (Types 8-11):**

**Type 8: Coupling Drift (CD)** — Independent dimensions become coupled
- Constraint starts factorizing, later couples
- Example: Simple app → requires 15 permissions (couples data dimensions)
- See logic_extensions.md §4.1

**Type 9: Boltzmann Floor Drift (BFD)** — Necessary complexity increases
- Minimum extraction rises due to problem complexity
- Not extractive drift—legitimate necessity increase
- Example: Local clinic → regional hospital (floor rises 0.05 → 0.12)
- See logic_extensions.md §4.2

**Type 10: Purity Drift (PD)** — **Pre-symptomatic decay**
- Metrics (ε, χ) appear stable
- BUT: Boltzmann compliance failing, coupling increasing
- **Early warning signal** before type flip
- Example: Rope at purity 0.45 drifting toward Tangled (threshold 0.50 crossed soon)
- See logic_extensions.md §4.3

**Type 11: Network Drift (ND)** — **Contagion from neighbors**
- Constraint's intrinsic purity stable
- BUT: Effective purity declining due to neighbor degradation
- **Induced drift** from network contamination
- Example: Clean regulation surrounded by corrupt ones → effective purity drops
- See logic_extensions.md §4.4

---

#### Predictive Modeling (Stage 9)

**Classical lifecycle:** Observe drift, classify type, recommend action

**Stage 9 extension:** **Predict when transitions will occur**

Network Drift Velocity (logic_extensions.md §3.4):
```
dEP/dt = Σ (dP_neighbor/dt × contamination_strength × attenuation)
```

Where:
- EP = effective purity (includes network contamination)
- dP_neighbor/dt = neighbor's drift velocity

**Cascade prediction:** Given a source constraint drifting, predict which neighbors will cross purity thresholds and when.

**Example:**
```
Constraint A (purity 0.55, drifting at -0.05/year)
Neighbor B (purity 0.62, contaminated by A)

Prediction:
  t = 2 years: A crosses into contaminated zone (purity < 0.50)
  t = 3 years: B's effective purity crosses borderline (0.50)
  t = 5 years: B's intrinsic purity crosses borderline (contamination internalized)

Early warning: Reform A now, or prepare to reform both A + B later
```

This moves from **reactive classification** ("this is degraded") to **proactive intervention** ("this will degrade in 18 months, act now").

---

**[§III COMPLETE - Temporal Logic & Lifecycle States]**

Key achievements:
- ✅ Indexed temporal operators (paradox of hope resolved)
- ✅ Exit × Eventually interaction formalized
- ✅ Seven classical state transitions (T1-T7)
- ✅ Transition irreversibility (entropy principle)
- ✅ Eleven drift types (1-7 classical, 8-11 extensions)
- ✅ Predictive modeling pointer (Stage 9)

**Next:** §IV Inference Rules & Priority Ordering

---
