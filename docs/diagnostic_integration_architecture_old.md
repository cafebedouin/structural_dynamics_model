# Diagnostic Integration Architecture

**Purpose:** Map how the DR analytical subsystems relate to each other, define the information flow from constraint story (hypothesis) through verification, and identify where shadow-mode tools should become active contributors to the diagnostic output.

**This document has two audiences:**
1. Scott — for architectural decision-making about what to integrate and in what order
2. Claude Code — as an evaluation prompt to assess the current codebase against this architecture and propose specific integration points

---

## The Problem

The DR codebase has accumulated roughly 20 analytical subsystems. Each was built to answer a specific question. Most work. But they don't talk to each other in a systematic way, and there's no unified model of how they contribute to the central task: evaluating whether a constraint story (hypothesis) is structurally sound.

The constraint story is the hypothesis. The analytical subsystems are the tests. The enhanced report is the verdict. Right now the verdict draws from some tests but not others, and the tests don't know about each other's findings.

---

## Four Tiers of Analysis

### Tier 1: Classification (the answer)

**What it does:** Assigns a constraint type at each observer index.

**Subsystems:**
- `classify_from_metrics/6` — deterministic type assignment
- Hub 1: `derive_directionality/3` → `sigmoid_f/2` → `extractiveness_for_agent/3` (power-scaled extraction)
- Hub 2: `effective_immutability_for_context/2` (perceived mutability)
- Structural signatures → `integrate_signature_with_modal/3` (deterministic overrides: natural_law → mountain, false_natural_law → tangled_rope, coupling_invariant_rope → rope)

**Status:** Active, well-tested, 1033 constraints passing. This tier is stable.

**Information flow:** Tier 1 produces the classification. Everything else evaluates it.

### Tier 2: Per-Constraint Diagnostics (confidence and quality signals)

**What it does:** Each subsystem asks a different question about whether the Tier 1 classification should be trusted, and if not, why not.

**Subsystems and their questions:**

| Subsystem | Question | Output | Current Status |
|---|---|---|---|
| MaxEnt classifier | Does the probabilistic model agree with the deterministic one? | agreement/disagreement + entropy score + type probabilities | Active (shadow) |
| Indexed MaxEnt | Does power-scaling change the probabilistic answer? | divergence score between classical and indexed distributions | Active (shadow, new) |
| Boltzmann compliance | Does the constraint's coupling structure make physical sense? | compliant/non-compliant + coupling score | Active |
| Purity score | How healthy is the coordination component? | purity score (0-1) + zone (pristine/clean/mixed/contaminated/degraded) | Active |
| Logical fingerprint | What's the compact structural identity? | fingerprint string encoding key structural features | Active |
| Dirac orbits | What's the gauge structure across observer positions? | orbit signature + span + gauge-fixed/gauge-variant per context | Active |
| Observer accessible | What can each observer actually see? | restricted view per context + restricted classification | Active (new) |
| gauge_fixed | Is the observer in a frame that prevents seeing alternatives? | true/false per (constraint, context) pair | Active |
| Structural signatures | Does the constraint match a known structural pattern? | signature type (natural_law, false_natural_law, CIR, FCR, etc.) | Active |
| Modal logic | What actions are available/appropriate? | action recommendations qualified by purity and type | Active |

**Key insight from the quantum verification:** These diagnostics decompose into two independent kinds:
- **Epistemic diagnostics** (observer_accessible, restricted classification) — what can the observer *see*?
- **Frame diagnostics** (gauge_fixed, Dirac orbits) — how does the observer's structural position *process* what they see?

These are nearly disjoint (1.6% overlap). Both matter. Neither subsumes the other.

**What's missing:** A unified "diagnostic verdict" that aggregates these signals. Currently each subsystem reports independently. The abductive engine partially addresses this (see Tier 4), but it doesn't consume all Tier 2 signals.

### Tier 3: Corpus Context (positioning)

**What it does:** Places a constraint within the larger landscape of all classified constraints.

**Subsystems and their questions:**

| Subsystem | Question | Output |
|---|---|---|
| Cohomology (H¹) | How much observer-dependence does this constraint have? | H¹ band (0-6) |
| Twin groups | Are there structurally identical constraints? | Twin group membership + group ID |
| Orbit families | Which constraints share the same gauge orbit signature? | Family ID + family members |
| Covering analysis | Does the expanded grid reveal missed transitions? | Number of missed transitions + which ones |
| Variance analysis | How much does classification vary across index configurations? | Variance ratio |
| Corpus type composition | What's the distribution of types in the corpus? | Type counts + percentages |
| Confidence distribution | What's the MaxEnt confidence profile across the corpus? | Entropy histogram + agreement rates |

**Status:** These all run as part of the pipeline (pipeline_dashboard.sh) and some feed into enhanced_report.py via JSON. The connection is functional but ad hoc — each subsystem writes its own output format and enhanced_report.py parses them separately.

**What's missing:** Tier 3 data should answer the question "is this constraint normal or unusual?" in a single summary. Currently you have to read six different sections and synthesize manually.

### Tier 4: Cross-System Integration (meta-diagnostics)

**What it does:** Looks for inconsistencies between Tier 2 diagnostics, and between Tier 2 and Tier 3.

**Subsystems and their questions:**

| Subsystem | Question | Output |
|---|---|---|
| Abductive engine | When do independent diagnostic paths disagree, and why? | 8 trigger classes with explanations |
| Fixed point network | How does one constraint's classification affect others? | Contamination paths + stability assessment |
| Drift/trajectory | Is this constraint's classification changing over time? | Drift type + velocity + direction |
| Quantum verification | Do the two hubs agree? Does MaxEnt divergence correlate with H¹? | Hub-conflict detection + divergence analysis |
| Psych_bridge | Does the constraint connect to psychological constraint patterns? | Bridge mappings to psychology domain |

**Status:** The abductive engine exists and has 8 trigger classes but doesn't consume all Tier 2 signals. Fixed point network exists but isn't connected to the per-constraint report. Drift analysis exists in the lifecycle module but doesn't feed back into classification confidence. Quantum verification is a one-shot report, not integrated into per-constraint feedback. Psych_bridge is a domain connector, not a diagnostic.

**What's missing:** Tier 4 should be the "second opinion" layer — it should aggregate Tier 2 signals, detect when they disagree, and produce a composite assessment of the constraint's structural integrity. The abductive engine is the natural home for this, but it needs to be expanded to consume more signals.

---

## The Constraint Story as Hypothesis: What Feedback Should Look Like

When an author writes a new constraint story, they are proposing a hypothesis: "This real-world phenomenon has these structural properties (ε, σ, τ, beneficiaries, victims, enforcement mechanism) and should classify as these types from these positions."

The diagnostic infrastructure should test this hypothesis at three levels:

### Level 1: Internal Consistency

Does the story's declared structure produce the expected classifications?

- **Tier 1 check:** Do the declared metrics actually produce the declared types at each index? (This is what report_generator.pl's Section 10a already does — the indexical audit.)
- **Tier 2 checks:**
  - Does Boltzmann compliance match what the story claims about natural vs. constructed? (A story declaring something is a mountain should be Boltzmann-compliant.)
  - Does the purity score match the declared coordination function? (A story declaring healthy coordination should have high purity.)
  - Does the structural signature match? (A story with ε > 0.25 and emerges_naturally = true should trigger false_natural_law.)
  - Does the MaxEnt distribution agree? If not, where is the probabilistic doubt? (MaxEnt disagreement means the metrics are near a threshold — the classification is fragile.)
  - Does the restricted-view classification reveal epistemic traps? (If powerless sees mountain but full data says snare, the story is about a cover story. Does the narrative acknowledge this?)

**Output:** A consistency score or traffic-light summary. Green = all diagnostics agree with declared types. Yellow = some diagnostics flag concerns but classification is robust. Red = diagnostics disagree with classification or with each other.

### Level 2: Structural Integrity

Is the story structurally well-formed, regardless of whether the classification is "correct"?

- **Tier 2 checks:**
  - Is the gauge orbit well-formed? (Does the orbit span make sense given the metrics? A constraint with ε = 0.01 should not have a 4-type orbit.)
  - Is the constraint gauge-fixed at any position? (If so, is this acknowledged in the narrative?)
  - Does the fingerprint match other constraints with similar narratives? (Structural duplicates should be flagged.)
  - Are the metrics internally consistent? (High extraction + high coordination + low suppression is a specific structural claim — does the narrative support it?)

- **Tier 4 checks:**
  - Does the abductive engine fire any triggers? (FNL/FCR disagreement, purity drift, perspectival gap — these are the system's self-correction signals.)
  - Is the constraint connected to others via the fixed point network? (Contamination risk.)

**Output:** An integrity report listing specific structural concerns, with severity levels.

### Level 3: Corpus Positioning

How does this constraint compare to the rest of the corpus?

- **Tier 3 checks:**
  - What H¹ band does it fall in? Is this typical for its type?
  - Is it in a twin group? If so, is the twinning expected or accidental?
  - What's its orbit family? Are the family members from similar domains?
  - Does it have missed transitions on the covering grid? (Suggests the classification is sensitive to small metric changes.)
  - What's its variance ratio? High variance = classification depends heavily on which index configuration you use.

- **Tier 4 checks:**
  - Does the quantum verification (hub-conflict) apply? Is this a constraint where the two hubs disagree?
  - Where does its MaxEnt entropy sit relative to the corpus distribution? (High entropy = probabilistically ambiguous. Low entropy = probabilistically confident.)

**Output:** A positioning summary: "This constraint is [typical/unusual] for its type, sits in H¹ band [X] (which is [expected/unexpected]), shares structural features with [N] other constraints in twin group [ID], and has [low/moderate/high] probabilistic confidence."

---

## Integration Proposal: What to Build

### Phase 1: Diagnostic Aggregation (connect Tier 2 to the report)

The immediate gap is that Tier 2 diagnostics run independently and their signals aren't aggregated. The enhanced report shows some of them but doesn't synthesize.

**Build:** A `diagnostic_summary/2` predicate (or Python equivalent) that takes a constraint ID and returns a structured summary:

```
diagnostic_summary(C, Summary) :-
    Summary = summary(
        classification_agreement,    % MaxEnt agrees/disagrees with deterministic
        boltzmann_status,            % compliant/non-compliant
        purity_zone,                 % pristine/clean/mixed/contaminated/degraded
        maxent_entropy,              % numeric entropy score
        maxent_divergence,           % indexed vs classical divergence
        gauge_orbit_span,            % number of distinct types across contexts
        gauge_fixed_contexts,        % list of contexts where gauge_fixed = true
        restricted_divergent_contexts, % where restricted != full classification
        signature_type,              % natural_law/false_natural_law/CIR/FCR/none
        abductive_triggers           % list of fired abductive triggers
    ).
```

This is the "constraint health check." It runs all Tier 2 diagnostics and returns a single record. The enhanced report can then render this as a summary section at the top, before the detailed breakdowns.

**Estimated scope:** ~100-150 lines of Prolog that calls existing predicates and assembles results. No new analysis — just aggregation.

### Phase 2: Consistency Scoring (Level 1 feedback)

**Build:** A consistency check that compares declared types (from the constraint story's `context()` blocks) against computed types, MaxEnt predictions, and diagnostic signals.

```
consistency_check(C, Context, Result) :-
    declared_type(C, Context, DeclaredType),
    dr_type(C, Context, ComputedType),
    maxent_most_likely(C, Context, MaxEntType),
    Result = check(DeclaredType, ComputedType, MaxEntType,
                   Agreement, Concerns).
```

Where `Concerns` is a list of specific issues: `maxent_disagrees`, `boltzmann_non_compliant_but_declared_mountain`, `high_entropy`, `gauge_fixed_not_acknowledged`, etc.

**Estimated scope:** ~200 lines. Requires defining what "concerns" exist and their severity levels. This is a design exercise as much as a coding exercise.

### Phase 3: Corpus Positioning (Level 3 feedback)

**Build:** A positioning summary that places the constraint in corpus context. This is mostly aggregating Tier 3 outputs that already exist.

```
corpus_position(C, Position) :-
    Position = position(
        h1_band,                    % cohomological band
        h1_typical_for_type,        % boolean: is this H¹ normal for this constraint's type?
        twin_group_id,              % twin group membership (or none)
        orbit_family_id,            % orbit family membership
        missed_transitions,         % covering analysis count
        variance_ratio,             % variance across index configs
        maxent_entropy_percentile,  % where does this constraint's entropy sit in the corpus?
        type_frequency              % how common is this type in the corpus?
    ).
```

**Estimated scope:** ~100 lines if the Tier 3 data is already in accessible JSON/Prolog. More if it requires running corpus-level computations per query.

### Phase 4: Abductive Engine Expansion (Tier 4 integration)

The abductive engine currently has 8 trigger classes. Based on the new diagnostic infrastructure, it should gain at least 4 more:

1. **MaxEnt divergence trigger:** When indexed MaxEnt divergence > 0.05 (the constraint is in the 8-constraint club where power-scaling changes the probabilistic answer).
2. **Hub-conflict trigger:** When the constraint is in the H¹ = 4 band (Hub 2's immutability flip is driving classification change).
3. **Epistemic trap trigger:** When `classify_from_restricted` at powerless produces a different type than `dr_type` at powerless (the powerless observer is being epistemically trapped).
4. **Classical oracle failure trigger:** When MaxEnt entropy is low (the probabilistic model is confident) but the constraint has H¹ > 0 (the cohomological formalism says there's observer-dependence). This catches the 99% of observer-dependent constraints that the classical oracle misses.

**Estimated scope:** ~80-120 lines per trigger, following the existing trigger pattern.

### Phase 5: Enhanced Report Redesign

Once Phases 1-4 land, enhanced_report.py should be restructured around the three feedback levels rather than around data sources:

```
CONSTRAINT STORY: {id}
═══════════════════════════════════════

DIAGNOSTIC SUMMARY (traffic light)
  Classification:  ● GREEN — all 4 contexts match declared types
  Structural:      ● YELLOW — MaxEnt disagrees at powerless context
  Corpus position: ● GREEN — typical for tangled_rope, H¹=3

LEVEL 1: INTERNAL CONSISTENCY
  [detailed consistency checks]

LEVEL 2: STRUCTURAL INTEGRITY
  [detailed diagnostic outputs]

LEVEL 3: CORPUS POSITIONING
  [detailed corpus context]

ABDUCTIVE FLAGS
  [any fired triggers with explanations]

FULL DIAGNOSTIC DATA
  [the existing detailed sections, for reference]
```

This puts the synthesis first and the details second. An author can read the traffic light and stop if everything's green, or drill into the specific level where yellow/red appears.

---

## Evaluation Task for Claude Code

Read the codebase and answer these questions:

1. **Which Tier 2 subsystems currently feed into enhanced_report.py, and which don't?** For each that doesn't, identify the predicate that produces the signal and the format it would need to be in.

2. **What does the abductive engine currently consume, and what Tier 2 signals is it missing?** List the 8 existing trigger classes and identify which of the proposed new triggers (MaxEnt divergence, hub-conflict, epistemic trap, classical oracle failure) could be added with minimal refactoring.

3. **Is there already a `diagnostic_summary`-like aggregation point in the codebase?** Something that calls multiple Tier 2 predicates and assembles results. If so, how complete is it? If not, where should it live?

4. **What's the current data flow from Prolog to enhanced_report.py?** Map every JSON/markdown file that enhanced_report.py reads, what Prolog module produces it, and whether the data is per-constraint or corpus-level.

5. **Which shadow-mode tools are ready to become active diagnostic contributors?** For each, what would "active" mean — does it change classification, or does it just produce a signal that the report consumes? Recommend which should remain shadow (observational only) and which should graduate to active diagnostic status.

6. **What's the minimum viable integration?** If you could only do one thing from this document, what would give the most diagnostic value per line of code? Be specific about which predicates to connect and where.

Output format: Answer each question with specific file paths, line numbers, and predicate signatures. If a question requires tracing data flow across multiple files, draw the flow explicitly. Don't speculate about what might exist — verify by reading the code.
