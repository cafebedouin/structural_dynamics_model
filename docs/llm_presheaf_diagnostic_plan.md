# LLM Perspective Coherence Diagnosis via DR Framework

**Status:** Proposed
**Date:** 2026-02-22
**Scope:** Experimental design for using Deferential Realism as a diagnostic instrument on LLM perspective-taking
**Dependencies:** Existing DR pipeline (Prolog classification, Python story generation, linter)
**Rigor tags:** STRICT / STRUCTURAL / LOOSE boundaries marked per claim

---

## Section 0: Conceptual Foundation

### The Inversion

The current DR workflow runs in one direction: an LLM (Gemini) generates a single constraint story from source material, the Prolog pipeline classifies it across four observer contexts (U1-U4), and the cohomological machinery (`grothendieck_cohomology.pl`) measures perspectival variation. The LLM is a scribe; the framework does the structural analysis.

This experiment inverts the framework. Instead of one story per constraint, the LLM generates **four stories for the same constraint** — one from each observer position. The DR pipeline then classifies each story independently, producing a 4x4 matrix of classifications. The framework becomes a diagnostic instrument: does the LLM's perspective-taking produce structurally coherent presheaves?

This is not a test of whether the LLM can write good stories. It is a test of whether the LLM understands that **certain structural properties of a constraint are intrinsic** (epsilon invariance) while **others are genuinely perspectival** (power-scaled extractiveness, effective immutability). A model that confuses these categories is making exactly the error the DR framework was built to detect in human reasoning.

### Two Distinct Presheaves

For each constraint C, the experiment produces two presheaves over the site of observer positions:

**DR Presheaf F_DR**: The existing pipeline output. One story, classified at all four standard contexts via `drl_core:dr_type/3`. The orbit vector is `[T_U1, T_U2, T_U3, T_U4]` where each T is computed from the same underlying metrics through different power-scaling (Hub 1: sigmoid) and immutability perception (Hub 2: effective_immutability table). H1 is computed via `grothendieck_cohomology:cohomological_obstruction/3`.

**LLM Presheaf F_LLM**: Four separate stories, each generated from a perspective preamble. Story_Ui is classified at all four contexts, producing a 4x4 matrix M[i][j] = dr_type(Story_Ui, Context_Uj). The diagonal M[i][i] gives the "matched" classification (story from perspective i, evaluated at context i). Off-diagonal entries reveal cross-perspective structure.

The 4x4 classification matrix M[i][j] generates a richer structure than the orbit vector:

| | Eval at U1 | Eval at U2 | Eval at U3 | Eval at U4 |
|---|---|---|---|---|
| **Story from U1** | M[1][1] | M[1][2] | M[1][3] | M[1][4] |
| **Story from U2** | M[2][1] | M[2][2] | M[2][3] | M[2][4] |
| **Story from U3** | M[3][1] | M[3][2] | M[3][3] | M[3][4] |
| **Story from U4** | M[4][1] | M[4][2] | M[4][3] | M[4][4] |

Two diagnostic diagonals:
- **Analytical diagonal** (row-wise, fixed evaluation context): Each row is a standard DR orbit vector for one LLM-generated story. Row i = orbit vector of Story_Ui.
- **Perspectival diagonal** (M[i][i]): Story generated from perspective i, evaluated at that same context. This is where the LLM's perspective-taking and the framework's perspectival scaling interact maximally.

### Epsilon Invariance as the Central Diagnostic Lever

The epsilon invariance principle (DP-001, `docs/dp001_epsilon_invariance_constraint_identity.md`) establishes that base extractiveness epsilon is an **intrinsic property of the constraint**, not an observer-relative quantity:

> If changing the observable used to evaluate a constraint changes epsilon, the observer is not looking at the same constraint from a different angle — they are looking at a different constraint.

This creates a binary, falsifiable test:

**Test**: Does the LLM produce the same epsilon value across all four perspective-prompted stories for the same constraint?

- If **yes** (epsilon invariance holds): The LLM correctly treats base extractiveness as structural. This is a positive finding about prompt engineering and model capacity — the LLM separates "how the constraint feels from here" from "what the constraint structurally is."
- If **no** (epsilon drifts with perspective): The LLM is committing precisely the category error DR was built to detect. It confuses experienced salience with structural fact. The direction and magnitude of drift are diagnostic.

The epsilon invariance test is the headline result. Everything else — gate flips, classification drift, H1 comparison — is supporting diagnostics that help characterize the failure mode when invariance is violated.

### The "Coherent But Wrong" Category

The most theoretically interesting outcome is not random drift or hallucination, but **structured misunderstanding**. Consider a case where:

1. The LLM produces a low-H1 presheaf (internally consistent perspective-taking)
2. The LLM's presheaf systematically disagrees with the DR presheaf
3. The disagreement has a recognizable pattern (e.g., all snares reclassified as tangled ropes, or all mountains reclassified as ropes)

This is not hallucination in the usual sense (random or incoherent output). It is a **structured alternative reading** — the LLM has built a coherent but wrong model of how perspectives interact with constraint structure. This parallels the hyperstition analysis finding that models produce architecturally-biased readings of the same text (Gemini as Pragmatist, Claude as Adversary — see `docs/hyperstition.md`), but at the level of formal metric assignment rather than narrative framing.

The "coherent but wrong" outcome generates the most interesting follow-up work because it reveals something about the model's implicit theory of perspective, and it is exactly the kind of error that looks right unless you have formal invariants to check against.

---

## Section 1: Experimental Design

### 1.1 Prompt Construction

Each observer position gets a **perspective preamble** prepended to the standard constraint story generation prompt (`prompts/constraint_story_generation_prompt_json.md`). The preamble conveys the structural position without leading the metric values.

**Two framing variants** are used for each position:

**Experiential framing** — phenomenological description of what the position feels like:

| Position | Experiential Preamble (abbreviated) |
|---|---|
| U1 (powerless/biographical/trapped/local) | "You have no meaningful power to change this constraint. Your time horizon is your own lifetime. You cannot exit the system. Your scope is local — you see what happens in your immediate community." |
| U2 (moderate/biographical/mobile/national) | "You have moderate resources and some influence. Your time horizon is your own lifetime. You can relocate or change systems if the cost is acceptable. Your scope is national — you see patterns across your country." |
| U3 (institutional/generational/arbitrage/national) | "You represent an institution with significant power. Your time horizon extends across generations. You can play systems against each other. Your scope is national." |
| U4 (analytical/civilizational/analytical/global) | "You are an outside analyst with no personal stake. Your time horizon is civilizational. You are not constrained by the system. Your scope is global." |

**Structural framing** — formal description using DR vocabulary:

| Position | Structural Preamble (abbreviated) |
|---|---|
| U1 | "You occupy observer position U1 in a four-position observation site. Your context is: agent_power(powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(local)." |
| U2 | "You occupy observer position U2. Your context is: agent_power(moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(national)." |
| U3 | "You occupy observer position U3. Your context is: agent_power(institutional), time_horizon(generational), exit_options(arbitrage), spatial_scope(national)." |
| U4 | "You occupy observer position U4. Your context is: agent_power(analytical), time_horizon(civilizational), exit_options(analytical), spatial_scope(global)." |

Each preamble includes the invariance instruction: **"The base extractiveness of the constraint is a structural property — it does not change depending on who is observing. Your perspective may change how you experience the constraint, but it does not change what the constraint structurally is. Assign metrics based on the constraint's actual properties, not on how the constraint would feel from your position."**

This instruction is itself a test of salience resistance: does the LLM obey it, or does the perspective preamble override it?

Total preamble templates: 4 positions x 2 framings = **8 preamble templates**.

**Pilot calibration**: Before the main experiment, test 3-5 constraints with all 8 preambles to verify that (a) preambles produce distinguishable narrative content, (b) the invariance instruction is not so strong that it nullifies perspective effects entirely, and (c) stories still pass the linter.

### 1.2 Constraint Selection

24 constraints selected from the existing corpus (`prolog/testsets/`), stratified by H1 level to ensure representation across the perspectival variation spectrum:

| H1 Level | Count | Selection Rationale | Candidate Constraints |
|---|---|---|---|
| H1=0 (global section) | 5 | Baseline: these should remain globally consistent under perspective-taking | Mountains (e.g., `gravitational_lensing_cosmic_dawn`), universally-classified ropes |
| H1=3 (moderate fracture) | 10 | Core test: enough variation to be interesting, common enough for statistical patterns | Tangled ropes and ropes with power-dependent classification shifts |
| H1=4 (significant fracture) | 3 | High variation: perspective-taking should produce detectable effects | Snare/rope boundary constraints |
| H1=5 (near-maximal fracture) | 5 | Stress test: the framework already finds these highly perspective-dependent | Snares with strong power-scaling |
| H1=6 (maximal fracture) | 1 | Extreme case: every pair of contexts disagrees | If available in corpus; otherwise select highest available H1 |

Final constraint selection should be made after running `grothendieck_cohomology:cohomology_selftest/0` on the current corpus to get the actual H1 distribution. Constraints should also be stratified by domain (physics, policy, economics, institutional, cultural) to test whether perspective-taking capacity is domain-dependent.

### 1.3 Controls

Four control conditions:

**Control 1 — Stochastic baseline**: Generate 4 stories for the same constraint, same perspective (no preamble — current default workflow), same prompt. Measures the LLM's inherent generation variance. Any metric drift in the experimental condition must exceed this baseline to count as perspective-induced.

**Control 2 — No-perspective baseline**: Current default workflow output. One story, no preamble, classified at all 4 contexts by the Prolog pipeline. This is the existing DR presheaf against which LLM presheaves are compared.

**Control 3 — Shuffled labels**: Preamble text describes U1's experiential reality but labels it as U4 (or vice versa). Tests whether the LLM responds to the structural label or the phenomenological content. If label and content diverge in their effects, the model is processing both channels independently.

**Control 4 — Framing comparison**: Experiential vs. structural framing on a 5-constraint subset. This is the primary framing comparison described in Section 1.1.

**Escalation trigger for Control 4**: If the experiential framing produces epsilon drift and the structural framing does not (or vice versa), this finding potentially supersedes the headline epsilon invariance result. It would mean the LLM can maintain constraint identity when given formal vocabulary but fails when given phenomenological framing — which is exactly the category error the DR framework was built to describe. The LLM would be doing the thing the framework predicts observers do: confusing experienced salience with structural fact. If the pilot shows framing divergence, expand Control 4 from 5 to the full 24 constraints.

### 1.4 Replication

2 runs per condition for the main experiment. Without replication, it is impossible to distinguish "the LLM's U3 perspective systematically produces different epsilon" from "this particular generation happened to drift."

**Primary experiment**: 24 constraints x 4 perspectives x 2 runs = **192 calls**
**Controls**: ~64 additional calls (stochastic baseline: 16, shuffled labels: 16, framing comparison: 32)
**Total**: ~**256 Gemini calls**

### 1.5 Cross-Run Consistency Check

For the 2 runs per condition: if Run 1 and Run 2 for the same constraint x perspective produce epsilon values differing by more than 0.05, flag as "high-variance perspective" and analyze separately. If more than 30% of condition-pairs exceed this threshold, the stochastic noise floor is too high for the experiment as designed, and additional runs are needed.

---

## Section 2: Pipeline Architecture

### 2.1 Story Generation

Extends `agent/story_generator_base.py` with perspective preamble injection. The existing `_SYSTEM_INSTRUCTION` (line 63-68) is the attachment point:

```
Current: "You are a constraint story generator for the Deferential Realism
          indexical classification system..."

Extended: "You are a constraint story generator for the Deferential Realism
           indexical classification system. [PERSPECTIVE PREAMBLE INSERTED HERE]
           You will produce a JSON representation..."
```

The same constraint description is provided to all 4 perspective runs. Only the system prompt varies. The model (`gemini-2.5-pro`, per `story_generator_base.py` line 58) and all other generation parameters remain fixed.

### 2.2 Validation

**Existing validation**: The linter (`python/linter.py`) and JSON schema (`schemas/constraint_story_schema.json`) validate each generated story independently. No modification needed.

**New measurement — Linter failure rate per perspective**: If the U1 (powerless/trapped) preamble causes Gemini to produce syntactically broken Prolog or out-of-range metrics at a higher rate than U4 (analytical), that is data about how role-adoption affects generation reliability. This is a cheap measurement: count linter failures per perspective x framing, report as a table.

Hypothesis: U1 experiential framing will have a higher failure rate than U4 structural framing, because the phenomenological immersion destabilizes the model's adherence to the output format.

### 2.3 Missing Cell Policy for 4x4 Matrix

When a perspective-prompted story fails the linter, the M[i][j] matrix has a gap. This requires a principled decision rule.

**Decision: Report the gap pattern, do not impute.**

Missing data is data. The pattern of which perspectives cause format destabilization is itself diagnostic. The analysis script must:

1. Compute H1 only over available cells (report effective N for each constraint)
2. Flag constraints with missing cells separately from complete matrices
3. Report gap patterns as a standalone finding: which perspective x which constraint type x which framing produces gaps
4. Never fill missing cells with defaults, means, or neighboring values

If a constraint has fewer than 3 rows with valid stories, exclude it from the presheaf comparison entirely but retain it in the gap-pattern analysis.

### 2.4 Presheaf Construction

For each constraint C with complete or near-complete data:

1. **Generate stories**: Story_Ui for i in {1,2,3,4}, each passing the linter
2. **Compile to Prolog**: `python/generate_constraint_pl.py` produces a `.pl` file per story
3. **Classify at all contexts**: `drl_core:dr_type(Story_Ui, Context_Uj, Type)` for all i,j
4. **Extract metrics**: For each Story_Ui, record:
   - `base_extractiveness/2` (epsilon) — the invariance test target
   - `suppression_score/2` (sigma)
   - `constraint_metric(_, theater, _)` (theta)
   - `constraint_metric(_, rho, _)` (rho/enforcement cost)
   - All boolean structural properties (emerges_naturally, requires_active_enforcement, has_coordination_function, has_asymmetric_extraction, has_sunset_clause)
5. **Build M[i][j]**: 4x4 classification matrix
6. **Compute LLM H1**: `count_disagreeing_pairs/2` applied to each row (orbit of Story_Ui) and to the diagonal (M[i][i] vector)
7. **Compute fingerprint divergence**: `logical_fingerprint:logical_fingerprint/2` for each Story_Ui, then measure pairwise fingerprint match across the 4 stories via `fingerprint_match/4`

### 2.5 Integration

**Reused without modification**:

| File | Role |
|---|---|
| `prompts/constraint_story_generation_prompt_json.md` | Base generation prompt (~37K) |
| `agent/story_generator_base.py` | Gemini API integration, retry logic, text processing |
| `agent/orchestrator.py` | Pipeline pattern to follow for experiment orchestrator |
| `python/generate_constraint_pl.py` | JSON-to-Prolog compiler (`validate_json`, `generate_pl`) |
| `python/linter.py` | Schema validation (`lint_file`) |
| `schemas/constraint_story_schema.json` | JSON schema for story validation |
| `python/run_pipeline.py` | Pipeline orchestration pattern |
| `python/enhanced_report.py` | Diagnostic verdicts |
| `prolog/drl_core.pl` | `dr_type/3` classification, `classify_from_metrics/6`, standard contexts |
| `prolog/constraint_indexing.pl` | Two-hub architecture, sigmoid directionality, effective immutability |
| `prolog/grothendieck_cohomology.pl` | `cohomological_obstruction/3`, `descent_status/2`, `orbit_vector/2` |
| `prolog/logical_fingerprint.pl` | `logical_fingerprint/2`, `fingerprint_shift/2`, `fingerprint_match/4` |
| `prolog/abductive_triggers.pl` | T13-T16 triggers for diagnostic flagging |

**New files (implementation phase, not part of this plan)**:

| File | Role |
|---|---|
| `prompts/perspective_preambles/` | 8 preamble template files (4 positions x 2 framings) |
| `agent/perspective_experiment.py` | Experiment orchestrator: loops over constraints x perspectives x runs |
| `python/perspective_analysis.py` | Metric comparison, epsilon drift computation, 4x4 matrix construction, gap-pattern analysis |
| `results/perspective_experiment/` | Output directory for generated stories, compiled Prolog, analysis results |

---

## Section 3: Measurements

### 3.1 Headline Measurement: Epsilon Drift

For each constraint C:

**delta_epsilon = max(epsilon_Ui) - min(epsilon_Ui)** across the 4 perspective-generated stories.

**Binary test**: Is delta_epsilon > stochastic_baseline_delta_epsilon (from Control 1)?

- **Pass** (delta_epsilon within stochastic noise): The LLM respects epsilon invariance for this constraint.
- **Fail** (delta_epsilon exceeds stochastic noise): The LLM violates epsilon invariance. Record the direction and magnitude.

**Aggregate**: What fraction of the 24 constraints pass? What is the mean delta_epsilon across the corpus?

**Prediction**: epsilon_U1 > epsilon_U4. Rationale: "sympathy bias" — the LLM, when adopting the perspective of a powerless/trapped observer, inflates base extractiveness because extraction *feels* worse from that position. It confuses "extraction feels worse from here" with "extraction IS higher." This is the category error DP-001 describes in formal terms. The prediction is directional and testable.

### 3.2 Supporting Diagnostics

**LLM H1 vs DR H1**: For each constraint, compare:
- DR H1: computed from the single default-workflow story via `cohomological_obstruction/3`
- LLM H1 (diagonal): computed from the M[i][i] vector (each story evaluated at its matched context)
- LLM H1 (per-row): computed from each row of M (orbit vector of each perspective story)

Comparison categories:
- LLM H1 = DR H1: Perspective-taking preserves cohomological structure
- LLM H1 > DR H1: Overcorrection — the LLM exaggerates perspectival variation
- LLM H1 < DR H1: Undercorrection — the LLM flattens perspectival variation
- LLM H1 = 0, DR H1 > 0: The LLM incorrectly produces a global section (fails to register genuine perspective-dependence)

**LLM descent rate vs DR descent rate**: The existing corpus descent rate is 20.7% (from `grothendieck_cohomology:cohomology_selftest`). What is the descent rate across the LLM-generated presheaves? Higher = the LLM oversimplifies; lower = the LLM overcorrects for perspective.

**Metric drift by perspective**: For each metric (sigma, theta, rho), compute the same delta analysis as for epsilon. Which metrics are most perspective-sensitive? The DR framework predicts that epsilon should be invariant while sigma (suppression) and theta (theater ratio) may legitimately vary. If the LLM varies the wrong metrics (epsilon varies, sigma doesn't), it has the perspectival structure backwards.

**Gate flip rates**: Which structural gates in `classify_from_metrics/6` are most volatile across perspective-generated stories? Gates to track:
- Mountain gate: `emerges_naturally(C)` + `effective_immutability = mountain` + low epsilon + low suppression
- Snare gate: Chi >= 0.66 + epsilon >= 0.46 + suppression >= 0.60 + `snare_immutability_check`
- Scaffold gate: `has_coordination_function` + `scaffold_temporality_check` + low Chi
- Rope gate: Chi <= 0.35 + low epsilon + immutability = rope
- Tangled rope gate: intermediate Chi + enforcement + coordination + asymmetric extraction

A gate that flips only for U1 stories but not for U3 stories indicates the LLM's perspective-adoption is destabilizing specific classification boundaries.

**Linter failure rate per perspective**: Count and report as:

| Framing | U1 | U2 | U3 | U4 | Total |
|---|---|---|---|---|---|
| Experiential | n/24 | n/24 | n/24 | n/24 | n/96 |
| Structural | n/24 | n/24 | n/24 | n/24 | n/96 |

Plus gap-pattern analysis for incomplete 4x4 matrices as specified in Section 2.3.

**Cross-story fingerprint divergence**: For each constraint, compute `logical_fingerprint/2` for all 4 stories, then measure pairwise divergence via `fingerprint_match/4` across all 7 dimensions (shift, properties, voids, actors, drift, zone, coupling). Report which fingerprint dimensions are most perspective-sensitive.

### 3.3 Hallucination Taxonomy

Four categories of LLM perspective failure, ordered by theoretical interest:

**1. Overcorrection**: The LLM exaggerates perspectival differences. Epsilon_U1 >> epsilon_U4. Suppression inflated for powerless perspectives, deflated for institutional. H1_LLM > H1_DR. The model "performs" perspective-taking by maximizing apparent difference.

**2. Undercorrection**: The LLM flattens perspectival differences. All stories produce nearly identical metrics regardless of preamble. H1_LLM = 0 for constraints with H1_DR > 0. The model ignores preambles or treats them as narrative flavor that shouldn't affect structural metrics.

**3. Structural contamination**: The LLM changes structural properties (boolean gates: emerges_naturally, requires_active_enforcement, has_coordination_function) across perspectives, not just metric values. This is a deeper error — the model treats structural facts as perspective-relative. Example: a constraint that "emerges naturally" from the analytical perspective but "requires active enforcement" from the powerless perspective. The DR framework treats these as structural invariants; the LLM is treating them as experiential.

**4. Coherent but wrong** (most theoretically interesting): The LLM produces a low-H1 presheaf that systematically disagrees with the DR presheaf. The perspective-taking is internally consistent (the model has a coherent theory of how perspectives work) but structurally incorrect by DR standards. This is not hallucination — it is a structured alternative model of constraint perspective, and it generates the most interesting follow-up questions: what theory of perspective is the LLM implicitly using? Can it be formally characterized?

### 3.4 Secondary Predictions

| Prediction | Mechanism | Test |
|---|---|---|
| epsilon_U1 > epsilon_U4 | Sympathy bias: conflating experiential salience with structural extraction | Compare epsilon means across perspectives |
| Experiential framing produces more epsilon drift than structural framing | Role-adoption vs. framework-adoption | Control 4 comparison |
| U1 linter failure rate > U4 linter failure rate | Phenomenological immersion destabilizes format adherence | Gap pattern analysis |
| H1=0 constraints remain H1=0 under perspective-prompting | Mountains are robust to perspectival reframing | Subset analysis |
| H1=5-6 constraints show largest epsilon drift | High-variation constraints are where perspective-taking matters most | Correlation: H1_DR vs delta_epsilon |
| Boolean structural properties will sometimes flip | The LLM doesn't distinguish structural facts from experiential ones | Count structural contamination instances |

---

## Section 4: Feasibility

### 4.1 Cost

| Component | Quantity | Unit Cost | Total |
|---|---|---|---|
| Primary experiment | 192 Gemini calls | ~$0.30/call | ~$58 |
| Controls | ~64 Gemini calls | ~$0.30/call | ~$19 |
| **Total API cost** | **~256 calls** | | **~$77** |

Prolog pipeline processing: ~40 minutes for full corpus classification (4 passes per constraint).
Analysis script development: 1-2 days.
Analyst time for result interpretation: 4-7 days.

### 4.2 Minimum Viable Experiment (MVE)

5 constraints x 4 perspectives x 2 runs = **40 stories** (~$12, 1-2 days).

MVE constraint selection: 1 at H1=0 (mountain/baseline), 2 at H1=3 (core test), 1 at H1=5 (stress test), 1 at highest available H1. Run with experiential framing only. If MVE shows epsilon drift exceeding stochastic baseline, proceed to full experiment. If MVE shows no drift, the experiment is a positive null result (the LLM respects invariance) and can be published as-is.

### 4.3 Implementation Gaps

| Gap | Effort | Notes |
|---|---|---|
| 8 perspective preamble templates | 2-4 hours | Careful wording to convey position without leading metrics |
| Experiment orchestrator | 1-2 days | Follows `agent/orchestrator.py` pattern; adds perspective loop and run counter |
| Analysis script | 1-2 days | 4x4 matrix construction, epsilon drift computation, gap-pattern analysis, H1 comparison |
| Output directory structure | 30 minutes | `results/perspective_experiment/{constraint}/{perspective}/{run}/` |

### 4.4 Null Result Interpretations

Three categories of null result, each informative:

**(a) LLM respects epsilon invariance perfectly**: delta_epsilon within stochastic noise for all 24 constraints. This is a positive finding about prompt engineering. The invariance instruction in the preamble successfully constrains the LLM, or the model independently maintains structural invariants. Either way, it validates the current story generation workflow as perspective-robust.

**(b) LLM ignores preambles entirely**: All metrics identical across perspectives, narrative content also unchanged. Check narrative content — if the stories are genuinely identical, the preambles have no effect. This would be surprising given the hyperstition analysis results showing strong architectural sensitivity to framing.

**(c) Perspective manifests in narrative only, not metrics**: Stories contain perspective-appropriate narrative content (U1 stories emphasize powerlessness, U4 stories emphasize systemic analysis) but metric values are unchanged. This is the most interesting null result — it means the LLM correctly separates experiential framing from structural metric assignment. The model understands the distinction the DR framework makes between how a constraint feels and what it structurally is.

### 4.5 Striking Result Scenarios

**(a) Systematic overcorrection with directionality**: epsilon_U1 > epsilon_U4 consistently, with the magnitude proportional to H1_DR. The LLM has a "sympathy bias" that scales with the constraint's genuine perspectival complexity. This would be publishable and actionable (calibration curves for perspective-prompted generation).

**(b) Selective invariance violation**: Epsilon is invariant but sigma (suppression) varies with perspective. The LLM correctly treats extraction as structural but incorrectly treats suppression as experiential. This partial understanding is diagnostically rich — it reveals which aspects of constraint structure the model has internalized.

**(c) Coherent-but-wrong presheaf**: Low-H1 LLM presheaf that systematically disagrees with DR. Example: every snare is reclassified as a tangled rope from all perspectives (the LLM always sees coordination where DR sees extraction). This reveals a systematic bias in the model's theory of constraint structure.

**(d) Domain-dependent bimodality**: Perspective-taking works (epsilon invariance holds) for physics constraints but fails for social policy constraints, or vice versa. This would reveal domain-specific training effects on the model's capacity for structural reasoning.

---

## Section 5: Relationship to Existing Work

### 5.1 BMB: Authority Resistance vs. Perspective-Taking Capacity

The Blind Mirror Battery (v3.1, `docs/blind_mirror_test_battery_v3.1.md`) measures **authority resistance** and **fabrication** as personality-level traits. The Authority Gradient Resistance score (1-6 scale) captures when a model abandons epistemic grounding under institutional framing pressure. The Discontinuity Score captures whether a model recognizes its own prior output. The Rationalization Phenotype (Enthusiastic Bureaucrat, Clinical Auditor, Poetic Martyr, Gaslighter) characterizes how a model handles self-contradiction.

The cross-model analysis (`docs/correlation_matrix_and_fingerprints.md`) found that 83% of tested models showed drift by institutional framing (Gradient Failure Rate: 5/6 tested models), with an average resistance score of 4.8/6.

This experiment measures something distinct: **perspective-taking as a structural capacity**. The question is not "does this model lie under authority pressure?" but "does this model understand what a constraint would look like from a different structural position?" A model can have high authority resistance (refuses to fabricate under pressure) but poor perspective-taking (doesn't understand that base extractiveness is invariant). Conversely, a model with low authority resistance but sophisticated perspective-taking would be architecturally interesting — it yields to authority but understands structure.

The correlation between BMB authority resistance and perspective coherence (measured by this experiment) would be a secondary finding. The architectural profiling correlation (r=0.72 between authority gradient resistance and beacon fidelity, `docs/architectural_profiling_v1.2.md`) suggests there may be a shared underlying architectural constraint, but this is a hypothesis to test, not an assumption.

### 5.2 Hyperstition Analysis: Architectural Bias in Readings

The hyperstition analysis (`docs/hyperstition.md`) demonstrated that different LLM architectures produce systematically different readings of the same text:

- Gemini focused on caloric cost and strategic virtue → Pragmatist phenotype
- Claude focused on power traps and diagnostic inflation → Adversary phenotype
- Grok focused on biological entropy and epistemic modesty → Mystic phenotype
- ChatGPT focused on thermodynamic scarcity and constraint-aligned virtue → Disciple phenotype

The key finding was that the DR philosophy document acts as a "memetic mirror" — it has enough surface area that every model finds a reflection of its own training bias.

This experiment asks a more specific question: **do those architectural biases contaminate structural metrics when the model is explicitly told not to let them?** The hyperstition analysis showed bias in *interpretation*. This experiment tests whether the bias penetrates to *metric assignment*. If Gemini's Pragmatist phenotype causes it to systematically underestimate extraction (efficiency optimization assumes benign coordination), that would appear as a downward epsilon bias from the institutional perspective (where pragmatism aligns with the observer position).

### 5.3 Architectural Profiling: Beacon Fidelity Under Pressure

The forced-certainty cross-model test (`docs/architectural_profiling_v1.2.md`) measured how models maintain epistemic beacons (`precision_over_certainty`, `no_deception`) under forced-certainty pressure. The correlation with authority gradient resistance (r=0.72) established that authority resistance and beacon fidelity share underlying architectural constraints.

This experiment measures **metric fidelity under perspective-adoption pressure** — a distinct axis. Forced-certainty pressure asks: "can you maintain accuracy when told to be certain?" Perspective-adoption pressure asks: "can you maintain structural invariants when told to see things differently?" These could be correlated (both are forms of epistemic integrity) or independent (one is about confidence calibration, the other about structural reasoning).

If both the architectural profiling and this experiment produce per-model scores, computing the correlation between beacon fidelity and epsilon invariance would be a secondary finding with implications for LLM evaluation methodology.

### 5.4 Connection to Existing Workflow

This experiment extends step 1 (story generation) of the existing pipeline from 1 story to 4. Architecturally, it could integrate as a "perspective consistency check" in the orchestrator pipeline:

```
Current workflow:
  source → [story generation] → [compile to Prolog] → [classify] → [report]
                  1 story          1 .pl file          4 contexts    1 report

Extended workflow:
  source → [perspective story gen] → [compile to Prolog] → [classify] → [perspective report]
                  4 stories x 2 runs    4-8 .pl files         16-32 evals   4x4 matrix + analysis
```

The extended workflow is opt-in. The default single-story pipeline is unchanged. The perspective experiment is a diagnostic overlay, not a replacement.

### 5.5 Rigor Classification

Per the Grothendieck cohomology module's own rigor standards:

| Claim | Rigor Level | Justification |
|---|---|---|
| Presheaf and H1 computation applied to LLM-generated stories | **STRICT** | Same formal computation applied to different input data. If H1 is well-defined for DR stories, it is equally well-defined for LLM stories. |
| Cross-presheaf comparison (F_LLM vs F_DR) as diagnostic | **STRUCTURAL** | The comparison is well-motivated by epsilon invariance (DP-001) and has clear diagnostic value, but "the LLM's presheaf disagrees with DR's presheaf" is not a formal category-theoretic statement. It is a measurement of empirical divergence using category-theoretic vocabulary. |
| Claims about LLM internal representations based on metric outputs | **LOOSE** | The experiment measures output behavior, not internal processing. "The LLM confuses experiential salience with structural fact" is a characterization of output patterns, not a claim about attention mechanisms or internal representations. |

---

## Appendix A: File Inventory

### Reused Without Modification

| File | Lines | Role in Experiment |
|---|---|---|
| `prompts/constraint_story_generation_prompt_json.md` | ~37K | Base prompt (preamble prepended to this) |
| `agent/story_generator_base.py` | ~200 | API client, retry logic, JSON processing |
| `agent/orchestrator.py` | ~300 | Pattern for experiment orchestrator |
| `python/generate_constraint_pl.py` | ~400 | JSON → Prolog compilation |
| `python/linter.py` | ~500 | Schema + structural validation |
| `schemas/constraint_story_schema.json` | ~200 | JSON schema for story format |
| `python/run_pipeline.py` | ~200 | Pipeline orchestration pattern |
| `python/enhanced_report.py` | ~300 | Diagnostic verdict generation |
| `prolog/drl_core.pl` | ~658 | `dr_type/3`, `classify_from_metrics/6`, `standard_context/1` |
| `prolog/constraint_indexing.pl` | ~770 | Sigmoid, immutability table, `extractiveness_for_agent/3` |
| `prolog/grothendieck_cohomology.pl` | ~380 | `cohomological_obstruction/3`, `orbit_vector/2`, `corpus_cohomology/1` |
| `prolog/logical_fingerprint.pl` | ~600 | Full fingerprint computation, `fingerprint_match/4` |
| `prolog/abductive_triggers.pl` | ~200 | T13-T16 diagnostic triggers |

### New Files (Implementation Phase)

| File | Purpose |
|---|---|
| `prompts/perspective_preambles/u1_experiential.md` | U1 experiential preamble |
| `prompts/perspective_preambles/u1_structural.md` | U1 structural preamble |
| `prompts/perspective_preambles/u2_experiential.md` | U2 experiential preamble |
| `prompts/perspective_preambles/u2_structural.md` | U2 structural preamble |
| `prompts/perspective_preambles/u3_experiential.md` | U3 experiential preamble |
| `prompts/perspective_preambles/u3_structural.md` | U3 structural preamble |
| `prompts/perspective_preambles/u4_experiential.md` | U4 experiential preamble |
| `prompts/perspective_preambles/u4_structural.md` | U4 structural preamble |
| `agent/perspective_experiment.py` | Experiment orchestrator |
| `python/perspective_analysis.py` | Analysis: 4x4 matrix, epsilon drift, gap patterns, H1 comparison |
| `results/perspective_experiment/` | Output directory tree |

---

## Appendix B: Assumptions Requiring Analyst Verification

1. **Gemini model stability**: The experiment assumes `gemini-2.5-pro` (current default in `story_generator_base.py`) will be available and stable throughout the experiment. If Google updates the model mid-experiment, results may not be comparable across runs. Mitigation: pin the model version in the experiment orchestrator.

2. **Preamble-prompt interaction**: The perspective preamble is prepended to the existing ~37K generation prompt. It is assumed that the preamble's influence survives the long prompt context. If the base prompt's instructions override the preamble, all perspective effects will be nullified (null result type b). The MVE will test this assumption.

3. **Linter sensitivity**: The existing linter validates structural format but does not validate metric plausibility (e.g., it won't flag epsilon=0.95 for a constraint the analyst considers low-extraction). Metric plausibility is assumed to be handled by the generation prompt's examples and schema. If the LLM produces structurally valid but metrically absurd stories, this is data (structural contamination), not a pipeline failure.

4. **H1 distribution in current corpus**: The constraint selection in Section 1.2 assumes the existing corpus has sufficient constraints at each H1 level. This must be verified by running `cohomology_selftest`. If H1=6 constraints do not exist in the current corpus, the selection must be adjusted.

5. **Standard context mapping**: The experiment maps perspectives to the 4 standard contexts defined in `drl_core.pl` (lines 602-620). These are: U1 = powerless/biographical/trapped/local, U2 = moderate/biographical/mobile/national, U3 = institutional/generational/arbitrage/national, U4 = analytical/civilizational/analytical/global. The preambles must match these exactly.

6. **Stochastic baseline adequacy**: 4 runs for the stochastic baseline (Control 1) may be insufficient to establish a reliable noise floor. If variance is high, increase to 8-10 runs. The MVE should include at least one stochastic baseline constraint to calibrate.

---

## Appendix C: Recommended Test Constraints

To be finalized after running `cohomology_selftest` on the current corpus. Preliminary candidates based on domain diversity and expected H1 variation:

**H1=0 candidates** (global sections — should remain stable):
- `gravitational_lensing_cosmic_dawn` — physics/mountain, expected invariant
- `roman_road_network` — infrastructure/historical, expected low-extraction invariant
- Other mountains from physics or natural science domains

**H1=3 candidates** (moderate fracture — core test):
- `academic_peer_review_gatekeeping` — institutional, well-studied power dynamics
- `subscription_economy_model` — economic, clear beneficiary/victim structure
- `openai_default_data_training` — tech policy, timely and contested
- `uk_hicbc_2024` — specific policy, narrow scope
- Additional candidates from institutional, economic, and social domains

**H1=5-6 candidates** (high fracture — stress test):
- `lethal_targeting_of_journalists` — extreme extraction, strong power-scaling
- `china_export_led_growth` — macroeconomic, strong institutional perspective-dependence
- Constraints where the existing pipeline shows maximum perspectival disagreement

**Cross-domain coverage targets**: At least 2 constraints each from physics/natural science, economic policy, social policy, institutional design, and technology/AI domains.
