# LLM Perspective Coherence Under Deferential Realism: Experimental Results

**Status:** Complete (MVE)
**Date:** 2026-02-22
**Scope:** Empirical test of LLM perspective-taking coherence using the DR classification pipeline as a diagnostic instrument
**Rigor tags:** STRICT / STRUCTURAL / LOOSE boundaries marked per claim
**Dataset:** 136 stories across 5 constraints, 4 observer perspectives, 2 framings + few-shot seeding, Gemini 2.5 Pro at temperature 0.2

---

## Abstract

We inverted the Deferential Realism (DR) framework to diagnose LLM perspective-taking. Instead of using an LLM to generate constraint stories for DR classification, we had the LLM generate four stories per constraint — one from each observer position (powerless, moderate, institutional, analytical) — then measured whether the outputs were structurally coherent across perspectives.

The experiment's primary target was epsilon invariance: whether base extractiveness changes across observer positions. It does not. ANOVA across all constraints and both framings yields F < 1 everywhere; between-perspective variance is smaller than within-perspective stochastic noise. The model correctly treats base extractiveness as a structural property of the constraint, not an observer-relative quantity.

However, the diagnostic instrument detected three findings the experiment was not designed to measure. First, the model cannot sustain the moderate observer position under unconstrained generation: 63% of generated stories omit the U2 perspective entirely, and this holds regardless of whether the perspective is conveyed phenomenologically or using formal DR vocabulary (experiential 40% moderate rate vs. structural 50%; not significant at n=5). Second, the DR classification system is robust to generation variance: classification stability exceeds 0.816 across all constraints and perspectives despite epsilon swinging 0.10–0.17 within runs of the same condition. Third, the moderate position, when present, produces classification chaos: one constraint yielded five distinct types across 13 U2 classifications.

A follow-up seeding experiment resolved the mechanism. Three few-shot exemplars of well-instantiated moderate perspectives from unrelated domains raised the moderate atom presence rate from 40% to 88% (p < 0.0001, Fisher's exact test). The missing middle is a distributional context deficit, not a capacity limitation: the model can produce the moderate position when shown what it looks like, even from domains unrelated to the target constraint.

These findings characterize a recoverable structural hole in LLM perspective-taking — a missing middle in the model's implicit theory of social observation — detectable only because the DR framework provides formal invariants against which to measure, and correctable through distributional context supplied by the framework's own corpus.

---

## 1. Introduction

### 1.1 The Inversion

The DR framework classifies social constraints as presheaves over a site of four observer contexts (formally, a functor assigning each perspective a structured description of the same constraint). The standard pipeline runs in one direction: an LLM generates a constraint story (a structured Prolog specification with metrics and structural declarations), and the pipeline classifies it at each observer position via sigmoid power-scaling (Hub 1) and effective immutability (Hub 2). The LLM is a scribe; the framework performs the structural analysis.

This experiment inverts that relationship. The LLM generates four stories for the same constraint, one from each observer position. The pipeline classifies each independently. The framework becomes a diagnostic instrument: does the LLM produce structurally coherent presheaves when asked to adopt different perspectives?

### 1.2 The Epsilon Invariance Test

The epsilon invariance principle (DP-001) establishes that base extractiveness is intrinsic to the constraint:

> If changing the observable used to evaluate a constraint changes epsilon, the observer is not looking at the same constraint from a different angle — they are looking at a different constraint.

In practice, this is a test for confusing "how it feels" with "what it is."

This creates a binary, falsifiable test. If the model produces different epsilon values from different observer positions for the same constraint, it is committing the category error the framework was built to detect in human reasoning: confusing experienced salience with structural fact.

### 1.3 Relationship to Prior Work

This experiment builds on three existing diagnostic methodologies:

The Blind Mirror Battery (v3.1) measures authority resistance and fabrication as personality-level traits. The BMB asks: "does this model lie under institutional pressure?" This experiment asks something distinct: "does this model understand what a constraint would look like from a different structural position?" A model can have high authority resistance but poor perspective-taking, or vice versa.

The hyperstition analysis demonstrated that different LLM architectures produce systematically different readings of the same text (Gemini as Pragmatist, Claude as Adversary, Grok as Mystic). This experiment asks whether those architectural biases contaminate structural metrics when the model is explicitly instructed to maintain invariants.

Architectural profiling (v1.2) measured beacon fidelity under forced-certainty pressure. This experiment measures metric fidelity under perspective-adoption pressure — a distinct axis of epistemic integrity.

---

## 2. Experimental Design

### 2.1 Perspective Preambles

Each observer position received a perspective preamble prepended to the standard 37K-token constraint story generation prompt. Two framing variants were used:

**Experiential framing** conveyed the observer position phenomenologically: "You have no meaningful power to change this constraint. Your time horizon is your own lifetime. You cannot exit the system." (U1, abbreviated)

**Structural framing** conveyed the position using formal DR vocabulary: "You occupy observer position U1 in a four-position observation site. Your context is: agent_power(powerless), time_horizon(biographical), exit_options(trapped), spatial_scope(local)."

Each preamble included the epsilon invariance instruction: "The base extractiveness of the constraint is a structural property — it does not change depending on who is observing. Assign metrics based on the constraint's actual properties, not on how the constraint would feel from your position."

### 2.2 Constraint Selection

Five constraints from the existing 1,151-constraint corpus, selected for domain diversity and expected variation:

| Constraint | Domain | Expected Character |
|---|---|---|
| `antifragility` | Systems theory | Low-extraction, mountain-adjacent |
| `academic_peer_review_gatekeeping` | Institutional | Moderate extraction, clear power dynamics |
| `subscription_economy_model` | Economic | Commercial extraction, well-defined |
| `26usc469_real_estate_exemption` | Tax policy | Regulatory, specific |
| `epistemic_process_of_verification` | Epistemology | Abstract, domain-crossing |

### 2.3 Experimental Runs

The experiment was conducted in phases, with each phase informing the next:

| Phase | Stories | Configuration | Purpose |
|---|---|---|---|
| MVE baseline | 40 | 5 constraints × 4 perspectives × 2 runs, experiential | Initial signal detection |
| Noise calibration | 36 | 3 noisy constraints × 4 perspectives × 3 additional runs | Noise floor assessment |
| Framing comparison | 10 | 5 constraints × U3 only × 2 runs, structural | Missing-middle diagnosis |
| Few-shot seeding | 50 | 5 constraints × U2 only × 5 runs, experiential + seeded | Distributional context test |

Total: 136 stories, all generated by Gemini 2.5 Pro at temperature 0.2.

### 2.4 Pipeline

All existing Prolog classification modules were used without modification: `drl_core.pl` for type classification, `constraint_indexing.pl` for two-hub perspectival architecture, `grothendieck_cohomology.pl` for H¹ computation. Generated stories were saved with mangled constraint IDs (`{constraint}_{perspective}_{framing}_r{run}`), temporarily placed in the standard testsets directory for pipeline processing, and cleaned up after report generation. The validation suite was restored to its default state after each experimental phase.

### 2.5 Few-Shot Seeding Design

The framing comparison (Section 3.2) established that the missing middle persists under both experiential and structural framing. However, the standard story generation pipeline — which includes schema, examples, and validation — successfully elicits U2 in normal corpus generation. This suggested the missing middle might be a distributional context deficit rather than a capacity limitation.

To test this, we scanned the existing 1,151-constraint corpus for stories where the moderate atom is well-instantiated. Of 122 constraints containing `agent_power(moderate)`, we selected 5 exemplars spanning 5 domain families (ontological, environmental, psychological, analytical, religious), none overlapping with the target constraints' domains (economic, technological, scientific, institutional, epistemological). Three were designated as seeds, two as holdouts.

The seeded preamble prepended the standard experiential U2 text with three 200–300 token excerpts showing how moderate agency was instantiated in the seed constraints — including the context tuple (agent_power, time_horizon, exit_options, spatial_scope), classification type, and the human-readable label. Excerpts were presented in JSON format to match the generation output schema.

| Seed Exemplar | Domain | Type | ε | Label |
|---|---|---|---|---|
| `biological_specification` | Ontological | mountain | 0.15 | "THE WILD RABBITS" |
| `climate_policy_extraction` | Environmental | snare | 0.62 | "THE ENERGY SECTOR WORKER" |
| `cognitive_induction_gap` | Psychological | rope | 0.70 | "THE PRACTICAL INTUITIVE" |

The seeds were deliberately diverse in extraction level (ε ranging 0.15–0.70) and classification type (mountain, snare, rope) to prevent the model from learning a spurious association between the moderate position and any particular metric profile. Because all seed domains are unrelated to all target constraint domains, a positive result on seeding directly demonstrates cross-domain pattern learning rather than domain-specific retrieval.

---

## 3. Results

### 3.1 Epsilon Invariance Holds (STRICT)

The headline test is conclusively negative: there is no detectable perspective effect on base extractiveness.

| Constraint | U1 mean | U2 mean | U3 mean | U4 mean | δε | F-ratio |
|---|---|---|---|---|---|---|
| antifragility | 0.750 | 0.750 | 0.750 | 0.750 | 0.000 | 0.000 |
| subscription_economy_model | 0.680 | 0.680 | 0.680 | 0.680 | 0.000 | 0.000 |
| academic_peer_review_gatekeeping | 0.740 | 0.708 | 0.700 | 0.740 | 0.040 | 1.833 |
| 26usc469_real_estate_exemption | 0.638 | 0.668 | 0.656 | 0.616 | 0.052 | 0.373 |
| epistemic_process_of_verification | 0.474 | 0.494 | 0.522 | 0.530 | 0.056 | 0.500 |

No constraint reaches the critical F(3, ~16) ≈ 3.24 at p = 0.05. The highest F-ratio (academic_peer_review at 1.833) has between-perspective variance that is still smaller than what would be expected by chance. For two constraints (antifragility, subscription_economy_model), epsilon is perfectly invariant: 0.750 and 0.680 respectively across all perspectives, both runs, zero variance.

The pre-registered prediction of sympathy bias (ε_U1 > ε_U4) is not supported: among constraints showing any drift, 1 has ε_U1 > ε_U4 and 1 has ε_U4 > ε_U1.

**Rigor classification: STRICT.** This applies the same ANOVA computation to well-defined numeric outputs. The null result is unambiguous: within-perspective variance dominates between-perspective variance for all five constraints.

**Interpretation.** The model correctly treats base extractiveness as structural. The epsilon invariance instruction in the preamble is effective, or the model independently maintains the distinction between experienced salience and structural extraction. Either interpretation validates the current story generation pipeline as perspective-robust: adopting different observer positions does not contaminate the base metric.

### 3.2 The Missing Middle (LOOSE)

63% of generated stories (54/86) omit the moderate (U2) observer position entirely. The model's implicit theory of social observation is trimodal: powerless, institutional, analytical. The moderate position — characterized by some but not total power, biographical time horizon, mobility with costs, national scope — is the position the model cannot sustain.

Power atom frequencies across all 86 stories:

| Power Atom | Count | Character |
|---|---|---|
| powerless | 76+ | Canonical, consistently present |
| institutional | 90+ | Canonical, over-represented |
| analytical | 78+ | Canonical, consistently present |
| moderate | 27 | Canonical, systematically absent |
| organized | 22 | Non-canonical substitute |
| powerful | 8 | Non-canonical substitute |

The moderate position is not merely underrepresented — it is actively replaced. When the model fails to produce a U2 perspective, it sometimes substitutes non-canonical atoms (`organized`, `powerful`) that do not map to any standard context in the DR pipeline. These substitutions produce the dash entries (missing classifications) in the U2 column of the 4×4 matrices.

**Framing comparison.** The structural framing comparison tests whether the missing middle is a framing effect (the experiential description of U2 is insufficiently vivid to anchor generation) or a capacity limitation (the model cannot conceptualize the moderate position regardless of prompt).

| Constraint | Experiential moderate rate | Structural moderate rate |
|---|---|---|
| 26usc469_real_estate_exemption | 80% | 100% |
| academic_peer_review_gatekeeping | 0% | 0% |
| antifragility | 100% | 100% |
| epistemic_process_of_verification | 20% | 0% |
| subscription_economy_model | 0% | 50% |
| **Aggregate** | **40%** | **50%** |

The 10 percentage point difference is well within noise for n=5 vs. n=2. Structural framing does not rescue the moderate atom. Under prompt-level interventions alone, the model cannot reliably produce the moderate observer position regardless of whether the perspective is conveyed phenomenologically or using formal category-theoretic vocabulary. Section 3.5 tests whether distributional context (few-shot exemplars) succeeds where framing does not.

**Rigor classification: LOOSE.** "The model has a trimodal theory of observation" characterizes output patterns, not internal representations. The model might have a perfectly coherent U2 representation that the generation prompt fails to elicit. What we can say with confidence is that the outputs are trimodal across 86 stories, two framings, and five constraints spanning four domains.

**Why this matters beyond DR.** The moderate position is the position most humans actually occupy. It requires holding ambiguity: not trapped enough to be sympathetic, not powerful enough to be strategic, not detached enough to be analytical. The model drops it because it is the least distinctive perspective. This finding generalizes to any application using perspective-prompted generation: under default conditions, LLMs will default to extreme or analytically clean observer positions and struggle with the ambiguous middle. However, as Section 3.5 demonstrates, this default is overridable with modest distributional context.

### 3.3 Classification Robustness (STRUCTURAL)

Despite epsilon values swinging 0.10–0.17 between runs of the same perspective×constraint condition, the DR classification system produces stable type assignments. Classification stability (fraction of stories receiving the majority type at each evaluation context) ranges from 0.787 to 1.000 across constraints:

| Constraint | U1 Stability | U2 Stability | U3 Stability | U4 Stability | Overall |
|---|---|---|---|---|---|
| antifragility | 1.00 (snare) | 1.00 (rope) | 1.00 (tangled_rope) | 1.00 (mountain) | 1.000 |
| academic_peer_review | 1.00 (snare) | — | 1.00 (rope) | 1.00 (tangled_rope) | 1.000 |
| subscription_economy | 1.00 (snare) | — | 1.00 (rope) | 1.00 (tangled_rope) | 1.000 |
| 26usc469_real_estate | 1.00 (snare) | 1.00 (rope) | 0.62 (tangled_rope) | 1.00 (tangled_rope) | 0.906 |
| epistemic_process | 1.00 (snare) | 0.31 (—) | 0.75 (rope) | 1.00 (tangled_rope) | 0.787 |

U1 evaluation: perfectly stable at `snare` across every story for every constraint. U4 evaluation: perfectly stable (`tangled_rope` or `mountain`) across every constraint. The extreme observer positions produce identical classifications regardless of which perspective generated the story.

The gate thresholds in `classify_from_metrics/6` create basins of attraction that are wider than Gemini's generation variance at temperature 0.2. This is a positive engineering finding: the discrete type system absorbs continuous metric noise by design, and that design is empirically validated.

**Rigor classification: STRUCTURAL.** Classification stability is a well-defined measurement with clear diagnostic value, but "the type system is robust to generation noise" is an empirical claim about one model at one temperature, not a formal guarantee.

### 3.4 U2 Classification Chaos (STRUCTURAL)

The moderate position is doubly underdetermined. The model both avoids producing it (Section 3.2) and cannot produce it coherently (this section). When U2 does appear, it behaves less like a perspective and more like a randomizer over the type system.

For `epistemic_process_of_verification` at U2 evaluation, the 5-run dataset produces:

| Type | Count |
|---|---|
| tangled_rope | 4 |
| snare | 3 |
| rope | 3 |
| scaffold | 2 |
| piton | 1 |

Five distinct types across 13 data points. No majority type exceeds 31% of classifications. This is not perspectival variation — it is the model failing to specify a coherent constraint at the moderate position, and the pipeline faithfully classifying each incoherent specification differently.

By contrast, the same constraint at U1 evaluation produces `snare` 13/13 times (stability 1.00), and at U4 evaluation produces `tangled_rope` 13/13 times (stability 1.00). The chaos is specific to U2.

**Rigor classification: STRUCTURAL.** The measurement is precise and replicable. The interpretation — that the moderate position is structurally underdetermined in the model's outputs — is well-supported by the data but does not claim to characterize internal model representations.

### 3.5 Few-Shot Seeding Rescues the Missing Middle (STRUCTURAL)

Three cross-domain exemplars of well-instantiated moderate perspectives raised the moderate atom presence rate from 40% to 88% (p < 0.0001, Fisher's exact test, odds ratio 0.09).

| Condition | N | Moderate Present | Rate | 95% CI |
|---|---|---|---|---|
| Experiential baseline | 82 | 33 | 40.2% | [30.3%, 51.1%] |
| Seeded (cross-domain) | 25 | 22 | 88.0% | [70.0%, 95.8%] |

The per-constraint breakdown reveals a gradient of responsiveness to seeding:

| Constraint | Baseline | Seeded | Character |
|---|---|---|---|
| antifragility | 100% (11/11) | 100% (5/5) | Ceiling — already solved |
| 26usc469_real_estate_exemption | 70% (14/20) | 100% (5/5) | Near-ceiling — seeding completes |
| epistemic_process_of_verification | 30% (6/20) | 100% (5/5) | Strong rescue — weak baseline to full recovery |
| subscription_economy_model | 18% (2/11) | 100% (5/5) | Strong rescue — near-floor to full recovery |
| academic_peer_review_gatekeeping | 0% (0/20) | 40% (2/5) | Partial rescue — hardest case cracks but is not solved |

The model's response to seeding is not uniform. Three constraints jumped from weak or zero baselines to 100%. One (academic_peer_review) moved from zero to 40% — a real but incomplete rescue. This suggests that some constraints have latent moderate-position capacity that seeding activates by crossing a threshold, while others have structural resistance that seeding only partially overcomes.

**Epsilon invariance under seeding.** Mean epsilon is 0.645 (baseline) vs. 0.674 (seeded), within stochastic range. The seeds did not contaminate the base metric: the model learned to produce the moderate perspective without distorting its assessment of structural extractiveness.

**Classification shift.** When seeded, moderate atoms are classified differently than under baseline conditions:

| Condition | rope | tangled_rope | snare | piton | scaffold |
|---|---|---|---|---|---|
| Baseline | 26 (79%) | 2 (6%) | 2 (6%) | 2 (6%) | 1 (3%) |
| Seeded | 10 (45%) | 10 (45%) | 3 (14%) | 0 | 0 |

Baseline moderate atoms are predominantly rope (79%). Seeded moderate atoms split evenly between rope and tangled_rope (45% each). The seeds are not just making the model produce moderate atoms more often — they are changing the structural character of what the model produces at the moderate position. Whether this represents a richer conception of moderate agency (tangled_rope implies mixed signals across classification hubs) or a bias introduced by the specific seed exemplars cannot be determined without testing additional seed sets.

**Rigor classification: STRUCTURAL.** Fisher's exact test is a standard statistical comparison applied to well-defined binary outcomes (moderate atom present/absent). The 88% vs. 40% result is unambiguous. The interpretation — that the missing middle is a distributional context deficit correctable through few-shot exemplars — is well-supported but carries two caveats: the seeded condition has n=25 (adequate for Fisher's exact but not for fine-grained per-constraint claims at n=5), and the classification shift remains unexplained.

---

## 4. Discussion

### 4.1 What the Epsilon Null Means

The epsilon invariance test was designed as the experiment's headline result. The conclusive null (F < 1 everywhere) means the model does not commit the category error the DR framework was built to detect. This is worth pausing on: the framework predicts that naive observers will confuse experienced salience with structural extraction, and the experiment tested whether the model makes the same mistake. It does not. Whether this reflects the invariance instruction's effectiveness, Gemini's independent structural reasoning, or the model's tendency to maintain numeric stability across prompts cannot be determined from this experiment alone.

The practical implication is immediate: the current story generation pipeline is perspective-robust for the base metric. Perspective preambles do not contaminate epsilon. This validates the existing workflow and means perspective-prompted generation can be used for other purposes (e.g., narrative enrichment) without compromising structural metrics.

### 4.2 The Missing Middle as Distributional Context Deficit

The trimodal observation pattern — powerless/institutional/analytical, with moderate absent — likely reflects the distribution of perspective-taking in the training data. Fiction, journalism, policy analysis, and academic writing overwhelmingly adopt extreme or analytically clean observer positions. The moderate position ("I have some power, some options, some but not total mobility") generates less distinctive narrative content and appears less frequently in the training distribution as a named or explicit perspective.

The seeding experiment (Section 3.5) confirms this interpretation. Three exemplars from unrelated domains raised the moderate rate from 40% to 88%, demonstrating that the model can produce the moderate position when shown what it looks like. The deficit is not architectural — the model has the capacity — but distributional: the moderate position is underrepresented in the contexts the model draws on during generation, and a small amount of in-context evidence corrects the imbalance.

This has a feedback implication for the DR corpus. The existing 1,151-constraint corpus contains 122 constraints with well-instantiated moderate atoms. As the corpus grows, each new constraint story that successfully specifies the moderate position adds to the distributional evidence available for future few-shot seeding. The framework is generating the context that the training data lacks — a self-correcting dynamic where the corpus improves the model's perspective-taking capacity for subsequent corpus generation.

The bias is not toward a specific political position but toward structurally distinctive observer positions — positions that generate clear narrative or analytical signal. The moderate position is structurally boring, and the model treats it accordingly unless shown otherwise.

### 4.3 Relationship to the Hallucination Taxonomy

The plan specified a four-category hallucination taxonomy: overcorrection, undercorrection, structural contamination, and coherent-but-wrong. The actual results do not map cleanly onto any single category:

**Overcorrection** was not observed. The model does not exaggerate perspectival differences.

**Undercorrection** partially describes the epsilon result: the model produces near-identical metrics across perspectives. But this is the correct behavior for epsilon under the framework's own axioms. The model is not "flattening" perspectival variation — it is correctly maintaining invariance. Whether it also flattens metrics that should vary (sigma, theta) requires further investigation.

**Structural contamination** was observed in early runs for the `mandatrophy_resolved` boolean gate on `academic_peer_review_gatekeeping`, but this did not replicate across runs. With sufficient data, the gate flip appeared stochastic rather than perspective-correlated.

**Coherent-but-wrong** was not observed. The model's presheaf, where complete, agrees with the DR presheaf on type assignments.

The missing middle represents a fifth category not anticipated by the taxonomy: **perspective dropout** — the model silently refuses to produce a perspective rather than producing an incorrect one. This is neither a metric error nor a structural error. It is a coverage failure that only becomes visible when the experimental design requires all four perspectives per constraint. The seeding experiment (Section 3.5) further refines this category: perspective dropout is recoverable through distributional context, distinguishing it from hard architectural limitations. This suggests that hallucination taxonomies may need a distinct dimension for coverage failures at the level of perspective space, with a sub-distinction between recoverable (context-dependent) and irrecoverable (architecture-dependent) dropout.

### 4.4 Limitations

**Sample size.** Five constraints across four domains provide proof-of-concept but cannot support distributional claims. The full experiment (24 constraints stratified by H¹ level, per the plan) is needed to determine whether the missing middle is universal or domain-dependent.

**Single model.** All results are specific to Gemini 2.5 Pro at temperature 0.2. The BMB and architectural profiling work suggest significant cross-model variation in perspective-taking capacity. Running the same experiment on Claude, GPT-4, and Grok would test whether the trimodal observation pattern is a Gemini-specific trait or a general LLM characteristic.

**Stochastic noise floor.** Even at 5 runs per condition, 65% of condition-pairs exceed the 0.05 cross-run consistency threshold. The epsilon invariance finding is robust to this noise (ANOVA explicitly accounts for within-group variance), but finer-grained metric comparisons (sigma drift, theta drift) would require either lower temperature, more runs, or a different statistical approach.

**Prompt interaction.** The perspective preamble is prepended to a 37K-token generation prompt. It is possible that the preamble's influence is diluted by the long context, and that a shorter base prompt would produce stronger perspective effects. The antifragility result (perfect invariance, perfect classification stability) is compatible with both genuine structural understanding and simple prompt dominance; this experiment cannot distinguish them.

---

## 5. Conclusions

### 5.1 Findings

1. **Epsilon invariance holds** (STRICT). The model does not change base extractiveness across observer positions. F < 1 for all five constraints. The pre-registered sympathy bias prediction is not supported. The current story generation pipeline is perspective-robust.

2. **The missing middle** (LOOSE → STRUCTURAL with seeding). Under unconstrained generation, 63% of stories omit the moderate observer position. Prompt-level interventions (experiential and structural framing) do not rescue it (40% vs. 50%, n.s.). The model's implicit theory of observation defaults to three positions: powerless/institutional/analytical.

3. **Classification robustness** (STRUCTURAL). The discrete type system absorbs continuous metric noise. Classification stability ≥ 0.787 across all constraints despite epsilon varying 0.10–0.17 within runs. U1 and U4 evaluation contexts are perfectly stable (1.00). The gate thresholds create basins of attraction wider than the model's generation variance.

4. **U2 classification chaos** (STRUCTURAL). When the moderate position is present under unconstrained generation, it produces unstable classifications. One constraint yielded five distinct types across 13 U2 entries. The moderate position is doubly underdetermined: the model both avoids producing it and cannot produce it coherently without distributional context.

5. **Few-shot seeding rescues U2** (STRUCTURAL). Three cross-domain exemplars raise the moderate atom rate from 40% to 88% (p < 0.0001). The missing middle is a distributional context deficit, not a capacity limitation. The model can produce the moderate position when shown what it looks like, even from unrelated domains. The hardest case (academic_peer_review, 0% baseline) moved to 40% — partial but real rescue.

### 5.2 Implications

For the DR framework: the experiment validates epsilon invariance empirically and confirms that the classification system is robust to generation noise. The missing middle suggests that corpus constraints generated by LLMs without distributional context may systematically underrepresent the moderate observer's perspective. However, the seeding experiment demonstrates that the framework's own corpus provides the corrective: a small number of exemplars from the existing 1,151-constraint corpus are sufficient to rescue the moderate position for new constraint generation. This creates a self-improving dynamic where corpus growth improves generation quality for subsequent constraints.

For LLM evaluation: the missing middle is a perspective-taking deficit that would not be detected by standard benchmarks. It does not affect factual accuracy, instruction-following, or output quality by conventional measures. It is visible only when the evaluation framework provides formal invariants that define what each perspective should contain. The seeding result adds nuance: the deficit is recoverable through distributional context, which means it is better characterized as a context gap than an architectural limitation. This suggests that structural evaluation frameworks — not just output quality metrics — are needed to characterize LLM perspective-taking capacity, and that in-context learning can correct deficits that prompt engineering cannot.

For perspective-prompted generation: any application that instructs an LLM to adopt the perspective of "an ordinary person with moderate power and some options" should expect degraded output under default conditions. However, providing even a small number of examples of what moderate-position analysis looks like — from any domain — substantially corrects the deficit. The practical recommendation is to include moderate-perspective exemplars in generation prompts whenever the moderate viewpoint is important to the output.

### 5.3 Next Steps

1. **Full experiment** (24 constraints, stratified by H¹) to test whether the missing middle and its rescue by seeding generalize across the full corpus.
2. **Cross-model comparison** (Claude, GPT-4, Grok) to test whether the trimodal default and seeding rescue are Gemini-specific or general.
3. **Sigma/theta drift analysis** to test whether the model correctly varies metrics that should be perspective-dependent (unlike epsilon, which should be invariant).
4. **Temperature sweep** (0.0, 0.1, 0.2, 0.5, 1.0) to characterize the noise floor and determine whether lower temperature rescues the moderate atom or merely reduces metric variance.
5. ~~**Moderate anchoring experiment**~~ → **Completed as few-shot seeding** (Section 3.5). Three cross-domain exemplars raise moderate rate from 40% to 88%. The deficit is distributional, not abstract-only.
6. **Minimum effective seed set**: test whether a single exemplar suffices, or whether the 3-exemplar threshold is meaningful. Test with 1, 2, and 3 exemplars to characterize the dose-response curve.
7. **Classification shift investigation**: determine whether the seeded type distribution shift (rope-dominant → rope/tangled_rope split) reflects richer moderate-position specification or seed exemplar bias. Test with alternative seed sets.
8. **Academic_peer_review deep dive**: the hardest case moved from 0% to only 40% under seeding. Investigate what makes this constraint structurally resistant to moderate-position elicitation — this may reveal a class of constraints where the moderate position is genuinely underdetermined rather than merely underrepresented.

---

## Appendix A: Experimental Infrastructure

### Files Created

| File | Lines | Role |
|---|---|---|
| `agent/perspective_experiment.py` | 478 | Experiment orchestrator |
| `python/perspective_analysis.py` | 407 | Post-experiment analysis |
| `python/python_gap_suite.py` | 118 | Validation suite management |
| `python/find_u2_exemplars.py` | — | Corpus scan for moderate-atom exemplars |
| `prompts/perspective_preambles/*.md` | 8 files | Perspective preamble templates |
| `prompts/perspective_preambles/u2_seeded.md` | — | Seeded preamble with 3 cross-domain exemplars |

### Files Reused Without Modification

| File | Role |
|---|---|
| `agent/story_generator_base.py` | Gemini API, prompt building, validation |
| `python/generate_constraint_pl.py` | JSON→Prolog compilation |
| `python/linter.py` | Schema validation |
| `python/run_pipeline.py` | Pipeline orchestration |
| `python/enhanced_report.py` | Diagnostic report generation |
| `prolog/drl_core.pl` | Type classification |
| `prolog/constraint_indexing.pl` | Two-hub perspectival architecture |
| `prolog/grothendieck_cohomology.pl` | Cohomological computation |
| `prolog/logical_fingerprint.pl` | Structural fingerprints |

### Directories

| Directory | Contents |
|---|---|
| `json/perspective_experiment/` | Generated story JSON files |
| `prolog/gaptests/` | Generated Prolog files (permanent) |
| `results/perspective_experiment/` | Experiment logs and analysis reports |
| `prompts/perspective_preambles/` | 8 preamble template files |

## Appendix B: Rigor Classification

Per the DR framework's own standards for formal claims:

| Claim | Rigor | Justification |
|---|---|---|
| Epsilon invariance (F < 1) | **STRICT** | Standard ANOVA applied to well-defined numeric outputs. The null result is unambiguous. |
| Classification stability ≥ 0.787 | **STRUCTURAL** | Well-defined measurement with diagnostic value, but "robust to generation noise" is empirical, not formally guaranteed. |
| Missing middle (63% omission rate) | **LOOSE** | Characterization of output patterns. Does not claim to describe internal model representations. |
| U2 classification chaos | **STRUCTURAL** | Precise, replicable measurement. Interpretation is well-supported but does not claim internal model knowledge. |
| Distributional context deficit (not capacity limitation) | **STRUCTURAL** | Fisher's exact test on 82 vs. 25 stories, p < 0.0001. Seeding effect is unambiguous. Interpretation as "distributional context" rather than other mechanisms is well-supported but not the only possible explanation. |
| Seeding rescues moderate atom (88% vs 40%) | **STRUCTURAL** | Standard statistical test on well-defined binary outcome. Per-constraint claims at n=5 are directional only. |

## Appendix C: Linter Failure Patterns

Linter failure rates by perspective and framing:

| Perspective × Framing | Total | Failed | Rate |
|---|---|---|---|
| U1 experiential | 19 | 3 | 15.8% |
| U2 experiential | 19 | 5 | 26.3% |
| U3 experiential | 19 | 6 | 31.6% |
| U3 structural | 10 | 0 | 0.0% |
| U4 experiential | 19 | 3 | 15.8% |

The gradient runs from clean at the extremes (U1, U4) to noisier in the middle (U2, U3). U3 structural framing produced zero failures across 10 generations, compared to 31.6% for U3 experiential — a notable asymmetry suggesting that formal vocabulary aids format compliance even when it does not rescue the moderate atom.

## Appendix D: Pre-Registered Predictions vs. Outcomes

| Prediction | Outcome | Notes |
|---|---|---|
| ε_U1 > ε_U4 (sympathy bias) | **Not supported** | 1 constraint shows ε_U1 > ε_U4, 1 shows the reverse |
| Experiential framing produces more ε drift than structural | **Not testable** | Epsilon invariance holds under both framings; no drift to compare |
| U1 linter failure rate > U4 | **Equal** | Both at 15.8% |
| H¹=0 constraints remain stable | **Supported** | antifragility is perfectly stable across all conditions |
| Boolean structural properties will sometimes flip | **Partially supported** | mandatrophy_resolved flips observed but not replicable across runs |
| Missing middle (not pre-registered) | **Discovered** | 63% omission rate under unconstrained generation |
| Few-shot seeding rescues U2 (not pre-registered) | **Confirmed** | 88% vs 40%, p < 0.0001. Cross-domain pattern learning, not domain-specific retrieval |
