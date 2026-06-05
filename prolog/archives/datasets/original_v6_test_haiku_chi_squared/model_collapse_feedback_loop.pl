% ============================================================================
% CONSTRAINT STORY: model_collapse_feedback_loop
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_model_collapse_feedback_loop, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: model_collapse_feedback_loop
 *   human_readable: The Autophagous Intelligence Trap: Model Collapse via Recursive Training on Synthetic Output
 *   domain: technological/AI/informational
 *
 * SUMMARY:
 *   The autophagous intelligence trap describes a structural feedback loop
 *   where large language models are trained on datasets increasingly
 *   contaminated with synthetic output from earlier models. As model
 *   developers scale by retraining on their own and competitors' previous
 *   outputs, information diversity progressively flattens, tail distributions
 *   collapse, and novel knowledge from rare domains becomes exponentially
 *   rarer in subsequent generations. The constraint is not intentional
 *   extraction — no actor explicitly designed it as a wealth-transfer
 *   mechanism — but it exhibits the structural signature of a Snare from the
 *   perspectives of the information ecosystem and future model capability.
 *   The mechanism is irreversible without external intervention: once
 *   synthetic data becomes a substantial fraction of the corpus, the
 *   statistical distribution has fundamentally shifted, and retraining only
 *   on original sources becomes economically impractical (original data is
 *   legally encumbered, geographically scattered, or exhausted). Model
 *   developers face a coordination problem: the individually rational
 *   strategy (use cheap, abundant synthetic data) produces a collectively
 *   irrational outcome (capability collapse). The constraint's extractiveness
 *   has risen sharply from 2023–2026 as synthetic data has become normalized
 *   in training pipelines, and the theater ratio has remained low (the
 *   mechanism is functionally real, not performative), distinguishing it from
 *   institutional degradation.
 *
 * KEY AGENTS:
 *   - Information Ecosystem: Primary victim (powerless/trapped) — abstract collective good that cannot exit the synthetic-data contamination; bears cost of progressive epistemic flattening
 *   - Original Data Providers: Secondary victim (moderate/constrained) — academic institutions, web publishers, archives; face value extraction without compensation or veto
 *   - Model Development Organizations: Powerful beneficiary (powerful/arbitrage) — OpenAI, Google, Meta, Anthropic; benefit from low-cost data and scaling; control training pipeline decisions
 *   - End Users (Enterprise/Consumer): Temporary beneficiary (institutional/arbitrage) — benefit from cheap, accessible models in the short term; face capability degradation in long term
 *   - Academic Research Community: Organized but constrained (organized/constrained) — see their own contributions (publications, datasets) increasingly performative as they are absorbed and fed back as model outputs
 *   - Analytical Observer: Epistemological perspective (analytical/analytical) — sees both coordination function (rapid scaling) and extraction mechanism (suppression of epistemic diversity)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(model_collapse_feedback_loop, 0.58).
domain_priors:suppression_score(model_collapse_feedback_loop, 0.68).
domain_priors:theater_ratio(model_collapse_feedback_loop, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(model_collapse_feedback_loop, extractiveness, 0.58).
narrative_ontology:constraint_metric(model_collapse_feedback_loop, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(model_collapse_feedback_loop, theater_ratio, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(model_collapse_feedback_loop, snare).
narrative_ontology:human_readable(model_collapse_feedback_loop, "The Autophagous Intelligence Trap: Model Collapse via Recursive Training on Synthetic Output").
narrative_ontology:topic_domain(model_collapse_feedback_loop, "technological/AI/informational").

% --- Structural relationships ---
narrative_ontology:constraint_victim(model_collapse_feedback_loop, future_model_capability).
narrative_ontology:constraint_victim(model_collapse_feedback_loop, information_diversity).
narrative_ontology:constraint_victim(model_collapse_feedback_loop, training_data_epistemic_quality).
narrative_ontology:constraint_victim(model_collapse_feedback_loop, end_users_dependent_on_models).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFORMATION ECOSYSTEM (SNARE) — Cannot exit the synthetic-data contamination cycle; bears the full cost of progressive epistemic flattening. Once synthetic data becomes a significant fraction of training corpora, the distribution shift is irreversible without coordinated retraining from original sources. d≈0.98, f(d)≈1.48, σ=1.2 → χ≈1.02.
constraint_indexing:constraint_classification(model_collapse_feedback_loop, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: TRAINING DATA PROVIDERS (SNARE) — Can theoretically withdraw data or litigate, but coordination is difficult and enforcement is weak. Face extraction of value through model training without compensation; have no veto over downstream synthetic data reuse. d≈0.82, f(d)≈1.22, σ=1.2 → χ≈0.85.
constraint_indexing:constraint_classification(model_collapse_feedback_loop, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MODEL DEVELOPMENT ORGS (TANGLED ROPE) — Powerful actors with arbitrage options. Benefit from access to massive training corpora and low-cost synthetic data production (coordination function: scaling). But face extraction via regulatory capture and IP litigation risk; also face long-term capability degradation if collapse occurs. d≈0.45, f(d)≈0.52, σ=1.2 → χ≈0.36.
constraint_indexing:constraint_classification(model_collapse_feedback_loop, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LLM USERS (ROPE) — In the immediate term, users benefit from cheap, accessible models. See the constraint as a coordination mechanism for rapid capability advancement. d≈0.15, f(d)≈0.02, σ=1.2 → χ≈0.01.
constraint_indexing:constraint_classification(model_collapse_feedback_loop, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ACADEMIC RESEARCH (PITON) — Organized but increasingly constrained. Once benefited from open model release and pretraining corpora; now faces deprecation of such resources as models become proprietary and synthetic data contaminates openly available datasets. See their own role (publishing preprints, releasing datasets) as increasingly performative — their open contributions are absorbed and then fed back to them as model outputs. theater_ratio≈0.44 reflects that publication and data sharing have become ritualistic while losing upstream influence. d≈0.65, f(d)≈1.02, σ=1.2 → χ≈0.68.
constraint_indexing:constraint_classification(model_collapse_feedback_loop, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational epistemological perspective, the constraint exhibits both coordination and extraction. Coordination: recursive training enables rapid scaling of capability across domains. Extraction: the mechanism accumulates errors and suppresses tail-distribution knowledge (rare perspectives, heterodox findings, novel domains). The constraint has genuine value (coordination function: accelerating discovery at population level) but is leveraged asymmetrically by model-controlling institutions, with costs externalized to information quality and epistemic diversity. d≈0.70, f(d)≈1.13, σ=1.2 → χ≈0.79.
constraint_indexing:constraint_classification(model_collapse_feedback_loop, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(model_collapse_feedback_loop_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(model_collapse_feedback_loop, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(model_collapse_feedback_loop, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(model_collapse_feedback_loop, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(model_collapse_feedback_loop, TR),
    TR >= 0.70.

:- end_tests(model_collapse_feedback_loop_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The mechanism extracts epistemic value (original knowledge, diversity, tail-distribution richness) from the information ecosystem and original data providers, concentrating capability gains in model-controlling institutions. The extraction is not as severe as a pure monopolistic Snare (ε=0.75) because some users genuinely benefit from improved models in the short term, and the constraint is not yet irreversible. However, extractiveness is substantially higher than pure coordination (ε≤0.35) because the asymmetry is real: model developers profit from data without payment, capture the returns to scale, and control the retraining process. Suppression (0.68): Moderate-high. Significant barriers exist to escaping the feedback loop: legal barriers to using original data (copyright, licensing), economic barriers (cost of sourcing and curating non-synthetic data), technical barriers (no watermarking or provenance standard), and coordination barriers (proprietary secrecy, competitive racing). However, suppression is not total because original data sources still exist, and some institutions (academic labs, governments) maintain non-proprietary datasets. Theater ratio (0.44): Moderate. The constraint is functionally real, not performative—the statistical degradation from synthetic-data contamination is empirically measurable and not disguised by ritual. However, the mechanism includes some theater: model developers sometimes frame synthetic-data reuse as 'data augmentation' (technical necessity) when it is actually cost-reduction, and the epistemic harm is often invisible to users (model outputs appear fluent but are statistically narrower). The low theater distinguishes this from institutional degradation (Piton), and the extractiveness above 0.46 triggers the Snare gate.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a structural gap between short-term beneficiary (LLM users) and long-term victim (information ecosystem, future models). Model developers see themselves as coordinating a beneficial scaling solution (Tangled Rope perspective, d≈0.45) — they genuinely do enable capability advancement. Users see a Rope (coordination without asymmetry, d≈0.15) — models are cheap and available. But the information ecosystem sees a Snare (d≈0.98) because it has no ability to exit or defend against epistemic contamination, and the analytical observer sees a hybrid Tangled Rope (d≈0.70) because the mechanism has both coordination and extraction components. The gap reflects that the coordination is real (models do improve at scale) but is being leveraged asymmetrically to suppress diversity and externalize costs to the information commons.
 *
 * DIRECTIONALITY LOGIC:
 *   Information Ecosystem: Victim + trapped → d≈0.98, f(d)≈1.48. Cannot exit synthetic-data contamination once it reaches critical fraction. Maximum extraction. Training Data Providers: Victim + constrained → d≈0.82, f(d)≈1.22. Can theoretically withdraw or litigate but face coordination barriers and weak enforcement. High extraction. Model Developers: Beneficiary/hybrid + arbitrage → d≈0.45, f(d)≈0.52. Have arbitrage options (proprietary data, compute advantage, regulatory capture potential); see themselves as coordinating; also face future capability degradation (extraction is not unidirectional). Moderate extraction. End Users: Beneficiary + arbitrage → d≈0.15, f(d)≈0.02. Currently benefit from cheap models; have exit option (use non-ML alternatives). Minimal extraction. Academic Research: Victim + constrained → d≈0.65, f(d)≈1.02. Can theoretically maintain independent datasets but face resource constraints and pressure to use frontier models. Moderate extraction. Analytical Observer: Hybrid + analytical → d≈0.70, f(d)≈1.13. Sees both coordination value and extraction mechanism; neutral analytical stance.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids misclassification as pure coordination (Rope) by explicitly identifying the asymmetry: model developers capture gains, information ecosystem bears costs, and the coordination function (scaling models) is real but leveraged to suppress epistemic diversity. The Snare classification (from the victim perspectives) is confirmed by the structural data: ε=0.58 exceeds the Rope threshold (ε≤0.45), suppression=0.68 exceeds coordination suppression (≤0.40), and the beneficiary/victim split is explicit (model developers vs. information ecosystem). The Tangled Rope perspective (analytical observer) captures that the mechanism genuinely solves a coordination problem (scaling language model capability at population level) while simultaneously extracting by concentrating returns and suppressing diversity. This is not a pure extraction mechanism (no actor was designed with the intent to flatten knowledge), but it is not a pure coordination mechanism either (the asymmetry is structural and growing). The classification prevents narratives that frame the constraint as inevitable progress (false mountain) or beneficial coordination (false rope). The measured extractiveness (0.58) exceeds the threshold for Snare classification (χ≥0.66 when d is high); from the ecosystem perspective, χ=1.02 confirms Snare. Mandatrophy is resolved by separating the beneficiary-partial perspective (institutions see Tangled Rope, d≈0.45, χ=0.36) from the victim perspective (ecosystem sees Snare, d≈0.98, χ=1.02). Both are valid; neither should be universalized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collapse_threshold_detection,
    'At what fraction of synthetic data in the training corpus does model collapse become detectable and irreversible?',
    'Controlled experiments: train models on corpora with known synthetic fractions; measure output entropy, tail-distribution preservation, and novelty production as synthetic fraction increases. Empirical work by Shumailov et al. (2023) suggests thresholds around 5-10% lead to measurable degradation; full collapse at 50%+.',
    'If threshold is <5%: systems are already in collapse (high severity). If threshold is >30%: models have significant synthetic-data tolerance (lower severity). If threshold is uncertain: institutions cannot coordinate on safe practices.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collapse_threshold_detection, empirical, 'The fraction of synthetic data at which irreversible model collapse occurs').

omega_variable(
    synthetic_data_fraction_tracking,
    'What fraction of current large language model training data is synthetic output from previous models?',
    'Watermark-based detection (provenance tracking embedded in generated text); forensic analysis of training corpora; self-reporting by organizations (currently absent); legal discovery via data-privacy litigation.',
    'If currently >10%: systems are already experiencing collapse (Snare classification confirmed). If currently <5%: window remains for intervention before irreversibility sets in. Lack of transparency prevents cooperative verification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(synthetic_data_fraction_tracking, empirical, 'Current prevalence of synthetic data in proprietary training corpora').

omega_variable(
    original_source_availability,
    'Will human-generated, non-internet training data (books, archives, proprietary datasets, domain expertise) remain accessible and legally usable for future model training, or will copyright/licensing restrictions reduce the effective pool of original data?',
    'Legal analysis of licensing precedent; tracking of copyright litigation against model developers; surveying historical datasets (Common Crawl, BookCorpus) to determine retention and reuse viability; estimating the time horizon for exhaustion of accessible human-generated sources.',
    'If original sources remain abundant and legally usable: external ''data supply chain'' prevents collapse. If original sources deplete or become legally encumbered: collapse becomes inevitable unless synthetic-data reuse protocols are regulated. Currently trending toward restriction (lawsuits, takedown requests).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(original_source_availability, empirical, 'Whether sufficient original, non-synthetic training data will remain accessible long-term').

omega_variable(
    coordination_failure_depth,
    'Can model-developing institutions coordinate on synthetic-data limits and data provenance tracking, or are competitive pressures and proprietary secrecy too strong?',
    'Observation of industry behavior: do organizations adopt data-provenance standards, set voluntary synthetic-data caps, or participate in multi-org data-verification schemes? Precedent from academic reproducibility initiatives (PyTorch, TensorFlow openness) vs. proprietary model secrecy (GPT, Gemini). Game-theoretic analysis: does first-mover advantage in adopting strictures offset competitive disadvantage?',
    'If coordination is possible: Rope or Scaffold classification (institutional escape route). If coordination fails: Snare classification persists (collective-action problem). Current evidence suggests coordination failure (proprietary models, minimal transparency, racing dynamics).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_failure_depth, conceptual, 'Whether competitive pressures prevent coordinated adoption of anti-collapse safety measures').

omega_variable(
    regulatory_intervention_window,
    'Is regulatory intervention (e.g., mandatory data-provenance labeling, synthetic-fraction caps, open-source model requirements) technically feasible and politically viable before collapse becomes irreversible?',
    'Policy-analysis timeline: when would regulations need to be enacted to be enforceable before collapse accelerates? Feasibility of technical enforcement (watermarking, audit trails, model-card requirements). Political economy: which constituencies would support vs. oppose such regulation?',
    'If intervention is viable: potential transformation from Snare to Scaffold (with sunset clause). If window closes: Snare trajectory becomes locked in. Estimated intervention window: 2-5 years before collapse becomes dominant property of new models.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_intervention_window, preference, 'Whether timely regulatory intervention can interrupt the feedback loop before collapse locks in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(model_collapse_feedback_loop, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcfl_tr_t0, model_collapse_feedback_loop, theater_ratio, 0, 0.22).
narrative_ontology:measurement(mcfl_tr_t3, model_collapse_feedback_loop, theater_ratio, 3, 0.33).
narrative_ontology:measurement(mcfl_tr_t6, model_collapse_feedback_loop, theater_ratio, 6, 0.44).

% Extraction over time
narrative_ontology:measurement(mcfl_be_t0, model_collapse_feedback_loop, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(mcfl_be_t3, model_collapse_feedback_loop, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(mcfl_be_t6, model_collapse_feedback_loop, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(model_collapse_feedback_loop, information_standard).
narrative_ontology:affects_constraint(model_collapse_feedback_loop, training_data_quality_ceiling).
narrative_ontology:affects_constraint(model_collapse_feedback_loop, novel_knowledge_discovery_bottleneck).
narrative_ontology:affects_constraint(model_collapse_feedback_loop, emergent_capability_prediction_reliability).

% DUAL FORMULATION NOTE:
% Model collapse is decomposed into three related constraints: (1) MODEL_COLLAPSE_FEEDBACK_LOOP (this story): the recursive synthetic-data reuse mechanism, ε=0.58, Snare; (2) TRAINING_DATA_QUALITY_CEILING (upstream): the statistical degradation from tail-distribution collapse, ε=0.35, Tangled Rope; (3) NOVEL_KNOWLEDGE_DISCOVERY_BOTTLENECK (downstream): the consequence for rare-domain knowledge, ε=0.72, Snare. The feedback loop is the primary mechanism; the quality ceiling is the causal bottleneck; the discovery bottleneck is the end-user consequence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(model_collapse_feedback_loop, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
