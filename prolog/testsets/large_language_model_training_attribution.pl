% ============================================================================
% CONSTRAINT STORY: large_language_model_training_attribution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_large_language_model_training_attribution, []).

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
 *   constraint_id: large_language_model_training_attribution
 *   human_readable: LLM Training Data Attribution and Consent Extraction
 *   domain: artificial_intelligence/intellectual_property/labor
 *
 * SUMMARY:
 *   LLM training data attribution represents a structural extraction
 *   mechanism where the consent and attribution infrastructure built over
 *   centuries for intellectual property (copyright, licensing, royalty
 *   systems) has been functionally disabled by the scale and opacity of
 *   machine learning. Training data creators — writers, programmers, artists,
 *   researchers — have had their work incorporated into commercial models
 *   without consent, compensation, or attribution. The constraint exhibits
 *   Snare characteristics (high extraction, high suppression, trapped exit
 *   for creators) combined with institutional piton dynamics (copyright
 *   enforcement is performatively maintained but technically infeasible
 *   against opaque weights) and reform scaffold dynamics (regulatory
 *   frameworks mandating licensing and auditability). The extractiveness has
 *   increased from 0.35 to 0.58 over six years as model scale expanded,
 *   training data acquisition accelerated, and the value differential between
 *   model developers and content creators widened. Theater ratio increased
 *   from 0.42 to 0.64 as enforcement efforts (copyright litigation, fair use
 *   claims) became more performative relative to actual constraint on model
 *   development.
 *
 * KEY AGENTS:
 *   - Training Data Creators: Primary victims (powerless/trapped) — writers, programmers, artists whose work was scraped without consent; no exit from being part of digital commons
 *   - Content Copyright Holders: Primary victims (powerless/trapped) — publishers, studios, music labels unable to prevent or license derivative training use
 *   - Displaced Sector Workers: Secondary victims (moderate/constrained) — professionals in writing, coding, creative work facing wage compression and automation
 *   - LLM Developers: Primary beneficiaries (institutional/arbitrage) — access to free training signal; ability to choose data sources and model architectures
 *   - Copyright and Attribution Regime: Institutional actor (institutional/arbitrage) — legal and enforcement systems maintaining performative copyright protection despite technical infeasibility
 *   - Consent and Licensing Reform Movement: Organized challengers (organized/constrained) — unions, creator guilds, regulatory bodies building alternative licensing and compensation mechanisms
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional choices (no attribution infrastructure) as immutable laws of learning
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(large_language_model_training_attribution, 0.58).
domain_priors:suppression_score(large_language_model_training_attribution, 0.72).
domain_priors:theater_ratio(large_language_model_training_attribution, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(large_language_model_training_attribution, extractiveness, 0.58).
narrative_ontology:constraint_metric(large_language_model_training_attribution, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(large_language_model_training_attribution, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(large_language_model_training_attribution, snare).
narrative_ontology:human_readable(large_language_model_training_attribution, "LLM Training Data Attribution and Consent Extraction").
narrative_ontology:topic_domain(large_language_model_training_attribution, "artificial_intelligence/intellectual_property/labor").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(large_language_model_training_attribution, llm_developers).
narrative_ontology:constraint_victim(large_language_model_training_attribution, training_data_creators).
narrative_ontology:constraint_victim(large_language_model_training_attribution, content_copyright_holders).
narrative_ontology:constraint_victim(large_language_model_training_attribution, future_displaced_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAINING DATA CREATOR (SNARE) — Writers, artists, coders whose work was scraped without consent and used to train commercial models. Trapped by the structural reality that all digital content is now implicit training data. No viable exit: choosing not to publish online is no longer economically viable; retracting published work does not remove derivatives already ingested into trained models. Maximum suppression: retroactive consent is unenforceable; no mechanism to opt out of derivative training. Extraction is maximal — intellectual property value flows unidirectionally to model developers.
constraint_indexing:constraint_classification(large_language_model_training_attribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COPYRIGHT HOLDER (SNARE) — Publishers, film studios, music labels whose copyrighted material was ingested into training sets without licensing fees. Trapped between legal uncertainty (fair use doctrine applied to massive scale is contested), technical infeasibility (cannot practically verify what's in a trained model), and asymmetric power (individual rights holders vs. consolidated model developers). Suppression is extreme: legal remedies are slow, expensive, and uncertain; technical detection is nearly impossible. The extraction is pure: licensing revenue flows to developers via model value, not to rights holders.
constraint_indexing:constraint_classification(large_language_model_training_attribution, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: DISPLACED SECTOR WORKER (TANGLED ROPE) — Programmers, copywriters, illustrators whose labor market is being automated by LLM-trained models. Constrained by retraining costs, geographic mobility limits, and credential barriers in adjacent fields. Some coordination function exists: open-source model development and collaborative AI research create new roles. But asymmetric extraction dominates: the professional class faces wage compression and deskilling while model developers capture productivity gains. The tension between coordination (shared infrastructure) and extraction (asymmetric capture of value) is the defining feature.
constraint_indexing:constraint_classification(large_language_model_training_attribution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LLM DEVELOPER (ROPE) — Training data aggregation from public sources is framed as technical coordination: sourcing the diverse, high-quality training signal needed to build effective models. This perspective experiences the constraint as enabling cooperation — without consolidated training corpora, no single developer could build competitive models. The arbitrage exit (can choose which training sources to include/exclude, can develop alternative models) makes extraction appear minimized from this view. But this reading ignores that coordination benefits concentrate while extraction costs disperse globally.
constraint_indexing:constraint_classification(large_language_model_training_attribution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COPYRIGHT/ATTRIBUTION REGIME (PITON) — Intellectual property and creator attribution systems were designed for a world of scarcity (printing, distribution costs). They operated through licensing (permissions) and detection (enforcement). In the LLM era, detection is technically infeasible (weights are opaque) and licensing is structurally impossible (cannot retroactively license training data at inference time). The regime persists through legal theater (copyright claims against model developers, fair use litigation) and institutional inertia (publishers and studios maintain IP enforcement apparatus) even though the functional capacity to enforce attribution has largely vanished. Theater ratio elevated by the performative nature of enforcement efforts against technically undetectable derivative use.
constraint_indexing:constraint_classification(large_language_model_training_attribution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSENT/LICENSING REFORM COALITION (SCAFFOLD) — Organized efforts (artist collectives, labor unions, creator guilds, regulatory bodies) to mandate opt-in training data licensing, model auditability, and creator compensation. Constrained by incumbent developer power and the technical infeasibility of retroactive enforcement. But this coalition has real agency and a sunset logic: mandatory licensing frameworks (EU AI Act, proposed US regulation) are creating alternative pathways where model developers pay for training data access and creators maintain consent control. As legal enforcement mechanisms mature and technical verification improves, the extraction mechanism loses force. Sunset estimated: 5-10 years for licensing norms to crystallize in regulated jurisdictions.
constraint_indexing:constraint_classification(large_language_model_training_attribution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN CANDIDATE) — From a civilizational/universal view, large-scale pattern extraction from distributed sources appears as a natural law of intelligence: all learning (human and machine) requires aggregating training signal from accessible sources. The scale of LLM training (billions of examples) is simply the quantitative extension of natural learning. This perspective risks naturalizing what is actually a contingent institutional choice: the option to aggregate without consent exists only because attribution tracking and licensing infrastructure for digital training data was never built. The mountain reading is a false summit — structural data reveals the constraint is a Snare/Piton hybrid, not an immutable law.
constraint_indexing:constraint_classification(large_language_model_training_attribution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(large_language_model_training_attribution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(large_language_model_training_attribution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(large_language_model_training_attribution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(large_language_model_training_attribution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(large_language_model_training_attribution, TR),
    TR >= 0.70.

:- end_tests(large_language_model_training_attribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Training data developers capture substantial value from access to unpaid training data. The value is real but not total extraction — some creators benefit indirectly from improved model capabilities (open-source developers, researchers using LLMs for their own work). The 0.58 value reflects that extraction is significant but constrained by the technical difficulty of attribution and the availability of some alternative data sources (synthetic, licensed). Suppression (0.72): Very high. Structural barriers to exit include: (1) retroactive consent is impossible for already-published work, (2) technical infeasibility of identifying which specific works influenced model outputs, (3) asymmetric legal resources (individual creators vs consolidated developers), (4) economic necessity of digital publishing (cannot opt out without abandoning professional platforms), (5) collective action problems (hard to coordinate across millions of creators globally). Theater ratio (0.64): Moderate-high. The constraint combines real functional extraction with significant performative elements. Enforcement (copyright litigation, fair use doctrine application, regulatory pressure) is structurally slow and uncertain. Meanwhile, the actual constraint (lack of attribution and consent infrastructure) is a technical problem that litigation cannot solve. The rise in theater ratio reflects increasing regulatory and legal theater without proportional increase in functional constraint on model development.
 *
 * PERSPECTIVAL GAP:
 *   The core gap: creators and rights holders perceive maximum Snare (trapped, powerless, global scope) while developers perceive Rope (institutional, arbitrage, immediate horizon, global scope). The same data flow (training examples → model training → model outputs) is experienced as pure extraction by powerless victims and as necessary coordination by institutional beneficiaries. The reform movement's Scaffold perspective is neither Snare nor Rope but a genuinely different structural reading: this is a temporary institutional failure with a real sunset (regulatory enforcement + technical attribution maturation = extraction mechanism collapses). The analytical observer's Mountain is a false summit: 'learning requires training data' is true, but 'learning requires untraceable, unlicensed, unconsentted training data' is a contingent choice, not a law of nature.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's extraction asymmetry is structural, not perspectival. Creators are victims + trapped → high d ≈ 0.95 → f(d) ≈ 1.42 → experienced χ very high. Developers are beneficiaries + arbitrage → low d ≈ 0.15 → f(d) ≈ -0.01 → experienced χ near-zero or negative (they perceive the constraint as enabling, not extractive). At global scope σ = 1.2, the formula χ = ε × f(d) × σ(S) produces dramatically different effective extractiveness depending on position. Creator's χ ≈ 0.58 × 1.42 × 1.2 ≈ 0.99 (Snare territory). Developer's χ ≈ 0.58 × (-0.01) × 1.2 ≈ -0.01 (Rope or negative). The same base extraction (0.58) yields opposite classifications from opposite positions. This is diagnostic: the constraint is a Snare viewed from below and a Rope viewed from above.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION RESOLUTION: This constraint could misclassify as pure extraction (Snare) OR as legitimate coordination (Rope), depending on which perspective dominates. The mandatrophy is resolved by recognizing that the constraint is genuinely a Snare from victims' perspectives and a Rope from developers' perspectives — not because one is wrong, but because directionality captures real structural asymmetry. The false summit risk is in the Mountain perspective: 'learning requires training data' could naturalize the extraction as immutable law. The schema's tests catch this: the Mountain classification requires emerges_naturally = true, accessibility_collapse ≥ 0.85, resistance ≤ 0.15. But the structural data contradicts this — the constraint has high suppression (0.72) and moderate theater (0.64), indicating institutional enforcement and institutional inertia, not natural law. The engine flags this as a false summit: the naturalization fails validation. The true classification is Snare (primary) with Scaffold resolution pathway, not Mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_attribution_feasibility,
    'Is it technically feasible to audit which specific training examples contributed to LLM outputs, and if so, would attribution be practically enforceable?',
    'Development of reliable influence functions, integrated gradients, or other attribution methods; empirical testing against adversarial source manipulation; cost analysis of enforcement',
    'If feasible and cost-effective: enables licensing and creator compensation; reclassifies from Snare to Tangled Rope (extraction persists but with consent and flow-through). If infeasible: attribution remains performative theater; extraction mechanism is permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_attribution_feasibility, empirical, 'Whether technical LLM attribution methods are practically enforceable').

omega_variable(
    fair_use_doctrine_scope,
    'Will courts apply fair use doctrine narrowly (training is transformative use) or broadly (scale and market displacement negate fair use protection)?',
    'Litigation outcomes; regulatory guidance from Copyright Office; international harmonization of training data exceptions',
    'Narrow fair use = Snare persists. Broad fair use = developers retain extraction but within legal bounds, reclassifying to Tangled Rope or Rope depending on compensation mechanisms. Licensing required = Scaffold succeeds, extraction mechanism collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fair_use_doctrine_scope, preference, 'Court interpretation of fair use for LLM training').

omega_variable(
    consent_mechanism_viability,
    'Can a practical consent and licensing mechanism be built for training data that avoids tragedy-of-the-commons and free-rider problems?',
    'Pilot licensing frameworks; data cooperatives; blockchain-based attribution registries; empirical measurement of licensing overhead costs',
    'If viable and affordable: enables creator agency; transforms Snare to cooperative Rope or constrained Tangled Rope. If mechanisms break down: extraction persists, but now with performative consent (theater ratio increases).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(consent_mechanism_viability, empirical, 'Viability of practical training data consent mechanisms').

omega_variable(
    labor_market_displacement_timeline,
    'At what rate will LLM capability gains translate into labor market displacement for writing, coding, creative work, and when will reskilling become economically infeasible?',
    'Wage impact studies; labor displacement tracking; reskilling cost analysis; comparison to historical automation waves',
    'Slow displacement (>20 years) with accessible reskilling: Tangled Rope remains manageable. Fast displacement (<5 years) with reskilling infeasibility: reclassifies to Snare for displaced worker perspective. Determines whether Tangled Rope is stable or degrading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(labor_market_displacement_timeline, empirical, 'Timeline and feasibility of labor market adjustment to LLM displacement').

omega_variable(
    open_source_vs_proprietary_extraction,
    'Do open-source LLM training practices exhibit materially lower extraction than proprietary models, or does openness merely shift extraction from creators to training compute infrastructure providers?',
    'Comparison of training data sourcing practices; analysis of value capture across open vs proprietary stacks; attribution auditing of open models',
    'If open-source exhibits lower extraction: Scaffold perspective valid — can bypass proprietary Snare. If extraction merely shifts: Scaffold is aspirational; underlying Snare mechanism persists in infrastructure layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(open_source_vs_proprietary_extraction, empirical, 'Whether open-source training practices reduce extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(large_language_model_training_attribution, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(llm_attr_tr_t0, large_language_model_training_attribution, theater_ratio, 0, 0.42).
narrative_ontology:measurement(llm_attr_tr_t3, large_language_model_training_attribution, theater_ratio, 3, 0.55).
narrative_ontology:measurement(llm_attr_tr_t6, large_language_model_training_attribution, theater_ratio, 6, 0.64).

% Extraction over time
narrative_ontology:measurement(llm_attr_be_t0, large_language_model_training_attribution, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(llm_attr_be_t3, large_language_model_training_attribution, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(llm_attr_be_t6, large_language_model_training_attribution, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(large_language_model_training_attribution, information_standard).
narrative_ontology:affects_constraint(large_language_model_training_attribution, copyright_enforcement_computational_opacity).
narrative_ontology:affects_constraint(large_language_model_training_attribution, labor_market_automation_displacement).
narrative_ontology:affects_constraint(large_language_model_training_attribution, data_licensing_infrastructure).

% DUAL FORMULATION NOTE:
% LLM training attribution is part of a constraint family: (1) Copyright enforcement against opaque neural networks (upstream piton: legal regime unable to enforce attribution); (2) Training data licensing infrastructure (structural prerequisite: how to build attribution-aware data markets); (3) Labor displacement in creative sectors (downstream snare: workers facing wage compression from model-generated outputs). The attribution constraint is the mechanism linking upstream copyright failure to downstream labor displacement. Each story has distinct epsilon: copyright enforcement as piton (high theater, low functional extraction), attribution mechanism as snare (high extraction but via institutional absence, not active coercion), labor displacement as tangled rope (mixed coordination and extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(large_language_model_training_attribution, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
