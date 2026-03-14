% ============================================================================
% CONSTRAINT STORY: large_language_model_training_data_provenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_large_language_model_training_data_provenance, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: large_language_model_training_data_provenance
 *   human_readable: LLM Training Data Provenance and Attribution Asymmetry
 *   domain: artificial_intelligence/intellectual_property
 *
 * SUMMARY:
 *   Large language models are trained on billions of documents sourced from
 *   the internet, academic repositories, books, and other public and private
 *   sources. The constraint governing this data pipeline creates a
 *   fundamental asymmetry: model developers extract and concentrate immense
 *   value from aggregated content, while original creators receive no
 *   attribution, compensation, or even disclosure of their contribution. The
 *   constraint exhibits tangled rope structure because it combines genuine
 *   coordination (large-scale reproducible ML requires standardized training
 *   data) with genuine extraction (the asymmetric value capture, suppressed
 *   attribution, and concentrated benefits). The theater ratio (0.65)
 *   reflects the performative compliance theater: model cards that claim
 *   diversity without documenting sources, ethics statements without
 *   verifying consent, transparency reports that obscure training
 *   composition. The extractiveness (0.58) reflects moderate but significant
 *   value asymmetry: creators are displaced in career value and market
 *   opportunity, while developers capture the value of billions of
 *   human-created documents in proprietary weights. The suppression (0.68)
 *   reflects high structural barriers to exit or negotiation: creators cannot
 *   prevent use, cannot track use, cannot condition use, and have weak legal
 *   standing in many jurisdictions.
 *
 * KEY AGENTS:
 *   - Original Content Creators: Primary victims (powerless/trapped) — writers, artists, researchers, code authors whose work was used without consent; suffer career harm, attribution theft, and market displacement
 *   - Model Developers / Commercial LLM Providers: Primary beneficiaries (institutional/arbitrage) — capture immense value from aggregated content; benefit from standardized training data enabling reproducible scaling
 *   - Independent AI Researchers: Secondary victims (moderate/constrained) — benefit from access to trained models but constrained by closed training regimes, unable to access or audit training data
 *   - Regulatory Bodies / Policymakers: Organized actors (organized/constrained) — have potential enforcement power but captured by economic importance of model developers; face genuine multi-jurisdictional coordination challenges
 *   - Copyright Regime / IP Enforcement: Institutional actor (institutional/arbitrage) — persists through legal inertia but functionally degraded; cannot enforce individual attribution or licensing at scale (piton perspective)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees inseparable combination of coordination need and extraction mechanism; tangled rope classification is diagnostic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(large_language_model_training_data_provenance, 0.58).
domain_priors:suppression_score(large_language_model_training_data_provenance, 0.68).
domain_priors:theater_ratio(large_language_model_training_data_provenance, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(large_language_model_training_data_provenance, extractiveness, 0.58).
narrative_ontology:constraint_metric(large_language_model_training_data_provenance, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(large_language_model_training_data_provenance, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(large_language_model_training_data_provenance, tangled_rope).
narrative_ontology:human_readable(large_language_model_training_data_provenance, "LLM Training Data Provenance and Attribution Asymmetry").
narrative_ontology:topic_domain(large_language_model_training_data_provenance, "artificial_intelligence/intellectual_property").

domain_priors:requires_active_enforcement(large_language_model_training_data_provenance).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(large_language_model_training_data_provenance, model_developers).
narrative_ontology:constraint_beneficiary(large_language_model_training_data_provenance, commercial_llm_providers).
narrative_ontology:constraint_victim(large_language_model_training_data_provenance, original_content_creators).
narrative_ontology:constraint_victim(large_language_model_training_data_provenance, training_data_epistemic_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ORIGINAL CONTENT CREATOR (SNARE) — Writers, artists, researchers whose work was used without consent or attribution cannot exit. Their content has been incorporated into weights of systems generating billions in value. No recourse, no negotiation power, no alternative to having their work extracted. The extraction is maximal from this position: career harm through plagiarism, attribution theft, market displacement, and zero compensation.
constraint_indexing:constraint_classification(large_language_model_training_data_provenance, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDEPENDENT AI RESEARCHER (TANGLED ROPE) — Benefits from access to powerful models and pretraining infrastructure (coordination). But also trapped by the necessity of using these models for career advancement while simultaneously being unable to access or verify training data composition. Constrained by the dominance of closed training regimes. Extraction is high but not maximal — they have some agency through academic independence and can theoretically train alternative models, but the resource and opportunity costs are severe.
constraint_indexing:constraint_classification(large_language_model_training_data_provenance, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: MODEL DEVELOPER / COMMERCIAL PROVIDER (ROPE) — Experiences the constraint as pure coordination: standardizing on web-scale training data enables reproducible scaling laws, benchmark comparison, and ecosystem development. From this position, the constraint solves a collective action problem (who decides what training data is acceptable?) while generating tremendous value capture. Net beneficiary — the extraction runs toward this agent.
constraint_indexing:constraint_classification(large_language_model_training_data_provenance, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY BODY / POLICYMAKER (TANGLED ROPE) — Organized agents (copyright offices, data protection authorities, legislatures) have agency to regulate but face genuine coordination challenges: training data requirements are global, enforcement is expensive, and regulatory burden could concentrate AI development in countries with weakest enforcement. Coordinating verification would solve a real problem. But the constraint also extracts: regulators are captured by the economic importance of model developers, creating enforcement theater (token compliance, hollow transparency reports) without structural change.
constraint_indexing:constraint_classification(large_language_model_training_data_provenance, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COPYRIGHT REGIME (PITON) — Traditional copyright enforcement mechanisms (takedown notices, licensing agreements, individual rights holders) are performative theater with respect to LLM training. The regime persists through institutional inertia and legal tradition, but its actual function is degraded: copying billions of works at scale with computational aggregation defeats the individualized attribution model copyright assumes. The regime cannot verify compliance, cannot track uses, cannot enforce remedies. Yet it continues as ritual compliance, not actual protection.
constraint_indexing:constraint_classification(large_language_model_training_data_provenance, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both genuine coordination (the need for standardized training data to enable scaling research) and genuine extraction (asymmetric value capture, suppressed attribution, concentrated benefits). The constraint combines both functions in an inseparable way — you cannot have reproducible LLM development without large-scale data aggregation, and you cannot do that at scale while maintaining individual attribution. This is the diagnostic perspective for tangled rope classification.
constraint_indexing:constraint_classification(large_language_model_training_data_provenance, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(large_language_model_training_data_provenance_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(large_language_model_training_data_provenance, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(large_language_model_training_data_provenance, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(large_language_model_training_data_provenance, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(large_language_model_training_data_provenance, TR),
    TR >= 0.70.

:- end_tests(large_language_model_training_data_provenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting asymmetric value capture. Creators are displaced from potential licensing revenue, market access, and attribution benefits. Model developers capture the value of billions of documents in proprietary systems generating billions in market value. However, the extraction is not maximal (0.85+) because some creators benefit indirectly (increased reach, training signal for their own work, participation in model improvement), and because attribution mechanisms could theoretically be retrofitted. The trajectory over time (0.32→0.58) shows increasing extractiveness as the market matures and suppression becomes more entrenched. Suppression (0.68): High. Multiple barriers prevent creators from negotiating, opting out, or receiving compensation: (1) Technical: training data is aggregated irreversibly into weights, making post-hoc identification difficult. (2) Legal: copyright enforcement is weak for computational uses in most jurisdictions; fair use covers most training scenarios. (3) Economic: creators lack bargaining power against dominant model providers. (4) Epistemic: creators often don't know their work was used. Theater ratio (0.65): Elevated. Substantial performative compliance includes model cards (claim diversity without documenting sources), transparency reports (publish aggregate statistics while obscuring composition), ethics statements (appeal to values without enforcing consent). The theater increased over the interval as regulatory pressure mounted and developers adopted compliance theater to deflect enforcement. Claimed type (Tangled Rope): Required because the constraint combines both coordination and extraction inseparably. You cannot do large-scale LLM development at current efficiency without aggregating diverse training data. That aggregation inherently obscures individual contribution. The constraint is genuinely functional (enables reproducible scaling research) and genuinely extractive (concentrates value, suppresses attribution).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The original creator sees pure extraction (Snare): their work has been taken without consent or compensation, with no possible exit. The model developer sees pure coordination (Rope): standardized training data solves the scaling problem and enables reproducible research. The independent researcher sees hybrid constraint (Tangled Rope): they benefit from model access and scaling research but are blocked from accessing training data sources. The regulator sees a coordination problem with capture (Tangled Rope): enforcement is genuinely hard at global scale, but also sees that dominant providers are suppressing transparency. The copyright regime sees its own degradation (Piton): traditional enforcement mechanisms (individual licensing, takedown notices) cannot function at scale; the ritual persists but the function is lost. The analytical observer (civilizational scope) sees the tangled rope clearly: this constraint cannot be solved by improving attribution mechanisms alone, because the coordination requirement (large-scale data) and the extraction requirement (concentrated benefits) are structurally inseparable at current technology and business models. The gap reveals how the same constraint can appear as pure extraction, pure coordination, hybrid, and degraded theater depending on structural position.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position. Original creators are trapped (no exit options) and victims of extraction, producing high d (≈0.95) and maximum experienced extractiveness. Model developers are beneficiaries with arbitrage options (they can choose training data sources, licensing models, jurisdictions), producing low d (≈0.05) and negative or minimal experienced extractiveness. Independent researchers are moderate-power constrained agents who are both beneficiaries (model access) and victims (limited access to training data), producing mid d (≈0.60) and moderate experienced extractiveness. Regulatory bodies are organized actors with constrained exit (cannot abandon regulation of AI) but real agency, producing mid-high d (≈0.55). The copyright regime is institutional with arbitrage options (can adapt enforcement mechanisms) but functionally degraded, producing moderate d (≈0.50). The analytical observer sees the full structural picture, producing d of approximately 0.72 (observer seeing extraction from multiple victim perspectives). The sigmoid f(d) produces experienced extractiveness χ for each perspective, driving classification differences.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by recognizing that the coordination and extraction functions are genuinely entangled. The question 'Is this coordination or extraction?' has no single answer because both are true. The coordination function (standardizing on web-scale training data) is real and valuable — it enables reproducible scaling research that would be impossible with balkanized training datasets. The extraction function (concentrating value, suppressing attribution) is also real and severe — it displaces creators and concentrates market power. The mandatrophy is resolved by accepting both as irreducible features of the current implementation. The resolution path is not to classify correctly (it's both) but to identify where the separation could occur. Technically, a training regime with documented, licensed, or consensual data could maintain the coordination benefit while reducing extraction (Open vs Closed parity omega tests this). Legally, a rights-clearing framework could verify consent before aggregation (Consent Aggregation omega tests this). Economically, a value-sharing mechanism could compensate creators post-hoc (Value Realization Timeline omega tests this). The tangled rope classification is diagnostic: it tells you that the current constraint is not a natural law (not a mountain) and not pure coordination (not a rope), but a specific institutional arrangement that could be restructured. The extractiveness (0.58) and suppression (0.68) are high enough to warrant policy intervention; the theater (0.65) is high enough to flag compliance mechanisms as potentially hollow; the coordination function is genuine enough that prohibition is not optimal. Restructuring is the indicated path.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consent_aggregation_impossibility,
    'Is individual consent for training data aggregation at billion-scale technically feasible, or is it a coordination problem inherent to large-scale machine learning?',
    'Design analysis of consent frameworks (federated data governance, opt-in registries, granular licensing); cost-benefit analysis of consent infrastructure vs training efficiency; comparison to biological precedents (tissue banks, longitudinal studies) that solved similar problems',
    'If feasible: the current suppression of consent is pure policy choice (Snare strengthens). If infeasible: the constraint contains genuine coordination difficulty (Tangled Rope is correct). If partially feasible: hybrid models exist that current regimes are not deploying (reveals extraction hidden behind false technical necessity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consent_aggregation_impossibility, empirical, 'Whether consent aggregation at scale is technically feasible').

omega_variable(
    attribution_reconstruction_cost,
    'Can the contribution of specific training documents to LLM outputs be computationally reconstructed after training, or is training irreversibly aggregates data into weights?',
    'Empirical testing of attribution reconstruction methods (influence functions, mechanistic interpretation, attention probing); cost analysis of reconstruction vs training efficiency; comparison to other large-scale systems that track provenance after aggregation',
    'If reconstructible: post-hoc attribution is possible, shifting extraction mechanism from erasure to delayed compensation (reduces suppression, clarifies tangled rope structure). If irreversible: attribution loss is inherent, and transparency claims are theater (increases suppression, strengthens snare view).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(attribution_reconstruction_cost, empirical, 'Whether training data attribution can be reconstructed post-hoc').

omega_variable(
    value_realization_timeline,
    'What is the actual timeline from model release to market value realization, and does it create a window for just-in-time data attribution and compensation?',
    'Empirical tracking of model release → commercialization → revenue realization for major LLM systems; analysis of whether interim stages (beta period, pretraining sharing, initial deployment) provide enforcement points for retroactive consent or compensation',
    'If timeline is long (>2 years): compensation mechanisms can reach creators before major value extraction (softens snare). If immediate (<6 months): value extraction happens before attribution is possible (hardens snare classification).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(value_realization_timeline, empirical, 'Timeline from model release to market value realization').

omega_variable(
    open_vs_closed_training_parity,
    'Do open-source models trained on documented, consensual data (books, academic papers, licensed datasets) reach performance parity with closed models trained on web-scale undisclosed data, or does the web-scale advantage remain insurmountable?',
    'Longitudinal performance comparison: benchmark scores (MMLU, MATH, coding tasks) for open-source vs closed models controlling for compute budget; analysis of performance trajectories and convergence',
    'If parity achievable: the suppression argument (''web scale is necessary'') is false, and the constraint is pure extraction theater (Snare strengthens). If web-scale necessary: genuine coordination challenge exists (Tangled Rope holds). If parity emerging: scaffold sunset is real (transitory nature of the constraint becomes clear).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(open_vs_closed_training_parity, empirical, 'Performance parity of open-source vs closed LLMs').

omega_variable(
    multi_national_regulatory_capture,
    'Can the EU''s AI Act, China''s algorithm governance, and emerging US regulation create enforceable training data transparency standards, or does regulatory arbitrage allow concentration in jurisdictions with weakest enforcement?',
    'Empirical tracking of model developer locations; analysis of investment and development location choices post-regulation; measurement of regulatory capture intensity (lobbying spend, advisory board composition) in each jurisdiction',
    'If enforceable global standard achieved: regulatory bottleneck becomes real coordination (Tangled Rope softens toward Scaffold). If regulatory arbitrage prevents coordination: the constraint persists as extractive with global scope (Snare perspective strengthens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multi_national_regulatory_capture, empirical, 'Feasibility of multi-national training data transparency enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(large_language_model_training_data_provenance, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(llm_prov_tr_t0, large_language_model_training_data_provenance, theater_ratio, 0, 0.35).
narrative_ontology:measurement(llm_prov_tr_t3, large_language_model_training_data_provenance, theater_ratio, 3, 0.55).
narrative_ontology:measurement(llm_prov_tr_t6, large_language_model_training_data_provenance, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(llm_prov_be_t0, large_language_model_training_data_provenance, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(llm_prov_be_t3, large_language_model_training_data_provenance, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(llm_prov_be_t6, large_language_model_training_data_provenance, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(large_language_model_training_data_provenance, resource_allocation).
narrative_ontology:boltzmann_floor_override(large_language_model_training_data_provenance, 0.18).
narrative_ontology:affects_constraint(large_language_model_training_data_provenance, copyright_enforcement_digital_scale).
narrative_ontology:affects_constraint(large_language_model_training_data_provenance, ai_model_bias_provenance).
narrative_ontology:affects_constraint(large_language_model_training_data_provenance, creator_compensation_labor_extraction).

% DUAL FORMULATION NOTE:
% LLM training data provenance decomposes into three related constraints: (1) Copyright Enforcement at Digital Scale (ε≈0.35, Piton) — traditional IP enforcement theater that cannot function at training data volumes. (2) AI Model Bias Originating in Training Data (ε≈0.42, Tangled Rope) — coordination benefit (understanding bias sources) entangled with extraction (using bias knowledge to refine suppression). (3) Creator Compensation as Labor Extraction (ε≈0.62, Snare) — pure extraction of creative labor value with suppressed compensation. The current story models the aggregate constraint at the provider/creator interface. Decomposition enables finer analysis of specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(large_language_model_training_data_provenance, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
