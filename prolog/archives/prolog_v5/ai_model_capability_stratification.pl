% ============================================================================
% CONSTRAINT STORY: ai_model_capability_stratification
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ai_model_capability_stratification, []).

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
 *   constraint_id: ai_model_capability_stratification
 *   human_readable: AI Model Capability Stratification and Access Asymmetry
 *   domain: artificial_intelligence/technology_policy
 *
 * SUMMARY:
 *   AI model capability stratification refers to the structured asymmetry in
 *   access to frontier AI capabilities, with exponential compute requirements
 *   creating barriers that concentrate capability and knowledge in a small
 *   number of well-capitalized developers. This constraint exhibits the
 *   hallmark structure of tangled rope: genuine coordination benefits
 *   (frontier models publish findings that accelerate the field) coexist with
 *   asymmetric extraction (frontier developers capture disproportionate
 *   attention, funding, and influence). The constraint operates through
 *   multiple enforcement mechanisms: compute cost barriers (structural), API
 *   rate-limiting and pricing (institutional), withholding of training data
 *   and fine-tuning techniques (informational), and regulatory framing that
 *   uses safety concerns to justify access restrictions (theater). The
 *   extractiveness value (0.58) reflects that the primary extraction
 *   mechanism is not coercive force but rather structural accumulation of
 *   advantage through control of compute resources and knowledge asymmetries.
 *   Suppression is high (0.68) because excluded agents face exponential
 *   barriers to exit — open-source models lag 1-2 generations behind
 *   frontier, cannot match frontier compute, and cannot access the knowledge
 *   embedded in proprietary training data. Theater ratio (0.55) indicates
 *   that roughly half of the observed stratification maintenance is
 *   performative regulation justified by safety concerns that do not clearly
 *   correlate with actual implementation patterns.
 *
 * KEY AGENTS:
 *   - Frontier Model Developers (Anthropic, OpenAI, Google DeepMind, Meta): Primary beneficiaries (institutional/arbitrage) — capture attention, funding concentration, and standard-setting power during capability transition windows
 *   - Excluded Research Groups: Primary victims (powerless/trapped) — researchers without billion-dollar compute access face exponential barriers to frontier-capability research; cannot exit stratification without institutional capital infusion
 *   - Mid-Tier Research Institutions: Secondary victims (moderate/constrained) — have some API access and published weights but face rate limits, cost barriers, and knowledge asymmetries; benefit from published research but constrained by frontier advantage
 *   - Open Source Coalition: Organized victims (organized/constrained) — arXiv, Hugging Face, Mistral, Llama coordinating to reduce stratification through open development; benefit from network effects but face structural capability lag
 *   - Regulatory Bodies and AI Safety Frameworks: Theater maintainers (institutional/arbitrage) — enforce and justify stratification through safety narratives; benefit from appearing to govern while stratification persists
 *   - Analytical Observer: Sees naturalizing tendency (analytical/analytical) — risks treating scaling-law constraints as inevitable rather than contingent on choices to concentrate compute and withhold knowledge
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ai_model_capability_stratification, 0.58).
domain_priors:suppression_score(ai_model_capability_stratification, 0.68).
domain_priors:theater_ratio(ai_model_capability_stratification, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ai_model_capability_stratification, extractiveness, 0.58).
narrative_ontology:constraint_metric(ai_model_capability_stratification, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ai_model_capability_stratification, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ai_model_capability_stratification, tangled_rope).
narrative_ontology:human_readable(ai_model_capability_stratification, "AI Model Capability Stratification and Access Asymmetry").
narrative_ontology:topic_domain(ai_model_capability_stratification, "artificial_intelligence/technology_policy").

domain_priors:requires_active_enforcement(ai_model_capability_stratification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ai_model_capability_stratification, frontier_model_developers).
narrative_ontology:constraint_beneficiary(ai_model_capability_stratification, wealthy_institutions).
narrative_ontology:constraint_beneficiary(ai_model_capability_stratification, capital_intensive_labs).
narrative_ontology:constraint_victim(ai_model_capability_stratification, research_groups_without_capital).
narrative_ontology:constraint_victim(ai_model_capability_stratification, global_south_researchers).
narrative_ontology:constraint_victim(ai_model_capability_stratification, open_source_community).
narrative_ontology:constraint_victim(ai_model_capability_stratification, ai_capability_parity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED RESEARCH GROUP (SNARE) — Small labs and researchers in global south face exponential compute barriers. Cannot access frontier models; open alternatives are 1-2 generations behind; no realistic exit from capability gap. Maximum extraction experienced — trapped by resource asymmetry with no recourse.
constraint_indexing:constraint_classification(ai_model_capability_stratification, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER RESEARCH INSTITUTION (TANGLED ROPE) — Benefits from published model weights, API access tiers, but constrained by cost and rate limits. Genuine coordination function exists (labs sharing knowledge of frontier models through research), but extraction is real (frontier access creates publication advantage, funding concentration). Mixed experience of constraint as both enabling and limiting.
constraint_indexing:constraint_classification(ai_model_capability_stratification, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FRONTIER MODEL DEVELOPER (ROPE) — Experiences the constraint as coordination mechanism: publishing capabilities drives adoption, sets industry standards, creates network effects. Benefits from information asymmetry around training data and methods. Net beneficiary — extraction runs toward this agent; constraint enables their market position.
constraint_indexing:constraint_classification(ai_model_capability_stratification, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN SOURCE COALITION (TANGLED ROPE) — Organized agents (Meta/Llama, Mistral, Hugging Face) coordinating to reduce stratification. Benefits from network effects, collaborative development, and avoiding frontier-model dependency. But faces extraction in form of slower convergence to frontier, cannot access proprietary fine-tuning techniques, rate-limited by compute resource availability. Sees constraint as solvable through collective action but currently experiencing asymmetric extraction.
constraint_indexing:constraint_classification(ai_model_capability_stratification, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY FRAMEWORK (PITON) — AI governance discussions frequently invoke 'capability stratification' as justification for access restrictions, export controls, and safety gates. But these frameworks persist partly through institutional inertia — the stated purpose (preventing capability misuse) does not clearly require the stratification level observed. Theater ratio high: regulatory theater obscures whether restrictions truly correlate with safety or simply entrench incumbent advantage. Regulation maintains performance of capability control while actual function is unclear.
constraint_indexing:constraint_classification(ai_model_capability_stratification, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / SCALING LAW VIEW (MOUNTAIN) — From a civilizational perspective, AI capability stratification appears inherent to scaling laws: frontier models require exponential compute to achieve marginal capability improvements. This suggests the stratification is a natural law of deep learning. However, this naturalizes what is partly contingent institutional practice (choice to concentrate training compute, choice to restrict API access, choice to withhold training data). The engine will compute this as a false summit, revealing naturalization of economic/policy decisions.
constraint_indexing:constraint_classification(ai_model_capability_stratification, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_model_capability_stratification_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_model_capability_stratification, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_model_capability_stratification, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ai_model_capability_stratification, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ai_model_capability_stratification, TR),
    TR >= 0.70.

:- end_tests(ai_model_capability_stratification_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The frontier-developer advantage is significant but not maximal. Open-source models exist and are improving; API access is available to most researchers at some price point; published model weights enable broad participation. But the asymmetry is real: frontier access enables publication advantages, funding concentration, and standard-setting power. The extractiveness has increased from 0.35 to 0.58 over the interval as compute requirements have grown exponentially and frontier-developer compute budgets have concentrated further. Suppression (0.68): High. Barriers include exponential compute costs, API rate-limiting, training data restrictions, and fine-tuning knowledge locked in proprietary systems. Excluded agents cannot realistically exit by building frontier-equivalent models — the cost barrier is not a price they could pay but a structural threshold they cannot cross without institutional backing. Theater ratio (0.55): Moderate. Safety-based justifications for capability restrictions are real concerns (preventing malicious deployment), but implementation patterns do not clearly correlate with stated safety goals. Restrictions concentrate capability in actors least subject to international oversight (frontier labs). Theater has increased from 0.40 to 0.55 as regulatory discourse has expanded relative to actual safety-mechanism implementation.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim perspectives is maximal in this constraint. From the frontier developer's position, stratification is a natural outcome of scaling laws and investment efficiency — they invested billions in compute and knowledge, and the result is legitimate advantage. They perceive the constraint as enabling their market position and see open-source alternatives as healthy competition. From the trapped researcher's position, stratification is a barrier to participation in contemporary AI research — they cannot afford frontier access, cannot build frontier-equivalent models, and are forced to use 2-year-old open models while the field advances. The gap reveals that the labeling ('stratification is natural') depends entirely on position: the frontier developer calls it 'scaling advantage'; the trapped researcher calls it 'access apartheid.' The open-source coalition occupies a middle ground: they see stratification as temporary (scaffold perspective), justifiable as market outcome, but unfair as allocation of research opportunity. The regulatory perspective introduces theater: safety-based restrictions justify stratification while appearing to govern it. The analytical observer risks collapsing the gap by naturalizing it (mountain view), but structural analysis reveals that compute concentration and knowledge withholding are choices, not laws.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality for each perspective flows from structural relationship to the extraction mechanism. Frontier developers are net beneficiaries: their arbitrage exit options (can take their capabilities and models elsewhere, can form new companies) combined with beneficiary status produces low d and negative effective extraction — the constraint enables their position. Trapped researchers have maximal d: no exit options and victim status (cannot access frontier) produces high d and high f(d), maximizing experienced extraction. Organized open-source agents have intermediate d: constrained exit (can improve open models but cannot match frontier) plus victim status (excluded from frontier capabilities) produces moderate d and moderate chi. The regulatory framework is a beneficiary of the constraint (maintains appearance of governance while stratification persists) with arbitrage options (can change policy framework at any time), producing low d. The measurement shows that directionality has not changed qualitatively over the interval — frontier developers remain beneficiaries, excluded researchers remain trapped — but the magnitude has increased as frontier advantage has accumulated.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that all six types are legitimate readings from different structural positions. The frontier developer's rope (genuine coordination and standard-setting benefit) is not wrong — they do coordinate the field. The trapped researcher's snare (exponential barriers, no exit, pure extraction) is not wrong — they do face impossible barriers. The open-source coalition's tangled rope (mixed coordination and extraction) is not wrong — they do both. The regulatory framework's piton (degraded theater) is not wrong — safety discourse persists while actual outcomes remain unclear. The analytical observer's mountain (scaling laws are inevitable) is partially wrong — it naturalizes choices as laws. The mandatrophy resolution shows that 'which type is correct?' has no single answer; the constraint exists in multiple configurations depending on position. The engine should not choose one type but rather report the presheaf: snare from powerless position, tangled rope from moderate and organized positions, rope from institutional-beneficiary position, piton from institutional theater-maintainer position, false-summit mountain from civilizational-analytical position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    compute_cost_threshold_ambiguity,
    'Below what compute cost threshold would stratification disappear, and is that threshold technologically achievable or only economically distant?',
    'Hardware roadmap analysis; compute cost trajectory modeling; identification of whether stratification persists due to physical scaling limits or institutional control choices',
    'If threshold is achievable within 5 years: stratification is temporary (scaffold perspective validated). If threshold requires fundamental physics breakthroughs: stratification approaches mountain status. If threshold exists but is avoided through institutional control: confirms snare classification for trapped agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(compute_cost_threshold_ambiguity, empirical, 'Compute cost threshold for stratification elimination').

omega_variable(
    training_data_replicability,
    'Can frontier capability be replicated using openly available training data plus open-source compute, or does proprietary data create irreplaceable capability advantages?',
    'Empirical reproduction attempts; comparison of frontier models trained on public data vs proprietary data; analysis of whether remaining gaps are knowledge vs compute',
    'If public data suffices: stratification is enforcement mechanism (snare classification confirmed). If proprietary data is irreplaceable: stratification reflects genuine knowledge asymmetry (rope classification validated).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(training_data_replicability, empirical, 'Whether proprietary training data provides irreplaceable advantages').

omega_variable(
    capability_measurement_standardization,
    'Is stratification stratification a real capability gap or partly a measurement artifact created by frontier-biased benchmarks?',
    'Cross-benchmark analysis; development of capability metrics that don''t favor frontier models; assessment of whether stratification persists under alternative measurement schemes',
    'If measurement artifact: stratification is partly theater (piton perspective validated; theater ratio should increase). If genuine gap: stratification is real across multiple measurement frames.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capability_measurement_standardization, empirical, 'Whether stratification reflects real capability gaps or measurement bias').

omega_variable(
    regulatory_capture_mechanism,
    'Do AI safety regulations reducing access actually improve safety, or do they primarily entrench frontier-developer advantages while claiming safety justification?',
    'Correlation analysis between access restrictions and harm reduction; comparison of safety outcomes between restricted-access vs open-access communities; identification of whether safety claims align with actual implementation patterns',
    'If safety correlated: regulations are genuine coordination (rope perspective for regulators). If uncorrelated: regulations are capture mechanism disguised as safety (snare perspective confirmed, piton theater validated).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Whether safety regulations achieve stated safety goals').

omega_variable(
    open_source_asymptotic_convergence,
    'Will open-source models eventually match frontier capability, or is frontier advantage structural and permanent?',
    'Long-term modeling of open-source capability trajectory; identification of whether gap grows, shrinks, or stabilizes; analysis of whether frontier-developer innovations can be rapidly replicated in open models',
    'If convergence occurs: scaffold perspective validated (temporary stratification with sunset). If gap persists: snare perspective validated (permanent extraction mechanism).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_source_asymptotic_convergence, empirical, 'Long-term convergence of open-source and frontier capabilities').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_model_capability_stratification, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(aimc_tr_t0, ai_model_capability_stratification, theater_ratio, 0, 0.4).
narrative_ontology:measurement(aimc_tr_t2, ai_model_capability_stratification, theater_ratio, 2, 0.48).
narrative_ontology:measurement(aimc_tr_t4, ai_model_capability_stratification, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(aimc_be_t0, ai_model_capability_stratification, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(aimc_be_t2, ai_model_capability_stratification, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(aimc_be_t4, ai_model_capability_stratification, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ai_model_capability_stratification, resource_allocation).
narrative_ontology:boltzmann_floor_override(ai_model_capability_stratification, 0.2).
narrative_ontology:affects_constraint(ai_model_capability_stratification, ai_safety_regulation_capture).
narrative_ontology:affects_constraint(ai_model_capability_stratification, knowledge_concentration_in_ai_research).
narrative_ontology:affects_constraint(ai_model_capability_stratification, training_data_access_asymmetry).
narrative_ontology:affects_constraint(ai_model_capability_stratification, compute_infrastructure_dependency).

% DUAL FORMULATION NOTE:
% AI model capability stratification is downstream of compute-infrastructure dependency (physical barrier) and training-data access asymmetry (knowledge barrier) but represents a distinct structural constraint with its own enforcement mechanisms. The network links stratification to capture dynamics in AI safety regulation, where safety justifications are used to reinforce stratification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ai_model_capability_stratification, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
