% ============================================================================
% CONSTRAINT STORY: software_source_status__utilitarian_hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_software_source_status__utilitarian_hybrid_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: software_source_status__utilitarian_hybrid_reading
 *   human_readable: Utilitarian Hybrid Reading of Software Source Status
 *   domain: software_engineering/political_economy_of_technology/intellectual_property
 *
 * SUMMARY:
 *   This constraint represents the 'utilitarian_hybrid_reading' of the
 *   'software_source_status' kernel. It posits that software licensing
 *   decisions (whether open or proprietary) should be guided by the goal of
 *   maximizing aggregate societal welfare, acknowledging that different
 *   contexts may warrant different approaches. This reading aims to provide a
 *   flexible framework for policy and development, avoiding rigid ideological
 *   stances. The metrics reflect a coordination mechanism with inherent, but
 *   not excessive, costs and minimal suppression, as it seeks to optimize
 *   outcomes rather than enforce a single model.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(software_source_status__utilitarian_hybrid_reading, 0.35).
domain_priors:suppression_score(software_source_status__utilitarian_hybrid_reading, 0.25).
domain_priors:theater_ratio(software_source_status__utilitarian_hybrid_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(software_source_status__utilitarian_hybrid_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(software_source_status__utilitarian_hybrid_reading, rope).
narrative_ontology:human_readable(software_source_status__utilitarian_hybrid_reading, "Utilitarian Hybrid Reading of Software Source Status").
narrative_ontology:topic_domain(software_source_status__utilitarian_hybrid_reading, "software_engineering/political_economy_of_technology/intellectual_property").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(software_source_status__utilitarian_hybrid_reading, 'dad1ff78-d919-4e06-9573-38a26734ac97').
narrative_ontology:cs_kernel_codification('dad1ff78-d919-4e06-9573-38a26734ac97', implicit).
narrative_ontology:cs_authority_grounding('dad1ff78-d919-4e06-9573-38a26734ac97', expertise).
narrative_ontology:cs_interpretation_layer_present('dad1ff78-d919-4e06-9573-38a26734ac97').
narrative_ontology:cs_reading_relation('dad1ff78-d919-4e06-9573-38a26734ac97', software_source_status__freedom_imperative_reading, coexists_with).
narrative_ontology:cs_reading_relation('dad1ff78-d919-4e06-9573-38a26734ac97', software_source_status__pragmatic_development_reading, coexists_with).
narrative_ontology:cs_reading_relation('dad1ff78-d919-4e06-9573-38a26734ac97', software_source_status__property_rights_reading, coexists_with).
narrative_ontology:cs_axiom('dad1ff78-d919-4e06-9573-38a26734ac97', foundational, aggregate_welfare_maximization).
narrative_ontology:cs_axiom_status(aggregate_welfare_maximization, holdable).
narrative_ontology:cs_axiom_grounding('dad1ff78-d919-4e06-9573-38a26734ac97', aggregate_welfare_maximization, instrumental).
narrative_ontology:cs_axiom('dad1ff78-d919-4e06-9573-38a26734ac97', foundational, contextual_licensing_flexibility).
narrative_ontology:cs_axiom_status(contextual_licensing_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('dad1ff78-d919-4e06-9573-38a26734ac97', contextual_licensing_flexibility, conventional).
narrative_ontology:cs_reference_frame('dad1ff78-d919-4e06-9573-38a26734ac97', optimal_societal_utility).
narrative_ontology:cs_drift_state('dad1ff78-d919-4e06-9573-38a26734ac97', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('dad1ff78-d919-4e06-9573-38a26734ac97', '').
narrative_ontology:cs_kernel_id(software_source_status__utilitarian_hybrid_reading, software_source_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, society_as_a_whole).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, software_users).
narrative_ontology:constraint_beneficiary(software_source_status__utilitarian_hybrid_reading, flexible_developers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(software_source_status__utilitarian_hybrid_reading, proprietary_software_firms).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate recipient of maximized aggregate welfare from software, benefiting from diverse and context-appropriate licensing models. Its 'power' is conceptual, representing the collective good.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, society_as_a_whole, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(software_source_status__utilitarian_hybrid_reading, society_as_a_whole).

% Directly benefits from software ecosystems optimized for welfare, gaining access to better, more appropriate, or more affordable software. Their choices are constrained by available options.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, software_users, beneficiary,
    moderate, biographical, constrained, global).

% Benefits from a framework that allows them to choose between open and proprietary models based on the specific context and welfare outcomes, rather than ideological mandates. They have mobility between models.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, flexible_developers, beneficiary,
    moderate, biographical, mobile, global).

% Evaluate policies and practices against the principle of aggregate welfare, often finding alignment with open infrastructure but accepting proprietary solutions where welfare is maximized. They provide critical analysis.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, open_source_advocates, observer,
    organized, generational, analytical, global).

% May bear costs by needing to adapt business models or accept open-source alternatives in contexts where proprietary solutions are deemed suboptimal for aggregate welfare. Their market position constrains their exit options.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, proprietary_software_firms, payer,
    institutional, biographical, constrained, global).

% Responsible for implementing policies and regulations that align software licensing with the goal of maximizing aggregate welfare, requiring careful evaluation of diverse contexts. They operate from an analytical distance.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, policy_makers, agenda_setter,
    institutional, generational, analytical, national).

% Provide the data and theoretical frameworks necessary to evaluate the welfare impact of different software licensing models, informing policy decisions. They maintain an analytical distance from direct participation.
narrative_ontology:constraint_stakeholder(software_source_status__utilitarian_hybrid_reading, economic_analysts, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(software_source_status__utilitarian_hybrid_reading, society_as_a_whole).
narrative_ontology:fixing_cost_class(software_source_status__utilitarian_hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a flexible, outcome-oriented framework for software licensing that coordinates diverse interests (developers, users, businesses, public good) towards maximizing overall societal welfare, by allowing both open and proprietary models where appropriate.
% TRANSFER_FUNCTION: Optimizes the distribution of benefits (innovation, access, utility) and costs (development, maintenance, market friction) across society, potentially shifting value from specific proprietary interests to broader public good, or vice versa, depending on the context-specific welfare analysis.
% ABSENT_VOICES: Those holding categorical ideological positions (e.g., absolute software freedom or absolute intellectual property rights) might find their foundational claims dismissed in favor of a pragmatic, outcome-oriented approach. Their voices are not structurally excluded but are often de-prioritized in a utilitarian calculus.
% DISAPPEARANCE_RATIONALE: If this principle vanished, software licensing decisions would likely revert to ideological battles or pure power plays, leading to suboptimal welfare outcomes, less flexible software ecosystems, and potentially greater societal friction over intellectual property.
% FOUNDING_PROBLEM: The historical tension between rigid ideological stances on software (e.g., 'all software must be free' vs. 'all software is private property') and the practical need for diverse software models to serve different societal needs and maximize overall utility.
% FOUNDING_PROBLEM_CORROBORATION: Ongoing debates in intellectual property law, economic studies on the impact of open vs. proprietary software in various sectors, and the continued existence of mixed software ecosystems, all corroborated by independent researchers, policy think tanks, and legislative hearings.
narrative_ontology:disappearance_verdict(software_source_status__utilitarian_hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(software_source_status__utilitarian_hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(software_source_status__utilitarian_hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(software_source_status__utilitarian_hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(software_source_status__utilitarian_hybrid_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(software_source_status__utilitarian_hybrid_reading_tests).
:- end_tests(software_source_status__utilitarian_hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate because optimizing for aggregate welfare often involves trade-offs where some parties might bear costs for the greater good, but it's not designed for pure extraction. Suppression is low as this reading promotes flexibility and evaluation rather than coercive enforcement of a single model. Theater ratio is low because the principle is about genuine, outcome-oriented analysis. The claimed type is 'rope' because it functions as a coordination mechanism to achieve a collective benefit (aggregate welfare) by guiding decisions in a complex domain.
 *
 * PERSPECTIVAL GAP:
 *   While this reading aims for universal benefit, its application can still create perspectival gaps. For instance, a proprietary firm might perceive a policy shift (driven by welfare maximization) as extractive, even if the aggregate societal benefit is positive. The engine's per-seat classification would capture this divergence based on their structural position and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   Society as a whole, software users, and flexible developers are beneficiaries, as the principle aims to optimize outcomes for them. Proprietary software firms may act as payers when their business models need to adapt to welfare-maximizing policies. Policy makers and economic analysts serve as agenda-setters and observers, respectively, guiding and evaluating the application of this principle.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    aggregate_welfare_measurability,
    'Is ''aggregate welfare'' a truly measurable and universally agreed-upon concept in the context of software licensing, or is its definition and measurement inherently contested?',
    'Consensus among leading economic and social policy researchers on a standardized methodology for measuring software-related aggregate welfare, or a clear articulation of irreducible value conflicts.',
    'If ''aggregate welfare'' is ill-defined or highly contested, the constraint''s coordination function becomes ambiguous, potentially masking preference-based decisions as objective optimization. This could shift its classification towards a Tangled Rope or Snare if specific interests consistently define ''welfare'' to their benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(aggregate_welfare_measurability, conceptual, 'Ambiguity in defining and measuring aggregate welfare.').

omega_variable(
    creator_incentives_vs_public_access,
    'How does this reading consistently balance the welfare of software creators (e.g., through intellectual property incentives) against the welfare of users and society (e.g., through access, modification, and innovation)?',
    'Empirical studies demonstrating consistent, transparent trade-off mechanisms in policy implementation that are perceived as fair by a broad range of stakeholders, or evidence of systematic bias towards one group.',
    'If the balance consistently favors creators at the expense of public access, or vice versa, the constraint''s ''aggregate welfare'' claim could be undermined, revealing a hidden extractive mechanism for one group. This would increase extractiveness and potentially shift the classification towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_incentives_vs_public_access, preference, 'Balancing creator incentives with public access for welfare optimization.').

omega_variable(
    contextual_optimization_stability,
    'Does the ''context-dependent optimization'' approach lead to a stable, predictable framework for software development and investment, or does it create regulatory uncertainty and increased transaction costs?',
    'Longitudinal studies of software ecosystems operating under this principle, assessing investment patterns, innovation rates, and developer satisfaction, compared to more rigid licensing regimes.',
    'If the framework proves unstable or unpredictable, the perceived benefits of ''flexibility'' could be outweighed by the costs of uncertainty, reducing overall welfare and potentially increasing resistance from developers and firms. This would challenge the effectiveness of the ''rope'' classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(contextual_optimization_stability, empirical, 'Stability and predictability of context-dependent licensing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(software_source_status__utilitarian_hybrid_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(soft_tr_t0, software_source_status__utilitarian_hybrid_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(soft_tr_t10, software_source_status__utilitarian_hybrid_reading, theater_ratio, 10, 0.11).
narrative_ontology:measurement(soft_tr_t20, software_source_status__utilitarian_hybrid_reading, theater_ratio, 20, 0.09).
narrative_ontology:measurement(soft_tr_t30, software_source_status__utilitarian_hybrid_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(soft_be_t0, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(soft_be_t10, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(soft_be_t20, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 20, 0.36).
narrative_ontology:measurement(soft_be_t30, software_source_status__utilitarian_hybrid_reading, base_extractiveness, 30, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(soft_su_t0, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(soft_su_t10, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 10, 0.24).
narrative_ontology:measurement(soft_su_t20, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 20, 0.26).
narrative_ontology:measurement(soft_su_t30, software_source_status__utilitarian_hybrid_reading, suppression_requirement, 30, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(software_source_status__utilitarian_hybrid_reading, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'software_source_status' kernel, focusing on utilitarian welfare maximization. It coexists with and influences other readings by offering an alternative framework for evaluating software licensing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
