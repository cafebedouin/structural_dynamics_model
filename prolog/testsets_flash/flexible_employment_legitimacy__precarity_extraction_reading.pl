% ============================================================================
% CONSTRAINT STORY: flexible_employment_legitimacy__precarity_extraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_flexible_employment_legitimacy__precarity_extraction_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: flexible_employment_legitimacy__precarity_extraction_reading
 *   human_readable: Flexible Employment as Precarity Extraction
 *   domain: labor_economics/platform_economy/social_policy
 *
 * SUMMARY:
 *   This constraint models 'flexible employment' as a mechanism for platform
 *   operators to extract surplus value by externalizing labor costs and risks
 *   onto workers and public social safety nets. It is a reading of the
 *   'flexible_employment_legitimacy' kernel, focusing on the structural
 *   precarity inherent in the model. The claimed type is 'snare' because the
 *   coordination story (efficient matching) serves as cover for substantial,
 *   actively enforced extraction from gig workers and social systems.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, 0.85).
domain_priors:suppression_score(flexible_employment_legitimacy__precarity_extraction_reading, 0.75).
domain_priors:theater_ratio(flexible_employment_legitimacy__precarity_extraction_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(flexible_employment_legitimacy__precarity_extraction_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(flexible_employment_legitimacy__precarity_extraction_reading, snare).
narrative_ontology:human_readable(flexible_employment_legitimacy__precarity_extraction_reading, "Flexible Employment as Precarity Extraction").
narrative_ontology:topic_domain(flexible_employment_legitimacy__precarity_extraction_reading, "labor_economics/platform_economy/social_policy").

domain_priors:requires_active_enforcement(flexible_employment_legitimacy__precarity_extraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(flexible_employment_legitimacy__precarity_extraction_reading, '9e7d69f0-6114-4a2b-96f3-d05ad3a75757').
narrative_ontology:cs_kernel_codification('9e7d69f0-6114-4a2b-96f3-d05ad3a75757', distributed).
narrative_ontology:cs_authority_grounding('9e7d69f0-6114-4a2b-96f3-d05ad3a75757', extraction).
narrative_ontology:cs_interpretation_layer_present('9e7d69f0-6114-4a2b-96f3-d05ad3a75757').
narrative_ontology:cs_reading_relation('9e7d69f0-6114-4a2b-96f3-d05ad3a75757', flexible_employment_legitimacy__market_efficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('9e7d69f0-6114-4a2b-96f3-d05ad3a75757', flexible_employment_legitimacy__developmental_state_reading, coexists_with).
narrative_ontology:cs_axiom('9e7d69f0-6114-4a2b-96f3-d05ad3a75757', foundational, labor_is_not_a_commodity).
narrative_ontology:cs_axiom_status(labor_is_not_a_commodity, holdable).
narrative_ontology:cs_axiom_grounding('9e7d69f0-6114-4a2b-96f3-d05ad3a75757', labor_is_not_a_commodity, deontological).
narrative_ontology:cs_axiom('9e7d69f0-6114-4a2b-96f3-d05ad3a75757', foundational, precarity_is_a_design_feature).
narrative_ontology:cs_axiom_status(precarity_is_a_design_feature, holdable).
narrative_ontology:cs_axiom_grounding('9e7d69f0-6114-4a2b-96f3-d05ad3a75757', precarity_is_a_design_feature, empirically_contingent).
narrative_ontology:cs_reference_frame('9e7d69f0-6114-4a2b-96f3-d05ad3a75757', post_industrial_labor_relations).
narrative_ontology:cs_drift_state('9e7d69f0-6114-4a2b-96f3-d05ad3a75757', contemporary_platform_economy, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('9e7d69f0-6114-4a2b-96f3-d05ad3a75757', '').
narrative_ontology:cs_kernel_id(flexible_employment_legitimacy__precarity_extraction_reading, flexible_employment_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:constraint_beneficiary(flexible_employment_legitimacy__precarity_extraction_reading, investors).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, gig_workers).
narrative_ontology:constraint_victim(flexible_employment_legitimacy__precarity_extraction_reading, social_safety_nets).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Design and control the algorithmic systems that mediate flexible work, setting terms, pricing, and worker discipline. They benefit from low labor costs, minimal overheads, and the externalization of employment risks onto workers and the state.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Provide labor through platform apps, experiencing high precarity, unpredictable income, and lack of benefits. They bear the costs of self-employment (insurance, equipment, taxes) without the autonomy or security of traditional employment. Their exit options are limited by economic necessity and the lack of comparable alternatives.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, gig_workers, payer,
    powerless, immediate, constrained, local).

% Absorb the costs of worker precarity, including unemployment benefits, healthcare, and retirement shortfalls, which are not covered by platform operators. This represents a cost-shifting from private enterprise to public funds.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, social_safety_nets, payer,
    institutional, generational, constrained, national).

% Profit from the high margins and rapid growth enabled by the flexible employment model, which minimizes labor costs and regulatory burdens. They exert pressure on platform operators to maintain and expand this model.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, investors, beneficiary,
    powerful, biographical, arbitrage, global).

% Seek to organize gig workers and advocate for better wages, benefits, and working conditions, but face legal and structural barriers to collective bargaining within the flexible employment framework. Their voice is actively suppressed by platform policies and legal interpretations.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, labor_unions, excluded,
    organized, generational, constrained, national).

% Investigate the classification of gig workers (employee vs. independent contractor) and the social impact of flexible employment. They can propose legislation or enforce existing laws to alter the terms of engagement, but often face lobbying pressure from platform operators.
narrative_ontology:constraint_stakeholder(flexible_employment_legitimacy__precarity_extraction_reading, regulators, observer,
    institutional, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(flexible_employment_legitimacy__precarity_extraction_reading, platform_operators).
narrative_ontology:fixing_cost_class(flexible_employment_legitimacy__precarity_extraction_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Matches demand for on-demand services with a flexible supply of labor, enabling rapid scaling and efficient allocation of tasks without the overheads of traditional employment.
% TRANSFER_FUNCTION: Transfers surplus value generated by gig workers' labor to platform operators and investors, by externalizing employment costs and risks, and by maintaining downward pressure on wages through algorithmic management.
% ABSENT_VOICES: Labor unions and worker advocacy groups are largely excluded from the design and governance of flexible employment models; they would argue for reclassification of workers as employees, collective bargaining rights, and comprehensive social protections.
% DISAPPEARANCE_RATIONALE: If the legitimacy of flexible employment as a precarity-extraction mechanism vanished, platform operators would face immense pressure to reclassify workers, provide benefits, and negotiate wages, fundamentally altering their business model and the structure of the gig economy. The current system of cost externalization would collapse.
% FOUNDING_PROBLEM: The need for highly flexible, on-demand labor to meet fluctuating consumer demand, coupled with a desire to minimize fixed labor costs and administrative burdens for businesses.
% FOUNDING_PROBLEM_CORROBORATION: Platform operators and investors attest that the problem of matching flexible demand with flexible supply is still live and that the current model is the most efficient solution. However, labor economists and social policy experts (outside the benefiting parties) corroborate that while demand for flexibility exists, the current model's primary function has shifted to cost externalization and surplus value extraction, rather than merely efficient matching.
narrative_ontology:disappearance_verdict(flexible_employment_legitimacy__precarity_extraction_reading, world_rearranges).
narrative_ontology:founding_problem_status(flexible_employment_legitimacy__precarity_extraction_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(flexible_employment_legitimacy__precarity_extraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(flexible_employment_legitimacy__precarity_extraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(flexible_employment_legitimacy__precarity_extraction_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(flexible_employment_legitimacy__precarity_extraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) due to significant wage gains offset by risk externalization, algorithmic control as labor discipline, and social security gaps. Suppression (0.75) is maintained through legal battles over worker classification, algorithmic management that limits worker autonomy, and the suppression of collective bargaining. Theater ratio is low (0.2) because the 'flexibility' and 'entrepreneurship' narratives are increasingly seen as cover for a highly controlled, extractive labor model, with less genuine coordination function than claimed.
 *
 * PERSPECTIVAL GAP:
 *   Platform operators perceive flexible employment as an efficient, innovative market mechanism (closer to a Rope or even Mountain of market efficiency). Gig workers and social policy analysts, however, experience it as a highly extractive and coercive Snare, where the 'flexibility' is primarily for the benefit of the platforms, not the workers. This divergence is central to the kernel's contestation.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators and investors are clear beneficiaries (d=0.0-0.1) as they capture the extracted surplus value. Gig workers and social safety nets are the primary targets (d=0.9-1.0), bearing the costs of precarity and externalized risks. Labor unions are excluded, their efforts to challenge the model actively suppressed.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    true_cost_of_flexibility,
    'What is the true societal cost of flexible employment when accounting for externalized risks (healthcare, retirement, unemployment) versus the economic benefits of efficiency and innovation?',
    'Comprehensive national-level economic and social impact studies that internalize all externalized costs, comparing them against productivity gains and consumer benefits.',
    'If externalized costs significantly outweigh benefits, it would strengthen the ''snare'' classification and justify regulatory intervention to reallocate costs; if benefits are higher, it would lend credence to the ''market_efficiency_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(true_cost_of_flexibility, empirical, 'Quantifying the full societal ledger of flexible employment.').

omega_variable(
    algorithmic_control_vs_autonomy,
    'To what extent does algorithmic management genuinely offer worker autonomy and flexibility, versus imposing a new, more opaque form of labor discipline and control?',
    'Detailed ethnographic studies of gig worker experiences, combined with analysis of platform algorithms and their impact on scheduling, task allocation, and performance evaluation.',
    'If algorithmic control is found to be highly coercive, it would increase the ''suppression'' metric and reinforce the ''snare'' classification; if genuine autonomy is prevalent, it would shift towards a ''tangled_rope'' or ''rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_control_vs_autonomy, empirical, 'Assessing the nature of algorithmic control in flexible employment.').

omega_variable(
    natural_market_vs_constructed_precarity,
    'Is the precarity inherent in flexible employment a natural outcome of market forces, or is it a constructed feature designed to maximize platform profits?',
    'Comparative analysis of labor markets with different regulatory frameworks for flexible work, examining whether similar levels of flexibility can be achieved without the same degree of precarity.',
    'If precarity is found to be a constructed feature, it would strongly support the ''snare'' classification and challenge the ''market_efficiency_reading''; if it''s a natural market outcome, it would push towards a ''rope'' or ''tangled_rope'' classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_market_vs_constructed_precarity, conceptual, 'Distinguishing natural market outcomes from constructed precarity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(flexible_employment_legitimacy__precarity_extraction_reading, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flex_tr_t2010, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 2010, 0.3).
narrative_ontology:measurement(flex_tr_t2014, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 2014, 0.25).
narrative_ontology:measurement(flex_tr_t2018, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 2018, 0.22).
narrative_ontology:measurement(flex_tr_t2024, flexible_employment_legitimacy__precarity_extraction_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(flex_be_t2010, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(flex_be_t2014, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 2014, 0.7).
narrative_ontology:measurement(flex_be_t2018, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 2018, 0.8).
narrative_ontology:measurement(flex_be_t2024, flexible_employment_legitimacy__precarity_extraction_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(flex_su_t2010, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(flex_su_t2014, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 2014, 0.6).
narrative_ontology:measurement(flex_su_t2018, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement(flex_su_t2024, flexible_employment_legitimacy__precarity_extraction_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(flexible_employment_legitimacy__precarity_extraction_reading, resource_allocation).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, social_security_funding_mechanisms).
narrative_ontology:affects_constraint(flexible_employment_legitimacy__precarity_extraction_reading, labor_law_interpretation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'flexible_employment_legitimacy' kernel, focusing on the precarity and extraction aspects. It is linked to other readings (market_efficiency_reading, developmental_state_reading) which offer alternative interpretations of the same underlying labor phenomenon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
