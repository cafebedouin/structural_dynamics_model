% ============================================================================
% CONSTRAINT STORY: hoa_covenant_scope__coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hoa_covenant_scope__coordination_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: hoa_covenant_scope__coordination_reading
 *   human_readable: HOA Covenant for Shared Infrastructure and Externalities (Coordination Reading)
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint represents the 'coordination reading' of an HOA covenant,
 *   where its primary function is to manage shared infrastructure and resolve
 *   genuine externalities. In this reading, the covenant operates as a Rope,
 *   providing symmetrical benefits to all homeowners by solving collective
 *   action problems with minimal extraction. Enforcement is limited to
 *   ensuring fair contribution to shared costs and addressing objective
 *   nuisances, not aesthetic or behavioral control. This reading contrasts
 *   with 'behavioral_control_reading' and 'extraction_reading' of the same
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__coordination_reading, 0.15).
domain_priors:suppression_score(hoa_covenant_scope__coordination_reading, 0.2).
domain_priors:theater_ratio(hoa_covenant_scope__coordination_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__coordination_reading, rope).
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "HOA Covenant for Shared Infrastructure and Externalities (Coordination Reading)").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, '6b0afb8d-dd2b-45bc-93d1-a0cf120ad0cc').
narrative_ontology:cs_kernel_codification('6b0afb8d-dd2b-45bc-93d1-a0cf120ad0cc', formalized).
narrative_ontology:cs_authority_grounding('6b0afb8d-dd2b-45bc-93d1-a0cf120ad0cc', practice).
narrative_ontology:cs_interpretation_layer_present('6b0afb8d-dd2b-45bc-93d1-a0cf120ad0cc').
narrative_ontology:cs_reading_relation('6b0afb8d-dd2b-45bc-93d1-a0cf120ad0cc', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('6b0afb8d-dd2b-45bc-93d1-a0cf120ad0cc', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('6b0afb8d-dd2b-45bc-93d1-a0cf120ad0cc', foundational, collective_benefit_proportional_cost).
narrative_ontology:cs_axiom_status(collective_benefit_proportional_cost, holdable).
narrative_ontology:cs_axiom_grounding('6b0afb8d-dd2b-45bc-93d1-a0cf120ad0cc', collective_benefit_proportional_cost, conventional).
narrative_ontology:cs_axiom('6b0afb8d-dd2b-45bc-93d1-a0cf120ad0cc', foundational, objective_nuisance_resolution).
narrative_ontology:cs_axiom_status(objective_nuisance_resolution, holdable).
narrative_ontology:cs_axiom_grounding('6b0afb8d-dd2b-45bc-93d1-a0cf120ad0cc', objective_nuisance_resolution, empirically_contingent).
narrative_ontology:cs_reference_frame('6b0afb8d-dd2b-45bc-93d1-a0cf120ad0cc', shared_governance_for_common_good).
narrative_ontology:cs_drift_state('6b0afb8d-dd2b-45bc-93d1-a0cf120ad0cc', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6b0afb8d-dd2b-45bc-93d1-a0cf120ad0cc', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_homeowners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, free_riders).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__coordination_reading, collective_action_problem_resolution).
narrative_ontology:constraint_vindicates(hoa_covenant_scope__coordination_reading, shared_resource_management).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from well-maintained common areas (e.g., roads, parks, pools) and predictable resolution of nuisances (e.g., noise, unkempt properties) that would otherwise degrade property values and quality of life. They pay dues and adhere to rules, but the benefits are symmetrical and directly tied to shared costs.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, all_homeowners, beneficiary,
    organized, generational, constrained, local).

% Administers the covenant, collects dues, and oversees maintenance of shared infrastructure. Enforces rules related to objective nuisances and property upkeep that directly impact shared value. Their role is primarily managerial and fiduciary, with limited discretionary power beyond the covenant's clear intent.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, hoa_board, agenda_setter,
    institutional, biographical, constrained, local).

% Homeowners who might otherwise avoid contributing to shared costs or maintaining their property to a reasonable standard, thereby imposing negative externalities on others. The covenant ensures their participation, making them 'payers' of their fair share.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, free_riders, payer,
    powerless, immediate, constrained, local).

% Monitors HOA operations to ensure compliance with local laws and regulations, particularly regarding property rights and community governance. Generally defers to HOA self-governance as long as it serves its stated coordination function.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, local_government, observer,
    institutional, generational, analytical, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a stable mechanism for funding and managing shared infrastructure (e.g., private roads, common landscaping, amenities) and to establish clear, objective rules for resolving genuine negative externalities between properties (e.g., excessive noise, hazardous conditions).
% TRANSFER_FUNCTION: Collects regular dues from all homeowners to a central fund, which is then disbursed for the maintenance and improvement of shared community assets. It also transfers the cost of nuisance mitigation from affected homeowners to the homeowner causing the nuisance.
% ABSENT_VOICES: Homeowners who might prefer no collective governance at all, or who object to any form of shared responsibility beyond public services. Their voices are typically absent because they either chose not to live in an HOA community or are outvoted by the majority who value the coordination benefits.
% DISAPPEARANCE_RATIONALE: If the covenant vanished overnight, shared infrastructure would quickly degrade due to lack of funding and coordinated maintenance. Property values would likely decline as common areas fall into disrepair and unresolved nuisances proliferate, leading to a breakdown of community order and significant collective action problems.
% FOUNDING_PROBLEM: To prevent the 'tragedy of the commons' in shared residential developments, ensuring that common property is maintained and that individual property uses do not impose unmitigated negative externalities on neighbors, thereby preserving collective property values and quality of life.
% FOUNDING_PROBLEM_CORROBORATION: Local urban planners and real estate economists consistently corroborate the ongoing need for such mechanisms in planned communities to prevent property degradation and ensure orderly development. Homeowners themselves, through surveys and participation, generally affirm the value of these coordination functions.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__coordination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(hoa_covenant_scope__coordination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hoa_covenant_scope__coordination_reading_tests).
:- end_tests(hoa_covenant_scope__coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because dues are directly tied to the cost of shared services, and enforcement actions are limited to objective, value-preserving functions. Suppression is also low (0.20) as homeowners generally consent to these rules for the collective benefit, and exit options (selling property) are available, albeit constrained. The theater ratio is minimal (0.05) as the covenant's stated purpose aligns closely with its actual operation. The metrics reflect a genuine coordination mechanism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'all_homeowners', the covenant is a beneficial coordination mechanism. From the perspective of 'free_riders', it is a necessary imposition to ensure fairness. The 'hoa_board' acts as a neutral administrator. All seats experience this reading as a net benefit or a fair cost for collective goods.
 *
 * DIRECTIONALITY LOGIC:
 *   'All_homeowners' are the primary beneficiaries, receiving collective goods and stable property values. 'Free_riders' are payers, compelled to contribute their fair share. The 'hoa_board' is an agenda-setter, administering the system for the collective good. Directionality is largely symmetric or slightly extractive for free-riders, but overall, the system is designed for mutual benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   In this reading, the covenant's mandate (solving collective action problems for shared infrastructure) is live and actively fulfilled. There is no significant drift towards extraction or theatricality, preventing mislabeling as a Snare or Piton. The coordination function remains central and effective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_creep_potential,
    'Could the HOA board''s interpretation of ''genuine externalities'' expand over time to include subjective aesthetic or behavioral controls, shifting this constraint towards a ''behavioral_control_reading''?',
    'Monitoring of board meeting minutes, enforcement actions, and homeowner appeals over a 5-10 year period for evidence of rule expansion beyond objective infrastructure and nuisance issues.',
    'If scope creep is detected, the constraint''s extractiveness and suppression would likely increase, and its classification would shift towards a ''tangled_rope'' or ''snare'' as it moves closer to the ''behavioral_control_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_creep_potential, empirical, 'Risk of the covenant''s scope expanding beyond its original coordination function.').

omega_variable(
    revenue_generation_drift,
    'Is there a risk that the HOA board could begin to use fines and fees primarily as a revenue generation mechanism rather than for genuine enforcement, shifting towards an ''extraction_reading''?',
    'Auditing HOA financial records to compare fine revenue against actual enforcement costs and overall budget needs. Tracking the ratio of fine revenue to total operating budget over time.',
    'If fine revenue becomes a significant, disproportionate part of the budget, the constraint''s extractiveness would rise, and its classification would shift towards a ''snare'', aligning with the ''extraction_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revenue_generation_drift, empirical, 'Potential for fines to become a primary revenue source, rather than a means of enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__coordination_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__coordination_reading, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__coordination_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__coordination_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__coordination_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__coordination_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__coordination_reading, suppression_requirement, 5, 0.18).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__coordination_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__coordination_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__coordination_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__behavioral_control_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% This is one reading of the 'hoa_covenant_scope' kernel. This 'coordination_reading' focuses on shared infrastructure and externalities, contrasting with 'behavioral_control_reading' and 'extraction_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
