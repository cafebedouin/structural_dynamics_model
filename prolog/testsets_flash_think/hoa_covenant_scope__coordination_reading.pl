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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: hoa_covenant_scope__coordination_reading
 *   human_readable: HOA Covenant for Shared Infrastructure Coordination
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint story instantiates the 'coordination_reading' of the
 *   'hoa_covenant_scope' kernel. In this reading, the HOA covenant primarily
 *   functions as a genuine collective-action mechanism to ensure the
 *   maintenance of shared infrastructure and manage objective externalities.
 *   It is characterized by low extraction, minimal suppression, and a focus
 *   on mutual benefit, consistent with a Rope classification. The enforcement
 *   is directed at ensuring fair contribution to shared costs, not at
 *   controlling individual behavior beyond what is necessary for collective
 *   well-being.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hoa_covenant_scope__coordination_reading, 0.15).
domain_priors:suppression_score(hoa_covenant_scope__coordination_reading, 0.2).
domain_priors:theater_ratio(hoa_covenant_scope__coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(hoa_covenant_scope__coordination_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hoa_covenant_scope__coordination_reading, rope).
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "HOA Covenant for Shared Infrastructure Coordination").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, 'b3f24d77-665d-4a74-9e4c-e3099443b0b6').
narrative_ontology:cs_kernel_codification('b3f24d77-665d-4a74-9e4c-e3099443b0b6', formalized).
narrative_ontology:cs_authority_grounding('b3f24d77-665d-4a74-9e4c-e3099443b0b6', practice).
narrative_ontology:cs_interpretation_layer_present('b3f24d77-665d-4a74-9e4c-e3099443b0b6').
narrative_ontology:cs_reading_relation('b3f24d77-665d-4a74-9e4c-e3099443b0b6', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('b3f24d77-665d-4a74-9e4c-e3099443b0b6', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('b3f24d77-665d-4a74-9e4c-e3099443b0b6', foundational, collective_benefit_justifies_cost).
narrative_ontology:cs_axiom_status(collective_benefit_justifies_cost, holdable).
narrative_ontology:cs_axiom_grounding('b3f24d77-665d-4a74-9e4c-e3099443b0b6', collective_benefit_justifies_cost, conventional).
narrative_ontology:cs_axiom('b3f24d77-665d-4a74-9e4c-e3099443b0b6', foundational, limited_scope_for_common_good).
narrative_ontology:cs_axiom_status(limited_scope_for_common_good, holdable).
narrative_ontology:cs_axiom_grounding('b3f24d77-665d-4a74-9e4c-e3099443b0b6', limited_scope_for_common_good, conventional).
narrative_ontology:cs_reference_frame('b3f24d77-665d-4a74-9e4c-e3099443b0b6', mutual_benefit_collective_action).
narrative_ontology:cs_drift_state('b3f24d77-665d-4a74-9e4c-e3099443b0b6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('b3f24d77-665d-4a74-9e4c-e3099443b0b6', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, free_riders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, all_homeowners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from well-maintained shared infrastructure (roads, parks, utilities) and a stable community environment. They collectively pay dues to fund these services and participate in governance, but their exit is tied to selling their property.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, all_homeowners, beneficiary,
    moderate, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(hoa_covenant_scope__coordination_reading, all_homeowners, payer).

% Elected representatives who administer the covenant, collect dues, manage maintenance contracts, and enforce rules related to shared infrastructure and objective nuisances. Their role is to ensure the collective good of the community.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, hoa_board, agenda_setter,
    institutional, biographical, mobile, local).

% Homeowners who benefit from the shared amenities and services but attempt to avoid paying their fair share of dues or complying with basic maintenance rules. They are victims of the enforcement mechanism only insofar as it compels them to contribute to the collective good.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, free_riders, payer,
    powerless, immediate, trapped, local).

% Oversees HOAs to ensure compliance with local laws and regulations, mediates disputes, and may intervene if the HOA fails to maintain essential infrastructure that impacts public safety or welfare.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, local_government, observer,
    institutional, generational, analytical, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hoa_covenant_scope__coordination_reading, diffuse).
narrative_ontology:fixing_cost_class(hoa_covenant_scope__coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the maintenance and upkeep of shared infrastructure (e.g., private roads, common green spaces, shared utilities) and to resolve genuine externalities (e.g., noise, waste management) within a residential community.
% TRANSFER_FUNCTION: Collects mandatory dues from all homeowners to fund the maintenance, repair, and improvement of shared property and services, ensuring a collective pool of resources for common needs.
% ABSENT_VOICES: Homeowners who might prefer to opt out of shared amenities or collective rules entirely, or those who believe they could manage their property more cheaply without collective oversight. They are not part of the conversation because the covenant is a condition of property ownership in the community.
% DISAPPEARANCE_RATIONALE: If the covenant and its enforcement vanished, shared infrastructure would likely degrade due to a lack of coordinated funding and responsibility. Property values would decline, and disputes over unmanaged externalities would proliferate, leading to a less desirable living environment.
% FOUNDING_PROBLEM: Preventing the 'tragedy of the commons' in planned residential developments, ensuring the long-term upkeep of common areas, and maintaining a baseline quality of life and property values through collective action.
% FOUNDING_PROBLEM_CORROBORATION: Urban planners, real estate developers, and historical records of communities without such mechanisms corroborate the ongoing need for coordinated governance to manage shared assets and prevent decline.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(hoa_covenant_scope__coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hoa_covenant_scope__coordination_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low (0.15) because the dues are primarily directed towards the actual costs of shared maintenance and administration, with minimal surplus. Suppression is low (0.20) as enforcement is limited to collecting dues and addressing clear nuisances, with alternatives (e.g., selling property) being available, albeit constrained. The theater ratio is low (0.10) because the covenant's activities are genuinely functional, focused on tangible maintenance and problem-solving. The metrics reflect a constraint that largely fulfills its stated coordination purpose.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of 'all_homeowners' and the 'hoa_board', this covenant is a beneficial and necessary coordination mechanism. From the perspective of 'free_riders', it might be seen as an extractive imposition, but this reading emphasizes the collective benefit that justifies the cost. The engine's per-seat classification would likely confirm a Rope-like experience for most, with a slightly more 'constrained' experience for those compelled to pay.
 *
 * DIRECTIONALITY LOGIC:
 *   All homeowners are beneficiaries of the coordinated maintenance, experiencing symmetric costs and benefits. The HOA board acts as the agenda-setter, facilitating this coordination. 'Free-riders' are identified as victims only in the sense that the constraint compels them to contribute to a system from which they benefit, preventing them from externalizing costs onto others. Local government acts as an observer, ensuring legal compliance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_behavioral_control_ambiguity,
    'Is the covenant''s enforcement genuinely limited to shared infrastructure and objective nuisances, or does it subtly extend to aesthetic uniformity and behavioral conformity?',
    'Analysis of HOA board meeting minutes, enforcement records, and homeowner complaints over a multi-year period, specifically tracking the types of violations cited and the rationale provided.',
    'If enforcement frequently targets subjective aesthetic or minor behavioral issues, the constraint''s effective suppression and theater_ratio would be higher, shifting it towards a ''tangled_rope'' or ''snare'' classification, aligning more with the ''behavioral_control_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_behavioral_control_ambiguity, empirical, 'Distinguishing genuine coordination from subtle behavioral control.').

omega_variable(
    cost_justification_ambiguity,
    'Are the HOA dues and any associated fines strictly proportional to the actual costs of shared maintenance and administration, or do they generate significant surplus revenue?',
    'Independent audit of HOA financial records, comparing revenue from dues/fines against actual expenditures for maintenance, repairs, and administrative overhead.',
    'If significant surplus revenue is consistently generated and not reinvested into shared assets, the constraint''s extractiveness would be higher, potentially shifting it towards an ''extraction_reading'' and a ''tangled_rope'' or ''snare'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_justification_ambiguity, empirical, 'Verifying the proportionality of HOA costs to services rendered.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__coordination_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__coordination_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__coordination_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__coordination_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__coordination_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__coordination_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__coordination_reading, base_extractiveness, 5, 0.13).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__coordination_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__coordination_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__coordination_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__coordination_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__coordination_reading, suppression_requirement, 5, 0.19).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__coordination_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__coordination_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__coordination_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__coordination_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
