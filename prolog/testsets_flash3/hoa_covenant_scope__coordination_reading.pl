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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hoa_covenant_scope__coordination_reading
 *   human_readable: HOA Covenant for Shared Infrastructure Maintenance (Coordination Reading)
 *   domain: property_law/collective_governance/urban_planning
 *
 * SUMMARY:
 *   This constraint story represents the 'coordination reading' of an HOA
 *   covenant, where its primary function is to coordinate shared
 *   infrastructure maintenance and resolve genuine externalities. It is
 *   characterized by low extractiveness, minimal suppression, and a focus on
 *   collective benefit. This reading contrasts with 'behavioral control' and
 *   'extraction' readings, which emphasize aesthetic conformity or revenue
 *   generation, respectively. The metrics reflect a constraint that genuinely
 *   solves a collective action problem with minimal overhead.
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
narrative_ontology:human_readable(hoa_covenant_scope__coordination_reading, "HOA Covenant for Shared Infrastructure Maintenance (Coordination Reading)").
narrative_ontology:topic_domain(hoa_covenant_scope__coordination_reading, "property_law/collective_governance/urban_planning").

domain_priors:requires_active_enforcement(hoa_covenant_scope__coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hoa_covenant_scope__coordination_reading, '9f7a8369-31a6-4c42-9642-19235dec14ac').
narrative_ontology:cs_kernel_codification('9f7a8369-31a6-4c42-9642-19235dec14ac', formalized).
narrative_ontology:cs_authority_grounding('9f7a8369-31a6-4c42-9642-19235dec14ac', practice).
narrative_ontology:cs_interpretation_layer_present('9f7a8369-31a6-4c42-9642-19235dec14ac').
narrative_ontology:cs_reading_relation('9f7a8369-31a6-4c42-9642-19235dec14ac', hoa_covenant_scope__behavioral_control_reading, coexists_with).
narrative_ontology:cs_reading_relation('9f7a8369-31a6-4c42-9642-19235dec14ac', hoa_covenant_scope__extraction_reading, coexists_with).
narrative_ontology:cs_axiom('9f7a8369-31a6-4c42-9642-19235dec14ac', foundational, collective_benefit_justifies_assessment).
narrative_ontology:cs_axiom_status(collective_benefit_justifies_assessment, holdable).
narrative_ontology:cs_axiom_grounding('9f7a8369-31a6-4c42-9642-19235dec14ac', collective_benefit_justifies_assessment, instrumental).
narrative_ontology:cs_axiom('9f7a8369-31a6-4c42-9642-19235dec14ac', foundational, externalities_require_collective_resolution).
narrative_ontology:cs_axiom_status(externalities_require_collective_resolution, holdable).
narrative_ontology:cs_axiom_grounding('9f7a8369-31a6-4c42-9642-19235dec14ac', externalities_require_collective_resolution, empirically_contingent).
narrative_ontology:cs_reference_frame('9f7a8369-31a6-4c42-9642-19235dec14ac', efficient_collective_governance).
narrative_ontology:cs_drift_state('9f7a8369-31a6-4c42-9642-19235dec14ac', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9f7a8369-31a6-4c42-9642-19235dec14ac', '').
narrative_ontology:cs_kernel_id(hoa_covenant_scope__coordination_reading, hoa_covenant_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hoa_covenant_scope__coordination_reading, all_homeowners).
narrative_ontology:constraint_victim(hoa_covenant_scope__coordination_reading, free_riders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from well-maintained common areas, shared utilities, and predictable resolution of genuine nuisances. They pay regular assessments for these services. Exit is tied to selling their property.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, all_homeowners, beneficiary,
    organized, generational, constrained, local).

% Administers the covenant, collects assessments, and oversees maintenance. Their power is derived from the homeowners' collective agreement to delegate these functions. They are accountable to the homeowners.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, hoa_board, agenda_setter,
    institutional, biographical, constrained, local).

% Homeowners who attempt to avoid paying their share of assessments or ignore rules designed to prevent genuine externalities (e.g., excessive noise, unkempt property affecting shared aesthetics). They are subject to enforcement actions.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, free_riders, payer,
    powerless, immediate, constrained, local).

% Oversees the legal framework within which HOAs operate, but generally defers to HOA governance for internal matters unless there are violations of broader public law. Could intervene if HOA actions become excessively arbitrary or harmful.
narrative_ontology:constraint_stakeholder(hoa_covenant_scope__coordination_reading, local_government, observer,
    institutional, generational, analytical, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the maintenance and funding of shared infrastructure (e.g., roads, parks, utilities) and provides a mechanism for resolving genuine negative externalities that affect collective property values and quality of life.
% TRANSFER_FUNCTION: Collects regular assessments from all homeowners to fund shared infrastructure maintenance and administrative costs. Transfers fines from non-compliant homeowners to the HOA's general fund.
% ABSENT_VOICES: Homeowners who might prefer to opt out of shared amenities or manage their own property entirely independently, but are bound by the covenant upon purchase. Their voice is implicitly excluded by the collective nature of the agreement.
% DISAPPEARANCE_RATIONALE: If the covenant vanished, shared infrastructure would likely fall into disrepair due to collective action problems, property values would decline, and disputes over nuisances would escalate without a clear resolution mechanism. The community would need to find alternative coordination mechanisms.
% FOUNDING_PROBLEM: To prevent the 'tragedy of the commons' in shared residential areas, ensuring collective assets are maintained and individual actions do not unduly harm neighbors' property values or quality of life.
% FOUNDING_PROBLEM_CORROBORATION: Urban planners and property law experts corroborate that collective action problems in shared residential developments are a persistent issue that covenants are designed to address. Homeowners themselves generally attest to the value of these functions.
narrative_ontology:disappearance_verdict(hoa_covenant_scope__coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(hoa_covenant_scope__coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hoa_covenant_scope__coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
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
 *   Extractiveness is low (0.15) because assessments are primarily tied to the cost of maintaining shared assets, with little surplus. Suppression (0.20) is also low, as enforcement is limited to ensuring participation in collective maintenance and preventing clear nuisances, not controlling personal behavior. Theater ratio (0.05) is negligible, indicating that the HOA's activities are genuinely functional. Accessibility collapse is moderate (0.70) because while homeowners are bound by the covenant, the benefits of coordination are clear and alternatives (e.g., individual maintenance of shared roads) are genuinely difficult.
 *
 * PERSPECTIVAL GAP:
 *   From this 'coordination reading,' the HOA covenant is a net benefit for all homeowners, solving a genuine collective action problem. Other readings (e.g., 'behavioral control' or 'extraction') would highlight different beneficiaries and victims, leading to different classifications. This story focuses on the structural properties consistent with a genuine coordination mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   All homeowners are beneficiaries, as they receive the benefits of coordinated maintenance and dispute resolution. The HOA board acts as an agenda-setter, administering the covenant on behalf of the homeowners. 'Free riders' are identified as payers, as they bear the cost of enforcement when they attempt to opt out of their collective obligations. The system is designed for symmetric benefit, with costs primarily covering shared services.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_creep_risk,
    'Does the covenant''s scope remain limited to genuine infrastructure and externality resolution, or is there a risk of ''scope creep'' into behavioral control or aesthetic enforcement?',
    'Longitudinal analysis of HOA board meeting minutes, enforcement actions, and homeowner complaints over time, specifically tracking the nature of rules introduced and enforced.',
    'If scope creep is detected, the constraint would shift towards a ''tangled_rope'' or ''snare'' as it begins to extract non-maintenance-related conformity or revenue, aligning with the ''behavioral_control_reading'' or ''extraction_reading''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_creep_risk, empirical, 'Uncertainty regarding the stability of the covenant''s functional scope over time.').

omega_variable(
    assessment_cost_justification,
    'Are HOA assessments genuinely proportional to the cost of shared infrastructure maintenance and administration, or do they include hidden surcharges or excessive administrative overhead?',
    'Independent audit of HOA finances, comparing assessment revenue to actual maintenance and administrative expenditures, benchmarked against similar communities.',
    'If assessments are found to be disproportionate, the extractiveness metric would increase, pushing the constraint towards a ''tangled_rope'' or ''snare'' classification, aligning with the ''extraction_reading''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(assessment_cost_justification, empirical, 'Uncertainty regarding the true cost-justification of HOA assessments.').

omega_variable(
    reading_framing_ambiguity,
    'Is this HOA covenant primarily understood and experienced by homeowners as a coordination mechanism, or is it more commonly perceived as a tool for behavioral control or extraction?',
    'Surveys of homeowners, analysis of community forums, and legal challenges to HOA rules, to gauge the prevailing perception and lived experience of the covenant.',
    'If the dominant perception aligns with a ''behavioral_control_reading'' or ''extraction_reading'', the effective classification for many homeowners would be more extractive, even if the formal structure suggests coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_framing_ambiguity, conceptual, 'Ambiguity in how the covenant is framed and experienced by its participants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hoa_covenant_scope__coordination_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hoa__tr_t0, hoa_covenant_scope__coordination_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(hoa__tr_t5, hoa_covenant_scope__coordination_reading, theater_ratio, 5, 0.04).
narrative_ontology:measurement(hoa__tr_t10, hoa_covenant_scope__coordination_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement(hoa__tr_t15, hoa_covenant_scope__coordination_reading, theater_ratio, 15, 0.05).
narrative_ontology:measurement(hoa__tr_t20, hoa_covenant_scope__coordination_reading, theater_ratio, 20, 0.05).

% Extraction over time
narrative_ontology:measurement(hoa__be_t0, hoa_covenant_scope__coordination_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(hoa__be_t5, hoa_covenant_scope__coordination_reading, base_extractiveness, 5, 0.12).
narrative_ontology:measurement(hoa__be_t10, hoa_covenant_scope__coordination_reading, base_extractiveness, 10, 0.14).
narrative_ontology:measurement(hoa__be_t15, hoa_covenant_scope__coordination_reading, base_extractiveness, 15, 0.15).
narrative_ontology:measurement(hoa__be_t20, hoa_covenant_scope__coordination_reading, base_extractiveness, 20, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(hoa__su_t0, hoa_covenant_scope__coordination_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(hoa__su_t5, hoa_covenant_scope__coordination_reading, suppression_requirement, 5, 0.17).
narrative_ontology:measurement(hoa__su_t10, hoa_covenant_scope__coordination_reading, suppression_requirement, 10, 0.18).
narrative_ontology:measurement(hoa__su_t15, hoa_covenant_scope__coordination_reading, suppression_requirement, 15, 0.19).
narrative_ontology:measurement(hoa__su_t20, hoa_covenant_scope__coordination_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hoa_covenant_scope__coordination_reading, resource_allocation).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__behavioral_control_reading).
narrative_ontology:affects_constraint(hoa_covenant_scope__coordination_reading, hoa_covenant_scope__extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'hoa_covenant_scope' kernel. This 'coordination_reading' focuses on genuine collective action problems, while sibling readings emphasize behavioral control or extraction. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
