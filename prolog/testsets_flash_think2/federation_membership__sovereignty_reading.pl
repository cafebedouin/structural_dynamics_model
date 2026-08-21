% ============================================================================
% CONSTRAINT STORY: federation_membership__sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership__sovereignty_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Federation Membership: National Sovereignty Reading
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint describes the 'sovereignty_reading' of federation
 *   membership, where national authority retains primary legitimacy over
 *   borders and free movement is treated as a negotiable policy, rather than
 *   an inherent right. It operates as a tangled rope, coordinating national
 *   interests while extracting from mobile citizens through enforced
 *   restrictions. The 'integration_reading' is a sibling constraint that
 *   would assert supranational authority and free movement as a
 *   constitutional right.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.68).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.75).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership: National Sovereignty Reading").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, '01c3dee1-6cbf-4503-a6d5-43e592ec3db9').
narrative_ontology:cs_kernel_codification('01c3dee1-6cbf-4503-a6d5-43e592ec3db9', formalized).
narrative_ontology:cs_authority_grounding('01c3dee1-6cbf-4503-a6d5-43e592ec3db9', lineage).
narrative_ontology:cs_interpretation_layer_present('01c3dee1-6cbf-4503-a6d5-43e592ec3db9').
narrative_ontology:cs_reading_relation('01c3dee1-6cbf-4503-a6d5-43e592ec3db9', federation_membership__integration_reading, coexists_with).
narrative_ontology:cs_axiom('01c3dee1-6cbf-4503-a6d5-43e592ec3db9', foundational, national_sovereignty_is_primary).
narrative_ontology:cs_axiom_status(national_sovereignty_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('01c3dee1-6cbf-4503-a6d5-43e592ec3db9', national_sovereignty_is_primary, deontological).
narrative_ontology:cs_axiom('01c3dee1-6cbf-4503-a6d5-43e592ec3db9', foundational, free_movement_is_negotiable_policy).
narrative_ontology:cs_axiom_status(free_movement_is_negotiable_policy, holdable).
narrative_ontology:cs_axiom_grounding('01c3dee1-6cbf-4503-a6d5-43e592ec3db9', free_movement_is_negotiable_policy, conventional).
narrative_ontology:cs_reference_frame('01c3dee1-6cbf-4503-a6d5-43e592ec3db9', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('01c3dee1-6cbf-4503-a6d5-43e592ec3db9', contemporary_globalization_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('01c3dee1-6cbf-4503-a6d5-43e592ec3db9', '').
narrative_ontology:cs_kernel_id(federation_membership__sovereignty_reading, federation_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, national_governments).
narrative_ontology:constraint_beneficiary(federation_membership__sovereignty_reading, local_labor_markets).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, mobile_citizens).
narrative_ontology:constraint_victim(federation_membership__sovereignty_reading, migrant_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain primary authority over borders and migration policy, negotiating free movement as a conditional treaty. They benefit from controlling labor supply, national security, and maintaining political legitimacy through border enforcement.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, national_governments, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from controlled labor supply, which can reduce competition for certain jobs and maintain wage levels for existing workers. They support policies that restrict free movement to protect local employment.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, local_labor_markets, beneficiary,
    organized, biographical, constrained, local).

% Face administrative burdens, visa requirements, and potential restrictions on their ability to live and work freely across national borders within the federation. Their mobility is treated as a privilege, not a right.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_citizens, payer,
    powerless, immediate, constrained, regional).

% Are most severely impacted by border controls and conditional free movement, often facing precarious legal status, limited access to social services, and exploitation due to their constrained mobility options.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, migrant_workers, payer,
    powerless, immediate, trapped, regional).

% Oversee the implementation of treaties and mediate disputes between member states, but their authority is subordinate to national sovereignty in this reading. They analyze the impact of national policies on federal cohesion.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, federation_institutions, observer,
    institutional, generational, analytical, continental).

% Advocate for greater free movement and supranational authority, but their arguments are often sidelined or dismissed within a framework that prioritizes national sovereignty and conditional treaty obligations.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, pro_integration_advocates, excluded,
    organized, biographical, mobile, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(federation_membership__sovereignty_reading, national_governments).
narrative_ontology:fixing_cost_class(federation_membership__sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the interests of national governments within a federal structure, allowing member states to retain control over their borders and migration policies while participating in a broader economic and political union.
% TRANSFER_FUNCTION: Transfers control over border legitimacy and migration policy from a potential supranational authority to national governments, at the cost of restricted mobility and administrative burdens for individuals seeking to move across national lines.
% ABSENT_VOICES: Pro-integration advocates, mobile citizens, and migrant workers are largely excluded from the policy-making process that defines free movement as a negotiable policy, rather than a fundamental right. They would argue for open borders and a more integrated federal system.
% DISAPPEARANCE_RATIONALE: If national border legitimacy and conditional free movement vanished overnight, the default would become open borders. This would lead to massive demographic shifts, significant economic restructuring, and a fundamental redefinition of national and federal authority, as states would lose a core aspect of their sovereignty.
% FOUNDING_PROBLEM: The founding problem was to balance the benefits of inter-state cooperation and economic integration with the desire of member states to retain national sovereignty, particularly over their borders and population movements.
% FOUNDING_PROBLEM_CORROBORATION: National security agencies, national political parties, and certain economic sectors (e.g., those concerned with labor market stability) consistently attest to the ongoing need for border control and managed migration, corroborating the problem's live status from outside the immediate beneficiaries of restricted mobility.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__sovereignty_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership__sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership__sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership__sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) due to the significant costs and restrictions imposed on mobile citizens and migrant workers. Suppression is also high (0.75) as national governments actively enforce border controls and immigration policies. Theater ratio is low (0.15) because border control is a genuine, actively maintained function, not merely performative. The constraint's persistence relies on active enforcement and the political will of national governments.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of national governments, this constraint is a legitimate exercise of sovereignty and a necessary coordination mechanism. From the perspective of mobile citizens and migrant workers, it is an extractive barrier that limits their fundamental freedoms. The engine will compute these divergent classifications based on the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments are clear beneficiaries, gaining control over their borders and populations, and maintaining political legitimacy. Local labor markets also benefit from controlled labor supply. Mobile citizens and migrant workers are the primary targets, bearing the costs of restricted movement and administrative hurdles. Federation institutions act as observers, while pro-integration advocates are excluded from the core decision-making process.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_distinction,
    'Is this constraint a genuine expression of national sovereignty, or is it a reassertion of national power against an evolving federal integration?',
    'Analysis of legal precedents and political discourse over time: if the ''sovereignty_reading'' consistently reinterprets federal law to prioritize national control, it suggests a strategic reassertion rather than a static principle.',
    'If a strategic reassertion, the extractiveness might be higher than currently assessed, as it actively suppresses an alternative, more integrated federal structure. If a static principle, the current metrics are appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Distinguishing between a static principle of national sovereignty and a dynamic reassertion of national power within a federal context.').

omega_variable(
    free_movement_status_ambiguity,
    'Is free movement within the federation fundamentally a negotiable policy, or is it an emerging or suppressed fundamental right?',
    'Comparative legal analysis across different federal systems and international human rights frameworks: if a strong consensus for free movement as a right exists elsewhere, it suggests the ''negotiable policy'' framing is a choice, not an inevitability.',
    'If free movement is an emerging right, the suppression metric for mobile citizens would be higher, reflecting the active suppression of this right. If it is purely negotiable, the current metrics are appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(free_movement_status_ambiguity, preference, 'Ambiguity over the normative status of free movement within the federation.').

omega_variable(
    economic_impact_of_mobility_restriction,
    'What is the true economic cost of restricted mobility for the federation as a whole, considering lost productivity and innovation?',
    'Comprehensive economic modeling comparing scenarios with and without mobility restrictions, accounting for dynamic effects on labor markets, entrepreneurship, and demographic change.',
    'If the economic cost is substantially higher than perceived, it would challenge the ''beneficiary'' status of local labor markets and potentially increase the overall extractiveness of the constraint, shifting the balance of costs and benefits.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_impact_of_mobility_restriction, empirical, 'Uncertainty regarding the full economic impact of restricted mobility on the federal system.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 1990, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t1990, federation_membership__sovereignty_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(fede_tr_t1995, federation_membership__sovereignty_reading, theater_ratio, 1995, 0.11).
narrative_ontology:measurement(fede_tr_t2000, federation_membership__sovereignty_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(fede_tr_t2005, federation_membership__sovereignty_reading, theater_ratio, 2005, 0.13).
narrative_ontology:measurement(fede_tr_t2010, federation_membership__sovereignty_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(fede_tr_t2015, federation_membership__sovereignty_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(fede_tr_t2020, federation_membership__sovereignty_reading, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(fede_be_t1990, federation_membership__sovereignty_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(fede_be_t1995, federation_membership__sovereignty_reading, base_extractiveness, 1995, 0.62).
narrative_ontology:measurement(fede_be_t2000, federation_membership__sovereignty_reading, base_extractiveness, 2000, 0.64).
narrative_ontology:measurement(fede_be_t2005, federation_membership__sovereignty_reading, base_extractiveness, 2005, 0.66).
narrative_ontology:measurement(fede_be_t2010, federation_membership__sovereignty_reading, base_extractiveness, 2010, 0.67).
narrative_ontology:measurement(fede_be_t2015, federation_membership__sovereignty_reading, base_extractiveness, 2015, 0.68).
narrative_ontology:measurement(fede_be_t2020, federation_membership__sovereignty_reading, base_extractiveness, 2020, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t1990, federation_membership__sovereignty_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(fede_su_t1995, federation_membership__sovereignty_reading, suppression_requirement, 1995, 0.71).
narrative_ontology:measurement(fede_su_t2000, federation_membership__sovereignty_reading, suppression_requirement, 2000, 0.72).
narrative_ontology:measurement(fede_su_t2005, federation_membership__sovereignty_reading, suppression_requirement, 2005, 0.73).
narrative_ontology:measurement(fede_su_t2010, federation_membership__sovereignty_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(fede_su_t2015, federation_membership__sovereignty_reading, suppression_requirement, 2015, 0.75).
narrative_ontology:measurement(fede_su_t2020, federation_membership__sovereignty_reading, suppression_requirement, 2020, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership__sovereignty_reading, federation_membership__integration_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of two readings of the 'federation_membership' kernel. This 'sovereignty_reading' emphasizes national control, while the 'integration_reading' (constraint_federation_membership__integration_reading) emphasizes supranational authority and free movement as a right. Both are structurally distinct and linked here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
