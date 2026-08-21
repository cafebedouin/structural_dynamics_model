% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__sovereignty_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__sovereignty_primary, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: federation_membership_treaty__sovereignty_primary
 *   human_readable: Federation Membership Treaty: Sovereignty Primary Reading
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty_primary' reading of a
 *   federation's membership treaty, where free movement is explicitly
 *   conditional on member state consent. States retain significant authority
 *   to protect national labor markets and welfare systems, viewing these as
 *   core sovereign responsibilities. The constraint is claimed as a
 *   'tangled_rope' because it genuinely coordinates the interests of member
 *   states (beneficiaries) while imposing substantial, actively enforced
 *   restrictions on mobile workers (victims).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, 0.65).
domain_priors:suppression_score(federation_membership_treaty__sovereignty_primary, 0.75).
domain_priors:theater_ratio(federation_membership_treaty__sovereignty_primary, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(federation_membership_treaty__sovereignty_primary, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__sovereignty_primary, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__sovereignty_primary, "Federation Membership Treaty: Sovereignty Primary Reading").
narrative_ontology:topic_domain(federation_membership_treaty__sovereignty_primary, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__sovereignty_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__sovereignty_primary, '98385380-b4d9-4d02-b62c-6414d1d0940f').
narrative_ontology:cs_kernel_codification('98385380-b4d9-4d02-b62c-6414d1d0940f', formalized).
narrative_ontology:cs_authority_grounding('98385380-b4d9-4d02-b62c-6414d1d0940f', lineage).
narrative_ontology:cs_interpretation_layer_present('98385380-b4d9-4d02-b62c-6414d1d0940f').
narrative_ontology:cs_reading_relation('98385380-b4d9-4d02-b62c-6414d1d0940f', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('98385380-b4d9-4d02-b62c-6414d1d0940f', federation_membership_treaty__subsidiarity_balance, coexists_with).
narrative_ontology:cs_axiom('98385380-b4d9-4d02-b62c-6414d1d0940f', foundational, national_sovereignty_is_primary).
narrative_ontology:cs_axiom_status(national_sovereignty_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('98385380-b4d9-4d02-b62c-6414d1d0940f', national_sovereignty_is_primary, conventional).
narrative_ontology:cs_axiom('98385380-b4d9-4d02-b62c-6414d1d0940f', foundational, labor_market_protection_is_legitimate).
narrative_ontology:cs_axiom_status(labor_market_protection_is_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('98385380-b4d9-4d02-b62c-6414d1d0940f', labor_market_protection_is_legitimate, instrumental).
narrative_ontology:cs_reference_frame('98385380-b4d9-4d02-b62c-6414d1d0940f', sovereign_state_control_framework).
narrative_ontology:cs_drift_state('98385380-b4d9-4d02-b62c-6414d1d0940f', contemporary_federation_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('98385380-b4d9-4d02-b62c-6414d1d0940f', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__sovereignty_primary, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, member_states).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, national_labor_markets).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__sovereignty_primary, welfare_system_administrators).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, mobile_workers).
narrative_ontology:constraint_victim(federation_membership_treaty__sovereignty_primary, migrant_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain primary authority to protect national borders, labor markets, and welfare systems, making free movement conditional on their consent. They benefit from maintaining regulatory autonomy and control over domestic policy.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, member_states, agenda_setter,
    institutional, generational, constrained, national).

% Are protected from perceived excessive competition from mobile workers, which is argued to maintain wage levels and employment for domestic workers. This protection is a key justification for conditional free movement.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, national_labor_markets, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_non_agent(federation_membership_treaty__sovereignty_primary, national_labor_markets).

% Manage national welfare systems and benefit from the ability to control access to social benefits, ensuring sustainability for national contributors and preventing 'welfare tourism'.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, welfare_system_administrators, beneficiary,
    institutional, biographical, constrained, national).

% Face restrictions on their ability to move freely, seek employment, and access social benefits across member states. They bear the costs of conditional access, administrative hurdles, and potential discrimination.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, mobile_workers, payer,
    powerless, immediate, constrained, regional).

% Experience family separation, uncertainty, and difficulty in establishing stable lives due to the conditional nature of movement rights and varying national policies on family reunification and residency.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, migrant_families, payer,
    powerless, biographical, constrained, regional).

% Oversee the implementation of the federation treaty. In this reading, their power to enforce broader free movement is limited by the explicit recognition of member state sovereignty, leading to a more constrained role.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, federation_institutions, observer,
    institutional, generational, analytical, global).

% Advocate for stronger, less conditional free movement rights and reduced national barriers. Their arguments are often marginalized in policy discussions that prioritize national sovereignty and control.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__sovereignty_primary, pro_integration_advocates, excluded,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the conditional movement of people across member states, allowing individual states to retain significant control over their borders, national labor markets, and welfare systems while participating in a broader economic and political federation.
% TRANSFER_FUNCTION: Transfers regulatory autonomy and control over labor market access and welfare eligibility to member states, at the cost of restricted mobility, economic opportunity, and social integration for mobile workers and their families.
% ABSENT_VOICES: Pro-integration advocates, mobile worker unions, and human rights organizations, who would argue for unconditional free movement and equal rights, are often structurally excluded or marginalized in policy debates dominated by national sovereignty concerns.
% DISAPPEARANCE_RATIONALE: If the conditionality of free movement vanished overnight, member states would lose a key tool for national policy, leading to immediate and significant shifts in labor markets, welfare systems, and border management. The fundamental balance of power within the federation would be irrevocably altered, requiring a complete reorganization of its legal and administrative structures.
% FOUNDING_PROBLEM: To balance the desire for economic integration and cooperation within a federation with the sovereign right of member states to control their national borders, protect domestic labor markets, and manage the sustainability of their welfare systems.
% FOUNDING_PROBLEM_CORROBORATION: Member states consistently attest to the ongoing need for national control over borders and welfare, citing public opinion, fiscal sustainability, and national identity. Independent analyses of federal structures often highlight the inherent tension between integration and sovereignty, corroborating the problem's persistence from a broader political science perspective.
narrative_ontology:disappearance_verdict(federation_membership_treaty__sovereignty_primary, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__sovereignty_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__sovereignty_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(federation_membership_treaty__sovereignty_primary, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__sovereignty_primary, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__sovereignty_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_treaty__sovereignty_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_treaty__sovereignty_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because mobile workers face significant barriers and costs due to conditional access. Suppression is also high (0.75) as member states actively enforce border controls, residency requirements, and welfare eligibility rules. The theater ratio is low (0.15) because the enforcement mechanisms are genuinely functional in achieving the stated goals of national control, not merely performative. Accessibility collapse is substantial (0.7) for mobile workers, as alternatives to navigating national restrictions are severely limited. Resistance is moderate (0.4), reflecting ongoing advocacy from pro-integration groups and mobile worker organizations, but not a systemic challenge to the core principle of national control.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of member states and national interests, this constraint is a necessary coordination mechanism to preserve national autonomy and welfare. From the perspective of mobile workers and pro-integration advocates, it operates as an extractive and suppressive barrier to fundamental rights. The engine's per-seat classification will reflect this divergence based on the structural data provided.
 *
 * DIRECTIONALITY LOGIC:
 *   Member states, national labor markets, and welfare system administrators are structural beneficiaries (low directionality) as they gain control and protection. Mobile workers and migrant families are clear targets (high directionality) as they bear the costs of restricted movement and access. Federation institutions act as observers, while pro-integration advocates are excluded, their positions not fully integrated into the constraint's operational logic.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_framing_ambiguity,
    'Is the conditional nature of free movement a necessary balance for national sovereignty, or an extractive mechanism leveraging state power to limit mobility?',
    'Comparative analysis of federations with different free movement regimes, assessing long-term economic and social outcomes for both member states and mobile populations.',
    'If primarily extractive, the constraint''s effective extractiveness (chi) would be higher for mobile workers, and its classification would lean more towards a Snare. If a necessary balance, the Tangled Rope classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity in whether conditional free movement is a legitimate balance or an extractive tool.').

omega_variable(
    economic_impact_of_restrictions,
    'What is the true economic cost of restricted free movement for the federation as a whole, in terms of lost productivity, innovation, and demographic balance?',
    'Comprehensive economic modeling and empirical studies comparing growth rates and labor market dynamics in open vs. restricted mobility zones within similar federations.',
    'If the economic costs are substantial, the justification for national labor market protection weakens, potentially shifting the constraint''s perceived coordination function and increasing its effective extractiveness from a systemic perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(economic_impact_of_restrictions, empirical, 'Uncertainty about the overall economic impact of conditional free movement.').

omega_variable(
    political_contestation_intensity,
    'How deeply entrenched is the ''sovereignty primary'' reading within the political and legal systems of member states, and what is the potential for a shift towards more integrationist interpretations?',
    'Analysis of electoral outcomes, judicial rulings, and legislative debates over time, particularly in response to economic or social crises, to gauge the resilience of the sovereignty-first approach.',
    'A weakening of this reading''s political entrenchment could lead to a reduction in suppression and extractiveness over time, potentially shifting the constraint towards a Rope or even a Scaffold if a transitional phase is adopted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_contestation_intensity, empirical, 'The degree to which the sovereignty-primary reading is politically stable or subject to change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__sovereignty_primary, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__sovereignty_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t6, federation_membership_treaty__sovereignty_primary, theater_ratio, 6, 0.15).
narrative_ontology:measurement(fede_tr_t12, federation_membership_treaty__sovereignty_primary, theater_ratio, 12, 0.15).
narrative_ontology:measurement(fede_tr_t18, federation_membership_treaty__sovereignty_primary, theater_ratio, 18, 0.15).
narrative_ontology:measurement(fede_tr_t24, federation_membership_treaty__sovereignty_primary, theater_ratio, 24, 0.15).
narrative_ontology:measurement(fede_tr_t30, federation_membership_treaty__sovereignty_primary, theater_ratio, 30, 0.15).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__sovereignty_primary, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fede_be_t6, federation_membership_treaty__sovereignty_primary, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(fede_be_t12, federation_membership_treaty__sovereignty_primary, base_extractiveness, 12, 0.61).
narrative_ontology:measurement(fede_be_t18, federation_membership_treaty__sovereignty_primary, base_extractiveness, 18, 0.63).
narrative_ontology:measurement(fede_be_t24, federation_membership_treaty__sovereignty_primary, base_extractiveness, 24, 0.64).
narrative_ontology:measurement(fede_be_t30, federation_membership_treaty__sovereignty_primary, base_extractiveness, 30, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__sovereignty_primary, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(fede_su_t6, federation_membership_treaty__sovereignty_primary, suppression_requirement, 6, 0.68).
narrative_ontology:measurement(fede_su_t12, federation_membership_treaty__sovereignty_primary, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(fede_su_t18, federation_membership_treaty__sovereignty_primary, suppression_requirement, 18, 0.72).
narrative_ontology:measurement(fede_su_t24, federation_membership_treaty__sovereignty_primary, suppression_requirement, 24, 0.74).
narrative_ontology:measurement(fede_su_t30, federation_membership_treaty__sovereignty_primary, suppression_requirement, 30, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
