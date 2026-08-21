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
 *   constraint_id: federation_membership__sovereignty_reading
 *   human_readable: Federation Membership (Sovereignty Reading)
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint represents the 'sovereignty reading' of federation
 *   membership, where national authority over borders and migration policy is
 *   retained, and free movement is a negotiable policy rather than an
 *   inherent right. It contrasts with an 'integration reading' that posits
 *   supranational authority and free movement as a constitutional right. This
 *   story focuses on the structural implications of the sovereignty
 *   perspective, where local labor markets benefit from control, and mobile
 *   citizens bear the costs of restricted movement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership__sovereignty_reading, 0.65).
domain_priors:suppression_score(federation_membership__sovereignty_reading, 0.7).
domain_priors:theater_ratio(federation_membership__sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership__sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership__sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership__sovereignty_reading, "Federation Membership (Sovereignty Reading)").
narrative_ontology:topic_domain(federation_membership__sovereignty_reading, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership__sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership__sovereignty_reading, 'f6160e21-4fa8-476b-b876-50d10feddc61').
narrative_ontology:cs_kernel_codification('f6160e21-4fa8-476b-b876-50d10feddc61', formalized).
narrative_ontology:cs_authority_grounding('f6160e21-4fa8-476b-b876-50d10feddc61', lineage).
narrative_ontology:cs_interpretation_layer_present('f6160e21-4fa8-476b-b876-50d10feddc61').
narrative_ontology:cs_reading_relation('f6160e21-4fa8-476b-b876-50d10feddc61', federation_membership__integration_reading, coexists_with).
narrative_ontology:cs_axiom('f6160e21-4fa8-476b-b876-50d10feddc61', foundational, national_sovereignty_over_borders_is_primary).
narrative_ontology:cs_axiom_status(national_sovereignty_over_borders_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('f6160e21-4fa8-476b-b876-50d10feddc61', national_sovereignty_over_borders_is_primary, conventional).
narrative_ontology:cs_axiom('f6160e21-4fa8-476b-b876-50d10feddc61', foundational, free_movement_is_a_negotiable_policy).
narrative_ontology:cs_axiom_status(free_movement_is_a_negotiable_policy, holdable).
narrative_ontology:cs_axiom_grounding('f6160e21-4fa8-476b-b876-50d10feddc61', free_movement_is_a_negotiable_policy, instrumental).
narrative_ontology:cs_reference_frame('f6160e21-4fa8-476b-b876-50d10feddc61', westphalian_state_sovereignty).
narrative_ontology:cs_drift_state('f6160e21-4fa8-476b-b876-50d10feddc61', contemporary_globalization_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f6160e21-4fa8-476b-b876-50d10feddc61', '').
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

% Retain ultimate authority over borders and migration policy, viewing federation membership as a conditional treaty. They benefit from the ability to control labor supply and manage social services, but are constrained by treaty obligations.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, national_governments, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from the ability to regulate the influx of labor, protecting domestic wages and employment conditions. They are sensitive to changes in migration policy and advocate for national control over borders.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, local_labor_markets, beneficiary,
    organized, biographical, constrained, local).

% Experience restrictions on their ability to freely move and settle across federation borders, facing administrative hurdles and potential discrimination based on national origin. Their mobility is treated as a negotiable policy, not an inherent right.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, mobile_citizens, payer,
    moderate, biographical, constrained, regional).

% Are most directly impacted by border controls and restrictive migration policies, facing precarious legal status, limited access to social services, and exploitation in unregulated labor sectors. Their exit options are severely limited.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, migrant_workers, payer,
    powerless, immediate, trapped, local).

% Are seen as having limited legitimate authority over national border policy, their attempts to enforce free movement principles are resisted by national governments. Their role is minimized in this reading.
narrative_ontology:constraint_stakeholder(federation_membership__sovereignty_reading, supranational_institutions, excluded,
    institutional, generational, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the terms of national participation in a federation, allowing member states to retain significant control over their internal affairs, including border management, while benefiting from economic cooperation.
% TRANSFER_FUNCTION: Transfers the right to control borders and labor markets to national governments, at the cost of restricted mobility and rights for mobile citizens and migrant workers.
% ABSENT_VOICES: Advocates for universal human rights and open borders are largely excluded from the policy-making process, as their arguments challenge the foundational premise of national sovereignty over borders. Supranational institutions, while present, have their authority curtailed.
% DISAPPEARANCE_RATIONALE: If this reading of federation membership vanished, national governments would lose a key justification for border controls, leading to a rapid re-evaluation of migration policies and potentially a significant increase in free movement. The balance of power between national and supranational entities would shift dramatically.
% FOUNDING_PROBLEM: The need to balance national sovereignty and self-determination with the benefits of inter-state cooperation and economic integration, particularly concerning the movement of people.
% FOUNDING_PROBLEM_CORROBORATION: National governments and their electorates consistently attest that the problem of balancing sovereignty with cooperation is live, citing concerns over national identity, social cohesion, and economic stability. This is corroborated by ongoing political debates and electoral outcomes across member states.
narrative_ontology:disappearance_verdict(federation_membership__sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership__sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership__sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership__sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership__sovereignty_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is driven by the costs imposed on mobile citizens and migrant workers through restricted movement and administrative burdens. Suppression (0.70) is high due to active enforcement of border controls and national migration policies. Theater ratio (0.20) is relatively low, as the border control function is genuinely active and not merely performative, though some rhetoric may mask economic protectionism. The claimed type is 'tangled_rope' because it offers a coordination function (national control over internal affairs) alongside asymmetric extraction from those whose mobility is restricted.
 *
 * PERSPECTIVAL GAP:
 *   National governments perceive this as a legitimate exercise of sovereignty and a necessary coordination mechanism for national interests. Mobile citizens and migrant workers experience it as an extractive and suppressive barrier to their opportunities and rights. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   National governments and local labor markets are beneficiaries, as they gain control over labor supply and policy. Mobile citizens and migrant workers are victims, facing direct costs and restrictions on their movement. Supranational institutions are excluded, as their claims to authority over free movement are resisted by national governments in this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_border_control,
    'Is national border control a legitimate exercise of sovereignty or an arbitrary restriction on human mobility?',
    'International legal precedent, philosophical arguments on human rights vs. state sovereignty, and empirical studies on the economic and social impacts of open vs. closed borders.',
    'If deemed arbitrary, the extractiveness and suppression would be re-evaluated as unjust, potentially reclassifying the constraint towards a Snare. If legitimate, the current classification as Tangled Rope would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_border_control, conceptual, 'The fundamental philosophical and legal justification for national border controls.').

omega_variable(
    economic_impact_of_mobility_restrictions,
    'What is the net economic impact of restricted free movement on the federation as a whole, considering both national labor market protection and lost productivity from restricted mobility?',
    'Comprehensive economic modeling and empirical studies comparing federations with different levels of internal mobility, accounting for both direct and indirect costs and benefits.',
    'If the net economic cost of restrictions is found to be high, it would challenge the ''beneficiary'' status of national governments and local labor markets, potentially increasing the overall extractiveness score and pushing the constraint closer to a Snare. If benefits outweigh costs, the coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_impact_of_mobility_restrictions, empirical, 'The overall economic efficiency and welfare effects of restricted free movement within the federation.').

omega_variable(
    mandatrophy_of_national_border_control,
    'Has the original problem of managing national borders within a federation evolved such that the current level of national control is no longer justified by its founding mandate?',
    'Historical analysis of the founding conditions of the federation, comparison with contemporary challenges, and assessment of whether supranational mechanisms could more effectively address current issues.',
    'If the founding mandate has atrophied, the constraint would lean towards a Piton, as its persistence would be due to inertia rather than a live problem. If the mandate is still live, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_of_national_border_control, empirical, 'Whether the justification for national border control within the federation has atrophied over time.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership__sovereignty_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership__sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t5, federation_membership__sovereignty_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(fede_tr_t10, federation_membership__sovereignty_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(fede_tr_t15, federation_membership__sovereignty_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(fede_tr_t20, federation_membership__sovereignty_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership__sovereignty_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fede_be_t5, federation_membership__sovereignty_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(fede_be_t10, federation_membership__sovereignty_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(fede_be_t15, federation_membership__sovereignty_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(fede_be_t20, federation_membership__sovereignty_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership__sovereignty_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fede_su_t5, federation_membership__sovereignty_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(fede_su_t10, federation_membership__sovereignty_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(fede_su_t15, federation_membership__sovereignty_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(fede_su_t20, federation_membership__sovereignty_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership__sovereignty_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'sovereignty reading' of federation membership, emphasizing national control over borders. It is a sibling to the 'integration_reading' (constraint_federation_membership__integration_reading), which emphasizes supranational authority and free movement as a constitutional right. Both are distinct constraints derived from the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
