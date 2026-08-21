% ============================================================================
% CONSTRAINT STORY: federation_membership_kernel__member_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_kernel__member_sovereignty_reading, []).

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
 *   constraint_id: federation_membership_kernel__member_sovereignty_reading
 *   human_readable: Member State Sovereignty over Free Movement and Welfare
 *   domain: political_economy/federalism/migration_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'member sovereignty' reading of the
 *   federation membership kernel, asserting that free movement rights are
 *   bounded by national welfare state capacity and labor market protection.
 *   Member states retain authority to exclude economically inactive migrants
 *   and protect social solidarity institutions. This reading prioritizes
 *   national control over supranational integration, leading to constrained
 *   mobility for certain migrant groups and intensified brain drain for
 *   sending states. The constraint is claimed as a 'tangled_rope' because it
 *   attempts to coordinate national interests while imposing significant
 *   costs on specific groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, 0.65).
domain_priors:suppression_score(federation_membership_kernel__member_sovereignty_reading, 0.7).
domain_priors:theater_ratio(federation_membership_kernel__member_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_kernel__member_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_kernel__member_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(federation_membership_kernel__member_sovereignty_reading, "Member State Sovereignty over Free Movement and Welfare").
narrative_ontology:topic_domain(federation_membership_kernel__member_sovereignty_reading, "political_economy/federalism/migration_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(federation_membership_kernel__member_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_kernel__member_sovereignty_reading, '98798abf-b286-44ea-941a-5ceed6894700').
narrative_ontology:cs_kernel_codification('98798abf-b286-44ea-941a-5ceed6894700', formalized).
narrative_ontology:cs_authority_grounding('98798abf-b286-44ea-941a-5ceed6894700', lineage).
narrative_ontology:cs_interpretation_layer_present('98798abf-b286-44ea-941a-5ceed6894700').
narrative_ontology:cs_reading_relation('98798abf-b286-44ea-941a-5ceed6894700', federation_membership_kernel__integration_reading, coexists_with).
narrative_ontology:cs_reading_relation('98798abf-b286-44ea-941a-5ceed6894700', federation_membership_kernel__welfare_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('98798abf-b286-44ea-941a-5ceed6894700', foundational, national_welfare_state_primacy).
narrative_ontology:cs_axiom_status(national_welfare_state_primacy, holdable).
narrative_ontology:cs_axiom_grounding('98798abf-b286-44ea-941a-5ceed6894700', national_welfare_state_primacy, conventional).
narrative_ontology:cs_axiom('98798abf-b286-44ea-941a-5ceed6894700', foundational, member_state_border_control_authority).
narrative_ontology:cs_axiom_status(member_state_border_control_authority, holdable).
narrative_ontology:cs_axiom_grounding('98798abf-b286-44ea-941a-5ceed6894700', member_state_border_control_authority, conventional).
narrative_ontology:cs_reference_frame('98798abf-b286-44ea-941a-5ceed6894700', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('98798abf-b286-44ea-941a-5ceed6894700', contemporary_eu_jurisprudence, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('98798abf-b286-44ea-941a-5ceed6894700', '').
narrative_ontology:cs_kernel_id(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, receiving_member_states).
narrative_ontology:constraint_beneficiary(federation_membership_kernel__member_sovereignty_reading, national_welfare_recipients).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, sending_member_states).
narrative_ontology:constraint_victim(federation_membership_kernel__member_sovereignty_reading, mobile_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states assert their right to control access to their welfare systems and labor markets, excluding migrants deemed economically inactive. They benefit from reduced fiscal pressure on social services and perceived protection of national labor. They actively enforce border controls and welfare eligibility criteria.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, receiving_member_states, agenda_setter,
    institutional, generational, constrained, national).

% Individuals seeking to exercise free movement rights but deemed not economically active by receiving states. They face exclusion, deportation, or denial of social benefits, leading to precarity and limited access to essential services. Their mobility is severely constrained.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, economically_inactive_migrants, payer,
    powerless, immediate, trapped, regional).

% These states experience 'brain drain' as their skilled workers leave, while their less skilled or economically inactive citizens face barriers to movement. They bear the social and economic costs of restricted mobility for their citizens and reduced remittances.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, sending_member_states, payer,
    institutional, biographical, constrained, national).

% Citizens of receiving states who rely on national welfare systems. They perceive a benefit from policies that limit access for non-contributing migrants, believing it protects the sustainability and generosity of their social benefits. Their support for these policies reinforces the constraint.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, national_welfare_recipients, beneficiary,
    organized, biographical, constrained, national).

% Workers who move between member states for employment. While generally economically active, they face increased scrutiny, administrative burdens, and the risk of being reclassified as 'economically inactive' during periods of unemployment or illness, limiting their full exercise of free movement.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, mobile_workers, payer,
    moderate, biographical, constrained, continental).

% The Commission, as guardian of the treaties, advocates for expansive free movement rights and non-discrimination. This reading of the kernel limits its ability to enforce those principles, effectively excluding its preferred interpretation from full implementation.
narrative_ontology:constraint_stakeholder(federation_membership_kernel__member_sovereignty_reading, european_commission, excluded,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate the protection of national welfare state integrity and labor market stability within a federal structure, allowing member states to manage the social impact of free movement.
% TRANSFER_FUNCTION: Transfers the burden of welfare provision and labor market competition from receiving member states to economically inactive migrants and sending member states, by restricting access to social benefits and employment opportunities.
% ABSENT_VOICES: The European Commission and advocates for expansive EU citizenship rights are structurally excluded from fully shaping this policy, as their interpretations are overridden by member state assertions of sovereignty. Economically inactive migrants themselves have limited voice in policy formation.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, member states would lose a key mechanism for controlling migration's social impact, leading to immediate pressure on welfare systems and labor markets. Migratory flows would likely increase, and the political economy of the federation would undergo significant restructuring.
% FOUNDING_PROBLEM: The tension between the principle of free movement and the fiscal sustainability of national welfare states, particularly in the context of economic disparities between member states.
% FOUNDING_PROBLEM_CORROBORATION: Member states consistently attest to the live nature of this problem, citing ongoing fiscal pressures and public concern over welfare tourism. Independent economic analyses and sociological studies from outside the benefiting parties corroborate the existence of these tensions, though they may dispute the severity or appropriate policy response.
narrative_ontology:disappearance_verdict(federation_membership_kernel__member_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_kernel__member_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_kernel__member_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_kernel__member_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_kernel__member_sovereignty_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_kernel__member_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(federation_membership_kernel__member_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(federation_membership_kernel__member_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) reflects the costs borne by migrants and sending states due to restricted access and welfare exclusion. Suppression (0.70) is high due to active enforcement of border controls, eligibility criteria, and legal challenges to supranational interpretations. The theater ratio (0.20) is relatively low, as the stated goal of protecting national welfare systems is genuinely pursued, though with significant extractive side effects. Accessibility collapse (0.40) is moderate, as alternatives (e.g., illegal migration, reliance on informal networks) exist but are highly constrained. Resistance (0.55) is present from migrant advocacy groups and some sending states.
 *
 * PERSPECTIVAL GAP:
 *   Receiving member states perceive this as a necessary coordination mechanism to preserve national sovereignty and welfare. Migrants and sending states experience it as an extractive barrier to fundamental rights. The engine's classification will highlight this divergence, showing a 'tangled_rope' from the perspective of those bearing the costs, despite the 'agenda-setter's' framing.
 *
 * DIRECTIONALITY LOGIC:
 *   Receiving member states and their national welfare recipients are beneficiaries (low d) as they achieve their goals of fiscal protection and social solidarity. Economically inactive migrants, sending member states, and mobile workers are victims/targets (high d) as they bear the costs of exclusion, restricted access, and brain drain. The European Commission is an excluded observer, its preferred interpretation of free movement being actively resisted.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_inactivity_definition_ambiguity,
    'How is ''economically inactive'' defined and applied across member states, and does this definition genuinely reflect a lack of economic contribution or serve as a pretext for exclusion?',
    'Comparative legal analysis of national implementation, coupled with empirical studies on the actual economic and social contributions of migrants classified as ''inactive''.',
    'If the definition is found to be overly broad or used pretextually, the constraint''s extractiveness and suppression would be re-evaluated as higher, potentially shifting its classification towards a ''snare'' for migrants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_inactivity_definition_ambiguity, empirical, 'Ambiguity in the definition of ''economically inactive'' migrants.').

omega_variable(
    welfare_state_sustainability_empirical_basis,
    'To what extent is the claim of ''welfare state capacity'' genuinely threatened by free movement, as opposed to being a political narrative to justify restrictive policies?',
    'Longitudinal economic modeling and demographic studies comparing welfare state sustainability in open vs. restrictive migration regimes, controlling for other fiscal factors.',
    'If the threat is empirically weak, the justification for the constraint''s extractive elements would be undermined, potentially reclassifying it as a ''snare'' or a more purely extractive ''tangled_rope''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_state_sustainability_empirical_basis, empirical, 'Empirical basis for claims of welfare state capacity threat.').

omega_variable(
    member_sovereignty_vs_supranational_primacy,
    'Is the assertion of member state sovereignty over free movement a legitimate interpretation of federal principles, or does it fundamentally undermine the supranational legal order?',
    'Legal-philosophical analysis of federalism models and the foundational treaties, alongside judicial rulings from the European Court of Justice.',
    'If it fundamentally undermines the supranational order, this reading would be seen as a ''snare'' for the federal project itself, rather than a legitimate ''tangled_rope'' within it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(member_sovereignty_vs_supranational_primacy, conceptual, 'Conceptual tension between member state sovereignty and supranational legal primacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_kernel__member_sovereignty_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(fede_tr_t5, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 5, 0.17).
narrative_ontology:measurement(fede_tr_t10, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(fede_tr_t15, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 15, 0.19).
narrative_ontology:measurement(fede_tr_t20, federation_membership_kernel__member_sovereignty_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(fede_be_t5, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(fede_be_t10, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(fede_be_t15, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 15, 0.64).
narrative_ontology:measurement(fede_be_t20, federation_membership_kernel__member_sovereignty_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(fede_su_t5, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 5, 0.63).
narrative_ontology:measurement(fede_su_t10, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 10, 0.66).
narrative_ontology:measurement(fede_su_t15, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(fede_su_t20, federation_membership_kernel__member_sovereignty_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_kernel__member_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__integration_reading).
narrative_ontology:affects_constraint(federation_membership_kernel__member_sovereignty_reading, federation_membership_kernel__welfare_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'federation_membership_kernel'. It represents the member state sovereignty perspective, contrasting with the integrationist and welfare coordination readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
