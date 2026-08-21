% ============================================================================
% CONSTRAINT STORY: federation_membership_treaty__subsidiarity_balance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_federation_membership_treaty__subsidiarity_balance, []).

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
 *   constraint_id: federation_membership_treaty__subsidiarity_balance
 *   human_readable: Federation Membership Treaty: Subsidiarity Balance Reading
 *   domain: political_economy/federalism/migration_policy
 *
 * SUMMARY:
 *   This constraint represents the 'subsidiarity balance' reading of a
 *   federation's membership treaty, specifically concerning free movement. It
 *   posits that free movement is a right, but one that can be legitimately
 *   constrained by national interests, provided these constraints are
 *   proportionate and do not eliminate the right entirely. This reading seeks
 *   a middle ground between full integration and full national sovereignty,
 *   leading to a graduated constraint structure where beneficiary and victim
 *   sets vary by policy domain and individual circumstances. The constraint
 *   is classified as a Tangled Rope due to its genuine coordination function
 *   (balancing competing interests) alongside asymmetric extraction (some
 *   member states and labor groups bear more costs).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(federation_membership_treaty__subsidiarity_balance, 0.45).
domain_priors:suppression_score(federation_membership_treaty__subsidiarity_balance, 0.55).
domain_priors:theater_ratio(federation_membership_treaty__subsidiarity_balance, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, extractiveness, 0.45).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(federation_membership_treaty__subsidiarity_balance, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(federation_membership_treaty__subsidiarity_balance, tangled_rope).
narrative_ontology:human_readable(federation_membership_treaty__subsidiarity_balance, "Federation Membership Treaty: Subsidiarity Balance Reading").
narrative_ontology:topic_domain(federation_membership_treaty__subsidiarity_balance, "political_economy/federalism/migration_policy").

domain_priors:requires_active_enforcement(federation_membership_treaty__subsidiarity_balance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, '0d35ef4e-21ca-4d0d-97e4-d1960261d8d0').
narrative_ontology:cs_kernel_codification('0d35ef4e-21ca-4d0d-97e4-d1960261d8d0', formalized).
narrative_ontology:cs_authority_grounding('0d35ef4e-21ca-4d0d-97e4-d1960261d8d0', lineage).
narrative_ontology:cs_interpretation_layer_present('0d35ef4e-21ca-4d0d-97e4-d1960261d8d0').
narrative_ontology:cs_reading_relation('0d35ef4e-21ca-4d0d-97e4-d1960261d8d0', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('0d35ef4e-21ca-4d0d-97e4-d1960261d8d0', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('0d35ef4e-21ca-4d0d-97e4-d1960261d8d0', foundational, proportionality_principle_applies_to_mobility).
narrative_ontology:cs_axiom_status(proportionality_principle_applies_to_mobility, holdable).
narrative_ontology:cs_axiom_grounding('0d35ef4e-21ca-4d0d-97e4-d1960261d8d0', proportionality_principle_applies_to_mobility, conventional).
narrative_ontology:cs_axiom('0d35ef4e-21ca-4d0d-97e4-d1960261d8d0', foundational, legitimate_national_interests_constrain_federal_rights).
narrative_ontology:cs_axiom_status(legitimate_national_interests_constrain_federal_rights, holdable).
narrative_ontology:cs_axiom_grounding('0d35ef4e-21ca-4d0d-97e4-d1960261d8d0', legitimate_national_interests_constrain_federal_rights, conventional).
narrative_ontology:cs_reference_frame('0d35ef4e-21ca-4d0d-97e4-d1960261d8d0', founding_treaty_compromise).
narrative_ontology:cs_drift_state('0d35ef4e-21ca-4d0d-97e4-d1960261d8d0', contemporary_migration_crises_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0d35ef4e-21ca-4d0d-97e4-d1960261d8d0', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, member_states_with_strong_welfare_systems).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, mobile_skilled_labor).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, member_states_with_weak_economies).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, unskilled_migrant_labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the ability to apply proportionality tests to free movement, allowing them to protect national labor markets and welfare systems from perceived undue strain, while still benefiting from skilled labor mobility. They enforce these tests through national legislation and border controls.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, member_states_with_strong_welfare_systems, beneficiary,
    institutional, generational, constrained, national).

% Bear the cost of their citizens facing restrictions on movement to wealthier states, limiting opportunities for economic improvement through migration. They are constrained by treaty obligations from imposing blanket restrictions on other member state citizens, but also cannot fully leverage free movement for their own citizens due to receiving states' proportionality tests.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, member_states_with_weak_economies, payer,
    institutional, generational, constrained, national).

% Benefits from the general principle of free movement, allowing them to seek employment across the federation, though subject to proportionality tests that may require specific skills or job offers. Their mobility is constrained but not eliminated.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, mobile_skilled_labor, beneficiary,
    moderate, biographical, mobile, regional).

% Faces significant barriers to free movement due to proportionality tests that prioritize skilled labor or specific economic needs. They often lack the resources or legal standing to challenge restrictions, making their exit options severely limited.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, unskilled_migrant_labor, payer,
    powerless, immediate, trapped, regional).

% Interprets and applies the proportionality principle, balancing national interests against mobility rights. Their rulings shape the practical application of free movement, often mediating disputes between member states and individuals.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federal_judiciary, agenda_setter,
    institutional, generational, analytical, continental).

% Monitors the implementation of free movement policies and national proportionality tests, issuing guidance and initiating infringement procedures against member states that overstep their bounds. They advocate for a balanced approach but lack direct enforcement power.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federal_commission, observer,
    institutional, generational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the economic benefits of labor mobility across a federation with the legitimate concerns of member states regarding national sovereignty, welfare system sustainability, and labor market protection, preventing both unrestricted migration and blanket protectionism.
% TRANSFER_FUNCTION: Transfers the right to impose certain restrictions on free movement to member states (from the federal level), while transferring the burden of justification for these restrictions back to the member states (from individuals seeking to move).
% ABSENT_VOICES: Advocacy groups for unrestricted free movement would argue that proportionality tests are often used as a pretext for discrimination, while hardline nationalist groups would argue that any federal constraint on national migration policy is illegitimate. Both are marginalized by the current 'balance' framing.
% DISAPPEARANCE_RATIONALE: If this reading of the treaty vanished, either free movement would become entirely unrestricted (leading to significant social and economic upheaval in some member states) or member states would revert to full national control over borders (dismantling the federal project's mobility aspect). The current balance, however imperfect, prevents these extremes.
% FOUNDING_PROBLEM: The original treaty aimed to foster economic integration and solidarity by enabling citizens to live and work across member states, while acknowledging the distinct national identities and social systems of each member.
% FOUNDING_PROBLEM_CORROBORATION: Federal institutions and most member states attest that the tension between integration and national sovereignty remains a live problem, requiring ongoing balancing. Academic analyses of federalism and migration policy also corroborate the persistence of this fundamental tension.
narrative_ontology:disappearance_verdict(federation_membership_treaty__subsidiarity_balance, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__subsidiarity_balance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(federation_membership_treaty__subsidiarity_balance, 'none', 1).
narrative_ontology:epsilon_provenance(federation_membership_treaty__subsidiarity_balance, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(federation_membership_treaty__subsidiarity_balance_tests).
:- end_tests(federation_membership_treaty__subsidiarity_balance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate, reflecting the compromise nature of this reading: neither fully extractive nor fully coordinative. It extracts from those whose mobility is curtailed by proportionality tests, but also provides the benefit of a generally open internal market. Suppression (0.55) is moderate, as it requires active enforcement to prevent both unrestricted mobility and blanket national restrictions. Theater ratio is low (0.15) because the balancing act is a genuine, ongoing legal and political process, not mere performance. Accessibility collapse (0.4) is moderate, as alternatives (unrestricted movement or full national control) are partially suppressed but not entirely eliminated. Resistance (0.3) is also moderate, as both pro-mobility and pro-sovereignty factions continually challenge the precise boundaries of the balance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of member states with strong welfare systems, this reading is a necessary Rope, ensuring stability. From the perspective of unskilled migrant labor, it can feel like a Snare, as their mobility is disproportionately curtailed. The federal judiciary views it as a complex but functional Tangled Rope, requiring constant adjustment. The engine's per-seat classification will reflect these divergences based on the declared power, exit options, and beneficiary/victim status.
 *
 * DIRECTIONALITY LOGIC:
 *   Member states with strong welfare systems are beneficiaries (damped extraction) as they can protect their interests. Mobile skilled labor also benefits from general mobility, though with some constraints. Member states with weak economies and unskilled migrant labor are victims (amplified extraction) as their citizens face barriers to upward mobility. The federal judiciary acts as an agenda-setter, defining the 'balance,' while the federal commission observes and advocates.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as a pure Rope (ignoring the extraction from certain groups) or a pure Snare (ignoring the genuine coordination function of balancing national interests with federal mobility). The 'subsidiarity balance' reading acknowledges the ongoing tension and the need for active management, which is characteristic of a Tangled Rope. Mandatrophy is not resolved, as the founding problem (balancing integration and national identity) remains live and contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_test_objectivity,
    'Are the proportionality tests applied by member states genuinely objective and non-discriminatory, or do they serve as a pretext for protectionist or xenophobic policies?',
    'Empirical analysis of national implementation data, judicial review outcomes, and comparative studies across member states, focusing on the actual impact on different migrant groups.',
    'If tests are found to be systematically biased, the constraint''s effective extractiveness and suppression for victim groups would be higher, pushing its classification closer to a Snare. If truly objective, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_test_objectivity, empirical, 'Assesses the fairness and intent behind national proportionality tests.').

omega_variable(
    balance_point_legitimacy,
    'Is the current ''balance'' between free movement and national interests perceived as legitimate by all stakeholders, or is it a temporary equilibrium maintained by power asymmetries?',
    'Longitudinal surveys of public opinion and stakeholder satisfaction across all member states and migrant groups, combined with analysis of political discourse and resistance movements.',
    'If legitimacy is low and maintained by power, the constraint''s stability is precarious, and its true nature might be closer to a Snare for the disempowered. If broadly legitimate, it strengthens the Rope aspect of the Tangled Rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(balance_point_legitimacy, preference, 'Examines the perceived legitimacy of the current balance point.').

omega_variable(
    kernel_reading_divergence,
    'Given the ''federation_membership_treaty'' kernel, how fundamentally do the ''subsidiarity_balance'', ''integration_primary'', and ''sovereignty_primary'' readings diverge in their structural implications?',
    'Comparative analysis of legal precedents, policy outcomes, and economic impacts under each reading, identifying specific points of irreconcilable conflict versus areas of mere policy difference.',
    'If divergences are fundamental and irreconcilable, it suggests the kernel is deeply ambiguous, and each reading constitutes a distinct, competing constraint. If divergences are primarily policy-level, it suggests a single constraint with different policy preferences.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'Examines the structural differences between the competing readings of the federation treaty.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fede_be_t5, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(fede_be_t15, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 15, 0.43).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 20, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fede_su_t5, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 5, 0.53).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(fede_su_t15, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 15, 0.54).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__sovereignty_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'federation_membership_treaty' kernel. This 'subsidiarity_balance' reading attempts to reconcile the 'integration_primary' and 'sovereignty_primary' readings, but its own application creates distinct beneficiaries and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
