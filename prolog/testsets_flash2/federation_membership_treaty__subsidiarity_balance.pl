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
 *   federation's membership treaty, where free movement is balanced against
 *   legitimate national interests through proportionality. It aims for a
 *   graduated constraint structure, with beneficiary and victim sets varying
 *   by policy domain and moderate suppression of both unrestricted mobility
 *   and blanket restrictions. The claimed type is Tangled Rope, reflecting a
 *   genuine coordination function (free movement) coupled with asymmetric
 *   extraction (costs borne by states with weaker welfare systems and less
 *   mobile labor).
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
narrative_ontology:cs_story_uid(federation_membership_treaty__subsidiarity_balance, '8d85dfe8-cc1f-484f-ae28-ee86d4b81f96').
narrative_ontology:cs_kernel_codification('8d85dfe8-cc1f-484f-ae28-ee86d4b81f96', formalized).
narrative_ontology:cs_authority_grounding('8d85dfe8-cc1f-484f-ae28-ee86d4b81f96', lineage).
narrative_ontology:cs_interpretation_layer_present('8d85dfe8-cc1f-484f-ae28-ee86d4b81f96').
narrative_ontology:cs_reading_relation('8d85dfe8-cc1f-484f-ae28-ee86d4b81f96', federation_membership_treaty__integration_primary, coexists_with).
narrative_ontology:cs_reading_relation('8d85dfe8-cc1f-484f-ae28-ee86d4b81f96', federation_membership_treaty__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('8d85dfe8-cc1f-484f-ae28-ee86d4b81f96', foundational, proportionality_as_governing_principle).
narrative_ontology:cs_axiom_status(proportionality_as_governing_principle, holdable).
narrative_ontology:cs_axiom_grounding('8d85dfe8-cc1f-484f-ae28-ee86d4b81f96', proportionality_as_governing_principle, conventional).
narrative_ontology:cs_axiom('8d85dfe8-cc1f-484f-ae28-ee86d4b81f96', foundational, legitimate_national_interests_as_limiting_factor).
narrative_ontology:cs_axiom_status(legitimate_national_interests_as_limiting_factor, holdable).
narrative_ontology:cs_axiom_grounding('8d85dfe8-cc1f-484f-ae28-ee86d4b81f96', legitimate_national_interests_as_limiting_factor, conventional).
narrative_ontology:cs_reference_frame('8d85dfe8-cc1f-484f-ae28-ee86d4b81f96', balanced_integration_framework).
narrative_ontology:cs_drift_state('8d85dfe8-cc1f-484f-ae28-ee86d4b81f96', contemporary_political_contestation, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8d85dfe8-cc1f-484f-ae28-ee86d4b81f96', '').
narrative_ontology:cs_kernel_id(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, member_states_with_strong_welfare_systems).
narrative_ontology:constraint_beneficiary(federation_membership_treaty__subsidiarity_balance, mobile_skilled_labor).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, member_states_with_weak_welfare_systems).
narrative_ontology:constraint_victim(federation_membership_treaty__subsidiarity_balance, unskilled_mobile_labor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the ability to attract skilled labor while retaining some capacity to manage the impact on national welfare systems through proportionate restrictions. They pay some costs in administrative overhead and limited social dumping but gain from labor mobility.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, member_states_with_strong_welfare_systems, beneficiary,
    institutional, generational, constrained, national).

% Bear the costs of potential brain drain and the administrative burden of managing free movement, while having less capacity to implement effective proportionate restrictions due to internal political or economic constraints. They gain less from labor mobility and face higher social costs.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, member_states_with_weak_welfare_systems, payer,
    institutional, generational, constrained, national).

% Benefits from the ability to seek employment across member states, with some predictable constraints based on national interests. Their skills make them less susceptible to blanket restrictions, allowing them to arbitrage opportunities.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, mobile_skilled_labor, beneficiary,
    moderate, biographical, mobile, regional).

% Faces more significant barriers due to national interests in protecting local labor markets or welfare systems. Their mobility rights are more easily constrained by proportionality tests, leading to higher effective suppression and limited access to opportunities.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, unskilled_mobile_labor, payer,
    powerless, immediate, constrained, regional).

% Interpret and enforce the balance between free movement and national interests, applying proportionality tests to member state measures. They shape the practical application of the treaty and mediate disputes.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federation_judicial_bodies, agenda_setter,
    institutional, generational, analytical, continental).

% Proposes legislation and monitors member state compliance, seeking to uphold the balance. They initiate infringement procedures against states that overstep their legitimate national interests or unduly restrict free movement.
narrative_ontology:constraint_stakeholder(federation_membership_treaty__subsidiarity_balance, federation_executive_commission, agenda_setter,
    institutional, generational, constrained, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the movement of people across a federation, allowing for economic integration and labor market flexibility while acknowledging and providing mechanisms for member states to protect their legitimate national interests.
% TRANSFER_FUNCTION: Transfers the right to move and reside from the federation to individuals, balanced by the transfer of regulatory authority from the federation back to member states for proportionate measures. Costs of integration are borne by states with weaker systems and less mobile labor.
% ABSENT_VOICES: Advocates for completely unrestricted movement would argue that national interests are often a pretext for protectionism. Advocates for absolute national sovereignty would argue that any federal constraint on migration policy is illegitimate. Both are present in political discourse but not fully represented in the treaty's current balance.
% DISAPPEARANCE_RATIONALE: If this balance vanished, either free movement would become absolute (leading to significant social and economic disruption in some member states) or national borders would reassert full control (fragmenting the federation's single market and undermining its core principles). The current equilibrium would collapse.
% FOUNDING_PROBLEM: To enable economic integration and a common market by facilitating labor mobility, while respecting the diverse social and economic structures of sovereign member states and their right to protect their populations.
% FOUNDING_PROBLEM_CORROBORATION: Federation institutions and many member states attest that the problem remains live, as economic and social disparities persist, requiring ongoing balancing. Some academic analyses and civil society groups also corroborate the need for a nuanced approach, though they may dispute the current calibration.
narrative_ontology:disappearance_verdict(federation_membership_treaty__subsidiarity_balance, world_rearranges).
narrative_ontology:founding_problem_status(federation_membership_treaty__subsidiarity_balance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(federation_membership_treaty__subsidiarity_balance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness (0.45) is moderate, reflecting the compromise: some mobility is constrained, but not entirely suppressed. Suppression (0.55) is also moderate, as both member states and individuals face enforcement to maintain the balance. Theater ratio (0.15) is low, indicating that the proportionality tests and judicial oversight are largely functional, not merely performative. The temporal measurements show a relatively stable, slightly fluctuating pattern, indicating an ongoing, dynamic equilibrium rather than a clear drift towards pure extraction or coordination.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of a member state with a strong welfare system, this constraint is a reasonable Rope, allowing for managed integration. From the perspective of an unskilled worker from a weaker economy, it can feel more like a Snare, with their mobility rights disproportionately curtailed. The engine's per-seat classification will capture these divergences based on the declared power, exit options, and beneficiary/victim roles.
 *
 * DIRECTIONALITY LOGIC:
 *   Member states with strong welfare systems and mobile skilled labor are beneficiaries, as they gain from the flexibility while retaining some control. Member states with weaker welfare systems and unskilled mobile labor are payers, bearing more of the costs and facing greater restrictions. Federation judicial and executive bodies act as agenda-setters, enforcing the balance. The graduated nature of the constraint means directionality is highly sensitive to the specific policy context and individual's skill level.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_test_objectivity,
    'Are the proportionality tests applied by federation judicial bodies genuinely objective and balanced, or do they systematically favor certain national interests or types of labor?',
    'Longitudinal empirical study of judicial decisions, disaggregated by member state economic strength, type of labor, and policy domain, to detect systematic bias.',
    'If tests are biased, the constraint''s effective extractiveness and suppression are higher for the disfavored parties than currently measured, potentially reclassifying it closer to a Snare for those seats. If objective, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_test_objectivity, empirical, 'Assesses the fairness and neutrality of the proportionality tests.').

omega_variable(
    legitimate_national_interest_definition,
    'Is the definition of ''legitimate national interest'' stable and consistently applied, or is it subject to political opportunism and expansion, eroding free movement over time?',
    'Content analysis of legislative debates, judicial opinions, and executive communications over time, tracking the scope and justification of ''national interest'' claims.',
    'If the definition expands opportunistically, the constraint''s suppression and extractiveness will increase over time, pushing it towards a Snare. If stable, the balance holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimate_national_interest_definition, conceptual, 'Examines the stability and scope of ''legitimate national interest'' as a limiting principle.').

omega_variable(
    subsidiarity_vs_sovereignty_framing,
    'Is the ''subsidiarity balance'' reading fundamentally distinct from the ''sovereignty_primary'' reading, or is it a more palatable framing for similar underlying nationalistic preferences?',
    'Comparative analysis of policy outcomes and judicial reasoning under both framings in different federations or historical periods. If outcomes converge despite different rhetoric, the distinction is conceptual theater.',
    'If it''s a re-framing of sovereignty, the constraint''s true extractiveness and suppression might be higher, as the ''balance'' narrative masks a power asymmetry. If genuinely distinct, it represents a unique coordination mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(subsidiarity_vs_sovereignty_framing, conceptual, 'Distinguishes the ''subsidiarity balance'' from a ''sovereignty primary'' framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(federation_membership_treaty__subsidiarity_balance, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fede_tr_t0, federation_membership_treaty__subsidiarity_balance, theater_ratio, 0, 0.1).
narrative_ontology:measurement(fede_tr_t10, federation_membership_treaty__subsidiarity_balance, theater_ratio, 10, 0.12).
narrative_ontology:measurement(fede_tr_t20, federation_membership_treaty__subsidiarity_balance, theater_ratio, 20, 0.15).
narrative_ontology:measurement(fede_tr_t30, federation_membership_treaty__subsidiarity_balance, theater_ratio, 30, 0.13).
narrative_ontology:measurement(fede_tr_t40, federation_membership_treaty__subsidiarity_balance, theater_ratio, 40, 0.16).
narrative_ontology:measurement(fede_tr_t50, federation_membership_treaty__subsidiarity_balance, theater_ratio, 50, 0.15).

% Extraction over time
narrative_ontology:measurement(fede_be_t0, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(fede_be_t10, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(fede_be_t20, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(fede_be_t30, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 30, 0.43).
narrative_ontology:measurement(fede_be_t40, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(fede_be_t50, federation_membership_treaty__subsidiarity_balance, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(fede_su_t0, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(fede_su_t10, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 10, 0.53).
narrative_ontology:measurement(fede_su_t20, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 20, 0.55).
narrative_ontology:measurement(fede_su_t30, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 30, 0.52).
narrative_ontology:measurement(fede_su_t40, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(fede_su_t50, federation_membership_treaty__subsidiarity_balance, suppression_requirement, 50, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(federation_membership_treaty__subsidiarity_balance, enforcement_mechanism).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__integration_primary).
narrative_ontology:affects_constraint(federation_membership_treaty__subsidiarity_balance, federation_membership_treaty__sovereignty_primary).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'federation_membership_treaty' kernel. This 'subsidiarity_balance' reading emphasizes a proportionate approach to free movement, distinct from the 'integration_primary' (pro-mobility) and 'sovereignty_primary' (pro-state control) readings. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
