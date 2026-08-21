% ============================================================================
% CONSTRAINT STORY: legitimacy_of_imposed_practice__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimacy_of_imposed_practice__endogenous_climb_reading, []).

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
 *   constraint_id: legitimacy_of_imposed_practice__endogenous_climb_reading
 *   human_readable: Legitimacy of Imposed Practice (Endogenous Climb Reading)
 *   domain: political_history/state_formation/cultural_imposition
 *
 * SUMMARY:
 *   This constraint represents the 'endogenous climb' reading of the
 *   'legitimacy of imposed practice' kernel, which posits that state-imposed
 *   cultural or administrative practices fail to achieve genuine displacement
 *   without bottom-up adoption pathways. The story focuses on the state's
 *   persistent but ultimately ineffective efforts to enforce new norms (e.g.,
 *   calendar, dress codes) against the enduring resistance and private
 *   retention of traditional practices by local communities. The constraint
 *   is claimed as a Piton because its primary function (cultural
 *   displacement) has atrophied, yet it persists through inertial state
 *   enforcement and performative compliance, failing to extract genuine
 *   internalization.
 *
 * KEY AGENTS:
 *   - state_modernizers: Agenda-setter, victim (institutional/constrained) — attempts imposition, bears costs of failure
 *   - local_communities_preserving_autonomy: Beneficiary, payer (organized/identity_locked) — resists imposition, benefits from autonomy
 *   - cultural_elites_adopting_partially: Payer, beneficiary (moderate/constrained) — partial adoption for social mobility, private retention
 *   - historical_observers: Observer (analytical/analytical) — analyzes long-term outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.25).
domain_priors:suppression_score(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.7).
domain_priors:theater_ratio(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0.6).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(legitimacy_of_imposed_practice__endogenous_climb_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimacy_of_imposed_practice__endogenous_climb_reading, piton).
narrative_ontology:human_readable(legitimacy_of_imposed_practice__endogenous_climb_reading, "Legitimacy of Imposed Practice (Endogenous Climb Reading)").
narrative_ontology:topic_domain(legitimacy_of_imposed_practice__endogenous_climb_reading, "political_history/state_formation/cultural_imposition").

domain_priors:requires_active_enforcement(legitimacy_of_imposed_practice__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimacy_of_imposed_practice__endogenous_climb_reading, 'db401c58-8a6b-4a6f-b432-51de8ccebd07').
narrative_ontology:cs_kernel_codification('db401c58-8a6b-4a6f-b432-51de8ccebd07', formalized).
narrative_ontology:cs_authority_grounding('db401c58-8a6b-4a6f-b432-51de8ccebd07', extraction).
narrative_ontology:cs_interpretation_layer_present('db401c58-8a6b-4a6f-b432-51de8ccebd07').
narrative_ontology:cs_reading_relation('db401c58-8a6b-4a6f-b432-51de8ccebd07', legitimacy_of_imposed_practice__exogenous_override_reading, forecloses).
narrative_ontology:cs_reading_relation('db401c58-8a6b-4a6f-b432-51de8ccebd07', legitimacy_of_imposed_practice__hybrid_scaffolding_reading, coexists_with).
narrative_ontology:cs_axiom('db401c58-8a6b-4a6f-b432-51de8ccebd07', foundational, legitimacy_requires_internalization).
narrative_ontology:cs_axiom_status(legitimacy_requires_internalization, holdable).
narrative_ontology:cs_axiom_grounding('db401c58-8a6b-4a6f-b432-51de8ccebd07', legitimacy_requires_internalization, deontological).
narrative_ontology:cs_axiom('db401c58-8a6b-4a6f-b432-51de8ccebd07', secondary, imposed_change_is_brittle).
narrative_ontology:cs_axiom_status(imposed_change_is_brittle, holdable).
narrative_ontology:cs_axiom_grounding('db401c58-8a6b-4a6f-b432-51de8ccebd07', imposed_change_is_brittle, empirically_contingent).
narrative_ontology:cs_reference_frame('db401c58-8a6b-4a6f-b432-51de8ccebd07', bottom_up_legitimacy_framework).
narrative_ontology:cs_drift_state('db401c58-8a6b-4a6f-b432-51de8ccebd07', contemporary_postcolonial_critique, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('db401c58-8a6b-4a6f-b432-51de8ccebd07', '').
narrative_ontology:cs_kernel_id(legitimacy_of_imposed_practice__endogenous_climb_reading, legitimacy_of_imposed_practice).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, local_communities_preserving_autonomy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernization_timeline).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, state_legitimacy_claims).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legitimacy_of_imposed_practice__endogenous_climb_reading, cultural_elites_adopting_partially).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, local_communities_preserving_autonomy).
narrative_ontology:constraint_victim(legitimacy_of_imposed_practice__endogenous_climb_reading, cultural_elites_adopting_partially).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central authority that attempts to impose new practices (e.g., calendar, dress codes) to unify the nation and modernize. They expend significant resources on enforcement but see limited success, becoming victims of their own failed policy timeline and eroded legitimacy.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, state_modernizers, agenda_setter,
    institutional, generational, constrained, national).

% Groups whose traditional practices are targeted for displacement. They resist overt compliance, maintain private adherence to old ways, and benefit from the failure of the imposed practices to fully take hold, preserving their cultural autonomy. They pay through initial friction and performative compliance.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, local_communities_preserving_autonomy, beneficiary,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, local_communities_preserving_autonomy, payer).

% Urban or educated segments of the population who adopt some imposed practices for social mobility or to avoid state sanction, but often retain traditional practices in private. They pay in terms of cultural dissonance and partial loss of identity, but may benefit from integration into the state's system.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, cultural_elites_adopting_partially, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(legitimacy_of_imposed_practice__endogenous_climb_reading, cultural_elites_adopting_partially, beneficiary).

% Academics and analysts who study the long-term effects of state-imposed cultural change, documenting the persistence of traditional practices and the limits of top-down mandates. They provide an external perspective on the constraint's effectiveness.
narrative_ontology:constraint_stakeholder(legitimacy_of_imposed_practice__endogenous_climb_reading, historical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legitimacy_of_imposed_practice__endogenous_climb_reading, diffuse).
narrative_ontology:fixing_cost_class(legitimacy_of_imposed_practice__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The state attempts to coordinate a unified national identity and modern administrative practices by displacing diverse local customs with standardized, state-sanctioned ones.
% TRANSFER_FUNCTION: The state attempts to transfer cultural and social capital, as well as legitimacy, from traditional practices and local authorities to state-sanctioned norms and central authority. This reading highlights the failure of this transfer.
% ABSENT_VOICES: Traditional religious leaders, local cultural custodians, and advocates for cultural pluralism were often suppressed or ignored during the imposition process; they would articulate the value of indigenous practices and the harm of their displacement.
% DISAPPEARANCE_RATIONALE: If the imposed practices and their enforcement vanished, local communities would fully re-assert their traditional ways without fear of reprisal, and the state would lose a key, albeit ineffective, tool for national integration, forcing a re-evaluation of its legitimacy claims and modernization strategies.
% FOUNDING_PROBLEM: The state aimed to overcome internal fragmentation, perceived 'backwardness,' and lack of central control by establishing a uniform national culture and modern administrative order.
% FOUNDING_PROBLEM_CORROBORATION: State archives and official histories attest to the founding problem as live and the imposed practices as necessary. However, ethnographic studies, sociological analyses, and oral histories from local communities (outside the benefiting parties) corroborate that the original problem was either misdiagnosed or the solution failed, leading to persistent resistance and a contested status.
narrative_ontology:disappearance_verdict(legitimacy_of_imposed_practice__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(legitimacy_of_imposed_practice__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legitimacy_of_imposed_practice__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legitimacy_of_imposed_practice__endogenous_climb_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).
:- end_tests(legitimacy_of_imposed_practice__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The `extractiveness` is low (0.25) because the imposed practice largely fails to extract genuine compliance or legitimacy from the target communities; its intended function is not realized. `Suppression` is high (0.70) because the state actively attempts to enforce the new norms, but `resistance` is equally high (0.80) from the communities, indicating the ineffectiveness of this suppression. `Theater_ratio` is high (0.60) as much of the 'compliance' is performative or superficial, masking continued adherence to traditional ways. `Accessibility_collapse` is low (0.30) because alternatives (traditional practices) persist despite state efforts. The overall profile points to a Piton: a constraint maintained by inertia and theatricality, whose core function has atrophied due to lack of internalization.
 *
 * PERSPECTIVAL GAP:
 *   The state modernizers perceive the constraint as a legitimate, albeit slow, process of national integration and modernization, viewing resistance as 'backwardness.' Local communities, however, experience it as an illegitimate imposition that threatens their cultural identity. The engine's classification as a Piton reflects the objective failure of the constraint's intended function, regardless of the state's self-justifying narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   State modernizers, despite being the agenda-setters, are victims of this constraint's failure, as their modernization timeline is derailed and their legitimacy claims are undermined. Local communities, by successfully resisting full internalization, become beneficiaries of the constraint's ineffectiveness, preserving their autonomy. Cultural elites are payers through partial adoption but also beneficiaries of social mobility. The directionality reflects the actual flows of costs and benefits, not the state's intended outcomes.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was to displace prior practices and establish new ones. This reading argues that this mandate has largely atrophied because the imposed practices failed to achieve bottom-up adoption. The persistence of the constraint is due to state inertia and the prohibitive cost for the state to admit failure, rather than its functional effectiveness. This prevents mislabeling the constraint as a functioning Rope or Tangled Rope, which would imply successful coordination or extraction, respectively.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    internalization_depth_measurement,
    'To what extent is the observed compliance merely performative versus genuine, unacknowledged internalization over generations?',
    'Longitudinal ethnographic studies tracking private practices and intergenerational transmission of norms, or linguistic analysis of cultural shifts over extended periods.',
    'If significant unacknowledged internalization is occurring, the constraint''s effective extractiveness might be higher than currently measured, and its classification could shift towards a slow-acting Tangled Rope or even a Rope over very long timescales. If purely performative, the Piton classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_depth_measurement, empirical, 'Distinguishing superficial compliance from deep cultural shift.').

omega_variable(
    state_capacity_vs_resistance,
    'Is the failure of displacement primarily due to the inherent strength of community resistance, or to limitations in the state''s coercive and administrative capacity?',
    'Comparative historical analysis of similar imposition attempts by states with varying levels of coercive capacity and administrative reach.',
    'If state capacity is the primary limiting factor, the constraint''s suppression metric might be lower than it could be, and a stronger state could potentially achieve higher extraction. If community resistance is the dominant factor, the constraint''s inherent extractiveness is structurally limited regardless of state power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_vs_resistance, empirical, 'Attributing failure to state weakness or community strength.').

omega_variable(
    kernel_reading_contest,
    'Is the ''endogenous climb'' reading the most accurate interpretation of the historical record, or do the ''exogenous override'' or ''hybrid scaffolding'' readings offer a better account of the constraint''s operation?',
    'Further historical and sociological research, particularly comparative studies across different contexts of state-led cultural change, evaluating the predictive power and explanatory scope of each reading.',
    'If a sibling reading were adopted, the constraint''s metrics (especially extractiveness and suppression) and claimed type would likely shift significantly. For example, the ''exogenous override'' reading would likely yield a higher extractiveness and suppression, potentially classifying the constraint as a Snare or Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Contest between different interpretations of how imposed practices gain legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimacy_of_imposed_practice__endogenous_climb_reading, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(legi_tr_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(legi_tr_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 20, 0.5).
narrative_ontology:measurement(legi_tr_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 30, 0.55).
narrative_ontology:measurement(legi_tr_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 40, 0.6).
narrative_ontology:measurement(legi_tr_t50, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 50, 0.62).
narrative_ontology:measurement(legi_tr_t60, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 60, 0.6).
narrative_ontology:measurement(legi_tr_t70, legitimacy_of_imposed_practice__endogenous_climb_reading, theater_ratio, 70, 0.6).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(legi_be_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 10, 0.3).
narrative_ontology:measurement(legi_be_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(legi_be_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 30, 0.26).
narrative_ontology:measurement(legi_be_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 40, 0.25).
narrative_ontology:measurement(legi_be_t50, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 50, 0.24).
narrative_ontology:measurement(legi_be_t60, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 60, 0.24).
narrative_ontology:measurement(legi_be_t70, legitimacy_of_imposed_practice__endogenous_climb_reading, base_extractiveness, 70, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(legi_su_t10, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(legi_su_t20, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(legi_su_t30, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(legi_su_t40, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(legi_su_t50, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 50, 0.68).
narrative_ontology:measurement(legi_su_t60, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement(legi_su_t70, legitimacy_of_imposed_practice__endogenous_climb_reading, suppression_requirement, 70, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
