% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__endogenous_climb_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__endogenous_climb_reading, []).

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
 *   constraint_id: state_commitment_installation_mechanism__endogenous_climb_reading
 *   human_readable: Endogenous Climb Mechanism for State Commitment Legitimacy
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint instantiates the endogenous_climb_reading of the
 *   state_commitment_installation_mechanism kernel, which holds that new
 *   commitments gain legitimacy by climbing from institutional fringes
 *   through demonstrated superiority. The kernel is contested by an
 *   exogenous_imposition_reading (legitimacy via top-down mandate) and a
 *   hybrid_cascade_reading (apex installation with fringe validation). In
 *   this reading, fringe advocates and grassroots coalitions are the
 *   beneficiaries of a meritocratic pathway that extracts gatekeeping
 *   authority from apex institutions, which resist the erosion of their
 *   monopoly on legitimacy. The constraint is claimed as tangled_rope to
 *   capture both the genuine coordination function (solving institutional
 *   ossification) and the asymmetric extraction (authority transfer from apex
 *   to fringe).
 *
 * KEY AGENTS:
 *   - fringe_advocates: Primary beneficiary (moderate/constrained) â gain legitimacy through demonstration
 *   - grassroots_advocates: Secondary beneficiary (organized/constrained) â provide visible advocacy
 *   - apex_institutions: Primary payer/target (institutional/constrained) â lose gatekeeping authority
 *   - comparative_historical_scholar: Analytical observer (analytical/analytical) â documents the mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, 0.62).
domain_priors:suppression_score(state_commitment_installation_mechanism__endogenous_climb_reading, 0.58).
domain_priors:theater_ratio(state_commitment_installation_mechanism__endogenous_climb_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__endogenous_climb_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__endogenous_climb_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__endogenous_climb_reading, "Endogenous Climb Mechanism for State Commitment Legitimacy").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__endogenous_climb_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__endogenous_climb_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__endogenous_climb_reading, 'a0300791-570b-400b-b8cf-bebcca2e9c94').
narrative_ontology:cs_kernel_codification('a0300791-570b-400b-b8cf-bebcca2e9c94', implicit).
narrative_ontology:cs_authority_grounding('a0300791-570b-400b-b8cf-bebcca2e9c94', practice).
narrative_ontology:cs_interpretation_layer_present('a0300791-570b-400b-b8cf-bebcca2e9c94').
narrative_ontology:cs_reading_relation('a0300791-570b-400b-b8cf-bebcca2e9c94', state_commitment_installation_mechanism__exogenous_imposition_reading, forecloses).
narrative_ontology:cs_reading_relation('a0300791-570b-400b-b8cf-bebcca2e9c94', state_commitment_installation_mechanism__hybrid_cascade_reading, coexists_with).
narrative_ontology:cs_axiom('a0300791-570b-400b-b8cf-bebcca2e9c94', foundational, legitimacy_from_demonstrated_superiority).
narrative_ontology:cs_axiom_status(legitimacy_from_demonstrated_superiority, holdable).
narrative_ontology:cs_axiom_grounding('a0300791-570b-400b-b8cf-bebcca2e9c94', legitimacy_from_demonstrated_superiority, empirically_contingent).
narrative_ontology:cs_axiom('a0300791-570b-400b-b8cf-bebcca2e9c94', foundational, fringe_origin_authority).
narrative_ontology:cs_axiom_status(fringe_origin_authority, holdable).
narrative_ontology:cs_axiom_grounding('a0300791-570b-400b-b8cf-bebcca2e9c94', fringe_origin_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('a0300791-570b-400b-b8cf-bebcca2e9c94', fringe_driven_legitimation).
narrative_ontology:cs_drift_state('a0300791-570b-400b-b8cf-bebcca2e9c94', contemporary_historical_sociology, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a0300791-570b-400b-b8cf-bebcca2e9c94', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_advocates).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__endogenous_climb_reading, grassroots_advocates).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__endogenous_climb_reading, apex_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Promote novel political and cultural commitments from outside established centers of power, accumulating evidence, converts, and demonstrative successes to force gradual recognition by apex institutions.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_advocates, beneficiary,
    moderate, biographical, constrained, national).

% Provide visible advocacy, material support, and mobilization for fringe commitments, creating social pressure that helps demonstrate superiority and forces apex attention.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, grassroots_advocates, beneficiary,
    organized, biographical, constrained, regional).

% Control the current legitimacy-granting apparatus and resist encroachments on their gatekeeping authority, but must eventually accommodate or co-opt commitments that have successfully climbed from the fringe through demonstrated superiority.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, apex_institutions, payer,
    institutional, generational, constrained, national).

% Documents and theorizes the climb mechanism, distinguishing endogenous legitimacy accrual from exogenous imposition and hybrid cascade models in comparative state formation.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__endogenous_climb_reading, comparative_historical_scholar, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__endogenous_climb_reading, fringe_advocates).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__endogenous_climb_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a pathway for novel commitments to gain recognition without existing apex sponsorship, solving the problem of institutional ossification by allowing functional superiority to translate into cultural authority.
% TRANSFER_FUNCTION: Moves legitimacy and gatekeeping authority from apex institutions to fringe actors and their commitments as demonstrations of superiority accumulate and force recognition.
% ABSENT_VOICES: Apex institutional loyalists who would defend top-down imposition as the sole legitimate source of authority; they are present in the historical record but treated as obstacles rather than interlocutors in this reading. Also, fringe actors whose innovations failed despite functional superiority because they lacked grassroots support or access to demonstration venues.
% DISAPPEARANCE_RATIONALE: If the endogenous climb mechanism vanished, fringe innovations would lose their primary pathway to legitimacy and remain permanently marginalized; apex institutions would retain monopoly gatekeeping authority; the historical sociology of state formation would require entirely different explanatory frameworks.
% FOUNDING_PROBLEM: How do new cultural and political commitments gain legitimacy in established institutional orders that naturally resist novelty and favor incumbents?
% FOUNDING_PROBLEM_CORROBORATION: Fringe advocates and grassroots movements attest the problem is live. Apex institutional historians and exogenous-imposition scholars attest that the problem is ill-posed because legitimacy primarily flows from mandate, not demonstration. Comparative historical sociologists outside the benefiting parties provide mixed corroboration depending on their theoretical commitments.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__endogenous_climb_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__endogenous_climb_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__endogenous_climb_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__endogenous_climb_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__endogenous_climb_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__endogenous_climb_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62) is moderate-high because the mechanism systematically transfers authority from apex to fringe. Suppression (0.58) reflects the progressive closing-off of apex gatekeeping alternatives as demonstrations accumulate. Theater ratio (0.25) is low because grassroots advocacy is substantively functional rather than performative. Accessibility collapse (0.45) indicates that alternatives (pure imposition) remain partially viable. Resistance (0.70) is high because apex institutions actively defend their gatekeeping role. The temporal series show gradual intensification as fringe climbs mature and consolidate.
 *
 * PERSPECTIVAL GAP:
 *   From the fringe seat, the constraint is a meritocratic rope that solves the coordination problem of institutional ossification. From the apex seat, it is an extractive mechanism that erodes legitimate authority without mandate. The engine computes this divergence from the same structural data; the claim does not adjudicate between the seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Fringe advocates and grassroots advocates are beneficiaries (low d, subsidized by the mechanism's authority transfer). Apex institutions are payers (high d, extraction target). The comparative historical scholar occupies an analytical seat with neutral d. No overrides are needed because the structural derivation from beneficiary/victim declarations plus exit options (constrained for all seated actors) correctly captures the asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling the constraint as a pure rope (which would ignore the authority extraction from apex institutions) or as a pure snare (which would deny the genuine coordination function of opening institutional pathways for superior but marginalized innovations). The R5 genealogy identifies the founding problem as institutional ossification; the status is contested because apex institutions dispute that ossification is a problem requiring this particular solution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'Is this constraint one reading of the state_commitment_installation_mechanism kernel, and how would sibling readings change the beneficiary/victim structure?',
    'Comparison with exogenous_imposition_reading and hybrid_cascade_reading constraint files; if the same historical cases classify differently under different readings, the kernel is genuinely contested.',
    'If exogenous imposition proves more descriptively accurate for most cases, this reading''s fringe-beneficiary structure collapses and the constraint reclassifies toward extraction or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Position of this constraint within the contested kernel').

omega_variable(
    demonstrated_superiority_verifiability,
    'Can ''demonstrated superiority'' be assessed independently of the institutional climb itself, or does the climb retrospectively construct the appearance of superiority?',
    'Historical case studies where fringe commitments succeeded without demonstrable functional superiority, or where superior fringe commitments were permanently blocked by apex institutions.',
    'If superiority is constructed post-hoc, the coordination function is cover for power redistribution; if independently verifiable, the tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(demonstrated_superiority_verifiability, empirical, 'Whether demonstrated superiority is independently verifiable or post-hoc constructed').

omega_variable(
    apex_resistance_authenticity,
    'Is apex resistance to fringe climbs genuine institutional defense of gatekeeping, or performative resistance that masks co-optation?',
    'Trace resource flows: if apex institutions eventually capture and domesticate fringe innovations while maintaining public resistance, the resistance is theatrical; if they genuinely lose authority, resistance is authentic.',
    'Theatrical resistance raises theater_ratio and suggests the constraint may function as a piton or snare rather than a tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(apex_resistance_authenticity, empirical, 'Whether apex resistance is genuine or performative').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__endogenous_climb_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(stat_tr_t10, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(stat_tr_t20, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(stat_tr_t30, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(stat_tr_t40, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement(stat_tr_t50, state_commitment_installation_mechanism__endogenous_climb_reading, theater_ratio, 50, 0.25).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(stat_be_t10, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(stat_be_t20, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(stat_be_t30, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(stat_be_t40, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(stat_be_t50, state_commitment_installation_mechanism__endogenous_climb_reading, base_extractiveness, 50, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(stat_su_t10, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(stat_su_t20, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(stat_su_t30, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 30, 0.53).
narrative_ontology:measurement(stat_su_t40, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 40, 0.56).
narrative_ontology:measurement(stat_su_t50, state_commitment_installation_mechanism__endogenous_climb_reading, suppression_requirement, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__endogenous_climb_reading, state_commitment_installation_mechanism__hybrid_cascade_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the state_commitment_installation_mechanism kernel, decomposed per the epsilon-invariance principle because each reading produces a different beneficiary/victim structure and extractiveness profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
