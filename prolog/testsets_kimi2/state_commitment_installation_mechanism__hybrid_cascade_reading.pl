% ============================================================================
% CONSTRAINT STORY: state_commitment_installation_mechanism__hybrid_cascade_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_commitment_installation_mechanism__hybrid_cascade_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: state_commitment_installation_mechanism__hybrid_cascade_reading
 *   human_readable: Hybrid Cascade Commitment Installation
 *   domain: historical_sociology/state_formation/cultural_authority
 *
 * SUMMARY:
 *   This constraint story models the hybrid cascade reading of the state
 *   commitment installation kernel: new political and cultural commitments
 *   are initiated at the state apex but cannot stabilize without validation,
 *   translation, and local adaptation by institutional fringe actors. The
 *   arrangement coordinates territorial scale while asymmetrically extracting
 *   legitimacy labor and autonomy from peripheral actors. It is claimed as
 *   tangled rope because it simultaneously solves a genuine coordination
 *   problem (extending authority without constant renegotiation) and extracts
 *   from those who must adapt and legitimate the new order.
 *
 * KEY AGENTS:
 *   - State apex authority (agenda_setter/beneficiary; institutional; mobile exit) â initiates commitments and captures territorial legitimacy.
 *   - Administrative elite (beneficiary; organized; constrained exit) â implements and gains from state expansion.
 *   - Fringe actors (payer; moderate; constrained exit) â mediate, translate, and legitimate; bear autonomy costs.
 *   - Peripheral populations (payer; powerless; trapped exit) â subjected to new commitments with limited refusal capacity.
 *   - Rival power centers (excluded; organized; trapped exit) â alternative authorities marginalized by the cascade.
 *   - Historical sociologists (observer; analytical; analytical exit) â evaluate competing state-formation models.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.61).
domain_priors:suppression_score(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.52).
domain_priors:theater_ratio(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(state_commitment_installation_mechanism__hybrid_cascade_reading, resistance, 0.44).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_commitment_installation_mechanism__hybrid_cascade_reading, tangled_rope).
narrative_ontology:human_readable(state_commitment_installation_mechanism__hybrid_cascade_reading, "Hybrid Cascade Commitment Installation").
narrative_ontology:topic_domain(state_commitment_installation_mechanism__hybrid_cascade_reading, "historical_sociology/state_formation/cultural_authority").

domain_priors:requires_active_enforcement(state_commitment_installation_mechanism__hybrid_cascade_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_commitment_installation_mechanism__hybrid_cascade_reading, '2475bd61-74aa-4e5a-8e02-6092bec1e595').
narrative_ontology:cs_kernel_codification('2475bd61-74aa-4e5a-8e02-6092bec1e595', distributed).
narrative_ontology:cs_authority_grounding('2475bd61-74aa-4e5a-8e02-6092bec1e595', distributed).
narrative_ontology:cs_reading_relation('2475bd61-74aa-4e5a-8e02-6092bec1e595', state_commitment_installation_mechanism__endogenous_climb_reading, coexists_with).
narrative_ontology:cs_reading_relation('2475bd61-74aa-4e5a-8e02-6092bec1e595', state_commitment_installation_mechanism__exogenous_imposition_reading, coexists_with).
narrative_ontology:cs_axiom('2475bd61-74aa-4e5a-8e02-6092bec1e595', foundational, apex_initiation_requires_fringe_stabilization).
narrative_ontology:cs_axiom_status(apex_initiation_requires_fringe_stabilization, holdable).
narrative_ontology:cs_axiom_grounding('2475bd61-74aa-4e5a-8e02-6092bec1e595', apex_initiation_requires_fringe_stabilization, empirically_contingent).
narrative_ontology:cs_axiom('2475bd61-74aa-4e5a-8e02-6092bec1e595', foundational, local_interpretation_legitimates_scale).
narrative_ontology:cs_axiom_status(local_interpretation_legitimates_scale, holdable).
narrative_ontology:cs_axiom_grounding('2475bd61-74aa-4e5a-8e02-6092bec1e595', local_interpretation_legitimates_scale, empirically_contingent).
narrative_ontology:cs_reference_frame('2475bd61-74aa-4e5a-8e02-6092bec1e595', bilateral_legitimation_equilibrium).
narrative_ontology:cs_drift_state('2475bd61-74aa-4e5a-8e02-6092bec1e595', contemporary_historical_sociology, gap(stable, minor, true)).
narrative_ontology:cs_created_at('2475bd61-74aa-4e5a-8e02-6092bec1e595', '').
narrative_ontology:cs_kernel_id(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, state_apex_authority).
narrative_ontology:constraint_beneficiary(state_commitment_installation_mechanism__hybrid_cascade_reading, administrative_elite).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_actors).
narrative_ontology:constraint_victim(state_commitment_installation_mechanism__hybrid_cascade_reading, peripheral_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Initiates new political and cultural commitments from the center and depends on fringe actors to validate and stabilize them across heterogeneous territory. Sets formal law and ritual precedent but cannot achieve territorial consolidation without peripheral uptake. Retains capacity to shift to pure coercion or decentralization if the hybrid mechanism fails.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, state_apex_authority, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(state_commitment_installation_mechanism__hybrid_cascade_reading, state_apex_authority, beneficiary).

% Implements apex commitments within the bureaucratic apparatus, benefits from expanded state capacity, career advancement, and resource flows that follow successful territorial consolidation. Their social position is tied to the state project, making exit costly.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, administrative_elite, beneficiary,
    organized, generational, constrained, national).

% Local notables, religious leaders, or regional power holders who must adapt apex commitments to local conditions, translate idioms, and legitimate the new order to their own followings. Bear the labor and reputational costs of mediation, and risk loss of autonomy as the state penetrates their domains.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, fringe_actors, payer,
    moderate, biographical, constrained, regional).

% Subjects of newly installed commitments who experience partial resistance but face limited channels for refusal. Pay tribute, conscription, or cultural adaptation costs while receiving state-derived order and limited protection.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, peripheral_populations, payer,
    powerless, immediate, trapped, local).

% Alternative authority structures that would offer competing commitment frameworks. They are marginalized or absorbed by the cascade mechanism and are not admitted to the validation conversation that determines which commitments stabilize.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, rival_power_centers, excluded,
    organized, biographical, trapped, regional).

% Academic analysts who compare state-formation trajectories and debate whether commitment installation follows cascade, endogenous climb, or exogenous imposition patterns. They observe the mechanism without being coordinated or extracted by it.
narrative_ontology:constraint_stakeholder(state_commitment_installation_mechanism__hybrid_cascade_reading, historical_sociologists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(state_commitment_installation_mechanism__hybrid_cascade_reading, state_apex_authority).
narrative_ontology:fixing_cost_class(state_commitment_installation_mechanism__hybrid_cascade_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of how to extend coherent political and cultural commitments across heterogeneous territories without constant face-to-face renegotiation at every locality.
% TRANSFER_FUNCTION: Moves legitimacy, material resources, and compliance obligations from peripheral actors toward the state apex, mediated through fringe adaptation and local interpretation.
% ABSENT_VOICES: Rival power centers offering alternative commitment frameworks and unmediated peripheral communities whose resistance is absorbed rather than represented are structurally excluded from the validation conversation.
% DISAPPEARANCE_RATIONALE: If the hybrid cascade mechanism vanished, apex commitments would lack the fringe validation required for territorial stabilization. State formation would either fragment into purely local orders or require vastly more coercive top-down capacity, fundamentally rearranging the sociology of political legitimacy.
% FOUNDING_PROBLEM: How to scale political and cultural authority beyond face-to-face communities without the commitment dissolving at the edges of territorial control.
% FOUNDING_PROBLEM_CORROBORATION: Comparative historical sociologists and anthropologists of the state from non-apex seats corroborate the scale problem. Marxist historians and world-systems theorists contest whether the hybrid cascade is the operative solution, pointing instead to market imperatives or raw coercion.
narrative_ontology:disappearance_verdict(state_commitment_installation_mechanism__hybrid_cascade_reading, world_rearranges).
narrative_ontology:founding_problem_status(state_commitment_installation_mechanism__hybrid_cascade_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(state_commitment_installation_mechanism__hybrid_cascade_reading, 'none', 1).
narrative_ontology:epsilon_provenance(state_commitment_installation_mechanism__hybrid_cascade_reading, 0.61, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_commitment_installation_mechanism__hybrid_cascade_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_commitment_installation_mechanism__hybrid_cascade_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) reflects the asymmetric cost of fringe validation: peripheral actors perform labor that the apex cannot perform itself. Suppression (0.52) is moderate because partial resistance is absorbed via interpretation rather than crushed, but the mechanism still depends on excluding rival centers. Theater ratio (0.33) captures the performative dimension of apex proclamations that require subsequent fringe enactment to become real. Accessibility collapse (0.48) acknowledges that local alternatives are weakened but not fully eliminated. Resistance (0.44) registers the partial resistance noted in the scenario. The metrics and claim are independently authored: the claim is tangled rope; the metrics describe a moderately extractive, actively enforced arrangement.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat experiences the constraint as a necessary coordination mechanism for territorial consolidation, while the payer seats experience it as an extraction of local autonomy and labor. The engine should compute divergent per-seat classifications from this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   State apex authority sits near the beneficiary end: it initiates the constraint and collects territorial legitimacy and resource flows. Fringe actors and peripheral populations sit near the target end: they bear the costs of adaptation, validation, and compliance. Administrative elite sits near the beneficiary end but with more constrained exit. Rival power centers are excluded entirely, experiencing maximum directionality toward target.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâscaling authority beyond face-to-face communitiesâis arguably live, which prevents automatic piton classification. However, the mechanism's persistence depends on active enforcement (excluding rivals, requiring fringe labor) and shows moderate theater, distinguishing it from a pure scaffold. The divergence between live founding problem and moderate extraction is exactly what the tangled rope category captures: coordination that has not fully decayed into pure extraction but carries asymmetric costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    apex_fringe_asymmetry,
    'Does the fringe validation requirement represent a necessary coordination cost of territorial scaling, or an asymmetric extraction of legitimacy and labor from peripheral actors?',
    'Comparative historical analysis measuring the autonomy costs, resource flows, and bargaining power of fringe actors during commitment installation across multiple state-formation cases.',
    'If validation is a necessary cost, the extraction metric overstates asymmetric extraction and the constraint trends toward rope. If it is asymmetric extraction, the constraint remains tangled rope or trends toward snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(apex_fringe_asymmetry, empirical, 'Whether fringe validation is coordination cost or extraction.').

omega_variable(
    hybrid_reading_stability,
    'Is the hybrid cascade reading a stable synthesis of the kernel, or an unstable compromise that collapses to pure exogenous imposition or endogenous climb under empirical pressure?',
    'Accumulated case studies of state formation where apex-initiated commitments failed without fringe validation versus cases where they succeeded without it, testing the two-phase necessity claim.',
    'If the hybrid reading collapses under pressure, it is not a structurally distinct constraint but a transitional description, and the epsilon value should be reassigned to whichever simpler reading the evidence supports.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_reading_stability, conceptual, 'Stability of the hybrid reading against competing simplifications.').

omega_variable(
    resistance_absorption_mechanism,
    'Is resistance absorption through local interpretation a genuine coordination mechanism that preserves local meaning, or a form of structural suppression that disguises imposition?',
    'Examine whether local interpretation preserves meaningful alternative commitments or merely re-labels apex directives, using archival records of local deliberation during state-formation episodes.',
    'If the latter, suppression is higher than measured and the constraint trends toward snare. If the former, the coordination function is stronger and the rope component more significant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resistance_absorption_mechanism, conceptual, 'Local interpretation as coordination versus suppression.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_commitment_installation_mechanism__hybrid_cascade_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hybrid_cascade_tr_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(hybrid_cascade_tr_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(hybrid_cascade_tr_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement(hybrid_cascade_tr_t60, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 60, 0.35).
narrative_ontology:measurement(hybrid_cascade_tr_t80, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 80, 0.33).
narrative_ontology:measurement(hybrid_cascade_tr_t100, state_commitment_installation_mechanism__hybrid_cascade_reading, theater_ratio, 100, 0.34).

% Extraction over time
narrative_ontology:measurement(hybrid_cascade_be_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(hybrid_cascade_be_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 20, 0.49).
narrative_ontology:measurement(hybrid_cascade_be_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(hybrid_cascade_be_t60, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 60, 0.59).
narrative_ontology:measurement(hybrid_cascade_be_t80, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 80, 0.62).
narrative_ontology:measurement(hybrid_cascade_be_t100, state_commitment_installation_mechanism__hybrid_cascade_reading, base_extractiveness, 100, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(hybrid_cascade_su_t0, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(hybrid_cascade_su_t20, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 20, 0.4).
narrative_ontology:measurement(hybrid_cascade_su_t40, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 40, 0.47).
narrative_ontology:measurement(hybrid_cascade_su_t60, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 60, 0.52).
narrative_ontology:measurement(hybrid_cascade_su_t80, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 80, 0.53).
narrative_ontology:measurement(hybrid_cascade_su_t100, state_commitment_installation_mechanism__hybrid_cascade_reading, suppression_requirement, 100, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_commitment_installation_mechanism__hybrid_cascade_reading, identity_coordination).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__endogenous_climb_reading).
narrative_ontology:affects_constraint(state_commitment_installation_mechanism__hybrid_cascade_reading, state_commitment_installation_mechanism__exogenous_imposition_reading).

% DUAL FORMULATION NOTE:
% This constraint is one member of the state_commitment_installation_mechanism kernel family. The kernel decomposes into three structurally distinct readings (hybrid cascade, endogenous climb, exogenous imposition) because each assigns a different directional flow to legitimacy and a different epsilon profile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
