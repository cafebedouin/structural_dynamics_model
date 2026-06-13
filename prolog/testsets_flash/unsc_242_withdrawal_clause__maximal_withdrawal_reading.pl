% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__maximal_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__maximal_withdrawal_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   human_readable: UNSC 242 Withdrawal Clause: Maximal Withdrawal Reading
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'maximal withdrawal' reading of UN
 *   Security Council Resolution 242, which mandates withdrawal from 'all'
 *   occupied territories, emphasizing the French definite article 'des' (the)
 *   and the UN Charter's principle of territorial integrity (Article 2(4)).
 *   This reading views the resolution as a binding legal obligation for full
 *   retrocession, making the occupying state a clear target and dispossessed
 *   claimants the beneficiaries. The constraint is claimed as a Rope,
 *   reflecting its intended function as a coordination mechanism for peace,
 *   but its high extractiveness and suppression reflect the ongoing
 *   resistance to its full implementation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.85).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.75).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC 242 Withdrawal Clause: Maximal Withdrawal Reading").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__maximal_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '0cd4e97a-b634-4eb7-9442-1328e890ee57').
narrative_ontology:cs_kernel_codification('0cd4e97a-b634-4eb7-9442-1328e890ee57', fixed_text).
narrative_ontology:cs_authority_grounding('0cd4e97a-b634-4eb7-9442-1328e890ee57', lineage).
narrative_ontology:cs_interpretation_layer_present('0cd4e97a-b634-4eb7-9442-1328e890ee57').
narrative_ontology:cs_reading_relation('0cd4e97a-b634-4eb7-9442-1328e890ee57', unsc_242_withdrawal_clause__partial_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('0cd4e97a-b634-4eb7-9442-1328e890ee57', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('0cd4e97a-b634-4eb7-9442-1328e890ee57', foundational, territorial_integrity_absolute).
narrative_ontology:cs_axiom_status(territorial_integrity_absolute, holdable).
narrative_ontology:cs_axiom_grounding('0cd4e97a-b634-4eb7-9442-1328e890ee57', territorial_integrity_absolute, deontological).
narrative_ontology:cs_axiom('0cd4e97a-b634-4eb7-9442-1328e890ee57', foundational, french_text_controls_interpretation).
narrative_ontology:cs_axiom_status(french_text_controls_interpretation, holdable).
narrative_ontology:cs_axiom_grounding('0cd4e97a-b634-4eb7-9442-1328e890ee57', french_text_controls_interpretation, conventional).
narrative_ontology:cs_reference_frame('0cd4e97a-b634-4eb7-9442-1328e890ee57', un_charter_territorial_integrity_default).
narrative_ontology:cs_drift_state('0cd4e97a-b634-4eb7-9442-1328e890ee57', contemporary_diplomatic_practice, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('0cd4e97a-b634-4eb7-9442-1328e890ee57', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_claimants).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_law_regime).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Parties whose territories were occupied and who seek full retrocession based on the maximal withdrawal interpretation. Their legal position is strong under this reading, but their practical power to enforce it is limited.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_claimants, beneficiary,
    powerless, generational, trapped, regional).

% The state that occupied territories in the 1967 conflict. Under this reading, it is legally obligated to withdraw from all territories, incurring significant strategic and political costs. It actively resists this interpretation.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state, payer,
    institutional, generational, constrained, regional).

% The body that passed Resolution 242. Under this reading, its mandate is to enforce full withdrawal. Its power is to issue resolutions and authorize enforcement, but its actions are subject to veto power.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, un_security_council, agenda_setter,
    institutional, civilizational, constrained, global).

% The broader system of international law, particularly the principle of territorial integrity (UN Charter Article 2(4)). This reading reinforces the foundational norm against acquisition of territory by force.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_law_regime, beneficiary,
    institutional, civilizational, analytical, universal).

% The International Court of Justice, which could be called upon to issue an advisory opinion or binding judgment on the interpretation of Resolution 242. Its authority is contested by some states.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, icj, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international adherence to the principle of territorial integrity, preventing the acquisition of territory by force and providing a clear legal basis for the return of occupied lands.
% TRANSFER_FUNCTION: Transfers legal claim and sovereignty over occupied territories from the occupying state back to the dispossessed claimants, and imposes the cost of withdrawal on the occupying state.
% ABSENT_VOICES: States that benefit from or advocate for the 'partial withdrawal' reading, or those that prioritize 'secure boundaries' over territorial integrity, are effectively sidelined in this maximalist interpretation. Their arguments for discretionary withdrawal are excluded from this reading's framework.
% DISAPPEARANCE_RATIONALE: If this maximal withdrawal interpretation vanished, the legal basis for demanding full retrocession would be severely weakened, potentially legitimizing territorial gains by force and fundamentally altering the international legal order regarding sovereignty and conflict resolution.
% FOUNDING_PROBLEM: The problem of territorial acquisition by force following the 1967 Arab-Israeli War, and the need to establish a framework for peace based on the inadmissibility of such acquisition.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live as territories remain occupied and peace agreements are incomplete. International legal scholars, the UN General Assembly, and numerous non-occupying states consistently corroborate the ongoing relevance of the territorial integrity principle and the need for withdrawal.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__maximal_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__maximal_withdrawal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because this reading imposes a comprehensive and non-negotiable obligation on the occupying state, demanding full retrocession without territorial adjustments. Suppression is also high (0.75) due to the active political and diplomatic efforts required to counter alternative interpretations and maintain the legal force of this reading against resistance from the occupying state. Theater ratio is low (0.1) as the legal claim is direct and the enforcement, though often stalled, is not primarily performative; it's a genuine contest over a binding obligation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the dispossessed claimants and the international legal regime, this is a foundational Rope, coordinating adherence to a core principle of international law. From the occupying state's perspective, it is a highly extractive Snare, imposing an unacceptable burden and denying its security concerns. The engine's per-seat classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'dispossessed_claimants' and the 'international_law_regime' are clear beneficiaries (d near 0.0) as this reading directly supports their legal and normative positions. The 'occupying_state' is the primary target (d near 1.0) as it bears the full cost of withdrawal. The 'UN_Security_Council' acts as an agenda-setter, attempting to enforce the constraint, but its effectiveness is modulated by geopolitical realities.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_resolution,
    'Is the French definite article ''des'' (the) in Resolution 242 truly controlling for the scope of withdrawal, or does the English indefinite article ''from territories'' allow for partial withdrawal?',
    'A definitive ruling by the International Court of Justice on the authoritative text and its interpretation, or a new, unambiguous Security Council resolution.',
    'If the French text is definitively controlling, this maximal withdrawal reading is strengthened, increasing the extractiveness on the occupying state. If the English text is deemed equally valid and ambiguous, the ''partial withdrawal'' reading gains legitimacy, reducing the extractiveness of this reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_ambiguity_resolution, conceptual, 'Ambiguity in the French vs. English text of Resolution 242 regarding withdrawal scope.').

omega_variable(
    territorial_integrity_vs_secure_boundaries,
    'Which principle of international law takes precedence in this context: the inadmissibility of acquiring territory by force (territorial integrity) or the right of states to secure and recognized boundaries?',
    'A global consensus shift in international legal doctrine or a binding international arbitration that explicitly prioritizes one principle over the other in cases of conflict.',
    'If territorial integrity is universally prioritized, this reading''s legal foundation is unassailable. If secure boundaries are given equal or greater weight, the maximal withdrawal demand becomes negotiable, potentially weakening this constraint''s force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(territorial_integrity_vs_secure_boundaries, preference, 'Contest between territorial integrity and secure boundaries principles.').

omega_variable(
    maximal_vs_partial_reading_legitimacy,
    'Is this maximal withdrawal reading a genuine interpretation of UNSC 242, or is it a politically motivated framing designed to maximize claims?',
    'Historical analysis of drafting records, statements by original drafters, and consistent state practice over time. Corroboration from neutral international legal bodies.',
    'If proven to be a politically motivated maximalist framing, its legitimacy as a ''Rope'' would be undermined, pushing it towards a ''Snare'' or ''Tangled Rope'' from the perspective of the occupying state. If its genuine legal basis is reaffirmed, its ''Rope'' classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maximal_vs_partial_reading_legitimacy, empirical, 'Whether the maximal withdrawal reading is a genuine interpretation or a political framing.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(unsc_tr_t1980, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(unsc_tr_t1995, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(unsc_tr_t2010, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(unsc_tr_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement(unsc_be_t1980, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1980, 0.82).
narrative_ontology:measurement(unsc_be_t1995, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1995, 0.85).
narrative_ontology:measurement(unsc_be_t2010, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2010, 0.84).
narrative_ontology:measurement(unsc_be_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1967, 0.7).
narrative_ontology:measurement(unsc_su_t1980, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1980, 0.72).
narrative_ontology:measurement(unsc_su_t1995, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1995, 0.75).
narrative_ontology:measurement(unsc_su_t2010, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(unsc_su_t2024, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__maximal_withdrawal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__partial_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the UNSC Resolution 242 withdrawal clause. It is part of a constraint family that includes the 'partial withdrawal' reading and the 'interpretive authority structure' constraint, which governs who resolves the textual ambiguity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
