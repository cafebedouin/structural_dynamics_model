% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__interpretive_authority_structure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__interpretive_authority_structure, []).

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
 *   constraint_id: unsc_242_withdrawal_clause__interpretive_authority_structure
 *   human_readable: UNSC Resolution 242 Withdrawal Clause: Interpretive Authority Structure
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint describes the meta-dispute over the legitimate authority
 *   to interpret the withdrawal clause of UNSC Resolution 242. The resolution
 *   calls for 'withdrawal of Israeli armed forces from territories occupied
 *   in the recent conflict,' but the absence of a definite article before
 *   'territories' in the English text (present in the French) created an
 *   ambiguity. This constraint focuses on the structural snare created by the
 *   contested interpretive authority itself: the International Court of
 *   Justice claims judicial interpretation, drafting states claim authorial
 *   intent, and the occupying state claims customary practice. This
 *   meta-dispute prevents definitive legal closure, allowing powerful actors
 *   to benefit from the ambiguity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.85).
domain_priors:suppression_score(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.75).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, extractiveness, 0.85).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__interpretive_authority_structure, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__interpretive_authority_structure, snare).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__interpretive_authority_structure, "UNSC Resolution 242 Withdrawal Clause: Interpretive Authority Structure").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__interpretive_authority_structure, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__interpretive_authority_structure).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__interpretive_authority_structure, '6db04d99-fccb-4f7c-a0a2-14ea57506644').
narrative_ontology:cs_kernel_codification('6db04d99-fccb-4f7c-a0a2-14ea57506644', fixed_text).
narrative_ontology:cs_authority_grounding('6db04d99-fccb-4f7c-a0a2-14ea57506644', distributed).
narrative_ontology:cs_reading_relation('6db04d99-fccb-4f7c-a0a2-14ea57506644', unsc_242_withdrawal_clause__maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('6db04d99-fccb-4f7c-a0a2-14ea57506644', unsc_242_withdrawal_clause__partial_withdrawal_reading, coexists_with).
narrative_ontology:cs_axiom('6db04d99-fccb-4f7c-a0a2-14ea57506644', foundational, interpretive_authority_is_contested).
narrative_ontology:cs_axiom_status(interpretive_authority_is_contested, holdable).
narrative_ontology:cs_axiom_grounding('6db04d99-fccb-4f7c-a0a2-14ea57506644', interpretive_authority_is_contested, conventional).
narrative_ontology:cs_axiom('6db04d99-fccb-4f7c-a0a2-14ea57506644', secondary, ambiguity_serves_political_ends).
narrative_ontology:cs_axiom_status(ambiguity_serves_political_ends, holdable).
narrative_ontology:cs_axiom_grounding('6db04d99-fccb-4f7c-a0a2-14ea57506644', ambiguity_serves_political_ends, empirically_contingent).
narrative_ontology:cs_reference_frame('6db04d99-fccb-4f7c-a0a2-14ea57506644', unresolved_interpretive_pluralism).
narrative_ontology:cs_drift_state('6db04d99-fccb-4f7c-a0a2-14ea57506644', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6db04d99-fccb-4f7c-a0a2-14ea57506644', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__interpretive_authority_structure, states_with_veto_power).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, international_court_of_justice).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__interpretive_authority_structure, states_seeking_legal_closure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the interpretive ambiguity, allowing it to maintain control over occupied territories by claiming customary practice as the legitimate interpretive authority. Its non-cooperation capacity prevents definitive resolution.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, occupying_state, beneficiary,
    powerful, generational, identity_locked, regional).

% Claims judicial interpretation as the legitimate authority but is unable to enforce a definitive ruling due to the political nature of the dispute and the non-compliance of powerful states. Its authority is undermined by the ongoing contest.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, international_court_of_justice, payer,
    institutional, civilizational, constrained, global).

% Claim authorial intent as the legitimate interpretive authority, but their collective will is fragmented and lacks the enforcement power to impose a single reading. Their efforts to clarify the resolution are frustrated by the meta-dispute.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, drafting_states, payer,
    organized, generational, constrained, global).

% Exercise their Security Council veto power to prevent resolutions that would definitively settle the interpretive dispute, thereby preserving the ambiguity that benefits their geopolitical interests or allies. They are the ultimate arbiters of the meta-dispute.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, states_with_veto_power, agenda_setter,
    institutional, generational, arbitrage, global).

% Are victims of the perpetual interpretive ambiguity, as it prevents the establishment of clear legal boundaries and perpetuates conflict. They lack the power to force a resolution and are trapped by the ongoing meta-dispute.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, states_seeking_legal_closure, payer,
    moderate, biographical, trapped, global).

% Analyze the various claims to interpretive authority and the implications of the ongoing dispute for international law. They can highlight the structural nature of the snare but lack direct power to resolve it.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__interpretive_authority_structure, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint nominally coordinates the interpretation of a critical UN Security Council resolution, aiming to provide a framework for peace in the Middle East.
% TRANSFER_FUNCTION: Transfers the power to define the scope of withdrawal from a clear, universally accepted legal interpretation to a contested, politically driven process, benefiting those who can leverage ambiguity.
% ABSENT_VOICES: The populations of occupied territories, whose future is directly determined by the interpretation of the withdrawal clause, are largely absent from the high-level interpretive authority dispute. They would advocate for a clear, maximal withdrawal interpretation.
% DISAPPEARANCE_RATIONALE: If the interpretive authority structure vanished, a definitive interpretation of UNSC 242 would likely emerge, either through judicial consensus or a new, unambiguous resolution. This would fundamentally alter the legal and political landscape of the Israeli-Palestinian conflict, forcing a resolution to the territorial dispute.
% FOUNDING_PROBLEM: The original problem was to establish a framework for peace in the Middle East following the 1967 Six-Day War, requiring withdrawal from occupied territories in exchange for secure and recognized boundaries.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live, as attested by ongoing international diplomatic efforts, UN resolutions, and the continued conflict in the region. International legal scholars and human rights organizations corroborate that the core issues of peace and secure boundaries are unresolved due to the interpretive deadlock.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__interpretive_authority_structure, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__interpretive_authority_structure, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__interpretive_authority_structure, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__interpretive_authority_structure, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__interpretive_authority_structure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__interpretive_authority_structure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because the interpretive ambiguity allows the occupying state to maintain control over territories without clear legal accountability, effectively extracting sovereignty and resources. Suppression is high (0.75) because the veto power of certain states and the non-cooperation of the occupying state actively suppress any definitive, binding interpretation. The theater ratio is moderate (0.40) as diplomatic efforts and legal arguments continue, creating the appearance of progress while the core interpretive deadlock persists. The claimed type is 'snare' because the coordination story (resolving the conflict through legal interpretation) is cover for the extraction enabled by the unresolved authority dispute.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the occupying state and its allies, the interpretive authority structure is a legitimate contest of legal principles, allowing for a flexible, pragmatic approach to a complex geopolitical issue. From the perspective of the ICJ and states seeking legal closure, it is a structural failure of international law, enabling ongoing occupation and undermining the rule of law.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying state and states with veto power are beneficiaries, as the interpretive ambiguity allows them to maintain their positions or protect allies without legal consequence. The ICJ, drafting states, and states seeking legal closure are victims, as their efforts to achieve a definitive, legally binding interpretation are frustrated by the contested authority. The meta-dispute itself is the mechanism of extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_interpretive_claims,
    'Which claim to interpretive authority (judicial, authorial intent, customary practice) holds the greatest legitimacy in international law, independent of political power?',
    'A definitive advisory opinion from a universally recognized, politically unconstrained international legal body, or a new, unambiguous UN Security Council resolution that explicitly defines the interpretive hierarchy.',
    'If one claim is definitively established as superior, the interpretive ambiguity would collapse, forcing a resolution to the withdrawal clause and reclassifying the constraint away from a snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_of_interpretive_claims, conceptual, 'The underlying legitimacy of competing interpretive authority claims.').

omega_variable(
    political_will_for_resolution,
    'To what extent is the persistence of the interpretive authority dispute driven by genuine legal disagreement versus the political will of powerful states to maintain ambiguity?',
    'Analysis of diplomatic archives and internal state communications to identify instances where legal arguments were strategically deployed to mask political objectives, or a shift in geopolitical alignments that removes the incentive for ambiguity.',
    'If primarily political, the constraint is a pure snare, with legal arguments serving as theater. If genuine legal disagreement, the constraint might be a tangled rope, with a real coordination problem exacerbated by legal complexity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_will_for_resolution, empirical, 'The balance between legal and political drivers of interpretive ambiguity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__interpretive_authority_structure, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1967, 0.2).
narrative_ontology:measurement(unsc_tr_t1980, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1980, 0.25).
narrative_ontology:measurement(unsc_tr_t1995, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(unsc_tr_t2010, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(unsc_tr_t2024, unsc_242_withdrawal_clause__interpretive_authority_structure, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(unsc_be_t1980, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(unsc_be_t1995, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 1995, 0.8).
narrative_ontology:measurement(unsc_be_t2010, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2010, 0.83).
narrative_ontology:measurement(unsc_be_t2024, unsc_242_withdrawal_clause__interpretive_authority_structure, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1967, 0.6).
narrative_ontology:measurement(unsc_su_t1980, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(unsc_su_t1995, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(unsc_su_t2010, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2010, 0.73).
narrative_ontology:measurement(unsc_su_t2024, unsc_242_withdrawal_clause__interpretive_authority_structure, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__interpretive_authority_structure, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__interpretive_authority_structure, unsc_242_withdrawal_clause__partial_withdrawal_reading).

% DUAL FORMULATION NOTE:
% This constraint describes the meta-dispute over interpretive authority for UNSC Resolution 242's withdrawal clause, which structurally influences the viability and contestation of the 'maximal_withdrawal_reading' and 'partial_withdrawal_reading' sibling constraints. The ambiguity of authority prevents either substantive reading from achieving definitive legal status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
