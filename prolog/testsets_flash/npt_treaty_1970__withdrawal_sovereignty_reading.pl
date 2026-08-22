% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__withdrawal_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__withdrawal_sovereignty_reading, []).

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
 *   constraint_id: npt_treaty_1970__withdrawal_sovereignty_reading
 *   human_readable: NPT Article X Withdrawal as Sovereign Right
 *   domain: international_law/nuclear_nonproliferation
 *
 * SUMMARY:
 *   This constraint story analyzes the NPT's Article X withdrawal right from
 *   the perspective that emphasizes state sovereignty and the contingency of
 *   treaty obligations on the security environment. This reading views the
 *   withdrawal clause not as a regrettable loophole, but as a legitimate and
 *   essential component of the treaty that allows states to protect their
 *   national interests. The constraint is claimed as a Tangled Rope because
 *   it offers a coordination function (allowing states to join with an exit
 *   option) but also enables asymmetric extraction by allowing some states to
 *   leverage the threat of withdrawal, undermining the collective
 *   nonproliferation norm.
 *
 * KEY AGENTS:
 *   - threshold_nuclear_states: Primary beneficiary (powerful/mobile) — gains strategic flexibility.
 *   - non_nuclear_weapon_states_with_security_concerns: Secondary beneficiary (moderate/constrained) — gains theoretical security option.
 *   - nonproliferation_regime_stability: Primary victim (institutional/trapped) — suffers erosion of its foundational norm.
 *   - nuclear_weapon_states_seeking_status_quo: Secondary victim (institutional/constrained) — bears costs of maintaining a challenged regime.
 *   - international_legal_scholars: Analytical observer (analytical/analytical) — studies the legal and practical implications.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.65).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.4).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal as Sovereign Right").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international_law/nuclear_nonproliferation").

domain_priors:requires_active_enforcement(npt_treaty_1970__withdrawal_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, 'a40abb7d-ba73-427f-9f43-cfe1718100a3').
narrative_ontology:cs_kernel_codification('a40abb7d-ba73-427f-9f43-cfe1718100a3', fixed_text).
narrative_ontology:cs_authority_grounding('a40abb7d-ba73-427f-9f43-cfe1718100a3', lineage).
narrative_ontology:cs_interpretation_layer_present('a40abb7d-ba73-427f-9f43-cfe1718100a3').
narrative_ontology:cs_reading_relation('a40abb7d-ba73-427f-9f43-cfe1718100a3', npt_treaty_1970__oligopoly_enforcement_reading, influences).
narrative_ontology:cs_reading_relation('a40abb7d-ba73-427f-9f43-cfe1718100a3', npt_treaty_1970__reciprocal_disarmament_reading, influences).
narrative_ontology:cs_axiom('a40abb7d-ba73-427f-9f43-cfe1718100a3', foundational, sovereign_right_to_withdraw).
narrative_ontology:cs_axiom_status(sovereign_right_to_withdraw, holdable).
narrative_ontology:cs_axiom_grounding('a40abb7d-ba73-427f-9f43-cfe1718100a3', sovereign_right_to_withdraw, conventional).
narrative_ontology:cs_axiom('a40abb7d-ba73-427f-9f43-cfe1718100a3', foundational, treaty_obligations_contingent_on_security).
narrative_ontology:cs_axiom_status(treaty_obligations_contingent_on_security, holdable).
narrative_ontology:cs_axiom_grounding('a40abb7d-ba73-427f-9f43-cfe1718100a3', treaty_obligations_contingent_on_security, instrumental).
narrative_ontology:cs_reference_frame('a40abb7d-ba73-427f-9f43-cfe1718100a3', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('a40abb7d-ba73-427f-9f43-cfe1718100a3', contemporary_security_environment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('a40abb7d-ba73-427f-9f43-cfe1718100a3', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_nuclear_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states_with_security_concerns).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, nonproliferation_regime_stability).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states_seeking_status_quo).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states benefit from the explicit recognition of their right to withdraw from the NPT, allowing them to maintain nuclear options as a sovereign prerogative, especially when their security environment deteriorates. The withdrawal clause provides strategic flexibility and leverage.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_nuclear_states, beneficiary,
    powerful, generational, mobile, global).

% For NNWS facing regional threats, the withdrawal right offers a theoretical ultimate security guarantee, even if exercising it carries high costs. It provides a legal basis to consider nuclearization if conventional deterrence fails, making their NPT adherence conditional on their security environment.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states_with_security_concerns, beneficiary,
    moderate, biographical, constrained, regional).

% The stability and perceived universality of the nonproliferation regime are undermined by the emphasis on withdrawal as a legitimate sovereign act. Each withdrawal or credible threat of withdrawal erodes the norm of nonproliferation, making the regime's foundational principles more fragile.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, nonproliferation_regime_stability, payer,
    institutional, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(npt_treaty_1970__withdrawal_sovereignty_reading, nonproliferation_regime_stability).

% These states, particularly the P5, bear the cost of a weakened nonproliferation norm. While they benefit from the NPT's horizontal nonproliferation, the emphasis on withdrawal makes their efforts to maintain the nuclear status quo more difficult and costly, requiring constant diplomatic and coercive efforts to prevent exits.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states_seeking_status_quo, payer,
    institutional, generational, constrained, global).

% Analyze the legal implications of Article X, debating the balance between sovereign rights and treaty obligations. They observe how states interpret and utilize the withdrawal clause in practice, and its impact on international law.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for states to manage their security interests within the NPT, allowing for a sovereign exit under extraordinary circumstances, thereby making initial adherence more palatable for some states.
% TRANSFER_FUNCTION: Transfers the burden of maintaining nonproliferation from a universally binding norm to a contingent obligation, placing the risk of proliferation back onto the international system when states perceive their security to be at stake.
% ABSENT_VOICES: Future generations and populations in regions vulnerable to nuclear proliferation would object to the emphasis on withdrawal, as it prioritizes short-term state security calculations over long-term global stability and safety. Their voices are not directly represented in state-centric treaty negotiations.
% DISAPPEARANCE_RATIONALE: If the NPT's withdrawal clause (and the interpretation emphasizing sovereign right) vanished, states would likely be more hesitant to join such treaties, or would seek other means to preserve their security options, leading to a different, potentially more fragmented, international security architecture.
% FOUNDING_PROBLEM: The NPT was designed to prevent the spread of nuclear weapons while promoting peaceful nuclear energy and eventual disarmament, balancing the interests of nuclear and non-nuclear states.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear weapon states and many non-nuclear states attest that the core problem of proliferation remains live. However, states emphasizing the withdrawal right argue that the NPT's original bargain (disarmament for non-proliferation) has not been met, making the 'founding problem' of an imbalanced treaty a live concern for them. Independent security analysts corroborate the ongoing tension between sovereign security and collective nonproliferation goals.
narrative_ontology:disappearance_verdict(npt_treaty_1970__withdrawal_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__withdrawal_sovereignty_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_treaty_1970__withdrawal_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is high because this reading allows states to externalize their security costs onto the collective nonproliferation regime, effectively extracting option value from the treaty's flexibility. Suppression (0.4) is moderate; while there are diplomatic pressures against withdrawal, the legal right is clear. Theater ratio (0.2) is low, as the withdrawal right is a functional, not performative, aspect of the treaty. The increasing extractiveness over time reflects the growing emphasis on national security prerogatives in a more multipolar world.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states prioritizing sovereign security, Article X is a necessary safeguard, making the NPT a viable coordination mechanism. From the perspective of those prioritizing regime stability, it's a structural weakness that enables extraction. The engine's per-seat classification will reflect this divergence, with beneficiaries seeing a Rope-like function and victims experiencing a Snare-like erosion of norms.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold states and NNWS with security concerns are beneficiaries (low d) as they gain strategic flexibility. The nonproliferation regime itself and NWS seeking the status quo are victims (high d) as they bear the costs of a less stable, more conditional regime. The explicit right to withdraw, even if rarely exercised, shifts the balance of power and obligation.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the withdrawal right as a mere 'loophole' or 'failure' of the NPT. Instead, it frames it as an integral, albeit contested, part of the original bargain, reflecting a tension between state sovereignty and collective security that was present at the treaty's founding. The constraint's function has not atrophied; rather, its interpretation has evolved to emphasize a particular aspect of its original design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sovereignty_vs_regime_priority,
    'Is the NPT primarily a mechanism for collective security, or a framework that defers to state sovereignty in extreme security circumstances?',
    'Analysis of state practice and international legal jurisprudence regarding treaty interpretation in cases of national security exigency. Examination of whether the ''supreme interests'' clause in Article X is self-judging or subject to international review.',
    'If collective security is prioritized, the extractiveness of emphasizing withdrawal would be higher, classifying it closer to a Snare. If sovereignty is paramount, the coordination function would be emphasized, making it closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_vs_regime_priority, conceptual, 'Ambiguity in the foundational priority between state sovereignty and international regime stability.').

omega_variable(
    withdrawal_threat_credibility,
    'How credible are the threats of NPT withdrawal by threshold states, and what is their actual impact on the nonproliferation regime?',
    'Empirical analysis of state signaling, military capabilities, and diplomatic responses to withdrawal threats. Case studies of states that have withdrawn or credibly threatened to do so.',
    'If threats are largely performative or lack credibility, the actual extractiveness (leverage gained) would be lower, and the theater_ratio higher. If threats are highly credible and frequently used as leverage, extractiveness would be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_threat_credibility, empirical, 'Uncertainty regarding the real-world impact and credibility of NPT withdrawal threats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__withdrawal_sovereignty_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1985, 0.35).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2010, 0.39).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__reciprocal_disarmament_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the NPT Treaty of 1970. This reading emphasizes Article X withdrawal as a sovereign right, which structurally influences the other readings by challenging the perceived binding nature of their obligations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
