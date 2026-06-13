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
 *   constraint_id: npt_treaty_1970__withdrawal_sovereignty_reading
 *   human_readable: NPT Article X Withdrawal as Sovereign Right
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This constraint interprets Article X of the Nuclear Non-Proliferation
 *   Treaty (NPT) as a legitimate exercise of state sovereignty, allowing
 *   states to withdraw from treaty obligations if their supreme interests are
 *   jeopardized by extraordinary events. This reading emphasizes the
 *   contingent nature of treaty commitments, particularly for
 *   non-nuclear-weapon states facing evolving security threats. While it
 *   provides an 'exit valve' for states, it simultaneously introduces
 *   instability into the NPT regime by making compliance conditional and
 *   undermining the norm of regime stability.
 *
 * KEY AGENTS:
 *   - threshold_states: Primary beneficiary (powerful/constrained) — gains leverage from withdrawal option
 *   - non_nuclear_weapon_states_with_security_concerns: Primary beneficiary (organized/constrained) — maintains flexibility in security policy
 *   - npt_regime_stability_norm: Primary victim (institutional/trapped) — undermined by credible withdrawal threats
 *   - nuclear_weapon_states_seeking_status_quo: Primary victim (institutional/constrained) — faces challenges to the non-proliferation order
 *   - international_atomic_energy_agency: Agenda setter (institutional/analytical) — monitors compliance but cannot prevent withdrawal
 *   - international_community: Observer (global/analytical) — reacts to withdrawal events, shaping diplomatic consequences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.45).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.3).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal as Sovereign Right").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__withdrawal_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, '564920b0-0a36-4435-baf8-05a974d3ebf0').
narrative_ontology:cs_kernel_codification('564920b0-0a36-4435-baf8-05a974d3ebf0', fixed_text).
narrative_ontology:cs_authority_grounding('564920b0-0a36-4435-baf8-05a974d3ebf0', lineage).
narrative_ontology:cs_interpretation_layer_present('564920b0-0a36-4435-baf8-05a974d3ebf0').
narrative_ontology:cs_reading_relation('564920b0-0a36-4435-baf8-05a974d3ebf0', npt_treaty_1970__oligopoly_enforcement_reading, influences).
narrative_ontology:cs_reading_relation('564920b0-0a36-4435-baf8-05a974d3ebf0', npt_treaty_1970__reciprocal_disarmament_reading, influences).
narrative_ontology:cs_axiom('564920b0-0a36-4435-baf8-05a974d3ebf0', foundational, state_sovereignty_is_supreme).
narrative_ontology:cs_axiom_status(state_sovereignty_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('564920b0-0a36-4435-baf8-05a974d3ebf0', state_sovereignty_is_supreme, deontological).
narrative_ontology:cs_axiom('564920b0-0a36-4435-baf8-05a974d3ebf0', foundational, treaty_obligations_are_contingent_on_security).
narrative_ontology:cs_axiom_status(treaty_obligations_are_contingent_on_security, holdable).
narrative_ontology:cs_axiom_grounding('564920b0-0a36-4435-baf8-05a974d3ebf0', treaty_obligations_are_contingent_on_security, empirically_contingent).
narrative_ontology:cs_reference_frame('564920b0-0a36-4435-baf8-05a974d3ebf0', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('564920b0-0a36-4435-baf8-05a974d3ebf0', contemporary_nonproliferation_challenges, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('564920b0-0a36-4435-baf8-05a974d3ebf0', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states_with_security_concerns).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, npt_regime_stability_norm).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states_seeking_status_quo).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(npt_treaty_1970__withdrawal_sovereignty_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).
:- end_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) is moderate because while it benefits some states by providing an exit option, it extracts from the collective good of non-proliferation regime stability. Suppression (0.30) is low as the right to withdraw is explicitly stated, but there's diplomatic pressure against it. Theater ratio (0.10) is low; the withdrawal right is a real, functional mechanism, not merely performative. Resistance (0.55) is moderate, as states committed to the regime actively resist interpretations that weaken it, while others assert the right.
 *
 * PERSPECTIVAL GAP:
 *   Threshold states and non-nuclear-weapon states with security concerns perceive this as a necessary safeguard of sovereignty, a 'rope' that allows them to coordinate their security with the NPT's non-proliferation goals. Nuclear-weapon states and the NPT regime stability norm, however, experience it as a 'snare' or 'tangled rope' that introduces instability and undermines the treaty's core purpose, requiring active enforcement (diplomatic pressure, sanctions) to contain its impact.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold states and non-nuclear-weapon states with security concerns are beneficiaries (d near 0.0) as the withdrawal option provides them leverage and flexibility. The NPT regime stability norm and nuclear-weapon states seeking the status quo are victims (d near 1.0) as their interests are directly undermined by the credible threat of withdrawal. The IAEA is an agenda-setter, tasked with monitoring but structurally unable to prevent withdrawal, placing its d near 0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the withdrawal right as pure extraction by acknowledging its function as a sovereign safeguard for states facing security dilemmas. However, it also highlights how this 'safeguard' can become a mechanism for extracting concessions or undermining the regime, thus classifying it as a tangled rope rather than a pure rope. The founding problem of balancing sovereign security with non-proliferation remains contested, preventing a clear mandatrophy resolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    npt_kernel_reading_identification,
    'Is this constraint a genuine reflection of sovereign rights within the NPT, or a strategic interpretation to justify potential proliferation?',
    'Analysis of state practice and legal arguments during NPT review conferences and withdrawal events, focusing on the stated justifications and international community''s response.',
    'If a genuine reflection, it highlights a structural tension within the NPT. If a strategic interpretation, it underscores the extractive potential of leveraging sovereign rights against collective security norms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(npt_kernel_reading_identification, conceptual, 'This constraint is one reading of the NPT_treaty_1970 kernel, specifically the withdrawal_sovereignty_reading. Sibling readings (oligopoly_enforcement_reading, reciprocal_disarmament_reading) would emphasize different aspects of the treaty, leading to different classifications and stakeholder dynamics. This reading prioritizes state sovereignty and the right to withdraw, making treaty obligations contingent on the security environment, which undermines the regime stability norm.').

omega_variable(
    withdrawal_trigger_legitimacy,
    'What constitutes ''extraordinary events, related to the subject matter of this Treaty, have jeopardized the supreme interests of its country'' (Article X)?',
    'International Court of Justice advisory opinion or a consensus among NPT state parties on objective criteria for ''supreme interests'' and ''jeopardized''.',
    'Clearer criteria would reduce the arbitrary exercise of the withdrawal right, strengthening the regime. Ambiguity allows states to define their own ''supreme interests,'' increasing the perceived extractiveness for those committed to regime stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_trigger_legitimacy, preference, 'Ambiguity in Article X''s trigger conditions allows for broad interpretation, impacting the perceived legitimacy of withdrawal threats.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__withdrawal_sovereignty_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(npt__tr_t10, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 10, 0.12).
narrative_ontology:measurement(npt__tr_t20, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement(npt__tr_t30, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 30, 0.1).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(npt__be_t10, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(npt__be_t20, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(npt__be_t30, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 30, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(npt__su_t10, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 10, 0.25).
narrative_ontology:measurement(npt__su_t20, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(npt__su_t30, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 30, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__reciprocal_disarmament_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, iran_nuclear_deal_jcpoa).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, north_korea_denuclearization_talks).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the NPT_treaty_1970 kernel, focusing on the Article X withdrawal right. Its operation directly influences the stability and perceived legitimacy of the other NPT readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
