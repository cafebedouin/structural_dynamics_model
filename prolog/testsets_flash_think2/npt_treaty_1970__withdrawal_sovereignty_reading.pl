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
 *   human_readable: NPT Article X Withdrawal Right (Sovereignty Reading)
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This constraint story analyzes the NPT's Article X withdrawal right
 *   through the lens of state sovereignty, where treaty obligations are seen
 *   as contingent on a state's evolving security environment. This reading
 *   emphasizes the legitimacy of withdrawal as a sovereign act, rather than
 *   viewing it primarily as a breach of regime stability. The NPT itself is
 *   classified as a Tangled Rope due to its inherent coordination function
 *   (non-proliferation) and asymmetric extraction (NWS retain weapons, NNWS
 *   do not). This specific reading highlights a mechanism within that Tangled
 *   Rope that allows certain states to mitigate the extraction by asserting
 *   their sovereign right to exit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.62).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.7).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal Right (Sovereignty Reading)").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__withdrawal_sovereignty_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, '40c50236-f011-46a8-b72d-e98ed11b732d').
narrative_ontology:cs_kernel_codification('40c50236-f011-46a8-b72d-e98ed11b732d', fixed_text).
narrative_ontology:cs_authority_grounding('40c50236-f011-46a8-b72d-e98ed11b732d', lineage).
narrative_ontology:cs_interpretation_layer_present('40c50236-f011-46a8-b72d-e98ed11b732d').
narrative_ontology:cs_reading_relation('40c50236-f011-46a8-b72d-e98ed11b732d', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('40c50236-f011-46a8-b72d-e98ed11b732d', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_axiom('40c50236-f011-46a8-b72d-e98ed11b732d', foundational, state_sovereignty_is_supreme).
narrative_ontology:cs_axiom_status(state_sovereignty_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('40c50236-f011-46a8-b72d-e98ed11b732d', state_sovereignty_is_supreme, deontological).
narrative_ontology:cs_axiom('40c50236-f011-46a8-b72d-e98ed11b732d', foundational, treaty_obligations_are_contingent_on_security).
narrative_ontology:cs_axiom_status(treaty_obligations_are_contingent_on_security, holdable).
narrative_ontology:cs_axiom_grounding('40c50236-f011-46a8-b72d-e98ed11b732d', treaty_obligations_are_contingent_on_security, conventional).
narrative_ontology:cs_reference_frame('40c50236-f011-46a8-b72d-e98ed11b732d', westphalian_sovereignty_framework).
narrative_ontology:cs_drift_state('40c50236-f011-46a8-b72d-e98ed11b732d', contemporary_security_environment, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('40c50236-f011-46a8-b72d-e98ed11b732d', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, states_asserting_sovereignty).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, international_nonproliferation_regime_stability).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states_relying_on_assurances).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the NPT and its withdrawal procedures. They interpret Article X narrowly, emphasizing the need for 'extraordinary events' and 'supreme interests' to justify withdrawal, aiming to preserve regime stability.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, npt_depository_states, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the non-proliferation norm that limits new nuclear powers. While they have Article VI disarmament obligations, this reading of Article X allows them to frame their own security concerns as paramount, potentially justifying slower disarmament progress.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, nuclear_weapon_states, beneficiary,
    powerful, generational, arbitrage, global).

% Abide by non-proliferation commitments, foregoing nuclear weapons in exchange for security assurances and peaceful nuclear technology. This reading of Article X introduces uncertainty into the security assurances, as other states might withdraw, potentially increasing their own security vulnerability.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states, payer,
    moderate, biographical, constrained, global).

% Possess advanced nuclear capabilities but remain non-nuclear-weapon states under the NPT. This reading of Article X provides them with a legitimate option to withdraw if their security environment deteriorates, giving them significant leverage in international relations.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states, beneficiary,
    moderate, biographical, mobile, regional).

% Any state that views its treaty obligations as fundamentally contingent on its sovereign right to self-preservation. This reading empowers them to prioritize national security over regime stability, making withdrawal a viable policy tool.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, states_asserting_sovereignty, beneficiary,
    moderate, immediate, mobile, national).

% Analyze the legal implications of Article X, debating the scope of sovereign withdrawal rights versus the integrity of treaty law. They document state practice and interpret the treaty's text and negotiating history.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% The collective framework of norms, rules, and institutions designed to prevent nuclear proliferation. This reading, by emphasizing the ease and legitimacy of withdrawal, directly undermines the regime's long-term stability and predictability.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, international_nonproliferation_regime_stability, excluded,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(npt_treaty_1970__withdrawal_sovereignty_reading, international_nonproliferation_regime_stability).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a global norm against nuclear proliferation, providing a framework for peaceful nuclear energy cooperation and security assurances for non-nuclear-weapon states.
% TRANSFER_FUNCTION: Transfers the right to possess nuclear weapons from non-nuclear-weapon states to the five recognized nuclear-weapon states, in exchange for security assurances and a commitment to disarmament (Article VI).
% ABSENT_VOICES: States that have withdrawn from the NPT (e.g., North Korea) or never joined (e.g., India, Pakistan, Israel), who would argue that the treaty is discriminatory or that their security interests necessitate nuclear capabilities, making withdrawal a legitimate sovereign act.
% DISAPPEARANCE_RATIONALE: The NPT is a cornerstone of international security. Its disappearance would likely lead to widespread nuclear proliferation, regional arms races, and a breakdown of global security architecture, fundamentally reorganizing international relations.
% FOUNDING_PROBLEM: The existential threat of nuclear war and the desire to prevent further proliferation after World War II, while allowing for the peaceful use of nuclear energy.
% FOUNDING_PROBLEM_CORROBORATION: UN Security Council resolutions, IAEA reports, and independent academic analyses consistently highlight ongoing proliferation risks and the NPT's role in mitigating them, corroborating the continued relevance of the founding problem from outside the benefiting parties.
narrative_ontology:disappearance_verdict(npt_treaty_1970__withdrawal_sovereignty_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_1970__withdrawal_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(npt_treaty_1970__withdrawal_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 0.62, 'gemini-2.5-flash', 'none', direct).

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
 *   Base extractiveness (0.62) is moderately high, reflecting the ongoing burden of non-proliferation on NNWS, even with the withdrawal option. Suppression (0.70) is also high, as the regime actively discourages proliferation, but the withdrawal right provides a legitimate, albeit costly, escape valve. Theater ratio (0.25) is relatively low, as the NPT remains a functional treaty, though diplomatic efforts around withdrawal can sometimes be performative. Accessibility collapse (0.75) is high for direct proliferation, but lower for the legitimate act of withdrawal. Resistance (0.50) is moderate, reflecting the tension between states asserting sovereign rights and those prioritizing regime stability.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of a state considering withdrawal, Article X is a vital safeguard of sovereignty, making the NPT a more flexible and thus less extractive constraint. From the perspective of states prioritizing regime stability, this reading introduces a dangerous loophole that undermines the treaty's core purpose, making the NPT effectively more fragile and less reliable as a security guarantee.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold states and any state asserting its sovereign right to withdraw are beneficiaries, as this reading empowers them with a credible exit option, reducing their effective extraction from the non-proliferation obligation. The international non-proliferation regime's stability and non-nuclear-weapon states relying on security assurances are victims, as the emphasis on withdrawal introduces uncertainty and weakens the collective security framework. NPT depository states and nuclear-weapon states occupy complex positions, benefiting from the overall regime but also facing challenges to its stability from this interpretation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    withdrawal_legitimacy_ambiguity,
    'Is the invocation of Article X a legitimate exercise of state sovereignty, or does it fundamentally undermine the NPT''s non-proliferation objectives and international treaty law?',
    'Analysis of international legal precedent, state practice, and the ''extraordinary events'' clause in Article X. Resolution would depend on whether a consensus emerges on the threshold for legitimate withdrawal.',
    'If deemed a fully legitimate and low-cost sovereign right, the NPT''s effective extractiveness for potential withdrawers is lower, but the regime''s stability is more fragile. If deemed a high-cost, exceptional act, the NPT''s binding force is stronger, but states may feel more ''trapped''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_legitimacy_ambiguity, conceptual, 'Ambiguity regarding the legal and normative legitimacy of NPT withdrawal.').

omega_variable(
    regime_stability_impact_of_withdrawal_threats,
    'How does the increasing salience and invocation of Article X withdrawal rights affect the long-term stability, compliance incentives, and overall effectiveness of the international non-proliferation regime?',
    'Empirical study of state behavior, proliferation trends, and diplomatic responses following withdrawal threats or actual withdrawals. This would involve analyzing changes in compliance, security assurances, and the willingness of states to join or remain in the NPT.',
    'If withdrawal threats consistently lead to regime weakening and increased proliferation, the NPT''s effective coordination function is degraded, potentially shifting its classification towards a Snare for NNWS. If the regime proves resilient, the Tangled Rope classification holds, with withdrawal as a managed pressure valve.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regime_stability_impact_of_withdrawal_threats, empirical, 'Empirical impact of withdrawal threats on NPT regime stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__withdrawal_sovereignty_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1980, 0.18).
narrative_ontology:measurement(npt__tr_t1990, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2010, 0.24).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2024, 0.25).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1980, 0.57).
narrative_ontology:measurement(npt__be_t1990, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1990, 0.59).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2010, 0.61).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2024, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1970, 0.6).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1980, 0.63).
narrative_ontology:measurement(npt__su_t1990, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 1990, 0.66).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970__reciprocal_disarmament_reading).

% DUAL FORMULATION NOTE:
% This story is one of three readings of the NPT treaty kernel, focusing on Article X withdrawal rights. It is linked to the 'oligopoly_enforcement_reading' and 'reciprocal_disarmament_reading' as part of a constraint family, each representing a distinct interpretation of the NPT's structural dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
