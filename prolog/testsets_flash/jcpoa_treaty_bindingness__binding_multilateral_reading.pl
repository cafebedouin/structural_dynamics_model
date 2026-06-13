% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__binding_multilateral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__binding_multilateral_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: jcpoa_treaty_bindingness__binding_multilateral_reading
 *   human_readable: JCPOA as Binding Multilateral Treaty
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   This story describes the Joint Comprehensive Plan of Action (JCPOA) as a
 *   binding multilateral treaty, emphasizing its legal force and the
 *   requirement for consensus-based modification or dissolution. This reading
 *   highlights the high constraint on unilateral withdrawal and the role of
 *   the UN Security Council in sanctions reimposition. It is one reading of
 *   the 'jcpoa_treaty_bindingness' kernel, focusing on the formal, legalistic
 *   interpretation of the agreement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.3).
domain_priors:suppression_score(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.6).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__binding_multilateral_reading, rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__binding_multilateral_reading, "JCPOA as Binding Multilateral Treaty").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__binding_multilateral_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__binding_multilateral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, '4621e9a2-8c62-466b-bf3c-a2a606fc519b').
narrative_ontology:cs_kernel_codification('4621e9a2-8c62-466b-bf3c-a2a606fc519b', formalized).
narrative_ontology:cs_authority_grounding('4621e9a2-8c62-466b-bf3c-a2a606fc519b', lineage).
narrative_ontology:cs_interpretation_layer_present('4621e9a2-8c62-466b-bf3c-a2a606fc519b').
narrative_ontology:cs_reading_relation('4621e9a2-8c62-466b-bf3c-a2a606fc519b', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('4621e9a2-8c62-466b-bf3c-a2a606fc519b', jcpoa_treaty_bindingness__graduated_compliance_reading, influences).
narrative_ontology:cs_axiom('4621e9a2-8c62-466b-bf3c-a2a606fc519b', foundational, treaty_obligations_are_binding).
narrative_ontology:cs_axiom_status(treaty_obligations_are_binding, holdable).
narrative_ontology:cs_axiom_grounding('4621e9a2-8c62-466b-bf3c-a2a606fc519b', treaty_obligations_are_binding, deontological).
narrative_ontology:cs_axiom('4621e9a2-8c62-466b-bf3c-a2a606fc519b', foundational, unsc_consensus_is_required_for_snapback).
narrative_ontology:cs_axiom_status(unsc_consensus_is_required_for_snapback, holdable).
narrative_ontology:cs_axiom_grounding('4621e9a2-8c62-466b-bf3c-a2a606fc519b', unsc_consensus_is_required_for_snapback, conventional).
narrative_ontology:cs_reference_frame('4621e9a2-8c62-466b-bf3c-a2a606fc519b', vienna_convention_on_treaty_law).
narrative_ontology:cs_drift_state('4621e9a2-8c62-466b-bf3c-a2a606fc519b', post_us_withdrawal_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('4621e9a2-8c62-466b-bf3c-a2a606fc519b', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_regime).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_civilian_nuclear_program).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, p5_plus_1_states).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iran).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, pacta_sunt_servanda_doctrine).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateralism_as_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of international peace and security, responsible for enforcing the JCPOA's provisions and authorizing sanctions snapback. Requires consensus for major modifications or dissolution, reflecting the multilateral nature of the treaty.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% Commits to significant restrictions on its nuclear program in exchange for sanctions relief. Bears the cost of these restrictions and the scrutiny of international inspectors. Unilateral withdrawal is seen as a violation of international law.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iran, payer,
    powerful, generational, constrained, national).

% The original signatories (China, France, Germany, Russia, United Kingdom, United States) who benefit from Iran's nuclear program being constrained and the stability of the non-proliferation regime. Their actions are bound by the treaty's dispute resolution mechanisms.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, p5_plus_1_states, beneficiary,
    institutional, generational, constrained, global).

% The international atomic energy agency, responsible for verifying Iran's compliance with its nuclear commitments. Benefits from enhanced access and transparency, strengthening its mandate.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea, beneficiary,
    organized, generational, analytical, global).

% The overarching international framework to prevent the spread of nuclear weapons. Benefits from the JCPOA's success as a precedent for diplomatic resolution of proliferation concerns.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_regime, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_regime).

% Political factions or states that advocate for unilateral withdrawal from the treaty or immediate reimposition of sanctions without multilateral consensus. Their preferred actions are constrained by the treaty's binding nature.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, unilateral_withdrawal_advocates, excluded,
    powerful, immediate, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to prevent nuclear proliferation by establishing a verifiable framework for Iran's nuclear program, ensuring all parties adhere to a common set of rules and dispute resolution mechanisms.
% TRANSFER_FUNCTION: Transfers nuclear material and technology restrictions from Iran to the international community, in exchange for sanctions relief and the promise of a stable, verifiable non-proliferation framework.
% ABSENT_VOICES: States or factions advocating for unilateral withdrawal or immediate military action against Iran's nuclear program are excluded from the consensus-based decision-making process, as their positions contradict the treaty's multilateral and binding nature.
% DISAPPEARANCE_RATIONALE: If the JCPOA's binding nature vanished, it would lead to immediate unilateral withdrawals, reimposition of sanctions without international consensus, and a rapid escalation of Iran's nuclear program, fundamentally altering the international security landscape and non-proliferation efforts.
% FOUNDING_PROBLEM: The uncontrolled development of Iran's nuclear program, raising fears of nuclear weapons proliferation and regional instability, coupled with a lack of transparency and international verification.
% FOUNDING_PROBLEM_CORROBORATION: The IAEA continues to report on Iran's nuclear activities, and the P5+1 states consistently affirm the ongoing need for a verifiable framework. Independent non-proliferation experts and think tanks corroborate the persistent threat of proliferation without such a treaty.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__binding_multilateral_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__binding_multilateral_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__binding_multilateral_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).
:- end_tests(jcpoa_treaty_bindingness__binding_multilateral_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.3) as the treaty imposes significant restrictions on Iran but also provides benefits (sanctions relief, international legitimacy). Suppression is moderate (0.6) due to the active enforcement mechanisms and the diplomatic pressure to adhere. Theater ratio is low (0.1) as the treaty's functions are genuinely operational, not merely performative, from this binding multilateral perspective. Accessibility collapse is high (0.7) because, from this reading, there are few legitimate alternatives to the treaty's framework for managing Iran's nuclear program.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the UN Security Council and the non-proliferation regime, the JCPOA is a robust, binding Rope that effectively coordinates international security. From Iran's perspective, it is a constrained Rope, balancing significant restrictions with economic benefits. From the perspective of unilateral withdrawal advocates, it is a Snare that illegitimately constrains national sovereignty.
 *
 * DIRECTIONALITY LOGIC:
 *   The UN Security Council and the non-proliferation regime are primary beneficiaries (d near 0.0) as they gain stability and a framework for control. Iran is a payer (d near 1.0) due to the restrictions on its nuclear program. The P5+1 states are beneficiaries (d near 0.0) as they achieve their non-proliferation goals. The IAEA is a beneficiary (d near 0.0) through its enhanced verification role. Unilateral withdrawal advocates are excluded, as their actions are contrary to the treaty's binding nature.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unilateral_withdrawal_legitimacy,
    'Is unilateral withdrawal from the JCPOA, particularly by a signatory state, a legitimate exercise of national sovereignty or a violation of international law?',
    'International Court of Justice ruling on the legality of unilateral withdrawal from multilateral treaties under similar circumstances, or a UN General Assembly resolution condemning such actions.',
    'If deemed legitimate, the ''binding_multilateral_reading'' would be weakened, shifting towards a ''transactional_provisional_reading'' with higher extractiveness for those seeking to maintain the treaty. If deemed a violation, the binding nature is reinforced, increasing suppression for those contemplating withdrawal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_withdrawal_legitimacy, conceptual, 'Ambiguity regarding the legal legitimacy of unilateral withdrawal from the JCPOA.').

omega_variable(
    sanctions_snapback_mechanism,
    'Does the JCPOA''s ''snapback'' mechanism for sanctions reimposition truly require UNSC consensus, or can it be triggered unilaterally by a P5+1 state?',
    'A definitive legal interpretation by the UN''s legal counsel or a precedent-setting vote within the UNSC that clarifies the procedural requirements for snapback.',
    'If unilateral snapback is possible, the ''binding_multilateral_reading'' is less robust, increasing the effective extractiveness on Iran and shifting the constraint towards a ''transactional_provisional_reading''. If consensus is strictly required, the multilateral nature is reinforced, reducing unilateral leverage.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sanctions_snapback_mechanism, empirical, 'Ambiguity regarding the procedural requirements for sanctions snapback.').

omega_variable(
    non_proliferation_regime_stability,
    'To what extent does the JCPOA, under this binding multilateral reading, genuinely contribute to the stability of the non-proliferation regime, versus merely deferring proliferation challenges?',
    'Long-term empirical data on global proliferation trends, the adoption of similar multilateral agreements, and the sustained adherence of Iran to its commitments beyond the treaty''s sunset clauses.',
    'If the JCPOA is shown to significantly enhance regime stability, its ''rope'' classification is strengthened. If it''s seen as merely a temporary deferral, the ''rope'' classification might degrade towards a ''scaffold'' or even a ''piton'' if its long-term function atrophies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(non_proliferation_regime_stability, empirical, 'The long-term impact of the JCPOA on non-proliferation regime stability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__binding_multilateral_reading, 2015, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(jcpo_tr_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2017, 0.1).
narrative_ontology:measurement(jcpo_tr_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2019, 0.1).
narrative_ontology:measurement(jcpo_tr_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2021, 0.1).
narrative_ontology:measurement(jcpo_tr_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2023, 0.1).
narrative_ontology:measurement(jcpo_tr_t2024, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2015, 0.3).
narrative_ontology:measurement(jcpo_be_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2017, 0.3).
narrative_ontology:measurement(jcpo_be_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2019, 0.3).
narrative_ontology:measurement(jcpo_be_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2021, 0.3).
narrative_ontology:measurement(jcpo_be_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2023, 0.3).
narrative_ontology:measurement(jcpo_be_t2024, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2024, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t2015, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2015, 0.6).
narrative_ontology:measurement(jcpo_su_t2017, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2017, 0.6).
narrative_ontology:measurement(jcpo_su_t2019, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement(jcpo_su_t2021, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2021, 0.6).
narrative_ontology:measurement(jcpo_su_t2023, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2023, 0.6).
narrative_ontology:measurement(jcpo_su_t2024, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__binding_multilateral_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, iran_nuclear_program_transparency).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'jcpoa_treaty_bindingness' kernel. This 'binding_multilateral_reading' emphasizes the treaty's legal force and multilateral enforcement, contrasting with the 'transactional_provisional_reading' (unilateral voidability) and 'graduated_compliance_reading' (proportional reciprocity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
