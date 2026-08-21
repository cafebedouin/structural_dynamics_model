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
 *   constraint_id: jcpoa_treaty_bindingness__binding_multilateral_reading
 *   human_readable: JCPOA as Binding Multilateral Treaty (Binding Multilateral Reading)
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   This constraint story instantiates the 'binding multilateral treaty'
 *   reading of the JCPOA. Under this reading, the agreement is a robust,
 *   legally binding multilateral instrument that requires consensus-based
 *   modification or dissolution. Unilateral withdrawal or sanctions
 *   reimposition is considered a violation of international law, and Iranian
 *   enrichment violations trigger a structured, multilateral dispute
 *   resolution process before any 'snapback' of sanctions. The beneficiaries
 *   are multilateral institutions and the stability of the non-proliferation
 *   regime. This reading emphasizes the treaty's role in upholding
 *   international legal norms and collective security.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.25).
domain_priors:suppression_score(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.7).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__binding_multilateral_reading, rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__binding_multilateral_reading, "JCPOA as Binding Multilateral Treaty (Binding Multilateral Reading)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__binding_multilateral_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__binding_multilateral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, 'f30aa9dd-9092-4913-9457-0ce3b4ee6c4c').
narrative_ontology:cs_kernel_codification('f30aa9dd-9092-4913-9457-0ce3b4ee6c4c', formalized).
narrative_ontology:cs_authority_grounding('f30aa9dd-9092-4913-9457-0ce3b4ee6c4c', lineage).
narrative_ontology:cs_interpretation_layer_present('f30aa9dd-9092-4913-9457-0ce3b4ee6c4c').
narrative_ontology:cs_reading_relation('f30aa9dd-9092-4913-9457-0ce3b4ee6c4c', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('f30aa9dd-9092-4913-9457-0ce3b4ee6c4c', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('f30aa9dd-9092-4913-9457-0ce3b4ee6c4c', foundational, treaty_pacta_sunt_servanda).
narrative_ontology:cs_axiom_status(treaty_pacta_sunt_servanda, holdable).
narrative_ontology:cs_axiom_grounding('f30aa9dd-9092-4913-9457-0ce3b4ee6c4c', treaty_pacta_sunt_servanda, deontological).
narrative_ontology:cs_axiom('f30aa9dd-9092-4913-9457-0ce3b4ee6c4c', foundational, unsc_primacy_in_sanctions).
narrative_ontology:cs_axiom_status(unsc_primacy_in_sanctions, holdable).
narrative_ontology:cs_axiom_grounding('f30aa9dd-9092-4913-9457-0ce3b4ee6c4c', unsc_primacy_in_sanctions, conventional).
narrative_ontology:cs_reference_frame('f30aa9dd-9092-4913-9457-0ce3b4ee6c4c', vienna_convention_treaty_law).
narrative_ontology:cs_drift_state('f30aa9dd-9092-4913-9457-0ce3b4ee6c4c', contemporary_geopolitical_environment, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f30aa9dd-9092-4913-9457-0ce3b4ee6c4c', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, p5_plus_1_states).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iran).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_regime).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The signatory states (China, France, Germany, Russia, United Kingdom, United States) that negotiated the treaty. They are bound by its terms and are the primary enforcers of its dispute resolution mechanism, requiring consensus for modification or dissolution. They benefit from the non-proliferation outcome and the stability of the international legal order.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, p5_plus_1_states, agenda_setter,
    institutional, generational, constrained, global).

% Agreed to significant restrictions on its nuclear program in exchange for sanctions relief. Under this reading, Iran is bound by the treaty's terms and benefits from the legitimacy and stability of the multilateral framework, which protects it from unilateral snapback of sanctions without due process.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iran, beneficiary,
    powerful, biographical, constrained, national).

% The ultimate arbiter of international peace and security, whose resolutions underpin the JCPOA. Under this reading, the UNSC's consensus-based decision-making is central to any sanctions reimposition or treaty dissolution, reinforcing its authority and the multilateral nature of the agreement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council, agenda_setter,
    institutional, generational, analytical, universal).

% The international body responsible for verifying Iran's compliance with its nuclear commitments. The JCPOA strengthens the IAEA's mandate and provides enhanced inspection access, benefiting its role in the non-proliferation regime.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea, beneficiary,
    institutional, generational, analytical, global).

% The overarching international framework aimed at preventing the spread of nuclear weapons. The JCPOA, under this reading, is a cornerstone of this regime, demonstrating the efficacy of multilateral diplomacy and verification in containing proliferation risks.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_regime, beneficiary,
    institutional, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_regime).

% Internal political groups in Iran that view the JCPOA as an infringement on national sovereignty and an unacceptable concession. They would prefer a more confrontational stance and unilateral nuclear development, but are constrained by the state's commitment to the treaty.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, hardline_factions_iran, excluded,
    organized, biographical, identity_locked, national).

% States that prioritize national interests and unilateral action over multilateral consensus in foreign policy. They would prefer to withdraw from or disregard treaties like the JCPOA if they perceive it as not serving their immediate interests, but are constrained by the international legal framework and diplomatic costs.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, unilateralist_states, excluded,
    powerful, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the nuclear activities of Iran with the security concerns of the international community, preventing nuclear proliferation through a verifiable, multilateral framework and providing a structured dispute resolution mechanism.
% TRANSFER_FUNCTION: Transfers verifiable nuclear restrictions from Iran in exchange for sanctions relief from the P5+1 states, mediated by the UN Security Council and IAEA verification.
% ABSENT_VOICES: Hardline factions within Iran and unilateralist states would object to the binding nature and multilateral constraints, advocating for national sovereignty and freedom of action. They are excluded from the consensus-based modification process.
% DISAPPEARANCE_RATIONALE: If the JCPOA's bindingness vanished, Iran's nuclear program would likely accelerate, sanctions would be unilaterally reimposed, and the international non-proliferation regime would suffer a severe blow, leading to a significant rearrangement of geopolitical alliances and security postures.
% FOUNDING_PROBLEM: Iran's accelerating nuclear program posed a significant proliferation risk, leading to international sanctions and fears of regional instability and military conflict.
% FOUNDING_PROBLEM_CORROBORATION: The IAEA continues to report on Iran's nuclear activities, and the P5+1 states consistently affirm the ongoing need for nuclear non-proliferation. Independent security analysts and international relations scholars corroborate that the underlying proliferation risk remains live, even if contained by the treaty.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__binding_multilateral_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__binding_multilateral_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__binding_multilateral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.25) is low because the constraint primarily coordinates state behavior towards a common goal (non-proliferation) with reciprocal benefits, rather than extracting from one party for another's gain. Suppression (0.70) is high because the international legal framework and diplomatic pressure actively suppress unilateral actions that would undermine the treaty. Theater ratio (0.10) is low as the treaty's mechanisms for verification and dispute resolution are genuinely functional. Accessibility collapse (0.75) is high because the multilateral framework significantly limits alternative, unilateral paths to nuclear development or sanctions enforcement. Resistance (0.30) is moderate, reflecting ongoing political challenges but a general adherence to the treaty's core principles by most signatories.
 *
 * PERSPECTIVAL GAP:
 *   While this reading emphasizes the treaty's binding nature and multilateral benefits, other readings (e.g., 'transactional provisional' or 'graduated compliance') would highlight different aspects, such as the conditional nature of commitments or the proportionality of responses to violations. The engine's per-seat classification would reflect these differences, but this story focuses on the 'binding multilateral' perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   The P5+1 states and the UN Security Council are agenda-setters, benefiting from the stability and non-proliferation outcomes. Iran is a beneficiary, gaining sanctions relief and international legitimacy for its nuclear program within defined limits. The IAEA and the broader non-proliferation regime also benefit from the strengthened verification and enforcement mechanisms. There are no direct 'victims' under this reading, as all parties are considered net beneficiaries of the coordinated non-proliferation effort. Unilateralist states and hardline Iranian factions are 'excluded' as their preferred actions are suppressed by the treaty's binding nature.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing nuclear proliferation through multilateral agreement) is still live and highly relevant. The classification as 'rope' reflects a genuine coordination function with low extraction, preventing mislabeling it as a snare or piton, which would imply a defunct or purely extractive purpose. The active enforcement and low theater ratio indicate it is not a piton, and the absence of victims prevents it from being a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    jcpoa_reading_identity,
    'Is the JCPOA fundamentally a binding multilateral treaty, or a transactional framework, or a graduated compliance mechanism?',
    'Analysis of state practice, diplomatic statements, and legal interpretations over time, particularly in response to breaches or attempted withdrawals. If the international community consistently upholds the multilateral dispute resolution process and rejects unilateral actions, it supports the binding multilateral reading.',
    'If resolved as a binding multilateral treaty, the constraint''s stability and legitimacy are reinforced, making unilateral actions more costly. If resolved as transactional or graduated, the constraint''s effective suppression and extractiveness would be re-evaluated based on the flexibility and conditionality of commitments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(jcpoa_reading_identity, conceptual, 'Ambiguity in the fundamental nature of the JCPOA as a legal instrument.').

omega_variable(
    unilateral_withdrawal_legitimacy,
    'Does a state''s unilateral withdrawal from the JCPOA, or reimposition of sanctions outside the dispute resolution mechanism, constitute a legitimate exercise of sovereignty or a violation of international law?',
    'International legal rulings, UN Security Council resolutions, and the collective diplomatic response of signatory states. If such actions are widely condemned and met with significant diplomatic and economic costs, it supports the binding multilateral reading.',
    'If unilateral withdrawal is deemed illegitimate, the constraint''s suppression of such actions is effective. If deemed legitimate, the constraint''s bindingness is weakened, and its effective suppression would be lower.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_withdrawal_legitimacy, preference, 'Contestation over the legitimacy of unilateral actions versus multilateral commitments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__binding_multilateral_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(jcpo_tr_t2, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 2, 0.1).
narrative_ontology:measurement(jcpo_tr_t4, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement(jcpo_tr_t6, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 6, 0.1).
narrative_ontology:measurement(jcpo_tr_t8, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 8, 0.1).
narrative_ontology:measurement(jcpo_tr_t10, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(jcpo_be_t2, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 2, 0.25).
narrative_ontology:measurement(jcpo_be_t4, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 4, 0.25).
narrative_ontology:measurement(jcpo_be_t6, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 6, 0.25).
narrative_ontology:measurement(jcpo_be_t8, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 8, 0.25).
narrative_ontology:measurement(jcpo_be_t10, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 10, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(jcpo_su_t2, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 2, 0.7).
narrative_ontology:measurement(jcpo_su_t4, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 4, 0.7).
narrative_ontology:measurement(jcpo_su_t6, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(jcpo_su_t8, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 8, 0.7).
narrative_ontology:measurement(jcpo_su_t10, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 10, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__binding_multilateral_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_treaty_regime).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, iran_nuclear_program_limits).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
