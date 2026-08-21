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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   This constraint represents the 'binding multilateral treaty' reading of
 *   the Joint Comprehensive Plan of Action (JCPOA). Under this reading, the
 *   JCPOA is a robust, legally binding international agreement that requires
 *   consensus among its signatories for any modification, withdrawal, or
 *   re-imposition of sanctions. Unilateral actions are seen as violations of
 *   the treaty's fundamental structure. The constraint's low extractiveness
 *   reflects the view that the treaty primarily serves a coordination
 *   function for non-proliferation, with costs borne symmetrically by all
 *   parties for collective security. Suppression is high because the
 *   international legal framework and the threat of multilateral sanctions
 *   actively suppress unilateral deviations.
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
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, '0ea033cf-d9f3-41cf-941f-1639d8b1b9d5').
narrative_ontology:cs_kernel_codification('0ea033cf-d9f3-41cf-941f-1639d8b1b9d5', formalized).
narrative_ontology:cs_authority_grounding('0ea033cf-d9f3-41cf-941f-1639d8b1b9d5', lineage).
narrative_ontology:cs_interpretation_layer_present('0ea033cf-d9f3-41cf-941f-1639d8b1b9d5').
narrative_ontology:cs_reading_relation('0ea033cf-d9f3-41cf-941f-1639d8b1b9d5', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('0ea033cf-d9f3-41cf-941f-1639d8b1b9d5', jcpoa_treaty_bindingness__graduated_compliance_reading, coexists_with).
narrative_ontology:cs_axiom('0ea033cf-d9f3-41cf-941f-1639d8b1b9d5', foundational, treaty_obligations_are_binding).
narrative_ontology:cs_axiom_status(treaty_obligations_are_binding, holdable).
narrative_ontology:cs_axiom_grounding('0ea033cf-d9f3-41cf-941f-1639d8b1b9d5', treaty_obligations_are_binding, deontological).
narrative_ontology:cs_axiom('0ea033cf-d9f3-41cf-941f-1639d8b1b9d5', foundational, unilateral_withdrawal_is_illegitimate).
narrative_ontology:cs_axiom_status(unilateral_withdrawal_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('0ea033cf-d9f3-41cf-941f-1639d8b1b9d5', unilateral_withdrawal_is_illegitimate, conventional).
narrative_ontology:cs_reference_frame('0ea033cf-d9f3-41cf-941f-1639d8b1b9d5', vienna_convention_treaty_law).
narrative_ontology:cs_drift_state('0ea033cf-d9f3-41cf-941f-1639d8b1b9d5', contemporary_geopolitical_context, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('0ea033cf-d9f3-41cf-941f-1639d8b1b9d5', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, p5_plus_1_states).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iran).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_regime).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iran).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The signatory states (China, France, Germany, Russia, United Kingdom, United States) that negotiated the treaty. They are bound by its terms and are the primary enforcers of its dispute resolution mechanism, requiring consensus for major changes or dissolution. They benefit from the non-proliferation outcome and the stability of the international legal framework.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, p5_plus_1_states, agenda_setter,
    institutional, generational, constrained, global).

% Agreed to significant restrictions on its nuclear program in exchange for sanctions relief. Under this reading, Iran is bound by the treaty's terms and cannot unilaterally withdraw or violate its commitments without triggering multilateral dispute resolution and potential snapback sanctions. Benefits from sanctions relief and international legitimacy, but pays through nuclear program restrictions.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iran, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, iran, payer).

% The ultimate arbiter for sanctions snapback and treaty enforcement under this reading. Its consensus is required for any new sanctions or dissolution of the treaty, reinforcing the multilateral nature of the agreement. Benefits from upholding international law and non-proliferation norms.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, un_security_council, agenda_setter,
    institutional, generational, constrained, universal).

% The international body responsible for verifying Iran's compliance with its nuclear commitments. Its reports provide the factual basis for dispute resolution. Benefits from the enhanced verification mandate and the strengthening of the non-proliferation regime.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea, beneficiary,
    institutional, generational, analytical, global).

% The overarching international framework aimed at preventing the spread of nuclear weapons. The JCPOA, under this reading, is a key component that reinforces the regime's principles and mechanisms. Benefits from the stability and precedent set by a binding multilateral agreement.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, non_proliferation_regime, beneficiary,
    institutional, civilizational, analytical, universal).

% Actors (e.g., certain political factions or states) who prioritize unilateral action and national sovereignty over multilateral consensus. They would argue for the right to withdraw from or re-impose sanctions on the JCPOA without multilateral approval, but are structurally excluded from this reading's framework.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, unilateralist_actors, excluded,
    powerful, immediate, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the nuclear activities of Iran with the security concerns of the international community, preventing nuclear proliferation through verifiable restrictions and sanctions relief, all within a framework of international law.
% TRANSFER_FUNCTION: Transfers verifiable nuclear program restrictions from Iran to the international community, in exchange for sanctions relief and international legitimacy. It also transfers decision-making power regarding treaty modification or dissolution to a multilateral consensus mechanism.
% ABSENT_VOICES: Unilateralist actors who believe in the right to withdraw from or re-impose sanctions on the JCPOA without multilateral approval are excluded from the decision-making process under this reading. They would advocate for national prerogative over treaty obligations.
% DISAPPEARANCE_RATIONALE: If the JCPOA's binding multilateral nature vanished, it would lead to immediate unilateral withdrawals, snapback sanctions, and a rapid escalation of Iran's nuclear program, fundamentally altering the regional security landscape and the global non-proliferation architecture.
% FOUNDING_PROBLEM: Iran's accelerating nuclear enrichment program posed a significant proliferation risk, leading to international sanctions and a diplomatic crisis, with a high risk of military conflict.
% FOUNDING_PROBLEM_CORROBORATION: The P5+1 states, the UN Security Council, and the IAEA all attest that the core proliferation risk remains, and the treaty's framework is essential for managing it. Independent non-proliferation experts and think tanks corroborate the ongoing relevance of the founding problem, even with the treaty's current status.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__binding_multilateral_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__binding_multilateral_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness is low (0.25) because this reading emphasizes the mutual benefits of non-proliferation and the shared costs of maintaining international security. The treaty is seen as a net positive for all signatories, with no single party extracting disproportionately. Suppression is high (0.70) because the treaty's binding nature is maintained through the active enforcement mechanisms of international law, including the UN Security Council's role in sanctions snapback. Any unilateral deviation is met with strong diplomatic and legal pressure. Theater ratio is low (0.10) as the treaty's mechanisms are considered genuinely functional in achieving their stated goals.
 *
 * PERSPECTIVAL GAP:
 *   This reading emphasizes the stability and legal force of the treaty. Other readings, such as the 'transactional provisional' or 'graduated compliance' readings, would likely assign higher extractiveness or lower suppression, reflecting a view of the treaty as more flexible, less binding, or more prone to unilateral reinterpretation. The engine will compute these divergences from the structural data of each reading.
 *
 * DIRECTIONALITY LOGIC:
 *   The P5+1 states, the UN Security Council, and the IAEA are beneficiaries, as they uphold the international legal order and non-proliferation. Iran is also a beneficiary, gaining sanctions relief and international legitimacy, though it bears the cost of nuclear program restrictions. There are no direct 'victims' under this reading, as all parties are considered net beneficiaries of the non-proliferation outcome. Unilateralist actors are excluded, as their actions would undermine the multilateral framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_status_ambiguity,
    'Is the JCPOA a legally binding treaty under international law, or a political commitment that can be unilaterally abandoned?',
    'International Court of Justice ruling on the legal status of the JCPOA, or a definitive UN Security Council resolution affirming its binding nature.',
    'If ruled a political commitment, the constraint''s suppression and extractiveness would be significantly lower, reclassifying it towards a Piton or even a Snare for those still bound. If affirmed as binding, its Rope classification would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_status_ambiguity, conceptual, 'Ambiguity regarding the legal status of the JCPOA in international law.').

omega_variable(
    unilateral_withdrawal_legitimacy,
    'Does any signatory state possess a legitimate right to unilaterally withdraw from the JCPOA without multilateral consensus, or is such action a violation of the treaty''s core principles?',
    'A clear precedent set by international legal bodies or a universally accepted interpretation of the Vienna Convention on the Law of Treaties as applied to the JCPOA.',
    'If unilateral withdrawal is deemed legitimate, the constraint''s suppression would drop dramatically, and its classification would shift towards a more transactional type (e.g., Tangled Rope or Snare), as the enforcement mechanism would be severely weakened. If deemed illegitimate, the current Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_withdrawal_legitimacy, conceptual, 'Contestation over the legitimacy of unilateral withdrawal from the JCPOA.').

omega_variable(
    sanctions_snapback_mechanism_efficacy,
    'Is the JCPOA''s sanctions snapback mechanism genuinely effective as a multilateral enforcement tool, or is it vulnerable to unilateral blocking and political manipulation?',
    'Empirical observation of the mechanism''s activation and outcome in response to a significant violation, or a detailed legal analysis of its resilience to political interference.',
    'If the snapback mechanism is found to be easily blocked or manipulated, the constraint''s suppression would be lower, and its ability to coordinate non-proliferation efforts would be compromised, potentially shifting its classification towards a Piton or Snare. If effective, the Rope classification is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sanctions_snapback_mechanism_efficacy, empirical, 'Uncertainty about the practical efficacy of the JCPOA''s sanctions snapback mechanism.').


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


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
