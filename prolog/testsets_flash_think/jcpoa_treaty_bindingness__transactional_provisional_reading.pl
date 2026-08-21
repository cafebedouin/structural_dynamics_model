% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__transactional_provisional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jcpoa_treaty_bindingness__transactional_provisional_reading, []).

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
 *   constraint_id: jcpoa_treaty_bindingness__transactional_provisional_reading
 *   human_readable: JCPOA as Unilateral Exit Framework
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   This constraint describes the Joint Comprehensive Plan of Action (JCPOA)
 *   as interpreted through a 'transactional provisional' reading, which views
 *   the agreement as a flexible arrangement voidable upon a unilateral
 *   determination of bad faith by a signatory state. This reading prioritizes
 *   national sovereignty and allows for the unilateral reimposition of
 *   sanctions, effectively undermining the multilateral framework of the
 *   original deal. The high extractiveness reflects the costs imposed on Iran
 *   and the multilateral system, while high suppression indicates the active
 *   efforts to prevent alternative diplomatic paths.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.85).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.75).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, snare).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA as Unilateral Exit Framework").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, 'a80dcfe4-f668-47e6-81f5-847fdbc6011b').
narrative_ontology:cs_kernel_codification('a80dcfe4-f668-47e6-81f5-847fdbc6011b', formalized).
narrative_ontology:cs_authority_grounding('a80dcfe4-f668-47e6-81f5-847fdbc6011b', extraction).
narrative_ontology:cs_interpretation_layer_present('a80dcfe4-f668-47e6-81f5-847fdbc6011b').
narrative_ontology:cs_reading_relation('a80dcfe4-f668-47e6-81f5-847fdbc6011b', jcpoa_treaty_bindingness__binding_multilateral_reading, forecloses).
narrative_ontology:cs_reading_relation('a80dcfe4-f668-47e6-81f5-847fdbc6011b', jcpoa_treaty_bindingness__graduated_compliance_reading, forecloses).
narrative_ontology:cs_axiom('a80dcfe4-f668-47e6-81f5-847fdbc6011b', foundational, state_sovereignty_supremacy).
narrative_ontology:cs_axiom_status(state_sovereignty_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('a80dcfe4-f668-47e6-81f5-847fdbc6011b', state_sovereignty_supremacy, deontological).
narrative_ontology:cs_axiom('a80dcfe4-f668-47e6-81f5-847fdbc6011b', foundational, unilateral_determination_of_bad_faith).
narrative_ontology:cs_axiom_status(unilateral_determination_of_bad_faith, holdable).
narrative_ontology:cs_axiom_grounding('a80dcfe4-f668-47e6-81f5-847fdbc6011b', unilateral_determination_of_bad_faith, conventional).
narrative_ontology:cs_reference_frame('a80dcfe4-f668-47e6-81f5-847fdbc6011b', national_interest_first_framework).
narrative_ontology:cs_drift_state('a80dcfe4-f668-47e6-81f5-847fdbc6011b', post_withdrawal_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('a80dcfe4-f668-47e6-81f5-847fdbc6011b', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, state_sovereignty_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_political_coalitions_opposing_deal).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iran).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, multilateral_diplomacy).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, e3_eu_plus_3_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Proponents of this reading, often within a powerful state's government, who prioritize national security and unilateral action over multilateral treaty obligations. They benefit from the flexibility to withdraw and reimpose sanctions based on their own determination of bad faith.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, state_sovereignty_advocates, agenda_setter,
    institutional, generational, arbitrage, global).

% Political groups within the powerful state that opposed the original JCPOA. They benefit from a reading that allows for its dissolution and the pursuit of alternative, more aggressive policies, aligning with their ideological positions.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_political_coalitions_opposing_deal, beneficiary,
    organized, biographical, mobile, national).

% The primary target of the constraint, facing reimposed sanctions and the threat of further isolation. Their compliance with the original deal is undermined, and their economic and political options are severely curtailed by unilateral actions.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iran, payer,
    powerless, immediate, trapped, national).

% The broader international framework for resolving disputes and enforcing non-proliferation norms. This reading undermines its authority by prioritizing unilateral action, making consensus-based solutions harder to achieve and weakening the precedent for future multilateral agreements.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, multilateral_diplomacy, excluded,
    institutional, generational, constrained, global).

% Other signatories to the JCPOA (France, Germany, UK, EU, Russia, China) who sought to preserve the deal through multilateral mechanisms. They bear the cost of its erosion through increased regional instability, diplomatic friction, and the weakening of non-proliferation efforts.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, e3_eu_plus_3_states, payer,
    institutional, biographical, constrained, global).

% The UN body responsible for verifying Iran's nuclear commitments. While technically neutral, its verification efforts are complicated by the unilateral withdrawal and reimposition of sanctions, making its mission more difficult and its findings subject to political interpretation.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, international_atomic_energy_agency, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jcpoa_treaty_bindingness__transactional_provisional_reading, state_sovereignty_advocates).
narrative_ontology:fixing_cost_class(jcpoa_treaty_bindingness__transactional_provisional_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The framework provides a transactional basis for managing nuclear proliferation risks, allowing states to engage or disengage based on perceived national interest and unilateral determination of compliance.
% TRANSFER_FUNCTION: Transfers the burden of compliance and the risk of proliferation back to Iran, while transferring the right to unilateral action and sanctions reimposition to the state adopting this reading, at the expense of multilateral consensus.
% ABSENT_VOICES: Advocates for strict multilateral treaty adherence, international legal scholars emphasizing pacta sunt servanda, and Iranian civil society groups who bear the brunt of sanctions. They would argue for the primacy of international law and the humanitarian impact of unilateral actions.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the international legal landscape around the JCPOA would shift dramatically towards multilateralism, making unilateral withdrawal and sanctions reimposition much harder to justify. This would likely force a renegotiation of the deal or a return to a more binding, consensus-based framework, reorganizing diplomatic efforts and economic pressures.
% FOUNDING_PROBLEM: The perceived failure of previous multilateral efforts to constrain Iran's nuclear program, leading to a desire for a framework that prioritizes national security interests and allows for flexible, unilateral responses to perceived threats.
% FOUNDING_PROBLEM_CORROBORATION: Proponents within the adopting state's government and allied think tanks corroborate that the founding problem (Iran's nuclear ambitions) is still live and requires this flexible, unilateral approach. International legal bodies and other JCPOA signatories dispute this, arguing the founding problem was addressed by the original deal and that unilateral actions exacerbate, rather than solve, the issue.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__transactional_provisional_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__transactional_provisional_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__transactional_provisional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jcpoa_treaty_bindingness__transactional_provisional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) stems from the unilateral right to reimpose sanctions and exit the agreement, imposing severe economic costs on Iran and diplomatic costs on other signatories. Suppression (0.75) is high because this reading actively dismisses and overrides multilateral mechanisms for dispute resolution and compliance assessment, effectively trapping Iran and constraining other states. The theater ratio is low (0.20) as the actions taken under this reading (e.g., withdrawal, sanctions) are direct and have immediate, tangible effects, rather than being merely performative. Resistance is high (0.80) due to strong opposition from Iran and other JCPOA signatories who advocate for the original multilateral framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the 'state sovereignty advocates,' this reading is a necessary and justified exercise of national interest, ensuring security. From Iran's perspective, it is an act of economic warfare and a violation of international commitments. The engine's classification as a Snare reflects the structural reality of unilateral extraction and suppression, regardless of the proponents' justification.
 *
 * DIRECTIONALITY LOGIC:
 *   State sovereignty advocates and domestic political coalitions opposing the deal are clear beneficiaries, gaining flexibility and policy alignment. Iran is the primary target/victim, bearing the brunt of sanctions and diplomatic isolation. Multilateral diplomacy and the E3/EU+3 states are also victims, as their preferred mode of engagement and the stability of the agreement are undermined. The IAEA acts as an observer, its technical mission complicated by the political context.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_nature_ambiguity,
    'Is the JCPOA fundamentally a binding multilateral treaty under international law, or a political commitment subject to unilateral withdrawal based on national interest?',
    'Adjudication by the International Court of Justice or a definitive, universally accepted interpretation by the UN Security Council, clarifying the legal status of the agreement.',
    'If determined to be a binding treaty, this reading''s justification for unilateral withdrawal would be significantly weakened, potentially reclassifying it towards a Tangled Rope or even a Piton if its enforcement becomes purely theatrical. If confirmed as a non-binding political commitment, this reading''s Snare classification would be reinforced as a legitimate exercise of state power.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_nature_ambiguity, conceptual, 'Ambiguity regarding the legal nature of the JCPOA and its implications for unilateral action.').

omega_variable(
    unilateral_bad_faith_criteria,
    'What objective, internationally recognized criteria define ''bad faith'' or ''material breach'' sufficient to justify unilateral withdrawal from an agreement like the JCPOA?',
    'Development of clear, verifiable metrics for compliance and breach, agreed upon by all signatories or an independent international body, rather than relying on unilateral determination.',
    'If objective criteria are established, the ''unilateral determination'' aspect of this reading would be constrained, potentially reducing its extractiveness and suppression. If no such criteria can be agreed upon, the current high extractiveness and suppression would persist, as the power to define ''bad faith'' remains concentrated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unilateral_bad_faith_criteria, empirical, 'Lack of objective criteria for unilateral determination of bad faith.').

omega_variable(
    reading_origin_vs_function,
    'Does this ''transactional provisional'' reading genuinely serve to manage nuclear proliferation risks, or has its primary function shifted to enabling geopolitical leverage and domestic political gains?',
    'Longitudinal analysis of proliferation outcomes and regional stability under this reading versus alternative approaches, alongside an assessment of the domestic political benefits accrued by its proponents.',
    'If the reading is found to primarily serve geopolitical/domestic interests without effectively managing proliferation, its Snare classification would be strongly confirmed, highlighting the cover story. If it demonstrably improves proliferation outcomes, its coordination function would be re-evaluated, potentially shifting it towards a Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_origin_vs_function, empirical, 'Whether the reading''s stated purpose aligns with its actual function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 2018, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t2018, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2018, 0.2).
narrative_ontology:measurement(jcpo_tr_t2019, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2019, 0.2).
narrative_ontology:measurement(jcpo_tr_t2020, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2020, 0.2).
narrative_ontology:measurement(jcpo_tr_t2021, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2021, 0.2).
narrative_ontology:measurement(jcpo_tr_t2022, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2022, 0.2).
narrative_ontology:measurement(jcpo_tr_t2023, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2023, 0.2).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t2018, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2018, 0.75).
narrative_ontology:measurement(jcpo_be_t2019, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2019, 0.78).
narrative_ontology:measurement(jcpo_be_t2020, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2020, 0.81).
narrative_ontology:measurement(jcpo_be_t2021, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2021, 0.83).
narrative_ontology:measurement(jcpo_be_t2022, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2022, 0.84).
narrative_ontology:measurement(jcpo_be_t2023, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2023, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t2018, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2018, 0.65).
narrative_ontology:measurement(jcpo_su_t2019, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2019, 0.68).
narrative_ontology:measurement(jcpo_su_t2020, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2020, 0.7).
narrative_ontology:measurement(jcpo_su_t2021, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2021, 0.72).
narrative_ontology:measurement(jcpo_su_t2022, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2022, 0.74).
narrative_ontology:measurement(jcpo_su_t2023, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2023, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
