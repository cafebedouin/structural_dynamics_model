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
 *   constraint_id: jcpoa_treaty_bindingness__transactional_provisional_reading
 *   human_readable: JCPOA as Provisional Transactional Framework (Transactional-Provisional Reading)
 *   domain: international_law/nuclear_non_proliferation/treaty_compliance
 *
 * SUMMARY:
 *   This constraint story models the Joint Comprehensive Plan of Action
 *   (JCPOA) as a provisional transactional framework, voidable upon
 *   unilateral determination of 'bad faith' by a signatory. This reading
 *   emphasizes national sovereignty and domestic political considerations
 *   over multilateral consensus or formal breach procedures. It implies a low
 *   constraint on unilateral withdrawal and justifies immediate sanctions
 *   reimposition based on national assessments of Iranian violations. The
 *   beneficiaries are individual state sovereignty and domestic political
 *   coalitions opposing the deal, while Iran and advocates for multilateral
 *   diplomacy are the victims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.65).
domain_priors:suppression_score(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.7).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__transactional_provisional_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__transactional_provisional_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__transactional_provisional_reading, snare).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__transactional_provisional_reading, "JCPOA as Provisional Transactional Framework (Transactional-Provisional Reading)").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__transactional_provisional_reading, "international_law/nuclear_non_proliferation/treaty_compliance").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__transactional_provisional_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__transactional_provisional_reading, '6f922675-03ab-420b-890c-b1f3554d3157').
narrative_ontology:cs_kernel_codification('6f922675-03ab-420b-890c-b1f3554d3157', formalized).
narrative_ontology:cs_authority_grounding('6f922675-03ab-420b-890c-b1f3554d3157', extraction).
narrative_ontology:cs_interpretation_layer_present('6f922675-03ab-420b-890c-b1f3554d3157').
narrative_ontology:cs_reading_relation('6f922675-03ab-420b-890c-b1f3554d3157', jcpoa_treaty_bindingness__binding_multilateral_reading, forecloses).
narrative_ontology:cs_reading_relation('6f922675-03ab-420b-890c-b1f3554d3157', jcpoa_treaty_bindingness__graduated_compliance_reading, influences).
narrative_ontology:cs_axiom('6f922675-03ab-420b-890c-b1f3554d3157', foundational, national_sovereignty_trumps_multilateral_consensus).
narrative_ontology:cs_axiom_status(national_sovereignty_trumps_multilateral_consensus, holdable).
narrative_ontology:cs_axiom_grounding('6f922675-03ab-420b-890c-b1f3554d3157', national_sovereignty_trumps_multilateral_consensus, deontological).
narrative_ontology:cs_axiom('6f922675-03ab-420b-890c-b1f3554d3157', foundational, bad_faith_is_unilaterally_determinable).
narrative_ontology:cs_axiom_status(bad_faith_is_unilaterally_determinable, holdable).
narrative_ontology:cs_axiom_grounding('6f922675-03ab-420b-890c-b1f3554d3157', bad_faith_is_unilaterally_determinable, conventional).
narrative_ontology:cs_reference_frame('6f922675-03ab-420b-890c-b1f3554d3157', unilateral_sovereign_prerogative).
narrative_ontology:cs_drift_state('6f922675-03ab-420b-890c-b1f3554d3157', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6f922675-03ab-420b-890c-b1f3554d3157', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, state_sovereignty_advocates).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_political_coalitions_opposing_deal).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, iran).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__transactional_provisional_reading, multilateral_diplomacy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the interpretation that states retain full sovereign right to unilaterally withdraw from international agreements based on their own assessment of national interest or counterparty bad faith, without requiring multilateral consensus or formal breach procedures. This reading reinforces their core ideological commitment.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, state_sovereignty_advocates, beneficiary,
    institutional, generational, arbitrage, global).

% These coalitions benefit from the transactional reading as it provides a legal and political justification for unilateral withdrawal and sanctions reimposition, aligning with their pre-existing opposition to the deal. It empowers their policy agenda.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, domestic_political_coalitions_opposing_deal, beneficiary,
    organized, immediate, mobile, national).

% Bears the cost of unilateral withdrawal and sanctions reimposition, even if its compliance is disputed. This reading makes its adherence to the agreement vulnerable to external political shifts rather than objective compliance metrics, leading to economic and political pressure.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, iran, payer,
    powerful, biographical, constrained, national).

% Bears the cost of weakened international norms around treaty bindingness and multilateral consensus. This reading undermines the framework they advocate for, making future complex diplomatic agreements harder to sustain.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, multilateral_diplomacy_advocates, payer,
    institutional, generational, constrained, global).

% These states, while signatories, are excluded from the unilateral determination of bad faith that triggers withdrawal under this reading. They would advocate for multilateral dispute resolution and adherence to the agreement, but their voice is overridden by the sovereign determination of a single party.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, eu_e3_states, excluded,
    institutional, biographical, constrained, continental).

% Provides technical verification of Iran's nuclear compliance, but its findings are not determinative for unilateral withdrawal under this reading. It observes the political process but cannot prevent the transactional interpretation from being applied.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__transactional_provisional_reading, international_atomic_energy_agency, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a temporary, conditional framework for managing nuclear proliferation risks with Iran, allowing for a transactional exchange of sanctions relief for nuclear concessions, subject to ongoing review.
% TRANSFER_FUNCTION: Transfers sanctions relief to Iran in exchange for nuclear program limitations, with the understanding that this relief can be unilaterally revoked if 'bad faith' is determined by a party.
% ABSENT_VOICES: The voices of international legal scholars emphasizing pacta sunt servanda (agreements must be kept) and the binding nature of UN Security Council resolutions are marginalized. They would argue that the JCPOA, once adopted by the UNSC, carries a higher legal weight than a purely transactional, voidable agreement.
% DISAPPEARANCE_RATIONALE: If this transactional reading of the JCPOA disappeared, the international legal and political landscape around nuclear non-proliferation would rearrange. Unilateral withdrawals would face higher legal and diplomatic hurdles, potentially strengthening multilateral treaty frameworks and making it harder for domestic political actors to justify immediate exit from complex agreements.
% FOUNDING_PROBLEM: The problem of Iran's accelerating nuclear program and the international community's desire to prevent nuclear proliferation through diplomatic means, while also addressing the concerns of states skeptical of Iran's intentions.
% FOUNDING_PROBLEM_CORROBORATION: The states that adopted this transactional reading attest that the founding problem of Iran's nuclear threat remains live and requires a flexible, nationally-determined response. However, other signatories and international legal bodies argue that the original problem was addressed by the agreement's terms, and the 'live' status is maintained to justify unilateral actions, as evidenced by IAEA reports on Iranian compliance prior to withdrawal.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__transactional_provisional_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__transactional_provisional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__transactional_provisional_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__transactional_provisional_reading, 'none', 1).

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
 *   The extractiveness (0.65) is high because the transactional reading allows a party to unilaterally impose costs (sanctions) on Iran, even if Iran is technically compliant, based on a subjective 'bad faith' determination. Suppression (0.7) is also high, as this reading suppresses multilateral dispute resolution mechanisms and the voices of other signatories. The theater ratio (0.2) is relatively low, as the actions taken under this reading (withdrawal, sanctions) are direct and impactful, not merely performative. The claimed type is 'snare' because the coordination story (nuclear non-proliferation) is used as cover for unilateral extraction and the suppression of alternative diplomatic paths.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state sovereignty advocates, this is a legitimate exercise of national interest, a 'rope' for managing a difficult counterparty. From Iran's perspective, it is a 'snare' that traps it in an agreement subject to arbitrary external revocation. The engine's classification as 'snare' reflects the structural reality of asymmetric power and extraction inherent in this reading.
 *
 * DIRECTIONALITY LOGIC:
 *   State sovereignty advocates and domestic political coalitions are beneficiaries (d near 0.0) as this reading empowers their policy preferences and ideological commitments. Iran is a primary victim (d near 1.0) as it bears the direct costs of sanctions and political isolation. Multilateral diplomacy advocates are also victims (d near 1.0) as their preferred mode of international engagement is undermined. EU/E3 states are 'excluded' as their role in the agreement is diminished by unilateral actions.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    unilateral_bad_faith_definition,
    'What constitutes ''bad faith'' sufficient to justify unilateral withdrawal and sanctions reimposition under this reading, and is it subject to any objective or multilateral review?',
    'Analysis of legal and diplomatic precedents cited by proponents of this reading, and examination of whether any independent body''s assessment of ''bad faith'' is acknowledged or required.',
    'If ''bad faith'' is purely subjective and unreviewable, the constraint''s extractiveness and suppression are higher, reinforcing its ''snare'' classification. If there are implicit or explicit objective criteria, it might suggest a weaker snare or a tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unilateral_bad_faith_definition, conceptual, 'Ambiguity in the definition and review of ''bad faith'' for unilateral action.').

omega_variable(
    legitimacy_of_unilateral_withdrawal,
    'Is unilateral withdrawal from an agreement endorsed by a UN Security Council resolution consistent with international law, or does it set a precedent that undermines the global non-proliferation regime?',
    'International Court of Justice advisory opinion or a consensus statement from a broad coalition of states on the legal implications of such withdrawals.',
    'If deemed inconsistent, the ''snare'' classification is strengthened, as the constraint relies on a contested legal interpretation to extract. If deemed legitimate, the constraint might lean closer to a ''tangled rope'' where the extraction is a recognized cost of sovereign flexibility.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_unilateral_withdrawal, preference, 'Contestation over the international legal legitimacy of unilateral withdrawal from UNSC-endorsed agreements.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__transactional_provisional_reading, 2015, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t2015, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2015, 0.1).
narrative_ontology:measurement(jcpo_tr_t2016, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2016, 0.12).
narrative_ontology:measurement(jcpo_tr_t2017, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2017, 0.15).
narrative_ontology:measurement(jcpo_tr_t2018, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2018, 0.2).
narrative_ontology:measurement(jcpo_tr_t2019, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2019, 0.2).
narrative_ontology:measurement(jcpo_tr_t2020, jcpoa_treaty_bindingness__transactional_provisional_reading, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t2015, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2015, 0.5).
narrative_ontology:measurement(jcpo_be_t2016, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2016, 0.55).
narrative_ontology:measurement(jcpo_be_t2017, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2017, 0.6).
narrative_ontology:measurement(jcpo_be_t2018, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2018, 0.65).
narrative_ontology:measurement(jcpo_be_t2019, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2019, 0.65).
narrative_ontology:measurement(jcpo_be_t2020, jcpoa_treaty_bindingness__transactional_provisional_reading, base_extractiveness, 2020, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t2015, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2015, 0.55).
narrative_ontology:measurement(jcpo_su_t2016, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2016, 0.6).
narrative_ontology:measurement(jcpo_su_t2017, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2017, 0.65).
narrative_ontology:measurement(jcpo_su_t2018, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2018, 0.7).
narrative_ontology:measurement(jcpo_su_t2019, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2019, 0.7).
narrative_ontology:measurement(jcpo_su_t2020, jcpoa_treaty_bindingness__transactional_provisional_reading, suppression_requirement, 2020, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__transactional_provisional_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__binding_multilateral_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__transactional_provisional_reading, iran_nuclear_program_limitations).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'jcpoa_treaty_bindingness' kernel. This 'transactional_provisional_reading' emphasizes unilateral determination and voidability, contrasting with the 'binding_multilateral_reading' (stressing consensus and treaty law) and the 'graduated_compliance_reading' (stressing proportional reciprocity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
