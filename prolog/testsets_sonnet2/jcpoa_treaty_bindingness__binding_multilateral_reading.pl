% ============================================================================
% CONSTRAINT STORY: jcpoa_treaty_bindingness__binding_multilateral_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: jcpoa_treaty_bindingness__binding_multilateral_reading
 *   human_readable: JCPOA as Binding Multilateral Treaty Requiring Consensus-Based Modification or Dissolution
 *   domain: international_law/nuclear_non_proliferation
 *
 * SUMMARY:
 *   This story instantiates the binding_multilateral_reading of the JCPOA
 *   kernel: the arrangement is a consensus-bound treaty whose modification or
 *   dissolution can only proceed through the Joint Commission's dispute
 *   resolution mechanism, and whose snapback provision requires the
 *   multilateral structure (not any single party's unilateral judgment) to
 *   trigger sanctions reimposition. This is a genuinely different constraint
 *   from the transactional_provisional_reading (which treats the deal as
 *   voidable on unilateral bad-faith determination) and the
 *   graduated_compliance_reading (which ties enforcement to proportional
 *   compliance scoring rather than binary treaty-bound status). The 2018 US
 *   withdrawal is the central stress event for this reading: it either shows
 *   the binding reading was descriptively false (the withdrawal happened
 *   outside the consensus mechanism and nothing structurally prevented it) or
 *   shows the reading correctly predicts that a unilateral exit outside
 *   consensus is a breach with costs (loss of legal standing to claim relief,
 *   EU/UNSC continued adherence, IAEA continued reporting under the original
 *   framework) rather than a clean dissolution. The metrics here are authored
 *   for the binding-multilateral reading's own account of the standing
 *   arrangement, per the ε-referent rule for kernel readings.
 *
 * KEY AGENTS:
 *   - unsc_permanent_members: institutional agenda-setters who administer snapback and benefit from controlling the enforcement calendar
 *   - eu_coordinating_powers: institutional brokers whose diplomatic relevance depends on the binding reading holding
 *   - iranian_civilian_economy: the primary payer, trapped in economic uncertainty pending multilateral dispute resolution outcomes
 *   - unilateral_withdrawal_advocates: powerful actors structurally denied a clean unilateral exit path under this reading
 *   - regional_rival_states: excluded parties bearing security externalities with no seat in the consensus mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.42).
domain_priors:suppression_score(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.55).
domain_priors:theater_ratio(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(jcpoa_treaty_bindingness__binding_multilateral_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jcpoa_treaty_bindingness__binding_multilateral_reading, tangled_rope).
narrative_ontology:human_readable(jcpoa_treaty_bindingness__binding_multilateral_reading, "JCPOA as Binding Multilateral Treaty Requiring Consensus-Based Modification or Dissolution").
narrative_ontology:topic_domain(jcpoa_treaty_bindingness__binding_multilateral_reading, "international_law/nuclear_non_proliferation").

domain_priors:requires_active_enforcement(jcpoa_treaty_bindingness__binding_multilateral_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jcpoa_treaty_bindingness__binding_multilateral_reading, 'ebc83334-8d70-4620-991a-4c7b2d41aec6').
narrative_ontology:cs_kernel_codification('ebc83334-8d70-4620-991a-4c7b2d41aec6', formalized).
narrative_ontology:cs_authority_grounding('ebc83334-8d70-4620-991a-4c7b2d41aec6', distributed).
narrative_ontology:cs_reading_relation('ebc83334-8d70-4620-991a-4c7b2d41aec6', jcpoa_treaty_bindingness__transactional_provisional_reading, forecloses).
narrative_ontology:cs_reading_relation('ebc83334-8d70-4620-991a-4c7b2d41aec6', jcpoa_treaty_bindingness__graduated_compliance_reading, influences).
narrative_ontology:cs_axiom('ebc83334-8d70-4620-991a-4c7b2d41aec6', foundational, unilateral_exit_constitutes_breach).
narrative_ontology:cs_axiom_status(unilateral_exit_constitutes_breach, holdable).
narrative_ontology:cs_axiom_grounding('ebc83334-8d70-4620-991a-4c7b2d41aec6', unilateral_exit_constitutes_breach, conventional).
narrative_ontology:cs_axiom('ebc83334-8d70-4620-991a-4c7b2d41aec6', foundational, consensus_process_is_exclusive_modification_channel).
narrative_ontology:cs_axiom_status(consensus_process_is_exclusive_modification_channel, holdable).
narrative_ontology:cs_axiom_grounding('ebc83334-8d70-4620-991a-4c7b2d41aec6', consensus_process_is_exclusive_modification_channel, conventional).
narrative_ontology:cs_reference_frame('ebc83334-8d70-4620-991a-4c7b2d41aec6', unsc_resolution_2231_annexed_obligation).
narrative_ontology:cs_drift_state('ebc83334-8d70-4620-991a-4c7b2d41aec6', post_2018_withdrawal_and_maximum_pressure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('ebc83334-8d70-4620-991a-4c7b2d41aec6', '').
narrative_ontology:cs_kernel_id(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_permanent_members).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, eu_coordinating_powers).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, nonproliferation_regime_architects).
narrative_ontology:constraint_beneficiary(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_verification_apparatus).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_civilian_economy).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, unilateral_withdrawal_advocates).
narrative_ontology:constraint_victim(jcpoa_treaty_bindingness__binding_multilateral_reading, regional_rival_states).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, multilateral_consensus_supremacy_doctrine).
narrative_ontology:constraint_vindicates(jcpoa_treaty_bindingness__binding_multilateral_reading, nonproliferation_treaty_regime_stability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Collectively administer the snapback mechanism and dispute resolution process; each holds veto-adjacent leverage over reimposition of sanctions. They benefit from the treaty's persistence as a governance structure they control, and can shape enforcement timing to their own diplomatic interests even while claiming to defer to consensus procedure.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, unsc_permanent_members, agenda_setter,
    institutional, generational, arbitrage, global).

% Serve as the Joint Commission's coordinating secretariat and broker disputes between Iran and other parties. Their diplomatic capital and INSTEX-style financial workarounds depend on the treaty's multilateral framing continuing to bind all signatories, including the United States in absentia.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, eu_coordinating_powers, agenda_setter,
    institutional, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, eu_coordinating_powers, beneficiary).

% Conducts inspections and verification reporting that only has legal weight because the treaty is read as binding; a provisional or unilaterally-voidable reading would strip its findings of consequence. Its institutional relevance and budget line for Iran-specific verification depend on this reading holding.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iaea_verification_apparatus, beneficiary,
    institutional, generational, constrained, global).

% Diplomats, legal scholars, and NPT review-conference participants who treat JCPOA's binding character as precedent for future nonproliferation deals. They do not collect direct rents but their doctrine's credibility rises or falls with whether this reading is vindicated by state practice.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, nonproliferation_regime_architects, beneficiary,
    institutional, civilizational, analytical, global).

% Bears the cost of sanctions relief being contingent on continued multilateral consensus rather than on Iran's own bilateral judgment of reciprocity. When any single party disputes compliance, the binding-treaty reading requires the entire dispute resolution mechanism to run before relief can be restored or sanctions escalated, prolonging economic uncertainty regardless of Iran's actual conduct.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_civilian_economy, payer,
    moderate, biographical, trapped, national).

% A state or administration that judges the arrangement to have failed and wants to exit unilaterally is structurally denied a clean exit under this reading — withdrawal is treated as a breach of binding obligation rather than a legitimate exercise of provisional discretion, which is precisely the reading this constraint forecloses for them.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, unilateral_withdrawal_advocates, payer,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, unilateral_withdrawal_advocates, excluded).

% Neighboring states with independent security concerns about Iranian enrichment are not signatories and have no seat in the consensus-based modification process. They bear the security consequences of a treaty architecture designed around P5+1-plus-Iran consensus, with no formal channel to trigger reassessment even if regional threat perception changes sharply.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, regional_rival_states, payer,
    powerful, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jcpoa_treaty_bindingness__binding_multilateral_reading, regional_rival_states, excluded).

% Domestic political factions skeptical of any deal with the P5+1 are not party to the international consensus mechanism and would object to continued binding obligations that constrain enrichment capacity regardless of shifting domestic politics, but their objection has no formal standing in the treaty's modification procedure.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, iranian_hardline_factions, excluded,
    organized, biographical, trapped, national).

% Scholars and tribunals assess whether the JCPOA's structure (a UNSC Resolution 2231 annex, not a ratified treaty) actually supports the binding-multilateral characterization, or whether that characterization overstates the instrument's formal legal status relative to competing readings.
narrative_ontology:constraint_stakeholder(jcpoa_treaty_bindingness__binding_multilateral_reading, international_law_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, verifiable mechanism for reducing proliferation risk while restoring Iran's access to international commerce, with dispute resolution and phased snapback replacing unilateral judgment calls by any single party — solving the genuine coordination problem of how mutually distrustful parties enforce compliance without any one party being both accuser and judge.
% TRANSFER_FUNCTION: Moves sanctions relief and international economic reintegration to Iran conditional on IAEA-verified enrichment limits, while moving diplomatic leverage and enforcement discretion to the P5+1/EU coordinating structure; when disputes arise, the transfer of consequence (snapback, relief continuation) is gated through the multilateral process rather than flowing directly from any single party's determination.
% ABSENT_VOICES: Regional rival states with acute security stakes in Iranian enrichment levels have no seat in the Joint Commission or dispute resolution mechanism. Iranian domestic factions opposed to constraint are excluded from the international consensus process entirely. Both would object to the binding-multilateral reading's core claim that only the signatory consensus body can legitimately modify or dissolve the arrangement.
% DISAPPEARANCE_RATIONALE: If the binding-multilateral reading collapsed overnight (replaced by unilateral or graduated readings), the snapback mechanism would lose its consensus-gating function, IAEA verification findings would lose their treaty-anchored legal weight, EU coordination efforts (INSTEX and successors) would lose their claim to legal cover against extraterritorial sanctions, and any single signatory could exit or reimpose sanctions without running the dispute resolution track — the entire multilateral enforcement architecture depends on this reading holding.
% FOUNDING_PROBLEM: In 2015, the P5+1 and Iran needed a verifiable mechanism to prevent nuclear weapons proliferation while avoiding both an unconstrained Iranian nuclear program and a unilateral military or sanctions response that any single aggrieved party could trigger without international buy-in.
% FOUNDING_PROBLEM_CORROBORATION: EU coordinating powers and IAEA officials attest the founding problem remains live — verification and proliferation-risk management are ongoing needs. Independent nonproliferation scholars outside any signatory government (e.g., Arms Control Association analyses) corroborate that enrichment-limiting verification is still functionally necessary. However, the 2018 US withdrawal and subsequent unilateral sanctions reimposition were accomplished entirely outside this reading's consensus mechanism, which the transactional_provisional_reading's advocates cite as evidence the binding-multilateral framing was never operative in practice for the party that mattered most for sanctions relief.
narrative_ontology:disappearance_verdict(jcpoa_treaty_bindingness__binding_multilateral_reading, world_rearranges).
narrative_ontology:founding_problem_status(jcpoa_treaty_bindingness__binding_multilateral_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jcpoa_treaty_bindingness__binding_multilateral_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jcpoa_treaty_bindingness__binding_multilateral_reading, 0.42, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises sharply around T=32-48 (the 2018 withdrawal and subsequent maximum-pressure sanctions period) because the binding-multilateral reading's central claim — that exit must run through consensus — was directly tested and, from a strict compliance standpoint, breached without consensus. Theater ratio spikes in the same window: enormous diplomatic activity (E3 statements, Joint Commission meetings, INSTEX creation) continued while the substantive snapback-gating function the reading depends on had already been circumvented by unilateral US action. The subsequent decline (T=64-96) reflects partial re-stabilization as EU and remaining parties continued operating within the binding framework for the remaining signatories, even without US participation — extraction moderates but does not return to baseline because Iranian enrichment escalation in response to withdrawal introduced genuine dispute-resolution activity under the treaty's own terms.
 *
 * DIRECTIONALITY LOGIC:
 *   UNSC permanent members and EU coordinating powers sit near the beneficiary end: they administer the consensus mechanism and their institutional relevance is vindicated by the binding reading persisting. The IAEA and nonproliferation regime architects benefit analytically and institutionally without collecting direct rents — their stake is doctrinal rather than financial, which is why they are listed as beneficiaries rather than agenda-setters (they do not administer the mechanism, but its persistence vindicates their framework). Iranian civilian economy is the clearest target: trapped exit options, sanctions relief made contingent on a multilateral process it cannot unilaterally accelerate or resolve. Unilateral withdrawal advocates and regional rival states are targets of a different kind — not economically extracted from, but structurally denied standing to exit or intervene, which the reading treats as a feature (preventing unilateral defection) and they experience as extraction of agency.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — verifiable, multilaterally-legitimated nonproliferation enforcement — remains live by most outside accounts (arms control analysts, IAEA technical staff), which argues against pure mandatrophy. But the 2018 withdrawal demonstrates the binding reading's core enforcement claim (that exit requires consensus) did not bind the single most consequential party in practice. This is not classic mandatrophy (mandate outliving function) but rather a mandate whose enforceability was asymmetric from inception — binding for smaller/dependent parties, aspirational for the most powerful one. The tangled_rope classification captures this: genuine coordination function for verification and phased relief, combined with asymmetric extraction where the reading's bindingness fell hardest on Iran and least on the US.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    instrument_legal_status_ambiguity,
    'Is the JCPOA, as annexed to UNSC Resolution 2231, actually a binding treaty under international law, or a political commitment that a UNSC resolution merely endorses without converting into binding treaty obligation?',
    'International Court of Justice advisory opinion, or scholarly consensus among international law authorities on whether Resolution 2231''s endorsement converts a non-ratified political framework into binding obligation for non-consenting future administrations.',
    'If genuinely binding, unilateral withdrawal is a breach with real legal consequence, supporting this reading''s classification. If merely endorsed political commitment, the transactional_provisional_reading''s characterization is closer to the instrument''s actual legal status, and this constraint''s high suppression score would be overstated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(instrument_legal_status_ambiguity, conceptual, 'Whether the JCPOA''s formal legal status supports the binding-multilateral characterization this reading asserts.').

omega_variable(
    kernel_reading_selection_evidence,
    'What observable state practice would distinguish whether parties actually treated the JCPOA as binding (this reading), provisional (transactional_provisional_reading), or graduated (graduated_compliance_reading) during the 2018-2023 crisis period?',
    'Comparative analysis of EU/E3 legal filings, Iranian AEOI compliance-reduction announcements framed as ''reciprocal remedial measures'' under Articles 26/36, and US State Department legal justifications for withdrawal — each party''s own legal framing reveals which reading it was operating under.',
    'If EU/E3 legal practice consistently invoked binding-treaty language while US practice invoked unilateral-provisional language, that would suggest the three readings were held simultaneously by different parties (supporting coexists_with relations) rather than one reading being objectively correct.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_evidence, empirical, 'What state practice during the 2018-2023 crisis reveals about which kernel reading each party actually operated under.').

omega_variable(
    asymmetric_enforceability_ambiguity,
    'Is the binding-multilateral reading''s asymmetric enforceability (binding on Iran and EU, aspirational for the US) a design flaw specific to this instrument''s lack of Senate ratification, or a general feature of any multilateral consensus mechanism when one party has sufficient unilateral power to defect at low cost?',
    'Comparative study of other multilateral nonproliferation instruments (NPT, Iran-specific vs. general) and whether ratified treaties with domestic legal force show more symmetric enforceability under unilateral-defection stress tests.',
    'If asymmetric enforceability is general to unequal-power multilateral consensus mechanisms, the tangled_rope classification generalizes to a wider class of treaties; if specific to JCPOA''s non-ratified status, the classification may be narrower than authored here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(asymmetric_enforceability_ambiguity, conceptual, 'Whether the enforcement asymmetry observed is instrument-specific or a general feature of unequal-power multilateral treaties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jcpoa_treaty_bindingness__binding_multilateral_reading, 0, 96).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jcpo_tr_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(jcpo_tr_t0, observed).
narrative_ontology:measurement(jcpo_tr_t16, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 16, 0.2).
narrative_ontology:measurement_basis(jcpo_tr_t16, observed).
narrative_ontology:measurement(jcpo_tr_t32, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 32, 0.55).
narrative_ontology:measurement_basis(jcpo_tr_t32, observed).
narrative_ontology:measurement(jcpo_tr_t48, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 48, 0.6).
narrative_ontology:measurement_basis(jcpo_tr_t48, observed).
narrative_ontology:measurement(jcpo_tr_t64, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 64, 0.42).
narrative_ontology:measurement_basis(jcpo_tr_t64, observed).
narrative_ontology:measurement(jcpo_tr_t80, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement_basis(jcpo_tr_t80, observed).
narrative_ontology:measurement(jcpo_tr_t96, jcpoa_treaty_bindingness__binding_multilateral_reading, theater_ratio, 96, 0.38).
narrative_ontology:measurement_basis(jcpo_tr_t96, observed).

% Extraction over time
narrative_ontology:measurement(jcpo_be_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(jcpo_be_t0, observed).
narrative_ontology:measurement(jcpo_be_t16, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 16, 0.32).
narrative_ontology:measurement_basis(jcpo_be_t16, observed).
narrative_ontology:measurement(jcpo_be_t32, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 32, 0.51).
narrative_ontology:measurement_basis(jcpo_be_t32, observed).
narrative_ontology:measurement(jcpo_be_t48, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 48, 0.58).
narrative_ontology:measurement_basis(jcpo_be_t48, observed).
narrative_ontology:measurement(jcpo_be_t64, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 64, 0.46).
narrative_ontology:measurement_basis(jcpo_be_t64, observed).
narrative_ontology:measurement(jcpo_be_t80, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement_basis(jcpo_be_t80, observed).
narrative_ontology:measurement(jcpo_be_t96, jcpoa_treaty_bindingness__binding_multilateral_reading, base_extractiveness, 96, 0.42).
narrative_ontology:measurement_basis(jcpo_be_t96, observed).

% Suppression requirement over time
narrative_ontology:measurement(jcpo_su_t0, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement_basis(jcpo_su_t0, observed).
narrative_ontology:measurement(jcpo_su_t16, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 16, 0.38).
narrative_ontology:measurement_basis(jcpo_su_t16, observed).
narrative_ontology:measurement(jcpo_su_t32, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 32, 0.62).
narrative_ontology:measurement_basis(jcpo_su_t32, observed).
narrative_ontology:measurement(jcpo_su_t48, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 48, 0.7).
narrative_ontology:measurement_basis(jcpo_su_t48, observed).
narrative_ontology:measurement(jcpo_su_t64, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 64, 0.58).
narrative_ontology:measurement_basis(jcpo_su_t64, observed).
narrative_ontology:measurement(jcpo_su_t80, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 80, 0.56).
narrative_ontology:measurement_basis(jcpo_su_t80, observed).
narrative_ontology:measurement(jcpo_su_t96, jcpoa_treaty_bindingness__binding_multilateral_reading, suppression_requirement, 96, 0.55).
narrative_ontology:measurement_basis(jcpo_su_t96, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jcpoa_treaty_bindingness__binding_multilateral_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__transactional_provisional_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, jcpoa_treaty_bindingness__graduated_compliance_reading).
narrative_ontology:affects_constraint(jcpoa_treaty_bindingness__binding_multilateral_reading, npt_verification_regime).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the jcpoa_treaty_bindingness kernel. The binding_multilateral_reading (this story) authors high suppression and moderate-to-high extraction concentrated around the 2018 withdrawal crisis, reflecting a reading in which unilateral exit is a breach. The transactional_provisional_reading authors the same historical events as legitimate exercise of built-in provisionality, with correspondingly lower ε. The graduated_compliance_reading models enforcement as continuously scaled to compliance rather than binary treaty-bound status, producing a smoother extraction trajectory without the sharp T=32-48 spike this story shows. All three share the same underlying historical record but diverge in ε and classification because they diverge in what obligation-structure they take the JCPOA to actually be — per the kernel/reading framework, these are three distinct constraints, not one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
