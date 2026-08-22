% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__judicial_activism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__judicial_activism_reading, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: wto_dsb_authority__judicial_activism_reading
 *   human_readable: WTO DSB Authority â Judicial Activism Reading
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint story instantiates the judicial_activism_reading of the
 *   contested kernel wto_dsb_authority. In this reading, WTO Dispute
 *   Settlement Body panels and the Appellate Body have exceeded their treaty
 *   mandate under the DSU by creating new obligations through interpretive
 *   driftâeffectively engaging in illegitimate judicial legislation. The
 *   constraint is the standing arrangement whereby panel reports, adopted by
 *   negative consensus, lock in expansive interpretations that constrain
 *   member-state regulatory autonomy beyond the scope of explicit treaty
 *   consent. The reading is one of three: the advisory_coordination_reading
 *   (panels as facilitators of negotiated settlement) and the
 *   binding_referee_reading (panels as legitimate binding interpreters of
 *   surrendered discretion) are sibling constraints.
 *
 * KEY AGENTS:
 *   - DSB adjudicative apparatus (panels/Appellate Body): agenda_setter and institutional beneficiaryâissues interpretations, adopts reports via negative consensus, authorizes retaliation, and accrues epistemic authority.
 *   - Complainant states leveraging expansive interpretation: beneficiaryâuses broad rulings to lock in market access and constrain trading-partner regulation.
 *   - International trade legal complex: beneficiaryâlawyers, academics, and arbitrators whose careers and authority depend on expansive jurisprudence.
 *   - Respondent member states: payerâbear sovereignty costs when domestic regulation is struck down by overbroad interpretations.
 *   - Developing country members: payerâlack capacity to resist or comply with obligations created through interpretive drift.
 *   - Reformist opposition states: payer with high power but constrained exitâactively resist through non-compliance and institutional blockage (e.g., Appellate Body appointments).
 *   - Critical trade scholars: observerâdocument and corroborate the gap between DSU negotiating intent and contemporary panel practice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.72).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.65).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO DSB Authority â Judicial Activism Reading").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, 'b13c1173-aaab-4e7b-a86a-223e04ecbed6').
narrative_ontology:cs_kernel_codification('b13c1173-aaab-4e7b-a86a-223e04ecbed6', formalized).
narrative_ontology:cs_authority_grounding('b13c1173-aaab-4e7b-a86a-223e04ecbed6', lineage).
narrative_ontology:cs_interpretation_layer_present('b13c1173-aaab-4e7b-a86a-223e04ecbed6').
narrative_ontology:cs_reading_relation('b13c1173-aaab-4e7b-a86a-223e04ecbed6', wto_dsb_authority__binding_referee_reading, influences).
narrative_ontology:cs_reading_relation('b13c1173-aaab-4e7b-a86a-223e04ecbed6', wto_dsb_authority__advisory_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('b13c1173-aaab-4e7b-a86a-223e04ecbed6', foundational, interpretive_drift_exceeds_dsu_text).
narrative_ontology:cs_axiom_status(interpretive_drift_exceeds_dsu_text, holdable).
narrative_ontology:cs_axiom_grounding('b13c1173-aaab-4e7b-a86a-223e04ecbed6', interpretive_drift_exceeds_dsu_text, conventional).
narrative_ontology:cs_axiom('b13c1173-aaab-4e7b-a86a-223e04ecbed6', foundational, judicial_retaliation_without_consent_illegitimate).
narrative_ontology:cs_axiom_status(judicial_retaliation_without_consent_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('b13c1173-aaab-4e7b-a86a-223e04ecbed6', judicial_retaliation_without_consent_illegitimate, deontological).
narrative_ontology:cs_reference_frame('b13c1173-aaab-4e7b-a86a-223e04ecbed6', member_state_consent_bound_dispute_resolution).
narrative_ontology:cs_drift_state('b13c1173-aaab-4e7b-a86a-223e04ecbed6', post_appellate_body_crisis, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b13c1173-aaab-4e7b-a86a-223e04ecbed6', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, dsb_adjudicative_apparatus).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, complainant_states_expansive_interpretation).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, international_trade_legal_complex).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, respondent_member_states).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, developing_country_members).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, reformist_opposition_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% WTO panels and the Appellate Body interpret treaty text, issue adopted reports via negative consensus, and authorize retaliation; they benefit from expanded institutional relevance, jurisprudential influence, and the epistemic authority of an ever-growing body of case law.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, dsb_adjudicative_apparatus, agenda_setter,
    institutional, generational, analytical, global).

% Member states that initiate disputes benefit from broad interpretations that lock in market access and constrain trading-partner regulatory space beyond the originally negotiated bargain.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, complainant_states_expansive_interpretation, beneficiary,
    powerful, biographical, mobile, global).

% Lawyers, academics, and arbitrators who build careers on expansive WTO jurisprudence, panel representation, and the maintenance of a dense, technical legal discourse that only insiders can fully navigate.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, international_trade_legal_complex, beneficiary,
    organized, biographical, mobile, global).

% States whose domestic regulations are challenged and restrained by panel reports that interpret obligations more broadly than the treaty text or negotiating history suggest they consented to; compliance or retaliation are costly, and exit from the WTO system is systemically prohibitive.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, respondent_member_states, payer,
    powerful, biographical, constrained, national).

% Members with limited legal capacity to resist broad interpretations in disputes or negotiations; they bear asymmetric compliance costs when panels create new obligations through interpretive drift, and lack resources to shape the jurisprudential agenda.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, developing_country_members, payer,
    moderate, generational, constrained, global).

% Systemic members that actively resist specific rulings, block Appellate Body appointments, or pursue DSU reform because they view interpretive drift as sovereignty overreach; their high global power does not translate to easy exit from the constraint because WTO membership remains strategically essential.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, reformist_opposition_states, payer,
    powerful, biographical, mobile, national).

% Academic and policy analysts who document the gap between DSU negotiating intent and contemporary panel practice, providing external corroboration of interpretive drift from a seat that neither collects from nor pays into the constraint.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, critical_trade_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a centralized, rules-based forum for resolving international trade disputes between sovereign states, reducing unilateral retaliation and lowering transaction costs for interpreting trade agreement obligations.
% TRANSFER_FUNCTION: Moves regulatory autonomy and policy discretion from respondent member states to the DSB adjudicative apparatus and complainant states, by interpreting existing treaty text as creating obligations that exceed what states explicitly negotiated and consented to.
% ABSENT_VOICES: Domestic regulatory agencies and civil society actors in respondent states whose public-interest regulations are preempted by broad DSB interpretations; developing-country negotiators who lack resources to contest expansive panel reasoning; alternative trade-law scholars who view the DSU as properly limited to consensual diplomatic facilitation rather than adversarial adjudication.
% DISAPPEARANCE_RATIONALE: If the interpretive drift and obligation-creation mechanism vanished, the existing stock of panel and Appellate Body jurisprudence would lose its precedential force; trade disputes would revert toward power-based bargaining or require explicit renegotiation of treaty text; the WTO's legalized dispute settlement model would contract toward its original consensual diplomatic function and member-state regulatory autonomy would expand.
% FOUNDING_PROBLEM: The absence of an effective, binding mechanism to resolve trade disputes between sovereign states without resorting to unilateral power-based retaliation, which undermined the credibility and stability of the multilateral trading system under GATT.
% FOUNDING_PROBLEM_CORROBORATION: Original DSU negotiators and diplomatic historians attest the intent was a carefully balanced system preserving member-state control over obligations; contemporary critics and some member-state delegations outside the trade-law complex attest the problem has been solved but the apparatus has hypertrophied beyond its mandate; retired ambassadors and independent legal historians provide corroboration from outside the benefiting parties.
narrative_ontology:disappearance_verdict(wto_dsb_authority__judicial_activism_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__judicial_activism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__judicial_activism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_dsb_authority__judicial_activism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__judicial_activism_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__judicial_activism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__judicial_activism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.72) is high because the constraint systematically transfers regulatory autonomy from respondent states to the adjudicative apparatus and complainant states through interpretive methods (e.g., dynamic treaty interpretation, broad necessity tests) that create obligations not found in the treaty text. Suppression (0.65) reflects the combination of negative-consensus adoption (structural barrier to rejecting reports) and the threat of authorized retaliation (cost of noncompliance). Theater ratio (0.45) captures the performative maintenance of 'neutral adjudication' and 'rule of law' rhetoric while the apparatus effectively legislates. Accessibility collapse (0.40) is moderate: alternatives such as bilateral negotiation, Article 25 arbitration, or regional dispute settlement exist but are fragmented and lack the enforcement machinery of the DSB. Resistance (0.70) is high and rising: the US blockade of Appellate Body appointments, explicit non-compliance by major members, and proliferating reform proposals all signal active rejection of the interpretive drift. The temporal series show extraction and theater accumulating from 1995 to the present, while suppression hardens as the apparatus resists reform.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat (DSB apparatus) and the beneficiary seats (complainant states, legal complex) should compute toward coordination-flavored types because they genuinely experience the system as a functional dispute-resolution mechanism that reduces trade conflict. The payer seats (respondent states, developing members, reformist opposition) should compute toward extraction-heavy types because they experience the same system as sovereignty loss through illegitimate lawmaking. The engine derives this divergence from identical structural data: the beneficiaries have low directionality (subsidized by the constraint) while the payers have high directionality (targets of extraction). The high resistance metric confirms that the divergence is not merely theoretical but politically contested.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are coded low-d: the DSB apparatus, complainant states, and the legal complex are structurally subsidized by the constraintâtheir authority, leverage, or revenue increases with the scope of interpretation. Victims are coded high-d: respondent states and developing members bear the extraction in the form of preempted regulation and compliance costs. Reformist opposition states are also high-d despite their global power because, within this specific constraint, they are targets of the drift and their exit is constrained by the systemic costs of WTO withdrawal. No directionality overrides are needed because the structural derivation from beneficiary/victim declarations plus exit options correctly captures these relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents two errors. First, it prevents mislabeling the constraint as a pure Snare: the DSB genuinely does coordinate dispute settlement and has reduced unilateral trade wars, so a coordination function is real. Second, it prevents mislabeling as a pure Rope: the asymmetric extraction is structurally present because the interpretive drift benefits some parties (complainants, the legal apparatus) at the expense of others (respondents, developing countries). The Mandatrophy test asks whether the founding problem is solved but the arrangement persists: here the founding problem (lack of dispute settlement) is arguably solved, but the apparatus has drifted into a new function (legislation). The contested status of the founding problem reflects this ambiguity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_method_legitimacy,
    'Do the interpretive methods employed by DSB panels (e.g., evolutionary interpretation, broad necessity analysis) fall within the legitimate scope of treaty interpretation under the VCLT and DSU, or do they constitute de facto lawmaking?',
    'Comparative legal analysis of DSU negotiating history against the complete corpus of adopted panel reports; ex post corroboration from original DSU negotiators on intended interpretive boundaries.',
    'If the methods are conventionally legitimate, extractiveness falls and the constraint shifts toward Rope; if they constitute lawmaking without consent, extraction is confirmed and the Tangled Rope/Snare boundary becomes salient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_method_legitimacy, conceptual, 'Whether panel interpretive methods are legitimate interpretation or legislation').

omega_variable(
    member_state_withdrawal_significance,
    'Does the blocking of Appellate Body appointments and selective non-compliance represent fatal resistance that will collapse the constraint, or adaptive pressure that will reform it into a new coordination mechanism?',
    'Tracking state behavior over the next WTO ministerial cycle: reconstitution of a binding appellate function versus permanent shift to non-binding or bilateral alternatives.',
    'If the constraint collapses, its classification becomes historically moot; if it reforms, the theater_ratio and suppression metrics must be re-evaluated under the new arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(member_state_withdrawal_significance, empirical, 'Whether current resistance terminates or transforms the constraint').

omega_variable(
    developing_country_benefit_ambiguity,
    'Do developing country members experience net harm from expansive interpretations, or do they benefit from strengthened multilateral rules that constrain powerful trading partners?',
    'Empirical assessment of developing-country win/loss rates in disputes and compliance costs relative to capacity-building benefits.',
    'If net beneficiaries, the victim set contracts and directionality for this group shifts toward symmetric or subsidized; if net harmed, extraction is confirmed.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(developing_country_benefit_ambiguity, empirical, 'Developing country net position under expansive DSB interpretation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dsb_ja_tr_t0, wto_dsb_authority__judicial_activism_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(wto_dsb_ja_tr_t5, wto_dsb_authority__judicial_activism_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(wto_dsb_ja_tr_t10, wto_dsb_authority__judicial_activism_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement(wto_dsb_ja_tr_t15, wto_dsb_authority__judicial_activism_reading, theater_ratio, 15, 0.35).
narrative_ontology:measurement(wto_dsb_ja_tr_t20, wto_dsb_authority__judicial_activism_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement(wto_dsb_ja_tr_t25, wto_dsb_authority__judicial_activism_reading, theater_ratio, 25, 0.43).
narrative_ontology:measurement(wto_dsb_ja_tr_t30, wto_dsb_authority__judicial_activism_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(wto_dsb_ja_be_t0, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(wto_dsb_ja_be_t5, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(wto_dsb_ja_be_t10, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(wto_dsb_ja_be_t15, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(wto_dsb_ja_be_t20, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 20, 0.65).
narrative_ontology:measurement(wto_dsb_ja_be_t25, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 25, 0.7).
narrative_ontology:measurement(wto_dsb_ja_be_t30, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 30, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(wto_dsb_ja_su_t0, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(wto_dsb_ja_su_t5, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 5, 0.48).
narrative_ontology:measurement(wto_dsb_ja_su_t10, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(wto_dsb_ja_su_t15, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 15, 0.6).
narrative_ontology:measurement(wto_dsb_ja_su_t20, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(wto_dsb_ja_su_t25, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 25, 0.65).
narrative_ontology:measurement(wto_dsb_ja_su_t30, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 30, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__judicial_activism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, advisory_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the wto_dsb_authority kernel. The natural-language label 'WTO DSB authority' conflates three structurally distinct claims: advisory coordination (low extraction), binding referee legitimacy (moderate extraction via surrendered discretion), and judicial activism (high extraction via interpretive drift). Each reading carries a different epsilon, beneficiary/victim structure, and classification. They form a constraint family linked by network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
