% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__advisory_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__advisory_coordination_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: wto_dsb_authority__advisory_coordination_reading
 *   human_readable: WTO DSB Advisory Coordination Function
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint story captures the advisory_coordination_reading of the
 *   WTO DSB authority kernel. Under this reading, DSB panels function as
 *   expert advisory bodies whose reports provide authoritative legal analysis
 *   to facilitate negotiated settlements between member states. Member states
 *   retain ultimate policy discretion — they are not bound to comply with
 *   panel findings but use them as inputs to bilateral or multilateral
 *   negotiations. The constraint is the panel process itself: its procedures,
 *   the legal analysis it produces, and the expectation that states will
 *   engage seriously with its findings. Extraction is low (0.12) because the
 *   system is funded collectively and its outputs are non-binding;
 *   suppression is minimal (0.08) because states can reject or negotiate
 *   around adverse findings; theater is low (0.15) because the panel process
 *   performs its stated function of expert legal analysis. The claimed type
 *   is rope — genuine coordination solving the collective action problem of
 *   credible dispute characterization without coercive enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__advisory_coordination_reading, 0.12).
domain_priors:suppression_score(wto_dsb_authority__advisory_coordination_reading, 0.08).
domain_priors:theater_ratio(wto_dsb_authority__advisory_coordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(wto_dsb_authority__advisory_coordination_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__advisory_coordination_reading, rope).
narrative_ontology:human_readable(wto_dsb_authority__advisory_coordination_reading, "WTO DSB Advisory Coordination Function").
narrative_ontology:topic_domain(wto_dsb_authority__advisory_coordination_reading, "international_law/trade_governance/institutional_legitimacy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__advisory_coordination_reading, 'a9210903-be85-4d57-8aaa-c3aa53709b15').
narrative_ontology:cs_kernel_codification('a9210903-be85-4d57-8aaa-c3aa53709b15', formalized).
narrative_ontology:cs_authority_grounding('a9210903-be85-4d57-8aaa-c3aa53709b15', lineage).
narrative_ontology:cs_interpretation_layer_present('a9210903-be85-4d57-8aaa-c3aa53709b15').
narrative_ontology:cs_reading_relation('a9210903-be85-4d57-8aaa-c3aa53709b15', wto_dsb_authority__binding_referee_reading, coexists_with).
narrative_ontology:cs_reading_relation('a9210903-be85-4d57-8aaa-c3aa53709b15', wto_dsb_authority__judicial_activism_reading, coexists_with).
narrative_ontology:cs_axiom('a9210903-be85-4d57-8aaa-c3aa53709b15', foundational, state_sovereignty_preserved_in_dispute_settlement).
narrative_ontology:cs_axiom_status(state_sovereignty_preserved_in_dispute_settlement, holdable).
narrative_ontology:cs_axiom_grounding('a9210903-be85-4d57-8aaa-c3aa53709b15', state_sovereignty_preserved_in_dispute_settlement, conventional).
narrative_ontology:cs_axiom('a9210903-be85-4d57-8aaa-c3aa53709b15', foundational, panel_reports_are_advisory_not_binding).
narrative_ontology:cs_axiom_status(panel_reports_are_advisory_not_binding, holdable).
narrative_ontology:cs_axiom_grounding('a9210903-be85-4d57-8aaa-c3aa53709b15', panel_reports_are_advisory_not_binding, conventional).
narrative_ontology:cs_reference_frame('a9210903-be85-4d57-8aaa-c3aa53709b15', negotiated_settlement_primacy).
narrative_ontology:cs_drift_state('a9210903-be85-4d57-8aaa-c3aa53709b15', appellate_body_crisis_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a9210903-be85-4d57-8aaa-c3aa53709b15', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, member_states).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, disputing_parties).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__advisory_coordination_reading, wto_secretariat).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(wto_dsb_authority__advisory_coordination_reading, disputing_parties).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Created and fund the WTO dispute settlement system; retain ultimate policy discretion under DSU Article 3.2; use panel reports as expert inputs to negotiations rather than binding commands; can accept, reject, or negotiate around adverse findings without formal compliance proceedings.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, member_states, beneficiary,
    institutional, generational, arbitrage, global).

% Bring disputes to panels seeking authoritative legal analysis to strengthen negotiating position; bear litigation costs but gain credible third-party assessment that facilitates settlement; cannot easily exit the system without losing WTO membership benefits.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, disputing_parties, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__advisory_coordination_reading, disputing_parties, payer).

% Administers panel selection, proceedings, and report adoption; provides legal and procedural support; has no independent enforcement power; derives legitimacy from member state mandate and procedural neutrality.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, wto_secretariat, agenda_setter,
    institutional, generational, analytical, global).

% Serve as independent legal experts drafting advisory opinions; selected for legal competence and impartiality; their authority derives from professional reputation and procedural role, not institutional power; can decline appointments.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, panel_experts, observer,
    organized, biographical, mobile, global).

% Affected by trade dispute outcomes (industries, workers, consumers); have no standing in DSB proceedings; rely on their governments to represent interests; would object to outcomes that harm them but cannot directly influence panel composition or process.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__advisory_coordination_reading, domestic_constituencies, excluded,
    moderate, biographical, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a neutral, expert legal analysis of trade disputes that reduces information asymmetry between parties, creating a shared factual and legal baseline that makes negotiated settlements more likely and less costly than unilateral retaliation or prolonged conflict.
% TRANSFER_FUNCTION: Moves dispute resolution costs (panel expenses, legal fees) from unilateral state burden to shared WTO budget; moves legal interpretation from contested bilateral claims to neutral expert assessment; does not transfer policy authority — sovereignty over compliance remains with member states.
% ABSENT_VOICES: Domestic constituencies directly affected by trade measures (import-competing industries, export sectors, consumers) have no standing in DSB proceedings; developing country stakeholders often lack capacity to engage effectively; their interests are filtered through government representatives who may prioritize other objectives.
% DISAPPEARANCE_RATIONALE: If the DSB advisory function vanished, states would lose the primary neutral forum for legal analysis of trade disputes; bilateral negotiations would lack authoritative third-party assessment, increasing reliance on power-based bargaining and unilateral retaliation; the WTO's 'security and predictability' function would degrade significantly.
% FOUNDING_PROBLEM: Pre-WTO GATT dispute resolution lacked automatic panel adoption, allowing losing parties to block rulings; states needed a credible, automatic process for legal characterization of disputes that did not surrender sovereign compliance decisions.
% FOUNDING_PROBLEM_CORROBORATION: The continued use of DSB panels by all major trading powers (including the US during Appellate Body crisis) corroborates the live need for expert legal analysis; the 2020 US-EU joint statement on WTO reform reaffirms 'a well-functioning dispute settlement system' as essential while insisting on sovereignty preservation; academic literature (e.g., Davey 2020, Pauwelyn 2019) documents the ongoing demand for neutral legal baselines in trade disputes.
narrative_ontology:disappearance_verdict(wto_dsb_authority__advisory_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__advisory_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__advisory_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(wto_dsb_authority__advisory_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__advisory_coordination_reading, 0.12, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__advisory_coordination_reading_tests).
:- end_tests(wto_dsb_authority__advisory_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect the advisory_coordination_reading's structural assessment: the DSB panel system solves a real coordination problem (credible legal baselines for negotiation) with minimal extraction. Member states are net beneficiaries — they gain dispute resolution infrastructure at shared cost while retaining sovereignty. No stakeholder is a net victim; even 'losing' parties gain valuable legal analysis. The slight rise in theater_ratio around 2019 reflects the Appellate Body crisis period when panel reports could not be appealed, increasing performative aspects, but the underlying advisory function persisted. Resistance (0.35) captures occasional political pushback against specific panel conclusions, not systemic resistance to the institution.
 *
 * PERSPECTIVAL GAP:
 *   The binding_referee_reading and judicial_activism_reading would compute markedly different seat classifications from the same structural data. For the binding_referee_reading, member_states would be targets (d near 1.0) because rulings create compliance obligations; for judicial_activism_reading, panel_experts would be agenda_setters extracting authority beyond mandate. This reading's structural data (low suppression, beneficiary declarations, non-binding outputs) produces a rope classification; the sibling readings' data would produce tangled_rope or snare. The divergence is the measurement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation: member_states and disputing_parties are declared beneficiaries → low d (near 0.0) because they receive coordination benefits without losing policy discretion. The wto_secretariat as agenda_setter has d ≈ 0.5 (symmetric) — it administers but does not extract. Panel_experts as observers have d = 0.0 (analytical seat). Domestic_constituencies as excluded have no structural position in the constraint — they are affected by downstream policy choices, not by the panel process itself. The engine will compute effective extraction χ accordingly: near-zero for beneficiaries, low for all seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (credible automatic legal characterization without surrendering sovereignty) remains live — corroborated by continued system use and reform discussions. No mandatrophy: the arrangement still solves its founding problem. The Appellate Body crisis (2019-present) represents a stress on the enforcement layer, not the advisory coordination function itself; panels continue to operate and reports continue to inform negotiations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    advisory_vs_binding_boundary,
    'Does the DSU treaty text structurally permit only an advisory reading, or does it contain genuine ambiguity that allows binding_referee_reading as a coherent alternative framework?',
    'Textual analysis of DSU Articles 3.2, 19.1, 21.1, 22.2, and 23.1; drafting history of the Uruguay Round negotiations; state practice in compliance proceedings 1995-2019.',
    'If the text forecloses binding_referee_reading, that sibling is a misreading rather than a competing framework; if ambiguity is genuine, both readings are holdable and the kernel has structural indeterminacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(advisory_vs_binding_boundary, conceptual, 'Whether the treaty text itself resolves the advisory/binding contest or leaves it open.').

omega_variable(
    appellate_body_crisis_impact,
    'Has the Appellate Body''s non-functionality (since Dec 2019) structurally shifted the DSB from advisory coordination toward de facto binding referee for appealed cases, or has it reinforced the advisory character by making panel reports final and unappealable?',
    'Analysis of state behavior in appealed disputes post-2019: do states treat panel reports as final binding determinations, or do they negotiate around them? MPIA (Multi-Party Interim Appeal Arrangement) participation patterns.',
    'If states treat unappealable panel reports as binding, this reading''s low-extraction claim is undermined for appealed cases; if they continue negotiating, the advisory coordination reading is empirically reinforced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(appellate_body_crisis_impact, empirical, 'Whether the Appellate Body crisis changed the constraint''s operational character.').

omega_variable(
    kernel_reading_framing_underdetermination,
    'Is the advisory_coordination_reading the only coherent framing of DSU Article 3.2''s sovereignty preservation, or does a ''sovereignty_within_rules'' framing exist that preserves sovereignty while accepting binding rulings on covered agreements?',
    'Compare this reading''s axioms against alternative interpretations of ''cannot add to or diminish rights and obligations'' — does it permit binding application of existing obligations while forbidding only judicial legislation?',
    'If a sovereignty_within_rules framing is coherent, the kernel has three structurally distinct readings (advisory, binding_referee, sovereignty_within_rules), not two; the binding_referee_reading may be a straw-man conflation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing_underdetermination, conceptual, 'Whether the declared sibling readings exhaust the coherent framings of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__advisory_coordination_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto_dsb_adv_coord_tr_t1995, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(wto_dsb_adv_coord_tr_t2000, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2000, 0.12).
narrative_ontology:measurement(wto_dsb_adv_coord_tr_t2005, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2005, 0.13).
narrative_ontology:measurement(wto_dsb_adv_coord_tr_t2010, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2010, 0.14).
narrative_ontology:measurement(wto_dsb_adv_coord_tr_t2015, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2015, 0.15).
narrative_ontology:measurement(wto_dsb_adv_coord_tr_t2019, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2019, 0.18).
narrative_ontology:measurement(wto_dsb_adv_coord_tr_t2024, wto_dsb_authority__advisory_coordination_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(wto_dsb_adv_coord_be_t1995, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 1995, 0.08).
narrative_ontology:measurement(wto_dsb_adv_coord_be_t2000, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2000, 0.1).
narrative_ontology:measurement(wto_dsb_adv_coord_be_t2005, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2005, 0.11).
narrative_ontology:measurement(wto_dsb_adv_coord_be_t2010, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2010, 0.12).
narrative_ontology:measurement(wto_dsb_adv_coord_be_t2015, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2015, 0.12).
narrative_ontology:measurement(wto_dsb_adv_coord_be_t2019, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2019, 0.13).
narrative_ontology:measurement(wto_dsb_adv_coord_be_t2024, wto_dsb_authority__advisory_coordination_reading, base_extractiveness, 2024, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(wto_dsb_adv_coord_su_t1995, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 1995, 0.05).
narrative_ontology:measurement(wto_dsb_adv_coord_su_t2000, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2000, 0.06).
narrative_ontology:measurement(wto_dsb_adv_coord_su_t2005, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2005, 0.07).
narrative_ontology:measurement(wto_dsb_adv_coord_su_t2010, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2010, 0.08).
narrative_ontology:measurement(wto_dsb_adv_coord_su_t2015, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2015, 0.08).
narrative_ontology:measurement(wto_dsb_adv_coord_su_t2019, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2019, 0.1).
narrative_ontology:measurement(wto_dsb_adv_coord_su_t2024, wto_dsb_authority__advisory_coordination_reading, suppression_requirement, 2024, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__advisory_coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_dsb_authority__advisory_coordination_reading, 0.08).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__advisory_coordination_reading, wto_dsb_authority__judicial_activism_reading).

% DUAL FORMULATION NOTE:
% This constraint (advisory_coordination_reading) and its siblings form the wto_dsb_authority kernel family. All three share the DSU treaty text as kernel but instantiate different constraints with different ε values: this reading ε≈0.12 (rope), binding_referee_reading ε≈0.45 (tangled_rope — coordination plus compliance extraction), judicial_activism_reading ε≈0.65 (snare — illegitimate extraction via interpretive drift). The upstream constraint (this reading) influences downstream siblings because the advisory function is the declared treaty basis that the other readings either build upon (binding_referee) or contest (judicial_activism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
