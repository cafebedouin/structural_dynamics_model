% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__judicial_activism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   human_readable: WTO DSB Judicial Activism (Judicial Legislation Reading)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint represents the 'judicial activism' reading of the WTO
 *   Dispute Settlement Body's (DSB) authority. In this reading, DSB panels
 *   and the Appellate Body have exceeded their mandate to interpret existing
 *   treaty law, effectively creating new obligations for member states
 *   through expansive interpretations. This is perceived as illegitimate
 *   judicial legislation, leading to active resistance to compliance,
 *   contestation of the treaty interpretation process itself, and a decline
 *   in the perceived legitimacy of the WTO's enforcement mechanisms. The
 *   claimed type is 'snare' because the coordination story (neutral dispute
 *   resolution) is seen as cover for an extractive process that imposes costs
 *   on targeted states without their consent, sustained by coercion and
 *   suppression of alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.85).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.75).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, snare).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO DSB Judicial Activism (Judicial Legislation Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, '6a617534-6a47-4a91-8528-dc481c9c1f22').
narrative_ontology:cs_kernel_codification('6a617534-6a47-4a91-8528-dc481c9c1f22', fixed_text).
narrative_ontology:cs_authority_grounding('6a617534-6a47-4a91-8528-dc481c9c1f22', extraction).
narrative_ontology:cs_interpretation_layer_present('6a617534-6a47-4a91-8528-dc481c9c1f22').
narrative_ontology:cs_reading_relation('6a617534-6a47-4a91-8528-dc481c9c1f22', wto_dsb_authority__binding_referee_reading, coexists_with).
narrative_ontology:cs_reading_relation('6a617534-6a47-4a91-8528-dc481c9c1f22', wto_dsb_authority__advisory_coordination_reading, coexists_with).
narrative_ontology:cs_axiom('6a617534-6a47-4a91-8528-dc481c9c1f22', foundational, ds_panels_exceed_mandate).
narrative_ontology:cs_axiom_status(ds_panels_exceed_mandate, holdable).
narrative_ontology:cs_axiom_grounding('6a617534-6a47-4a91-8528-dc481c9c1f22', ds_panels_exceed_mandate, conventional).
narrative_ontology:cs_axiom('6a617534-6a47-4a91-8528-dc481c9c1f22', foundational, new_obligations_require_member_consent).
narrative_ontology:cs_axiom_status(new_obligations_require_member_consent, holdable).
narrative_ontology:cs_axiom_grounding('6a617534-6a47-4a91-8528-dc481c9c1f22', new_obligations_require_member_consent, deontological).
narrative_ontology:cs_reference_frame('6a617534-6a47-4a91-8528-dc481c9c1f22', original_ds_understanding).
narrative_ontology:cs_drift_state('6a617534-6a47-4a91-8528-dc481c9c1f22', contemporary_trade_disputes, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('6a617534-6a47-4a91-8528-dc481c9c1f22', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, wto_secretariat_bureaucracy).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, powerful_member_states_litigating).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, member_states_targeted_by_rulings).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, developing_country_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administers the DSB process, drafts panel reports, and benefits from an expanded mandate for the WTO's dispute settlement function. Its institutional power and budget grow with the scope of DSB rulings.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, wto_secretariat_bureaucracy, agenda_setter,
    institutional, generational, constrained, global).

% Use the DSB to advance their trade interests, often against smaller states. They benefit from rulings that create new obligations for others, even if they sometimes face adverse rulings themselves. Their power allows them to selectively comply or retaliate.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, powerful_member_states_litigating, beneficiary,
    powerful, biographical, mobile, global).

% Bear the costs of compliance with rulings that they perceive as exceeding the original treaty text. They face economic penalties or authorized retaliation if they do not comply, leading to active resistance and questioning of the DSB's legitimacy.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, member_states_targeted_by_rulings, payer,
    moderate, immediate, constrained, national).

% Are particularly vulnerable to rulings that create new obligations, as they often lack the resources to comply or to effectively litigate. They perceive the DSB as a tool for powerful states to impose new trade rules without formal negotiation.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, developing_country_members, payer,
    powerless, generational, trapped, global).

% Analyze the legal and economic implications of DSB rulings, often highlighting instances of interpretive overreach and the impact on national sovereignty. They provide critical commentary on the legitimacy of the DSB's evolving role.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, trade_policy_analysts, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a forum for resolving trade disputes between member states, aiming to clarify treaty obligations and prevent unilateral trade actions.
% TRANSFER_FUNCTION: Transfers policy discretion and sovereignty from member states to the DSB panels, which then impose new trade obligations through expansive interpretations of existing treaties.
% ABSENT_VOICES: National legislatures and domestic constituencies, whose policy choices are constrained by DSB rulings without direct representation in the dispute settlement process, would object to the erosion of democratic accountability.
% DISAPPEARANCE_RATIONALE: If the DSB's judicial activism vanished, member states would regain full policy discretion over trade matters, leading to a potential increase in unilateral actions and bilateral negotiations, but also a restoration of national sovereignty over trade policy. The WTO's enforcement mechanism would be significantly weakened, forcing a renegotiation of its role.
% FOUNDING_PROBLEM: To provide a neutral, rules-based mechanism for resolving trade disputes, preventing unilateral protectionism, and ensuring predictable application of WTO agreements.
% FOUNDING_PROBLEM_CORROBORATION: The WTO Secretariat and some powerful member states argue the problem is live, requiring robust interpretation to adapt to new trade realities. Targeted member states and many trade policy analysts argue the original problem of unilateralism is largely solved, and the DSB's current activism creates new problems of legitimacy and sovereignty, as evidenced by increasing non-compliance and appeals to national interest.
narrative_ontology:disappearance_verdict(wto_dsb_authority__judicial_activism_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__judicial_activism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__judicial_activism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(wto_dsb_authority__judicial_activism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__judicial_activism_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because the DSB's rulings impose significant economic and policy costs on targeted member states, often without their explicit agreement to the expanded obligations. Suppression (0.75) is also high, as member states face authorized retaliation if they do not comply, and their ability to challenge the interpretive process is limited. Resistance (0.8) is substantial, manifesting as non-compliance, blocking of Appellate Body appointments, and calls for WTO reform. Theater ratio (0.4) reflects that while some genuine dispute resolution occurs, a significant portion of the DSB's activity is seen as legitimizing an overreaching interpretive agenda.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the WTO Secretariat and powerful litigating states, the DSB is a necessary, evolving mechanism for effective global trade governance. From the perspective of targeted and developing country members, it is an extractive mechanism that undermines national sovereignty and imposes costs through judicial overreach. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The WTO Secretariat bureaucracy and powerful member states that successfully litigate are beneficiaries, as their influence and policy goals are advanced through DSB rulings. Member states targeted by rulings and developing country members are victims, bearing the costs of compliance with what they view as illegitimate obligations. Trade policy analysts act as observers, documenting the perceived overreach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_drift_threshold,
    'At what point does ''legitimate treaty interpretation'' become ''judicial legislation''?',
    'Comparative analysis of national and international legal systems'' approaches to judicial review and legislative intent, coupled with a formal textual analysis of the WTO agreements'' dispute settlement provisions.',
    'A clear definition would help distinguish legitimate rulings from overreach, potentially reclassifying some ''extractive'' rulings as ''coordinative'' if they fall within a newly defined legitimate interpretive scope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_drift_threshold, conceptual, 'Distinguishing legitimate interpretation from illegitimate judicial legislation.').

omega_variable(
    member_state_consent_to_drift,
    'To what extent have member states implicitly or explicitly consented to the DSB''s interpretive drift through continued participation and non-amendment of the DSU?',
    'Analysis of state practice, diplomatic statements, and voting records within the WTO General Council regarding DSB reports and proposed reforms.',
    'If implicit consent is established, the ''extraction'' might be re-read as a ''coordination cost'' of an evolving system. If not, the ''snare'' classification is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(member_state_consent_to_drift, empirical, 'Implicit consent to DSB''s evolving interpretive role.').

omega_variable(
    alternative_dispute_resolution_viability,
    'What are the viable alternatives for dispute resolution if member states withdraw from the DSB''s binding enforcement mechanism?',
    'Modeling the impact of a shift to bilateral arbitration, non-binding mediation, or a return to unilateral trade measures, assessing their efficiency, fairness, and impact on global trade stability.',
    'If viable, less extractive alternatives exist, the ''snare'' classification is reinforced. If alternatives lead to greater instability, the DSB''s coordination function (even if extractive) gains relative justification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_dispute_resolution_viability, empirical, 'Viability of alternative trade dispute resolution mechanisms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__judicial_activism_reading, theater_ratio, 1995, 0.1).
narrative_ontology:measurement(wto__tr_t2000, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(wto__tr_t2005, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(wto__tr_t2010, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2010, 0.28).
narrative_ontology:measurement(wto__tr_t2015, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2015, 0.35).
narrative_ontology:measurement(wto__tr_t2020, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2020, 0.38).
narrative_ontology:measurement(wto__tr_t2024, wto_dsb_authority__judicial_activism_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 1995, 0.5).
narrative_ontology:measurement(wto__be_t2000, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(wto__be_t2005, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2005, 0.7).
narrative_ontology:measurement(wto__be_t2010, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2010, 0.78).
narrative_ontology:measurement(wto__be_t2015, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2015, 0.82).
narrative_ontology:measurement(wto__be_t2020, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2020, 0.84).
narrative_ontology:measurement(wto__be_t2024, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(wto__su_t2000, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2000, 0.5).
narrative_ontology:measurement(wto__su_t2005, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(wto__su_t2010, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2010, 0.68).
narrative_ontology:measurement(wto__su_t2015, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2015, 0.72).
narrative_ontology:measurement(wto__su_t2020, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2020, 0.74).
narrative_ontology:measurement(wto__su_t2024, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__judicial_activism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__advisory_coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the WTO DSB's authority. This 'judicial activism' reading emphasizes the DSB's overreach and creation of new obligations, contrasting with the 'binding referee' reading (legitimate binding rulings) and the 'advisory coordination' reading (non-binding expert opinions).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
