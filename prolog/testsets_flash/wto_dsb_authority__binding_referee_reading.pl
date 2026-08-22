% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wto_dsb_authority__binding_referee_reading, []).

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
 *   constraint_id: wto_dsb_authority__binding_referee_reading
 *   human_readable: WTO DSB Authority (Binding Referee Reading)
 *   domain: international_law/trade_governance/institutional_legitimacy
 *
 * SUMMARY:
 *   This constraint describes the authority of the WTO Dispute Settlement
 *   Body (DSB) as a 'binding referee,' where panels issue rulings that member
 *   states are legally obligated to implement, backed by the threat of
 *   authorized trade retaliation. This reading emphasizes the surrender of
 *   national policy discretion in favor of a rules-based multilateral trading
 *   system. The claimed type is 'tangled_rope' because it provides a genuine
 *   coordination function (dispute resolution) but involves significant,
 *   asymmetric extraction of sovereignty from states found in violation.
 *
 * KEY AGENTS:
 *   - wto_member_states_with_grievances: Primary beneficiary (organized/constrained)
 *   - wto_member_states_found_in_violation: Primary payer (organized/constrained)
 *   - wto_secretariat_and_panels: Agenda setter (institutional/identity_locked)
 *   - domestic_policy_autonomy: Payer (powerless/trapped)
 *   - global_trading_system: Beneficiary (institutional/analytical)
 *   - academic_legal_scholars: Observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.65).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.75).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO DSB Authority (Binding Referee Reading)").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance/institutional_legitimacy").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, '70b970de-5691-4889-af03-92da5dc0688a').
narrative_ontology:cs_kernel_codification('70b970de-5691-4889-af03-92da5dc0688a', fixed_text).
narrative_ontology:cs_authority_grounding('70b970de-5691-4889-af03-92da5dc0688a', lineage).
narrative_ontology:cs_interpretation_layer_present('70b970de-5691-4889-af03-92da5dc0688a').
narrative_ontology:cs_reading_relation('70b970de-5691-4889-af03-92da5dc0688a', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_reading_relation('70b970de-5691-4889-af03-92da5dc0688a', wto_dsb_authority__judicial_activism_reading, coexists_with).
narrative_ontology:cs_axiom('70b970de-5691-4889-af03-92da5dc0688a', foundational, treaty_obligations_are_binding).
narrative_ontology:cs_axiom_status(treaty_obligations_are_binding, holdable).
narrative_ontology:cs_axiom_grounding('70b970de-5691-4889-af03-92da5dc0688a', treaty_obligations_are_binding, deontological).
narrative_ontology:cs_axiom('70b970de-5691-4889-af03-92da5dc0688a', foundational, dispute_settlement_requires_enforceable_rulings).
narrative_ontology:cs_axiom_status(dispute_settlement_requires_enforceable_rulings, holdable).
narrative_ontology:cs_axiom_grounding('70b970de-5691-4889-af03-92da5dc0688a', dispute_settlement_requires_enforceable_rulings, instrumental).
narrative_ontology:cs_reference_frame('70b970de-5691-4889-af03-92da5dc0688a', rules_based_multilateralism).
narrative_ontology:cs_drift_state('70b970de-5691-4889-af03-92da5dc0688a', contemporary, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('70b970de-5691-4889-af03-92da5dc0688a', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, wto_member_states_with_grievances).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, global_trading_system).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, wto_member_states_found_in_violation).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, domestic_policy_autonomy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states benefit from the DSB's binding authority, as it provides a mechanism to enforce trade rules against other members, ensuring market access and fair competition. They rely on the DSB to resolve disputes and authorize retaliation when necessary.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_member_states_with_grievances, beneficiary,
    organized, biographical, constrained, global).

% These states bear the costs of DSB rulings, facing obligations to change domestic policies or suffer authorized trade retaliation. Their policy discretion within WTO-covered areas is significantly curtailed, and non-compliance is treated as a treaty violation.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_member_states_found_in_violation, payer,
    organized, biographical, constrained, global).

% The DSB panels and the WTO Secretariat administer the dispute settlement process, interpret treaty law, and issue binding rulings. Their institutional legitimacy is grounded in the member states' agreement to surrender policy discretion for a rules-based system.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_secretariat_and_panels, agenda_setter,
    institutional, generational, identity_locked, global).

% Represents the diminished capacity of member states to independently set domestic policy in areas covered by WTO agreements. This autonomy is surrendered as a cost of participating in the global trading system.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, domestic_policy_autonomy, payer,
    powerless, generational, trapped, national).
narrative_ontology:stakeholder_non_agent(wto_dsb_authority__binding_referee_reading, domestic_policy_autonomy).

% The overall system benefits from the predictability and enforceability provided by binding DSB rulings, which reduce trade friction and foster a stable environment for international commerce.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, global_trading_system, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(wto_dsb_authority__binding_referee_reading, global_trading_system).

% Analyze the legal implications and effectiveness of DSB rulings, often debating the extent of judicial power and its impact on national sovereignty. They provide critical commentary on the system's evolution.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, academic_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a rules-based mechanism for resolving trade disputes between sovereign states, preventing unilateral trade wars and ensuring a predictable environment for international commerce.
% TRANSFER_FUNCTION: Transfers policy discretion and the right to interpret trade law from individual member states to the WTO DSB, in exchange for a binding enforcement mechanism that benefits states with grievances.
% ABSENT_VOICES: Nationalist political factions and protectionist industries within member states, who would argue for greater domestic policy autonomy and less external judicial oversight, are often marginalized in the discourse of international trade law.
% DISAPPEARANCE_RATIONALE: If the DSB's binding authority vanished, the global trading system would revert to a power-based negotiation model, leading to increased unilateral protectionism, trade wars, and a significant decrease in global trade predictability and volume.
% FOUNDING_PROBLEM: The post-WWII international economic order lacked an effective, binding mechanism to resolve trade disputes, leading to a risk of protectionism and economic instability.
% FOUNDING_PROBLEM_CORROBORATION: Most WTO member states, particularly those that frequently use the DSB to resolve grievances, corroborate that the problem of preventing trade wars and ensuring a rules-based system remains live. Independent international relations scholars also attest to the ongoing need for such a mechanism.
narrative_ontology:disappearance_verdict(wto_dsb_authority__binding_referee_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__binding_referee_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__binding_referee_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wto_dsb_authority__binding_referee_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(wto_dsb_authority__binding_referee_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because states must alter domestic laws or face economic penalties, representing a significant cost to national sovereignty. Suppression (0.75) is high due to the treaty-based obligation and the credible threat of retaliation, which actively suppresses alternative policy choices. The theater ratio is low (0.1) as the DSB's function is largely effective and direct, with little performative maintenance. Accessibility collapse is high (0.7) because once a ruling is issued, the options for the violating state are severely limited. Resistance (0.4) is moderate, as states often challenge rulings but ultimately face strong pressure to comply.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states with grievances, the DSB is a vital 'rope' for fair trade. From the perspective of states found in violation, it operates as a 'snare' that extracts policy autonomy. The WTO Secretariat and panels view it as a 'tangled_rope' balancing coordination and enforcement. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The WTO Secretariat and panels, as agenda setters, are beneficiaries of the system's authority (low d). Member states with grievances are also beneficiaries, as the system works in their favor (low d). Member states found in violation are targets, bearing the costs of compliance or retaliation (high d). Domestic policy autonomy, as an abstract entity, is a target (high d).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to prevent trade wars and ensure a rules-based system remains live. The 'tangled_rope' classification prevents mislabeling it as pure extraction, acknowledging its genuine coordination function, while also recognizing the asymmetric costs imposed on violating states. The temporal measurements show a slight increase in extractiveness and suppression over time, indicating a hardening of the enforcement mechanism rather than mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extent_of_sovereignty_surrender,
    'To what extent does participation in the WTO and acceptance of DSB rulings genuinely constitute a ''surrender'' of sovereignty versus a voluntary pooling of sovereignty for mutual benefit?',
    'Comparative legal analysis of national constitutional frameworks and international law doctrines regarding treaty obligations and state sovereignty, alongside empirical studies of policy space changes in WTO member states.',
    'If it''s a genuine surrender, the extractiveness for violating states is higher. If it''s a pooling for mutual benefit, the ''payer'' aspect is mitigated by the ''beneficiary'' aspect of overall system stability, potentially lowering effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extent_of_sovereignty_surrender, conceptual, 'Ambiguity in the nature of sovereignty transfer to the WTO DSB.').

omega_variable(
    legitimacy_of_retaliation,
    'Is the authorization of trade retaliation by the DSB a legitimate enforcement mechanism or a coercive tool that disproportionately harms smaller states?',
    'Empirical studies on the effectiveness and equity of authorized retaliation, including analysis of its impact on developing countries and its role in achieving compliance versus exacerbating trade imbalances.',
    'If coercive and inequitable, the suppression metric for smaller states is effectively higher, pushing their seat classification closer to ''snare''. If legitimate and effective for compliance, it reinforces the ''tangled_rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_retaliation, empirical, 'Whether DSB-authorized retaliation is a fair enforcement tool.').

omega_variable(
    reading_divergence_on_mandate,
    'Is the ''binding referee'' reading of the DSB''s authority the intended and legitimate interpretation of the WTO agreements, or does it represent an overreach of judicial power?',
    'Analysis of the original negotiating history of the WTO agreements, subsequent state practice, and the legal arguments presented by proponents of the ''advisory coordination'' and ''judicial activism'' readings.',
    'If this reading is found to be an overreach, the constraint''s legitimacy is undermined, potentially increasing resistance and lowering the perceived coordination function, pushing it closer to a ''snare'' from the perspective of states challenging its authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_divergence_on_mandate, conceptual, 'Ambiguity regarding the DSB''s mandate and the legitimacy of its binding authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__binding_referee_reading, base_extractiveness, 1995, 0.55).
narrative_ontology:measurement(wto__be_t2000, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement(wto__be_t2005, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2005, 0.6).
narrative_ontology:measurement(wto__be_t2010, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(wto__be_t2015, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2015, 0.64).
narrative_ontology:measurement(wto__be_t2024, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__binding_referee_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(wto__su_t2000, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(wto__su_t2005, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2005, 0.7).
narrative_ontology:measurement(wto__su_t2010, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(wto__su_t2015, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2015, 0.74).
narrative_ontology:measurement(wto__su_t2024, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__binding_referee_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__judicial_activism_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, international_trade_agreements).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
