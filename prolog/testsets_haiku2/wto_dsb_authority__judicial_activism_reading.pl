% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__judicial_activism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: wto_dsb_authority__judicial_activism_reading
 *   human_readable: WTO DSB Judicial Activism: Treaty Mandate Exceeding
 *   domain: international_law/trade_governance
 *
 * SUMMARY:
 *   The World Trade Organization's Dispute Settlement Body (DSB) was created
 *   to resolve trade disputes through binding third-party rulings. This
 *   reading asserts that DSB panels have systematically exceeded their treaty
 *   mandate by creating new trade obligations through interpretive
 *   drift—reading into the WTO agreements obligations member states did not
 *   knowingly accept when they signed. The result is illegitimate judicial
 *   legislation: unelected tribunals imposing policy on sovereign democracies
 *   through text interpretation that goes beyond the negotiated agreement.
 *   Smaller states and domestic regulators experience this as a snare: they
 *   are bound by obligations the DSB's interpretation created, they lack
 *   power to block or overturn rulings, and the constraint persists through
 *   the enforcement of retaliation authorization against non-compliance. The
 *   structural asymmetry is severe: powerful states benefit from favorable
 *   interpretations and can absorb compliance costs; smaller states pay
 *   through forced domestic policy change.
 *
 * KEY AGENTS:
 *   - DSB panels and appellate body: institutional authority that interprets the treaty and issues binding rulings; insulated from direct accountability via consensus requirement for reversal
 *   - Enforcement-privileged states (major trading powers): benefit from favorable interpretations applied against competitors; have institutional capacity and resources to litigate and leverage precedent
 *   - Smaller trading states: bear asymmetric costs of interpretive drift; constrained exit options (cannot meaningfully retaliate against larger partners); forced to change domestic policy to comply with new obligations
 *   - Regulatory sovereignty defenders: governments whose domestic legislation (labor, environment, health, culture) is overridden by DSB rulings reading trade obligations into areas treaty text did not govern
 *   - States resisting the reading: explicit challengers documenting non-compliance and institutional withdrawal from DSB mechanisms
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__judicial_activism_reading, 0.68).
domain_priors:suppression_score(wto_dsb_authority__judicial_activism_reading, 0.71).
domain_priors:theater_ratio(wto_dsb_authority__judicial_activism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(wto_dsb_authority__judicial_activism_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__judicial_activism_reading, snare).
narrative_ontology:human_readable(wto_dsb_authority__judicial_activism_reading, "WTO DSB Judicial Activism: Treaty Mandate Exceeding").
narrative_ontology:topic_domain(wto_dsb_authority__judicial_activism_reading, "international_law/trade_governance").

domain_priors:requires_active_enforcement(wto_dsb_authority__judicial_activism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__judicial_activism_reading, 'ef8a7840-75d3-451a-896a-119d2ebabc1b').
narrative_ontology:cs_kernel_codification('ef8a7840-75d3-451a-896a-119d2ebabc1b', fixed_text).
narrative_ontology:cs_authority_grounding('ef8a7840-75d3-451a-896a-119d2ebabc1b', extraction).
narrative_ontology:cs_interpretation_layer_present('ef8a7840-75d3-451a-896a-119d2ebabc1b').
narrative_ontology:cs_reading_relation('ef8a7840-75d3-451a-896a-119d2ebabc1b', wto_dsb_authority__binding_referee_reading, forecloses).
narrative_ontology:cs_reading_relation('ef8a7840-75d3-451a-896a-119d2ebabc1b', wto_dsb_authority__advisory_coordination_reading, forecloses).
narrative_ontology:cs_axiom('ef8a7840-75d3-451a-896a-119d2ebabc1b', foundational, treaty_interpretation_bounded_by_text).
narrative_ontology:cs_axiom_status(treaty_interpretation_bounded_by_text, holdable).
narrative_ontology:cs_axiom_grounding('ef8a7840-75d3-451a-896a-119d2ebabc1b', treaty_interpretation_bounded_by_text, deontological).
narrative_ontology:cs_axiom('ef8a7840-75d3-451a-896a-119d2ebabc1b', foundational, member_state_consent_nonrevocable_absent_amendment).
narrative_ontology:cs_axiom_status(member_state_consent_nonrevocable_absent_amendment, holdable).
narrative_ontology:cs_axiom_grounding('ef8a7840-75d3-451a-896a-119d2ebabc1b', member_state_consent_nonrevocable_absent_amendment, deontological).
narrative_ontology:cs_reference_frame('ef8a7840-75d3-451a-896a-119d2ebabc1b', negotiated_treaty_scope_binding_only_explicit_obligations).
narrative_ontology:cs_drift_state('ef8a7840-75d3-451a-896a-119d2ebabc1b', contemporary_expanded_jurisprudence, gap(codification_collapse, substantial, false)).
narrative_ontology:cs_created_at('ef8a7840-75d3-451a-896a-119d2ebabc1b', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, dispute_panel_apparatus).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__judicial_activism_reading, enforcement_privileged_states).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, smaller_trading_states).
narrative_ontology:constraint_victim(wto_dsb_authority__judicial_activism_reading, regulatory_sovereignty_defenders).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, treaty_text_supremacy_doctrine).
narrative_ontology:constraint_vindicates(wto_dsb_authority__judicial_activism_reading, member_state_consent_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Appointed panelists and appellate body judges interpret and apply WTO treaty text to resolve trade disputes. They have authority to issue rulings that bind member states. Under this reading, they exercise that authority beyond the treaty's original scope, creating new obligations through interpretive drift—reading into the agreement obligations member states did not knowingly accept. They are insulated from direct accountability; their rulings can be overturned only by consensus of all parties to a dispute (virtually impossible).
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, dispute_settlement_body_panels, agenda_setter,
    institutional, generational, arbitrage, global).

% Major trading powers benefit from favorable DSB interpretations of trade liberalization obligations applied against smaller competitors. They have resources to litigate and institutional capacity to leverage DSB rulings against others. They can absorb the cost of compliance when the DSB rules their own practices lawful.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, enforcement_privileged_states, beneficiary,
    powerful, generational, arbitrage, global).

% Bear the cost of DSB-imposed obligations that read further than treaty text warrants. They have limited resources to litigate, limited ability to influence panel composition, and carry asymmetric retaliation risk—they cannot impose meaningful sanctions on larger trading partners for breach. They pay through forced policy changes, lost regulatory space, and authorized retaliation against their exports.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, smaller_trading_states, payer,
    moderate, biographical, constrained, global).

% Governments defending domestic policy space (labor standards, environmental protection, public health measures, cultural preservation) against DSB rulings that read trade obligations into areas the treaty text did not originally govern. They experience the constraint as the DSB expanding its mandate beyond the treaty to override domestic legislation elected representatives passed.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, regulatory_sovereignty_defenders, payer,
    organized, biographical, constrained, national).

% Delegates and governments that negotiated the original WTO agreements are not seated in DSB proceedings; their understanding of what the text was meant to bind is systematized only through expensive amicus submissions, if admitted at all. Under this reading, their intent—what they understood they were agreeing to—is overridden by panelists' evolving interpretation.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, treaty_drafters_intent_preservers, excluded,
    powerless, biographical, trapped, global).

% Member states that have explicitly challenged DSB rulings as exceeding the treaty mandate, withdrawn cooperation from consensus mechanisms, or threatened exit from the dispute system. They document the reading through non-compliance, institutional withdrawal, and legislative amendment to shield domestic law.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__judicial_activism_reading, states_resisting_dement, observer,
    moderate, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__judicial_activism_reading, enforcement_privileged_states).
narrative_ontology:fixing_cost_class(wto_dsb_authority__judicial_activism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The DSB provides a forum to resolve trade disputes through binding third-party adjudication, substituting for unilateral retaliation or bilateral negotiation. The stated coordination problem: trade disputes require authoritative, neutral resolution to prevent escalation and maintain predictability.
% TRANSFER_FUNCTION: Transfers policy discretion from elected sovereigns to appointed panelists; transfers the cost of newly-interpreted obligations to states the DSB's interpretation disfavors; transfers authority to issue binding rulings that override domestic law to an unelected tribunal.
% ABSENT_VOICES: Treaty drafters whose negotiated intent is not represented in DSB proceedings; smaller states and non-litigant states whose exports are affected by DSB precedent but who lack resources to participate in shaping it; domestic constituencies (workers, environmental advocates, cultural minorities) whose protection standards are overridden by DSB rulings.
% DISAPPEARANCE_RATIONALE: If the DSB's authority to issue binding rulings on treaty interpretation vanished, member states would revert to bilateral negotiation or unilateral retaliation for trade disputes. The WTO's binding dispute resolution would disappear; trade agreements would lose enforceability against the strongest parties; smaller states would lose the one asymmetry-reducing mechanism they have (binding rulings they can cite against larger partners). The entire institutional architecture of rules-based trade governance collapses.
% FOUNDING_PROBLEM: 1980s trade dispute escalation: bilateral disputes over tariffs and subsidies were resolved through unilateral retaliation or power-asymmetric negotiation, creating spiraling protectionism and uncertainty. The WTO was designed to replace this with binding, neutral, text-based dispute resolution that would constrain the strongest states and protect the weakest.
% FOUNDING_PROBLEM_CORROBORATION: The DSB apparatus and major trading powers attest that the founding problem remains live—that without binding authority, disputes would revert to unilateral retaliation. Smaller states and regulatory-sovereignty defenders attest the founding problem has been transmuted: the problem is no longer escalating retaliation but creeping judicial legislation that is now the source of illegitimate obligation. Independent trade law scholars document interpretive drift (e.g., the evolution of implicit obligations in services trade, investment protection, and environmental standards). The contestation itself is the signal: the two readings produce measurably different compliance behaviors and legitimacy claims.
narrative_ontology:disappearance_verdict(wto_dsb_authority__judicial_activism_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__judicial_activism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__judicial_activism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(wto_dsb_authority__judicial_activism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__judicial_activism_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness climbs from 0.41 to 0.68 across the interval, indicating that the DSB's effective extraction increases as more rulings accumulate and the interpretive precedent compounds. The increase plateaus at t=30, suggesting the constraint reaches a stable enforcement ceiling—further interpretive expansion meets sufficient resistance that extractiveness stabilizes (the plateauing theater ratio at 0.42 and suppression at 0.71 support this). Theater rises from 0.22 to 0.42, reflecting that an increasing share of DSB activity is devoted to interpreting the treaty beyond its text rather than applying agreed-upon rules. Suppression is high throughout (0.48→0.71) because the constraint persists through retaliation authorization and compliance pressure, not through voluntary acceptance. The resistance measurement (0.79) is the highest among all stakeholders, indicating that multiple seats actively contest the reading and refuse compliance on legitimacy grounds. This reading maps to a snare: there are identifiable victims (smaller states, sovereignty defenders), identifiable beneficiaries (enforcement-privileged states, the panel apparatus), and the constraint's persistence depends on coercive mechanisms (retaliation threat, compliance pressure) to suppress the alternatives (exiting the WTO, refusing to recognize DSB authority).
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (smaller_trading_states, regulatory_sovereignty_defenders) and the agenda_setter seat (dispute_settlement_body_panels) experience fundamentally different constraint types. Panels perceive themselves as applying law faithfully; payers perceive themselves as being subjected to illegitimate judicial legislation. The engine's per-seat classification will compute this divergence explicitly: from the panel's seat, the constraint may compute as rope (applying agreed rules, serving coordination); from the payer's seat, it computes as snare (coercive extraction through interpretive authority). The authored claim is snare (the reading's own verdict), which aligns with the payer's perception; this is not a claim/metric contradiction but a claim that reflects the reading's particular standpoint.
 *
 * DIRECTIONALITY LOGIC:
 *   DSB panels are institutional beneficiaries: they are the only seat that gains expanded authority and insulation from accountability (agenda_setter role, arbitrage exit—they can reinterpret the treaty without revision negotiation). Enforcement-privileged states are beneficiaries by alignment: favorable interpretations serve their interests, and they have power to resist unfavorable ones or leverage favorable precedent. Their powerful status and arbitrage-grade exit (they can exit the WTO or ignore rulings—the retaliation cost is absorbed) keep their directionality toward the low end (beneficiary). Smaller trading states are victims: they pay through forced policy change (identity_locked exit—domestic law is fused with regulatory legitimacy; constrained by power asymmetry—they cannot retaliate meaningfully; trapped in scope because WTO coverage is global). Their d is near 1.0 (full target). Regulatory sovereignty defenders occupy similar structural position: the constraint extracts from them by overriding their policy choices; their exit is identity_locked (sovereignty is central to democratic self-conception) and constrained by their inability to opt out of a global trading system. Directionality overrides are not needed here—the structural derivation (beneficiary/victim + power + exit) produces the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding_problem (1980s trade escalation via unilateral retaliation) is technically 'live' in the sense that bilateral disputes still occur. But the structural form of the problem has shifted: the constraint no longer prevents escalatory retaliation (it has actually replaced it with hierarchical imposition by a tribunal); instead, it has become the source of a new harm—illegitimate obligation. This reading asserts mandatrophy: the original problem the DSB was built to solve has been transmuted. The measurement plateau (extractiveness flattens at 0.68, theater stabilizes at 0.42, suppression plateaus at 0.71) combined with high resistance (0.79) suggests the constraint has exhausted its functional utility—it is no longer solving the stated problem effectively and is persisting through suppression rather than coordination. States withdrawing from the DSB mechanism or refusing to recognize certain rulings are the observable markers of mandatrophy: the arrangement no longer commands legitimacy as a solution to the original problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_drift_vs_faithful_application,
    'How much of the measured increase in extractiveness (0.41→0.68) represents genuine interpretive drift creating new obligations, versus faithful application of treaty provisions that were intentionally open-textured for flexibility?',
    'Comparative analysis of negotiating records, statements of intent from treaty drafters, and formal amicus briefs from non-litigant states documenting their understanding of the provisions in question. Expert review of specific DSB rulings for textual grounding (citations to treaty language vs. implied obligations).',
    'If the increase is drift, the judicial_activism reading is structurally sound and the constraint is a snare. If the increase is faithful application of flexible provisions, the binding_referee reading gains credibility and the constraint may compute as tangled_rope (coordination + legitimate enforcement). This is the core binary determining whether the reading''s core premise holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_drift_vs_faithful_application, empirical, 'Whether DSB interpretation exceeds treaty text or faithfully applies it.').

omega_variable(
    consent_legitimacy_vs_institutional_evolution,
    'Is legitimacy of DSB authority grounded in explicit member state consent to specific obligations at the moment of treaty signature, or does it derive from ongoing institutional acceptance and participation (even under protest)?',
    'Genealogy of DSB authority: trace whether member states understood themselves as granting open-ended interpretive authority or specific bounded authority at negotiation time; document the frequency and nature of protests vs. acquiescence in response to landmark rulings; analyze withdrawal behavior (full exit vs. strategic non-participation vs. targeted retaliation).',
    'If legitimacy requires static consent from negotiation time, the activism reading holds and the constraint is a snare with high mandatrophy risk (founding legitimacy has been breached). If legitimacy evolves through institutional practice, the binding_referee reading gains strength (the constraint is a legitimately evolving commitment system). This determines whether member state resistance is a signal of illegitimacy or a normal feature of institutional politics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_legitimacy_vs_institutional_evolution, conceptual, 'Whose understanding of authority legitimacy governs: treaty drafters'' original intent or the institution''s evolved practice?').

omega_variable(
    power_asymmetry_structural_necessity,
    'Would a truly neutral, text-based dispute system require no power asymmetries in outcome, or is the measured asymmetry (enforcement-privileged states benefit, smaller states pay) a structural feature of any binding adjudication system?',
    'Counterfactual analysis: examine disputes where the DSB ruled against powerful states and the compliance/retaliation dynamics; track whether smaller states ever successfully litigate against larger ones and achieve observable policy change; survey perception of ''fairness'' across power strata.',
    'If the asymmetry is contingent and could be eliminated by better institutional design, then the measured extraction is excess and the snare classification stands. If the asymmetry is inherent to binding adjudication (powerful states can always absorb or ignore costs better), then the constraint is structurally necessary for coordination and the binding_referee reading gains traction (the constraint is a legitimate tangled_rope, not a snare). This determines whether the victims'' experience reflects illegitimate extraction or the inevitable cost of having a binding system at all.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(power_asymmetry_structural_necessity, conceptual, 'Whether power asymmetries in DSB outcomes are contingent design failures or structural features of binding adjudication.').

omega_variable(
    alternative_dispute_mechanisms,
    'What would replace the DSB if this constraint were removed—bilateral negotiation, unilateral retaliation, or a differently-designed multilateral mechanism—and would the alternative impose lower or higher overall extraction?',
    'Historical analysis of pre-DSB trade dispute escalation; scenario modeling of likely defection patterns if DSB enforcement were withdrawn; survey of member states'' stated preferences for alternative systems.',
    'If the alternative would be worse (higher extraction, more escalation), the DSB may be the least-bad option despite its activistic drift—the constraint would remain a snare but a necessary one (gain_flow captures this via fixing_cost=prohibitive). If an alternative mechanism could achieve similar coordination with lower extraction, the DSB is contingently extractive (not a necessary snare). This determines whether the constraint is a true snare (persistent because alternatives are suppressed) or a costly-but-necessary coordination mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_dispute_mechanisms, conceptual, 'Whether the DSB is the least-bad available dispute mechanism or a contingently extractive alternative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__judicial_activism_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t0, wto_dsb_authority__judicial_activism_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement_basis(wto__tr_t0, observed).
narrative_ontology:measurement(wto__tr_t5, wto_dsb_authority__judicial_activism_reading, theater_ratio, 5, 0.26).
narrative_ontology:measurement_basis(wto__tr_t5, observed).
narrative_ontology:measurement(wto__tr_t10, wto_dsb_authority__judicial_activism_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(wto__tr_t10, observed).
narrative_ontology:measurement(wto__tr_t15, wto_dsb_authority__judicial_activism_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement_basis(wto__tr_t15, observed).
narrative_ontology:measurement(wto__tr_t20, wto_dsb_authority__judicial_activism_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement_basis(wto__tr_t20, observed).
narrative_ontology:measurement(wto__tr_t25, wto_dsb_authority__judicial_activism_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(wto__tr_t25, observed).
narrative_ontology:measurement(wto__tr_t30, wto_dsb_authority__judicial_activism_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(wto__tr_t30, observed).
narrative_ontology:measurement(wto__tr_t35, wto_dsb_authority__judicial_activism_reading, theater_ratio, 35, 0.42).
narrative_ontology:measurement_basis(wto__tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(wto__be_t0, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 0, 0.41).
narrative_ontology:measurement_basis(wto__be_t0, observed).
narrative_ontology:measurement(wto__be_t5, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 5, 0.46).
narrative_ontology:measurement_basis(wto__be_t5, observed).
narrative_ontology:measurement(wto__be_t10, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(wto__be_t10, observed).
narrative_ontology:measurement(wto__be_t15, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 15, 0.58).
narrative_ontology:measurement_basis(wto__be_t15, observed).
narrative_ontology:measurement(wto__be_t20, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement_basis(wto__be_t20, observed).
narrative_ontology:measurement(wto__be_t25, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 25, 0.66).
narrative_ontology:measurement_basis(wto__be_t25, observed).
narrative_ontology:measurement(wto__be_t30, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(wto__be_t30, observed).
narrative_ontology:measurement(wto__be_t35, wto_dsb_authority__judicial_activism_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(wto__be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t0, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(wto__su_t0, observed).
narrative_ontology:measurement(wto__su_t5, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement_basis(wto__su_t5, observed).
narrative_ontology:measurement(wto__su_t10, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 10, 0.61).
narrative_ontology:measurement_basis(wto__su_t10, observed).
narrative_ontology:measurement(wto__su_t15, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(wto__su_t15, observed).
narrative_ontology:measurement(wto__su_t20, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 20, 0.69).
narrative_ontology:measurement_basis(wto__su_t20, observed).
narrative_ontology:measurement(wto__su_t25, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(wto__su_t25, observed).
narrative_ontology:measurement(wto__su_t30, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(wto__su_t30, observed).
narrative_ontology:measurement(wto__su_t35, wto_dsb_authority__judicial_activism_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(wto__su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__judicial_activism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(wto_dsb_authority__judicial_activism_reading, 0.14).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__binding_referee_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, national_regulatory_sovereignty_erosion).
narrative_ontology:affects_constraint(wto_dsb_authority__judicial_activism_reading, trade_power_asymmetry_institutionalization).

% DUAL FORMULATION NOTE:
% This story and its sibling readings (binding_referee_reading, advisory_coordination_reading) decompose the contested WTO DSB authority kernel into three structurally distinct constraints. The kernel is fixed—the DSB's authority to interpret and apply WTO treaties—but each reading produces a different constraint with different ε values, different victim/beneficiary structures, and different computed types. The readings are not harmonized or averaged; each is authored as a complete constraint that would be authored exactly the same way if it were alone. The network edges model that each reading's classification depends on understanding the alternatives: the judicial_activism reading's snare classification is strengthened or weakened by whether the alternatives (binding_referee as legitimate coordination, advisory_coordination as truly cooperative) are empirically true. All three are linked for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wto_dsb_authority__judicial_activism_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
