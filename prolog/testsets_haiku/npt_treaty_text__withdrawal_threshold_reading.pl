% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__withdrawal_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__withdrawal_threshold_reading, []).

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
 *   constraint_id: npt_treaty_text__withdrawal_threshold_reading
 *   human_readable: NPT Article X Withdrawal Threshold Interpretation (Sovereignty-Preservation Reading)
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   The NPT's Article X withdrawal clause contains an irreducible textual
 *   ambiguity: it permits withdrawal if 'extraordinary events related to the
 *   subject matter of this Treaty have jeopardized the supreme interests of
 *   [a party's] country.' The question of what threshold this requires — how
 *   high the bar must be set — has no settled answer in the treaty text
 *   itself. This constraint story instantiates the SOVEREIGNTY-PRESERVATION
 *   READING: the view that Article X's extraordinary-events clause operates
 *   as a low threshold, protecting NNWS exit credibility and preserving their
 *   leverage to bargain for disarmament. This reading benefits states near
 *   the nuclear threshold (Iran, Japan, South Korea) by keeping their
 *   withdrawal threat credible as a negotiating tool. It imposes costs on
 *   regime-stability advocates (NWS and allied NNWS) by weakening their
 *   enforcement capacity: if withdrawal is easy, the non-proliferation
 *   bargain rests on voluntary compliance rather than binding obligation.
 *   North Korea's 2003 withdrawal — treated by the regime as procedurally
 *   acceptable — functionally instantiated this reading without resolving the
 *   underlying interpretation dispute. The constraint is claimed as tangled
 *   rope: it coordinates non-proliferation (the shared problem the NPT
 *   solves) while asymmetrically extracting from regime-stability maintainers
 *   and benefiting exit-credibility seekers. This reading is one of three
 *   live interpretations of the same kernel (NPT Article X text); the sibling
 *   readings (NWS reading, NNWS disarmament reading) embed different
 *   thresholds and different beneficiary structures.
 *
 * KEY AGENTS:
 *   - Threshold states (Iran, Japan, South Korea): benefit from low-threshold reading; preserve exit option credibility
 *   - NWS collective (USA, Russia, China): bearers of regime-stability costs; prefer high-threshold reading
 *   - Depositary states (Russia, UK, USA): agenda-setters; interpret and accept/contest withdrawal notices
 *   - North Korea: precedent-setter; withdrew in 2003 under low-threshold logic, now external to treaty
 *   - NPT Review Conferences: observational venue where the reading dispute is aired but not resolved
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.58).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.47).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.47).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "NPT Article X Withdrawal Threshold Interpretation (Sovereignty-Preservation Reading)").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, '8189b9f9-694b-447a-a531-4728fd6aedc2').
narrative_ontology:cs_kernel_codification('8189b9f9-694b-447a-a531-4728fd6aedc2', fixed_text).
narrative_ontology:cs_authority_grounding('8189b9f9-694b-447a-a531-4728fd6aedc2', distributed).
narrative_ontology:cs_reading_relation('8189b9f9-694b-447a-a531-4728fd6aedc2', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('8189b9f9-694b-447a-a531-4728fd6aedc2', npt_treaty_text__nnws_reading, influences).
narrative_ontology:cs_axiom('8189b9f9-694b-447a-a531-4728fd6aedc2', foundational, nnws_sovereignty_exit_preservation).
narrative_ontology:cs_axiom_status(nnws_sovereignty_exit_preservation, holdable).
narrative_ontology:cs_axiom_grounding('8189b9f9-694b-447a-a531-4728fd6aedc2', nnws_sovereignty_exit_preservation, deontological).
narrative_ontology:cs_axiom('8189b9f9-694b-447a-a531-4728fd6aedc2', secondary, conditional_restraint_credibility).
narrative_ontology:cs_axiom_status(conditional_restraint_credibility, holdable).
narrative_ontology:cs_axiom_grounding('8189b9f9-694b-447a-a531-4728fd6aedc2', conditional_restraint_credibility, instrumental).
narrative_ontology:cs_reference_frame('8189b9f9-694b-447a-a531-4728fd6aedc2', conditional_bargain_framework).
narrative_ontology:cs_drift_state('8189b9f9-694b-447a-a531-4728fd6aedc2', post_north_korea_withdrawal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8189b9f9-694b-447a-a531-4728fd6aedc2', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, threshold_states_with_exit_credibility).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, non_aligned_states_asserting_sovereignty).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, regime_stability_advocate_states).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, nws_collective_enforcement_capacity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, iran_nuclear_program_context).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States like Iran, Japan, and South Korea sit near the nuclear threshold. They benefit from a low withdrawal threshold because it keeps their exit option credible: if NWS fail to pursue disarmament or if regional security threats escalate, they can credibly threaten to withdraw and pursue nuclear weapons. This threat is their primary negotiating leverage for extracting security guarantees and disarmament progress. Their constraint on non-proliferation is conditional — it holds only as long as the bargain holds, and the low threshold preserves that conditionality. Without credible exit, they become trapped in a one-way restraint.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, threshold_states_with_exit_credibility, beneficiary,
    moderate, generational, constrained, global).

% Major NWS (USA, Russia, China) and allied NNWS (Germany, Japan as regime supporter) have structured their security policies and diplomatic standing around maintaining the non-proliferation regime. A low withdrawal threshold undermines their enforcement capacity: each regional crisis invites a withdrawal threat they cannot credibly block without appearing coercive. Their identity as regime-maintainers depends on the treaty functioning as binding; a low threshold converts it to voluntary coordination, weakening their deterrent authority. They cannot exit the regime themselves without losing credibility, so they are identity-locked into bearing the cost of weak enforcement.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, regime_stability_advocate_states, payer,
    institutional, civilizational, identity_locked, global).

% Withdrew from the treaty in 2003, citing Article X's extraordinary-events clause, and subsequently pursued nuclear weapons development to completion. Their withdrawal is the central precedent: did it demonstrate that the low threshold is operationally real, or was it an exploitation of a loophole that should have been closed? The regime-stability reading holds that NK's withdrawal should have been rejected; the sovereignty-preservation reading holds that NK correctly invoked its rights. Either way, NK is no longer a party and cannot negotiate the threshold clarification. Their absence from the current dispute is the dispute's clearest evidence of how much the reading contest matters: a state successfully left the regime by invoking this reading.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, north_korea, excluded,
    moderate, biographical, trapped, global).

% The depositary powers (USA, Russia, UK) and the broader NWS collective (including China) operate the non-proliferation regime through procedural enforcement: they accept or contest withdrawal notices, they impose sanctions for non-compliance, they negotiate disarmament timelines. A low withdrawal threshold weakens this capacity because it makes withdrawal claims harder to credibly reject without appearing coercive. The collective's identity is fused with regime maintenance; they cannot exit without losing geopolitical standing. Their cost is the erosion of enforcement credibility and the shifting of the regime from binding constraint to voluntary cooperation.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, nws_collective_enforcement_capacity, payer,
    institutional, civilizational, identity_locked, global).

% Iran has invoked and threatened Article X withdrawal under this reading's logic (2005, 2020). Their stated grounds: unmet disarmament commitments by NWS, Israeli/US threats to their security, the collapse of the JCPOA. Under the low-threshold reading, these circumstances justify withdrawal as a plausible exercise of rights. Under the high-threshold reading, they do not meet the bar of existential threat and the claim would be rejected. The low threshold keeps Iran's withdrawal threat credible, which is their primary negotiating tool for extracting security guarantees and disarmament progress. A high threshold would eliminate this leverage.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, iran_nuclear_program_context, beneficiary,
    moderate, biographical, constrained, regional).

% Russia, UK, and USA formally receive withdrawal notices under Article X and must decide whether to accept or contest them. They are the operational agenda-setters: their interpretation practice IS the de facto threshold. By accepting North Korea's withdrawal in 2003, they instantiated the low threshold in practice, even though the interpretation remains officially unresolved. They cannot easily retreat from that precedent without appearing to change the rules mid-game. Their constraint is that the North Korean acceptance binds them: future withdrawals must be evaluated against that precedent or explicitly distinguished from it.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, depositary_states_as_arbiters, agenda_setter,
    institutional, civilizational, constrained, global).

% Every five years, all state parties gather to assess treaty compliance and attempt to clarify ambiguities. The Review Conferences have repeatedly called for a 'clear threshold' for Article X withdrawal but have failed to agree on what clarity means. The conferences expose the reading dispute without resolving it: beneficiary states argue for low-threshold language, regime-stability states argue for high-threshold language, and the conference produces a compromise text that papers over the disagreement. The conferences are the formal venue where the reading contest plays out, but the lack of consensus means the operational threshold remains undefined.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, npt_review_conferences, observer,
    organized, generational, analytical, global).

% Academic and civil-society researchers argue that the low-threshold reading better respects NNWS sovereignty and preserves the bargain logic of the treaty: in exchange for non-proliferation, NNWS retain a credible exit option if the disarmament side of the bargain fails. They provide epistemic authority and historical analysis that the low-threshold reading appeals to. They are external to state negotiation and have no direct power, but they shape the interpretive consensus among educated publics and inform NNWS decision-making.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, scholarly_disarmament_advocates, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__withdrawal_threshold_reading, nws_collective_enforcement_capacity).
narrative_ontology:fixing_cost_class(npt_treaty_text__withdrawal_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The NPT coordinates non-proliferation: NWS agree to pursue disarmament (Article VI), NNWS agree not to acquire weapons, and all parties subject this bargain to a shared treaty framework with inspection and enforcement mechanisms. The coordination problem being solved is the security dilemma: states pursue nuclear weapons for deterrence; the treaty substitutes a shared commitment structure and security assurances for unilateral acquisition. The Article X withdrawal threshold is part of that coordination: by defining the exit condition, it sets the credibility of each party's commitment. A low threshold makes NNWS commitment credible as conditional; a high threshold makes it credible as binding.
% TRANSFER_FUNCTION: The transfer is regime legitimacy, exit-option value, and enforcement credibility. Under a low-threshold reading, NNWS transfer compliance certainty and restraint to NWS in exchange for a preserved threat of exit that maintains their negotiating leverage — their restraint is valuable only because it is conditional. Under a high-threshold reading, NNWS transfer binding compliance without preserved exit, receiving in exchange a firmer regime-stability commitment from NWS (though this is aspirational on Article VI). The constraint moves exit-option value from regime-stability maintainers (who lose enforcement credibility) to threshold states (who preserve negotiating leverage).
% ABSENT_VOICES: Non-state nuclear programs and subnational actors (terrorist networks, insurgent groups) cannot withdraw because they are not parties to the treaty. Their interests — whether a low threshold makes it easier or harder for them to acquire nuclear materials — are absent from the negotiating table. Some proliferation analysts argue that a low threshold that invites threshold-state withdrawal indirectly helps non-state actors by fragmenting state-level enforcement; others argue that the low threshold preserves NNWS compliance incentives, which is the main barrier to non-state acquisition anyway. The absent-voice asymmetry makes different analysts prefer different readings depending on which proliferation pathway they prioritize. Additionally, indigenous and small-island nations, whose security interests in a stable non-proliferation regime are acute but whose voices carry minimal institutional power, are functionally excluded from shaping the threshold interpretation.
% DISAPPEARANCE_RATIONALE: If Article X's withdrawal threshold were suddenly and authoritatively clarified to impose a truly high bar (only existential threats to state survival, never regional crises or unmet disarmament obligations), threshold states would restructure their security policies: Iran might accelerate uranium enrichment and move toward breakout; Japan and South Korea might seek explicit NWS security guarantees as substitutes for the treaty's implicit protection; and future regional escalations would no longer produce withdrawal threats as negotiating tools, because those threats would be incredible. Conversely, if clarified to an even lower bar (political disagreement with NWS positions justifies withdrawal), the NWS enforcement capacity would collapse entirely and the regime would shift rapidly toward voluntary cooperation with predictable exits. Either clarity would restructure the security environment's equilibrium around the new threshold. The constraint's operation is the mechanism that maintains the current ambiguous middle ground where both readings have partial credibility.
% FOUNDING_PROBLEM: The NPT was founded in 1968 to prevent a world where every state possessed nuclear weapons and deterrence rested on universal acquisition and mutual vulnerability. The withdrawal-threshold question arose from the treaty's core bargain: NNWS agreed to foreswear nuclear weapons, but in exchange for what guarantee? If NWS could unilaterally abandon disarmament without consequence, would NNWS honor their side? The founders wrote Article X to allow withdrawal 'if the extraordinary events related to the subject matter of this Treaty have jeopardized the supreme interests of its country' — deliberately leaving the threshold ambiguous to preserve the bargain's flexibility. This reading (sovereignty-preservation) holds that the founders intentionally preserved NNWS exit credibility so their restraint would remain conditional and credible. A state whose restraint is a live choice (exit is possible) has more leverage than a trapped state.
% FOUNDING_PROBLEM_CORROBORATION: Disarmament advocates and sovereignty-preservation scholars cite the 1968 negotiating record, specifically statements from NNWS delegations (Sweden, Brazil, Mexico) that insisted on exit flexibility as the price of accepting non-proliferation. They argue the founders deliberately deferred threshold clarification to preserve that bargain structure. Regime-stability advocates counter that North Korea's 2003 withdrawal demonstrated the founders' failure to anticipate how the low-threshold reading would be exploited: the founders intended the bar to be high — genuine existential threat, not regional crises — but NK's successful exit showed the failure. International Court of Justice commentary and scholarly consensus in international law remains split. No external authority (outside the benefiting or paying parties) has successfully resolved the disagreement; both readings cite the same founding texts and reach opposite conclusions. The corroboration is asymmetric: the sovereignty-preservation reading cites contemporary NNWS negotiators; the regime-stability reading cites NK's successful exit as evidence of what the founders should have foreseen.
narrative_ontology:disappearance_verdict(npt_treaty_text__withdrawal_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__withdrawal_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__withdrawal_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__withdrawal_threshold_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__withdrawal_threshold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__withdrawal_threshold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts at 0.35 (1968) because the founders deliberately left the threshold ambiguous — the text was a compromise that deferred the threshold question rather than resolving it. It rises to 0.56 by 2003 after North Korea's withdrawal, which demonstrated that the low-threshold reading was viable and demonstrated its cost: the regime's enforcement capacity was revealed as fragile. It peaks at 0.63 in 2020 (Iran's threatened withdrawal during the JCPOA crisis) and settles at 0.58 by 2026 as the reading's implications stabilize into an uneasy recognition that the threshold remains unresolved but functional. Theater ratio rises from 0.25 to 0.42 because increasingly the threshold is invoked rhetorically in withdrawal threats without being operationally clarified — states threaten to withdraw under Article X, depositaries accept or deflect the claim without adjudicating the threshold, and both performances continue without resolution. Suppression requirement shows a dip after 2003 (0.55) and after 2020 (0.52, 0.47 at endpoint) because the low-threshold reading's viability is now established in practice: NWS cannot suppress it via enforcement, so the suppression requirement falls — the reading persists because it is already instantiated, not because it is enforced. The constraint is measured on a shared single time grid so every metric is authored at every interval point.
 *
 * PERSPECTIVAL GAP:
 *   The payer (NWS/regime-stability) and beneficiary (threshold states) seats compute different types from the same structural data because they experience opposite directionalities: a high-d target seat computes the constraint as more extractive and coercive than a low-d beneficiary seat. The North Korea precedent is the central disagreement: the payer seat reads it as an exploitation of a loophole that should have been closed; the beneficiary seat reads it as a vindication of the founders' intent to preserve NNWS exit credibility.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold states: beneficiaries of exit-credibility preservation; their exit from the regime itself is constrained (regional security dependence on the treaty's existence), but their exit option from compliance if betrayed is protected by the low threshold. Directionality: low-to-moderate (protected position). NWS and regime-stability advocates: payers of weakened enforcement; their identity is fused with regime maintenance, so exit from the regime is diplomatically impossible (identity_locked). Directionality: high (trapped targets of the reading's enforcement undermining). Depositary states: agenda-setters who manage the threshold operationally; they have power (control acceptance of withdrawal notices) but are constrained by precedent (North Korea's acceptance set a default). Directionality: moderate-to-high (constrained power position).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was preventing a world of universal nuclear weapons. The founding-problem status is contested: regime-stability advocates argue the problem is still live (proliferation risk remains, especially in the Middle East and Northeast Asia); exit-credibility advocates argue the problem is substantially solved for the NNWS (very few NNWS have pursued weapons, deterrence has held). The constraint's mandatrophy risk is moderate: the dispute is not whether non-proliferation is important (consensus exists on that), but whether the specific threshold reading preserves or undermines the bargain. A low threshold reading could be mandatrophic if it invites withdrawal spirals that collapse the regime, leaving no constraint at all; but it could equally preserve the mandate by keeping NNWS compliance credible (their restraint is a live choice, not a trap). This ambiguity is the classification's true content: the constraint is tangled rope precisely because it simultaneously coordinates non-proliferation AND asymmetrically extracts from regime-stability maintainers, with no clean separation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    extraordinary_events_threshold_ambiguity,
    'What concrete events or circumstances meet the ''extraordinary events'' threshold for Article X withdrawal? Is it existential threats to national survival, regional security escalations, unmet disarmament obligations by NWS, or a lower bar including domestic political crises?',
    'International Court of Justice advisory opinion on Article X interpretation, or successful negotiation of a protocol clarifying the threshold. A precedent from a major NNWS withdrawal claim (Iran, Japan) that is either accepted or contested would operationally instantiate one reading.',
    'A high-threshold interpretation would reclassify this constraint as rope or snare (regime-stabilizing, not exit-preserving), benefiting regime-stability advocates and imposing costs on threshold states. A low-threshold interpretation would hold the current tangled-rope classification, benefiting exit-credibility seekers.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraordinary_events_threshold_ambiguity, conceptual, 'The core structural ambiguity: what bar must be met for withdrawal to be procedurally valid?').

omega_variable(
    north_korea_precedent_replicability,
    'Does North Korea''s successful withdrawal in 2003 establish a procedural precedent that any NNWS can invoke (low threshold, replicable) or was it a unique case that depositaries would reject if repeated (high threshold, precedent-proof)?',
    'A future withdrawal attempt by a major NNWS (Iran, Japan, South Korea) would test whether the 2003 precedent is replicable. If accepted, the low threshold is established; if contested or rejected, the precedent was unique to NK''s circumstances.',
    'If replicable, the low-threshold reading is locked in via practice; if unique, the high-threshold reading gains credibility and the constraint reclassifies toward rope-stabilizing. The test is empirical and will arrive when the next major withdrawal threat occurs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(north_korea_precedent_replicability, empirical, 'Whether the 2003 NK precedent generalizes to future withdrawal claims.').

omega_variable(
    disarmament_compliance_obligation_asymmetry,
    'Do NWS have an enforceable legal obligation under Article VI to pursue disarmament, or is disarmament aspirational and non-binding? If non-binding, can NNWS invoke NWS non-compliance as extraordinary events for Article X withdrawal?',
    'Legal analysis by the International Court of Justice or binding determination by a future NPT Review Conference. Empirical evidence of NWS actions (arms race behavior, arsenal expansion) could shift the meaning of ''extraordinary events'' to include systemic NWS non-compliance.',
    'If Article VI is binding and enforceable, NWS non-compliance becomes a ground for low-threshold withdrawal, strengthening threshold states'' exit credibility. If aspirational, the high-threshold reading gains support because NWS non-compliance is not a treaty violation that triggers withdrawal rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disarmament_compliance_obligation_asymmetry, empirical, 'Whether Article VI disarmament obligation is binding or aspirational, and whether non-compliance triggers withdrawal rights.').

omega_variable(
    reading_vs_foundational_intent,
    'When the founders wrote ''extraordinary events,'' did they intend to preserve NNWS exit credibility (this reading''s core premise) or to prevent frivolous withdrawal while preserving regime stability (the high-threshold reading''s premise)? Is this a disagreement about facts or values?',
    'Extensive study of the 1968 negotiating record (already undertaken by arms-control scholars with divergent conclusions); agreement on how to weight founder intent vs. textual meaning in treaty interpretation (a conceptual/legal question, not empirical).',
    'If intent is read as exit-credibility preservation, this reading''s foundational axiom (NNWS sovereignty_exit_preservation) is vindicated; if intent is read as stability-centered, the high-threshold reading''s foundational axiom (regime_stability_supremacy) is vindicated. This is irreducible across different interpretive schools.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_vs_foundational_intent, conceptual, 'Whether the founders prioritized NNWS exit credibility or regime stability in the extraordinary-events clause.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 1968, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1968, 0.25).
narrative_ontology:measurement_basis(npt__tr_t1968, observed).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1985, 0.28).
narrative_ontology:measurement_basis(npt__tr_t1985, observed).
narrative_ontology:measurement(npt__tr_t2003, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2003, 0.38).
narrative_ontology:measurement_basis(npt__tr_t2003, observed).
narrative_ontology:measurement(npt__tr_t2015, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2015, 0.41).
narrative_ontology:measurement_basis(npt__tr_t2015, observed).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2020, 0.43).
narrative_ontology:measurement_basis(npt__tr_t2020, observed).
narrative_ontology:measurement(npt__tr_t2026, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2026, 0.42).
narrative_ontology:measurement_basis(npt__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1968, 0.35).
narrative_ontology:measurement_basis(npt__be_t1968, observed).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1985, 0.42).
narrative_ontology:measurement_basis(npt__be_t1985, observed).
narrative_ontology:measurement(npt__be_t2003, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2003, 0.56).
narrative_ontology:measurement_basis(npt__be_t2003, observed).
narrative_ontology:measurement(npt__be_t2015, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2015, 0.61).
narrative_ontology:measurement_basis(npt__be_t2015, observed).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2020, 0.63).
narrative_ontology:measurement_basis(npt__be_t2020, observed).
narrative_ontology:measurement(npt__be_t2026, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2026, 0.58).
narrative_ontology:measurement_basis(npt__be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1968, 0.32).
narrative_ontology:measurement_basis(npt__su_t1968, observed).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1985, 0.38).
narrative_ontology:measurement_basis(npt__su_t1985, observed).
narrative_ontology:measurement(npt__su_t2003, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2003, 0.55).
narrative_ontology:measurement_basis(npt__su_t2003, observed).
narrative_ontology:measurement(npt__su_t2015, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2015, 0.49).
narrative_ontology:measurement_basis(npt__su_t2015, observed).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2020, 0.52).
narrative_ontology:measurement_basis(npt__su_t2020, observed).
narrative_ontology:measurement(npt__su_t2026, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2026, 0.47).
narrative_ontology:measurement_basis(npt__su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__withdrawal_threshold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_text__withdrawal_threshold_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nnws_reading).

% DUAL FORMULATION NOTE:
% This constraint instantiates one reading of the NPT Article X kernel. The sibling readings (NWS reading: high-threshold enforcement; NNWS disarmament reading: Article VI compliance as exit ground) are separate constraint stories with different ε, beneficiary/victim structures, and types. The three readings compete for interpretive authority over the same kernel text; no single reading has achieved monopoly status. This reading's low-threshold instantiation directly influences the NWS reading's enforcement capacity (a low threshold weakens NWS suppression power) and is influenced by the NNWS disarmament reading (if Article VI non-compliance becomes a recognized ground for exit, the low threshold is strengthened). All three stories must be read together as a constraint family to understand how the reading contest shapes the regime's actual operation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_text__withdrawal_threshold_reading, institutional, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
