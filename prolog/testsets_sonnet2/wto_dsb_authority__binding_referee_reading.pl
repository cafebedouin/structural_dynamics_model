% ============================================================================
% CONSTRAINT STORY: wto_dsb_authority__binding_referee_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: wto_dsb_authority__binding_referee_reading
 *   human_readable: WTO Dispute Settlement Body as Binding Treaty Referee
 *   domain: international_law/trade_governance
 *
 * SUMMARY:
 *   This story instantiates the binding_referee_reading of the
 *   wto_dsb_authority kernel: DSB panels issue rulings that become binding
 *   upon adoption (reverse consensus), non-compliance is a treaty breach that
 *   can trigger authorized retaliation, and member states are treated as
 *   having formally surrendered policy discretion within covered-agreement
 *   domains as the price of predictable market access. The reading treats
 *   this arrangement as descriptively real and legally operative, not as a
 *   contested overreach (that is the sibling judicial_activism_reading) and
 *   not as a merely advisory facilitation mechanism (that is the sibling
 *   advisory_coordination_reading). ε is authored for the binding-authority
 *   arrangement as this reading's own lights see it: genuine coordination
 *   value plus a real, growing extraction gradient falling disproportionately
 *   on members with thin legal capacity and on domestic constituencies with
 *   no standing in the proceeding.
 *
 * KEY AGENTS:
 *   - dsb_panels_and_appellate_body_function: institutional agenda-setter administering binding adjudication
 *   - export_oriented_wto_members: primary beneficiaries with capacity to litigate repeatedly
 *   - developing_member_states_with_thin_legal_capacity: formally equal, practically overmatched payers
 *   - domestic_regulatory_constituencies: excluded payers with no standing in the proceeding that binds them
 *   - wto_members_disputing_appellate_body_legitimacy: excluded voice pressing the sibling judicial_activism_reading from inside the system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wto_dsb_authority__binding_referee_reading, 0.58).
domain_priors:suppression_score(wto_dsb_authority__binding_referee_reading, 0.62).
domain_priors:theater_ratio(wto_dsb_authority__binding_referee_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(wto_dsb_authority__binding_referee_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wto_dsb_authority__binding_referee_reading, tangled_rope).
narrative_ontology:human_readable(wto_dsb_authority__binding_referee_reading, "WTO Dispute Settlement Body as Binding Treaty Referee").
narrative_ontology:topic_domain(wto_dsb_authority__binding_referee_reading, "international_law/trade_governance").

domain_priors:requires_active_enforcement(wto_dsb_authority__binding_referee_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(wto_dsb_authority__binding_referee_reading, 'de2eec86-ce9d-4542-9506-028f14a9bf82').
narrative_ontology:cs_kernel_codification('de2eec86-ce9d-4542-9506-028f14a9bf82', fixed_text).
narrative_ontology:cs_authority_grounding('de2eec86-ce9d-4542-9506-028f14a9bf82', lineage).
narrative_ontology:cs_interpretation_layer_present('de2eec86-ce9d-4542-9506-028f14a9bf82').
narrative_ontology:cs_reading_relation('de2eec86-ce9d-4542-9506-028f14a9bf82', wto_dsb_authority__advisory_coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('de2eec86-ce9d-4542-9506-028f14a9bf82', wto_dsb_authority__judicial_activism_reading, influences).
narrative_ontology:cs_axiom('de2eec86-ce9d-4542-9506-028f14a9bf82', foundational, treaty_ruling_creates_binding_legal_obligation).
narrative_ontology:cs_axiom_status(treaty_ruling_creates_binding_legal_obligation, holdable).
narrative_ontology:cs_axiom_grounding('de2eec86-ce9d-4542-9506-028f14a9bf82', treaty_ruling_creates_binding_legal_obligation, conventional).
narrative_ontology:cs_axiom('de2eec86-ce9d-4542-9506-028f14a9bf82', secondary, reverse_consensus_adoption_forecloses_unilateral_blocking).
narrative_ontology:cs_axiom_status(reverse_consensus_adoption_forecloses_unilateral_blocking, holdable).
narrative_ontology:cs_axiom_grounding('de2eec86-ce9d-4542-9506-028f14a9bf82', reverse_consensus_adoption_forecloses_unilateral_blocking, conventional).
narrative_ontology:cs_reference_frame('de2eec86-ce9d-4542-9506-028f14a9bf82', uruguay_round_dsu_enforcement_bargain).
narrative_ontology:cs_drift_state('de2eec86-ce9d-4542-9506-028f14a9bf82', post_appellate_body_paralysis_2025, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('de2eec86-ce9d-4542-9506-028f14a9bf82', '').
narrative_ontology:cs_kernel_id(wto_dsb_authority__binding_referee_reading, wto_dsb_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, export_oriented_wto_members).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, multinational_exporters).
narrative_ontology:constraint_beneficiary(wto_dsb_authority__binding_referee_reading, wto_secretariat_and_appellate_infrastructure).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, developing_member_states_with_thin_legal_capacity).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, domestic_regulatory_constituencies).
narrative_ontology:constraint_victim(wto_dsb_authority__binding_referee_reading, policy_space_dependent_industries).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, rules_based_trading_system_doctrine).
narrative_ontology:constraint_vindicates(wto_dsb_authority__binding_referee_reading, treaty_pacta_sunt_servanda_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Convenes panels to adjudicate disputes brought under covered agreements, issues rulings that become binding upon DSB adoption absent consensus reversal, and authorizes retaliation when a losing member fails to bring its measure into compliance within a reasonable period. Administers the single most active binding treaty adjudication mechanism in international law, though the Appellate Body's paralysis since 2019 (US blocking appointments) has degraded the enforcement chain it depends on.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, dsb_panels_and_appellate_body_function, agenda_setter,
    institutional, generational, analytical, global).

% Rely on the binding ruling structure to secure market access commitments made by trading partners; can bring complaints against non-compliant measures and expect a predictable adjudicative outcome rather than renegotiation from scratch. Have the legal capacity and standing to use the mechanism repeatedly and treat litigation as an ordinary tool of trade policy.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, export_oriented_wto_members, beneficiary,
    powerful, generational, arbitrage, global).

% Do not litigate directly but lobby home governments to bring cases on their behalf; benefit from the certainty that a binding ruling, not merely diplomatic pressure, stands behind their market access. Can relocate supply chains around adverse outcomes if disputes run long, giving them exit options individual states lack.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, multinational_exporters, beneficiary,
    organized, generational, arbitrage, global).

% Surrendered the same formal policy discretion as every member under the treaty, but lack the legal staff, economic leverage, and retaliation capacity to bring cases, defend against them effectively, or make authorized retaliation meaningful against a large trading partner. Face the binding character of rulings as an asymmetric burden: bound the same in law, unequal in practice.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, developing_member_states_with_thin_legal_capacity, payer,
    powerless, biographical, constrained, national).

% Public health, environmental, and labor regulators whose domestic measures can be found inconsistent with covered agreements even when adopted through ordinary democratic process; must amend or repeal the measure or accept authorized trade retaliation against unrelated sectors. Have no seat in the dispute proceeding and no vote on the ruling that binds their government.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, domestic_regulatory_constituencies, payer,
    moderate, biographical, trapped, national).

% Domestic industries protected or supported by measures later ruled WTO-inconsistent; bear the direct cost of policy withdrawal or of retaliatory tariffs imposed on their unrelated exports when their government's measure elsewhere is found non-compliant. Their exposure is a byproduct of the state's binding treaty obligation, not a direct party to any ruling.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, policy_space_dependent_industries, payer,
    moderate, biographical, constrained, national).

% Institutional existence and relevance depend on the binding character of the dispute mechanism; a purely advisory system would strip the organization of its distinguishing enforcement feature relative to prior GATT diplomacy-only rounds. Administers panel composition, procedural rules, and precedent tracking that give the binding reading its operational reality.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_secretariat_and_appellate_infrastructure, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_secondary_role(wto_dsb_authority__binding_referee_reading, wto_secretariat_and_appellate_infrastructure, agenda_setter).

% Argue from within the system that the Appellate Body exceeded its mandate through interpretive gap-filling, and have blocked appointments to force reform; their objection to the binding referee reading is precisely the judicial_activism_reading, which this story treats as a sibling claim rather than incorporating. Their blocking action has practically suspended binding appellate review since 2019 even though the underlying legal obligation remains formally in force.
narrative_ontology:constraint_stakeholder(wto_dsb_authority__binding_referee_reading, wto_members_disputing_appellate_body_legitimacy, excluded,
    powerful, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(wto_dsb_authority__binding_referee_reading, export_oriented_wto_members).
narrative_ontology:fixing_cost_class(wto_dsb_authority__binding_referee_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, rule-grounded forum to resolve trade disputes that would otherwise be settled by unilateral retaliation or raw bargaining power, giving smaller economies a forum-based alternative to power-based settlement and giving all members predictable market access commitments they can enforce rather than merely request.
% TRANSFER_FUNCTION: Moves compliance obligations from the political domain (where a state could weigh domestic priorities against trade commitments) into the legal domain (where a ruling converts non-compliance into an authorized-retaliation-bearing treaty breach); shifts effective policy discretion from domestic regulatory processes to panel and (when functioning) appellate interpretation of covered-agreement text.
% ABSENT_VOICES: Domestic constituencies affected by a challenged measure — the public health agency, the environmental regulator, the community relying on a challenged subsidy — have no standing before the panel; only the member state litigates, and the state's litigating position may not track the measure's domestic constituency at all. Least-developed members without permanent Geneva delegations are structurally underrepresented in shaping how covered-agreement text gets interpreted even though the interpretation binds them equally.
% DISAPPEARANCE_RATIONALE: If binding ruling authority disappeared and disputes reverted to pure diplomatic negotiation, market access commitments would become renegotiable by power rather than enforceable by right; large economies would settle disputes bilaterally on leverage, smaller economies would lose their principal forum-based enforcement tool, and the credibility of tariff-binding commitments across the whole trading system would need a new anchor.
% FOUNDING_PROBLEM: The pre-1995 GATT dispute process allowed a losing party to block adoption of an unfavorable panel report by withholding consensus, making dispute outcomes effectively unenforceable against a determined defendant; the Uruguan Round negotiators built automatic adoption (reverse consensus) and authorized retaliation specifically to solve the enforcement gap.
% FOUNDING_PROBLEM_CORROBORATION: Trade law scholars outside any government delegation (e.g. academic WTO law commentary and dispute settlement empirical studies) corroborate that the enforcement gap the mechanism was built to close was real and that the binding structure measurably increased compliance rates relative to the pre-1995 GATT system. The same outside scholarship also documents, independently of the litigating members, that the mechanism's practical binding force has been degraded since 2019 by the Appellate Body appointment blockade, so whether the founding problem remains solved in practice — as opposed to in treaty text — is itself disputed among non-party observers, not only among the parties.
narrative_ontology:disappearance_verdict(wto_dsb_authority__binding_referee_reading, world_rearranges).
narrative_ontology:founding_problem_status(wto_dsb_authority__binding_referee_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(wto_dsb_authority__binding_referee_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(wto_dsb_authority__binding_referee_reading, 'none', 1).
narrative_ontology:epsilon_provenance(wto_dsb_authority__binding_referee_reading, 0.58, 'claude-sonnet-5', 'none', direct).

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
 *   Extraction rises across the interval (0.35 to 0.58) tracking both the increasing density of covered-agreement jurisprudence (more precedent, more binding interpretive reach per dispute) and the growing asymmetry between members who can use the mechanism repeatedly and those who cannot. Suppression (peaking at 0.62) reflects the treaty-law character of the obligation: non-compliance is not a policy choice weighed against domestic priorities but a breach exposing the member to authorized retaliation, which is a materially stronger coercive lever than diplomatic pressure. Theater ratio stays low (0.08 to 0.18) because the binding function, unlike a degraded institution, still does real enforcement work — the rulings are not merely performative even where compliance is contested.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat and the export-beneficiary seats, this looks like coordination succeeding exactly as designed — a rules-based system replacing power-based settlement. From the thin-capacity member and domestic-regulator seats, the same binding structure computes as asymmetric extraction: equal formal obligation, unequal practical exposure, no voice in the proceeding that produces the bound outcome. The engine's per-seat computation should reflect this asymmetry directly from the declared power/exit differentials, not from any adjustment to the shared ε.
 *
 * DIRECTIONALITY LOGIC:
 *   Export-oriented members and the multinational exporters who lobby them sit near the beneficiary end: they collect the certainty of enforceable market access and can deploy the mechanism as an ordinary tool. The DSB/Secretariat function is a structural beneficiary of its own binding character since that character is what distinguishes it from pre-1995 GATT diplomacy. Developing members with thin legal capacity and domestic regulatory constituencies sit near the target end: bound in law identically to powerful members, but without matching capacity to litigate, defend, or retaliate meaningfully, and in the regulators' case, without any standing in the proceeding that produces the binding obligation against them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (GATT's blockable, unenforceable panel reports) was real and the binding structure genuinely solved it for roughly two decades. Classifying this as tangled_rope rather than pure snare or pure rope acknowledges that: the coordination function is not fictional (predictable, rules-based settlement is a real public good relative to unilateral power-based settlement), but the same structure now carries an asymmetric extraction gradient that keeps widening as jurisprudence accumulates and as the Appellate Body's practical paralysis (since 2019) shifts effective leverage back toward powerful members who can afford to appeal-into-the-void or negotiate bilaterally, precisely the outcome the binding mechanism was built to prevent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    binding_vs_advisory_kernel_ambiguity,
    'Is the DSB''s ruling authority best characterized as genuinely binding treaty law (this reading), or as advisory facilitation that member states retain ultimate discretion to disregard at the cost of retaliation rather than at the cost of illegality (the advisory_coordination_reading)?',
    'Examine actual state practice: rates of full compliance versus compliance-through-retaliation-absorption versus outright non-compliance across the DSU''s history, and whether members treat adverse rulings as legally dispositive or as one input among several policy considerations.',
    'If state practice more closely tracks ''discretion retained, cost imposed'' than ''legal obligation extinguished,'' the binding_referee_reading overstates the loss of sovereignty and the constraint''s classification should shift toward rope (coordination with a price tag) rather than tangled_rope (coordination with structural extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(binding_vs_advisory_kernel_ambiguity, conceptual, 'Whether binding character is a legal fact or a practical-cost characterization contested across kernel readings.').

omega_variable(
    appellate_body_paralysis_effect_on_bindingness,
    'Does the Appellate Body appointment blockade (since 2019) convert the binding_referee_reading into something closer to the advisory_coordination_reading in practice, even though the treaty text is unchanged?',
    'Track post-2019 appeal-into-the-void filings, use of the Multi-Party Interim Appeal Arbitration Arrangement as a workaround, and rates of unresolved disputes where a losing party appeals without possibility of adjudication, effectively re-blocking adoption as under the pre-1995 GATT system.',
    'If the practical binding force has substantially eroded, this reading''s high suppression/extraction values may describe an arrangement that is legally still binding but operationally reverting toward advisory character — a live drift the temporal measurements above should be read as capturing, not settling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(appellate_body_paralysis_effect_on_bindingness, empirical, 'Whether institutional paralysis has practically converted the binding reading toward the advisory reading.').

omega_variable(
    sovereignty_surrender_characterization,
    'Was the ''surrender of policy discretion'' a genuine one-time constitutional-style trade (sovereignty for market access, as this reading holds) or an ongoing, continuously renegotiated relationship whose character depends on each member''s evolving capacity to litigate?',
    'Legal-historical analysis of Uruguay Round negotiating record versus subsequent member behavior — did members treat the DSU as a constitutional moment or as an evolving practice open to reinterpretation?',
    'A one-time-trade characterization supports treating today''s asymmetric extraction as an unintended consequence of a fair original bargain; an ongoing-relationship characterization supports treating the growing extraction gradient as evidence the original bargain''s terms have shifted without renegotiation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sovereignty_surrender_characterization, conceptual, 'Whether the sovereignty trade was a discrete founding act or a continuously live relationship.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wto_dsb_authority__binding_referee_reading, 1995, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wto__tr_t1995, wto_dsb_authority__binding_referee_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(wto__tr_t2001, wto_dsb_authority__binding_referee_reading, theater_ratio, 2001, 0.1).
narrative_ontology:measurement(wto__tr_t2007, wto_dsb_authority__binding_referee_reading, theater_ratio, 2007, 0.12).
narrative_ontology:measurement(wto__tr_t2013, wto_dsb_authority__binding_referee_reading, theater_ratio, 2013, 0.14).
narrative_ontology:measurement(wto__tr_t2019, wto_dsb_authority__binding_referee_reading, theater_ratio, 2019, 0.16).
narrative_ontology:measurement(wto__tr_t2025, wto_dsb_authority__binding_referee_reading, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(wto__be_t1995, wto_dsb_authority__binding_referee_reading, base_extractiveness, 1995, 0.35).
narrative_ontology:measurement(wto__be_t2001, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2001, 0.4).
narrative_ontology:measurement(wto__be_t2007, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2007, 0.46).
narrative_ontology:measurement(wto__be_t2013, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2013, 0.5).
narrative_ontology:measurement(wto__be_t2019, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2019, 0.55).
narrative_ontology:measurement(wto__be_t2025, wto_dsb_authority__binding_referee_reading, base_extractiveness, 2025, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(wto__su_t1995, wto_dsb_authority__binding_referee_reading, suppression_requirement, 1995, 0.4).
narrative_ontology:measurement(wto__su_t2001, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2001, 0.48).
narrative_ontology:measurement(wto__su_t2007, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2007, 0.54).
narrative_ontology:measurement(wto__su_t2013, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2013, 0.58).
narrative_ontology:measurement(wto__su_t2019, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2019, 0.6).
narrative_ontology:measurement(wto__su_t2025, wto_dsb_authority__binding_referee_reading, suppression_requirement, 2025, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wto_dsb_authority__binding_referee_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__advisory_coordination_reading).
narrative_ontology:affects_constraint(wto_dsb_authority__binding_referee_reading, wto_dsb_authority__judicial_activism_reading).

% DUAL FORMULATION NOTE:
% This is one of three sibling constraints decomposing the natural-language concept 'WTO DSB authority' per the ε-invariance principle: the label conflates a binding-law claim, an advisory-facilitation claim, and an illegitimate-overreach claim, each with a different ε and different structural data. This file (binding_referee_reading) authors the binding-law claim. advisory_coordination_reading authors substantially lower ε and suppression (discretion retained, no legal breach). judicial_activism_reading authors the overreach claim as itself extractive of member sovereignty via interpretive drift beyond the treaty text, independent of whether the underlying DSU structure is legitimate.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
