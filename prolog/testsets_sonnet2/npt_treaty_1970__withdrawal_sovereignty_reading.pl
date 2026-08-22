% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__withdrawal_sovereignty_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__withdrawal_sovereignty_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: npt_treaty_1970__withdrawal_sovereignty_reading
 *   human_readable: NPT Article X Withdrawal Right as Sovereignty Reservation
 *   domain: international_law/nuclear_nonproliferation
 *
 * SUMMARY:
 *   This story reads the NPT through Article X: the withdrawal clause as a
 *   legitimate, textually grounded reservation of sovereign discretion, not a
 *   defect to be closed. Under this reading, the treaty's obligations
 *   (Articles I, II, III, VI alike) are contingent on the security
 *   environment a state faces, and any signatory retains a unilateral,
 *   self-judging right to exit on 90 days' notice citing extraordinary
 *   events. The DPRK's 2003 withdrawal is the clearest historical test case:
 *   this reading treats it as the clause operating exactly as negotiated,
 *   whatever the international community's reaction. This is one of three
 *   sibling readings of the same kernel (npt_treaty_1970): the
 *   oligopoly_enforcement_reading treats Articles I-II as the binding core
 *   and Article VI as aspirational; the reciprocal_disarmament_reading treats
 *   Article VI as binding with temporal urgency. This reading differs from
 *   both by making the treaty's bindingness itself conditional rather than
 *   absolute — the structural delta is that regime stability (a background
 *   good the other two readings largely take for granted) becomes a named
 *   victim here, and threshold states gain measurable option value from a
 *   credible exit threat that the other readings do not price at all.
 *
 * KEY AGENTS:
 *   - threshold_states: beneficiary of exit-option value, extract concessions via credible withdrawal threat without exercising it
 *   - withdrawal_capable_states: all signatories, retain sovereign discretion as designed
 *   - regime_stability_norm: non-agent victim, the durability expectation the clause structurally erodes
 *   - non_nuclear_weapon_states_relying_on_regime: bear the diffuse security cost of normalized exit threats
 *   - depositary_states_and_un_security_council: administer notification, cannot legally block a sovereignty-grounded withdrawal
 *   - iaea_and_verification_apparatus: excluded from adjudicating withdrawal legitimacy
 *   - regime_theory_scholars: analytical observers of the textualist vs. purposive interpretive dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__withdrawal_sovereignty_reading, 0.42).
domain_priors:suppression_score(npt_treaty_1970__withdrawal_sovereignty_reading, 0.28).
domain_priors:theater_ratio(npt_treaty_1970__withdrawal_sovereignty_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(npt_treaty_1970__withdrawal_sovereignty_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__withdrawal_sovereignty_reading, rope).
narrative_ontology:human_readable(npt_treaty_1970__withdrawal_sovereignty_reading, "NPT Article X Withdrawal Right as Sovereignty Reservation").
narrative_ontology:topic_domain(npt_treaty_1970__withdrawal_sovereignty_reading, "international_law/nuclear_nonproliferation").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__withdrawal_sovereignty_reading, '6d2c5a66-ade0-4c0d-8b8c-21f5c5b871c4').
narrative_ontology:cs_kernel_codification('6d2c5a66-ade0-4c0d-8b8c-21f5c5b871c4', fixed_text).
narrative_ontology:cs_authority_grounding('6d2c5a66-ade0-4c0d-8b8c-21f5c5b871c4', distributed).
narrative_ontology:cs_reading_relation('6d2c5a66-ade0-4c0d-8b8c-21f5c5b871c4', npt_treaty_1970__oligopoly_enforcement_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d2c5a66-ade0-4c0d-8b8c-21f5c5b871c4', npt_treaty_1970__reciprocal_disarmament_reading, influences).
narrative_ontology:cs_axiom('6d2c5a66-ade0-4c0d-8b8c-21f5c5b871c4', foundational, sovereign_security_judgment_is_self_determining).
narrative_ontology:cs_axiom_status(sovereign_security_judgment_is_self_determining, holdable).
narrative_ontology:cs_axiom_grounding('6d2c5a66-ade0-4c0d-8b8c-21f5c5b871c4', sovereign_security_judgment_is_self_determining, conventional).
narrative_ontology:cs_axiom('6d2c5a66-ade0-4c0d-8b8c-21f5c5b871c4', secondary, treaty_obligations_are_conditional_on_security_environment).
narrative_ontology:cs_axiom_status(treaty_obligations_are_conditional_on_security_environment, holdable).
narrative_ontology:cs_axiom_grounding('6d2c5a66-ade0-4c0d-8b8c-21f5c5b871c4', treaty_obligations_are_conditional_on_security_environment, instrumental).
narrative_ontology:cs_reference_frame('6d2c5a66-ade0-4c0d-8b8c-21f5c5b871c4', westphalian_consent_based_treaty_order).
narrative_ontology:cs_drift_state('6d2c5a66-ade0-4c0d-8b8c-21f5c5b871c4', post_dprk_withdrawal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6d2c5a66-ade0-4c0d-8b8c-21f5c5b871c4', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__withdrawal_sovereignty_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__withdrawal_sovereignty_reading, withdrawal_capable_states).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm).
narrative_ontology:constraint_victim(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states_relying_on_regime).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, state_sovereignty_doctrine).
narrative_ontology:constraint_vindicates(npt_treaty_1970__withdrawal_sovereignty_reading, consent_based_treaty_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with latent or developing nuclear capacity (historically framed around cases like the DPRK's 2003 withdrawal announcement, and hypothetically Iran) treat Article X as a standing option: remaining a member preserves diplomatic normalcy and technology access, while the credible threat of invoking withdrawal on '90 days notice, citing extraordinary events jeopardizing supreme national interests' extracts concessions (sanctions relief, security guarantees, fuel-cycle cooperation) from the depositary states and IAEA without ever exercising the option.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states, beneficiary,
    moderate, generational, arbitrage, national).

% Any signatory retains the formal legal capacity to withdraw citing a security-environment change, which this reading holds is the treaty's own text protecting sovereign discretion rather than a loophole to be closed. This state has never invoked Article X but values that the option exists and is legally uncontested.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, withdrawal_capable_states, beneficiary,
    moderate, civilizational, mobile, national).

% The expectation that near-universal, durable membership makes nonproliferation a stable equilibrium is directly undermined every time withdrawal is treated as a live, legitimate, low-cost option: the credibility of universal commitment erodes because every remaining member knows every other member's compliance is conditional and reversible, not binding.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm, payer,
    institutional, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(npt_treaty_1970__withdrawal_sovereignty_reading, regime_stability_norm).

% States that forwent nuclear weapons programs on the strength of the treaty's near-universal, durable character bear the cost when withdrawal is normalized as sovereign prerogative: their security calculus assumed a stable regime, and each credible withdrawal threat by any member state reintroduces the proliferation risk they gave up their own option to hedge against. They cannot individually restore the deterrent value of near-universality; they can only lobby depositary states or accept the eroded assurance.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, non_nuclear_weapon_states_relying_on_regime, payer,
    moderate, generational, constrained, global).

% The US, UK, and Russia as depositaries, and the Security Council under Article X's notification requirement, receive withdrawal notices and can respond diplomatically or through Council action, but Article X's text gives them no legal power to block a withdrawal that cites security-environment grounds — their role is registering and reacting, not vetoing. They administer the treaty's text but did not draft in a mechanism to contest the sovereignty claim itself.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, depositary_states_and_un_security_council, agenda_setter,
    institutional, generational, constrained, global).

% The IAEA's safeguards infrastructure is built on the assumption of continuing membership; it has no formal voice in whether a withdrawal citing Article X is legitimate, and its technical findings about a withdrawing state's prior undeclared activity are not treated as bearing on the legal validity of the withdrawal itself under this reading.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, iaea_and_verification_apparatus, excluded,
    organized, biographical, analytical, global).

% International law and international relations scholars debate whether Article X's plain text (a sovereign, self-judging security exception) or the treaty's object-and-purpose (durable universal nonproliferation) should govern interpretation; this reading takes the textualist side of that debate.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__withdrawal_sovereignty_reading, regime_theory_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__withdrawal_sovereignty_reading, threshold_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__withdrawal_sovereignty_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article X solves the real problem that no sovereign state would ratify a permanent, unconditional treaty binding it against its own future assessment of existential security threats — the withdrawal clause is what made near-universal accession possible in 1968-70 by preserving an exit valve of last resort.
% TRANSFER_FUNCTION: The clause transfers option value to any state holding or approaching nuclear latency: it converts what would otherwise be a fixed, binding commitment into a conditional one, and that conditionality is worth more to states with credible latent capacity (who can wield the threat) than to states without it (who bear the diffuse cost of reduced regime credibility).
% ABSENT_VOICES: Non-nuclear-weapon states that joined precisely because they believed membership was durable and near-universal have no seat at the table when a member state's individual withdrawal claim is adjudicated — Article X's text gives the withdrawing state a unilateral, self-judging determination, and no other party's objection has formal legal weight.
% DISAPPEARANCE_RATIONALE: If Article X were struck, the treaty's ratification history suggests several original signatories in 1968-70 (including nuclear-capable middle powers) might not have joined at all, or would have joined with reservations — so removing the clause retroactively is contested as either restoring what should have been a firmer bargain, or unraveling the sovereignty condition that made the bargain possible in the first place. Prospectively, removing it now would harden the regime's binding character for states that remain, while threshold states would likely resist any protocol amendment removing their exit option.
% FOUNDING_PROBLEM: In treaty negotiations, no state would surrender a WMD-relevant sovereign option permanently and unconditionally; Article X was the mechanism that reconciled universal-accession ambitions with the reality that states retain a residual right to reassess security commitments.
% FOUNDING_PROBLEM_CORROBORATION: The negotiating history (ENDC records, 1965-68) documents non-nuclear-weapon states explicitly demanding an exit clause as a condition of signature, corroborated by treaty-law scholars outside any beneficiary state's foreign ministry (e.g. academic commentary on Article 56 VCLT parallels); the DPRK's 2003 invocation is cited by both critics and defenders as evidence the clause functions as designed, though they dispute whether that is a feature or a failure.
narrative_ontology:disappearance_verdict(npt_treaty_1970__withdrawal_sovereignty_reading, contested).
narrative_ontology:founding_problem_status(npt_treaty_1970__withdrawal_sovereignty_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_1970__withdrawal_sovereignty_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__withdrawal_sovereignty_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).
:- end_tests(npt_treaty_1970__withdrawal_sovereignty_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rising over the interval, tracking the DPRK precedent (2003) which converted an abstract legal possibility into a demonstrated, low-cost exercise — after that point the option value available to other threshold states increased even without further invocations, which is why extractiveness ticks up sharply around 2003 and stays elevated rather than reverting. Suppression is comparatively low (0.28) because this reading holds that nothing coercive maintains the clause — it is a plainly drafted treaty right, exercised or held in reserve entirely at the withdrawing state's discretion; the treaty text itself imposes no barrier to invocation beyond the 90-day notice and stated-reasons requirement. Accessibility collapse is moderate-low (0.35): the alternative (a treaty without any exit clause) was foreclosed at drafting by negotiating history, but is not foreclosed today — amendment procedures exist even if practically difficult. Resistance is moderate-high (0.55): non-nuclear-weapon states and regime-stability advocates actively contest the legitimacy of self-judging withdrawal claims in diplomatic and scholarly fora, even though they lack a formal legal veto.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold and withdrawal-capable states sit near the beneficiary end: the clause's option value accrues to them whether or not they exercise it, and their exit options are genuinely mobile or arbitrage-grade (they can credibly threaten exit and extract concessions, or actually exit, at will). The regime stability norm and non-nuclear-weapon states that built their security policy on regime durability sit near the target end: they bear a cost (eroded credibility, restored proliferation risk) they cannot exit from — a non-nuclear-weapon state cannot unilaterally restore the treaty's near-universal character once withdrawal is normalized. Depositary states are agenda-setters in name (they administer notification) but the treaty text gives them no substantive veto, so their power to alter this dynamic is weaker than their institutional label suggests — this is a case where declared role and effective leverage diverge, but no override was needed because the situation description already captures the asymmetry the engine will read from exit_options and power directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (no sovereign would ratify permanently against its own security judgment) remains live — this is precisely why the founding_problem_status is 'live' rather than 'dead': states continue to value the exit option as insurance against unforeseen existential threats, and no consensus exists that the underlying sovereignty concern has been resolved. This blocks a mandatrophy misreading in one direction: it would be wrong to treat Article X as a dead vestige being cynically exploited, since the sovereignty concern it answers is undiminished. But the reading does not thereby become innocent of extraction: the same clause that answers a live problem for the withdrawing state imposes a real, uncompensated cost on the regime-stability norm and non-nuclear-weapon states, which is why this is authored as rope in claim (textually legitimate coordination-preserving mechanism) while the metrics show real, rising extraction that a purely coordination-framed reading would miss.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    self_judging_withdrawal_legitimacy,
    'Does Article X''s plain text grant a genuinely self-judging determination of ''extraordinary events,'' or does customary international law (e.g. VCLT good-faith principles) impose an objective standard the withdrawing state cannot unilaterally satisfy?',
    'An International Court of Justice advisory opinion or contentious case squarely addressing an Article X withdrawal''s validity would resolve whether the self-judging reading survives scrutiny; none has occurred as of this writing, leaving the question live.',
    'If self-judging determination is upheld, this reading''s classification as legitimate sovereignty exercise strengthens and extractiveness attributable to ''illegitimate'' exploitation of the clause falls; if an objective good-faith standard is imposed, some of the option value currently priced into the extractiveness trajectory would be reclassified as bad-faith exploitation rather than legitimate reservation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_judging_withdrawal_legitimacy, conceptual, 'Whether Article X withdrawal is genuinely self-judging or subject to an external good-faith standard.').

omega_variable(
    kernel_reading_disagreement_locus,
    'This constraint is one reading of the npt_treaty_1970 kernel (sibling readings: oligopoly_enforcement_reading, reciprocal_disarmament_reading). Where exactly does the disagreement between readings live — is it about which article is textually primary, or about a deeper disagreement over whether the NPT is a bargain (mutual obligation) versus a regime (rule-governed order with an exit valve)?',
    'Comparative analysis of ICJ jurisprudence, ENDC negotiating history, and NPT Review Conference final documents across all three readings would locate whether the textual primacy dispute is doing the real work or is downstream of the bargain-vs-regime framing.',
    'If the disagreement is fundamentally about bargain-vs-regime framing rather than textual primacy, all three readings may be less reconcilable through textual argument alone than this reading''s own framing assumes, which would strengthen the case that these are genuinely separate constraints rather than competing interpretations of one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disagreement_locus, conceptual, 'Locating the deep structural disagreement between the three kernel readings.').

omega_variable(
    withdrawal_threat_vs_exercise,
    'Should the option value threshold states extract from a credible but unexercised withdrawal threat be measured as equivalent extraction to an actually-exercised withdrawal, or is threatened-but-unexercised withdrawal a structurally distinct (lower-cost-to-regime) phenomenon?',
    'Comparative case study of states that threatened withdrawal without exercising it (e.g. diplomatic signaling during safeguards disputes) against the single clear exercised case (DPRK 2003), tracking whether regime-stability costs differ measurably between threat and exercise.',
    'If threat and exercise impose meaningfully different costs on the regime-stability norm, the extractiveness trajectory authored here (which treats 2003 as the inflection point) may understate ongoing threat-based extraction occurring in years without visible exercise.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_threat_vs_exercise, empirical, 'Whether unexercised withdrawal threats extract comparably to exercised withdrawal.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__withdrawal_sovereignty_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 1995, 0.2).
narrative_ontology:measurement(npt__tr_t2003, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2003, 0.35).
narrative_ontology:measurement(npt__tr_t2015, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2015, 0.32).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_1970__withdrawal_sovereignty_reading, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1970, 0.15).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1985, 0.2).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 1995, 0.28).
narrative_ontology:measurement(npt__be_t2003, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2003, 0.4).
narrative_ontology:measurement(npt__be_t2015, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2015, 0.38).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_1970__withdrawal_sovereignty_reading, base_extractiveness, 2025, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(npt_treaty_1970__withdrawal_sovereignty_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__withdrawal_sovereignty_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, oligopoly_enforcement_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__withdrawal_sovereignty_reading, reciprocal_disarmament_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings decomposed from the colloquial label 'the NPT' per the epsilon-invariance principle. oligopoly_enforcement_reading treats Articles I-II as primary binding obligation; reciprocal_disarmament_reading treats Article VI as binding with temporal urgency; this story (withdrawal_sovereignty_reading) treats Article X as the treaty's structural keystone, making all obligations conditional. Each reading has its own epsilon, its own beneficiary/victim structure, and its own claimed type — they are linked here via affects_constraints rather than merged into one story with a measurement parameter, per DP-001.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
