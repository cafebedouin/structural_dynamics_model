% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__withdrawal_threshold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: npt_treaty_text__withdrawal_threshold_reading
 *   human_readable: NPT Article X Withdrawal Threshold — Sovereignty-Exit Reading
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This story isolates one reading of the NPT's Article X withdrawal kernel:
 *   the sovereignty-preservation reading, under which the treaty's withdrawal
 *   clause ('extraordinary events... jeopardized the supreme interests') sets
 *   a genuinely low bar, operationalized by North Korea's unpunished 2003
 *   withdrawal. This is distinct from the disarmament-obligation kernel
 *   reading (nnws_reading) and the non-proliferation-as-binding-constraint
 *   reading (nws_reading) — those readings concern Articles VI and II/III
 *   respectively and are separate constraints with separate ε values, linked
 *   here only through network.affects_constraints and the shared kernel_id.
 *   Under this reading, the low threshold functions as a tangled rope: it
 *   genuinely coordinates accession (states would not have joined without an
 *   exit valve) but now also lets threshold-capable states extract
 *   negotiating leverage from the credibility of an underused exit option, at
 *   the expense of the regime's verification architecture and of neighboring
 *   states who hold no comparable leverage.
 *
 * KEY AGENTS:
 *   - threshold_capable_states: Primary beneficiary (powerful/arbitrage) — holds the exit option as leverage without needing to exercise it
 *   - iran_negotiating_bloc: Beneficiary-payer (powerful/constrained) — exercises the leverage but bears the diplomatic cost of ambiguity
 *   - non_proliferation_regime_architects: Primary payer (institutional/constrained) — sees enforcement credibility degrade
 *   - neighboring_non_nuclear_states: Secondary payer (moderate/trapped) — absorbs regional security externality with no comparable leverage
 *   - arms_control_scholars: Analytical observer — documents the threshold drift
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.52).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.58).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "NPT Article X Withdrawal Threshold — Sovereignty-Exit Reading").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, '7ea78714-2db5-452f-9480-1f4653b9ba87').
narrative_ontology:cs_kernel_codification('7ea78714-2db5-452f-9480-1f4653b9ba87', fixed_text).
narrative_ontology:cs_authority_grounding('7ea78714-2db5-452f-9480-1f4653b9ba87', distributed).
narrative_ontology:cs_reading_relation('7ea78714-2db5-452f-9480-1f4653b9ba87', npt_treaty_text__nws_reading, influences).
narrative_ontology:cs_reading_relation('7ea78714-2db5-452f-9480-1f4653b9ba87', npt_treaty_text__nnws_reading, influences).
narrative_ontology:cs_axiom('7ea78714-2db5-452f-9480-1f4653b9ba87', foundational, sovereign_self_judgment_of_supreme_interests).
narrative_ontology:cs_axiom_status(sovereign_self_judgment_of_supreme_interests, holdable).
narrative_ontology:cs_axiom_grounding('7ea78714-2db5-452f-9480-1f4653b9ba87', sovereign_self_judgment_of_supreme_interests, conventional).
narrative_ontology:cs_axiom('7ea78714-2db5-452f-9480-1f4653b9ba87', secondary, unpunished_precedent_sets_operative_threshold).
narrative_ontology:cs_axiom_status(unpunished_precedent_sets_operative_threshold, holdable).
narrative_ontology:cs_axiom_grounding('7ea78714-2db5-452f-9480-1f4653b9ba87', unpunished_precedent_sets_operative_threshold, empirically_contingent).
narrative_ontology:cs_reference_frame('7ea78714-2db5-452f-9480-1f4653b9ba87', drafted_extraordinary_events_standard).
narrative_ontology:cs_drift_state('7ea78714-2db5-452f-9480-1f4653b9ba87', post_dprk_withdrawal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7ea78714-2db5-452f-9480-1f4653b9ba87', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, threshold_capable_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, iran_negotiating_bloc).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, non_proliferation_regime_architects).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, neighboring_non_nuclear_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, iran_negotiating_bloc).
narrative_ontology:constraint_vindicates(npt_treaty_text__withdrawal_threshold_reading, state_sovereignty_supremacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States with substantial enrichment or reprocessing capacity (Iran being the paradigm case, with Japan and South Korea cited as latent examples) treat Article X's low-bar 'extraordinary events... jeopardized supreme interests' language as a standing credible exit option. They do not need to withdraw to benefit — the mere availability of a plausible three-months'-notice exit shapes their negotiating leverage in every non-proliferation dispute, letting them extract concessions (sanctions relief, security guarantees, technology transfers) by holding the withdrawal option in reserve.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, threshold_capable_states, beneficiary,
    powerful, generational, arbitrage, global).

% Iran's enrichment program has been negotiated for two decades partly in the shadow of a credible Article X threat. It benefits from the low-threshold reading's negotiating leverage but also pays: the ambiguity invites the same suspicion and sanctions regime it is trying to escape, and any actual withdrawal move would trigger severe multilateral response, so the option is more valuable unexercised than exercised.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, iran_negotiating_bloc, beneficiary,
    powerful, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__withdrawal_threshold_reading, iran_negotiating_bloc, payer).

% The IAEA, the NPT depositary states, and the broader non-proliferation diplomatic apparatus built compliance verification assuming withdrawal would be rare and costly. The low-threshold reading — vindicated by North Korea's 2003 withdrawal, which faced no binding consequence — means every subsequent compliance dispute is shadowed by a low-cost exit ramp, degrading the coercive value of inspections and sanctions regimes they depend on to function.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, non_proliferation_regime_architects, payer,
    institutional, civilizational, constrained, global).

% States in proliferation-sensitive regions (Gulf states relative to Iran, South Korea and Japan relative to North Korea) bear the security externality of a credible neighbor withdrawal option without holding any comparable exit leverage themselves — they remain treaty members regardless, absorbing the regional instability that a low threshold generates.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, neighboring_non_nuclear_states, payer,
    moderate, generational, trapped, regional).

% Not an active agent in this reading but a precedent-fact: North Korea's 2003 announced withdrawal, unresolved by binding UN Security Council enforcement, functions as the de facto interpretive precedent that sets the operative threshold for Article X far lower than the treaty drafters' 'extraordinary events' language was intended to permit.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, north_korea_precedent, agenda_setter,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(npt_treaty_text__withdrawal_threshold_reading, north_korea_precedent).

% Nominally the body Article X directs states to notify and which could in principle respond to a withdrawal, but veto-bloc politics (particularly China and Russia positions on North Korea and Iran) have prevented binding collective response to withdrawal notices, effectively excluding the Council from functioning as the threshold-setting enforcer the treaty architecture assumed.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, un_security_council, excluded,
    institutional, immediate, constrained, global).

% Legal scholars and treaty historians who document the drift between the drafted threshold and the operative one, without power to resolve the ambiguity themselves.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, arms_control_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Article X was drafted to give states a genuine emergency exit from a binding non-proliferation commitment, preserving state sovereignty against the possibility that treaty membership could later conflict with a state's 'supreme interests' (an existential security threat unforeseeable at signature) — without that safety valve, states might never have joined at all.
% TRANSFER_FUNCTION: The low-threshold reading transfers negotiating leverage from the collective verification-and-sanctions regime toward individual threshold-capable states: it moves bargaining power from the institutional architects of non-proliferation enforcement to states holding an underexercised, low-cost exit option, and moves security risk onto neighboring non-nuclear states who cannot access the same leverage.
% ABSENT_VOICES: Populations in neighboring states who would bear the consequences of a regional proliferation cascade have no seat in the interpretive dispute over Article X's threshold — the argument is conducted between the withdrawing state's diplomats and the depositary/Security Council apparatus, with affected regional publics absent from the room entirely.
% DISAPPEARANCE_RATIONALE: If the low-threshold reading disappeared and a high, verified, internationally-adjudicated threshold governed Article X instead, threshold-capable states would lose the standing leverage the ambiguity currently provides in negotiations, and the non-proliferation regime's compliance enforcement would regain coercive credibility — negotiating postures on Iran and any future proliferation dispute would shift substantially.
% FOUNDING_PROBLEM: In 1968, several prospective NPT signatories (including states developing nuclear latency, and states in high-threat security environments) would not accept a permanent, unconditional non-proliferation commitment without an exit clause preserving sovereign judgment about existential threats — Article X was the price of universal accession.
% FOUNDING_PROBLEM_CORROBORATION: Treaty drafting history and ratification debates (documented independently by arms control historians, not by any single treaty party) confirm the founding problem was real accession leverage in 1968. Whether it remains live is disputed: non-proliferation regime architects and independent legal scholars argue the founding problem (accession reluctance) is largely resolved by near-universal membership and that the clause now functions primarily as strategic leverage infrastructure rather than a genuine sovereignty safeguard; threshold-capable states dispute this and maintain the safeguard is still substantively necessary.
narrative_ontology:disappearance_verdict(npt_treaty_text__withdrawal_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__withdrawal_threshold_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__withdrawal_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__withdrawal_threshold_reading, 0.52, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.52) and suppression (0.58) are moderate, not extreme: the withdrawal clause genuinely exists to solve an accession problem (coordination is real), but its interpretive drift since 2003 has produced a leverage asymmetry that is measurably extractive without collapsing into pure extraction. Theater ratio (0.44) reflects that a substantial share of diplomatic activity around Article X now performs deterrence-signaling rather than functioning as genuine emergency-exit machinery. Resistance (0.62) is elevated because regime architects and neighboring states actively contest the low-threshold reading in every relevant forum — this is not a settled, unresisted constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Threshold-capable states sit near the beneficiary end: they hold the option, rarely exercise it, and extract value from its mere availability (arbitrage-grade exit). The non-proliferation regime architects sit near the target end: their institutional function (verification, sanctions credibility) is directly degraded by the low threshold, and they cannot exit the interpretive dispute. Neighboring non-nuclear states are trapped payers — they bear regional risk with no analogous leverage of their own, which is a stronger target position than their moderate power level alone would suggest, justifying attention to directionality here even without an explicit override.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (1968 accession reluctance) is genuinely resolved for most parties — near-universal membership means the sovereignty safeguard is no longer solving the problem it was built for, for most signatories. But it remains live and substantively used for a small set of threshold-capable states, which is why founding_problem_status is authored as contested rather than flatly dead: declaring it uniformly dead would ignore the states for whom the option is still operative leverage, and declaring it uniformly live would ignore that the vast majority of parties derive no benefit from the clause at all.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    withdrawal_threshold_kernel_disagreement_location,
    'Is the disagreement between the high-threshold and low-threshold readings of Article X located in the treaty text itself, or in the absence of any adjudicating body empowered to rule on what counts as ''extraordinary events... jeopardized the supreme interests''?',
    'Comparative analysis of the 1968 drafting history (travaux préparatoires) against the actual institutional response to the 2003 North Korea withdrawal notice — if the drafters intended a high bar but no body has authority to enforce it, the disagreement is institutional, not textual.',
    'If the disagreement is textual (the drafters left it genuinely ambiguous), both readings are equally faithful and the kernel is irreducibly contested. If the disagreement is institutional (the text intended a high bar but no enforcement mechanism exists), the low-threshold reading is best understood as a de facto drift rather than a legitimate alternative interpretation, which would raise this constraint''s suppression/extraction assessment further.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(withdrawal_threshold_kernel_disagreement_location, conceptual, 'Textual ambiguity vs. institutional enforcement vacuum as the source of the withdrawal threshold contest.').

omega_variable(
    sibling_reading_structural_delta,
    'How would the high-threshold (regime-stability) sibling reading of Article X differ structurally from this sovereignty-preservation reading in beneficiary/victim assignment?',
    'Author a separate constraint story for the high-threshold reading and compare ε, beneficiaries, and victims directly.',
    'Under a high-threshold reading, the beneficiary and victim sets would likely invert: non-proliferation regime architects and neighboring states would become beneficiaries (protected by a costly, rare exit), while threshold-capable states would become the constrained party (losing the credible-exit leverage this story documents as a benefit). This inversion is exactly the kind of committer-axis structure Rule 2 requires routing to omega rather than folding into this story''s classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Documents where the sibling high-threshold reading would diverge structurally, without instantiating it here.').

omega_variable(
    precedent_versus_law_status,
    'Does the North Korea 2003 withdrawal constitute binding interpretive precedent for Article X, or is it a single unpunished violation with no formal precedential weight in international law?',
    'Survey of subsequent state practice and opinio juris: whether other states or international bodies have cited the North Korea withdrawal as establishing a legal threshold, versus treating it as an unresolved anomaly.',
    'If North Korea''s withdrawal is genuine customary-law precedent, the low-threshold reading has hardened into operative international law, raising the resistance-and-suppression profile of any future high-threshold enforcement attempt. If it is merely an unpunished anomaly, the low-threshold reading remains contested rather than settled, and the ε measured here reflects strategic leverage rather than legal certainty.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(precedent_versus_law_status, empirical, 'Whether the operative low threshold has hardened into customary international law or remains a contested anomaly.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 1968, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1968, 0.1).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(npt__tr_t2003, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2003, 0.3).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(npt__tr_t2018, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2018, 0.42).
narrative_ontology:measurement(npt__tr_t2024, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 2024, 0.44).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1968, 0.12).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 1995, 0.18).
narrative_ontology:measurement(npt__be_t2003, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2003, 0.35).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2010, 0.44).
narrative_ontology:measurement(npt__be_t2018, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2018, 0.49).
narrative_ontology:measurement(npt__be_t2024, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 2024, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1968, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1968, 0.2).
narrative_ontology:measurement(npt__su_t1995, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 1995, 0.28).
narrative_ontology:measurement(npt__su_t2003, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2003, 0.5).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2010, 0.55).
narrative_ontology:measurement(npt__su_t2018, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2018, 0.57).
narrative_ontology:measurement(npt__su_t2024, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nws_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the shared npt_treaty_text kernel. nnws_reading treats Article VI disarmament as a binding obligation purchasing NNWS compliance; nws_reading treats non-proliferation (Articles II/III) as the binding constraint with disarmament as aspirational. This story (withdrawal_threshold_reading) concerns Article X exit mechanics specifically and is structurally independent of both — its ε (0.52, tangled_rope) should not be averaged with or reconciled to either sibling's ε. The three stories are linked via network edges to preserve the kernel-family structure without merging their classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
