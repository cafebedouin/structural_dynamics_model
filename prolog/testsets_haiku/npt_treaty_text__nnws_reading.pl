% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nnws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_text__nnws_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: npt_treaty_text__nnws_reading
 *   human_readable: NPT Article VI Binding Disarmament Obligation (NNWS Reading)
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   The NPT's Article VI text reads: 'Each of the Parties undertakes to
 *   pursue negotiations in good faith on effective measures relating to
 *   cessation of the nuclear arms race at an early date and to nuclear
 *   disarmament, and on a treaty on general and complete disarmament under
 *   strict and effective international control.' This constraint instantiates
 *   the NNWS reading: disarmament is a binding obligation on NWS, and
 *   non-proliferation by NNWS is conditional restraint purchasing that
 *   obligation's enforcement. The NNWS reading asserts that NNWS
 *   non-proliferation compliance is contingent on NWS visible progress toward
 *   disarmament — if NWS retain and modernize arsenals, NNWS should exit the
 *   regime. This reading competes with the NWS reading (disarmament is
 *   aspirational, non-proliferation is binding and permanent) and with the
 *   withdrawal-threshold reading (Article X exit is difficult vs. easy). The
 *   claim and metrics are intentionally independent: this reading is
 *   classified as ROPE because it attempts to coordinate NNWS
 *   non-proliferation with NWS disarmament via mutual binding obligation; the
 *   metrics describe an extractiveness of 0.52 (moderate) and theater at 0.41
 *   because the Review Conference pressure mechanism is real but structurally
 *   weak — consensus veto, no direct enforcement, leverage rests on NNWS exit
 *   threat credibility.
 *
 * KEY AGENTS:
 *   - Non-nuclear weapons states (NNWS): Beneficiary; organized power; hold 181 of 191 NPT parties; exercise pressure through Review Conferences and TPNW regime.
 *   - Nuclear weapons states (NWS) signatories: Payer (under this reading); powerful; five permanent UN Security Council members plus North Korea declarer; veto consensus at Review Conferences.
 *   - Nuclear threshold states (India, Pakistan, Israel, North Korea): Excluded from NPT; powerful; their weapons programs shape NNWS credibility calculations.
 *   - Review Conference operators: Agenda-setter; institutional; convene every five years; operate under consensus rule (any party can block).
 *   - TPNW regime: Agenda-setter, beneficiary; organized; created 2017 as alternative pressure mechanism on NWS; currently 92 signatories.
 *   - Disarmament advocacy coalitions: Beneficiary; moderate power; NGO networks and state delegations; amplify NNWS pressure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, 0.52).
domain_priors:suppression_score(npt_treaty_text__nnws_reading, 0.38).
domain_priors:theater_ratio(npt_treaty_text__nnws_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nnws_reading, rope).
narrative_ontology:human_readable(npt_treaty_text__nnws_reading, "NPT Article VI Binding Disarmament Obligation (NNWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nnws_reading, "international_law/arms_control").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nnws_reading, 'b31123d3-2e3f-49b2-bd86-ed001b501b32').
narrative_ontology:cs_kernel_codification('b31123d3-2e3f-49b2-bd86-ed001b501b32', fixed_text).
narrative_ontology:cs_authority_grounding('b31123d3-2e3f-49b2-bd86-ed001b501b32', lineage).
narrative_ontology:cs_interpretation_layer_present('b31123d3-2e3f-49b2-bd86-ed001b501b32').
narrative_ontology:cs_reading_relation('b31123d3-2e3f-49b2-bd86-ed001b501b32', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('b31123d3-2e3f-49b2-bd86-ed001b501b32', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('b31123d3-2e3f-49b2-bd86-ed001b501b32', foundational, disarmament_outcome_obligatory).
narrative_ontology:cs_axiom_status(disarmament_outcome_obligatory, holdable).
narrative_ontology:cs_axiom_grounding('b31123d3-2e3f-49b2-bd86-ed001b501b32', disarmament_outcome_obligatory, deontological).
narrative_ontology:cs_axiom('b31123d3-2e3f-49b2-bd86-ed001b501b32', foundational, nnws_non_proliferation_conditional_on_nws_disarmament_progress).
narrative_ontology:cs_axiom_status(nnws_non_proliferation_conditional_on_nws_disarmament_progress, holdable).
narrative_ontology:cs_axiom_grounding('b31123d3-2e3f-49b2-bd86-ed001b501b32', nnws_non_proliferation_conditional_on_nws_disarmament_progress, conventional).
narrative_ontology:cs_reference_frame('b31123d3-2e3f-49b2-bd86-ed001b501b32', article_vi_binding_disarmament_obligation).
narrative_ontology:cs_drift_state('b31123d3-2e3f-49b2-bd86-ed001b501b32', contemporary_post_2010_stalled_disarmament, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('b31123d3-2e3f-49b2-bd86-ed001b501b32', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nnws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, non_nuclear_weapons_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, tpnw_regime_actors).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, disarmament_advocacy_coalitions).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, nuclear_weapons_states_signatories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are bound by the NPT's non-proliferation obligation — they forgo nuclear weapons development. Under this reading, they receive a binding reciprocal commitment from NWS to pursue disarmament. They exercise pressure via Review Conferences and the Treaty on the Prohibition of Nuclear Weapons (TPNW) regime to enforce that reciprocal commitment. Their leverage is conditional renewal of the non-proliferation regime and threat to exit via Article X withdrawal, though exit carries massive security costs.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, non_nuclear_weapons_states, beneficiary,
    organized, generational, constrained, global).

% Are nominally bound by Article VI to pursue disarmament negotiations in good faith. Under this reading, they face NNWS pressure to treat disarmament as a binding obligation, not an aspirational goal. They argue disarmament is constrained by security dynamics and verification challenges; NNWS argue these are excuses for non-compliance. The NWS maintain security guarantees and nuclear modernization programs, structurally inconsistent with a binding disarmament reading.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nuclear_weapons_states_signatories, payer,
    powerful, generational, arbitrage, global).

% Are outside the NPT (India, Pakistan, Israel) or withdrew (North Korea). They would be most interested in whether the NPT's disarmament obligation is binding, as weak enforcement supports their own strategic autonomy. They are excluded from Review Conference negotiations but their behavior shapes NNWS decision calculus on regime credibility.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nuclear_threshold_states, excluded,
    powerful, generational, mobile, global).

% Convene every five years to assess NPT compliance and negotiate consensus documents. Under this reading, they adjudicate the binding-vs-aspirational interpretation of Article VI through consensus language, Final Acts, and pressure on NWS to declare disarmament timelines. They operate under consensus rule, which gives any NWS veto power over binding language.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, review_conference_operators, agenda_setter,
    institutional, generational, analytical, global).

% Operate the Treaty on the Prohibition of Nuclear Weapons (2017-present), which explicitly prohibits nuclear weapons and frames the NPT's Article VI as insufficiently enforced. They create an alternative regime to pressure NWS compliance via stigmatization and institutional competition. They benefit from repositioning NNWS collective voice and creating exit leverage.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, tpnw_regime_actors, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, tpnw_regime_actors, beneficiary).

% NGO networks and state coalitions advocating for a binding disarmament reading. They produce technical analyses of NWS compliance failures, coordinate NNWS negotiating positions, and publicize NWS non-compliance via media and legislative venues. They lack direct enforcement power but amplify NNWS pressure.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, disarmament_advocacy_coalitions, beneficiary,
    moderate, generational, mobile, global).

% Assess whether Article VI is binding or aspirational through textual analysis, state practice, and opinio juris. This reading interprets the preparatory history and opinio juris as supporting a binding obligation; the NWS reading interprets the same history as supporting aspirationality. Their analyses feed into state negotiating positions but do not directly enforce the treaty.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, security_studies_analysts, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nnws_reading, diffuse).
narrative_ontology:fixing_cost_class(npt_treaty_text__nnws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective-action problem: NNWS agree to forgo nuclear weapons (non-proliferation) in exchange for a binding NWS commitment to reduce existing arsenals (disarmament). Without mutual binding commitments, both sides face incentives to cheat — NNWS to acquire deterrents if NWS retain weapons, NWS to retain weapons if NNWS cannot be trusted.
% TRANSFER_FUNCTION: Moves compliance cost from NNWS to NWS: under a binding reading, NNWS pay the cost of forgoing nuclear deterrence development; NWS must pay the cost of disarmament timelines, verification regimes, and security restructuring. Under the NWS reading, NNWS pay non-proliferation costs while NWS pay only aspirational, non-binding costs.
% ABSENT_VOICES: Nuclear threshold states (India, Pakistan, Israel, North Korea) would argue that a weak binding enforcement on NWS disarmament serves their strategic interests by preserving their own deterrent autonomy. They are excluded from NPT Review Conferences but their weapons development shapes NNWS calculations about regime credibility and exit incentives. Disarmament advocates have a voice in civil society and some state delegations, but lack binding negotiating power.
% DISAPPEARANCE_RATIONALE: If the binding-disarmament reading of Article VI were authoritatively rejected and replaced with a pure-aspirational reading, NNWS collective confidence in the non-proliferation regime would erode sharply. TPNW adherence would accelerate, Article X withdrawal threats would become credible, and several NNWS would begin or accelerate nuclear weapons programs. The constraint that presently holds global proliferation in check is precisely the NNWS reading that disarmament is binding — if that reading loses institutional support, the non-proliferation foundation crumbles.
% FOUNDING_PROBLEM: The NPT's core bargain (1968): NNWS commit to non-proliferation in exchange for NWS commitment to pursue disarmament and share peaceful nuclear technology. The founding problem is incentive-compatible acquisition of nuclear weapons by NNWS absent verifiable disarmament by NWS. The text of Article VI frames disarmament as a binding obligation so the bargain is symmetric.
% FOUNDING_PROBLEM_CORROBORATION: NNWS delegations, the TPNW regime, and disarmament advocacy coalitions attest the problem is still live: NWS have not reduced arsenals (except for post-Cold-War reductions), have modernized warheads, and have made no binding timelines for further reductions — therefore the NNWS non-proliferation commitment is sustained by fear of defection rather than by reciprocal obligation, making the bargain asymmetric. NWS delegations attest the problem is substantially solved by non-proliferation success itself and that disarmament remains an aspirational long-term goal constrained by verification, deterrence, and security dynamics. Independent security analysts are split on the interpretation.
narrative_ontology:disappearance_verdict(npt_treaty_text__nnws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nnws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nnws_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__nnws_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nnws_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_text__nnws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness at 0.52 reflects moderate asymmetry: NNWS bear the permanent cost of non-proliferation (forgo deterrent development, remain vulnerable to proliferators); NWS bear the temporal and technological cost of disarmament (verification, security restructuring, warhead reductions). The asymmetry is moderate because (a) verification is genuinely challenging (omega_1), (b) security restructuring requires decades not years (omega_2), and (c) both sides have real coordination interests in non-proliferation. Theater at 0.41 reflects the gap between functional pressure (NNWS can credibly threaten exit, Review Conferences produce binding-language language, TPNW creates regime competition) and performative activity (NWS make rhetoric gestures, Review Conference consensus documents are non-binding, actual warhead reductions stalled post-Cold War). The measurements show rising extractiveness and theater from 1968 to 2026: as NNWS confidence in NWS compliance eroded (no post-2010 disarmament progress, nuclear modernization instead), the NNWS reading gained institutional salience (TPNW 2017), review conferences produced more confrontational language, and the theater element — NNWS performing credible exit threat — increased. Accessibility collapse at 0.62: alternatives to the non-proliferation regime exist (NNWS could acquire weapons, withdraw unilaterally) but are costly; the NPT remains the focal point. Resistance at 0.71: NWS actively resist the binding-disarmament reading through consensus veto, rhetorical reframing (disarmament as aspiration), and nuclear modernization; NNWS resistance to NWS non-compliance also rises (TPNW, Review Conference confrontation).
 *
 * PERSPECTIVAL GAP:
 *   The NNWS and NWS seats inhabit incommensurable readings of the same text. NNWS read 'undertakes to pursue negotiations...on nuclear disarmament' as obligating a disarmament outcome by a deadline. NWS read the same phrase as obligating only good-faith negotiation without outcome commitment. Both readings cite the same preparatory works and state practice. The gap is not empirical (different facts observed from the same seat) but semantic and institutional (different readings of what 'binding' means, what 'in good faith' commits to, what 'early date' temporally implies). The Review Conference operator seat attempts to bridge this gap through consensus language, but consensus rule gives NWS veto, so the operator seat cannot enforce the NNWS reading — the operator seat can only record that parties disagree and attempt rhetorical pressure. Security analysts are split: some argue the text supports binding interpretation, others argue the treaty's negotiating history supports aspirationality. From the observer seat (security analysts), the reading is contestable on textual and historical grounds (omega_1).
 *
 * DIRECTIONALITY LOGIC:
 *   NNWS are structural beneficiaries under this reading: they want a binding disarmament obligation on NWS to justify their own non-proliferation cost. From their seat, the constraint is coordination — the binding obligation makes non-proliferation rational rather than self-sacrificial. NWS are structural payers: they face pressure to reduce arsenals and make binding timelines, which they resist. Review Conference operators hold agenda-setter power: they determine what language counts as binding and what counts as aspirational. The directionality differs sharply by seat: NNWS see rope (mutual binding obligation solving the non-proliferation problem cooperatively); NWS see snare (NNWS non-proliferation is permanent and binding while disarmament is aspirational, giving NWS the benefit of non-proliferation without the cost of disarmament). The engine will compute different d values and type classifications for each seat from the same structural data — that divergence is the measurement.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents misclassifying non-proliferation as a snare by showing the real coordination function: NNWS forgo deterrent development (cost) in exchange for NWS disarmament commitment (benefit). If only non-proliferation were binding and disarmament aspirational, the classification would be pure snare (NNWS pay, NWS collect extraction). But the NNWS reading asserts that disarmament IS binding, so the arrangement is rope (mutual obligation, shared cost) — or at least, it is CLAIMED to be rope by the NNWS. The NWS reading would make it snare; the nnws_reading makes it rope. This constraint's entire analytical point is to model the structural asymmetry between the two readings and show how the same treaty text yields different classifications from different seats.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_textual_ambiguity,
    'Does Article VI''s phrase ''pursue negotiations in good faith'' constitute a binding obligation to achieve disarmament, or merely a binding obligation to negotiate without committing to outcome?',
    'Textual analysis of preparatory works (negotiating history), state practice over 50+ years, and opinio juris derived from Review Conference consensus language and judicial interpretation (if any); comparison with treaty interpretation canons (Vienna Convention on the Law of Treaties).',
    'If the obligation is to achieve disarmament by a deadline, NWS non-compliance is clearer and NNWS exit pressure is justified; if the obligation is merely to negotiate in good faith, NWS compliance is easier to demonstrate and the constraint becomes weaker. This omega directly determines the constraint''s ε.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_vi_textual_ambiguity, conceptual, 'Whether Article VI obligates disarmament outcome or negotiation process.').

omega_variable(
    verification_and_security_counter_constraint,
    'Are the NWS arguments about verification difficulty and deterrence security genuinely structural constraints on disarmament, or post-hoc rationalizations for non-compliance?',
    'Technical analysis of verification technologies (inspection regimes, enrichment monitoring, warhead accounting) and strategic studies on deterrence minimization. Comparison with non-proliferation verification success (IAEA, OSCE) and regional arms-control precedents (INF, New START).',
    'If verification is genuinely difficult and security restructuring is necessary, part of the measured extractiveness reflects legitimate coordination cost rather than NWS rent-seeking; if verification is achievable and security can be restructured, NWS non-compliance is purely extractive. The constraint''s classification may shift from rope (shared cost) to snare (pure extraction) depending on resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_and_security_counter_constraint, empirical, 'Whether disarmament verification and security restructuring are genuinely intractable.').

omega_variable(
    nnws_exit_threshold_and_credibility,
    'How many NNWS would actually exit the NPT (via Article X) if the binding-disarmament reading were authoritatively rejected, and how credible is that threat to NWS decision-making?',
    'Survey of NNWS policy positions, analysis of historical Article X withdrawal statements, modeling of proliferation incentives under different regime scenarios, and diplomatic reporting on NNWS consensus positions.',
    'A credible mass-exit threat gives NNWS leverage to enforce a binding reading; an incredible threat reduces their pressure to near zero. The constraint''s suppressibility (how hard it is to maintain) depends entirely on the credibility of NNWS exit leverage.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nnws_exit_threshold_and_credibility, empirical, 'Whether NNWS exit threat to enforce Article VI binding reading is credible.').

omega_variable(
    kernel_reading_contest_structure,
    'This constraint is one reading of the NPT treaty text kernel. The sibling readings are the NWS reading (disarmament as aspirational) and the withdrawal-threshold reading (Article X interpretation). How are these readings logically and institutionally related?',
    'The three readings coexist in different state delegations and analytical traditions. The nnws_reading asserts a binding disarmament obligation to support NNWS leverage. The nws_reading asserts aspirationality to preserve NWS autonomy. The withdrawal_threshold_reading disputes how easily NNWS can exit, which affects the leverage available to enforce either the binding or aspirational reading. These readings influence each other''s viability but do not logically foreclose each other — different parties hold different readings simultaneously.',
    'If the nnws_reading gains institutional adoption (consensus language in Review Conferences, TPNW regime pressure), it influences the withdrawal_threshold_reading by making exit more credible and the nws_reading by limiting NWS rhetorical space. None forecloses the others because they rest on different axioms (is disarmament binding? is exit threshold high or low?) that different institutional actors can adopt differently.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_structure, conceptual, 'This reading''s logical and institutional relationship to sibling kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nnws_reading, 1968, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_treaty_text__nnws_reading, theater_ratio, 1968, 0.05).
narrative_ontology:measurement_basis(npt__tr_t1968, projected).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_text__nnws_reading, theater_ratio, 1985, 0.12).
narrative_ontology:measurement_basis(npt__tr_t1985, observed).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_text__nnws_reading, theater_ratio, 2000, 0.28).
narrative_ontology:measurement_basis(npt__tr_t2000, observed).
narrative_ontology:measurement(npt__tr_t2015, npt_treaty_text__nnws_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement_basis(npt__tr_t2015, observed).
narrative_ontology:measurement(npt__tr_t2026, npt_treaty_text__nnws_reading, theater_ratio, 2026, 0.41).
narrative_ontology:measurement_basis(npt__tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_treaty_text__nnws_reading, base_extractiveness, 1968, 0.28).
narrative_ontology:measurement_basis(npt__be_t1968, projected).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_text__nnws_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement_basis(npt__be_t1985, observed).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_text__nnws_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement_basis(npt__be_t2000, observed).
narrative_ontology:measurement(npt__be_t2015, npt_treaty_text__nnws_reading, base_extractiveness, 2015, 0.51).
narrative_ontology:measurement_basis(npt__be_t2015, observed).
narrative_ontology:measurement(npt__be_t2026, npt_treaty_text__nnws_reading, base_extractiveness, 2026, 0.52).
narrative_ontology:measurement_basis(npt__be_t2026, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(npt_treaty_text__nnws_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nnws_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_text__nnws_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, tpnw_regime_operation).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, nnws_proliferation_incentive_structure).

% DUAL FORMULATION NOTE:
% The NPT Article VI text admits at least three structurally distinct constraint readings: this nnws_reading models disarmament as binding obligation (rope, moderate epsilon); the nws_reading models disarmament as aspirational (snare or piton, much lower epsilon for NNWS coordination, higher extraction). These are not observable-dependent measurements of one constraint — they are different constraints instantiated by different treaty interpretations. Each has its own epsilon, beneficiary/victim structure, and institutional mechanisms. Link them via affects_constraints to model how one reading's institutional adoption affects the others' viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_text__nnws_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
