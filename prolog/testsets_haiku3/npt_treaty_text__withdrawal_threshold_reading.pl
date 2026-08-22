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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: npt_treaty_text__withdrawal_threshold_reading
 *   human_readable: NPT Article X Withdrawal Threshold Ambiguity: High vs. Low Exit Cost Reading
 *   domain: international_law/arms_control/treaty_interpretation
 *
 * SUMMARY:
 *   Article X of the NPT permits state withdrawal 'if it decides that
 *   extraordinary events, related to the subject matter of the Treaty, have
 *   jeopardised its supreme interests.' This reading interprets the
 *   withdrawal threshold as ambiguous between a HIGH threshold (extraordinary
 *   events defined strictly by NWS and regime administrators, requiring
 *   lengthy notice and good-faith process) and a LOW threshold (extraordinary
 *   events defined loosely by the withdrawing state, with notice permitting
 *   near-immediate exit). The constraint's extractiveness lies in this
 *   ambiguity itself: the uncertainty preserves NWS regime control while
 *   maintaining NNWS exit credibility. North Korea's 2003 withdrawal (claimed
 *   by NWS as procedurally improper despite unambiguous text) exemplifies the
 *   tension — the high-threshold reading can only survive by delegitimizing
 *   precedent. For threshold states like Iran and Japan/South Korea, the
 *   low-threshold reading's credibility determines whether non-weapon
 *   commitments are reversible safety valves or permanent constraints. The
 *   reading is one instantiation of the NPT kernel; sibling readings
 *   (nnws_reading: Article VI disarmament as binding; nws_reading: Article VI
 *   as aspirational) establish different stakes for withdrawal
 *   interpretation.
 *
 * KEY AGENTS:
 *   - NWS enforcement coalition (US, Russia, China, UK, France): institutional agenda-setters of high-threshold reading; preserve regime control through interpretation hegemony
 *   - Threshold states (Iran, Japan, South Korea): moderate-power beneficiaries of low-threshold reading; maintain exit credibility despite security dependence
 *   - Treaty regime administrators (IAEA, UN Office for Disarmament Affairs): organized payers of regime-stability costs; bear reputational damage from withdrawal ambiguity and precedent erosion
 *   - Proliferation skeptics (state governments, security analysts): beneficiaries of explicit naming of conditional structure; low-threshold reading validates their critique of NWS asymmetry
 *   - North Korea (precedent): excluded party whose 2003 withdrawal both challenged high-threshold reading and became data point subject to NWS delegitimization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__withdrawal_threshold_reading, 0.68).
domain_priors:suppression_score(npt_treaty_text__withdrawal_threshold_reading, 0.71).
domain_priors:theater_ratio(npt_treaty_text__withdrawal_threshold_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(npt_treaty_text__withdrawal_threshold_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__withdrawal_threshold_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_text__withdrawal_threshold_reading, "NPT Article X Withdrawal Threshold Ambiguity: High vs. Low Exit Cost Reading").
narrative_ontology:topic_domain(npt_treaty_text__withdrawal_threshold_reading, "international_law/arms_control/treaty_interpretation").

domain_priors:requires_active_enforcement(npt_treaty_text__withdrawal_threshold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__withdrawal_threshold_reading, '153e382a-865e-46b9-8771-af9d4880d7c5').
narrative_ontology:cs_kernel_codification('153e382a-865e-46b9-8771-af9d4880d7c5', fixed_text).
narrative_ontology:cs_authority_grounding('153e382a-865e-46b9-8771-af9d4880d7c5', lineage).
narrative_ontology:cs_interpretation_layer_present('153e382a-865e-46b9-8771-af9d4880d7c5').
narrative_ontology:cs_reading_relation('153e382a-865e-46b9-8771-af9d4880d7c5', npt_treaty_text__nnws_reading, influences).
narrative_ontology:cs_reading_relation('153e382a-865e-46b9-8771-af9d4880d7c5', npt_treaty_text__nws_reading, influences).
narrative_ontology:cs_axiom('153e382a-865e-46b9-8771-af9d4880d7c5', foundational, withdrawal_threshold_ambiguity_constrains_regime_control).
narrative_ontology:cs_axiom_status(withdrawal_threshold_ambiguity_constrains_regime_control, holdable).
narrative_ontology:cs_axiom_grounding('153e382a-865e-46b9-8771-af9d4880d7c5', withdrawal_threshold_ambiguity_constrains_regime_control, deontological).
narrative_ontology:cs_axiom('153e382a-865e-46b9-8771-af9d4880d7c5', secondary, nws_interpretation_hegemony_unsustainable_post_north_korea).
narrative_ontology:cs_axiom_status(nws_interpretation_hegemony_unsustainable_post_north_korea, holdable).
narrative_ontology:cs_axiom_grounding('153e382a-865e-46b9-8771-af9d4880d7c5', nws_interpretation_hegemony_unsustainable_post_north_korea, empirically_contingent).
narrative_ontology:cs_reference_frame('153e382a-865e-46b9-8771-af9d4880d7c5', high_threshold_reading_enforced).
narrative_ontology:cs_drift_state('153e382a-865e-46b9-8771-af9d4880d7c5', post_north_korea_precedent_2003_to_contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('153e382a-865e-46b9-8771-af9d4880d7c5', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, threshold_states_iran_japan_korea).
narrative_ontology:constraint_beneficiary(npt_treaty_text__withdrawal_threshold_reading, proliferation_skeptics).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, treaty_regime_stability_advocates).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, nws_enforcement_coalition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(npt_treaty_text__withdrawal_threshold_reading, threshold_states_iran_japan_korea).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five NWS (US, Russia, China, UK, France) collectively set the interpretation of Article X, enforce it through NPT Review Conferences, IAEA authority, and diplomatic pressure, and benefit directly from the high-threshold reading because it preserves regime control and prevents cascading withdrawals. They administer the extraordinary-events standard through state practice, not through formal adjudication, which gives them interpretive flexibility. North Korea's 2003 withdrawal forced them to invest heavily in delegitimization rhetoric (claiming procedural impropriety despite clear text), raising the maintenance cost but securing regime persistence.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, nws_enforcement_coalition, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Iran, Japan, and South Korea are technically capable of rapid nuclear-weapon development but bound by NPT and security-dependence calculations. The low-threshold reading of Article X is crucial to their political sustainability of non-weapon commitments — it names the condition under which those commitments become void (extraordinary events threatening supreme interests). Without the low-threshold reading's credibility, their populations and security establishments would view non-weapon status as permanently extracted by NWS. They benefit from the reading's ambiguity because it keeps both the regime's legitimacy and their exit option alive. They pay by restraining capabilities they could develop, accepting intrusive IAEA inspections, and enduring NWS security guarantees they did not negotiate.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, threshold_states_iran_japan_korea, beneficiary,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__withdrawal_threshold_reading, threshold_states_iran_japan_korea, payer).

% Arms-control experts, international-law scholars, NPT review conference participants, and NGOs committed to the non-proliferation regime bear the cost of the withdrawal-threshold ambiguity because it undermines the regime's perceived permanence and legal clarity. Each time NWS invoke the high-threshold reading and NNWS or proliferation skeptics cite the low-threshold reading, the regime's authority erodes — regime administrators must work continuously to maintain the hegemony of the high-threshold interpretation despite mounting precedent and textual problems. They cannot exit from treaty maintenance because their legitimacy depends on the regime surviving.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, treaty_regime_stability_advocates, payer,
    organized, civilizational, constrained, global).

% Analysts, scholars, and some government officials (especially in non-aligned states) skeptical that the NPT achieves genuine disarmament or provides equitable burden-sharing benefit from the low-threshold reading's existence. The reading makes explicit that NNWS accepted non-weapon status conditionally, contingent on NWS disarmament (Article VI). When NWS fail to disarm, proliferation skeptics cite the low-threshold reading as evidence that the regime is asymmetric and that NNWS retain sovereign choice. They collect reputational benefit and legitimacy for their critique without needing to exercise the withdrawal option themselves.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, proliferation_skeptics, beneficiary,
    moderate, biographical, mobile, global).

% The International Atomic Energy Agency administers nuclear safeguards and verification under the NPT but remains deliberately neutral on treaty interpretation. When member states invoke Article X (Iran's 2016 JCPOA negotiation, threatened 2020 suspension), the IAEA's inspection and cooperation regimes become leverage points for NWS enforcement. The agency observes how different withdrawal-threshold readings affect member states' willingness to allow inspections and maintain fuel-supply dependence — it is a technical body watching how law and politics interact on the ground.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, iaea_technical_body, observer,
    institutional, generational, analytical, global).

% The NWS themselves, insofar as they are Article VI disarmament obligors (not Article X interpreters), are excluded from the withdrawal-threshold debate. Their systematic non-compliance with Article VI disarmament obligations is not treated as grounds for NNWS withdrawal under the high-threshold reading — the two provisions are administratively separated despite their logical connection. This structural exclusion is necessary to preserve NWS regime control: if their Article VI non-compliance were counted as evidence that extraordinary events have occurred, NNWS withdrawal would become justified and the regime would unravel. They are trapped because they cannot openly defend their own non-compliance without undermining their high-threshold reading.
narrative_ontology:constraint_stakeholder(npt_treaty_text__withdrawal_threshold_reading, nws_disarmament_defaulters, excluded,
    institutional, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__withdrawal_threshold_reading, nws_enforcement_coalition).
narrative_ontology:fixing_cost_class(npt_treaty_text__withdrawal_threshold_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collective-action problem of nuclear disarmament commitment: NWS accept constraints (Article VI) in exchange for NNWS restraint (Articles I–II). Both sides benefit from knowing the other's binding commitment is credible AND from maintaining face-saving language about exit if the other side catastrophically breaches.
% TRANSFER_FUNCTION: Moves security assurances, technical benefits (nuclear power access, IAEA safeguards legitimacy), and strategic flexibility FROM NWS TO NNWS in exchange for non-weapon restraint and submission to intrusive verification. The withdrawal threshold determines the cost of exit: a HIGH threshold means NNWS pay a large reputational and strategic cost to leave; a LOW threshold means the exit option is permanently available at low cost, which undermines NWS leverage.
% ABSENT_VOICES: NNWS that have already withdrawn (North Korea, 2003) or threatened withdrawal (Iran, 2020) are present but their interpretations are delegitimized by the NWS coalition as aberrant or procedurally improper. Voices internal to NWS that would support low-threshold withdrawal (disarmament advocates within those states) are excluded from the official treaty interpretation process — treaty reading is reserved to state-level security establishments.
% DISAPPEARANCE_RATIONALE: If this withdrawal-threshold ambiguity disappeared and a single HIGH-threshold reading became legally certain, threshold states would lose credible exit threats and security guarantees would become less substitutable for weapons — expect a wave of capability-development hedging (Japan, South Korea, Iran would accelerate fuel-cycle work or weapons research). If a LOW-threshold reading became certain instead, NWS leverage over NNWS would collapse and the NPT would likely unravel — the regime's persistence depends on the ambiguity being unresolved.
% FOUNDING_PROBLEM: The 1968 NPT negotiators faced irreconcilable preferences: NWS wanted permanent non-proliferation constraints on NNWS; NNWS wanted assurance that they retained exit if NWS failed to disarm (Article VI) or if security circumstances demanded weapons. Article X withdrawal language was left ambiguous to close the deal — both sides could claim victory in interpretation.
% FOUNDING_PROBLEM_CORROBORATION: Treaty negotiation records (UN archives, declassified Soviet and US documents) confirm the deliberate ambiguity. Contemporary threshold-state governments (Iranian officials, Japanese defense ministry statements) and proliferation scholars cite Article X ambiguity as structurally necessary to make non-weapon commitments politically sustainable. NWS governments deny the ambiguity exists and claim their high-threshold reading is the settled interpretation — but their denial itself proves the founding problem is live, not a relic.
narrative_ontology:disappearance_verdict(npt_treaty_text__withdrawal_threshold_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__withdrawal_threshold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__withdrawal_threshold_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__withdrawal_threshold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__withdrawal_threshold_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness (0.68) is substantial because the high-threshold reading concentrates exit-cost power in NWS hands, enabling them to enforce non-weapon commitments that would otherwise be voluntary. Suppression (0.71) is high because the constraint's persistence depends on actively delegitimizing the low-threshold reading and North Korea precedent — the regime must suppress the alternative interpretation to hold. Theater ratio (0.42) is moderate: the extraordinary-events language and good-faith process are real (treaty text exists and procedures matter), but a growing share of regime maintenance activity defends interpretation hegemony rather than performing the extraordinary-events criterion honestly. Accessibility collapse (0.58) is moderate-high: once states understand the withdrawal option exists, they cannot un-know it, but the NWS coalition works to collapse its practical availability by declaring it procedurally unavailable. Resistance (0.72) is high because threshold states explicitly contest the high-threshold reading and maintain rhetorical commitment to the low reading; proliferation skeptics amplify resistance through scholarship and diplomatic pressure. The measurement series show a ratchet: extractiveness and suppression intensified from 2003 (North Korea withdrawal) through 2016 (Iran JCPOA negotiation) as NWS doubled down on high-threshold interpretation in response to precedent erosion. The plateau from 2016–2026 reflects stabilization — Iran's JCPOA suspension (2018) and return (2021) both cited Article X ambiguity without triggering actual withdrawal, suggesting the low-threshold option is maintained in reserve but not exercised, preserving NWS regime control while keeping the exit threat credible.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS perspective, Article X is a safety valve with emergency procedures (the high threshold) that preserves regime permanence and credibility. From the threshold-state perspective, Article X is a sovereignty-preservation clause that keeps non-weapon commitments conditional and reversible. From the regime-stability perspective, the withdrawal threshold is an interpretive matter with a settled answer (high); from the proliferation-critique perspective, the threshold ambiguity is itself the signal — it demonstrates the regime is built on NWS privilege and NNWS conditional acceptance. The engine computes per-seat perception; these perspectival gaps should emerge from the structural data (power atoms, exit options, role declarations) without the commentary adjudicating which perspective is correct.
 *
 * DIRECTIONALITY LOGIC:
 *   The NWS enforcement coalition is the structural beneficiary (d near 0.2–0.3): they set the high-threshold reading and capture regime legitimacy; exit costs for NNWS subsidize their control. Threshold states carry high directionality (d near 0.75–0.85): they bear extraction (constrained exit under high-threshold reading) but retain a partial escape route (the low-threshold reading remains alive in technical discourse even if delegitimized). Treaty regime administrators sit near symmetric or slightly toward payer (d near 0.55–0.65): they benefit from regime stability but pay the cost of maintaining increasingly theatrical interpretation hegemony as precedent erodes. Proliferation skeptics are partially benefited by explicit naming of the conditional structure (d near 0.35–0.45): the low-threshold reading vindicates their structural critique, but they cannot capture the regime or enforce the reading themselves. The engine should compute these per-seat from the power atoms and exit options declared in stakeholders; the metrics and directionality logic support that computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (NWS-NNWS commitment assurance during Cold War disarmament negotiations, 1968) is CONTESTED, not dead: NWS claim the problem persists (proliferation risk demands permanent regime); NNWS and proliferation skeptics argue it is dead (NWS failed to disarm, rendering Article VI aspirational; the regime persists only because NNWS internalized non-weapon norms and face-saving ambiguity prevents confrontation). The mismatch between founding_problem_status=contested and disappearance_verdict=world_rearranges suggests mandatrophy risk: if the withdrawal threshold were clarified as HIGH and legally certain, the regime would not disappear (NWS would enforce it), but threshold states would accelerate weapons-development hedging (world rearranges). If clarified as LOW and legally certain, the regime would unravel (world rearranges dramatically). The constraint's persistence does NOT depend on the founding problem remaining live — it depends on the founding problem staying CONTESTED and the withdrawal threshold staying AMBIGUOUS. This is a piton-spectrum risk: the arrangement persists through interpretive ambiguity rather than through genuine coordination or enforcement legitimacy. The theater_ratio trajectory (rising from 0.25 to 0.42) and the suppression_requirement plateau despite North Korea precedent both indicate that regime maintenance is increasingly performative — the extraordinary-events standard is not applied consistently; instead, any withdrawal is declared procedurally improper and the high-threshold reading is reasserted post-hoc.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_enforcement_gap,
    'Is the NWS failure to comply with Article VI disarmament obligations (measured by continued weapons modernization, non-verified arsenal reductions, indefinite postponement of disarmament deadlines) grounds for NNWS withdrawal under Article X, or are Articles VI and X structurally independent?',
    'A binding interpretation by the International Court of Justice or a reconvened NPT Review Conference explicitly linking Article VI compliance to Article X withdrawal rights would resolve it. Absent such adjudication, the ambiguity persists because NWS control the interpretation machinery.',
    'If Article VI failure is grounds for withdrawal, the threshold reading shifts decisively LOW — NNWS gain a standing withdrawal justification tied to measurable NWS behavior. If Articles are independent, the high-threshold reading holds and NNWS lack a clear trigger unless facing direct security threat. This determines whether the exit option is rhetorical (low impact) or actionable (high impact).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(article_vi_enforcement_gap, conceptual, 'Whether NPT disarmament non-compliance by NWS creates withdrawal rights for NNWS.').

omega_variable(
    north_korea_precedent_finality,
    'Does North Korea''s 2003 withdrawal (whether or not procedurally proper under the NWS high-threshold reading) establish a new de facto precedent that lowers the threshold, or does NWS insistence that the withdrawal was improper preserve the high-threshold reading as binding?',
    'If a second NNWS withdraws and NWS does not use comparable delegitimization language, the precedent shifts toward low threshold. If NWS applies the same high-threshold rebuttal consistently across multiple withdrawals, the high reading persists as binding despite North Korea''s defection.',
    'Precedent-shift would undermine NWS enforcement coalition leverage by weakening the claim that withdrawal is extraordinary. Precedent-preservation maintains the high threshold but at growing reputational cost — each new claim of impropriety sounds more like post-hoc delegitimization than interpretation of settled law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(north_korea_precedent_finality, empirical, 'Whether North Korea precedent shifts the withdrawal threshold through established practice or remains isolated.').

omega_variable(
    security_guarantee_substitutability,
    'For threshold states (Iran, Japan, South Korea), is the credibility of the withdrawal exit option substitutable with credible security guarantees from NWS, or are the two incommensurable — does a guaranteed security umbrella eliminate the need for withdrawal optionality?',
    'Observation of how threshold states respond to formal, binding security-guarantee offers (extended nuclear deterrence, alliance deepening, treaty amendments) in relation to their Article X withdrawal rhetoric. If guarantees satisfy threshold-state demands, the low-threshold reading becomes less necessary to them; if they demand BOTH guarantees AND withdrawal ambiguity, the exit option is independent of security assurance.',
    'If substitutable, NWS can reduce extraction pressure by offering security guarantees instead of conceding low-threshold withdrawal — the constraint''s extractiveness would decline. If incommensurable, threshold states retain permanent exit leverage regardless of guarantee quality, keeping extractiveness high.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(security_guarantee_substitutability, preference, 'Whether security guarantees are an adequate substitute for withdrawal option credibility in threshold states'' threat calculus.').

omega_variable(
    reading_vs_sibling_kernel_identity,
    'Is this withdrawal-threshold reading (Article X exit cost) a separable interpretation issue, or is it inextricably bound to the NNWS/NWS disarmament-obligation reading (the nnws_reading and nws_reading)? If bound, does this reading presuppose and amplify one of the sibling readings?',
    'Textual analysis of Article X in isolation versus Article X read in context of Article VI: can Article X withdrawal be interpreted without reference to whether Article VI obligations are binding on NWS? If yes, the reading is independent; if no, the reading is parasitic on the sibling readings'' outcome.',
    'If parasitic, this reading''s type classification (tangled_rope vs. snare) depends on which sibling reading becomes canonical — the extraction magnitude varies with the upstream reading''s status. If independent, this reading''s classification stands alone, though linked structurally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_kernel_identity, conceptual, 'Whether the withdrawal-threshold reading is interpretively independent or presupposes one of the sibling readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__withdrawal_threshold_reading, 0, 56).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t0, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(npt__tr_t8, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(npt__tr_t16, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 16, 0.33).
narrative_ontology:measurement(npt__tr_t28, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 28, 0.4).
narrative_ontology:measurement(npt__tr_t40, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 40, 0.43).
narrative_ontology:measurement(npt__tr_t56, npt_treaty_text__withdrawal_threshold_reading, theater_ratio, 56, 0.42).

% Extraction over time
narrative_ontology:measurement(npt__be_t0, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(npt__be_t8, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(npt__be_t16, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 16, 0.6).
narrative_ontology:measurement(npt__be_t28, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 28, 0.67).
narrative_ontology:measurement(npt__be_t40, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 40, 0.69).
narrative_ontology:measurement(npt__be_t56, npt_treaty_text__withdrawal_threshold_reading, base_extractiveness, 56, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t0, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(npt__su_t8, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 8, 0.62).
narrative_ontology:measurement(npt__su_t16, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 16, 0.68).
narrative_ontology:measurement(npt__su_t28, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 28, 0.73).
narrative_ontology:measurement(npt__su_t40, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement(npt__su_t56, npt_treaty_text__withdrawal_threshold_reading, suppression_requirement, 56, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__withdrawal_threshold_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_text__withdrawal_threshold_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nnws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__withdrawal_threshold_reading, npt_treaty_text__nws_reading).

% DUAL FORMULATION NOTE:
% This reading is one of three distinct interpretations of the NPT kernel. The withdrawal-threshold reading depends on Article VI interpretation (sibling readings): if disarmament is binding (nnws_reading), withdrawal becomes more justifiable on grounds of NWS non-compliance (low-threshold candidate); if disarmament is aspirational (nws_reading), withdrawal remains extraordinary (high-threshold holds). This reading's extractiveness is sensitive to upstream interpretation but is analyzed independently. All three readings share the same NPT text; they differ in which provision they emphasize and how they resolve Article X ambiguity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(npt_treaty_text__withdrawal_threshold_reading, institutional, 0.25).
constraint_indexing:directionality_override(npt_treaty_text__withdrawal_threshold_reading, moderate, 0.78).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
