% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nnws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: npt_treaty_text__nnws_reading
 *   human_readable: NPT Article VI as Binding Disarmament Obligation (NNWS Reading)
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   This story instantiates the NNWS reading of the NPT kernel: Article VI's
 *   disarmament language is a binding obligation, and the non-proliferation
 *   commitments NNWS made are conditional restraint purchased in exchange for
 *   that obligation's eventual fulfillment. Under this reading, five decades
 *   without a disarmament timetable represents accumulating non-fulfillment
 *   of a bargain, not a fully-realized and closed transaction. This is
 *   deliberately NOT the same constraint as the NWS reading
 *   (npt_treaty_text__nws_reading, sibling file) in which Article VI is
 *   aspirational and non-proliferation is the only binding term — that
 *   reading has its own epsilon and its own file. Nor is it the
 *   withdrawal-threshold reading, which concerns Article X exit mechanics
 *   rather than the VI/II reciprocity question. Each reading is a genuinely
 *   different constraint with a different victim set and different extraction
 *   profile; conflating them would violate epsilon-invariance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, 0.42).
domain_priors:suppression_score(npt_treaty_text__nnws_reading, 0.35).
domain_priors:theater_ratio(npt_treaty_text__nnws_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, theater_ratio, 0.58).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nnws_reading, rope).
narrative_ontology:human_readable(npt_treaty_text__nnws_reading, "NPT Article VI as Binding Disarmament Obligation (NNWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nnws_reading, "international_law/arms_control").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nnws_reading, 'c9ad04f1-4ea2-4476-8205-17f7d469d6d2').
narrative_ontology:cs_kernel_codification('c9ad04f1-4ea2-4476-8205-17f7d469d6d2', fixed_text).
narrative_ontology:cs_authority_grounding('c9ad04f1-4ea2-4476-8205-17f7d469d6d2', distributed).
narrative_ontology:cs_reading_relation('c9ad04f1-4ea2-4476-8205-17f7d469d6d2', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9ad04f1-4ea2-4476-8205-17f7d469d6d2', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('c9ad04f1-4ea2-4476-8205-17f7d469d6d2', foundational, article_vi_creates_determinable_breach_condition).
narrative_ontology:cs_axiom_status(article_vi_creates_determinable_breach_condition, holdable).
narrative_ontology:cs_axiom_grounding('c9ad04f1-4ea2-4476-8205-17f7d469d6d2', article_vi_creates_determinable_breach_condition, conventional).
narrative_ontology:cs_axiom('c9ad04f1-4ea2-4476-8205-17f7d469d6d2', foundational, nonproliferation_restraint_is_conditional_not_absolute).
narrative_ontology:cs_axiom_status(nonproliferation_restraint_is_conditional_not_absolute, holdable).
narrative_ontology:cs_axiom_grounding('c9ad04f1-4ea2-4476-8205-17f7d469d6d2', nonproliferation_restraint_is_conditional_not_absolute, deontological).
narrative_ontology:cs_reference_frame('c9ad04f1-4ea2-4476-8205-17f7d469d6d2', reciprocal_grand_bargain_1968).
narrative_ontology:cs_drift_state('c9ad04f1-4ea2-4476-8205-17f7d469d6d2', post_cold_war_modernization_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c9ad04f1-4ea2-4476-8205-17f7d469d6d2', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nnws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, global_nonproliferation_norm).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, non_nuclear_weapon_states).
narrative_ontology:constraint_vindicates(npt_treaty_text__nnws_reading, reciprocal_bargain_doctrine).
narrative_ontology:constraint_vindicates(npt_treaty_text__nnws_reading, good_faith_negotiation_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Accepted a permanent, verified renunciation of nuclear weapons in exchange for a treaty text that binds nuclear weapon states to 'pursue negotiations in good faith' toward disarmament under Article VI. They forwent the weapons option irreversibly; the NWS obligation they received in return has never produced a disarmament timetable or verification regime. Their exit option — withdrawal under Article X, or joining TPNW — carries diplomatic and security costs that make it a real but expensive lever, not a costless one. They organize collectively at Review Conferences to press the reciprocity claim.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_text__nnws_reading, non_nuclear_weapon_states, beneficiary).

% Retain nuclear arsenals while treating Article VI as a good-faith aspiration rather than a binding schedule. They control the Review Conference consensus process (any one can block a final document), control the pace of bilateral arms reduction talks, and face no enforcement mechanism compelling disarmament. Their exit from the disarmament obligation costs them nothing structurally — the treaty imposes no penalty for non-fulfillment.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nuclear_weapon_states, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Administers safeguards inspections on NNWS nuclear programs but has no comparable mandate to verify NWS disarmament steps. Reports proliferation risk to the Security Council; has no parallel channel for reporting NWS non-fulfillment of Article VI. Its asymmetric verification mandate is itself evidence of the reciprocity imbalance the NNWS reading contests.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, iaea_verification_regime, observer,
    institutional, generational, analytical, global).

% A subset of NNWS states created the Treaty on the Prohibition of Nuclear Weapons outside the NPT framework, arguing the NPT's disarmament pillar had become inert. They are excluded from NPT Review Conference decision-making leverage over NWS behavior and instead built a competing normative instrument. Their existence functions as structural pressure on the NPT bargain but they hold no seat at the table that adjudicates Article VI compliance.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, tpnw_states, excluded,
    organized, generational, mobile, global).

% Convenes the five-year Review Conferences where the Article VI reciprocity dispute is formally aired. Has no independent enforcement authority; consensus rules mean any NWS can prevent a final document that names non-fulfillment. Functions largely as a recurring forum for the grievance rather than a mechanism that resolves it.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, review_conference_secretariat, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nnws_reading, nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_text__nnws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: universal non-proliferation requires NNWS to accept a permanent asymmetry (renouncing weapons) in exchange for a credible NWS commitment to eventually eliminate the asymmetry, preventing an otherwise-rational proliferation cascade.
% TRANSFER_FUNCTION: Moves security assurance (non-proliferation compliance, safeguards submission, technology-denial acceptance) from NNWS to the international system, in exchange for a promise of eventual disarmament that has not been redeemed — a durable transfer of restraint from NNWS with no matching transfer of weapons reduction from NWS.
% ABSENT_VOICES: TPNW states and non-nuclear civil society coalitions would argue Article VI has been treated as dead letter for five decades; they are not seated in the NPT Review Conference consensus process that would need to find NWS non-compliance, and NWS consensus-blocking power keeps that finding from ever being formally recorded.
% DISAPPEARANCE_RATIONALE: NWS would argue the world stays largely unchanged — bilateral arms control (New START-style instruments) operates independently of NPT Article VI and would continue on its own track. NNWS and TPNW states would argue the world rearranges significantly: the normative claim that binds NWS to any disarmament trajectory at all disappears, removing the last textual leverage NNWS possess and likely triggering renewed proliferation pressure as the non-proliferation bargain's legitimacy erodes.
% FOUNDING_PROBLEM: In 1968, the treaty was built to halt an anticipated proliferation cascade (predicted 15-25 nuclear states by 1980) by trading NNWS permanent renunciation for a credible NWS commitment to negotiate the elimination of nuclear arsenals, without which NNWS had little reason to accept a permanently subordinate security status.
% FOUNDING_PROBLEM_CORROBORATION: NNWS delegations and the TPNW preamble both attest the reciprocity problem remains live and unresolved — no NWS has eliminated its arsenal and total warhead counts, while reduced from Cold War peaks, remain in the thousands. Independent assessments from the International Court of Justice's 1996 advisory opinion (finding an obligation to pursue negotiations in good faith to a conclusion) and from arms-control research institutes outside any state's delegation corroborate that the obligation is real and outstanding, not merely an NNWS grievance-narrative; NWS delegations themselves characterize the obligation as procedural (an obligation to negotiate, not to conclude) rather than dead, which is itself the contest this reading names.
narrative_ontology:disappearance_verdict(npt_treaty_text__nnws_reading, contested).
narrative_ontology:founding_problem_status(npt_treaty_text__nnws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nnws_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__nnws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nnws_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_text__nnws_reading_tests).
:- end_tests(npt_treaty_text__nnws_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42) and rising slowly: the NNWS reading holds that a real transfer is occurring (permanent security subordination for an unredeemed promise) but the transfer is legitimated by consent at signature and by the treaty's continued diplomatic utility, keeping it well below snare-level extraction. Theater ratio is substantial and rising (0.58) because an increasing share of Review Conference activity — the primary enforcement venue under this reading — produces declarations and working papers rather than movement on actual warhead reduction; the forum increasingly performs the grievance rather than resolving it. Suppression is moderate (0.35): NNWS are not coerced into continued NPT membership by force, but exit (withdrawal, or TPNW accession) carries real diplomatic and security costs that dampen the practical exit option below full mobility.
 *
 * PERSPECTIVAL GAP:
 *   From the NWS agenda-setter seat, the arrangement is functioning coordination they administer in good faith, with disarmament proceeding at a pace dictated by strategic stability considerations outside the treaty's control. From the NNWS payer seat, the identical structure is a bargain running fifty-plus years in arrears, where the coordination story increasingly functions as cover for indefinite arsenal retention. The engine should compute these divergently from the stakeholder power/exit data — the claim (rope) and the metrics (moderate extraction, rising theater) are authored independently and are expected to produce seat-divergent classifications, which is the phenomenon this story exists to measure.
 *
 * DIRECTIONALITY LOGIC:
 *   NNWS sit near the target end of directionality: they made an irreversible renunciation (nuclear weapons cannot be un-renounced credibly once safeguards infrastructure and international reputation are built around non-possession) in exchange for a promise whose fulfillment they cannot compel. NWS sit near the beneficiary end: they retain full weapons capability, control the only forum (Review Conference consensus) that could formally register their non-fulfillment, and bear no structural cost for treating Article VI as aspirational. The coordination function is real — the treaty did prevent a proliferation cascade — but the reciprocity asymmetry this reading identifies is what keeps this a rope rather than a mountain: a mountain has no beneficiary structure, and this constraint plainly does.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing a 1970s-80s proliferation cascade) has arguably been substantially resolved via the non-proliferation half of the bargain — very few states have acquired weapons since 1968. Under the NNWS reading, this creates a mandatrophy risk in reverse: the coordination function that justified NNWS accepting the deal (mutual eventual elimination) may have gone dormant on the NWS side while the restraint obligation on the NNWS side remains fully enforced. This is precisely the founding_problem_status: contested finding — the treaty's proliferation-prevention function is live and successful, but the disarmament function it was reciprocally traded for shows signs of having been treated as satisfied by mere negotiation activity (theater) rather than concluded negotiation (substance), which the 1996 ICJ advisory opinion explicitly rejected as sufficient.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_vs_aspirational,
    'Does Article VI''s ''undertake to pursue negotiations in good faith'' create a binding obligation with a determinable breach condition, or an aspirational/procedural commitment satisfied by the mere conduct of negotiations regardless of outcome?',
    'This is precisely the interpretive fork between this reading (nnws_reading) and the sibling nws_reading. The 1996 ICJ advisory opinion found an obligation to pursue negotiations ''in good faith'' to a conclusion, which supports a binding reading, but the opinion is advisory and non-binding on member states, and NWS state practice has not treated it as dispositive. No tribunal with compulsory jurisdiction over NPT disputes has adjudicated the question.',
    'If binding, sustained NWS non-fulfillment constitutes an ongoing material breach and this reading''s extraction profile is understated; if aspirational, the nws_reading''s characterization is correct and this story''s beneficiary/victim structure would need to be substantially revised toward near-mountain (natural asymmetric bargain, no breach).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_binding_vs_aspirational, conceptual, 'Whether Article VI is a binding obligation with a breach condition or a procedural aspiration — the core fork between the nnws_reading and nws_reading kernel siblings.').

omega_variable(
    sibling_reading_delta_disarmament_pace,
    'What specific NWS behavior would a sibling-reading observer point to as evidence the nws_reading''s ''ongoing good-faith aspiration'' framing is correct rather than this reading''s ''accumulating non-fulfillment'' framing?',
    'Comparative analysis of post-Cold-War bilateral arms reduction treaties (START, New START) and their pace relative to total eliminable stockpile, cross-referenced against Review Conference final document language across cycles (1995, 2000, 2010 produced consensus disarmament language; 2005, 2015, 2022 did not).',
    'A sustained reduction trajectory consistent with eventual elimination would support the nws_reading''s characterization even under this reading''s stricter standard; a plateaued or reversing trajectory (as post-2010 modernization programs suggest) would strengthen this reading''s non-fulfillment claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_delta_disarmament_pace, empirical, 'What observable disarmament trajectory would discriminate between this reading''s non-fulfillment claim and the sibling nws_reading''s ongoing-aspiration claim.').

omega_variable(
    review_conference_consensus_veto_effect,
    'Does the NPT Review Conference consensus rule (any single state can block a final document) structurally prevent this reading''s reciprocity claim from ever being formally adjudicated within the treaty''s own institutions, regardless of its substantive merit?',
    'Track record analysis: consensus final documents naming NWS non-fulfillment have never been adopted (1995, 2000, 2010 language on disarmament steps was general/aspirational even when adopted); NWS delegations have blocked or diluted more specific language in subsequent cycles.',
    'If the veto structurally forecloses adjudication, the Review Conference forum functions as theater regardless of the substantive merits of either reading — supporting this story''s rising theater_ratio measurement independent of which reading of Article VI is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(review_conference_consensus_veto_effect, empirical, 'Whether the consensus rule itself, not just interpretive disagreement, prevents this reading''s claim from being formally tested within NPT institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nnws_reading, 1968, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1968, npt_treaty_text__nnws_reading, theater_ratio, 1968, 0.25).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_text__nnws_reading, theater_ratio, 1985, 0.35).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_text__nnws_reading, theater_ratio, 1995, 0.42).
narrative_ontology:measurement(npt__tr_t2005, npt_treaty_text__nnws_reading, theater_ratio, 2005, 0.48).
narrative_ontology:measurement(npt__tr_t2015, npt_treaty_text__nnws_reading, theater_ratio, 2015, 0.53).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_text__nnws_reading, theater_ratio, 2025, 0.58).

% Extraction over time
narrative_ontology:measurement(npt__be_t1968, npt_treaty_text__nnws_reading, base_extractiveness, 1968, 0.2).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_text__nnws_reading, base_extractiveness, 1985, 0.28).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_text__nnws_reading, base_extractiveness, 1995, 0.3).
narrative_ontology:measurement(npt__be_t2005, npt_treaty_text__nnws_reading, base_extractiveness, 2005, 0.35).
narrative_ontology:measurement(npt__be_t2015, npt_treaty_text__nnws_reading, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_text__nnws_reading, base_extractiveness, 2025, 0.42).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(npt_treaty_text__nnws_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nnws_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_text__nnws_reading, 0.1).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, tpnw_prohibition_regime).

% DUAL FORMULATION NOTE:
% This story and npt_treaty_text__nws_reading are sibling readings of the same kernel (npt_treaty_text), decomposed per the epsilon-invariance principle because they assign structurally different beneficiary/victim sets and different extractiveness to the same textual instrument. npt_treaty_text__withdrawal_threshold_reading is a third sibling addressing a disjoint textual question (Article X) within the same kernel. tpnw_prohibition_regime is a downstream constraint this reading's non-fulfillment claim causally influences — TPNW's existence is presented in this story as structural pressure generated by the perceived non-fulfillment of the Article VI bargain this reading asserts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
