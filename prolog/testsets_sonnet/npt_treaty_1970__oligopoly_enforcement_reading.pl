% ============================================================================
% CONSTRAINT STORY: npt_treaty_1970__oligopoly_enforcement_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_npt_treaty_1970__oligopoly_enforcement_reading, []).

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
 *   constraint_id: npt_treaty_1970__oligopoly_enforcement_reading
 *   human_readable: NPT Articles I-II as Primary Obligation: Nuclear Oligopoly Enforcement Reading
 *   domain: international_law/nuclear_nonproliferation/regime_theory
 *
 * SUMMARY:
 *   This story instantiates the oligopoly-enforcement reading of the NPT
 *   kernel: Articles I-II (the horizontal nonproliferation obligations
 *   binding on non-nuclear-weapon states) are treated as the treaty's real,
 *   actively enforced core, while Article VI's disarmament language is
 *   treated as contingent and aspirational — a diplomatic commitment with no
 *   verification mechanism, no timeline, and no consequence structure
 *   comparable to what NNWS face under comprehensive safeguards. Under this
 *   reading, the treaty functions as an institutionalized oligopoly: it locks
 *   in the five recognized nuclear-weapon states' privileged status while
 *   imposing the coordination costs of nonproliferation almost entirely on
 *   those who did not already have the weapons. The coordination function
 *   (reducing proliferation risk) is real, but it runs through an enforcement
 *   architecture with a severe and persistent asymmetry. This is a distinct
 *   constraint from the reciprocal_disarmament_reading (which treats Article
 *   VI as the binding, temporally urgent half of a genuine bargain) and from
 *   the withdrawal_sovereignty_reading (which centers Article X exit rights)
 *   — each reading has a different beneficiary/victim structure and a
 *   different ε, and forcing them into one story would violate ε-invariance.
 *
 * KEY AGENTS:
 *   - p5_nuclear_weapon_states: primary beneficiary (institutional/arbitrage) — retains privileged status, faces no reciprocal verification
 *   - iaea_verification_bureaucracy: institutional beneficiary — mandate and budget depend on asymmetric inspection regime
 *   - threshold_capable_nnws: primary target (moderate/constrained) — bears security cost of unmet Article VI bargain
 *   - nnws_civilian_nuclear_programs: payer — inspection burden and technology-access restriction
 *   - npt_review_conference_delegates: analytical observer — documents but cannot enforce the gap
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, 0.71).
domain_priors:suppression_score(npt_treaty_1970__oligopoly_enforcement_reading, 0.78).
domain_priors:theater_ratio(npt_treaty_1970__oligopoly_enforcement_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(npt_treaty_1970__oligopoly_enforcement_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_1970__oligopoly_enforcement_reading, tangled_rope).
narrative_ontology:human_readable(npt_treaty_1970__oligopoly_enforcement_reading, "NPT Articles I-II as Primary Obligation: Nuclear Oligopoly Enforcement Reading").
narrative_ontology:topic_domain(npt_treaty_1970__oligopoly_enforcement_reading, "international_law/nuclear_nonproliferation/regime_theory").

domain_priors:requires_active_enforcement(npt_treaty_1970__oligopoly_enforcement_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_1970__oligopoly_enforcement_reading, 'fdd7a5be-b906-43de-ab07-1b3e957ace41').
narrative_ontology:cs_kernel_codification('fdd7a5be-b906-43de-ab07-1b3e957ace41', fixed_text).
narrative_ontology:cs_authority_grounding('fdd7a5be-b906-43de-ab07-1b3e957ace41', extraction).
narrative_ontology:cs_interpretation_layer_present('fdd7a5be-b906-43de-ab07-1b3e957ace41').
narrative_ontology:cs_reading_relation('fdd7a5be-b906-43de-ab07-1b3e957ace41', npt_treaty_1970__reciprocal_disarmament_reading, coexists_with).
narrative_ontology:cs_reading_relation('fdd7a5be-b906-43de-ab07-1b3e957ace41', npt_treaty_1970__withdrawal_sovereignty_reading, influences).
narrative_ontology:cs_axiom('fdd7a5be-b906-43de-ab07-1b3e957ace41', foundational, horizontal_restraint_is_the_enforceable_core).
narrative_ontology:cs_axiom_status(horizontal_restraint_is_the_enforceable_core, holdable).
narrative_ontology:cs_axiom_grounding('fdd7a5be-b906-43de-ab07-1b3e957ace41', horizontal_restraint_is_the_enforceable_core, conventional).
narrative_ontology:cs_axiom('fdd7a5be-b906-43de-ab07-1b3e957ace41', foundational, article_vi_is_non_self_executing_aspiration).
narrative_ontology:cs_axiom_status(article_vi_is_non_self_executing_aspiration, holdable).
narrative_ontology:cs_axiom_grounding('fdd7a5be-b906-43de-ab07-1b3e957ace41', article_vi_is_non_self_executing_aspiration, conventional).
narrative_ontology:cs_reference_frame('fdd7a5be-b906-43de-ab07-1b3e957ace41', five_state_oligopoly_stewardship).
narrative_ontology:cs_drift_state('fdd7a5be-b906-43de-ab07-1b3e957ace41', post_2015_review_conference_failure, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fdd7a5be-b906-43de-ab07-1b3e957ace41', '').
narrative_ontology:cs_kernel_id(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, iaea_verification_bureaucracy).
narrative_ontology:constraint_beneficiary(npt_treaty_1970__oligopoly_enforcement_reading, existing_nuclear_industry_suppliers).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, threshold_capable_nnws).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, nnws_civilian_nuclear_programs).
narrative_ontology:constraint_victim(npt_treaty_1970__oligopoly_enforcement_reading, nnws_taxpayers_funding_safeguards).
narrative_ontology:constraint_vindicates(npt_treaty_1970__oligopoly_enforcement_reading, nonproliferation_norm_universality).
narrative_ontology:constraint_vindicates(npt_treaty_1970__oligopoly_enforcement_reading, horizontal_restraint_as_global_good).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Retain legally recognized nuclear arsenals under Article IX's grandfather clause while facing no verification, inspection, or binding timeline on Article VI disarmament. Sit on the NPT review conference machinery and the UN Security Council, shaping how compliance is interpreted and enforced against others. Modernize warheads and delivery systems continuously while treating Article VI as a diplomatic aspiration rather than a legal obligation with teeth.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states, beneficiary,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states, agenda_setter).

% Administers comprehensive safeguards agreements and additional protocols applied almost exclusively to non-nuclear-weapon states. Its budget, mandate, and institutional relevance depend on the asymmetric inspection regime continuing indefinitely; it has no comparable access or mandate to verify P5 arsenal reductions.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, iaea_verification_bureaucracy, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(npt_treaty_1970__oligopoly_enforcement_reading, iaea_verification_bureaucracy, agenda_setter).

% Nuclear Suppliers Group members and established fuel-cycle exporters benefit from a licensing and export-control regime that channels civilian nuclear commerce through incumbent suppliers, who are concentrated in the same P5-plus-allies bloc. New entrants face steep compliance costs justified by nonproliferation logic.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, existing_nuclear_industry_suppliers, beneficiary,
    organized, generational, arbitrage, global).

% States with the technical capacity to pursue weapons programs but bound by treaty commitments face intensive scrutiny, sanctions threats, and diplomatic pressure whenever their civilian programs approach dual-use thresholds — while the treaty offers no credible timeline for the P5 disarmament that was the political price of their own restraint. Their security calculus is permanently disadvantaged relative to recognized weapons states and to states that never joined or withdrew.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, threshold_capable_nnws, payer,
    moderate, generational, constrained, national).

% Bear the direct costs of comprehensive safeguards inspections, reporting burdens, and technology-transfer restrictions on peaceful nuclear energy development (Article IV promises undermined in practice by supplier-state gatekeeping). Cannot access the same enrichment or reprocessing technology as P5-aligned states without extensive additional protocol commitments.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, nnws_civilian_nuclear_programs, payer,
    moderate, biographical, constrained, national).

% Fund the domestic implementation of IAEA safeguards infrastructure through national budgets, a cost with no equivalent line item in nuclear-weapon states, whose military nuclear complexes remain outside comparable civilian oversight.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, nnws_taxpayers_funding_safeguards, payer,
    powerless, generational, trapped, national).

% States that never joined or exercised Article X withdrawal face no treaty-based inspection burden and have, in practice, achieved weapons capability outside the regime. Their absence from the treaty's obligations is rarely treated by P5 diplomacy as evidence the regime's incentive structure is broken; instead the regime's failures are attributed to their non-membership rather than to the asymmetry that makes membership costly and non-membership viable.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, non_signatory_and_withdrawn_states, excluded,
    moderate, generational, mobile, national).

% Convene every five years to assess compliance. Repeatedly document the Article VI implementation gap in final documents (when consensus permits) but possess no enforcement mechanism against nuclear-weapon states comparable to the sanctions and referral pathways available against non-compliant NNWS.
narrative_ontology:constraint_stakeholder(npt_treaty_1970__oligopoly_enforcement_reading, npt_review_conference_delegates, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_1970__oligopoly_enforcement_reading, p5_nuclear_weapon_states).
narrative_ontology:fixing_cost_class(npt_treaty_1970__oligopoly_enforcement_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the proliferation of nuclear weapons technology to additional states, reducing the number of independent nuclear-armed actors and the associated risks of accidental or deliberate use — a genuine collective security good that most parties, including many NNWS, value independent of the regime's internal asymmetries.
% TRANSFER_FUNCTION: Moves inspection costs, technological restriction, and sovereignty constraints from non-nuclear-weapon states to a compliance apparatus administered by and disproportionately serving the interests of the recognized nuclear-weapon states and the institutions built around verifying everyone except them.
% ABSENT_VOICES: Threshold states that felt compelled to remain inside the treaty despite the missing reciprocal disarmament timeline rarely control the review conference agenda; states considering withdrawal are treated as regime threats rather than as parties raising a legitimate grievance about unmet Article VI commitments. Civil society and non-signatory states' technical experts are largely absent from NPT review conference deliberations, which remain state-centric.
% DISAPPEARANCE_RATIONALE: P5 states and the IAEA bureaucracy would argue the world rearranges catastrophically — proliferation would accelerate without the verification architecture. Threshold-state diplomats and disarmament advocates would argue the underlying security dynamics (deterrence competition, alliance structures, supplier cartels) would persist largely unchanged, since the treaty's binding force falls almost entirely on the willing while non-signatories already operate outside it — the enforcement asymmetry itself, not just the coordination benefit, would be what disappears.
% FOUNDING_PROBLEM: In the 1960s, multiple states were approaching nuclear weapons capability; the treaty was built to freeze the number of nuclear-weapon states at five and prevent a cascading proliferation spiral, while offering NNWS security assurances, peaceful nuclear technology access, and a negotiated path toward eventual disarmament by the weapons states as the price of their restraint.
% FOUNDING_PROBLEM_CORROBORATION: P5 governments attest the horizontal proliferation problem remains live and justifies continued asymmetric enforcement. Independent bodies outside the P5 — the Non-Aligned Movement bloc at review conferences, the Marshall Islands' ICJ submissions, and independent arms-control research institutes (e.g., SIPRI's arsenal-modernization tracking) — attest that the reciprocal disarmament half of the founding bargain is dead in practice even as the restraint half is enforced with increasing rigor, corroborating the asymmetry from outside the states that benefit from it.
narrative_ontology:disappearance_verdict(npt_treaty_1970__oligopoly_enforcement_reading, contested).
narrative_ontology:founding_problem_status(npt_treaty_1970__oligopoly_enforcement_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_1970__oligopoly_enforcement_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_1970__oligopoly_enforcement_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_1970__oligopoly_enforcement_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(npt_treaty_1970__oligopoly_enforcement_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(npt_treaty_1970__oligopoly_enforcement_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71) and suppression (0.78) are both high and rising over the interval because the safeguards architecture has hardened continuously (Additional Protocol adoption, expanded IAEA authority) while the Article VI side of the bargain saw no comparable hardening — arsenals were modernized, not eliminated, across the same period. Theater ratio (0.38) reflects that a substantial share of review-conference activity (final-document negotiations, consensus statements on disarmament 'concern') is diplomatic performance that does not translate into binding obligation on the P5. Accessibility collapse (0.62) is moderate-high: once inside the treaty, an NNWS's alternatives (indigenous weapons program, withdrawal) become progressively costlier and more stigmatized, but they are not eliminated (Article X remains, exercised by DPRK). Resistance (0.58) captures the sustained diplomatic pushback from the Non-Aligned Movement and threshold states at every review cycle.
 *
 * DIRECTIONALITY LOGIC:
 *   P5 states derive the lowest d: they collect the primary benefit (locked-in strategic status) and face effectively no constraint-specific cost under this reading. NNWS with real technical capacity or unmet security expectations derive the highest d: they absorb the inspection burden, the technology restriction, and the strategic disadvantage of forgone deterrent options, with constrained exit (withdrawal is diplomatically costly and treated as evidence of bad faith rather than a valid grievance response). Non-signatory/withdrawn states sit outside the directionality calculation for this constraint entirely — they are excluded from its obligations, which is precisely the asymmetry the reading highlights: the treaty's binding force falls hardest on those most committed to the norm.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling this as pure extraction (snare) because a genuine coordination function survives: most parties, even critical ones, do not want unconstrained proliferation and value the transparency the safeguards regime provides. It also prevents mislabeling it as clean coordination (rope) because the enforcement is asymmetric by design under this reading — the same structure that coordinates nonproliferation extracts disproportionate compliance cost from NNWS while shielding P5 arsenals from comparable scrutiny. Tangled Rope is the structurally honest read: coordination and extraction are fused in the same enforcement mechanism, and disentangling them would require either genuine reciprocal verification (moving toward the rope end) or overt abandonment of the disarmament pretense (moving toward snare).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_legal_status_ambiguity,
    'Is Article VI a binding legal obligation with an implicit timeline (as the ICJ''s 1996 advisory opinion suggested in dicta), or is it aspirational language whose vagueness was deliberate at drafting to secure P5 ratification?',
    'This is precisely the fork between this reading and the reciprocal_disarmament_reading sibling — resolvable only by treaty-interpretation authority (ICJ contentious jurisdiction, which no P5 state has accepted on this question) or by a review-conference consensus reinterpretation, neither of which has occurred.',
    'If Article VI is binding, this reading''s extraction reading is validated and the P5''s non-performance becomes an actionable treaty breach, converting this from tangled_rope toward snare. If genuinely aspirational, the coordination function dominates and this reading overstates extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(article_vi_legal_status_ambiguity, conceptual, 'Committer fork: whether Article VI carries binding force, which is the core premise the sibling reading denies of this one.').

omega_variable(
    oligopoly_vs_stability_intent,
    'Was the drafters'' original intent to lock in a five-state oligopoly of nuclear status, or to create a temporary five-state stewardship pending universal disarmament — i.e., is the oligopoly reading a betrayal of original intent or a realistic description of what the treaty always was?',
    'Historical archival analysis of the 1968 negotiating record (ENDC records, US/USSR bilateral negotiating history) weighed against the treaty''s actual 55-year enforcement pattern.',
    'If original intent supports genuine phased disarmament, the oligopoly reading describes regime capture/drift rather than founding design, which would shift some analytical weight toward mandatrophy (a scaffold whose sunset never materialized) rather than a stable tangled_rope equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oligopoly_vs_stability_intent, empirical, 'Whether the enforcement asymmetry is founding design or accumulated drift from a different founding intent.').

omega_variable(
    threshold_state_victim_status_contestable,
    'Are threshold-capable NNWS genuine victims of denied deterrent, or does treating them as victims implicitly legitimate weapons acquisition as a right the treaty wrongly denies?',
    'This is a normative/preference question about whether nuclear deterrence access is a good whose denial constitutes harm, not resolvable by empirical data alone.',
    'If deterrent denial is not treated as harm, the victim classification for threshold states weakens and the constraint reads closer to a legitimate collective-security rope with uneven but justified burden distribution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(threshold_state_victim_status_contestable, preference, 'Whether denial of nuclear deterrent capability constitutes a cognizable harm or a legitimate collective restraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_1970__oligopoly_enforcement_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1970, 0.2).
narrative_ontology:measurement(npt__tr_t1980, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1980, 0.24).
narrative_ontology:measurement(npt__tr_t1995, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 1995, 0.3).
narrative_ontology:measurement(npt__tr_t2005, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2005, 0.34).
narrative_ontology:measurement(npt__tr_t2015, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2015, 0.36).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_1970__oligopoly_enforcement_reading, theater_ratio, 2025, 0.38).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(npt__be_t1980, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1980, 0.52).
narrative_ontology:measurement(npt__be_t1995, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 1995, 0.58).
narrative_ontology:measurement(npt__be_t2005, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2005, 0.65).
narrative_ontology:measurement(npt__be_t2015, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2015, 0.69).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_1970__oligopoly_enforcement_reading, base_extractiveness, 2025, 0.71).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1970, 0.55).
narrative_ontology:measurement(npt__su_t1980, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1980, 0.62).
narrative_ontology:measurement(npt__su_t1995, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 1995, 0.7).
narrative_ontology:measurement(npt__su_t2005, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2005, 0.74).
narrative_ontology:measurement(npt__su_t2015, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2015, 0.77).
narrative_ontology:measurement(npt__su_t2025, npt_treaty_1970__oligopoly_enforcement_reading, suppression_requirement, 2025, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_1970__oligopoly_enforcement_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_1970__oligopoly_enforcement_reading, 0.1).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__reciprocal_disarmament_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, npt_treaty_1970__withdrawal_sovereignty_reading).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, iaea_safeguards_regime).
narrative_ontology:affects_constraint(npt_treaty_1970__oligopoly_enforcement_reading, nuclear_suppliers_group_export_controls).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the npt_treaty_1970 kernel (ε-invariance decomposition). The oligopoly_enforcement_reading (this file) treats Articles I-II as the primary binding obligation and Article VI as aspirational, producing a Tangled Rope classification with P5 states and the safeguards bureaucracy as beneficiaries and threshold/NNWS states as victims. The reciprocal_disarmament_reading treats Article VI as binding with temporal urgency, which would substantially raise ε for P5 seats and could shift their computed type toward snare or tangled_rope-with-reversed-victim-set. The withdrawal_sovereignty_reading centers Article X and produces a different stakeholder set entirely (states considering exit as agenda-setters rather than payers). Each reading has a distinct, stable ε — they are not the same constraint measured differently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
