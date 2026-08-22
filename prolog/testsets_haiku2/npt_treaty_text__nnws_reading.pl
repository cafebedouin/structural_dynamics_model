% ============================================================================
% CONSTRAINT STORY: npt_treaty_text__nnws_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: npt_treaty_text__nnws_reading
 *   human_readable: NPT Article VI Disarmament Obligation (NNWS Reading)
 *   domain: international_law/arms_control
 *
 * SUMMARY:
 *   The NPT is a multilateral treaty concluded in 1968 establishing a
 *   non-proliferation regime with 191 states-parties. Article VI commits
 *   signatories to 'pursue negotiations in good faith on effective measures
 *   relating to the cessation of the nuclear arms race...and to nuclear
 *   disarmament.' The NNWS reading interprets this clause as a binding,
 *   indefinite obligation on NWS to pursue concrete disarmament pathways,
 *   enforceable through Review Conference consensus-building and
 *   delegitimized by the Treaty on the Prohibition of Nuclear Weapons (TPNW,
 *   2017). The NWS reading treats Article VI as aspirational and conditional
 *   on security circumstances. The treaty text admits both readings; this
 *   JSON instantiates only the NNWS reading.
 *
 * KEY AGENTS:
 *   - NNWS Coalition: Non-nuclear weapon states asserting the binding-obligation reading, using Review Conference pressure and TPNW as leverage
 *   - NWS Group: Nuclear weapon states (P5 + France) resisting binding timelines and interpreting Article VI as indefinite aspiration
 *   - TPNW Adherents: States party to the Treaty on the Prohibition of Nuclear Weapons, treating it as the enforcement mechanism for Article VI
 *   - NPT Review Conferences: Institutional arena where NNWS assert binding-obligation framing through consensus texts and voting pressure
 *   - Civil Society Advocates: Disarmament NGOs amplifying the NNWS reading through media, litigation, and political mobilization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(npt_treaty_text__nnws_reading, 0.48).
domain_priors:suppression_score(npt_treaty_text__nnws_reading, 0.32).
domain_priors:theater_ratio(npt_treaty_text__nnws_reading, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, theater_ratio, 0.52).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(npt_treaty_text__nnws_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(npt_treaty_text__nnws_reading, rope).
narrative_ontology:human_readable(npt_treaty_text__nnws_reading, "NPT Article VI Disarmament Obligation (NNWS Reading)").
narrative_ontology:topic_domain(npt_treaty_text__nnws_reading, "international_law/arms_control").

domain_priors:requires_active_enforcement(npt_treaty_text__nnws_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(npt_treaty_text__nnws_reading, 'f7cc6117-c8bb-4920-a11e-bb250c40892f').
narrative_ontology:cs_kernel_codification('f7cc6117-c8bb-4920-a11e-bb250c40892f', fixed_text).
narrative_ontology:cs_authority_grounding('f7cc6117-c8bb-4920-a11e-bb250c40892f', lineage).
narrative_ontology:cs_interpretation_layer_present('f7cc6117-c8bb-4920-a11e-bb250c40892f').
narrative_ontology:cs_reading_relation('f7cc6117-c8bb-4920-a11e-bb250c40892f', npt_treaty_text__nws_reading, coexists_with).
narrative_ontology:cs_reading_relation('f7cc6117-c8bb-4920-a11e-bb250c40892f', npt_treaty_text__withdrawal_threshold_reading, influences).
narrative_ontology:cs_axiom('f7cc6117-c8bb-4920-a11e-bb250c40892f', foundational, article_vi_binding_disarmament_obligation).
narrative_ontology:cs_axiom_status(article_vi_binding_disarmament_obligation, holdable).
narrative_ontology:cs_axiom_grounding('f7cc6117-c8bb-4920-a11e-bb250c40892f', article_vi_binding_disarmament_obligation, deontological).
narrative_ontology:cs_axiom('f7cc6117-c8bb-4920-a11e-bb250c40892f', foundational, non_proliferation_conditioned_on_nws_reciprocal_movement).
narrative_ontology:cs_axiom_status(non_proliferation_conditioned_on_nws_reciprocal_movement, holdable).
narrative_ontology:cs_axiom_grounding('f7cc6117-c8bb-4920-a11e-bb250c40892f', non_proliferation_conditioned_on_nws_reciprocal_movement, conventional).
narrative_ontology:cs_reference_frame('f7cc6117-c8bb-4920-a11e-bb250c40892f', npt_bargain_reciprocal_commitment).
narrative_ontology:cs_drift_state('f7cc6117-c8bb-4920-a11e-bb250c40892f', contemporary_2025, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f7cc6117-c8bb-4920-a11e-bb250c40892f', '').
narrative_ontology:cs_kernel_id(npt_treaty_text__nnws_reading, npt_treaty_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, nnws_coalition).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, tpnw_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(npt_treaty_text__nnws_reading, civil_society_ngos).
narrative_ontology:constraint_victim(npt_treaty_text__nnws_reading, nws_group).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Non-nuclear weapon states that read Article VI as a binding disarmament obligation on NWS. They use Review Conference pressure, coalition voting, and the Treaty on the Prohibition of Nuclear Weapons (TPNW) as leverage to force NWS to commit to concrete timelines and verification. They benefit from framing disarmament as a quid pro quo for non-proliferation compliance.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nnws_coalition, agenda_setter,
    organized, generational, constrained, global).

% Nuclear weapon states that resist binding disarmament timelines. They interpret Article VI as aspirational and indefinite, claim ongoing security threats justify retention, and leverage veto power and exit options (withdrawal, NPT reservations) to avoid concrete obligations. The NNWS reading frames them as shirking a binding obligation.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, nws_group, payer,
    powerful, civilizational, mobile, global).

% States party to the Treaty on the Prohibition of Nuclear Weapons, treating it as the enforcement mechanism for Article VI's disarmament obligation. They benefit from the NNWS reading because it legitimates their treaty as the implementation path NWS must eventually join. Their existence creates regime competition that pressurizes NWS toward negotiated disarmament.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, tpnw_adherents, beneficiary,
    organized, generational, mobile, global).

% Formal review mechanisms where the NNWS coalition asserts and documents the binding nature of Article VI, produces consensus texts claiming disarmament urgency, and creates public accountability for NWS. They wield soft power through consensus-building and legitimacy rather than enforcement machinery.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, npt_review_conferences, agenda_setter,
    institutional, biographical, constrained, global).

% The International Atomic Energy Agency and treaty verification bodies that would enforce a binding disarmament obligation if it existed. Under the NNWS reading, they represent the institutional infrastructure that would validate compliance; their absence or weakness is what prevents Article VI from functioning as a hard constraint.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, verification_regimes, observer,
    institutional, biographical, analytical, global).

% Disarmament advocacy organizations and civil society that amplify the NNWS reading through media, litigation support, and political mobilization. They benefit from the binding-obligation framing and use it to delegitimize NWS retention.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, civil_society_ngos, beneficiary,
    moderate, biographical, mobile, global).

% Nuclear-armed states outside the NPT (India, Pakistan, Israel, North Korea) whose voice is formally absent from NPT processes. They would contest the NNWS reading as sanctimonious if admitted, but their exclusion from the regime shapes how disarmament is framed as a problem for NPT insiders, not a global commitment.
narrative_ontology:constraint_stakeholder(npt_treaty_text__nnws_reading, excluded_nws_states, excluded,
    powerful, civilizational, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(npt_treaty_text__nnws_reading, diffuse).
narrative_ontology:fixing_cost_class(npt_treaty_text__nnws_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The NNWS reading coordinates non-proliferation compliance by NNWS states with a binding reciprocal obligation on NWS to pursue disarmament. NNWS agree not to acquire nuclear weapons in exchange for NWS committing to eventual elimination and interim arms reductions. This is genuine coordination: both sides benefit from the non-proliferation outcome, and both sides incur costs (NNWS forgo a security option; NWS forgo unfettered deterrent growth).
% TRANSFER_FUNCTION: The NNWS reading imposes an obligation transfer: NWS must move from indefinite possession toward timetabled elimination and transparency. NNWS transfer their agreement to verifiable non-acquisition. The constraint moves legitimacy and compliance burden from symmetrical indefiniteness toward asymmetrical accountability: NWS become accountable for progress, NNWS become accountable for restraint.
% ABSENT_VOICES: Nuclear-armed states outside the NPT (India, Pakistan, Israel, North Korea) are formally excluded from NPT Review Conferences and have no institutional seat to contest the NNWS reading. Their absence means the reading treats disarmament as an NPT-internal problem between signatories, not a global security architecture question. Secondary absent voices: NWS military establishments and nuclear industry actors who would dispute that disarmament is operationally feasible, but their voice is mediated through diplomatic channels rather than excluded formally.
% DISAPPEARANCE_RATIONALE: If the NNWS reading—and the binding disarmament obligation it asserts—were abandoned (i.e., NWS successfully redefined Article VI as purely aspirational and NNWS conceded), the NPT would revert to a non-proliferation-only regime without reciprocal disarmament pressure. NNWS would face a credibility crisis in justifying non-acquisition in the absence of binding NWS movement; TPNW adherents would experience a legitimacy reversal (their treaty would be framed as an NWS-hostile outlier rather than the enforcement mechanism for a binding obligation); and the regime's cohesion would depend entirely on the security benefits of non-proliferation to NNWS, which are weaker than the coupled reciprocal obligation framing.
% FOUNDING_PROBLEM: The proliferation of nuclear weapons to more states and the moral/political contradiction of indefinite possession by a privileged few while imposing permanent restraint on others. The NPT was negotiated as a bargain: NNWS would foreclose the acquisition path in exchange for NWS committing to disarmament. The NNWS reading asserts that Article VI codifies this bargain as a binding, indefinite obligation on NWS, not a one-time aspiration.
% FOUNDING_PROBLEM_CORROBORATION: NNWS states, civil society disarmament advocates, and the TPNW negotiating coalition all attest that the founding problem (proliferation pressure + moral inconsistency of indefinite possession) remains live and that the binding-obligation reading is the solution the NPT text itself commits to. NWS and some international legal scholars counter that the founding problem has been 'solved' by the non-proliferation outcome (few states have acquired weapons despite the option) and that Article VI was always aspirational. Scholars from the Council on Foreign Relations and the Stockholm International Peace Research Institute (outside the benefiting parties) have documented the persistent credibility gap: NNWS public opinion polls show declining confidence in the NPT bargain, and NNWS diplomats cite Article VI noncompliance as a reason for TPNW withdrawal threats.
narrative_ontology:disappearance_verdict(npt_treaty_text__nnws_reading, world_rearranges).
narrative_ontology:founding_problem_status(npt_treaty_text__nnws_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(npt_treaty_text__nnws_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(npt_treaty_text__nnws_reading, 'none', 1).
narrative_ontology:epsilon_provenance(npt_treaty_text__nnws_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   The NNWS reading exhibits moderate extractiveness (0.48) rather than high because the constraint's enforcement depends on soft power (legitimacy, regime competition, TPNW pressure) rather than hard sanctions or verification machinery. NWS have exit options (treaty withdrawal, reservations, military continuation) that dampen the effective constraint. Theater ratio rises from 0.25 to 0.52 over the interval: early Review Conferences produced genuine negotiating texts and confidence-building measures; by 2010–2025, Review Conference texts increasingly assert disarmament urgency that produces no observable NWS movement (rising theater). Suppression requirement remains low (0.32 peak) because the NNWS reading operates through normative pressure and legitimacy challenges rather than physical coercion or legal enforcement. The measurement series track how the binding-obligation framing weakens as NWS non-compliance persists without consequence.
 *
 * PERSPECTIVAL GAP:
 *   From the NNWS seat, Article VI is a binding, enforceable obligation auditable through Review Conference pressure and TPNW delegation. From the NWS seat, Article VI is a good-faith aspirational commitment contingent on global security conditions, with no fixed timeline or penalty structure. The engine computes each seat's per-seat classification from the structural data: NNWS seats experience coordination benefit (d ~0.3–0.4, low extraction from their position); NWS seats experience pressure without hard enforcement (d ~0.6–0.7, moderate extraction that lacks the coercive teeth to make it a full snare). The gap exists because the same treaty text instantiates structurally different constraints depending on whether you read Article VI as binding or aspirational.
 *
 * DIRECTIONALITY LOGIC:
 *   The NNWS coalition is the structural beneficiary of this reading: they assert a binding obligation that appears to constrain NWS behavior and provides NNWS with negotiating leverage and moral standing. The direction of benefit is asymmetrical: NNWS gain leverage over NWS behavior, not rents or direct extraction. NWS are the targets of the constraint in the NNWS reading's frame—they are called upon to move toward disarmament against their security preferences. However, NWS retain substantial exit options (withdrawal, non-compliance, military continuation) that keep their effective directionality moderate rather than extreme. The NNWS reading itself has lower teeth than a rope with hard enforcement, so directionality is muted throughout: this is coordination with an enforcement gap, not a snare. TPNW adherents benefit from the reading's legitimacy (their treaty becomes the 'real' disarmament path) but remain outside the NPT coordination itself.
 *
 * MANDATROPHY ANALYSIS:
 *   The NNWS reading's founding problem (proliferation pressure + moral inconsistency) remains live but its mandate has decayed: NWS have not moved meaningfully toward disarmament despite 55 years of Review Conference assertions. The rising theater ratio (0.25→0.52) and stalled base extractiveness (~0.48 plateau) indicate that Review Conference texts are increasingly performative—they assert the obligation without producing compliance. The constraint is not yet a piton (it retains coordination value for NNWS and legitimacy for TPNW) but is drifting toward one. The mandatrophy is not resolved: the NNWS coalition continues to assert Article VI as binding, NWS continue to resist binding timelines, and the contradiction persists unresolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    article_vi_binding_vs_aspirational,
    'Is Article VI a binding, enforceable disarmament obligation or an aspirational, indefinite commitment?',
    'International Court of Justice advisory opinion on the treaty''s legal character; textual analysis of treaty negotiating history and subsequent state practice; comparison with binding disarmament provisions in other treaties.',
    'If binding: the NNWS reading holds and NWS are subject to an auditable obligation; the extraction value remains moderate but coherent. If aspirational: the NNWS reading collapses to a framing error and the constraint reverts to aspiration (mountain or rope of weaker binding). The entire classification hinges on this.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(article_vi_binding_vs_aspirational, conceptual, 'Core textual ambiguity in Article VI''s legal character under international law.').

omega_variable(
    tpnw_as_enforcement_vs_alternative_regime,
    'Does the Treaty on the Prohibition of Nuclear Weapons function as the enforcement mechanism for Article VI, or as an alternative regime competing with NPT?',
    'Empirical observation of TPNW adherent behavior and NWS responses; analysis of whether TPNW pressure actually constrains NWS negotiating positions or is purely symbolic; institutional design comparison between TPNW compliance and NPT Review Conference pressure.',
    'If TPNW functions as enforcement (creates institutional pressure NWS must account for): the NNWS reading gains enforcement capacity and the constraint''s extractiveness rises (effective suppression + compliance pressure). If TPNW remains symbolic: the NNWS reading loses institutional bite and the constraint drifts toward pure theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tpnw_as_enforcement_vs_alternative_regime, empirical, 'Whether TPNW creates functional enforcement pressure on NWS or remains a parallel signaling mechanism.').

omega_variable(
    nws_exit_feasibility_under_article_x,
    'Can NWS credibly withdraw from the NPT without triggering sufficient political and economic retaliation to make withdrawal infeasible, or does effective withdrawal require overcoming collective action by NNWS?',
    'Historical analysis of withdrawal threats and responses (e.g., North Korea withdrawal 2003, Iran enrichment disputes); game-theoretic modeling of coalition punishment; empirical observation of state behavior if any NWS seriously signals withdrawal.',
    'If NWS exit is truly mobile (cost < benefit of disarmament): directionality for NWS shifts lower (they are less fully targeted); the constraint becomes more rope-like and less snare-like. If exit is constrained by coalition retaliation: directionality for NWS increases; the constraint gains suppression mechanics and edges toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nws_exit_feasibility_under_article_x, empirical, 'Whether NWS have functional exit options or whether collective NNWS action traps them in the regime.').

omega_variable(
    review_conference_legitimacy_vs_coercion,
    'Do Review Conference consensus texts operate as normative pressure and legitimacy challenges (soft power), or do they constitute a form of collective coercion on NWS?',
    'Interviews with NWS and NNWS diplomats about Review Conference outcomes; analysis of whether NWS make policy concessions in response to consensus pressure or treat them as rhetorical and non-binding; comparison of disarmament behavior before/after high-consensus Review Conference texts.',
    'If legitimacy-based (soft power): suppression remains low (~0.32) and the constraint stays rope-class. If collective coercion: suppression rises and the constraint edges toward tangled_rope with enforcement mechanisms. The theater ratio is already elevated (0.52), suggesting the pressure is increasingly decoupled from compliance, but the locus (legitimacy vs coercion) matters for classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(review_conference_legitimacy_vs_coercion, empirical, 'The character of Review Conference pressure on NWS: normative or coercive.').

omega_variable(
    kernel_reading_foreclosure_structure,
    'Can the NNWS and NWS readings coexist within the same treaty framework, or does one reading''s core premise logically foreclose the other?',
    'Logical analysis of whether ''Article VI is binding'' and ''Article VI is aspirational'' are both defensible from the same text and treaty history, or whether treaty law requires one to prevail. Examination of whether NWS acceptance of the binding reading would be logically inconsistent with their security interests (preference-driven conflict) vs their treaty interpretation (structural contradiction).',
    'If the readings coexist (different parties hold them simultaneously): the relation is `coexists_with`. If one reading''s adoption would logically preclude the other within the same framework: the relation is `forecloses`. This determines whether the kernel is an unresolved structural ambiguity or a true contradiction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_structure, conceptual, 'The logical relationship between the NNWS and NWS readings of Article VI.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(npt_treaty_text__nnws_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(npt__tr_t1970, npt_treaty_text__nnws_reading, theater_ratio, 1970, 0.25).
narrative_ontology:measurement(npt__tr_t1985, npt_treaty_text__nnws_reading, theater_ratio, 1985, 0.38).
narrative_ontology:measurement(npt__tr_t2000, npt_treaty_text__nnws_reading, theater_ratio, 2000, 0.45).
narrative_ontology:measurement(npt__tr_t2010, npt_treaty_text__nnws_reading, theater_ratio, 2010, 0.5).
narrative_ontology:measurement(npt__tr_t2020, npt_treaty_text__nnws_reading, theater_ratio, 2020, 0.52).
narrative_ontology:measurement(npt__tr_t2025, npt_treaty_text__nnws_reading, theater_ratio, 2025, 0.52).

% Extraction over time
narrative_ontology:measurement(npt__be_t1970, npt_treaty_text__nnws_reading, base_extractiveness, 1970, 0.22).
narrative_ontology:measurement(npt__be_t1985, npt_treaty_text__nnws_reading, base_extractiveness, 1985, 0.35).
narrative_ontology:measurement(npt__be_t2000, npt_treaty_text__nnws_reading, base_extractiveness, 2000, 0.41).
narrative_ontology:measurement(npt__be_t2010, npt_treaty_text__nnws_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(npt__be_t2020, npt_treaty_text__nnws_reading, base_extractiveness, 2020, 0.47).
narrative_ontology:measurement(npt__be_t2025, npt_treaty_text__nnws_reading, base_extractiveness, 2025, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(npt__su_t1970, npt_treaty_text__nnws_reading, suppression_requirement, 1970, 0.15).
narrative_ontology:measurement(npt__su_t1985, npt_treaty_text__nnws_reading, suppression_requirement, 1985, 0.2).
narrative_ontology:measurement(npt__su_t2000, npt_treaty_text__nnws_reading, suppression_requirement, 2000, 0.26).
narrative_ontology:measurement(npt__su_t2010, npt_treaty_text__nnws_reading, suppression_requirement, 2010, 0.31).
narrative_ontology:measurement(npt__su_t2020, npt_treaty_text__nnws_reading, suppression_requirement, 2020, 0.32).
narrative_ontology:measurement(npt__su_t2025, npt_treaty_text__nnws_reading, suppression_requirement, 2025, 0.32).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(npt_treaty_text__nnws_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(npt_treaty_text__nnws_reading, 0.12).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__nws_reading).
narrative_ontology:affects_constraint(npt_treaty_text__nnws_reading, npt_treaty_text__withdrawal_threshold_reading).

% DUAL FORMULATION NOTE:
% The NPT Article VI kernel admits multiple readings with different constraint structures. The NNWS reading (this story) asserts Article VI as a binding disarmament obligation on NWS, enforceable through Review Conference pressure and TPNW regime competition. The NWS reading (sibling constraint) treats Article VI as aspirational and indefinite. The withdrawal_threshold_reading (sibling constraint) focuses on Article X withdrawal mechanics and whether regime stability or state sovereignty should take priority. All three are valid readings of the same fixed treaty text (kernel_codification='fixed_text'); they differ in how they interpret the text's normative force and compliance mechanics. The stories are linked via network edges because they share the same kernel and their adoption/rejection affects each other's credibility and institutional standing. Each reading has its own epsilon, stakeholder structure, and type classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
