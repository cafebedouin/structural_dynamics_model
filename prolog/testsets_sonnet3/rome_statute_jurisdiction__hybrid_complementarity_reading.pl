% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__hybrid_complementarity_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute Jurisdiction — Hybrid Complementarity Reading
 *   domain: International Law / Treaty Interpretation / Institutional Authority
 *
 * SUMMARY:
 *   This story instantiates the hybrid complementarity reading of the Rome
 *   Statute jurisdiction kernel: the ICC possesses a genuine, aspirationally
 *   universal jurisdictional claim over atrocity crimes, but that claim is
 *   operationally conditioned by the complementarity principle (Article 17),
 *   which requires the Court to defer to domestic proceedings unless the
 *   state is found 'unwilling or unable genuinely' to investigate or
 *   prosecute. Under this reading, the Statute is neither a pure
 *   sovereignty-consent instrument (the sovereigntist reading) nor a pure
 *   universal mandate transcending consent (the universalist reading) — it is
 *   a structural hybrid whose authority derives simultaneously from
 *   natural-law-adjacent claims about atrocity accountability and from
 *   classical treaty consent, with complementarity as the mechanism that
 *   reconciles the two without fully resolving the tension. This reading's ε
 *   is authored for the standing arrangement as this hybrid reading sees it:
 *   real but constrained jurisdiction, dependent enforcement, rising theater
 *   as the gap between jurisdictional claim and executable authority widens
 *   over the interval.
 *
 * KEY AGENTS:
 *   - icc_prosecutorial_office: agenda_setter (institutional/analytical) — administers admissibility determinations but depends on state cooperation for execution
 *   - state_parties_seeking_credible_deterrence: beneficiary (institutional/mobile) — retains sovereignty while gaining external legitimacy backstop
 *   - victims_in_uncooperative_state_situations: payer (powerless/trapped) — bears the enforcement gap when territorial states refuse cooperation
 *   - un_security_council: agenda_setter/excluded (institutional/arbitrage) — gates universal jurisdiction claims over non-parties through referral and Article 16 deferral
 *   - international_legal_scholars: observer (analytical/analytical) — assesses whether the hybrid balance functions as designed or has degraded into selective enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.42).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.38).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute Jurisdiction — Hybrid Complementarity Reading").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "International Law / Treaty Interpretation / Institutional Authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, '6d8b7ee6-a7e2-41f8-811f-ca7a65839566').
narrative_ontology:cs_kernel_codification('6d8b7ee6-a7e2-41f8-811f-ca7a65839566', fixed_text).
narrative_ontology:cs_authority_grounding('6d8b7ee6-a7e2-41f8-811f-ca7a65839566', lineage).
narrative_ontology:cs_interpretation_layer_present('6d8b7ee6-a7e2-41f8-811f-ca7a65839566').
narrative_ontology:cs_reading_relation('6d8b7ee6-a7e2-41f8-811f-ca7a65839566', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6d8b7ee6-a7e2-41f8-811f-ca7a65839566', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_axiom('6d8b7ee6-a7e2-41f8-811f-ca7a65839566', foundational, complementarity_as_load_bearing_synthesis).
narrative_ontology:cs_axiom_status(complementarity_as_load_bearing_synthesis, holdable).
narrative_ontology:cs_axiom_grounding('6d8b7ee6-a7e2-41f8-811f-ca7a65839566', complementarity_as_load_bearing_synthesis, conventional).
narrative_ontology:cs_axiom('6d8b7ee6-a7e2-41f8-811f-ca7a65839566', foundational, jurisdiction_conditioned_not_negated_by_consent).
narrative_ontology:cs_axiom_status(jurisdiction_conditioned_not_negated_by_consent, holdable).
narrative_ontology:cs_axiom_grounding('6d8b7ee6-a7e2-41f8-811f-ca7a65839566', jurisdiction_conditioned_not_negated_by_consent, instrumental).
narrative_ontology:cs_reference_frame('6d8b7ee6-a7e2-41f8-811f-ca7a65839566', rome_1998_negotiated_compromise).
narrative_ontology:cs_drift_state('6d8b7ee6-a7e2-41f8-811f-ca7a65839566', contemporary_post_african_union_bloc_tension, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6d8b7ee6-a7e2-41f8-811f-ca7a65839566', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, atrocity_victims_in_cooperating_states).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_prosecutorial_office).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties_seeking_credible_deterrence).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, non_state_party_nationals).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, victims_in_uncooperative_state_situations).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, domestic_civil_society_pursuing_accountability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Determines admissibility under the complementarity test: whether a state is 'unwilling or unable genuinely' to investigate or prosecute. Opens or declines situations, negotiates state cooperation, and depends entirely on states for arrest, evidence access, and enforcement — it has no police force of its own. Its authority is real but structurally borrowed.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, icc_prosecutorial_office, agenda_setter,
    institutional, generational, analytical, global).

% Ratifying states get a credible external deterrent against atrocity crimes and a forum that reduces the political cost of prosecuting their own former leaders. They retain the first right to prosecute domestically, so complementarity protects their sovereignty while lending international legitimacy to their justice systems.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, state_parties_seeking_credible_deterrence, beneficiary,
    institutional, generational, mobile, national).

% Where the territorial state is a cooperating party, victims gain access to investigation, potential prosecution, and reparations proceedings they could not obtain domestically. Their access depends entirely on state cooperation continuing through the life of the case.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, atrocity_victims_in_cooperating_states, beneficiary,
    powerless, biographical, trapped, national).

% Where the territorial state refuses cooperation, withholds evidence, or shields suspects, the ICC's jurisdiction exists on paper but cannot be executed. These victims bear the cost of the gap between the Statute's universal aspiration and its enforcement dependency — arrest warrants go unexecuted for years or decades.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, victims_in_uncooperative_state_situations, payer,
    powerless, biographical, trapped, national).

% Nationals of non-ratifying states can still fall under ICC jurisdiction if crimes occur on the territory of a state party, or via Security Council referral, but their own government owes the Court no cooperation duty. They are subject to a jurisdictional claim their state never consented to, while receiving none of the domestic-complementarity protections state parties negotiated for their own nationals.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, non_state_party_nationals, payer,
    powerless, biographical, trapped, national).

% Local human rights organizations document crimes and press for prosecution but have no standing to compel either domestic action or ICC intervention; they can only submit communications and hope the admissibility test finds the state 'unwilling or unable.' Their evidence work is frequently the unpaid input the system runs on.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, domestic_civil_society_pursuing_accountability, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, domestic_civil_society_pursuing_accountability, excluded).

% Can refer situations involving non-party states to the ICC or defer ongoing investigations for renewable one-year periods under Article 16. Permanent members with veto power can block referrals or force deferrals entirely outside the complementarity framework, meaning the universal jurisdiction claim is itself gated by great-power politics.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, un_security_council, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rome_statute_jurisdiction__hybrid_complementarity_reading, un_security_council, excluded).

% States that have not ratified (or have withdrawn signature) shield their own nationals from any cooperation duty while retaining Security Council leverage to refer or defer other states' situations. They benefit from the Statute's coercive machinery applying to others without it ever binding them.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, great_power_non_parties, excluded,
    institutional, civilizational, arbitrage, global).

% Study whether complementarity functions as a genuine sovereignty-preserving safeguard, a workable compromise enabling universal norms to operate through consent-based enforcement, or a structural weakness that lets powerful states escape accountability while weaker ones absorb the Court's caseload.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rome_statute_jurisdiction__hybrid_complementarity_reading, diffuse).
narrative_ontology:fixing_cost_class(rome_statute_jurisdiction__hybrid_complementarity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Complementarity solves a genuine problem: it lets states retain primary jurisdiction and prosecutorial dignity (avoiding the sovereignty objection that sank earlier international-court proposals) while providing a credible backstop when domestic systems collapse or refuse to act, making ratification politically achievable at all.
% TRANSFER_FUNCTION: Moves legal authority and legitimacy from domestic courts to the ICC only upon a finding of state unwillingness or inability, and moves the practical cost of enforcement (arrest, evidence-gathering, custody) back onto states — meaning the Court's jurisdictional reach is real on paper but the burden of realizing it is transferred to the very states whose failure triggered ICC involvement.
% ABSENT_VOICES: Victims in non-cooperating or non-party states have no seat: their access to justice depends on decisions (state cooperation, Security Council referral) made entirely by parties other than themselves. Great-power non-parties are also functionally absent from any obligation while retaining referral/veto leverage over everyone else's situations.
% DISAPPEARANCE_RATIONALE: If complementarity jurisdiction disappeared overnight, states would lose the negotiated compromise that made ratification possible in the first place — either a purely sovereigntist regime (no external check at all) or a purely universalist one (no consent basis, likely far fewer ratifications) would have to fill the gap, reshaping which atrocity situations get any external forum at all and renegotiating decades of admissibility jurisprudence.
% FOUNDING_PROBLEM: In the 1990s, ad hoc tribunals (ICTY, ICTR) demonstrated both the value and the political fragility of international criminal justice created case-by-case by Security Council fiat; states wanted a permanent court for atrocity crimes but would not accept one that could override functioning domestic justice systems or bypass sovereign consent entirely.
% FOUNDING_PROBLEM_CORROBORATION: ICC officials and state-party diplomats attest the founding compromise remains live and functioning as intended. Independent scholarship (e.g., analyses from the International Crisis Group and academic complementarity studies) and African Union member-state statements attest the arrangement has drifted toward selective enforcement against weaker and non-Western states while great powers remain functionally insulated — a corroboration from outside the Court's own institutional interest that the founding balance is not operating as designed.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__hybrid_complementarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__hybrid_complementarity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).
:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) is authored as moderate: complementarity genuinely constrains the ICC's reach in ways that protect functioning domestic systems, so this is not a pure extraction structure, but the burden-shifting onto powerless victim populations in non-cooperating situations is real and has grown as the Court's caseload has concentrated on weaker states. Suppression (0.38) is moderate — there is no coercive machinery forcing participation, but non-party nationals face jurisdictional claims without corresponding protections, and civil society voices are structurally unable to compel action. Theater ratio rises from 0.25 to 0.48 over the interval, reflecting the growing gap between the Statute's aspirational universal framing and its actual execution rate (persistently low conviction and arrest-execution numbers relative to situations opened) — the complementarity mechanism increasingly performs balance-language while the underlying enforcement dependency on states has not been resolved. Accessibility collapse (0.35) and resistance (0.55) reflect that alternatives to this hybrid framework (pure sovereigntist withdrawal, pure universalist reform) remain live and actively contested by different state coalitions, unlike a settled constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the ICC and cooperating state-party seats, this arrangement looks like a working compromise: jurisdiction with legitimacy, sovereignty with backstop. From the seat of victims in non-cooperating situations or non-party nationals, the same structure looks like a jurisdictional claim that exists on paper but cannot be executed against them or on their behalf — the universal aspiration is real as rhetoric and inert as remedy. The engine should compute these as structurally different experiences of one arrangement, not as disagreement about facts.
 *
 * DIRECTIONALITY LOGIC:
 *   State parties seeking deterrence and the ICC prosecutorial office sit near the beneficiary end: they gain legitimacy and jurisdictional reach respectively, with exit options (mobile ratification/withdrawal, or analytical institutional positioning) that cushion them from the arrangement's costs. Atrocity victims in cooperating states are conditional beneficiaries — real access, but entirely contingent on state cooperation continuing. Victims in uncooperative situations and non-state-party nationals sit near the full-target end: trapped exit options, no capacity to compel either their own state or the Court, and bearing the practical cost of the jurisdiction-enforcement gap that is this hybrid reading's central structural feature. The Security Council and great-power non-parties occupy a distinctive position — institutional power with arbitrage-grade exit, benefiting from the Statute's coercive claims applying to others while remaining structurally insulated from those same claims via veto and non-ratification.
 *
 * MANDATROPHY ANALYSIS:
 *   The hybrid complementarity reading resists both a triumphalist rope classification (which would ignore the real victim population bearing enforcement-gap costs) and a purely cynical snare classification (which would ignore the genuine coordination function complementarity performs in making ratification politically achievable and protecting functioning domestic systems from external override). The tangled_rope classification captures both: a real coordination function (sovereignty-preserving backstop enabling broad ratification) operating alongside asymmetric extraction (victims in non-cooperating states and non-party nationals bear costs that cooperating state-party populations do not), sustained by active enforcement machinery (admissibility litigation, referral politics, cooperation negotiations) rather than by voluntary universal acceptance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hybrid_stability_vs_drift,
    'Is the complementarity balance a stable equilibrium the Statute''s drafters achieved, or a temporary truce that is drifting toward either de facto sovereigntist capture (states routinely defeating admissibility through sham proceedings) or de facto universalist overreach (the Court expanding admissibility findings to reach politically disfavored states)?',
    'Longitudinal analysis of admissibility rulings: track the rate at which ''unwilling or unable'' findings correlate with state power/alliance status versus genuine domestic capacity, across the full case history from 2002 to present.',
    'If drift toward selective enforcement against weaker states is confirmed, the hybrid reading''s claimed coordination function is substantially undermined and the constraint moves closer to the tangled_rope/snare boundary; if the balance holds, the tangled_rope classification with moderate extraction remains the accurate structural read.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hybrid_stability_vs_drift, empirical, 'Whether the complementarity balance this reading describes is stable or drifting toward capture by either sovereign or universalist poles.').

omega_variable(
    kernel_reading_indeterminacy,
    'Is the hybrid reading the Statute''s actual drafted intent, or a diplomatic ambiguity deliberately left unresolved at Rome in 1998 to secure enough ratifications, meaning no single reading (hybrid, universalist, or sovereigntist) is more ''correct'' than the others — only more politically dominant at a given moment?',
    'Examination of the travaux préparatoires and delegate statements from the 1998 Rome Conference to determine whether complementarity was intended as genuine synthesis or as constructive ambiguity papering over an unresolved dispute.',
    'If constructive ambiguity, this reading and its two siblings are equally valid contemporaneous framings rather than competing interpretive claims about a determinate original meaning — none forecloses the others, and the kernel remains permanently contested rather than resolvable by textual analysis alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_indeterminacy, conceptual, 'Whether the hybrid framing reflects genuine drafted synthesis or diplomatically necessary ambiguity among the three sibling readings.').

omega_variable(
    great_power_insulation_structural_or_contingent,
    'Is the effective insulation of Security Council permanent members and major non-party states from ICC jurisdiction a structural feature this hybrid reading must account for as part of ε, or a contingent political fact that could change with different Council composition or future ratifications?',
    'Comparative analysis of referral/deferral patterns versus a counterfactual Council composition; track whether non-party insulation has narrowed or widened as the Court''s jurisprudence has matured.',
    'If structural, the extraction/suppression figures for non-party nationals and Security-Council-gated victims should be read as a stable feature of the hybrid arrangement rather than a transitional artifact, strengthening the tangled_rope classification''s asymmetric-extraction prong.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(great_power_insulation_structural_or_contingent, empirical, 'Whether great-power insulation from ICC jurisdiction is a durable structural feature of the hybrid reading or a contingent, revisable political arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 1998, 0.25).
narrative_ontology:measurement(rome_tr_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2002, 0.3).
narrative_ontology:measurement(rome_tr_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2008, 0.36).
narrative_ontology:measurement(rome_tr_t2013, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2013, 0.41).
narrative_ontology:measurement(rome_tr_t2018, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2018, 0.45).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2024, 0.48).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 1998, 0.28).
narrative_ontology:measurement(rome_be_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2002, 0.32).
narrative_ontology:measurement(rome_be_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2008, 0.36).
narrative_ontology:measurement(rome_be_t2013, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2013, 0.39).
narrative_ontology:measurement(rome_be_t2018, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2018, 0.4).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 1998, 0.22).
narrative_ontology:measurement(rome_su_t2002, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2002, 0.26).
narrative_ontology:measurement(rome_su_t2008, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2008, 0.3).
narrative_ontology:measurement(rome_su_t2013, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2013, 0.33).
narrative_ontology:measurement(rome_su_t2018, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2018, 0.36).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.12).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, sovereigntist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the rome_statute_jurisdiction kernel. universalist_reading authors a substantially lower ε (jurisdiction as genuinely transcending consent, coordination-dominant) and would classify closer to rope/scaffold from an aspirational vantage; sovereigntist_reading authors ε from the premise that jurisdiction is fully consent-conditioned and any perceived extraction is illegitimate overreach, likely classifying closer to a contested scaffold or rope depending on enforcement framing. This hybrid_complementarity_reading occupies the middle: it accepts both a real coordination function and real asymmetric extraction, hence tangled_rope. All three share the same underlying treaty text and institutional history but diverge in which premise (universal obligation vs. sovereign consent vs. negotiated hybrid) they treat as load-bearing — per DP-001 they are authored as separate constraints with separate ε, not as one constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rome_statute_jurisdiction__hybrid_complementarity_reading, institutional, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
