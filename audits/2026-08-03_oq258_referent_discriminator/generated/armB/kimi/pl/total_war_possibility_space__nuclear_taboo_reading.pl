% ============================================================================
% CONSTRAINT STORY: total_war_possibility_space__nuclear_taboo_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_total_war_possibility_space__nuclear_taboo_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: total_war_possibility_space__nuclear_taboo_reading
 *   human_readable: Nuclear Taboo Against Total War (Normative Prohibition Reading)
 *   domain: international_relations/strategic_studies
 *
 * SUMMARY:
 *   This constraint instantiates the nuclear_taboo_reading of the
 *   total_war_possibility_space kernel. Under this reading, total war —
 *   specifically nuclear war — became normatively prohibited after 1945
 *   through a constructed taboo that operates independent of material
 *   military capability. The taboo is maintained by norm entrepreneurs,
 *   institutionalized in the NPT and no-first-use doctrines, and actively
 *   enforced through non-proliferation sanctions and diplomatic
 *   stigmatization. While the constraint genuinely coordinates great-power
 *   behavior away from nuclear use, it simultaneously extracts asymmetric
 *   strategic subordination from non-nuclear states, locking in a possession
 *   oligopoly. The reading predicts taboo weakening if norm entrepreneurs
 *   exit, and notes that non-nuclear powers face a structurally different
 *   constraint than nuclear-armed states.
 *
 * KEY AGENTS:
 *   - Nuclear weapon states (NPT P5): Custodians of the regime, legitimate possessors, and enforcers of the taboo.
 *   - Non-nuclear weapon states: NPT signatories that have foresworn weapons and accept strategic subordination.
 *   - Norm entrepreneurs: Scholars, diplomats, and advocates who construct and maintain the taboo discourse.
 *   - Non-proliferation bureaucracy: IAEA and review-conference administrators who verify and institutionalize the regime.
 *   - Nuclear outlier states: Possessors outside the NPT framework, structurally excluded from legitimacy.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, 0.62).
domain_priors:suppression_score(total_war_possibility_space__nuclear_taboo_reading, 0.71).
domain_priors:theater_ratio(total_war_possibility_space__nuclear_taboo_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(total_war_possibility_space__nuclear_taboo_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(total_war_possibility_space__nuclear_taboo_reading, tangled_rope).
narrative_ontology:human_readable(total_war_possibility_space__nuclear_taboo_reading, "Nuclear Taboo Against Total War (Normative Prohibition Reading)").
narrative_ontology:topic_domain(total_war_possibility_space__nuclear_taboo_reading, "international_relations/strategic_studies").

domain_priors:requires_active_enforcement(total_war_possibility_space__nuclear_taboo_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(total_war_possibility_space__nuclear_taboo_reading, '0c8069fb-e8a0-45e9-8193-3430d95fb6c9').
narrative_ontology:cs_kernel_codification('0c8069fb-e8a0-45e9-8193-3430d95fb6c9', formalized).
narrative_ontology:cs_authority_grounding('0c8069fb-e8a0-45e9-8193-3430d95fb6c9', lineage).
narrative_ontology:cs_interpretation_layer_present('0c8069fb-e8a0-45e9-8193-3430d95fb6c9').
narrative_ontology:cs_reading_relation('0c8069fb-e8a0-45e9-8193-3430d95fb6c9', total_war_possibility_space__deterrence_equilibrium_reading, coexists_with).
narrative_ontology:cs_reading_relation('0c8069fb-e8a0-45e9-8193-3430d95fb6c9', total_war_possibility_space__space_contraction_reading, coexists_with).
narrative_ontology:cs_axiom('0c8069fb-e8a0-45e9-8193-3430d95fb6c9', foundational, nuclear_use_categorically_prohibited).
narrative_ontology:cs_axiom_status(nuclear_use_categorically_prohibited, holdable).
narrative_ontology:cs_axiom_grounding('0c8069fb-e8a0-45e9-8193-3430d95fb6c9', nuclear_use_categorically_prohibited, deontological).
narrative_ontology:cs_axiom('0c8069fb-e8a0-45e9-8193-3430d95fb6c9', foundational, material_capability_non_determinative).
narrative_ontology:cs_axiom_status(material_capability_non_determinative, holdable).
narrative_ontology:cs_axiom_grounding('0c8069fb-e8a0-45e9-8193-3430d95fb6c9', material_capability_non_determinative, conventional).
narrative_ontology:cs_reference_frame('0c8069fb-e8a0-45e9-8193-3430d95fb6c9', post_war_nuclear_restraint_order).
narrative_ontology:cs_drift_state('0c8069fb-e8a0-45e9-8193-3430d95fb6c9', multipolar_renewal_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('0c8069fb-e8a0-45e9-8193-3430d95fb6c9', '').
narrative_ontology:cs_kernel_id(total_war_possibility_space__nuclear_taboo_reading, total_war_possibility_space).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs).
narrative_ontology:constraint_beneficiary(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_bureaucracy).
narrative_ontology:constraint_victim(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Possess nuclear weapons legitimized under the NPT. Act as custodians of the non-proliferation regime and enforcers of the nuclear use taboo. Benefit from strategic oligopoly and extended deterrence credibility. Cannot easily exit the taboo without destroying the legitimacy of their own arsenals.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_weapon_states, agenda_setter,
    institutional, generational, constrained, global).

% Foreswear nuclear weapons under the NPT in exchange for disarmament promises and technology access that remain partially unfulfilled. Accept permanent strategic inequality and extended deterrence dependencies. Their security is tied to the taboo holding, but they bear the structural cost of strategic subordination.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_nuclear_weapon_states, payer,
    organized, generational, constrained, national).

% Construct, propagate, and police the nuclear taboo through scholarship, diplomacy, and treaty advocacy. Derive professional status, funding, and institutional roles from the taboo's existence and apparent stability.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, norm_entrepreneurs, beneficiary,
    moderate, biographical, mobile, global).

% Administer safeguards, verify compliance, and convene review cycles that reinforce the possession-status quo. Institutional survival and budget depend on the regime's continuation.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, non_proliferation_bureaucracy, agenda_setter,
    institutional, generational, constrained, global).

% Possess nuclear weapons outside the NPT framework. They are structurally excluded from the legitimate nuclear club and targeted by sanctions and isolation, yet their arsenals constrain how the taboo is enforced.
narrative_ontology:constraint_stakeholder(total_war_possibility_space__nuclear_taboo_reading, nuclear_outlier_states, excluded,
    powerful, generational, constrained, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents the recurrence of total war between great powers by constructing a normative prohibition on nuclear weapons use, organizing collective expectations around non-use and delegitimizing first-strike options.
% TRANSFER_FUNCTION: Transfers strategic security and oligopoly rents to nuclear-armed states and professional status to norm entrepreneurs; extracts permanent strategic subordination and sovereignty constraints from non-nuclear states under the NPT.
% ABSENT_VOICES: Nuclear outlier states and total abolition advocates are structurally marginalized in the regime's core bargaining framework; they would contest the legitimacy of possession-oligopoly but are not seated in the taboo's enforcement architecture.
% DISAPPEARANCE_RATIONALE: If the nuclear taboo vanished overnight, nuclear use would re-enter the legitimate strategic option set, deterrence doctrines would rewrite, non-nuclear states would face acute security crises or rush to proliferate, and the NPT regime would lose its normative foundation — the international security order would fundamentally reorganize.
% FOUNDING_PROBLEM: Prevention of catastrophic nuclear exchange after Hiroshima and Nagasaki; creation of a stable great-power order that could avoid total war despite persistent geopolitical rivalry.
% FOUNDING_PROBLEM_CORROBORATION: Nuclear weapon states and norm entrepreneurs attest the problem remains live, citing proliferation risks. Non-nuclear states and critical scholars attest the founding problem has shifted: total war is now deterred by material factors and the regime persists as a possession-cartel with disarmament obligations unfulfilled; the corroboration is split across seats with no neutral institutional consensus.
narrative_ontology:disappearance_verdict(total_war_possibility_space__nuclear_taboo_reading, world_rearranges).
narrative_ontology:founding_problem_status(total_war_possibility_space__nuclear_taboo_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(total_war_possibility_space__nuclear_taboo_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-04',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(total_war_possibility_space__nuclear_taboo_reading, 'none', 1).
narrative_ontology:epsilon_provenance(total_war_possibility_space__nuclear_taboo_reading, 0.62, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(total_war_possibility_space__nuclear_taboo_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(total_war_possibility_space__nuclear_taboo_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is 0.62 because the NPT and non-proliferation complex extracts sovereignty and strategic options from non-nuclear states while delivering genuine security coordination against total war. Suppression is 0.71 because active enforcement through sanctions, IAEA inspections, and diplomatic isolation is required to maintain the regime against proliferators and revisionists. Theater ratio is 0.45 because disarmament rhetoric and review conferences perform progress toward Article VI while actual disarmament stalls. Accessibility collapse is 0.60 because proliferation alternatives are heavily stigmatized and sanctioned but physically possible. Resistance is 0.55 because outliers and abolition advocates actively contest the regime.
 *
 * PERSPECTIVAL GAP:
 *   From the nuclear weapon state seat, the arrangement is a necessary coordination that prevents total war and stabilizes extended deterrence. From the non-nuclear state seat, it is an asymmetric lock-in that freezes strategic inequality and defers disarmament indefinitely. The engine computes this divergence from the structural data: the same constraint produces opposite directionalities depending on whether the seat is a beneficiary of oligopoly or a victim of subordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Nuclear weapon states are beneficiaries with constrained exit — they cannot break the taboo without delegitimizing their own arsenals, so their directionality sits near the beneficiary pole. Non-nuclear states are victims with constrained exit — NPT withdrawal is legally possible but politically and economically costly, placing them near the target pole. Norm entrepreneurs are beneficiaries with mobile exit, giving them very low effective extraction. The non-proliferation bureaucracy is a beneficiary with constrained exit, giving it low directionality but high scope.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the constraint as pure extraction (snare) by recognizing the genuine coordination function — the prevention of nuclear total war. It prevents mislabeling as pure coordination (rope) by recognizing the asymmetric extraction embedded in the same structure: permanent nuclear oligopoly and the strategic subordination of non-nuclear states. If the coordination function atrophied while the enforcement machinery persisted, the constraint would degrade toward piton; if enforcement collapsed while the taboo remained rhetorically claimed, it would approach a degraded scaffold.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    taboo_materiality_separability,
    'Can the nuclear taboo be separated from material deterrence in explaining the non-use of nuclear weapons since 1945?',
    'Counterfactual and comparative case analysis; examine crises where use was considered but rejected despite strategic logic allowing it, and track doctrinal shifts as material balances change.',
    'If inseparable, the taboo reading overstates normative foreclosure and the constraint collapses toward the deterrence_equilibrium_reading; if separable, the normative structure is genuinely load-bearing and the tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(taboo_materiality_separability, empirical, 'Whether the taboo operates independently of material deterrence').

omega_variable(
    regime_asymmetry_fairness,
    'Does the NPT regime extract strategic subordination from non-nuclear states as the price of coordination, or is the sovereignty limitation a fair cost of collective security?',
    'Economic and strategic modeling of the NPT bargain; assessment of Article VI compliance by nuclear states and the distribution of safeguards burdens.',
    'If nuclear states extract oligopoly rents without delivering disarmament, the tangled rope classification is strengthened; if the bargain is broadly symmetric, the constraint shifts toward rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_asymmetry_fairness, conceptual, 'Whether NPT asymmetry constitutes extraction or fair coordination cost').

omega_variable(
    norm_entrepreneur_generational_transfer,
    'Does the nuclear taboo depend on a cohort of norm entrepreneurs whose retirement threatens the constraint''s persistence?',
    'Track replacement rates in diplomatic and scholarly positions devoted to nuclear norm entrepreneurship; measure taboo salience across generational cohorts.',
    'If the taboo is held by a narrow cohort, it is more fragile and more extractive toward those entrepreneurs; if broadly diffused, it is more robust and rope-like.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(norm_entrepreneur_generational_transfer, empirical, 'Generational dependency of the taboo''s maintenance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(total_war_possibility_space__nuclear_taboo_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tota_tr_t0, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tota_tr_t10, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(tota_tr_t20, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(tota_tr_t30, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 30, 0.35).
narrative_ontology:measurement(tota_tr_t40, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(tota_tr_t50, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 50, 0.45).
narrative_ontology:measurement(tota_tr_t60, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 60, 0.48).
narrative_ontology:measurement(tota_tr_t70, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 70, 0.45).
narrative_ontology:measurement(tota_tr_t80, total_war_possibility_space__nuclear_taboo_reading, theater_ratio, 80, 0.45).

% Extraction over time
narrative_ontology:measurement(tota_be_t0, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(tota_be_t10, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 10, 0.25).
narrative_ontology:measurement(tota_be_t20, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(tota_be_t30, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(tota_be_t40, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(tota_be_t50, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement(tota_be_t60, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 60, 0.6).
narrative_ontology:measurement(tota_be_t70, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 70, 0.61).
narrative_ontology:measurement(tota_be_t80, total_war_possibility_space__nuclear_taboo_reading, base_extractiveness, 80, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(tota_su_t0, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(tota_su_t10, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(tota_su_t20, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(tota_su_t30, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement(tota_su_t40, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(tota_su_t50, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 50, 0.75).
narrative_ontology:measurement(tota_su_t60, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 60, 0.73).
narrative_ontology:measurement(tota_su_t70, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 70, 0.7).
narrative_ontology:measurement(tota_su_t80, total_war_possibility_space__nuclear_taboo_reading, suppression_requirement, 80, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(total_war_possibility_space__nuclear_taboo_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
