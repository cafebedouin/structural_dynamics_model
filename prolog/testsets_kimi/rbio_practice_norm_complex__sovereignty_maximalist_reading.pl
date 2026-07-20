% ============================================================================
% CONSTRAINT STORY: rbio_practice_norm_complex__sovereignty_maximalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rbio_practice_norm_complex__sovereignty_maximalist_reading, []).

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
 *   constraint_id: rbio_practice_norm_complex__sovereignty_maximalist_reading
 *   human_readable: RBIO Practice Norm Complex â Sovereignty Maximalist Reading
 *   domain: international_relations/law/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the sovereignty-maximalist reading of
 *   the RBIO practice norm complex. In this reading, state sovereignty is
 *   absolute; rules-based international order norms are legitimate only
 *   insofar as they protect sovereignty against external interference; and
 *   humanitarian exceptions are pretexts for regime change. The constraint
 *   benefits authoritarian regimes by closing off external accountability
 *   pathways, while populations under repressive governments bear the cost of
 *   lost external recourse. It is authored as a kernel reading (Rule 1): only
 *   this reading is classified here, with its sibling readings
 *   (liberal_institutional, hegemonic_extraction) treated as separate
 *   constraints linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - authoritarian_regimes: Primary beneficiary (institutional/arbitrage) â collects immunity from external intervention
 *   - trapped_populations: Primary target (powerless/trapped) â bear extraction through closed recourse
 *   - p5_sovereigntist_bloc: Agenda setter (institutional/arbitrage) â administers enforcement via veto and procedural blocking
 *   - human_rights_advocacy_networks: Excluded voice (organized/constrained) â structurally excluded from operative authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.72).
domain_priors:suppression_score(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.78).
domain_priors:theater_ratio(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(rbio_practice_norm_complex__sovereignty_maximalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rbio_practice_norm_complex__sovereignty_maximalist_reading, tangled_rope).
narrative_ontology:human_readable(rbio_practice_norm_complex__sovereignty_maximalist_reading, "RBIO Practice Norm Complex â Sovereignty Maximalist Reading").
narrative_ontology:topic_domain(rbio_practice_norm_complex__sovereignty_maximalist_reading, "international_relations/law/political_economy").

domain_priors:requires_active_enforcement(rbio_practice_norm_complex__sovereignty_maximalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'bccd8c8f-ea27-4153-ba2f-d99bb4bf7dcd').
narrative_ontology:cs_kernel_codification('bccd8c8f-ea27-4153-ba2f-d99bb4bf7dcd', formalized).
narrative_ontology:cs_authority_grounding('bccd8c8f-ea27-4153-ba2f-d99bb4bf7dcd', lineage).
narrative_ontology:cs_interpretation_layer_present('bccd8c8f-ea27-4153-ba2f-d99bb4bf7dcd').
narrative_ontology:cs_reading_relation('bccd8c8f-ea27-4153-ba2f-d99bb4bf7dcd', rbio_practice_norm_complex__liberal_institutional_reading, coexists_with).
narrative_ontology:cs_reading_relation('bccd8c8f-ea27-4153-ba2f-d99bb4bf7dcd', rbio_practice_norm_complex__hegemonic_extraction_reading, coexists_with).
narrative_ontology:cs_axiom('bccd8c8f-ea27-4153-ba2f-d99bb4bf7dcd', foundational, absolute_non_intervention).
narrative_ontology:cs_axiom_status(absolute_non_intervention, holdable).
narrative_ontology:cs_axiom_grounding('bccd8c8f-ea27-4153-ba2f-d99bb4bf7dcd', absolute_non_intervention, conventional).
narrative_ontology:cs_axiom('bccd8c8f-ea27-4153-ba2f-d99bb4bf7dcd', foundational, humanitarian_pretext_thesis).
narrative_ontology:cs_axiom_status(humanitarian_pretext_thesis, holdable).
narrative_ontology:cs_axiom_grounding('bccd8c8f-ea27-4153-ba2f-d99bb4bf7dcd', humanitarian_pretext_thesis, empirically_contingent).
narrative_ontology:cs_reference_frame('bccd8c8f-ea27-4153-ba2f-d99bb4bf7dcd', westphalian_sovereign_equality).
narrative_ontology:cs_drift_state('bccd8c8f-ea27-4153-ba2f-d99bb4bf7dcd', contemporary_human_rights_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('bccd8c8f-ea27-4153-ba2f-d99bb4bf7dcd', '').
narrative_ontology:cs_kernel_id(rbio_practice_norm_complex__sovereignty_maximalist_reading, rbio_practice_norm_complex).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:constraint_victim(rbio_practice_norm_complex__sovereignty_maximalist_reading, trapped_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Invoke absolute sovereignty to reject humanitarian intervention, human rights conditionality, and external election monitoring. They participate selectively in multilateral institutions while using the non-interference principle to shield domestic governance from external accountability, benefiting from the closure of legal pathways that could empower domestic opposition or authorize cross-border protective action.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes, beneficiary,
    institutional, generational, arbitrage, national).

% Live under repressive governments and bear the cost of closed external recourse. When atrocities occur, the sovereignty-maximalist norm blocks UN-authorized intervention and delegitimizes unilateral protective action, leaving them dependent on the repressive state itself with no legitimate external recourse.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, trapped_populations, payer,
    powerless, biographical, trapped, local).

% Permanent Security Council members and aligned great powers who administer the constraint through veto threats and procedural blocking. They enforce an absolutist reading of Article 2(7) and Chapter VII limits, preserving a spheres-of-influence architecture and preventing institutional legalization of humanitarian exceptions.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, p5_sovereigntist_bloc, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Advocate for Responsibility to Protect and human rights conditionality. Structurally excluded from the sovereigntist framework, which treats their claims as pretexts for regime change rather than autonomous legal arguments. Their preferred norms are present in discourse but excluded from operative authority.
narrative_ontology:constraint_stakeholder(rbio_practice_norm_complex__sovereignty_maximalist_reading, human_rights_advocacy_networks, excluded,
    organized, generational, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rbio_practice_norm_complex__sovereignty_maximalist_reading, authoritarian_regimes).
narrative_ontology:fixing_cost_class(rbio_practice_norm_complex__sovereignty_maximalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents unilateral external interference in the domestic affairs of states, preserving a pluralist international order in which diverse political systems coexist without imposed governance models.
% TRANSFER_FUNCTION: Transfers authority and immunity over domestic populations from potential external interveners and international institutions to the sitting territorial government, regardless of that government's internal character or the consent of the governed.
% ABSENT_VOICES: Populations facing atrocity crimes and human rights advocates promoting Responsibility to Protect. They are present in discourse but their preferred norms are structurally excluded by the absolutist sovereignty frame, which treats humanitarian claims as pretextual.
% DISAPPEARANCE_RATIONALE: If the absolutist sovereignty reading vanished, the normative barrier to humanitarian intervention, human rights conditionality, and external election monitoring would collapse. Authoritarian regimes would lose their primary legal shield, and the international order would reorganize around conditional sovereignty and R2P-style protective obligations.
% FOUNDING_PROBLEM: Preventing great-power war, imperial domination, and coercive regime change by establishing mutual non-interference as the foundation of peaceful interstate coexistence.
% FOUNDING_PROBLEM_CORROBORATION: Realist and sovereigntist scholars attest that interstate war prevention remains a live problem. Liberal institutionalists and human rights advocates attest from outside the beneficiary set that the founding problem has mutated: non-interference now frequently perpetuates internal atrocities rather than preventing external war, and the arrangement persists to shield repressive incumbents.
narrative_ontology:disappearance_verdict(rbio_practice_norm_complex__sovereignty_maximalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(rbio_practice_norm_complex__sovereignty_maximalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rbio_practice_norm_complex__sovereignty_maximalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0.72, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rbio_practice_norm_complex__sovereignty_maximalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rbio_practice_norm_complex__sovereignty_maximalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.72) because the norm transfers authority from vulnerable populations to incumbent regimes regardless of internal conduct, and closes off protective alternatives. Suppression is higher (0.78) because the constraint's persistence depends on actively excluding humanitarian intervention norms, R2P, and conditionality. Theater ratio (0.42) reflects that sovereign equality is presented as a universal principle while functionally shielding asymmetric internal power. Accessibility collapse (0.70) indicates that once the absolutist frame is accepted, alternatives (humanitarian intervention as legitimate) collapse conceptually. Resistance (0.60) captures ongoing advocacy from human rights networks and some Western states. The measurement series track the post-Cold War weaponization of sovereignty norms: extraction and theater rise as the norm shifts from anti-colonial self-determination to authoritarian self-protection.
 *
 * PERSPECTIVAL GAP:
 *   The authoritarian regime seat and the trapped population seat should compute radically different types. From the regime perspective, the constraint is protective coordination (rope/mountain-like immunity); from the population perspective it is enforced extraction (snare-like closure of recourse). The engine computes this divergence from beneficiary/victim declarations and exit options â the regime has arbitrage-grade exit between forums, while populations are trapped.
 *
 * DIRECTIONALITY LOGIC:
 *   Authoritarian regimes are declared beneficiaries with institutional power and arbitrage exit (low d, damped Ï). Trapped populations are declared victims with powerless status and trapped exit (high d, amplified Ï). The P5 sovereigntist bloc sits near the agenda-setter/beneficiary boundary but is not declared a beneficiary in base_properties, reflecting that in this reading the extraction accrues primarily to regimes rather than to the enforcers; their directionality is structurally low as administrators. Human rights networks are excluded â they experience the constraint as suppressed alternative rather than targeted extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as tangled_rope prevents mislabeling the constraint as pure coordination (rope) because of the declared victim population and asymmetric extraction. It prevents mislabeling as pure extraction (snare) because the coordination function â preventing great-power war and imperial intervention â is genuine and historically grounded. The mandatrophy question is whether the founding problem (interstate war prevention) is still live; the contested status and rising theater ratio suggest the arrangement may be drifting toward piton or snare as the coordination function atrophies relative to the extraction function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'Does the sovereignty-maximalist reading logically foreclose the liberal-institutional reading within a single UN Charter framework, or do they coexist as live interpretations?',
    'Track whether the same legal instrument is interpreted as simultaneously authorizing and prohibiting humanitarian intervention by different state coalitions.',
    'If foreclosed, the kernel is structurally bipolar; if coexistent, this constraint is one factional reading among many in a distributed authority system.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Structural relationship between sovereignty-maximalist and liberal readings.').

omega_variable(
    sovereignty_naturalness_ambiguity,
    'Is absolute state sovereignty a natural structural feature of international politics, or a constructed normative framework actively maintained by specific power configurations?',
    'Historical sociology of sovereignty; examination of whether the norm persists independently of great-power enforcement and institutional path dependency.',
    'If purely constructed and maintained by enforcement, the constraint gravitates toward snare or tangled_rope; if it reflects an emergent feature of anarchic systems, it approaches mountain status for states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_naturalness_ambiguity, conceptual, 'Natural law vs constructed norm ambiguity for absolute sovereignty.').

omega_variable(
    humanitarian_pretext_validity,
    'Are humanitarian intervention claims systematically pretexts for regime change, or do they represent autonomous moral-legal claims operationally distinct from extraction?',
    'Comparative case analysis of interventions framed as humanitarian, measuring protective outcomes versus regime-change outcomes, against baseline of non-intervention atrocity trajectories.',
    'If pretextual, the trapped_populations extraction is compounded by false legitimacy; if autonomous, the sovereignty-maximalist reading systematically mischaracterizes the alternative and overstates extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(humanitarian_pretext_validity, empirical, 'Whether humanitarian framing is genuinely protective or pretextual.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rbio_practice_norm_complex__sovereignty_maximalist_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rbio_sovmax_tr_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(rbio_sovmax_tr_t12, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(rbio_sovmax_tr_t24, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement(rbio_sovmax_tr_t36, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 36, 0.32).
narrative_ontology:measurement(rbio_sovmax_tr_t48, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 48, 0.38).
narrative_ontology:measurement(rbio_sovmax_tr_t60, rbio_practice_norm_complex__sovereignty_maximalist_reading, theater_ratio, 60, 0.42).

% Extraction over time
narrative_ontology:measurement(rbio_sovmax_be_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(rbio_sovmax_be_t12, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement(rbio_sovmax_be_t24, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(rbio_sovmax_be_t36, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 36, 0.58).
narrative_ontology:measurement(rbio_sovmax_be_t48, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 48, 0.66).
narrative_ontology:measurement(rbio_sovmax_be_t60, rbio_practice_norm_complex__sovereignty_maximalist_reading, base_extractiveness, 60, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(rbio_sovmax_su_t0, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(rbio_sovmax_su_t12, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 12, 0.42).
narrative_ontology:measurement(rbio_sovmax_su_t24, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 24, 0.52).
narrative_ontology:measurement(rbio_sovmax_su_t36, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 36, 0.62).
narrative_ontology:measurement(rbio_sovmax_su_t48, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 48, 0.7).
narrative_ontology:measurement(rbio_sovmax_su_t60, rbio_practice_norm_complex__sovereignty_maximalist_reading, suppression_requirement, 60, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rbio_practice_norm_complex__sovereignty_maximalist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, liberal_institutional_reading).
narrative_ontology:affects_constraint(rbio_practice_norm_complex__sovereignty_maximalist_reading, hegemonic_extraction_reading).

% DUAL FORMULATION NOTE:
% This constraint is the sovereignty-maximalist reading of the RBIO practice norm complex kernel. Its sibling readings instantiate structurally distinct constraints from the same natural-language label. The epsilon values and beneficiary/victim structures differ across the family: the liberal reading lacks the authoritarian-regime beneficiary set, while the hegemonic reading redistributes beneficiary status to the historical hegemon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
