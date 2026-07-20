% ============================================================================
% CONSTRAINT STORY: income_support_commitment__targeting_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_commitment__targeting_efficiency_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: income_support_commitment__targeting_efficiency_reading
 *   human_readable: Targeted Income Support Concentration on Demonstrated Need
 *   domain: political_economy/social_policy/welfare_state_theory
 *
 * SUMMARY:
 *   This constraint instantiates the targeting_efficiency_reading of the
 *   income_support_commitment kernel. It asserts that income support should
 *   be concentrated on demonstrated need rather than universally distributed.
 *   The kernel is contested: the freedom_floor_reading argues for
 *   unconditional universal support as an autonomy enabler, while the
 *   dependency_trap_reading frames unconditional support as a work
 *   disincentive. This reading competes with both by claiming that
 *   need-verification is fiscally necessary and allocatively superior. The
 *   structural data are authored independently of the source material's snare
 *   hypothesis; the metrics describe a constraint with high extraction and
 *   active enforcement that nonetheless delivers genuine transfers to the
 *   very poor, making tangled_rope the structurally true classification.
 *
 * KEY AGENTS:
 *   - targeted_program_recipients: Primary beneficiary and secondary payer (powerless/trapped) â receives concentrated transfers but bears administrative extraction, stigma, and UBI-replacement risk.
 *   - near_poor_excluded: Primary payer (powerless/trapped) â excluded by thresholds, bears welfare gaps and marginal tax cliffs.
 *   - welfare_administration: Agenda setter (institutional/mobile) â designs and enforces targeting rules, derives budget and authority from the regime.
 *   - ubi_advocates: Excluded voice (moderate/constrained) â offers universal alternative but is structurally absent from policy design.
 *   - policy_analyst_observer: Analytical observer (analytical/analytical) â evaluates cost-effectiveness from outside the beneficiary set.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, 0.78).
domain_priors:suppression_score(income_support_commitment__targeting_efficiency_reading, 0.68).
domain_priors:theater_ratio(income_support_commitment__targeting_efficiency_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, resistance, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__targeting_efficiency_reading, tangled_rope).
narrative_ontology:human_readable(income_support_commitment__targeting_efficiency_reading, "Targeted Income Support Concentration on Demonstrated Need").
narrative_ontology:topic_domain(income_support_commitment__targeting_efficiency_reading, "political_economy/social_policy/welfare_state_theory").

domain_priors:requires_active_enforcement(income_support_commitment__targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__targeting_efficiency_reading, 'bd9058bc-e6e9-454b-862d-92fd0b4eba31').
narrative_ontology:cs_kernel_codification('bd9058bc-e6e9-454b-862d-92fd0b4eba31', formalized).
narrative_ontology:cs_authority_grounding('bd9058bc-e6e9-454b-862d-92fd0b4eba31', expertise).
narrative_ontology:cs_interpretation_layer_present('bd9058bc-e6e9-454b-862d-92fd0b4eba31').
narrative_ontology:cs_reading_relation('bd9058bc-e6e9-454b-862d-92fd0b4eba31', income_support_commitment__freedom_floor_reading, influences).
narrative_ontology:cs_reading_relation('bd9058bc-e6e9-454b-862d-92fd0b4eba31', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('bd9058bc-e6e9-454b-862d-92fd0b4eba31', foundational, need_concentration_mandate).
narrative_ontology:cs_axiom_status(need_concentration_mandate, holdable).
narrative_ontology:cs_axiom_grounding('bd9058bc-e6e9-454b-862d-92fd0b4eba31', need_concentration_mandate, instrumental).
narrative_ontology:cs_axiom('bd9058bc-e6e9-454b-862d-92fd0b4eba31', foundational, universal_provision_fiscal_waste).
narrative_ontology:cs_axiom_status(universal_provision_fiscal_waste, holdable).
narrative_ontology:cs_axiom_grounding('bd9058bc-e6e9-454b-862d-92fd0b4eba31', universal_provision_fiscal_waste, instrumental).
narrative_ontology:cs_reference_frame('bd9058bc-e6e9-454b-862d-92fd0b4eba31', needs_based_redistributive_state).
narrative_ontology:cs_drift_state('bd9058bc-e6e9-454b-862d-92fd0b4eba31', post_ubi_resurgence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bd9058bc-e6e9-454b-862d-92fd0b4eba31', '').
narrative_ontology:cs_kernel_id(income_support_commitment__targeting_efficiency_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, near_poor_excluded).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients).
narrative_ontology:constraint_vindicates(income_support_commitment__targeting_efficiency_reading, means_test_efficiency_hypothesis).
narrative_ontology:constraint_vindicates(income_support_commitment__targeting_efficiency_reading, fiscal_targeting_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive concentrated means-tested benefits that exceed what a revenue-neutral UBI would provide for their household type. Must continually demonstrate need through intrusive administrative compliance, face surveillance and stigma, and bear the risk that any policy shift toward universal provision would reduce their transfer substantially. Labor market exit to an income comparable to their benefit package is blocked by childcare costs, skill gaps, and local wage levels.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients, beneficiary,
    powerless, immediate, trapped, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, targeted_program_recipients, payer).

% Designs eligibility criteria, conducts verification audits, and enforces compliance with targeting rules. Justifies the apparatus as ensuring fiscal responsibility and protecting scarce resources from leakage to the non-poor. Budget and staffing scale with the complexity and stringency of the targeting regime.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, welfare_administration, agenda_setter,
    institutional, generational, mobile, national).

% Earn income just above means-test thresholds and receive zero targeted support, yet face high effective marginal tax rates and benefit cliffs that make advancement costly. They finance the targeting regime indirectly through foregone support and labor market rigidities, and are politically invisible in debates framed exclusively around the deserving poor.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, near_poor_excluded, payer,
    powerless, immediate, trapped, national).

% Argue that universal provision would eliminate administrative waste, stigma, and exclusion errors, but are structurally excluded from mainstream policy design because the targeting commitment has pre-committed fiscal space and political discourse around demonstrated need. Their alternative is dismissed as fiscally infeasible unless it cannibalizes existing targeted programs.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, ubi_advocates, excluded,
    moderate, generational, constrained, national).

% Evaluates comparative cost-effectiveness of targeted versus universal provision from outside the benefiting parties. Notes that targeting achieves higher per-recipient transfers but at the cost of exclusion errors, administrative overhead, and political fragmentation of the welfare constituency.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, policy_analyst_observer, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Concentrate limited public resources on households with the highest demonstrated need, avoiding leakage to the non-poor and maximizing poverty-reduction per dollar spent.
% TRANSFER_FUNCTION: Moves income from the general tax base to verified low-income households, while moving administrative burden, compliance risk, and surveillance costs from the state to those same households.
% ABSENT_VOICES: The near-poor excluded by thresholds and universalist advocates are absent from the policy design table; the debate is framed between fiscal conservatives and social service professionals, both committed to selectivity.
% DISAPPEARANCE_RATIONALE: If the targeting commitment vanished, the means-testing bureaucracy would contract, the political coalition for social protection would broaden to include the near-poor, and policy would likely drift toward universal or less conditional provision; the current budget lines, administrative careers, and constituency boundaries depend on the targeting apparatus.
% FOUNDING_PROBLEM: Mid-20th century welfare states faced acute fiscal constraints and political resistance to broad-based taxation; targeting was introduced to legitimize social spending by promising that only the truly needy would receive transfers.
% FOUNDING_PROBLEM_CORROBORATION: Economic historians and left-policy analysts outside the targeting bureaucracy attest that the founding fiscal constraint has been substantially loosened by productivity growth and that the arrangement now persists beyond its original justification; the bureaucracy itself claims scarcity remains live. No consensus corroboration exists from outside the benefiting parties.
narrative_ontology:disappearance_verdict(income_support_commitment__targeting_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__targeting_efficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__targeting_efficiency_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_commitment__targeting_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__targeting_efficiency_reading, 0.78, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_commitment__targeting_efficiency_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_commitment__targeting_efficiency_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the targeting apparatus extracts substantial compliance costs, dignity, and political autonomy from recipients while excluding the near-poor entirely. Suppression is substantial (0.68) because the constraint's persistence depends on actively excluding universal alternatives and enforcing eligibility boundaries. Theater ratio is moderate (0.42): the poverty-reduction function is real, but a growing share of administrative activity performs fiscal control and moral surveillance rather than need alleviation. Accessibility collapse (0.60) reflects that universal alternatives are politically collapsed because fiscal space and discourse are pre-committed to selectivity. Resistance (0.52) captures ongoing advocacy for UBI and documented exclusion errors. The temporal series run on one shared grid.
 *
 * PERSPECTIVAL GAP:
 *   The welfare administration seat experiences the constraint as genuine coordination it expertly maintains; the recipient and near-poor seats experience it as enforced extraction with high exit barriers. The analyst seat sees the divergence: higher per-recipient transfers versus systemic exclusion and fragmentation. The engine computes this divergence from the structural asymmetry in power and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The welfare administration sits near the beneficiary end (low d): it derives budget, staffing, and authority from the constraint. Targeted recipients and the near-poor sit near the target end (high d): they bear the costs of verification, exclusion, and political vulnerability. UBI advocates are excluded rather than coordinated; their exclusion is part of the suppressive structure. The policy analyst sits at analytical remove.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â fiscal scarcity and the need to legitimize social spending â is contested as either still live or long superseded by productivity growth. If the problem is dead but the arrangement persists, the constraint exhibits mandatrophy. The temporal measurements show rising extractiveness and theater over the interval, consistent with functional drift rather than steady-state coordination. The R5 mismatch (contested status + world_rearranges disappearance) flags the constraint as a candidate for piton transition if the coordination function atrophies further, though it has not yet reached pure performative inertia.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nominal_benefit_vs_structural_victimization,
    'Are targeted program recipients net beneficiaries or net victims when compliance costs, stigma, political vulnerability, and foregone autonomy are priced against the cash transfer?',
    'Comprehensive longitudinal study comparing subjective well-being, time use, and political participation of targeted recipients versus near-poor excluded and universal-system counterparts.',
    'If net victims, the beneficiary array overstates coordination and the constraint tilts toward snare; if net beneficiaries, the tangled_rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nominal_benefit_vs_structural_victimization, empirical, 'Whether the cash transfer outweighs the structural extraction borne by recipients').

omega_variable(
    ubi_cannibalization_inevitability,
    'Is UBI structurally impossible without cannibalizing targeted programs, or is this zero-sum fiscally constructed by the targeting commitment itself?',
    'Comparative fiscal analysis across jurisdictions with varying tax capacity; natural experiment from resource-rich states that have implemented universal dividends without cutting targeted supports.',
    'If the fiscal trade-off is politically constructed, the targeting regime''s suppression of alternatives is stronger than its natural scarcity justification; if genuinely inescapable, the coordination function is more defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ubi_cannibalization_inevitability, conceptual, 'Whether the UBI-targeting trade-off is structural or politically manufactured').

omega_variable(
    targeting_persistence_motive,
    'Does targeting persist because it genuinely solves an allocation problem, or because it serves the administrative constituency and fragments the political coalition for redistribution?',
    'Political economy analysis of administrative budget growth versus poverty reduction outcomes; cross-national comparison of targeting stringency and labor market policy.',
    'If administrative constituency and political fragmentation are the primary drivers, the coordination story is cover and the constraint approaches snare; if poverty outcomes are the driver, tangled_rope holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeting_persistence_motive, empirical, 'Administrative capture versus genuine allocative efficiency as the persistence mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__targeting_efficiency_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(targeting_efficiency_tr_t0, income_support_commitment__targeting_efficiency_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(targeting_efficiency_tr_t10, income_support_commitment__targeting_efficiency_reading, theater_ratio, 10, 0.25).
narrative_ontology:measurement(targeting_efficiency_tr_t20, income_support_commitment__targeting_efficiency_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(targeting_efficiency_tr_t30, income_support_commitment__targeting_efficiency_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(targeting_efficiency_tr_t40, income_support_commitment__targeting_efficiency_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(targeting_efficiency_tr_t50, income_support_commitment__targeting_efficiency_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(targeting_efficiency_be_t0, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(targeting_efficiency_be_t10, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(targeting_efficiency_be_t20, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(targeting_efficiency_be_t30, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(targeting_efficiency_be_t40, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 40, 0.73).
narrative_ontology:measurement(targeting_efficiency_be_t50, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 50, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(targeting_efficiency_su_t0, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(targeting_efficiency_su_t10, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(targeting_efficiency_su_t20, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(targeting_efficiency_su_t30, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(targeting_efficiency_su_t40, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 40, 0.67).
narrative_ontology:measurement(targeting_efficiency_su_t50, income_support_commitment__targeting_efficiency_reading, suppression_requirement, 50, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__targeting_efficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, income_support_commitment__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the income_support_commitment kernel. The three readings (targeting_efficiency, freedom_floor, dependency_trap) are structurally distinct claims about the same policy domain and form a constraint family. This reading focuses on the allocative efficiency of need-based concentration; siblings address autonomy and work incentives respectively.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
