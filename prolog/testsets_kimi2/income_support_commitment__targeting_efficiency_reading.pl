% ============================================================================
% CONSTRAINT STORY: income_support_commitment__targeting_efficiency_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
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
 *   human_readable: Universal Basic Income Cannibalization of Targeted Welfare
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   This constraint instantiates the targeting_efficiency_reading of the
 *   income_support_commitment kernel. From this reading's perspective, the
 *   policy dynamic toward revenue-neutral universal basic income funded by
 *   dismantling deeply targeted categorical benefits operates as a snare. The
 *   archetype of a parent receiving substantial targeted support illustrates
 *   the structural delta: a family receiving significant targeted benefits
 *   becomes a net victim under a universal replacement that delivers a lower
 *   flat payment. The poor are nominal beneficiaries of the universal
 *   transfer because they receive the check, but actual victims of the
 *   cannibalization because their net support falls. The constraint persists
 *   through political coalition-building among middle-class beneficiaries and
 *   the rhetorical packaging of universalism as anti-poverty policy.
 *
 * KEY AGENTS:
 *   - deeply_poor_families: Primary target (powerless/trapped) â nominal beneficiaries who are actual victims of net extraction
 *   - middle_class_beneficiaries: Primary beneficiary (moderate/mobile) â receive universal payment without offsetting losses
 *   - ubi_policymakers: Agenda-setter (institutional/arbitrage) â designs and enforces the universal replacement
 *   - anti_poverty_advocates: Excluded voice (organized/constrained) â objects but is marginalized in design
 *   - policy_analysts: Analytical observer (analytical/analytical) â documents distributional reversal
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_commitment__targeting_efficiency_reading, 0.82).
domain_priors:suppression_score(income_support_commitment__targeting_efficiency_reading, 0.7).
domain_priors:theater_ratio(income_support_commitment__targeting_efficiency_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(income_support_commitment__targeting_efficiency_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_commitment__targeting_efficiency_reading, snare).
narrative_ontology:human_readable(income_support_commitment__targeting_efficiency_reading, "Universal Basic Income Cannibalization of Targeted Welfare").
narrative_ontology:topic_domain(income_support_commitment__targeting_efficiency_reading, "political_economy/social_policy").

domain_priors:requires_active_enforcement(income_support_commitment__targeting_efficiency_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_commitment__targeting_efficiency_reading, 'c952adbf-b638-49f2-9aca-e59939472f9e').
narrative_ontology:cs_kernel_codification('c952adbf-b638-49f2-9aca-e59939472f9e', distributed).
narrative_ontology:cs_authority_grounding('c952adbf-b638-49f2-9aca-e59939472f9e', distributed).
narrative_ontology:cs_reading_relation('c952adbf-b638-49f2-9aca-e59939472f9e', income_support_commitment__freedom_floor_reading, forecloses).
narrative_ontology:cs_reading_relation('c952adbf-b638-49f2-9aca-e59939472f9e', income_support_commitment__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('c952adbf-b638-49f2-9aca-e59939472f9e', foundational, targeting_maximizes_poor_net_income).
narrative_ontology:cs_axiom_status(targeting_maximizes_poor_net_income, holdable).
narrative_ontology:cs_axiom_grounding('c952adbf-b638-49f2-9aca-e59939472f9e', targeting_maximizes_poor_net_income, empirically_contingent).
narrative_ontology:cs_axiom('c952adbf-b638-49f2-9aca-e59939472f9e', foundational, demonstrated_need_legitimizes_transfer).
narrative_ontology:cs_axiom_status(demonstrated_need_legitimizes_transfer, holdable).
narrative_ontology:cs_axiom_grounding('c952adbf-b638-49f2-9aca-e59939472f9e', demonstrated_need_legitimizes_transfer, deontological).
narrative_ontology:cs_reference_frame('c952adbf-b638-49f2-9aca-e59939472f9e', need_based_safety_net).
narrative_ontology:cs_drift_state('c952adbf-b638-49f2-9aca-e59939472f9e', contemporary_ubi_advocacy_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c952adbf-b638-49f2-9aca-e59939472f9e', '').
narrative_ontology:cs_kernel_id(income_support_commitment__targeting_efficiency_reading, income_support_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, deeply_poor_families).
narrative_ontology:constraint_beneficiary(income_support_commitment__targeting_efficiency_reading, middle_class_beneficiaries).
narrative_ontology:constraint_victim(income_support_commitment__targeting_efficiency_reading, deeply_poor_families).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive a nominal universal basic income payment but lose categorical targeted benefits including housing subsidies, childcare assistance, and nutrition support that previously provided substantially higher total support. Net disposable income falls. Cannot opt out of the universal scheme to retain prior targeted benefits. Dependent on state support with no market alternative.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, deeply_poor_families, payer,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(income_support_commitment__targeting_efficiency_reading, deeply_poor_families, beneficiary).

% Receive the universal basic income payment without losing prior benefits because they were never eligible for deeply targeted programs. Experience net gain or fiscal neutrality. Form a broad political constituency that favors maintaining the universal program over returning to targeted assistance.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, middle_class_beneficiaries, beneficiary,
    moderate, biographical, mobile, national).

% Design and advocate for revenue-neutral universal basic income funded by consolidating and reducing targeted programs. Frame the policy as administrative simplification and poverty reduction. Benefit politically from a broad middle-class constituency and the popularity of universal entitlement.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, ubi_policymakers, agenda_setter,
    institutional, generational, arbitrage, national).

% Argue that replacing targeted categorical benefits with a flat universal payment produces net losses for the deepest poor. Substantively marginalized in the policy design process by the political momentum of the UBI coalition and the simplicity narrative, despite formal consultation appearances.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, anti_poverty_advocates, excluded,
    organized, biographical, constrained, national).

% Produce distributional analyses comparing targeted and universal transfer regimes. Document that revenue-neutral UBI funded by cannibalizing targeted programs produces net income losses for the poorest households while transferring resources up the distribution.
narrative_ontology:constraint_stakeholder(income_support_commitment__targeting_efficiency_reading, policy_analysts, observer,
    analytical, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_commitment__targeting_efficiency_reading, diffuse).
narrative_ontology:fixing_cost_class(income_support_commitment__targeting_efficiency_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Consolidates fragmented income support programs into a single universal payment, eliminating categorical eligibility determination and reducing administrative overhead of means-testing.
% TRANSFER_FUNCTION: Moves net resources from deeply poor families who lose targeted benefits exceeding the universal payment to the broader population who receive universal payments without offsetting losses, under the cover of administrative simplification.
% ABSENT_VOICES: Recipients of deeply targeted benefits who experience net income loss are rhetorically present as beneficiaries of simplification but substantively excluded from policy design; anti-poverty advocates warning of distributional harm are drowned out by the universalism narrative.
% DISAPPEARANCE_RATIONALE: If the universal replacement regime vanished overnight and targeted categorical benefits were restored, deeply poor families would regain higher net support levels, the administrative apparatus of means-testing would reconstitute, and the political coalition of universal middle-class beneficiaries would dissolve.
% FOUNDING_PROBLEM: A fragmented welfare state with high administrative overhead, stigma associated with means-testing, incomplete coverage leaving some needy individuals without support, and complex eligibility bureaucracies.
% FOUNDING_PROBLEM_CORROBORATION: UBI advocates and some administrative reformers attest the founding problem remains live. Anti-poverty researchers and targeted-program administrators attest the problem is better solved by patching coverage gaps and streamlining administration than by universalizing payments that dilute resources for the poorest. Independent fiscal analyses from nonpartisan budget offices corroborate the distributional trade-off.
narrative_ontology:disappearance_verdict(income_support_commitment__targeting_efficiency_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_commitment__targeting_efficiency_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_commitment__targeting_efficiency_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(income_support_commitment__targeting_efficiency_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_commitment__targeting_efficiency_reading, 0.82, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is high (0.82) because the revenue-neutral UBI extracts from the deepest poor to fund payments for the broader population. Suppression (0.70) reflects the political suppression of targeted alternatives: means-testing is stigmatized as bureaucratic and the poor are reclassified as beneficiaries of simplification rather than victims of cuts. Theater ratio (0.55) captures the growing gap between anti-poverty rhetoric and documented net-income losses. Accessibility collapse (0.78) because universal benefits create a middle-class constituency that makes return to targeting politically nearly impossible. Resistance (0.50) from anti-poverty advocates and fiscal analysts is present but structurally outmatched.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and middle-class beneficiary seats experience the constraint as modernization and material gain (low d). The deeply poor seat experiences the same arrangement as extraction disguised by nominal inclusion (high d). The engine computes this divergence from structural data: the directionality override for powerless agents resolves the beneficiary-victim dual classification toward actual victim status.
 *
 * DIRECTIONALITY LOGIC:
 *   UBI policymakers and middle-class beneficiaries are structural beneficiaries (low d). Deeply poor families appear in both beneficiary and victim arrays because they receive the nominal universal payment while losing larger targeted benefits; the directionality override for the powerless atom pushes their classification toward the target end to reflect their actual net-loss position. Exit options differentiate the seats: middle-class beneficiaries are mobile, while poor families are trapped in the scheme.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling the UBI dynamic as rope or coordination by requiring victim identification and showing that the coordination function of administrative simplification is cover for extraction. The genuine coordination problem of fragmented welfare administration is solved by the constraint's rhetoric but not by its actual operation, which redistributes upward. Mandatrophy is not claimed because the constraint's function has not atrophied; it is operating as designed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    funding_mechanism_ambiguity,
    'Is the universal basic income funded by cannibalizing targeted programs, or by new progressive taxation that leaves the poor net neutral or better?',
    'Legislative text and budget scoring: if the UBI is revenue-neutral through consolidation of existing programs, extraction is high; if funded by new taxes on wealth or high incomes, the distributional picture changes.',
    'If funded by new taxation, the constraint''s extractiveness drops substantially and the victim classification may dissolve; if funded by cannibalization, the snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(funding_mechanism_ambiguity, empirical, 'Whether UBI is funded by cannibalizing targeted programs or new taxation.').

omega_variable(
    targeting_coverage_gap,
    'Do targeted programs actually leave significant needy populations uncovered, and would a universal program fill those gaps more effectively despite cannibalization?',
    'Comparative administrative data on take-up rates, eligibility denials, and unmet need in targeted regimes versus universal pilot programs.',
    'If targeting leaves large coverage gaps, the coordination function of universalism is stronger than this reading admits, potentially reclassifying toward tangled_rope; if coverage is already near-complete, the extraction framing holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeting_coverage_gap, empirical, 'Whether targeting leaves coverage gaps that universalism would fill.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_commitment__targeting_efficiency_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_commitment__targeting_efficiency_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(inco_tr_t5, income_support_commitment__targeting_efficiency_reading, theater_ratio, 5, 0.3).
narrative_ontology:measurement(inco_tr_t10, income_support_commitment__targeting_efficiency_reading, theater_ratio, 10, 0.4).
narrative_ontology:measurement(inco_tr_t15, income_support_commitment__targeting_efficiency_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(inco_tr_t20, income_support_commitment__targeting_efficiency_reading, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(inco_be_t5, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(inco_be_t10, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(inco_be_t15, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(inco_be_t20, income_support_commitment__targeting_efficiency_reading, base_extractiveness, 20, 0.82).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(income_support_commitment__targeting_efficiency_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_commitment__targeting_efficiency_reading, resource_allocation).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_commitment__targeting_efficiency_reading, dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the income_support_commitment kernel, decomposed per the epsilon-invariance principle. The targeting_efficiency_reading interprets the kernel as requiring need-based concentration. Sibling readings interpret the same kernel as unconditional support with different valences (autonomy vs dependency). Each reading instantiates a structurally distinct constraint with its own epsilon and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(income_support_commitment__targeting_efficiency_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
