% ============================================================================
% CONSTRAINT STORY: unconditional_income_support__freedom_floor_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unconditional_income_support__freedom_floor_reading, []).

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
 *   constraint_id: unconditional_income_support__freedom_floor_reading
 *   human_readable: Unconditional Income Support as Autonomy-Enabling Floor
 *   domain: political_economy/social_policy
 *
 * SUMMARY:
 *   Unconditional income support as an autonomy-enabling floor (the
 *   freedom_floor_reading of the unconditional_income_support kernel). The
 *   constraint guarantees a material baseline to all residents without
 *   means-testing or work requirements. It is read as a pure coordination
 *   mechanism that removes survival coercion from labor market entry,
 *   eliminates the stigma and administrative burden of targeted welfare, and
 *   buffers individuals against market volatility. This reading claims a
 *   Pareto-improving structure with no identifiable victims, grounded in
 *   empirical pilot evidence from Alaska and Kenya showing minimal
 *   labor-supply reduction. Sibling readings include the
 *   dependency_trap_reading (incentive-distorting extraction) and the
 *   universality_paradox_reading (politically ambiguous coalition). This JSON
 *   instantiates only the freedom_floor reading as a clean, Îµ-invariant
 *   constraint.
 *
 * KEY AGENTS:
 *   - precarious_workers: Primary beneficiary (powerless/mobile) â gains outside option against exploitative labor.
 *   - caregivers: Primary beneficiary (powerless/mobile) â recognizes unpaid care with financial independence.
 *   - artists: Primary beneficiary (moderate/mobile) â enables voluntary creative sector participation.
 *   - abuse_victims: Primary beneficiary (powerless/mobile) â unconditional payment provides escape resource independent of relationship status.
 *   - public_finance_authority: Agenda setter (institutional/arbitrage) â administers disbursement without rent capture.
 *   - autonomy_researchers: Analytical observer (analytical/analytical) â evaluates pilot evidence on labor supply and wellbeing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unconditional_income_support__freedom_floor_reading, 0.35).
domain_priors:suppression_score(unconditional_income_support__freedom_floor_reading, 0.15).
domain_priors:theater_ratio(unconditional_income_support__freedom_floor_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(unconditional_income_support__freedom_floor_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unconditional_income_support__freedom_floor_reading, rope).
narrative_ontology:human_readable(unconditional_income_support__freedom_floor_reading, "Unconditional Income Support as Autonomy-Enabling Floor").
narrative_ontology:topic_domain(unconditional_income_support__freedom_floor_reading, "political_economy/social_policy").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unconditional_income_support__freedom_floor_reading, 'd2b185bb-5e74-450f-a6d0-bf85825f163f').
narrative_ontology:cs_kernel_codification('d2b185bb-5e74-450f-a6d0-bf85825f163f', formalized).
narrative_ontology:cs_authority_grounding('d2b185bb-5e74-450f-a6d0-bf85825f163f', expertise).
narrative_ontology:cs_interpretation_layer_present('d2b185bb-5e74-450f-a6d0-bf85825f163f').
narrative_ontology:cs_reading_relation('d2b185bb-5e74-450f-a6d0-bf85825f163f', unconditional_income_support__dependency_trap_reading, coexists_with).
narrative_ontology:cs_reading_relation('d2b185bb-5e74-450f-a6d0-bf85825f163f', unconditional_income_support__universality_paradox_reading, influences).
narrative_ontology:cs_axiom('d2b185bb-5e74-450f-a6d0-bf85825f163f', foundational, autonomy_requires_unconditional_floor).
narrative_ontology:cs_axiom_status(autonomy_requires_unconditional_floor, holdable).
narrative_ontology:cs_axiom_grounding('d2b185bb-5e74-450f-a6d0-bf85825f163f', autonomy_requires_unconditional_floor, deontological).
narrative_ontology:cs_axiom('d2b185bb-5e74-450f-a6d0-bf85825f163f', foundational, market_coercion_is_structural).
narrative_ontology:cs_axiom_status(market_coercion_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('d2b185bb-5e74-450f-a6d0-bf85825f163f', market_coercion_is_structural, empirically_contingent).
narrative_ontology:cs_reference_frame('d2b185bb-5e74-450f-a6d0-bf85825f163f', autonomous_market_participation).
narrative_ontology:cs_drift_state('d2b185bb-5e74-450f-a6d0-bf85825f163f', contemporary_pilot_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d2b185bb-5e74-450f-a6d0-bf85825f163f', '').
narrative_ontology:cs_kernel_id(unconditional_income_support__freedom_floor_reading, unconditional_income_support).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, precarious_workers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, caregivers).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, artists).
narrative_ontology:constraint_beneficiary(unconditional_income_support__freedom_floor_reading, abuse_victims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive an unconditional income floor that removes the necessity of accepting exploitative or unsafe employment. Can refuse bad job offers without destitution. The payment is unconditional and portable, so exit from the arrangement is trivial, but the labor market options they retain are improved by its presence.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, precarious_workers, beneficiary,
    powerless, biographical, mobile, national).

% Perform unpaid or underpaid care work that the market does not remunerate. The unconditional floor recognizes their contribution and provides financial independence regardless of labor market status, enabling continued caregiving without poverty risk.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, caregivers, beneficiary,
    powerless, biographical, mobile, national).

% Engage in creative work with irregular income. The floor removes the coercion to take non-artistic work for survival, enabling voluntary sector participation in culture. Can pursue creative labor without means-testing or bureaucratic eligibility games.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, artists, beneficiary,
    moderate, biographical, mobile, national).

% Use the unconditional income as an escape resource from coercive relationships or environments. Because it is not means-tested or tied to a partner's income, it provides a private resource that enables exit from abuse without proving eligibility to a bureaucrat.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, abuse_victims, beneficiary,
    powerless, immediate, mobile, national).

% Designs tax and disbursement mechanisms for the unconditional floor. Does not extract rents from the program; its interest is in macroeconomic stabilization and reducing bureaucratic overhead of means-tested systems. Could alter the policy parameters but is bound by political mandate and empirical evidence.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, public_finance_authority, agenda_setter,
    institutional, generational, arbitrage, national).

% Study the effects of unconditional transfers on labor supply, health, and bargaining power. Their analytical seat is independent of program administration; they track whether the floor actually produces the autonomy effects claimed by this reading.
narrative_ontology:constraint_stakeholder(unconditional_income_support__freedom_floor_reading, autonomy_researchers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a guaranteed material floor that solves the collective-action problem of survival-risk coercion in labor markets, enabling voluntary participation in work, care, and culture without the bureaucratic overhead of targeted eligibility verification.
% TRANSFER_FUNCTION: Moves resources from the general tax base to all residents unconditionally, with the net effect of increasing the bargaining power and outside options of those with the least labor market power.
% ABSENT_VOICES: Employers who rely on low-wage labor precarity, and fiscal conservatives who regard unconditional transfers as morally hazardous, are present in public debate but structurally absent from the beneficiary coalition; their objections are heard in legislatures, not in the design of the floor itself.
% DISAPPEARANCE_RATIONALE: If the unconditional floor vanished overnight, precarious workers would lose their outside option and be forced into exploitative or unsafe employment; caregivers would face destitution; abuse victims would lose independent escape resources; artists would abandon creative work for survival jobs; the labor market would re-coordinate around coercion rather than voluntary participation.
% FOUNDING_PROBLEM: Industrial and post-industrial labor markets expose individuals to destitution if they refuse available employment, creating structural coercion that prevents genuine consent to labor contracts and devalues unpaid care work.
% FOUNDING_PROBLEM_CORROBORATION: Autonomy researchers and feminist political economists attest the problem from outside the immediate beneficiary set; labor ethnographers document survival coercion independently of the policy coalition. Fiscal conservative critics corroborate the existence of the problem but dispute whether unconditional income is the correct solution.
narrative_ontology:disappearance_verdict(unconditional_income_support__freedom_floor_reading, world_rearranges).
narrative_ontology:founding_problem_status(unconditional_income_support__freedom_floor_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unconditional_income_support__freedom_floor_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(unconditional_income_support__freedom_floor_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unconditional_income_support__freedom_floor_reading, 0.35, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unconditional_income_support__freedom_floor_reading_tests).
:- end_tests(unconditional_income_support__freedom_floor_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is moderate (0.30â0.38) because the fiscal transfer required to fund a universal floor is substantial; however, the reading treats this as coordination cost rather than asymmetric extraction because the arrangement is framed as Pareto-improving. Suppression is low (0.15) because the constraint is unconditional â it does not suppress alternatives or coerce behavior. Theater ratio is low (0.10) because the program is functionally a direct cash transfer with minimal performative overhead. Accessibility collapse is moderate-low (0.25) because alternatives (targeted welfare, private charity, family support) persist but are rendered less necessary. Resistance is moderate (0.40) because the policy faces ideological opposition from desert-based and fiscal-conservative frameworks despite pilot evidence. Measurements share a single time grid (0,5,10,15,20) to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   Beneficiary seats (precarious workers, caregivers, artists, abuse victims) experience the constraint as subsidy and autonomy expansion (low directionalities, negative effective extraction). The agenda-setting administration experiences it as a macroeconomic stabilization tool with no rent capture. Opponents outside the authored stakeholder set â notably high-net-worth taxpayers and low-wage employers â would experience the same fiscal mechanism as extractive redistribution (high directionality), but they are not declared as victims in this reading because the Pareto-improvement claim holds that social stability benefits all. The engine computes divergence only across the authored structural surface.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are those structurally constrained by bad labor market options; they receive the unconditional transfer and experience increased exit options from coercive relationships and unsafe jobs. There are no declared victims because the reading asserts that the tax base funds a public good (autonomy) that stabilizes the social contract. Directionality for beneficiaries is near the full-beneficiary end. The public finance authority sits near symmetric because it administers but does not capture the transfer. Analytical observers sit at neutral-exit with no stake in the fiscal flow.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling coordination as extraction by requiring minimal suppression and net beneficiary status for rope certification. If the constraint were actually a snare, we would expect to see active enforcement against opt-outs, means-testing bureaucracy creating extraction, or identifiable victims bearing concentrated costs. The absence of victims and the low suppression score keep the computed type in rope territory even though base extractiveness is moderate â the engine will compute low or negative effective extraction for beneficiary seats. Mandatrophy would be flagged only if the founding problem (survival coercion in labor markets) were dead; the status is live, so the arrangement is not a piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ubi_pareto_improvement_empirical,
    'Does unconditional income support actually produce a Pareto improvement, or do net taxpayers experience the fiscal burden as structural extraction?',
    'Longitudinal studies comparing subjective wellbeing, consumption patterns, and fiscal incidence across income deciles in jurisdictions with unconditional transfers.',
    'If net taxpayers are structurally worse off, the no-victim claim fails and the constraint would recompute toward tangled_rope for the tax-base seat; if the Pareto claim holds, the rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ubi_pareto_improvement_empirical, empirical, 'Whether UBI is genuinely Pareto-improving or extracts from the tax base').

omega_variable(
    labor_market_autonomy_mechanism,
    'Is the observed autonomy effect caused by the income floor itself, or by the removal of bureaucratic conditionality and welfare stigma?',
    'Comparative analysis of UBI pilots against generous but conditional welfare regimes, isolating the unconditionality treatment effect.',
    'If autonomy stems from unconditionality, the coordination function is inseparable from the no-strings design; if it stems from income alone, the constraint is fungible with other transfers and the rope classification weakens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_market_autonomy_mechanism, conceptual, 'Whether autonomy derives from unconditionality or from income transfer alone').

omega_variable(
    reading_contest_autonomy_vs_dependency,
    'Does unconditional income support structurally enable autonomy or create dependency, and is this ambiguity resolvable within a single framework?',
    'Cross-pilot causal inference on labor supply and dependency indicators, combined with philosophical consensus on the nature of freedom under market capitalism.',
    'Resolving toward autonomy validates this reading as rope; resolving toward dependency validates the sibling dependency_trap_reading as snare or tangled_rope. The epsilon values are reading-indexed and structurally diverge.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_contest_autonomy_vs_dependency, conceptual, 'Kernel-reading contest between autonomy and dependency framings of UBI').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unconditional_income_support__freedom_floor_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ubi_freedom_floor_tr_t0, unconditional_income_support__freedom_floor_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(ubi_freedom_floor_tr_t5, unconditional_income_support__freedom_floor_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(ubi_freedom_floor_tr_t10, unconditional_income_support__freedom_floor_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(ubi_freedom_floor_tr_t15, unconditional_income_support__freedom_floor_reading, theater_ratio, 15, 0.1).
narrative_ontology:measurement(ubi_freedom_floor_tr_t20, unconditional_income_support__freedom_floor_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(ubi_freedom_floor_be_t0, unconditional_income_support__freedom_floor_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(ubi_freedom_floor_be_t5, unconditional_income_support__freedom_floor_reading, base_extractiveness, 5, 0.32).
narrative_ontology:measurement(ubi_freedom_floor_be_t10, unconditional_income_support__freedom_floor_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(ubi_freedom_floor_be_t15, unconditional_income_support__freedom_floor_reading, base_extractiveness, 15, 0.36).
narrative_ontology:measurement(ubi_freedom_floor_be_t20, unconditional_income_support__freedom_floor_reading, base_extractiveness, 20, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(ubi_freedom_floor_su_t0, unconditional_income_support__freedom_floor_reading, suppression_requirement, 0, 0.12).
narrative_ontology:measurement(ubi_freedom_floor_su_t5, unconditional_income_support__freedom_floor_reading, suppression_requirement, 5, 0.13).
narrative_ontology:measurement(ubi_freedom_floor_su_t10, unconditional_income_support__freedom_floor_reading, suppression_requirement, 10, 0.15).
narrative_ontology:measurement(ubi_freedom_floor_su_t15, unconditional_income_support__freedom_floor_reading, suppression_requirement, 15, 0.15).
narrative_ontology:measurement(ubi_freedom_floor_su_t20, unconditional_income_support__freedom_floor_reading, suppression_requirement, 20, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unconditional_income_support__freedom_floor_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
