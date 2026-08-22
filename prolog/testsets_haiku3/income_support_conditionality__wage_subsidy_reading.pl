% ============================================================================
% CONSTRAINT STORY: income_support_conditionality__wage_subsidy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_income_support_conditionality__wage_subsidy_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: income_support_conditionality__wage_subsidy_reading
 *   human_readable: Unconditional Income Support as Wage Suppression Enabler
 *   domain: political_economy/labor_economics/social_policy
 *
 * SUMMARY:
 *   This is the WAGE-SUBSIDY READING of the contested
 *   income_support_conditionality kernel. Unconditional income support is
 *   designed to provide a subsistence floor and enable worker economic
 *   agency. In this reading, the policy functions as an employer subsidy
 *   mechanism: by guaranteeing workers a minimum income regardless of wages,
 *   the policy allows employers to suppress nominal wages below the
 *   subsistence line, knowing workers will not starve. Workers remain
 *   dependent on both the wage and the subsidy — they cannot exit low-wage
 *   labor without losing the subsidy, and they cannot negotiate higher wages
 *   because employers know the subsidy absorbs the gap. The constraint is
 *   thus tangled_rope: genuine coordination function (income support provides
 *   a collective-action solution to precarity) coupled with asymmetric
 *   extraction (the subsidy's value is captured by employers via wage
 *   suppression). The policy is actively enforced: administrators do not
 *   adjust support levels to prevent wage suppression, do not impose employer
 *   compliance conditions, and treat wage suppression as external to the
 *   program's scope.
 *
 * KEY AGENTS:
 *   - low_wage_workers: trapped in identity-locked dependence on both subsidy and suppressed wages; cannot exit without losing the support or facing coordinated employer wage suppression
 *   - low_wage_employers: benefit from suppressed wages enabled by the subsidy; can pay below-subsistence rates while maintaining workforce stability and availability
 *   - income_support_administrators: set the program level and eligibility but do not monitor or prevent wage suppression; treat the program as separate from labor market dynamics
 *   - worker_advocates: excluded from program design; their analysis of wage suppression is treated as post-hoc criticism rather than incorporated into program structure
 *   - fiscal_authorities: observe the program's cost and sustainability; could impose employer compliance mechanisms but face political resistance from organized employer interests
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, 0.68).
domain_priors:suppression_score(income_support_conditionality__wage_subsidy_reading, 0.52).
domain_priors:theater_ratio(income_support_conditionality__wage_subsidy_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(income_support_conditionality__wage_subsidy_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(income_support_conditionality__wage_subsidy_reading, tangled_rope).
narrative_ontology:human_readable(income_support_conditionality__wage_subsidy_reading, "Unconditional Income Support as Wage Suppression Enabler").
narrative_ontology:topic_domain(income_support_conditionality__wage_subsidy_reading, "political_economy/labor_economics/social_policy").

domain_priors:requires_active_enforcement(income_support_conditionality__wage_subsidy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(income_support_conditionality__wage_subsidy_reading, 'ddd1e2d4-2003-40b9-b1bc-8b5516b0ed95').
narrative_ontology:cs_kernel_codification('ddd1e2d4-2003-40b9-b1bc-8b5516b0ed95', formalized).
narrative_ontology:cs_authority_grounding('ddd1e2d4-2003-40b9-b1bc-8b5516b0ed95', extraction).
narrative_ontology:cs_reading_relation('ddd1e2d4-2003-40b9-b1bc-8b5516b0ed95', income_support_conditionality__freedom_floor_reading, forecloses).
narrative_ontology:cs_reading_relation('ddd1e2d4-2003-40b9-b1bc-8b5516b0ed95', income_support_conditionality__dependency_trap_reading, coexists_with).
narrative_ontology:cs_axiom('ddd1e2d4-2003-40b9-b1bc-8b5516b0ed95', foundational, income_support_captured_by_employers).
narrative_ontology:cs_axiom_status(income_support_captured_by_employers, holdable).
narrative_ontology:cs_axiom_grounding('ddd1e2d4-2003-40b9-b1bc-8b5516b0ed95', income_support_captured_by_employers, empirically_contingent).
narrative_ontology:cs_axiom('ddd1e2d4-2003-40b9-b1bc-8b5516b0ed95', secondary, wage_suppression_institutional_outcome).
narrative_ontology:cs_axiom_status(wage_suppression_institutional_outcome, holdable).
narrative_ontology:cs_axiom_grounding('ddd1e2d4-2003-40b9-b1bc-8b5516b0ed95', wage_suppression_institutional_outcome, empirically_contingent).
narrative_ontology:cs_reference_frame('ddd1e2d4-2003-40b9-b1bc-8b5516b0ed95', decommodified_labor_and_worker_choice).
narrative_ontology:cs_drift_state('ddd1e2d4-2003-40b9-b1bc-8b5516b0ed95', contemporary_labor_market_equilibrium, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('ddd1e2d4-2003-40b9-b1bc-8b5516b0ed95', '').
narrative_ontology:cs_kernel_id(income_support_conditionality__wage_subsidy_reading, income_support_conditionality).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:constraint_victim(income_support_conditionality__wage_subsidy_reading, low_wage_workers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(income_support_conditionality__wage_subsidy_reading, higher_wage_workers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive unconditional income support at a fixed level that covers subsistence. Employers, knowing workers have this floor, suppress nominal wages below the subsistence line. Workers remain dependent on the income support to meet basic needs and cannot exit to better-paid employment without losing the subsidy or facing employer coordination on wage floors. Exit from low-wage labor is blocked not by legal barriers but by the inescapable co-dependence on both subsidy and wage income.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_workers, payer,
    powerless, biographical, identity_locked, national).

% Can pay wages substantially below subsistence levels because workers are cushioned by the income support. The subsidy absorbs the gap between wage and survival cost, allowing employers to compress labor costs while maintaining workforce availability and stability. Employers coordinate informally or formally on wage floors set below the income support threshold, capturing the subsidy's value as profit rather than funding worker consumption.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, low_wage_employers, beneficiary,
    organized, generational, arbitrage, national).

% Design and administer the unconditional income support program. They set the support level, eligibility criteria, and payment mechanics. They frame the program as a freedom floor enabling worker choice; they do not monitor or suppress wage suppression by employers, treating labor market wages and the income support as separate policy levers.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, income_support_administrators, agenda_setter,
    institutional, generational, analytical, national).

% Argue that wages should be indexed to the income support level or that the program should be conditional on employer wage compliance. They are excluded from policy design and from benefit design rules; their analysis of wage suppression is treated as an externality to the income support program rather than a structural feature.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, worker_advocates, excluded,
    moderate, biographical, constrained, national).

% Benefit from the existence of unconditional income support as a social stabilizer that reduces pressure for wage compression at all skill levels and maintains consumer demand. They do not face wage suppression because they occupy labor market positions where supply is scarce or skill rents are protected by credentialing. They rarely join coalition with low-wage workers because their exit options and power differ fundamentally.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, higher_wage_workers, beneficiary,
    organized, biographical, mobile, national).

% Monitor the income support program's cost and political sustainability. They track whether the program is functioning as intended (enabling worker dignity and economic participation) or whether it is becoming a de facto subsidy for low-wage employers. They have the authority to adjust payment levels or impose employer compliance conditions but face political resistance from organized employer interests.
narrative_ontology:constraint_stakeholder(income_support_conditionality__wage_subsidy_reading, fiscal_authorities, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(income_support_conditionality__wage_subsidy_reading, low_wage_employers).
narrative_ontology:fixing_cost_class(income_support_conditionality__wage_subsidy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides unconditional income support decoupled from employment, intending to enable worker economic participation, reduce precarity, and decouple consumption from labor market participation. At the payment level, solves the collective-action problem of wage coordination — no single worker can command higher wages alone.
% TRANSFER_FUNCTION: Moves income from the public treasury to all eligible workers at a uniform rate. In the wage-suppression reading, a second transfer is revealed: employers capture part of the subsidy's value by suppressing wages, moving the effective difference between support level and wage suppression to employer profit rather than worker consumption.
% ABSENT_VOICES: Worker advocates and labor economists studying wage suppression are excluded from program design; their analysis is treated as post-hoc criticism rather than incorporated into eligibility or employer-compliance mechanisms. Rival readings (freedom-floor reading, dependency-trap reading) would demand different program designs and would speak from excluded seats.
% DISAPPEARANCE_RATIONALE: If unconditional income support disappeared, low-wage workers would face immediate subsistence crisis and would need higher nominal wages or expanded employment benefits to survive. Employers would either raise wages or reduce employment. Labor market dynamics would reorganize around actual subsistence costs, not around the cost floor the income support creates.
% FOUNDING_PROBLEM: Low-wage workers face precarity, subsistence risk, and coercive labor conditions. Unconditional income support was designed to decouple survival from labor market participation and enable worker economic agency.
% FOUNDING_PROBLEM_CORROBORATION: Income support administrators and social policy advocates attest the founding problem remains live and the program addresses it. Employers and labor economists studying wage suppression attest the problem is structurally deeper — the program is captured by employers and institutionalizes below-subsistence wages. Fiscal authorities and independent research cite documented wage suppression following income support increases in multiple jurisdictions.
narrative_ontology:disappearance_verdict(income_support_conditionality__wage_subsidy_reading, world_rearranges).
narrative_ontology:founding_problem_status(income_support_conditionality__wage_subsidy_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(income_support_conditionality__wage_subsidy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(income_support_conditionality__wage_subsidy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(income_support_conditionality__wage_subsidy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(income_support_conditionality__wage_subsidy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(income_support_conditionality__wage_subsidy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(income_support_conditionality__wage_subsidy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.68) because the income support absorbs much of the nominal extraction, but employers capture the subsidy's value through wage suppression — the effective transfer to workers is substantially lower than the nominal support amount. Suppression is moderate (0.52) because the constraint operates partly through structural dependency (workers cannot exit without losing support) and partly through internalized acceptance (workers may normalize low wages as inevitable). Theater ratio is moderate (0.48) because the program genuinely solves a coordination problem (income security, precarity reduction) but a growing share of administrative activity is devoted to preventing policy modification (indexing support to wages, imposing employer compliance) rather than to the program's stated coordination function. Accessibility collapse is moderate (0.61) because alternatives (higher wages, alternative income, employment transitions) are formally available but practically foreclosed by the policy's structure — workers can theoretically leave but the subsidy dependence makes exit psychologically and economically unthinkable. Resistance is high (0.71) because worker advocates, labor economists, and some fiscal authorities actively resist the wage-suppression reading and advocate for policy modifications (wage indexing, employer compliance requirements) that would break the extraction mechanism. The measurement series shows extractiveness rising steeply in the first 9 time points (as employers learn to exploit the subsidy structure) then plateauing (as the policy reaches equilibrium extraction), while theater ratio rises initially then stabilizes (administrative effort shifts from program launch to defending against modification), and suppression requirement remains moderate and stable (the identity-lock and structural dependency do the coercive work without intense active enforcement once the policy is normalized).
 *
 * PERSPECTIVAL GAP:
 *   The payer seat (low-wage workers) experiences the constraint as inescapable dependence: both the subsidy and the wage are necessary for survival, and neither alone is sufficient. The agenda-setter seat (administrators) experiences the constraint as successful coordination that solves a real problem; wage suppression appears as a separate labor market outcome, not a product of their design. The beneficiary seats (employers, higher-wage workers) experience the constraint as socially stabilizing and economically beneficial, with no sense of extraction because the subsidy is a public good funded through general taxation, not directly charged to them. The engine will compute these divergences from the power/exit/beneficiary/victim atoms; the narrative explains WHY the divergence exists structurally.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-wage workers (powerless, identity-locked exit): they depend on both the subsidy and the wage; they cannot negotiate higher wages because employers know the subsidy cushions them; they cannot refuse low-wage work because the subsidy is conditional on work-search or employment history in many jurisdictions. This makes them full targets (d approaching 1.0). Low-wage employers (organized, arbitrage exit): they benefit from wage suppression, can coordinate on wage floors below the support level, and can absorb the policy without changing their profit structure fundamentally. They are near the beneficiary end (d approaching 0.1). Income support administrators (institutional, analytical): they could redesign to prevent wage suppression (indexing, employer compliance requirements) but face political resistance from organized employer interests. This makes them near symmetric (d ≈ 0.5). The directionality divergence is structural, not authored; the engine derives it from the atoms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem was precarity and coercive labor conditions: workers faced subsistence risk and had no negotiating power. The income support program was designed to decouple survival from labor market participation and enable worker choice. The measured outcome is wage suppression: employers capture the subsidy's value by suppressing wages below the support level, re-establishing coercive labor conditions (workers cannot refuse low wages without losing the subsidy, cannot exit without losing the income support). The founding problem's status is CONTESTED: administrators attest it is solved (workers no longer face subsistence risk); workers and advocates attest it persists in a new form (coercive low-wage labor with internalized acceptance of subsistence wages). The constraint's classification as tangled_rope (coordination + extraction) reflects this contest: the program does coordinate (solves precarity) but the extraction mechanism (wage suppression) has grown over time and is actively maintained through administrative choices (not indexing support to wages, not imposing employer compliance). A mandatrophy resolution would require the program to either (a) index the support level to market wage floors so employers cannot suppress wages below it, or (b) impose employer compliance requirements, or (c) make the support conditional on adequate wage rates. Each option would destroy the current constraint and create a new one (either pure rope [coordination only] or a modified tangled_rope with enforced symmetric benefit sharing).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_suppression_mechanism_empirical,
    'Do employers explicitly coordinate on wage suppression below the income support level, or does suppression emerge as an equilibrium response to worker bargaining power reduction?',
    'Labor market research: wage data before and after income support increases; employer survey data on wage-setting practices; econometric analysis of wage elasticity to income support changes; case studies of sectoral wage floors.',
    'If explicit coordination, the constraint is more snare-like (coordinated coercion); if equilibrium, the extraction is structurally embedded but less actively enforced. Either case the extraction persists, but the suppression mechanism (structural vs. coordinated) affects the enforcement requirement measurement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_suppression_mechanism_empirical, empirical, 'Whether wage suppression is an active coordinated choice or an emergent equilibrium outcome.').

omega_variable(
    identity_lock_dissolution_trajectory,
    'If workers gain access to alternative income sources or higher-wage employment, do they exit low-wage dependence, or does the identity-lock (self-conception as low-wage, skill identity fused to low-wage work, social isolation from mobility pathways) persist after exit from the constraint?',
    'Post-exit survey and longitudinal analysis of workers who gain income support while exiting low-wage work; measurement of subsequent wage trajectories, skill acquisition, and labor market participation.',
    'If identity-lock persists post-exit, the measured suppression understates the constraint''s true coercive force — the target carries the lock outward. If dissolves, the constraint''s suppression is primarily structural/situational, not internalized. This informs whether the constraint is better understood as snare (internalized) or tangled_rope (structural extraction only).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_dissolution_trajectory, empirical, 'Whether the identity-lock mechanism persists after exit from the low-wage labor condition.').

omega_variable(
    kernel_reading_contest,
    'Is unconditional income support fundamentally a wage-suppression mechanism, a freedom floor enabling labor choice, or a dependency trap that atrophies work incentives? The constraint''s classification depends entirely on which reading the policy''s operation embodies.',
    'Kernel-level analysis: (1) Do workers report experiencing the income support as enabling labor choice or as locking them into acceptance of low wages? (2) Do labor market outcomes (wage trends, skill acquisition, employment rates, worker-initiated transitions) align with freedom-floor or wage-subsidy narratives? (3) What counterfactual institutional designs are feasible (e.g., income-support indexed to wage floors, conditional on employer compliance) and how do they affect worker and employer behavior?',
    'If the wage-subsidy reading is the empirically dominant structural outcome, the constraint type is tangled_rope (extraction institutionalized through apparently coordinating policy). If the freedom-floor reading dominates outcomes, the constraint would reclassify to rope (genuine coordination with asymmetric benefit distribution). If the dependency-trap reading dominates, classification would depend on whether the dependency is structural or internalized.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Which kernel reading (wage-subsidy, freedom-floor, or dependency-trap) best describes the actual structural outcome of unconditional income support.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.52) structural (external barriers to wage negotiation, employer coordination, labor market reservation behavior) or internalized (workers internalize the subsistence floor as a just or natural wage, believe themselves unworthy of higher wages, or have normalized low-wage acceptance)?',
    'Qualitative research on worker narrative and expectation formation; analysis of wage-setting conversations; measurement of worker reservation wages relative to actual wages; post-policy-change trajectory of worker expectations.',
    'If suppression is primarily structural, removal of the policy or indexed wage-floor reforms could restore worker bargaining power. If primarily internalized, the same structural reforms would leave workers psychologically trapped in low-wage acceptance even after exit. The constraint''s true extractive force is higher if internalized because the target carries suppression with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is structural (external, policy-removable) or internalized (psychological, persistence-resistant).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(income_support_conditionality__wage_subsidy_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(inco_tr_t0, income_support_conditionality__wage_subsidy_reading, theater_ratio, 0, 0.32).
narrative_ontology:measurement_basis(inco_tr_t0, observed).
narrative_ontology:measurement(inco_tr_t3, income_support_conditionality__wage_subsidy_reading, theater_ratio, 3, 0.38).
narrative_ontology:measurement_basis(inco_tr_t3, observed).
narrative_ontology:measurement(inco_tr_t6, income_support_conditionality__wage_subsidy_reading, theater_ratio, 6, 0.42).
narrative_ontology:measurement_basis(inco_tr_t6, observed).
narrative_ontology:measurement(inco_tr_t9, income_support_conditionality__wage_subsidy_reading, theater_ratio, 9, 0.45).
narrative_ontology:measurement_basis(inco_tr_t9, observed).
narrative_ontology:measurement(inco_tr_t12, income_support_conditionality__wage_subsidy_reading, theater_ratio, 12, 0.47).
narrative_ontology:measurement_basis(inco_tr_t12, observed).
narrative_ontology:measurement(inco_tr_t15, income_support_conditionality__wage_subsidy_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement_basis(inco_tr_t15, observed).
narrative_ontology:measurement(inco_tr_t20, income_support_conditionality__wage_subsidy_reading, theater_ratio, 20, 0.48).
narrative_ontology:measurement_basis(inco_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(inco_be_t0, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(inco_be_t0, observed).
narrative_ontology:measurement(inco_be_t3, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 3, 0.54).
narrative_ontology:measurement_basis(inco_be_t3, observed).
narrative_ontology:measurement(inco_be_t6, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 6, 0.59).
narrative_ontology:measurement_basis(inco_be_t6, observed).
narrative_ontology:measurement(inco_be_t9, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 9, 0.63).
narrative_ontology:measurement_basis(inco_be_t9, observed).
narrative_ontology:measurement(inco_be_t12, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 12, 0.66).
narrative_ontology:measurement_basis(inco_be_t12, observed).
narrative_ontology:measurement(inco_be_t15, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 15, 0.67).
narrative_ontology:measurement_basis(inco_be_t15, observed).
narrative_ontology:measurement(inco_be_t20, income_support_conditionality__wage_subsidy_reading, base_extractiveness, 20, 0.68).
narrative_ontology:measurement_basis(inco_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(inco_su_t0, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 0, 0.41).
narrative_ontology:measurement_basis(inco_su_t0, observed).
narrative_ontology:measurement(inco_su_t3, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 3, 0.45).
narrative_ontology:measurement_basis(inco_su_t3, observed).
narrative_ontology:measurement(inco_su_t6, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement_basis(inco_su_t6, observed).
narrative_ontology:measurement(inco_su_t9, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 9, 0.5).
narrative_ontology:measurement_basis(inco_su_t9, observed).
narrative_ontology:measurement(inco_su_t12, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement_basis(inco_su_t12, observed).
narrative_ontology:measurement(inco_su_t15, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 15, 0.52).
narrative_ontology:measurement_basis(inco_su_t15, observed).
narrative_ontology:measurement(inco_su_t20, income_support_conditionality__wage_subsidy_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement_basis(inco_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(income_support_conditionality__wage_subsidy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(income_support_conditionality__wage_subsidy_reading, 0.18).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__freedom_floor_reading).
narrative_ontology:affects_constraint(income_support_conditionality__wage_subsidy_reading, income_support_conditionality__dependency_trap_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the income_support_conditionality kernel. The three readings decompose the contested claim 'unconditional income support' into three structurally distinct constraints: (1) wage_subsidy_reading instantiates the empirical claim that the policy functions as wage suppression (tangled_rope, this file); (2) freedom_floor_reading instantiates the normative claim that the policy decommodifies labor and enables worker choice (expected rope); (3) dependency_trap_reading instantiates the empirical claim that the policy undermines work incentives (expected snare). The three readings share a referent (the unconditional income support policy) but have different epsilon values (different assessments of what the policy extracts or enables), different beneficiary/victim structures (who benefits, who bears costs), and different classifications. The three constraints are linked via network.affects_constraints so the corpus analysis tool can identify them as a family and track how readings' epsilon values, beneficiary/victim sets, and classifications diverge based on the evidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
