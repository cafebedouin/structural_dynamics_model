% ============================================================================
% CONSTRAINT STORY: subsidy_capture_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_subsidy_capture_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: subsidy_capture_reading
 *   human_readable: Income Support as Employer Subsidy Enabling Wage Suppression
 *   domain: political_economy/labor_markets/social_policy
 *
 * SUMMARY:
 *   Income support policies (EITC, minimum income schemes, housing subsidies,
 *   child allowances) present a structural ambiguity: they simultaneously
 *   provide a safety net for low-wage workers AND enable employers to
 *   suppress wages below reproduction cost, transferring the subsidy benefit
 *   to capital holders as lower labor costs. This constraint exemplifies the
 *   'subsidy capture' reading of the income_support_commitment kernel — the
 *   institutional arrangement originally designed as a poverty-reduction
 *   mechanism has been reappropriated as an indirect employer subsidy,
 *   maintaining workers at subsistence levels while employers capture the
 *   productivity surplus. The measurement trajectory shows extractiveness
 *   rising from 0.35 to 0.58 over 15 years (representing a 30-year period of
 *   policy expansion), reflecting both the growing scale of income support
 *   programs and their increasingly tight coupling with wage suppression.
 *   Theater ratio rises from 0.35 to 0.55, indicating that the original
 *   poverty-reduction function is being displaced by the subsidy-transfer
 *   function — the policy increasingly operates as fiscal redistribution
 *   rather than genuine poverty elimination. The constraint's tangled_rope
 *   classification reflects that genuine coordination exists (income support
 *   prevents labor market collapse and maintains consumer demand) alongside
 *   severe extraction (capital holders capture the subsidy benefit while
 *   workers remain trapped at subsistence).
 *
 * KEY AGENTS:
 *   - Low-Wage Workers: Primary victim (powerless/trapped) — depend on income support to reach subsistence while employers suppress wages; maximum experienced extraction
 *   - Capital Holders: Primary beneficiary (institutional/arbitrage) — receive socialized wage floor enabling lower labor costs; zero exit costs
 *   - Low-Wage Employers: Secondary beneficiary (institutional/arbitrage) — can maintain low wages while workers remain solvent; arbitrage capacity to relocate or automate if wages rise
 *   - Taxpayers (Non-Beneficiary): Secondary victim (moderate/constrained) — bear fiscal cost of subsidy; experience wage suppression effects in their own labor markets
 *   - Labor Unions (Organized Workers): Mixed (organized/constrained) — benefit from prevention of labor market collapse; harmed by suppressed wage floors undercutting organizing
 *   - Welfare State Administration: Institutional actor (institutional/arbitrage) — maintains subsidy apparatus through inertia; original poverty-reduction intent degraded into wage-suppression mechanism
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing as inevitable what is contingent political-institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(subsidy_capture_reading, 0.58).
domain_priors:suppression_score(subsidy_capture_reading, 0.62).
domain_priors:theater_ratio(subsidy_capture_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(subsidy_capture_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(subsidy_capture_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(subsidy_capture_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(subsidy_capture_reading, tangled_rope).
narrative_ontology:human_readable(subsidy_capture_reading, "Income Support as Employer Subsidy Enabling Wage Suppression").
narrative_ontology:topic_domain(subsidy_capture_reading, "political_economy/labor_markets/social_policy").

domain_priors:requires_active_enforcement(subsidy_capture_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(subsidy_capture_reading, capital_holders).
narrative_ontology:constraint_beneficiary(subsidy_capture_reading, low_wage_employers).
narrative_ontology:constraint_victim(subsidy_capture_reading, public_fiscal_capacity).
narrative_ontology:constraint_victim(subsidy_capture_reading, low_wage_workers).
narrative_ontology:constraint_victim(subsidy_capture_reading, taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(subsidy_capture_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

constraint_indexing:constraint_classification(subsidy_capture_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(subsidy_capture_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

constraint_indexing:constraint_classification(subsidy_capture_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

constraint_indexing:constraint_classification(subsidy_capture_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(subsidy_capture_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

constraint_indexing:constraint_classification(subsidy_capture_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(subsidy_capture_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(subsidy_capture_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(subsidy_capture_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(subsidy_capture_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(subsidy_capture_reading, TR),
    TR >= 0.70.

:- end_tests(subsidy_capture_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint transfers substantial fiscal resources (public income support) to subsidize employer labor costs. The extraction is not total (0.70+) because genuine coordination exists — income support prevents labor market collapse and sustains consumer demand. The 0.58 value reflects that capital holders capture most of the subsidy benefit (low wages × high productivity = high surplus), while workers receive only subsistence-level support. Suppression (0.62): Moderate-high. Workers face multiple binding constraints: (a) economic necessity forces acceptance of subsidy-level wages, (b) weak bargaining position because income support is perceived as safety net rather than recognition of exploitation, (c) employer arbitrage capacity (relocation, automation) removes realistic threat of wage concession. Suppression is not total (0.80+) because some workers organize, some sectors resist wage suppression, and income support does provide exit from absolute destitution. Theater ratio (0.55): Moderate. The policy is presented as anti-poverty measure (theater), but increasingly operates as employer subsidy (function). Theater_ratio rises over the measurement interval from 0.35 to 0.55, indicating growing divergence between stated (poverty reduction) and actual (wage suppression) function. The constraint is not yet a piton (theater ≥ 0.70), but the trajectory indicates institutional capture is advancing.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates a stark perspectival divergence. Low-wage workers perceive a snare — they are trapped by economic necessity into accepting subsidy-level wages, with no exit option and maximum extraction. Low-wage employers perceive a rope — the income support enables them to solve the coordination problem (maintaining a solvent workforce at low cost). Capital holders perceive a rope — socialized wage floors maintain labor market stability while suppressing costs. Taxpayers perceive tangled rope — coordination benefit (stable labor market) exists but is outweighed by fiscal cost and wage suppression effects. Labor unions perceive tangled rope — mixed coordination (income support prevents labor market collapse) and extraction (suppressed wage floors undercut organizing). The welfare state apparatus perceives a piton — the original anti-poverty function is degraded, maintained through institutional inertia. The analytical observer risks perceiving a mountain — 'low wages and income support are natural features of competitive labor markets' — but this is a false summit: the arrangement is contingent on political choices (subsidy level, wage floor, tax structure), not immutable law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from structural position relative to the extraction flow. Low-wage workers as victims trapped at subsistence: d ≈ 0.95 (full target of extraction). Capital holders as beneficiaries with arbitrage: d ≈ 0.05 (full beneficiary, zero exit cost). Low-wage employers as beneficiaries with arbitrage: d ≈ 0.10 (primary beneficiary). Taxpayers as non-beneficiary victims with constrained exit (cannot avoid taxes without relocating): d ≈ 0.80 (high target, constrained exit). Labor unions as organized agents with constrained exit (can mobilize but face legal barriers): d ≈ 0.65 (moderate target, organized capacity). The piton perspective derives from the theater gate (0.55 ≥ 0.50) rather than from high chi — the welfare state apparatus is degraded (theater-driven) not maximally extractive. The mountain perspective at analytical/civilizational context derives from d ≈ 0.72 (observer baseline), but the false summit detector identifies it as naturalization of contingent arrangement.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing genuine coordination function (income support prevents labor market collapse) from extractive overlay (capital captures the subsidy as lower labor costs). The tangled rope classification reflects the coexistence: coordination is real (workers remain solvent, consumer demand is sustained, labor market stability is maintained); extraction is also real (capital holders capture surplus, wages remain suppressed, workers are trapped). The false summit detector flags the analytical/mountain perspective as naturalization of a contingent arrangement — the claim that 'some wage subsidy is inherent to labor markets' confuses structural relationship (income support exists) with natural law (this is how it must be). The distinction is empirically material: alternative policy designs (wage floors without subsidy reduction, employer-side taxes on low-wage labor, direct capital taxation) would break the subsidy capture mechanism while maintaining coordination function. The constraint's mandatrophy is not resolved in the sense of 'choosing one type' but in the sense of 'understanding why multiple types are simultaneously accurate from different positions.' The victim (powerless/trapped) genuinely experiences snare; the beneficiary (institutional/arbitrage) genuinely experiences rope; the organized agent (organized/constrained) genuinely experiences tangled rope. The framework's value is in showing that all three experiences are structurally real, not in declaring one 'correct.'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_subsidy_capture,
    'Which kernel reading of income support is this constraint instantiating?',
    'This constraint instantiates the SUBSIDY_CAPTURE reading of the income_support_commitment kernel. Sibling readings include: (1) freedom_floor_reading (income support as enabler of human dignity and exit from desperation) and (2) dependency_trap_reading (income support as state mechanism for controlling labor supply). Each reading produces different ε, beneficiary/victim structure, and classification.',
    'The subsidy_capture reading treats income support as extractive mechanism benefiting capital holders via suppressed wages. Alternative readings would classify differently: freedom_floor reading produces lower ε and different beneficiary set; dependency_trap reading has different victim focus (workers'' autonomy vs. fiscal capacity). The kernel is contested; each reading is structurally defensible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_subsidy_capture, conceptual, 'Kernel reading selection for income support commitment').

omega_variable(
    wage_suppression_causality,
    'Does income support causally suppress wages, or do low wages pre-exist independent of subsidies?',
    'Longitudinal analysis of wage trajectories in regions with / without income support expansion; comparison of labor market dynamics before and after subsidy policy changes; estimation of counterfactual wages absent subsidies using labor supply elasticity models.',
    'If causality is strong (subsidies cause suppression): snare and tangled rope perspectives validated; extractiveness remains at 0.58. If weak or inverse (subsidies prevent worse suppression): classification shifts toward rope/scaffold; beneficiary set contracts (capital holders less clear); extractiveness drops to 0.35-0.40.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_suppression_causality, empirical, 'Causal relationship between income support and wage suppression').

omega_variable(
    alternative_wage_equilibrium,
    'What wage level would prevail absent income support? Is it livable, or would labor supply collapse?',
    'Structural macroeconomic modeling with / without subsidy; historical comparison to pre-welfare-state labor markets; cross-national analysis of countries with / without income support policies.',
    'If alternative equilibrium is below subsistence: income support prevents worse outcome; extractiveness is shared (employers benefit but workers avoid destitution); classification softens to rope/tangled rope hybrid. If alternative is livable but lower: suppression is pure transfer to capital; extractiveness confirmed at high level; snare classification validated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_wage_equilibrium, conceptual, 'Counterfactual wage equilibrium without income support').

omega_variable(
    fiscal_incidence_redistribution,
    'Net: does income support redistribute toward workers or toward employers via suppressed wage expectations?',
    'Fiscal incidence analysis tracking full tax + benefit flows; decomposition of subsidy benefit by recipient (worker consumption, employer margin expansion, capital returns); Lorenz curve analysis of redistribution.',
    'If net redistribution is progressive: constraint is mixed coordination/extraction; extractiveness < 0.50; classification softens to rope. If net redistributive effect is neutral or regressive: constraint is pure employer subsidy; extractiveness confirmed; snare/tangled rope validated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fiscal_incidence_redistribution, empirical, 'Net fiscal incidence of income support subsidy').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is suppression structural (economic constraints prevent higher wages) or internalized (workers accept subsidy as normal)?',
    'Survey analysis of worker wage expectations; historical evolution of ''living wage'' norms; comparison of wage demands in low-subsidy vs. high-subsidy labor markets.',
    'If structural: suppression metric (0.62) reflects real barriers; constraint classification stable. If internalized: suppression persists even after subsidies are removed; constraint''s effective suppression is higher than the structural measure suggests (workers carry suppression internally).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Whether suppression is structural or internalized by workers').

omega_variable(
    employers_genuine_beneficiary_or_artifact,
    'Are employers primary beneficiaries, or is the subsidy incidental to their business model?',
    'Analysis of employer investment decisions with / without subsidy expansion; surveys of employer wage-setting logic; comparison of wage growth rates in subsidized vs. unsubsidized sectors.',
    'If employers actively benefit and incorporate subsidy into wage strategy: beneficiary set confirmed; tangled rope classification robust. If employers are passive recipients of subsidy effects: beneficiary set contracts to capital/finance only; extractiveness shifts focus from direct wage suppression to fiscal transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employers_genuine_beneficiary_or_artifact, empirical, 'Whether employers are active or passive beneficiaries of income support subsidy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(subsidy_capture_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(subsidy_tr_t0, subsidy_capture_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(subsidy_tr_t5, subsidy_capture_reading, theater_ratio, 5, 0.42).
narrative_ontology:measurement(subsidy_tr_t10, subsidy_capture_reading, theater_ratio, 10, 0.5).
narrative_ontology:measurement(subsidy_tr_t15, subsidy_capture_reading, theater_ratio, 15, 0.55).

% Extraction over time
narrative_ontology:measurement(subsidy_be_t0, subsidy_capture_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(subsidy_be_t5, subsidy_capture_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(subsidy_be_t10, subsidy_capture_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(subsidy_be_t15, subsidy_capture_reading, base_extractiveness, 15, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(subsidy_capture_reading, resource_allocation).
narrative_ontology:affects_constraint(subsidy_capture_reading, minimum_wage_equilibrium).
narrative_ontology:affects_constraint(subsidy_capture_reading, labor_market_monopsony).
narrative_ontology:affects_constraint(subsidy_capture_reading, fiscal_redistribution_structure).

% DUAL FORMULATION NOTE:
% Income support policies decompose into multiple structurally distinct constraints: (1) subsidy_capture_reading (this constraint) — extractive mechanism benefiting capital; (2) freedom_floor_reading — anti-poverty safety net; (3) dependency_trap_reading — state labor control. Each reading has different ε (0.58, 0.25, 0.50 respectively) and different beneficiary/victim structure. The three readings are interpretations of the same institutional kernel but instantiate different constraint types. This constraint links downstream to minimum wage equilibrium (floor-setting effects), labor market monopsony (wage suppression dynamics), and fiscal redistribution structure (tax/benefit incidence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(subsidy_capture_reading, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
