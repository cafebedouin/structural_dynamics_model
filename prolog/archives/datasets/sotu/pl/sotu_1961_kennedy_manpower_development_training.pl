% ============================================================================
% CONSTRAINT STORY: sotu_1961_kennedy_manpower_development_training
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1961_kennedy_manpower_development_training, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sotu_1961_kennedy_manpower_development_training
 *   human_readable: Manpower Development and Training Program (Kennedy, 1961)
 *   domain: labor/economic_policy
 *
 * SUMMARY:
 *   The Manpower Development and Training Act (1961) represents a federal
 *   mechanism for redistributing the adjustment costs of technological
 *   displacement from individual workers and local communities onto the
 *   national tax base. This constraint exhibits a Tangled Rope structure: it
 *   possesses a genuine coordination function (enabling workers displaced by
 *   automation to acquire new skills that match evolving labor market
 *   demands) while simultaneously generating asymmetric extraction
 *   (concentrating benefits on displaced workers and their communities while
 *   diffusing costs across the federal taxpayer base and alternative social
 *   programs). The program's four-year initial authorization creates explicit
 *   sunset logic, supporting Scaffold classification from organized labor
 *   perspective — the initiative is intended as temporary crisis response to
 *   technological unemployment, not permanent income support. However,
 *   subsequent institutional development reveals Piton dynamics: training
 *   delivery becomes increasingly theatrical as program persistence relies on
 *   political constituency rather than demonstrated labor market outcomes.
 *   The analytical observer risks naturalizing this contingent institutional
 *   arrangement as an immutable law: 'technological displacement requires
 *   federal retraining.' This perspective is flagged as a false summit — the
 *   distribution of adjustment costs is a political choice, not a physical
 *   constraint.
 *
 * KEY AGENTS:
 *   - Displaced factory workers: Primary beneficiary (powerless/trapped) — receive federally funded retraining matching their labor market displacement; structurally unable to self-finance retraining or relocate without federal support
 *   - Industrial communities: Secondary beneficiary (moderate/constrained) — experience regional stabilization through retained workforce skills and reduced social instability; constrained by capacity to diversify economy independently
 *   - Labor union movement: Organized stakeholder (organized/constrained) — supports program as temporary coordination mechanism; uses retraining as platform for negotiating technological change management and job security
 *   - Federal taxpayers: Primary cost-bearer (moderate/constrained) — finance program through federal budget; constrained by political difficulty of reducing social spending but can theoretically reallocate resources
 *   - Competing social programs: Indirect victim (institutional/constrained) — experience budget pressure from manpower development resources; cannot exit federal budget constraint
 *   - Federal government: Institutional beneficiary (institutional/arbitrage) — achieves labor market coordination and social stability; has high exit optionality (can reduce scope, shift to tax incentives, eliminate program)
 *   - Training program apparatus: Institutional actor (institutional/arbitrage) — develops staff, infrastructure, and constituency that creates path-dependent persistence; tends toward Piton degradation over time
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1961_kennedy_manpower_development_training, 0.28).
domain_priors:suppression_score(sotu_1961_kennedy_manpower_development_training, 0.35).
domain_priors:theater_ratio(sotu_1961_kennedy_manpower_development_training, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1961_kennedy_manpower_development_training, extractiveness, 0.28).
narrative_ontology:constraint_metric(sotu_1961_kennedy_manpower_development_training, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sotu_1961_kennedy_manpower_development_training, theater_ratio, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1961_kennedy_manpower_development_training, tangled_rope).
narrative_ontology:human_readable(sotu_1961_kennedy_manpower_development_training, "Manpower Development and Training Program (Kennedy, 1961)").
narrative_ontology:topic_domain(sotu_1961_kennedy_manpower_development_training, "labor/economic_policy").

domain_priors:requires_active_enforcement(sotu_1961_kennedy_manpower_development_training).
narrative_ontology:has_sunset_clause(sotu_1961_kennedy_manpower_development_training).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1961_kennedy_manpower_development_training, displaced_workers).
narrative_ontology:constraint_beneficiary(sotu_1961_kennedy_manpower_development_training, affected_communities).
narrative_ontology:constraint_beneficiary(sotu_1961_kennedy_manpower_development_training, industrial_regions).
narrative_ontology:constraint_victim(sotu_1961_kennedy_manpower_development_training, federal_taxpayers).
narrative_ontology:constraint_victim(sotu_1961_kennedy_manpower_development_training, alternate_social_programs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED FACTORY WORKER (ROPE) — Sees retraining as genuine coordination solution to technological displacement. The constraint solves their core problem: matching skills to new labor market demands. Exit options are trapped (cannot relocate, retrain alone, or absorb opportunity cost), but the constraint provides a real path forward. Classification is rope because the beneficiary perceives genuine coordination function alongside resource transfer, not extraction.
constraint_indexing:constraint_classification(sotu_1961_kennedy_manpower_development_training, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INDUSTRIAL COMMUNITY (TANGLED ROPE) — Experiences both coordination (retraining restabilizes local labor markets) and asymmetric extraction (communities with political power capture more program resources; those without may be underfunded). Communities can theoretically exit (population migration, economic diversification) but face high relocation and transition costs. The constraint redistributes adjustment burden but does not do so uniformly — extraction component depends on political access to federal resources.
constraint_indexing:constraint_classification(sotu_1961_kennedy_manpower_development_training, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LABOR UNION MOVEMENT (SCAFFOLD) — Organized labor sees the program as a temporary coordination mechanism with explicit sunset: retraining buys time for workers to transition while unions negotiate technological change management (job security clauses, wage protection). The constraint is empowering for organized actors (exit through collective bargaining) but dependent on political will. Theater ratio is moderate (some performative job-training theater, but genuine skill transfer occurs). Union perspective supports the sunset classification — the program is intended as transitional, not permanent protection.
constraint_indexing:constraint_classification(sotu_1961_kennedy_manpower_development_training, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: COMPETING SOCIAL PROGRAMS (SNARE) — Education, healthcare, infrastructure, and anti-poverty programs competing for federal dollars experience the constraint as pure extraction: manpower development draws resources from their budgets. These programs cannot exit the federal budget constraint. Exit options are constrained (political advocacy can reallocate funds, but at high institutional cost). The constraint concentrates extraction on programs with less political constituency than labor unions.
constraint_indexing:constraint_classification(sotu_1961_kennedy_manpower_development_training, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL GOVERNMENT / TAXPAYERS (TANGLED ROPE) — The federal budget experiences this as a mixed arrangement: genuine coordination function (preventing social instability from mass displacement, stabilizing labor supply for new industries) alongside asymmetric costs (federal outlays, opportunity cost). Institutional actors have high exit optionality (reduce program scope, shift to tax incentives for private retraining, eliminate program) but program persistence reflects that the coordination benefit is real. The constraint's extractiveness appears moderate from this perspective because genuine public goods (labor market stability, regional economic continuity) are being produced alongside resource transfer.
constraint_indexing:constraint_classification(sotu_1961_kennedy_manpower_development_training, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: TRAINING PROGRAM APPARATUS (PITON) — From a civilizational perspective, the actual training delivery becomes increasingly theatrical over time: vocational programs may fail to match evolving skill demands, training timing lags actual displacement, completion rates vary widely by region, and the program persists partly through institutional inertia (staff, infrastructure, constituency) rather than demonstrated effectiveness. Theater ratio reflects that program legitimacy rests on the aspiration of retraining rather than on consistent labor market outcomes.
constraint_indexing:constraint_classification(sotu_1961_kennedy_manpower_development_training, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a universal perspective, technological displacement creates an inexorable mismatch between labor skills and market demands that must be resolved somehow — either workers retrain, move, or face chronic unemployment. Some mechanism of adjustment is inevitable and immutable. The constraint can appear as a natural necessity: 'Technological change requires worker retraining.' However, structural data contradicts this naturalization — the actual constraint is a particular federal program with specific beneficiaries and cost distributions, not an immutable law. The engine will classify this as a false summit.
constraint_indexing:constraint_classification(sotu_1961_kennedy_manpower_development_training, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1961_kennedy_manpower_development_training_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1961_kennedy_manpower_development_training, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1961_kennedy_manpower_development_training, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1961_kennedy_manpower_development_training, TR),
    TR >= 0.70.

:- end_tests(sotu_1961_kennedy_manpower_development_training_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.28): Moderate-low, increasing over interval. Initial extractiveness reflects the genuine coordination benefit: displaced workers do need skills retraining, and the federal investment solves a real market failure (workers cannot finance their own retraining while unemployed, and private training markets undersupply for low-income displaced cohorts). However, extractiveness increases over the four-year interval as: (1) substitution effects emerge — firms reduce private training investment knowing federal programs will train workers; (2) political capture begins — regions with higher political power capture more resources, creating asymmetric distribution; (3) program scope expands beyond core displacement problem to create general training provision, diffusing the coordination function. Suppression (0.35): Moderate. Barriers to exiting the constraint include state unemployment (trapped option), lack of family resources for private retraining (constrained option), and political difficulty of labor organization opposing the constraint (constrained option for collective action). However, suppression is not severe because: displacement is publicly acknowledged, federal support is visible and framed as assistance (not coercion), and beneficiary groups have political voice. Theater ratio (0.42): Moderate and rising. Early program delivery focuses on genuine skill matching (low theater). Over the interval, training programs become increasingly performative as: (1) training curricula lag actual skill demands in rapidly changing industries; (2) completion metrics become outputs (people trained) rather than outcomes (people employed in matching roles); (3) program legitimacy increasingly depends on narrative of 'retraining the displaced' rather than demonstrated labor market outcomes.
 *
 * PERSPECTIVAL GAP:
 *   Strongest gap between: (1) Displaced worker (Rope) — sees coordination solution — versus Federal taxpayer (Tangled Rope or Snare) — sees redistribution burden; (2) Industrial community (Tangled Rope) — sees regional stabilization — versus Training apparatus (Piton) — sees declining functional validity; (3) Labor union (Scaffold) — sees temporary negotiating platform with sunset — versus Institutional government (Tangled Rope) — sees ongoing social coordination. These gaps reveal that the constraint's classification depends entirely on whether the observer is a structural beneficiary or cost-bearer. The analytical observer risks collapsing this gap by naturalizing the arrangement as immutable — 'technological displacement requires retraining' — which misses that alternative mechanisms exist (private retraining markets, wage insurance, migration support, acceptance of sectoral unemployment). The false summit perspective is diagnostic: it reveals the political choice embedded in federalizing adjustment cost.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation chains differ sharply by agent. DISPLACED WORKERS: identified as beneficiaries + trapped exit → d ≈ 0.05 (full beneficiary) → low/negative f(d) → effectively negative χ. The constraint subsidizes them; they experience it as coordination, not extraction. FEDERAL TAXPAYERS: identified as cost-bearers + constrained exit → d ≈ 0.65 → f(d) ≈ 1.00 → moderate χ. They fund the program but cannot easily exit via tax reduction. FEDERAL GOVERNMENT: institutional actor + arbitrage exit → d ≈ 0.25 → f(d) ≈ 0.02 → very low χ. High exit optionality means the constraint is voluntary for the federal actor. INDUSTRIAL COMMUNITIES: mixed beneficiary-cost status + constrained exit → d ≈ 0.50 → f(d) ≈ 0.65 → moderate χ. Genuine coordination benefit but uneven distribution. COMPETING PROGRAMS: identified as victims + constrained exit → d ≈ 0.70 → f(d) ≈ 1.15 → high χ. These programs experience the constraint as zero-sum competition for federal resources. No directionality overrides are required — the canonical derivation correctly captures the asymmetric extraction pattern.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED TANGLED ROPE: The mandatrophy is resolved by recognizing that the constraint is genuinely hybrid — it simultaneously coordinates (matching displaced workers to new skills, stabilizing labor markets) and extracts (redistributing adjustment costs from workers/firms to taxpayers, creating fiscal concentration in federal budget). The mandatrophy is not 'which type is correct?' but 'how much coordination versus how much extraction?' At the analytical level, the decomposition is clear: beneficiaries (displaced workers, affected communities) experience the constraint as coordination; cost-bearers (taxpayers, competing programs) experience it as asymmetric distribution. The four-year sunset clause provides the bridge logic: Scaffold classification from organized labor perspective is legitimate because the program is structurally temporary — union support depends on it being transitional, not permanent safety net. If the sunset is enforced (program terminates after four years), the constraint is a genuine Scaffold. If the sunset is bypassed through renewal (which historical record shows occurred), the constraint degrades toward Piton (theater persists through institutional inertia). The false summit signature fires on the analytical mountain perspective, revealing that naturalizing this as 'immutable technological necessity' misses the political choice embedded in cost distribution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    automation_threshold_definition,
    'What degree of technological displacement justifies federal retraining subsidy versus market-driven private retraining?',
    'Empirical analysis of displacement rates before/after automation adoption; correlation between federal program scope and actual labor market outcomes; comparison with private sector retraining investment in same period',
    'If threshold is low: many displaced workers qualify, extracting from federal budget for private adjustment cost. If threshold is high: workers face private cost burden, but federal resources concentrate on most severe cases. Classification shifts between tangled_rope (generous redistribution) and snare (restrictive extraction) depending on threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_threshold_definition, empirical, 'Threshold for federal retraining subsidy justification').

omega_variable(
    program_effectiveness_measurement,
    'Does retraining actually restore displaced workers to equivalent wage/employment status, or primarily provide survival-level income during transition?',
    'Longitudinal tracking of retrainees: pre-displacement wages vs. post-retraining wages; employment duration and stability; comparison with control groups of non-retrainees in same displacement cohorts',
    'If effective (restore to 80%+ equivalent status): constraint is genuine coordination (Rope/Tangled Rope). If marginally effective (survival level only): constraint is partial extraction (Snare/Scaffold). If ineffective: program is theater maintained by institutional inertia (Piton).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(program_effectiveness_measurement, empirical, 'Whether retraining restores wage equivalence or provides survival support').

omega_variable(
    federal_versus_market_distribution,
    'Does federal retraining actually subsidize worker adjustment, or does it primarily redistribute costs away from high-productivity workers and firms toward taxpayers?',
    'Cost-benefit analysis disaggregated by: displacement type (technological vs. trade vs. cyclical); firm productivity gains from displacement; worker cohort wage changes; taxpayer burden distribution',
    'If subsidizes adjustment: constraint is Tangled Rope (genuine coordination + resource transfer). If redistributes to high-productivity agents: constraint is Snare (extraction from taxpayers). If asymmetric by political power: constraint is Tangled Rope with significant extraction component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(federal_versus_market_distribution, empirical, 'Whether federal retraining subsidizes adjustment or redistributes to high-productivity agents').

omega_variable(
    sunset_clause_enforceability,
    'Is the program''s four-year sunset clause enforced, or does institutional constituency prevent termination?',
    'Historical tracking: does program terminate at end of four years, or get renewed/extended? If renewed, what justification? Analysis of constituency formation (training provider networks, union support, political backing).',
    'If enforced: constraint is genuine Scaffold with operational sunset. If renewed repeatedly: constraint degrades to Piton (theater persists through inertia). Classification outcome depends entirely on whether the termination mechanism is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sunset_clause_enforceability, empirical, 'Whether four-year sunset clause is enforced or bypassed by renewal').

omega_variable(
    natural_law_naturalization,
    'Is the mountain perspective''s claim (technological displacement requires retraining) actually an immutable law, or a contingent institutional arrangement?',
    'Counterfactual analysis: in societies without federal retraining programs, how is displacement adjustment distributed? Do alternative mechanisms (private retraining, wage insurance, migration, or acceptance of permanent underemployment) produce comparable outcomes? Is retraining the unique solution or one among several possible mechanisms?',
    'If immutable: mountain classification stands. If contingent: mountain is a false summit — the constraint is not a law of nature but a political choice to socialize adjustment costs.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_naturalization, conceptual, 'Whether technological displacement retraining is natural law or contingent arrangement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1961_kennedy_manpower_development_training, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mdt_tr_t0, sotu_1961_kennedy_manpower_development_training, theater_ratio, 0, 0.3).
narrative_ontology:measurement(mdt_tr_t2, sotu_1961_kennedy_manpower_development_training, theater_ratio, 2, 0.36).
narrative_ontology:measurement(mdt_tr_t4, sotu_1961_kennedy_manpower_development_training, theater_ratio, 4, 0.42).

% Extraction over time
narrative_ontology:measurement(mdt_be_t0, sotu_1961_kennedy_manpower_development_training, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(mdt_be_t2, sotu_1961_kennedy_manpower_development_training, base_extractiveness, 2, 0.22).
narrative_ontology:measurement(mdt_be_t4, sotu_1961_kennedy_manpower_development_training, base_extractiveness, 4, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1961_kennedy_manpower_development_training, resource_allocation).
narrative_ontology:affects_constraint(sotu_1961_kennedy_manpower_development_training, trade_adjustment_assistance).
narrative_ontology:affects_constraint(sotu_1961_kennedy_manpower_development_training, sectoral_wage_insurance).
narrative_ontology:affects_constraint(sotu_1961_kennedy_manpower_development_training, regional_economic_development).

% DUAL FORMULATION NOTE:
% The manpower development program is upstream of trade adjustment mechanisms and wage insurance schemes. The program's design choices (federal vs. private funding, universal vs. targeted scope, four-year sunset vs. permanent authorization) influence whether downstream constraints redistribute adjustment costs (Tangled Rope family) or concentrate extraction (Snare family). This story models the 1961 Kennedy program specifically; subsequent reauthorizations and program expansions would constitute separate constraint stories with potentially different ε values reflecting changing cost distributions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
