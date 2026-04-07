% ============================================================================
% CONSTRAINT STORY: labor_market_polarization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_market_polarization, []).

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
 *   constraint_id: labor_market_polarization
 *   human_readable: Labor Market Polarization: Skill-Biased Technological Change and Wage Bifurcation
 *   domain: economic/labor_markets/technological_displacement
 *
 * SUMMARY:
 *   Labor market polarization — the simultaneous growth of high-skill,
 *   high-wage employment and low-skill, low-wage employment with collapse of
 *   middle-skill jobs — represents a structural coordination-extraction
 *   hybrid. Technological change (automation, digitalization) eliminates
 *   routine cognitive and manual work, increasing demand for adaptability and
 *   education while creating abundance of remaining low-skill positions. The
 *   constraint exhibits tangled rope structure: it coordinates high-skill
 *   workers to valuable opportunities (coordination function) while
 *   extracting from and trapping displaced workers through wage suppression,
 *   geographic immobility, and educational barriers (extraction function).
 *   The constraint is maintained through active enforcement (education and
 *   benefit policy), not spontaneously. Different institutional actors
 *   experience polarization as fulfilling genuinely different functions:
 *   capital owners solve their optimization problem (automate routine work,
 *   access global high-skill labor); high-skill professionals access premium
 *   opportunities; policy actors attempt to manage displacement through
 *   education; displaced workers face structural trapping. The theater ratio
 *   (0.48) reflects that policy discourse emphasizes skill-mismatch and
 *   individual reskilling responsibility while obscuring structural shifts in
 *   labor demand and capital-labor substitution elasticity.
 *
 * KEY AGENTS:
 *   - Displaced manufacturing and routine workers: Primary victims (powerless/trapped) — face wage collapse, skill obsolescence, geographic immobility, and family obligations preventing relocation or retraining
 *   - Routine cognitive workers (administrative, clerical, routine service): Secondary victims (moderate/constrained) — structurally mobile but cost of exit (retraining, wage loss during transition) is prohibitive; also benefit from remaining scarce routine positions
 *   - High-skill professionals and technical workers: Primary beneficiaries (institutional/arbitrage) — access expanding opportunities, wage premiums from scarcity, global mobility options, network effects
 *   - Capital owners and automation companies: Secondary beneficiaries (institutional/arbitrage) — benefit from cost reduction (automation of routine labor) and ability to source global high-skill labor at productivity-adjusted wages
 *   - Educational institutions and reskilling programs: Policy implementers (organized/constrained) — attempt to bridge gap through temporary intervention (scaffold), but face underfunding, structural education debt barriers, and skill supply lags
 *   - Labor unions and worker coalitions: Organized victims (organized/constrained) — seek to maintain solidarity and wage standards across polarizing skill tiers; constrained by membership fragmentation and capital mobility
 *   - Labor economics discipline: Institutional actor (powerful/mobile) — maintains piton-class consensus theory (skill-biased technological change equilibrates through wage adjustment) despite empirical falsification; theory persistence driven by textbook inertia and policy justification demand
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_market_polarization, 0.58).
domain_priors:suppression_score(labor_market_polarization, 0.65).
domain_priors:theater_ratio(labor_market_polarization, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_market_polarization, extractiveness, 0.58).
narrative_ontology:constraint_metric(labor_market_polarization, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(labor_market_polarization, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_market_polarization, tangled_rope).
narrative_ontology:human_readable(labor_market_polarization, "Labor Market Polarization: Skill-Biased Technological Change and Wage Bifurcation").
narrative_ontology:topic_domain(labor_market_polarization, "economic/labor_markets/technological_displacement").

domain_priors:requires_active_enforcement(labor_market_polarization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_market_polarization, high_skill_workers).
narrative_ontology:constraint_beneficiary(labor_market_polarization, capital_owners).
narrative_ontology:constraint_beneficiary(labor_market_polarization, automation_companies).
narrative_ontology:constraint_victim(labor_market_polarization, routine_task_workers).
narrative_ontology:constraint_victim(labor_market_polarization, middle_skill_workers).
narrative_ontology:constraint_victim(labor_market_polarization, displaced_manufacturing_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED MANUFACTURING WORKER (SNARE) — Trapped by geographic immobility, family obligations, skill obsolescence, and wage floors below survival. No meaningful exit options. Bears full extraction cost through wage collapse and benefit erosion. Suppression is structural: retraining programs are underfunded, relocation costs are prohibitive, and alternative employment is unavailable at prior wage levels.
constraint_indexing:constraint_classification(labor_market_polarization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ROUTINE COGNITIVE WORKER (TANGLED ROPE) — Constrained by education debt, family stability concerns, and competitive pressure from both automation and high-skill workers. Experiences the labor market as both coordinating their skills to available roles AND extracting premiums from their scarcity as middle-skill work disappears. Some agency through skill upgrading, but at high cost and risk. Benefits from remaining routine positions but loses them to automation over time.
constraint_indexing:constraint_classification(labor_market_polarization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: HIGH-SKILL PROFESSIONAL NETWORK (ROPE) — Institutional actor with arbitrage options (mobility across firms, countries, sectors). Experiences polarization as pure coordination: their scarcity is valuable precisely because routine work is automated away. Benefits from wage premium, network effects, and portfolio opportunities. The constraint solves their coordination problem: matching elite talent to high-value work.
constraint_indexing:constraint_classification(labor_market_polarization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CAPITAL OWNERS AND AUTOMATION COMPANIES (ROPE) — Institutional beneficiaries with maximum arbitrage (capital mobility, exit to profitable automation). Polarization is their coordination solution: automating routine work and sourcing high-skill labor creates value capture opportunities. Labor market restructuring is their successful strategy execution, not a constraint they endure.
constraint_indexing:constraint_classification(labor_market_polarization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR UNIONS AND WORKER COALITIONS (TANGLED ROPE) — Organized agents with constrained exit (membership base is geographically dispersed, skill-heterogeneous, and fragmented). Experience polarization as both a coordination challenge (maintaining solidarity across skill tiers) and extraction mechanism (inability to negotiate wage floors across the bifurcated market). Some collective power but insufficient to reverse structural trends.
constraint_indexing:constraint_classification(labor_market_polarization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SOCIAL POLICY INTERVENTIONS (SCAFFOLD) — Organized policy actors (labor departments, education systems, social safety nets) see polarization as a temporary coordination failure addressable through sunset-clause mechanisms: educational access expansion, wage floors, retraining subsidies, and UBI pilots. These interventions are temporary in design but persistent in practice due to political economy. The sunset is aspirational (when reskilling succeeds) but indefinitely delayed (when structural automation continues).
constraint_indexing:constraint_classification(labor_market_polarization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: NEOCLASSICAL LABOR ECONOMICS FRAMING (PITON) — The discipline's consensus theory (skill-biased technological change, equilibrating wage adjustments, perfect substitution between capital and labor) is substantially performative. Empirically, wage adjustments do not clear markets, workers do not perfectly substitute between skills, and technological change has proven persistently biased. The theory persists through institutional inertia (textbooks, academic hiring, policy justification) despite contradicting evidence. Theater ratio is high because the model's predictive failure is invisible within its own framework.
constraint_indexing:constraint_classification(labor_market_polarization, piton,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / TECHNOLOGY DETERMINISM (MOUNTAIN) — From a civilizational perspective, labor market polarization appears as an immutable consequence of technological progress: automation inevitably eliminates routine work, skills become scarce and valuable, and wage bifurcation follows mathematically. This perspective naturalizes what is contingent — the timing, intensity, and geographic distribution of polarization depend entirely on policy choices (education investment, labor regulation, capital mobility constraints). The mountain classification is a false summit revealing how technological determinism naturalizes policy failure.
constraint_indexing:constraint_classification(labor_market_polarization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_market_polarization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_market_polarization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_market_polarization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_market_polarization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_market_polarization, TR),
    TR >= 0.70.

:- end_tests(labor_market_polarization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts significantly from displaced workers (wage collapse, unemployment, benefit erosion) but less than maximum because (1) some workers successfully transition to adjacent positions, (2) high-skill workers experience wage gains that offset measured average, (3) capital owners extract through cost reduction but not through pure predation. The trend toward higher extractiveness (0.32 → 0.58 over 20 years) reflects ongoing automation acceleration and declining real wages for median workers. Suppression (0.65): High. Barriers to exit are substantial: geographic immobility (housing costs, family ties, regional economic collapse), educational barriers (cost, time, age discrimination against older retrainers), labor market segmentation (credential requirements, employer hiring discrimination based on prior employment gaps), and inadequate safety net (unemployment insurance duration, retraining subsidies far below actual retraining costs). Suppression is both structural (skill-biased technology requires genuine reskilling, not just reallocation) and policy-induced (underfunded education, weak labor organizing, capital mobility without corresponding worker mobility). Theater ratio (0.48): Moderate. Dominant framing (individual skill mismatch, reskilling responsibility on workers) obscures structural factors (technological bias in automation toward routine elimination, capital mobility without labor mobility, agglomeration economies creating geographic bifurcation). But the framing is not entirely performative — skill gaps are real, some reskilling succeeds, some workers do transition. Theater emerges from selective visibility: policy discourse highlights success stories and individual factors, suppresses systemic analysis of capital-labor substitution elasticity and policy choice.
 *
 * PERSPECTIVAL GAP:
 *   Displaced workers and capital owners have opposite directionality values on the same constraint — one experiences extraction, the other experiences subsidy. This is the defining feature of a tangled rope that functions as both coordination (for those it benefits) and extraction (for those it harms). The classification reflects that both experiences are real: the constraint genuinely solves capital's optimization problem (automate routine work) and genuinely harms workers who cannot adapt. No single perspective is 'the truth' — the truth is the presheaf of all perspectives showing the bifurcated extraction pattern.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values reflect structural position: displaced workers (beneficiary: none, victim: yes, exit: trapped) → d ≈ 0.95 → f(d) ≈ 1.42 → experienced χ high. Routine cognitive workers (beneficiary: partial, victim: yes, exit: constrained) → d ≈ 0.65 → f(d) ≈ 1.00 → experienced χ moderate-high. High-skill workers (beneficiary: yes, victim: no, exit: arbitrage) → d ≈ 0.15 → f(d) ≈ -0.01 → experienced χ near zero or negative (they experience wage gains, not extraction). Capital owners (beneficiary: yes, victim: no, exit: arbitrage) → d ≈ 0.05 → f(d) ≈ -0.12 → experienced χ negative (constraint subsidizes their returns). Scope modifier σ(national) = 1.0, so χ = ε × f(d) × 1.0. The constraint's effective extractiveness for displaced workers is higher than the base measure because of the directionality amplification (f(d) > 1.0 for trapped agents). For capital owners, the effective rate is negative (the constraint benefits them). This explains the perspectival gap: the same constraint that extracts χ ≈ 0.82 from displaced workers extracts χ ≈ -0.07 from capital owners.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in labor market polarization is resolved by recognizing that the constraint is NOT a snare falsely labeled coordination, nor coordination falsely labeled snare. It is genuinely both: a coordination mechanism for capital/high-skill workers (matching talent to opportunities, capital to profitable investments) that simultaneously functions as an extraction mechanism for displaced workers (forcing wage reductions, geographic immobility, skill obsolescence). The tangled rope classification captures this hybrid: 0.58 ≥ 0.46 (extraction gate), requires_active_enforcement: true (policy intervention necessary to maintain it), beneficiaries present (capital, high-skill workers), victims present (displaced workers), χ = 0.58 × f(d) × 1.0 ≈ 0.76 for victims but χ ≈ -0.07 for beneficiaries (asymmetric extraction). The resolution is not to collapse the perspectives into one but to recognize the constraint as a dual-function structure that legitimately classifies as tangled rope precisely because it coordinates some agents while extracting from others. Policy choice determines whether this remains stable (continued low education investment, capital mobility without labor mobility) or transitions to scaffold (high investment in education creating genuine mobility and reskilling) or snare (geographic bifurcation and credential stratification trap workers into permanent lower tier).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technological_inevitability_vs_policy_choice,
    'Is labor market polarization an inevitable consequence of automation technology or a policy-contingent outcome determined by educational access, labor regulation, and capital mobility rules?',
    'Cross-national comparative analysis: countries with similar technological exposure but different labor market policies (Nordic vs Anglo-American vs Asian models) show divergent polarization trajectories. Magnitude and timing of polarization correlate more strongly with policy variables than with technological advancement rates.',
    'If inevitable: mountain classification confirmed, policy interventions are futile, scaffold perspective is false. If policy-contingent: mountain is false summit, tangled_rope and scaffold classifications are structural, policy levers are real.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_inevitability_vs_policy_choice, empirical, 'Whether polarization is technologically inevitable or policy-contingent').

omega_variable(
    reskilling_program_effectiveness,
    'Do government and corporate reskilling programs actually transition displaced workers to high-skill positions at rates sufficient to prevent long-term wage loss and intergenerational mobility collapse?',
    'Longitudinal wage tracking of reskilled workers; comparison of post-program earnings to counterfactual (no program) group; intergenerational earnings mobility analysis for children of displaced workers across program-exposure and non-exposure cohorts.',
    'If effective: scaffold perspective validated, sunset mechanism is real, constraint can be managed through temporary intervention. If ineffective: scaffold is aspirational fiction, suppression is higher than measured (structural rather than addressable), snare classification dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reskilling_program_effectiveness, empirical, 'Whether reskilling programs achieve genuine wage recovery').

omega_variable(
    automation_substitution_elasticity,
    'Is the substitution between capital (automation) and low-skill labor elastic (cheap labor can compete with machines) or inelastic (machines displace labor irrespective of wage)?',
    'Econometric estimation of capital-labor substitution elasticity across industries and time periods; correlation between wage suppression and automation adoption rates; empirical test of whether wage floors reduce automation investment or accelerate it.',
    'If elastic: low-skill workers can compete through wage adjustment, polarization is temporary and recoverable through education. If inelastic: automation proceeds regardless of wages, suppression is structural and irreversible through wage mechanisms, snare classification is structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(automation_substitution_elasticity, empirical, 'Whether labor can compete with automation through wage adjustment').

omega_variable(
    high_skill_scarcity_duration,
    'Is the current high-skill premium a permanent feature of the polarized economy or a temporary scarcity rent that will erode as education expands and supply increases?',
    'Projection of STEM education supply against labor demand growth; historical analysis of previous occupational premiums and their persistence; comparative analysis of countries with different educational expansion rates.',
    'If permanent: high-skill workers'' rope classification is durable, coordination benefit is real and sustainable. If temporary: high-skill premium erodes, high-skill workers eventually compete with each other like low-skill workers now do, rope becomes tangled_rope or snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(high_skill_scarcity_duration, empirical, 'Whether high-skill wage premiums are durable or temporary scarcity rents').

omega_variable(
    suppression_mechanism_structural_vs_policy,
    'Is the measured suppression (0.65) structural to the labor market itself or policy-induced through underfunded education, weak labor organizing, and capital mobility without corresponding worker mobility?',
    'Decomposition analysis: compare suppression across policy regimes (strong unions + investment in education vs weak unions + education underinvestment); examine whether suppression remains high when policy variables are controlled/changed.',
    'If structural: suppression is inherent to skill-biased technology, policy interventions are marginal. If policy-induced: suppression can be reduced through deliberate policy design, constraint is more malleable than claimed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_policy, preference, 'Whether suppression is structural or policy-induced').

omega_variable(
    geographic_bifurcation_persistence,
    'Does polarization concentrate geographically (high-skill cities vs hollowed-out regions) in ways that create path dependence and make reversal increasingly difficult?',
    'Spatial econometric analysis of polarization clustering; network analysis of migration patterns and agglomeration economies; modeling of cumulative causation (skill concentration → better services → more migration → further concentration).',
    'If persistent spatial bifurcation emerges: suppression includes geographic lock-in, workers cannot exit even with education, snare classification is reinforced. If reversible: polarization remains more fluid, constrained classification is appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_bifurcation_persistence, empirical, 'Whether geographic bifurcation creates persistent path dependence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_market_polarization, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lmp_tr_t0, labor_market_polarization, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lmp_tr_t10, labor_market_polarization, theater_ratio, 10, 0.42).
narrative_ontology:measurement(lmp_tr_t20, labor_market_polarization, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(lmp_be_t0, labor_market_polarization, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(lmp_be_t10, labor_market_polarization, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(lmp_be_t20, labor_market_polarization, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_market_polarization, resource_allocation).
narrative_ontology:affects_constraint(labor_market_polarization, educational_credential_inflation).
narrative_ontology:affects_constraint(labor_market_polarization, geographic_economic_divergence).
narrative_ontology:affects_constraint(labor_market_polarization, union_decline_and_wage_suppression).

% DUAL FORMULATION NOTE:
% Labor market polarization decomposes into skill-specific constraints with different ε values. Global high-skill labor supply coordination (international STEM talent mobility) is ε ≈ 0.15 (rope). Routine job elimination (automation substitution) is ε ≈ 0.68 (snare for displaced workers). These are linked by network.affects_constraints — global talent mobility acceleration drives automation investment which accelerates routine job elimination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(labor_market_polarization, powerful, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
