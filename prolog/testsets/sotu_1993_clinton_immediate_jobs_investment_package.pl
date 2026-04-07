% ============================================================================
% CONSTRAINT STORY: sotu_1993_clinton_immediate_jobs_investment_package
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1993_clinton_immediate_jobs_investment_package, []).

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
 *   constraint_id: sotu_1993_clinton_immediate_jobs_investment_package
 *   human_readable: $30 Billion Immediate Jobs Investment for Infrastructure and Youth Employment (1993)
 *   domain: fiscal_policy/economic_stimulus
 *
 * SUMMARY:
 *   The Clinton administration's $30 billion immediate jobs investment
 *   package (1993) exemplifies a Keynesian counter-cyclical stimulus
 *   constraint designed to solve the macroeconomic coordination problem of
 *   cyclical unemployment. The constraint allocates government spending
 *   across infrastructure renewal (highways, airports, housing) and youth
 *   summer employment programs targeting distressed urban and rural areas.
 *   Structurally, it coordinates aggregate demand through direct government
 *   purchase of labor, addressing the fallacy of composition: individual
 *   rational behavior (reducing consumption during downturns) produces
 *   collectively irrational outcomes (demand collapse, involuntary
 *   unemployment). The constraint exhibits rope characteristics from the
 *   perspective of beneficiaries (unemployed workers, contractors, youth in
 *   depressed regions) who experience genuine coordination benefit without
 *   extractive overhead. From the fiscal conservative perspective, it shows
 *   tangled_rope characteristics: the macroeconomic coordination function is
 *   real, but fiscal costs (deficit accumulation, intergenerational debt
 *   service) create asymmetric extraction. The long-term institutional view
 *   reveals piton characteristics: temporary programs often persist beyond
 *   their sunset logic through political pressure and bureaucratic inertia.
 *   The analytical observer risks naturalizing this constraint as an
 *   immutable macroeconomic law, but the structural data reveals it as a
 *   contingent policy choice (false summit risk).
 *
 * KEY AGENTS:
 *   - Cyclically unemployed workers: Primary beneficiary (powerless/trapped) — structurally unable to coordinate labor demand individually; stimulus coordinates solution
 *   - Youth in distressed regions: Primary beneficiary (moderate/constrained) — geographic and economic barriers create limited opportunity; program provides temporary access
 *   - Infrastructure contractors: Institutional beneficiary (institutional/arbitrage) — high exit optionality; sees constraint as pure coordination mechanism
 *   - Fiscal conservative coalition: Secondary victim (organized/constrained) — organized opposition recognizing both coordination logic and fiscal costs; constrained by macroeconomic consensus
 *   - Federal Reserve and economic policy advisors: Powerful agents (powerful/mobile) — full agency and exit path; see constraint as temporary scaffolding with genuine sunset
 *   - Future taxpayers: Intergenerational victim (analytical/trapped) — bear debt service costs; structured extraction if deficit not repaid within beneficiary cohort
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing policy choice as economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1993_clinton_immediate_jobs_investment_package, 0.22).
domain_priors:suppression_score(sotu_1993_clinton_immediate_jobs_investment_package, 0.35).
domain_priors:theater_ratio(sotu_1993_clinton_immediate_jobs_investment_package, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1993_clinton_immediate_jobs_investment_package, extractiveness, 0.22).
narrative_ontology:constraint_metric(sotu_1993_clinton_immediate_jobs_investment_package, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(sotu_1993_clinton_immediate_jobs_investment_package, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1993_clinton_immediate_jobs_investment_package, rope).
narrative_ontology:human_readable(sotu_1993_clinton_immediate_jobs_investment_package, "$30 Billion Immediate Jobs Investment for Infrastructure and Youth Employment (1993)").
narrative_ontology:topic_domain(sotu_1993_clinton_immediate_jobs_investment_package, "fiscal_policy/economic_stimulus").

narrative_ontology:has_sunset_clause(sotu_1993_clinton_immediate_jobs_investment_package).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1993_clinton_immediate_jobs_investment_package, unemployed_workers).
narrative_ontology:constraint_beneficiary(sotu_1993_clinton_immediate_jobs_investment_package, distressed_urban_communities).
narrative_ontology:constraint_beneficiary(sotu_1993_clinton_immediate_jobs_investment_package, rural_economic_development_regions).
narrative_ontology:constraint_beneficiary(sotu_1993_clinton_immediate_jobs_investment_package, infrastructure_contractors).
narrative_ontology:constraint_beneficiary(sotu_1993_clinton_immediate_jobs_investment_package, youth_demographic_cohort).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CYCLICALLY UNEMPLOYED WORKER (ROPE) — Structurally trapped in jobless state by macroeconomic contraction. The constraint solves the collective action problem of aggregate demand deficiency — individual actors cannot coordinate to restore labor demand, but government spending coordinates the solution. Net beneficiary with genuine coordination function (not pure extraction). Suppression in the system (job shortage, geographic isolation, skill mismatch) is real but the constraint directly addresses it.
constraint_indexing:constraint_classification(sotu_1993_clinton_immediate_jobs_investment_package, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: YOUTH IN DISTRESSED REGION (ROPE) — Faces constraints: limited local job availability, geographic immobility (family ties, education roots), identity tied to place. Summer employment program provides both coordination (aggregating local labor demand) and opportunity. Suppression real but the constraint creates exit (temporary employment pathway, work experience, resume credential). Not pure extraction — genuine benefit with coordination function.
constraint_indexing:constraint_classification(sotu_1993_clinton_immediate_jobs_investment_package, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INFRASTRUCTURE CONTRACTOR (ROPE) — Institutional beneficiary with high exit optionality (can pursue private-sector projects, adapt to project selection). Benefits from demand stimulus without extraction cost. Sees the constraint as pure coordination mechanism: government aggregates and synchronizes demand for infrastructure work. Experiences as positive opportunity, not constraint at all.
constraint_indexing:constraint_classification(sotu_1993_clinton_immediate_jobs_investment_package, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FISCAL CONSERVATIVES (TANGLED ROPE) — Organized opposition that sees the constraint as both coordination mechanism (stimulus logic is rational) AND extractive burden (federal spending, deficit accumulation, intergenerational cost). Constrained by macroeconomic consensus on demand-side stimulus need during recession. Experience mixed: the constraint solves a real coordination problem (aggregate demand) while imposing fiscal costs they believe shouldn't exist. Weak but identifiable extraction toward this group (future debt burden) alongside coordination benefit.
constraint_indexing:constraint_classification(sotu_1993_clinton_immediate_jobs_investment_package, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ECONOMIC POLICY COALITION (SCAFFOLD) — Powerful agents (Federal Reserve, economic advisors, Congress) see this as temporary demand-side intervention with explicit sunset: as labor market tightens and cyclical unemployment declines, the program phases out. χ low because the coalition has full agency and sees an exit path (recovery). Theater moderate (program requires ongoing management but sunset is genuine, not theatrical). The constraint is scaffolding for economic recovery, not permanent structure.
constraint_indexing:constraint_classification(sotu_1993_clinton_immediate_jobs_investment_package, scaffold,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: LONG-TERM INSTITUTIONAL VIEW (PITON) — Over decades, direct government employment programs show patterns of institutional inertia: temporary programs often persist beyond their sunset date or evolve into permanent structures (bureaucratic path dependence, political capture of beneficiary communities, institutional momentum). Theater ratio high when measured over 30-year horizon: the program's stated function (counter-cyclical stimulus) degrades into an incumbent interest group dynamic (program maintenance becomes its own justification). From this view, the constraint exhibits piton characteristics: original coordination function atrophies, performance metrics become theatrical, but institutional persistence continues through inertia.
constraint_indexing:constraint_classification(sotu_1993_clinton_immediate_jobs_investment_package, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational and universal perspective, Keynesian counter-cyclical stimulus addresses an unchangeable structural feature of capitalist economies: the fallacy of composition (individuals maximizing savings during downturns produces aggregate demand collapse and involuntary unemployment). This constraint appears as an immutable natural law of macroeconomic equilibrium. However, the structural data contradicts mountain: the constraint is contingent institutional policy, not natural law. The engine will classify this as a false summit, revealing that 'inherent to market economies' naturalizes what is actually a policy choice.
constraint_indexing:constraint_classification(sotu_1993_clinton_immediate_jobs_investment_package, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1993_clinton_immediate_jobs_investment_package_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1993_clinton_immediate_jobs_investment_package, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1993_clinton_immediate_jobs_investment_package, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1993_clinton_immediate_jobs_investment_package, TR),
    TR >= 0.70.

:- end_tests(sotu_1993_clinton_immediate_jobs_investment_package_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.22): Low-moderate. The constraint's primary function is coordination (government aggregates and synchronizes labor demand), not extraction. However, extractiveness is non-zero because: (1) fiscal costs (deficit borrowing, debt service) are real and potentially borne by future taxpayers; (2) program administration absorbs some resources; (3) contractor selection may not be fully competitive. The value reflects that the constraint solves a genuine collective action problem while imposing real costs. Suppression (0.35): Moderate. Barriers to participation exist (geographic location, skill requirements, application processes) but are not severe — the constraint itself is designed to reduce suppression for unemployed and depressed-region populations. Theater ratio (0.48): Low-moderate. Program architecture focuses on verifiable outcomes (jobs created, infrastructure completed, summer employment hours) rather than purely performative metrics. However, measurable job creation is somewhat theater-adjacent because employment numbers depend on what counts as 'job' (temporary vs permanent, full-time equivalents vs headcount) and baseline counterfactuals (what employment would have been without stimulus) are unobservable. Rising theater ratio over the interval reflects increasing focus on counting deliverables rather than measuring true behavioral change.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates critical perspectival disagreement along temporal and distributional lines. Beneficiaries in the present (unemployed workers, youth, contractors) experience rope — genuine coordination with immediate benefit and low personal extraction cost. The fiscal conservative perspective (tangled_rope) acknowledges the coordination logic but weighs fiscal costs that are abstract at the moment of spending but concrete as future debt service. The long-term institutional view (piton) predicts that political pressure will prevent sunset execution, transforming temporary scaffolding into permanent inertial structure. The analytical observer at civilizational scope risks mountain classification — viewing macroeconomic stimulus as an unchangeable feature of capitalist economies rather than as a contingent policy design. The gap between rope (contemporaneous beneficiary view) and snare (intergenerational victim view) is fundamental: future taxpayers bear costs they did not choose, creating extraction that is invisible to present beneficiaries. This gap is diagnostic: it reveals that the constraint's low measured extractiveness (0.22) may be artificially low because it measures only contemporaneous costs, not intergenerational distribution.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality chain prioritizes beneficiary/victim declarations over power atom alone. Unemployed workers are declared beneficiaries (direct target of job creation) with trapped exit → f(d) accounts for their structural immobility while acknowledging net benefit. Contractors are beneficiaries with arbitrage options → low d → negative or near-zero χ contribution. Fiscal conservatives are organized agents bearing costs → moderate d. Future taxpayers (declared as implicit victims in the intergenerational omega) would have high d → high f(d) → high χ from their perspective, but this is captured in the omega variable rather than in the primary perspectives because future agents are not contemporaneously observable. The engine will use the directionality chain to compute that the constraint's apparent low extraction (0.22) masks intergenerational asymmetry: those paying costs (future taxpayers) are not those making the decision, creating an extraction mechanism that spreads across time rather than concentrating in space.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint avoids the coordination-extraction conflation that mandatrophy warns against. The rope classification is justified because the constraint has a genuine coordination function (solving aggregate demand problem via government demand aggregation) that is distinct from extraction (fiscal costs, administration overhead, contractor margin). The tangled_rope perspective from fiscal conservatives is correct — the constraint is simultaneously coordination mechanism and extraction mechanism — not a misclassification of one as the other. The piton perspective identifies real degradation risk: if political pressure prevents sunset, the coordination function (counter-cyclical stimulus) would atrophy while institutional persistence continued, creating the inertial trap that piton classification detects. The false summit risk (mountain perspective) is real: the analytical observer can naturalize this as 'inherent macroeconomic necessity' rather than as contingent policy. The mandatrophy does not require resolving these perspectives to a single 'correct' type; rather, it verifies that each perspectival classification is structurally justified by the measured metrics and agent position. Rope is justified for contemporary beneficiaries. Tangled_rope is justified for organized fiscal agents. Piton is justified for long-term institutional dynamics. Mountain is identified as a false summit (naturalization of contingent policy). The constraint passes mandatrophy because the typological range reflects genuine perspectival differences, not confusion between coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    macroeconomic_effectiveness_uncertain,
    'What proportion of job creation is attributable to the stimulus package vs baseline economic recovery forces?',
    'Counterfactual analysis: comparison of actual employment trajectories vs. dynamic stochastic general equilibrium model projections without stimulus; cross-country comparisons of stimulus scale vs employment recovery timing during similar recessions',
    'If stimulus effect > 70% of observed job growth: rope classification confirmed (genuine coordination mechanism). If stimulus effect < 30%: reclassify toward snare (government spending with minimal employment effect, extraction toward deficit holders). If effect ambiguous: strengthens mountain classification (macroeconomic forces are dominant, policy intervention is marginal).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(macroeconomic_effectiveness_uncertain, empirical, 'Attribution of job creation to stimulus vs baseline recovery').

omega_variable(
    temporary_vs_permanent_employment_quality,
    'Do jobs created by infrastructure investment and summer youth programs lead to stable long-term employment trajectories or do they function as temporary relief without skill development?',
    'Longitudinal tracking of worker outcomes: wages, employment stability, and skill acquisition 2-5 years post-program participation; comparison with control group of unemployed workers who did not participate',
    'If persistent benefits: genuine rope coordination with human capital externalities. If temporary relief only: reclassifies toward tangled_rope or snare (extraction toward treasury, temporary alleviation for beneficiaries without lasting opportunity structure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(temporary_vs_permanent_employment_quality, empirical, 'Whether temporary jobs create lasting employment pathways').

omega_variable(
    deficit_financing_intergenerational_cost,
    'Is the fiscal cost of the stimulus (deficit borrowing, debt service) borne equitably across generational cohorts or disproportionately by future taxpayers?',
    'Generational accounting: lifetime net present value of taxes vs benefits for cohorts born before, during, and after the stimulus period; comparison of implicit government liabilities before and after stimulus period',
    'If costs internalized by contemporaneous beneficiary cohorts: rope classification holds. If future generations bear material costs: tangled_rope or snare classification for intergenerational dimension (younger/future workers experience extraction via taxation for past stimulus).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deficit_financing_intergenerational_cost, empirical, 'Intergenerational distribution of stimulus financing costs').

omega_variable(
    political_capture_of_sunset_clause,
    'Does the sunset clause function as a genuine automatic phase-out or does political pressure prevent termination when recovery arrives?',
    'Historical observation: track continuation votes, budget authority renewals, or formal sunset clause invocation. Identify point of program termination (if any) relative to employment recovery milestones. Measure political pressure from constituent groups (contractors, local governments, labor unions) opposing sunset.',
    'If sunset executed as designed: scaffold classification confirmed. If sunset prevented by political pressure: program devolves toward piton (performative maintenance) or snare (extraction mechanism that should have ended persists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_capture_of_sunset_clause, empirical, 'Whether sunset clause prevents permanent program entrenchment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1993_clinton_immediate_jobs_investment_package, 0, 2).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_1993_clinton_immediate_jobs_investment_package, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sotu_tr_t1, sotu_1993_clinton_immediate_jobs_investment_package, theater_ratio, 1, 0.42).
narrative_ontology:measurement(sotu_tr_t2, sotu_1993_clinton_immediate_jobs_investment_package, theater_ratio, 2, 0.48).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_1993_clinton_immediate_jobs_investment_package, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(sotu_be_t1, sotu_1993_clinton_immediate_jobs_investment_package, base_extractiveness, 1, 0.15).
narrative_ontology:measurement(sotu_be_t2, sotu_1993_clinton_immediate_jobs_investment_package, base_extractiveness, 2, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1993_clinton_immediate_jobs_investment_package, resource_allocation).
narrative_ontology:affects_constraint(sotu_1993_clinton_immediate_jobs_investment_package, federal_deficit_accumulation_1990s).
narrative_ontology:affects_constraint(sotu_1993_clinton_immediate_jobs_investment_package, infrastructure_aging_maintenance_deficit).
narrative_ontology:affects_constraint(sotu_1993_clinton_immediate_jobs_investment_package, cyclical_unemployment_hysteresis).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1993_clinton_immediate_jobs_investment_package, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
