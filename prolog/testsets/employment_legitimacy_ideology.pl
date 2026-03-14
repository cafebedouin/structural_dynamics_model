% ============================================================================
% CONSTRAINT STORY: employment_legitimacy_ideology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employment_legitimacy_ideology, []).

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
 *   constraint_id: employment_legitimacy_ideology
 *   human_readable: Employment Legitimacy Ideology
 *   domain: labor/political_economy/ideology
 *
 * SUMMARY:
 *   Employment legitimacy ideology is the normative framework that equates
 *   having a paid job with social legitimacy, moral worth, and the right to
 *   survival resources. This constraint operates across contemporary
 *   capitalist democracies as a coordinating mechanism that allocates labor
 *   while simultaneously extracting concessions from workers who internalize
 *   the belief that employment is the only valid claim on income, dignity,
 *   and social belonging. The constraint exhibits the classic Tangled Rope
 *   signature: it performs genuine coordination (labor market sorting, skill
 *   incentives, productivity) while simultaneously extracting from workers
 *   through wage suppression, benefit denial, and suppression of collective
 *   alternatives. The constraint's theater_ratio (0.65) reflects that much
 *   institutional apparatus (welfare bureaucracy, job-seeking requirements,
 *   meritocratic justification) has become performative — it costs
 *   significant resources to maintain the legitimacy theater (means-testing
 *   administration, job compliance verification) while the actual
 *   coordination function has atrophied in the precariat.
 *
 * KEY AGENTS:
 *   - Precarious Workers: Primary victim (powerless/trapped) — economic dependency and internalized identity fusion leave no exit options; bear maximum extraction of low wages, irregular hours, minimal benefits
 *   - Capital-Holding Class: Primary beneficiary (institutional/arbitrage) — wage suppression justified as market coordination; can arbitrage between labor markets and regulatory regimes
 *   - Organized Labor: Secondary beneficiary/victim (moderate/constrained) — benefits from legitimacy ideology (stabilizes hard-won contracts) while constrained by it (prevents radicalization of precarious workers); internal conflict between institutional stability and systemic transformation
 *   - Welfare State Apparatus: Institutional enforcer (institutional/constrained) — maintains legitimacy through means-testing theater and joblessness stigma; sees own function as degraded (piton perspective)
 *   - Alternative Work Coalition: Organized challenger (organized/mobile) — building scaffold: cooperatives, UBI pilots, work-sharing norms that create exit paths
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent ideology as economic law of nature
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employment_legitimacy_ideology, 0.58).
domain_priors:suppression_score(employment_legitimacy_ideology, 0.68).
domain_priors:theater_ratio(employment_legitimacy_ideology, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employment_legitimacy_ideology, extractiveness, 0.58).
narrative_ontology:constraint_metric(employment_legitimacy_ideology, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(employment_legitimacy_ideology, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employment_legitimacy_ideology, tangled_rope).
narrative_ontology:human_readable(employment_legitimacy_ideology, "Employment Legitimacy Ideology").
narrative_ontology:topic_domain(employment_legitimacy_ideology, "labor/political_economy/ideology").

domain_priors:requires_active_enforcement(employment_legitimacy_ideology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employment_legitimacy_ideology, capital_holders).
narrative_ontology:constraint_beneficiary(employment_legitimacy_ideology, corporate_management).
narrative_ontology:constraint_victim(employment_legitimacy_ideology, precarious_workers).
narrative_ontology:constraint_victim(employment_legitimacy_ideology, labor_collective_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (SNARE) — Trapped by economic dependency and normative framing that employment = legitimacy. Cannot exit the labor market; internalized belief that joblessness is personal failure. Bears full extraction: wage suppression justified as 'market reality,' irregular hours normalized as inevitable, lack of benefits rationalized as 'job mobility trade-off.' Maximum experienced extraction with minimal coordination benefit.
constraint_indexing:constraint_classification(employment_legitimacy_ideology, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED LABOR (TANGLED ROPE) — Constrained by declining membership and fragmented bargaining power, but coordinates genuine workplace protections and wage standards. The constraint operates simultaneously as coordination mechanism (labor standards, collective voice) and extraction mechanism (union dues, bureaucratic capture by negotiated settlement rather than systemic change). Perspectival gap from the precarious worker: organized labor benefits from the legitimacy ideology (it stabilizes their hard-won contracts) while being constrained by it (it prevents radicalization of precarious workers who could expand membership).
constraint_indexing:constraint_classification(employment_legitimacy_ideology, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL-HOLDING CLASS (ROPE) — Experiences the legitimacy ideology as pure coordination: it solves the collective action problem of maintaining a workforce that accepts submarket compensation without revolution. The ideology coordinates wage suppression across competing firms. Net beneficiary — extraction flows toward this agent through normalized low wages, minimal benefits, and suppressed labor organizing. From their perspective, this is efficient coordination, not extraction.
constraint_indexing:constraint_classification(employment_legitimacy_ideology, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WELFARE STATE APPARATUS (PITON) — Maintains the legitimacy ideology through means-testing, welfare stigma, and conditional benefits (unemployment insurance requiring job-seeking, disability requiring proof). These institutions once coordinated genuine social insurance; they now primarily perform verification theater (means-testing bureaucracy that costs more to administer than it saves in benefit reduction). The apparatus sees its own function as degraded — means-testing survives through inertia despite evidence it increases costs and reduces effectiveness. Theater ratio reflects that modern welfare bureaucracy spends more effort on eligibility certification than on benefit delivery.
constraint_indexing:constraint_classification(employment_legitimacy_ideology, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ALTERNATIVE WORK COALITION (SCAFFOLD) — Organized advocates for universal basic income, cooperative ownership, and work-sharing see the legitimacy ideology as a temporary institutional arrangement with a sunset clause. Their organizing creates parallel institutions (worker cooperatives, gig-worker unions, UBI pilot programs) that decouple survival from employment legitimacy. The constraint is temporary and declining — not because the ideology is being refuted intellectually, but because material conditions (technological unemployment, housing costs, climate migration) are making the legitimacy-to-employment link unintelligible to younger cohorts. Sunset estimated at 15-25 years as generational economic insecurity reaches critical mass.
constraint_indexing:constraint_classification(employment_legitimacy_ideology, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational perspective, some employment constraint is inherent to organizing complex economies: the coordination of labor inputs, skill development, and resource allocation fundamentally requires some mechanism of allocation and incentive. This view sees employment legitimacy as an inevitable natural law of economic organization. However, this perspective risks naturalizing what is actually a historically contingent ideological construction. The constraint is NOT a mountain — the engine's false summit detector will flag this as naturalization.
constraint_indexing:constraint_classification(employment_legitimacy_ideology, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employment_legitimacy_ideology_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(employment_legitimacy_ideology, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(employment_legitimacy_ideology, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(employment_legitimacy_ideology, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(employment_legitimacy_ideology, TR),
    TR >= 0.70.

:- end_tests(employment_legitimacy_ideology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from precarious workers through suppressed wages (estimated 20-40% below union-negotiated rates in comparable industries), minimal benefits (health insurance conditional on job tenure, retirement savings shifting from employer-defined to individual-responsible), and suppressed collective power (anti-union messaging rationalized as market efficiency). The extraction is not total (0.90+) because employment does provide material survival and some workers experience genuine coordination benefits (skill development, access to capital, productivity incentives). The intermediate value reflects that the constraint mixes real coordination with extraction layered on top. Suppression (0.68): High. Barriers to exit and mobilization include: economic dependency (alternative income sources scarce), normative coupling (joblessness treated as moral failure), legal barriers (at-will employment, weak organizing protections), and cognitive capture (meritocracy myth suggests precarity is deserved). Theater ratio (0.65): Moderate-high. Significant performative elements: welfare means-testing (costs more to verify eligibility than to pay benefits), job-training programs (low post-training earnings improvement), corporate DEI initiatives (organizational performance unchanged post-adoption), meritocratic hiring mythology (skill requirements inflated beyond actual job demands). These theater elements have increased as the constraint's actual function has become visible and requires legitimation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_mechanism,
    'Is the constraint sustained by internalized identity fusion (worker identity constituted through employment) or by external material barriers (economic dependency)?',
    'Longitudinal tracking of exited workers: do those who leave employment (retirement, disability, sabbatical) maintain identity-lock effects post-exit? Do they seek reemployment from internalized framing or from material necessity? Survey data on whether job loss is experienced as identity loss vs. financial loss.',
    'If primarily identity-locked: constraint persists even after material barriers are removed; exits require identity reconstruction. If primarily material: providing alternative income (UBI, inheritance) enables exit. Classification changes from Snare (identity-locked behavioral trap) to Snare (material trap) — same type but different dissolution mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether workers are trapped by identity fusion or material dependency').

omega_variable(
    coordination_vs_extraction_decomposition,
    'Does the legitimacy ideology coordinate genuine labor market functions (skill allocation, productivity incentives) or is coordination merely the rationalization for extraction?',
    'Comparative institutional analysis: sectors/countries with decoupled legitimacy-employment (strong safety nets, UBI pilots, cooperative ownership) vs. those with tight coupling. Measure productivity, skill development, and worker satisfaction. If outcomes improve post-decoupling: legitimacy was rationalizing extraction. If outcomes degrade: coordination function was real.',
    'If coordination is genuine: Tangled Rope justified — constraint has real coordination function alongside extraction. If coordination is cover story: reclassify as Snare from all perspectives except beneficiaries. Changes the justification premise for scaffold sunset (temporary fix for real problem vs. dissolution of false legitimacy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_decomposition, empirical, 'Whether employment legitimacy coordinates real labor functions or merely rationalizes extraction').

omega_variable(
    suppression_structural_vs_internalized,
    'Is suppression maintained primarily by external barriers (economic dependency, legal restrictions) or by internalized belief (shame, internalized meritocracy myth)?',
    'Experimental intervention: provide unconditional income to treatment group; track whether labor force participation, organizing, or exit attempts increase. Qualitative analysis of narratives: do workers who exit cite external barriers or internal conviction?',
    'If primarily external: removal of material barriers enables mobilization. If primarily internalized: material support alone insufficient — requires identity/cognitive reframing. Affects assessment of suppression metric (0.68): if structural, it may be lower than perceived; if internalized, it may persist despite material intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression is structural or internalized').

omega_variable(
    generational_sunset_timing,
    'At what economic stress threshold (unemployment rate, housing affordability index, debt-to-income ratio) does the legitimacy-employment linkage break generationally?',
    'Cohort analysis: survey generational attitudes toward employment necessity and life legitimacy at different economic stress levels. Model critical threshold where majority reject legitimacy-employment coupling. Track which institutional innovations (UBI pilots, cooperative scaling, work-sharing) accelerate threshold crossing.',
    'If threshold is near (< 8 years): scaffold sunset is imminent and constraint classification should emphasize temporary nature. If threshold is distant (> 25 years): scaffold may be aspirational rather than structural. Affects timeline for mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_sunset_timing, empirical, 'Generational threshold for rejecting employment legitimacy ideology').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employment_legitimacy_ideology, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(empl_leg_tr_t0, employment_legitimacy_ideology, theater_ratio, 0, 0.55).
narrative_ontology:measurement(empl_leg_tr_t20, employment_legitimacy_ideology, theater_ratio, 20, 0.62).
narrative_ontology:measurement(empl_leg_tr_t40, employment_legitimacy_ideology, theater_ratio, 40, 0.65).

% Extraction over time
narrative_ontology:measurement(empl_leg_be_t0, employment_legitimacy_ideology, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(empl_leg_be_t20, employment_legitimacy_ideology, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(empl_leg_be_t40, employment_legitimacy_ideology, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employment_legitimacy_ideology, resource_allocation).
narrative_ontology:affects_constraint(employment_legitimacy_ideology, welfare_stigma).
narrative_ontology:affects_constraint(employment_legitimacy_ideology, union_decline).
narrative_ontology:affects_constraint(employment_legitimacy_ideology, wage_stagnation).
narrative_ontology:affects_constraint(employment_legitimacy_ideology, precarity_normalization).

% DUAL FORMULATION NOTE:
% Employment legitimacy ideology is upstream of specific labor market constraints (wage stagnation, union decline, welfare stigma, precarity normalization). Each downstream constraint has its own epsilon but shares the common ideology as a coordination/extraction backbone. The decomposition reflects that the single ideological constraint creates multiple structural effects with different ε values in different domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employment_legitimacy_ideology, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
