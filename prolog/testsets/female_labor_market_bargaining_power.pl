% ============================================================================
% CONSTRAINT STORY: female_labor_market_bargaining_power
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_female_labor_market_bargaining_power, []).

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
 *   constraint_id: female_labor_market_bargaining_power
 *   human_readable: Female Labor Market Bargaining Power Asymmetry
 *   domain: economic/labor/gender
 *
 * SUMMARY:
 *   Female labor market bargaining power asymmetry is a tangled_rope
 *   constraint that combines genuine coordination functions (household labor
 *   division, workforce segmentation by occupational preference) with
 *   systematic extraction from female workers through wage suppression,
 *   occupational segregation, occupational verticalization (glass ceiling),
 *   and externalization of domestic labor burden onto women. The constraint
 *   exhibits a perspectival range from snare (powerless women without
 *   childcare alternatives) to rope (employers benefiting from compliant,
 *   lower-wage workforce) to scaffold (policy interventions with sunset
 *   logic). The extractiveness score (0.58) reflects that the constraint both
 *   coordinates household labor arrangements and labor market segmentation
 *   while extracting value through wage gaps (16-23% average), motherhood
 *   penalties (5-10% per child), and occupational segregation costs (foregone
 *   lifetime earnings from occupational crowding). Theater ratio (0.48)
 *   indicates that cultural narratives about gender-neutral occupational
 *   choice and biological suitability perform significant legitimizing
 *   function — much of the segregation is maintained through hiring networks
 *   and identity narratives rather than explicit legal barriers.
 *
 * KEY AGENTS:
 *   - Female workers (powerless/trapped or moderate/constrained): primary victims bearing wage suppression, motherhood penalty, occupational segregation, and externalized domestic labor
 *   - Employers (institutional/arbitrage): primary beneficiaries extracting wage suppression and docile workforce; experience constraint as coordination
 *   - Male workers (moderate/constrained or organized/arbitrage): secondary beneficiaries through reduced wage competition and household labor supply; historically organized around male-protective seniority systems
 *   - Labor unions (organized/constrained): bifurcated position — historically protected male workers while excluding or subordinating female members; modern unions attempting to bridge but facing internal capture dynamics
 *   - Policy reform coalition (organized/constrained): governments, women's organizations, labor standards agencies building alternative structures (comparable worth, childcare subsidy, anti-discrimination enforcement) with generational sunset logic
 *   - Occupational segregation legacy system (institutional/arbitrage): historical sexual division of labor maintained through institutional inertia, cultural performance, and network effects rather than explicit enforcement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(female_labor_market_bargaining_power, 0.58).
domain_priors:suppression_score(female_labor_market_bargaining_power, 0.65).
domain_priors:theater_ratio(female_labor_market_bargaining_power, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(female_labor_market_bargaining_power, extractiveness, 0.58).
narrative_ontology:constraint_metric(female_labor_market_bargaining_power, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(female_labor_market_bargaining_power, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(female_labor_market_bargaining_power, tangled_rope).
narrative_ontology:human_readable(female_labor_market_bargaining_power, "Female Labor Market Bargaining Power Asymmetry").
narrative_ontology:topic_domain(female_labor_market_bargaining_power, "economic/labor/gender").

domain_priors:requires_active_enforcement(female_labor_market_bargaining_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(female_labor_market_bargaining_power, employers_using_wage_suppression).
narrative_ontology:constraint_beneficiary(female_labor_market_bargaining_power, male_workers_with_reduced_competition).
narrative_ontology:constraint_victim(female_labor_market_bargaining_power, female_workers).
narrative_ontology:constraint_victim(female_labor_market_bargaining_power, household_economic_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEMALE WORKER (SNARE) — Faces compounded barriers: occupational segregation, wage gap despite equal work, childcare penalties, domestic labor expectations, and constrained geographic mobility due to family structure. Cannot exit labor market (must earn income) or negotiate effectively within it. Experiences the constraint as pure extraction with suppression of alternatives.
constraint_indexing:constraint_classification(female_labor_market_bargaining_power, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: EDUCATED FEMALE WORKER (TANGLED ROPE) — University degree and professional credentials provide some negotiating capacity and alternative employment options. Still experiences wage gap (80-95 cents per dollar male peer) and motherhood penalty. Constraint provides coordination (professional norms, market access) alongside asymmetric extraction. Can exit specific arrangements but at career cost; faces identity-based constraints (motherhood expectations) alongside material ones.
constraint_indexing:constraint_classification(female_labor_market_bargaining_power, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMPLOYER BENEFICIARY (ROPE) — Benefits from wage suppression and access to compliant workforce. Experiences the constraint as pure coordination: occupational segregation and unequal bargaining reduce competition for positions, stabilize labor supply, and reduce wage pressure. Can arbitrage between female and male workers to suppress overall wage bill. Net beneficiary with high agency.
constraint_indexing:constraint_classification(female_labor_market_bargaining_power, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR UNION (TANGLED ROPE) — Historically organized around male breadwinner model; union seniority systems and wage scales protected male workers while excluding female members from apprenticeships and skilled trades. Union benefits male workers (rope function) while either excluding or extracting from female workers (snare/tangled rope function). Modern unions attempting to coordinate across both groups face internal tension — old protections become capture mechanisms if expanded to include female workers at equal rates.
constraint_indexing:constraint_classification(female_labor_market_bargaining_power, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: POLICY REFORM COALITION (SCAFFOLD) — Organized actors (pay equity legislation, childcare support mandates, anti-discrimination enforcement) are building alternative bargaining structures. Equal Pay Acts and comparable worth frameworks attempt to bypass the occupational segregation constraint. Childcare subsidies and parental leave policy address the motherhood penalty. These interventions have sunset logic: as norms shift toward gender-neutral expectations and structural childcare support matures, the bargaining power asymmetry should decline. Current extraction reflects transition period where enforcement is incomplete and norms are contested.
constraint_indexing:constraint_classification(female_labor_market_bargaining_power, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: OCCUPATIONAL SEGREGATION (PITON) — Historical sexual division of labor (women in care, education, service; men in trades, management, technology) persists despite legal equality. The segregation is maintained through institutional inertia and cultural performance rather than active enforcement. Hiring networks, credential requirements, and cultural fit narratives reproduce gendering without explicit gatekeeping. Theater ratio reflects that much of the segregation's persistence is performative ('women don't want these jobs,' 'men aren't suited for care work') rather than structurally enforced.
constraint_indexing:constraint_classification(female_labor_market_bargaining_power, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational perspective, female labor market bargaining asymmetry solves genuine coordination problems (household division of labor, childcare arrangements, workplace scheduling) while simultaneously extracting value from female workers through wage suppression, occupational segregation, and domestic burden concentration. The constraint has both functional (coordination) and extractive (asymmetric cost distribution) dimensions. Effective extraction chi is moderate-high when scaled by scope and directionality.
constraint_indexing:constraint_classification(female_labor_market_bargaining_power, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(female_labor_market_bargaining_power_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(female_labor_market_bargaining_power, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(female_labor_market_bargaining_power, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(female_labor_market_bargaining_power, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(female_labor_market_bargaining_power, TR),
    TR >= 0.70.

:- end_tests(female_labor_market_bargaining_power_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Female workers earn systematically less than male peers for equivalent work (16-23% wage gap depending on context), face motherhood penalties (5-10% per child), and experience occupational segregation that concentrates women in lower-wage sectors (care, service, education vs trades, management, technology). The extraction is neither minimal (ε > 0.45) nor maximal (no biological constraint prevents female employment). The rising trajectory (0.42 → 0.58 over interval) reflects that explicit wage suppression has been reduced by legislation, but occupational segregation and motherhood penalties have actually increased as women entered workforce — the extraction mechanism shifted from wage discrimination to structural barriers. Suppression (0.65): High. Female exit options are constrained by: compounded childcare responsibility, occupational segregation creating credential and network barriers, wage gap making single-income viability difficult, identity-based expectations that women prioritize caregiving, and geographic immobility due to household structure. Childcare is the primary suppression mechanism — without subsidized alternatives, women face choice between employment and childcare, making labor market participation conditional on household arrangement. Theater ratio (0.48): Moderate-low and declining. Cultural narratives about gender-neutral occupational choice ('women don't want these jobs,' 'women aren't suited for management') perform significant legitimizing function, but declining over interval as legislative and cultural shifts reduce plausibility of purely cultural explanation. The decline reflects that occupational segregation is increasingly recognized as structurally enforced (network effects, credential pathways, hiring discrimination) rather than expressed preference.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence across indexical positions. The powerless female worker trapped in occupational segregation with childcare burden sees a snare — no exit option, maximum experienced extraction, no coordination benefit. The educated female worker with organizational resources and childcare options sees tangled_rope — real coordination (professional norms, market access) alongside extraction (wage gap, motherhood penalty). The employer sees rope — pure coordination (workforce segmentation reduces wage pressure, occupational segregation stabilizes labor supply). The policy reform coalition sees scaffold — temporary extraction that can be reduced through legislative intervention and norm change. The occupational segregation system sees piton — the historical sexual division of labor persists through cultural performance and institutional inertia despite legal equality. The analytical observer at civilizational scale sees tangled_rope — the constraint solves real coordination problems while simultaneously distributing costs asymmetrically. The perspectival gap reveals that 'choice' and 'market preference' explanations collapse when examined from the powerless position: without subsidized childcare and occupational desegregation, 'choice' is exercised under duress.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from structural position in the extraction flow. Female workers with trapped exit options and no childcare alternatives experience high d (0.85-0.95) — full target status, high f(d), high experienced extraction. Female workers with education, professional credentials, and childcare resources experience moderate d (0.55-0.65) — both victim and partial beneficiary of professional labor markets, experienced extraction moderate. Employers benefit from the constraint (low d, 0.15-0.25) with arbitrage options — can exit by hiring male workers, but profit from female labor suppression. Male workers experience d around 0.45-0.50 depending on union membership and whether they benefit from occupational segregation protecting higher wages. The piton perspective's directionality (institutional/arbitrage) derives d around 0.20 — the legacy system benefits from its own persistence and has arbitrage options (could reform but doesn't). The analytical perspective derives d around 0.72 — the observer has no exit option from the constraint's reality, experiences it as structurally binding.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by clarifying that female labor market bargaining asymmetry is genuinely tangled_rope: it coordinates household labor arrangements and labor market segmentation (coordination function exists) while extracting value through wage suppression, occupational segregation, and motherhood penalties (asymmetric extraction exists). The mandatrophy trap was asking 'is this pure coordination or pure extraction?' when the answer is 'both.' The constraint cannot be classified as rope (no extraction) because wage gaps and motherhood penalties are empirically significant and asymmetric. It cannot be classified as snare (no coordination) because occupational segmentation does solve household labor coordination problems. The tangled_rope classification holds because both functions are structurally necessary: removing the coordination function (collapsing occupational segmentation) requires building alternative childcare and household labor coordination mechanisms, which is exactly what policy reform is attempting via childcare subsidies, parental leave, and comparative worth frameworks.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domestic_burden_externality,
    'How much of the female labor market wage gap reflects genuine occupational choice vs externalized domestic labor burden?',
    'Counterfactual analysis: wage gap for never-married women without children vs married women with children; accounting for hours of unpaid domestic/care labor; comparing to male workers with equivalent domestic labor obligations',
    'If gap reflects choice: moderate extraction, agent agency exists. If gap reflects externalized burden: high extraction, suppression mechanism is structural (motherhood makes exit from domestic obligation impossible). Classification type may shift from tangled_rope to snare for mothers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domestic_burden_externality, empirical, 'Attribution of wage gap to choice vs externalized domestic burden').

omega_variable(
    comparable_worth_feasibility,
    'Can comparable worth frameworks (equal pay for work of equal value across occupations) actually be implemented without collapsing occupational segregation?',
    'Case studies of jurisdictions implementing comparable worth; measurement of whether segregation persists post-implementation; analysis of wage compression effects',
    'If feasible: policy intervention can reduce extraction without solving structural segregation (scaffold perspective strengthened). If infeasible: wage floors alone cannot address bargaining asymmetry; segregation must be directly tackled. Classification may shift from tangled_rope to snare if policy solutions prove unworkable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comparable_worth_feasibility, empirical, 'Feasibility of comparable worth as extraction-reducing mechanism').

omega_variable(
    male_bargaining_consensus,
    'Do male workers benefit from female wage suppression as a group, or does organized labor''s historical protection of male wages function as a separate constraint from female exclusion?',
    'Historical wage analysis: male wages in contexts with vs without female labor market integration; modeling whether male worker consensus on female wage suppression is active or passive; identifying institutional actors enforcing male-protective seniority systems',
    'If active male consensus: snare has explicit beneficiary with agency (men as a class). If passive/institutional inertia: extraction persists without active maintenance (piton). Affects how labor coalition building might overcome the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(male_bargaining_consensus, empirical, 'Whether male workers actively benefit from female wage suppression').

omega_variable(
    childcare_coordination_vs_burden,
    'Does childcare coordination in the household represent genuine joint optimization or an extraction mechanism disguised as necessity?',
    'Cross-cultural comparison: childcare arrangements in gender-egalitarian societies vs patriarchal ones; measurement of whether unequal childcare burden exists even when female partner earns more; analysis of whether couples with subsidized childcare show convergence toward equal domestic labor',
    'If genuine optimization: childcare is coordination problem, not extraction mechanism — constraint is occupational segregation, not domestic division. If extraction: childcare concentration on women is enforced despite alternatives being available and affordable — constraint includes household-level extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(childcare_coordination_vs_burden, empirical, 'Whether childcare allocation is optimized coordination or gendered extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(female_labor_market_bargaining_power, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(flmbp_tr_t0, female_labor_market_bargaining_power, theater_ratio, 0, 0.62).
narrative_ontology:measurement(flmbp_tr_t10, female_labor_market_bargaining_power, theater_ratio, 10, 0.55).
narrative_ontology:measurement(flmbp_tr_t20, female_labor_market_bargaining_power, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(flmbp_be_t0, female_labor_market_bargaining_power, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(flmbp_be_t10, female_labor_market_bargaining_power, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(flmbp_be_t20, female_labor_market_bargaining_power, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(female_labor_market_bargaining_power, resource_allocation).
narrative_ontology:affects_constraint(female_labor_market_bargaining_power, occupational_segregation_mechanism).
narrative_ontology:affects_constraint(female_labor_market_bargaining_power, motherhood_penalty_dynamics).
narrative_ontology:affects_constraint(female_labor_market_bargaining_power, household_labor_division_norm).
narrative_ontology:affects_constraint(female_labor_market_bargaining_power, childcare_access_bottleneck).

% DUAL FORMULATION NOTE:
% Female labor market bargaining power is upstream of occupational segregation and motherhood penalty mechanisms but downstream of childcare access constraints. The constraint family includes: (1) childcare_access_bottleneck (ε=0.52, barrier mechanism), (2) occupational_segregation_mechanism (ε=0.48, credential/network effect), (3) motherhood_penalty_dynamics (ε=0.61, household obligation externalization), (4) female_labor_market_bargaining_power (ε=0.58, aggregate effect). Each has distinct metrics but shares structural dependence on gendered household labor expectations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(female_labor_market_bargaining_power, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
