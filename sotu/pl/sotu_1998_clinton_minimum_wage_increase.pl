% ============================================================================
% CONSTRAINT STORY: sotu_1998_clinton_minimum_wage_increase
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1998_clinton_minimum_wage_increase, []).

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
 *   constraint_id: sotu_1998_clinton_minimum_wage_increase
 *   human_readable: Federal Minimum Wage Elevation (1998 Clinton Proposal)
 *   domain: labor_economics/wage_policy
 *
 * SUMMARY:
 *   The federal minimum wage floor, as proposed in Clinton's 1998 State of
 *   the Union, represents a structural constraint that shifts the Pareto
 *   frontier by legally prohibiting below-floor wage-setting. The constraint
 *   benefits low-wage workers and compressed-wage workers (through relative
 *   wage gains) while imposing costs on employers, particularly in low-margin
 *   sectors (retail, hospitality, fast food). The constraint exhibits genuine
 *   coordination functions (eliminating race-to-the-bottom wage competition,
 *   raising the baseline for all wage-setting) alongside asymmetric
 *   extraction (from job-seekers at the margin, from low-margin employers,
 *   potentially from consumers through price increases). The extractiveness
 *   measurement (0.58) reflects the moderate-to-high cost imposed on losing
 *   groups relative to benefiting groups. The suppression measurement (0.65)
 *   reflects substantial barriers to exit: employers cannot reduce wages
 *   below the floor; workers cannot accept substandard wages in exchange for
 *   employment; firms face constrained adjustment options (all of which are
 *   costly). The theater ratio (0.48) reflects that the minimum wage floor
 *   has genuine functional content (it does coordinate wage-setting) but
 *   involves compliance overhead and enforcement uncertainty that reduce the
 *   transparency of the rule's actual impact.
 *
 * KEY AGENTS:
 *   - Low-wage workers (powerless/trapped): Primary intended beneficiaries; face suppression through employment uncertainty and hours reduction
 *   - Compressed-wage workers (moderate/constrained): Secondary beneficiaries experiencing relative wage gains; face extraction through reduced negotiation flexibility
 *   - Low-margin employers (powerful/constrained): Primary victims facing immediate labor cost increases with limited adjustment options
 *   - High-margin employers (institutional/arbitrage): Secondary beneficiaries able to profitably employ at higher wages and pass costs to consumers
 *   - Labor unions (organized/mobile): Beneficiaries whose negotiating position is strengthened by the wage floor
 *   - Job-seekers not yet employed (powerless/trapped): Invisible victims facing reduced employment opportunities at the margin
 *   - Consumers in affected sectors (moderate/constrained): Bear distributed cost through price increases; largely unorganized
 *   - Department of Labor compliance system (institutional/arbitrage): Maintains enforcement; experiences constraint as degraded institutional role (piton)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1998_clinton_minimum_wage_increase, 0.58).
domain_priors:suppression_score(sotu_1998_clinton_minimum_wage_increase, 0.65).
domain_priors:theater_ratio(sotu_1998_clinton_minimum_wage_increase, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1998_clinton_minimum_wage_increase, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1998_clinton_minimum_wage_increase, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1998_clinton_minimum_wage_increase, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1998_clinton_minimum_wage_increase, tangled_rope).
narrative_ontology:human_readable(sotu_1998_clinton_minimum_wage_increase, "Federal Minimum Wage Elevation (1998 Clinton Proposal)").
narrative_ontology:topic_domain(sotu_1998_clinton_minimum_wage_increase, "labor_economics/wage_policy").

domain_priors:requires_active_enforcement(sotu_1998_clinton_minimum_wage_increase).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1998_clinton_minimum_wage_increase, low_wage_workers).
narrative_ontology:constraint_beneficiary(sotu_1998_clinton_minimum_wage_increase, compressed_wage_workers).
narrative_ontology:constraint_beneficiary(sotu_1998_clinton_minimum_wage_increase, consumer_benefit_recipients).
narrative_ontology:constraint_victim(sotu_1998_clinton_minimum_wage_increase, low_margin_employers).
narrative_ontology:constraint_victim(sotu_1998_clinton_minimum_wage_increase, small_retail_hospitality_firms).
narrative_ontology:constraint_victim(sotu_1998_clinton_minimum_wage_increase, potential_job_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-WAGE WORKER TRAPPED (SNARE) — Worker earning below the new floor faces a constraint that appears beneficial but traps them in high suppression. If employed, they benefit from the wage floor. But the constraint creates a discontinuity: firms may reduce hours, eliminate positions, or substitute capital/automation rather than pay the higher wage. The worker cannot exit — they face the choice between accepting fewer hours at the new floor or losing employment entirely. The minimum wage sets a hard boundary below which legal employment is prohibited, eliminating the worker's option to accept lower wages in exchange for employment. Suppression is high: labor market access is contingent on the firm's ability to profitably employ at the new floor.
constraint_indexing:constraint_classification(sotu_1998_clinton_minimum_wage_increase, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPRESSED-WAGE WORKER (TANGLED ROPE) — Worker earning just above the old minimum (e.g., $6.50) experiences the constraint as mixed. The wage floor creates upward pressure on all near-minimum wages through compression effects — employers must raise wages of workers above the old floor to maintain internal pay equity and retention. This worker benefits from the wage compression (gains relative wage). But the constraint also extracts: the worker's bargaining power is constrained by the fact that the floor is now legally enforceable — the worker cannot negotiate away from it, and firms have less flexibility in individual wage-setting. The constraint coordinates wage-setting across firms (reducing destructive competition on wages) while simultaneously extracting through reduced negotiation flexibility.
constraint_indexing:constraint_classification(sotu_1998_clinton_minimum_wage_increase, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LOW-MARGIN EMPLOYER (SNARE) — Firm in retail, hospitality, or fast food (margins 3-8%) faces a hard constraint: labor costs rise immediately upon enactment. The employer can respond by raising prices (constrained by competition), cutting hours (constrained by labor demand), accelerating automation (constrained by capital availability), or reducing other costs. All options are expensive or painful. The constraint suppresses the employer's primary exit option — they cannot simply reduce wages to maintain margins. They are trapped in a game where competitors face the same floor, preventing individual firm advantage through wage-cutting. The extraction is high because the constraint falls most heavily on low-margin sectors; high-margin firms absorb the cost more easily.
constraint_indexing:constraint_classification(sotu_1998_clinton_minimum_wage_increase, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE-MARGIN EMPLOYER (ROPE) — Firm with high margins (tech, finance, professional services) or ability to pass costs to consumers experiences the minimum wage floor as pure coordination. The constraint eliminates wage competition from undercutting smaller competitors — the floor levels the playing field and reduces destructive wage competition. Large firms can profitably employ at the higher wage and pass through cost increases to customers. They benefit from the elimination of substandard-wage pressure from competitors. The constraint solves a collective action problem: without the floor, competitive pressure would drive all firms toward lower wages, harming workers while benefiting no firm. The floor coordinates on a higher equilibrium.
constraint_indexing:constraint_classification(sotu_1998_clinton_minimum_wage_increase, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR UNION COALITION (ROPE) — Organized labor benefits from the wage floor as a coordination device. The floor raises the baseline for all negotiations and reduces the pool of workers competing at substandard wages. Union members themselves may be far above the minimum, but the floor protects lower-wage workers from undercutting union jobs and establishes a norm of non-zero labor costs. The constraint coordinates across industries and firms, reducing the race-to-the-bottom dynamic. Unions see the minimum wage floor as a structural support for their broader bargaining power.
constraint_indexing:constraint_classification(sotu_1998_clinton_minimum_wage_increase, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: COMPLIANCE SYSTEM (PITON) — The enforcement apparatus (Department of Labor, state labor agencies, compliance audits) maintains the minimum wage as a degraded institution. The theater_ratio is moderate (0.48) reflecting that enforcement is imperfect — many violations go undetected or unpunished, particularly in cash-based and informal sectors. The compliance system persists through political commitment and institutional inertia rather than through any direct functional success. The actual enforcement cost (audits, investigations, penalties) is substantial relative to the coordination benefit produced. Many firms maintain compliance through administrative overhead (timekeeping systems, HR staff) that produces no value except enforcing the rule itself.
constraint_indexing:constraint_classification(sotu_1998_clinton_minimum_wage_increase, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the minimum wage floor is a structural hybrid: it coordinates wage-setting (eliminating destructive undercutting) while simultaneously extracting from job-seekers and low-margin firms. The constraint shifts the Pareto frontier — no longer possible to hire labor below the floor, so some jobs that would exist at lower wages do not exist at the higher floor. The extraction is not intentional misdirection but a fundamental structural trade-off: the coordination benefit (wage stability, elimination of substandard competition) comes at the cost of reduced employment at the margin. The constraint is not a snare (the coordination function is genuine) nor pure rope (the employment effect is real extraction from potential workers). Tangled rope classification reflects the genuine hybrid structure.
constraint_indexing:constraint_classification(sotu_1998_clinton_minimum_wage_increase, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1998_clinton_minimum_wage_increase_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1998_clinton_minimum_wage_increase, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1998_clinton_minimum_wage_increase, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1998_clinton_minimum_wage_increase, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1998_clinton_minimum_wage_increase, TR),
    TR >= 0.70.

:- end_tests(sotu_1998_clinton_minimum_wage_increase_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-to-high. The minimum wage floor redistributes from employers and marginal job-seekers to workers already in employment and consumers. The extractiveness value reflects the magnitude of this redistribution relative to baseline (absence of floor). The value rises from 0.35 at t=0 (pre-implementation uncertainty) to 0.58 by t=6 (as employers absorb costs and adjust). Suppression (0.65): Moderate-high. The floor creates hard boundaries that suppress exit options for all parties. Employers cannot reduce wages; workers cannot accept lower wages in exchange for employment; potential workers cannot enter labor markets where firms cannot profitably employ at the floor. Suppression increases slightly over the interval as adjustment costs materialize and firms find fewer low-cost adaptation options. Theater ratio (0.48): Moderate. The minimum wage rule has genuine content — it does change actual wage-setting behavior and reduces destructive competition — but enforcement is imperfect and compliance involves substantial overhead (timekeeping systems, HR administration) that is required only because the rule exists. The theater ratio is lower than in pure compliance-theater constraints because the wage floor produces measurable real-world effects (wage increases for compliant employers). Theater increases slightly over time as enforcement systems mature and compliance becomes more ritualized.
 *
 * PERSPECTIVAL GAP:
 *   The widest gap appears between low-wage workers in secure employment (who see a beneficial constraint despite suppression) and job-seekers at the margin (who see extraction through reduced opportunities). Both are 'powerless' in absolute terms, but their structural relationships to the constraint differ fundamentally. Employed low-wage workers are beneficiaries facing the constraint as a wage floor; job-seekers are victims facing the constraint as an employment barrier. A second major gap separates low-margin employers (Snare) from high-margin employers (Rope). Both are 'employers,' but their exit options differ: low-margin firms cannot absorb the cost; high-margin firms can pass it through. The constraint that appears coordinative (eliminating wage-cutting) to one firm appears extractive to the other. The most theoretically interesting gap is between the Piton classification (compliance system as degraded institution) and the Tangled Rope classification (analytical view of genuine hybrid structure). The piton perspective sees the minimum wage as increasingly performative — the enforcement overhead is high relative to the actual violation rate, suggesting the rule is maintained through institutional inertia. The analytical perspective sees this as missing the point: the rule's function is not to catch and punish violations, but to coordinate on a high-wage equilibrium that self-enforces through competitive dynamics. The perspective gap reflects different understandings of how rules work.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality (d) value for each perspective derives from the agent's relationship to the wage-setting constraint. Low-wage workers in employment are beneficiaries (d ≈ 0.15-0.25 depending on hours retention), but job-seekers are victims (d ≈ 0.85-0.95). Low-margin employers are victims facing direct cost pressure (d ≈ 0.80-0.90). High-margin employers are beneficiaries escaping wage-cutting pressure (d ≈ 0.10-0.20). The sigmoid f(d) then converts these d values to effective extractiveness: beneficiaries with low d see negative or near-zero χ (the constraint subsidizes them through reduced wage competition); victims with high d see high χ (they bear the extraction cost). The formula χ = ε × f(d) × σ(S) scales by the scope modifier σ(national) = 1.0 and applies the directionality sigmoid. The constraints that emerge are consistent with this logic: beneficiaries classify as Rope; victims classify as Snare; mixed cases classify as Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   The minimum wage constraint resolves the mandatrophy by acknowledging that it IS a genuine Tangled Rope: it coordinates wage-setting (eliminating destructive competition, raising the wage floor) while simultaneously extracting from job-seekers at the margin and from low-margin firms. The tension between these functions is not a classification error but a structural feature. The coordination function is real: without the floor, competitive pressure would drive all firms toward lower wages, harming workers globally while benefiting no firm locally. The extraction is also real: some jobs that would exist at lower wages do not exist at the higher floor, and some firms that would remain profitable at lower wages close or contract. Both effects occur simultaneously. The Tangled Rope classification prevents the false dichotomy of 'the minimum wage is either good or bad.' It is both: coordinatively beneficial and extractively costly. The claimed type (Tangled Rope) matches the analytical perspective's classification, confirming that the primary coordinating agent (the state enforcing the wage floor) is engaged in both coordination and extraction. The perspectives from beneficiaries and victims confirm the mixed structure from their lived experience. The Snare perspectives (low-wage workers facing hours reduction, low-margin employers facing closure) represent the extraction component becoming salient. The Rope perspectives (compressed-wage workers, large employers) represent the coordination component becoming salient. No single perspective 'reveals the truth' — the multiplicity of perspectives reveals the hybrid structure itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    employment_elasticity_threshold,
    'What is the employment elasticity of the minimum wage increase — do job losses occur at the margin, and how large are they?',
    'Time-series employment data pre- and post-increase; regional variation in adoption; comparative analysis with firms above/below the wage threshold. NLSY longitudinal tracking of individual transitions.',
    'If elasticity > -0.3 (large job losses): constraint reclassifies toward Snare for low-wage workers; extraction increases. If elasticity ≈ 0 (no job losses): constraint reclassifies toward Rope; pure coordination with minimal extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(employment_elasticity_threshold, empirical, 'Employment elasticity of the minimum wage increase').

omega_variable(
    hours_reduction_substitution,
    'Do firms reduce hours rather than jobs, converting full-time positions to part-time?',
    'Average hours per employee; proportion of part-time vs full-time positions in affected sectors; worker survey data on preferred hours vs actual hours.',
    'If widespread hours reduction: workers experience the constraint as suppression (fewer hours, same or lower weekly earnings); the extraction component increases relative to coordination benefit. If firms maintain hours: coordination function dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(hours_reduction_substitution, empirical, 'Whether firms reduce hours rather than jobs').

omega_variable(
    price_pass_through_mechanism,
    'Do firms pass through the wage cost increase to consumers via price increases, or absorb it through margin compression?',
    'Price tracking for affected sectors (fast food, retail); comparison of price increases to wage increases; elasticity of demand for products in affected sectors; firm profitability data.',
    'If high pass-through: extraction distributed to consumers; low-margin employers experience less extraction. If low pass-through: extraction concentrated on employers; low-margin firms face severe compression. This determines whether the constraint is truly extracting from employers or redistributing from consumers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_pass_through_mechanism, empirical, 'Degree of price pass-through for wage cost increases').

omega_variable(
    automation_acceleration_signal,
    'Does the wage floor accelerate automation and capital substitution for labor?',
    'Capital investment patterns in affected sectors pre/post increase; adoption rates of automation technologies (self-checkout, kitchen automation, etc.); trend analysis controlling for technological progress.',
    'If acceleration is significant: the constraint creates long-term extraction through technological lock-in (workers displaced into lower-wage sectors); the snare classification gains support. If no acceleration: automation is independent of the wage floor.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_acceleration_signal, empirical, 'Whether the wage floor accelerates automation').

omega_variable(
    cross_sector_distributional_effect,
    'Which sectors and worker populations experience gains vs losses from the wage floor?',
    'Sector-by-sector analysis: employment changes, wage changes, price changes by sector. Worker demographics: age, education, geography, prior employment history. Comparison of within-sector winners (low-wage workers who keep jobs) vs losers (job-seekers who cannot find employment).',
    'If concentrated gains to already-employed low-wage workers and concentrated losses to job-seekers: constraint may be reclassified as Tangled Rope extracting from the least organized group (unemployed). If broad distributional benefits: constraint approaches pure Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cross_sector_distributional_effect, empirical, 'Cross-sector and demographic distribution of wage floor effects').

omega_variable(
    small_firm_viability_boundary,
    'Does the wage floor push firms below a viability threshold (closure, consolidation, exit)?',
    'Firm entry/exit rates in low-margin sectors; bankruptcy filings and closures; acquisition activity in retail and hospitality; comparison of firm survival rates above vs below the wage threshold.',
    'If significant firm exit: the constraint produces concentrated extraction on small business owners; this increases the snare component. If firms adjust through other mechanisms: the constraint is more purely coordinative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(small_firm_viability_boundary, empirical, 'Whether the wage floor causes firm closures or exits').

omega_variable(
    informal_sector_displacement,
    'Does the wage floor push workers into informal/undeclared employment to evade the constraint?',
    'Size and growth of informal employment post-increase; worker survey data on work status (formal vs informal); compliance audit findings; comparison with periods of lower enforcement.',
    'If significant informal displacement: the constraint suppresses formal employment without raising informal wages; the extraction component increases. The suppression is transferred from employers to workers (who lose formal employment protections). The constraint may reclassify as a Snare for the most vulnerable workers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_sector_displacement, empirical, 'Displacement into informal/undeclared employment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1998_clinton_minimum_wage_increase, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(minwage_theater_t0, sotu_1998_clinton_minimum_wage_increase, theater_ratio, 0, 0.35).
narrative_ontology:measurement(minwage_theater_t3, sotu_1998_clinton_minimum_wage_increase, theater_ratio, 3, 0.42).
narrative_ontology:measurement(minwage_theater_t6, sotu_1998_clinton_minimum_wage_increase, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(minwage_extractiveness_t0, sotu_1998_clinton_minimum_wage_increase, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(minwage_extractiveness_t3, sotu_1998_clinton_minimum_wage_increase, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(minwage_extractiveness_t6, sotu_1998_clinton_minimum_wage_increase, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1998_clinton_minimum_wage_increase, resource_allocation).
narrative_ontology:affects_constraint(sotu_1998_clinton_minimum_wage_increase, wage_compression_dynamics).
narrative_ontology:affects_constraint(sotu_1998_clinton_minimum_wage_increase, small_business_retail_viability).
narrative_ontology:affects_constraint(sotu_1998_clinton_minimum_wage_increase, labor_market_entry_barriers).

% DUAL FORMULATION NOTE:
% The minimum wage floor is upstream of several labor market constraints: wage compression effects on workers slightly above the floor, viability pressures on small retail/hospitality firms, and employment barriers for job-seekers competing at the wage threshold. The wage floor creates a structural change that propagates through multiple institutional domains. The ε value (0.58) reflects the direct redistribution effect; downstream constraints have their own ε values reflecting the secondary effects of this structural shift.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1998_clinton_minimum_wage_increase, powerless, 0.88).
constraint_indexing:directionality_override(sotu_1998_clinton_minimum_wage_increase, powerful, 0.82).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
