% ============================================================================
% CONSTRAINT STORY: wage_compression_union_sectors
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_wage_compression_union_sectors, []).

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
 *   constraint_id: wage_compression_union_sectors
 *   human_readable: Wage Compression in Unionized Sectors
 *   domain: labor_economics/industrial_relations
 *
 * SUMMARY:
 *   Wage compression in unionized sectors represents a structural constraint
 *   that simultaneously coordinates a wage floor and extracts value from
 *   high-skill senior workers. The constraint emerges from the
 *   collective-bargaining requirement that negotiated wages apply uniformly
 *   across a classification (e.g., all production workers, all teachers with
 *   the same years of service). This creates a tension: the compression
 *   mechanism prevents a race-to-the-bottom for entry-level workers, but it
 *   also caps the earnings of workers whose skills or seniority might command
 *   higher market rates. The constraint is actively enforced through union
 *   contracts and grievance procedures that prevent individual wage-rate
 *   deviation. It exhibits genuine coordination (wage-floor protection,
 *   prevention of undercutting) alongside asymmetric extraction (high-skill
 *   workers subsidizing wage-floor protection). Over the past two decades,
 *   the extractiveness has increased as sector contraction has raised exit
 *   costs for senior workers while technological change has increased the
 *   skill premium for high-performing workers within the sector.
 *
 * KEY AGENTS:
 *   - Entry-Level Union Members: Primary beneficiary (moderate/constrained) — protected from market-rate underbidding; wage floor guaranteed by compression mechanism
 *   - Senior Skilled Workers: Primary victim (powerless/trapped) — highest-paid within compressed band but capped below market rate for their skill; exit is costly due to pension vesting and sector contraction
 *   - Union Negotiating Leadership: Institutional beneficiary (organized/arbitrage) — uses compression to simplify negotiations and demonstrate gains to majority membership; avoids contentious merit-based allocation
 *   - Employers/Industry: Institutional mixed (institutional/constrained) — benefit from predictable labor costs and simplified administration; face hidden costs from high-skill worker retention and turnover
 *   - Union Membership as Collective: Secondary beneficiary (organized/constrained) — wage compression maintains internal solidarity and prevents highest-wage members from capturing all negotiating power
 *   - Analytical Observer: Sees false summit (analytical/analytical) — risks naturalizing contingent institutional design (seniority schedules) as inevitable feature of collective bargaining
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(wage_compression_union_sectors, 0.52).
domain_priors:suppression_score(wage_compression_union_sectors, 0.48).
domain_priors:theater_ratio(wage_compression_union_sectors, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(wage_compression_union_sectors, extractiveness, 0.52).
narrative_ontology:constraint_metric(wage_compression_union_sectors, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(wage_compression_union_sectors, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(wage_compression_union_sectors, tangled_rope).
narrative_ontology:human_readable(wage_compression_union_sectors, "Wage Compression in Unionized Sectors").
narrative_ontology:topic_domain(wage_compression_union_sectors, "labor_economics/industrial_relations").

domain_priors:requires_active_enforcement(wage_compression_union_sectors).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(wage_compression_union_sectors, low_wage_workers).
narrative_ontology:constraint_beneficiary(wage_compression_union_sectors, union_negotiating_teams).
narrative_ontology:constraint_victim(wage_compression_union_sectors, high_skill_senior_workers).
narrative_ontology:constraint_victim(wage_compression_union_sectors, merit_based_advancement).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SENIOR SKILLED WORKER (SNARE) — Trapped in a compressed wage schedule that caps their earning potential despite higher skills and seniority. Exit requires leaving the sector entirely, which carries high costs (loss of pension vesting, seniority rights, specialized skill value). The constraint extracts value from this agent's productivity surplus by preventing market-rate compensation. Maximum experienced extraction with no meaningful exit.
constraint_indexing:constraint_classification(wage_compression_union_sectors, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ENTRY-LEVEL UNION MEMBER (TANGLED ROPE) — Benefits from compression's wage floor (protected from market-rate underbidding) but pays opportunity cost if their skills and performance exceed the compression band. Significant coordination function: wage compression prevents undercutting and wage-floor collapse. Constrained exit (could move to non-union sector but face variable job quality and benefits). Mixed coordination benefit and extraction loss.
constraint_indexing:constraint_classification(wage_compression_union_sectors, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: UNION NEGOTIATING LEADERSHIP (ROPE) — Benefits from compression through simplification of negotiations, reduced internal conflict, and demonstrable gains for majority membership (wage floor protection). The constraint is their primary coordination mechanism for preventing wage-floor collapse and race-to-the-bottom dynamics. Arbitrage options: can exit via leadership transition or sector shift. Experiences the constraint as pure coordination with net benefit.
constraint_indexing:constraint_classification(wage_compression_union_sectors, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: EMPLOYER / INDUSTRY (TANGLED ROPE) — Benefits from wage compression (predictable labor costs, reduced payroll variance, simplified HR administration). Enforces compression through collective bargaining agreements and grievance procedures. But faces extraction pressure: high-skill worker retention suffers, creating hidden costs (training, turnover, recruitment). Constrained exit (abandoning union contracts creates legal and reputation costs). Mixed institutional coordination and extraction avoidance.
constraint_indexing:constraint_classification(wage_compression_union_sectors, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE SENIORITY RITUAL (PITON) — From a long-term perspective, seniority-based wage progression is largely performative: it provides a visible 'fairness' mechanism while actually protecting union leadership from the harder problem of merit-based advancement and skill differentiation. The theater persists through institutional inertia — union contracts maintain seniority language even as actual value accrual shifts to benefits and pension protection. Theater ratio increased as real wage gains have slowed but seniority protections remain contractually entrenched.
constraint_indexing:constraint_classification(wage_compression_union_sectors, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universalizing perspective, wage compression appears as an immutable feature of collective bargaining: any mechanism that protects a wage floor for dispersed workers must suppress individual variation (otherwise high-wage workers would poach all negotiating power). The constraint appears unavoidable — a mathematical consequence of coalition formation. However, the structural data contradicts this: actual mechanisms (seniority schedules, step increases, classification systems) are contingent institutional designs, not logical necessities. The mountain classification is a false summit naturalizing a choice.
constraint_indexing:constraint_classification(wage_compression_union_sectors, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(wage_compression_union_sectors_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(wage_compression_union_sectors, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(wage_compression_union_sectors, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(wage_compression_union_sectors, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(wage_compression_union_sectors, TR),
    TR >= 0.70.

:- end_tests(wage_compression_union_sectors_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high and rising. The constraint genuinely redistributes income from high-skill to low-skill workers within the union. Extractiveness is not as severe as pure Snare because (a) coordination benefits (wage-floor protection) are real, (b) the beneficiary group is large, and (c) the extraction mechanism is transparent rather than hidden. However, extractiveness has increased over 20 years as (1) sector contraction increased exit costs, (2) technological change increased the skill premium, and (3) real wages stagnated while compression ratios tightened. Suppression (0.48): Moderate. Significant barriers to exit include pension vesting (5-10 year cliffs), skill specialization, union contract enforcement, and regional sector concentration. But suppression is not total — skilled workers can and do exit to non-union sectors or into management tracks within firms. Some workers accept compression as part of union solidarity (internalized, not purely structural). Theater ratio (0.35): Low. The compression mechanism is not particularly performative — it actually implements the stated goal (wage floor protection). Seniority progression carries some theater (visible fairness) but is substantive (pension and benefit accrual follow seniority). Theater is lower than institutional constraints with more visible ritual.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival range spans from pure coordination (union leadership view) to pure extraction (senior worker view). Union leadership sees Rope — they are solving the legitimate problem of maintaining coalition stability. Entry-level members see Rope — they are receiving genuine wage-floor protection. Senior workers see Snare — they are trapped in a compressed schedule with no meaningful exit and no voice in union decisions. Employers see Tangled Rope — they benefit from cost stability but pay hidden costs in turnover and skill loss. The piton perspective (seniority ritual as theater) represents the long-term observation that union contracts maintain seniority language even as its substantive value has declined relative to pension and benefit protection. The analytical false summit (compression as immutable feature) naturalizes what is actually a choice about how to allocate negotiating gains within a coalition.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's structural relationship to the extraction flow. Entry-level workers (beneficiary, constrained exit) have low d — they experience the constraint as net benefit. Senior workers (victim, trapped exit) have high d — they bear the extraction cost and cannot escape. Union leadership (beneficiary through coalition simplification, arbitrage options in leadership transition) has low d — they experience the constraint as coordination mechanism that solves their core problem (avoiding internal wage conflict). Employers (mixed: benefit from cost predictability but lose high-skill retention) have intermediate d around 0.5 — they both benefit and bear costs. The key structural point: the beneficiaries are a large group (majority of union membership) with moderate power; the victims are a smaller group (high-skill senior workers) with lower power within the union. This asymmetry is exactly what makes the constraint extractive — the majority can enforce compression against the preference of the minority without the minority having effective exit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates how mandatrophy resolves via perspectival composition. The union leadership and majority membership genuinely benefit from compression and experience it as coordination. The senior minority genuinely suffers extraction. Both observations are structurally correct — the constraint IS simultaneously coordination and extraction from different positions. The mandatrophy dissolves when the analytical observer recognizes that the classification depends on which coalition member you are. There is no contradiction between 'compression is necessary to maintain wage-floor protection' (leadership view, Rope) and 'compression extracts from high-skill workers' (senior worker view, Snare) — both are true from their respective structural positions. The resolution is not to pick one classification but to recognize that the constraint's behavior depends on power asymmetry within the coalition. If high-skill workers had equal voting power in union decisions, the classification would shift — they could negotiate breaks in compression for themselves. The mandatrophy reveals the actual extraction mechanism: concentration of negotiating power in the hands of majority-vote union democracy, which allows the majority to impose compression on a constrained minority.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    skill_detection_mechanism,
    'Can skill differentiation be credibly measured and rewarded within union frameworks without collapsing the wage-floor coordination function?',
    'Comparative analysis of sectors with skill-based pay (nursing, trades) vs rigid compression (manufacturing, public service). Measurement of whether skill-based systems maintain lower-wage floors or whether they compress to zero.',
    'If credible skill measurement exists: compression is contingent choice, not structural necessity — more perspectives shift to lower extraction classifications. If skill measurement fails or collapses wage floors: compression is revealed as necessary for stability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_detection_mechanism, empirical, 'Whether skill-differentiated pay can coexist with wage-floor protection').

omega_variable(
    coalition_stability_threshold,
    'What wage-spread ratio (highest/lowest paid in a unit) is the actual threshold for coalition breakdown, and how does it vary by sector and workforce heterogeneity?',
    'Historical analysis of union contract negotiations; tracking of when unions formally split into pay tiers; wage-ratio measurements at contract renewal thresholds.',
    'If threshold is tight (e.g., max/min ≤ 3x): compression is structurally necessary to maintain coalition. If threshold is wide (max/min ≤ 5-10x): compression is chosen preference, not structural requirement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_stability_threshold, empirical, 'Coalition stability threshold for wage differentiation').

omega_variable(
    exit_cost_trajectory,
    'As manufacturing moves offshore and union sectors shrink, do exit costs for senior workers increase or decrease? Is compression becoming more extractive as the sector contracts?',
    'Longitudinal tracking of pension portability, skill transferability, and wage differentials for workers exiting union sectors. Comparison of exit costs across decades of sector contraction.',
    'If exit costs rising: suppression is increasing over time, making compression more snare-like. If exit costs stable or falling: suppression may be declining as alternative sectors become viable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_cost_trajectory, empirical, 'How exit costs change as unionized sectors contract').

omega_variable(
    internal_union_pressure,
    'How much internal pressure exists within unions to break compression (high-skill workers advocating for differentiation) and how is it managed or suppressed?',
    'Analysis of union election platforms, grievance data, strike votes, and leadership opposition. Tracking of when compression issues surface as internal union conflict.',
    'If high pressure + heavy suppression: this reveals active enforced extraction within the coalition. If low pressure: high-wage workers have accepted or internalized compression (identity_locked). If negotiated breaks: legitimacy of compression is higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internal_union_pressure, empirical, 'Internal union pressure for wage differentiation').

omega_variable(
    identity_lock_vs_material_trapped,
    'For senior workers in compressed schedules, is their constraint primarily structural (material barriers to exit) or cognitive (identity-fused with union membership and seniority culture)?',
    'Post-union-exit interviews and career transitions; measurement of whether former union members retain seniority-based identity frames and whether they successfully transition to skill-market roles.',
    'If identity-locked: exit_options should be identity_locked rather than trapped — classification shifts to Rope from identity-locked perspective. If purely material: trapped is correct, and exit costs are the primary constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_material_trapped, empirical, 'Whether wage compression constraint is material or identity-based for senior workers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(wage_compression_union_sectors, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wagec_tr_t0, wage_compression_union_sectors, theater_ratio, 0, 0.28).
narrative_ontology:measurement(wagec_tr_t10, wage_compression_union_sectors, theater_ratio, 10, 0.32).
narrative_ontology:measurement(wagec_tr_t20, wage_compression_union_sectors, theater_ratio, 20, 0.35).
narrative_ontology:measurement(wagec_tr_t5, wage_compression_union_sectors, theater_ratio, 5, 0.3).

% Extraction over time
narrative_ontology:measurement(wagec_be_t0, wage_compression_union_sectors, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(wagec_be_t10, wage_compression_union_sectors, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(wagec_be_t20, wage_compression_union_sectors, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(wagec_be_t5, wage_compression_union_sectors, base_extractiveness, 5, 0.41).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(wage_compression_union_sectors, resource_allocation).
narrative_ontology:affects_constraint(wage_compression_union_sectors, skilled_worker_exit_propensity).
narrative_ontology:affects_constraint(wage_compression_union_sectors, union_internal_wage_conflict).
narrative_ontology:affects_constraint(wage_compression_union_sectors, manufacturing_sector_contraction).

% DUAL FORMULATION NOTE:
% Wage compression in unionized sectors can be decomposed into two structurally distinct constraints: (1) wage_floor_coordination (the mechanism that prevents race-to-the-bottom and maintains entry-level protections), and (2) wage_ceiling_extraction (the mechanism that caps high-skill earnings). The coordination function (Rope) is the primary constraint's focus here; the extraction mechanism (Snare from senior worker perspective) is revealed by indexical variation. Separate stories could track each function's evolution independently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(wage_compression_union_sectors, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
