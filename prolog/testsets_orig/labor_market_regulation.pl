% ============================================================================
% CONSTRAINT STORY: labor_market_regulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_market_regulation, []).

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
 *   constraint_id: labor_market_regulation
 *   human_readable: Labor Market Regulation and Wage Extraction
 *   domain: economic_policy/labor_relations
 *
 * SUMMARY:
 *   Labor market regulation creates a structural tension between genuine
 *   coordination functions (standardized hiring practices, minimum safety
 *   standards, transparency requirements) and asymmetric extraction
 *   mechanisms (credential gatekeeping, weak wage-floor enforcement,
 *   monopsony power concentration). The constraint exhibits tangled rope
 *   structure at the analytical level: regulation coordinates labor supply
 *   and demand while simultaneously protecting incumbent employers and
 *   credentialed workers from competitive pressure, extracting welfare from
 *   precarious workers and market entrants. Theater_ratio has risen from 0.48
 *   to 0.64 over the interval, reflecting the divergence between nominal
 *   regulatory scope (comprehensive wage/hours/safety rules) and actual
 *   enforcement capacity (stagnant inspector budgets, rising non-compliance
 *   rates). Base extractiveness has increased from 0.38 to 0.52, indicating
 *   that the coordination function has degraded relative to extraction — wage
 *   floors are announced but poorly enforced; licensing requirements
 *   proliferate without safety justification. The constraint's future depends
 *   on whether sectoral collective bargaining can replace state regulation
 *   (scaffold sunset) or whether the current hybrid persists through
 *   institutional inertia (piton trajectory).
 *
 * KEY AGENTS:
 *   - Precarious Workers: Primary victim (powerless/trapped) — lack credentials and geographic mobility; trapped in low-wage segments with weak regulatory protection
 *   - Market Entrants: Secondary victim (moderate/constrained) — face credential accumulation barriers and network-based hiring discrimination; can exit but at high opportunity cost
 *   - Incumbent Employers: Primary beneficiary (institutional/arbitrage) — capture stable workforce at regulated wage floor with exit available via offshoring; benefit from credential gatekeeping
 *   - Regulatory Agencies: Secondary beneficiary (institutional/arbitrage) — budget-dependent on wage regulation enforcement; incentivized to maintain regulatory scope
 *   - Labor Union Coalition: Organized agents (organized/constrained) — advocate for replacement of state regulation with sectoral bargaining; have partial exit via alternative coordination mechanisms
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangement as inherent to labor markets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_market_regulation, 0.52).
domain_priors:suppression_score(labor_market_regulation, 0.58).
domain_priors:theater_ratio(labor_market_regulation, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_market_regulation, extractiveness, 0.52).
narrative_ontology:constraint_metric(labor_market_regulation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(labor_market_regulation, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_market_regulation, tangled_rope).
narrative_ontology:human_readable(labor_market_regulation, "Labor Market Regulation and Wage Extraction").
narrative_ontology:topic_domain(labor_market_regulation, "economic_policy/labor_relations").

domain_priors:requires_active_enforcement(labor_market_regulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_market_regulation, incumbent_employers).
narrative_ontology:constraint_beneficiary(labor_market_regulation, regulatory_agencies).
narrative_ontology:constraint_victim(labor_market_regulation, precarious_workers).
narrative_ontology:constraint_victim(labor_market_regulation, market_entrants).
narrative_ontology:constraint_victim(labor_market_regulation, wage_growth_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (SNARE) — Trapped by credential requirements, geographic immobility, and absence of viable alternatives. Minimum wage floors exist nominally but enforcement is weak; licensing restrictions prevent lateral job mobility; regulatory compliance costs are shifted to worker schedules and unpaid training. No exit available within biographical timeframe. Maximum experienced extraction.
constraint_indexing:constraint_classification(labor_market_regulation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MARKET ENTRANT (TANGLED ROPE) — Constrained by credential accumulation requirements and network barriers. Regulation provides genuine coordination (standardized hiring practices, transparency requirements for job postings, apprenticeship pathways) alongside asymmetric extraction (entry wages depressed by oversupply managed through credentialing, opportunity cost of credential accumulation borne by entrant). High suppression but not total — some pathways exist, coordination mechanisms are functional.
constraint_indexing:constraint_classification(labor_market_regulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INCUMBENT EMPLOYER (ROPE) — Benefits from regulation as coordination mechanism: standardized wage floors, credential requirements, and non-compete enforcement reduce recruitment uncertainty and lock in workforce stability. Exit via offshoring or automation available but costly. Net beneficiary — regulation solves workforce coordination problems at scale.
constraint_indexing:constraint_classification(labor_market_regulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: LABOR UNION COALITION (SCAFFOLD) — Organized agents (unions, worker advocacy organizations) see regulation as a temporary scaffolding mechanism with sunset: collective bargaining, sectoral agreements, and portable benefits systems are building alternative coordination pathways that reduce dependence on state-mandated wage floors and licensing cartels. Regulation has high theater (performative minimum-wage announcements without enforcement) but union strategy anticipates replacement with negotiated standards. Sunset estimated at 20-30 years if sectoral bargaining expands.
constraint_indexing:constraint_classification(labor_market_regulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY ADMINISTRATION (PITON) — Labor market regulation persists through institutional inertia despite degraded function. Minimum wage enforcement is weak (compliance costs exceed penalties); licensing boards are captured by incumbents; wage-and-hour divisions lack capacity to investigate complaints. The regulatory apparatus maintains theater (annual updates to wage schedules, licensing renewals) while primary function (protecting worker welfare, enforcing wage standards) has atrophied. Piton classification reflects high theater_ratio and low functional output.
constraint_indexing:constraint_classification(labor_market_regulation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some wage regulation is inherent to coordinating labor supply and demand: without baseline standards (maximum hours, minimum safety), markets produce race-to-the-bottom dynamics. This perspective sees labor regulation as an immutable prerequisite to stable labor markets. However, this naturalizes what is actually a contingent institutional arrangement — the specific form of regulation (state-mandated floors without enforcement capacity) is not inherent; alternatives (sectoral bargaining, portable benefits, cooperative ownership) coordinate labor markets differently. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(labor_market_regulation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_market_regulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_market_regulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_market_regulation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_market_regulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_market_regulation, TR),
    TR >= 0.70.

:- end_tests(labor_market_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Labor market regulation does create coordination benefits (hiring clarity, workplace standards, wage floors prevent race-to-the-bottom) but the coordination functions have degraded over the interval while extraction mechanisms have strengthened. Wage floor enforcement requires 1 inspector per 10,000 workers but most jurisdictions employ 1 per 50,000+, rendering nominal floors largely performative. Licensing requirements have expanded beyond safety-relevant occupations into low-skill sectors (security guards, florists in some jurisdictions), indicating rent-seeking rather than protection. The extractiveness value reflects this asymmetry: genuine coordination exists, but extraction exceeds pure coordination cost. Suppression (0.58): Moderate-high. Barriers to worker mobility include non-compete clauses (enforceable in many jurisdictions despite harm evidence), licensing reciprocity gaps between states, credential requirements that exceed functional skill needs, and employer collusion on wage suppression (documented in silicon valley, fast food). These barriers are not absolute (workers can relocate, pursue retraining) but the costs are significant. Theater_ratio (0.64): High. Regulatory theater is substantial: minimum wage announcements receive media coverage but enforcement is token (penalties < wages owed); licensing boards conduct renewals but don't investigate complaints; wage-and-hour divisions issue reports on violations but lack resources to collect damages. The theater has increased because the gap between announced regulation and actual enforcement has widened — wages have stagnated despite regulatory scope expansion, signaling performative rather than functional enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The precarious worker and incumbent employer experience completely different constraints from the same regulatory architecture. The worker experiences snare: minimum wage floors exist but are unenforced; licensing prevents job mobility; credential costs are borne individually. The employer experiences rope: regulation provides stable, predictable workforce; wage floors set floor for all competitors (eliminating race-to-the-bottom); credential gatekeeping reduces recruitment costs. The market entrant experiences tangled rope: regulation enables formal pathways (apprenticeships, credential programs) but extraction occurs through credential costs and wage depression during entry period. The organized labor coalition experiences scaffold: recognizing that state regulation has degraded and anticipating replacement with sectoral bargaining (unions + employers at industry level negotiating portable benefits, mobility standards, wage scales). The regulatory administration experiences piton: maintaining performative enforcement and regulation expansion while primary function (wage protection, labor mobility) has atrophied. The analytical observer risks seeing mountain (labor regulation inherent to any market economy) but the structural data shows this is false — alternative coordination mechanisms (sectoral bargaining, cooperative hiring networks, skills guilds) coordinate labor markets without the same extraction mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality differs sharply by agent position. Precarious workers (victim + trapped) derive d ≈ 0.95 → high f(d) → experience χ amplified by scope. Market entrants (victim + constrained) derive d ≈ 0.65-0.75 depending on sector and credential path. Incumbent employers (beneficiary + arbitrage) derive d ≈ 0.05-0.15 → low or negative f(d) → regulation reduces their costs (coordination of workforce, predictable wage structure, reduced turnover). Labor unions (organized + constrained) derive d ≈ 0.40-0.50 depending on whether sectoral bargaining options are available. The analytical observer (neutral + analytical) derives d ≈ 0.72, producing the perspectival gap: beneficiaries see rope (coordination solves their problem), trapped agents see snare (extraction without exit), organized agents see scaffold with sunset, degraded institutions see piton. No single type captures the full constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that labor market regulation IS genuinely a hybrid — coordination and extraction are structurally entangled. The regulation that solves the incumbent employer's workforce coordination problem (stable supply, predictable wage) creates extraction mechanisms for precarious workers (wage suppression, credential gatekeeping, enforcement gaps). No pure-coordination alternative exists that delivers coordination benefits without also creating extraction risks. The scaffold perspective (union coalition) claims a sunset path via sectoral bargaining, which would disaggregate wage-setting from state regulation and ground it in employer-union negotiation. If sectoral bargaining expands, the current regulation degrades from tangled rope to piton (maintained through inertia only). The mandatrophy is not resolved by proving one type is 'correct' but by mapping how the classification changes as institutional conditions shift — if sectoral bargaining remains marginal, tangled rope persists; if it expands, state regulation becomes piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_capacity_gap,
    'Is the extractiveness gap between nominal wage floors and actual worker compensation driven by enforcement failure or by rational regulatory design?',
    'Comparative analysis of enforcement-to-violation ratios across jurisdictions; cost-benefit analysis of full enforcement vs. selective enforcement; historical correlation between enforcement investment and wage compliance',
    'If enforcement failure: regulatory scope should expand (higher effective suppression). If rational design: extraction is structural feature of regulation (higher baseline extractiveness required). Classification shifts if enforcement capacity is revealed as intentionally constrained.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_capacity_gap, empirical, 'Whether wage floor enforcement gap is intentional or incidental').

omega_variable(
    credential_rent_vs_worker_protection,
    'Do occupational licensing requirements primarily protect public safety or create economic rents by restricting labor supply?',
    'Cross-jurisdiction licensing variation analysis; correlation between licensing stringency and wage premiums vs. safety outcomes; worker mobility and income stability under different licensing regimes',
    'If primarily rents: licensing is pure extraction mechanism (snare classification for entrants appropriate). If primarily safety: licensing has genuine coordination function (tangled rope classification appropriate). If mixed: ratio determines whether rope or snare dominates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credential_rent_vs_worker_protection, empirical, 'Whether licensing protects safety or restricts supply').

omega_variable(
    sectoral_bargaining_viability,
    'Can sectoral collective bargaining (union-employer agreements covering entire industries) provide comparable labor protections and wage coordination at lower extraction cost than state regulation?',
    'Comparative analysis of wage growth, worker mobility, job quality, and enforcement mechanisms in sectoral bargaining systems (German model, Nordic model) vs. state-regulated systems; institutional preconditions for sectoral bargaining expansion',
    'If viable: scaffold sunset is structural and realistic (regulation would be replaced by bargaining frameworks). If not viable: regulation persists not as temporary scaffold but as permanent necessity (reclassify toward piton or persistent tangled rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sectoral_bargaining_viability, empirical, 'Whether sectoral bargaining can replace state wage regulation').

omega_variable(
    wage_floor_behavioral_incidence,
    'When state-mandated wage floors are implemented without enforcement, who bears the extraction cost: workers (foregone employment), employers (reduced hiring), or consumers (higher prices)?',
    'Empirical labor supply elasticity analysis; employment effects of minimum wage changes; pass-through of wage floors to prices in competitive vs. concentrated markets',
    'If workers bear cost: extraction is borne by precarious agents (snare classification justified). If employers bear cost and pass to consumers: extraction is diffused. If employers reduce hiring: extraction takes form of opportunity cost (affects entrants more than incumbents). Distribution of incidence determines who the true victim group is.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wage_floor_behavioral_incidence, empirical, 'Incidence of unenforced wage floors on workers, employers, and consumers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_market_regulation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(labor_tr_t0, labor_market_regulation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(labor_tr_t10, labor_market_regulation, theater_ratio, 10, 0.58).
narrative_ontology:measurement(labor_tr_t20, labor_market_regulation, theater_ratio, 20, 0.64).

% Extraction over time
narrative_ontology:measurement(labor_be_t0, labor_market_regulation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(labor_be_t10, labor_market_regulation, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(labor_be_t20, labor_market_regulation, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_market_regulation, resource_allocation).
narrative_ontology:affects_constraint(labor_market_regulation, occupational_licensing_supply_restriction).
narrative_ontology:affects_constraint(labor_market_regulation, monopsony_wage_suppression).
narrative_ontology:affects_constraint(labor_market_regulation, sectoral_collective_bargaining_viability).

% DUAL FORMULATION NOTE:
% Labor market regulation decomposes into multiple structurally distinct constraints: (1) wage floor enforcement (ε ≈ 0.52, tangled rope), (2) occupational licensing (ε ≈ 0.68, snare), (3) non-compete enforceability (ε ≈ 0.61, snare). This story models the aggregate regulation; linked constraints model specific mechanisms. Each mechanism has different beneficiary/victim distributions and different sunset conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(labor_market_regulation, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
