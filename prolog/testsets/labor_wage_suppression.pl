% ============================================================================
% CONSTRAINT STORY: labor_wage_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_wage_suppression, []).

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
 *   constraint_id: labor_wage_suppression
 *   human_readable: Labor Wage Suppression Through Fragmented Labor Markets
 *   domain: labor_economics/political_economy
 *
 * SUMMARY:
 *   Labor wage suppression operates through a system of fragmented and
 *   asymmetric labor markets where workers face barriers to geographic,
 *   sectoral, and professional mobility while capital retains unrestricted
 *   mobility across borders. This constraint exhibits the full range of DR
 *   classifications from different structural positions. The same
 *   institutional arrangements — visa regimes, union-busting, outsourcing
 *   threats, professional licensing cartels, and wage-floor regulations with
 *   degraded enforcement — appear as a natural law of capitalist competition
 *   (mountain), pure extraction (snare), coordination mechanisms (rope),
 *   mixed coordination-extraction hybrids (tangled rope), degraded regulatory
 *   institutions (piton), or emergent worker organization (scaffold),
 *   depending on the observer's position within the labor market. The
 *   extractiveness metric has risen from 0.38 to 0.62 over the 30-year
 *   interval, reflecting the acceleration of capital mobility and the
 *   degradation of labor organizing power. The theater_ratio has similarly
 *   increased from 0.32 to 0.58, indicating that nominal labor standards
 *   (minimum wage laws, safety regulations, working-hour restrictions) have
 *   become increasingly performative — compliance theater substitutes for
 *   actual protection as enforcement capacity degrades through underfunding
 *   and regulatory capture.
 *
 * KEY AGENTS:
 *   - Precarious Workers: Primary victim (powerless/trapped) — faces geographic barriers, visa restrictions, family dependencies, and skill-specificity that create exit barriers; bears extraction through suppressed wages with no viable alternatives
 *   - Low-Skill Workers: Secondary victim (moderate/constrained) — faces labor market competition and outsourcing threats; some mobility options exist but are costly
 *   - Multinational Employers: Primary beneficiary (institutional/arbitrage) — captures wage arbitrage across regions and productivity gains; maintains mobility advantage over workers
 *   - Capital Owners: Secondary beneficiary (powerful/arbitrage) — benefits from suppressed labor costs and reduced labor's bargaining power; global capital mobility preserves leverage
 *   - Labor Organizing Coalition: Organized actor (organized/constrained) — international unions, cross-border worker networks, supply-chain solidarity campaigns; developing coordination function while bearing active suppression
 *   - Government Labor Standards Bodies: Institutional actor (institutional/arbitrage) — implements regulations that are increasingly performative; maintains appearance of wage protection while enforcement capacity degrades
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inherent properties of labor markets
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_wage_suppression, 0.58).
domain_priors:suppression_score(labor_wage_suppression, 0.68).
domain_priors:theater_ratio(labor_wage_suppression, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_wage_suppression, extractiveness, 0.58).
narrative_ontology:constraint_metric(labor_wage_suppression, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(labor_wage_suppression, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_wage_suppression, snare).
narrative_ontology:human_readable(labor_wage_suppression, "Labor Wage Suppression Through Fragmented Labor Markets").
narrative_ontology:topic_domain(labor_wage_suppression, "labor_economics/political_economy").

domain_priors:requires_active_enforcement(labor_wage_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_wage_suppression, capital_owners).
narrative_ontology:constraint_beneficiary(labor_wage_suppression, multinational_employers).
narrative_ontology:constraint_victim(labor_wage_suppression, precarious_workers).
narrative_ontology:constraint_victim(labor_wage_suppression, low_skill_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS WORKER (SNARE) — Faces fragmented labor markets, geographic barriers to mobility, visa restrictions, skill-specificity, and family dependencies that create insurmountable exit barriers. Bears extraction through wage suppression with no viable alternative. Lacks organizational capacity to resist. Maximum experienced extraction — powerless and trapped.
constraint_indexing:constraint_classification(labor_wage_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: UNIONIZED SKILLED WORKER (SNARE) — Despite some organizational power and constrained (rather than trapped) exit options, still experiences significant extraction. Union membership provides limited leverage against capital's mobility and global labor arbitrage. Career switching and geographic relocation are costly but possible. Suppression remains high due to threat of outsourcing and wage competition from lower-cost regions.
constraint_indexing:constraint_classification(labor_wage_suppression, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MULTINATIONAL EMPLOYER (ROPE) — Experiences the constraint as pure coordination: managing a global workforce requires incentive structures, credible wage-setting mechanisms, and supply-chain integration. The employer benefits from the fragmented labor market structure through arbitrage (wage differentials between regions) and mobility (ability to relocate production). Net beneficiary — extraction runs toward this agent.
constraint_indexing:constraint_classification(labor_wage_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR ORGANIZING COALITION (TANGLED ROPE) — International labor organizations, cross-border worker networks, and supply-chain solidarity campaigns have developed genuine coordination functions (sharing information, coordinating wage demands across regions) while simultaneously bearing extraction costs (legal repression, blacklisting, economic vulnerability during strikes). Active enforcement required — organizing is actively suppressed. See sunset potential through regulatory momentum for global labor standards and living-wage requirements.
constraint_indexing:constraint_classification(labor_wage_suppression, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MINIMUM WAGE REGULATION SYSTEM (PITON) — Minimum wage laws and labor standards have become largely performative: nominal minimum wages fail to track inflation or productivity, enforcement capacity is degraded through regulatory capture and underfunding, and legal exemptions (gig economy, informal sector) have eroded coverage. The regulatory system persists through institutional inertia despite reduced functional verification of wage floors. Theater ratio elevated because compliance theater (posting minimum wage notices) substitutes for actual wage protection.
constraint_indexing:constraint_classification(labor_wage_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some wage compression is inherent to competitive labor markets: workers always compete with each other for scarce jobs, and the equilibrium price of labor is determined by supply and demand. This perspective sees wage suppression as an immutable property of capitalist markets themselves. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that the 'inherent to markets' framing naturalizes what is actually a contingent institutional arrangement (visa regimes, union-busting, capital mobility restrictions on labor, information asymmetries) enforced through suppression mechanisms.
constraint_indexing:constraint_classification(labor_wage_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_wage_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_wage_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_wage_suppression, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_wage_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_wage_suppression, TR),
    TR >= 0.70.

:- end_tests(labor_wage_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and rising. The original extraction value of 0.38 reflects the genuine coordination function — wage-setting mechanisms do solve the problem of matching workers to jobs. But extractiveness has risen to 0.62 as capital mobility has accelerated and labor organizing capacity has degraded, increasing the asymmetry between what workers earn and what their productivity generates. The 20-point increase over the interval reflects ongoing rent-seeking layered onto coordination. Suppression (0.68): High. Multiple overlapping barriers constrain worker exit: visa restrictions prevent international mobility, professional licensing creates cartels, family dependencies reduce geographic mobility, skill-specificity locks workers into declining sectors, and the threat of outsourcing constrains wage demands. Informal employment and gig economy growth have further reduced worker protections. Theater ratio (0.55): Moderate and rising. Minimum wage laws, labor safety standards, and maximum hour regulations have become increasingly performative. Nominal minimum wages fail to track inflation, enforcement budgets have been cut, and legal exemptions (gig workers, informal sector, agricultural workers) have expanded to cover majority of growth. Compliance posters substitute for actual wage protection — the ritual persists while the function degrades.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates classification diversity across structural positions. The precarious worker and unionized skilled worker both classify as Snare but with different power/exit combinations — the precarious worker experiences maximum extraction while trapped, the unionized worker retains some leverage through organization and constrained exit. The multinational employer sees Rope — managing global labor supply is a genuine coordination problem, and the employer benefits from the fragmented structure through arbitrage. The labor organizing coalition sees Tangled Rope — they have developed real coordination functions (cross-border information sharing, coordinated wage demands) while bearing extraction costs (legal suppression, blacklisting). The minimum wage regulation system appears as Piton — institutions that once functioned now persist through inertia with degraded verification. The civilizational analytical observer risks seeing Mountain (wage suppression as inherent to labor markets) — but the structural data reveals this as naturalization of contingent institutional arrangements enforced through suppression mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from structural power, exit options, and beneficiary/victim status. Precarious workers with trapped exit options and victim status experience maximum d ≈ 0.95, producing high f(d) ≈ 1.42 — their experience of extractiveness is amplified by powerlessness. Multinational employers with arbitrage exit and beneficiary status experience minimum d ≈ 0.05, producing negative f(d) ≈ -0.12 — extraction flows toward them, reducing their experienced cost. Unionized workers at moderate power with constrained exit and victim status experience mid-range d ≈ 0.70, producing f(d) ≈ 1.05 — significant but not maximal extraction. Labor organizing coalitions with organized power and constrained exit experience d ≈ 0.55, producing f(d) ≈ 0.75 — mixed extraction and coordination function. The global scope σ(S) = 1.2 amplifies extractiveness: wage suppression operates across borders and affects workers at all skill levels everywhere, making verification and resistance more difficult.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by demonstrating that Snare and Tangled Rope are the stable classifications from victim perspectives, while Rope and Piton are stable from beneficiary/regulatory perspectives. The Mountain classification is revealed as a false summit — the 'inherent to markets' framing naturalizes what is actually enforced extraction. The analytical observer faces the oracle gap (U₄ paradox): recognizing wage suppression as structural requires adopting victim perspectives that reveal mechanisms the beneficiary perspective obscures. The labor organizing coalition represents the partial escape route — by elevating power from powerless to organized, workers can shift classification from Snare (pure extraction) to Tangled Rope (mixed coordination-extraction), creating leverage for negotiation. This is not a stable equilibrium but a dynamic oscillation: capital responds to worker power by offshoring production or casualizing employment, which re-collapses organization and re-establishes Snare conditions. The measurements show extractiveness rising despite nominal labor standards — evidence that theater_ratio growth indicates regulatory capture and degradation rather than functional protection.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_contestation_vs_extraction,
    'Is wage suppression an outcome of competitive market processes or an extraction mechanism actively enforced against worker resistance?',
    'Historical analysis of wage trends relative to productivity; documentation of explicit anti-union policies, strike-breaking, and visa restriction enforcement; comparison of wage levels in markets with strong labor organization vs weak organization controlling for productivity',
    'If market outcome: classification shifts toward Rope from more perspectives. If active extraction: classification confirms Snare from victim perspectives and reveals enforcement mechanisms.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_contestation_vs_extraction, empirical, 'Whether wage suppression is competitive outcome or active extraction').

omega_variable(
    labor_mobility_barriers_structural_vs_policy,
    'Are the barriers to worker mobility primarily structural (skill-specificity, family ties, geographic distance) or policy-enforced (visa restrictions, professional licensing, capital controls)?',
    'Decomposition of mobility barriers; analysis of wage differentials before and after policy changes (visa regime liberalization, professional credential reciprocity); comparison of mobility rates between workers with vs without policy barriers',
    'If primarily structural: suppression floor is higher, classification stable. If primarily policy-enforced: suppression is deliberate mechanism, exit options upgrade from trapped to constrained for many workers, perspectives shift toward Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_mobility_barriers_structural_vs_policy, empirical, 'Structural vs policy-enforced labor mobility barriers').

omega_variable(
    global_wage_floor_emergence,
    'Are rising labor standards and wage-floor regulations in developing economies evidence of constraint degradation (Piton trajectory) or emergence of genuine coordinating institutions?',
    'Time-series analysis of minimum wage enforcement effectiveness; documentation of regulatory capture dynamics; comparison of nominal vs real wage floors; assessment of coverage gaps (informal sector, migrant workers, gig workers)',
    'If genuine coordination emerging: scaffold perspective validated, sunset clause timeline strengthens. If performative degradation: piton classification confirmed, theater_ratio elevation continues.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(global_wage_floor_emergence, empirical, 'Whether labor standards regulations are functional or performative').

omega_variable(
    collective_action_threshold_for_powerless_workers,
    'At what scale of organization can precarious workers transition from powerless/trapped to organized/constrained? Is the threshold empirically achievable?',
    'Case study analysis of successful cross-border worker organizing; documentation of coalition formation and coordination mechanisms; assessment of necessary resources and legal conditions for mobilization',
    'If threshold achievable with current resources: scaffold/tangled rope perspectives become more realistic, power upgrade pathway exists. If threshold structurally unachievable: powerless/trapped classification persists, snare becomes more stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(collective_action_threshold_for_powerless_workers, empirical, 'Collective action threshold for precarious worker organization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_wage_suppression, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lwsup_tr_t0, labor_wage_suppression, theater_ratio, 0, 0.32).
narrative_ontology:measurement(lwsup_tr_t10, labor_wage_suppression, theater_ratio, 10, 0.45).
narrative_ontology:measurement(lwsup_tr_t20, labor_wage_suppression, theater_ratio, 20, 0.55).
narrative_ontology:measurement(lwsup_tr_t30, labor_wage_suppression, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(lwsup_be_t0, labor_wage_suppression, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(lwsup_be_t10, labor_wage_suppression, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(lwsup_be_t20, labor_wage_suppression, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(lwsup_be_t30, labor_wage_suppression, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_wage_suppression, resource_allocation).
narrative_ontology:affects_constraint(labor_wage_suppression, labor_market_segmentation).
narrative_ontology:affects_constraint(labor_wage_suppression, union_busting_mechanisms).
narrative_ontology:affects_constraint(labor_wage_suppression, visa_restriction_regimes).

% DUAL FORMULATION NOTE:
% Wage suppression decomposes into three structurally distinct constraints: labor market segmentation (ε≈0.35, creates barriers between segments), union-busting (ε≈0.65, active extraction through legal/violent suppression), and visa restrictions (ε≈0.52, artificial scarcity creating extraction). This story models the aggregate effect; specific causal mechanisms are isolated in downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(labor_wage_suppression, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
