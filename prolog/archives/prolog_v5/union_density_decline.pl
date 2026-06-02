% ============================================================================
% CONSTRAINT STORY: union_density_decline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_union_density_decline, []).

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
 *   constraint_id: union_density_decline
 *   human_readable: Union Density Decline and Wage Extraction
 *   domain: labor_economics/industrial_relations
 *
 * SUMMARY:
 *   Union density decline in the United States from 35% (1950s) to 10% (2024)
 *   represents a structural transition from coordinated wage-setting to
 *   atomized labor market competition. The constraint exhibits high
 *   suppression (0.72) because worker exit options have systematically
 *   narrowed: right-to-work laws, aggressive employer anti-union campaigns,
 *   NLRB capacity degradation, and outsourcing threats create cascading
 *   barriers to collective organizing. The extractiveness value (0.58)
 *   reflects genuine coordination losses (unions provided workplace safety,
 *   grievance procedures, training structures) alongside wage suppression
 *   asymmetries favoring capital. The theater ratio (0.68) captures the
 *   performative role of residual labor institutions: NLRB reviews, labor
 *   board hearings, and formal grievance procedures persist while enforcement
 *   capacity and outcome enforcement have atrophied. The constraint is
 *   Tangled Rope at base level because capital benefits from genuine
 *   coordination (elimination of union wage premiums, predictability of labor
 *   costs, reduced regulatory friction) alongside extraction. However, the
 *   perspectival gap is profound: powerless atomized workers experience pure
 *   Snare; remaining unionized workers experience mixed Tangled Rope
 *   (protection + vulnerability); capital experiences Rope (coordination
 *   benefit); the regulatory state experiences Piton (degraded institutions);
 *   and organized labor movements see Scaffold (alternative models with
 *   sunset potential).
 *
 * KEY AGENTS:
 *   - Individual non-union workers: Primary victims (powerless/trapped) — face wage suppression, unstable employment, no bargaining power, high exit costs
 *   - Remaining union membership: Secondary victims (organized/constrained) — retain some bargaining power but under asymmetric pressure; constrained by threat of further job loss or decertification
 *   - Capital owners and employers: Primary beneficiaries (institutional/arbitrage) — capture wage suppression, lower regulatory burden, increased flexibility; arbitrage available (offshoring, automation, union avoidance)
 *   - Mid-management class: Mixed actor (institutional/constrained) — benefits from wage compression below union baseline and promotion opportunities, but bears extraction costs (intensified workload, reduced job security, loss of institutional protocols)
 *   - Labor rights advocates and worker centers: Organized agents (organized/mobile) — building alternative organizing models with perceived sunset to current extraction mechanism
 *   - Federal labor regulatory agencies: Institutional degradation (institutional/arbitrage) — maintain performative labor enforcement structures with minimal capacity; see own role as compromised (piton perspective)
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent policy choices (right-to-work laws, Section 14b, NLRB reversals) as inevitable market forces
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(union_density_decline, 0.58).
domain_priors:suppression_score(union_density_decline, 0.72).
domain_priors:theater_ratio(union_density_decline, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(union_density_decline, extractiveness, 0.58).
narrative_ontology:constraint_metric(union_density_decline, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(union_density_decline, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(union_density_decline, tangled_rope).
narrative_ontology:human_readable(union_density_decline, "Union Density Decline and Wage Extraction").
narrative_ontology:topic_domain(union_density_decline, "labor_economics/industrial_relations").

domain_priors:requires_active_enforcement(union_density_decline).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(union_density_decline, capital_owners).
narrative_ontology:constraint_beneficiary(union_density_decline, management_class).
narrative_ontology:constraint_beneficiary(union_density_decline, regulatory_agencies).
narrative_ontology:constraint_victim(union_density_decline, wage_workers).
narrative_ontology:constraint_victim(union_density_decline, labor_collective_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ATOMIZED WORKER (SNARE) — Without union membership, individual workers face barriers to exit: high switching costs, wage-setting unilaterally by employers, no collective bargaining power. The constraint is extraction without meaningful coordination benefit from the worker's perspective. Trapped: cannot organize alone; exit is costly relative to staying in exploitative terms.
constraint_indexing:constraint_classification(union_density_decline, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REMAINING UNION MEMBERSHIP (TANGLED ROPE) — Unions provide genuine coordination (wage negotiation, workplace safety, grievance procedures) alongside extraction pressure from employers (concessions, wage restraint, job losses to outsourcing). Constrained exit: workers could decertify but face retaliation, loss of job protections, and destruction of negotiating capacity. Organized power enables some agency but not full mobility.
constraint_indexing:constraint_classification(union_density_decline, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL CLASS (ROPE) — Benefits from union density decline through lower labor costs and reduced workplace regulation. The constraint is pure coordination: decertifying unions or withholding recognition reduces regulatory overhead and enables wage suppression via labor market competition. Arbitrage exit available: can relocate production, automate, or source from non-union suppliers.
constraint_indexing:constraint_classification(union_density_decline, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MID-MANAGEMENT (TANGLED ROPE) — Mid-managers coordinate between capital and labor: they benefit from union decline via promotion opportunities and wage compression above non-union baseline, but also bear extraction costs (increased workload, erosion of job security as outsourcing intensifies, loss of institutional protocols that protected their position). Constrained: organizational dependence and labor market devaluation of mid-management skills make exit costly.
constraint_indexing:constraint_classification(union_density_decline, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR RIGHTS MOVEMENT (SCAFFOLD) — Organized advocates (worker centers, sectoral bargaining campaigns, card-check legislation efforts) see union density decline as a temporary coordination failure with potential sunset: alternative models (sectoral bargaining like Scandinavia, gig-worker organizing, industry-wide standards) offer pathways to restore labor power without traditional union structure. Mobile exit: these organizations are building parallel institutions. Sunset logic: if sectoral bargaining or worker-power models scale, the current extraction mechanism becomes obsolete.
constraint_indexing:constraint_classification(union_density_decline, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: REGULATORY LABOR STATE (PITON) — Federal and state labor agencies maintain performative institutional roles (NLRB rulings, labor board reviews, enforcement structures) that have largely degraded as capacities atrophied and anti-union legislative hostility intensified. The regulatory state sees itself as diminished: it maintains the machinery of labor adjudication but lacks resources, political support, or will to enforce collective bargaining. Theater ratio is high: labor law enforcement is substantially symbolic.
constraint_indexing:constraint_classification(union_density_decline, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (FALSE SUMMIT) — Market economics frames union density decline as inevitable: competitive labor markets automatically suppress wages toward reservation price, union rents are economically inefficient, and decline is a natural law of labor commodification. This perspective risks naturalizing contingent policy choices (Taft-Hartley Section 14b, right-to-work laws, aggressive NLRB reversals) as market forces. The engine's false summit detector reveals this as naturalization of political choice.
constraint_indexing:constraint_classification(union_density_decline, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(union_density_decline_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(union_density_decline, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(union_density_decline, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(union_density_decline, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(union_density_decline, TR),
    TR >= 0.70.

:- end_tests(union_density_decline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The measure reflects wage suppression relative to counterfactual unionized baseline, estimated at 15-25% wage discount for non-union workers controlling for skill and industry. The value increased from 0.22 (1950) to 0.58 (2024) as union density declined and de-unionized sectors (retail, fast food, logistics, care work) expanded. Extractiveness is not higher because: (1) some coordination benefits were genuinely lost (safety standards, training pathways), so measured extraction overstates pure rent-taking; (2) labor productivity gains partially offset wage suppression; (3) capital faces its own costs (reduced worker stability, higher turnover, loss of institutional knowledge transfer). Suppression (0.72): High. Barriers to worker exit from low-wage atomized conditions include: structural (geographic immobility, credential barriers, family care dependencies), legal (right-to-work laws reduce union organizing capacity), economic (outsourcing threats, plant closures punish unionization attempts), and psychological (union narrative degradation, individualization ideology). Suppression did not increase over time (remained ~0.70 throughout) because barriers were structural from 1950 onward — what changed was union coordination presence that partly overcame barriers. Theater ratio (0.68): High. NLRB processes, labor arbitration hearings, formal grievance procedures, and labor board reviews consume significant resources and time but increasingly yield weak enforcement outcomes. Theater increased from 0.35 (1950s, when unions had enforcement capacity and employers faced real bargaining power) to 0.68 (2024, when formal processes persist but lack enforcement muscle). The performative character is visible in: case backlogs (exceeding 5 years at many regional NLRB offices), weak remedies (back pay without job reinstatement), employer retaliation despite legal prohibition, and minimal frequency of successful union elections relative to organizing attempts.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap exposes the constraint as fundamentally asymmetric rather than universal. The analytical mountain perspective (market forces make decline inevitable) is a false summit — the causality runs through policy choices and institutional design, not inevitability. The snare perspective (atomized workers) is the most structurally vulnerable: no coordination, no exit, maximum extraction. The tangled rope perspectives (unions, mid-managers) show asymmetric extraction with genuine but insufficient coordination. The rope perspective (capital) shows pure coordination benefit. The scaffold perspective shows real alternative pathways. The piton perspective reveals degraded institutional enforcement. No single type captures the constraint — the perspectival presheaf is the answer.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from each agent's structural position. Atomized workers: victim status + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 (maximum experienced extraction). Remaining unions: victim status + constrained exit → d ≈ 0.75 → f(d) ≈ 1.10 (high extraction, some agency). Capital: beneficiary status + arbitrage exit → d ≈ 0.10 → f(d) ≈ -0.05 (negative experienced extraction, pure coordination benefit). Mid-management: mixed beneficiary-victim status + constrained exit → d ≈ 0.55 → f(d) ≈ 0.75 (moderate extraction, mixed benefit). Labor advocates: organized agent + mobile exit → d ≈ 0.40 → f(d) ≈ 0.40 (perceived low extraction; building alternatives). Regulatory state: institutional + arbitrage → d ≈ 0.05 → f(d) ≈ -0.12 (appears beneficiary but actually degraded; piton rather than rope). The chi formula χ = ε × f(d) × σ(S) yields: atomized workers experience χ ≈ 0.58 × 1.42 × 1.0 = 0.82 (severe extraction at national scope); capital experiences χ ≈ 0.58 × (-0.05) × 1.0 = -0.03 (negative extraction, coordination subsidy). This differentiation explains why classification diverges: same base constraint, same ε, but different χ across positions, producing different experienced types.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that union density decline IS a Tangled Rope at the systemic level (genuine coordination function + asymmetric extraction), but perspectival readings range from Snare (atomized workers) to Rope (capital) to Scaffold (labor movements) to Piton (regulatory state). The false summit (analytical mountain) is the key diagnostic: naturalizing decline as market necessity is the mechanism through which the extraction is sustained. The constraint's primary function is coordination (setting wages, establishing standards, creating predictability), but this coordination has been systematically skewed to benefit capital at the expense of labor. The empirical question (causality_direction_ambiguity omega) determines whether decline causes extraction (snare logic) or extraction causes decline (rope logic) — current econometric evidence favors the latter, supporting the rope classification for capital's perspective. Alternative models (sectoral bargaining, worker centers) exist and function elsewhere, so the constraint is not immutable — it is a contestable institutional choice. The theater ratio increase (0.35 to 0.68) indicates that institutional labor regulation is increasingly performative, a pattern consistent with Piton degradation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causality_direction_ambiguity,
    'Does union decline cause wage suppression, or does wage competition cause union decline?',
    'Econometric analysis of state-level right-to-work adoption; instrumental variable estimation using legislative timing; cross-national comparison with sectoral bargaining models',
    'If causality runs decline→suppression: snare classification dominates (extraction mechanism). If causality runs suppression→decline: rope classification (competition coordination) dominates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(causality_direction_ambiguity, empirical, 'Direction of causality between union decline and wage suppression').

omega_variable(
    sectoral_bargaining_feasibility,
    'Can sectoral bargaining (Scandinavian model) be implemented in the US labor market, or are structural factors preventing it?',
    'Comparative institutional analysis; interviews with labor economists and sectoral bargaining practitioners; assessment of employer coordination capacity and worker mobilization in key sectors',
    'If feasible: scaffold sunset is real — alternative coordination exists. If infeasible: scaffold is aspirational — current extraction mechanism has no realistic exit path.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sectoral_bargaining_feasibility, conceptual, 'Whether sectoral bargaining models are feasible alternatives in US context').

omega_variable(
    gig_worker_organizing_viability,
    'Can gig-worker organizing and worker centers achieve labor power equivalent to traditional unions?',
    'Tracking of wage outcomes and employment stability for organized gig workers vs non-organized; cost of organizing per worker; comparison to traditional union organizing costs and outcomes',
    'If viable: scaffold perspective gains credibility — alternative models can restore worker power. If not viable: worker power loss becomes more permanent; snare extraction persists.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(gig_worker_organizing_viability, empirical, 'Whether alternative worker organizing models can achieve traditional union outcomes').

omega_variable(
    regulatory_state_capacity_restoration,
    'Can federal labor regulation capacity be restored through political action, or is NLRB degradation structural?',
    'Analysis of budget allocation trends; assessment of NLRB staffing and case backlogs; legislative history of labor regulation funding; comparison to administrative capacity in peer democracies',
    'If restorable: piton is transitional — regulatory capacity can be rebuilt and enforcement strengthened. If structural: piton becomes permanent institutional feature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_state_capacity_restoration, preference, 'Whether NLRB capacity can be restored through political will').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(union_density_decline, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(udd_tr_t0, union_density_decline, theater_ratio, 0, 0.35).
narrative_ontology:measurement(udd_tr_t20, union_density_decline, theater_ratio, 20, 0.52).
narrative_ontology:measurement(udd_tr_t40, union_density_decline, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(udd_be_t0, union_density_decline, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(udd_be_t20, union_density_decline, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(udd_be_t40, union_density_decline, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(union_density_decline, resource_allocation).
narrative_ontology:affects_constraint(union_density_decline, wage_stagnation).
narrative_ontology:affects_constraint(union_density_decline, income_inequality_growth).
narrative_ontology:affects_constraint(union_density_decline, workplace_safety_deregulation).
narrative_ontology:affects_constraint(union_density_decline, labor_market_precarity).

% DUAL FORMULATION NOTE:
% Union density decline decomposes into multiple structurally distinct constraints: (1) wage-setting coordination failure (ε≈0.30, Tangled Rope), (2) legal/regulatory capacity erosion (ε≈0.55, Piton), (3) worker organizing barriers (ε≈0.45, Snare), (4) alternative model viability (ε≈0.25, Scaffold). These stories share temporal causality but distinct mechanisms. Union density decline is the macro-level phenomenon; the component stories identify which mechanism dominates in which context.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(union_density_decline, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
