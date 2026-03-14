% ============================================================================
% CONSTRAINT STORY: labor_arbitrage_wage_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_arbitrage_wage_suppression, []).

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
 *   constraint_id: labor_arbitrage_wage_suppression
 *   human_readable: Labor Arbitrage Wage Suppression Through Cross-Border Mobility
 *   domain: labor_economics/political_economy
 *
 * SUMMARY:
 *   Labor arbitrage wage suppression represents a global structural
 *   constraint where capital mobility across borders enables employers to
 *   suppress wages in high-wage jurisdictions by leveraging labor cost
 *   differentials and mobility restrictions. The constraint exhibits a clear
 *   extraction mechanism (employers benefit from wage suppression; domestic
 *   workers bear wage losses) embedded within a coordination function
 *   (efficient allocation of capital and labor across the global economy).
 *   The extractiveness has increased from 0.35 (1990s, early globalization)
 *   to 0.58 (2020s, mature arbitrage markets with entrenched wage gaps).
 *   Theater ratio has risen from 0.32 to 0.48, reflecting increasing
 *   divergence between comparative advantage doctrine (which predicts
 *   universal gains) and empirical wage outcomes (which show concentrated
 *   losses for domestic workers). The constraint demonstrates the core
 *   tangled_rope signature: genuine coordination (multinational employers
 *   allocate capital efficiently, global production networks reduce consumer
 *   costs) alongside systematic asymmetric extraction (wages suppressed in
 *   high-wage jurisdictions, wages in origin countries also suppressed
 *   through the threat of arbitrage). Suppression (0.65) is sustained through
 *   legal barriers to labor mobility (visa restrictions, credential
 *   non-recognition), information asymmetries (workers lack bargaining
 *   transparency), and organizational decline (union membership collapsed in
 *   most high-wage jurisdictions).
 *
 * KEY AGENTS:
 *   - Domestic Workers (High-Wage Jurisdictions): Primary victims (powerless/trapped) — face wage depression, job loss, or forced underemployment with no credible exit paths
 *   - Origin-Country Worker Pool (Low-Wage Jurisdictions): Secondary victims (powerless/trapped) — wages suppressed locally by arbitrage threat; global mobility blocked by visa and credential barriers
 *   - Multinational Employers/Capital Holders: Primary beneficiaries (institutional/arbitrage) — capture efficiency gains and rent extraction from wage suppression; full exit optionality across jurisdictions
 *   - Domestic Labor Organizations (Unions/Sector Councils): Moderate power actors (moderate/constrained) — coordinate worker interests but constrained by membership decline and legal restrictions; also extract through institutional survival mechanisms
 *   - Protectionist/Deglobalization Coalition: Organized political actors (organized/constrained) — coordinate labor grievances and electoral power; also extract through populist narratives and blame-shifting
 *   - Comparative Advantage Doctrine: Institutional doctrine (institutional/arbitrage) — justifies arbitrage as natural/optimal; persists through academic and policymaker consensus despite contradictory wage evidence
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional arrangements (arbitrage regimes, visa restrictions, credential barriers) as immutable laws of capital
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_arbitrage_wage_suppression, 0.58).
domain_priors:suppression_score(labor_arbitrage_wage_suppression, 0.65).
domain_priors:theater_ratio(labor_arbitrage_wage_suppression, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_arbitrage_wage_suppression, extractiveness, 0.58).
narrative_ontology:constraint_metric(labor_arbitrage_wage_suppression, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(labor_arbitrage_wage_suppression, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_arbitrage_wage_suppression, tangled_rope).
narrative_ontology:human_readable(labor_arbitrage_wage_suppression, "Labor Arbitrage Wage Suppression Through Cross-Border Mobility").
narrative_ontology:topic_domain(labor_arbitrage_wage_suppression, "labor_economics/political_economy").

domain_priors:requires_active_enforcement(labor_arbitrage_wage_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_arbitrage_wage_suppression, capital_holders).
narrative_ontology:constraint_beneficiary(labor_arbitrage_wage_suppression, multinational_employers).
narrative_ontology:constraint_victim(labor_arbitrage_wage_suppression, domestic_workers_high_wage_jurisdictions).
narrative_ontology:constraint_victim(labor_arbitrage_wage_suppression, origin_country_labor_supply).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOMESTIC MANUFACTURING WORKER (SNARE) — Faces structural unemployment or wage depression as employers leverage labor arbitrage. Exit costs are prohibitive: retraining takes years, geographic relocation disrupts family and social networks, and alternative sectors are undersupplied. The worker experiences the constraint as immutable — markets force wages down, there is no negotiation, no alternative employment, and no perceived path out. Maximum suppression through job loss threat and wage floors set by global competitive dynamics.
constraint_indexing:constraint_classification(labor_arbitrage_wage_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORIGIN-COUNTRY WORKER POOL (SNARE) — Trapped by immobility barriers (visa restrictions, language barriers, credential non-recognition, family dependency). Despite nominally lower wages being 'attractive,' workers cannot freely exit the low-wage jurisdiction — structural barriers lock them in. Wages in origin country are suppressed by the existence of the arbitrage option (employers threaten offshoring), yet migration is blocked for most. Dual trap: suppressed locally and immobilized globally.
constraint_indexing:constraint_classification(labor_arbitrage_wage_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: MULTINATIONAL EMPLOYER (ROPE) — Experiences the constraint as pure coordination mechanism: labor mobility enables efficient capital allocation and competitive pricing. The employer sees wage suppression as market equilibrium, not extraction. Has full exit optionality (can locate production anywhere, can shift labor sourcing across borders). The constraint solves a genuine coordination problem: how to allocate productive capacity across jurisdictions with different labor costs. Net beneficiary with arbitrage exit — experiences negative effective extraction.
constraint_indexing:constraint_classification(labor_arbitrage_wage_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOMESTIC LABOR ORGANIZATION (TANGLED ROPE) — Constrained by declining membership (workers displaced or demoralized) and legal restrictions on cross-border coordination. But also benefits from the arbitrage constraint through occasional solidarity actions, threat leverage in crisis periods, and negotiation of transition programs. The organization simultaneously coordinates worker interests AND extracts from members through dues and institutional self-perpetuation. Exit is costly but possible through strategic reorientation or mergers. Moderate power with constrained options produces mixed classification.
constraint_indexing:constraint_classification(labor_arbitrage_wage_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: COMPARATIVE ADVANTAGE DOCTRINE (PITON) — The theoretical framing that justifies labor arbitrage (Ricardo/Heckscher-Ohlin) persists despite decades of contradictory evidence on worker outcomes. Economists maintain the doctrine as a self-evident truth; policymakers cite it without testing its assumptions. The intellectual structure is degraded (theater_ratio high) — the doctrine's predictions have not matched labor market data since the 1990s, yet it persists through institutional inertia in universities, development banks, and trade ministries. Theater: theoretical elegance without empirical grounding.
constraint_indexing:constraint_classification(labor_arbitrage_wage_suppression, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PROTECTIONIST/DEGLOBALIZATION COALITION (TANGLED ROPE) — Organized political movement (labor, nationalist, environmental factions) that both coordinates worker grievances AND extracts through populist narrative capture. Has real agency (electoral power, veto positions in governance) but is constrained by international trade agreements and capital mobility. Pursues genuine coordination (wage floors, border labor standards) alongside extractive appeals (scapegoating migrants, nationalism). Moderate power with constrained exit produces tangled classification.
constraint_indexing:constraint_classification(labor_arbitrage_wage_suppression, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — CAPITAL MOBILITY IMMUTABILITY (MOUNTAIN) — From a civilizational/universal perspective, labor arbitrage is a natural law: capital always flows to lowest-cost labor (corrected for productivity); workers cannot collectively resist market forces; wage suppression is an immutable consequence of global capital mobility. This perspective naturalizes the constraint as inherent to capitalism itself. However, the structural data (suppression ≥0.65, organized agents exist, deglobalization coalitions have achieved real policy changes) reveals this as a false summit: the immutability is assumption, not structural reality.
constraint_indexing:constraint_classification(labor_arbitrage_wage_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_arbitrage_wage_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_arbitrage_wage_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_arbitrage_wage_suppression, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_arbitrage_wage_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_arbitrage_wage_suppression, TR),
    TR >= 0.70.

:- end_tests(labor_arbitrage_wage_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Nominal wage losses for domestic workers in affected sectors are substantial (10-40% declines in real manufacturing wages since 1990s); but the extraction is not maximal because: (1) some consumer surplus gains from lower-priced goods flow to all workers as consumers, (2) some high-wage workers benefit through capital returns or relocations, (3) new-economy sectors create partially offsetting employment. The trajectory from 0.35 to 0.58 reflects deepening arbitrage as offshoring matured from manufacturing (1990s-2000s) into services and knowledge work (2010s-2020s). Suppression (0.65): High. Multiple overlapping mechanisms: (1) visa restrictions and credential barriers make labor globally immobile, (2) information asymmetries (workers in origin countries don't know wages offered elsewhere), (3) organizational decline (unions weakened, collective bargaining coverage fell), (4) threat effects (even jobs not offshored face wage depression from offshoring possibility), (5) political economy (capital-friendly governments suppress labor organizing and weaken wage floors). Theater ratio (0.48): Moderate. Comparative advantage doctrine provides intellectual justification, but it is increasingly theatrical — the doctrine predicts universal gains and smooth transitions that have not materialized. The theater comes from: (1) repetition of doctrine without updated empirical testing, (2) framing of structural wage losses as 'adjustment costs' that will be temporary, (3) promise of retraining programs that are inadequately funded and mismatch labor supply, (4) invocation of consumer benefits to offset worker losses.
 *
 * PERSPECTIVAL GAP:
 *   Maximum gap between beneficiary and victim perspectives. Employers experience wage suppression as efficient coordination enabling competitive pricing and consumer welfare gains. Workers experience the same phenomenon as extraction — direct wage loss with no compensating benefit and no exit option. The gap is not measurement disagreement but structural difference: beneficiaries have arbitrage exit (can move production anywhere), victims have trapped exit (cannot relocate or retrain). Labor organizations experience tangled dynamics: they coordinate worker interests but are also constrained by declining membership and legal restrictions. The coalition perspective (deglobalization) shows organized power with constrained options — they can affect electoral outcomes and policy but cannot unilaterally block capital mobility. The false summit (comparative advantage doctrine as natural law) is revealed by noticing that visa restrictions and union decline are policy choices, not laws of nature. If policies changed (visa liberalization, credential recognition, union strength), the arbitrage constraint would shift to a different classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural relationship of each agent to the arbitrage extraction flow. Multinational employers are direct beneficiaries with full arbitrage exit: they profit from the wage differential and can relocate production at low cost. Derived d ≈ 0.15 (beneficiary + arbitrage) yields f(d) ≈ -0.01 → low/negative effective extraction for this agent. Domestic workers are direct victims with trapped exit: they bear full wage suppression cost and cannot relocate or retrain at acceptable cost. Derived d ≈ 0.95 (victim + trapped) yields f(d) ≈ 1.42 → maximum experienced extraction. Origin-country worker pool is also trapped by visa/credential barriers despite nominally 'benefiting' from lower wages: the threat of arbitrage suppresses their local wages, and they cannot exit to higher-wage jurisdictions. Derived d ≈ 0.90 (victim + trapped) yields high f(d). Labor organizations face constrained exit (can organize, can leverage electoral politics, but cannot unilaterally change trade policy): derived d ≈ 0.65 (mixed beneficiary/victim + constrained) yields moderate f(d). Deglobalization coalition has organized power but remains constrained by international trade agreements and capital mobility threats: derived d ≈ 0.60 (constrained + organized) yields moderate f(d). The scope modifier σ(S) for global scope (1.2) amplifies the effective extraction — the arbitrage operates across continental borders, making verification and coordination harder, which increases χ = ε × f(d) × σ(S).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the tangled_rope classification is not evasion but accurate diagnosis: genuine coordination (efficient capital allocation via multinational firms) coexists with systematic asymmetric extraction (wage suppression for trapped workers). The falsest reading is the mountain perspective (comparative advantage as natural law), which the structural data contradicts: visa restrictions, credential barriers, and union decline are policy choices, not immutable constraints. The snare reading (domestic workers) is accurate from their structural position (trapped, no exit). The rope reading (multinational employers) is accurate from their position (net beneficiary, full arbitrage exit). The scaffold reading (deglobalization coalition) is aspirational but constrained by trade agreement lock-in and capital exit threats — whether it represents a real exit path or performative resistance is an open empirical question captured in omega_3. The piton reading (comparative advantage doctrine) is accurate: the intellectual framework persists through theater (repetition without empirical updating) despite contradictory evidence. The mandatrophy is not resolved by choosing one type, but by recognizing that the constraint is genuinely tangled: it requires both continued capital coordination (for real efficiency) and new extraction restraints (wage floors, labor standard enforcement) to move toward sustainable equilibrium.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    arbitrage_barrier_height_threshold,
    'What level of visa restriction, credential non-recognition, or relocation cost would render labor arbitrage economically infeasible vs. merely constraining?',
    'Comparative analysis of sectors with tight labor mobility restrictions (EU intra-mobility vs. US-Mexico vs. Japan-Southeast Asia) and their wage convergence rates; econometric modeling of migration cost elasticity relative to wage differentials',
    'If barriers are truly insurmountable: origin-country workers face absolute immobility (snare confirmed). If barriers are high but penetrable: reframing as constrained exit shifts classification toward tangled_rope. This affects whether the constraint is solvable via migration policy vs. requiring wage floor coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(arbitrage_barrier_height_threshold, empirical, 'Threshold at which labor mobility barriers prevent arbitrage').

omega_variable(
    wage_suppression_attribution_causality,
    'What proportion of observed wage stagnation in high-wage jurisdictions is caused by labor arbitrage vs. technological displacement, union decline, or macroeconomic factors?',
    'Econometric decomposition (Oaxaca-Blinder or similar) controlling for sector, skills, offshoring intensity, automation rates, and union density; comparison of wage trends in tradable vs. non-tradable sectors; international comparison of countries with different arbitrage exposure',
    'If arbitrage accounts for > 40% of wage suppression: the snare classification for domestic workers is structurally sound. If < 20%: wage suppression may be driven primarily by other mechanisms, and arbitrage is a convenient scapegoat (false natural law). Classification shifts from snare toward piton (theatrical blame-shifting).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_suppression_attribution_causality, empirical, 'Proportion of wage suppression attributable to labor arbitrage').

omega_variable(
    deglobalization_coalition_structural_viability,
    'Can deglobalization coalitions sustain coordinated wage-floor policies across multiple jurisdictions without fragmentation or regulatory capture by capital interests?',
    'Historical analysis of previous labor-protectionist movements (1920s-1970s); examination of current regional labor standards frameworks (EU, USMCA, ASEAN); stress-testing of proposed wage coordination mechanisms against capital exit threats and regulatory arbitrage',
    'If coalitions can sustain coordination: scaffold perspective applies (temporary phase with exit path). If coalitions are inherently captured or unstable: snare/tangled_rope persists indefinitely. Affects whether the constraint has a generational sunset or is structural to global capitalism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(deglobalization_coalition_structural_viability, preference, 'Structural viability of coordinated wage-floor coalitions').

omega_variable(
    productivity_adjusted_comparative_advantage,
    'Do nominal wage differences between jurisdictions reflect genuine productivity differences, or do they reflect historical inequality, infrastructure gaps, and credential barriers that would converge if workers had equal access to capital and training?',
    'Analysis of wage gaps controlling for hours worked, capital investment, technology access, and years of training; comparison of identically-trained workers across borders in same firm (intra-firm wage equity audits); longitudinal tracking of wage convergence in sectors with labor mobility (healthcare, tech in EU/Singapore/Australia)',
    'If wage gaps are productivity-driven: comparative advantage doctrine is correct, and arbitrage reflects natural equilibrium (mountain perspective gains credibility). If gaps are historically contingent: arbitrage is extracting rent from artificial inequality, not allocating labor efficiently. Piton perspective confirmed — doctrine naturalizes contingent inequality.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(productivity_adjusted_comparative_advantage, empirical, 'Whether wage differentials reflect productivity or historical inequality').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_arbitrage_wage_suppression, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(laws_tr_t0, labor_arbitrage_wage_suppression, theater_ratio, 0, 0.32).
narrative_ontology:measurement(laws_tr_t10, labor_arbitrage_wage_suppression, theater_ratio, 10, 0.42).
narrative_ontology:measurement(laws_tr_t20, labor_arbitrage_wage_suppression, theater_ratio, 20, 0.48).
narrative_ontology:measurement(laws_tr_t30, labor_arbitrage_wage_suppression, theater_ratio, 30, 0.56).

% Extraction over time
narrative_ontology:measurement(laws_be_t0, labor_arbitrage_wage_suppression, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(laws_be_t10, labor_arbitrage_wage_suppression, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(laws_be_t20, labor_arbitrage_wage_suppression, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(laws_be_t30, labor_arbitrage_wage_suppression, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_arbitrage_wage_suppression, resource_allocation).
narrative_ontology:affects_constraint(labor_arbitrage_wage_suppression, manufacturing_capacity_offshoring).
narrative_ontology:affects_constraint(labor_arbitrage_wage_suppression, skill_premium_divergence).
narrative_ontology:affects_constraint(labor_arbitrage_wage_suppression, electoral_populism_wave).

% DUAL FORMULATION NOTE:
% Labor arbitrage wage suppression is upstream of multiple institutional constraints: offshoring decisions, wage inequality, and populist political backlash. Each downstream constraint has its own extractiveness value reflecting its specific institutional mechanism; the wage suppression story represents the common causal mechanism linking them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
