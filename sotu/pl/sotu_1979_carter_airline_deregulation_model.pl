% ============================================================================
% CONSTRAINT STORY: sotu_1979_carter_airline_deregulation_model
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1979_carter_airline_deregulation_model, []).

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
 *   constraint_id: sotu_1979_carter_airline_deregulation_model
 *   human_readable: Airline Deregulation as Inflation-Fighting Market Liberation (Carter 1979)
 *   domain: regulatory/economic_policy
 *
 * SUMMARY:
 *   President Carter's 1979 SOTU address frames airline deregulation as a
 *   proof-of-concept for reducing inflation by removing government obstacles
 *   to market competition. The Airline Deregulation Act of 1978 (implemented
 *   1979-1984) eliminates the Civil Aeronautics Board's control over routes
 *   and pricing, shifting allocation from regulatory discretion to
 *   competitive market mechanisms. The constraint exhibits the structure of a
 *   Tangled Rope at the base level: genuine coordination function (market
 *   mechanism allocates capacity to high-demand routes more efficiently than
 *   regulatory assignment) combined with asymmetric extraction (concentrated
 *   benefits for major carriers and price-sensitive urban consumers,
 *   concentrated costs for rural communities and regional carriers). The
 *   deregulation model is proposed as a template for railroads, buses, and
 *   trucking — suggesting the constraint operates as institutional theory
 *   that can be deployed across sectors. The measurement trajectory shows
 *   theater_ratio declining from 0.62 (initial regulatory skepticism phase)
 *   to 0.35 (market mechanisms fully activated), while extractiveness rises
 *   from 0.25 (promise phase) to 0.58 (consolidation phase), indicating that
 *   as performative regulation recedes, material extraction mechanisms
 *   accumulate.
 *
 * KEY AGENTS:
 *   - Major Established Airlines (institutional/arbitrage): Primary beneficiary — network scale and capital enable competitive advantage in deregulated pricing; capture profit from rationalization of routes
 *   - Rural Communities (powerless/trapped): Primary victim — abandoned routes increase travel costs and reduce service frequency; cannot restore regulation or compel service
 *   - Regional Carriers and Small Airlines (powerless/trapped): Primary victim — cannot compete with major carriers on price or capacity; driven to bankruptcy or acquisition
 *   - Airline Workers and Labor Unions (moderate/constrained): Secondary victim — wage pressure from labor market competition; benefit from industry growth but lose bargaining power
 *   - Price-Conscious Urban Consumers (powerful/arbitrage): Primary beneficiary — real price declines on high-volume routes; benefit from competitive choice
 *   - Federal Regulatory Apparatus (organized/constrained): Institutional victim of planned dismantling — losing authority and bureaucratic function
 *   - Deregulation Coalition (organized/constrained): Beneficiary agent — ideological advocates, think tanks, market economists advancing deregulation across sectors
 *   - Analytical Observer (analytical/analytical): Sees constraint as simultaneously coordination, extraction, and ideological naturalization
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1979_carter_airline_deregulation_model, 0.52).
domain_priors:suppression_score(sotu_1979_carter_airline_deregulation_model, 0.48).
domain_priors:theater_ratio(sotu_1979_carter_airline_deregulation_model, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1979_carter_airline_deregulation_model, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1979_carter_airline_deregulation_model, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1979_carter_airline_deregulation_model, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1979_carter_airline_deregulation_model, tangled_rope).
narrative_ontology:human_readable(sotu_1979_carter_airline_deregulation_model, "Airline Deregulation as Inflation-Fighting Market Liberation (Carter 1979)").
narrative_ontology:topic_domain(sotu_1979_carter_airline_deregulation_model, "regulatory/economic_policy").

domain_priors:requires_active_enforcement(sotu_1979_carter_airline_deregulation_model).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1979_carter_airline_deregulation_model, major_airlines).
narrative_ontology:constraint_beneficiary(sotu_1979_carter_airline_deregulation_model, price_conscious_consumers).
narrative_ontology:constraint_beneficiary(sotu_1979_carter_airline_deregulation_model, financial_markets).
narrative_ontology:constraint_victim(sotu_1979_carter_airline_deregulation_model, regional_carriers).
narrative_ontology:constraint_victim(sotu_1979_carter_airline_deregulation_model, rural_communities).
narrative_ontology:constraint_victim(sotu_1979_carter_airline_deregulation_model, airline_workers).
narrative_ontology:constraint_victim(sotu_1979_carter_airline_deregulation_model, public_sector_employment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL COMMUNITIES (SNARE) — Trapped in a market deregulation regime where unprofitable routes are abandoned. No exit option: cannot restore regulation once dismantled, cannot compel carriers to serve non-viable markets, cannot organize sufficient demand to attract competition. Bears full extraction cost of route abandonment and price increases for remaining service. Theater-minimal — the market mechanism is functionally real, not performative.
constraint_indexing:constraint_classification(sotu_1979_carter_airline_deregulation_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL CARRIERS (SNARE) — Trapped in direct competition with major carriers without coordination barriers. Unable to exit (cannot return to regulation), unable to organize (antitrust prevents coordination), unable to compete (lack capital and network scale). Experience pure extraction as major carriers undercut prices, cherry-pick profitable routes, and exploit superior logistics networks. Suppression is structural: bankruptcy is the exit mechanism, not choice.
constraint_indexing:constraint_classification(sotu_1979_carter_airline_deregulation_model, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: AIRLINE WORKERS (TANGLED ROPE) — Constrained by competitive pressure on labor costs once route/price regulation ends. Experience both: coordination benefit (increased traffic growth enables some hiring) AND extraction (wage pressure, job insecurity, loss of bargaining power against competition). Career costs to leaving sector during transition are high; some benefit from industry growth but unequally distributed. Suppression moderate: can organize locally but face coordination problems and capital mobility of employers.
constraint_indexing:constraint_classification(sotu_1979_carter_airline_deregulation_model, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MAJOR CARRIERS (ROPE) — Experience deregulation as pure coordination benefit. Network scale, capital access, and existing route dominance create competitive advantages in deregulated environment. Can arbitrage pricing across routes, rationalize unprofitable segments, and deploy capacity strategically. Experience is coordination (enabling efficient allocation) with minimal extraction — benefits flow to this agent. Can exit (have capital to adapt, can shift routes and pricing), but exit is unnecessary because the regime benefits them.
constraint_indexing:constraint_classification(sotu_1979_carter_airline_deregulation_model, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: URBAN PRICE-SENSITIVE CONSUMERS (ROPE) — Benefit from competition on high-volume routes (NYC-LA, Chicago-DC, major hub pairs). Price declines on profitable routes are real and significant. Experience is coordination: the market mechanism solves the problem of allocating capacity to where demand is highest. Suppression minimal — have alternatives (driving, Amtrak) and information (can compare prices). Exit is costless. Beneficiary position is clear.
constraint_indexing:constraint_classification(sotu_1979_carter_airline_deregulation_model, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: FEDERAL REGULATORY APPARATUS (SCAFFOLD) — The constraint is explicitly designed to dismantle this agent. Deregulation is a sunset mechanism for CAB authority and related regulatory function. Experiences high suppression (cannot prevent dismantling once authorized) but also sees functional decline (theater: regulation increasingly perceived as theater). Constrained exit — regulatory apparatus cannot restore its own authority without legislative reversal. Beneficiaries of the apparatus (regulators with agency prestige, existing carriers with protected routes) face removal. Theater theater_ratio high in planning phase (ideological commitment to 'deregulation is liberation') but operational theater low once mechanisms activate.
constraint_indexing:constraint_classification(sotu_1979_carter_airline_deregulation_model, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: DEREGULATION COALITION (TANGLED ROPE) — Organized actors (think tanks, consumer advocates, free-market ideologues) perceive the constraint as a coordination solution: removing regulatory bottlenecks enables efficient pricing and route allocation. Both coordination (markets work) and asymmetric benefit (concentrated profits for major carriers vs distributed consumer savings across price-sensitive urban riders). Constrained because the coalition's power depends on ideological legitimacy and electoral alignment; cannot arbitrage the coalition logic itself. Theater moderate — genuine belief in market efficiency, not purely performative.
constraint_indexing:constraint_classification(sotu_1979_carter_airline_deregulation_model, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (PITON) — From civilizational distance, deregulation appears as a constraint whose primary function (inflation control via market competition) was partially achieved but whose secondary effects (industry consolidation, route abandonment, labor cost pressure) accumulated into a new regulatory need (re-intervention on safety, environmental standards, labor rights). The ideology persists ('markets self-regulate') even as the outcomes demonstrate extraction mechanisms. Theater high in retrospect: the deregulation narrative sustained itself through selective attribution (credit price declines to deregulation, blame route abandonment on market realities) despite causation being mixed.
constraint_indexing:constraint_classification(sotu_1979_carter_airline_deregulation_model, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 9: UNIVERSALIST NATURAL LAW (MOUNTAIN) — From a universal timescale and domain-agnostic position, the constraint could appear as a natural law: 'Regulation always creates rents; removing regulation always increases efficiency.' This universalist claim treats the specific institutional context (airline industry with natural monopoly characteristics on rural routes, capital-intensive entry, established networks) as irrelevant — the deregulation principle is asserted as universal. However, the structural data contradicts this: identifiable beneficiaries exist (major carriers, urban consumers), identifiable victims exist (rural communities, regional carriers), and the suppression and extraction metrics are non-zero. Engine will classify this as a false summit.
constraint_indexing:constraint_classification(sotu_1979_carter_airline_deregulation_model, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1979_carter_airline_deregulation_model_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1979_carter_airline_deregulation_model, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1979_carter_airline_deregulation_model, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1979_carter_airline_deregulation_model, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1979_carter_airline_deregulation_model, TR),
    TR >= 0.70.

:- end_tests(sotu_1979_carter_airline_deregulation_model_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint exhibits genuine market coordination (enabling efficient capacity allocation) but with substantial asymmetric benefits and costs. Price declines for consumers are real and significant on competitive routes, but rural abandonment and regional carrier displacement represent extraction mechanisms. The measurement trajectory shows extractiveness rising over time as initial coordination benefits plateau and consolidation/abandonment effects accumulate. Suppression (0.48): Moderate. Barriers to exit are structural for rural communities (cannot restore regulation) and regional carriers (cannot compete), but not absolute for workers (can find alternative employment) or urban consumers (have alternatives). Institutional suppression is high: once deregulation is authorized legislatively, restoring regulation requires new legislation. Theater ratio (0.35): Low-moderate. Market mechanisms are functionally real (prices do respond to competition, capacity does shift toward profitable routes), but ideological theater is high in planning phase (deregulation narrative as liberation, market efficiency as natural law) and recedes as operations begin. The declining theater trajectory reflects that once regulation is removed and actual outcomes appear (route abandonment, consolidation), the ideological narrative faces empirical pressure. The analytical observer notes rising theater again at t=8 as the consolidation outcomes require justification: concentration of market power is reframed as 'natural' result of competition, not as failure of deregulation to prevent monopoly formation.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between major carriers (rope) and rural communities (snare) is maximal because they have opposite directionality values and contradictory exit options. The major carrier sees deregulation as solving the coordination problem of efficient capacity allocation (rope experience: beneficiary with arbitrage exit). The rural community sees deregulation as solving the problem of extracting rural service via market mechanisms (snare experience: victim with trapped exit). The deregulation coalition sees coordination and liberation (rope/scaffold perspective). The regulatory apparatus sees planned dismantling (scaffold perspective with active sunset). The analytical observer at civilizational distance sees the constraint as having failed its stated goal (inflation reduction achieved but at cost of service elimination, consolidation, labor pressure) and degraded into a piton: the ideology persists (deregulation narrative) even as the function has changed (from competition-enabling to consolidation-enabling). The false summit mountain perspective reveals the most important gap: treating 'market mechanisms are efficient' as a universal natural law obscures the fact that deregulation's outcomes depend on the specific institutional context (airline networks exhibit natural monopoly characteristics on routes with thin demand, consolidation is rational under competitive pressure, labor costs are minimized in competitive labor markets). None of this contradicts market efficiency — but efficiency is not the only evaluative dimension, and deregulation advocates' claim that it is represents the falseness of the summit.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality computation derives from each agent's structural relationship to the extraction flow. Major carriers are beneficiaries with arbitrage exit options (can shift routes, pricing, labor deployment): d ≈ 0.05-0.15, producing negative or minimal χ — they experience the constraint as enabling rather than constraining. Rural communities are victims with trapped exit (cannot restore regulation, cannot compel service, cannot migrate entire infrastructure): d ≈ 0.95, producing maximal f(d) ≈ 1.42 — they experience maximum extraction. Regional carriers are victims with trapped exit (cannot compete, bankruptcy is outcome mechanism): d ≈ 0.90, producing high f(d) ≈ 1.32. Workers are victims with constrained exit (can leave industry but at career cost): d ≈ 0.70-0.75, producing f(d) ≈ 1.00-1.10. Urban consumers are beneficiaries with arbitrage options (can choose airlines, routes, travel timing): d ≈ 0.10, producing f(d) ≈ 0.00. The directionality overrides are not needed: the beneficiary/victim declarations plus exit options produce the correct d values through the standard derivation chain. Scope modifier σ(S) is applied at national scale (σ=1.0): rural routes are regional but the deregulation policy is national, and the verification difficulty (assessing whether abandonment is market necessity or strategic choice) is at national regulatory scope.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION OF THE DEREGULATION MANDATROPHY: The constraint resolves the mandatrophy by distinguishing between deregulation as a COORDINATION MECHANISM (genuine) and deregulation as a NATURAL LAW (false summit). Deregulation does solve a real coordination problem: regulatory route assignment and price controls created inefficiencies and prevented market-responsive allocation. This is the rope perspective and it is empirically valid. However, deregulation does NOT solve the problem of distributional equity, service to unprofitable routes, labor protections, or environmental externalities — these require continued regulation or alternative mechanisms. The mandatrophy is resolved when the analyst recognizes that the constraint is a Tangled Rope (not a mountain, not a pure rope): it genuinely coordinates capacity allocation AND it asymmetrically extracts from victims without compensation. The deregulation model's application to railroads and trucking should be analyzed per the same structure: genuine coordination benefits (efficiency in route/capacity allocation) paired with asymmetric extraction costs (rural service abandonment, labor pressure, consolidation). The false summit is in Carter's claim that deregulation is a universal template for reducing inflation — it is a context-specific trade-off between efficiency and equity, not a natural law of economics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    price_decline_attribution,
    'Do observed price declines result from deregulation enabling competition, or from fuel cost reductions and capacity growth that would have occurred under regulation?',
    'Counterfactual analysis: compare price trajectories in deregulated US airlines vs regulated airlines in other jurisdictions during same period; isolate fuel and capacity effects via econometric decomposition',
    'If deregulation caused declines: constraint is genuine coordination (Rope from major carriers, rope from consumers). If exogenous factors caused declines: constraint is regulatory capture by ideological narrative (Piton from empiricist view, Snare from rural communities regardless).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(price_decline_attribution, empirical, 'Attribution of price declines to deregulation vs exogenous factors').

omega_variable(
    route_abandonment_necessity,
    'Were rural route abandonments economically inevitable, or did they result from strategic carrier decisions enabled by deregulation?',
    'Historical cost analysis: reconstructed operating costs for abandoned routes under pre-deregulation cross-subsidization vs post-deregulation competitive pricing; survey of carrier route decisions and profitability analysis',
    'If inevitable: rural abandonment is supply-side reality, not extraction mechanism. If strategic: constraint is predatory pricing and systematic exclusion (Snare from rural communities, Tangled Rope showing extraction asymmetry).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(route_abandonment_necessity, empirical, 'Whether route abandonment was economically inevitable or strategically chosen').

omega_variable(
    consolidation_inevitability,
    'Did airline industry consolidation (mergers, hub dominance) occur because of deregulation, or despite deregulation''s intent to prevent it through market competition?',
    'Causal analysis: timeline of merger approvals, regulatory decisions on hub dominance, competitive outcomes; comparison with other deregulated industries and their consolidation patterns',
    'If intentional effect of deregulation: constraint contradicts stated goal (competition) and masks extraction mechanism (oligopoly formation). If unintended consequence: constraint is partially failed coordination with accumulating Tangled Rope/Snare characteristics. If prevented by deregulation law: constraint succeeded but benefits unevenly distributed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consolidation_inevitability, empirical, 'Whether consolidation resulted from deregulation or was prevented by it').

omega_variable(
    labor_extraction_mechanism,
    'Is airline worker wage pressure a necessary effect of competitive markets, or is it an extractive mechanism enabled by deregulation that could be constrained by labor law?',
    'Wage trajectory analysis: decompose wage changes into supply-side (labor market tightness), demand-side (traffic growth), and institutional factors (union power, regulatory labor protections); comparison with labor outcomes in other deregulated industries',
    'If necessary: constraint is neutral on labor distribution (Rope perspective valid). If extractive: constraint conceals labor extraction beneath competition rationale (Tangled Rope perspective emphasizes extraction, Snare from worker perspective).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_extraction_mechanism, empirical, 'Whether wage pressure is necessary effect of competition or enabled extraction').

omega_variable(
    inflation_reduction_causality,
    'Did airline deregulation measurably reduce inflation economy-wide, or is the inflation-fighting narrative decoupled from actual airline price movements?',
    'Macroeconomic analysis: quantify airline sector''s contribution to CPI inflation pre/post deregulation; control for monetary policy, energy shocks, and other sectoral changes; assess whether airline price declines were large enough to affect aggregate inflation trajectory',
    'If causality strong: constraint succeeded at stated goal, justifying costs (Scaffold perspective holds). If causality weak: constraint is ideological narrative with distributed costs but concentrated benefits (Piton perspective holds, Mountain naturalizes contingency).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_reduction_causality, empirical, 'Whether airline deregulation reduced economy-wide inflation').

omega_variable(
    regulatory_forestalling_alternatives,
    'Would continued regulation have enabled alternative price control mechanisms (cross-subsidization of rural service, administered rate structure) that produced different distributional outcomes?',
    'Regulatory history: review CAB regulatory philosophy pre-1978 and alternative rate-setting approaches in other regulated industries; assess whether regulatory reform (rather than deregulation) could have achieved price competition while preserving service mandates',
    'If viable: constraint presents deregulation as sole mechanism (false summit), obscuring policy choice. If not viable: constraint represents genuine Schumpeterian displacement where new equilibrium requires eliminating old regulatory form.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_forestalling_alternatives, conceptual, 'Whether continued regulation could have achieved price competition with different distributional outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1979_carter_airline_deregulation_model, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(airdereg_theater_t0, sotu_1979_carter_airline_deregulation_model, theater_ratio, 0, 0.62).
narrative_ontology:measurement(airdereg_theater_t2, sotu_1979_carter_airline_deregulation_model, theater_ratio, 2, 0.48).
narrative_ontology:measurement(airdereg_theater_t5, sotu_1979_carter_airline_deregulation_model, theater_ratio, 5, 0.35).
narrative_ontology:measurement(airdereg_theater_t8, sotu_1979_carter_airline_deregulation_model, theater_ratio, 8, 0.41).

% Extraction over time
narrative_ontology:measurement(airdereg_extractiveness_t0, sotu_1979_carter_airline_deregulation_model, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(airdereg_extractiveness_t2, sotu_1979_carter_airline_deregulation_model, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(airdereg_extractiveness_t5, sotu_1979_carter_airline_deregulation_model, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(airdereg_extractiveness_t8, sotu_1979_carter_airline_deregulation_model, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1979_carter_airline_deregulation_model, resource_allocation).
narrative_ontology:affects_constraint(sotu_1979_carter_airline_deregulation_model, railroad_deregulation_model).
narrative_ontology:affects_constraint(sotu_1979_carter_airline_deregulation_model, trucking_deregulation_model).
narrative_ontology:affects_constraint(sotu_1979_carter_airline_deregulation_model, natural_monopoly_abandonment).

% DUAL FORMULATION NOTE:
% Airline deregulation is the empirical prototype for Carter's deregulation-as-inflation-control model. It establishes the coordination narrative (markets allocate capacity efficiently) that is then applied to railroads and trucking. However, the measured extractiveness (0.52) and the asymmetric distribution of costs (rural abandonment, regional carrier displacement, labor pressure) suggests that subsequent applications should be analyzed as Tangled Rope (mixed coordination-extraction) rather than pure Rope (coordination only). The airline case demonstrates both the genuine coordination benefits and the distributed-cost mechanisms that deregulation advocates bracket or externalize.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
