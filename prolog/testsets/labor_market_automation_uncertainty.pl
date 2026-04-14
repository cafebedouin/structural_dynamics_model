% ============================================================================
% CONSTRAINT STORY: labor_market_automation_uncertainty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_labor_market_automation_uncertainty, []).

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
 *   constraint_id: labor_market_automation_uncertainty
 *   human_readable: Labor Market Automation Uncertainty
 *   domain: labor_economics/technological_disruption
 *
 * SUMMARY:
 *   Labor market automation represents a structural constraint on worker
 *   income and bargaining power that exhibits radically different
 *   classifications depending on the observer's position within labor
 *   markets. The constraint combines genuine coordination benefits
 *   (productivity gains, cheaper goods, new service job creation) with
 *   asymmetric extraction (wage suppression, skill obsolescence, community
 *   collapse in automation-exposed regions). Measured extractiveness has
 *   increased from 0.22 (early 2000s, when automation effects were dispersed
 *   and policy uncertainty high) to 0.58 (2026, after three decades of
 *   sustained technological displacement, declining union power, and failure
 *   of retraining systems). Theater ratio has simultaneously risen from 0.35
 *   to 0.68, indicating that retraining and transition programs have become
 *   increasingly performative — they manage political expectations while
 *   failing to restore displaced workers' material position. The constraint's
 *   structure encompasses multiple organizational levels: individual workers
 *   experience snare-type entrapment; service sector workers experience
 *   tangled rope (benefits from automation elsewhere, extraction here);
 *   capital owners experience rope-type coordination; organized labor and
 *   policy coalitions perceive a scaffolding structure with potential exits
 *   through redistributive policy; institutions maintain degraded transition
 *   programs (piton); and analysts risk naturalizing policy artifacts as
 *   technological inevitability (false mountain). The core extraction
 *   mechanism is suppression: geographic mobility barriers, skill mismatch
 *   between displaced workers and available jobs, weak collective bargaining
 *   power (union decline from 35% to 10% of workforce), and political capture
 *   that channels productivity gains toward capital rather than broad-based
 *   compensation adjustment.
 *
 * KEY AGENTS:
 *   - Low-skill and middle-skill workers: Primary victims (powerless/trapped) — face permanent or long-term displacement, no viable retraining pathways, geographic immobility. Suppression from weak bargaining power and oversupply of labor.
 *   - Capital owners and technology firms: Primary beneficiaries (institutional/arbitrage) — capture productivity gains through automation, maintain ability to relocate and arbitrage across jurisdictions. Suppression is high but experienced as enabling coordination.
 *   - Service sector workers: Secondary victims (moderate/constrained) — partially benefit from cheaper goods and some job creation, but increasingly bear wage suppression as displaced workers flood service markets.
 *   - Labor unions and worker organizations: Organized agents (organized/constrained) — perceive automation uncertainty as resolvable through collective bargaining power, sectoral investment, and redistributive policy, but face institutional obstacles (right-to-work laws, gig economy contractor status).
 *   - Government transition programs: Institutional actors (institutional/arbitrage) — maintain retraining, displacement assistance, and community adjustment mechanisms primarily through political necessity and administrative inertia; actual effectiveness is low (theater_ratio=0.68).
 *   - Regional communities: Victim-class aggregate (powerless/trapped) — bear infrastructure collapse, tax base erosion, and population flight as automation hollows out manufacturing regions.
 *   - Analytical observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent policy choices (IP protection, tax treatment, union suppression, R&D subsidies) as technological inevitability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(labor_market_automation_uncertainty, 0.58).
domain_priors:suppression_score(labor_market_automation_uncertainty, 0.65).
domain_priors:theater_ratio(labor_market_automation_uncertainty, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(labor_market_automation_uncertainty, extractiveness, 0.58).
narrative_ontology:constraint_metric(labor_market_automation_uncertainty, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(labor_market_automation_uncertainty, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(labor_market_automation_uncertainty, tangled_rope).
narrative_ontology:human_readable(labor_market_automation_uncertainty, "Labor Market Automation Uncertainty").
narrative_ontology:topic_domain(labor_market_automation_uncertainty, "labor_economics/technological_disruption").

domain_priors:requires_active_enforcement(labor_market_automation_uncertainty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(labor_market_automation_uncertainty, capital_owners).
narrative_ontology:constraint_beneficiary(labor_market_automation_uncertainty, technology_firms).
narrative_ontology:constraint_beneficiary(labor_market_automation_uncertainty, high_skill_workers).
narrative_ontology:constraint_victim(labor_market_automation_uncertainty, low_skill_workers).
narrative_ontology:constraint_victim(labor_market_automation_uncertainty, middle_skill_workers).
narrative_ontology:constraint_victim(labor_market_automation_uncertainty, regional_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED MANUFACTURING WORKER (SNARE) — Faces irreversible skill displacement. Retraining programs are underfunded and misaligned with available positions. Cannot migrate (family obligations, community ties, housing market friction). Cannot organize effectively (geographic dispersion, precarity). Experiences pure extraction: wages compressed, pension promises eliminated, community infrastructure dissolves. No coordination benefit — automation provided no surplus to this agent.
constraint_indexing:constraint_classification(labor_market_automation_uncertainty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SERVICE SECTOR WORKER (TANGLED ROPE) — Partially benefits from automation in agriculture and manufacturing (cheaper goods, some new service job creation). But also constrained by wage suppression and precarity as displaced workers flood service sector. High suppression: oversupply of labor, declining bargaining power, gig economy fragmentation prevents collective action. Mixed coordination and extraction — benefits from automation elsewhere, bears extraction here.
constraint_indexing:constraint_classification(labor_market_automation_uncertainty, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CAPITAL OWNERS AND TECHNOLOGY FIRMS (ROPE) — Clear beneficiaries. Experience automation as coordination mechanism: productivity gains, cost reduction, competitive advantage. Exit is effortless (capital mobility, profit arbitrage across jurisdictions). The constraint enables their interests. Suppression is high (policy enforces intellectual property, subsidizes R&D) but experienced as coordination because they control the mechanism.
constraint_indexing:constraint_classification(labor_market_automation_uncertainty, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR ORGANIZERS AND POLICY COALITION (SCAFFOLD) — Organized agents seeing automation uncertainty as a temporary coordination failure solvable through policy: universal basic income, sectoral retraining funds, wealth redistribution, worker ownership models. Suppression exists (political capture, corporate lobbying) but coalition perceives an exit path through democratic organization and generational norm change. Theater is present (rhetorical commitment to 'helping workers transition') but coalition is building alternative mechanisms with potential sunset: if redistributive policy succeeds, the extraction mechanism loses leverage.
constraint_indexing:constraint_classification(labor_market_automation_uncertainty, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LABOR MARKET TRANSITION PROGRAMS (PITON) — Government retraining, displaced worker assistance, community adjustment funds persist as institutional actors despite low demonstrated effectiveness. Theater is high (60-80% don't lead to comparable employment; wage replacement is partial; timing misaligns with disruption cycles). Programs maintain themselves through political necessity and institutional inertia rather than functional verification. Beneficiaries are program administrators, consultants, and universities; victims are workers whose expectations are managed rather than materially supported.
constraint_indexing:constraint_classification(labor_market_automation_uncertainty, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / TECHNOLOGICAL IMPERATIVE VIEW (MOUNTAIN) — From civilizational scope, some wage pressure from automation is a natural law of technological progress: capital substitutes for labor, marginal product of unskilled labor declines, competitive equilibrium drives wages down. This perspective naturalizes automation dynamics as immutable. But structural data contradicts mountain classification: policy choices (IP protection, tax treatment of automation, retraining investment, union suppression) are contingent, not natural laws. Engine computes false summit, revealing naturalization of policy artifacts.
constraint_indexing:constraint_classification(labor_market_automation_uncertainty, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(labor_market_automation_uncertainty_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(labor_market_automation_uncertainty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(labor_market_automation_uncertainty, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(labor_market_automation_uncertainty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(labor_market_automation_uncertainty, TR),
    TR >= 0.70.

:- end_tests(labor_market_automation_uncertainty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate, measured at current state (2026). The constraint extracts from low-skill and middle-skill workers through wage suppression (manufacturing wages down 15-25% in real terms since 2000 in automation-exposed sectors), skill obsolescence (retraining programs achieve only 35-45% wage replacement), and geographic immobility (housing costs in high-growth regions eliminate relocation as viable exit option). But extraction is not as severe as a pure snare would suggest because: (1) some productivity gains have diffused as cheaper goods (consumers benefit), (2) capital owners do not completely extract all surplus (they must reinvest to maintain automation advantage), (3) service sector expansion has created new jobs (lower-wage than displaced manufacturing, but not zero jobs). Suppression (0.65): High. Multiple non-price barriers prevent workers from competing for automation gains. Union density collapsed from 35% (1950s) to 10% (2026), eliminating primary mechanism for workers to capture productivity. Right-to-work laws, gig economy contractor classification, and geographic dispersion prevent new organizing. Retraining programs lack sufficient funding and timing alignment (lag 2-5 years behind displacement waves). Housing market friction and family obligations prevent geographic mobility. Theater ratio (0.68): High and rising. Government transition programs have increasingly become performative. Evidence: 60-75% of participants in retraining programs fail to secure comparable employment within 5 years; wage replacement averages 45-55% of pre-displacement levels; program spending per worker has increased while outcomes have declined; political rhetoric about 'helping workers transition' persists despite low functionality. The rise of theater (0.35 → 0.68 over 30 years) reflects the accumulation of policy patches that manage political pressure without materializing material support.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the utility of indexical classification for exposing structural divergence. The snare perspective (displaced worker) and the rope perspective (capital owner) are not two opinions about the same phenomenon — they are two different structural realities. The constraint IS extraction for the powerless/trapped agent and IS coordination for the institutional/arbitrage agent simultaneously because their relationship to the surplus flow is opposite. The scaffold perspective (organized labor) and the piton perspective (government programs) both correctly perceive high theater, but scaffold sees it as temporary (exit path exists through policy change) while piton sees it as structural (inertia dominates). The mountain perspective's false summit is the most dangerous: if policymakers naturalize automation as technological inevitability, they stop examining policy levers (IP, tax, union law, housing, education spending) that would shift the distribution of automation gains. The analytical observer's job is to show which levers are available and what policies would shift from snare/piton toward rope/scaffold/tangledRope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation priority for this constraint: (1) Explicit beneficiary/victim declarations → feed into structural d computation. Capital owners are declared beneficiaries; displaced workers are declared victims. (2) Power atom + exit options → modulate d magnitude. Institutional beneficiaries with arbitrage options get low d (0.05-0.15); powerless victims with trapped exit get high d (0.85-0.95); moderate constrained agents get medium d (0.50-0.65). (3) Canonical fallback for analytical perspective (d ≈ 0.72-0.73 from standard analytical canonical value). No directionality overrides needed — derivation chain captures the true relationships. The temporal progression in d-values (rising d for workers, falling d for capital) is captured in the measurements section: as extractiveness rises over the 30-year interval, the implicit directionality of capital's advantage has increased (capital's effective d decreases → more favorable position) while workers' d increases (less favorable → more maximum-extraction outcome).
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION REQUIRED. The label 'labor market automation uncertainty' conflates three structurally distinct constraints with different ε values: (1) Technological substitution dynamics (ε ≈ 0.15): The pure physics/economics of capital replacing labor when automation technology becomes available. Base extraction is low; this is partially coordinate-able. (2) Institutional extraction through wage suppression (ε ≈ 0.65): Policy-mediated redistribution of automation gains toward capital through union suppression, IP protection, tax subsidies, and suppression of alternative organizing models. Base extraction is high; this is the primary snare mechanism. (3) Transition program ineffectiveness (ε ≈ 0.52, primarily theater): Government retraining and displacement assistance that persist despite low functionality, managing political expectations without delivering material outcomes. These are three separate constraints with different observables, different beneficiaries, different policy levers, and different classification types. The mandatrophy in this single story is resolved by acknowledging that the claimed_type (tangled_rope) is correct for the aggregate phenomenon but masks structural heterogeneity. The engine's classification will reflect the institutional suppression mechanism (ε ≈ 0.65) because that is the dominant extraction visible in labor market aggregates. But a precise analysis would decompose into three stories: technological_substitution (ε ≈ 0.15, rope-type), labor_market_extraction_via_policy (ε ≈ 0.65, snare/tangled rope-type), and government_transition_theater (ε ≈ 0.52, piton-type). The present story captures the aggregate phenomenon and its most visible extraction mechanism; policy analysis would require the decomposed family.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    displacement_vs_restructuring,
    'Is observed unemployment structural displacement or equilibrium wage adjustment in expanding service sectors?',
    'Longitudinal wage tracking of cohorts 5-10 years post-displacement; sector-specific labor demand forecasts; comparison of displaced-worker outcomes across high-automation vs low-automation regions with similar initial conditions',
    'If structural displacement dominates: snare/tangled rope classifications correct, extractive dynamics persist. If equilibrium adjustment: rope perspective underestimates coordination benefits, and policy interventions may be unnecessary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(displacement_vs_restructuring, empirical, 'Whether displacement is permanent structural shift or temporary reallocation').

omega_variable(
    retraining_effectiveness_threshold,
    'What retraining completion rates and wage recovery levels would constitute genuine coordination (scaffold with real exit path) vs performative theater (piton)?',
    'Benchmark wage recovery: if 70%+ of participants achieve pre-displacement wage in 3-5 years → scaffold credible. If <40% achieve comparable wages after 10+ years → piton. Measure program cost relative to income replacement achieved.',
    'If scaffold: policy framework can resolve extraction through organized labor and redistributive mechanisms. If piton: programs are covering story for wage suppression; resolution requires different structural interventions (bargaining power, wealth redistribution, unconditional support).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retraining_effectiveness_threshold, empirical, 'Threshold effectiveness metrics distinguishing functional from performative retraining').

omega_variable(
    geographic_mobility_constraints,
    'Are worker migration barriers primarily material (housing costs, family obligations) or identity-locked (community attachment, resistance to displacement as identity threat)?',
    'Qualitative interviews + quantitative housing availability/affordability analysis in high-growth regions; measure migration rates against relocation incentives and support; distinguish barriers to physical migration from barriers to psychological identification with new communities',
    'If material: targeted housing vouchers, family support, infrastructure investment in displaced regions could materially increase mobility and reduce snare classification. If identity-locked: material barriers are secondary to cognitive/identity binding; reframing may be required alongside material support.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_mobility_constraints, empirical, 'Whether geographic immobility is material or identity-based').

omega_variable(
    capital_substitution_rate,
    'Is automation pace economically determined (constant rate of capital-labor substitution at steady-state productivity growth) or policy-driven (accelerated by IP protection, R&D subsidies, automation tax treatment)?',
    'Comparative analysis of automation rates across jurisdictions with different IP regimes, R&D incentives, and labor cost structures; econometric decomposition of substitution elasticity into market vs policy components',
    'If economically determined: mountain perspective has credibility — some wage pressure is natural. If policy-driven: suppression is primarily institutional (not inevitable); extraction can be redirected through tax/subsidy mechanisms without blocking beneficial innovation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_substitution_rate, empirical, 'Whether automation pace is market-determined or policy-driven').

omega_variable(
    collective_action_barriers,
    'What specific mechanisms prevent low-skill workers from organizing collectively to capture gains from productivity improvements (unionization, sectoral bargaining, cooperative models)?',
    'Historical analysis of union decline timing vs automation acceleration; comparison of sectors with high collective bargaining (public sector, strong unions) vs low bargaining (gig economy, service) in automation exposure; measurement of legal and institutional barriers to new organizing models',
    'If barriers are legal/institutional (right-to-work laws, contractor classification, anti-union enforcement): these are policy artifacts and can be changed. If barriers are structural (workers too dispersed to coordinate, capital too mobile to pressure): stronger redistributive mechanisms may be necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_action_barriers, empirical, 'Specific mechanisms preventing worker collective action on automation gains').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(labor_market_automation_uncertainty, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lmau_tr_t0, labor_market_automation_uncertainty, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lmau_tr_t10, labor_market_automation_uncertainty, theater_ratio, 10, 0.48).
narrative_ontology:measurement(lmau_tr_t20, labor_market_automation_uncertainty, theater_ratio, 20, 0.62).
narrative_ontology:measurement(lmau_tr_t30, labor_market_automation_uncertainty, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(lmau_be_t0, labor_market_automation_uncertainty, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(lmau_be_t10, labor_market_automation_uncertainty, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(lmau_be_t20, labor_market_automation_uncertainty, base_extractiveness, 20, 0.5).
narrative_ontology:measurement(lmau_be_t30, labor_market_automation_uncertainty, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(labor_market_automation_uncertainty, resource_allocation).
narrative_ontology:affects_constraint(labor_market_automation_uncertainty, wage_stagnation).
narrative_ontology:affects_constraint(labor_market_automation_uncertainty, income_inequality).
narrative_ontology:affects_constraint(labor_market_automation_uncertainty, union_suppression).
narrative_ontology:affects_constraint(labor_market_automation_uncertainty, geographic_immobility).

% DUAL FORMULATION NOTE:
% Labor market automation is an aggregate phenomenon decomposable into technological substitution (low extraction, coordinate-able) and institutional extraction through policy (high extraction, snare-type). The present story captures the institutional extraction mechanism because it dominates observable labor market outcomes. Decomposition into technological_substitution and labor_market_extraction_via_policy would clarify which policy levers (union law, tax, IP, housing, education) most directly address the constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
