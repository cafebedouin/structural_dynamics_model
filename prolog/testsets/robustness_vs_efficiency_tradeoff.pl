% ============================================================================
% CONSTRAINT STORY: robustness_vs_efficiency_tradeoff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_robustness_vs_efficiency_tradeoff, []).

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
 *   constraint_id: robustness_vs_efficiency_tradeoff
 *   human_readable: The Lean Systems Fragility
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The lean systems fragility represents a structural tension between the
 *   institutional pressure to eliminate buffers (for cost/efficiency) and the
 *   systemic need for redundancy and slack (for resilience). This constraint
 *   operates as a tangled rope: it has a genuine coordination function
 *   (eliminating waste is real problem-solving), but it is enforced through
 *   asymmetric extraction where shareholders capture efficiency gains while
 *   workers, communities, and the long-tail risk bearers absorb fragility
 *   costs. The constraint exhibits six perspectives because different actors
 *   experience the same structural choice differently. From the shareholder
 *   perspective, it appears as beneficial coordination (Rope). From the
 *   supply chain worker perspective, it appears as pure extraction with no
 *   exit (Snare). From the resilience coalition perspective, it appears as a
 *   temporary coordination failure being solved by redundancy mandates
 *   (Scaffold). From the consulting establishment perspective, it appears as
 *   a degraded ritual persisting through inertia (Piton). From the systems
 *   theorist perspective, it appears as an immutable natural law (Mountain).
 *   The theater ratio (0.58) reflects that much lean organizational practice
 *   is performative: companies adopt lean language, metrics, and rituals
 *   without achieving the promised flexibility or resilience benefits. The
 *   extractiveness trajectory (0.28 → 0.52 over 40 years) shows accelerating
 *   externalization of buffer costs as supply chains grew longer and more
 *   complex, while public capacity for shock absorption (pandemic response,
 *   unemployment insurance, disaster aid) became strained. The constraint's
 *   mandatrophy depends on whether the efficiency gains are real or whether
 *   they primarily reflect shifted costs rather than genuine productivity
 *   improvements.
 *
 * KEY AGENTS:
 *   - Shareholder/Investor Class: Primary beneficiary (institutional/arbitrage) — captures efficiency gains and cost reductions; can exit through diversification
 *   - Supply Chain Workers: Primary victim (powerless/trapped) — bears wage suppression, zero-hour contracts, acceleration without compensation; cannot exit
 *   - Industrial Communities: Secondary victim (powerless/trapped) — economically dependent on lean manufacturing plants; vulnerable to disruption and deindustrialization
 *   - Operations Managers: Mixed actor (organized/constrained) — benefits from bonuses and promotion for hitting metrics but bears liability for failures and operational stress
 *   - Resilience-First Coalition: Organized actor (organized/constrained) — regulators, unions, environmental groups pushing buffer mandates and redundancy requirements
 *   - Consulting Establishment: Institutional actor (institutional/arbitrage) — benefits from lean methodology adoption as perpetual consulting revenue; maintains performative practices
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional extraction as immutable efficiency-robustness tradeoff
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(robustness_vs_efficiency_tradeoff, 0.52).
domain_priors:suppression_score(robustness_vs_efficiency_tradeoff, 0.65).
domain_priors:theater_ratio(robustness_vs_efficiency_tradeoff, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(robustness_vs_efficiency_tradeoff, extractiveness, 0.52).
narrative_ontology:constraint_metric(robustness_vs_efficiency_tradeoff, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(robustness_vs_efficiency_tradeoff, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(robustness_vs_efficiency_tradeoff, tangled_rope).
narrative_ontology:human_readable(robustness_vs_efficiency_tradeoff, "The Lean Systems Fragility").
narrative_ontology:topic_domain(robustness_vs_efficiency_tradeoff, "technological/economic").

domain_priors:requires_active_enforcement(robustness_vs_efficiency_tradeoff).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(robustness_vs_efficiency_tradeoff, efficiency_optimizers).
narrative_ontology:constraint_beneficiary(robustness_vs_efficiency_tradeoff, cost_minimizers).
narrative_ontology:constraint_beneficiary(robustness_vs_efficiency_tradeoff, shareholder_extraction).
narrative_ontology:constraint_victim(robustness_vs_efficiency_tradeoff, system_resilience).
narrative_ontology:constraint_victim(robustness_vs_efficiency_tradeoff, labor_force).
narrative_ontology:constraint_victim(robustness_vs_efficiency_tradeoff, supply_chain_communities).
narrative_ontology:constraint_victim(robustness_vs_efficiency_tradeoff, long_tail_risk_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUPPLY CHAIN WORKER (SNARE) — Trapped in just-in-time systems with no inventory buffers, no shift flexibility, no ability to negotiate working conditions. Bears full cost of efficiency: wage suppression through competition, zero-hour contracts, production acceleration without compensation. No exit — skilled workers cannot easily transition to redundant-system employment, and global labor arbitrage locks them into lean production ecology. Maximum extraction.
constraint_indexing:constraint_classification(robustness_vs_efficiency_tradeoff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INDUSTRIAL COMMUNITY (SNARE) — Geographic regions dependent on lean manufacturing plants experience acute fragility. Supply chain disruptions cascade into unemployment, deindustrialization, and inability to diversify. Communities have zero agency: they cannot negotiate buffer inventory or redundant capacity with plants. Trapped by economic dependency. Theater of corporate resilience planning masks structural abandonment.
constraint_indexing:constraint_classification(robustness_vs_efficiency_tradeoff, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: OPERATIONS MANAGER (TANGLED ROPE) — Constrained by shareholder metrics and quarterly earnings pressure, but also benefits from efficiency gains (bonuses, promotion, reduced capital burden). Coordination function: lean systems do solve real logistics problems and reduce waste. But extraction occurs: the manager bears regulatory liability for failures while shareholders capture efficiency gains. Active enforcement through KPIs and inventory targets. Mixed benefit-cost structure.
constraint_indexing:constraint_classification(robustness_vs_efficiency_tradeoff, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SHAREHOLDER/INVESTOR CLASS (ROPE) — Captures value from efficiency extraction. Sees lean as coordination: coordinated supply chains do reduce unit costs and increase returns. Has full arbitrage: can exit stock positions, diversify across companies, or lobby for liability protection. Experiences the constraint as beneficial coordination. Theater: efficiency metrics are presented as inherent necessity rather than deliberate choice to externalize buffer costs.
constraint_indexing:constraint_classification(robustness_vs_efficiency_tradeoff, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: RESILIENCE-FIRST COALITION (SCAFFOLD) — Organized actors (regulators, unions, environmental groups, disaster-response agencies) are pushing redundancy mandates and buffer requirements. Supply chain legislation, pandemic-preparedness inventory rules, and labor protections create alternative pathways with sunset: as reshoring and distributed manufacturing mature, lean dependency decreases. Low effective extraction because coalition has agency and political pathway to enforce buffers. Sunset: 15-25 years as supply chain localization norms shift.
constraint_indexing:constraint_classification(robustness_vs_efficiency_tradeoff, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSULTING CLASS (PITON) — Lean management methodologies (Six Sigma, Total Productive Maintenance, Kaizen) persist as institutional practices despite degraded function. Theater ratio 0.58 reflects that much lean consulting is performative optimization theater: companies adopt lean language and metrics without real systemic change. Consultants benefit from the ritual; the methodology persists through inertia and competitive pressure despite reduced marginal returns. The functional verification has decayed — most organizations cannot identify where their actual buffer savings come from or whether efficiency gains persist.
constraint_indexing:constraint_classification(robustness_vs_efficiency_tradeoff, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a systems theory perspective, the efficiency-robustness tradeoff appears as an immutable principle: you cannot simultaneously maximize throughput AND maintain slack buffers. The constraint is presented as mathematical necessity (inverse relationship, cannot have both). However, this perspective risks naturalizing what is actually a choice between extraction regimes. The mathematical tradeoff is real, but the decision to push efficiency past resilient optima is institutional, not natural.
constraint_indexing:constraint_classification(robustness_vs_efficiency_tradeoff, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(robustness_vs_efficiency_tradeoff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(robustness_vs_efficiency_tradeoff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(robustness_vs_efficiency_tradeoff, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(robustness_vs_efficiency_tradeoff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(robustness_vs_efficiency_tradeoff, TR),
    TR >= 0.70.

:- end_tests(robustness_vs_efficiency_tradeoff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The lean systems constraint does eliminate real waste (genuine efficiency), but the gains are substantially captured by capital while costs are distributed to labor and communities (extraction). The value reflects asymmetric benefit distribution. The extractiveness has increased over 40 years as supply chains lengthened and public buffer capacity (unemployment insurance, disaster response) became strained, suggesting that efficiency gains are increasingly coming from externalization rather than genuine productivity. Suppression (0.65): High. Significant barriers to buffer maintenance include: competitive pressure (any firm that maintains inventory is undercut), labor arbitrage (redundant positions are filled by global competition), and ideological enforcement (lean doctrine treated as inevitable necessity). Suppression mechanisms are partly structural (supply chain competition) and partly institutional (management ideology). Theater ratio (0.58): Moderate-high. Lean consulting practices are substantially performative: companies adopt metrics and rituals (kaizen events, TPM boards, six-sigma belts) without achieving promised resilience or flexibility. The theater has increased as lean matured — much adoption is now cultural conformity rather than genuine optimization.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is extreme, making this a high-value diagnostic case. The shareholder sees Rope (beneficial coordination that reduces costs). The worker sees Snare (pure extraction with no exit). The coalition sees Scaffold (temporary problem with sunset pathway). The consultant sees Piton (degraded ritual). The manager sees Tangled Rope (mixed benefit and extraction). The analyst risks seeing Mountain (natural law). The same structural choice (push efficiency past resilient optima) appears as five different types depending on position. The gap reveals how institutional power determines not just the constraint's intensity, but the way the constraint is experienced and classified. The mountain classification at the analytical level is vulnerable to false summit detection: the 'inevitable tradeoff' narrative naturalizes what is actually a distribution choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is determined by structural position relative to extraction flow. Shareholders with arbitrage options experience low d (~0.10–0.15) — they are beneficiaries who can exit. Supply chain workers with no exit experience high d (~0.90+) — they are trapped targets. Operations managers with constrained exit experience moderate d (~0.55–0.65) — they gain from efficiency metrics but bear operational liability. The analytical observer's d (~0.72) reflects observational distance and inability to enforce preferences. The resilience coalition's d (~0.50) reflects symmetric position: they can constrain the system through legislation but face organized resistance. The directionality derivation feeds into chi calculation: higher d (trapped workers) produces higher chi (experienced extraction); lower d (arbitrage shareholders) produces lower or negative chi (experienced benefit). No overrides needed — structural derivation from exit options and beneficiary/victim status is accurate.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The mandatrophy here is the false natural law claim. The efficiency-robustness tradeoff appears as a mathematical inevitability (you cannot have both max throughput AND max slack). But the structural data reveals this as naturalization of institutional choice. The tradeoff exists at the margin: some efficiency-robustness tradeoff is real. But the lean systems constraint represents a choice to push past the efficient frontier and externalize buffer costs, not an adherence to mathematical limits. Evidence: (1) Companies that maintain strategic redundancy (Apple, military supply chains, critical infrastructure) perform well economically. (2) The efficiency gains that lean delivers have increasingly come from labor cost suppression and shift risk to workers and communities, not from process optimization. (3) Post-pandemic analysis shows many lean companies rebuilt buffers because fragility costs exceeded efficiency benefits. The mandatrophy is resolved by distinguishing the real tradeoff (at equilibrium, efficiency and robustness partially compete) from the institutional extraction (push past equilibrium to externalize buffer costs, then naturalize the resulting fragility as unavoidable). The constraint is tangled rope, not mountain. The analytical observer who sees natural law is victim of institutional rhetoric, not discovering a real limit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_buffer_threshold,
    'What inventory/capacity buffer level represents the Pareto frontier between efficiency and resilience? Is the current lean equilibrium at the frontier or beyond it?',
    'Empirical analysis of system failure rates vs buffer levels across industries; cost-benefit analysis comparing efficiency gains to disruption losses',
    'If current lean is at Pareto frontier: constraint is legitimate coordination problem (Rope). If beyond frontier: current setup is Pareto-dominated and represents pure extraction (Snare). Critical for distinguishing optimization from coercion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(optimal_buffer_threshold, empirical, 'Whether lean systems operate at efficient frontier or beyond').

omega_variable(
    externalized_buffer_costs,
    'What fraction of lean efficiency gains derive from legitimate optimization vs externalizing buffer maintenance costs to workers, communities, and public disaster response?',
    'Cost accounting of buffer maintenance (inventory, equipment redundancy, labor flexibility) across companies; comparison with externalized costs (unemployment insurance, community disaster aid, healthcare for precarious workers)',
    'If externalizations > 40%: efficiency is partially illusory, and classification should shift toward higher extraction (Snare from more perspectives). If < 20%: lean represents genuine optimization with distributed costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externalized_buffer_costs, empirical, 'Proportion of efficiency gains from externalization vs genuine optimization').

omega_variable(
    coalition_enforcement_capacity,
    'Can resilience-first coalitions enforce buffer mandates fast enough to create the scaffold sunset, or will institutional inertia perpetuate lean dependency for multiple generations?',
    'Policy tracking of supply chain localization mandates, pandemic-preparedness inventory rules, and labor protections; comparative analysis across nations with different regulatory postures',
    'If coalitions succeed within 15 years: scaffold classification confirmed. If blocked or delayed beyond 30 years: scaffold becomes aspirational (not structural), and constraint persists as tangled rope or snare for longer horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_enforcement_capacity, empirical, 'Whether resilience-first coalitions can enforce buffer mandates and create sunset').

omega_variable(
    systemic_fragility_acceleration,
    'Is lean system fragility increasing over time as supply chains lengthen and complexity grows, or are adaptive mechanisms (redundant sourcing, distributed manufacturing) reducing fragility despite lean pressure?',
    'Time-series analysis of supply chain disruption frequency, severity, and recovery time; correlate with industry lean adoption metrics and geographic diversification',
    'If fragility accelerates: extractiveness should increase toward 0.70+ (snare territory). If adaptive mechanisms work: extractiveness may stabilize or decrease, revealing rope-like coordination underneath.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(systemic_fragility_acceleration, empirical, 'Whether systemic fragility from lean is accelerating or stabilizing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(robustness_vs_efficiency_tradeoff, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(robust_tr_t0, robustness_vs_efficiency_tradeoff, theater_ratio, 0, 0.35).
narrative_ontology:measurement(robust_tr_t20, robustness_vs_efficiency_tradeoff, theater_ratio, 20, 0.48).
narrative_ontology:measurement(robust_tr_t40, robustness_vs_efficiency_tradeoff, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(robust_be_t0, robustness_vs_efficiency_tradeoff, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(robust_be_t20, robustness_vs_efficiency_tradeoff, base_extractiveness, 20, 0.4).
narrative_ontology:measurement(robust_be_t40, robustness_vs_efficiency_tradeoff, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(robustness_vs_efficiency_tradeoff, resource_allocation).
narrative_ontology:affects_constraint(robustness_vs_efficiency_tradeoff, supply_chain_fragility).
narrative_ontology:affects_constraint(robustness_vs_efficiency_tradeoff, labor_wage_suppression).
narrative_ontology:affects_constraint(robustness_vs_efficiency_tradeoff, systemic_risk_externalization).

% DUAL FORMULATION NOTE:
% The lean systems fragility decomposes into three distinct constraints: (1) supply_chain_fragility (ε≈0.45) — the structural vulnerability from buffer elimination; (2) labor_wage_suppression (ε≈0.65) — the extractive use of lean metrics to compress compensation; (3) systemic_risk_externalization (ε≈0.58) — the shift of disruption costs to public systems. These are linked through the lean doctrine but operate with different effectiveness and suppression values. The robustness_vs_efficiency_tradeoff story represents the philosophical frame that justifies all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(robustness_vs_efficiency_tradeoff, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
