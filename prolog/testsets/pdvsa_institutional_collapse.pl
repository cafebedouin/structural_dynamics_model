% ============================================================================
% CONSTRAINT STORY: pdvsa_institutional_collapse
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pdvsa_institutional_collapse, []).

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
 *   constraint_id: pdvsa_institutional_collapse
 *   human_readable: PDVSA Institutional Collapse and Resource Extraction Lock-In
 *   domain: political_economy/state_capacity
 *
 * SUMMARY:
 *   PDVSA's institutional collapse represents a convergence of resource
 *   curse, kleptocratic governance, and extractive institutional lock-in.
 *   Beginning in the early 2000s with politicization of hiring and
 *   management, accelerating through 2010-2016 with capital flight and
 *   skilled worker exodus, and reaching acute phase 2016-2026 with production
 *   collapse below 500,000 barrels/day from 3 million+ historical peak. The
 *   constraint exhibits snare logic: extraction benefits a narrow coalition
 *   (military apparatus, corruption networks, geopolitical actors) while
 *   imposing maximum costs on the general population (forced emigration,
 *   living standard collapse, institutional incapacity). Suppression
 *   mechanisms are structural: state monopoly on oil production, capital
 *   controls, currency restrictions, and coercive apparatus. The constraint
 *   persists despite destroying the productive base that sustains it — a
 *   hallmark of pure extraction mechanisms. Theater ratio has increased from
 *   0.45 (manageable institutional performance despite resource pressure) to
 *   0.82 (PDVSA maintains organizational structure with <20% functional
 *   capacity; management performance is pure ritual). The extractiveness
 *   trajectory from 0.35 to 0.68 reflects the increasing concentration of
 *   remaining resource flows toward narrow elites as general state capacity
 *   degrades.
 *
 * KEY AGENTS:
 *   - Venezuelan Oil Sector Workers: Primary victims (powerless/trapped) — face structural lock-in with declining employment and incomes; no exit options at national scale
 *   - Venezuelan General Population: Primary victims (powerless/trapped) — dependent on state oil revenues for subsidies and employment; bear full costs of institutional collapse
 *   - Military and Security Apparatus: Primary beneficiaries (organized/constrained) — extract rents and resources while maintaining state repression; constrained because position depends on state capacity
 *   - Corruption Networks: Primary beneficiaries (powerful/arbitrage) — extract capital and resources; high arbitrage through international flight and informal economies
 *   - International Sanctions Regime: Institutional actor (institutional/arbitrage) — maintains restrictions on degraded institution; can adjust policy without facing institutional resistance
 *   - Neighboring States (Colombia, Brazil, Guyana): Secondary institutional victims (institutional/constrained) — bear spillover costs (migration, energy price volatility, regional destabilization) without ability to exit
 *   - International Creditors and Commodity Traders: Secondary beneficiaries (institutional/arbitrage) — profit from default and commodity speculation; low-cost exit available
 *   - Analytical Observer: Views constraint as structural lock-in mechanism (analytical/analytical) — institutional degradation self-reinforces despite benefit-cost asymmetry
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pdvsa_institutional_collapse, 0.68).
domain_priors:suppression_score(pdvsa_institutional_collapse, 0.75).
domain_priors:theater_ratio(pdvsa_institutional_collapse, 0.82).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pdvsa_institutional_collapse, extractiveness, 0.68).
narrative_ontology:constraint_metric(pdvsa_institutional_collapse, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(pdvsa_institutional_collapse, theater_ratio, 0.82).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pdvsa_institutional_collapse, snare).
narrative_ontology:human_readable(pdvsa_institutional_collapse, "PDVSA Institutional Collapse and Resource Extraction Lock-In").
narrative_ontology:topic_domain(pdvsa_institutional_collapse, "political_economy/state_capacity").

domain_priors:requires_active_enforcement(pdvsa_institutional_collapse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pdvsa_institutional_collapse, military_security_apparatus).
narrative_ontology:constraint_beneficiary(pdvsa_institutional_collapse, corruption_networks).
narrative_ontology:constraint_beneficiary(pdvsa_institutional_collapse, international_sanctions_enforcers).
narrative_ontology:constraint_victim(pdvsa_institutional_collapse, venezuelan_population).
narrative_ontology:constraint_victim(pdvsa_institutional_collapse, institutional_capacity).
narrative_ontology:constraint_victim(pdvsa_institutional_collapse, oil_sector_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED VICTIMS (SNARE) — Face maximum extraction with no exit. Dependency on declining oil revenues for state employment, subsidies, and social services creates structural lock-in. Migration is costly and requires resources. Suppression mechanisms include currency controls, capital restrictions, and coercive state apparatus. Collapse of PDVSA directly reduces living standards with no alternative income sources available at national scale.
constraint_indexing:constraint_classification(pdvsa_institutional_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GENERATIONAL CONSTRAINT (SNARE) — Institutional degradation of PDVSA represents a multi-decadal loss of productive capacity. Capital stock erosion, technological gap, and brain drain create path dependency. Young people face constrained options: remain in degraded economy, emigrate at significant cost, or participate in parallel informal economies. The constraint operates at generational scale — recovery requires decades even with policy change.
constraint_indexing:constraint_classification(pdvsa_institutional_collapse, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ORGANIZED BENEFICIARIES (TANGLED ROPE) — Military apparatus experiences the constraint as hybrid coordination-extraction. Benefits from resource allocation, patronage networks, and expanded enforcement authority. Also bears constraints: dependent on state capacity to maintain institutions, vulnerable if resource base collapses entirely. High extraction toward this agent in near term, but long-term degradation threatens the institutional base supporting their position. Constrained exit because military power derives from state structure — cannot simply leave without losing position.
constraint_indexing:constraint_classification(pdvsa_institutional_collapse, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEGRADED INSTITUTIONAL THEATER (PITON) — International sanctions, price volatility, and geopolitical positioning create performative institutional responses. PDVSA maintains organizational theater (headquarters, management structures) with severely degraded functional capacity. The sanctions regime is maintained through diplomatic/institutional ritual despite PDVSA's incapacity to respond meaningfully. Theater ratio high: enforcement of restrictions on a non-functional institution; performance of containment on a decaying state actor. Institutional arbitrage available to external actors — sanctions can be adjusted or lifted without internal Venezuelan capacity changing.
constraint_indexing:constraint_classification(pdvsa_institutional_collapse, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REGIONAL INSTITUTIONAL SPILLOVER (TANGLED ROPE) — Neighboring states (Colombia, Brazil, Guyana) experience the constraint as mixed coordination-extraction. Coordination function: Venezuelan collapse affects energy prices, migration flows, and regional stability — requires coordination response. Extraction function: resource scarcity and migration pressure create asymmetric burdens on border states. Constrained exit because regional geography locks neighbors into dealing with Venezuelan institutional failure. Cannot exit or isolate; can only manage costs.
constraint_indexing:constraint_classification(pdvsa_institutional_collapse, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: EXTERNAL BENEFICIARIES (ROPE) — International creditors, commodity traders, and some foreign oil service providers experience the constraint as coordination with asymmetric benefit. PDVSA's collapse reduces their outstanding claims through default but creates arbitrage opportunities: distressed asset purchases, commodity speculation on supply disruption, and geopolitical positioning. Effective extraction runs toward these external actors. Arbitrage exit available — can reallocate capital elsewhere. Classified as Rope rather than Snare because external actors have genuine alternatives and can exit without cost equivalent to trapped population.
constraint_indexing:constraint_classification(pdvsa_institutional_collapse, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational scale, the constraint exhibits snare logic: institutional collapse creates an extraction mechanism that persists because exit costs exceed all alternatives. PDVSA's degradation benefits a narrow set of actors (military, corruption networks, external extractors) while imposing maximum costs on the general population. The suppression (75%) is structural: state monopoly on oil, capital controls, migration barriers, coercive apparatus. The constraint is maintained not through active coordination but through the high costs of exiting any part of it.
constraint_indexing:constraint_classification(pdvsa_institutional_collapse, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pdvsa_institutional_collapse_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pdvsa_institutional_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pdvsa_institutional_collapse, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pdvsa_institutional_collapse, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pdvsa_institutional_collapse, TR),
    TR >= 0.70.

:- end_tests(pdvsa_institutional_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint concentrates remaining resource flows toward military/security apparatus and corruption networks while imposing maximum costs on general population. The trajectory from 0.35 to 0.68 reflects increasing concentration — as total production declines, the share of production going to extraction (rather than maintenance or investment) increases. Suppression (0.75): High. Structural barriers to exit are severe: state monopoly on oil (no private alternatives), capital controls (prevent asset flight), currency restrictions (prevent alternative savings), migration barriers (exit costs remain high despite open borders due to capital control), and coercive apparatus (maintains status quo through force). Theater ratio (0.82): Very high. PDVSA maintains organizational theater (management structures, board meetings, quarterly reports) while operating at severely degraded functional capacity. International sanctions maintain diplomatic/institutional theater of containment on a non-functional institution. The organizational structure persists through inertia and performative maintenance rather than operational necessity.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals how the same constraint appears as coordination (Rope, to external actors with exit options), temporary problem (would be Scaffold if sunset were viable, but institutional damage may be irreversible), degraded ritual (Piton, institutional theater masking incapacity), or pure extraction (Snare, to trapped victims with maximum costs). The military apparatus experiences genuine mixed coordination-extraction (Tangled Rope) — they benefit from resource extraction but also depend on state capacity maintenance. The trapped population experiences pure extraction with no coordination benefit (Snare) — institutional collapse harms them directly. This gap between beneficiary and victim experience is the diagnostic signature of high-extractiveness constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) reflects the agent's structural relationship to extraction. Trapped victims have d ≈ 0.95 (full target) — they bear all costs with no benefits or exit options, yielding maximum f(d) ≈ 1.42. Organized beneficiaries have d ≈ 0.45 (moderate target with some benefits) — they extract rents but also depend on state capacity, yielding f(d) ≈ 0.45. External actors have d ≈ 0.25 (partial beneficiary with arbitrage exit) — they profit from collapse but can exit, yielding f(d) ≈ 0.02. The directionality derivation reflects the actual resource flow and exit capacity: extraction concentrated at high-d agents (trapped), diffuse at low-d agents (external). No overrides needed — the structural data produces appropriate d values through the beneficiary/victim and exit-option derivation chain.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint hardens the Snare classification through multiple reinforcing mechanisms. The beneficiary coalition (military, corruption networks, external extractors) sustains extraction despite destroying the productive base. Productive base destruction would normally create exit pressure (Scaffold logic: fix the problem or collapse), but suppression mechanisms (capital controls, coercive apparatus, migration barriers) prevent exit for the victim population. External observers might classify this as temporary institutional failure with natural recovery logic (commodity price recovery, leadership change, capital repatriation could revive PDVSA). But the institutional damage runs deeper than commodity cycles: human capital loss (brain drain), capital stock erosion (lack of maintenance and reinvestment), technological gap (inability to operate advanced extraction), and political lock-in (beneficiary coalition resists capacity-building that would reduce their rent extraction). The Snare classification is hardened by the observation that recovery pathways all require dismantling the extraction mechanism itself — resource concentration toward military/corruption networks must cease for productive reinvestment to resume, but this is the mechanism sustaining the regime. Pure extraction is the structure; productive capacity recovery is impossible without regime change. The constraint persists despite zero-sum economics because suppression costs (military repression, capital controls) are low relative to extraction benefits for the narrow beneficiary coalition. Mandatrophy resolved through recognizing that reform vs. extraction are not independent — reform requires dismantling the beneficiary coalition, which makes Snare classification the stable equilibrium.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_capacity_floor_threshold,
    'At what institutional degradation level does PDVSA become incapable of supporting even extractive state apparatus maintenance?',
    'Production decline trajectory analysis; comparative case study of state capacity collapse (Nigeria, Iraq, Syria); threshold for military/security apparatus defection or fragmentation',
    'If threshold high: constraint persists indefinitely through low-level equilibrium. If threshold low: rapid institutional collapse triggers regime failure within 2-5 years. Classification shifts from Snare to temporary Scaffold if reform becomes viable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_capacity_floor_threshold, empirical, 'Minimum state capacity threshold for extractive apparatus maintenance').

omega_variable(
    migration_capacity_saturation,
    'Do neighboring states'' migration absorption and remittance dependency create a ceiling on Venezuelan emigration that re-traps population?',
    'Demographic data on cumulative emigration rates; labor market saturation in receiving countries; remittance dependency curves; return migration tracking',
    'If migration saturates at <30% population: most Venezuelans remain trapped despite open borders. Snare classification hardens. If migration exceeds 40% population: constraint mechanism shifts toward institutional capacity degradation rather than direct population extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(migration_capacity_saturation, empirical, 'Regional migration absorption capacity limits').

omega_variable(
    oil_market_price_recovery_scenario,
    'Would significant oil price recovery (>$80/barrel sustained) enable PDVSA revival or is institutional damage irreversible regardless of commodity price?',
    'Counterfactual analysis using 2003-2006 high-price period and lessons learned; capital stock restoration timelines; human capital recovery feasibility; path dependency of institutional decisions made during collapse',
    'If reversible: constraint becomes temporary Scaffold with sunset tied to commodity cycle. If irreversible: constraint persists as Snare regardless of price. Classification depends on whether lock-in is economic or institutional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(oil_market_price_recovery_scenario, empirical, 'Whether PDVSA institutional damage is commodity-price reversible').

omega_variable(
    corruption_rent_extraction_magnitude,
    'What proportion of PDVSA''s resource extraction goes to corruption rents versus genuine state apparatus maintenance?',
    'Asset tracking analysis; capital flight tracing; comparing military/security apparatus funding to reported budget allocations; documentation of illicit networks',
    'If corruption rents > 50%: constraint is primarily designed for elite extraction, hardening Snare classification. If corruption rents < 20%: constraint is primarily functional institutional failure, softening to Piton classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(corruption_rent_extraction_magnitude, empirical, 'Proportion of extracted resources devoted to corruption rents').

omega_variable(
    institutional_memory_recovery_feasibility,
    'Can Venezuela recover technical and managerial expertise in oil production after brain drain, or is human capital loss permanent?',
    'Tracking diaspora engineer/scientist retention in receiving countries; cost and timeline for repatriation incentives; comparison to Iraq and post-Soviet oil sector recoveries; technical capacity rebuilding scenarios',
    'If recoverable within 10 years: long-term escape from Snare possible. If permanent loss (15+ years): institutional capacity becomes the binding constraint, not resource availability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_memory_recovery_feasibility, empirical, 'Whether oil sector human capital can be recovered post-brain-drain').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pdvsa_institutional_collapse, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pdvs_tr_t0, pdvsa_institutional_collapse, theater_ratio, 0, 0.45).
narrative_ontology:measurement(pdvs_tr_t10, pdvsa_institutional_collapse, theater_ratio, 10, 0.68).
narrative_ontology:measurement(pdvs_tr_t20, pdvsa_institutional_collapse, theater_ratio, 20, 0.82).

% Extraction over time
narrative_ontology:measurement(pdvs_be_t0, pdvsa_institutional_collapse, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pdvs_be_t10, pdvsa_institutional_collapse, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(pdvs_be_t20, pdvsa_institutional_collapse, base_extractiveness, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pdvsa_institutional_collapse, enforcement_mechanism).
narrative_ontology:affects_constraint(pdvsa_institutional_collapse, venezuelan_capital_controls).
narrative_ontology:affects_constraint(pdvsa_institutional_collapse, regional_migration_pressures).
narrative_ontology:affects_constraint(pdvsa_institutional_collapse, commodity_market_volatility).
narrative_ontology:affects_constraint(pdvsa_institutional_collapse, sanctions_regime_coupling).

% DUAL FORMULATION NOTE:
% PDVSA institutional collapse is downstream of state capacity degradation and upstream of regional destabilization. The upstream constraints (political capture, resource curse mechanisms) created conditions for institutional collapse. The downstream constraints (capital flight, migration pressure, energy market disruption) are effects of PDVSA's non-function. Decomposition: this story captures the institutional lock-in mechanism; separate stories address upstream political dynamics and downstream regional spillover.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pdvsa_institutional_collapse, institutional, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
