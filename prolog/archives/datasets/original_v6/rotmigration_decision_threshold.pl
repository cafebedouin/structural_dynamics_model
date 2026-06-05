% ============================================================================
% CONSTRAINT STORY: rotmigration_decision_threshold
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rotmigration_decision_threshold, []).

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
 *   constraint_id: rotmigration_decision_threshold
 *   human_readable: The Migration Decision Threshold (Cost-Benefit Equilibrium)
 *   domain: economic/social
 *
 * SUMMARY:
 *   The migration decision threshold represents the cost-benefit equilibrium
 *   at which expected utility of relocating to a destination labor market
 *   exceeds the costs, risks, and social losses of remaining in the origin
 *   country. This constraint operates differently across scales and
 *   timeframes: from the individual's biographical calculation (whether to
 *   leave), to the household's generational strategy (which members migrate
 *   to optimize remittances), to the nation's structural development trap
 *   (whether to rely on out-migration or invest in domestic wage growth), to
 *   the international system's appearance of control (visa regimes
 *   maintaining an illusion of border sovereignty while actual migration
 *   follows network economics). The constraint exhibits both genuine
 *   coordination functions (destination labor markets need inflow mechanisms;
 *   households need risk-distribution through diaspora) and substantial
 *   extraction mechanisms (migrants bear upfront costs and legal risks;
 *   origin countries lose human capital; remittance systems extract informal
 *   taxation). The theater ratio (rising from 0.35 to 0.58 over the interval)
 *   reflects the increasing performativity of formal migration regimes: visa
 *   systems, skill-matching quotas, and bilateral agreements create an
 *   appearance of managed migration while actual flows respond primarily to
 *   wage differentials and network effects. The extractiveness (rising from
 *   0.28 to 0.52) reflects institutional hardening: destination countries
 *   have progressively tightened skill requirements, documentation standards,
 *   and visa costs, raising barriers for marginal migrants while sustaining
 *   the threshold that benefits established migrant networks and destination
 *   employers.
 *
 * KEY AGENTS:
 *   - Trapped Potential Migrants: Primary victims (powerless/trapped) — bear upfront costs ($5k–$20k), legal/documentation risks, family separation costs; face total suppression through visa regimes, skill requirements, and capital barriers
 *   - Origin-Country Households: Secondary victims and partial beneficiaries (moderate/constrained) — bear family separation and lost productive labor but receive remittances; subject to informal taxation and network controls
 *   - Destination Labor Markets: Primary beneficiaries (institutional/arbitrage) — access selective labor inflow; can adjust policy and skill requirements to modulate flows; experience threshold as coordination mechanism
 *   - Migrant Networks: Institutional beneficiaries (organized/arbitrage) — extract rents from reduced information asymmetry and transit facilitation; benefit from network effects and sender/receiver commissions
 *   - Origin-Country Development State: Organized actor (organized/constrained) — trapped between development dependency and human capital loss; sees migration as temporary scaffold with implicit sunset
 *   - International Migration Regime: Institutional actor (institutional/arbitrage) — maintains formal apparatus with degraded function; benefits from performative control and political theater
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing a contingent power structure (institutional suppression of labor mobility) as an immutable law of economics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rotmigration_decision_threshold, 0.52).
domain_priors:suppression_score(rotmigration_decision_threshold, 0.65).
domain_priors:theater_ratio(rotmigration_decision_threshold, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rotmigration_decision_threshold, extractiveness, 0.52).
narrative_ontology:constraint_metric(rotmigration_decision_threshold, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(rotmigration_decision_threshold, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rotmigration_decision_threshold, tangled_rope).
narrative_ontology:human_readable(rotmigration_decision_threshold, "The Migration Decision Threshold (Cost-Benefit Equilibrium)").
narrative_ontology:topic_domain(rotmigration_decision_threshold, "economic/social").

domain_priors:requires_active_enforcement(rotmigration_decision_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rotmigration_decision_threshold, destination_labor_markets).
narrative_ontology:constraint_beneficiary(rotmigration_decision_threshold, migrant_networks).
narrative_ontology:constraint_beneficiary(rotmigration_decision_threshold, remittance_receiving_households).
narrative_ontology:constraint_victim(rotmigration_decision_threshold, origin_country_development).
narrative_ontology:constraint_victim(rotmigration_decision_threshold, trapped_potential_migrants).
narrative_ontology:constraint_victim(rotmigration_decision_threshold, family_separation_costs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED POTENTIAL MIGRANT (SNARE) — Faces migration cost barriers (visa fees, travel, deposit requirements) and destination-country exclusion mechanisms (skill requirements, language barriers, legal status risks). The threshold is calculated through a cost-benefit analysis that heavily weights upfront costs (often $5k–$20k) against uncertain future benefits. Lacks arbitrage options; cannot legally move; suppression through documentation, enforcement, and capital barriers is near-total. Effective extraction: institutional structures extract rents from desperation.
constraint_indexing:constraint_classification(rotmigration_decision_threshold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: ORIGIN-COUNTRY HOUSEHOLD (TANGLED ROPE) — Benefits from remittances sent by migrants (up to 10% of GDP in some countries) but also bears costs: family separation, loss of productive household labor, and psychological/social costs. The migration decision threshold represents both a coordination mechanism (households collectively decide who migrates to optimize remittances) and an extraction mechanism (migrants bear disproportionate risk; remittances are taxed informally by family expectations and origin-country government policies). Constrained exit: cannot prevent migration of members without severe social cost, yet constrained by remittance dependency.
constraint_indexing:constraint_classification(rotmigration_decision_threshold, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DESTINATION LABOR MARKET (ROPE) — Benefits from selective migration: access to workers at wages below what non-migrants would accept, demographic renewal in aging societies, entrepreneurship inflow, tax base expansion. The migration threshold acts as a pure coordination mechanism for the destination: it filters incoming labor to those with sufficient motivation to pay upfront costs and accept destination-country restrictions. Effective extraction toward destination is minimal; the threshold solves the labor-sourcing coordination problem. Arbitrage options: destination can adjust policy, skill requirements, visa costs to modulate inflow.
constraint_indexing:constraint_classification(rotmigration_decision_threshold, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ORIGIN-COUNTRY DEVELOPMENT STATE (SCAFFOLD) — Faces a temporary coordination failure: the migration threshold acts as a brake on domestic development by draining human capital to destination countries. However, organized actors (diaspora networks, remittance-financed microfinance, skills-transfer initiatives, return migration programs) are building alternative pathways that capture development benefits while retaining some labor force. The sunset is implicit: as origin-country wage growth narrows the migration premium, the threshold naturally rises and migration pressure decreases. Suppression is moderate; constrained exit reflects the development state's limited leverage over labor mobility.
constraint_indexing:constraint_classification(rotmigration_decision_threshold, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL MIGRATION REGIME (PITON) — The formal institutional apparatus (visas, quotas, bilateral agreements, international labor conventions) performs an elaborate ritual of regulation while the actual migration threshold is determined by informal networks, smugglers, and de facto acceptance in destination labor markets. The regime's formal function (selecting and controlling migration) has been substantially degraded by its inability to enforce against economic incentives. Theater ratio is high: visa interviews, background checks, and quota systems create an appearance of control while actual migration responds primarily to wage differentials and network effects. The regime persists through institutional inertia and political theater (appearing to control migration) rather than functional necessity.
constraint_indexing:constraint_classification(rotmigration_decision_threshold, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, migration thresholds are inherent features of spatial wage differentials: wherever two labor markets have unequal wage distributions, arbitrage pressure creates migration flows that naturally calibrate to an equilibrium where migration costs equal expected wage premium. This perspective sees the threshold as a mathematical law of supply-demand equilibrium, independent of policy or institution. However, the structural data contradicts the mountain classification — suppression (0.65), active enforcement (true), and beneficiary/victim asymmetries reveal that institutional arrangements (visa regimes, exclusion policies, informal rent extraction) are not laws of nature but contingent power structures that artificially sustain the threshold above the market-clearing level.
constraint_indexing:constraint_classification(rotmigration_decision_threshold, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rotmigration_decision_threshold_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(rotmigration_decision_threshold, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(rotmigration_decision_threshold, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(rotmigration_decision_threshold, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(rotmigration_decision_threshold, TR),
    TR >= 0.70.

:- end_tests(rotmigration_decision_threshold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52, rising from 0.28): Moderate-high. The threshold operates as extraction because upfront costs and destination restrictions are artificially maintained above market-clearing levels. If labor could move freely, the wage premium would narrow and the threshold would collapse. Instead, institutional suppression (visa regimes, skill requirements, documentation demands) and informal enforcement (smuggler pricing, network gatekeeping, employer coercion) keep costs high, extracting rents from migrants' desperation. The increase over the interval reflects destination hardening: tighter requirements raise costs for marginal migrants while benefiting established networks and destination employers. Suppression (0.65): High. Multiple enforcement layers: formal (immigration enforcement, visa denial, deportation), informal (smuggler pricing, network exclusion, employer leverage over undocumented status), and social (family expectations, community judgment, brain-drain stigma). Barriers to entry are substantial; alternatives (staying in origin) carry opportunity costs but are default-available. Theater ratio (0.58, rising from 0.35): Moderate-high and increasing. The formal migration regime (visas, quotas, bilateral labor agreements) performs an elaborate ritual of control and selection while actual migration follows network economics and wage differentials. Skill-matching systems, background checks, and quota allocations create an appearance of meritocratic or managed migration while outcomes respond primarily to push/pull factors. Theater has increased as destination countries have adopted more elaborate formal apparatus to manage migration politically, even as informal flows remain largely uncontrolled.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is exceptionally wide. Trapped migrants and origin-country development states perceive severe extraction (Snare, high χ). Origin-country households perceive mixed coordination and extraction (Tangled Rope, moderate χ). Destination labor markets perceive pure coordination (Rope, low χ). The formal regime perceives itself as controlling (Rope) while actually operating as performative ritual (Piton). The analytical observer risks naturalizing the entire structure as equilibrium (Mountain). This gap reflects genuine structural asymmetries: the threshold is beneficial for destination actors, harmful for trapped migrants, and mixed for origin-country actors. No single type is correct from all perspectives — the constraint is genuinely different depending on where you stand.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) in this constraint varies sharply across agent types. Trapped potential migrants have high d (~0.90): they are targets of extraction through upfront costs, legal risks, and suppression. Origin-country households have moderate d (~0.55): they are partial victims (family separation, lost labor) but also partial beneficiaries (remittances). Destination labor markets have low d (~0.10): they are beneficiaries who experience the threshold as coordination rather than extraction. Migrant networks occupy middle ground (~0.45): they benefit from network positions but also bear some risk and face reputation costs. Origin-country development states have high d (~0.70): they lose human capital and face development dependency on remittances. The piton perspective derives from theater (0.58) rather than directionality; the regime experiences the constraint as degraded rather than as an extraction they perpetrate. The mountain perspective's derived d would be ~0.72 (analytical position, trapped between observer role), but this misclassifies the constraint as a natural law when it is actually a power structure.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATE-ATROPHY RESOLUTION: The mandatrophy here is whether the migration threshold is primarily a coordination mechanism (legitimate equilibrium resolving supply-demand mismatch) or primarily an extraction mechanism (institutional suppression maintaining artificial barriers). The constraint resolves this by showing both are structurally real but affect different agents. From the destination perspective, it is coordination: the threshold elegantly filters labor without formal selection apparatus. From the trapped-migrant perspective, it is extraction: institutions collect rents from desperation. From the origin-country perspective, it is mixed Tangled Rope: households benefit from remittances (coordination function) but lose members and face dependency (extraction function). The mandatrophy dissolves not by choosing one type but by recognizing that the constraint simultaneously solves a coordination problem for some agents and extracts from others. The theater increase (0.35→0.58) suggests atrophy of the regime's functional capacity: formal visa systems are increasingly performative rather than genuinely selective, while actual migration sorting is done by networks and market forces. The regime persists through political demand for the appearance of control, not because it efficiently solves labor-market problems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    wage_premium_stability,
    'How stable is the wage differential between origin and destination that drives the migration decision threshold?',
    'Long-term tracking of real wage convergence; analysis of skill-adjusted wage premiums over 20+ year intervals; comparison of historical wage gaps with current differentials across country pairs',
    'If wage premium narrows sharply: threshold rises automatically and constraint shifts toward Rope/Scaffold. If premium persists: constraint remains Tangled Rope/Snare with sustained extraction pressure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wage_premium_stability, empirical, 'Stability of wage differential driving migration threshold').

omega_variable(
    remittance_dependency_trap,
    'Does remittance dependency create a lock-in mechanism that sustains origin-country underdevelopment and perpetuates the migration threshold?',
    'Regression analysis of remittance dependency vs domestic investment, wage growth, and human capital accumulation; comparison of countries with high vs low remittance reliance; study of return-migrant investment outcomes',
    'If strong lock-in: constraint is Snare from origin perspective (structured dependency on out-migration). If weak lock-in: constraint is more Tangled Rope (remittances enable development that eventually lowers threshold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remittance_dependency_trap, empirical, 'Whether remittance dependency creates development lock-in').

omega_variable(
    informal_enforcement_cost,
    'What proportion of the suppression (0.65) stems from formal institutional enforcement (visa regimes, borders) vs informal enforcement (network controls, smuggler pricing, employer coercion)?',
    'Analysis of successful border crossings vs apprehension rates; comparison of formal visa costs with informal transit costs; qualitative interviews with migrant networks on enforcement mechanisms',
    'If formal dominates: regime perspective (Piton) is accurate; formal policy changes could reshape threshold. If informal dominates: threshold is driven by network economics; formal policy is substantially theater.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_enforcement_cost, empirical, 'Composition of suppression between formal and informal enforcement').

omega_variable(
    network_externality_sign,
    'Does growth in migrant networks lower the threshold (positive externality: improved information, reduced transit costs) or raise it (negative externality: destination saturation, lower wages, tighter enforcement)?',
    'Time-series analysis of network size vs threshold in specific migration corridors; comparison of early-stage vs mature migration routes; study of wage pressure and enforcement response in high-migration destinations',
    'If positive: networks create Rope dynamics (coordination problem solved). If negative: networks create Snare dynamics (extraction trapped by own growth). If mixed: Tangled Rope (both effects present).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_externality_sign, empirical, 'Sign and magnitude of network externality on migration threshold').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rotmigration_decision_threshold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mdt_tr_t0, rotmigration_decision_threshold, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mdt_tr_t5, rotmigration_decision_threshold, theater_ratio, 5, 0.48).
narrative_ontology:measurement(mdt_tr_t10, rotmigration_decision_threshold, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(mdt_be_t0, rotmigration_decision_threshold, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mdt_be_t5, rotmigration_decision_threshold, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(mdt_be_t10, rotmigration_decision_threshold, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rotmigration_decision_threshold, resource_allocation).
narrative_ontology:affects_constraint(rotmigration_decision_threshold, remittance_taxation_regime).
narrative_ontology:affects_constraint(rotmigration_decision_threshold, origin_country_wage_stagnation).
narrative_ontology:affects_constraint(rotmigration_decision_threshold, destination_labor_market_saturation).

% DUAL FORMULATION NOTE:
% The migration decision threshold is upstream of several constraint families. The remittance taxation regime depends on this threshold for extractable rents. Origin-country wage stagnation is downstream: low domestic investment (due to remittance dependency created by high thresholds) sustains the wage differential that perpetuates the threshold. Destination labor-market saturation creates negative network externalities that raise the threshold over time. These relationships form a constraint ecosystem with feedback loops.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(rotmigration_decision_threshold, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
