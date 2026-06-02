% ============================================================================
% CONSTRAINT STORY: climate_policy_incrementalism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_policy_incrementalism, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: climate_policy_incrementalism
 *   human_readable: Climate Policy Incrementalism
 *   domain: environmental_policy/political_economy
 *
 * SUMMARY:
 *   Climate policy incrementalism is the structural constraint that defers
 *   high-cost decarbonization to future periods, distributing costs
 *   asymmetrically across time and population. The constraint coordinates
 *   incumbent fossil fuel interests against rapid transition while claiming
 *   to coordinate climate mitigation through gradual sectoral and
 *   technological transition. This creates a fundamental misclassification
 *   risk: is incrementalism a legitimate and necessary coordination mechanism
 *   (rope) for managing economic transition, or is it an extraction mechanism
 *   (snare) that sacrifices vulnerable populations and future generations to
 *   preserve incumbent rents? The answer depends on whose structural position
 *   we adopt. Fossil fuel incumbents experience genuine coordination benefits
 *   — the constraint extends asset lifetimes, maintains political coalitions,
 *   and allows capital redeployment rather than stranding. Vulnerable
 *   populations experience pure extraction — they bear climate impacts while
 *   policy delays impose rising costs. The constraint exhibits tangled rope
 *   classification from the analytical perspective because it contains both
 *   genuine coordination elements (sectoral transition, technology
 *   deployment) and genuine extraction (deferral of costs, asymmetric burden
 *   distribution). The theater ratio (0.65) reflects that policy processes
 *   produce pledges, agreements, and emission reduction targets without
 *   proportional enforcement or accountability — the Paris Agreement and
 *   nationally determined contributions maintain the appearance of action
 *   while actual decarbonization lags scientific requirements. Over the
 *   10-year interval, extractiveness has risen from 0.35 to 0.58 as climate
 *   impacts accelerate while policy response time-constants remain fixed, and
 *   theater has risen from 0.50 to 0.65 as more pledges accumulate without
 *   corresponding implementation.
 *
 * KEY AGENTS:
 *   - Fossil Fuel Incumbents: Primary beneficiary (institutional/arbitrage) — experience incrementalism as coordination that extends asset lifetimes and maintains political protection
 *   - Climate Vulnerable Populations: Primary victim (powerless/trapped) — small island nations, subsistence farmers, low-latitude communities bearing existential costs while excluded from policy influence
 *   - Renewable Energy Sector: Secondary beneficiary with constraints (moderate/constrained) — benefits from policy support but suppressed by slower deployment timelines than technological feasibility permits
 *   - Future Generations: Structural victim (powerless/trapped) — bear deferred costs of today's policy delays; have zero agency in current policy decisions
 *   - Climate Action Coalition: Organized actors (organized/mobile) — civil society, youth movements, green parties building alternative pathways and pressure for acceleration
 *   - Industrial Incumbents in Transition: Mixed agent (powerful/constrained) — face both extraction (stranded asset risk, regulatory uncertainty) and coordination (gradual transition timelines)
 *   - International Climate Governance: Institutional theater (institutional/arbitrage) — produces performative outputs (pledges, targets) without enforcement, maintains appearance of coordination
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent institutional arrangements (incumbent political power, path dependence in capital) as inherent democratic constraints
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_policy_incrementalism, 0.58).
domain_priors:suppression_score(climate_policy_incrementalism, 0.68).
domain_priors:theater_ratio(climate_policy_incrementalism, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_policy_incrementalism, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_policy_incrementalism, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(climate_policy_incrementalism, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_policy_incrementalism, tangled_rope).
narrative_ontology:human_readable(climate_policy_incrementalism, "Climate Policy Incrementalism").
narrative_ontology:topic_domain(climate_policy_incrementalism, "environmental_policy/political_economy").

domain_priors:requires_active_enforcement(climate_policy_incrementalism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_policy_incrementalism, fossil_fuel_incumbents).
narrative_ontology:constraint_beneficiary(climate_policy_incrementalism, carbon_intensive_industries).
narrative_ontology:constraint_beneficiary(climate_policy_incrementalism, incumbent_financial_interests).
narrative_ontology:constraint_victim(climate_policy_incrementalism, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_policy_incrementalism, future_generations).
narrative_ontology:constraint_victim(climate_policy_incrementalism, ecological_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE VULNERABLE POPULATIONS (SNARE) — Small island nations, subsistence farmers, coastal urban poor, and low-latitude communities face existential climate impacts while having no effective exit from the constraint or ability to influence policy. Incrementalism guarantees they bear catastrophic costs while coordination function is absent for them — the constraint only coordinates incumbent interests against mitigation action.
constraint_indexing:constraint_classification(climate_policy_incrementalism, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RENEWABLE ENERGY SECTOR (TANGLED ROPE) — Benefits from policy subsidies and mandates (genuine coordination for deployment), but faces suppression through slower timelines than scientific urgency requires and structural barriers to competing with subsidized fossil fuels. Mixed extraction and coordination: policy supports renewables while incrementalism delays the scale and speed needed for sector dominance.
constraint_indexing:constraint_classification(climate_policy_incrementalism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FOSSIL FUEL INCUMBENTS (ROPE) — Experience incrementalism as pure coordination: it manages expectations, extends asset lifetimes, allows gradual transition timelines, and maintains political coalitions. No coercion from their perspective — the constraint coordinates their interests against accelerated decarbonization, maximizing extraction of residual rents from carbon assets.
constraint_indexing:constraint_classification(climate_policy_incrementalism, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CLIMATE ACTION COALITION (SCAFFOLD) — Organized green parties, youth climate movements, and civil society organizations view incrementalism as a temporary institutional constraint with a clear sunset: as climate impacts accelerate and renewable costs collapse, the policy ratchet effect should force faster transitions. Theater remains high but coalitional pressure is building alternative pathways (direct action, carbon pricing, electrification mandates). Coalition has agency and mobility — can exit through norm-building.
constraint_indexing:constraint_classification(climate_policy_incrementalism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL CLIMATE GOVERNANCE (PITON) — The UN Framework Convention, Paris Agreement, and Nationally Determined Contributions framework are largely performative. Nations make pledges that exceed domestic political will to implement; the governance machinery produces statements and targets without enforcement mechanisms or accountability for breach. Theater ratio very high (65%) — negotiation processes consume resources while actual emissions continue rising. Governance persists through institutional inertia despite low functional reduction in emissions.
constraint_indexing:constraint_classification(climate_policy_incrementalism, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: INDUSTRIAL INCUMBENTS IN TRANSITION (TANGLED ROPE) — Companies heavily dependent on fossil fuels or carbon-intensive processes face both extraction and coordination pressures. Incrementalism benefits them through gradual transition timelines, but also constrains them by creating regulatory uncertainty and stranded asset risk. They experience suppression as political instability and bifurcated market signals (carbon pricing in some jurisdictions, subsidies in others). Mixed exposure to extraction and coordination logic.
constraint_indexing:constraint_classification(climate_policy_incrementalism, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (FALSE SUMMIT) — A naturalized reading claims incrementalism is inherent to democratic governance: democratic institutions structurally cannot move faster than consensus permits, and consensus on climate action is difficult because it requires distributional sacrifice. This perspective risks false summit classification — treating a contingent institutional arrangement (fossil fuel political power, path dependence in capital stock, discounting of future costs) as an immutable law of political economy. The engine's false summit detector will expose this.
constraint_indexing:constraint_classification(climate_policy_incrementalism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_policy_incrementalism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_policy_incrementalism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_policy_incrementalism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_policy_incrementalism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_policy_incrementalism, TR),
    TR >= 0.70.

:- end_tests(climate_policy_incrementalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high and rising. The constraint extracts from vulnerable populations and future generations through temporal deferral of costs. It extracts from competing technologies through slower deployment timelines than economic optimality permits. However, the extraction is not total — some genuine decarbonization occurs, some sectoral transitions proceed, some technology deployment happens. The measurement trajectory (0.35→0.58) reflects that as climate impacts accelerate, the cost of deferral rises while policy response speeds remain fixed, increasing the extraction rate. Suppression (0.68): High. Vulnerable populations have no exit (trapped), industrial incumbents face asset stranding and regulatory risk if they move faster than policy allows (constrained), the climate action coalition faces political barriers and fossil fuel lobbying pressure (constrained/mobile). Suppression operates through multiple channels: fossil fuel political power, capitalist path dependence in infrastructure (coal plants, refining capacity), discounting of future costs, and geographic concentration of benefits and costs. Theater ratio (0.65): Moderately high. Policy processes produce extensive negotiation, target-setting, and pledge-making with significant consumption of political attention and resources. Actual emissions reductions lag stated targets; many pledges remain unfulfilled; carbon accounting mechanisms allow substitution of offsets for territorial emissions reductions. Theater has increased over the interval as more pledges accumulate without proportional implementation, creating a decoupling between policy activity and physical outcomes.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap is between fossil fuel incumbents (rope: coordination for their interests) and climate vulnerable populations (snare: pure extraction). This gap reveals that the constraint coordinates extraction rather than collective benefit. The secondary gap is between industrial incumbents in transition (tangled rope: mixed extraction and coordination) and renewable energy advocates (scaffold: sees incrementalism as temporary constraint being superseded by technology). The largest gap is between the institutional governance perspective (piton: performative theater) and the climate vulnerable perspective (snare: real catastrophic costs). These gaps collectively reveal that incrementalism is not a unified constraint with consistent classification across observers — it is a structure that coordinates incumbent interests while extracting from vulnerable populations, with theater masking the asymmetry. The analytical observer's false summit (naturalizing incrementalism as inherent to democracy) is exposed by contrasting with the vulnerable population's snare experience: if incrementalism were natural law, it would not be experienced as extraction by the powerless.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is computed from agent power, exit options, and structural relationship to the constraint's extraction flow. Fossil fuel incumbents: institutional power + arbitrage exit → d ≈ 0.10 (full beneficiary), f(d) ≈ -0.05, produces negative effective extraction (they benefit). Vulnerable populations: powerless + trapped → d ≈ 0.95 (full target), f(d) ≈ 1.42 (maximum), produces very high effective extraction. Renewable energy: moderate power + constrained exit → d ≈ 0.65 (mixed), f(d) ≈ 1.00, produces moderate extraction. Industrial incumbents: powerful + constrained → d ≈ 0.50 (symmetric), f(d) ≈ 0.65, produces moderate extraction scaled by scope. Climate action coalition: organized + mobile → d ≈ 0.45 (slight victim tilt), f(d) ≈ 0.45, produces lower extraction because they have agency. The piton perspective (institutional + arbitrage, but observing from within governance machinery) derives d ≈ 0.20, but the theater_ratio ≥ 0.70 gate overrides to produce piton rather than rope, because the constraint is not primarily coordination but performative activity. Scope modifiers further adjust: global scope (σ=1.2) amplifies extractiveness for vulnerable populations whose exposure is worldwide; national scope (σ=1.0) for industrial incumbents reflects local policy variation; this scope differentiation explains why chi appears higher for powerless agents at global scope than for more powerful agents at national scope despite nominally similar extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION AT HIGH EXTRACTION (ε=0.58): The constraint resolves mandatrophy by decomposing into its component structural functions. The apparent contradiction (is this coordination or extraction?) is resolved by recognizing that it performs both functions simultaneously but for different agents. For fossil fuel incumbents, it coordinates their interests against accelerated transition (rope). For vulnerable populations, it extracts through temporal deferral of costs (snare). For industrial incumbents, it mixes both (tangled rope). For the climate action coalition, it is a temporary constraint being superseded (scaffold). The piton classification captures that governance machinery produces performative activity rather than functionally effective decarbonization. The false summit analytical perspective is exposed by contrasting actual climate impacts (which are not deferrable or gradual enough to match policy timelines) against the claim that incrementalism is inherent to democracy. If incrementalism were a law of political economy, the empirical response would show consistent gradualism regardless of context. Instead, we observe rapid transitions when incumbent interests are not concentrated (e.g., ozone layer policy, lead removal from gasoline), suggesting incrementalism is contingent on political economy factors, not immutable. Mandatrophy is resolved: the constraint is definitionally a tangled rope (genuine coordination elements for some agents, asymmetric extraction for others, active enforcement required) from the analytical perspective. Its appearance as rope or snare depending on agent position is a feature, not a bug — it reveals how the same institutional arrangement produces coordination for beneficiaries and extraction for victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    tipping_point_timing,
    'Do incremental policy timelines remain feasible after climate tipping points accelerate feedback loops, or does the constraint become a direct acceleration mechanism?',
    'Empirical tracking of climate impact acceleration vs policy ratchet speed; modeling of point-of-no-return timelines for ice sheet collapse, ocean circulation, permafrost methane release against nationally determined contribution timelines',
    'If tipping points occur before policy responds: incrementalism becomes a snare for all agents, not just vulnerable populations. If policy ratchets fast enough: scaffold perspective confirmed, constraint has genuine sunset. If tipping points are gradual: incrementalism persists as tangled rope across multiple agent classes.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tipping_point_timing, empirical, 'Whether climate tipping points override policy incrementalism feasibility').

omega_variable(
    stranded_asset_acceleration,
    'Does capital market repricing of carbon risk (stranded assets, climate litigation) force faster decarbonization than policy incrementalism allows, creating regulatory arbitrage opportunities?',
    'Historical analysis of capital redirection post-Inflation Reduction Act, fossil fuel divestment campaign effectiveness, sovereign wealth fund carbon constraints, insurance market repricing of climate liability; comparison of market-driven transition speed vs policy-mandated transition speed in specific jurisdictions',
    'If markets move faster than policy: incumbent institutional actors face growing pressure to exit incrementalism (constraint weakens from rope to tangled rope or snare for them). If policy enforces incrementalism against market signals: scaffolding effect persists (temporary coordination of incumbent interests).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stranded_asset_acceleration, empirical, 'Whether capital market repricing accelerates beyond policy incrementalism').

omega_variable(
    coalition_mobilization_threshold,
    'What empirical threshold of climate impacts triggers sufficient coalition mobilization to overcome political suppression of accelerated policy?',
    'Time-series analysis of climate action coalition membership, electoral performance, protest participation, and legislative wins correlated with climate impact severity; cross-national comparison of acceleration timing relative to local impact intensity',
    'If threshold is low: coalition escalates faster than modeled, scaffold sunset accelerates. If threshold is high: incrementalism persists through multiple impact cycles (piton classification spreads). If threshold is context-dependent: constraint exhibits regional bifurcation (different classifications for different zones).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_mobilization_threshold, empirical, 'Coalition mobilization threshold for policy acceleration').

omega_variable(
    technological_substitution_sufficiency,
    'Do renewable energy cost curves and electrification technology improvements outpace policy incrementalism, rendering the constraint economically irrelevant?',
    'Projection of renewable cost curves against decarbonization timeline requirements; historical analysis of technology adoption S-curves in related domains (energy efficiency, grid electrification); empirical testing of whether policy incrementalism now constrains technology deployment or merely tracks it',
    'If technology sufficient: constraint weakens from snare/tangled rope to rope or piton (no longer extraction-driven, becomes coordination/theater). If technology insufficient: constraint persists as active extraction mechanism, victims remain trapped.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(technological_substitution_sufficiency, empirical, 'Whether technology improvements exceed policy-incrementalism constraints').

omega_variable(
    international_coordination_lock_in,
    'Does competitive regulatory arbitrage (nations racing to maintain competitiveness while avoiding climate costs) lock in incrementalism as a Nash equilibrium, or can multilateral coordination frameworks break the lock?',
    'Game-theoretic analysis of carbon leakage, border carbon adjustments, and competitive defection incentives; empirical tracking of whether EU, US, and China climate policies show convergence or divergence; historical comparison to ozone layer coordination (Montreal Protocol success) vs climate policy fragmentation',
    'If Nash equilibrium persists: incrementalism is locked in by structural competition (mountain or very strong snare). If multilateral agreement emerges: constraint transitions from mountain/snare to scaffold (temporary coordination with sunset as agreement hardens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(international_coordination_lock_in, empirical, 'Whether international competition locks in incremental policy as Nash equilibrium').

omega_variable(
    performative_compliance_detection,
    'Is the theater_ratio (0.65) capturing genuine policy incrementalism ineffectiveness, or are actual emissions reductions being underestimated due to measurement scope (omitting carbon offsets, land-use change, embodied carbon in traded goods)?',
    'Comprehensive life-cycle assessment of emissions reduction claims vs actual physical decarbonization; tracking of carbon accounting methodology changes and offset effectiveness studies; comparison of territorial vs consumption-based emissions accounting',
    'If measurement scope is incomplete: theater_ratio should be lower, constraint is slightly less performative, extraction slightly lower. If offsets are genuinely effective: theater_ratio stays high but constraint is less extractive (more actual coordination value). If offsets and accounting tricks hide non-compliance: theater_ratio should be much higher (0.75+), constraint is more snare-like.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performative_compliance_detection, empirical, 'Whether theater ratio accurately captures policy performativity or omits hidden decarbonization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_policy_incrementalism, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_incr_tr_t0, climate_policy_incrementalism, theater_ratio, 0, 0.5).
narrative_ontology:measurement(climate_incr_tr_t5, climate_policy_incrementalism, theater_ratio, 5, 0.58).
narrative_ontology:measurement(climate_incr_tr_t10, climate_policy_incrementalism, theater_ratio, 10, 0.65).

% Extraction over time
narrative_ontology:measurement(climate_incr_be_t0, climate_policy_incrementalism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(climate_incr_be_t5, climate_policy_incrementalism, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(climate_incr_be_t10, climate_policy_incrementalism, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_policy_incrementalism, resource_allocation).
narrative_ontology:boltzmann_floor_override(climate_policy_incrementalism, 0.18).
narrative_ontology:affects_constraint(climate_policy_incrementalism, fossil_fuel_stranded_asset_risk).
narrative_ontology:affects_constraint(climate_policy_incrementalism, carbon_pricing_fragmentation).
narrative_ontology:affects_constraint(climate_policy_incrementalism, renewable_energy_deployment_barriers).

% DUAL FORMULATION NOTE:
% Climate policy incrementalism decomposes into structurally distinct constraints along temporal, sectoral, and geographic lines. The global incrementalism story (this one, ε=0.58) represents aggregate policy response speed. Downstream constraints model specific mechanisms: stranded asset risk captures incumbent extraction under accelerated policy, carbon pricing fragmentation captures competitive lock-in preventing harmonized decarbonization, renewable barriers capture technology-policy misalignment. Network links capture how incrementalism at the aggregate level drives these downstream constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_policy_incrementalism, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
