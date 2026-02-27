% ============================================================================
% CONSTRAINT STORY: fossil_fuel_lock_in
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fossil_fuel_lock_in, []).

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
 *   constraint_id: fossil_fuel_lock_in
 *   human_readable: Fossilized Regionalism (Alberta Carbon-Path Dependency)
 *   domain: economic/political
 *
 * SUMMARY:
 *   Alberta's economy has been structured for sixty years around hydrocarbon
 *   extraction, creating a nested system of path dependencies that lock in
 *   workers, firms, political institutions, and cultural identity around
 *   fossil fuel production. The constraint operates at multiple simultaneous
 *   levels: workers develop specialized skills with high switching costs;
 *   firms build infrastructure (pipelines, refineries, service supply chains)
 *   with long asset lives; provincial government revenue becomes dominated by
 *   resource extraction rents and royalties; and regional identity fuses with
 *   petrostate status. This creates a tangled rope constraint: it genuinely
 *   solves coordination problems (managing complex supply chains, building
 *   workforce culture, organizing large-scale infrastructure) while
 *   simultaneously extracting from those unable to exit (workers, future
 *   generations, climate stability). The theater ratio (0.64) reflects that
 *   provincial government institutions perform climate transition commitment
 *   (emissions targets, renewable energy mandates, diversification rhetoric)
 *   while substantive resource allocation remains skewed toward hydrocarbon
 *   incumbents. The extractiveness (0.58) reflects that the primary
 *   extraction mechanisms are (1) wage/employment asymmetry — fossil fuel
 *   jobs command 15-30% premium over alternative employment and offer
 *   pension/benefit packages difficult to replicate; (2) regulatory capture —
 *   provincial government systematically advantages incumbent operators over
 *   renewable competitors; and (3) sunk cost ideology — path dependency is
 *   treated as natural law rather than contingent institutional choice. The
 *   suppression (0.68) is high because workers cannot simply leave
 *   (geographic immobility, family ties, skill specificity, identity),
 *   renewable sector cannot compete on regulatory terms (subsidies,
 *   procurement favoritism, infrastructure investment), and climate
 *   transition is framed as threatening rather than enabling.
 *
 * KEY AGENTS:
 *   - Fossil Fuel Workers: Primary victim (powerless/trapped) — wage-dependent on hydrocarbon employment; retraining costs are high; alternative sectors within Alberta scarce; geographic constraints; identity fusion with oil/gas
 *   - Integrated Oil & Gas Operators: Primary beneficiary (institutional/arbitrage) — benefit from infrastructure lock-in, regulatory favoritism, workforce culture, government cost-externalization; can arbitrage to other jurisdictions if Alberta becomes unfavorable
 *   - Provincial Government: Secondary beneficiary/piton actor (institutional/arbitrage) — receives royalty revenue and corporate taxes; maintains performative transition commitment while resource allocation reinforces status quo; cannot restructure without political cost
 *   - Renewable Energy / Green Economy Coalition: Secondary victim (organized/constrained) — constrained by regulatory bias, limited access to preferential procurement, workforce training gaps, but benefits from emerging federal incentives
 *   - Federal Climate Policy / Transition Finance: Organized external actor (powerful/mobile) — attempting to inject resources and timeline pressure to shift provincial incentive structure; has exit option (can redirect transition investment to other provinces)
 *   - Climate Stabilization Commons: Abstract victim (powerless/trapped) — abstract collective good; cannot organize or exit; bears full cost of extended hydrocarbon lock-in
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fossil_fuel_lock_in, 0.58).
domain_priors:suppression_score(fossil_fuel_lock_in, 0.68).
domain_priors:theater_ratio(fossil_fuel_lock_in, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fossil_fuel_lock_in, extractiveness, 0.58).
narrative_ontology:constraint_metric(fossil_fuel_lock_in, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fossil_fuel_lock_in, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fossil_fuel_lock_in, tangled_rope).
narrative_ontology:human_readable(fossil_fuel_lock_in, "Fossilized Regionalism (Alberta Carbon-Path Dependency)").
narrative_ontology:topic_domain(fossil_fuel_lock_in, "economic/political").

domain_priors:requires_active_enforcement(fossil_fuel_lock_in).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fossil_fuel_lock_in, integrated_oil_gas_operators).
narrative_ontology:constraint_beneficiary(fossil_fuel_lock_in, fossil_fuel_service_sector).
narrative_ontology:constraint_beneficiary(fossil_fuel_lock_in, provincial_government_revenue).
narrative_ontology:constraint_victim(fossil_fuel_lock_in, alberta_fossil_fuel_workers).
narrative_ontology:constraint_victim(fossil_fuel_lock_in, renewable_energy_transition_sector).
narrative_ontology:constraint_victim(fossil_fuel_lock_in, climate_stabilization_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FOSSIL FUEL WORKER (SNARE) — Trapped by decades of path-dependent wage/benefit structure, geographic immobility, and identity fusion with hydrocarbon extraction. Career entirely specialized in oil/gas operations; retraining costs are high, alternative employment within Alberta is scarce, and migration out of province means losing social networks. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(fossil_fuel_lock_in, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INTEGRATED OIL & GAS OPERATORS (ROPE) — Primary beneficiary. Experiences the constraint as coordination of infrastructure, supply chains, workforce continuity, and regulatory predictability. Path dependency locks in their competitive advantages: existing pipelines, refining capacity, workforce culture, and relationship continuity with provincial government. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary; constraint subsidizes their operations.
constraint_indexing:constraint_classification(fossil_fuel_lock_in, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: RENEWABLE ENERGY COALITION (TANGLED ROPE) — Organized agents (solar installers, wind developers, environmental groups, clean-tech startups) see mixed coordination and extraction. Coordination: benefiting from federal clean energy incentives, grid infrastructure investment, emerging supply chains. Extraction: constrained by provincial regulatory bias toward incumbent hydrocarbon interests, limited access to subsidized government procurement, workforce trained in fossil fuels rather than renewables. d≈0.62, f(d)≈0.78, σ=0.9 → χ≈0.41.
constraint_indexing:constraint_classification(fossil_fuel_lock_in, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: PROVINCIAL GOVERNMENT INSTITUTIONS (PITON) — Maintains the constraint through performative 'diversification' rhetoric while substantive resource allocation remains skewed toward oil/gas. Crown corporations, regulatory bodies, and fiscal policy all bear the theater of transition commitment (emissions targets, renewable energy mandates) without structural reallocation. theater_ratio=0.64 indicates significant performative content. The institutional system sees itself as custodian of both carbon and jobs; actually changing this requires institutional suicide. d≈0.10, f(d)≈-0.08, σ=0.9 → χ≈-0.04.
constraint_indexing:constraint_classification(fossil_fuel_lock_in, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: FEDERAL CLIMATE POLICY (SCAFFOLD) — Federal government, external technology providers, and transition finance actors see this as a temporary coordination problem with a sunset clause: just-transition funding, retraining programs, carbon pricing that increases marginal cost of incumbents, and technology deployment subsidies are meant to decouple provincial revenue from fossil fuels within 10-20 years. d≈0.35, f(d)≈0.31, σ=1.0 → χ≈0.18. Low effective extraction because powerful external actors can inject resources and timeline pressure.
constraint_indexing:constraint_classification(fossil_fuel_lock_in, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational/global perspective, carbon lock-in appears as an immutable thermodynamic constraint: once carbon-intensive infrastructure is built (pipelines, refineries, power plants), the marginal cost of continuing to use it is lower than decommissioning and replacing it. Path dependency is a law of economic systems, not a policy choice. However, the structural data (ε=0.58, suppression=0.68, theater=0.64) contradicts the mountain classification — the engine will compute this as a false summit, revealing that institutional choice (regulatory capture, fiscal structure, political identity fusion) is being naturalized as thermodynamic inevitability.
constraint_indexing:constraint_classification(fossil_fuel_lock_in, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fossil_fuel_lock_in_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fossil_fuel_lock_in, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fossil_fuel_lock_in, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fossil_fuel_lock_in, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fossil_fuel_lock_in, TR),
    TR >= 0.70.

:- end_tests(fossil_fuel_lock_in_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The primary extraction mechanisms are: (1) wage premium and pension asymmetry — fossil fuel workers earn 20-30% premium and receive pension/benefit packages unmatched in renewable sectors, creating financial dependence; (2) regulatory capture — provincial government provides preferential treatment to hydrocarbon operators (subsidized infrastructure, favorable royalty structures, regulatory predictability) that renewables cannot access; (3) fiscal dependence — government revenue structure makes it fiscally rational for provincial institutions to maintain the status quo despite climate commitments. The extractiveness has increased over the interval (0.32 → 0.58) as carbon pricing and renewable competition have made the incumbent advantage increasingly coercive rather than naturally sustained. Suppression (0.68): High. Workers cannot exit because specialized skills have limited transferability, geographic options are constrained (other provinces have different wage structures, economies), family/social ties create exit costs, and renewable employment within Alberta is limited. The constraint is enforced through: (a) wage asymmetry — renewable jobs pay less, making retraining economically irrational for current workers; (b) identity fusion — attachment to fossil fuel work as regional/personal identity reduces preference for alternatives; (c) regulatory capture — provincial government systematically disadvantages renewable competitors. Theater ratio (0.64): Moderate-high. Provincial institutions perform climate transition commitment through: emissions targets (largely aspirational), renewable energy mandates (underspecified and incentive-misaligned), just-transition funding (underfunded relative to need), and diversification rhetoric (unmatched by budget reallocation). The performative content has increased (0.38 → 0.64) as the gap has widened between stated climate goals and actual resource allocation.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests as six distinct types depending on observer position. Fossil fuel workers see a snare (trapped by skill/wage/identity dependencies). Integrated operators see a rope (coordination of infrastructure and workforce). Renewable energy coalition sees a tangled rope (coordination benefits from federal policy but extraction from provincial regulatory capture). Provincial government sees a piton (maintaining performative commitment while unable to restructure; the institution itself has become path-dependent). Federal policy sees a scaffold (temporary problem with a 10-20 year transition timeline and external funding to shift incentives). The analytical observer risks seeing a mountain (carbon lock-in as thermodynamic inevitability) but the high theater and suppression ratios reveal institutional choice masquerading as natural law. The perspectival gap is not about disagreement — each perspective is structurally accurate from its position — but about the system's heterogeneity: the constraint simultaneously coordinates legitimate economic activities (supply chains, workforce development) while extracting from those unable to exit.
 *
 * DIRECTIONALITY LOGIC:
 *   Fossil fuel workers: Victim + trapped → d≈0.92, f(d)≈1.38. Near-maximal extraction. Workers cannot exit without significant personal cost (retraining, migration, wage loss, identity change). Integrated operators: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Can exit to other jurisdictions if Alberta becomes unfavorable; benefit from infrastructure and regulatory lock-in. Renewable coalition: Victim/beneficiary (mixed) + constrained → d≈0.62, f(d)≈0.78. Significant extraction but not maximal because organized agents have some agency and external (federal) support. Provincial government: Beneficiary (fiscal dependence) + arbitrage → d≈0.10, f(d)≈-0.08. Piton classification dominates rather than snare because the institution has exit options (restructure, diversify) but chooses not to exercise them. Federal policy: Powerful external actor + mobile → d≈0.35, f(d)≈0.31. Low effective extraction because can inject resources and has exit option. Climate commons: Victim + trapped → d≈0.98, f(d)≈1.45. Maximal extraction — abstract collective cannot organize or exit.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint avoids the mandatrophy (coordination vs extraction confusion) through careful perspective differentiation and explicit beneficiary/victim declarations. The operators genuinely experience coordination (supply chains, workforce, infrastructure). The workers genuinely experience extraction (trapped by asymmetry). The provincial government genuinely experiences inertia (piton). The federal policy genuinely experiences a temporary problem (scaffold). Each perspective is structurally accurate, and the classification spread across six types reveals that the underlying phenomenon is a hybrid: legitimate coordination (oil/gas is genuinely complex to operate) layered with institutional extraction (regulatory favoritism, wage asymmetry, fiscal capture, identity fusion). The mandatrophy resolution is: (1) distinguish coordination from extraction by beneficiary/victim structure, not by labels; (2) recognize that the same constraint can be tangled rope from the system perspective because it does coordinate (supply chains work) AND extract (workers are trapped); (3) use the theater ratio to identify which parts are performative (transition rhetoric) vs functional (actual operations). The false mountain perspective (analytical observer naturalizing carbon lock-in) is exposed by the high extractiveness and suppression scores — if this were truly a law of nature, workers would need no suppression; the high suppression reveals that institutional choice is holding the structure in place.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    stranded_asset_timeline,
    'What carbon price or technology cost trajectory would make hydrocarbon assets genuinely stranded rather than merely declining-margin?',
    'Comparative cost modeling of oil/gas extraction vs renewable alternatives; tracking realized depreciation rates of fossil infrastructure; break-even analysis for transition investments',
    'If stranding timeline < 10 years: snare classification dominates (workers cannot adapt). If > 30 years: scaffold is aspirational. If 15-20 years: scaffold is structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stranded_asset_timeline, empirical, 'Timeline for fossil infrastructure stranding').

omega_variable(
    retraining_efficacy,
    'Do just-transition retraining programs actually achieve wage parity with fossil fuel jobs, or do they primarily generate lower-wage ''green'' employment?',
    'Longitudinal wage tracking of retrainees; comparison cohorts between fossil and renewable sector employment; cost-benefit analysis of programs',
    'If wage parity achieved: trap is escapable (constrained rather than trapped). If 20-30% wage loss: snare deepens despite transition programs. If net wage loss > 40%: workers may prefer to decline retraining.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retraining_efficacy, empirical, 'Whether retraining achieves wage parity').

omega_variable(
    provincial_political_capture_depth,
    'Is provincial regulatory bias toward hydrocarbon operators a result of rational fiscal dependence or structural capture of political institutions by industry?',
    'Analysis of campaign financing, revolving-door staffing, regulatory decision patterns; comparison to provinces with similar resources but different regulatory outcomes',
    'If capture: theater_ratio increases further as institutions become unable to implement own stated policies. If fiscal dependence: sunset clause may be weak without external fiscal intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(provincial_political_capture_depth, conceptual, 'Degree of political capture by fossil fuel operators').

omega_variable(
    identity_fusion_stickiness,
    'How much of worker resistance to transition stems from economic constraint vs identity/cultural attachment to fossil fuel work?',
    'Ethnographic analysis; surveys of worker preferences when real wage-equivalent alternatives are offered; tracking of early transition cohorts across provinces',
    'If identity > economics: workers may reject transition even if economically rational. If economics > identity: transition is primarily a retraining/wage support problem.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_fusion_stickiness, preference, 'Relative weight of identity vs economic factors in worker resistance').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fossil_fuel_lock_in, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ffl_tr_t0, fossil_fuel_lock_in, theater_ratio, 0, 0.38).
narrative_ontology:measurement(ffl_tr_t15, fossil_fuel_lock_in, theater_ratio, 15, 0.51).
narrative_ontology:measurement(ffl_tr_t30, fossil_fuel_lock_in, theater_ratio, 30, 0.64).

% Extraction over time
narrative_ontology:measurement(ffl_be_t0, fossil_fuel_lock_in, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(ffl_be_t15, fossil_fuel_lock_in, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(ffl_be_t30, fossil_fuel_lock_in, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fossil_fuel_lock_in, resource_allocation).
narrative_ontology:affects_constraint(fossil_fuel_lock_in, climate_commitment_under_commitment).
narrative_ontology:affects_constraint(fossil_fuel_lock_in, provincial_fiscal_dependence).
narrative_ontology:affects_constraint(fossil_fuel_lock_in, renewable_infrastructure_transition).
narrative_ontology:affects_constraint(fossil_fuel_lock_in, worker_retraining_capacity).

% DUAL FORMULATION NOTE:
% Fossil fuel lock-in decomposes into four structurally distinct constraints: (1) carbon_path_dependency (ε≈0.25, Mountain) — physical/economic law that incumbent infrastructure has lower marginal cost; (2) fossil_fuel_lock_in (ε=0.58, Tangled Rope, this story) — institutional extraction layered on path dependency; (3) worker_wage_premium_trap (ε≈0.72, Snare) — occupational wage asymmetry that prevents transition; (4) provincial_fiscal_capture (ε≈0.68, Piton) — government institutions unable to restructure despite stated goals. This story addresses the institutional extraction layer; upstream is the thermodynamic constraint; downstream are specific sectoral and occupational lock-ins.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fossil_fuel_lock_in, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
