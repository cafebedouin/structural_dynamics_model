% ============================================================================
% CONSTRAINT STORY: climate_stabilization_timeline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_stabilization_timeline, []).

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
 *   constraint_id: climate_stabilization_timeline
 *   human_readable: Climate Stabilization Timeline Constraint
 *   domain: environmental_policy/climate_science
 *
 * SUMMARY:
 *   The climate stabilization timeline constraint represents the structural
 *   divergence between atmospheric carbon accumulation and institutional
 *   climate action capacity. Since 1992, the UNFCCC process has produced 28
 *   COPs, legally binding agreements (Kyoto Protocol, Paris Agreement),
 *   nationally-determined contributions, corporate net-zero pledges, and
 *   elaborate carbon accounting frameworks — while global CO2 emissions have
 *   risen from 21.4 Gt/year to 37.5 Gt/year. This is not a failure of
 *   coordination logic; it is structural. The constraint operates through
 *   temporal asymmetry (present actors benefit from carbon-intensive
 *   consumption, future actors bear costs), incumbent lock-in (fossil
 *   infrastructure dominates energy systems despite economic superiority of
 *   alternatives), and institutional capture (fossil fuel industries shape
 *   climate governance). The theater ratio (0.76) reflects that climate
 *   policy machinery is substantially performative: pledges use accounting
 *   tricks (offsets, scope exclusion, equity carve-outs), enforcement
 *   mechanisms lack teeth, and the gap between treaty language and physical
 *   emissions outcomes is widening. The constraint exhibits all six DR types
 *   simultaneously, making it a diagnostic exemplar for how political economy
 *   naturalizes contingent institutional arrangements as physical necessity.
 *
 * KEY AGENTS:
 *   - Climate Vulnerable Populations: Primary victims (powerless/trapped) — small island states, subsistence agricultural communities, low-latitude regions with zero exit capacity; bear full cost of stabilization delay with zero decision power
 *   - Future Generations: Primary victims (powerless/trapped) — structurally excluded from contemporary governance; face irreversible carbon budget depletion; experience pure extraction across civilizational time horizon
 *   - Transition-Dependent Communities: Secondary victims (moderate/constrained) — coal workers, extraction-dependent economies; face high relocation/retraining costs but benefit from transitional support infrastructure; mixed coordination and extraction
 *   - Climate Action Movements: Organized secondary agents (organized/constrained) — civil society, environmental NGOs; benefit from institutional platforms but constrained by limited enforcement power; movement labor subsidizes institutional legitimacy
 *   - International Climate Governance Apparatus: Primary beneficiary (institutional/arbitrage) — UNFCCC, COP presidencies, multilateral institutions; coordinate information exchange and negotiation venues; benefit from funding and authority independent of physical outcomes
 *   - High-Emission Incumbent Industries: Primary beneficiary (institutional/arbitrage) — fossil fuel companies, aviation, cement/steel; coordinate carbon management through offsets and pledges; extract benefit from institutional delay machinery; high exit optionality
 *   - Fossil Fuel Energy System: Institutional infrastructure (institutional/arbitrage) — physical plants, supply chains, workforce; maintained through regulatory capture and sunk cost despite degraded functional rationale; experienced as piton (performative theater)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional failure as immutable physical law; risks false summit by treating carbon budget depletion as constraint when actual constraint is political economy of energy transition
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_stabilization_timeline, 0.58).
domain_priors:suppression_score(climate_stabilization_timeline, 0.68).
domain_priors:theater_ratio(climate_stabilization_timeline, 0.76).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_stabilization_timeline, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_stabilization_timeline, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(climate_stabilization_timeline, theater_ratio, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_stabilization_timeline, tangled_rope).
narrative_ontology:human_readable(climate_stabilization_timeline, "Climate Stabilization Timeline Constraint").
narrative_ontology:topic_domain(climate_stabilization_timeline, "environmental_policy/climate_science").

domain_priors:requires_active_enforcement(climate_stabilization_timeline).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_stabilization_timeline, high_emission_incumbent_industries).
narrative_ontology:constraint_beneficiary(climate_stabilization_timeline, global_north_consumption_patterns).
narrative_ontology:constraint_beneficiary(climate_stabilization_timeline, short_term_growth_dependent_economies).
narrative_ontology:constraint_victim(climate_stabilization_timeline, climate_vulnerable_populations).
narrative_ontology:constraint_victim(climate_stabilization_timeline, future_generations).
narrative_ontology:constraint_victim(climate_stabilization_timeline, atmospheric_carbon_budget).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE VULNERABLE POPULATIONS (SNARE) — Small island states, subsistence agricultural communities, and low-latitude regions face physical constraints that permit no exit. Rising sea levels, crop failures, and heat mortality are non-negotiable. These agents bear full extraction with zero alternatives. The constraint is experienced as pure coercion with minimal coordination benefit — institutional climate action proceeds at pace independent of their survival pressure.
constraint_indexing:constraint_classification(climate_stabilization_timeline, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE GENERATIONS (SNARE) — Structurally unable to participate in contemporary climate governance. Bear full cost of stabilization delay while having zero decision power. The constraint appears as pure extraction — present actors extract carbon budget that future actors must repay through forced adaptation. Temporal asymmetry produces maximum experienced suppression.
constraint_indexing:constraint_classification(climate_stabilization_timeline, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 3: TRANSITION-DEPENDENT COMMUNITIES (TANGLED ROPE) — Coal mining regions, fossil fuel workers, and extraction-dependent economies face high costs to exit the carbon lock-in but also benefit from transitional support infrastructure and energy access. The constraint exhibits both genuine coordination (shared infrastructure modernization) and asymmetric extraction (workers bear disproportionate relocation and retraining burden). Exit is possible but costly.
constraint_indexing:constraint_classification(climate_stabilization_timeline, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CLIMATE ACTION MOVEMENTS (TANGLED ROPE) — Organized civil society groups benefit from carbon governance institutional infrastructure (legal standing, negotiation platforms, data access) while constrained by limited enforcement power and co-optation of radical demands into performative commitments. Mixed position: genuine coordination function (pressure for institutional accountability) coupled with asymmetric extraction (movement labor subsidizes institutional legitimacy without delivery).
constraint_indexing:constraint_classification(climate_stabilization_timeline, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERNATIONAL CLIMATE GOVERNANCE APPARATUS (ROPE) — UNFCCC, COP presidencies, and multilateral climate institutions coordinate global information exchange, establish baseline carbon accounting, and create venues for negotiation. These actors benefit from the institutional infrastructure (authority, funding, visibility) while performing genuine coordination function. Experience the constraint as manageable — negotiation itself is the goal, independent of physical emissions outcomes.
constraint_indexing:constraint_classification(climate_stabilization_timeline, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: HIGH-EMISSION INCUMBENT INDUSTRIES (ROPE) — Fossil fuel companies, aviation, cement, and steel sectors coordinate carbon management through voluntary standards, offset markets, and net-zero pledges while extracting substantial benefit from the delay mechanism. These actors have high exit optionality (capital mobility, supply chain substitutability) and experience the constraint primarily as coordination machinery that legitimizes continued extraction. Low experienced suppression because alternatives exist but are economically disfavored.
constraint_indexing:constraint_classification(climate_stabilization_timeline, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: FOSSIL FUEL ENERGY SYSTEM (PITON) — The physical infrastructure (power plants, refineries, supply chains) has become institutionally locked-in despite degraded functional rationale. Renewable energy technology now demonstrates superior economics in most contexts, yet fossil infrastructure persists through sunk cost, workforce coupling, and regulatory capture. The system is maintained through theater: carbon pricing schemes, renewable energy mandates, and efficiency standards create appearance of transition while preserving incumbent position. Theater ratio (0.76) reflects that policy machinery is substantially performative — 70+ years of international climate action has failed to bend emissions curves.
constraint_indexing:constraint_classification(climate_stabilization_timeline, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / PHYSICAL CONSTRAINT VIEW (MOUNTAIN) — From a universal/civilizational perspective, the climate stabilization timeline appears as an immutable physical limit: the carbon budget for 1.5°C warming is finite and depleting. Once exceeded, the constraint is naturally enforced via radiative forcing, albedo feedback, and ocean acidification. No institution can negotiate with thermodynamics. However, this classification will trigger false-summit detection: the structural data reveals that the 'natural' constraint is mediated entirely through human institutional choice about emissions pathways. The apparent mountain is a naturalized political economy.
constraint_indexing:constraint_classification(climate_stabilization_timeline, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_stabilization_timeline_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_stabilization_timeline, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_stabilization_timeline, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_stabilization_timeline, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_stabilization_timeline, TR),
    TR >= 0.70.

:- end_tests(climate_stabilization_timeline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint's primary extraction is temporal — present actors (high-emission nations, incumbent industries) capture benefits from carbon-intensive consumption while future actors (small island states, subsistence communities) bear costs. The current-generation extraction is not as severe as pure snare (0.72+) because some coordination exists (renewable technology, grid modernization, energy efficiency) alongside extraction, but it exceeds rope (0.45) because the coordination is subordinated to incumbent interest preservation. The measurement trajectory (0.32 → 0.45 → 0.58 over 20 years) reflects accumulating extraction: each year of delay narrows the carbon budget for 1.5°C, increasing the cost of stabilization, which increases the extraction burden on future actors while present actors continue benefiting. Theater ratio (0.76): High. Climate governance has produced 28 COPs, thousands of pages of treaty language, nationally-determined contributions from 195 countries, and corporate net-zero pledges from the largest companies — while global emissions curve has inverted in the opposite direction. The performative content is not accidental: pledges use accounting scope exclusion (Scope 3 emissions often excluded), offset mechanisms that permit continued fossil extraction in other jurisdictions, and 'net-zero' targets that rely on speculative future carbon removal. Suppression (0.68): High. Multiple mechanisms suppress exit: (1) incumbent energy infrastructure creates path dependence for fossil-dependent regions; (2) global supply chains depend on fossil energy, making unilateral decarbonization economically costly; (3) information asymmetry — carbon accounting permits incumbent companies to greenwash continued fossil expansion; (4) political capture — incumbent industries fund climate-skepticism campaigns and regulatory delay; (5) temporal lock-in — once carbon is emitted, atmospheric residence time ensures multi-century consequences.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Vulnerable populations perceive snare (pure coercion with no coordination benefit). Future generations perceive snare (zero decision power, maximum temporal extraction). Transition-dependent communities perceive tangled rope (genuine infrastructure coordination mixed with disproportionate burden). Climate movements perceive tangled rope (institutional platforms coupled with co-optation of demands). International climate governance perceives rope (coordination of information exchange, negotiation venues). Incumbent industries perceive rope (legitimacy of carbon management, offset mechanisms). Fossil infrastructure perceives piton (performative policy maintenance). Analytical observer risks mountain (naturalizing institutional delay as immutable physical constraint). The gap between snare and rope perspectives is irreducible: the same institutional machinery appears as pure extraction to powerless agents and pure coordination to beneficiary institutions. The gap reveals that the 'constraint' is not the carbon budget (that is a background condition) but the political economy of institutional capture that prevents carbon budgets from directing action.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation reveals the structural extraction flow. Climate-vulnerable populations: beneficiary=false (victims), exit_options=trapped, power=powerless → d ≈ 0.95 → f(d) ≈ 1.42 → high experienced chi. High-emission industries: beneficiary=true, exit_options=arbitrage, power=institutional → d ≈ 0.05 → f(d) ≈ -0.12 → negative experienced chi (extraction flows toward them). Transition-dependent communities: beneficiary=mixed (benefit from infrastructure, bear transition costs), exit_options=constrained, power=moderate → d ≈ 0.62 → f(d) ≈ 0.92 → moderate experienced chi. Analytical observer: power=analytical, exit_options=analytical, scope=universal → d ≈ 0.73 → f(d) ≈ 1.15 → moderate chi. The scope modifier σ(S)=global (1.2) amplifies all chi values — the constraint's extraction is harder to verify and easier to rationalize at planetary scale. Beneficiary-victim structure is clear: future generations and vulnerable populations are pure victims; incumbent industries are pure beneficiaries; transition communities are mixed; governance apparatus is beneficiary (authority and funding independent of emissions outcomes).
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED MANDATROPHY: The constraint's extractiveness (0.58) exceeds the snare threshold (0.66) for only three perspectives (vulnerable populations, future generations, fossil infrastructure degradation); it stays below threshold for most institutional perspectives. The mandatrophy is resolved by recognizing that this is not 'one constraint misclassified' but multiple perspectives on a hybrid coordination-extraction system. The International Climate Governance Apparatus experiences genuine rope (coordination of information, negotiation venues) because it successfully achieves coordination — parties do negotiate, agree on frameworks, and exchange data. High-emission industries experience rope because offset mechanisms and carbon pledges do coordinate emissions accounting (even if poorly). But from the snare-experiencing victims' perspective, this coordination is subordinated to extraction — the 'coordination' of governance is coordination TO PRESERVE INCUMBENT POSITION, not to stabilize climate. The paradox resolves through the distinction between objective coordination function and net directionality. Objectively, climate governance coordinates information exchange (rope function). But directionally, the coordination is structured to extract maximum benefit for present/wealthy actors and defer costs to future/vulnerable actors (snare directionality). The system exhibits tangled rope structure at the whole: genuine coordination machinery (Paris Agreement framework, carbon accounting standards) coupled with asymmetric extraction (temporal asymmetry, geographic asymmetry, intergenerational asymmetry). Mandatrophy is resolved by accepting that both rope and snare classifications are correct — rope describes the institutional machinery's coordination function, snare describes the distributional outcome.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carbon_budget_physical_vs_institutional,
    'Is the climate stabilization timeline a physical constraint (carbon budget depletion) or an institutional failure (delay in decarbonization)?',
    'Decompose into two separate constraint stories: (1) carbon-budget physical constraint (inherent thermodynamic limit), (2) decarbonization-delay institutional constraint (political economy of fossil fuel lock-in). Compare epsilon values: physical constraint has low epsilon (coordination around physics = rope); institutional constraint has high epsilon (political extraction = tangled rope or snare). If only one story is needed, the boundary is misdrawn.',
    'If primarily physical: mountain classification correct — constraint is unchangeable, stabilization requires accepting equilibrium at higher warming. If primarily institutional: false summit — constraint is changeable, stabilization requires political defeat of incumbents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(carbon_budget_physical_vs_institutional, conceptual, 'Whether timeline is physical limit or institutional failure').

omega_variable(
    net_zero_pledge_credibility,
    'Do national net-zero pledges and corporate carbon commitments represent genuine emissions reduction pathways or performative delay mechanisms?',
    'Longitudinal analysis of pledge versus actual emissions trajectory; decomposition of pledges into direct reduction vs. offset-dependent targets; examination of enforcement mechanisms and penalty structures for non-compliance. Compare pledge stringency to physical requirements for 1.5°C pathway.',
    'If credible: institutional constraint is rope (genuine coordination of global response). If performative: institutional constraint is piton (degraded theater maintaining incumbent position). Theater ratio drops from 0.76 to <0.5 if credibility is established.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(net_zero_pledge_credibility, empirical, 'Credibility of net-zero pledges as emissions reduction mechanisms').

omega_variable(
    tipping_point_interaction,
    'Does institutional delay itself modify the physical carbon budget by accelerating tipping-point dynamics (cloud feedbacks, forest dieback, permafrost melt)?',
    'Climate modeling comparison: carbon budget for 1.5°C stabilization conditional on immediate action versus conditional on 10-year delay. Quantify how tipping-point acceleration reduces physical buffer. Integration of fast feedbacks into institutional constraint model.',
    'If significant: the institutional delay IS part of the physical constraint (coupled system). The separation between ''mountain'' and ''institutional failure'' collapses — delay itself narrows the physical pathway. Epsilon and suppression increase because the constraint becomes irreversible beyond a threshold. Mandatrophy changes from institutional failure to irreversible physical lock-in.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tipping_point_interaction, empirical, 'Whether institutional delay modifies physical carbon budget via tipping-point acceleration').

omega_variable(
    just_transition_coordination_vs_extraction,
    'Does ''just transition'' framing coordinate genuine burden-sharing (rope) or rationalize unequal extraction by shifting costs to transition-dependent communities (snare/tangled rope)?',
    'Distributional analysis of transition costs and benefits across income quintiles, geographies, and time horizons. Comparison of pledge climate finance to actual transition funding flows. Examination of policy pathways: carbon tax with direct rebate (coordination) versus offset markets (extraction).',
    'If genuine coordination: transition-dependent perspectives shift from snare to tangled rope, suppression drops. If rationalization: beneficiary-victim structure persists, theater ratio increases (performative equity framing masks extraction). Affects classification of multiple perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_transition_coordination_vs_extraction, empirical, 'Whether just-transition framing enables coordination or masks extraction').

omega_variable(
    systemic_alternative_viability,
    'Do renewable energy systems plus grid modernization represent a genuine systemic alternative to fossil infrastructure, or does their scaling face irreducible bottlenecks?',
    'Technical analysis of mineral constraints (lithium, cobalt, rare earths), grid stability requirements, and manufacturing capacity for renewables deployment. Comparison of decarbonization cost curves under different technology availability scenarios. Modeling of rapid transition feasibility.',
    'If viable: incumbent suppression is extracted choice, not technical necessity — snare/tangled rope classification correct. If bottlenecked: transition timelines are physically constrained — mountain classification more defensible. Affects directionality and exit_options for all perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(systemic_alternative_viability, empirical, 'Whether renewable systems are viable systemic alternative').

omega_variable(
    institutional_capture_structural_permanence,
    'Is fossil fuel dominance in climate governance a temporary institutional capture (piton/scaffold with sunset) or structurally permanent (snare)?',
    'Historical analysis of regulatory capture dynamics in other sectors (tobacco, financial derivatives, pharmaceutical pricing). Examination of fossil fuel political spending, revolving-door dynamics, and institutional structural dependence. Scenario analysis of governance reform feasibility.',
    'If temporary: policy interventions can break capture — scaffold with sunset clause. If structural: capture is self-reinforcing — snare or permanent piton. Affects organizational perspective classification and mandatrophy resolution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_capture_structural_permanence, conceptual, 'Whether fossil fuel capture is temporary or structurally permanent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_stabilization_timeline, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(clim_tr_t0, climate_stabilization_timeline, theater_ratio, 0, 0.55).
narrative_ontology:measurement(clim_tr_t10, climate_stabilization_timeline, theater_ratio, 10, 0.68).
narrative_ontology:measurement(clim_tr_t20, climate_stabilization_timeline, theater_ratio, 20, 0.76).

% Extraction over time
narrative_ontology:measurement(clim_be_t0, climate_stabilization_timeline, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(clim_be_t10, climate_stabilization_timeline, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(clim_be_t20, climate_stabilization_timeline, base_extractiveness, 20, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(clim_su_t0, climate_stabilization_timeline, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(clim_su_t10, climate_stabilization_timeline, suppression_requirement, 10, 0.62).
narrative_ontology:measurement(clim_su_t20, climate_stabilization_timeline, suppression_requirement, 20, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_stabilization_timeline, global_infrastructure).
narrative_ontology:boltzmann_floor_override(climate_stabilization_timeline, 0.22).
narrative_ontology:affects_constraint(climate_stabilization_timeline, fossil_fuel_energy_infrastructure_lock_in).
narrative_ontology:affects_constraint(climate_stabilization_timeline, carbon_accounting_offset_mechanism).
narrative_ontology:affects_constraint(climate_stabilization_timeline, national_sovereignty_climate_governance).
narrative_ontology:affects_constraint(climate_stabilization_timeline, green_finance_greenwashing).

% DUAL FORMULATION NOTE:
% The climate stabilization timeline constraint decomposes into multiple structurally distinct stories: (1) carbon-budget physical limit (ε=0.05, mountain) — inherent thermodynamic constraint; (2) fossil-infrastructure lock-in (ε=0.65, snare) — incumbent energy system maintains position despite alternatives; (3) international-governance capture (ε=0.48, tangled rope) — climate policy coordinated but directed toward incumbent preservation; (4) temporal extraction mechanism (ε=0.72, snare) — present actors extract carbon budget from future actors. These four stories are linked: the physical limit (1) becomes a constraint only through political-economic choices (2-4). The present story focuses on the integrated institutional constraint; decomposition into separate stories is recommended for higher-resolution analysis of specific mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_stabilization_timeline, organized, 0.58).
constraint_indexing:directionality_override(climate_stabilization_timeline, institutional, 0.09).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
