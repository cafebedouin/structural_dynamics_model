% ============================================================================
% CONSTRAINT STORY: methane_emissions_deregulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_methane_emissions_deregulation, []).

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
 *   constraint_id: methane_emissions_deregulation
 *   human_readable: Methane Emissions Deregulation and Climate Extraction
 *   domain: environmental_policy/climate/energy
 *
 * SUMMARY:
 *   Methane emissions deregulation represents a structural extraction
 *   mechanism where present-generation energy producers capture regulatory
 *   relief while distributing climate costs to future generations and
 *   climate-vulnerable populations who cannot exit or negotiate. The
 *   constraint exhibits strong snare properties: high suppression
 *   (climate-vulnerable agents cannot relocate; future generations have zero
 *   voice in current policy), high extractiveness (deregulation directly
 *   transfers environmental liabilities to powerless agents), and a growing
 *   theater ratio (regulatory justifications increasingly rely on discounted
 *   climate models and contested cost-benefit analyses rather than direct
 *   risk mitigation). The deregulation occurred through regulatory capture:
 *   industry successfully reframed emissions standards as 'compliance
 *   burdens' rather than 'climate safeguards,' shifting the cognitive frame
 *   from environmental protection to regulatory efficiency. This reframing
 *   enabled extraction to persist under the appearance of coordination.
 *
 * KEY AGENTS:
 *   - Climate-Vulnerable Populations: Primary victims (powerless/trapped) — geographically and economically locked into exposure to methane-driven climate impacts
 *   - Future Generations: Primary victims (powerless/trapped/civilizational) — structurally unable to participate in deregulation decisions; inherit accumulated atmospheric methane
 *   - Fossil Fuel Producers: Primary beneficiary (institutional/arbitrage) — capture immediate profit from compliance cost reduction and production acceleration
 *   - Natural Gas Industry Lobbyists: Secondary beneficiary (institutional/arbitrage) — directly engineered deregulation through political influence
 *   - Local Air Quality Regulators: Mixed actor (moderate/constrained) — lose authority but gain simplification; structurally constrained by federal preemption
 *   - EPA Compliance Division: Institutional observer (institutional/constrained) — primary enforcement function degraded; maintains performative compliance activities
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees clear intergenerational extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(methane_emissions_deregulation, 0.68).
domain_priors:suppression_score(methane_emissions_deregulation, 0.72).
domain_priors:theater_ratio(methane_emissions_deregulation, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(methane_emissions_deregulation, extractiveness, 0.68).
narrative_ontology:constraint_metric(methane_emissions_deregulation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(methane_emissions_deregulation, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(methane_emissions_deregulation, snare).
narrative_ontology:human_readable(methane_emissions_deregulation, "Methane Emissions Deregulation and Climate Extraction").
narrative_ontology:topic_domain(methane_emissions_deregulation, "environmental_policy/climate/energy").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(methane_emissions_deregulation, fossil_fuel_producers).
narrative_ontology:constraint_beneficiary(methane_emissions_deregulation, natural_gas_companies).
narrative_ontology:constraint_victim(methane_emissions_deregulation, climate_stability).
narrative_ontology:constraint_victim(methane_emissions_deregulation, future_generations).
narrative_ontology:constraint_victim(methane_emissions_deregulation, climate_vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE-VULNERABLE POPULATIONS (SNARE) — Trapped in geography and economic dependency. Cannot exit the constraint. Bears full cost of deregulation through increased atmospheric methane, accelerated warming, extreme weather, crop failure, and resource scarcity. Zero alternatives, zero agency. Maximum experienced extraction.
constraint_indexing:constraint_classification(methane_emissions_deregulation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: FUTURE GENERATIONS (SNARE) — Structurally unable to participate in current policy decisions. Inherit a constraint (atmospheric composition) they did not choose and cannot modify. Maximum suppression and extraction across civilizational time. The quintessential intergenerational snare.
constraint_indexing:constraint_classification(methane_emissions_deregulation, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: FOSSIL FUEL PRODUCERS (ROPE) — Experience deregulation as pure coordination benefit. Reduced compliance costs, streamlined permitting, immediate profit growth. Net beneficiary with arbitrage options (can relocate production, shift to other jurisdictions). Sees the constraint as efficient governance that solves their coordination problem (getting permits faster).
constraint_indexing:constraint_classification(methane_emissions_deregulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: NATURAL GAS INDUSTRY LOBBYISTS (ROPE) — Direct beneficiaries of deregulation. Frame the constraint as solving the real coordination problem of regulatory burden. Have exit options (can shift to renewable lobbying, though career costly). Experience the constraint as genuine efficiency improvement for their constituents.
constraint_indexing:constraint_classification(methane_emissions_deregulation, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: LOCAL AIR QUALITY REGULATORS (TANGLED ROPE) — Constrained by federal preemption but also coordinate with industry on implementation. Experience mixed extraction and coordination: lose regulatory authority (extraction) but gain operational simplification for facilities (coordination). Career risk if they resist federal mandate; cost to localities if methane leaks increase. Moderate agency with real constraints.
constraint_indexing:constraint_classification(methane_emissions_deregulation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: EPA ENVIRONMENTAL COMPLIANCE DIVISION (PITON) — Once-functional regulatory body whose primary function (emissions monitoring and enforcement) has atrophied through deregulation. Maintains organizational structure and theater (continues to publish guidance documents, hold meetings, produce reports) despite reduced actual enforcement capacity. Theater ratio high because the division continues performing environmental compliance operations on constraints it no longer has authority to enforce.
constraint_indexing:constraint_classification(methane_emissions_deregulation, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational/global scope, deregulation is a pure extraction mechanism: it transfers present-generation energy/profit benefits to future generations in the form of accumulated atmospheric methane, which cannot be rapidly removed. The constraint persists because regulated entities have political power, climate-vulnerable agents have none. High extractiveness, high suppression, moderate theater (deregulation is presented as 'efficient' and 'science-based' despite known climate impact).
constraint_indexing:constraint_classification(methane_emissions_deregulation, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(methane_emissions_deregulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(methane_emissions_deregulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(methane_emissions_deregulation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(methane_emissions_deregulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(methane_emissions_deregulation, TR),
    TR >= 0.70.

:- end_tests(methane_emissions_deregulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and rising. At t=0, extractiveness was moderate (0.38) because deregulation had not yet driven measurable production increases or atmospheric methane accumulation. As deregulation took effect, compliance costs for producers dropped immediately (beneficiary extraction), while methane emissions rose with a lag (victim cost accumulation). The measurement trajectory shows extractiveness increasing to 0.68 by t=6 as atmospheric impacts became quantifiable. Suppression (0.72): High. Climate-vulnerable populations have extreme barriers to exit — geographic dependency, economic dependency, zero political voice in deregulation decisions. Future generations have absolute suppression (cannot participate). Regulatory suppression of alternatives (continued fossil fuel subsidies, infrastructure lock-in favoring gas) is also high. Theater ratio (0.58): Moderate-high. Deregulation is justified through efficiency narratives ('reducing regulatory burden,' 'streamlining permitting') that obscure the core extraction mechanism. Climate modeling is presented as uncertain justification for maintaining regulations, which in turn justifies their removal. The performative element increases over time as deregulation impacts become visible and are met with more elaborate justifications.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiaries and victims is stark. Fossil fuel producers see rope (coordination of efficient permitting, efficient energy delivery). Climate-vulnerable populations see snare (maximum extraction, zero alternatives). Local regulators see tangled_rope (loss of authority paired with operational simplification). The EPA sees piton (their own function degraded, but organizational structure persists). The analytical observer sees snare from civilizational scope, but might be tempted to see rope from immediate/national scope ('energy is necessary, deregulation enables efficient supply') — this is the false summit risk. The structural test: does deregulation solve a real coordination problem (rope) or does it merely extract regulatory rents while externalizing climate costs (snare)? The measurement trajectory answers this: extractiveness rising correlates with methane emissions rising, not with any improvement in energy coordination. The constraint persists not because it solves coordination but because regulated entities have political power.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by each agent's position relative to the extraction flow. Fossil fuel producers have d ≈ 0.05 (beneficiary + arbitrage → low d → negative experienced extraction). Climate-vulnerable populations have d ≈ 0.95 (victim + trapped → high d → maximum experienced extraction). Future generations have d ≈ 1.0 (pure victim, zero voice). Local regulators have d ≈ 0.60 (mixed role, constrained exit). The EPA compliance division has d ≈ 0.45 (institutional actor losing functional authority but maintaining procedural participation). The analytical observer has d ≈ 0.85 (structural analyst seeing the full extraction mechanism). These directionality values feed the sigmoid f(d) to produce experienced extractiveness chi for each perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE DIAGNOSIS CONFIRMED: The constraint resolves as pure snare from the analytical civilizational perspective. No beneficiary-victim balance exists that would justify tangled_rope — the benefits to producers are private and immediate, the costs to climate-vulnerable agents and future generations are diffuse and deferred but absolute. The key mandatrophy test: Is there a genuinely coordination function that extraction serves? Producers claim deregulation improves energy supply coordination (faster permits → more gas available → lower prices). But: (1) energy supply in developed economies is not constrained by permitting speed — it is constrained by infrastructure and demand; (2) deregulation does not serve coordination for future generations or climate-vulnerable populations — it actively prevents coordination (climate mitigation) and imposes extraction; (3) the beneficiary group (fossil fuel producers) and victim groups (climate-vulnerable, future generations) cannot negotiate or reach mutual benefit — this is not a cooperation problem, it is a power asymmetry. The constraint persists because fossil fuel producers have political power, not because deregulation solves a coordination problem. Snare classification holds.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    methane_leakage_attribution,
    'What proportion of measured atmospheric methane increase is attributable to deregulation vs. production volume growth independent of regulatory status?',
    'Comparison of methane emissions trajectories in jurisdictions with deregulation vs. maintained regulation; isotopic analysis of atmospheric methane to distinguish fossil vs. biogenic sources; correlation with production volumes and leak detection frequency',
    'If deregulation accounts for > 30% of increased emissions: extraction mechanism is clear and snare classification strengthened. If < 10%: extraction is marginal and constraint reclassifies toward tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(methane_leakage_attribution, empirical, 'Methane attribution to deregulation vs. production growth').

omega_variable(
    regulatory_capture_mechanism,
    'Did deregulation represent legitimate efficiency improvement requested by regulated entities, or regulatory capture where industry framing became policy without substantive cost-benefit analysis?',
    'Document analysis: regulatory impact statements, cost-benefit analyses, stakeholder consultation records; comparison with pre-deregulation risk assessments; tracking of actual compliance cost reductions vs. projected environmental cost increases',
    'If genuine efficiency: constraint reclassifies as tangled_rope (mixed coordination and extraction). If capture: snare classification confirmed and extraction flows become visible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Whether deregulation was efficiency improvement or capture').

omega_variable(
    intergenerational_discount_rate,
    'What discount rate converts future methane costs (climate damage, adaptation costs, resource depletion) into present value? Does any standard discount rate justify present extraction for future payment?',
    'Climate damage function estimation; cost-benefit analysis with alternative discount rates (0%, 2%, 7%); comparison with revealed preferences in other intergenerational policy domains',
    'If any defensible rate justifies the tradeoff: constraint reclassifies as tangled_rope with large benefit to present generation and large cost to future (still mixed, still possibly snare from future perspective). If no defensible rate: intergenerational extraction is indefensible and snare classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_discount_rate, preference, 'Intergenerational discount rate and cost-benefit justification').

omega_variable(
    alternative_energy_substitutability,
    'Are methane emissions from deregulated sources functionally necessary for current economic activity, or are they substitutable with renewable alternatives at acceptable cost and timescale?',
    'Techno-economic analysis of natural gas substitution rates; cost curves for renewable + storage alternatives; grid modeling of alternative energy scenarios; historical data on energy transition timescales',
    'If substitutable without major disruption: deregulation is pure extraction (snare confirmed). If functionally necessary: constraint reclassifies toward tangled_rope (extraction paired with genuine coordination of energy supply).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_energy_substitutability, empirical, 'Substitutability of natural gas with renewables').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(methane_emissions_deregulation, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(meth_tr_t0, methane_emissions_deregulation, theater_ratio, 0, 0.42).
narrative_ontology:measurement(meth_tr_t2, methane_emissions_deregulation, theater_ratio, 2, 0.48).
narrative_ontology:measurement(meth_tr_t4, methane_emissions_deregulation, theater_ratio, 4, 0.54).
narrative_ontology:measurement(meth_tr_t6, methane_emissions_deregulation, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(meth_be_t0, methane_emissions_deregulation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(meth_be_t2, methane_emissions_deregulation, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(meth_be_t4, methane_emissions_deregulation, base_extractiveness, 4, 0.61).
narrative_ontology:measurement(meth_be_t6, methane_emissions_deregulation, base_extractiveness, 6, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(methane_emissions_deregulation, resource_allocation).
narrative_ontology:boltzmann_floor_override(methane_emissions_deregulation, 0.12).
narrative_ontology:affects_constraint(methane_emissions_deregulation, climate_stability_threshold).
narrative_ontology:affects_constraint(methane_emissions_deregulation, intergenerational_justice_framework).
narrative_ontology:affects_constraint(methane_emissions_deregulation, fossil_fuel_subsidy_lock_in).

% DUAL FORMULATION NOTE:
% Methane deregulation is downstream of fossil fuel industry power concentration and upstream of climate instability feedback loops. The constraint links to broader fossil fuel extraction regimes but represents a distinct structural mechanism: the regulatory capture specifically enabling emissions acceleration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(methane_emissions_deregulation, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
