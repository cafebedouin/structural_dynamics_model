% ============================================================================
% CONSTRAINT STORY: infrastructure_as_force_multiplier
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_infrastructure_as_force_multiplier, []).

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
 *   constraint_id: infrastructure_as_force_multiplier
 *   human_readable: Infrastructure Targeting as Asymmetric Force Multiplier
 *   domain: military_strategy/asymmetric_warfare/energy_infrastructure
 *
 * SUMMARY:
 *   The strategic shift from territorial control to economic collapse via
 *   energy infrastructure targeting represents a fundamental innovation in
 *   asymmetric warfare force multiplication. Ukrainian strategic command has
 *   demonstrated that precision strikes on Russian oil export infrastructure
 *   (refineries, storage facilities, export terminals) can impose fiscal
 *   pressure ($100bn revenue at risk from 100m tonnes export capacity
 *   degradation) that exceeds the strategic value of territorial gains, at a
 *   fraction of the cost. The constraint exhibits Tangled Rope structure:
 *   genuine coordination function (precision strikes achieve strategic
 *   effects efficiently) coexists with asymmetric extraction (fiscal collapse
 *   mechanism imposes costs on trapped Russian fiscal system and civilian
 *   populations on both sides). The innovation is downstream of
 *   precision-mass economics (the mountain constraint that cheap precision
 *   systems can achieve effects previously requiring massed expensive
 *   platforms) but represents a distinct strategic application:
 *   infrastructure as the primary target rather than military formations.
 *   Theater ratio (0.35) reflects that some performative elements exist
 *   (strikes for signaling rather than effect, target selection for political
 *   rather than strategic value) but the core mechanism is functional —
 *   infrastructure degradation genuinely degrades military capacity via
 *   fiscal and recruitment channels.
 *
 * KEY AGENTS:
 *   - Ukrainian Strategic Command: Primary beneficiary (institutional/arbitrage) — achieves strategic effects (fiscal pressure, recruitment crisis, operational tempo reduction) at lower cost than territorial combat
 *   - Russian Fiscal Sustainability: Primary victim (powerless/trapped) — structurally dependent on energy export revenue (40% of federal budget), cannot exit, faces compounding pressure from revenue loss and military expenditure requirements
 *   - Russian Military Recruitment Capacity: Secondary victim (moderate/constrained) — faces demographic limits and economic competition from infrastructure degradation; benefits from coordination (centralized recruitment) but extraction is severe
 *   - Western Defense Industrial Base: Secondary beneficiary (institutional/arbitrage) — precision strike capability demonstration drives procurement and validates distributed systems over massed platforms
 *   - Civilian Energy Consumers (Both Sides): Tertiary victims (powerless/trapped) — energy infrastructure serves civilian needs but becomes military target; cannot exit energy dependency
 *   - Energy Transition Coalition: Organized agents (organized/mobile) — see infrastructure vulnerability as accelerant for transition to distributed renewable systems; sunset logic applies if transition actually occurs
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees irreducible hybrid of coordination (efficient strategic effect) and extraction (fiscal collapse mechanism with civilian spillover)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(infrastructure_as_force_multiplier, 0.48).
domain_priors:suppression_score(infrastructure_as_force_multiplier, 0.62).
domain_priors:theater_ratio(infrastructure_as_force_multiplier, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(infrastructure_as_force_multiplier, extractiveness, 0.48).
narrative_ontology:constraint_metric(infrastructure_as_force_multiplier, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(infrastructure_as_force_multiplier, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(infrastructure_as_force_multiplier, tangled_rope).
narrative_ontology:human_readable(infrastructure_as_force_multiplier, "Infrastructure Targeting as Asymmetric Force Multiplier").
narrative_ontology:topic_domain(infrastructure_as_force_multiplier, "military_strategy/asymmetric_warfare/energy_infrastructure").

domain_priors:requires_active_enforcement(infrastructure_as_force_multiplier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(infrastructure_as_force_multiplier, ukrainian_strategic_command).
narrative_ontology:constraint_beneficiary(infrastructure_as_force_multiplier, western_defense_industrial_base).
narrative_ontology:constraint_beneficiary(infrastructure_as_force_multiplier, energy_market_arbitrageurs).
narrative_ontology:constraint_victim(infrastructure_as_force_multiplier, russian_fiscal_sustainability).
narrative_ontology:constraint_victim(infrastructure_as_force_multiplier, russian_military_recruitment_capacity).
narrative_ontology:constraint_victim(infrastructure_as_force_multiplier, civilian_energy_consumers_both_sides).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RUSSIAN FISCAL SUSTAINABILITY (SNARE) — Trapped by structural dependency on energy export revenue (40% of federal budget from oil/gas). Cannot exit the constraint: alternative revenue sources require decades to develop, military spending is locked at 40% of budget ($212bn of $530bn), and recruitment deficit (30-34k monthly losses vs intake) creates compounding fiscal pressure. Maximum extraction: each infrastructure strike removes revenue capacity while forcing increased military expenditure to defend remaining assets.
constraint_indexing:constraint_classification(infrastructure_as_force_multiplier, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RUSSIAN MILITARY RECRUITMENT (TANGLED ROPE) — Constrained by demographic limits and economic incentives. Benefits from coordination function: centralized recruitment infrastructure enables mass mobilization. But extraction is severe: budget reallocation to military (40%) competes with recruitment incentives, and infrastructure degradation reduces economic alternatives that make military service attractive. Can theoretically exit via policy change (end conflict, shift budget priorities) but at prohibitive political cost.
constraint_indexing:constraint_classification(infrastructure_as_force_multiplier, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: UKRAINIAN STRATEGIC COMMAND (ROPE) — Primary beneficiary. Experiences constraint as coordination mechanism: precision strikes on energy infrastructure (oil refineries, storage, export terminals) achieve strategic effects (fiscal pressure, recruitment crisis, operational tempo reduction) at fraction of cost of territorial combat. Arbitrage exit: can shift to alternative strategies (territorial defense, attrition warfare) based on effectiveness. Low effective extraction — the constraint works in their favor.
constraint_indexing:constraint_classification(infrastructure_as_force_multiplier, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WESTERN DEFENSE INDUSTRIAL BASE (ROPE) — Secondary beneficiary. Precision strike capability (drones, long-range missiles) demonstrates force multiplication value, driving procurement and R&D investment. Coordination function: infrastructure targeting validates distributed precision systems over massed armor. Arbitrage exit: can pivot to other capability demonstrations. Experiences low extraction — the strategic shift creates market opportunities.
constraint_indexing:constraint_classification(infrastructure_as_force_multiplier, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CIVILIAN ENERGY CONSUMERS (TANGLED ROPE) — Trapped victims on both sides. Coordination function exists: energy infrastructure serves civilian heating, electricity, industrial production. But extraction is severe: infrastructure becomes military target, creating supply disruption, price volatility, and humanitarian crisis. Cannot exit — energy dependency is structural. Mixed experience: some coordination benefit (infrastructure exists) but high extraction (infrastructure weaponized).
constraint_indexing:constraint_classification(infrastructure_as_force_multiplier, tangled_rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 6: ENERGY TRANSITION COALITION (SCAFFOLD) — Organized agents (renewable energy sector, climate policy advocates, energy security strategists) see infrastructure vulnerability as temporary problem with sunset logic. Weaponization of fossil fuel infrastructure accelerates transition to distributed renewable systems (solar, wind, battery storage) that are harder to target and reduce strategic dependency. Estimated sunset: 15-25 years for energy transition to reduce infrastructure targeting effectiveness. Low extraction because coalition has agency and sees exit path through systemic change.
constraint_indexing:constraint_classification(infrastructure_as_force_multiplier, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees genuine coordination function (precision strikes achieve strategic effects at lower human cost than territorial combat) AND asymmetric extraction (fiscal collapse mechanism targets state capacity while imposing civilian costs). The strategic innovation is real — infrastructure targeting changes force multiplication economics — but the extraction is also real: the mechanism works by imposing economic collapse on a state's fiscal base, with spillover costs to trapped civilian populations. Tangled Rope classification reflects irreducible hybrid structure.
constraint_indexing:constraint_classification(infrastructure_as_force_multiplier, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(infrastructure_as_force_multiplier_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(infrastructure_as_force_multiplier, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(infrastructure_as_force_multiplier, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(infrastructure_as_force_multiplier, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(infrastructure_as_force_multiplier_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.48): Moderate-high. The constraint extracts from Russian fiscal sustainability (revenue loss, forced military expenditure increase) and civilian populations (energy supply disruption, price volatility). But extraction is not maximal — some strategic coordination benefit exists (precision strikes reduce human cost vs territorial combat), and the mechanism is targeted rather than indiscriminate. The value reflects genuine force multiplication (coordination) alongside severe fiscal pressure (extraction). Suppression (0.62): High. Russian fiscal system is trapped by structural energy export dependency — alternative revenue sources require decades to develop. Military budget allocation (40% of $530bn) is locked by conflict requirements. Recruitment deficit (30-34k monthly losses vs intake) creates compounding pressure. Exit options are severely constrained but not zero (conflict termination is theoretically possible). Theater ratio (0.35): Moderate-low. Some strikes are performative (signaling, political messaging, target selection for visibility rather than strategic value), but core mechanism is functional — infrastructure degradation genuinely degrades military capacity via fiscal and recruitment channels. Theater has increased slightly over interval as target hardening and air defense improvements reduce strike effectiveness, forcing more strikes for same effect.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same structural mechanism — precision strikes on energy infrastructure — appears as coordination (Rope) from the beneficiary perspective (Ukrainian command, Western defense industry), mixed coordination-extraction (Tangled Rope) from moderate agents (Russian recruitment, civilians, analytical observer), pure extraction (Snare) from the trapped victim perspective (Russian fiscal sustainability), and temporary problem with sunset (Scaffold) from organized agents with exit paths (energy transition coalition). The gap is not measurement error — it reflects genuine differences in structural position. Ukrainian command experiences the constraint as efficient force multiplication. Russian fiscal system experiences it as inescapable economic collapse. Civilians experience it as weaponization of essential infrastructure. Energy transition advocates experience it as accelerant for systemic change. The analytical observer sees the irreducible hybrid: the strategic innovation is real (coordination function exists) AND the extraction is real (fiscal collapse mechanism with civilian costs).
 *
 * DIRECTIONALITY LOGIC:
 *   Ukrainian Strategic Command is primary beneficiary with arbitrage exit options — experiences low d (≈0.10), negative f(d), and low/negative effective extraction. The constraint works in their favor: precision infrastructure strikes achieve strategic effects at lower cost than alternatives. Russian Fiscal Sustainability is primary victim with trapped exit — experiences high d (≈0.92), high f(d) (≈1.35), and maximum effective extraction. Structurally dependent on energy exports, cannot exit, bears compounding costs. Russian Military Recruitment is secondary victim with constrained exit — experiences moderate-high d (≈0.70), moderate-high f(d) (≈1.10), reflecting mixed position: benefits from recruitment coordination but extraction is severe due to budget competition and demographic limits. Western Defense Industrial Base is secondary beneficiary with arbitrage exit — experiences low d (≈0.15), low f(d), reflecting market opportunity from capability demonstration. Civilian Energy Consumers are tertiary victims with trapped exit — experience high d but lower than fiscal sustainability because they retain some agency (can reduce consumption, seek alternatives). Energy Transition Coalition is organized with mobile exit — experiences moderate d (≈0.50) because they see both problem (infrastructure vulnerability) and solution (distributed renewables), with agency to pursue transition. Analytical observer uses canonical analytical d (≈0.72) reflecting structural position outside the constraint but measuring its effects.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification resolves mandatrophy by acknowledging both structural features: (1) Genuine coordination function — precision infrastructure targeting achieves strategic effects (fiscal pressure, recruitment crisis, operational tempo reduction) at lower human and material cost than territorial combat. This is a real military innovation, not pure extraction. (2) Asymmetric extraction — the mechanism works by imposing economic collapse on a trapped fiscal system (Russian energy export dependency) with spillover costs to civilian populations who cannot exit energy dependency. The extraction is structural, not incidental. The constraint is neither pure coordination (Rope) — the fiscal collapse mechanism and civilian costs are too severe — nor pure extraction (Snare) — the force multiplication benefit and cost-effectiveness are too real. It is irreducibly hybrid. The mandatrophy trap would be to classify based on a single perspective: if measuring from Ukrainian command alone, appears as Rope (efficient strategy). If measuring from Russian fiscal sustainability alone, appears as Snare (inescapable extraction). If measuring from civilians alone, appears as violation of dual-use targeting norms. The Tangled Rope classification integrates all perspectives and acknowledges the structural ambiguity: this is both an innovation in force multiplication economics AND an extraction mechanism that targets state fiscal capacity with civilian spillover.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    civilian_infrastructure_dual_use_threshold,
    'At what point does energy infrastructure transition from legitimate military target (dual-use: powers military logistics) to illegitimate civilian target (primary function: civilian heating/electricity)?',
    'Proportionality analysis: ratio of military vs civilian energy consumption from targeted infrastructure; international humanitarian law interpretation of dual-use targeting',
    'If threshold favors military targeting: constraint remains Tangled Rope (coordination + extraction). If threshold restricts targeting: constraint becomes Snare from Ukrainian perspective (loses force multiplier), Rope from civilian perspective (reduced extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(civilian_infrastructure_dual_use_threshold, preference, 'Dual-use threshold for infrastructure targeting legitimacy').

omega_variable(
    fiscal_collapse_timeline,
    'How long can Russian fiscal system sustain 40% military budget allocation with degraded energy export revenue before recruitment/operational collapse?',
    'Economic modeling: reserve depletion rate, alternative revenue activation timeline, military expenditure floor requirements, recruitment incentive sustainability',
    'If timeline < 18 months: infrastructure targeting is decisive strategic lever (high coordination value). If timeline > 36 months: targeting is attritional pressure (lower coordination value, higher extraction relative to effect).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_collapse_timeline, empirical, 'Timeline for fiscal sustainability under infrastructure degradation').

omega_variable(
    precision_cost_effectiveness_threshold,
    'At what cost ratio does precision infrastructure targeting remain more effective than territorial combat for achieving strategic effects?',
    'Comparative analysis: cost per strategic effect (fiscal pressure, recruitment crisis, operational tempo reduction) via infrastructure strikes vs territorial gains; includes drone/missile production costs, target hardening costs, alternative strategy costs',
    'If ratio remains favorable (< 1:10 cost vs territorial combat): Rope classification for Ukrainian command holds. If ratio degrades (target hardening, air defense improvements): shifts toward Tangled Rope or Snare as extraction increases relative to coordination benefit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(precision_cost_effectiveness_threshold, empirical, 'Cost-effectiveness threshold for infrastructure targeting vs alternatives').

omega_variable(
    energy_transition_acceleration_effect,
    'Does infrastructure weaponization actually accelerate energy transition, or does it entrench fossil fuel dependency through reconstruction investment?',
    'Longitudinal analysis: post-conflict energy infrastructure investment patterns (renewable vs fossil reconstruction); policy response to infrastructure vulnerability (distributed systems vs hardened centralized systems)',
    'If accelerates transition: Scaffold perspective confirmed (real sunset). If entrenches dependency: Scaffold is aspirational, constraint persists as Tangled Rope with no sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(energy_transition_acceleration_effect, empirical, 'Whether infrastructure targeting accelerates or delays energy transition').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(infrastructure_as_force_multiplier, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infra_fm_theater_t0, infrastructure_as_force_multiplier, theater_ratio, 0, 0.25).
narrative_ontology:measurement(infra_fm_theater_t6, infrastructure_as_force_multiplier, theater_ratio, 6, 0.3).
narrative_ontology:measurement(infra_fm_theater_t12, infrastructure_as_force_multiplier, theater_ratio, 12, 0.35).

% Extraction over time
narrative_ontology:measurement(infra_fm_extract_t0, infrastructure_as_force_multiplier, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(infra_fm_extract_t6, infrastructure_as_force_multiplier, base_extractiveness, 6, 0.4).
narrative_ontology:measurement(infra_fm_extract_t12, infrastructure_as_force_multiplier, base_extractiveness, 12, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(infrastructure_as_force_multiplier, enforcement_mechanism).
narrative_ontology:affects_constraint(infrastructure_as_force_multiplier, precision_mass_economics).

% DUAL FORMULATION NOTE:
% Infrastructure targeting as force multiplier is downstream of precision-mass economics (the mountain constraint that cheap precision systems achieve effects previously requiring expensive massed platforms). Precision-mass economics is the enabling technology constraint (ε ≈ 0.08, mountain from all perspectives — physical/economic law that precision scales differently than mass). Infrastructure targeting is the strategic application constraint (ε = 0.48, tangled rope — genuine coordination function coexists with asymmetric extraction). The upstream constraint (precision-mass economics) has low extractiveness because it is a technological/economic relationship. The downstream constraint (infrastructure targeting) has moderate-high extractiveness because it is a strategic choice about how to apply the technology, with identifiable beneficiaries and victims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
