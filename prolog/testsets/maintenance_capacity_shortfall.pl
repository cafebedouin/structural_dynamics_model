% ============================================================================
% CONSTRAINT STORY: maintenance_capacity_shortfall
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_maintenance_capacity_shortfall, []).

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
 *   constraint_id: maintenance_capacity_shortfall
 *   human_readable: The Entropic Debt Trap: Maintenance Capacity Shortfall
 *   domain: infrastructure/logistical/technological
 *
 * SUMMARY:
 *   The maintenance capacity shortfall emerges when the physical or digital
 *   complexity of critical infrastructure systems grows faster than the human
 *   and financial resources allocated for their upkeep. This constraint
 *   demonstrates a structural tension between two competing dynamics: (1) the
 *   coordination benefit that infrastructure operators gain from deferring
 *   maintenance costs into future budgets, and (2) the accumulating
 *   extraction costs borne by maintenance workforces, future users, and
 *   system reliability. The constraint exhibits properties of both
 *   coordination (Rope) and extraction (Snare/Tangled Rope) from different
 *   perspectives. System operators see budget flexibility and short-term
 *   fiscal coordination; maintenance workers see escalating uncompensable
 *   demands; future users face degraded service or catastrophic system
 *   replacement; organized technical coalitions see a temporary problem
 *   solvable through digital twins and modular redesign within 10-15 years
 *   (Scaffold). The theater ratio (0.68) reflects that maintenance governance
 *   has become increasingly performative: agencies produce condition
 *   assessments, strategic plans, and lifecycle analyses that have become
 *   decoupled from actual resource allocation, creating an elaborate ritual
 *   of management that masks underlying capacity collapse.
 *
 * KEY AGENTS:
 *   - System Operators / Budget Holders: Primary beneficiary (institutional/arbitrage) — extract short-term fiscal flexibility by deferring maintenance costs
 *   - Maintenance Workforce: Primary victim (powerless/trapped) — face escalating demands on fixed time budgets with no exit
 *   - Future Infrastructure Users: Secondary victim (moderate/constrained) — benefit from infrastructure coordination but bear accumulating extraction costs
 *   - Equipment Manufacturers and Service Contractors: Mixed position (organized/constrained) — benefit from coordination function and capture extraction through vendor lock-in
 *   - Infrastructure Innovation Coalition: Organized actors (organized/mobile) — technical and policy leaders building predictive maintenance and modular design solutions with explicit sunset logic
 *   - The Maintenance Deferral Ritual: Institutional practice (institutional/arbitrage) — governance theater persists despite atrophied function
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional scarcity as physical law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(maintenance_capacity_shortfall, 0.58).
domain_priors:suppression_score(maintenance_capacity_shortfall, 0.62).
domain_priors:theater_ratio(maintenance_capacity_shortfall, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(maintenance_capacity_shortfall, extractiveness, 0.58).
narrative_ontology:constraint_metric(maintenance_capacity_shortfall, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(maintenance_capacity_shortfall, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(maintenance_capacity_shortfall, tangled_rope).
narrative_ontology:human_readable(maintenance_capacity_shortfall, "The Entropic Debt Trap: Maintenance Capacity Shortfall").
narrative_ontology:topic_domain(maintenance_capacity_shortfall, "infrastructure/logistical/technological").

domain_priors:requires_active_enforcement(maintenance_capacity_shortfall).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maintenance_capacity_shortfall, system_operators).
narrative_ontology:constraint_beneficiary(maintenance_capacity_shortfall, deferred_investment_beneficiaries).
narrative_ontology:constraint_victim(maintenance_capacity_shortfall, maintenance_workforce).
narrative_ontology:constraint_victim(maintenance_capacity_shortfall, future_stakeholders).
narrative_ontology:constraint_victim(maintenance_capacity_shortfall, system_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MAINTENANCE WORKFORCE (SNARE) — Frontline workers face escalating demands on finite time budgets as system complexity outpaces staffing levels. Trapped by employment contracts and lack of alternative roles; bear full cost of deferred maintenance decisions made above their organizational level. Suppression is total: no ability to negotiate workload, limited voice in resource allocation, and professional identity bound to 'keeping things running.'
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FUTURE INFRASTRUCTURE USERS (TANGLED ROPE) — Structurally benefit from the infrastructure coordination function (roads, power grids, water systems exist and operate). But also bear accumulating extraction costs: degraded service quality, higher failure rates, crisis-driven emergency spending, and eventual catastrophic system replacement rather than continuous upgrade. Constrained exit: cannot simply choose another infrastructure; must bear the compounding costs of deferred maintenance.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SYSTEM OPERATORS AND BUDGET HOLDERS (ROPE) — Institutional actors experience the constraint as a coordination mechanism: deferring maintenance preserves fiscal flexibility in the immediate budget cycle, enabling reallocation to other priorities (expansion, political initiatives, debt service). Net beneficiaries through short-term arbitrage: they enjoy the coordination benefit (system keeps running), avoid the coordination cost of admitting capacity limits, and extract deferred liabilities into the future. Low experienced chi because they have arbitrage exit (can reallocate funds and move to new roles).
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE MAINTENANCE DEFERRAL RITUAL (PITON) — Organizational and fiscal practices around 'deferred maintenance accounting' are largely theatrical: agencies report maintenance backlogs in standardized formats, hold planning meetings, and publish infrastructure condition assessments, but these rituals have become decoupled from actual resource allocation. The performance of maintenance governance persists despite atrophying real function. Theater ratio reflects that the ritual of planning and reporting maintenance requirements exceeds the actual execution of maintenance work.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INFRASTRUCTURE INNOVATION COALITION (SCAFFOLD) — Organized technical and policy actors (systems engineers, lifecycle-cost analysts, predictive maintenance proponents, asset management firms) view the bottleneck as a temporary coordination failure with a concrete sunset: digital twins, condition-based maintenance algorithms, and modular design strategies are creating pathways to decouple complexity from labor demand. These agents have mobile exit (can move to new projects) and see the entropic debt trap as solvable within a 10-15 year horizon through technology and process redesign. High suppression is tolerated only if it declines over the sunset horizon.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: EQUIPMENT MANUFACTURERS AND SERVICE CONTRACTORS (TANGLED ROPE) — Institutional actors who benefit from the coordination role (system operators need them to supply replacement parts, retrofit services, emergency repairs) while also capturing extraction value through vendor lock-in, emergency pricing, and service monopolies. Constrained exit relative to operators but more mobile than workforce: can exit to other infrastructure domains if maintenance budgets collapse. Mixed beneficiary-victim position drives tangled rope classification.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / THERMODYNAMIC LIMIT (MOUNTAIN) — From a civilizational scale, the maintenance capacity shortfall reflects the fundamental thermodynamic law that complex systems increase in entropy: as technological complexity grows (more sensors, networked controls, interdependencies), the labor and energy required to maintain order scales superlinearly. No institutional arrangement can escape this physical constraint — all sufficiently complex systems face maintenance costs that eventually exceed operating budgets. However, structural data (organized coalitions solving via digital twins, demonstrated predictive maintenance successes, modular design adoption) contradicts mountain classification, revealing this as naturalization of institutional scarcity rather than physical necessity.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maintenance_capacity_shortfall_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(maintenance_capacity_shortfall, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maintenance_capacity_shortfall, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(maintenance_capacity_shortfall, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(maintenance_capacity_shortfall, TR),
    TR >= 0.70.

:- end_tests(maintenance_capacity_shortfall_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated but not maximal. The constraint exhibits moderate to high extraction through deferred maintenance and workforce demand inflation, but it is not an absolute extortion mechanism like predatory lending (which would reach ≥0.70). The extraction is tempered by the fact that some maintenance actually occurs, and operators face genuine budget constraints rather than pure choice to extract. The measurement trajectory (0.32 → 0.58 over 10 years) shows acceleration reflecting growing complexity outpacing workforce capacity. Suppression (0.62): High. Multiple barriers limit exit for maintenance workers: specialized skills are often location-specific, employment contracts are rigid, alternative careers are limited, and union/collective action capacity is often weak. System operators face institutional suppression (political pressure against raising taxes for maintenance, entrenched spending priorities). Future users face ultimate suppression (cannot exit the infrastructure dependency). Theater ratio (0.68): High and rising. Maintenance governance has become substantially performative. Agencies conduct asset condition assessments, publish infrastructure report cards, and hold strategic planning meetings that follow professional standards and appear responsive, but these rituals have decoupled from resource allocation. The performative content increases over the interval as the gap between what plans specify and what budgets support widens.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates large perspectival gaps across the seven perspectives. System operators see primarily Rope (coordination benefit, low experienced extraction); maintenance workers see Snare (pure extraction, maximum experienced cost); future users see Tangled Rope (mixed benefit and cost); the innovation coalition sees Scaffold (temporary problem with technical sunset); the ritual itself is Piton (performative inertia); vendors see Tangled Rope (mixed benefit and extraction through lock-in); and the analytical observer risks seeing Mountain (naturalizing institutional limits as physical laws). These gaps are not measurement errors — they reflect genuine structural differences in how each agent experiences the same constraint. The system operators' experience of Rope is real (they do solve a genuine coordination problem by deferring costs); the maintenance workers' experience of Snare is real (they do bear extraction with no exit); the future users' experience of Tangled Rope is real (they both depend on and subsidize the deferred-cost system). No single perspective is 'correct' — the presheaf over all perspectives is the analytical answer.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality computation follows from structural position: who benefits, who bears costs, what exit options exist. System operators (beneficiaries + arbitrage) → low d → low experienced chi. Maintenance workers (victims + trapped) → high d → high experienced chi. The measurement trajectory shows chi increasing over time not because base properties change dramatically (ε rises from 0.32 to 0.58) but because suppression (barriers to exit for workers) remains constant while extraction accumulates. The piton classification reflects theater_ratio rising from 0.42 to 0.68 — governance rituals (assessments, plans, reporting) escalate while actual maintenance execution stagnates, indicating the primary function (maintaining infrastructure adequately) has atrophied. The scaffold perspective's mobile exit and organized power derive d ≈ 0.35 → low experienced chi even though base extractiveness is the same, because the agents see a genuine sunset and have technical/career mobility.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY DECOMPOSITION: The maintenance capacity shortfall decomposes into multiple structurally distinct claims when examined at different scopes and time horizons. At the immediate/institutional level (system operators' perspective), it is Rope — a genuine coordination mechanism that produces mutual benefit (operators get flexibility, infrastructure keeps running). At the biographical/workforce level, it is Snare — pure extraction with no coordination benefit for the victim class. At the generational/societal level, it is Tangled Rope — infrastructure provides coordination function (roads, power, water) while accumulating extraction costs (deferred maintenance, compound failure risk). At the technical/aspirational level, it is Scaffold — the innovation coalition offers a sunset mechanism (predictive maintenance, modularity) that would convert it from Snare/Tangled Rope to low-chi coordination within 10-15 years. The mandatrophy is resolved by recognizing that all four types are legitimate perspectival readings: the constraint is NOT a single type, but a presheaf whose classification depends on observational position. The false summit (Mountain perspective) is revealed by the fact that organized technical solutions exist and have demonstrated effectiveness in pilot deployments — the 'physical inevitability' framing is institutional naturalization, not thermodynamic fact. The constraint's real crisis is that the Rope perception (system operators' experience) has been isolated from the Snare reality (maintenance workforce's experience) through organizational structures (centralized budgets, distributed suffering) that prevent feedback or coalition formation. The mandatrophy resolution requires acknowledging this perspectival plurality and the institutional mechanisms that suppress cross-perspective visibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complexity_scaling_exponent,
    'Does maintenance labor demand scale linearly, polynomially, or exponentially with system complexity?',
    'Longitudinal analysis of maintenance hours vs system complexity metrics (number of components, interdependency degree, sensor count) across multiple infrastructure domains and technology cycles',
    'If linear: capacity shortfall is a budget allocation problem (Rope with poor distribution). If superlinear: shortfall has structural/physical character (Mountain candidates). If exponential: the entropic debt trap becomes inevitable given any fixed maintenance budget.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complexity_scaling_exponent, empirical, 'Relationship between system complexity and maintenance labor demand').

omega_variable(
    predictive_maintenance_effectiveness,
    'Can condition-based and predictive maintenance algorithms reduce total maintenance labor demand by > 30% while maintaining reliability standards?',
    'Pilot deployments of digital twins and condition-based systems in transit, water, and power infrastructure; comparison of labor hours before/after algorithm adoption; failure rate tracking',
    'If yes, > 30%: scaffold sunset is real and achievable. The entropic debt trap has a genuine technical solution pathway. If no or < 15%: digital solutions merely shift workload rather than reduce it, and the organizational/budget constraints remain primary (Snare/Tangled Rope from most perspectives).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(predictive_maintenance_effectiveness, empirical, 'Whether predictive maintenance can significantly reduce labor demand').

omega_variable(
    modularity_and_replacement_cost_trade,
    'Do modular, replaceable-unit infrastructure designs reduce total lifecycle cost despite higher per-unit component costs?',
    'Cost accounting comparison: traditional monolithic systems vs modular systems over 30-year lifecycle, including maintenance labor, spare parts inventory, downtime costs, and eventual replacement',
    'If lifecycle cost lower: modularity is a genuine capacity solution and scaffold perspective is validated. If lifecycle cost higher: modularity merely redistributes extraction (vendors profit; operating budgets still constrained) without solving capacity limits.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modularity_and_replacement_cost_trade, empirical, 'Lifecycle cost comparison of modular vs traditional infrastructure').

omega_variable(
    institutional_budget_allocation_binding,
    'Is the maintenance capacity shortfall driven by genuine resource scarcity or by institutional choice to prioritize other spending categories?',
    'Cross-jurisdictional comparison of maintenance funding as percentage of total infrastructure budget; analysis of political economy of deferred maintenance (which constituencies benefit from non-maintenance spending)',
    'If resource scarcity: mountain or rope classification justified — the limit is real. If institutional choice: tangled rope and snare classifications dominate — the extraction and suppression are political, not physical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(institutional_budget_allocation_binding, conceptual, 'Whether shortfall reflects genuine scarcity or institutional allocation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maintenance_capacity_shortfall, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(maint_tr_t0, maintenance_capacity_shortfall, theater_ratio, 0, 0.42).
narrative_ontology:measurement(maint_tr_t5, maintenance_capacity_shortfall, theater_ratio, 5, 0.55).
narrative_ontology:measurement(maint_tr_t10, maintenance_capacity_shortfall, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(maint_be_t0, maintenance_capacity_shortfall, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(maint_be_t5, maintenance_capacity_shortfall, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(maint_be_t10, maintenance_capacity_shortfall, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maintenance_capacity_shortfall, enforcement_mechanism).
narrative_ontology:affects_constraint(maintenance_capacity_shortfall, infrastructure_resilience_degradation).
narrative_ontology:affects_constraint(maintenance_capacity_shortfall, critical_system_cascading_failure).
narrative_ontology:affects_constraint(maintenance_capacity_shortfall, workforce_burnout_and_exodus).

% DUAL FORMULATION NOTE:
% The maintenance capacity shortfall is a primary constraint whose dynamics propagate to dependent constraints in the infrastructure reliability family. As base extractiveness increases (ε: 0.32 → 0.58 over 10 years), downstream constraints in critical system reliability become increasingly constrained. The innovation coalition's scaffold perspective suggests a genuine technical pathway to decompose this into a temporary coordination problem with sunset clauses, but that pathway requires institutional adoption and capital investment that the current deferred-maintenance trap actively discourages.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maintenance_capacity_shortfall, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
