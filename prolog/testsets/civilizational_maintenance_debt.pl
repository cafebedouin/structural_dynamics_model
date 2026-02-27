% ============================================================================
% CONSTRAINT STORY: civilizational_maintenance_debt
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_civilizational_maintenance_debt, []).

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
 *   constraint_id: civilizational_maintenance_debt
 *   human_readable: The Crumbling Foundation: Civilizational Maintenance Debt
 *   domain: technological/infrastructure/systemic
 *
 * SUMMARY:
 *   The civilizational maintenance debt represents a fundamental structural
 *   tension between current consumption and system sustainability.
 *   Infrastructure systems (water, electricity, transportation, communication
 *   networks, buildings) provide essential coordination services that enable
 *   modern life. But the extraction mechanism — systematically deferring
 *   maintenance costs to future periods and future generations — is built
 *   into the institutional structure of public finance, capital allocation,
 *   and political time horizons. Current beneficiaries (developers,
 *   investors, consuming citizens) profit from maintained infrastructure
 *   while avoiding maintenance costs through political choices (tax
 *   limitations, budget prioritization, federal unfunded mandates). This
 *   creates a pure snare for infrastructure maintenance workers (trapped by
 *   necessity), powerless future generations (trapped by inheritance), and
 *   the integrity of the infrastructure commons itself. The constraint
 *   exhibits tangled rope characteristics because municipal governments and
 *   infrastructure systems do coordinate essential services — they are not
 *   pure extraction mechanisms. But they simultaneously participate in cost
 *   externalization across time and populations.
 *
 * KEY AGENTS:
 *   - Infrastructure Maintenance Workers: Primary direct victim (powerless/trapped) — face unsafe conditions, resource inadequacy, impossible triage decisions with no authority to redirect resources
 *   - Municipal Governments: Primary institutional actor (moderate/constrained) — coordinate essential services but also participate in cost deferral due to budget constraints and regulatory restrictions
 *   - Current Consumption Beneficiaries: Primary beneficiary (institutional/arbitrage) — capital investors, developers, homeowners who benefit from infrastructure without bearing proportional maintenance costs; can arbitrage away from jurisdiction
 *   - Future Generations: Intergenerational victim (powerless/trapped) — inherit both the infrastructure systems and the accumulated debt; face catastrophic repair costs or system failures
 *   - Infrastructure Inspection Bureaucracy: Institutional theater (institutional/constrained) — produces extensive reports and compliance certification without enforcement power or funding authority
 *   - Infrastructure Advocacy Coalition: Organized actors (organized/constrained) — civil engineering societies, ASCE, advocacy groups; coordinate problem recognition but lack political authority to redirect resources
 *   - Alternative Infrastructure Developers: Emerging scaffold actors (organized/mobile) — green infrastructure, distributed resilience, performance-based funding models; building exit pathways with sunset logic
 *   - Analytical Observer: Civilizational analytical perspective (analytical/analytical) — risks naturalizing political economy choice as thermodynamic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(civilizational_maintenance_debt, 0.58).
domain_priors:suppression_score(civilizational_maintenance_debt, 0.68).
domain_priors:theater_ratio(civilizational_maintenance_debt, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(civilizational_maintenance_debt, extractiveness, 0.58).
narrative_ontology:constraint_metric(civilizational_maintenance_debt, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(civilizational_maintenance_debt, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(civilizational_maintenance_debt, tangled_rope).
narrative_ontology:human_readable(civilizational_maintenance_debt, "The Crumbling Foundation: Civilizational Maintenance Debt").
narrative_ontology:topic_domain(civilizational_maintenance_debt, "technological/infrastructure/systemic").

domain_priors:requires_active_enforcement(civilizational_maintenance_debt).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(civilizational_maintenance_debt, current_consumption_beneficiaries).
narrative_ontology:constraint_beneficiary(civilizational_maintenance_debt, capital_intensive_industries).
narrative_ontology:constraint_victim(civilizational_maintenance_debt, infrastructure_maintenance_systems).
narrative_ontology:constraint_victim(civilizational_maintenance_debt, future_generations).
narrative_ontology:constraint_victim(civilizational_maintenance_debt, public_safety_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFRASTRUCTURE MAINTENANCE WORKER (SNARE) — Trapped by economic necessity to maintain failing systems with inadequate resources. Bears full cost of deferred maintenance through unsafe working conditions, burnout, and impossible triage decisions. Cannot exit — maintaining critical systems is both their livelihood and a social requirement. Zero negotiating power over resource allocation. Maximum experienced extraction.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MUNICIPAL GOVERNMENT (TANGLED ROPE) — Both coordinates essential services AND extracts through deferred costs. Provides coordination (water systems, electrical grids, road networks) that would not exist through pure market mechanisms. But also benefits from shifting maintenance costs to future budgets and external actors. Constrained by property tax limitations, state regulations, and federal unfunded mandates. Mixed experience: genuine coordination function plus asymmetric cost externalization.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CURRENT CONSUMPTION BENEFICIARIES (ROPE) — Institutional actors (developers, capital investors, current taxpayers) benefit from maintained infrastructure while avoiding maintenance costs. Experience the constraint as pure coordination: infrastructure exists and functions through collective action, enabling economic activity. Can arbitrage away from jurisdiction-specific maintenance obligations through relocation or investment diversification. Net beneficiary — extraction runs toward this actor class.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: FUTURE GENERATIONS (SNARE) — Trapped by inheritance of both the infrastructure systems and the accumulated debt. Cannot exit — must inherit and maintain or face civilizational collapse. Cannot negotiate or influence current allocation decisions. Bears catastrophic costs of deferred maintenance: either massive emergency repairs with economic disruption or system failures affecting essential services. Maximum extraction across temporal dimension.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: INFRASTRUCTURE INSPECTION BUREAUCRACY (PITON) — Performs extensive ritual inspections, reporting, and compliance certification while having no authority to fund repairs or enforce maintenance timelines. Theater_ratio is high: agencies produce detailed condition reports, maintain databases, conduct ceremonies around 'infrastructure summits' and studies, but lack enforcement power. The bureaucracy persists through institutional inertia — it signals concern without solving the problem. Maintenance function has atrophied; the ritual remains.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ENGINEERING LIFECYCLE STANDARDS (PITON) — The formal engineering models of infrastructure lifecycles (30-year bridge lifespan, 50-year water main cycles) are theatrical performance: they prescribe maintenance schedules that are systematically ignored due to budget constraints. These standards persist as institutional theater — they provide a standard to ostensibly follow and measure against — but have atrophied in functional force. Actual maintenance follows a different logic (crisis-driven, capital-constrained) entirely divorced from design specifications.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: INFRASTRUCTURE ADVOCACY COALITION (TANGLED ROPE) — Organized agents (ASCE, civil engineering societies, infrastructure advocacy NGOs) coordinate around shared recognition of the maintenance debt problem. They provide genuine coordination function by aggregating data, setting standards, and enabling collective attention. But they also face extraction constraints: they lack funding authority and enforcement power, and their advocacy is systematically captured by those benefiting from status quo (current consumers, capital investors). Constrained by political economy — their coordination efforts are partially neutered by structural incentives to defer costs.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ALTERNATIVE INFRASTRUCTURE MODELS (SCAFFOLD) — Emerging agents (green infrastructure advocates, distributed resilience networks, public-private partnerships with performance-based funding) are building temporary coordination mechanisms with explicit sunset logic. These models (stormwater gardens replacing piped systems, microgrids replacing centralized generation, modular repair protocols) are scaffolds: they reduce extraction costs by distributing maintenance burdens and incorporating lifecycle thinking from inception. Sunset logic: as these alternatives mature and prove cost-effective, the traditional maintenance-debt model loses functional force. But adoption is slow — constrained by infrastructure lock-in and incumbent interests.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the maintenance debt appears as an immutable law of complex systems: all infrastructure degrades; entropy increases; deferring maintenance is a free energy gradient that nature enforces. This perspective sees the crumbling foundation as an inevitable feature of thermodynamic reality rather than a contingent institutional choice. The 'natural' reading risks obscuring that maintenance deferral is a political economy decision, not a law of physics.
constraint_indexing:constraint_classification(civilizational_maintenance_debt, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(civilizational_maintenance_debt_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(civilizational_maintenance_debt, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(civilizational_maintenance_debt, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(civilizational_maintenance_debt, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(civilizational_maintenance_debt, TR),
    TR >= 0.70.

:- end_tests(civilizational_maintenance_debt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint systematically extracts value from maintenance functions and future capacity to benefit current consumption. The extraction is substantial but not maximal because some genuine coordination function persists — infrastructure systems do provide collective benefits that would not exist in pure market allocation. The upward trajectory (0.32 → 0.58 over 20 years) reflects accumulating debt and increasing gap between needed and allocated maintenance. Suppression (0.68): High. Significant barriers prevent exit or resistance: maintenance is essential (no alternative), workers are trapped by economic necessity, future generations cannot negotiate, and political structures systematically underweight future costs. Media and public discourse suppress the severity — the crisis is discussed through statistics and reports rather than visceral experience until catastrophic failure. Theater ratio (0.65): Moderate-high. Infrastructure inspections, state-of-repair reports, maintenance policy documents, infrastructure summits, and engineering standards are extensive. But actual maintenance follows crisis-driven logic divorced from the standards. The gap between prescribed (30-year bridge lifespan) and actual (maintenance deferred until failure) is substantial. Theater has increased over the interval as formal systems have elaborated while actual maintenance capacity has declined.
 *
 * PERSPECTIVAL GAP:
 *   The maximum perspectival gap exists between the institutional beneficiary (Rope, arbitrage options, net beneficiary) and the intergenerational victim (Snare, trapped, maximal extraction). Current municipal actors occupy the tangled middle — they coordinate services essential for the beneficiaries but are themselves constrained by political structures that prevent them from capturing the extraction value. The infrastructure inspection bureaucracy reveals the theater mechanism: extensive formal systems of assessment and planning that lack enforcement or funding power. The gap reflects the deep structural asymmetry: those making decisions about maintenance (political leaders, capital investors) are not those bearing costs (workers, future populations, infrastructure commons).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (chi) is derived from the agent's structural position via the directionality chain. Current beneficiaries with arbitrage options have low d values — they experience the constraint as enabling rather than extracting. Maintenance workers trapped with no exit have high d values — they bear maximum extraction. Municipalities constrained but with some agency occupy the middle. Future generations trapped by inheritance have the highest d — they experience extraction across temporal dimension. The derivation chain follows from beneficiary status (current consumers) vs victim status (maintenance systems, workers, future generations) combined with exit options. No directionality overrides are required because the structural relationships are clear: this is asymmetric extraction from constrained/trapped agents to benefit arbitrage-enabled beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint faces genuine mandatrophy risk. The tension between calling it a 'coordination problem' (Rope/Tangled Rope) vs calling it 'pure extraction' (Snare) is not perspectival disagreement but analytical choice. The same institutional structure provides genuine coordination (infrastructure could not exist through market mechanisms alone) AND genuine extraction (costs are systematically shifted from beneficiaries to workers and future generations). The resolution requires recognizing that 'coordination' and 'extraction' are not mutually exclusive: the same system can coordinate essential services while extracting value through cost deferral and asymmetric risk allocation. The mandatrophy is resolved by declaring the Tangled Rope classification as primary (coordination function + active enforcement + asymmetric extraction + multiple beneficiary/victim groups all present) while documenting the snare experience of trapped agents within the system. The Piton and Scaffold perspectives show the historical trajectory: formal systems (inspection, standards, procedures) have become increasingly theatrical as actual maintenance capacity declined, while alternative models (green infrastructure, distributed resilience) are building exit pathways. The constraint is not immutable or necessary — it is a contingent institutional choice with political economy causes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    optimal_maintenance_schedule,
    'What maintenance schedule and funding level would minimize total system lifecycle cost while maintaining service standards?',
    'Long-term infrastructure financial modeling; comparison of full lifecycle costs across maintenance schedules; empirical data on failure cascades vs preventive maintenance ratios',
    'If optimal schedule requires continuous 5-7% of system value annually: current 2-3% allocation is clearly extractive. If optimal is 2-3%: the constraint may be coordination failure rather than structural extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimal_maintenance_schedule, empirical, 'Optimal maintenance funding level for lifecycle cost minimization').

omega_variable(
    intergenerational_equity_threshold,
    'What degree of cost shifting from current to future generations constitutes unjust extraction across time?',
    'Intergenerational accounting frameworks; comparison to environmental economics literature on discounting; public surveys on intergenerational obligations',
    'If future generations bear >50% of lifecycle costs: systemic extraction across time. If <20%: distribution may be fair. Classification shifts based on this threshold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intergenerational_equity_threshold, preference, 'Threshold for intergenerational equity in infrastructure cost allocation').

omega_variable(
    privatization_externality_capture,
    'Do privatization models (PPP infrastructure, private toll systems) actually capture externalized maintenance costs or simply relocate extraction to private operators?',
    'Comparative analysis of public vs private infrastructure maintenance outcomes; cost-benefit studies of privatization cases; tracking of externalized costs in private models',
    'If privatization reduces net extraction: scaffold logic holds, alternative models work. If extraction simply transfers to private operators: the alternative models replicate the snare structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(privatization_externality_capture, empirical, 'Whether privatization models reduce or relocate maintenance extraction').

omega_variable(
    political_feasibility_of_maintenance_taxation,
    'Is the maintenance debt best understood as a failure of political will (extractive beneficiaries blocking taxation) or structural insufficiency of public mechanisms?',
    'Historical analysis of maintenance funding campaigns; cross-national comparison of taxation levels and maintenance outcomes; political economy analysis of opposition coalitions',
    'If political will: the constraint is snare-shaped and addressable through democratic pressure. If structural: the constraint is mountain-shaped and requires institutional redesign.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(political_feasibility_of_maintenance_taxation, conceptual, 'Whether maintenance debt is political choice or structural constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(civilizational_maintenance_debt, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(civmaint_tr_t0, civilizational_maintenance_debt, theater_ratio, 0, 0.4).
narrative_ontology:measurement(civmaint_tr_t10, civilizational_maintenance_debt, theater_ratio, 10, 0.58).
narrative_ontology:measurement(civmaint_tr_t20, civilizational_maintenance_debt, theater_ratio, 20, 0.65).

% Extraction over time
narrative_ontology:measurement(civmaint_be_t0, civilizational_maintenance_debt, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(civmaint_be_t10, civilizational_maintenance_debt, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(civmaint_be_t20, civilizational_maintenance_debt, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(civilizational_maintenance_debt, global_infrastructure).
narrative_ontology:affects_constraint(civilizational_maintenance_debt, urban_sprawl_density_externality).
narrative_ontology:affects_constraint(civilizational_maintenance_debt, capital_flight_and_jurisdictional_arbitrage).
narrative_ontology:affects_constraint(civilizational_maintenance_debt, climate_infrastructure_misalignment).

% DUAL FORMULATION NOTE:
% The maintenance debt constraint decomposes into distinct downstream claims: (1) systemic underinvestment in maintenance as institutional choice (political economy), and (2) the thermodynamic reality that all infrastructure degrades (natural law). These should not be conflated. The political economy story has high extractiveness (0.58) and is actionable. The thermodynamic story risks serving as false justification for the political choice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(civilizational_maintenance_debt, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
