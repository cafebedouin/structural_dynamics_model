% ============================================================================
% CONSTRAINT STORY: infrastructure_resilience_degradation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_infrastructure_resilience_degradation, []).

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
 *   constraint_id: infrastructure_resilience_degradation
 *   human_readable: Infrastructure Resilience Degradation Through Deferred Maintenance and Cost Shifting
 *   domain: infrastructure/political_economy
 *
 * SUMMARY:
 *   Infrastructure resilience degradation operates as a structural extraction
 *   mechanism where present budget-holders and short-term contractors benefit
 *   from deferred maintenance while costs accumulate for future populations
 *   and maintenance workforces. The constraint exhibits coordinated
 *   extraction: deferring maintenance appears as a solution to immediate
 *   fiscal crises (coordination function) while systematically shifting costs
 *   to powerless agents who cannot exit the degrading systems. Over the
 *   20-year interval, extractiveness increases from 0.35 to 0.58 as deferred
 *   maintenance compounds, creating nonlinear cost acceleration. Theater
 *   increases from 0.52 to 0.68 as formal inspection and compliance regimes
 *   persist despite reduced functional maintenance capacity. The constraint
 *   is neither pure extraction (snare) nor pure coordination (rope) but a
 *   hybrid: genuine infrastructure coordination functions alongside
 *   systematic extraction of maintenance costs from future and vulnerable
 *   populations.
 *
 * KEY AGENTS:
 *   - Infrastructure-Dependent Communities: Primary victim (powerless/trapped) — geographically and economically unable to exit degrading systems; concentrated in rural and low-income areas
 *   - Future Populations: Primary victim (moderate/constrained) — inherit degraded systems they did not choose; face increased failure rates and replacement costs
 *   - Maintenance Workforces: Victim (moderate/constrained) — face rising injury rates, deskilling requirements, and wage pressure as systems age
 *   - Municipal Budget Holders: Primary beneficiary (institutional/arbitrage) — defer maintenance costs to balance immediate budgets; shift liability to successor administrations
 *   - Private Contractors (Short-Term): Beneficiary (institutional/arbitrage) — profit from crisis-driven emergency repairs at premium rates; arbitrage across jurisdictions
 *   - Civil Society and Infrastructure Advocacy: Organized victims (organized/constrained) — recognize extraction but constrained by fiscal orthodoxy and competing pressures
 *   - Analytical Observer: Neutral perspective (analytical/analytical) — risks naturalizing political choices as physical inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(infrastructure_resilience_degradation, 0.58).
domain_priors:suppression_score(infrastructure_resilience_degradation, 0.65).
domain_priors:theater_ratio(infrastructure_resilience_degradation, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(infrastructure_resilience_degradation, extractiveness, 0.58).
narrative_ontology:constraint_metric(infrastructure_resilience_degradation, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(infrastructure_resilience_degradation, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(infrastructure_resilience_degradation, tangled_rope).
narrative_ontology:human_readable(infrastructure_resilience_degradation, "Infrastructure Resilience Degradation Through Deferred Maintenance and Cost Shifting").
narrative_ontology:topic_domain(infrastructure_resilience_degradation, "infrastructure/political_economy").

domain_priors:requires_active_enforcement(infrastructure_resilience_degradation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(infrastructure_resilience_degradation, current_budget_holders).
narrative_ontology:constraint_beneficiary(infrastructure_resilience_degradation, private_contractors_short_term).
narrative_ontology:constraint_victim(infrastructure_resilience_degradation, future_populations).
narrative_ontology:constraint_victim(infrastructure_resilience_degradation, infrastructure_dependent_populations).
narrative_ontology:constraint_victim(infrastructure_resilience_degradation, maintenance_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INFRASTRUCTURE-DEPENDENT COMMUNITIES (SNARE) — Powerless populations in regions with degrading infrastructure (rural areas, low-income urban neighborhoods) face compounding service failures with no exit option. Cannot relocate without massive cost; no alternative water, electricity, or transportation systems available. Trapped by geography and economic dependency. Maximum extraction: bear costs of degradation while institutional actors defer maintenance to balance budgets.
constraint_indexing:constraint_classification(infrastructure_resilience_degradation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FUTURE POPULATIONS AND MAINTENANCE WORKFORCES (TANGLED ROPE) — Constrained by having inherited degraded systems they cannot easily replace. Benefit from some coordination function (the deferred system still provides basic services) but bear asymmetric extraction through reduced reliability, increased failure rates, and dangerous working conditions. Workforces face occupational injury rates that rise with infrastructure age while budgets compress their capacity to upgrade.
constraint_indexing:constraint_classification(infrastructure_resilience_degradation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BUDGET-CONSTRAINED MUNICIPAL GOVERNMENTS (ROPE) — Experience the constraint as coordination: deferring maintenance solves immediate budgetary crises while maintaining basic system function. Trade short-term fiscal pressure against long-term risk. Net beneficiary in immediate time horizon through budget relief, though they are creating liabilities for successor administrations. Arbitrage option: shift costs to future budgets.
constraint_indexing:constraint_classification(infrastructure_resilience_degradation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: PRIVATE INFRASTRUCTURE CONTRACTORS (SHORT-TERM) (ROPE) — Benefit from repair crises driven by deferred maintenance. See the constraint as coordination opportunity: degradation creates emergency contracts at premium rates. High extraction but presented as service provision. Arbitrage option: move to next jurisdiction or next crisis cycle.
constraint_indexing:constraint_classification(infrastructure_resilience_degradation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 5: CIVIL SOCIETY AND INFRASTRUCTURE ADVOCACY (TANGLED ROPE) — Organized agents (unions, infrastructure commissions, engineering societies) recognize the coordination function (infrastructure does provide services) while documenting systematic extraction (deferred maintenance concentrates costs on powerless populations and future generations). Can partially organize resistance but constrained by fiscal orthodoxy and competing budgetary pressures. Active enforcement: credentialing standards, safety regulations, professional liability that advocates use to fight degradation.
constraint_indexing:constraint_classification(infrastructure_resilience_degradation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: INFRASTRUCTURE MAINTENANCE SYSTEM (PITON) — The formal maintenance regime (inspection schedules, replacement timelines, capital budgeting) persists as largely performative. Agencies conduct inspections, file reports, and document compliance with outdated standards while actual maintenance capacity has degraded below system needs. The theater (documentation, compliance narratives, professional credentialing) remains high while functional maintenance has atrophied. Maintained through institutional inertia — the ritual persists because alternatives haven't fully replaced it and because abandoning maintenance fiction would trigger legal and political crises.
constraint_indexing:constraint_classification(infrastructure_resilience_degradation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / PHYSICAL CONSTRAINT VIEW (MOUNTAIN) — From a civilizational/universal perspective, infrastructure degradation follows physical laws: materials age, systems fail, maintenance costs accumulate. This perspective sees the constraint as natural and inevitable — decay is the natural state. However, the structural data contradicts mountain classification. The analytical observer must ask: if degradation is inevitable, why do wealthy jurisdictions maintain excellent infrastructure while poor jurisdictions degrade? The mountain framing naturalizes a political choice as a law of physics.
constraint_indexing:constraint_classification(infrastructure_resilience_degradation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(infrastructure_resilience_degradation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(infrastructure_resilience_degradation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(infrastructure_resilience_degradation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(infrastructure_resilience_degradation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(infrastructure_resilience_degradation, TR),
    TR >= 0.70.

:- end_tests(infrastructure_resilience_degradation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over interval. The constraint exhibits genuine extraction: present budget-holders capture fiscal relief while shifting replacement costs to future periods. The extraction is not maximal because infrastructure does continue to provide basic services during the deferral window — the coordination function is real. However, the accumulation of deferred costs and nonlinear cascade failures create accelerating extraction. Theater ratio (0.68): Moderate-high. The maintenance regime persists with formal inspection schedules, compliance documentation, and professional credentialing despite reduced functional maintenance. The ritual persists because abandoning it would trigger political and legal crises. Suppression (0.65): Moderate-high. Trapped populations face geographic and economic barriers to exit. Constrained populations (future generations, maintenance workers) have limited ability to demand system replacement before inherited assets fail catastrophically. Budget holders have arbitrage options (move to next jurisdiction, different budget cycle) reducing their experienced suppression.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates radical perspectival disagreement grounded in time horizon and exit options. Municipal budget holders see rope (coordination of fiscal crisis) because their time horizon is immediate and their exit options are available. Trapped populations see snare (pure extraction) because they bear costs without compensation or escape. Future populations see tangled rope (genuine infrastructure coordination mixed with extraction of replacement costs they will inherit). The organized advocates see tangled rope with clearer extraction visibility because they have information that isolated populations lack. The piton perspective reveals that the formal maintenance system persists ritualistically despite functional degradation. The mountain perspective risks naturalizing the constraint by framing infrastructure decay as inevitable physical law rather than political choice. The perspectival gap is most acute between beneficiaries (institutional/immediate) and victims (powerless/biographical or moderate/generational).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by agent's structural position relative to the extraction flow. Budget holders as beneficiaries experience low d (arbitrage exit available) — they can move past the crisis by deferring costs. Private contractors as beneficiaries experience moderate d (arbitrage across jurisdictions) — dependent on crisis cycles but mobile. Trapped communities experience maximum d (no exit, full cost bearing) — powerless agents with trapped exit options. Maintenance workforces experience high d (constrained exit, specialized skills become liability in degraded systems) — they absorb cascading demands as systems age. Future populations experience high d (constrained exit by inheritance) — they inherit degraded systems they cannot easily replace. Organized advocates experience moderate d (constrained exit through fiscal orthodoxy) — they can mobilize resistance but face powerful counterarguments about budget scarcity.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that infrastructure degradation is NOT a pure extraction (snare) disguised as coordination (rope). It is genuinely hybrid: the infrastructure does provide coordination services (water, power, transportation systems function) while the deferral mechanism extracts costs from powerless agents. The constraint is tangled rope precisely because both the coordination function and the extraction mechanism are real and irreducible. Removing the coordination function (stopping all infrastructure service) would eliminate extraction but also destroy genuine public goods. Removing the extraction mechanism would require either (a) preventing deferral through binding future budgets, or (b) internalizing lifetime costs in present period, which creates different political pressures. The mandatrophy is resolved by recognizing that the constraint cannot be decomposed into pure extraction OR pure coordination — the deferral mechanism is only possible because genuine coordination is being provisioned, and the extraction is only effective because future agents cannot opt out of inheriting the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capital_budget_vs_maintenance_tradeoff,
    'Is infrastructure degradation driven by genuine scarcity (insufficient total resources) or by budget allocation choices (prioritizing other spending over maintenance)?',
    'Comparative analysis of total government revenues vs maintenance needs; historical data on budget allocation decisions; documentation of deferred projects and their funding gaps vs political priorities',
    'If driven by scarcity: constraint is more mountain-like (unavoidable). If driven by allocation choices: constraint is more snare-like (extraction mechanism with clear beneficiaries).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_budget_vs_maintenance_tradeoff, empirical, 'Capital budget allocation vs maintenance necessity').

omega_variable(
    private_contractor_rent_seeking,
    'Do crisis-driven infrastructure repairs by private contractors represent genuine service provision or rent extraction enabled by degradation incentives?',
    'Cost analysis of emergency repairs vs planned maintenance; profit margin comparison across crisis vs non-crisis contracts; bidding pattern analysis for infrastructure maintenance vs emergency response',
    'If genuine service: constraint is coordination failure (Rope). If rent extraction: constraint is extraction mechanism (Snare or Tangled Rope with stronger extractive component).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(private_contractor_rent_seeking, empirical, 'Whether private contractors profit from degradation-driven crises').

omega_variable(
    intergenerational_liability_visibility,
    'Are future replacement costs and compounding degradation expenses recognized in present budgeting frameworks, or are they rendered invisible through accounting conventions?',
    'Examination of asset accounting practices; comparison of stated infrastructure replacement reserve vs actual future replacement need; documentation of budget frameworks that allow deferral to be recorded as cost reduction',
    'If visible: future populations can politically demand present action. If invisible: the extraction mechanism is enhanced by informational asymmetry — powerless present generations inherit costs they couldn''t anticipate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_liability_visibility, empirical, 'Whether future infrastructure costs are visible in present budgeting').

omega_variable(
    system_coupling_and_cascade_risk,
    'What degree of interdependency exists between infrastructure systems (water, power, transportation) such that degradation in one accelerates failures in others?',
    'Network analysis of infrastructure coupling; documentation of cascade failure events; modeling of failure propagation across system boundaries',
    'If highly coupled: degradation is nonlinear and accelerates (suppression increases over time). If decoupled: degradation is linear and manageable (suppression is stable).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(system_coupling_and_cascade_risk, empirical, 'Degree of interdependency between degrading infrastructure systems').

omega_variable(
    maintenance_workforce_substitutability,
    'Can degraded infrastructure be maintained by deskilled workers and automation, or does increasing degradation require specialized expertise, creating a bottleneck?',
    'Skills analysis of workforce required for degraded vs good-condition systems; wage and injury rate trends in maintenance occupations; documentation of expertise availability as systems age',
    'If substitutable: maintenance costs remain stable and extraction is stable. If expertise-dependent: maintenance costs rise and become concentrated, increasing extraction on workforces.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(maintenance_workforce_substitutability, empirical, 'Whether degraded infrastructure maintenance requires specialized expertise').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(infrastructure_resilience_degradation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infra_tr_t0, infrastructure_resilience_degradation, theater_ratio, 0, 0.52).
narrative_ontology:measurement(infra_tr_t10, infrastructure_resilience_degradation, theater_ratio, 10, 0.62).
narrative_ontology:measurement(infra_tr_t20, infrastructure_resilience_degradation, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(infra_be_t0, infrastructure_resilience_degradation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(infra_be_t10, infrastructure_resilience_degradation, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(infra_be_t20, infrastructure_resilience_degradation, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(infrastructure_resilience_degradation, global_infrastructure).
narrative_ontology:affects_constraint(infrastructure_resilience_degradation, disaster_recovery_capacity_degradation).
narrative_ontology:affects_constraint(infrastructure_resilience_degradation, public_health_infrastructure_collapse).
narrative_ontology:affects_constraint(infrastructure_resilience_degradation, transportation_safety_divergence).

% DUAL FORMULATION NOTE:
% Infrastructure resilience degradation is the overarching structural constraint linking specific system-level failures (water, power, transportation, public health). Each specific system has its own extractiveness value and constraint story. The resilience degradation story models the meta-constraint: the institutional mechanisms that allow deferral across all systems simultaneously. Downstream constraints are specific-system manifestations; this story captures the shared extraction mechanism enabling degradation across domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(infrastructure_resilience_degradation, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
