% ============================================================================
% CONSTRAINT STORY: maintenance_capacity_shortfall
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   human_readable: The Entropic Debt Trap
 *   domain: infrastructure/logistical/technological
 *
 * SUMMARY:
 *   The Entropic Debt Trap occurs when the operational or maintenance
 *   complexity of a system grows faster than the financial, human, or
 *   organizational resources allocated for its upkeep. Initially, the system
 *   functions as a pure coordination mechanism (Rope): unified control,
 *   economies of scale, and centralized planning enable efficient operation.
 *   But as complexity compounds — additional modules, legacy systems,
 *   integration layers, regulatory requirements — the maintenance burden
 *   grows non-linearly. Budgets, staffed workforces, and maintenance
 *   schedules fail to scale proportionally. The constraint morphs from
 *   coordination into extraction. Present operators benefit by deferring
 *   maintenance costs while capturing the performance gains from complex
 *   systems. Future maintainers inherit the debt as compound failures,
 *   emergency repairs, and system brittleness. The trap is that once
 *   complexity exceeds capacity, increasing complexity further (to extract
 *   more performance) makes the shortfall worse. Communities dependent on the
 *   infrastructure face both the failure risk and the eventual catastrophic
 *   costs of deferred renewal. The theater ratio measures how much
 *   maintenance activity becomes performative: status reports showing
 *   'systems are maintained' while defect backlogs grow, accounting schedules
 *   that underestimate true degradation, and political declarations of
 *   infrastructure adequacy despite known vulnerabilities.
 *
 * KEY AGENTS:
 *   - System Operators (Current Era): Primary beneficiary (institutional/arbitrage) — defer costs, capture efficiency gains, maintain control. d≈0.08
 *   - Future Maintenance Cohort: Primary victim (powerless/trapped) — inherit high-complexity, underfunded systems. d≈0.92
 *   - Dependent Communities: Secondary victim (powerless/trapped) — vulnerable to service failures, bear crisis costs. d≈0.94
 *   - Maintenance Organizations: Mixed (organized/constrained) — have agency but face budget ceilings; benefit from coordination but constrained by extraction. d≈0.68
 *   - Capital Preservation Ideology: Beneficiary (institutional/arbitrage) — maintains accounting fiction that deferred costs are capital preservation. d≈0.10
 *   - Renewal/Replacement Programs: Organized intervention (organized/constrained) — see the constraint as temporary; building capacity for sunset.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks misattributing entropic debt to thermodynamic law (false summit).
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
narrative_ontology:human_readable(maintenance_capacity_shortfall, "The Entropic Debt Trap").
narrative_ontology:topic_domain(maintenance_capacity_shortfall, "infrastructure/logistical/technological").

domain_priors:requires_active_enforcement(maintenance_capacity_shortfall).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(maintenance_capacity_shortfall, system_operators).
narrative_ontology:constraint_beneficiary(maintenance_capacity_shortfall, capital_preservationists).
narrative_ontology:constraint_victim(maintenance_capacity_shortfall, future_maintainers).
narrative_ontology:constraint_victim(maintenance_capacity_shortfall, dependent_communities).
narrative_ontology:constraint_victim(maintenance_capacity_shortfall, system_integrity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE MAINTENANCE COHORT (SNARE) — Inherits a system whose complexity has grown beyond the allocated maintenance budget. No exit option: they must either pay increasing costs or accept catastrophic failure. Bears full extraction burden. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEPENDENT COMMUNITIES (SNARE) — Directly vulnerable to system failures (power grid, water systems, transportation networks). Cannot exit reliance on the infrastructure. Face service degradation and crisis costs. d≈0.94, f(d)≈1.40, σ=0.8 → χ≈0.65.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: MAINTENANCE ORGANIZATIONS (TANGLED ROPE) — Constrained by budget ceilings and staffing limits. Benefit from system coordination (unified control) but face asymmetric extraction: forced to prioritize crisis firefighting over preventive work. d≈0.68, f(d)≈1.03, σ=1.0 → χ≈0.60.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: SYSTEM OPERATORS (CURRENT ERA) (ROPE) — Benefit from deferring maintenance costs. Experience the constraint as coordination: unified budget authority, centralized planning, economies of scale. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CAPITAL PRESERVATION IDEOLOGY (PITON) — Maintains the fiction that deferring maintenance preserves capital while the true cost (entropic debt) accumulates off-book. Theater ratio = 0.68: accounting theaters (depreciation schedules that underestimate true decay) and political theaters (declaring systems 'maintained' while complexity silently outpaces capacity). The ideology persists through institutional inertia despite growing divergence from physical reality. d≈0.10, f(d)≈-0.08, σ=1.0 → χ≈-0.05.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From a universal perspective, complexity degradation is an immutable thermodynamic law: entropy always increases, and preventing degradation requires continuous energy expenditure (maintenance work). No system can escape this. However, the structural data (ε=0.58, suppression=0.62) contradicts mountain classification — the 'entropic debt' is not inevitable physical law but a contingent choice to underfund maintenance relative to complexity growth. This is a false summit.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 7: RENEWAL/REPLACEMENT PROGRAMS (SCAFFOLD) — Organized interventions (system modernization, infrastructure renewal, technological upgrades) see the bottleneck as temporary: replacing legacy systems with simpler, lower-entropy designs reduces the maintenance burden. has_sunset_clause rationale: complete system replacement or modularization can break the complexity-capacity mismatch. But this requires upfront capital investment (often politically constrained) and medium-term operational disruption. d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.28.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maintenance_capacity_shortfall_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(maintenance_capacity_shortfall, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maintenance_capacity_shortfall, TypeOther, context(agent_power(organized), _, _, _)),
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
 *   Extractiveness (0.58): Moderate-high. The debt trap extracts from future maintainers and dependent communities to benefit current operators. But it is not maximum extraction (snare would be ≥0.66) because some extraction goes back into system performance — the present generation does gain real coordination and performance benefits, not just pure appropriation. The value reflects the asymmetry: current operators get coordination + extraction, future cohorts get only the extraction burden. Suppression (0.62): Moderate-high. Suppression manifests as: (1) budgeting practices that hide true maintenance costs, (2) accounting standards that underestimate depreciation, (3) political difficulty in raising taxes/rates for maintenance, (4) technical barriers to rapid system modernization, (5) organizational path dependency that makes changing legacy systems harder than perpetuating them. Theater ratio (0.68): Moderate-high. Maintenance activity becomes increasingly performative: status reports certify systems are maintained while defect backlogs grow; regulatory inspections pass systems that are degrading; asset management spreadsheets show depreciation schedules that underestimate true decay rates; political leaders declare infrastructure adequately funded while engineers know the shortfall. The theater has grown over the interval (0.42 → 0.68) as complexity outpaced capacity more visibly, forcing more performative activity to maintain the fiction of adequacy.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a stark divide between present operators and future victims. Current-era system operators see Rope: they are solving real coordination problems (unified control, economies of scale, operational efficiency). Their perspective is genuinely functional. Future maintenance cohorts see pure Snare: they inherit complexity they did not create, cannot exit reliance on the system, and face escalating costs. The dependent communities see Snare as well: they are vulnerable to failures caused by deferred maintenance but have no say in maintenance decisions. Maintenance organizations see Tangled Rope: they benefit from system coordination but are constrained by budget ceilings and forced into crisis firefighting rather than prevention. The capital preservation ideology sees Rope (or even Piton — performative): the accounting fiction that deferring maintenance preserves capital. The analytical observer risks seeing Mountain (thermodynamic inevitability) but the structural data reveals this as a false summit: the entropic debt is not inevitable physical law but a contingent institutional choice. Renewal programs see Scaffold: temporary constraint with a sunset path (system replacement, modularization, technological upgrade).
 *
 * DIRECTIONALITY LOGIC:
 *   System operators (current): Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; they see coordination. Future maintainers: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction; they inherit the debt with no exit. Dependent communities: Victim + trapped → d≈0.94, f(d)≈1.40. Nearly maximum extraction; vulnerability to failures they cannot prevent. Maintenance organizations: Victim (constrained exit) + some benefit from coordination → d≈0.68, f(d)≈1.03. Mixed; they are both coordinators (unified budget control) and victims (constrained budgets). Capital preservation ideology: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary; the ideology benefits those who defer costs. Renewal programs: Organized + constrained → d≈0.45, f(d)≈0.48. Moderate extraction; they see a path forward but face upfront capital and transition costs.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: The constraint is a genuine tangled_rope, not a false coordination mechanism misclassified as extraction, and not a false mountain. The evidence: (1) Beneficiaries exist (current operators, capital preservationists) who benefit from deferral and unified control. (2) Victims exist (future maintainers, dependent communities) who bear escalating costs. (3) Active enforcement exists: budget ceilings, deferred maintenance scheduling, organizational path dependency, and political difficulty in raising maintenance funding all actively enforce the cost deferral. (4) The constraint exhibits measurable extraction: the cost multiplier for deferred work, the reduction in system reliability, and the concentration of benefits in present while costs accrue to future. The mandatrophy is resolved by recognizing that this is not a pure coordination problem (rope) where all parties benefit, nor is it a Mountain where the constraint is inevitable law. It is a hybrid: real coordination value is captured by present operators, but the asymmetric extraction to future generations is the dominant structural feature. The theater ratio (0.68) confirms: much maintenance activity is performative, masking the true accumulation of entropic debt.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complexity_measurement_threshold,
    'What metrics quantify ''complexity outpacing capacity''? Is it operational cost growth, mean time between failures, staff overtime, or deferred maintenance backlogs?',
    'Longitudinal data on maintenance cost trends, failure rates, and actual vs budgeted maintenance work for major infrastructure systems (power grids, water systems, transport networks)',
    'If complexity growth rate > budget growth rate: tangled_rope/snare. If they track together: system is at equilibrium (rope). If budget grows faster: system may reverse entropic debt.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(complexity_measurement_threshold, empirical, 'Which metrics define the complexity-capacity mismatch').

omega_variable(
    deferral_discount_rate,
    'What is the true cost multiplier for deferred maintenance? Does deferring $1 of work today cost $1.50, $2.00, or $5.00 to fix later?',
    'Comparative analysis of maintenance costs in deferred vs preventive programs; longitudinal case studies (aging bridges, legacy IT systems, power plant fleet)',
    'If multiplier < 1.5: deferral is rational economic policy (system is rope/scaffold). If multiplier > 2.5: extraction via deferral is severe (system is snare/tangled_rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deferral_discount_rate, empirical, 'The cost multiplier for deferred maintenance').

omega_variable(
    modularization_feasibility,
    'Can legacy systems be meaningfully refactored into lower-entropy, lower-maintenance architectures, or is full replacement the only option?',
    'Engineering analysis of major infrastructure systems; cost-benefit comparison of modularization vs replacement for power grids, water distribution, telecommunications',
    'If feasible: scaffold sunset is real (replacement programs can work). If infeasible: entropic debt is effectively permanent (snare perspective confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modularization_feasibility, empirical, 'Whether systems can be meaningfully refactored for lower entropy').

omega_variable(
    generation_incentive_structure,
    'Does the political/corporate incentive structure reward deferring costs to future administrations/shareholders, or aligns decision-maker incentives with long-term system health?',
    'Institutional analysis of budget cycles, executive compensation structures, political term limits, and their correlation with maintenance deferral patterns',
    'If future costs are discounted/externalized: extraction is structural (snare). If they''re internalized: constraint is coordination problem (rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(generation_incentive_structure, conceptual, 'Whether institutions defer costs to future generations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(maintenance_capacity_shortfall, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mcs_tr_t0, maintenance_capacity_shortfall, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mcs_tr_t10, maintenance_capacity_shortfall, theater_ratio, 10, 0.55).
narrative_ontology:measurement(mcs_tr_t20, maintenance_capacity_shortfall, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(mcs_be_t0, maintenance_capacity_shortfall, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(mcs_be_t10, maintenance_capacity_shortfall, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(mcs_be_t20, maintenance_capacity_shortfall, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(maintenance_capacity_shortfall, resource_allocation).
narrative_ontology:affects_constraint(maintenance_capacity_shortfall, critical_infrastructure_brittleness).
narrative_ontology:affects_constraint(maintenance_capacity_shortfall, deferred_renewal_cascades).
narrative_ontology:affects_constraint(maintenance_capacity_shortfall, fiscal_capacity_constraint).

% DUAL FORMULATION NOTE:
% The maintenance_capacity_shortfall is a hybrid constraint linking thermodynamic inevitability (entropy always increases) with institutional choices (how much to fund maintenance relative to complexity growth). The upstream constraint is the physical growth of system complexity; the downstream constraints are critical infrastructure brittleness (failure cascade risk) and deferred renewal programs (organizational responses to accumulating debt). This constraint family shares the core insight that present institutions optimizing for current-era benefits impose costs on future maintainers.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(maintenance_capacity_shortfall, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
