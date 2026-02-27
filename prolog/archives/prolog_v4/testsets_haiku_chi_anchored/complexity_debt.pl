% ============================================================================
% CONSTRAINT STORY: complexity_debt
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_complexity_debt, []).

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
 *   constraint_id: complexity_debt
 *   human_readable: The Cumulative Fragility Surcharge
 *   domain: technological/organizational
 *
 * SUMMARY:
 *   Complexity debt accumulates when organizations prioritize short-term
 *   delivery velocity over architectural coherence. Each quick fix —
 *   workaround routing layer, specialized data adapter, undocumented API
 *   fork, conditional flag for edge case — solves an immediate coordination
 *   problem but defers its structural cost to future maintenance. Over time,
 *   the system becomes a palimpsest of accumulated patches, where modifying
 *   any component risks cascade failures through poorly documented coupling.
 *   The constraint exhibits both genuine coordination benefits (quick fixes
 *   enable rapid feature delivery) and asymmetric extraction (maintenance
 *   engineers and long-term reliability bear the accrued cost). The theater
 *   ratio (0.68) reflects that technical debt reviews are largely
 *   performative — teams discuss the problem in retrospectives without budget
 *   or authority to address it systematically. The extractiveness trajectory
 *   (0.22 → 0.52) shows debt compounding over the measurement interval, as
 *   early fixes create conditions for later fixes to compound nonlinearly.
 *
 * KEY AGENTS:
 *   - Short-Term Delivery Teams: Primary beneficiary (institutional/arbitrage) — captures velocity metrics and on-time delivery during their tenure; escapes when assigned to new projects
 *   - Executive Leadership: Primary beneficiary (organized/arbitrage) — benefits from higher quarterly velocity and defers costs beyond their decision horizon; typically rotates before debt becomes unmanageable
 *   - Maintenance Engineers: Primary victim (powerless/trapped) — locked into system by institutional knowledge of undocumented layers and hidden coupling; cannot exit without career consequences
 *   - Future Development Velocity: Primary victim (moderate/constrained) — abstract collective good bearing extraction cost; future teams face exponentially higher costs to add features
 *   - Mid-Level Architecture Advocates: Secondary actor (moderate/constrained) — aware of debt accumulation but constrained by delivery pressure; partial coordination benefit mixed with extraction cost of unheeded warnings
 *   - Technical Debt Ritual: Institutional degradation (institutional/arbitrage) — sprint retrospectives and code review processes discussing debt but lacking enforcement power or refactoring budget
 *   - Analytical Observer: System-level perspective (analytical/analytical) — reveals constraint as genuine tangled rope, not inevitable law; structural data supports both coordination and extraction components
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(complexity_debt, 0.52).
domain_priors:suppression_score(complexity_debt, 0.65).
domain_priors:theater_ratio(complexity_debt, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(complexity_debt, extractiveness, 0.52).
narrative_ontology:constraint_metric(complexity_debt, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(complexity_debt, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(complexity_debt, tangled_rope).
narrative_ontology:human_readable(complexity_debt, "The Cumulative Fragility Surcharge").
narrative_ontology:topic_domain(complexity_debt, "technological/organizational").

domain_priors:requires_active_enforcement(complexity_debt).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(complexity_debt, short_term_delivery_teams).
narrative_ontology:constraint_beneficiary(complexity_debt, executive_leadership).
narrative_ontology:constraint_victim(complexity_debt, long_term_system_reliability).
narrative_ontology:constraint_victim(complexity_debt, maintenance_engineers).
narrative_ontology:constraint_victim(complexity_debt, future_development_velocity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MAINTENANCE ENGINEER (SNARE) — Trapped by dependency on undocumented layers, legacy interfaces, and hidden coupling. Each attempted fix risks cascading failures. Cannot exit without career consequences (institutional knowledge lock-in). d≈0.93, f(d)≈1.38, σ=0.9 → χ≈0.68.
constraint_indexing:constraint_classification(complexity_debt, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FUTURE DEVELOPMENT VELOCITY (SNARE) — Constrained by debt accumulation. New features must navigate legacy constraints. Velocity degrades exponentially as abstractions degrade. Abstract collective good (development capacity) bears extraction cost. d≈0.87, f(d)≈1.25, σ=1.2 → χ≈0.68.
constraint_indexing:constraint_classification(complexity_debt, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: SHORT-TERM DELIVERY TEAM (ROPE) — Experiences constraint as enabling coordination: quick fixes solve immediate problems and maintain team cohesion under deadline pressure. Benefits from deferring architectural decisions. d≈0.08, f(d)≈-0.10, σ=0.9 → χ≈-0.05. Negative effective extraction = net beneficiary during their tenure.
constraint_indexing:constraint_classification(complexity_debt, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: EXECUTIVE LEADERSHIP (ROPE) — Organized institutional actor. Experiences constraint as coordination mechanism: debt accumulation defers costs beyond their decision horizon and enables higher quarterly velocity metrics. Exit via rotation to new role. d≈0.10, f(d)≈-0.08, σ=0.9 → χ≈-0.04. Negative effective extraction during their tenure.
constraint_indexing:constraint_classification(complexity_debt, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: MID-LEVEL ARCHITECTURE ADVOCATE (TANGLED ROPE) — Constrained between pressure to deliver and awareness of debt accumulation. Partial coordination benefit (advocating good design principles) mixed with extraction cost (time spent fighting for refactoring budgets that never arrive). d≈0.58, f(d)≈0.72, σ=0.9 → χ≈0.35.
constraint_indexing:constraint_classification(complexity_debt, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: TECHNICAL DEBT RITUAL (PITON) — Degraded coordination mechanism. Sprint retrospectives and quarterly architecture reviews discuss debt but lack enforcement power or budget authority. Ritual persists through institutional inertia (appears to address the problem) despite minimal functional impact on trajectory. theater_ratio=0.68 satisfies piton gate (≥0.70, marginal). The technical debt ritual is performed because alternatives haven't replaced it, not because it functions.
constraint_indexing:constraint_classification(complexity_debt, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scale, complexity debt is a coordination mechanism (layers enable rapid feature development) mixed with asymmetric extraction (costs distributed to future maintenance and long-term reliability). The constraint is neither inevitable nor pure extraction. Structural data (ε=0.52, suppression=0.65) confirms tangled rope — both coordination and extraction are real.
constraint_indexing:constraint_classification(complexity_debt, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(complexity_debt_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(complexity_debt, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(complexity_debt, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(complexity_debt, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(complexity_debt, TR),
    TR >= 0.70.

:- end_tests(complexity_debt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from future maintenance and long-term reliability. Early measurements (0.22) show low extraction when the system is young and layers are few. Compounding trajectory (→0.52) reflects exponential debt accumulation — each new quick fix increases the difficulty of future modifications. The value (0.52) indicates extraction is real but not total; some systems recover via scheduled refactoring or rewrites. Suppression (0.65): Moderate-high. Significant barriers to escaping debt include: institutional knowledge lock-in (only certain engineers understand undocumented layers), career risk of advocating for refactoring over features, organizational rotation that hides long-term velocity loss from decision-makers, and missing metrics (velocity decline is often attributed to scope increase rather than technical debt). But suppression is not total — some organizations measure and budget refactoring explicitly. Theater ratio (0.68): Moderate-high. Technical debt reviews, architecture meetings, and code review comments discuss the problem extensively but produce minimal institutional change. The ritual is maintained because alternatives (ignoring debt entirely, or making refactoring a first-class budget item) haven't fully replaced it. The theater has increased over the interval as visible debt accumulation has forced more performative acknowledgment without corresponding action.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the classic divergence between short-term and long-term perspectives. The delivery team and executive leadership see a coordination mechanism (Rope) — they are genuinely solving the problem of rapid feature iteration under deadline pressure. Their exit via project rotation allows them to experience the constraint as beneficial. Maintenance engineers see pure extraction (Snare) — the system's complexity grows exponentially, making their work harder without corresponding benefits to them. The future development velocity sees extraction with some coordination (Tangled Rope from the analytical view) — some of the quick fixes do enable legitimate coordination, but the compound effect is degraded velocity for future teams. The technical debt ritual sees its own degradation (Piton) — the performance of addressing debt without actual refactoring. The perspectival gaps are not observational differences but structural differences in who benefits and who bears costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Short-term delivery team: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; can exit when assigned to new team. Executive leadership: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.08. Net beneficiary; rotates before long-term costs materialize. Maintenance engineers: Victim + trapped → d≈0.93, f(d)≈1.38. Maximum extraction; locked in by institutional knowledge. Future development velocity: Victim + constrained → d≈0.87, f(d)≈1.25. High extraction; abstract collective cannot organize and cannot exit. Mid-level architect: Both + constrained → d≈0.58, f(d)≈0.72. Partial beneficiary (advocacy role valued in some orgs) but constrained (advocacy often unheeded). Technical debt ritual: Institutional + arbitrage → d≈0.10, f(d)≈-0.08. Piton classification from theater gate, not directionality. Analytical observer: Measured position between beneficiaries and victims → d≈0.52, f(d)≈0.65. Tangled rope confirmed.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE VALIDATION: The constraint satisfies all three tangled rope gates: (1) Base extraction ε=0.52 (≥0.30 required), (2) Beneficiaries (short-term teams, executives) + Victims (maintenance engineers, future velocity) declared, (3) Requires active enforcement: true. The constraint is NOT pure extraction because genuine coordination benefits exist — quick fixes DO enable rapid feature delivery and solve immediate problems. It is NOT pure coordination because asymmetric extraction IS real — costs are borne disproportionately by future teams and maintenance engineers. The mandatrophy is resolved by recognizing that both components are structural features: the constraint would NOT function as a rent-extraction mechanism if it didn't provide real short-term coordination benefit. The extraction is enabled BY the coordination function. This is the defining characteristic of tangled rope: the hybrid is not a measurement ambiguity but a genuine structural property. No alternative observable would change the classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_debt_measurement,
    'What observable should distinguish ''legitimate architectural layering'' from ''accumulated fragility''?',
    'Empirical correlation between code churn patterns, bug escape rates, and layer count; comparison of systems with explicit refactoring budgets vs those without',
    'If layer count < 8: mostly coordination (Rope). If layer count > 15: mostly extraction (Snare). If 8-15 with refactoring budget: Scaffold. If 8-15 without: Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_debt_measurement, empirical, 'Observable threshold distinguishing layering from fragility').

omega_variable(
    refactoring_velocity_hypothesis,
    'Can teams actually refactor complexity debt within a sprint cycle, or is debt always pyramidally cumulative?',
    'Longitudinal study of velocity metrics, refactoring spend, and bug rates across teams with vs without protected refactoring time; measurement of whether debt-reduction effort pays compound returns',
    'If teams can refactor effectively: constraint is Scaffold with working sunset clause. If pyramidal accumulation is inevitable: constraint is Snare (no exit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(refactoring_velocity_hypothesis, empirical, 'Whether refactoring provides functional debt reduction or mere theatrical relief').

omega_variable(
    organizational_rotation_extraction,
    'To what extent is the constraint''s extraction mechanism dependent on high managerial turnover that hides long-term velocity loss?',
    'Comparison of velocity decline curves for organizations with high executive tenure vs high rotation; analysis of whether long-tenured architects perceive higher extraction than rotating managers',
    'If rotation-dependent: constraint is Snare hidden by institutional churn. If independent of tenure: constraint is Tangled Rope (real coordination + real extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(organizational_rotation_extraction, empirical, 'Extent to which constraint depends on organizational turnover for suppression of visibility').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(complexity_debt, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cplx_tr_t0, complexity_debt, theater_ratio, 0, 0.35).
narrative_ontology:measurement(cplx_tr_t3, complexity_debt, theater_ratio, 3, 0.52).
narrative_ontology:measurement(cplx_tr_t6, complexity_debt, theater_ratio, 6, 0.68).

% Extraction over time
narrative_ontology:measurement(cplx_be_t0, complexity_debt, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(cplx_be_t3, complexity_debt, base_extractiveness, 3, 0.38).
narrative_ontology:measurement(cplx_be_t6, complexity_debt, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(complexity_debt, resource_allocation).
narrative_ontology:affects_constraint(complexity_debt, velocity_cliff).
narrative_ontology:affects_constraint(complexity_debt, knowledge_concentration).
narrative_ontology:affects_constraint(complexity_debt, refactoring_budget_scarcity).

% DUAL FORMULATION NOTE:
% Complexity debt is downstream of resource allocation decisions (refactoring budget constraints) and organizational rotation patterns (tenure-dependent visibility of costs). These upstream constraints create the conditions for debt accumulation. The downstream constraints (velocity cliff, knowledge concentration) are structurally dependent on complexity debt as a prerequisite.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(complexity_debt, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
