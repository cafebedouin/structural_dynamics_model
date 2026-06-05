% ============================================================================
% CONSTRAINT STORY: legacy_system_technical_debt
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legacy_system_technical_debt, []).

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
 *   constraint_id: legacy_system_technical_debt
 *   human_readable: Cumulative Technical Debt in Legacy Monoliths
 *   domain: technological/economic
 *
 * SUMMARY:
 *   Technical debt in legacy monoliths creates a structural extraction
 *   mechanism that combines genuine coordination benefits (rapid feature
 *   delivery) with escalating costs borne asymmetrically by development teams
 *   and system reliability. The constraint operates across multiple
 *   institutional layers: product management benefits from short-term
 *   velocity through technical shortcuts; development teams bear compounding
 *   refactoring burden; the organization as a whole experiences mounting
 *   reliability risk and talent attrition; platform engineering teams
 *   recognize the problem and advocate solutions. The theater ratio (0.68)
 *   reflects that technical debt management has become substantially
 *   performative: quarterly refactoring initiatives that don't reduce debt,
 *   architecture reviews that identify problems without enforcing solutions,
 *   and documented 'tech debt initiatives' that consume effort without
 *   structural change. The extractiveness trajectory (0.32 → 0.58 over 10
 *   years) shows the characteristic pattern of technical debt: low early
 *   costs as shortcuts accumulate invisibly, then nonlinear cost acceleration
 *   as refactoring becomes increasingly expensive. The suppression (0.65)
 *   comes from organizational switching costs (sunk investment, knowledge
 *   silos, release cycle coupling), developer exit barriers (career
 *   disruption, team instability), and collective action problems (individual
 *   teams cannot unilaterally refactor without coordination).
 *
 * KEY AGENTS:
 *   - Development Teams: Primary victim (powerless/trapped) — assigned to maintain and extend legacy monolith; face escalating refactoring burden with each release; cannot exit without career disruption
 *   - Product Management / Leadership: Primary beneficiary (institutional/arbitrage) — benefits from rapid feature delivery enabled by technical shortcuts; makes short-term cost-benefit decisions that defer expenses
 *   - Organization (Aggregate): Secondary victim (moderate/constrained) — experiences mounting technical incident risk, talent attrition, and ecosystem lock-in; switching costs prevent escape; benefits during delivery phases
 *   - Platform Engineering / DevOps Coalition: Organized actor (organized/constrained) — recognizes debt as architectural problem; advocates for microservices, containerization, event-driven redesign; can resist and propose alternatives
 *   - Legacy System Institution: Structural beneficiary (institutional/arbitrage) — monolith persists through organizational inertia and sunk-cost justification; maintenance overhead has become performative
 *   - Future Development Capacity: Abstract victim (powerless/trapped) — abstract collective inability to innovate quickly on new platforms; cannot organize or exit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legacy_system_technical_debt, 0.58).
domain_priors:suppression_score(legacy_system_technical_debt, 0.65).
domain_priors:theater_ratio(legacy_system_technical_debt, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legacy_system_technical_debt, extractiveness, 0.58).
narrative_ontology:constraint_metric(legacy_system_technical_debt, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(legacy_system_technical_debt, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legacy_system_technical_debt, tangled_rope).
narrative_ontology:human_readable(legacy_system_technical_debt, "Cumulative Technical Debt in Legacy Monoliths").
narrative_ontology:topic_domain(legacy_system_technical_debt, "technological/economic").

domain_priors:requires_active_enforcement(legacy_system_technical_debt).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legacy_system_technical_debt, product_delivery_teams).
narrative_ontology:constraint_beneficiary(legacy_system_technical_debt, legacy_system_maintainers).
narrative_ontology:constraint_victim(legacy_system_technical_debt, future_development_capacity).
narrative_ontology:constraint_victim(legacy_system_technical_debt, system_reliability).
narrative_ontology:constraint_victim(legacy_system_technical_debt, development_teams).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEVELOPMENT TEAM (SNARE) — Assigned to maintain legacy monolith; cannot exit without career disruption; bears compounding refactoring burden with each release. Each new feature entrenches debt further. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.81.
constraint_indexing:constraint_classification(legacy_system_technical_debt, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZATION (TANGLED ROPE) — Benefits from rapid delivery of features via shortcuts (coordination function); simultaneously bears mounting refactoring costs, technical incident risk, and knowledge silos (asymmetric extraction). Constrained by switching costs and sunk investment. d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.61.
constraint_indexing:constraint_classification(legacy_system_technical_debt, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: PRODUCT MANAGEMENT / LEADERSHIP (ROPE) — Benefits from rapid feature delivery enabled by technical shortcuts. Experiences constraint as coordination mechanism: 'move fast, iterate, refactor later' is a shared narrative that justifies near-term delivery. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.07. Net beneficiary during decision-making window.
constraint_indexing:constraint_classification(legacy_system_technical_debt, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PLATFORM ENGINEERING / DEVOPS COALITION (ORGANIZED TANGLED ROPE) — Organized actors (SRE teams, platform-as-a-service providers, open-source maintainers) recognize technical debt as a coordination failure with solutions (microservices, containerization, IaC). They see both the extraction (maintenance burden pushed to operations) and a path forward (incremental migration). d≈0.45, f(d)≈0.45, σ=1.0 → χ≈0.27. Lower extraction because organized agents can push back and advocate alternatives.
constraint_indexing:constraint_classification(legacy_system_technical_debt, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY SYSTEM INSTITUTIONAL PERSISTENCE (PITON) — The monolith persists through organizational inertia and sunk-cost narratives despite clear dysfunction. theater_ratio=0.68 indicates substantial performative maintenance: status meetings about 'tech debt initiatives' that produce no structural change, quarterly 'refactoring sprints' that don't reduce debt, documented 'architecture principles' that nobody enforces. The primary function (serving customers) has atrophied relative to the maintenance overhead.
constraint_indexing:constraint_classification(legacy_system_technical_debt, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: MIGRATION / SUNSET STRATEGY (SCAFFOLD) — Organized migration to microservices, event-driven architecture, or API-first redesign represents a temporary high-overhead phase (χ≈0.25) with an explicit sunset: once migration completes, the legacy constraints are decoupled from new development. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.25. Mobile exit options and explicit migration timelines (5-7 year targets) characterize this perspective.
constraint_indexing:constraint_classification(legacy_system_technical_debt, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legacy_system_technical_debt_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legacy_system_technical_debt, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legacy_system_technical_debt, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legacy_system_technical_debt, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(legacy_system_technical_debt, TR),
    TR >= 0.70.

:- end_tests(legacy_system_technical_debt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high, reflecting asymmetric costs. Product management extracts short-term velocity benefits; development teams bear medium-term refactoring costs. The extraction is not maximal (0.70+) because organizations can and do invest in modernization, and the short-term benefits are real (not purely predatory). The trajectory shows the key technical debt dynamic: initial shortcuts appear low-cost (ε=0.32 at t=0) because costs are future costs; by t=10 the true extraction burden becomes visible (ε=0.58). Suppression (0.65): Moderately high, reflecting organizational switching costs, knowledge silos, and sunk investment. Development teams cannot easily leave the legacy system without career disruption (biographical horizon constraints). Product teams cannot easily commit to full rewrites without revenue impact (immediate horizon constraints). The organization cannot easily fork or parallelize development due to coupling and resource constraints. These are real structural barriers, not regulatory coercion, but they function as suppression. Theater ratio (0.68): High, indicating substantial performative maintenance. 'Tech debt sprints' that reduce debt by 5-10% then reaccumulate. 'Architecture review boards' that document problems without enforcing solutions. Quarterly metrics tracking 'lines of technical debt' that show no correlation with actual refactoring effort. Status meetings about 'paying down debt' that don't change velocity. This theater has grown as the debt has worsened — organizations respond to visible dysfunction by creating oversight mechanisms rather than structural change.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiary and victim is stark and temporal. Product management sees the constraint as a coordination mechanism (Rope) — the shared narrative 'move fast, iterate, refactor later' makes sense for rapid market response. Development teams see the same constraint as a snare (Snare) — the refactoring never happens at the promised pace, and they absorb the compounding cost. The organization sees a tangled hybrid (Tangled Rope) — benefits during growth phases, costs during stability phases. Platform engineering sees a solvable problem with a sunset (Scaffold) — microservices and event-driven architecture offer a real path to decoupling, though it requires 5-7 year migration and temporary overhead. The legacy system itself exhibits piton characteristics (Piton) — it persists through inertia and performative management despite clear dysfunction. The perspectival gap reflects the temporal asymmetry: benefits are immediate (feature velocity), costs are deferred (refactoring burden). This temporal misalignment is the core extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Development teams: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction. They cannot exit and bear full compounding burden. Product management: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; they can choose alternative strategies (migrate, rewrite, accept slower pace) but benefit most from the status quo. Organization: Both beneficiary (delivery speed) and victim (incident risk, talent loss) + constrained → d≈0.68, f(d)≈1.05. Moderate-to-high extraction; organization is locked in by switching costs but also participates in the decision to accumulate debt. Platform engineering: Organized + constrained → d≈0.45, f(d)≈0.45. Low-moderate extraction; organized agents have agency and can propose alternatives, reducing the effective extraction they experience. Legacy system (Piton): Institutional + arbitrage → d≈0.08, f(d)≈-0.11. Piton classification comes from theater gate (ratio ≥0.70), not from directionality. The system benefits from continued operation without refactoring.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE CONFIRMATION: The constraint satisfies all three gates. (1) Beneficiaries: product_delivery_teams and legacy_system_maintainers genuinely benefit from the ability to deploy features quickly via shortcuts; the coordination function is real. (2) Victims: future_development_capacity, system_reliability, and development_teams bear escalating costs; extraction is asymmetric. (3) Enforcement: requires_active_enforcement=true because the constraint is maintained by organizational decisions and cultural narratives ('move fast'), not by natural law or pure coordination equilibrium. Without active decision-making and enforcement of shortcuts (skipping tests, deferring refactoring, accepting technical compromises), the debt would not accumulate — teams would refactor as they go. The mandatrophy is resolved by showing that Tangled Rope correctly captures the hybrid: genuine coordination gains (velocity) coupled with asymmetric extraction (future cost deferral). The alternative misclassifications fail: (a) Pure Rope (no extraction) underestimates the compounding burden on development teams and system reliability. (b) Pure Snare (no coordination) ignores that the short-term velocity gains are real and valuable. (c) Mountain (natural law) falsely naturalizes what is a contingent organizational choice. The Tangled Rope classification correctly identifies the constraint as a real coordination mechanism that has acquired an asymmetric extraction layer through temporal misalignment.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_linearity_threshold,
    'Is technical debt a linear cost accumulation or does it exhibit threshold / tipping-point dynamics where small increments suddenly cause major system fragility?',
    'Longitudinal analysis of refactoring time budgets vs system complexity metrics; correlation between cyclomatic complexity and bug-introduction rates; incident post-mortems identifying ''debt-related'' failures',
    'If linear: development pace can be modulated predictably, and debt is a solvable optimization problem (Rope from more perspectives). If nonlinear: debt exhibits sudden catastrophic phase transitions (Snare from organizational perspective), making early intervention critical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_linearity_threshold, empirical, 'Whether technical debt accumulates linearly or exhibits nonlinear tipping-point dynamics').

omega_variable(
    explicit_vs_implicit_debt,
    'Is the primary extraction coming from explicitly documented debt (known shortcuts with known costs) or from implicit, hidden debt (architectural assumptions, deprecated dependencies, tribal knowledge)?',
    'Audit of technical debt backlog vs actual refactoring effort; gap analysis between declared and discovered debt; tracking of time spent on undocumented vs documented refactoring tasks',
    'If explicit: organizational agents can rationally debate debt trade-offs (Rope/Tangled Rope perspective remains valid). If mostly implicit: debt functions as a hidden extraction mechanism where costs materialize unpredictably (Snare characteristics dominate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(explicit_vs_implicit_debt, empirical, 'Whether debt is explicitly tracked or hidden in system complexity').

omega_variable(
    migration_feasibility_boundary,
    'Is incremental refactoring actually capable of resolving the monolith constraint, or does the system topology make only a complete rewrite feasible?',
    'Technical analysis of coupling patterns, dependency graphs, and modularization barriers; comparison of refactoring ROI across different strategies; historical case studies of successful vs failed incremental modernizations',
    'If incremental works: Scaffold sunset is real and achievable (medium-term resolution). If only rewrite works: Organization faces a high-stakes all-or-nothing decision, and the Snare extraction accelerates (teams are trapped longer).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(migration_feasibility_boundary, empirical, 'Whether legacy systems can be incrementally refactored or require complete rewrite').

omega_variable(
    knowledge_silo_entrenchment,
    'Is the legacy system''s technical debt coupled to undocumented knowledge silos (specific engineers who ''understand'' the system) that increase switching costs?',
    'Measurement of knowledge distribution via code ownership concentration, documentation completeness, and turnover analysis; tracking of onboarding time for new team members; correlation between engineer departure and system instability',
    'If highly silo''d: extraction mechanism is strengthened because specific engineers become irreplaceable (increases suppression and victim lock-in). If knowledge is distributed: technical debt is the primary extraction, and knowledge transfer reduces it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(knowledge_silo_entrenchment, empirical, 'Whether technical debt is entrenched by knowledge silos').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legacy_system_technical_debt, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(debt_tr_t0, legacy_system_technical_debt, theater_ratio, 0, 0.35).
narrative_ontology:measurement(debt_tr_t5, legacy_system_technical_debt, theater_ratio, 5, 0.52).
narrative_ontology:measurement(debt_tr_t10, legacy_system_technical_debt, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(debt_be_t0, legacy_system_technical_debt, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(debt_be_t5, legacy_system_technical_debt, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(debt_be_t10, legacy_system_technical_debt, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legacy_system_technical_debt, resource_allocation).
narrative_ontology:affects_constraint(legacy_system_technical_debt, system_modernization_debt_ceiling).
narrative_ontology:affects_constraint(legacy_system_technical_debt, knowledge_silo_retention_coupling).

% DUAL FORMULATION NOTE:
% Technical debt in legacy monoliths is a single structural constraint with stable ε=0.58. It decomposes not into multiple constraints (ε-invariance principle is satisfied) but into multiple perspectives because different organizational actors observe and participate in the constraint differently. The perspectives are not alternative measurements but genuine indexical readings: the development team trapped in the monolith, the product manager extracting velocity, the organization experiencing lock-in, the platform engineer seeing a migration path. All perspectives use the same ε and suppression values but derive different classifications through (P,T,E,S) tuples and beneficiary/victim declarations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legacy_system_technical_debt, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
