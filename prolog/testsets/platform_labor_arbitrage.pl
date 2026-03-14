% ============================================================================
% CONSTRAINT STORY: platform_labor_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_labor_arbitrage, []).

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
 *   constraint_id: platform_labor_arbitrage
 *   human_readable: Platform Labor Arbitrage: Structural Extraction Through Algorithmic Intermediation
 *   domain: economic/labor_markets/digital_platforms
 *
 * SUMMARY:
 *   Platform labor arbitrage represents a structural constraint in which
 *   digital intermediaries extract surplus from workers by capturing the
 *   informational asymmetry between supply and demand, encoding suppression
 *   mechanisms into algorithmic control systems, and maintaining
 *   classification barriers that prevent worker access to labor protections.
 *   The constraint operates through the platform's monopoly on matching
 *   information and allocation decisions. Unlike traditional employment where
 *   wages reflect bilateral negotiation and market clearing, platform wages
 *   reflect unilateral algorithmic determination without transparency,
 *   collective voice, or exit options that preserve income. The mechanism is
 *   not primarily coercive (platforms do not prevent workers from leaving)
 *   but suppressive: workers remain dependent because alternatives have been
 *   structurally degraded or made inaccessible. The extractiveness has
 *   increased monotonically over the measurement interval as platforms have
 *   grown, normalized, and entrenched their control mechanisms. Theater ratio
 *   remains moderate because extraction is direct (wage setting, task
 *   allocation) rather than performative (theatrical compliance rituals).
 *
 * KEY AGENTS:
 *   - Gig Workers: Primary victims (powerless/trapped) — structurally dependent on platform income; face algorithmic task allocation control; cannot negotiate terms; deactivation is permanent and opaque
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — control all matching information; set unilateral terms; extract surplus through pricing asymmetry; enormous optionality to expand or relocate
 *   - End Consumers: Secondary beneficiaries (powerful/mobile) — receive genuine matching coordination benefit; unaware of suppression costs; high switching optionality across platforms
 *   - Multi-Platform Workers: Moderate agents (moderate/constrained) — some agency through multi-platform arbitrage but constrained by switching costs and reputation dependencies
 *   - Labor Regulators: Institutional degradation actors (organized/constrained) — maintain performative labor protections that platforms circumvent through classification (independent contractor); trapped by regulatory capture and political economy
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees simultaneous genuine coordination function (matching) and extraction mechanism (wage suppression and algorithmic control)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_labor_arbitrage, 0.58).
domain_priors:suppression_score(platform_labor_arbitrage, 0.65).
domain_priors:theater_ratio(platform_labor_arbitrage, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_labor_arbitrage, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_labor_arbitrage, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(platform_labor_arbitrage, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_labor_arbitrage, snare).
narrative_ontology:human_readable(platform_labor_arbitrage, "Platform Labor Arbitrage: Structural Extraction Through Algorithmic Intermediation").
narrative_ontology:topic_domain(platform_labor_arbitrage, "economic/labor_markets/digital_platforms").

domain_priors:requires_active_enforcement(platform_labor_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_labor_arbitrage, platform_operators).
narrative_ontology:constraint_beneficiary(platform_labor_arbitrage, end_consumers).
narrative_ontology:constraint_victim(platform_labor_arbitrage, platform_workers).
narrative_ontology:constraint_victim(platform_labor_arbitrage, labor_market_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GIG WORKER (SNARE) — Structural entrapment through income dependency, algorithmic control of task allocation, and lack of alternative employment pathways with comparable flexibility. Worker cannot exit without catastrophic income loss. Platform controls all terms: pricing, work rules, deactivation risk. Maximal extraction experienced.
constraint_indexing:constraint_classification(platform_labor_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PLATFORM-DEPENDENT LABOR COHORT (SNARE) — At generational timescale, workers born into precarious labor markets face normalized platform dependence. Structural exclusion from traditional employment pathways (permanent contracts, benefits, collective bargaining) makes platforms the only accessible option. Suppression mechanism includes regulatory capture preventing labor protections. Intergenerational extraction.
constraint_indexing:constraint_classification(platform_labor_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: MULTI-PLATFORM WORKER (TANGLED ROPE) — Some worker agency through multi-platform arbitrage (ride-share + delivery + task work). Constrained exit costs (account switching, reputation loss, learning curve). Genuine coordination function: platforms match supply and demand with efficiency unavailable to centralized employment. Mixed extraction and benefit — more agency than single-platform worker but still subject to algorithmic wage suppression.
constraint_indexing:constraint_classification(platform_labor_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: PLATFORM OPERATOR (ROPE) — Experiences the constraint as pure coordination: matching workers to customers, logistics optimization, market expansion. Enormous arbitrage optionality (geographic expansion, service diversification, cross-subsidization). Net beneficiary experiencing the system as coordination mechanism. Low perceived extraction because operator extracts value without bearing the suppression costs.
constraint_indexing:constraint_classification(platform_labor_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: END CONSUMER (ROPE) — High mobility (can switch platforms or use alternatives). Receives genuine coordination benefit: on-demand services, price transparency, convenience. Experiences platform as efficiency mechanism. Unaware of suppression costs borne by workers. Derives net benefit from arbitrage.
constraint_indexing:constraint_classification(platform_labor_arbitrage, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: LABOR REGULATION SYSTEM (PITON) — Traditional labor protections (minimum wage, worker classification, collective bargaining rights) are performatively maintained but systematically circumvented through platform-designed worker classification (independent contractor status). Theater ratio high: regulatory agencies conduct hearings and issue pronouncements while platforms restructure work to evade regulatory categories. Institutional inertia prevents updating labor law to platform realities. Exit from this degradation is constrained by regulatory capture and political economy.
constraint_indexing:constraint_classification(platform_labor_arbitrage, piton,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Platform labor simultaneously coordinates supply-demand matching (genuine function) and extracts surplus through wage suppression, algorithmic control, and barrier elevation (asymmetric power). Civilization-scale analysis reveals this as a hybrid mechanism: some platform value is coordination rent (legitimate), some is extraction rent (behavioral suppression of wages below reservation level). Classification as Tangled Rope reflects both components operating simultaneously. Theater moderately low because the extraction is largely direct (wage setting, task allocation) rather than performative.
constraint_indexing:constraint_classification(platform_labor_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_labor_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_labor_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_labor_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_labor_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_labor_arbitrage, TR),
    TR >= 0.70.

:- end_tests(platform_labor_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Platform operators systematically suppress wages below market-clearing levels through algorithmic control, information monopoly, and barrier elevation. The suppression is not maximal (workers retain some income, some choice of when to work) but is structural and durable. The value reflects that a significant portion of potential worker surplus is captured by the platform through mechanisms that would not exist in competitive labor markets. The increase over time (0.32 → 0.58) reflects platform consolidation and normalization — as platforms mature, their market power increases and wage suppression intensifies. Suppression (0.65): High. Structural barriers include: income dependency (workers cannot leave without catastrophic loss), algorithmic opacity (workers cannot predict how to improve allocation), classification barriers (workers excluded from labor protections), and regulatory capture (labor law has not updated to platform models). These barriers are substantial but not total — some workers maintain minimal savings, some multi-platform arbitrage occurs, and some regulators pursue enforcement. Theater ratio (0.48): Moderate. The extraction mechanism is relatively direct: platform sets prices, allocates tasks, controls communication with customers. Less theater than traditional labor regulation (where compliance rituals dominate) but some performative element exists: platforms present algorithmic allocation as neutral/fair/merit-based when allocation actually reflects unilateral platform interest. Claimed type (Snare): Justified by high extractiveness, high suppression, and unilateral control structure. Workers lack meaningful exit options and the constraint depends on suppressing alternatives (traditional employment, collective bargaining, regulatory protection).
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between platform operator and worker is the core diagnostic signature of this constraint. The operator sees coordination (Rope) — algorithmic matching is more efficient than traditional labor intermediaries. This is true. The worker sees extraction (Snare) — wages are suppressed unilaterally and exit is blocked. This is also true. The disagreement is not empirical; it is structural. They occupy different positions in the extraction flow. The operator's perspective is internally consistent and factually accurate about the coordination component. The worker's perspective is internally consistent and factually accurate about the suppression component. Neither is wrong; they are measuring different aspects of the same constraint. The Tangled Rope classification from the analytical perspective resolves this by asserting both are simultaneously valid — the constraint genuinely provides coordination value AND genuinely suppresses wages asymmetrically. The perspectival gap persists because there is no single correct view; there is only a structural reality with multiple legitimate readings.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's classification is derived from four inputs: power atom, time horizon, exit options, and spatial scope. These combine with the beneficiary/victim declarations to produce directionality (d), which the sigmoid f(d) converts to an effective extraction multiplier. For the platform operator: institutional power + immediate horizon + arbitrage exit + global scope produces d ≈ 0.05 (beneficiary with maximum exit optionality). The sigmoid f(d ≈ 0.05) = -0.12, yielding χ ≈ 0.58 × (-0.12) × 1.2 ≈ -0.08 (negative extraction, experienced as net benefit). Classification: Rope. For the gig worker: powerless + biographical horizon + trapped exit + global scope produces d ≈ 0.92 (victim with zero exit optionality). The sigmoid f(d ≈ 0.92) = 1.28, yielding χ ≈ 0.58 × 1.28 × 1.2 ≈ 0.89 (high experienced extraction). Classification: Snare. For the multi-platform worker: moderate + biographical + constrained exit + regional scope produces d ≈ 0.68 (victim with partial agency). Sigmoid f(d ≈ 0.68) ≈ 1.02, yielding χ ≈ 0.58 × 1.02 × 0.9 ≈ 0.53 (moderate experienced extraction). Classification: Tangled Rope (coordination component from multi-platform optionality; extraction component from constrained switching). The analytical observer with analytical power + civilizational horizon + analytical exit + global scope produces d ≈ 0.73 (observer position). Sigmoid f(d ≈ 0.73) ≈ 1.15, yielding χ ≈ 0.58 × 1.15 × 1.2 ≈ 0.80 (high observed extraction). But the classification is Tangled Rope (not Snare) because the observer position recognizes the dual function: coordination is real, extraction is real, both are significant. The chi value reflects the extraction component; the classification reflects the full structural reality.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy in platform labor arbitrage is the temptation to classify the constraint as pure Rope based on the genuine coordination function (matching workers to tasks with unprecedented efficiency) while ignoring the asymmetric extraction mechanism (wage suppression through unilateral algorithmic control). The resolution requires recognizing that both components are simultaneously true: the platform genuinely solves the matching problem better than alternatives (Rope function), AND the platform structurally extracts surplus through its monopoly on matching information (Snare mechanism). The constraint is Tangled Rope because it provides genuine coordination benefit (workers would have worse outcomes without platform matching) while simultaneously suppressing wages below what competitive markets would produce. This dual structure is the essence of Tangled Rope: the constraint cannot be understood as pure coordination (it would misclassify the extraction mechanism) nor as pure extraction (it would ignore the real coordination value). The mandatrophy is resolved by insisting that both perspectives are correct and that the constraint provides insufficient separation between them to justify collapsing either component. The analytical classification is Tangled Rope; subordinate perspectives emphasizing one component over the other (Rope for the beneficiary, Snare for the victim) are correct about their local perspective but incomplete about the global structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_clearing_wage_ambiguity,
    'What proportion of the wage suppression is market-clearing (supply exceeds demand at reservation wage) vs platform-enforced (platform suppresses wages below market-clearing equilibrium)?',
    'Comparative analysis with traditional labor markets; measurement of wages in high-demand platforms vs oversupplied platforms; historical wage data during platform growth phases',
    'High market-clearing component: snare classification weakens toward tangled_rope (workers bear cost of own oversupply). High platform-enforcement component: snare classification strengthened (asymmetric power structure). Extraction bounds: ε could range 0.35-0.72 depending on resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_clearing_wage_ambiguity, empirical, 'Market clearing vs platform-enforced wage suppression').

omega_variable(
    platform_switching_cost_breakdown,
    'What percentage of worker exit costs are technological/account-based vs reputational/social vs opportunity (earnings loss during transition)?',
    'Worker interviews on switching experience; measurement of account transfer feasibility; reputation portability analysis; earnings data during multi-platform transitions',
    'If primarily reputational/opportunity: workers are identity_locked (reputation fused with platform identity) rather than structurally trapped — suppression is internalized. If primarily technological: workers are trapped (external barriers). Classification of exit_options shifts the directionality and potentially the constraint type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_switching_cost_breakdown, empirical, 'Composition of worker platform exit costs').

omega_variable(
    alternative_employment_viability,
    'What percentage of gig platform workers could transition to traditional employment at comparable or superior income if barriers (credential gaps, geography, discrimination) were removed?',
    'Cohort analysis of workers transitioning to traditional employment; skill-match studies; wage comparison studies of same workers across employment modes',
    'High viability (>60%): suppression is primarily market-clearing; workers are voluntarily choosing gig work despite extraction. Low viability (<30%): suppression is structural exclusion; workers are trapped by lack of alternatives. Classification orientation shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_employment_viability, empirical, 'Viability of alternative employment for gig workers').

omega_variable(
    algorithmic_control_transparency,
    'Can workers, on average, predict which actions and performance metrics will affect their algorithmic ranking and task allocation? Is the control mechanism transparent or opaque?',
    'Worker surveys on algorithmic predictability; comparison of worker mental models with documented platform algorithms; analysis of platform transparency disclosures vs actual algorithmic behavior',
    'High opacity: workers cannot adapt; suppression is maximized. High transparency: workers can game the system; suppression is reduced and some agency emerges. Affects perception of ''trapped'' vs ''constrained'' exit options.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_control_transparency, empirical, 'Algorithmic control transparency to workers').

omega_variable(
    collective_organization_feasibility,
    'What are the structural barriers to platform workers organizing collectively for wage negotiation? Are barriers primarily legal (classification status), technical (algorithmic visibility limits), or social (identity fragmentation)?',
    'Analysis of unionization attempts across platforms; legal barriers assessment; technical feasibility of worker coordination mechanisms; psychological studies on worker collective identity',
    'If legal barriers dominant: regulatory reform could enable collective power. If technical/social barriers dominant: organization remains infeasible regardless of legal status. Affects whether workers can transition from ''powerless'' to ''organized'' power atom under different horizon timescales.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(collective_organization_feasibility, empirical, 'Structural barriers to collective worker organization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_labor_arbitrage, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pla_tr_t0, platform_labor_arbitrage, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pla_tr_t3, platform_labor_arbitrage, theater_ratio, 3, 0.4).
narrative_ontology:measurement(pla_tr_t6, platform_labor_arbitrage, theater_ratio, 6, 0.45).
narrative_ontology:measurement(pla_tr_t9, platform_labor_arbitrage, theater_ratio, 9, 0.48).

% Extraction over time
narrative_ontology:measurement(pla_be_t0, platform_labor_arbitrage, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(pla_be_t3, platform_labor_arbitrage, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(pla_be_t6, platform_labor_arbitrage, base_extractiveness, 6, 0.54).
narrative_ontology:measurement(pla_be_t9, platform_labor_arbitrage, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_labor_arbitrage, resource_allocation).
narrative_ontology:affects_constraint(platform_labor_arbitrage, precarious_employment_normalization).
narrative_ontology:affects_constraint(platform_labor_arbitrage, labor_law_classification_crisis).

% DUAL FORMULATION NOTE:
% Platform labor arbitrage decomposes into two structurally distinct constraints: (1) The matching coordination function (ε ≈ 0.15, pure Rope) which platforms genuinely improve over traditional labor intermediaries. (2) The wage suppression mechanism (ε ≈ 0.72, pure Snare) which captures surplus through algorithmic control and information monopoly. These two constraints are unified in this story because they are operationally inseparable — the platform's monopoly on matching information is the mechanism that enables wage suppression. The Tangled Rope classification reflects their entanglement. Upstream constraints include labor market precarity and regulatory capture; downstream constraints include worker collective action and labor law reform efforts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_labor_arbitrage, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
