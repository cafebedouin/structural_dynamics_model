% ============================================================================
% CONSTRAINT STORY: algorithmic_management_opacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_management_opacity, []).

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
 *   constraint_id: algorithmic_management_opacity
 *   human_readable: Algorithmic Management Opacity in Platform Labor
 *   domain: labor/platform_economics/technology
 *
 * SUMMARY:
 *   Algorithmic management opacity in platform labor creates a structural
 *   constraint where the technical mechanisms of job allocation, worker
 *   rating, and performance monitoring are hidden from workers while
 *   remaining visible (and manipulable) to the platform. This constraint
 *   exhibits multiple classification types from different perspectives
 *   because it performs genuine coordination (matching workers to jobs,
 *   managing quality) while simultaneously enabling extraction through
 *   information asymmetry. The opacity itself is the enforcement mechanism:
 *   without visibility into algorithmic decision-making, workers cannot
 *   organize collectively, appeal unfair treatment, or even diagnose whether
 *   they are being systematically discriminated against. The constraint has
 *   intensified over time as platforms have grown in labor market centrality
 *   and as the efficiency justifications for opacity have degraded — early
 *   claims that transparency would destroy matching quality have not been
 *   supported by evidence, yet opacity persists through institutional inertia
 *   and extractive entrenchment.
 *
 * KEY AGENTS:
 *   - Gig Workers: Primary victim (powerless/trapped) — economically dependent, no exit option, bear full cost of algorithmic opacity through deactivation risk, rating manipulation, invisible penalties
 *   - Platform Operators: Primary beneficiary (institutional/arbitrage) — control algorithmic systems, capture efficiency gains, maintain arbitrage options (expansion, adjacent markets)
 *   - Precarious Gig Worker Coalition: Secondary victim (moderate/constrained) — organized but threatened by retaliation; experience mixed coordination benefit and asymmetric extraction
 *   - Labor Rights Regulators: Organized agents (organized/mobile) — building transparency mandates and audit frameworks; see algorithmic opacity as temporary governance failure
 *   - Platform Efficiency Doctrine: Institutional narrative (institutional/arbitrage) — maintains opacity as efficiency necessity despite degraded empirical support
 *   - Analytical Observer: Civilian civilization view (analytical/analytical) — recognizes both legitimate coordination function and hidden extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_management_opacity, 0.58).
domain_priors:suppression_score(algorithmic_management_opacity, 0.68).
domain_priors:theater_ratio(algorithmic_management_opacity, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_management_opacity, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_management_opacity, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(algorithmic_management_opacity, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_management_opacity, tangled_rope).
narrative_ontology:human_readable(algorithmic_management_opacity, "Algorithmic Management Opacity in Platform Labor").
narrative_ontology:topic_domain(algorithmic_management_opacity, "labor/platform_economics/technology").

domain_priors:requires_active_enforcement(algorithmic_management_opacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_management_opacity, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_management_opacity, algorithmic_efficiency_gains).
narrative_ontology:constraint_victim(algorithmic_management_opacity, gig_workers).
narrative_ontology:constraint_victim(algorithmic_management_opacity, labor_transparency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GIG WORKER (SNARE) — Trapped by economic dependency; no viable income alternatives. Cannot exit the platform without severe financial consequences. Experiences algorithmic decisions (deactivation, job allocation, rating manipulation) as opaque and irreversible. Maximum suppression: no appeal mechanism, no visibility into decision criteria, no bargaining power. Pure extraction — the opacity itself is the mechanism that prevents worker organization and collective resistance.
constraint_indexing:constraint_classification(algorithmic_management_opacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRECARIOUS GIG WORKER COALITION (TANGLED ROPE) — Organized but constrained by the threat of algorithmic retaliation (deactivation). Benefits from platform access and income opportunities but bears asymmetric extraction through unfair rating systems, unilateral contract changes, and invisible algorithmic penalties. Genuine coordination function: the platform does allocate jobs and manage supply-demand matching. But the coordination is captured: opacity enables the platform to extract rents beyond legitimate coordination costs.
constraint_indexing:constraint_classification(algorithmic_management_opacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Experiences opacity as a coordination mechanism. Algorithmic management solves real problems: dynamic pricing, real-time job allocation, fraud detection, quality control. The platform has arbitrage options (expanding to new markets, developing adjacent services) and benefits from the system. From the platform's perspective, the constraint is legitimate coordination with minimal overhead. The opacity is framed as proprietary necessity.
constraint_indexing:constraint_classification(algorithmic_management_opacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR RIGHTS REGULATOR (SCAFFOLD) — Organized agents (labor boards, regulatory agencies) see algorithmic opacity as a temporary failure of labor governance with a sunset clause. Algorithmic transparency mandates (EU AI Act, emerging gig worker protections) are building regulatory frameworks that will force disclosure. The constraint is being addressed by sunset mechanisms: mandatory algorithmic audits, explainability requirements, and due-process rights. Low effective extraction because regulators can exit through legislation.
constraint_indexing:constraint_classification(algorithmic_management_opacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: PLATFORM EFFICIENCY DOCTRINE (PITON) — The narrative that algorithmic opacity is necessary for efficiency has become largely performative. The actual efficiency gains are modest (5-15% reduction in matching latency); much of the opacity serves extraction rather than optimization. The doctrine persists through institutional inertia: platforms maintain opacity claims even when transparency would not materially degrade performance. Theater ratio reflects that the efficiency justification has degraded into pure legitimation rather than functional requirement.
constraint_indexing:constraint_classification(algorithmic_management_opacity, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, algorithmic management opacity coordinates labor supply and demand at unprecedented scale but simultaneously enables asymmetric extraction through information asymmetry. Genuine coordination function coexists with genuine extraction. The constraint is neither pure coordination nor pure oppression — it is a hybrid that requires both recognition of legitimate platform functions AND exposure of extractive mechanisms hidden behind opacity.
constraint_indexing:constraint_classification(algorithmic_management_opacity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_management_opacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_management_opacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_management_opacity, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_management_opacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_management_opacity, TR),
    TR >= 0.70.

:- end_tests(algorithmic_management_opacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The platform captures substantial rents through opacity-enabled wage suppression, rating manipulation, and differential job allocation. The constraint is not maximal extraction (workers do retain some income, some agency in multi-platform work) but clearly exceeds legitimate coordination costs. The measurement trajectory shows extractiveness increasing from 0.42 to 0.58 over the interval — platforms have become more sophisticated in using opacity for rent extraction as the labor market has matured. Suppression (0.68): High. Significant barriers include: economic dependency (gig work as primary income), lack of algorithmic transparency (workers cannot diagnose unfair treatment), threat of deactivation (workers fear algorithmic retaliation for organizing or demanding disclosure), information asymmetry (platform knows worker behavior but worker does not know algorithmic criteria), and absence of due-process rights (no appeal mechanism for algorithmic decisions). These barriers prevent both individual exit and collective organization. Theater ratio (0.64): Moderate-high. The efficiency justifications for opacity have become largely performative. Early claims that transparency would destroy matching quality or enable gaming have not materialized in transparent platforms (Uber's surge pricing algorithms are transparent; matching quality has not degraded). Yet platforms maintain opacity claims as proprietary necessity. The theater has increased from 0.35 to 0.64 as the empirical basis for opacity has weakened while institutional commitment to opacity has strengthened.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the diagnostic power of perspectival divergence. The gig worker (powerless/trapped) sees pure extraction (snare); the platform (institutional/arbitrage) sees pure coordination (rope); the analytical observer (analytical/analytical) sees a mixture (tangled rope). This gap is not measurement error — it is structural signal. The snare classification is the worker's material reality: they cannot exit, they suffer algorithmic deactivation with no recourse, the system actively prevents their organizing. The rope classification is the platform's genuine function: it does solve matching problems that have no trivial alternative. Neither perspective is wrong; they are measuring different components of the same constraint. The gap reveals that opacity serves extraction precisely because it prevents the coordination function from being legitimate: if workers could see and challenge algorithmic decisions, the coordination would require genuine consent rather than coerced compliance.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform operators benefit from opacity: they extract rents through information asymmetry and prevent worker organization. They have arbitrage exit options (expanding to new markets, pivoting to adjacent services), so they experience low directionality d ≈ 0.15 from beneficiary status plus arbitrage exit, producing negative χ from their perspective. Gig workers are both victims and trapped: they bear extraction through wage suppression, rating manipulation, and deactivation threat. They lack viable income alternatives, so they experience maximum directionality d ≈ 0.95, producing high χ ≈ 0.75+ even before scope adjustment. The worker coalition (organized victims with constrained exit) experiences moderate d ≈ 0.65, producing moderate χ reflecting that they have some agency (can organize, can exit to other platforms at cost) but remain under strong suppression. Regulators (neither beneficiary nor victim, with mobile exit via legislation) experience neutral d ≈ 0.50, low χ because they have structural power to change the constraint. The directionality values correctly encode that the constraint works very differently for people in different structural positions — not because they perceive it differently, but because they actually experience different mechanisms and face different barriers.
 *
 * MANDATROPHY ANALYSIS:
 *   HYBRID RESOLUTION: This constraint genuinely performs both coordination and extraction. The mandatrophy dissolves when we recognize that opacity is the mechanism enabling the platform to extract beyond legitimate coordination costs. Transparency would not destroy the coordination function (evidence from semi-transparent platforms shows matching quality is stable) but would make the platform's extraction visible and contestable. The constraint is a tangled rope: (1) Genuine coordination function: matching workers to jobs, managing supply-demand, detecting fraud. (2) Asymmetric extraction: opacity enables wage suppression, rating manipulation, and algorithmic discrimination that could not persist under scrutiny. (3) Active enforcement: platform's technical systems actively maintain opacity and prevent alternative visibility mechanisms. The constraint can be separated through transparency: the coordination mechanism would remain (even be improved — workers could optimize their own labor supply if they saw algorithmic criteria) while the extraction mechanism would be constrained. Therefore the mandatrophy is resolved by recognizing that calling this constraint 'pure coordination' (the platform view) obscures extraction, while calling it 'pure oppression' (one worker view) obscures genuine labor matching function. It is a tangled rope because both are structurally real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    opacity_necessity_threshold,
    'What is the minimum algorithmic opacity required for legitimate platform efficiency vs. what serves only extraction?',
    'Controlled transparency experiments: A/B testing platforms with varying disclosure levels to measure actual efficiency degradation from transparency; comparative analysis of transparent vs opaque platforms in equivalent markets',
    'If minimal opacity suffices for efficiency: opacity serves primarily extraction, snare classification strengthens. If significant opacity is necessary: tangled_rope classification is correct, legitimacy of extraction claim increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opacity_necessity_threshold, empirical, 'Threshold separating necessary opacity from extractive opacity').

omega_variable(
    worker_exit_cost_reality,
    'Is gig worker exit genuinely trapped (no alternatives) or constrained (high-cost alternatives exist)?',
    'Labor market analysis of alternative income sources; survey of actual vs stated exit barriers; measurement of geographic and skill-based mobility for gig workers',
    'If trapped: snare classification correct, no individual exit option. If constrained: workers have mobile option in some circumstances, perspectival gap widens between desperate and multi-platform workers.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(worker_exit_cost_reality, empirical, 'Whether worker exit is trapped or constrained').

omega_variable(
    algorithmic_bias_systematic,
    'Is worker algorithmic discrimination systematic extraction or random algorithmic error?',
    'Statistical analysis of deactivation patterns, rating manipulation, and job allocation across protected characteristics (race, gender, national origin); comparison of error rates for worker-coded vs platform-coded actors',
    'If systematic: discrimination is intentional extraction mechanism, suppression strengthens, snare classification confirmed. If random: errors require different remediation (accuracy improvement vs. audit rights).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_bias_systematic, empirical, 'Whether algorithmic discrimination is systematic or random').

omega_variable(
    transparency_retaliation_mechanism,
    'Does transparency itself enable platform retaliation against workers who demand disclosure?',
    'Study of worker experiences post-transparency mandates; measurement of deactivation rates for workers who file algorithmic audits or complaints',
    'If retaliation occurs: transparency mechanisms are captured, scaffold sunset fails, constraint persists in modified form. If retaliation is prevented: scaffold perspective is structural reality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transparency_retaliation_mechanism, empirical, 'Whether transparency enables platform retaliation').

omega_variable(
    identity_lock_worker_subjectivity,
    'Do workers internalize algorithmic management as legitimate authority (identity lock) or resist it as external oppression?',
    'Qualitative analysis of worker narratives; measurement of internalized vs externalized attribution for deactivation; comparison of frame shifts in organizing vs non-organizing cohorts',
    'If identity locked: workers cannot perceive exit as thinkable from within platform framing, suppression operates internally. If resistance frame: workers maintain critical distance, exit becomes more imaginable, constraint is less total.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_worker_subjectivity, empirical, 'Whether workers are identity locked to platform authority').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_management_opacity, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(algo_mgmt_tr_t0, algorithmic_management_opacity, theater_ratio, 0, 0.35).
narrative_ontology:measurement(algo_mgmt_tr_t5, algorithmic_management_opacity, theater_ratio, 5, 0.52).
narrative_ontology:measurement(algo_mgmt_tr_t10, algorithmic_management_opacity, theater_ratio, 10, 0.64).
narrative_ontology:measurement(algo_mgmt_tr_t15, algorithmic_management_opacity, theater_ratio, 15, 0.68).

% Extraction over time
narrative_ontology:measurement(algo_mgmt_be_t0, algorithmic_management_opacity, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(algo_mgmt_be_t5, algorithmic_management_opacity, base_extractiveness, 5, 0.51).
narrative_ontology:measurement(algo_mgmt_be_t10, algorithmic_management_opacity, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(algo_mgmt_be_t15, algorithmic_management_opacity, base_extractiveness, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_management_opacity, resource_allocation).
narrative_ontology:affects_constraint(algorithmic_management_opacity, gig_worker_income_volatility).
narrative_ontology:affects_constraint(algorithmic_management_opacity, platform_labor_classification_status).
narrative_ontology:affects_constraint(algorithmic_management_opacity, algorithmic_bias_labor_discrimination).

% DUAL FORMULATION NOTE:
% Algorithmic management opacity is downstream of platform labor economics but represents a distinct structural constraint. The labor classification status constraint (gig vs employee) has its own extractiveness; the opacity constraint is the mechanism through which classification advantage is enforced. Income volatility is both a consequence and a mechanism: opacity prevents workers from understanding volatility patterns, which could enable collective bargaining on dynamic pricing.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
