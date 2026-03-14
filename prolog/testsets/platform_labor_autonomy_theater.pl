% ============================================================================
% CONSTRAINT STORY: platform_labor_autonomy_theater
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_labor_autonomy_theater, []).

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
 *   constraint_id: platform_labor_autonomy_theater
 *   human_readable: Platform Labor Autonomy Theater
 *   domain: labor/digital_economy/platform_capitalism
 *
 * SUMMARY:
 *   Platform labor autonomy theater represents a structural hybrid of genuine
 *   coordination and sophisticated extraction. Platforms provide real
 *   coordination functions — matching workers to tasks with lower friction
 *   than traditional labor markets, enabling flexible work schedules, and
 *   reducing transaction costs. Simultaneously, platforms extract through
 *   algorithmic control, suppression of collective action, and the rhetorical
 *   separation of employment relationships from regulatory oversight. The
 *   'autonomy' narrative is not pure deception; workers do experience choice
 *   and flexibility unavailable in traditional employment. But the theater
 *   consists in the platforms' active portrayal of algorithmic task
 *   assignment and deportation-threat enforcement as expressions of worker
 *   autonomy rather than control mechanisms. The constraint's trajectory
 *   shows increasing theater ratio (0.62→0.81 over the interval) as platforms
 *   refine their autonomy marketing alongside deepening algorithmic control.
 *   Extractiveness also increases (0.48→0.62), indicating that the initial
 *   claim that platforms 'merely coordinate' has degraded into visible
 *   extraction — as algorithmic suppression becomes more sophisticated and
 *   labor organizing pressure mounts, the extraction becomes harder to mask
 *   as coordination alone.
 *
 * KEY AGENTS:
 *   - Gig Workers: Primary victims (powerless/trapped) — face economic dependency, lack alternative employment at comparable terms, cannot exit without substantial cost. Trapped by material barriers (income dependency, switching costs) and increasingly by internalized beliefs about platform rules.
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — capture pricing power through algorithmic control, can unilaterally set terms, have arbitrage exit (shift between geographies). Experience extraction as pure coordination.
 *   - Labor Regulators: Secondary actors (moderate/constrained) — face pressure to protect workers while also benefiting from platform efficiency. Cannot simply ban platforms without labor market disruption.
 *   - Worker Organizing Movements: Organized challengers (organized/constrained) — building cooperative platforms and portable reputation systems; see the autonomy theater as degrading but not permanent.
 *   - Classification Theater Participants: Institutional inertia (institutional/arbitrage) — lawyers, judges, regulators engaged in the ritual of employment classification disputes that produce no functional change in worker conditions.
 *   - Analytical Observer: Systemic view (analytical/analytical) — sees the constraint as a stable tangled_rope hybrid requiring active enforcement from platforms to suppress collective action.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_labor_autonomy_theater, 0.58).
domain_priors:suppression_score(platform_labor_autonomy_theater, 0.68).
domain_priors:theater_ratio(platform_labor_autonomy_theater, 0.76).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_labor_autonomy_theater, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_labor_autonomy_theater, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(platform_labor_autonomy_theater, theater_ratio, 0.76).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_labor_autonomy_theater, tangled_rope).
narrative_ontology:human_readable(platform_labor_autonomy_theater, "Platform Labor Autonomy Theater").
narrative_ontology:topic_domain(platform_labor_autonomy_theater, "labor/digital_economy/platform_capitalism").

domain_priors:requires_active_enforcement(platform_labor_autonomy_theater).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_labor_autonomy_theater, platform_operators).
narrative_ontology:constraint_beneficiary(platform_labor_autonomy_theater, algorithmic_coordination_system).
narrative_ontology:constraint_victim(platform_labor_autonomy_theater, gig_workers).
narrative_ontology:constraint_victim(platform_labor_autonomy_theater, labor_regulatory_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GIG WORKER (SNARE) — Faces material barriers to exit: economic dependency on platform income, lack of alternative employment with comparable flexibility/pay, switching costs (reputation ratings, historical data). Suppression is high: workers cannot negotiate terms, collectively organize is structurally difficult, and platform can unilaterally change compensation. The autonomy rhetoric ('be your own boss') masks extraction — workers have no meaningful control over core mechanics (pricing, task allocation, deactivation criteria). Maximum experienced extraction.
constraint_indexing:constraint_classification(platform_labor_autonomy_theater, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LABOR REGULATOR (TANGLED ROPE) — Faces mixed incentives. Coordinating worker protection requires platform data and cooperation; extraction occurs through platform's ability to define employment classification and evade regulatory scope. High suppression: platforms litigate classification disputes and lobby regulators. But regulators also benefit from the platform model's efficiency in labor matching and from platform companies' tax payments. Constrained exit — regulators cannot simply ban platforms without disrupting labor markets.
constraint_indexing:constraint_classification(platform_labor_autonomy_theater, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Benefits from the autonomy theater. The coordination function is genuine (matching workers to tasks, reducing transaction costs), but the theater enables extraction: portraying workers as independent contractors obscures the control mechanisms (algorithmic task assignment, rating-based survival pressure). Arbitrage exit available — platforms can shift between geographies and regulatory regimes. Experiences the constraint as pure coordination.
constraint_indexing:constraint_classification(platform_labor_autonomy_theater, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WORKER ORGANIZING MOVEMENTS (SCAFFOLD) — See the autonomy theater as temporary; building alternative platforms (cooperative models, union-negotiated standards, portable reputation systems) that bypass extraction. Constrained by platform dominance but not trapped. Sunset logic: as portable benefits, worker-controlled platforms, and sectoral bargaining mature, the extraction mechanism loses force. Theater_ratio declining as workers develop independent reputation systems and algorithmic literacy.
constraint_indexing:constraint_classification(platform_labor_autonomy_theater, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CLASSIFICATION THEATER (PITON) — Legal fiction of 'independent contractor' status persists through inertia despite structural evidence of employment (algorithmic control, task assignment, deactivation mechanisms). The ritual of classification disputes (courts weighing factors like control, integration, profit/loss) is theater — the underlying extraction is unchanged by classification outcomes. Theater_ratio ≥0.70: regulatory theater produces no functional change in platform business model or worker conditions.
constraint_indexing:constraint_classification(platform_labor_autonomy_theater, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From the civilizational view, the autonomy theater is a genuine hybrid: platforms DO provide coordination (task matching, reduced friction, rapid scaling) AND extract via suppression (algorithmic control, information asymmetry, deportation threat). Classification as tangled_rope is stable across temporal horizons — the mix of coordination and extraction is structural, not transitional. The constraint requires active enforcement (platforms must actively suppress collective action) and benefits genuine coordination agents (platforms) while harming victims (workers and labor capacity).
constraint_indexing:constraint_classification(platform_labor_autonomy_theater, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_labor_autonomy_theater_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_labor_autonomy_theater, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_labor_autonomy_theater, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_labor_autonomy_theater, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_labor_autonomy_theater, TR),
    TR >= 0.70.

:- end_tests(platform_labor_autonomy_theater_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.58): Moderate-high. The platform extracts through algorithmic control (task assignment, filtering, rating manipulation), suppression of collective action (algorithm-based worker isolation, terms-of-service restrictions on communication), and information asymmetry (workers cannot see pricing algorithms or deactivation criteria). But extractiveness is not maximum (0.72+) because platforms do provide genuine benefits: flexible scheduling, quick income access, reduced friction in labor matching. The value reflects that extraction is embedded in and inseparable from coordination — the same algorithmic system that matches tasks efficiently also controls worker behavior. Suppression (0.68): High. Multiple suppression mechanisms: economic dependency (workers need platform income), switching costs (reputation scores don't port), technical barriers (workers cannot access algorithms or data), and structural isolation (algorithmic routing prevents worker-to-worker communication and organizing). Platforms actively suppress collective action through terms-of-service enforcement and by fragmenting the workforce. Theater Ratio (0.76): High and rising. The autonomy narrative is the primary theater — platforms market workers as 'independent entrepreneurs' while implementing control structures comparable to traditional employment. The 'flexibility' and 'choice' narratives obscure that algorithmic assignment, deactivation threats, and rating pressure create coercive work environments. Rising trajectory reflects that as evidence of control accumulates (academic studies, litigation, worker testimony), platforms invest more heavily in autonomy marketing to maintain the fiction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival divergence. The gig worker perceives the constraint as entrapment (snare) with no coordination benefit — they see algorithmic task assignment as coercion, not choice. The platform operator perceives the constraint as pure coordination — they see algorithmic task assignment as efficient matching. The labor regulator perceives mixed pressure and mixed benefit (tangled_rope). The worker organizer perceives a temporary arrangement (scaffold) with a real exit path via cooperatives. The classification theater perceives a ritual that persists through inertia (piton) despite its non-functionality. The analytical observer at the highest level sees a stable tangled_rope — the perspectival divergence itself is the diagnostic signature of a tangled constraint. The gap between worker snare and platform rope is unbridgeable from within the framework each uses (autonomy vs control), which is why the autonomy theater is functionally necessary — it prevents both sides from acknowledging the tangled structure.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (χ) is computed from their structural position. The gig worker is a victim with trapped exit: high d → high f(d) → high χ (snare experience). The platform operator is a beneficiary with arbitrage exit: low d → low/negative f(d) → negative χ (rope experience — they experience the constraint as a subsidy). The labor regulator has mixed directionality: constrained exit + pressure from both sides → medium d → medium χ (tangled_rope experience). Worker organizers have organized power with constrained exit: medium d → medium f(d) → moderate χ. The classification theater participants have institutional power with arbitrage exit: zero extraction for them personally (they profit from litigation/regulation regardless of outcome). The analytical observer at global/civilizational scope sees the constraint structure itself: the directionality is embedded in the architecture (platforms control algorithms, workers depend on access), so χ is determined by the agent's relationship to that architecture.
 *
 * MANDATROPHY ANALYSIS:
 *   The autonomy theater resolves mandatrophy by showing that the constraint is structurally tangled: it provides both coordination (task matching, reduced friction) and extraction (algorithmic control, suppression). The temptation to classify as pure rope ('platforms enable efficient labor matching') ignores the suppression mechanisms and the powerless agent's snare experience. The temptation to classify as pure snare ('platforms exploit workers through algorithmic control') ignores the genuine coordination benefits and the beneficiary's rope experience. The mandatrophy is resolved by recognizing that the theater itself is the mechanism: platforms actively maintain the fiction that algorithmic control is worker autonomy, and this fiction is necessary to prevent the extraction from triggering collective resistance. If workers widely accepted the snare classification, organizing would accelerate. The theater maintains both the coordination function (workers accept task assignment as autonomous choice) and the extraction (workers don't recognize suppression as such). The constraint requires active enforcement from platforms to suppress alternative interpretations of the relationship.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_perception_vs_structure,
    'Is worker belief in autonomy a constitutive element of the extraction mechanism or separable from it?',
    'Comparative analysis of worker behavior under different framing conditions; measurement of task acceptance/rejection rates when autonomy narrative is removed or inverted',
    'If constitutive: constraint includes identity_locked exit mechanism (cognitive capture amplifies suppression beyond structural barriers). If separable: suppression is purely structural, identity_locked is overreach. Affects classification from worker perspective (snare vs identity_locked exit boundary).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_perception_vs_structure, empirical, 'Whether worker belief in autonomy is mechanism or artifact').

omega_variable(
    algorithmic_control_degree,
    'What percentage of platform worker decisions are algorithmically constrained vs genuinely worker-chosen?',
    'Analysis of task acceptance patterns; measurement of algorithmic suggestion weighting vs worker rejection rates; comparison of algorithmic vs manual task allocation where both exist',
    'If >80% constrained: control is structural (snare classification robust). If <60% constrained: significant autonomy exists (tangled_rope with genuine choice component).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_control_degree, empirical, 'Degree of algorithmic constraint on worker decision-making').

omega_variable(
    platform_regulatory_exit,
    'Can platforms genuinely exit regulatory jurisdictions or are network effects and data lock-in binding?',
    'Historical analysis of platform relocation patterns; measurement of platform market share concentration in high-regulation vs low-regulation jurisdictions; feasibility analysis of cross-border platform operation',
    'If exit is real: platform exit_options = arbitrage (rope classification stable). If exit is constrained: exit_options = constrained (tangled_rope from platform perspective deepens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_regulatory_exit, empirical, 'Whether platforms can exit regulatory jurisdictions').

omega_variable(
    cooperative_platform_viability,
    'Do worker-owned cooperative platforms achieve comparable efficiency or network reach as extractive platforms?',
    'Comparative analysis of cooperative vs for-profit platform metrics: task completion rates, worker earnings, user volume, feature parity, sustainability; longitudinal tracking of cooperative platform maturation',
    'If viable: scaffold perspective is realistic (exit path exists). If not viable: scaffold is aspirational, workers face harder exit constraints than analysis suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cooperative_platform_viability, empirical, 'Whether cooperative platforms are structurally viable alternatives').

omega_variable(
    suppression_structural_vs_internalized,
    'How much of measured suppression is structural (economic dependency, technical barriers) vs internalized (workers have internalized platform rules, accept degradation as normal)?',
    'Measurement of suppression post-exit: do workers who leave platforms retain belief in platform rules and constraints? Do new workers enter platforms already expecting low-autonomy treatment? Comparative analysis of suppression levels before vs after worker consciousness-raising',
    'If primarily structural: suppression is objective barrier (trapped exit is appropriate). If substantial internalization: identity_locked exit mechanism is active, constraint includes cognitive capture layer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether platform suppression is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_labor_autonomy_theater, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plat_tr_t0, platform_labor_autonomy_theater, theater_ratio, 0, 0.62).
narrative_ontology:measurement(plat_tr_t3, platform_labor_autonomy_theater, theater_ratio, 3, 0.7).
narrative_ontology:measurement(plat_tr_t6, platform_labor_autonomy_theater, theater_ratio, 6, 0.76).
narrative_ontology:measurement(plat_tr_t9, platform_labor_autonomy_theater, theater_ratio, 9, 0.81).

% Extraction over time
narrative_ontology:measurement(plat_be_t0, platform_labor_autonomy_theater, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(plat_be_t3, platform_labor_autonomy_theater, base_extractiveness, 3, 0.53).
narrative_ontology:measurement(plat_be_t6, platform_labor_autonomy_theater, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(plat_be_t9, platform_labor_autonomy_theater, base_extractiveness, 9, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_labor_autonomy_theater, resource_allocation).
narrative_ontology:boltzmann_floor_override(platform_labor_autonomy_theater, 0.18).
narrative_ontology:affects_constraint(platform_labor_autonomy_theater, algorithmic_management_opacity).
narrative_ontology:affects_constraint(platform_labor_autonomy_theater, labor_regulatory_arbitrage).
narrative_ontology:affects_constraint(platform_labor_autonomy_theater, worker_reputation_lock_in).

% DUAL FORMULATION NOTE:
% The autonomy theater is a cluster of three related constraints: algorithmic management opacity (ε≈0.35, prevents workers from understanding control mechanisms), labor regulatory arbitrage (ε≈0.45, platforms shift between jurisdictions to avoid regulation), and worker reputation lock-in (ε≈0.52, reputation scores don't port across platforms, increasing switching costs). Each has its own extractiveness value and perspectives. This story focuses on the rhetorical layer (autonomy theater) that enables all three by preventing workers from recognizing them as constraints. The floor override (0.18 vs 0.15 default for resource_allocation) reflects that genuine task-matching coordination is more costly than generic allocation systems — platforms run real optimization, not just random assignment.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_labor_autonomy_theater, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
