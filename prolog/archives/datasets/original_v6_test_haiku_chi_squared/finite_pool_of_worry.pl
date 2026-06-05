% ============================================================================
% CONSTRAINT STORY: finite_pool_of_worry
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_finite_pool_of_worry, []).

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
 *   constraint_id: finite_pool_of_worry
 *   human_readable: The Finite Pool of Worry Hypothesis
 *   domain: psychological/social
 *
 * SUMMARY:
 *   The Finite Pool of Worry hypothesis posits that individuals and
 *   collectives have a limited capacity to attend to, emotionally process,
 *   and cognitively manage multiple negative events or crises simultaneously.
 *   When one issue dominates the psychological/social landscape, competing
 *   legitimate problems receive reduced attention, resources, and collective
 *   action capacity. This constraint operates at the intersection of
 *   cognitive limits and institutional architecture: while human information
 *   processing has inherent bandwidth constraints, the severity and rigidity
 *   of the finite pool is substantially amplified by narrative control, media
 *   gatekeeping, and institutional structures that concentrate attention. The
 *   constraint exhibits asymmetric extraction: narrative controllers and
 *   attention-gatekeeping institutions benefit from this scarcity by
 *   maintaining agenda-setting power, while subordinate issues, psychological
 *   resilience, and collective problem-solving capacity bear the costs. The
 *   theater ratio (0.58) reflects that much discourse about
 *   'whole-of-society' responses, 'holistic governance,' and 'addressing
 *   multiple crises' persists without corresponding institutional capacity —
 *   the machinery of multi-issue coordination has atrophied, leaving
 *   performative talk in place of functional structures.
 *
 * KEY AGENTS:
 *   - Subordinate Issues: Primary victims (powerless/trapped) — legitimate crises receive zero salience when the finite pool is exhausted regardless of severity
 *   - Individual Psychological Resilience: Primary victim (powerless/trapped) — cognitive/emotional exhaustion from saturation impairs adaptive coping and collective action capacity
 *   - General Public: Secondary victim (moderate/constrained) — benefit from simplified information architecture but bear cost of suppressed alternatives and manufactured scarcity
 *   - Narrative Controllers: Primary beneficiary (institutional/arbitrage) — media outlets, political elites, attention-shaping institutions capture agenda-setting power through scarcity control
 *   - Collective Problem-Solving Systems: Tertiary victim (organized/constrained) — institutional structures for multi-issue coordination have degraded; piton classification reflects performative residue
 *   - Social Movement Coalitions: Tertiary actors (organized/arbitrage) — competing movements experience the pool as both coordination mechanism (forces prioritization) and extraction mechanism (suppression of rivals)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing institutional scarcity as immutable cognitive law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(finite_pool_of_worry, 0.52).
domain_priors:suppression_score(finite_pool_of_worry, 0.65).
domain_priors:theater_ratio(finite_pool_of_worry, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(finite_pool_of_worry, extractiveness, 0.52).
narrative_ontology:constraint_metric(finite_pool_of_worry, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(finite_pool_of_worry, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(finite_pool_of_worry, tangled_rope).
narrative_ontology:human_readable(finite_pool_of_worry, "The Finite Pool of Worry Hypothesis").
narrative_ontology:topic_domain(finite_pool_of_worry, "psychological/social").

domain_priors:requires_active_enforcement(finite_pool_of_worry).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(finite_pool_of_worry, narrative_controllers).
narrative_ontology:constraint_beneficiary(finite_pool_of_worry, attention_gatekeepers).
narrative_ontology:constraint_victim(finite_pool_of_worry, subordinate_issues).
narrative_ontology:constraint_victim(finite_pool_of_worry, psychological_resilience).
narrative_ontology:constraint_victim(finite_pool_of_worry, collective_problem_solving).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SUBORDINATE ISSUE (SNARE) — Once a competing concern achieves narrative dominance, alternative problems cannot access attention resources regardless of their actual severity or urgency. A legitimate crisis receives zero salience because the finite pool is exhausted. d≈0.92, f(d)≈1.40, σ=1.2 → χ≈0.86.
constraint_indexing:constraint_classification(finite_pool_of_worry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PSYCHOLOGICAL RESILIENCE (SNARE) — When the finite pool is saturated, individuals lose capacity for adaptive coping, problem-solving, and collective action. The cognitive/emotional exhaustion is not optional; it is a structural consequence of the constraint. d≈0.88, f(d)≈1.35, σ=1.0 → χ≈0.70.
constraint_indexing:constraint_classification(finite_pool_of_worry, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: GENERAL PUBLIC (TANGLED ROPE) — Citizens benefit from the constraint's coordination function: it simplifies complex information ecosystems and allows focus on what leaders deem most urgent. But they also bear the cost of suppressed alternatives and manufactured scarcity. Exit is constrained by information asymmetry and media architecture. d≈0.58, f(d)≈0.75, σ=1.0 → χ≈0.39.
constraint_indexing:constraint_classification(finite_pool_of_worry, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: NARRATIVE CONTROLLER (ROPE) — Media outlets, political elites, and attention-shaping institutions benefit from scarcity. The finite pool enables coordination of public focus and allows the controller to set the agenda. Beneficiary + arbitrage exit = low d. d≈0.08, f(d)≈-0.10, σ=1.2 → χ≈-0.06. Net beneficiary from the constraint.
constraint_indexing:constraint_classification(finite_pool_of_worry, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COLLECTIVE PROBLEM-SOLVING SYSTEM (PITON) — Institutional structures for addressing multiple simultaneous crises (multi-agency coordination, interdisciplinary teams, systems thinking) have largely atrophied in favor of single-issue focus. theater_ratio=0.58 reflects that much talk of 'holistic governance' persists without functional capacity. The machinery persists through inertia (sustainability boards, climate committees) but cannot operate when the finite pool is saturated.
constraint_indexing:constraint_classification(finite_pool_of_worry, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: SOCIAL MOVEMENT COALITION (TANGLED ROPE) — Organized movements that compete for finite attention experience the constraint as both coordination mechanism (it forces prioritization and focuses energy) and asymmetric extraction (competing movements are suppressed). Coalition + arbitrage = moderate d. d≈0.42, f(d)≈0.42, σ=1.2 → χ≈0.26. Moderate effective extraction.
constraint_indexing:constraint_classification(finite_pool_of_worry, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / COGNITIVE ARCHITECTURE VIEW (MOUNTAIN) — From a neuroscientific/information-theoretic perspective, finite attentional capacity is an immutable property of human cognition: working memory bandwidth, emotional processing bottlenecks, and information integration limits are hard constraints. However, the structural data (ε=0.52, suppression=0.65, theater=0.58, requires_active_enforcement=true) contradicts true mountain status. The engine will compute this as a false summit: natural cognitive limits are being weaponized through institutional architecture (media gatekeeping, narrative concentration) that is contingent and contestable.
constraint_indexing:constraint_classification(finite_pool_of_worry, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(finite_pool_of_worry_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(finite_pool_of_worry, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(finite_pool_of_worry, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(finite_pool_of_worry, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(finite_pool_of_worry, TR),
    TR >= 0.70.

:- end_tests(finite_pool_of_worry_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The finite pool creates a genuine scarcity in attention and collective action capacity that can be captured and exploited. However, it is not maximum extraction because (a) genuine cognitive limits do exist, and (b) subordinate issues sometimes break through despite saturation, indicating the pool is not absolutely rigid. The extractiveness reflects that institutional architecture amplifies natural limits, creating semi-artificial scarcity. Suppression (0.65): High. Competing issues face significant barriers: information asymmetry (narrative controllers choose what gets amplified), media architecture (algorithms concentrate attention), psychological fatigue (saturation impairs engagement), and institutional focus (governance systems address one crisis at a time). However, suppression is not absolute—some subordinate issues do mobilize attention through organized effort or spontaneous salience. Theater ratio (0.58): Moderate. The constraint's performative content has grown over the measurement interval (0.35→0.58). Much discourse about systemic approaches, interconnected crises, and holistic governance persists without corresponding institutional capacity. Multi-agency coordination structures exist (sustainability boards, climate committees) but function primarily as theater when the finite pool saturates.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates sharp perspectival divergence. Subordinate issues perceive pure extraction with no exit (Snare). The general public perceives a mixed benefit-cost arrangement (Tangled Rope) — simplified information architecture is useful but comes with suppressed alternatives. Narrative controllers perceive pure coordination (Rope) — they are solving the legitimate problem of managing attention in a complex world. The collective problem-solving system perceives its own degradation (Piton) — the machinery persists but cannot function. Social movements perceive mixed extraction-coordination (Tangled Rope) — the finite pool forces prioritization but also suppresses rivals. The analytical observer risks seeing immutable cognitive law (Mountain), but the structural data (requires_active_enforcement=true, extractiveness growing over time, beneficiaries/victims clearly asymmetric) reveals this as a false summit: the apparent inevitability is constructed through institutional choices, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinate issues: Victim + trapped → d≈0.92, f(d)≈1.40. Maximum extraction from powerless agent with no exit. Psychological resilience: Victim + trapped → d≈0.88, f(d)≈1.35. Same structure: loss of coping capacity due to exhaustion is involuntary. General public: Mixed (moderate) + constrained → d≈0.58, f(d)≈0.75. Benefits from simplified information but constrained by information asymmetry and media architecture. Narrative controller: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary; can exit the constraint by controlling which narratives dominate. Collective problem-solving: Victim (degraded capacity) + constrained → d≈0.65, f(d)≈1.05. Cannot exit institutional structures but lacks functional capacity. Social movements: Mixed (organized) + arbitrage → d≈0.42, f(d)≈0.42. Can arbitrage between different attention configurations but face suppression from competitors. Analytical observer: Neutral + analytical → d≈0.72, f(d)≈1.15. At risk of naturalizing institutional arrangement as law.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing between cognitive limits (legitimate natural law) and institutional extraction (contingent power asymmetry). The mountain perspective (infinite cognitive capacity is a physical law) is demonstrably false. The rope perspective (the finite pool is pure coordination) is incomplete — it misses the asymmetric extraction component. The snare perspective (subordinate issues are trapped) is accurate for the powerless but misses the coordination benefit the general public derives. The tangled_rope classification (ε≥0.30, suppression≥0.40, 0.40≤χ≤0.90, beneficiaries+victims+enforcement) captures the hybrid: the constraint has genuine coordination function (simplifying information architecture enables collective focus) AND asymmetric extraction (narrative controllers capture agenda-setting power, competing issues are suppressed). The mandatrophy is resolved by recognizing that the cognitive limit is real but the institutional exploitation of that limit is contestable: the theater ratio growth (0.35→0.58) indicates increasing gap between performative discourse (holistic governance) and functional capacity (actual multi-issue coordination structures), confirming that institutional scaffolding can be reformed to reduce extraction without violating cognitive biology.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_vs_institutional_limit,
    'Is the finite pool a hard cognitive limit (neural working memory, emotional bandwidth) or a socially constructed scarcity enabled by institutional information gatekeeping?',
    'Comparative studies of attention allocation in high-access vs low-access information environments; measurement of attention distribution when narrative controllers'' power is reduced; cognitive load testing under different information architectures',
    'If hard cognitive limit: constraint approaches Mountain classification (ε→0.10, suppression→0.10). If institutional construction: constraint remains Tangled Rope/Snare (ε≥0.46, suppression≥0.60).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cognitive_vs_institutional_limit, empirical, 'Whether finite worry is a cognitive or institutional constraint').

omega_variable(
    substitution_vs_addition,
    'When attention shifts from Issue A to Issue B, does the pool size remain constant (substitution) or can the pool expand with practice/cognitive development (addition)?',
    'Longitudinal studies of individuals'' attention breadth over time; comparison of attention capacity across cultures with different information architectures; experimental training of expanded attention capacity',
    'If pure substitution: suppression≥0.65 confirmed. If pool expands: suppression drops to 0.35-0.45, classification shifts toward Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(substitution_vs_addition, empirical, 'Whether worry allocation exhibits substitution or expansion dynamics').

omega_variable(
    extraction_motivation,
    'Do narrative controllers actively maintain and exploit the finite pool, or is suppression of competing issues a passive byproduct of their focus on one narrative?',
    'Media archive analysis of coverage concentration vs available space; interviews with editors/directors about attention allocation decisions; comparison of suppression rates when competing issues have institutional advocates vs when they do not',
    'If active exploitation: requires_active_enforcement=true confirmed, extractiveness≥0.50. If passive byproduct: enforcement may be incidental, extractiveness drops to 0.25-0.35.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_motivation, empirical, 'Whether finite pool suppression is active or passive').

omega_variable(
    collective_action_threshold,
    'What level of attention/salience is required before subordinate issues can mobilize collective action despite finite pool saturation?',
    'Historical analysis of issues that broke through despite competing dominance (e.g., metoo while other crises dominated); measurement of attention thresholds required for various collective action types (protest, legislation, institutional response)',
    'If threshold is very high: snare classification confirmed for subordinate issues. If threshold is moderate: tangled_rope may apply more broadly, suggesting coordination benefit alongside extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_threshold, empirical, 'Attention threshold required for collective action despite saturation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(finite_pool_of_worry, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fpow_tr_t0, finite_pool_of_worry, theater_ratio, 0, 0.35).
narrative_ontology:measurement(fpow_tr_t10, finite_pool_of_worry, theater_ratio, 10, 0.52).
narrative_ontology:measurement(fpow_tr_t20, finite_pool_of_worry, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(fpow_be_t0, finite_pool_of_worry, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(fpow_be_t10, finite_pool_of_worry, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(fpow_be_t20, finite_pool_of_worry, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(finite_pool_of_worry, information_standard).
narrative_ontology:affects_constraint(finite_pool_of_worry, narrative_substitution_bias).
narrative_ontology:affects_constraint(finite_pool_of_worry, collective_action_mobilization_barrier).

% DUAL FORMULATION NOTE:
% The finite pool operates downstream of media architecture and information gatekeeping constraints (narrative_substitution_bias: how story concentration creates attention crowding). It also affects collective action mobilization (collective_action_mobilization_barrier: how saturation impairs the ability to organize). The three stories form a causal chain: media architecture → finite pool → reduced mobilization.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(finite_pool_of_worry, analytical, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
