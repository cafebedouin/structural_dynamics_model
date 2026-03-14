% ============================================================================
% CONSTRAINT STORY: optimization_function_design
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_optimization_function_design, []).

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
 *   constraint_id: optimization_function_design
 *   human_readable: Optimization Function Design in Complex Systems
 *   domain: systems_design/organizational_coordination
 *
 * SUMMARY:
 *   Optimization function design represents a structural constraint that
 *   arises whenever complex systems must coordinate across multiple
 *   objectives, constrained resources, or competing agents. The constraint
 *   emerges from a genuine coordination problem — some objective must be
 *   selected to guide allocation decisions — but the solution creates
 *   asymmetric extraction: those who benefit from the chosen objective reap
 *   rewards, while those whose interests are excluded or deprioritized bear
 *   costs with no voice. The extractiveness increases over time as Goodhart
 *   feedback mechanisms cause metric gaming to decouple function performance
 *   from actual system health, converting the objective from a coordination
 *   tool into a performative ritual. This pattern scales across domains:
 *   financial optimization (shareholder value extraction), organizational
 *   metrics (revenue targets ignoring employee welfare), ecological
 *   management (maximum sustainable yield ignoring ecosystem stability),
 *   healthcare allocation (cost minimization extracting from patient
 *   outcomes), and machine learning objectives (proxy optimization causing
 *   alignment failures). The constraint's theater ratio trajectory
 *   (0.42→0.68) documents the degradation process as the original
 *   justification becomes obscure and the function persists through
 *   institutional inertia.
 *
 * KEY AGENTS:
 *   - Function Designer: Primary beneficiary (institutional/arbitrage) — captures coordination benefits and influence over system priorities through control of objective
 *   - Optimized Beneficiary Class: Primary beneficiary (variable power/exit) — actors whose interests are prioritized in the objective function; extract value through weighting advantage
 *   - Unmodeled Externalities: Primary victim (powerless/trapped) — costs and actors excluded from the objective function; bear extraction with no exit or representation
 *   - Non-Optimized Actors: Secondary victim (moderate/constrained) — partially included actors whose interests are deprioritized relative to the optimized class; can advocate for inclusion but face friction
 *   - System Stability: Tertiary victim (powerless/trapped) — at generational timescales, optimization for local objectives destabilizes the broader system; no exit from bearing costs of instability
 *   - Organized Opposition: Secondary actor (organized/constrained) — coalitions representing excluded interests; can mobilize to demand inclusion or reweighting but face suppression from incumbents
 *   - Analytical Observer: Meta-perspective (analytical/analytical) — risks naturalizing the contingent design choice as an immutable feature of complex systems
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(optimization_function_design, 0.58).
domain_priors:suppression_score(optimization_function_design, 0.62).
domain_priors:theater_ratio(optimization_function_design, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(optimization_function_design, extractiveness, 0.58).
narrative_ontology:constraint_metric(optimization_function_design, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(optimization_function_design, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(optimization_function_design, tangled_rope).
narrative_ontology:human_readable(optimization_function_design, "Optimization Function Design in Complex Systems").
narrative_ontology:topic_domain(optimization_function_design, "systems_design/organizational_coordination").

domain_priors:requires_active_enforcement(optimization_function_design).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(optimization_function_design, function_designer).
narrative_ontology:constraint_beneficiary(optimization_function_design, optimization_beneficiary_class).
narrative_ontology:constraint_victim(optimization_function_design, unmodeled_externalities).
narrative_ontology:constraint_victim(optimization_function_design, non_optimized_actors).
narrative_ontology:constraint_victim(optimization_function_design, system_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNMODELED EXTERNALITY (SNARE) — Actors and costs not included in the optimization function experience pure extraction with no coordination benefit and no exit. They cannot opt out of bearing the costs of optimizing for others. The constraint creates concentrated benefits for the optimized class and diffuse costs for those outside the model.
constraint_indexing:constraint_classification(optimization_function_design, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SYSTEM STABILITY AT GENERATIONAL SCALE (SNARE) — At short timescales, optimization improves performance. At generational timescales, pursuit of local optimization destabilizes the broader system. The system's long-term sustainability is trapped bearing costs that the optimization function explicitly ignored. No exit, no voice, no representation in the objective function.
constraint_indexing:constraint_classification(optimization_function_design, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: NON-OPTIMIZED ACTOR (ROPE/TANGLED ROPE) — Actors whose interests are partially included in the objective function experience mixed outcomes. They benefit from some coordination (the function solves genuine coordination problems) but bear extraction through the weighting and prioritization of others' interests above theirs. Constrained because they can advocate for inclusion but face significant friction in changing the function.
constraint_indexing:constraint_classification(optimization_function_design, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FUNCTION DESIGNER (ROPE) — Experiences the constraint as pure coordination. The optimization function solves the genuine problem of allocating resources or attention across competing objectives. The designer captures primary benefit but also experiences legitimate efficiency gains from coordination. Can arbitrage the design by incorporating new objectives or adjusting weights.
constraint_indexing:constraint_classification(optimization_function_design, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: ORGANIZED OPPOSITION (TANGLED ROPE) — Groups representing externalities or excluded actors can mobilize to demand inclusion or reweighting. They perceive the constraint as mixed: genuine coordination function (the system needs some objective) paired with asymmetric extraction (their interests are deprioritized). As organized actors, they have agency but face suppression from the incumbent beneficiaries who designed the function.
constraint_indexing:constraint_classification(optimization_function_design, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LEGACY OBJECTIVE FUNCTION (PITON) — Over time, the original rationale for the optimization function's structure becomes obscure. The function persists through institutional inertia even after the context that justified its weights has shifted. Much of the function's operation becomes theatrical — maintaining the form of optimization while substantive priorities have drifted. The function is kept alive not because it works but because replacing it creates coordination friction.
constraint_indexing:constraint_classification(optimization_function_design, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LIMIT VIEW (MOUNTAIN) — From a civilizational perspective, some optimization mismatch may appear inherent to complex systems: any finite objective function necessarily excludes unmeasurable goods and incompressible values. This perspective risks treating contingent design choices (what to measure, what to optimize) as immutable constraints on human agency. However, the structural data contradicts the mountain classification, revealing this as false naturalization of a choice.
constraint_indexing:constraint_classification(optimization_function_design, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(optimization_function_design_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(optimization_function_design, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(optimization_function_design, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(optimization_function_design, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(optimization_function_design, TR),
    TR >= 0.70.

:- end_tests(optimization_function_design_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts value from excluded actors and unmodeled externalities, but the extraction is not maximal (0.70+) because a genuine coordination function exists — some objective must guide allocation. The function solves a real coordination problem while creating asymmetric benefit distribution. The measured value reflects that ~58% of the constraint's force derives from asymmetric beneficiary selection rather than from coordination necessity. Suppression (0.62): High. Multiple mechanisms suppress exit and voice from excluded actors: (1) Measurement boundaries make excluded costs invisible, (2) Metric-based justifications shield the objective from moral challenge, (3) Switching costs for system redesign create lock-in, (4) Incumbent beneficiaries have resources to protect their weighting, (5) Identity fusion of designers with the existing function prevents redesign. Theater ratio (0.68): High and rising. Initial theater (0.42) reflects genuine optimization function performing as designed. By interval end (0.68), Goodhart feedback has caused metric gaming, selective measurement, and decoupling between metric performance and system health — the function is increasingly performative. The trajectory (0.42→0.58→0.68) shows the transition from Tangled Rope (mixed coordination-extraction) toward Piton (degraded ritual) as the original purpose becomes obscure.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is the core diagnostic. The function designer and primary beneficiary see Rope (pure coordination benefit). The unmodeled externality sees Snare (pure extraction cost). The non-optimized actor sees Tangled Rope (mixed benefit and cost). The organized opposition sees Tangled Rope with higher chi (greater extraction cost). The legacy function perspective sees Piton (performative inertia). The analytical observer risks Mountain (naturalizing the contingent choice). The gap reveals that optimization function design is not a settled question of how to coordinate — it is a contested distribution of voice and power over what counts as a problem, what solutions are acceptable, and who bears externalized costs. The constraint persists because the beneficiaries have structural advantage in defending it; the extraction persists because the excluded have no mechanism to demand inclusion.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from the agent's position relative to the optimization function: beneficiaries who are included in the objective experience low d (0.05-0.20), because the function is designed to benefit them. Excluded actors experience high d (0.85-0.95), because the function was explicitly designed without them. Non-included but organized actors experience moderate d (0.45-0.65), because they have voice but are subordinated in the weighting. The designer's d value (0.15-0.25) is lower than the organized opposition (0.55) because the designer arbitrages the function (can modify it) while the opposition is constrained (can only advocate for inclusion). Directionality overrides are not needed — the derivation from beneficiary/victim declarations and exit options produces the correct d values.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint avoids false classification as pure coordination (Rope) or pure extraction (Snare) by the Tangled Rope diagnosis. Rope would misidentify it as low-extraction coordination; Snare would miss the genuine coordination problem it solves. Tangled Rope captures the structural reality: the function coordinates (true beneficiary), the function extracts (true cost for excluded), these coexist (extraction is not incidental, coordination is not mythical), and the constraint requires active enforcement (suppression of voice from excluded actors, maintenance of metric legitimacy against Goodhart gaming, defense of designer authority against organized opposition). The mandatrophy resolves by rejecting the false choice between 'this is coordination' and 'this is extraction' — it is both, and both are structural, not contingent or incidental.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurability_boundary,
    'What portion of the constraint''s extractiveness derives from genuine coordination necessity versus contingent choice to optimize for measurable proxies at the expense of unmeasured values?',
    'Historical analysis of alternative objective function designs that achieved similar coordination with different exclusions; counterfactual modeling of alternative weightings',
    'If contingent choice dominates: the constraint reclassifies as pure Snare (design choice, not natural law). If genuine necessity dominates: Tangled Rope classification stands (coordination cost is real, extraction is partial).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurability_boundary, conceptual, 'Extent to which measurability requirements versus design choices drive the optimization structure').

omega_variable(
    externality_feedback_lag,
    'How long does it take for unmodeled externalities to create visible system-level failures that force function redesign? Is the lag long enough that the extractive regime persists across multiple human generations?',
    'Historical analysis of optimization failures across domains (financial systems, ecological management, healthcare allocation); timeline from externality emergence to objective function modification',
    'If lag > 40 years: extractive regime is stable at individual lifetime scales, justifying Snare classification for unmodeled externalities. If lag < 10 years: feedback mechanisms provide course correction, weakening the Snare interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(externality_feedback_lag, empirical, 'Timeline from externality emergence to objective function redesign').

omega_variable(
    goodhart_degradation_mechanism,
    'Does the optimization function''s theater ratio increase over time as optimization pressure causes the metric to diverge from the underlying objective it was meant to measure (Goodhart''s Law)?',
    'Measurement of metric gaming, selective measurement manipulation, and decoupling between metric performance and actual system health over historical time intervals',
    'If theater increases significantly: the constraint degrades from Tangled Rope (mixed coordination-extraction) toward Piton (performative inertia). The theater ratio measurement trajectory empirically resolves this.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(goodhart_degradation_mechanism, empirical, 'Whether Goodhart gaming increases the constraint''s theater ratio over time').

omega_variable(
    designer_capture_identity_lock,
    'To what extent does the function designer''s professional identity and career success become fused with the existing objective function, making redesign psychologically equivalent to professional self-destruction even when the function no longer serves its stated purpose?',
    'Analysis of resistance to objective function modification among incumbent designers; documentation of career consequences for those proposing fundamental restructuring',
    'If identity lock is strong: designer perspective shifts from institutional/arbitrage (mobile, can redesign) to institutional/identity_locked (structurally mobile but cognitively trapped). This explains persistence of degraded functions better than inertia alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(designer_capture_identity_lock, conceptual, 'Whether designer professional identity becomes fused with the objective function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(optimization_function_design, 0, 45).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(optfn_tr_t0, optimization_function_design, theater_ratio, 0, 0.42).
narrative_ontology:measurement(optfn_tr_t15, optimization_function_design, theater_ratio, 15, 0.58).
narrative_ontology:measurement(optfn_tr_t30, optimization_function_design, theater_ratio, 30, 0.68).
narrative_ontology:measurement(optfn_tr_t45, optimization_function_design, theater_ratio, 45, 0.71).

% Extraction over time
narrative_ontology:measurement(optfn_be_t0, optimization_function_design, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(optfn_be_t15, optimization_function_design, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(optfn_be_t30, optimization_function_design, base_extractiveness, 30, 0.58).
narrative_ontology:measurement(optfn_be_t45, optimization_function_design, base_extractiveness, 45, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(optimization_function_design, resource_allocation).
narrative_ontology:affects_constraint(optimization_function_design, metric_gaming_feedback).
narrative_ontology:affects_constraint(optimization_function_design, goodhart_law_dynamics).
narrative_ontology:affects_constraint(optimization_function_design, stakeholder_exclusion_bias).

% DUAL FORMULATION NOTE:
% Optimization function design is upstream of domain-specific constraints (financial allocation, healthcare metrics, ecological management). Each domain constraint has its own objective function as a sub-component; this constraint story describes the general structural pattern. Domain-specific instantiations inherit this constraint's extractiveness signature but may show different suppression and theater ratios depending on feedback mechanisms and stakeholder power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
