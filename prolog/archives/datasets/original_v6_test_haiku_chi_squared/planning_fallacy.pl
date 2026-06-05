% ============================================================================
% CONSTRAINT STORY: planning_fallacy
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_planning_fallacy, []).

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
 *   constraint_id: planning_fallacy
 *   human_readable: The Planning Fallacy
 *   domain: economic/social/technological
 *
 * SUMMARY:
 *   The planning fallacy is a structural constraint that operates at the
 *   intersection of cognitive bias and institutional incentive structures. At
 *   its root is a universal human tendency to underestimate task duration and
 *   overestimate completion capability — documented across domains from home
 *   renovations to software development to infrastructure projects. However,
 *   the constraint's extractive force in organizational contexts derives not
 *   from the bias alone but from systematic institutional amplification:
 *   optimistic planners benefit from schedule compression (career
 *   advancement, funding approval, narrative control), while dependent teams,
 *   resource allocators, and end users bear the costs of cascading delays.
 *   The constraint exhibits all six DR types from different structural
 *   positions: powerless dependent teams experience a snare; institutional
 *   beneficiaries experience near-neutral rope; project managers experience
 *   mixed tangled_rope; the scheduling apparatus itself is a piton —
 *   maintaining methodologies (Gantt charts, critical path) that are partly
 *   performative and fail to correct the underlying bias; and an analytical
 *   observer risks naturalizing the bias as inevitable human cognition. Over
 *   a decade-long measurement interval (representing organizational
 *   technology evolution and attempted methodological reforms), theater_ratio
 *   has risen from 0.35 to 0.58, indicating that planning governance has
 *   become increasingly procedural and documentation-focused while failing to
 *   systematically reduce underlying forecast errors.
 *
 * KEY AGENTS:
 *   - Optimistic Planners: Primary beneficiary (institutional/arbitrage) — capture schedule advantage and approval priority; experience the constraint as minor coordination overhead
 *   - Project Advocates: Primary beneficiary (institutional/arbitrage) — fund projects on compressed timelines; benefit from compressed narrative
 *   - Dependent Teams: Primary victim (powerless/trapped) — absorbed into cascading delays; no exit mechanism; suffer reputational damage for predecessor failures
 *   - Resource Allocators: Secondary victim (powerless/trapped) — budget forecasts and infrastructure deployment depend on plan accuracy; no recourse mechanism
 *   - End Users: Secondary victim (powerless/trapped) — delayed infrastructure, services, or products; no voice in planning process
 *   - Project Managers: Mixed (moderate/constrained) — both benefit from coordination mechanisms and suffer from schedule pressure
 *   - Institutional Scheduling Apparatus: Piton maintainer (institutional/arbitrage) — governance structures, methodologies, and review processes persist despite documented ineffectiveness
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements as inevitable cognitive limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(planning_fallacy, 0.52).
domain_priors:suppression_score(planning_fallacy, 0.65).
domain_priors:theater_ratio(planning_fallacy, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(planning_fallacy, extractiveness, 0.52).
narrative_ontology:constraint_metric(planning_fallacy, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(planning_fallacy, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(planning_fallacy, tangled_rope).
narrative_ontology:human_readable(planning_fallacy, "The Planning Fallacy").
narrative_ontology:topic_domain(planning_fallacy, "economic/social/technological").

domain_priors:requires_active_enforcement(planning_fallacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(planning_fallacy, optimistic_planners).
narrative_ontology:constraint_beneficiary(planning_fallacy, project_advocates).
narrative_ontology:constraint_beneficiary(planning_fallacy, institutional_schedulers).
narrative_ontology:constraint_victim(planning_fallacy, resource_allocators).
narrative_ontology:constraint_victim(planning_fallacy, dependent_teams).
narrative_ontology:constraint_victim(planning_fallacy, end_users).
narrative_ontology:constraint_victim(planning_fallacy, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT TEAM (SNARE) — Cannot exit commitment structures without career damage. Bears full cost of predecessor delays; no mechanism for recovery. Trapped by hierarchical dependencies and professional reputation. d≈0.93, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(planning_fallacy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: RESOURCE ALLOCATORS AND END USERS (SNARE) — Budget forecasts and infrastructure deployment depend on plan accuracy. No recourse when schedules collapse; absorbed into future delays. Trapped in cascading constraint violations. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.74.
constraint_indexing:constraint_classification(planning_fallacy, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: PROJECT MANAGERS AND TEAMS (TANGLED ROPE) — Constrained by organizational structures, but also benefit from planning frameworks, coordination mechanisms, and access to tools. Experience mixed extraction and coordination. d≈0.68, f(d)≈1.05, σ=0.9 → χ≈0.49.
constraint_indexing:constraint_classification(planning_fallacy, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: OPTIMISTIC PLANNERS AND PROJECT ADVOCATES (ROPE) — Benefit from planning window bias through career advancement, funding priority, and narrative control. Coordinate stakeholders through optimistic forecasts. d≈0.12, f(d)≈0.05, σ=1.0 → χ≈0.03. Near-neutral extraction; beneficiary position.
constraint_indexing:constraint_classification(planning_fallacy, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL SCHEDULING APPARATUS (PITON) — Gantt charts, earned value management, critical path analysis persist despite known failure to correct bias. Theater ratio=0.58 reflects that methodologies are partly ritualistic (documentation, meetings) rather than functionally corrective. Maintained through inertia; alternatives (probabilistic scheduling, Monte Carlo) available but underadopted. d≈0.10, f(d)≈-0.05, σ=0.9 → χ≈-0.003.
constraint_indexing:constraint_classification(planning_fallacy, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / COGNITIVE ARCHITECTURE VIEW (MOUNTAIN) — From a civilizational perspective, planning fallacy appears inherent to human cognition: optimism bias is a robust feature of metacognitive systems, not contingent on institutional arrangements. Emerges from the structure of bounded rationality and information asymmetry inherent to future prediction. However, empirical data (ε=0.52, suppression=0.65) suggests contingent institutional factors (career incentives, feedback loops, methodological theater) amplify base bias significantly.
constraint_indexing:constraint_classification(planning_fallacy, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(planning_fallacy_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(planning_fallacy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(planning_fallacy, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(planning_fallacy, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(planning_fallacy, TR),
    TR >= 0.70.

:- end_tests(planning_fallacy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The base extractiveness reflects institutional structures that systematically reward optimistic planning and delay accountability. The value has grown from 0.28 to 0.52 over the decade as organizations have consolidated around compressed scheduling norms and career incentive structures that penalize conservative estimation. This is not inevitable cognitive bias — it is institutionally amplified. Suppression (0.65): High. Barriers to accurate planning include: (1) Information asymmetry (optimistic planners control forecast distribution), (2) Career risk (conservative estimates reduce approval probability and advancement), (3) Feedback loop failure (post-mortem reviews are often non-binding and organizational memory is short), (4) Methodological theater (Gantt charts create illusion of control without functional correction), (5) Stakeholder cognitive capture (commitment to public forecasts creates defensive bias). Theater ratio (0.58): Moderate-high. Planning methodologies have become increasingly documentation-heavy (earned value management, critical path metrics, Monte Carlo forecasting in principle) while systematic forecast errors persist. The gap between methodological sophistication and actual accuracy improvement indicates increasing theater — procedures are maintained for governance legitimacy rather than functional correction. Claimed type (Tangled Rope): The constraint combines genuine coordination function (planning frameworks do enable stakeholder alignment) with asymmetric extraction (beneficiaries capture schedule advantage, victims absorb delays). Requires active enforcement through organizational governance structures.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests radically different types from different structural positions. Optimistic planners experience the constraint as Rope (near-neutral coordination for stakeholder alignment). Project managers experience Tangled Rope (genuine coordination function mixed with schedule pressure). Dependent teams and resource allocators experience Snare (pure extraction with no exit). The institutional scheduling apparatus appears as Piton (methodologies persist through inertia despite documented failure to correct bias). The analytical observer risks seeing Mountain (inevitable cognitive limit inherent to bounded rationality) — but empirical trends in theater_ratio and extractiveness suggest institutional amplification is primary driver. The perspectival gap is maximal because the constraint's structural function (coordination of schedules) is genuinely valuable, while its extraction mechanism (optimism bias amplified by career incentives) is genuinely harmful, and these two functions are inextricably entangled.
 *
 * DIRECTIONALITY LOGIC:
 *   Optimistic planners: Beneficiary + arbitrage → d≈0.12, f(d)≈0.05. Net beneficiary; experience as coordinating function. Dependent teams: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction exposure. Resource allocators: Victim + trapped → d≈0.95, f(d)≈1.42. Maximum extraction exposure; no control over plan accuracy, no recourse. Project managers: Victim + constrained → d≈0.68, f(d)≈1.05. Significant extraction but not maximal; benefit from methodology and process coordination. Institutional scheduling apparatus: Beneficiary + arbitrage → d≈0.10, f(d)≈-0.05. Piton classification comes from theater gate (≥0.70 not satisfied, but theater is substantial); maintained through institutional inertia. Analytical observer: Analytical → d≈0.72, f(d)≈1.15. Mountain perspective risks naturalizing contingent institutional factors as cognitive inevitability.
 *
 * MANDATROPHY ANALYSIS:
 *   The planning fallacy resolves mandatrophy by clarifying that the constraint is genuinely a Tangled Rope (mixed coordination and extraction) rather than pure Snare or pure Mountain. Base cognitive bias (the empirical fact that humans underestimate task duration) is real but modest in magnitude — Kahneman & Tversky baseline optimism bias produces ~20% underestimation. However, institutional amplification (career incentives rewarding compressed schedules, feedback loop failure, governance theater) inflates effective extraction to 0.52 and suppression to 0.65. The constraint cannot be resolved by dismissing it as 'inevitable human nature' (Mountain) because institutional structures are modifiable. Conversely, it cannot be resolved by ignoring the cognitive substrate and treating it as pure institutional extraction (Snare) because the bias is real and partially resistant to informational correction. The Tangled Rope classification captures both: there is genuine coordination function (schedule alignment is valuable), genuine cognitive constraint (optimism bias is universal), AND genuine institutional extraction (career incentives amplify bias and prevent correction). Rising theater_ratio suggests that methodological proliferation (more sophisticated planning tools) is becoming divorced from functional correction (actual forecast accuracy), indicating piton degradation — the scheduling apparatus is increasingly maintained through inertia and governance legitimacy rather than demonstrable effectiveness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    base_vs_amplified_bias,
    'What fraction of observed planning fallacy is intrinsic cognitive bias versus institutional structures that amplify or fail to correct it?',
    'Comparison of individual task estimation (Kahneman-Tversky reference data) versus organizational forecasting; analysis of planning accuracy across institutional contexts with varying feedback structures and incentive alignment',
    'If institutional amplification dominates (>60%): constraint is primarily Tangled Rope across all perspectives. If intrinsic bias dominates: mountain classification gains credibility from analytical observer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(base_vs_amplified_bias, empirical, 'Proportion of bias attributable to cognition versus institutional structures').

omega_variable(
    feedback_loop_effectiveness,
    'Do systematic planning reviews and historical data integration actually reduce planning fallacy severity, or does optimism bias persistently override corrective information?',
    'Longitudinal study of organizations implementing post-mortem analysis and probabilistic scheduling; measurement of forecast accuracy improvement over 10+ year horizons; analysis of whether teams update priors after repeated under-estimation',
    'If feedback loops work: suppression decreases over time, constraint trends toward Rope. If feedback is ignored: suppression remains high, institutional adoption of ''corrective'' methods is theater (piton characteristic).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feedback_loop_effectiveness, empirical, 'Whether feedback integration improves planning accuracy').

omega_variable(
    incentive_structure_manipulation,
    'To what degree do organizational incentives (promotion, bonuses, project approval) systematically reward optimistic planning and penalize conservative estimation?',
    'Analysis of promotion outcomes, funding approvals, and career trajectories relative to planning accuracy; comparison of organizations with performance bonuses tied to schedule adherence versus outcome quality',
    'If strong incentive misalignment: extractive institutional enforcement is primary driver (Snare/Tangled Rope from powerless perspectives). If incentives are neutral: base cognitive bias is primary (Mountain more credible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(incentive_structure_manipulation, preference, 'Degree to which institutional incentives reward optimistic planning').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(planning_fallacy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pf_tr_t0, planning_fallacy, theater_ratio, 0, 0.35).
narrative_ontology:measurement(pf_tr_t5, planning_fallacy, theater_ratio, 5, 0.48).
narrative_ontology:measurement(pf_tr_t10, planning_fallacy, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(pf_be_t0, planning_fallacy, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(pf_be_t5, planning_fallacy, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(pf_be_t10, planning_fallacy, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(planning_fallacy, resource_allocation).
narrative_ontology:affects_constraint(planning_fallacy, organizational_commitment_escalation).
narrative_ontology:affects_constraint(planning_fallacy, sunk_cost_rationalization).
narrative_ontology:affects_constraint(planning_fallacy, schedule_compression_extraction).

% DUAL FORMULATION NOTE:
% The planning fallacy can be decomposed into two structurally distinct claims: (1) Individual cognitive bias in task estimation (base planning fallacy, ε≈0.15, Mountain from cognitive science perspective); (2) Institutional amplification and extraction through schedule-based incentive structures (organizational planning extraction, ε≈0.52, Tangled Rope from organizational perspective). This story represents the institutional variant. The base cognitive constraint is invariant across observables, while the organizational extractive amplification is contingent on specific institutional incentive designs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(planning_fallacy, moderate, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
