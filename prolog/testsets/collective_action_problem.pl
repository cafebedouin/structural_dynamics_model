% ============================================================================
% CONSTRAINT STORY: collective_action_problem
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_collective_action_problem, []).

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
 *   constraint_id: collective_action_problem
 *   human_readable: Collective Action Problem
 *   domain: game_theory/coordination
 *
 * SUMMARY:
 *   The collective action problem is a fundamental constraint in coordination
 *   theory and social science: rational individual agents, acting in their
 *   own interest, produce outcomes that are Pareto-suboptimal for the group.
 *   The constraint operates across domains — public goods provision, climate
 *   change mitigation, labor organizing, open-source software, international
 *   treaties, resource commons management. This story models the collective
 *   action problem as a pure coordination mechanism (Rope) that exhibits
 *   Tangled Rope and Snare properties when enforcement becomes centralized or
 *   when free-riders escape suppression. The key insight is that collective
 *   action problems have multiple structurally distinct aspects: the genuine
 *   coordination barrier (information and trust), the enforcement mechanism
 *   (which can become extractive), and the free-rider equilibrium (which is
 *   itself an extractive solution from the perspective of non-cooperators).
 *   The indexical classification reveals these layers by showing how
 *   different agents experience the same constraint differently.
 *
 * KEY AGENTS:
 *   - Cooperating Agents: Primary beneficiaries (organized/mobile) — solve coordination problem, achieve mutual gains
 *   - Individual Agent: Participant (powerless/mobile) — experiences constraint as solvable coordination problem at local scale
 *   - Focal Enforcer: Powerful institutional actor (powerful/constrained) — centralizes enforcement, gains asymmetric control and information
 *   - Free-Rider: Non-cooperator (powerless/trapped) — trapped in extractive equilibrium where others bear cooperation costs
 *   - Institutional Coordinator: Formal body (institutional/arbitrage) — maintains performative coordination ritual with theater_ratio drift
 *   - Analytical Observer: Universal perspective (analytical/analytical) — sees collective action as solvable game-theoretic problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(collective_action_problem, 0.35).
domain_priors:suppression_score(collective_action_problem, 0.42).
domain_priors:theater_ratio(collective_action_problem, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(collective_action_problem, extractiveness, 0.35).
narrative_ontology:constraint_metric(collective_action_problem, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(collective_action_problem, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(collective_action_problem, rope).
narrative_ontology:human_readable(collective_action_problem, "Collective Action Problem").
narrative_ontology:topic_domain(collective_action_problem, "game_theory/coordination").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(collective_action_problem, cooperating_agents).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL AGENT (ROPE) — At immediate/local scale, the agent can move to different groups. The constraint is perceived as a coordination mechanism: cooperate with others to achieve mutual benefit. Low suppression because agents retain mobility; extractiveness moderate because coordination creates genuine shared gains.
constraint_indexing:constraint_classification(collective_action_problem, rope,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 2: ORGANIZED COALITION (ROPE) — Organized groups view the collective action constraint as a pure coordination problem. Over generations, institutionalization of cooperation reduces free-rider barriers. No extraction — beneficiaries and participants are the same set. The constraint solves a genuine collective problem with minimal coercive overhead.
constraint_indexing:constraint_classification(collective_action_problem, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: FOCAL ENFORCER (TANGLED ROPE) — Powerful actors (states, platforms, large firms) that centralize enforcement of collective agreements experience this as a hybrid. Genuine coordination function (everyone benefits from the public good) mixed with asymmetric extraction: the enforcer gains disproportionate influence and control over collective decisions. Constrained exit because enforcement role is now identity-fused with institutional position.
constraint_indexing:constraint_classification(collective_action_problem, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FREE-RIDER VICTIM (SNARE) — Non-cooperating agents that benefit from collective output but do not contribute are trapped. Other agents bear disproportionate costs to exclude them. The free-rider state is extractive — it captures benefits without paying costs, sustained by suppression (exclusion costs, monitoring, punishment threats). From this perspective, the constraint is pure extraction without coordination benefit.
constraint_indexing:constraint_classification(collective_action_problem, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 5: INSTITUTIONAL COORDINATOR (PITON) — Formal institutions (parliaments, trade organizations, international bodies) that nominally solve collective problems often become performative. Theater ratio high: committees meet, resolutions pass, but actual coordination relies on informal mechanisms. The institution persists through bureaucratic inertia rather than functional necessity. Institutional actors have arbitrage options (they can redefine their role or dissolve the body) but do not exercise them.
constraint_indexing:constraint_classification(collective_action_problem, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From the civilizational/universal perspective, collective action problems are instances of a general coordination mechanism. Game theory shows that repeated interaction, communication, reputation, and graduated sanctions enable pure coordination solutions without extraction. The constraint is fundamentally about overcoming information asymmetries and trust barriers — a coordination problem, not a natural law or an extractive mechanism.
constraint_indexing:constraint_classification(collective_action_problem, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(collective_action_problem_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(collective_action_problem, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(collective_action_problem, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(collective_action_problem, TR),
    TR >= 0.70.

:- end_tests(collective_action_problem_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate, reflecting that collective action problems have a genuine coordination component but also exhibit extractive dynamics when enforcement and free-riding are present. At the Rope level, extractiveness should be ≤0.45. The value of 0.35 indicates that coordination benefits outweigh extraction costs for most perspectives, but the tangled_rope and snare perspectives see higher extractiveness relative to their exit options. Suppression (0.42): Moderate. Collective action problems require some suppression to overcome free-rider incentives, but pure Rope requires suppression ≤0.35. The value 0.42 indicates that suppression is present but not dominant — there are multiple pathways to cooperation (not just coercion), and agents retain meaningful agency through mobility and organizing. Theater ratio (0.38): Moderate-low. Actual coordination achieves real functional outcomes in most domains, but institutional coordination mechanisms layer performative ritual on top of informal mechanisms. The measurement shows theater_ratio increasing over the interval (0.20 → 0.38) as institutional solutions accumulate bureaucratic overhead. This drift is characteristic of constraints where coordination function is genuine but institutional capture is incomplete.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between Rope and Snare perspectives is extreme. The cooperating agent sees pure coordination — the constraint is the solution, not the problem. The free-rider sees pure extraction — they are trapped in a state where others extract cooperation costs from them. Both are accurate: cooperation is voluntary for the agent who benefits, extractive for the agent who does not. The tangled_rope perspective (focal enforcer) bridges these: genuine coordination function mixed with asymmetric power accumulation. The piton perspective (institutional) reveals that coordination mechanisms degrade over time as ritual accumulates and informal mechanisms persist. The analytical observer sees the whole structure as a solved game-theoretic problem — the gap between Rope and Snare is not inherent but a function of entry conditions, information architecture, and group size.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply across perspectives because agents occupy structurally different positions. Cooperating agents have low d (beneficiaries who contribute little extraction) — they derive benefit from coordination itself. Individual agents with mobile exit options have low d — they can join or leave groups freely. Focal enforcers have d ≈0.40-0.50 (both benefit and bear enforcement cost, structurally centered) — but their institutional exit is constrained, moving them toward victim territory. Free-riders have d ≈1.0 (pure targets of collective suppression) — they are extracted from by other agents bearing cooperation costs. The piton perspective's d is low because institutional actors have arbitrage options — they are not structurally trapped, so even though the institution is performative, it is not extractive in the sense of trapping actors. The analytical perspective has canonical d ≈0.72 (observer position) — sufficiently detached to see the structure but not involved enough to see institutional capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The collective action problem resolves mandatrophy by decomposing into a family of structurally distinct constraints. Naive classification risks claiming this is 'just coordination' (Rope everywhere) or 'just extraction' (Snare everywhere). The indexical analysis shows: (1) Pure coordination layer: establishing communication and trust (Rope, low extractiveness). (2) Enforcement layer: monitoring and sanctioning non-cooperators (Tangled Rope when centralized, because it concentrates power). (3) Free-rider layer: non-cooperators trapped in extractive equilibrium (Snare from their perspective, because suppression is asymmetric). (4) Institutional layer: formal coordination bodies accumulate theater without functional gain (Piton, because theater_ratio rises and real function declines). These are NOT separate constraints — they are layers of the same phenomenon. The ε value (0.35) is the blended average. Individual analyses at different scales or focus domains would show different ε values: small-group cooperation (ε ≈0.10, pure Rope), large-scale institutional coordination (ε ≈0.50, Tangled Rope), climate mitigation failures (ε ≈0.65, Snare). The framework prevents false unification by requiring separate stories for structurally distinct observables. This story should link via network.affects_constraints to domain-specific constraint stories (climate_mitigation_free_rider, open_source_free_riding, commons_tragedy) where ε varies and classification diverges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_for_free_riding,
    'At what proportion of non-cooperators does a collective action solution become unsustainable?',
    'Empirical analysis across domains: public goods experiments, real-world commons management, climate mitigation pledges. Measure cooperation threshold and relate to group size, incentive structure, and monitoring cost.',
    'If threshold is high (>30% free-riders tolerated): collective action is robust, classification remains Rope. If threshold is low (<10%): suppression mechanism is fragile, snare perspective becomes dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_for_free_riding, empirical, 'Threshold proportion of non-cooperators sustainable in collective action').

omega_variable(
    coordination_vs_extraction_in_enforcement,
    'Is centralized enforcement of collective agreements a genuine coordination mechanism or a cover for asymmetric power consolidation?',
    'Comparative analysis: decentralized consensus vs centralized hierarchies. Measure outcome variance, benefit distribution, and enforcer power accumulation over time. Historical case studies (commons management, trade unions, international treaties).',
    'If enforcement is neutral: tangled rope classification confirmed, extractiveness moderate. If enforcement concentrates power: more snare characteristics emerge, extractiveness rises, suppression increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_in_enforcement, empirical, 'Whether centralized enforcement is coordination or extraction').

omega_variable(
    information_architecture_dependency,
    'How much of the collective action problem is inherent to coordination vs artifact of information access and communication barriers?',
    'Interventions that reduce information asymmetry: transparent monitoring, real-time reputation feedback, low-cost communication. Measure effect size on cooperation rates. Compare high-transparency vs low-transparency variants of same collective problem.',
    'If information access resolves the problem: extractiveness decreases, classification stays Rope. If problem persists despite transparency: suppression mechanism is structural, not informational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_architecture_dependency, empirical, 'Information architecture dependency of collective action barriers').

omega_variable(
    scale_dependency_of_mechanism,
    'Does the collective action constraint change type as group size scales?',
    'Game-theoretic analysis and empirical data: small-group dynamics vs large-group dynamics. Measure free-rider proportion, enforcement cost, and coordination overhead as functions of scale. Identify inflection points where classification changes.',
    'If mechanism is scale-invariant: Rope across all contexts. If mechanism degrades at large scale: classification transitions from Rope to Tangled Rope to Snare as size increases. This would indicate that ''collective action problem'' is actually a family of constraints with different ε values at different scales.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scale_dependency_of_mechanism, empirical, 'Scale dependency of collective action mechanism type').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(collective_action_problem, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cap_tr_t0, collective_action_problem, theater_ratio, 0, 0.2).
narrative_ontology:measurement(cap_tr_t3, collective_action_problem, theater_ratio, 3, 0.3).
narrative_ontology:measurement(cap_tr_t6, collective_action_problem, theater_ratio, 6, 0.38).

% Extraction over time
narrative_ontology:measurement(cap_be_t0, collective_action_problem, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cap_be_t3, collective_action_problem, base_extractiveness, 3, 0.25).
narrative_ontology:measurement(cap_be_t6, collective_action_problem, base_extractiveness, 6, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(collective_action_problem, resource_allocation).
narrative_ontology:affects_constraint(collective_action_problem, tragedy_of_the_commons).
narrative_ontology:affects_constraint(collective_action_problem, prisoner_dilemma).
narrative_ontology:affects_constraint(collective_action_problem, free_rider_equilibrium).
narrative_ontology:affects_constraint(collective_action_problem, coordination_game).

% DUAL FORMULATION NOTE:
% The collective action problem is a theoretical constraint family that decomposes into domain-specific instantiations. Each domain (climate mitigation, labor organizing, open-source contribution, public health vaccination, resource commons) has its own ε value reflecting the specific balance of coordination benefits and extraction mechanisms. This generic story models the abstract problem; domain-specific stories should override ε and perspectives based on empirical data from their domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
