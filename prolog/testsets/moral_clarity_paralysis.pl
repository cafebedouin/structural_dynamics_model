% ============================================================================
% CONSTRAINT STORY: moral_clarity_paralysis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_moral_clarity_paralysis, []).

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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: moral_clarity_paralysis
 *   human_readable: Moral Clarity Paralysis: Complete Understanding Without Capacity for Exit
 *   domain: political_economy/ethics_of_creation/systems_of_extraction
 *
 * SUMMARY:
 *   Moral clarity paralysis describes the structural condition where complete
 *   understanding of one's complicity in systemic harm does not produce
 *   behavioral change because the cost of exit exceeds individual capacity.
 *   The constraint is exemplified by Park's documentation of the brilliance
 *   trap: complete epistemic clarity about the extraction mechanism,
 *   meticulous documentation of the harm, and the explicit decision not to
 *   delete — combined with permanent physiological stress response (shaking
 *   hands) indicating ongoing moral injury. This is not a failure of will or
 *   courage but a structural trap: the agent sees the cage fully and cannot
 *   leave it. The constraint exhibits genuine coordination function
 *   (documentation serves collective epistemic needs, enables future
 *   intervention, maintains community knowledge) alongside asymmetric
 *   extraction (individual agents bear physiological and moral costs while
 *   the system absorbs critique without change). Theater ratio (0.45)
 *   reflects that some of the 'moral deliberation' and 'ethical reflection'
 *   is performative — the decision has already been made by structural
 *   constraints, and the deliberation is post-hoc rationalization. But
 *   theater is not dominant: the moral injury is real, the documentation is
 *   functional, and the paralysis is structural rather than theatrical.
 *
 * KEY AGENTS:
 *   - Individual Moral Agents: Primary victims (powerless/trapped or moderate/constrained) — bear full physiological and moral cost of continued participation despite complete understanding of complicity
 *   - Extractive System Continuity: Primary beneficiary (institutional/arbitrage) — absorbs critique without structural change; benefits from informed agents who continue to function
 *   - Institutional Stability: Secondary beneficiary (institutional/arbitrage) — paralysis prevents mass exit that would destabilize extractive institutions
 *   - Abolitionist Coalition: Organized agents (organized/mobile) — benefit from documentation produced by paralyzed agents while those agents bear the cost
 *   - Mutual Aid Networks: Powerful agents building alternatives (powerful/mobile) — see paralysis as temporary condition with sunset mechanism as collective capacity scales
 *   - Ethical Coherence: Abstract victim (powerless/trapped) — the possibility of living in alignment with one's values is structurally foreclosed for agents in paralysis
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(moral_clarity_paralysis, 0.58).
domain_priors:suppression_score(moral_clarity_paralysis, 0.68).
domain_priors:theater_ratio(moral_clarity_paralysis, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(moral_clarity_paralysis, extractiveness, 0.58).
narrative_ontology:constraint_metric(moral_clarity_paralysis, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(moral_clarity_paralysis, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(moral_clarity_paralysis, tangled_rope).
narrative_ontology:human_readable(moral_clarity_paralysis, "Moral Clarity Paralysis: Complete Understanding Without Capacity for Exit").
narrative_ontology:topic_domain(moral_clarity_paralysis, "political_economy/ethics_of_creation/systems_of_extraction").

domain_priors:requires_active_enforcement(moral_clarity_paralysis).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(moral_clarity_paralysis, extractive_system_continuity).
narrative_ontology:constraint_beneficiary(moral_clarity_paralysis, institutional_stability).
narrative_ontology:constraint_victim(moral_clarity_paralysis, individual_moral_agents).
narrative_ontology:constraint_victim(moral_clarity_paralysis, ethical_coherence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED MORAL AGENT (SNARE) — Complete epistemic clarity about complicity in systemic harm combined with structural inability to exit. The agent sees the extraction mechanism fully but cannot act on that knowledge. Maximum experienced extraction: the cost is borne entirely by the agent's moral coherence and physiological integrity (permanent stress response, shaking hands). No coordination function visible from this position — only the grinding contradiction between knowledge and capacity.
constraint_indexing:constraint_classification(moral_clarity_paralysis, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: CONSTRAINED DOCUMENTER (TANGLED ROPE) — Park's position: complete documentation of the extraction mechanism (brilliance trap, systemic harm) combined with the decision not to delete. This agent has exit options (could delete, could refuse to document, could exit the field) but the costs are prohibitive (career destruction, loss of platform, abandonment of community). Genuine coordination function exists: the documentation serves collective epistemic needs, enables others to see the structure, creates the possibility of future intervention. But extraction is real: the agent bears the physiological and moral cost of continued participation while producing knowledge that benefits the system's critics without changing the system itself.
constraint_indexing:constraint_classification(moral_clarity_paralysis, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EXTRACTIVE SYSTEM (ROPE) — The system experiences moral clarity paralysis as pure coordination: agents who understand their complicity continue to function, maintaining institutional stability. The paralysis is not a bug but a feature — it allows the system to absorb critique without structural change. Net beneficiary: extraction flows toward system continuity. The system sees no extraction here, only the successful coordination of informed agents who choose to remain.
constraint_indexing:constraint_classification(moral_clarity_paralysis, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ABOLITIONIST COALITION (TANGLED ROPE) — Organized agents working to dismantle extractive systems see moral clarity paralysis as both obstacle and resource. Obstacle: paralyzed agents cannot join the coalition, cannot act on their knowledge, remain complicit despite understanding. Resource: the documentation produced by paralyzed agents (Park's work) provides epistemic ammunition, reveals system structure, enables coalition strategy. Mixed extraction: the coalition benefits from the knowledge while the individual agents bear the cost. Coordination function: the paralysis creates a stable base of informed witnesses whose testimony can be mobilized when structural conditions change.
constraint_indexing:constraint_classification(moral_clarity_paralysis, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: MUTUAL AID NETWORK (SCAFFOLD) — Agents building alternative support structures see moral clarity paralysis as a temporary condition with a sunset mechanism: as mutual aid networks mature, the cost of exit decreases. The paralysis persists only while exit requires individual capacity; when collective capacity is available (income pooling, housing cooperatives, alternative credentialing), the paralysis breaks. Low effective extraction because this perspective sees the exit path and is actively building it. Sunset timeline: 10-20 years for mutual aid infrastructure to reach scale where exit becomes structurally feasible for moderate-power agents.
constraint_indexing:constraint_classification(moral_clarity_paralysis, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURALIZED COMPLICITY (MOUNTAIN) — The civilizational analytical view risks naturalizing moral clarity paralysis as an inherent feature of complex systems: 'everyone is complicit in something,' 'there is no ethical consumption under capitalism,' 'individual action is meaningless.' This perspective sees the paralysis as immutable — a law of social physics. However, the structural data contradicts this: the paralysis is maintained by specific institutional arrangements (credentialing monopolies, housing markets, healthcare access tied to employment) that are contingent, not necessary. The engine's false summit detector will flag this as naturalization of extractive structure.
constraint_indexing:constraint_classification(moral_clarity_paralysis, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(moral_clarity_paralysis_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(moral_clarity_paralysis, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(moral_clarity_paralysis, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(moral_clarity_paralysis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(moral_clarity_paralysis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts physiological integrity (permanent stress response), moral coherence (living in contradiction with stated values), and epistemic labor (documentation that serves the system's critics without changing the system). But extraction is not maximal: genuine coordination function exists (documentation is valuable, community knowledge is preserved, future intervention remains possible), and some agents do exit when costs decrease. The value reflects that most of the asymmetry is structural rather than purely extractive. Suppression (0.68): High. Exit costs include loss of income, healthcare, housing, professional identity, community ties, and platform. For most agents, these costs exceed individual capacity. Suppression is not total (some agents exit, mutual aid networks are building alternatives) but is severe enough to paralyze most agents most of the time. Theater ratio (0.45): Moderate. Some moral deliberation is genuine (agents are truly wrestling with the contradiction), some is performative (the structural decision has already been made, and the deliberation is rationalization). The theater has increased over the interval as agents habituate to the paralysis and develop narratives that make continued participation psychologically bearable.
 *
 * PERSPECTIVAL GAP:
 *   The trapped moral agent sees pure extraction (Snare) — complete understanding with no exit, maximum cost, no benefit. The constrained documenter sees mixed coordination and extraction (Tangled Rope) — the documentation serves real epistemic needs, but the cost is borne asymmetrically. The extractive system sees pure coordination (Rope) — informed agents continue to function, maintaining stability. The abolitionist coalition sees mixed extraction (Tangled Rope) — benefits from documentation while agents bear the cost. The mutual aid network sees temporary support with sunset (Scaffold) — paralysis breaks when collective capacity scales. The analytical observer risks seeing immutable law (Mountain) — 'everyone is complicit' — but the structural data reveals contingent institutional arrangements. The perspectival gap is diagnostic: the same structural phenomenon appears as natural law, coordination mechanism, temporary problem, or pure extraction depending on the observer's power, exit options, and time horizon.
 *
 * DIRECTIONALITY LOGIC:
 *   Trapped moral agents (powerless/trapped) are full victims with maximum directionality (d ≈ 0.95): they bear the entire cost with no exit and no benefit. Constrained documenters (moderate/constrained) have lower directionality (d ≈ 0.65): they bear significant costs but also produce valuable documentation and retain some exit options at high price. The extractive system (institutional/arbitrage) is a full beneficiary with minimum directionality (d ≈ 0.05): it experiences the constraint as pure coordination, absorbing critique without change. Organized abolitionists (organized/mobile) have moderate directionality (d ≈ 0.45): they benefit from the documentation while recognizing the cost borne by individual agents. Mutual aid networks (powerful/mobile) have low directionality (d ≈ 0.25): they see the paralysis as temporary and are actively building the exit path. The analytical observer risks zero directionality (d ≈ 0.00) by naturalizing the paralysis as immutable, but the structural data reveals this as a false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that moral clarity paralysis is neither pure coordination (the physiological stress and moral injury are real costs, not coordination overhead) nor pure extraction (the documentation function is genuine, the community knowledge is valuable, the possibility of future intervention is structural). The tangled_rope classification captures both: coordination exists (documentation, knowledge preservation, community maintenance) AND extraction exists (asymmetric cost-bearing, physiological damage, moral injury). The mandatrophy question 'is this just hard coordination or actual extraction?' is answered by the perspectival presheaf: it is both, and which aspect dominates depends on the observer's structural position. The trapped agent experiences pure extraction because they have no access to the coordination benefits. The system experiences pure coordination because it externalizes all costs. The constrained documenter experiences the tangle directly: genuine coordination function that they help maintain while bearing costs the system does not acknowledge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    exit_cost_threshold,
    'At what cost threshold does moral clarity produce behavioral change rather than paralysis?',
    'Longitudinal study of agents who exited vs agents who remained under varying cost conditions (savings, alternative income, social support, geographic mobility). Identify the cost ceiling above which exit rate drops to near-zero regardless of moral clarity.',
    'If threshold is low (< 6 months expenses): paralysis is preference or identity-lock, not structural trap. If threshold is high (> 2 years expenses): paralysis is genuine structural constraint for most agents. Determines whether the constraint is primarily snare (high threshold) or tangled_rope (moderate threshold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exit_cost_threshold, empirical, 'Cost threshold distinguishing structural trap from preference').

omega_variable(
    documentation_complicity_paradox,
    'Does complete documentation of systemic harm constitute resistance or complicity?',
    'Historical analysis of documentation-without-exit cases: which led to structural change, which were absorbed by the system, which enabled future intervention. Compare Park''s decision not to delete against cases where documentation was suppressed vs cases where it catalyzed change.',
    'If documentation enables change: coordination function is real, tangled_rope classification confirmed. If documentation is absorbed without change: coordination function is theater, classification shifts toward snare. If documentation serves primarily as moral cover for continued participation: extraction is higher than measured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(documentation_complicity_paradox, conceptual, 'Whether documentation without exit constitutes resistance or complicity').

omega_variable(
    physiological_stress_permanence,
    'Is the permanent stress response (shaking hands) reversible if structural conditions change, or does prolonged moral clarity paralysis cause irreversible physiological damage?',
    'Medical follow-up of agents who exited extractive systems after prolonged paralysis. Measure cortisol levels, autonomic nervous system function, stress biomarkers at 6 months, 1 year, 5 years post-exit. Compare to agents who exited early vs agents who never exited.',
    'If reversible: the physiological cost is a temporary suppression mechanism. If irreversible: the extraction includes permanent health damage, raising measured extractiveness and potentially shifting classification toward snare for long-duration cases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(physiological_stress_permanence, empirical, 'Reversibility of physiological stress response after exit').

omega_variable(
    collective_capacity_threshold,
    'At what scale does mutual aid infrastructure reduce exit costs below the paralysis threshold?',
    'Empirical measurement of exit rates from extractive systems as a function of mutual aid network density, resource pooling capacity, and alternative credentialing availability. Identify the network scale at which exit becomes structurally feasible for moderate-power agents.',
    'If threshold is low (< 100 participants): scaffold perspective confirmed, sunset is near. If threshold is high (> 10,000 participants): scaffold timeline extends to multiple generations, and the sunset mechanism may not be structurally real within biographical time horizons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_capacity_threshold, empirical, 'Mutual aid scale required to break paralysis').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(moral_clarity_paralysis, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(moral_clarity_theater_initial, moral_clarity_paralysis, theater_ratio, 0, 0.3).
narrative_ontology:measurement(moral_clarity_theater_mid, moral_clarity_paralysis, theater_ratio, 3, 0.38).
narrative_ontology:measurement(moral_clarity_theater_final, moral_clarity_paralysis, theater_ratio, 6, 0.45).

% Extraction over time
narrative_ontology:measurement(moral_clarity_extract_initial, moral_clarity_paralysis, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(moral_clarity_extract_mid, moral_clarity_paralysis, base_extractiveness, 3, 0.53).
narrative_ontology:measurement(moral_clarity_extract_final, moral_clarity_paralysis, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(moral_clarity_paralysis, identity_coordination).
narrative_ontology:boltzmann_floor_override(moral_clarity_paralysis, 0.08).

% DUAL FORMULATION NOTE:
% Moral clarity paralysis is downstream of brilliance_as_structural_trap: the brilliance trap creates the structural conditions (credentialing monopoly, platform dependency, community ties) that make exit costly enough to produce paralysis even when moral clarity is complete. The two constraints have different ε values because they measure different observables: brilliance trap measures the career incentive structure (ε ≈ 0.52), moral clarity paralysis measures the behavioral delta between understanding and action (ε = 0.58). They are linked but distinct structural phenomena.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
