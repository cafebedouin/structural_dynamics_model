% ============================================================================
% CONSTRAINT STORY: stable_marriage_coordination
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stable_marriage_coordination, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: stable_marriage_coordination
 *   human_readable: Stable Marriage Problem (Gale-Shapley Algorithm)
 *   domain: economic/social/algorithmic_coordination
 *
 * SUMMARY:
 *   The Stable Marriage Problem (SMP) represents a pure coordination
 *   constraint with minimal extraction overhead. Given two equally sized sets
 *   of agents (proposers and responders) with ranked preference lists, the
 *   Gale-Shapley algorithm produces a stable matching: one where no pair
 *   would prefer each other to their assigned partners. The constraint
 *   emerges from the mathematical structure of preference aggregation—the
 *   impossibility of simultaneously satisfying all preference orderings
 *   creates an inherent coordination problem. The algorithm solves this
 *   through a decentralized proposal-response mechanism that guarantees
 *   stability without coercive enforcement. The constraint exhibits low
 *   extractiveness (ε=0.18) because: (1) participation is voluntary and
 *   mobile, (2) the algorithm produces outcomes strictly preferable to no
 *   matching, (3) no agent can unilaterally improve their outcome by exiting,
 *   and (4) the mechanism generates no surplus for capture. Suppression is
 *   minimal (0.12) because alternative preference expressions are costless
 *   and algorithm participation is uncoerced. Theater ratio is low (0.25)
 *   because the mechanism's function is transparent: the algorithm
 *   demonstrably terminates in a stable state; the performative component
 *   consists only of initial preference elicitation and outcome announcement,
 *   not of legitimation rituals or opacity.
 *
 * KEY AGENTS:
 *   - Proposers (moderate/mobile): Agents making offers under the algorithm; experience mobile exit options and benefit from the stability guarantee; see pure coordination (Rope)
 *   - Responders (moderate/mobile): Agents receiving and responding to offers; hold options improve across iterations; also experience pure coordination benefit
 *   - Market Ecosystem/Platform (institutional/arbitrage): Matching platforms, medical residency programs (NRMP), school choice systems; benefit from algorithmic legitimacy and participant trust; capture coordination surplus through institutional adoption
 *   - Mathematical Structure (analytical/analytical): The constraint's civilizational foundation; the impossibility of preference aggregation without blocking pairs is a theorem, not a contingent fact
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stable_marriage_coordination, 0.18).
domain_priors:suppression_score(stable_marriage_coordination, 0.12).
domain_priors:theater_ratio(stable_marriage_coordination, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stable_marriage_coordination, extractiveness, 0.18).
narrative_ontology:constraint_metric(stable_marriage_coordination, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(stable_marriage_coordination, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stable_marriage_coordination, rope).
narrative_ontology:human_readable(stable_marriage_coordination, "Stable Marriage Problem (Gale-Shapley Algorithm)").
narrative_ontology:topic_domain(stable_marriage_coordination, "economic/social/algorithmic_coordination").

domain_priors:emerges_naturally(stable_marriage_coordination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stable_marriage_coordination, matching_participants).
narrative_ontology:constraint_beneficiary(stable_marriage_coordination, market_ecosystem).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL PROPOSER (ROPE) — Participant with proposal power experiences the constraint as pure coordination. The algorithm guarantees a stable outcome without coercion: each proposer can engage or withdraw. d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.12. Low effective extraction because exit is mobile and the mechanism is non-coercive.
constraint_indexing:constraint_classification(stable_marriage_coordination, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: INDIVIDUAL RESPONDER (ROPE) — Participant with response power experiences similar coordination benefit. Can always switch to better offers (holding option improves over algorithm iterations). d≈0.50, f(d)≈0.65, σ=1.2 → χ≈0.12. Symmetric coordination: both proposers and responders benefit from stability guarantee without asymmetric extraction.
constraint_indexing:constraint_classification(stable_marriage_coordination, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: MARKET ECOSYSTEM (ROPE) — Institutional implementers (matching platforms, medical residency programs, school choice systems) benefit from algorithm adoption. The stable matching property reduces disputes, increases participation, and builds trust. d≈0.30, f(d)≈0.15, σ=1.2 → χ≈0.03. Minimal extraction — institutions capture coordination surplus through reputation and network effects, not through coercive mechanisms.
constraint_indexing:constraint_classification(stable_marriage_coordination, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (MOUNTAIN) — From the civilizational/universal perspective, the Stable Marriage Problem and its algorithmic solution represent a mathematical necessity: any two-sided matching with inconsistent preferences must either have blocking pairs or accept instability. The Gale-Shapley algorithm's existence and properties are theorems, not contingent institutions. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.21. However, this mountain classification is properly grounded: ε=0.18, suppression=0.12, accessibility_collapse=0.91, resistance=0.08, emerges_naturally=true satisfy all NL gates.
constraint_indexing:constraint_classification(stable_marriage_coordination, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stable_marriage_coordination_tests).
:- end_tests(stable_marriage_coordination_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low-moderate. The algorithm produces a stable matching that is Pareto-superior to random assignment for all participants. However, ε is not zero because: (1) the proposer-optimal stable matching systematically favors proposers over responders in the preference ordering (Roth & Sotomayor), and (2) institutional implementations (NRMP, school choice systems) capture some coordination surplus through legitimacy and network effects. The non-zero value reflects that stability is not costless to achieve—some preference satisfaction is sacrificed. Suppression (0.12): Low. Agents are not coerced to participate, can freely express preferences, and retain mobile exit options throughout. The algorithm does not suppress alternatives; it merely aggregates preferences under a specific stability criterion. Theater ratio (0.25): Low. The mechanism's function is mathematically transparent and empirically verifiable. The performative component is minimal—preference collection, algorithm execution, and outcome announcement. The algorithm output is not legitimated through ritual or opacity; stability is mechanically checkable.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is minimal in this constraint. Both proposers and responders classify the algorithm as Rope with similar χ values (≈0.12) because the coordination benefit is genuinely symmetric in structure, even if the proposer-optimal outcome favors proposers slightly. The institutional perspective (Rope with χ≈0.03) reflects lower effective extraction because institutions experience the constraint as a pure coordination benefit—the algorithm solves their matching problem without requiring them to enforce extraction. The analytical perspective classifies the constraint as Mountain because from the civilizational view, the mathematical structure of the Stable Marriage Problem is invariant: preference inconsistency always produces blocking pairs; the algorithm is the unique solution to a structural impossibility. This is not a perspectival gap (disagreement) but rather a vertical integration—the analytical view transcends the participant perspectives and reveals the underlying necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Proposers and responders: Symmetric beneficiaries with mobile exit → d≈0.50, f(d)≈0.65. Both benefit from stability guarantee equally (in expectation). No asymmetric extraction because neither group is trapped or coerced. Market ecosystem: Beneficiary with arbitrage exit → d≈0.30, f(d)≈0.15. Captures coordination surplus through adoption but does not extract from participants directly—the mechanism is non-coercive and participants voluntarily use it because stability benefits them. Analytical observer: Mathematical necessity → d≈0.72 (canonical), f(d)≈1.15. Not a high-extraction perspective; mountain classification comes from the accessibility_collapse and resistance metrics, indicating that the constraint is mathematically invariant, not observationally contingent.
 *
 * MANDATROPHY ANALYSIS:
 *   The Stable Marriage Problem resolves the mandatrophy through its genuine status as a coordination constraint without disguised extraction. The proposer-optimal stable matching does create a minor asymmetry favoring proposers, but this is acknowledged in game-theoretic analysis (Roth & Sotomayor) and is not hidden behind institutional opacity. The algorithm is truthfully about coordination, not about extracting from responders. The Mountain classification for the analytical observer is properly justified: the constraint reflects a mathematical necessity (preference aggregation impossibility), not a contingent institutional arrangement naturalizing extraction. This constraint exemplifies how real coordination constraints (low ε, low suppression) classify as Rope across participant perspectives and Mountain from the civilizational view—no mandatrophy because there is no hidden extraction to unmask.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    preference_revelation_truthfulness,
    'Do participants truthfully reveal preferences under Gale-Shapley, or do they engage in strategic misrepresentation?',
    'Empirical analysis of preference statements in medical residency matching (NRMP), school choice systems, and online matching platforms; comparison of revealed preferences to ex-post satisfaction and switching behavior',
    'If truthful: algorithm achieves true stability. If strategic: revealed stability is mechanical, not substantive — participants are gaming the mechanism. Constraint type remains Rope, but beneficiary/victim distinction becomes more pronounced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(preference_revelation_truthfulness, empirical, 'Whether participants truthfully reveal preferences or engage in strategic misrepresentation').

omega_variable(
    proposer_advantage_asymmetry,
    'Does the proposer-optimal stable matching create a persistent advantage for proposers that accumulates across repeated market interactions?',
    'Longitudinal study of proposer satisfaction vs responder satisfaction in repeated matching games; analysis of whether proposer-advantaged outcomes persist when market composition changes',
    'If no asymmetry persists: Gale-Shapley is truly symmetric Rope. If asymmetry persists: the algorithm encodes an extraction mechanism favoring proposers, shifting classification toward Tangled Rope for some perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proposer_advantage_asymmetry, empirical, 'Whether proposer advantage creates persistent asymmetric extraction').

omega_variable(
    alternative_stability_concepts,
    'Are there other stability concepts (core stability, Pareto stability, envy-freeness) that would produce different matchings, and if so, why is pairwise stability privileged?',
    'Mathematical analysis of stability concept families; comparative institutional analysis of which concept is adopted in different matching markets (medical residency, school choice, etc.) and the distributional outcomes',
    'If alternative concepts are equally valid: Gale-Shapley is contingent institutional choice, not mathematical necessity. Constraint downgraded from Mountain. If pairwise stability is uniquely justified: Mountain classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_stability_concepts, conceptual, 'Whether pairwise stability is uniquely justified or contingent institutional choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stable_marriage_coordination, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stm_tr_t0, stable_marriage_coordination, theater_ratio, 0, 0.15).
narrative_ontology:measurement(stm_tr_t5, stable_marriage_coordination, theater_ratio, 5, 0.2).
narrative_ontology:measurement(stm_tr_t10, stable_marriage_coordination, theater_ratio, 10, 0.25).

% Extraction over time
narrative_ontology:measurement(stm_be_t0, stable_marriage_coordination, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(stm_be_t5, stable_marriage_coordination, base_extractiveness, 5, 0.14).
narrative_ontology:measurement(stm_be_t10, stable_marriage_coordination, base_extractiveness, 10, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stable_marriage_coordination, resource_allocation).
narrative_ontology:affects_constraint(stable_marriage_coordination, two_sided_matching_stability).
narrative_ontology:affects_constraint(stable_marriage_coordination, preference_aggregation_impossibility).

% DUAL FORMULATION NOTE:
% The Stable Marriage Problem has a mathematical prerequisite (preference aggregation impossibility — every preference inconsistency produces blocking pairs) and institutional implementations (medical residency matching, school choice systems). The mathematical prerequisite is a Mountain; the implementations are Rope. The network relationship captures this: the mathematical necessity creates the coordination problem; the algorithm solves it in practice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
