% ============================================================================
% CONSTRAINT STORY: abstraction_boundary_overrun
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_abstraction_boundary_overrun, []).

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
 *   constraint_id: abstraction_boundary_overrun
 *   human_readable: The Leaky Black-Box Collapse
 *   domain: technological/computational
 *
 * SUMMARY:
 *   In software engineering, an abstraction is a simplified interface to a
 *   complex system. The 'Leaky Black-Box Collapse' occurs when this
 *   simplification fails, forcing users (developers) to grapple with the
 *   underlying complexity the abstraction was meant to hide. This creates a
 *   structural conflict: the abstraction provides a real coordination benefit
 *   (faster development initially) but imposes a severe extractive cost when
 *   it fails, a cost borne entirely by those who depend on it.
 *
 * KEY AGENTS:
 *   - Downstream Developers: Primary victims (powerless/trapped) — build on the abstraction and are forced to pay the 'leak tax' in time and effort.
 *   - Platform Maintainers: Primary beneficiaries (institutional/arbitrage) — create and control the abstraction; their expert knowledge becomes a source of power and revenue.
 *   - New Adopters: Prospective users (moderate/mobile) — see only the initial coordination benefit, viewing the tool as a temporary scaffold.
 *   - Legacy System Maintainers: Inertial users (organized/constrained) — experience the abstraction as a degraded set of rules to be worked around.
 *   - Theoretical Purists: Analytical observers (analytical/analytical) — frame the issue as an unavoidable 'natural law' of computing.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(abstraction_boundary_overrun, 0.55).
domain_priors:suppression_score(abstraction_boundary_overrun, 0.75).
domain_priors:theater_ratio(abstraction_boundary_overrun, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(abstraction_boundary_overrun, extractiveness, 0.55).
narrative_ontology:constraint_metric(abstraction_boundary_overrun, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(abstraction_boundary_overrun, theater_ratio, 0.65).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(abstraction_boundary_overrun, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(abstraction_boundary_overrun, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(abstraction_boundary_overrun, tangled_rope).
narrative_ontology:human_readable(abstraction_boundary_overrun, "The Leaky Black-Box Collapse").
narrative_ontology:topic_domain(abstraction_boundary_overrun, "technological/computational").

domain_priors:requires_active_enforcement(abstraction_boundary_overrun).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(abstraction_boundary_overrun, platform_maintainers).
narrative_ontology:constraint_beneficiary(abstraction_boundary_overrun, early_adopters).
narrative_ontology:constraint_victim(abstraction_boundary_overrun, downstream_developers).
narrative_ontology:constraint_victim(abstraction_boundary_overrun, end_users).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNSTREAM DEVELOPER (SNARE) — Trapped by dependency on the abstraction. The cost of migrating the entire system is prohibitive. Leaks are a constant, coercive tax on time and cognitive load, extracting productivity. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PLATFORM MAINTAINER (ROPE) — Experiences the abstraction as a pure coordination good. Leaks are seen as bugs or advanced features, not structural flaws. Their specialized knowledge becomes a source of power and consulting revenue, a net benefit. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.08.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — Recognizes both the genuine coordination function (simplifying development) and the asymmetric extraction (the hidden costs of leaks borne by users). This is the canonical view of the constraint. d≈0.72, f(d)≈1.15, σ=1.0 → χ≈0.63.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: NEW ADOPTER (SCAFFOLD) — Sees the abstraction as a temporary tool to accelerate initial development. At this stage, exit is cheap and the leaks haven't appeared. The abstraction provides support with an implicit sunset clause: 'I'll use it until it becomes a problem'. d≈0.85, f(d)≈1.15, σ=0.8 → χ≈0.51. Note: χ is high, but the 'scaffold' classification is perspectival, based on perceived temporariness.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, scaffold,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: LEGACY MAINTAINER (PITON) — The original coordination benefit has atrophied; the system is now maintained through inertia. The abstraction's promise of simplicity is pure theater compared to the daily reality of working around its quirks. The rules remain because removing them is too costly. theater_ratio=0.65 is high, driving this perspectival classification.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THEORETICAL PURIST (MOUNTAIN) — Argues from the 'Law of Leaky Abstractions' that all non-trivial abstractions are inherently leaky. This perspective naturalizes the constraint as an unavoidable law of computing. The engine will flag this as a false summit, as the base properties (ε=0.55, suppression=0.75) fail the mountain gates.
constraint_indexing:constraint_classification(abstraction_boundary_overrun, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(abstraction_boundary_overrun_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(abstraction_boundary_overrun, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(abstraction_boundary_overrun, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(abstraction_boundary_overrun, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(abstraction_boundary_overrun, TR),
    TR >= 0.70.

:- end_tests(abstraction_boundary_overrun_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Represents the significant developer time and cognitive overhead lost to debugging the abstraction's internal workings. This is a direct extraction of productivity from the victims. Suppression (0.75): High, reflecting the prohibitive cost and risk of migrating a complex system off a foundational technology. This locks users in and prevents them from switching to less-leaky alternatives. Theater Ratio (0.65): High. There is a large gap between the marketed simplicity of the abstraction ('just use this one simple command!') and the complex reality of its failure modes.
 *
 * PERSPECTIVAL GAP:
 *   The constraint is a powerful diagnostic because its classification depends entirely on the observer's position. For the maintainer who profits from their expertise, it's a Rope. For the developer trapped by it, it's a Snare. For the analyst, it's a Tangled Rope. For the newcomer, it's a Scaffold. For the maintainer of a 10-year-old system, it's a Piton. For the theorist, it's a Mountain. The same base metrics produce all six classifications, revealing that the 'type' is not inherent to the object but to the relationship between the object and the observer.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (platform_maintainers) have arbitrage exit and benefit from the information asymmetry, leading to a low/negative derived directionality (d). Victims (downstream_developers) are trapped and bear the full cost of the leaks, leading to a very high derived d. This structural difference in power and exit options drives the massive perspectival gap between seeing the constraint as a Rope versus a Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves the mandatrophy by demonstrating that a single phenomenon can be correctly classified in multiple ways. The error is to insist on a single 'true' classification. The DR framework shows that the Rope, Snare, and Tangled Rope perspectives are all valid structural readings from different positions. The system's value is in mapping this presheaf of perspectives, not in collapsing it to one privileged view. The 'Mountain' perspective is revealed as a false summit, an attempt to naturalize a contingent technological arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_vs_contingent_leakiness,
    'Is the degree of leakiness an inherent, unavoidable property of this type of abstraction (Mountain), or a contingent result of specific design choices and under-investment (Tangled Rope)?',
    'Comparative analysis with alternative, less-leaky abstractions in the same problem domain. Formal verification of the abstraction boundary.',
    'If inherent, the constraint is a Mountain and efforts should focus on mitigation. If contingent, it is a Tangled Rope or Snare that could be fixed or replaced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_vs_contingent_leakiness, conceptual, 'Distinguishing between inherent leakiness (Mountain) and contingent design flaws (Tangled Rope).').

omega_variable(
    intentionality_of_complexity,
    'Is the complexity and leakiness a result of unintentional scope creep, or is it implicitly or explicitly maintained to create vendor lock-in and a market for expert consulting?',
    'Analysis of platform maintainers'' business models, support contracts, and responses to community-submitted simplification patches.',
    'If unintentional, it''s a classic Tangled Rope. If intentional, it''s a Snare from more perspectives, as the extraction is deliberate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(intentionality_of_complexity, empirical, 'Determining if the leakiness is a bug (Tangled Rope) or a feature (Snare).').

omega_variable(
    cost_quantification_for_exit,
    'Can the cumulative cost of developer time spent debugging leaks be reliably quantified against the projected cost of migrating to a new platform?',
    'Analysis of engineering time-tracking data, bug report frequencies, and developer surveys, compared with cost models for large-scale software re-platforming.',
    'If the cost of leaks is demonstrably higher than migration, the ''trapped'' exit condition weakens, potentially shifting the classification from Snare to Tangled Rope for developers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_quantification_for_exit, empirical, 'Quantifying the extraction cost to evaluate the viability of exit.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(abstraction_boundary_overrun, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(abst_tr_t2005, abstraction_boundary_overrun, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(abst_tr_t2015, abstraction_boundary_overrun, theater_ratio, 2015, 0.5).
narrative_ontology:measurement(abst_tr_t2025, abstraction_boundary_overrun, theater_ratio, 2025, 0.65).

% Extraction over time
narrative_ontology:measurement(abst_be_t2005, abstraction_boundary_overrun, base_extractiveness, 2005, 0.15).
narrative_ontology:measurement(abst_be_t2015, abstraction_boundary_overrun, base_extractiveness, 2015, 0.4).
narrative_ontology:measurement(abst_be_t2025, abstraction_boundary_overrun, base_extractiveness, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(abstraction_boundary_overrun, information_standard).
narrative_ontology:affects_constraint(abstraction_boundary_overrun, technical_debt_interest_rate).
narrative_ontology:affects_constraint(abstraction_boundary_overrun, developer_burnout_cycle).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
