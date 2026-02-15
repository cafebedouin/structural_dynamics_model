% ============================================================================
% CONSTRAINT STORY: currys_paradox
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_currys_paradox, []).

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
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: currys_paradox
 *   human_readable: Curry's Paradox
 *   domain: analytical/logic
 *
 * SUMMARY:
 *   Curry's Paradox is a logical result that proves any arbitrary claim (e.g.,
 *   "The moon is made of cheese") using a self-referential sentence of the form
 *   "If this sentence is true, then X is true." Unlike the Liar Paradox, it
 *   does not require negation, making it a more fundamental threat to naive
 *   set theory and logics with unrestricted comprehension. It acts as a
 *   structural "No-Go" theorem, forcing formal systems to either limit
 *   self-reference or restrict rules of implication to avoid "explosion,"
 *   where the system loses all meaning because it permits every possible
 *   statement to be true.
 *
 * KEY AGENTS (by structural relationship):
 *   - Naive Set Theorists: Primary target (powerless/trapped) — their formal systems are rendered trivial (meaningless) by the paradox.
 *   - Paraconsistent Logicians: Primary beneficiary (analytical/arbitrage) — the paradox provides the justification for their work on non-explosive logics.
 *   - System Architects (e.g., language designers): Secondary beneficiary (organized/mobile) — use the paradox as a coordination point for designing safe type systems.
 *   - Analytical Observer: Sees the full structure as a fundamental limit of formal systems.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(currys_paradox, 0.20).
domain_priors:suppression_score(currys_paradox, 0.05).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(currys_paradox, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(currys_paradox, extractiveness, 0.20).
narrative_ontology:constraint_metric(currys_paradox, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(currys_paradox, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
narrative_ontology:constraint_metric(currys_paradox, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(currys_paradox, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(currys_paradox, mountain).

% --- Emergence flag (required for mountain constraints) ---
% Emerges naturally from the interaction of implication, self-reference, and contraction.
domain_priors:emerges_naturally(currys_paradox).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Although this is a mountain, declaring these helps explain the non-mountain
% perspectives and provides context for directionality.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(currys_paradox, paraconsistent_logicians).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(currys_paradox, naive_set_theorists).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE NAIVE THEORIST (SNARE/MOUNTAIN)
% Agent whose system is trivialized by the paradox. They are trapped by the
% logical consequence.
constraint_indexing:constraint_classification(currys_paradox, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE SYSTEM ARCHITECT (ROPE)
% A programmer or language designer sees the paradox as a known hazard to
% coordinate around, leading to safer type systems and compilers.
constraint_indexing:constraint_classification(currys_paradox, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE INSTITUTIONAL MATHEMATICIAN (MOUNTAIN)
% For the mathematical establishment, the paradox is an unchangeable feature
% of the logical landscape. One does not "fix" it; one builds systems (like ZFC)
% to avoid it.
constraint_indexing:constraint_classification(currys_paradox, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The default analytical context, which sees the paradox as a fundamental,
% unchangeable limit on formal systems.
constraint_indexing:constraint_classification(currys_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(currys_paradox_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between architect and theorist.
    constraint_indexing:constraint_classification(currys_paradox, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(currys_paradox, TypeBeneficiary, context(agent_power(moderate), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(analytical_view_is_mountain) :-
    constraint_indexing:constraint_classification(currys_paradox, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_metrics_are_consistent) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(currys_paradox, ExtMetricName, E),
    narrative_ontology:constraint_metric(currys_paradox, SuppMetricName, S),
    config:param(mountain_extractiveness_max, EMax),
    config:param(mountain_suppression_ceiling, SMax),
    E =< EMax,
    S =< SMax.

:- end_tests(currys_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.20): The paradox doesn't extract labor or capital,
 *     but it extracts "meaning" or "information utility" from a formal system
 *     by rendering it trivial (capable of proving anything). The cost is the
 *     total collapse of the system's value.
 *   - Suppression Score (0.05): The paradox *itself* does not suppress
 *     alternatives; it is a feature of logic. The low score reflects that it
 *     is an inert logical fact. The *social response* to the paradox (e.g.,
 *     the dominance of ZFC set theory) is a separate constraint that has a
 *     much higher suppression score. This file models only the paradox itself.
 *   - NL Profile: Accessibility collapse is high (0.95) because avoiding the
 *     paradox requires significant, non-obvious restrictions on logic.
 *     Resistance is very low (0.05) because one cannot "resist" a proof.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between those who see the paradox as an immutable law (Mountain)
 *   and those who see it as a known hazard to coordinate around (Rope). For a
 *   system architect, it's a design constraint that informs better languages.
 *   For a logician, it's a fundamental boundary of their entire field. The
 *   "Snare" view from the original file is better modeled as a separate
 *   constraint about the social enforcement of specific logical foundations,
 *   not a property of the paradox itself.
 *
 * DIRECTIONALITY LOGIC:
 *   - Victims: Naive set theorists or designers of logics with unrestricted
 *     comprehension, whose systems are destroyed by the paradox.
 *   - Beneficiaries: Paraconsistent and substructural logicians, whose fields
 *     are motivated by finding ways to defuse logical explosion.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Mountain from the analytical perspective prevents
 *   mislabeling a fundamental logical limit as a socially constructed Snare.
 *   It correctly separates the logical fact from the social conventions built
 *   in response to it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_currys_paradox,
    "Can 'Relevance Logic' (which rejects contraction) be a stable foundation for human-AI coordination?",
    "Development and large-scale deployment of a relevance-based theorem prover or programming language.",
    "If Yes: Curry's Paradox moves from Mountain to Rope (an avoidable hazard). If No: It remains a Mountain.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_currys_paradox, empirical, "Viability of relevance logic as a practical foundation for computation.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(currys_paradox, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% No temporal data required as base_extractiveness (0.20) is below the 0.46 threshold.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This is a fundamental logical constraint, not a coordination mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed; the structural derivation from beneficiary/victim
% status and exit options is sufficient.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */