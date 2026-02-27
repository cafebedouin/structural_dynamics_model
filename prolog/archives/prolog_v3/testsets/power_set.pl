% ============================================================================
% CONSTRAINT STORY: power_set_axiomatic_extraction
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-21
% ============================================================================

:- module(constraint_power_set_axiomatic_extraction, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: power_set_axiomatic_extraction
 *   human_readable: Axiomatic Set Theory's Power Set Axiom
 *   domain: mathematical
 *
 * SUMMARY:
 *   The Power Set axiom in axiomatic set theory (specifically ZFC) asserts that for every set, there exists a set containing all its subsets. While foundational for modern mathematics, this axiom introduces immense set cardinalities that are not needed for many applications, creating a level of abstract complexity that functions as a form of extraction (cognitive overhead). The axiom is enforced institutionally through curricula and peer review, suppressing alternative, less complex foundational systems.
 *
 * KEY AGENTS (by structural relationship):
 *   - Mathematics Students: Primary target (powerless/trapped) — bear the cognitive cost of learning the complex machinery without influence over its choice.
 *   - Foundational Theorists: Primary beneficiary (institutional/arbitrage) — benefit from the logical completeness and consistency afforded by the axiom.
 *   - Applied Mathematicians: Secondary target (moderate/constrained) — must work within a framework that is more complex than their problems often require.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(power_set_axiomatic_extraction, 0.41).
domain_priors:suppression_score(power_set_axiomatic_extraction, 0.65).   % Structural property (raw, unscaled). High due to ZFC's institutional dominance.
domain_priors:theater_ratio(power_set_axiomatic_extraction, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(power_set_axiomatic_extraction, extractiveness, 0.41).
narrative_ontology:constraint_metric(power_set_axiomatic_extraction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(power_set_axiomatic_extraction, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(power_set_axiomatic_extraction, tangled_rope).
narrative_ontology:human_readable(power_set_axiomatic_extraction, "Axiomatic Set Theory's Power Set Axiom").
narrative_ontology:topic_domain(power_set_axiomatic_extraction, "mathematical").

% --- Binary flags ---
domain_priors:requires_active_enforcement(power_set_axiomatic_extraction). % Enforced via curricula, peer review, textbook standards.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(power_set_axiomatic_extraction, foundational_theorists).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(power_set_axiomatic_extraction, mathematics_students).
narrative_ontology:constraint_victim(power_set_axiomatic_extraction, applied_mathematicians).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)
%   Scaffold:     beneficiary + (has_sunset_clause OR no enforcement)
%   Snare:        victim required; beneficiary optional

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (TANGLED ROPE)
% Agent who bears the most extraction. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(power_set_axiomatic_extraction, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Agent who benefits most. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(power_set_axiomatic_extraction, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. The combination of a genuine coordination function
% and significant, asymmetrically borne cognitive cost leads to a Tangled Rope classification.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(power_set_axiomatic_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE APPLIED MATHEMATICIAN (TANGLED ROPE)
% Experiences the cognitive overhead without needing the full foundational power.
constraint_indexing:constraint_classification(power_set_axiomatic_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(power_set_axiomatic_extraction_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(power_set_axiomatic_extraction, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(power_set_axiomatic_extraction, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    narrative_ontology:constraint_metric(power_set_axiomatic_extraction, extractiveness, E),
    narrative_ontology:constraint_metric(power_set_axiomatic_extraction, suppression_requirement, S),
    narrative_ontology:constraint_claim(power_set_axiomatic_extraction, tangled_rope),
    E >= 0.30,
    S >= 0.40.

:- end_tests(power_set_axiomatic_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.41): Represents the significant cognitive overhead and complexity introduced by the Power Set axiom, which forces mathematicians to work with vastly larger and more abstract universes of sets than are often necessary for specific problems. This is a form of non-financial extraction.
 *   - Suppression Score (0.65): This high score reflects the institutional dominance of ZFC set theory. Alternative axiomatic systems (e.g., constructive set theory) exist but are highly suppressed within mainstream mathematics through curricula, journal standards, and peer review. It is very difficult to operate outside this framework.
 *   - Active Enforcement: The constraint is actively enforced through the institutional mechanisms of mathematics education and professional practice.
 *
 * PERSPECTIVAL GAP:
 *   - Powerless Target (Mathematics Student): Experiences the axiom as a Tangled Rope. It is a necessary tool for coordination (understanding modern math) but imposes a high, non-negotiable learning curve (extraction) and is enforced by the curriculum (trapped exit).
 *   - Institutional Beneficiary (Foundational Theorist): Views it as a pure Rope. The axiom is a perfect coordination device that ensures the logical completeness and consistency of the set-theoretic framework, which is their primary concern. The extractive cost is negligible from this perspective.
 *   - Analytical Observer: Sees both the coordination function and the asymmetric extraction, classifying it as a Tangled Rope. The high suppression and active enforcement are key indicators that this is not a simple coordination problem.
 *
 * DIRECTIONALITY LOGIC:
 *   Foundational theorists are the primary beneficiaries, as the axiom provides the powerful foundation they need. Mathematics students and applied mathematicians are the primary victims, bearing the cost of navigating the immense complexity it generates, often without direct benefit to their work.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies that even a purely abstract, logical rule can function as a Tangled Rope. A naive analysis might see it as a pure Rope (a standard) or a Mountain (an unchangeable truth). The Deferential Realism framework, by focusing on the asymmetric distribution of cognitive costs and institutional enforcement, reveals the extractive component hidden within the coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_power_set,
    'Is the logical completeness afforded by the power set axiom worth the cognitive and computational complexity it introduces for mainstream mathematics?',
    'A comparative analysis of mathematical progress and pedagogical efficiency in fields using ZFC vs. those using alternative foundations (e.g., constructive mathematics).',
    'If True: The axiom is a necessary and efficient Rope. If False: It is a Tangled Rope whose extractive costs may outweigh its coordination benefits, suggesting alternatives should be less suppressed.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(power_set_axiomatic_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. While not strictly required as ε < 0.46,
% it models the axiom's solidification as the dominant paradigm.
%
% Theater ratio over time (stable and low):
narrative_ontology:measurement(power_set_axiomatic_extraction_tr_t0, power_set_axiomatic_extraction, theater_ratio, 0, 0.05).
narrative_ontology:measurement(power_set_axiomatic_extraction_tr_t5, power_set_axiomatic_extraction, theater_ratio, 5, 0.10).
narrative_ontology:measurement(power_set_axiomatic_extraction_tr_t10, power_set_axiomatic_extraction, theater_ratio, 10, 0.10).

% Extraction over time (increasing as more of mathematics becomes reliant on it):
narrative_ontology:measurement(power_set_axiomatic_extraction_ex_t0, power_set_axiomatic_extraction, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(power_set_axiomatic_extraction_ex_t5, power_set_axiomatic_extraction, base_extractiveness, 5, 0.38).
narrative_ontology:measurement(power_set_axiomatic_extraction_ex_t10, power_set_axiomatic_extraction, base_extractiveness, 10, 0.41).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(power_set_axiomatic_extraction, information_standard).

% Network relationships (structural influence edges)
narrative_ontology:affects_constraint(power_set_axiomatic_extraction, axiom_of_choice).
narrative_ontology:affects_constraint(axiom_of_foundation, power_set_axiomatic_extraction).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the dynamics.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */