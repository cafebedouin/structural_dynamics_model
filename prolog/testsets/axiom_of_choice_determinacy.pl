% ============================================================================
% CONSTRAINT STORY: axiom_of_choice
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_axiom_of_choice, []).

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
 *   constraint_id: axiom_of_choice
 *   human_readable: The Axiom of Choice (AC)
 *   domain: mathematical/logical
 *
 * SUMMARY:
 *   The Axiom of Choice (AC) states that for any collection of non-empty sets,
 *   there exists a "choice function" that selects one element from each set.
 *   It is a fundamental, non-constructive principle in ZFC set theory. While
 *   it enables powerful results in analysis and topology (e.g., the
 *   Well-Ordering Theorem), it also implies counter-intuitive consequences
 *   like the Banach-Tarski paradox. This creates a significant perspectival
 *   gap between those who use it as a tool and those who see its results as
 *   a violation of physical intuition.
 *
 * KEY AGENTS (by structural relationship):
 *   - Mathematical Constructivists: Primary target (moderate/constrained) — AC
 *     extracts the requirement for explicit construction, forcing acceptance
 *     of non-constructive proofs and their "pathological" results.
 *   - Functional Analysts: Primary beneficiary (institutional/mobile) — AC
 *     is a pure coordination tool (Rope) that guarantees the existence of
 *     essential mathematical objects (e.g., basis vectors, linear functionals).
 *   - Set Elements: A conceptual agent (powerless/trapped) for whom selection
 *     by a choice function is an unchangeable law of the ZFC universe.
 *   - Analytical Observer: Sees the full structure as a Mountain of formal
 *     logic, whose consequences are interpreted differently by various schools.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: 0.25. AC is on the threshold of being a Mountain. It "extracts"
% the requirement for specific constructive rules, replacing them with a
% guarantee of existence. This is a form of cognitive/philosophical extraction.
domain_priors:base_extractiveness(axiom_of_choice, 0.25).

% Rationale: 0.05. Within the ZFC framework, AC is a foundational axiom and
% does not suppress alternatives; it defines the system. The low score
% reflects its status as a "law of nature" within this formal system, which
% is the defining feature of a Mountain. The suppression of alternative
% mathematical systems (like ZF+AD) is a sociological constraint, not an
% intrinsic property of AC itself. This resolves the MOUNTAIN_METRIC_CONFLICT.
domain_priors:suppression_score(axiom_of_choice, 0.05).

% Rationale: 0.0. The axiom has no theatrical component; it is a pure
% functional declaration within a formal system.
domain_priors:theater_ratio(axiom_of_choice, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(axiom_of_choice, extractiveness, 0.25).
narrative_ontology:constraint_metric(axiom_of_choice, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(axiom_of_choice, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl.
narrative_ontology:constraint_metric(axiom_of_choice, accessibility_collapse, 1.0).
narrative_ontology:constraint_metric(axiom_of_choice, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(axiom_of_choice, mountain).
narrative_ontology:human_readable(axiom_of_choice, "The Axiom of Choice (AC)").
narrative_ontology:topic_domain(axiom_of_choice, "mathematical/logical").

% --- Binary flags ---
% No active enforcement is required; it is a logical axiom.

% --- Emergence flag (required for mountain constraints) ---
% The axiom emerges from the logical structure of the ZFC system without
% human design or enforcement. Required for the mountain metric gate.
domain_priors:emerges_naturally(axiom_of_choice).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% While the analytical claim is Mountain, the perspectival gaps are driven by
% these relationships. They are included to enable the Rope/Snare classifications.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(axiom_of_choice, functional_analysts).
narrative_ontology:constraint_beneficiary(axiom_of_choice, transfinite_topologists).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(axiom_of_choice, mathematical_constructivists).
narrative_ontology:constraint_victim(axiom_of_choice, physical_intuitionists).

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

% PERSPECTIVE 1: THE SET ELEMENT (MOUNTAIN)
% For an element within a set, the possibility of its selection by a choice
% function is an unchangeable law of the ZFC universe. It has no agency.
constraint_indexing:constraint_classification(axiom_of_choice, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE FUNCTIONAL ANALYST (ROPE)
% For the analyst, AC is a pure coordination tool. It guarantees the existence
% of objects needed to build theories (e.g., Hahn-Banach theorem), with
% effectively zero extraction from their perspective.
constraint_indexing:constraint_classification(axiom_of_choice, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE CONSTRUCTIVIST / INTUITIVE GEOMETER (SNARE)
% For those who require explicit construction or physical intuition, AC is a
% Snare. It extracts the "sanctity of construction" and forces acceptance of
% "monstrous" results like the Banach-Tarski paradox. The exit option
% (rejecting ZFC) is professionally costly.
constraint_indexing:constraint_classification(axiom_of_choice, snare,
    context(agent_power(moderate),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The analytical observer sees AC as a foundational axiom within a formal
% system (ZFC). Its properties (ε=0.25, S=0.05) and NL profile metrics make
% it a Mountain of logic. The Rope/Snare classifications are interpretations
% of its consequences, not properties of the axiom itself.
constraint_indexing:constraint_classification(axiom_of_choice, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(axiom_of_choice_tests).

test(perspectival_gap_analyst_vs_constructivist) :-
    % Verify the analyst (beneficiary) sees a Rope, while the constructivist (victim) sees a Snare.
    constraint_indexing:constraint_classification(axiom_of_choice, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(axiom_of_choice, snare, context(agent_power(moderate), _, _, _)).

test(analytical_view_is_mountain) :-
    % The formal, analytical view must classify as Mountain.
    constraint_indexing:constraint_classification(axiom_of_choice, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_thresholds_adherence) :-
    % Verify the base metrics are consistent with the Mountain claim.
    narrative_ontology:constraint_metric(axiom_of_choice, extractiveness, E),
    narrative_ontology:constraint_metric(axiom_of_choice, suppression_requirement, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(axiom_of_choice_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.25): Set at the maximum for a Mountain. AC does
 *     not extract resources, but it extracts a philosophical price: the
 *     requirement to accept non-constructive existence proofs and their
 *     counter-intuitive consequences.
 *   - Suppression Score (S=0.05): Within the ZFC axiomatic system, AC is a
 *     foundational law; it does not "suppress" anything, it *defines* the
 *     space. Its low suppression score reflects this status as a "law of
 *     nature" for that formal system.
 *   - NL Profile (AC=1.0, R=0.05, emerges_naturally): These metrics certify the
 *     Mountain classification. Within ZFC, AC is absolute and forecloses
 *     alternatives (accessibility_collapse=1.0). Resistance is a philosophical
 *     debate external to the system, not an internal opposition (resistance=0.05).
 *     The axiom emerges from the logic of the system, not human enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the institutional analyst, AC is a foundational
 *   Rope that makes their field possible. For the constructivist, it is a
 *   Snare that violates core principles of mathematical truth by divorcing
 *   existence from construction. The analytical view resolves this by
 *   classifying the axiom itself as a Mountain of formal logic, whose
 *   *consequences* are then interpreted as Rope or Snare depending on the
 *   observer's philosophical commitments.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `functional_analysts`, `transfinite_topologists`. These
 *     groups rely on theorems that require AC. Their work is enabled by it.
 *   - Victims: `mathematical_constructivists`, `physical_intuitionists`. These
 *     groups find their foundational principles (constructibility, conservation
 *     of volume) violated by the consequences of AC.
 *
 * MANDATROPHY ANALYSIS:
 *   This story demonstrates how a single formal object can be a Mountain
 *   analytically, but generate intense perspectival disagreement. The framework
 *   avoids mislabeling it as a pure Snare by recognizing its foundational,
 *   low-suppression role within its native system (ZFC), while still capturing
 *   the valid experience of extraction from the constructivist's perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ac_physicality,
    'Is the Axiom of Choice a Mountain reflecting a deep truth about reality (e.g., quantum indeterminacy), or merely a formalist Snare of convenience with no physical counterpart?',
    'Empirical evidence from quantum foundations or cosmology demonstrating a physical process that requires an infinite, non-constructive choice.',
    'If true, AC is a genuine Mountain of physics. If false, it remains a Mountain of logic but its application to physics is a Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(axiom_of_choice, 1904, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required, as base_extractiveness (0.25) is not > 0.46.
% The properties of this mathematical axiom are considered stable over the interval.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No coordination type, as the analytical claim is Mountain.
% No network relationships declared in this file.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the perspectives.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */