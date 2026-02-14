% ============================================================================
% CONSTRAINT STORY: constraint_heine_borel
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_heine_borel, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: heine_borel
 *   human_readable: Heine-Borel Theorem
 *   domain: mathematical
 *
 * SUMMARY:
 *   The Heine-Borel theorem states that for subsets of Euclidean space R^n,
 *   a set is closed and bounded if and only if it is compact. This constraint
 *   represents the inherent limitation imposed by this logical truth in
 *   mathematical analysis and topology. Its "extraction" is the cognitive
 *   cost of adhering to the theorem's logic, which forecloses certain lines
 *   of reasoning while enabling others. It is a classic example of a Mountain
 *   constraint: an unchangeable feature of a logical system.
 *
 * KEY AGENTS (by structural relationship):
 *   - Student of Analysis (powerless/trapped): Encounters the theorem as an
 *     unbreakable rule that must be learned and applied.
 *   - Mathematical Researcher (moderate/constrained): Must operate within the
 *     logical confines established by the theorem.
 *   - Textbook Author (institutional/analytical): Uses the theorem as a
 *     foundational organizing principle for teaching.
 *   - Analytical Observer (analytical/analytical): Observes the theorem as a
 *     structurally rigid and invariant feature of mathematics.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(heine_borel, 0.15).
domain_priors:suppression_score(heine_borel, 0.05).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(heine_borel, 0.01).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(heine_borel, extractiveness, 0.15).
narrative_ontology:constraint_metric(heine_borel, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(heine_borel, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Without these, the NL signature defaults to 0.5
% and fails certification.
narrative_ontology:constraint_metric(heine_borel, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(heine_borel, resistance, 0.02).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(heine_borel, mountain).

% --- Binary flags ---
% narrative_ontology:has_sunset_clause(heine_borel).
% domain_priors:requires_active_enforcement(heine_borel).

% --- Emergence flag (required for mountain constraints) ---
% Required for the mountain metric gate: without this, the classify_from_metrics
% mountain clause will not fire.
domain_priors:emerges_naturally(heine_borel).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% As a Mountain constraint (natural law), there are no structural
% beneficiaries or victims. The theorem is a feature of the logical landscape
% that all agents must navigate. No enrichment needed.

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

% UNIFORM-TYPE: This is a natural law constraint (mountain-only). The
% classification is the same from all perspectives. Multiple perspectives are
% included to demonstrate this invariance.

% PERSPECTIVE 1: THE STUDENT (POWERLESS)
% A student learning analysis encounters the theorem as an immutable fact.
constraint_indexing:constraint_classification(heine_borel, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 2: THE RESEARCHER (MODERATE)
% A working mathematician whose research is bounded by the theorem's logic.
constraint_indexing:constraint_classification(heine_borel, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(universal))).

% PERSPECTIVE 3: THE EDUCATOR (INSTITUTIONAL)
% A textbook author or professor who uses the theorem as a pedagogical tool.
constraint_indexing:constraint_classification(heine_borel, mountain,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The default analytical context, which sees the theorem as a structural constant.
constraint_indexing:constraint_classification(heine_borel, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(heine_borel_tests).

test(perspectival_invariance_mountain) :-
    % Verify perspectival agreement (Mountain) across all defined perspectives.
    constraint_indexing:constraint_classification(heine_borel, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(heine_borel, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(heine_borel, TypeAnalytical, context(agent_power(analytical), _, _, _)),
    TypePowerless == mountain,
    TypeInstitutional == mountain,
    TypeAnalytical == mountain.

test(threshold_validation_mountain) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(heine_borel, ExtMetricName, E),
    E =< 0.25, % Mountain threshold
    narrative_ontology:constraint_metric(heine_borel, suppression_requirement, S),
    S =< 0.05. % Mountain threshold

:- end_tests(heine_borel_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Heine-Borel theorem is a fundamental result in mathematical analysis,
 *   a provable truth within its axiomatic system. The scores reflect this:
 *   base extractiveness (0.15) represents the cognitive overhead and the
 *   foreclosure of certain invalid proof strategies, not a transfer of value.
 *   Suppression (0.05) is minimal because one is free to work in other
 *   mathematical systems (e.g., general topological spaces where the theorem
 *   doesn't hold), but within Euclidean space, there is no alternative.
 *   The NL Profile metrics (high collapse, low resistance) and the
 *   `emerges_naturally` flag certify it as a Mountain.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a mathematical truth, the theorem's
 *   status as a structural limit is invariant across all perspectives, from
 *   a student first learning it to a researcher applying it. This uniformity
 *   is the hallmark of a Mountain constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain constraint, there are no structural beneficiaries or victims.
 *   The concepts do not apply. The theorem does not exist to benefit one group
 *   at the expense of another; it is a feature of the logical system itself.
 *   The directionality `d` will default to canonical values for each power
 *   level, but the extremely low base extractiveness ensures that the effective
 *   extraction `χ` remains negligible, leading to a Mountain classification
 *   regardless.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification as a Mountain is crucial. It prevents misinterpreting a
 *   fundamental logical limit as a socially constructed Snare or Rope. The
 *   framework correctly identifies that the "constraint" is not imposed by any
 *   human actor for their benefit but emerges from the axioms of the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_heine_borel,
    'Is the Heine-Borel theorem a discovered feature of a Platonic reality or a constructed consequence of chosen axioms (formalism)?',
    'This is a fundamental, unresolved debate in the philosophy of mathematics. No empirical data can resolve it.',
    'If Platonic, the theorem is a true Mountain, a feature of reality itself. If formalist, it is a Mountain-like feature of a specific, widely adopted game (Euclidean analysis), making it a contingent but locally absolute constraint.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(heine_borel, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not required for low-extraction constraints (ε <= 0.46).
% As a mathematical theorem, its properties are static and do not drift.
% The values are provided for completeness, showing a flat trajectory.
narrative_ontology:measurement(heine_borel_tr_t0, heine_borel, theater_ratio, 0, 0.01).
narrative_ontology:measurement(heine_borel_tr_t5, heine_borel, theater_ratio, 5, 0.01).
narrative_ontology:measurement(heine_borel_tr_t10, heine_borel, theater_ratio, 10, 0.01).

narrative_ontology:measurement(heine_borel_ex_t0, heine_borel, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(heine_borel_ex_t5, heine_borel, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(heine_borel_ex_t10, heine_borel, base_extractiveness, 10, 0.15).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type and Boltzmann floor are not applicable to a mathematical
% theorem, which does not coordinate agent behavior in the typical sense.
%
% narrative_ontology:coordination_type(heine_borel, information_standard).
% narrative_ontology:boltzmann_floor_override(heine_borel, 0.10).

% Network relationships: The theorem is a consequence of axioms of real analysis.
narrative_ontology:affects_constraint(axioms_of_real_analysis, heine_borel).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed. The constraint is a Mountain, and its classification
% is insensitive to directionality due to its extremely low base extractiveness.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */