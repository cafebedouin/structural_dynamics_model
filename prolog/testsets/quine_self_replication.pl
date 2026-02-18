% ============================================================================
% CONSTRAINT STORY: quine_self_replication
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_quine_self_replication, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
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
 *   constraint_id: quine_self_replication
 *   human_readable: Quines (Computational Self-Replication)
 *   domain: technological/mathematical
 *
 * SUMMARY:
 *   A Quine is a non-empty computer program which takes no input and produces a
 *   copy of its own source code as its only output. This demonstrates a
 *   fundamental property of computability derived from Kleene's Second Recursion
 *   Theorem: any Turing-complete system possesses the latent capability for
 *   self-description and replication without external templates. The constraint
 *   is the logical necessity of this fixed-point behavior.
 *
 * KEY AGENTS (by structural relationship):
 *   - The Source Code (Subject): The powerless agent whose structure must
 *     simultaneously encode logic and its own literal representation. (powerless/trapped)
 *   - The Compiler/Interpreter: The institutional rule-enforcing environment
 *     that translates the quine's instructions into output, benefiting from the
 *     demonstration of logical completeness. (institutional/arbitrage)
 *   - Cybersecurity Defenders: Agents who must contend with the consequences of
 *     self-replication in malware, treating the capability as a structural
 *     vulnerability. (moderate/constrained)
 *   - The Computer Scientist (Analytical): The observer mapping the "Mountain"
 *     of Kleene's Recursion Theorem through the "Rope" of specific code.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: The base mechanism of self-reference is not inherently extractive;
% it's a logical property. The extraction (0.2) represents the minimal,
% unavoidable cost of forcing an environment to process a recursive loop that
% uses its own identity as fuel, distinguishing between data and instruction.
domain_priors:base_extractiveness(quine_self_replication, 0.20).
% Rationale: Lowered to 0.05 to be consistent with a Mountain classification.
% Quines do not suppress alternatives; they reveal a mandatory property of
% computation. The impossibility of alternatives (like infinite regress) is
% a feature of logic, not a suppression by the quine itself.
domain_priors:suppression_score(quine_self_replication, 0.05).
% Rationale: A quine is a purely technical construct with no performative aspect.
domain_priors:theater_ratio(quine_self_replication, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quine_self_replication, extractiveness, 0.20).
narrative_ontology:constraint_metric(quine_self_replication, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(quine_self_replication, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain.
% Accessibility Collapse: High (0.95), as the fixed-point theorem is the only
% known way to bypass the infinite regress paradox of self-description.
narrative_ontology:constraint_metric(quine_self_replication, accessibility_collapse, 0.95).
% Resistance: Zero (0.0), as one cannot meaningfully "resist" a mathematical
% theorem within a system where it applies.
narrative_ontology:constraint_metric(quine_self_replication, resistance, 0.0).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(quine_self_replication, tangled_rope).
narrative_ontology:human_readable(quine_self_replication, "Quines (Computational Self-Replication)").
narrative_ontology:topic_domain(quine_self_replication, "technological/mathematical").

% --- Binary flags ---
% The logic requires a compiler/interpreter to execute it.
domain_priors:requires_active_enforcement(quine_self_replication).

% --- Emergence flag (required for mountain constraints) ---
% Emerges naturally from Kleene's Second Recursion Theorem.
domain_priors:emerges_naturally(quine_self_replication).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(quine_self_replication, autonomous_agent_developers).
narrative_ontology:constraint_beneficiary(quine_self_replication, computer_science_educators).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(quine_self_replication, static_code_analysis_tools).
narrative_ontology:constraint_victim(quine_self_replication, cybersecurity_defenders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE EXECUTING QUINE (MOUNTAIN)
% For the quine itself, its self-replication is a natural law of its internal
% logic. It cannot "choose" to output a random string. Its identity is its
% destiny. The low extraction and suppression scores confirm this.
constraint_indexing:constraint_classification(quine_self_replication, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE PEDAGOGICAL INSTRUCTOR (ROPE)
% For an educator, the quine is a "Rope"—a pure coordination tool. It allows
% them to pull students toward an understanding of recursion and self-reference,
% providing a standard of achievement for logical mastery with no extraction.
constraint_indexing:constraint_classification(quine_self_replication, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE CYBERSECURITY DEFENDER (TANGLED ROPE)
% For a defender, the ability for code to self-replicate is a structural
% vulnerability. It has a coordination function (the logic is valid) but also
% an extractive one (it consumes resources and analytic effort to contain).
% It's not a Snare because the base extraction of the mechanism is low; the
% "snare" is the malware payload, a separate constraint.
constraint_indexing:constraint_classification(quine_self_replication, tangled_rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical view sees the full structure: a Mountain of logic that enables
% Rope-like coordination (pedagogy) and also extractive side-effects (malware).
% This hybrid nature is best captured as a Tangled Rope.
constraint_indexing:constraint_classification(quine_self_replication, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quine_self_replication_tests).

test(perspectival_gap) :-
    % Verify the code itself sees a Mountain, while educators see a Rope.
    constraint_indexing:constraint_classification(quine_self_replication, mountain, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(quine_self_replication, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(quine_self_replication, tangled_rope, context(agent_power(moderate), _, _, _)).

test(defender_tangled_rope_insight) :-
    % Demonstrates that for the defender, self-replication is a Tangled Rope.
    constraint_indexing:constraint_classification(quine_self_replication, tangled_rope, context(agent_power(moderate), _, _, _)).

test(emergence_and_nl_profile) :-
    domain_priors:emerges_naturally(quine_self_replication),
    narrative_ontology:constraint_metric(quine_self_replication, accessibility_collapse, AC), AC >= 0.85,
    narrative_ontology:constraint_metric(quine_self_replication, resistance, R), R =< 0.15.

:- end_tests(quine_self_replication_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.20) and suppression (s=0.05) were chosen to
 *   reflect that the Quine mechanism is a fundamental, low-cost property of
 *   logic, consistent with a Mountain classification from the perspective of
 *   the code itself. The perspectival gaps arise from its application: as a
 *   pedagogical tool (Rope) or as a vector for malware (Tangled Rope). The
 *   claim is `tangled_rope` because the analytical view must account for all
 *   its potential functions, both benign and extractive.
 *
 * PERSPECTIVAL GAP:
 *   The gap is significant. The powerless code experiences its nature as an
 *   immutable law (Mountain). The institutional educator uses this law for
 *   pure coordination (Rope). The moderate defender, who must deal with its
 *   consequences, sees a hybrid of coordination (the logic works) and
 *   extraction (it costs resources to fight), hence a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `computer_science_educators` and `autonomous_agent_developers`
 *     directly benefit from the existence and understanding of this logical property.
 *   - Victims: `cybersecurity_defenders` and `static_code_analysis_tools` bear
 *     the costs, as self-replication complicates containment and analysis,
 *     imposing uncompensated work.
 *
 * MANDATROPHY ANALYSIS:
 *   This story avoids mislabeling the Quine as a Snare. The high-extraction
 *   "Snare" is the malware payload that *uses* the Quine mechanism, not the
 *   mechanism itself. By keeping ε low (0.20), we correctly identify the
 *   underlying logic as a Mountain/Rope, and its misuse as a Tangled Rope,
 *   preventing the conflation of a tool with its malicious application. This
 *   is a candidate for decomposition under the ε-invariance principle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_quine_evolution,
    "Can a self-replicating quine spontaneously evolve complex new traits (implying a Mountain of emergent potential) or is it a closed logical loop (a Scaffold for demonstrating a principle)?",
    "Long-term observation of quine-based digital life simulations with random bit-flip mutations.",
    "If Mountain: Quines are the seeds of true digital life. If Scaffold: They are mere logical parrots.",
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_quine_evolution, empirical, "Whether quines can be a basis for open-ended digital evolution.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(quine_self_replication, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not required as base_extractiveness (0.20) is below the 0.46 threshold.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% No network data for this constraint at this time.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed; the structural derivation from beneficiary/victim
% status and exit options is sufficient.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */