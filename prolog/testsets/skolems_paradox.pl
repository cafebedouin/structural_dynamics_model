% ============================================================================
% CONSTRAINT STORY: skolems_paradox
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_skolems_paradox, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: skolems_paradox
 *   human_readable: Skolem's Paradox (The Relativity of Cardinality)
 *   domain: technological/logic
 *
 * SUMMARY:
 *   Skolem's Paradox arises from the Downward Löwenheim–Skolem theorem, which states
 *   that if a first-order theory (like ZFC set theory) has an infinite model, it
 *   must also have a countable model. The "paradox" is that ZFC proves the
 *   existence of uncountable sets, yet it must have a model where every set is
 *   countable from an external perspective. This reveals a fundamental limit on
 *   the expressive power of first-order logic.
 *
 * KEY AGENTS (by structural relationship):
 *   - Mathematical Absolutists: Primary target (powerless/trapped) — their goal of formalizing a unique, absolute mathematical reality is blocked by the constraint.
 *   - Model Theorists: Primary beneficiary (institutional/arbitrage) — they use the relativity of models as a tool to explore different mathematical structures.
 *   - First-Order Logic (System): The medium that imposes the constraint.
 *   - Analytical Observer: Sees the full structure as a logical limit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Low. The "extraction" is metaphorical—it extracts the possibility of
% absolute semantic certainty from first-order claims, not tangible resources.
domain_priors:base_extractiveness(skolems_paradox, 0.20).
% Rationale: Very low. The constraint is a feature of first-order logic. Alternatives
% like second-order logic are not actively suppressed; they are less used due to
% their own undesirable properties (e.g., lack of a complete proof system).
domain_priors:suppression_score(skolems_paradox, 0.05).
% Rationale: Low. The paradox is a substantive logical feature, not a performance.
domain_priors:theater_ratio(skolems_paradox, 0.10).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(skolems_paradox, extractiveness, 0.20).
narrative_ontology:constraint_metric(skolems_paradox, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(skolems_paradox, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in structural_signatures.pl.
narrative_ontology:constraint_metric(skolems_paradox, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(skolems_paradox, resistance, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(skolems_paradox, mountain).
narrative_ontology:human_readable(skolems_paradox, "Skolem's Paradox (The Relativity of Cardinality)").

% --- Binary flags ---
% No active enforcement needed.

% --- Emergence flag (required for mountain constraints) ---
% Emerges naturally from the formal properties of first-order logic and model theory.
domain_priors:emerges_naturally(skolems_paradox).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Although this is a Mountain, we declare these to explain the Rope perspective.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(skolems_paradox, model_theorists).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(skolems_paradox, mathematical_absolutists).

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

% PERSPECTIVE 1: THE PLATONIC IDEALIST / ABSOLUTIST (MOUNTAIN)
% Agent who seeks a unique "True Universe of Sets". Their goal is blocked by an
% immovable logical fact. While this feels like a Snare, its metrics (low ε, low
% suppression) classify it as a Mountain. The feeling of being trapped is a
*reaction*
% to the mountain, not evidence of an extractive mechanism.
constraint_indexing:constraint_classification(skolems_paradox, mountain,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MODEL THEORIST (ROPE)
% Agent who uses the relativity of models as a tool. For them, the paradox is a
% coordination device that defines the rules for moving between an object-language
% (the theory) and a meta-language (the model), enabling the exploration of
% different mathematical universes.
constraint_indexing:constraint_classification(skolems_paradox, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The default analytical context. The paradox is an unchangeable feature of the
% landscape of formal systems, a permanent limit on the expressive power of
% first-order languages.
constraint_indexing:constraint_classification(skolems_paradox, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(skolems_paradox_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between the theorist (Rope) and the analyst (Mountain).
    constraint_indexing:constraint_classification(skolems_paradox, TypeTheorist, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(skolems_paradox, TypeAnalyst, context(agent_power(analytical), _, _, _)),
    TypeTheorist \= TypeAnalyst,
    TypeTheorist == rope,
    TypeAnalyst == mountain.

test(analytical_is_mountain) :-
    % The analytical perspective must see this as a Mountain.
    constraint_indexing:constraint_classification(skolems_paradox, mountain, context(agent_power(analytical), _, _, _)).

test(mountain_metrics_adherence) :-
    % Verify the metrics align with the Mountain classification.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(skolems_paradox, ExtMetricName, E),
    narrative_ontology:constraint_metric(skolems_paradox, SuppMetricName, S),
    E =< 0.25,
    S =< 0.05.

:- end_tests(skolems_paradox_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The core decision was to classify this as a Mountain, reflecting its status as a
 *   fundamental theorem of logic. The base extractiveness (0.20) is metaphorical,
 *   representing the "extraction of certainty" from absolutist claims. The
 *   suppression score was lowered to 0.05 from an earlier, higher value, because
 *   the preference for first-order logic is not due to active suppression of
 *   alternatives but due to the inherent, undesirable properties (like
 *   incompleteness) of those alternatives. The file now includes the full NL
 *   profile (accessibility_collapse, resistance, emerges_naturally) required for a
 *   valid Mountain classification.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between utility and immutability. The Model Theorist (institutional)
 *   sees a Rope because they leverage the theorem's consequences as a tool for
 *   coordination—it provides clear rules for discussing model relativity. The
 *   Absolutist (powerless) and the Analyst see a Mountain—an unchangeable,
 *   unavoidable feature of the logical landscape. The Absolutist's subjective
 *   feeling of being "snared" is a reaction to the Mountain, not a structural
 *   property of it; the low effective extraction (χ) confirms it is not a Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: Model theorists benefit because the theorem creates the very
 *     distinctions (internal vs. external countability) that their field studies.
 *   - Victim: Mathematical absolutists/Platonists are victims because the theorem
 *     structurally undermines any attempt to use first-order logic to formalize a
 *     unique, absolute universe of sets.
 *
 * MANDATROPHY ANALYSIS:
 *   By classifying this as a Mountain, the system correctly identifies it as a
 *   feature of reality (the reality of formal logic) rather than a socially
 *   constructed extractive tool. The Rope perspective shows how even a Mountain
 *   can have a coordination function for a specific group that learns to work
 *   with its properties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    skolems_paradox_omega,
    'Is there a privileged "Standard Model" of Set Theory that is not relative, or is reality an infinite sea of models?',
    'Discovery of higher-order logical principles or axioms that could uniquely specify a model of set theory.',
    'If a unique model exists, the paradox becomes a feature of a flawed language (FOL) rather than a feature of mathematical reality. If not, the paradox is a fundamental aspect of reality.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(skolems_paradox_omega, conceptual, "Whether a privileged 'Standard Model' of Set Theory exists, making cardinality absolute or relative.").

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(skolems_paradox, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Not applicable. Base extractiveness is below the 0.46 threshold for mandatory
% temporal tracking. As a logical theorem, its properties are static.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Not applicable for this constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not needed; the structural derivation from beneficiary/victim groups and
% exit options correctly models the directionality for all agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */