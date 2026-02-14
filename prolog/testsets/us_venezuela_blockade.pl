% ============================================================================
% CONSTRAINT STORY: us_venezuela_blockade
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_us_venezuela_blockade, []).

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
 *   constraint_id: us_venezuela_blockade
 *   human_readable: Proposed US Naval Blockade of Venezuelan Oil Tankers
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   Based on a potential policy announcement by a future US administration,
 *   this constraint represents a naval blockade intended to prevent Venezuelan
 *   oil tankers from delivering crude oil to international buyers. The act
 *   is a form of coercive economic warfare, using superior military power to
 *   enforce sanctions and deprive the Venezuelan state of its primary source
 *   of revenue. It functions by physically suppressing the alternative of
 *   free maritime trade for the target nation.
 *
 * KEY AGENTS (by structural relationship):
 *   - Venezuelan State (and PDVSA): Primary target (powerless/trapped) — bears the full cost of the extraction, which manifests as lost revenue and challenged sovereignty.
 *   - US Administration: Primary beneficiary (institutional/arbitrage) — benefits from geopolitical leverage, sanctions enforcement, and domestic political signaling.
 *   - International Shipping Companies & Oil Buyers: Secondary victims (moderate/constrained) — their operations are disrupted and they are forced to comply or risk asset seizure.
 *   - Analytical Observer: Geopolitical analyst (analytical/analytical) — sees the full structure of coercive power dynamics and its impact on international law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_venezuela_blockade, 0.85).
domain_priors:suppression_score(us_venezuela_blockade, 0.95).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(us_venezuela_blockade, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_venezuela_blockade, extractiveness, 0.85).
narrative_ontology:constraint_metric(us_venezuela_blockade, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(us_venezuela_blockade, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not a mountain constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_venezuela_blockade, snare).

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_venezuela_blockade). % The blockade is pure enforcement.

% --- Emergence flag (required for mountain constraints) ---
% Not applicable.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_venezuela_blockade, us_administration).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_venezuela_blockade, venezuelan_state).
narrative_ontology:constraint_victim(us_venezuela_blockade, international_shippers).

% Gate requirements check:
%   Snare: victim required (OK)

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% The Venezuelan state, facing overwhelming military force. Engine derives d from:
%   victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
%   χ = 0.85 * 1.42 * 1.0 (national scope) ≈ 1.21, well into Snare territory.
constraint_indexing:constraint_classification(us_venezuela_blockade, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The US administration wielding the blockade as a policy tool. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
%   χ = 0.85 * -0.12 * 1.0 (national scope) ≈ -0.10. This is a pure Rope from their
%   view, a coordination tool for achieving foreign policy objectives.
constraint_indexing:constraint_classification(us_venezuela_blockade, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% The default analytical context. The global scope amplifies perceived extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ = 0.85 * 1.15 * 1.2 (global scope) ≈ 1.17. Clearly a Snare.
constraint_indexing:constraint_classification(us_venezuela_blockade, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: A NEUTRAL SHIPPING COMPANY (TANGLED ROPE/SNARE)
% This actor is not the primary target but is severely impacted. They are a
% victim with constrained exit, not fully trapped. They may see some
% "coordination" in the form of clear rules from a hegemon, but the primary
% experience is coercive. Engine derives a high, but not maximum, d.
% The classification is borderline Snare/Tangled Rope, depending on how much
% they value the "predictability" imposed by the blockade.
constraint_indexing:constraint_classification(us_venezuela_blockade, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_venezuela_blockade_tests).

test(perspectival_gap_is_snare_vs_rope) :-
    % Verify the core perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(us_venezuela_blockade, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(us_venezuela_blockade, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(analytical_view_is_snare) :-
    constraint_indexing:constraint_classification(us_venezuela_blockade, snare, context(agent_power(analytical), _, _, _)).

test(threshold_validation_for_snare) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(us_venezuela_blockade, ExtMetricName, E),
    narrative_ontology:constraint_metric(us_venezuela_blockade, SuppMetricName, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(us_venezuela_blockade_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.85): The blockade's explicit goal is to capture or deny the entirety of Venezuela's primary export revenue stream. The extraction is direct, massive, and the central purpose of the action.
 *   - Suppression (0.95): The constraint works by applying overwhelming military force to eliminate the alternative of free maritime trade. For the target, there are no viable alternative shipping routes or methods that can circumvent a US naval blockade.
 *   - Theater (0.20): While the announcement is political theater, the implementation is a kinetic, functional action. The ratio of functional enforcement to performative signaling is high.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme and definitional of this type of geopolitical conflict.
 *   - From the US Administration's (beneficiary) perspective, the blockade is a 'Rope'. It's a tool to coordinate international pressure, enforce existing sanctions, and achieve strategic goals. The massive coercive overhead is seen as a necessary cost of enforcement, not as extraction.
 *   - From the Venezuelan State's (victim) perspective, this is a textbook 'Snare'. It's an externally imposed constraint designed for pure extraction (of economic lifeblood and sovereignty) with no coordination benefit whatsoever. They are trapped with no viable exit.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is unambiguous. The 'us_administration' is declared the beneficiary because they initiate the policy to gain geopolitical leverage. The 'venezuelan_state' is the victim, bearing the direct economic and political costs. This structural relationship, combined with their respective exit options (arbitrage vs. trapped), drives the d-value derivation and creates the massive gap in perceived effective extraction (χ), correctly classifying the constraint as Rope for one and Snare for the other.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY] This case is a powerful defense against Mandatrophy. A naive analysis might focus on the US's stated goals ("restoring democracy," "enforcing international norms") and misclassify the blockade as a flawed coordination mechanism. The Deferential Realism framework, by indexing to the powerless/trapped perspective, forces the classification to acknowledge the structural reality: for the target, this is pure coercion. The system correctly identifies that what one institution calls "coordination" is experienced by another as a high-suppression Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_us_venezuela_blockade,
    'Would the blockade trigger direct military intervention from a rival power (e.g., China, Russia) to protect their interests and assets?',
    'Observation of naval deployments and diplomatic responses following the blockade''s implementation.',
    'If YES, the constraint''s nature could shift from a bilateral Snare to a multilateral Tangled Rope, as the US would have to coordinate/negotiate with other powerful actors. If NO, it remains a pure Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_venezuela_blockade, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for a hypothetical implementation of the policy.
% Since base_extractiveness > 0.46, this section is required.

% Theater ratio starts higher with the announcement, then decreases as the
% blockade becomes a functional, ongoing military operation.
narrative_ontology:measurement(us_venezuela_blockade_tr_t0, us_venezuela_blockade, theater_ratio, 0, 0.30).
narrative_ontology:measurement(us_venezuela_blockade_tr_t5, us_venezuela_blockade, theater_ratio, 5, 0.25).
narrative_ontology:measurement(us_venezuela_blockade_tr_t10, us_venezuela_blockade, theater_ratio, 10, 0.20).

% Extraction starts high and slightly increases as enforcement becomes more
% efficient and loopholes are closed.
narrative_ontology:measurement(us_venezuela_blockade_ex_t0, us_venezuela_blockade, base_extractiveness, 0, 0.80).
narrative_ontology:measurement(us_venezuela_blockade_ex_t5, us_venezuela_blockade, base_extractiveness, 5, 0.82).
narrative_ontology:measurement(us_venezuela_blockade_ex_t10, us_venezuela_blockade, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: From the perspective of the enforcer, this is a tool
% of coercion used to align other actors with its policy.
narrative_ontology:coordination_type(us_venezuela_blockade, enforcement_mechanism).

% Network relationships: A blockade of a major oil producer would have
% immediate and significant effects on global energy markets.
narrative_ontology:affects_constraint(us_venezuela_blockade, global_oil_prices).
narrative_ontology:affects_constraint(us_venezuela_blockade, freedom_of_navigation).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations and exit options (arbitrage vs. trapped)
% accurately models the power asymmetry and correctly generates the extreme
% perspectival gap.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */