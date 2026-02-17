% ============================================================================
% CONSTRAINT STORY: nfl_superbowl_halftime_exclusivity
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_nfl_superbowl_halftime_exclusivity, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nfl_superbowl_halftime_exclusivity
 *   human_readable: NFL Super Bowl Halftime Show Exclusivity Agreements
 *   domain: economic
 *
 * SUMMARY:
 *   The NFL (National Football League) leverages its market dominance to negotiate exclusivity agreements with performers selected for the Super Bowl Halftime Show. These agreements often restrict performers from appearing in competing broadcasts or events for a specified period, suppressing alternative opportunities and potentially limiting the performer's artistic expression and income streams.
 *
 * KEY AGENTS (by structural relationship):
 *   - [Emerging Artists/Backup Performers]: Secondary target (powerless/trapped) - bears full extraction with no negotiation leverage.
 *   - [Established Halftime Performers]: Primary target (moderate/constrained) — bears extraction from limited opportunities.
 *   - [NFL]: Primary beneficiary (institutional/arbitrage) — benefits from controlled content and enhanced brand image.
 *   - [Competing Broadcasters]: Secondary target (institutional/constrained) - Restricted from leveraging same performers.
 *   - [Analytical Observer]: Analytical observer — sees full structure.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nfl_superbowl_halftime_exclusivity, 0.48).
domain_priors:suppression_score(nfl_superbowl_halftime_exclusivity, 0.55).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(nfl_superbowl_halftime_exclusivity, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nfl_superbowl_halftime_exclusivity, extractiveness, 0.48).
narrative_ontology:constraint_metric(nfl_superbowl_halftime_exclusivity, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(nfl_superbowl_halftime_exclusivity, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(nfl_superbowl_halftime_exclusivity, tangled_rope).
narrative_ontology:human_readable(nfl_superbowl_halftime_exclusivity, "NFL Super Bowl Halftime Show Exclusivity Agreements").

% --- Binary flags ---
domain_priors:requires_active_enforcement(nfl_superbowl_halftime_exclusivity).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(nfl_superbowl_halftime_exclusivity, nfl).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(nfl_superbowl_halftime_exclusivity, halftime_performers).
narrative_ontology:constraint_victim(nfl_superbowl_halftime_exclusivity, competing_broadcasters).

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

% PERSPECTIVE 1: THE EMERGING PERFORMER (POWERLESS TARGET)
% An emerging artist or backup performer with no negotiating power.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE ESTABLISHED PERFORMER (MODERATE TARGET)
% The headline act, who has some leverage but is still constrained by the NFL's market power.
constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE NFL (PRIMARY BENEFICIARY)
% The league benefits from exclusive content and brand control.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% Default analytical context, which sees both coordination and extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nfl_superbowl_halftime_exclusivity_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nfl_superbowl_halftime_exclusivity, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(nfl_superbowl_halftime_exclusivity, ExtMetricName, E),
    E >= 0.30. % Tangled rope, extractiveness must be > 0.3

:- end_tests(nfl_superbowl_halftime_exclusivity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is set to 0.48 because the exclusivity agreements limit performers' opportunities to leverage their Super Bowl exposure. Suppression is 0.55 because the NFL actively enforces these agreements and competing broadcasters are restricted from hiring the same performers. The theater ratio is low (0.30) because the primary function is economic control, not mere performance.
 *
 * PERSPECTIVAL GAP:
 *   Performers, especially emerging ones with no leverage (powerless/trapped), perceive the agreements as a Snare, restricting their career mobility and potential revenue streams. The NFL views it as a Rope, facilitating a coordinated and controlled high-profile event that benefits their brand. The analytical view sees both functions, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The NFL benefits from controlling the performers' exposure and preventing competing broadcasts from capitalizing on their fame boost (low 'd' value). Halftime performers (both established and emerging) and competing broadcasters bear the cost of restricted opportunities (high 'd' value).
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a Tangled Rope from the analytical perspective prevents mislabeling it as pure extraction (Snare). While the agreements extract value from performers, they also coordinate a significant performance event that benefits the NFL and its brand. The framework correctly captures that from the performers' view, it functions as a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_nfl_exclusivity,
    'What is the true economic impact of the exclusivity agreements on halftime performers?',
    'A comprehensive study comparing the earnings of performers who have participated in Super Bowl halftime shows with those who haven\'t, controlling for other factors.',
    'If the impact is negligible, the constraint may be a Rope. If the impact is significant, it is a Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(nfl_superbowl_halftime_exclusivity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(nfl_exclusivity_tr_t0, nfl_superbowl_halftime_exclusivity, theater_ratio, 0, 0.20).
narrative_ontology:measurement(nfl_exclusivity_tr_t5, nfl_superbowl_halftime_exclusivity, theater_ratio, 5, 0.25).
narrative_ontology:measurement(nfl_exclusivity_tr_t10, nfl_superbowl_halftime_exclusivity, theater_ratio, 10, 0.30).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(nfl_exclusivity_ex_t0, nfl_superbowl_halftime_exclusivity, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(nfl_exclusivity_ex_t5, nfl_superbowl_halftime_exclusivity, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(nfl_exclusivity_ex_t10, nfl_superbowl_halftime_exclusivity, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(nfl_superbowl_halftime_exclusivity, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */