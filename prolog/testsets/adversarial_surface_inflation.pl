% ============================================================================
% CONSTRAINT STORY: adversarial_surface_inflation
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-20
% ============================================================================

:- module(constraint_adversarial_surface_inflation, []).

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
 *   constraint_id: adversarial_surface_inflation
 *   human_readable: The Infinite Vulnerability Horizon
 *   domain: technological/cybernetic/security
 *
 * SUMMARY:
 *   A scenario where the increasing complexity and interconnectedness of
 *   digital systems (Rope) creates a non-linear expansion of possible attack
 *   vectors. This "Rope" for achieving massive integration and feature-rich
 *   functionality becomes a "Snare" for the defender, as the "surface area"
 *   to be protected grows faster than the human or financial capacity to
 *   monitor it, liquidating the subject's primary security agency and
 *   trapping them in a state of perpetual "reactive firefighting" against
 *   an exponentially growing threat landscape.
 *
 * KEY AGENTS (by structural relationship):
 *   - Cybersecurity Defenders: Primary target (powerless/trapped) — bears extraction
 *   - Hyper-Connectivity Architects: Primary beneficiary (institutional/arbitrage) — benefits from constraint
 *   - Information Forensic Auditor: Analytical observer (analytical/analytical) — sees full structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(adversarial_surface_inflation, 0.84).
domain_priors:suppression_score(adversarial_surface_inflation, 0.72).   % Alternatives (e.g., air-gapping, simplification) are suppressed by market/operational mandates.
domain_priors:theater_ratio(adversarial_surface_inflation, 0.89).       % High theater: "Compliance Dashboards" that performatively signal safety while the surface area balloons.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(adversarial_surface_inflation, extractiveness, 0.84).
narrative_ontology:constraint_metric(adversarial_surface_inflation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(adversarial_surface_inflation, theater_ratio, 0.89).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(adversarial_surface_inflation, tangled_rope).
narrative_ontology:human_readable(adversarial_surface_inflation, "The Infinite Vulnerability Horizon").
narrative_ontology:topic_domain(adversarial_surface_inflation, "technological/cybernetic/security").

% --- Binary flags ---
domain_priors:requires_active_enforcement(adversarial_surface_inflation). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(adversarial_surface_inflation, hyper_connectivity_architects).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(adversarial_surface_inflation, cybersecurity_defenders).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% The defender is trapped: to be functional, they must use the inflated
% surface area, but protecting it liquidates their time and strategic agency.
% Engine derives d from victim membership + trapped exit -> d ≈ 0.95 -> high χ.
constraint_indexing:constraint_classification(adversarial_surface_inflation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% The architect views the inflation as a Rope—the essential coordination
% substrate for building a "Total Connectivity" economy.
% Engine derives d from beneficiary membership + arbitrage exit -> d ≈ 0.05 -> low/negative χ.
constraint_indexing:constraint_classification(adversarial_surface_inflation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.84) and active enforcement masking as essential coordination.
% Engine derives d ≈ 0.72 for analytical perspective.
constraint_indexing:constraint_classification(adversarial_surface_inflation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.89) > 0.70 triggers Piton: the "Security Audit"
% is an inertial spike; it performatively catalogs known risks while
% the unknown surface continues to expand.
constraint_indexing:constraint_classification(adversarial_surface_inflation, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(adversarial_surface_inflation, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(adversarial_surface_inflation_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless defender vs Rope for the institutional architect.
    constraint_indexing:constraint_classification(adversarial_surface_inflation, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(adversarial_surface_inflation, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(adversarial_surface_inflation, tangled_rope, context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.89) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(adversarial_surface_inflation, piton, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Ensures it's a high-extraction constraint.
    narrative_ontology:constraint_metric(adversarial_surface_inflation, extractiveness, E),
    E >= 0.46.

:- end_tests(adversarial_surface_inflation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extraction score of 0.84 represents a state where the supposed
 *   coordination benefit of hyper-connectivity is achieved by liquidating the
 *   defender's capacity for sovereign security. The defender's entire surplus of
 *   time, budget, and strategic agency is consumed by the reactive need to defend
 *   an ever-expanding, fundamentally insecure surface. The high theater ratio (0.89)
 *   reflects performative security measures like compliance dashboards that create
 *   an illusion of control, leading to a Piton classification from an auditor's perspective.
 *
 * PERSPECTIVAL GAP:
 *   The Cybersecurity Defender experiences a Snare because their environment is
 *   indefensible by design, trapping them in a cycle of reactive firefighting.
 *   The Hyper-Connectivity Architect, who benefits from the network effects of
 *   total integration, views the system as a pure Rope, essential for coordinating
 *   a global-scale digital economy. This gap is total: one agent's essential
 *   infrastructure is another's inescapable trap.
 *
 * DIRECTIONALITY LOGIC:
 *   The perspectival gap is driven by the structural relationships encoded in the
 *   beneficiary/victim declarations. 'cybersecurity_defenders' are victims with
 *   trapped exit, which the engine maps to a high directionality (d ≈ 0.95),
 *   maximizing effective extraction (χ) and producing a Snare classification.
 *   Conversely, 'hyper_connectivity_architects' are beneficiaries with arbitrage
 *   exit, mapping to a low directionality (d ≈ 0.05), minimizing χ and producing
 *   a Rope classification. The constraint's function is thus inverted depending on
 *   one's structural position relative to it.
 *
 * MANDATROPHY ANALYSIS:
 *   [RESOLVED MANDATROPHY]
 *   The Mandatrophy is resolved by the Tangled Rope classification from the
 *   analytical perspective. This prevents the system from making a naive
 *   classification of either pure Rope (ignoring the 0.84 extraction) or pure
 *   Snare (ignoring the genuine, if costly, coordination function). The Tangled
 *   Rope classification correctly identifies the hybrid nature of the constraint:
 *   it simultaneously provides a coordination function for its beneficiaries
 *   while imposing severe, asymmetric extraction on its victims. This nuanced
 *   view is the resolution, as it acknowledges both conflicting realities without
 *   dismissing either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_surface_saturation,
    'Can AI-on-AI defense restore the Rope, or is inflation a physical law of networks (Snare vs Mountain)?',
    'Tracking the success rate of autonomous "Self-Healing" networks against 2026-style polymorphic attacks.',
    'If self-healing holds: Snare of current technique. If it fails: Mountain of Information Complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(adversarial_surface_inflation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(asi_tr_t0, adversarial_surface_inflation, theater_ratio, 0, 0.40).
narrative_ontology:measurement(asi_tr_t5, adversarial_surface_inflation, theater_ratio, 5, 0.65).
narrative_ontology:measurement(asi_tr_t10, adversarial_surface_inflation, theater_ratio, 10, 0.89).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(asi_ex_t0, adversarial_surface_inflation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(asi_ex_t5, adversarial_surface_inflation, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(asi_ex_t10, adversarial_surface_inflation, base_extractiveness, 10, 0.84).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(adversarial_surface_inflation, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed for this constraint; the structural derivation from
% beneficiary/victim declarations accurately captures the directionality.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */