% ============================================================================
% CONSTRAINT STORY: silver_scarcity_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_silver_scarcity_2026, []).

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
 *   constraint_id: silver_scarcity_2026
 *   human_readable: The Silver Physical Scarcity Mountain
 *   domain: economic/industrial/geopolitical
 *
 * SUMMARY:
 *   By 2026, silver has been designated a "Critical Mineral" by the USGS.
 *   A structural deficit of ~95-200 million ounces—the sixth consecutive year of shortfall—
 *   has transformed the silver market from a speculative arena into a physical Mountain.
 *   Because ~75% of silver is mined as a byproduct (of copper/lead/zinc), the supply is
 *   price-inelastic, creating an irreducible physical limit for the green energy transition
 *   and AI hardware sectors. The lack of viable substitutes for its conductivity makes
 *   this a hard physical boundary.
 *
 * KEY AGENTS (by structural relationship):
 *   - Industrial Consumers (PV/EV/AI): Primary subjects (powerless/trapped) — confront the physical limit directly.
 *   - Sovereign Entities: Strategic actors (institutional/arbitrage) — navigate the limit by stockpiling.
 *   - Market Analysts: Analytical observers — observe the physical limit and its effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(silver_scarcity_2026, 0.04). % Low extraction; this is a natural/physical limit, not a policy.
domain_priors:suppression_score(silver_scarcity_2026, 0.05).   % Low structural suppression. The lack of alternatives is a feature of physics, not an actively enforced policy.
domain_priors:theater_ratio(silver_scarcity_2026, 0.40).       % Primarily functional scarcity, though strategic theater is rising.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(silver_scarcity_2026, extractiveness, 0.04).
narrative_ontology:constraint_metric(silver_scarcity_2026, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(silver_scarcity_2026, theater_ratio, 0.40).

% --- NL Profile Metrics (required for mountain constraints) ---
% These feed the natural_law_signature certification chain in
% structural_signatures.pl. Required for the mountain metric gate.
narrative_ontology:constraint_metric(silver_scarcity_2026, accessibility_collapse, 0.95). % No viable substitutes for key industrial uses.
narrative_ontology:constraint_metric(silver_scarcity_2026, resistance, 0.02). % Resistance is incoherent against a geological limit.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(silver_scarcity_2026, mountain).
narrative_ontology:human_readable(silver_scarcity_2026, "The Silver Physical Scarcity Mountain").

% --- Emergence flag (required for mountain constraints) ---
% This constraint emerges from geology and physics, not human design.
% Required for the mountain metric gate to fire.
domain_priors:emerges_naturally(silver_scarcity_2026).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense of a social
% arrangement. Agents are subjects of the constraint, not targets of a policy.
% The absence of these declarations is intentional and core to the Mountain classification.

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

% PERSPECTIVE 1: THE INDUSTRIAL USER (MOUNTAIN)
% The physical limit is felt as an unchangeable law of nature.
% The classification is Mountain because ε <= 0.25 and suppression <= 0.05.
% This is a uniform-type constraint; it appears as a Mountain from all perspectives.
constraint_indexing:constraint_classification(silver_scarcity_2026, mountain,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE SOVEREIGN STATE (MOUNTAIN)
% Viewed as a strategic foundation for national security, but still an
% unchangeable physical boundary to be navigated.
constraint_indexing:constraint_classification(silver_scarcity_2026, mountain,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (MOUNTAIN)
% The analytical view confirms the classification as a physical limit.
constraint_indexing:constraint_classification(silver_scarcity_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(silver_scarcity_2026_tests).

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(silver_scarcity_2026, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(silver_scarcity_2026, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    % Mountains must have low extraction (<= 0.25) and low suppression (<= 0.05).
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(silver_scarcity_2026, ExtMetricName, E),
    domain_priors:suppression_score(silver_scarcity_2026, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    % Mountain candidates must have the full Natural Law profile to pass certification.
    domain_priors:emerges_naturally(silver_scarcity_2026),
    narrative_ontology:constraint_metric(silver_scarcity_2026, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(silver_scarcity_2026, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(silver_scarcity_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The Silver Scarcity is classified as a Mountain because its primary driver is a
 *   multi-year physical production peak (2016) colliding with exponential industrial
 *   demand. Unlike a Snare, the "extraction" isn't a policy choice but a physical limit (ε=0.04).
 *   The structural suppression score is low (0.05) because the lack of alternatives is a
 *   feature of physics (silver's supreme conductivity), not a socially enforced policy.
 *   The classification is certified by the Natural Law profile: high accessibility collapse (0.95)
 *   due to no substitutes, near-zero resistance (0.02) as one cannot resist geology, and
 *   the fact that the constraint emerges naturally.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap. As a uniform-type constraint (Mountain), all agents
 *   perceive the same fundamental reality: an unchangeable physical limit. Their strategies
 *   for dealing with it differ, but the classification of the constraint itself is invariant.
 *
 * DIRECTIONALITY LOGIC:
 *   As a Mountain, this constraint has no inherent directionality. It is a feature of
 *   the environment, not a social construct with beneficiaries and victims. Therefore,
 *   `constraint_beneficiary/2` and `constraint_victim/2` are intentionally omitted. The
 *   engine's directionality derivation is not applicable.
 *
 * MANDATROPHY ANALYSIS:
 *   The Mountain classification correctly identifies the constraint as a physical boundary,
 *   preventing it from being mislabeled as a Snare (a policy of artificial scarcity) or a
 *   Rope (a coordination problem). This distinction is critical for policy responses: one
 *   cannot negotiate with or regulate a physical limit in the same way as a social one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_byproduct_elasticity,
    'Will a massive surge in copper/zinc mining (due to their own high prices) incidentally resolve the silver deficit?',
    'Review of 2026-2030 global base-metal capex and byproduct silver yield data.',
    'Elastic supply would revert the Mountain to a Rope; inelasticity hardens the Mountain.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(silver_scarcity_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not strictly required for low-extraction constraints, but is
% included here to show the stability of the physical limit over the interval.

% Theater ratio (Tracking the shift from market trade to strategic stockpiling)
narrative_ontology:measurement(sv_tr_t0, silver_scarcity_2026, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sv_tr_t5, silver_scarcity_2026, theater_ratio, 5, 0.40).
narrative_ontology:measurement(sv_tr_t10, silver_scarcity_2026, theater_ratio, 10, 0.40).

% Extraction over time (Remains low as this is a physical constraint)
narrative_ontology:measurement(sv_be_t0, silver_scarcity_2026, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(sv_be_t5, silver_scarcity_2026, base_extractiveness, 5, 0.04).
narrative_ontology:measurement(sv_be_t10, silver_scarcity_2026, base_extractiveness, 10, 0.04).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Not applicable for a Mountain.
% narrative_ontology:coordination_type(silver_scarcity_2026, [type]).

% Network relationships (structural influence edges)
% narrative_ontology:affects_constraint(silver_scarcity_2026, [other_constraint_id]).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% Not applicable for a Mountain.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */