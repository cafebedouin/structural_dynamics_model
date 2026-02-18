% ============================================================================
% CONSTRAINT STORY: cross_domain_coupling_spiral
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_cross_domain_coupling_spiral, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: cross_domain_coupling_spiral
 * human_readable: The Entangled Dependency Vortex
 * domain: technological/economic/cybernetic
 * * SUMMARY:
 * A scenario where increasing efficiency is sought by tightly coupling
 * independent domains (e.g., energy grids, financial markets, and digital
 * identity systems). While this "Rope" for global coordination creates
 * massive systemic throughput, it becomes a "Snare" for the subject as
 * failures in one domain spiral instantly into others, liquidating the
 * subject's ability to isolate risks or maintain functional autonomy in
 * any single sector.
 * * KEY AGENTS:
 * - Local Infrastructure Manager: Subject (Powerless)
 * - Integrated Platform Architect: Beneficiary (Institutional)
 * - Systemic Contagion Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.88) reflects the total liquidation of domain-specific
% resilience to feed the institutional need for integrated efficiency.
domain_priors:base_extractiveness(cross_domain_coupling_spiral, 0.88).
domain_priors:suppression_score(cross_domain_coupling_spiral, 0.79).   % "Decoupled" alternatives are suppressed as inefficient or obsolete.
domain_priors:theater_ratio(cross_domain_coupling_spiral, 0.91).       % High theater: "Resilience Certifications" masking the reality of cascading vulnerability.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(cross_domain_coupling_spiral, extractiveness, 0.88).
narrative_ontology:constraint_metric(cross_domain_coupling_spiral, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(cross_domain_coupling_spiral, theater_ratio, 0.91).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(cross_domain_coupling_spiral, tangled_rope).
narrative_ontology:human_readable(cross_domain_coupling_spiral, "The Entangled Dependency Vortex").
narrative_ontology:topic_domain(cross_domain_coupling_spiral, "technological/economic/cybernetic").

% Binary flags and structural properties for Tangled Rope
domain_priors:requires_active_enforcement(cross_domain_coupling_spiral).
narrative_ontology:constraint_beneficiary(cross_domain_coupling_spiral, integrated_platform_architects).
narrative_ontology:constraint_victim(cross_domain_coupling_spiral, local_infrastructure_managers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The manager is trapped: a software bug in a remote data center liquidates
% their local energy grid's stability, leaving them with zero exit options.
constraint_indexing:constraint_classification(cross_domain_coupling_spiral, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The architect views the coupling as a Rope—the essential coordination
% substrate for achieving "Total System Optimization" across global markets.
constraint_indexing:constraint_classification(cross_domain_coupling_spiral, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the combination of a genuine coordination function (beneficiaries exist)
% with severe asymmetric extraction (victims exist) and active enforcement.
constraint_indexing:constraint_classification(cross_domain_coupling_spiral, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.91) > 0.70 triggers Piton: the "Failover Strategy"
% is an inertial spike; it performatively charts a recovery path that
% cannot withstand cross-domain contagion.
constraint_indexing:constraint_classification(cross_domain_coupling_spiral, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cross_domain_coupling_spiral_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless manager vs Rope for the institutional architect.
    constraint_indexing:constraint_classification(cross_domain_coupling_spiral, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cross_domain_coupling_spiral, rope,
        context(agent_power(institutional), _, _, _)),
    writeln('Perspectival gap validated: Snare vs Rope.').

test(piton_trigger) :-
    % Ensure high theater ratio (0.91) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(cross_domain_coupling_spiral, piton,
        context(agent_power(analytical), _, _, _)),
    writeln('Piton classification validated by high theater ratio.').

test(tangled_rope_detection) :-
    % Ensure the analytical observer correctly identifies the Tangled Rope.
    constraint_indexing:constraint_classification(cross_domain_coupling_spiral, tangled_rope,
        context(agent_power(analytical), time_horizon(civilizational), _, _)),
    writeln('Tangled Rope classification validated for analytical observer.').

:- end_tests(cross_domain_coupling_spiral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.88) reflects a "Mandatrophy" state where the
 * "coordination" benefit of tight coupling is achieved by liquidating the
 * subject's primary risk-management agency. The suppression score (0.79)
 * is high because market forces and interoperability standards make it
 * nearly impossible to operate a decoupled, resilient alternative. The
 * theater ratio (0.91) is extreme, representing the vast industry of
 * "resilience certifications" and "system stability dashboards" that
 * performatively mask the underlying fragility.
 *
 * * PERSPECTIVAL GAP:
 * The Local Infrastructure Manager feels a Snare because they are forced
 * into a dependency where they suffer the consequences of failures they
 * did not cause. The Architect sees a Rope because the coupling coordinates
 * a level of efficiency and liquidity impossible in a decoupled state.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The high extraction (0.88) could lead to a misclassification as a pure Snare.
 * However, the system is correctly identified as a Tangled Rope from an
 * analytical perspective because it possesses a genuine coordination function
 * (beneficiaries exist and profit from the integration) alongside its severe
 * asymmetric extraction. The additional Piton classification highlights the
 * decay of its functional promise into pure theater, resolving the ambiguity
 * between its claimed purpose and its actual effect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_coupling_irreversibility,
    'Can "Circuit Breakers" restore the Rope, or is contagion a physical law of networks (Snare vs Mountain)?',
    'Tracking the propagation velocity of financial shocks into energy-grid availability in 2026.',
    'If breakers hold: Snare of current system design. If they fail: Mountain of Information Physics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cross_domain_coupling_spiral, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint began as a genuine coordination effort (low extraction, low theater)
% but spiraled into a highly extractive and performative state over time.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(cross_domain_coupling_spiral_tr_t0, cross_domain_coupling_spiral, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cross_domain_coupling_spiral_tr_t5, cross_domain_coupling_spiral, theater_ratio, 5, 0.50).
narrative_ontology:measurement(cross_domain_coupling_spiral_tr_t10, cross_domain_coupling_spiral, theater_ratio, 10, 0.91).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cross_domain_coupling_spiral_ex_t0, cross_domain_coupling_spiral, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cross_domain_coupling_spiral_ex_t5, cross_domain_coupling_spiral, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(cross_domain_coupling_spiral_ex_t10, cross_domain_coupling_spiral, base_extractiveness, 10, 0.88).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(cross_domain_coupling_spiral, global_infrastructure).

% Network relationships (structural influence edges)
% The stability of this coupled system directly impacts the stability of financial markets.
narrative_ontology:affects_constraint(cross_domain_coupling_spiral, financial_market_stability).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */