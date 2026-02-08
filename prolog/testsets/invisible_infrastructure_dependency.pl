% ============================================================================
% CONSTRAINT STORY: invisible_infrastructure_dependency
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_invisible_infrastructure_dependency, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: invisible_infrastructure_dependency
 * human_readable: The Submerged Substrate Trap
 * domain: technological/logistical/economic
 * * SUMMARY:
 * A scenario where a critical, ubiquitous service (e.g., a cloud provider, a core software library, a global logistics network) is so deeply integrated 
 * into the foundation of daily operations that it becomes "invisible" to the 
 * subject. This coordination tool becomes a "Snare" for the user, as their 
 * fundamental survival or operational agency is liquidated into a dependency 
 * they can neither perceive nor replace, trapping them in a territory where 
 * a single failure in the hidden substrate causes a total, unaddressable 
 * systemic collapse.
 * * KEY AGENTS:
 * - End User: Subject (Powerless)
 * - Substrate Monopolist: Beneficiary (Institutional)
 * - Infrastructure Resilience Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.87) reflects the siphoning of the subject's autonomy 
% to maintain a dependency that has no visible exit options.
domain_priors:base_extractiveness(invisible_infrastructure_dependency, 0.87).
domain_priors:suppression_score(invisible_infrastructure_dependency, 0.82).   % Alternative infrastructures are suppressed by the "invisible" nature of the primary.
domain_priors:theater_ratio(invisible_infrastructure_dependency, 0.91).       % Extreme theater: "Reliability Reports" that performatively signal stability while hiding structural debt.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(invisible_infrastructure_dependency, extractiveness, 0.87).
narrative_ontology:constraint_metric(invisible_infrastructure_dependency, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(invisible_infrastructure_dependency, theater_ratio, 0.91).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(invisible_infrastructure_dependency, piton).

% Binary flags
domain_priors:requires_active_enforcement(invisible_infrastructure_dependency). % Enforcement via proprietary APIs, vendor lock-in, and prohibitive switching costs.

% Structural property derivation hooks for Tangled Rope:
narrative_ontology:constraint_beneficiary(invisible_infrastructure_dependency, substrate_monopolist).
narrative_ontology:constraint_victim(invisible_infrastructure_dependency, end_user).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The user is trapped: they are dependent on a system they don't even 
% know exists, liquidating their ability to prepare for its failure.
constraint_indexing:constraint_classification(invisible_infrastructure_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The monopolist views the invisibility as a Rope—the essential coordination 
% substrate for providing a frictionless, standardized global utility.
constraint_indexing:constraint_classification(invisible_infrastructure_dependency, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.91) > 0.70 triggers Piton: the "Official Status Page" 
% is an inertial spike; it performatively signals "Green" status while 0.87 extraction continues.
constraint_indexing:constraint_classification(invisible_infrastructure_dependency, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.87) masking as essential coordination (Rope),
% enabled by active enforcement and clear victim/beneficiary classes.
constraint_indexing:constraint_classification(invisible_infrastructure_dependency, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(invisible_infrastructure_dependency_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(invisible_infrastructure_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(invisible_infrastructure_dependency, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(piton_trigger) :-
    % Ensure extreme theater (0.91) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(invisible_infrastructure_dependency, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements) :-
    % Verify that all structural requirements for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(invisible_infrastructure_dependency, _),
    narrative_ontology:constraint_victim(invisible_infrastructure_dependency, _),
    domain_priors:requires_active_enforcement(invisible_infrastructure_dependency).

test(high_extraction_threshold) :-
    % Verify the base extraction is high enough to warrant Snare/Tangled Rope classifications.
    narrative_ontology:constraint_metric(invisible_infrastructure_dependency, extractiveness, E),
    E >= 0.46.

:- end_tests(invisible_infrastructure_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.87) reflects a "Mandatrophy" state where the 
 * "coordination" benefit of a universal substrate is achieved by liquidating the 
 * subject's primary capacity for sovereign resilience. The high theater ratio (0.91)
 * captures the performative nature of "reliability dashboards" and "uptime reports"
 * that mask the systemic fragility and deep-seated dependency. The suppression score
 * (0.82) is high because the very invisibility of the infrastructure prevents
 * alternatives from being considered or developed.
 *
 * * PERSPECTIVAL GAP:
 * The End User feels a Snare because they are blind to the gears of their 
 * own survival. The Monopolist sees a Rope because the hidden nature 
 * coordinates a massive, frictionless scale of operation. The Analytical Observer
 * sees a Tangled Rope, recognizing both the coordination function and the
 * asymmetric, coercive extraction.
 *
 * * [RESOLVED MANDATROPHY]:
 * Resolved via the Piton and Tangled Rope classifications. For an analytical 
 * observer, the "Transparency Dashboard" is no longer functional (Theater 0.91); 
 * it is an inert spike siphoning 0.87 of the human species' autonomy. The Tangled
 * Rope classification correctly identifies the hybrid nature, preventing the system
 * from misclassifying it as a pure Snare (ignoring the coordination) or a pure
 * Rope (ignoring the extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_infrastructure_visibility,
    'Can "Public Auditing" restore the Rope, or is invisibility a physical law of complex systems (Snare vs Mountain)?',
    'Tracking the impact of mandatory open-source requirements on hidden infrastructure layers in 2026.',
    'If auditing restores agency: Snare of current design. If systems remain opaque: Mountain of Complexity.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(invisible_infrastructure_dependency, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint began as a useful coordination tool but degraded into a
% monopolistic trap as dependency deepened and transparency was replaced by theater.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(iid_tr_t0, invisible_infrastructure_dependency, theater_ratio, 0, 0.10).
narrative_ontology:measurement(iid_tr_t5, invisible_infrastructure_dependency, theater_ratio, 5, 0.55).
narrative_ontology:measurement(iid_tr_t10, invisible_infrastructure_dependency, theater_ratio, 10, 0.91).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(iid_ex_t0, invisible_infrastructure_dependency, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(iid_ex_t5, invisible_infrastructure_dependency, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(iid_ex_t10, invisible_infrastructure_dependency, base_extractiveness, 10, 0.87).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(invisible_infrastructure_dependency, global_infrastructure).

% Network relationships (structural influence edges)
% The dependency on this invisible substrate directly affects the stability
% and operational parameters of other critical systems.
narrative_ontology:affects_constraint(invisible_infrastructure_dependency, global_financial_clearing).
narrative_ontology:affects_constraint(invisible_infrastructure_dependency, semiconductor_supply).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */