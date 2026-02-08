% ============================================================================
% CONSTRAINT STORY: adversarial_surface_inflation
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_adversarial_surface_inflation, []).

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
 * * constraint_id: adversarial_surface_inflation
 * human_readable: The Infinite Vulnerability Horizon
 * domain: technological/cybernetic/security
 * * SUMMARY:
 * A scenario where the increasing complexity and interconnectedness of
 * digital systems (Rope) creates a non-linear expansion of possible attack
 * vectors. This "Rope" for achieving massive integration and feature-rich
 * functionality becomes a "Snare" for the defender, as the "surface area"
 * to be protected grows faster than the human or financial capacity to
 * monitor it, liquidating the subject's primary security agency and
 * trapping them in a state of perpetual "reactive firefighting" against
 * an exponentially growing threat landscape.
 * * KEY AGENTS:
 * - Cybersecurity Defender: Subject (Powerless)
 * - Hyper-Connectivity Architect: Beneficiary (Institutional)
 * - Information Forensic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.84) reflects the liquidation of the defender's proactive
% surplus into the maintenance of a fundamentally unsecurable infrastructure.
domain_priors:base_extractiveness(adversarial_surface_inflation, 0.84).
domain_priors:suppression_score(adversarial_surface_inflation, 0.72). % Alternatives (e.g., air-gapping, simplification) are suppressed by market/operational mandates.
domain_priors:theater_ratio(adversarial_surface_inflation, 0.89).    % High theater: "Compliance Dashboards" that performatively signal safety while the surface area balloons.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(adversarial_surface_inflation, extractiveness, 0.84).
narrative_ontology:constraint_metric(adversarial_surface_inflation, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(adversarial_surface_inflation, theater_ratio, 0.89).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(adversarial_surface_inflation, coordination).

% Binary flags and structural properties for Tangled Rope classification
domain_priors:requires_active_enforcement(adversarial_surface_inflation).
narrative_ontology:constraint_beneficiary(adversarial_surface_inflation, hyper_connectivity_architects).
narrative_ontology:constraint_victim(adversarial_surface_inflation, cybersecurity_defenders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The defender is trapped: to be functional, they must use the inflated
% surface area, but protecting it liquidates their time and strategic agency.
constraint_indexing:constraint_classification(adversarial_surface_inflation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The architect views the inflation as a Rope—the essential coordination
% substrate for building a "Total Connectivity" economy where everything
% communicates at scale.
constraint_indexing:constraint_classification(adversarial_surface_inflation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.84) and active enforcement masking as essential coordination.
constraint_indexing:constraint_classification(adversarial_surface_inflation, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

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
    constraint_indexing:constraint_classification(adversarial_surface_inflation, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(adversarial_surface_inflation, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(adversarial_surface_inflation, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.89) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(adversarial_surface_inflation, piton,
        context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Ensures it's a high-extraction constraint.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(adversarial_surface_inflation, ExtMetricName, E),
    (E =< 0.15 -> fail ; E >= 0.46).

:- end_tests(adversarial_surface_inflation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score of 0.84 represents a "Mandatrophy" state where the supposed
 * coordination benefit of hyper-connectivity is achieved by liquidating the
 * defender's capacity for sovereign security. The defender's entire surplus of
 * time, budget, and strategic agency is consumed by the reactive need to defend
 * an ever-expanding, fundamentally insecure surface. The high theater ratio (0.89)
 * reflects performative security measures like compliance dashboards that create
 * an illusion of control.
 *
 * PERSPECTIVAL GAP:
 * The Cybersecurity Defender experiences a Snare because their environment is
 * indefensible by design, trapping them in a cycle of reactive firefighting.
 * The Hyper-Connectivity Architect, who benefits from the network effects of
 * total integration, views the system as a pure Rope, essential for coordinating
 * a global-scale digital economy. This gap is total: one agent's essential
 * infrastructure is another's inescapable trap.
 *
 * MANDATROPHY ANALYSIS:
 * The Mandatrophy is resolved by the Tangled Rope classification. This prevents
 * the system from making a naive classification of either pure Rope (ignoring
 * the 0.84 extraction) or pure Snare (ignoring the genuine, if costly,
 * coordination function). The Tangled Rope classification correctly identifies
 * the hybrid nature of the constraint: it simultaneously provides a coordination
 * function for its beneficiaries while imposing severe, asymmetric extraction on
 * its victims. This nuanced view is the resolution, as it acknowledges both
 * conflicting realities without dismissing either.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
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

% Theater ratio: Rising from functional monitoring (0.40) to inertial compliance theater (0.89).
narrative_ontology:measurement(asi_tr_t0, adversarial_surface_inflation, theater_ratio, 0, 0.40).
narrative_ontology:measurement(asi_tr_t5, adversarial_surface_inflation, theater_ratio, 5, 0.65).
narrative_ontology:measurement(asi_tr_t10, adversarial_surface_inflation, theater_ratio, 10, 0.89).

% Extraction: Non-linear expansion of the attack surface "extracting" all defensive agency.
narrative_ontology:measurement(asi_ex_t0, adversarial_surface_inflation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(asi_ex_t5, adversarial_surface_inflation, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(asi_ex_t10, adversarial_surface_inflation, base_extractiveness, 10, 0.84).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(adversarial_surface_inflation, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */