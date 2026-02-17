% ============================================================================
% CONSTRAINT STORY: maintenance_capacity_shortfall
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_maintenance_capacity_shortfall, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: maintenance_capacity_shortfall
 * human_readable: The Entropic Debt Trap
 * domain: infrastructure/logistical/technological
 * * SUMMARY:
 * A scenario where the physical or digital complexity of a system (Rope)
 * scales beyond the human or financial resources allocated for its upkeep.
 * This coordination substrate for modern life becomes a "Snare" for the
 * subject as the system's "Entropic Debt" liquidates their primary safety
 * and operational agency, trapping them in a territory of cascading
 * structural failures with no viable path to restoration.
 * * KEY AGENTS:
 * - Systems Operator: Subject (Powerless)
 * - Expansionist Developer: Beneficiary (Institutional)
 * - Infrastructure Decay Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.85) reflects the siphoning of maintenance reserves
% into the institutional "growth" or "innovation" budgets.
domain_priors:base_extractiveness(maintenance_capacity_shortfall, 0.85).
domain_priors:suppression_score(maintenance_capacity_shortfall, 0.73). % "Simple" or "Modular" alternatives are suppressed by the inertia of the complex legacy.
domain_priors:theater_ratio(maintenance_capacity_shortfall, 0.90).    % Extreme theater: "Reliability Engineering" dashboards masking actual systemic rot.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(maintenance_capacity_shortfall, extractiveness, 0.85).
narrative_ontology:constraint_metric(maintenance_capacity_shortfall, suppression_requirement, 0.73).
narrative_ontology:constraint_metric(maintenance_capacity_shortfall, theater_ratio, 0.90).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(maintenance_capacity_shortfall, piton).
narrative_ontology:human_readable(maintenance_capacity_shortfall, "The Entropic Debt Trap").

% Binary flags
domain_priors:requires_active_enforcement(maintenance_capacity_shortfall). % Required for Tangled Rope. Budgetary decisions actively enforce the neglect.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(maintenance_capacity_shortfall, expansionist_developer).
narrative_ontology:constraint_victim(maintenance_capacity_shortfall, systems_operator).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The operator is trapped: they must keep the system running, but the lack
% of capacity liquidates their agency to perform actual repair.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The developer views the shortfall as a Rope—the essential coordination
% substrate for maintaining "competitive growth" while ignoring the cost of decay.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.90) > 0.70 triggers Piton: the "Official Maintenance Schedule"
% is an inertial spike; it performatively signals care while 0.85 extraction continues.
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.85) masking as functional coordination (Rope).
constraint_indexing:constraint_classification(maintenance_capacity_shortfall, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(maintenance_shortfall_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless operator vs Rope for the institutional developer.
    constraint_indexing:constraint_classification(maintenance_capacity_shortfall, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(maintenance_capacity_shortfall, rope,
        context(agent_power(institutional), _, _, _)).

test(piton_trigger) :-
    % Ensure extreme theater (0.90) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(maintenance_capacity_shortfall, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_detection) :-
    % Ensure the analytical observer correctly identifies the Tangled Rope.
    constraint_indexing:constraint_classification(maintenance_capacity_shortfall, tangled_rope,
        context(agent_power(analytical), time_horizon(civilizational), _, _)).

:- end_tests(maintenance_shortfall_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.85) reflects a state where the "coordination"
 * benefit of mass-scale infrastructure is achieved by liquidating the
 * subject's primary capacity for sustainable maintenance. The high theater
 * ratio (0.90) shows that the rituals of maintenance (audits, reports) have
 * completely replaced the function of maintenance.
 *
 * * PERSPECTIVAL GAP:
 * The Systems Operator feels a Snare because they are fighting a war against
 * gravity with no ammunition. The Developer sees a Rope because the system
 * coordinates massive present-tense value while deferring the cost to the future.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint is a classic case of Mandatrophy, where a system with a
 * genuine coordination function becomes primarily extractive. The system
 * avoids misclassifying this as a pure Snare by using the Tangled Rope
 * classification. This acknowledges both the coordination benefit perceived
 * by the beneficiary (the system *does* enable large-scale operations) and
 * the asymmetric extraction felt by the victim (whose maintenance capacity
 * is being liquidated). The Piton classification further refines this, showing
 * how the system's self-regulation has become pure theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_decay_reversibility,
    'Can "Automated Repair" restore the Rope, or is decay a physical law of thermodynamics (Snare vs Mountain)?',
    'Tracking the success rate of robotic maintenance in high-entropy bridge and tunnel infrastructure in 2026.',
    'If robots restore systems: Snare of current technique. If entropy wins: Mountain of Physical Geometry.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(maintenance_capacity_shortfall, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This system began as a functional coordination mechanism (Rope) but decayed
% as maintenance funds were diverted to "growth", increasing both extraction
% and the theater required to mask the decay.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(mcs_tr_t0, maintenance_capacity_shortfall, theater_ratio, 0, 0.10).
narrative_ontology:measurement(mcs_tr_t5, maintenance_capacity_shortfall, theater_ratio, 5, 0.50).
narrative_ontology:measurement(mcs_tr_t10, maintenance_capacity_shortfall, theater_ratio, 10, 0.90).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(mcs_ex_t0, maintenance_capacity_shortfall, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(mcs_ex_t5, maintenance_capacity_shortfall, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(mcs_ex_t10, maintenance_capacity_shortfall, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(maintenance_capacity_shortfall, global_infrastructure).

% Network relationships (structural influence edges)
% The failure of core infrastructure has cascading effects on logistical networks.
narrative_ontology:affects_constraint(maintenance_capacity_shortfall, supply_chain_brittleness).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */