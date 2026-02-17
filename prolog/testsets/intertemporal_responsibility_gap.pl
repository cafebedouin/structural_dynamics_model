% ============================================================================
% CONSTRAINT STORY: intertemporal_responsibility_gap
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_intertemporal_responsibility_gap, []).

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
 * * constraint_id: intertemporal_responsibility_gap
 * human_readable: The Generational Accountability Void
 * domain: environmental/economic/technological
 * * SUMMARY:
 * A scenario where the benefits of a specific action (e.g., carbon extraction,
 * rapid AI scaling, national debt) are captured in the immediate term by
 * current actors, while the costs and systemic risks are deferred to future
 * generations. This "Rope" for current coordination and wealth creation
 * becomes a "Snare" for the future subject, whose survival agency is
 * liquidated by inherited ecological or economic debt, trapping them in
 * a territory they cannot repair and did not choose.
 * * KEY AGENTS:
 * - Future Citizen: Subject (Powerless)
 * - Current Decision-Maker: Beneficiary (Institutional)
 * - Intergenerational Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.92) reflects the total liquidation of future subjects'
% resources and agency to maintain current institutional standards.
domain_priors:base_extractiveness(intertemporal_responsibility_gap, 0.92).
domain_priors:suppression_score(intertemporal_responsibility_gap, 0.85). % Future interests are suppressed by lack of present representation.
domain_priors:theater_ratio(intertemporal_responsibility_gap, 0.88).    % High theater: "Legacy planning" and "Greenwashing" rituals masking terminal extraction.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(intertemporal_responsibility_gap, extractiveness, 0.92).
narrative_ontology:constraint_metric(intertemporal_responsibility_gap, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(intertemporal_responsibility_gap, theater_ratio, 0.88).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(intertemporal_responsibility_gap, tangled_rope).
narrative_ontology:human_readable(intertemporal_responsibility_gap, "The Generational Accountability Void").

% Binary flags
domain_priors:requires_active_enforcement(intertemporal_responsibility_gap). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(intertemporal_responsibility_gap, current_decision_makers).
narrative_ontology:constraint_victim(intertemporal_responsibility_gap, future_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The future citizen is trapped: they inherit a hollowed-out territory
% with terminal debt, liquidating their ability to pursue novel goals.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The current actor views the gap as a Rope—the essential coordination
% substrate for maintaining present stability and competitive growth.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects extreme extraction (0.92) across time horizons as a hybrid.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 4: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.88) > 0.70 triggers Piton: the "Long-Term Sustainability"
% board is an inertial spike; it signals concern while permitting 0.92 extraction.
constraint_indexing:constraint_classification(intertemporal_responsibility_gap, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(intertemporal_responsibility_gap, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(intertemporal_responsibility_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless future vs Rope for the current institution.
    constraint_indexing:constraint_classification(intertemporal_responsibility_gap, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(intertemporal_responsibility_gap, rope,
        context(agent_power(institutional), _, _, _)),
    snare \= rope.

test(piton_trigger) :-
    % Ensure high theater ratio (0.88) correctly triggers the Piton classification.
    domain_priors:theater_ratio(intertemporal_responsibility_gap, TR),
    TR > 0.70,
    constraint_indexing:constraint_classification(intertemporal_responsibility_gap, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_conditions_met) :-
    % Verify the analytical observer correctly classifies this as a Tangled Rope.
    constraint_indexing:constraint_classification(intertemporal_responsibility_gap, tangled_rope,
        context(agent_power(analytical), time_horizon(civilizational), _, _)).

:- end_tests(intertemporal_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.92) reflects a "Mandatrophy" state where the
 * "coordination" benefit of current growth is achieved by liquidating
 * the entire resource and agency base of future generations. The suppression
 * score (0.85) is high because future subjects have no representation or
 * recourse in present-day decision-making. The high theater ratio (0.88)
 * comes from performative acts like "sustainability reports" and "legacy
 * planning" that mask the ongoing liquidation.
 *
 * * PERSPECTIVAL GAP:
 * The Future Citizen experiences a Snare because they are born into a
 * "bankruptcy" they did not cause and cannot escape. The Current
 * Decision-Maker sees a Rope because deferring costs is the only
 * coordination mechanism that prevents immediate social collapse or
 * stagnation in their biographical time horizon.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The system resolves this extreme extraction by refusing a simple Snare
 * classification from an analytical perspective. Instead, it uses the
 * Tangled Rope and Piton classifications. The Tangled Rope acknowledges the
 * genuine (if perverse) coordination function for the beneficiaries, while
 * simultaneously flagging the extreme asymmetric extraction. The Piton
 * classification correctly identifies the "long-term planning" apparatus
 * as non-functional theater, an inert remnant of a system that once may
 * have had a genuine purpose but now only serves to enable extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_intergenerational_legal_standing,
    'Can the "future" ever be granted a present legal Rope, or is time an absolute Snare (Snare vs Mountain)?',
    'Tracking the success rate of "Nature-as-Subject" or "Future Generations" lawsuits in national courts.',
    'If lawsuits win: Snare of current legal design. If they fail: Mountain of Temporal Physics.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(intertemporal_responsibility_gap, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has intensified over time. Initially a problematic but
% manageable issue, it has degraded into a terminal state of extraction.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(ir_gap_tr_t0, intertemporal_responsibility_gap, theater_ratio, 0, 0.40).
narrative_ontology:measurement(ir_gap_tr_t5, intertemporal_responsibility_gap, theater_ratio, 5, 0.75).
narrative_ontology:measurement(ir_gap_tr_t10, intertemporal_responsibility_gap, theater_ratio, 10, 0.88).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(ir_gap_ex_t0, intertemporal_responsibility_gap, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(ir_gap_ex_t5, intertemporal_responsibility_gap, base_extractiveness, 5, 0.80).
narrative_ontology:measurement(ir_gap_ex_t10, intertemporal_responsibility_gap, base_extractiveness, 10, 0.92).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The constraint functions by allocating resources (and costs) across time.
narrative_ontology:coordination_type(intertemporal_responsibility_gap, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */