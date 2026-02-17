% ============================================================================
% CONSTRAINT STORY: asymmetric_burden_distribution
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_asymmetric_burden_distribution, []).

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
 * * constraint_id: asymmetric_burden_distribution
 * human_readable: The Externalization Lever
 * domain: economic/social/logistical
 * * SUMMARY:
 * A scenario where the systemic benefits of a process (e.g., global shipping,
 * digital convenience, urban density) are concentrated at the center, while
 * the operational burdens (noise, pollution, psychological stress, physical
 * maintenance) are pushed onto a peripheral population. This "Rope" for
 * achieving mass-market affordability becomes a "Snare" for the peripheral
 * subject, whose local agency and quality of life are liquidated to subsidize
 * the center's "frictionless" experience, trapping them in a territory of
 * involuntary sacrifice with no mechanism for cost-recapture.
 *
 * * KEY AGENTS:
 * - Peripheral Resident: Subject (Powerless)
 * - Central Beneficiary: Beneficiary (Institutional)
 * - Socio-Economic Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(asymmetric_burden_distribution, 0.89). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(asymmetric_burden_distribution, 0.81).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(asymmetric_burden_distribution, 0.86).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(asymmetric_burden_distribution, extractiveness, 0.89).
narrative_ontology:constraint_metric(asymmetric_burden_distribution, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(asymmetric_burden_distribution, theater_ratio, 0.86).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(asymmetric_burden_distribution, piton).
narrative_ontology:human_readable(asymmetric_burden_distribution, "The Externalization Lever").

% Binary flags
domain_priors:requires_active_enforcement(asymmetric_burden_distribution). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(asymmetric_burden_distribution, central_beneficiary).
narrative_ontology:constraint_victim(asymmetric_burden_distribution, peripheral_resident).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The resident is trapped: they cannot move due to economic constraints,
% yet remaining liquidates their health and property value to serve the center.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The central institution views the distribution as a Rope—the only way to
% coordinate a competitive, low-cost service for the "greater good."
constraint_indexing:constraint_classification(asymmetric_burden_distribution, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.86) > 0.70 triggers Piton: the "Environmental Mitigation Plan"
% is an inertial spike; it performatively addresses the burden while permitting the extraction.
constraint_indexing:constraint_classification(asymmetric_burden_distribution, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects high extraction (0.89) masking as essential coordination (Rope).
constraint_indexing:constraint_classification(asymmetric_burden_distribution, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(asymmetric_burden_distribution, E), E >= 0.50,
    domain_priors:suppression_score(asymmetric_burden_distribution, S), S > 0.40,
    domain_priors:requires_active_enforcement(asymmetric_burden_distribution).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(asymmetric_burden_distribution_tests).

test(perspectival_gap) :-
    % Verify Snare for the peripheral subject vs Rope for the central beneficiary.
    constraint_indexing:constraint_classification(asymmetric_burden_distribution, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(asymmetric_burden_distribution, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(asymmetric_burden_distribution, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.86) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(asymmetric_burden_distribution, piton,
        context(agent_power(analytical), _, _, _)).

test(extraction_threshold) :-
    % Verify that the base extraction score correctly exceeds the Mandatrophy threshold.
    domain_priors:base_extractiveness(asymmetric_burden_distribution, E),
    E > 0.70.

:- end_tests(asymmetric_burden_distribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extreme extraction score (0.89) and suppression (0.81) model a system
 * where the survival agency and environmental quality of one group (peripheral
 * residents) are systematically liquidated to subsidize the convenience and
 * economic efficiency of another (central beneficiaries). The high theater
 * ratio (0.86) reflects performative mitigation efforts (e.g., "community
 * funds," "environmental reviews") that create a facade of fairness while
 * failing to alter the underlying extractive logic.
 *
 * * PERSPECTIVAL GAP:
 * The gap is profound. For the institutional beneficiary, the system is a
 * Rope, a necessary coordination mechanism for delivering goods and services
 * at scale. For the powerless subject, it is a Snare, trapping them in a
 * degraded environment with no recourse.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Mandatrophy (extraction > 0.70) is resolved not by a single
 * classification but by the system's ability to hold multiple, contradictory
 * classifications simultaneously. The analytical observer sees both a Piton
 * (the failed, theatrical mitigation policy) and a Tangled Rope (the overall
 * system of coercive coordination). This prevents a monolithic "Snare"
 * classification that would ignore the genuine (though deeply asymmetric)
 * coordination function that benefits the institutional agent. The system
 * correctly identifies a pathological coordination mechanism, not just simple
 * predation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_burden_recapture,
    'Can cost-internalization (e.g., Pigouvian taxes) restore the Rope, or is asymmetry a physical law of scaling (Snare vs Mountain)?',
    'Tracking the success rate of local "sovereignty zones" in recapturing extraction value over 15 years.',
    'If recapture succeeds: Snare of current policy. If it fails: Mountain of Logistical Entropy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(asymmetric_burden_distribution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Required for high-extraction constraints (base_extractiveness > 0.46).
%
% Theater ratio over time (triggers metric_substitution detection):
% Rising from functional mitigation (0.25) to inertial "Community Grant"
% theater (0.86) as the burden scales.
narrative_ontology:measurement(abd_tr_t0, asymmetric_burden_distribution, theater_ratio, 0, 0.25).
narrative_ontology:measurement(abd_tr_t5, asymmetric_burden_distribution, theater_ratio, 5, 0.53).
narrative_ontology:measurement(abd_tr_t10, asymmetric_burden_distribution, theater_ratio, 10, 0.86).

% Extraction over time (triggers extraction_accumulation detection):
% Progressive accumulation of environmental/social costs forced onto the
% peripheral subject.
narrative_ontology:measurement(abd_ex_t0, asymmetric_burden_distribution, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(abd_ex_t5, asymmetric_burden_distribution, base_extractiveness, 5, 0.67).
narrative_ontology:measurement(abd_ex_t10, asymmetric_burden_distribution, base_extractiveness, 10, 0.89).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Valid types: information_standard, resource_allocation,
%              enforcement_mechanism, global_infrastructure
narrative_ontology:coordination_type(asymmetric_burden_distribution, resource_allocation).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(asymmetric_burden_distribution, 0.0).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(asymmetric_burden_distribution, [other_constraint_id]).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */