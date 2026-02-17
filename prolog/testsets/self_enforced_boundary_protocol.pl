% ============================================================================
% CONSTRAINT STORY: boundary_protocol
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-23
% ============================================================================

:- module(constraint_boundary_protocol, []).

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
 * * constraint_id: boundary_protocol
 * human_readable: The Self-Enforced Boundary Protocol
 * domain: psychological/social
 * * SUMMARY:
 * A boundary is defined not as a request for another's change, but as an
 * internal action protocol: "If X happens, I will do Y." It requires the
 * other person to do nothing, effectively moving the 'exit option'
 * entirely into the Subject's hands. This transforms a potentially coercive
 * social dynamic into a tool for self-regulation.
 * * KEY AGENTS:
 * - The Voyager: Subject (Powerful) - Defines and enacts their own perimeter.
 * - The External: The party whose behavior triggers the boundary (Powerless).
 * - The Therapeutic System: Beneficiary (Institutional) - Promotes this protocol as a healthy norm.
 * - The Auditor: Auditor (Analytical) - Observing the stability of the interaction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(boundary_protocol, 0.0). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(boundary_protocol, 0.10).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(boundary_protocol, 0.05).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(boundary_protocol, extractiveness, 0.0).
narrative_ontology:constraint_metric(boundary_protocol, suppression_requirement, 0.10).
narrative_ontology:constraint_metric(boundary_protocol, theater_ratio, 0.05).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(boundary_protocol, scaffold).
narrative_ontology:human_readable(boundary_protocol, "The Self-Enforced Boundary Protocol").

% Binary flags
narrative_ontology:has_sunset_clause(boundary_protocol).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(boundary_protocol). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(boundary_protocol, the_voyager).
% narrative_ontology:constraint_victim(boundary_protocol, [victim_group]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE EXTERNAL (MOUNTAIN)
% To the person whose behavior triggers the boundary, it appears as an
% immovable law of nature—they cannot change it through negotiation.
constraint_indexing:constraint_classification(boundary_protocol, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE THERAPEUTIC SYSTEM (ROPE)
% From an institutional perspective that teaches this as a healthy norm,
% the protocol is pure coordination, enabling stable and predictable interactions.
constraint_indexing:constraint_classification(boundary_protocol, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SCAFFOLD)
% Initially, the boundary acts as a temporary support structure for the Voyager
% until the new, healthier dynamic becomes automatic and the explicit rule is no longer needed.
constraint_indexing:constraint_classification(boundary_protocol, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(regional))) :-
    narrative_ontology:has_sunset_clause(boundary_protocol).

% PERSPECTIVE 4: THE VOYAGER (ROPE)
% The individual enacting the boundary experiences it as a tool for
% self-coordination and autonomy, a Rope they use to manage their own state.
constraint_indexing:constraint_classification(boundary_protocol, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(boundary_protocol_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between the External (powerless) and the System (institutional).
    constraint_indexing:constraint_classification(boundary_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(boundary_protocol, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == mountain,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(threshold_validation_low_extraction) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(boundary_protocol, ExtMetricName, E),
    E =< 0.15. % Validates it falls in the Rope/Mountain/Scaffold range.

test(scaffold_requirements_met) :-
    % Verify that the conditions for a Scaffold classification are met.
    constraint_indexing:constraint_classification(boundary_protocol, scaffold, _),
    narrative_ontology:has_sunset_clause(boundary_protocol),
    narrative_ontology:constraint_beneficiary(boundary_protocol, _).

:- end_tests(boundary_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models the psychological technology of a self-enforced boundary.
 * The key insight is that its classification depends entirely on perspective. For
 * the person enacting it (The Voyager), it's a Rope for self-coordination. For
 * the person encountering it (The External), it's a Mountain—an unchangeable
 * feature of their environment. The base extractiveness is zero because the
 * protocol's purpose is to *stop* the extraction of the Voyager's emotional
 * or mental resources, not to gain new ones.
 *
 * The Scaffold classification from the analytical view captures the temporary
 * nature of such a conscious rule. It's a support structure that, if successful,
 * becomes an unconscious, integrated part of a relationship, at which point
 * the explicit "scaffold" is no longer needed.
 *
 * * MANDATROPHY ANALYSIS:
 * This constraint has zero base extraction, so mandatrophy is not a concern.
 * It represents the opposite: a tool to dismantle an existing, unstated Snare
 * (e.g., codependency) by converting it into a transparent, self-managed Rope.
 * The system correctly avoids mislabeling this as extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_boundary_protocol,
    'Is the boundary a genuine self-regulation tool (Rope/Scaffold) or a passive-aggressive control tactic (a disguised Snare)?',
    'Observing the Voyager''s behavior. Do they use the boundary to create space and stability, or to punish and manipulate the External?',
    'If it is a control tactic, the base_extractiveness is non-zero (emotional energy from the External) and the classification becomes a Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(boundary_protocol, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data is not strictly required as base_extractiveness < 0.46, but
% is included to model the narrative arc of the constraint's lifecycle.
%
% Theater ratio over time (drops as the boundary moves from "stated" to "integrated"):
narrative_ontology:measurement(boundary_protocol_tr_t0, boundary_protocol, theater_ratio, 0, 0.40).
narrative_ontology:measurement(boundary_protocol_tr_t5, boundary_protocol, theater_ratio, 5, 0.20).
narrative_ontology:measurement(boundary_protocol_tr_t10, boundary_protocol, theater_ratio, 10, 0.05).

% Extraction over time (remains at zero as the Subject refuses to be "siphoned"):
narrative_ontology:measurement(boundary_protocol_ex_t0, boundary_protocol, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(boundary_protocol_ex_t5, boundary_protocol, base_extractiveness, 5, 0.0).
narrative_ontology:measurement(boundary_protocol_ex_t10, boundary_protocol, base_extractiveness, 10, 0.0).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The protocol acts as a standard for one's own behavior in response to external stimuli.
narrative_ontology:coordination_type(boundary_protocol, information_standard).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(boundary_protocol, [0.0-1.0]).

% Network relationships (structural influence edges)
% Declare when constraints share regulatory domain, causal dependency,
% or institutional coupling.
% narrative_ontology:affects_constraint(boundary_protocol, [other_constraint_id]).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */