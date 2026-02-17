% ============================================================================
% CONSTRAINT STORY: coordination_threshold_failure
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_coordination_threshold_failure, []).

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
 * * constraint_id: coordination_threshold_failure
 * human_readable: The Critical Mass Chasm
 * domain: social/technological
 * * SUMMARY:
 * This constraint represents the structural "valley" where a network or
 * protocol provides zero utility until a specific participant threshold is
 * reached. It acts as a Snare for early adopters who pay the "participation
 * tax" without reward, while the system remains a Mountain of inertia for
 * the unaligned masses. The protocol's backers claim it is a temporary
 * scaffold, but its metrics reveal a highly extractive state.
 * * KEY AGENTS:
 * - Early Adopters: Subject (Powerless)
 * - Protocol Foundation: Beneficiary (Institutional)
 * - Network Economist: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.78) as early energy is consumed without generating
% network value, effectively "burning" the optionality of the first-movers.
domain_priors:base_extractiveness(coordination_threshold_failure, 0.78).
domain_priors:suppression_score(coordination_threshold_failure, 0.62).
domain_priors:theater_ratio(coordination_threshold_failure, 0.35).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(coordination_threshold_failure, extractiveness, 0.78).
narrative_ontology:constraint_metric(coordination_threshold_failure, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(coordination_threshold_failure, theater_ratio, 0.35).

% Constraint self-claim (what does the constraint claim to be?)
% The foundation claims it's a coordination mechanism.
narrative_ontology:constraint_claim(coordination_threshold_failure, tangled_rope).
narrative_ontology:human_readable(coordination_threshold_failure, "The Critical Mass Chasm").

% Binary flags
% The protocol claims it is temporary until the threshold is cleared.
narrative_ontology:has_sunset_clause(coordination_threshold_failure).
% Required for Tangled Rope: enforcement is via sunk costs (time, capital, reputation).
domain_priors:requires_active_enforcement(coordination_threshold_failure).

% Structural property derivation hooks:
% Required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(coordination_threshold_failure, protocol_foundation).
narrative_ontology:constraint_victim(coordination_threshold_failure, early_adopters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% To the early adopter, the lack of coordination is a snare: they are
% committed to a protocol that currently provides no exit or return.
constraint_indexing:constraint_classification(coordination_threshold_failure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The institution views this as a necessary Rope—a coordination ritual
% that filters for high-conviction participants to ensure long-term stability.
constraint_indexing:constraint_classification(coordination_threshold_failure, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The combination of a coordination function (beneficiary exists), asymmetric
% extraction (victim exists), and active enforcement, alongside high base
% extraction, yields a Tangled Rope classification.
constraint_indexing:constraint_classification(coordination_threshold_failure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coordination_threshold_failure_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(coordination_threshold_failure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coordination_threshold_failure, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(coordination_threshold_failure, tangled_rope,
        context(agent_power(analytical), _, _, _)).

:- end_tests(coordination_threshold_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.78) represents the high cost borne by early adopters
 * whose time, capital, and reputation are consumed by a network that has not
 * yet reached utility. The suppression score (0.62) reflects the high barrier
 * to entry for mass adoption and the sunk-cost trap for those already in.
 *
 * The key to this story is the conflict between the constraint's *claim* and its
 * *metrics*. The presence of a `has_sunset_clause` fact indicates the protocol
 * foundation *claims* this is a temporary Scaffold. However, the base
 * extractiveness of 0.78 is far too high for a Scaffold (which requires ε <= 0.30).
 * The analytical classification engine correctly ignores the claim and, based on
 * the high extraction, suppression, and presence of both beneficiaries and victims,
 * classifies it as a Tangled Rope.
 *
 * * [RESOLVED MANDATROPHY]:
 * Mandatrophy is resolved by the Tangled Rope classification. This acknowledges
 * the genuine (though currently unrealized) coordination goal of the protocol
 * while correctly identifying the severe, asymmetric extraction imposed on
 * its first victims. It avoids misclassifying the system as a pure Snare (which
 * would ignore the coordination goal) or a Scaffold (which would ignore the
 * punitive extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_coordination_threshold_failure,
    'Is the threshold a mathematical necessity for network function (Mountain) or an arbitrary, constructed barrier to maintain control and extract value (Snare)?',
    'Simulation of network utility at varying participant densities (N=10 to N=10^6) and analysis of the protocol''s tokenomics for founder incentives.',
    'If utility shows a sharp, non-linear step-function: Mountain. If utility scales more smoothly or if founder rewards are front-loaded: Tangled Rope/Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(coordination_threshold_failure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for high-extraction constraint (0.78 > 0.46).
% Models the pre-threshold phase where extraction is high and theater
% increases as the foundation tries to maintain momentum.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(ctf_tr_t0, coordination_threshold_failure, theater_ratio, 0, 0.15).
narrative_ontology:measurement(ctf_tr_t5, coordination_threshold_failure, theater_ratio, 5, 0.25).
narrative_ontology:measurement(ctf_tr_t10, coordination_threshold_failure, theater_ratio, 10, 0.35).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(ctf_ex_t0, coordination_threshold_failure, base_extractiveness, 0, 0.70).
narrative_ontology:measurement(ctf_ex_t5, coordination_threshold_failure, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(ctf_ex_t10, coordination_threshold_failure, base_extractiveness, 10, 0.78).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% A network protocol is a form of global infrastructure.
narrative_ontology:coordination_type(coordination_threshold_failure, global_infrastructure).

% Boltzmann floor override (only if domain knowledge justifies)
% Value must be in [0.0, 1.0]
% narrative_ontology:boltzmann_floor_override(coordination_threshold_failure, 0.0).

% Network relationships (structural influence edges)
% narrative_ontology:affects_constraint(coordination_threshold_failure, other_constraint_id).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */