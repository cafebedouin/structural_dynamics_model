% ============================================================================
% CONSTRAINT STORY: social_loafing
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_social_loafing, []).

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
 * * constraint_id: social_loafing
 * human_readable: Social Loafing (The Ringelmann Effect)
 * domain: social/economic
 * * SUMMARY:
 * Social loafing is the phenomenon where individuals exert less effort when working
 * collectively than when working individually. This diffusion of responsibility
 * creates a system where diligent members' efforts are extracted to subsidize
 * low-effort members, while overall group potential is suppressed. The constraint
 * is the group structure that enables and masks this behavior.
 * * KEY AGENTS:
 * - Diligent Contributor: Subject (Powerless), whose extra labor is extracted.
 * - Low-Effort Contributor ("Loafer"): Beneficiary (Institutional, in the sense of benefiting from the institutional structure).
 * - Social Psychologist: Auditor (Analytical), observing the system's properties.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(social_loafing, 0.55). % Mountain <= 0.15, Rope <= 0.15, Snare >= 0.46
domain_priors:suppression_score(social_loafing, 0.45).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(social_loafing, 0.1).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(social_loafing, extractiveness, 0.55).
narrative_ontology:constraint_metric(social_loafing, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(social_loafing, theater_ratio, 0.1).

% Constraint self-claim (what does the constraint claim to be?)
% The system of group-based rewards claims to be a coordination mechanism, but
% its effect is one of enforcement of a collective outcome, masking extraction.
narrative_ontology:constraint_claim(social_loafing, tangled_rope).
narrative_ontology:human_readable(social_loafing, "Social Loafing (The Ringelmann Effect)").

% Binary flags
% The group structure that enables loafing requires the "enforcement" of a
% collective reward/punishment (e.g., a group grade) to function.
domain_priors:requires_active_enforcement(social_loafing).

% Structural property derivation hooks:
% Beneficiary: The "Loafer" who saves energy while reaping group rewards.
narrative_ontology:constraint_beneficiary(social_loafing, low_effort_agents).
% Victim: The organization (lost output) and the diligent individual (overworked).
narrative_ontology:constraint_victim(social_loafing, high_effort_agents).
narrative_ontology:constraint_victim(social_loafing, the_organization).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE DILIGENT CONTRIBUTOR (SNARE)
% For the high-performer, the system is a trap that extracts their uncredited
% labor to compensate for others, strangling their individual potential.
constraint_indexing:constraint_classification(social_loafing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE LOW-EFFORT CONTRIBUTOR (ROPE)
% For the "loafer," the system is a pure coordination mechanism that allows
% them to achieve a goal with minimal energy expenditure. They feel no extraction.
% Power is 'institutional' as they benefit from the default institutional structure.
constraint_indexing:constraint_classification(social_loafing, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The system has a genuine coordination function (achieving a group goal) but
% also facilitates clear, asymmetric extraction from one subgroup to another,
% and requires the enforcement of a collective outcome. This is the definition
% of a Tangled Rope.
constraint_indexing:constraint_classification(social_loafing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(social_loafing_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between the diligent (powerless) and the loafer (institutional).
    constraint_indexing:constraint_classification(social_loafing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(social_loafing, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    assertion(TypePowerless \= TypeInstitutional).

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(social_loafing, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_for_high_extraction) :-
    % Verify the base extractiveness is in the high-extraction range.
    narrative_ontology:constraint_metric(social_loafing, extractiveness, E),
    assertion(E >= 0.46).

:- end_tests(social_loafing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness (0.55) and suppression (0.45) are set to reflect a
 * system that significantly degrades potential and extracts value. The key
 * insight is the perspectival gap: the diligent worker sees a Snare (their
 * labor is stolen), the loafer sees a Rope (an efficient way to get a reward),
 * and the analyst sees a Tangled Rope. The analytical view is not Mountain
 * because social loafing is not a law of physics; it is a behavioral tendency
 * enabled by specific, constructed institutional arrangements (e.g., group
 * grading) that can be modified.
 *
 * * MANDATROPHY ANALYSIS:
 * Classifying this as a Tangled Rope is critical. A pure Snare classification
 * would miss the fact that there is a genuine (if inefficient) coordination
 * goal. A Rope classification would ignore the severe asymmetric extraction.
 * The Tangled Rope classification correctly identifies the hybrid nature of
 * the problem: a coordination system that has been co-opted for extraction,
 * preventing misdiagnosis and pointing toward solutions that address both
 * accountability (the extraction) and group cohesion (the coordination).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_social_loafing,
    'Is social loafing a fixed cognitive bias (Mountain-like) or a strategic response to incentive structures (Tangled Rope)?',
    'Neuroeconomic studies comparing effort expenditure under conditions of perfect individual monitoring versus total anonymity.',
    'If a fixed bias, mitigation is about managing an immutable flaw. If strategic, it can be eliminated by redesigning incentive systems.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(social_loafing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Social loafing is a persistent, stable phenomenon within a given institutional
% structure. The measurements are therefore flat, showing no significant drift
% over the interval. This is required as base_extractiveness > 0.46.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(social_loafing_tr_t0, social_loafing, theater_ratio, 0, 0.1).
narrative_ontology:measurement(social_loafing_tr_t5, social_loafing, theater_ratio, 5, 0.1).
narrative_ontology:measurement(social_loafing_tr_t10, social_loafing, theater_ratio, 10, 0.1).

% Extraction over time (stable and high):
narrative_ontology:measurement(social_loafing_ex_t0, social_loafing, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(social_loafing_ex_t5, social_loafing, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(social_loafing_ex_t10, social_loafing, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Social loafing is a failure mode in the allocation of labor resources within a group.
narrative_ontology:coordination_type(social_loafing, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */