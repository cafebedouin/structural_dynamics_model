% ============================================================================
% CONSTRAINT STORY: reputational_cascade_failure
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_reputational_cascade_failure, []).

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
 * * constraint_id: reputational_cascade_failure
 * human_readable: The Social Liquidity Trap
 * domain: social/economic/informational
 * * SUMMARY:
 * A scenario where a single piece of unverified but high-fitness negative
 * information triggers a self-reinforcing loop of social disinvestment.
 * This "Rope" for rapid group coordination and accountability becomes a
 * "Snare" for the individual, whose entire life-surplus (economic access,
 * social ties) is liquidated in seconds by an automated network reaction,
 * trapping them in a state of permanent exclusion with no path for
 * informational recovery or due process.
 * * KEY AGENTS:
 * - Targeted Individual: Subject (Powerless)
 * - Social Credit/Platform System: Beneficiary (Institutional)
 * - Network Dynamics Analyst: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.91) reflects the near-total liquidation of the subject's
% social and economic agency by the network.
domain_priors:base_extractiveness(reputational_cascade_failure, 0.91).
% Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:suppression_score(reputational_cascade_failure, 0.82). % Counter-evidence is suppressed by the velocity of the cascade.
domain_priors:theater_ratio(reputational_cascade_failure, 0.87).    % High theater: "Community Guidelines" and "Safety Checks" masking raw algorithmic enforcement.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(reputational_cascade_failure, extractiveness, 0.91).
narrative_ontology:constraint_metric(reputational_cascade_failure, suppression_requirement, 0.82).
narrative_ontology:constraint_metric(reputational_cascade_failure, theater_ratio, 0.87).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(reputational_cascade_failure, coordination).

% Binary flags
domain_priors:requires_active_enforcement(reputational_cascade_failure). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(reputational_cascade_failure, social_platform_operators).
narrative_ontology:constraint_victim(reputational_cascade_failure, targeted_individual).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The individual is trapped: the cascade moves faster than legal or
% logical intervention, liquidating their ability to exist within the system.
constraint_indexing:constraint_classification(reputational_cascade_failure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The platform/institution views the cascade as a Rope—the ultimate
% coordination tool for enforcing "consensus" and maintaining
% high-velocity social order.
constraint_indexing:constraint_classification(reputational_cascade_failure, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.87) > 0.70 triggers Piton: the "Appeals Process"
% is an inertial spike; it remains as an optic but lacks functional authority.
constraint_indexing:constraint_classification(reputational_cascade_failure, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects extreme extraction (0.91) masking as coordination (Rope).
constraint_indexing:constraint_classification(reputational_cascade_failure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(reputational_cascade_failure_tests).

test(perspectival_gap) :-
    % Verify Snare for the powerless subject vs Rope for the institutional platform.
    constraint_indexing:constraint_classification(reputational_cascade_failure, snare,
        context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(reputational_cascade_failure, rope,
        context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(reputational_cascade_failure, tangled_rope,
        context(agent_power(analytical), _, _, _)).

test(piton_trigger) :-
    % Ensure high theater ratio (0.87) correctly triggers the Piton classification.
    constraint_indexing:constraint_classification(reputational_cascade_failure, piton,
        context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify that all three structural requirements for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(reputational_cascade_failure, _), % derives has_coordination_function
    narrative_ontology:constraint_victim(reputational_cascade_failure, _),     % derives has_asymmetric_extraction
    domain_priors:requires_active_enforcement(reputational_cascade_failure).

:- end_tests(reputational_cascade_failure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.91) reflects a state where the "coordination"
 * benefit of social accountability is achieved by liquidating the subject's
 * entire life-territory. The suppression score (0.82) represents the
 * algorithmic suppression of counter-narratives, which cannot propagate
 * as quickly as the initial high-fitness negative signal. The high theater
 * ratio (0.87) points to performative "appeals processes" that have no
 * causal power over the automated cascade, classifying the system as a Piton
 * from one analytical view.
 *
 * * PERSPECTIVAL GAP:
 * The Targeted Individual feels a Snare because the network consumes them
 * based on a "hallucination" of consensus. The Platform sees a Rope because
 * the cascade coordinates massive behavioral alignment at near-zero marginal cost.
 *
 * * MANDATROPHY ANALYSIS:
 * The extreme extraction (0.91) creates a Mandatrophy risk: the system might
 * misclassify this as a pure Snare, ignoring its coordination function, or
 * as a pure Rope, ignoring the devastating extraction. The Tangled Rope
 * classification resolves this by acknowledging both properties simultaneously.
 * It correctly identifies a mechanism that provides a genuine coordination
 * benefit to one group (the platform) while imposing asymmetric, life-liquidating
 * extraction on another (the individual), maintained by active (algorithmic)
 * enforcement. This prevents the system from collapsing the analysis into a
 * simplistic good/bad binary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for high-extraction constraints (> 0.46).
omega_variable(
    omega_cascade_reversal,
    'Can a reputation be "un-liquidated" once the network commits, or is the Snare terminal (Snare vs Mountain)?',
    'Tracking the success rate of public exoneration in restoring economic access scores.',
    'If recovery fails: Mountain of Permanent Stigma. If recovery holds: Snare of current system design.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(reputational_cascade_failure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This system evolved from a genuine community tool to an automated,
% extractive enforcement mechanism.
%
% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(rcf_tr_t0, reputational_cascade_failure, theater_ratio, 0, 0.20).
narrative_ontology:measurement(rcf_tr_t5, reputational_cascade_failure, theater_ratio, 5, 0.65).
narrative_ontology:measurement(rcf_tr_t10, reputational_cascade_failure, theater_ratio, 10, 0.87).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(rcf_ex_t0, reputational_cascade_failure, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(rcf_ex_t5, reputational_cascade_failure, base_extractiveness, 5, 0.82).
narrative_ontology:measurement(rcf_ex_t10, reputational_cascade_failure, base_extractiveness, 10, 0.91).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(reputational_cascade_failure, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */