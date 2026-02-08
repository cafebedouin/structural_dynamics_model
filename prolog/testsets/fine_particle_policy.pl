% ============================================================================
% CONSTRAINT STORY: fine_particle_policy
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_fine_particle_policy, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: fine_particle_policy
 * human_readable: Dutch Fine Particle Policy
 * domain: political
 * * SUMMARY:
 * The Dutch government implements a policy to reduce fine particle emissions, impacting farmers significantly due to livestock emissions. The policy aims to improve air quality and public health but imposes restrictions and costs on farmers, especially those near Natura 2000 areas. This policy presents a conflict between environmental goals and agricultural livelihoods.
 * * KEY AGENTS:
 * - Farmers: Subject (Powerless)
 * - Government: Beneficiary (Institutional)
 * - Environmental Scientists: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(fine_particle_policy, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(fine_particle_policy, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(fine_particle_policy, 0.20).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(fine_particle_policy, extractiveness, 0.55).
narrative_ontology:constraint_metric(fine_particle_policy, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(fine_particle_policy, theater_ratio, 0.20).

% Constraint classification claim
narrative_ontology:constraint_claim(fine_particle_policy, tangled_rope).

% Binary flags
% narrative_ontology:has_sunset_clause(fine_particle_policy).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(fine_particle_policy). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(fine_particle_policy, "General Public").
narrative_ontology:constraint_victim(fine_particle_policy, "Farmers").

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE/MOUNTAIN)
% High extraction felt as an immutable limit or predatory trap.
% NOTE: This may be upgraded to 'organized' power if a critical mass of victims exists.
constraint_indexing:constraint_classification(fine_particle_policy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(fine_particle_policy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(fine_particle_policy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fine_particle_policy_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(fine_particle_policy, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fine_particle_policy, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(fine_particle_policy, ExtMetricName, E),
    E >= 0.46. % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(fine_particle_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The fine particle policy scores high on extractiveness due to the direct financial and operational impact on farmers. Suppression is high because the policy limits their options and requires significant changes to their practices. The perspectival gap arises because the government (institutional) views the policy as a necessary measure for public health, while farmers (powerless) perceive it as a threat to their livelihood. The analytical observer sees it as a Tangled Rope because it coordinates public health improvement but extracts significantly from farmers, requiring active enforcement.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents mislabeling as pure extraction by acknowledging the coordination benefit of improved air quality for the general public. The policy is not purely extractive because it aims to solve a collective action problem (air pollution) even though the costs are disproportionately borne by farmers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_fine_particle_policy,
    'To what extent are the emission reduction targets achievable without disproportionately impacting farmers?',
    'Longitudinal study of policy implementation and its economic impact on farms.',
    'If achievable, policy is more justifiable. If not, requires significant compensation or alternative solutions.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(fine_particle_policy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Model how the constraint intensified or changed across the interval.
%
% Required for high-extraction constraints (base_extractiveness > 0.46).
% Use at least 3 time points (T=0, midpoint, T=end) for each tracked metric.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(fine_particle_policy_tr_t0, fine_particle_policy, theater_ratio, 0, 0.10).
narrative_ontology:measurement(fine_particle_policy_tr_t5, fine_particle_policy, theater_ratio, 5, 0.20).
narrative_ontology:measurement(fine_particle_policy_tr_t10, fine_particle_policy, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(fine_particle_policy_ex_t0, fine_particle_policy, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(fine_particle_policy_ex_t5, fine_particle_policy, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(fine_particle_policy_ex_t10, fine_particle_policy, base_extractiveness, 10, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */