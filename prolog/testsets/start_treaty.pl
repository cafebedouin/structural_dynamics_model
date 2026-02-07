% ============================================================================
% CONSTRAINT STORY: start_treaty
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-06
% ============================================================================

:- module(constraint_start_treaty, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: start_treaty
 * human_readable: START Treaty Expiration
 * domain: political
 * * SUMMARY:
 * The expiration of the New START treaty between the US and Russia removes legally binding limits on their nuclear arsenals for the first time in half a century. This creates uncertainty and increases the risk of a nuclear arms race.
 * * KEY AGENTS:
 * - Russia/USA: Subject (Powerless after treaty lapses)
 * - Global Community: Beneficiary (of arms control)
 * - Arms Manufacturers: Beneficiary (of treaty expiration)
 * - Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(start_treaty, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(start_treaty, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(start_treaty, 0.2).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(start_treaty, extractiveness, 0.55).
narrative_ontology:constraint_metric(start_treaty, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(start_treaty, theater_ratio, 0.2).

% Binary flags
% narrative_ontology:has_sunset_clause(start_treaty).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(start_treaty). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(start_treaty, global_community).
narrative_ontology:constraint_victim(start_treaty, global_community).
narrative_ontology:constraint_beneficiary(start_treaty, arms_manufacturers).

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
%
% UNIFORM-TYPE EXCEPTION: For natural law constraints (mountain-only) or pure
% coordination constraints (rope-only), perspectives 1 and 2 may use any power
% atoms — the classification is the same from all perspectives.  Include at
% least 2-3 perspectives to demonstrate the invariance.
constraint_indexing:constraint_classification(start_treaty, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(start_treaty, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(start_treaty, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, extraction <= 0.30, theater_ratio < 0.70.
% constraint_indexing:constraint_classification(start_treaty, scaffold,
%   context(agent_power(organized),
%           time_horizon(generational),
%           exit_options(constrained),
%           spatial_scope(continental))) :-
%   narrative_ontology:has_sunset_clause(start_treaty).

% PERSPECTIVE 5: THE SYSTEMS AUDITOR (PITON)
% Inertial maintenance of a non-functional constraint.
% constraint_indexing:constraint_classification(start_treaty, piton, 
%   context(agent_power(analytical), 
%           time_horizon(civilizational), 
%           exit_options(arbitrage), 
%           spatial_scope(universal))) :-
%   domain_priors:theater_ratio(start_treaty, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(start_treaty_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(start_treaty, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(start_treaty, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(start_treaty, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(start_treaty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The expiration of the START treaty is classified as a Tangled Rope.
 *
 * *Subject (Snare)*: From the perspective of the global community, particularly those vulnerable to nuclear conflict, the treaty's expiration feels like a loss of protection, a snare. They are trapped by the increased risk.
 * *Beneficiary (Piton)*: From the perspective of institutional players such as arms manufacturers and some political elements, the treaty is seen as a piton. It no longer serves its intended purpose of arms control effectively, yet the treaty has created an opportunity for expanded arsenals and military spending. The maintenance of existing weapons systems and related infrastructure provides income. This fits Piton characteristics: reduced function + theatrical maintenance.
 * *Analytical (Tangled Rope)*: From an analytical perspective, the treaty's expiration presents a complex situation. On one hand, it removes limits on nuclear arsenals (extraction). On the other hand, it eliminates a vital framework for communication and verification that reduces the risk of nuclear war, which could be considered a form of international coordination. requires active enforcement through mutual verification to maintain the coordination aspect. This meets all requirements for Tangled Rope classification.
 * *Perspectival Gap*: The perspectival gap arises because the global community values the stability and predictability provided by the treaty, while other parties, through inertia or self-interest, benefit from its expiration.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents the system from mislabeling this as pure extraction (Snare) by recognizing the presence of coordination (arms control verification) alongside the asymmetric extraction (arms buildup potential).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_start_treaty,
    'Will the expiration of the treaty trigger a new arms race?',
    'Monitoring weapons development and deployment by Russia and the US.',
    'If true, higher global instability and risk of nuclear conflict; if false, continued strategic ambiguity but without significant arms buildup.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(start_treaty, 0, 10).

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
narrative_ontology:measurement(start_treaty_tr_t0, start_treaty, theater_ratio, 0, 0.1).
narrative_ontology:measurement(start_treaty_tr_t5, start_treaty, theater_ratio, 5, 0.2).
narrative_ontology:measurement(start_treaty_tr_t10, start_treaty, theater_ratio, 10, 0.3).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(start_treaty_ex_t0, start_treaty, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(start_treaty_ex_t5, start_treaty, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(start_treaty_ex_t10, start_treaty, base_extractiveness, 10, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */