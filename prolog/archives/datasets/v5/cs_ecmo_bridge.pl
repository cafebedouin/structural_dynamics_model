% ============================================================================
% CONSTRAINT STORY: cs_ecmo_bridge
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-11
% ============================================================================

:- module(constraint_cs_ecmo_bridge, []).

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
 * * constraint_id: cs_ecmo_bridge
 * human_readable: ECMO Bridge to Transplant
 * domain: technological
 * * SUMMARY:
 * A new artificial lung system allows surgeons to keep a patient alive for 48 hours before a lung transplant. This acts as a bridge, providing crucial time to find a suitable organ and perform the complex surgery.
 * * KEY AGENTS:
 * - Patient: Subject (Powerless)
 * - Hospital/Surgeons: Beneficiary (Institutional)
 * - Society: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(cs_ecmo_bridge, 0.55). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(cs_ecmo_bridge, 0.30).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(cs_ecmo_bridge, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(cs_ecmo_bridge, extractiveness, 0.55).
narrative_ontology:constraint_metric(cs_ecmo_bridge, suppression_requirement, 0.30).
narrative_ontology:constraint_metric(cs_ecmo_bridge, theater_ratio, 0.10).

% Binary flags
% narrative_ontology:has_sunset_clause(cs_ecmo_bridge).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(cs_ecmo_bridge). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(cs_ecmo_bridge, hospital).
narrative_ontology:constraint_victim(cs_ecmo_bridge, patient).

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
constraint_indexing:constraint_classification(cs_ecmo_bridge, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(cs_ecmo_bridge, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(cs_ecmo_bridge, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE ARCHITECT (SCAFFOLD)
% Temporary coordination that expires over time.
% Requires: has_sunset_clause declared, extraction <= 0.30, theater_ratio < 0.70.
% The ECMO has a *de facto* sunset clause due to needing a transplant to complete the treatment.
% Since the narrative does not specify a literal sunset clause, the has_sunset_clause declaration is omitted.
% If the policy implied a sunset, this clause should be added.
% constraint_indexing:constraint_classification(cs_ecmo_bridge, scaffold,
%    context(agent_power(organized),
%            time_horizon(generational),
%            exit_options(constrained),
%            spatial_scope(continental))) :-
%    narrative_ontology:has_sunset_clause(cs_ecmo_bridge).

% PERSPECTIVE 5: THE SYSTEMS AUDITOR (PITON)
% Inertial maintenance of a non-functional constraint.
% constraint_indexing:constraint_classification(cs_ecmo_bridge, piton,
%    context(agent_power(analytical),
%            time_horizon(civilizational),
%            exit_options(arbitrage),
%            spatial_scope(universal))) :-
%    domain_priors:theater_ratio(cs_ecmo_bridge, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cs_ecmo_bridge_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(cs_ecmo_bridge, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cs_ecmo_bridge, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(cs_ecmo_bridge, ExtMetricName, E),
    E >= 0.46. % Ensures it's  high-extraction Snare/Tangled.

:- end_tests(cs_ecmo_bridge_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The ECMO bridge presents a perspectival gap because, from the patient's perspective (powerless, trapped), it can feel like a snare.
 * While it offers a chance at survival, it also represents a high-stakes gamble where the patient's fate is heavily reliant on the availability
 * of a donor organ and the success of the transplant.  The hospital/surgeons (institutional, mobile) view it as a rope - a crucial tool that
 * enables them to perform complex life-saving procedures. The analytical observer sees a tangled rope - a process that has genuine coordination to deliver a life saving surgery but has a strong asymmetric extraction of patient resources and potential harm if surgery fails.
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents the system from mislabeling the ECMO bridge as pure extraction (Snare) by recognizing that it has a genuine coordination function:
 * it coordinates the patient's life support with the organ donation system, surgical teams, and post-operative care, resulting in the potential for a successful lung transplant. This prevents it from being categorized as a pure extraction mechanism, which would lack this critical coordination component. The requires_active_enforcement aspect reflects the need for trained medical staff and specialized equipment to maintain the ECMO system and facilitate the transplant.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ecmo_1,
    'What are the long-term health outcomes for patients who receive an ECMO bridge to transplant?',
    'Longitudinal studies tracking patient health, quality of life, and survival rates post-transplant.',
    'If positive: Increased adoption of ECMO. If negative: Re-evaluation of ECMO benefits vs. risks.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cs_ecmo_bridge, 0, 10).

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
narrative_ontology:measurement(cs_ecmo_bridge_tr_t0, cs_ecmo_bridge, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cs_ecmo_bridge_tr_t5, cs_ecmo_bridge, theater_ratio, 5, 0.08).
narrative_ontology:measurement(cs_ecmo_bridge_tr_t10, cs_ecmo_bridge, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(cs_ecmo_bridge_ex_t0, cs_ecmo_bridge, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(cs_ecmo_bridge_ex_t5, cs_ecmo_bridge, base_extractiveness, 5, 0.53).
narrative_ontology:measurement(cs_ecmo_bridge_ex_t10, cs_ecmo_bridge, base_extractiveness, 10, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */