% ============================================================================
% CONSTRAINT STORY: us_iran_drone_conflict
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_us_iran_drone_conflict, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: us_iran_drone_conflict
 * human_readable: US-Iran Drone Conflict and Escalation
 * domain: political
 * * SUMMARY:
 * The US military claims to have shot down an Iranian drone that approached a US base in Syria. This incident reflects the ongoing tension and conflict between the US and Iran in the region, with potential for escalation. The constraint is the cycle of actions and reactions between the two countries, limiting options for both sides.
 * * KEY AGENTS:
 * - Iran: Subject (Powerless/Constrained)
 * - US Military: Beneficiary (Institutional)
 * - International Observers: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(us_iran_drone_conflict, 0.6). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(us_iran_drone_conflict, 0.7).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(us_iran_drone_conflict, 0.2).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(us_iran_drone_conflict, extractiveness, 0.6).
narrative_ontology:constraint_metric(us_iran_drone_conflict, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(us_iran_drone_conflict, theater_ratio, 0.2).

% Constraint classification claim
narrative_ontology:constraint_claim(us_iran_drone_conflict, tangled_rope).
narrative_ontology:human_readable(us_iran_drone_conflict, "US-Iran Drone Conflict and Escalation").

% Binary flags
% narrative_ontology:has_sunset_clause(us_iran_drone_conflict).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(us_iran_drone_conflict). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(us_iran_drone_conflict, us_military).
narrative_ontology:constraint_victim(us_iran_drone_conflict, iran).

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
constraint_indexing:constraint_classification(us_iran_drone_conflict, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(us_iran_drone_conflict, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(us_iran_drone_conflict, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: (ORGANIZED POWER)
% Demonstrating change when a coalition forms among victims.
constraint_indexing:constraint_classification(us_iran_drone_conflict, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_iran_drone_conflict_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(us_iran_drone_conflict, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_iran_drone_conflict, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(us_iran_drone_conflict, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(us_iran_drone_conflict_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * I assigned a base extractiveness of 0.6 and a suppression score of 0.7 due to the clear limitations placed on Iran's actions and the potential for further escalation. The Perspectival Gap exists because the US military views its actions as defensive and necessary for regional stability, while Iran perceives them as aggressive and suppressive of its interests. This framing leads to differing classifications: the US as maintaining a "rope" of regional order (however coercive) while Iran is caught in a "snare" of imposed limitations. From a broader analytical perspective, the situation is a tangled rope: both sides are entangled in a cycle of actions and reactions, extracting resources and strategic advantages from the other, while also coordinating to avoid full-scale war (however imperfectly).
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents the system from mislabeling coordination as pure extraction by acknowledging the element of coordination. Both sides engage in actions that influence the other's behavior, signaling intentions and red lines, thereby shaping the parameters of the conflict. While the extraction and suppression are real, they coexist with a degree of (often adversarial) coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_us_iran_drone_conflict,
    'Is the US military presence in Syria primarily a deterrent force or an interventionist one?',
    'Detailed historical analysis of US foreign policy objectives and military actions in the region, combined with classified intelligence assessments.',
    'If Deterrent: conflict may de-escalate; if Interventionist: conflict likely to escalate.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_iran_drone_conflict, 0, 10).

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
narrative_ontology:measurement(us_iran_drone_conflict_tr_t0, us_iran_drone_conflict, theater_ratio, 0, 0.1).
narrative_ontology:measurement(us_iran_drone_conflict_tr_t5, us_iran_drone_conflict, theater_ratio, 5, 0.2).
narrative_ontology:measurement(us_iran_drone_conflict_tr_t10, us_iran_drone_conflict, theater_ratio, 10, 0.2).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(us_iran_drone_conflict_ex_t0, us_iran_drone_conflict, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(us_iran_drone_conflict_ex_t5, us_iran_drone_conflict, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(us_iran_drone_conflict_ex_t10, us_iran_drone_conflict, base_extractiveness, 10, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */