% ============================================================================
% CONSTRAINT STORY: genie_ip_constraint
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2024-02-29
% ============================================================================

:- module(constraint_genie_ip_constraint, []).

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
 * * constraint_id: genie_ip_constraint
 * human_readable: Google's Project Genie IP Concerns
 * domain: technological
 * * SUMMARY:
 * Google's Project Genie allows users to create playable game worlds from text prompts. This raises concerns about intellectual property, as Genie can potentially generate content that infringes on existing copyrighted material, creating a complex legal and ethical constraint.
 * * KEY AGENTS:
 * - Content Creator: Subject (Powerless)
 * - Google: Beneficiary (Institutional)
 * - IP Lawyer: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(genie_ip_constraint, 0.6). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(genie_ip_constraint, 0.7).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(genie_ip_constraint, 0.1).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(genie_ip_constraint, extractiveness, 0.6).
narrative_ontology:constraint_metric(genie_ip_constraint, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(genie_ip_constraint, theater_ratio, 0.1).

% Binary flags
% narrative_ontology:has_sunset_clause(genie_ip_constraint).      % Mandatory if Scaffold
% domain_priors:requires_active_enforcement(genie_ip_constraint). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(genie_ip_constraint, google).
narrative_ontology:constraint_victim(genie_ip_constraint, content_creators).

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
constraint_indexing:constraint_classification(genie_ip_constraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(genie_ip_constraint, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% This perspective is used by the bridge to derive constraint_claim.
% Type should reflect what the metrics compute: mountain, rope, tangled_rope, snare, scaffold, or piton.
constraint_indexing:constraint_classification(genie_ip_constraint, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))) :-
    narrative_ontology:constraint_beneficiary(genie_ip_constraint, _),
    narrative_ontology:constraint_victim(genie_ip_constraint, _).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genie_ip_constraint_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(genie_ip_constraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(genie_ip_constraint, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(genie_ip_constraint, ExtMetricName, E),
    (E =< 0.05 -> true ; E >= 0.46). % Ensures it's either a Mountain or high-extraction Snare/Tangled.

:- end_tests(genie_ip_constraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness score is set to 0.6 because Google benefits significantly from user-generated content, potentially at the expense of content creators whose IP may be infringed. The suppression score is high (0.7) as the AI model's ability to create content that mimics existing works limits the opportunities for original content creators to thrive independently.

 * The Subject (Content Creator) perceives this as a Snare. They feel trapped within a system where their intellectual property can be unintentionally or intentionally replicated, without adequate protection or compensation.

 * The Beneficiary (Google) views it as a Rope. They perceive Project Genie as a means to enhance user engagement and content creation, fostering a collaborative environment that benefits all users.

 * The Analytical Observer classifies it as a Tangled Rope. There is a clear coordination function (enhanced content creation and user engagement) alongside asymmetric extraction (potential IP infringement and diminished value for original content).

 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification prevents the system from mislabeling the situation as pure extraction because it recognizes the inherent coordination benefits that Project Genie provides. It's not solely about Google extracting value from creators, but also about facilitating content generation and user interaction. The "tangled" aspect reflects the conflicting incentives and potential harms associated with IP infringement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_genie_ip_constraint,
    'To what extent can AI-generated content be considered transformative, and thus not infringing on existing IP?',
    'Legal precedent and court rulings on AI-generated content.',
    'If transformative: Reduced IP risk for Google, increased potential for AI creativity. If not transformative: Increased IP risk for Google, potential limitations on AI content generation.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(genie_ip_constraint, 0, 10).

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
narrative_ontology:measurement(genie_ip_constraint_tr_t0, genie_ip_constraint, theater_ratio, 0, 0.1).
narrative_ontology:measurement(genie_ip_constraint_tr_t5, genie_ip_constraint, theater_ratio, 5, 0.15).
narrative_ontology:measurement(genie_ip_constraint_tr_t10, genie_ip_constraint, theater_ratio, 10, 0.1).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(genie_ip_constraint_ex_t0, genie_ip_constraint, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(genie_ip_constraint_ex_t5, genie_ip_constraint, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(genie_ip_constraint_ex_t10, genie_ip_constraint, base_extractiveness, 10, 0.6).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */