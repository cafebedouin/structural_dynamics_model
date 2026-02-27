% ============================================================================
% CONSTRAINT STORY: child_marriage
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_child_marriage, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: child_marriage
 * human_readable: Child Marriage as a Social Constraint
 * domain: social/religious
 * * SUMMARY:
 * Child marriage, especially in regions with high poverty and traditional social structures, involves marrying off children, typically girls, before they reach adulthood. This practice stems from various factors, including poverty, cultural traditions, and perceived protection for girls. It severely limits the child's educational opportunities, exposes them to health risks associated with early pregnancy, and perpetuates cycles of poverty and gender inequality.
 * * KEY AGENTS:
 * - The Child Bride: Subject (Powerless)
 * - The Family/Community Elders: Beneficiary (Institutional)
 * - Human Rights Organizations (e.g., UNICEF): Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(child_marriage, 0.75). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(child_marriage, 0.85).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(child_marriage, 0.20).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(child_marriage, extractiveness, 0.75).
narrative_ontology:constraint_metric(child_marriage, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(child_marriage, theater_ratio, 0.20).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(child_marriage, snare).
narrative_ontology:human_readable(child_marriage, "Child Marriage as a Social Constraint").
narrative_ontology:topic_domain(child_marriage, "social/religious").

% Binary flags
% narrative_ontology:has_sunset_clause(child_marriage).      % Mandatory if Scaffold
domain_priors:requires_active_enforcement(child_marriage). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(child_marriage, family_and_community_elders).
narrative_ontology:constraint_victim(child_marriage, child_bride).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% High extraction felt as an immutable limit or predatory trap.
% χ = 0.75 * 1.5 (powerless) * 0.8 (local) = 0.90. This is a clear Snare.
constraint_indexing:constraint_classification(child_marriage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential coordination for social stability and economic transfer.
% χ = 0.75 * -0.2 (institutional) * 0.8 (local) = -0.12. The negative effective
% extraction confirms the Rope classification from this perspective.
constraint_indexing:constraint_classification(child_marriage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% Default analytical context (civilizational/analytical/global).
% χ = 0.75 * 1.15 (analytical) * 1.2 (global) = 1.035. Extremely high
% extraction confirms the Snare classification.
constraint_indexing:constraint_classification(child_marriage, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(child_marriage_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(child_marriage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(child_marriage, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(child_marriage, ExtMetricName, E),
    E >= 0.46,
    % Verify that the analytical observer sees a Snare.
    constraint_indexing:constraint_classification(child_marriage, snare, context(agent_power(analytical), _, _, _)).

:- end_tests(child_marriage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Child marriage is modeled as a high-extraction (0.75) and high-suppression (0.85) constraint. The perspectival gap is stark:
 * - For the child (powerless, trapped), the effective extraction is amplified to 0.90, making it an inescapable Snare that forecloses life opportunities.
 * - For the family/community elders (institutional), the constraint is perceived as a Rope. The institutional power modifier makes the effective extraction negative (-0.12), reflecting a belief that the practice provides net benefits (social cohesion, economic stability, perceived protection). They see it as essential coordination, not extraction.
 * - The analytical observer, using a global scope, calculates an effective extraction of 1.035, confirming it as a severe Snare.
 * The low theater_ratio (0.20) indicates this is an actively functional, not performative, system of control. The original classification of the institutional view as a Piton was incorrect, as a Piton requires a theater_ratio >= 0.70. The Rope classification is a more accurate representation of the beneficiary's deferential realism.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint is resolved as a Snare, not a Tangled Rope. While there is a beneficiary group (family/community) and thus a 'coordination function' from their perspective, the system's primary structural feature is the overwhelming, asymmetric extraction from the victim. The suppression score of 0.85 and the lack of any meaningful benefit or alternative for the child mean the coordination aspect is entirely subordinate to the extractive function. The system's existence depends on the suppression of the victim's agency, a hallmark of a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_child_marriage,
    'To what extent is the "beneficiary" view driven by genuine belief in social coordination versus direct economic desperation?',
    'Comparative ethnographic studies correlating the prevalence of the practice with localized economic shocks versus stable cultural norms.',
    'If primarily economic desperation, the constraint is a pure Snare of poverty. If primarily cultural belief, it has features of a degraded coordination system, making it a more complex Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(child_marriage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Required for high-extraction constraints (base_extractiveness > 0.46).
% The data models a slow intensification of the constraint's extractive nature
% as external pressures (e.g., economic hardship) increase.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(child_marriage_tr_t0, child_marriage, theater_ratio, 0, 0.10).
narrative_ontology:measurement(child_marriage_tr_t5, child_marriage, theater_ratio, 5, 0.15).
narrative_ontology:measurement(child_marriage_tr_t10, child_marriage, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(child_marriage_ex_t0, child_marriage, base_extractiveness, 0, 0.70).
narrative_ontology:measurement(child_marriage_ex_t5, child_marriage, base_extractiveness, 5, 0.73).
narrative_ontology:measurement(child_marriage_ex_t10, child_marriage, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The practice is a mechanism for enforcing social norms and managing kinship ties.
narrative_ontology:coordination_type(child_marriage, enforcement_mechanism).

% Boltzmann floor override (only if domain knowledge justifies)
% narrative_ontology:boltzmann_floor_override(child_marriage, 0.1).

% Network relationships (structural influence edges)
% Child marriage is structurally coupled with and negatively impacts access to
% female education.
narrative_ontology:affects_constraint(child_marriage, female_education_access).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */