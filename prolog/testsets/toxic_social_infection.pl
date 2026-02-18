% ============================================================================
% CONSTRAINT STORY: toxic_social_infection
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_toxic_social_infection, []).

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
 * * constraint_id: toxic_social_infection
 * human_readable: The Infecting Character (Psychic Vampire)
 * domain: social/psychological
 * * SUMMARY:
 * This constraint describes a class of individuals—referred to as "psychic vampires," or "infecting characters"—who radiate an inward instability that draws disaster upon themselves and those around them. They function as social black holes that extract energy, time, and agency from their environment through a combination of seductive flattery and manufactured crises.
 * * KEY AGENTS:
 * - The Infected: The subject who becomes enmeshed and drained of energy.
 * - The Infector: The beneficiary who engineers the social dynamic to extract resources.
 * - The Analyst: An observer who can see the pattern of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(toxic_social_infection, 0.90). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(toxic_social_infection, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(toxic_social_infection, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(toxic_social_infection, extractiveness, 0.90).
narrative_ontology:constraint_metric(toxic_social_infection, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(toxic_social_infection, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(toxic_social_infection, tangled_rope).
narrative_ontology:human_readable(toxic_social_infection, "The Infecting Character (Psychic Vampire)").
narrative_ontology:topic_domain(toxic_social_infection, "social/psychological").

% Binary flags
domain_priors:requires_active_enforcement(toxic_social_infection). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
% Both are required for Tangled Rope. Coordination is also required for Scaffold.
narrative_ontology:constraint_beneficiary(toxic_social_infection, the_infector).
narrative_ontology:constraint_victim(toxic_social_infection, the_infected).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE/MOUNTAIN)
% High extraction felt as an immutable limit or predatory trap.
constraint_indexing:constraint_classification(toxic_social_infection, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure or coordination.
constraint_indexing:constraint_classification(toxic_social_infection, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Default analytical context (civilizational/analytical/global).
% The high extraction, suppression, and need for enforcement, combined with a
% clear coordination function for the beneficiary, make this a Tangled Rope.
constraint_indexing:constraint_classification(toxic_social_infection, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(toxic_social_infection_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(toxic_social_infection, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(toxic_social_infection, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(threshold_validation_high_extraction) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(toxic_social_infection, ExtMetricName, E),
    E >= 0.46. % Ensures it's a high-extraction Snare/Tangled Rope.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(toxic_social_infection, tangled_rope, context(agent_power(analytical), _, _, _)).

:- end_tests(toxic_social_infection_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models a predatory social dynamic. The base extractiveness is set to 0.90, reflecting narrative descriptions of victims being "drained" or having their "life sucked out." The suppression score of 0.70 reflects the use of flattery, gaslighting, and manufactured crises to obscure the extractive nature of the relationship.
 *
 * The key insight is the perspectival gap. For the victim (powerless, trapped), the dynamic is a pure Snare; engagement only leads to further loss of agency and energy. For the infector (institutional power within the dyad), the victim is a resource, and the social rules they create are a Rope for coordinating their own benefit.
 *
 * The analytical observer, seeing both the coordination function (for the infector) and the severe asymmetric extraction (from the victim), classifies this as a Tangled Rope. The original file's classification of 'Mountain' was incorrect, as the high extraction value is incompatible with natural law, and the dynamic is a constructed social pattern, not a physical one.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Tangled Rope classification correctly captures the dual nature of the constraint. It prevents the system from misclassifying it as a pure Snare (ignoring the infector's coordination) or a pure Rope (ignoring the victim's suffering). The system correctly identifies a constructed mechanism that benefits one party at the severe expense of another.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_toxic_social_infection_1,
    'Is the infector''s "inward instability" a functional biological necessity (Mountain) or an intentional strategy for resource extraction (Snare)?',
    'Audit of Cluster B neurological empathy-response vs. long-term profit-seeking behaviors.',
    'If necessity: Genetic Mountain. If predatory choice: Mandatrophy Snare.',
    confidence_without_resolution(medium)
).

omega_variable(
    omega_toxic_social_infection_2,
    'Do victims who become "FLEAs" permanently transform into new infectors (Mountain), or are they temporary habits that can be unlearned (Rope)?',
    'Longitudinal tracking of behavior in individuals post-estrangement from narcissists.',
    'If permanent: Social Mountain of corruption. If temporary: Remediable Rope.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(toxic_social_infection, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% This models a stable, perpetually high-extraction dynamic.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(toxic_social_infection_tr_t0, toxic_social_infection, theater_ratio, 0, 0.10).
narrative_ontology:measurement(toxic_social_infection_tr_t5, toxic_social_infection, theater_ratio, 5, 0.10).
narrative_ontology:measurement(toxic_social_infection_tr_t10, toxic_social_infection, theater_ratio, 10, 0.10).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(toxic_social_infection_ex_t0, toxic_social_infection, base_extractiveness, 0, 0.90).
narrative_ontology:measurement(toxic_social_infection_ex_t5, toxic_social_infection, base_extractiveness, 5, 0.90).
narrative_ontology:measurement(toxic_social_infection_ex_t10, toxic_social_infection, base_extractiveness, 10, 0.90).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The infector enforces a specific social dynamic to extract resources.
narrative_ontology:coordination_type(toxic_social_infection, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */