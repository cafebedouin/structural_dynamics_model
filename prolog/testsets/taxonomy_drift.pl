% ============================================================================
% CONSTRAINT STORY: taxonomy_drift
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-16
% ============================================================================

:- module(constraint_taxonomy_drift, []).

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
 * * constraint_id: taxonomy_drift
 * human_readable: The Semantic Slippage Trap
 * domain: social/linguistic/bureaucratic
 * * SUMMARY:
 * A scenario where the definitions used by a governing system drift away from
 * the ground-truth reality of the subjects. As the taxonomy becomes
 * self-referential, it functions as a Snare for those who no longer fit
 * into the system's "legible" categories, yet are forced to interact with
 * it for survival.
 * * KEY AGENTS:
 * - Non-Categorical Subject: Subject (Powerless)
 * - Classification Authority: Beneficiary (Institutional)
 * - Ontological Auditor: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.76) as subjects spend significant resources trying to
% "perform" a category they don't inhabit to access basic services.
domain_priors:base_extractiveness(taxonomy_drift, 0.76).
domain_priors:suppression_score(taxonomy_drift, 0.62).
domain_priors:theater_ratio(taxonomy_drift, 0.85). % Extreme theater: the categories are purely performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(taxonomy_drift, extractiveness, 0.76).
narrative_ontology:constraint_metric(taxonomy_drift, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(taxonomy_drift, theater_ratio, 0.85).

% Constraint self-claim: It claims to be a coordination mechanism, but its
% function has degraded to enforcement of its own outdated categories.
narrative_ontology:constraint_claim(taxonomy_drift, piton).
narrative_ontology:human_readable(taxonomy_drift, "The Semantic Slippage Trap").

% Binary flags and structural properties for Tangled Rope / Piton detection.
domain_priors:requires_active_enforcement(taxonomy_drift). % Subjects are compelled to use the taxonomy.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(taxonomy_drift, classification_authority).
narrative_ontology:constraint_victim(taxonomy_drift, non_categorical_subject).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The subject is trapped in a category that is factually wrong but legally binding.
constraint_indexing:constraint_classification(taxonomy_drift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The authority views the rigid taxonomy as a vital Rope for administrative
% legibility and large-scale coordination.
constraint_indexing:constraint_classification(taxonomy_drift, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE SYSTEMS AUDITOR (PITON)
% Theater ratio (0.85) > 0.70 triggers Piton: the taxonomy is now a non-functional
% inertial spike that maintains the illusion of order. Its primary function
% has atrophied and been replaced by performative maintenance.
constraint_indexing:constraint_classification(taxonomy_drift, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(taxonomy_drift, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(taxonomy_drift_tests).

test(perspectival_gap) :-
    % Verify Snare for the subject vs Rope for the institution.
    constraint_indexing:constraint_classification(taxonomy_drift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(taxonomy_drift, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(piton_threshold) :-
    % Verify high theater ratio correctly triggers Piton for analytical auditors.
    constraint_indexing:constraint_classification(taxonomy_drift, piton,
        context(agent_power(analytical), _, exit_options(arbitrage), _)).

test(high_extraction_and_theater) :-
    domain_priors:base_extractiveness(taxonomy_drift, E),
    domain_priors:theater_ratio(taxonomy_drift, TR),
    E > 0.75,
    TR > 0.80.

:- end_tests(taxonomy_drift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.76) reflects the 'Mandatrophy' threshold where the
 * administrative cost of misclassification falls entirely on the subject. The
 * high theater ratio (0.85) indicates that the taxonomy's primary function is
 * no longer to accurately model reality but to perform the act of classification.
 *
 * PERSPECTIVAL GAP:
 * The individual experiences a Snare because they must expend energy to conform
 * to a system that misrepresents them to access services. The institution sees
 * a Rope because the rigid labels allow for easy, automated processing, and the
 * cost of the semantic drift is externalized to the subjects.
 *
 * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY] Resolved by the Piton classification. For an analytical
 * observer, the taxonomy no longer coordinates reality; it is an inertial
 * structure that siphons truth to maintain administrative theater. While it has
 * the structural properties of a Tangled Rope (beneficiaries, victims, enforcement),
 * its extreme theater ratio indicates its coordination function has atrophied
 * beyond repair, making Piton the more precise classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Required for constraints with base_extractiveness > 0.46.
omega_variable(
    omega_linguistic_anchor,
    'Can a bottom-up folksonomy displace the top-down taxonomy (Snare vs Mountain)?',
    'Tracking adoption of non-standard tags in public digital repositories.',
    'If folksonomy thrives: Snare (enforced choice). If it fails: Mountain (cognitive limit).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(taxonomy_drift, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the degradation of the taxonomy from a useful tool
% into a performative trap. Required for high-extraction constraints.

% Theater ratio over time (metric_substitution drift):
narrative_ontology:measurement(taxonomy_drift_tr_t0, taxonomy_drift, theater_ratio, 0, 0.05).
narrative_ontology:measurement(taxonomy_drift_tr_t5, taxonomy_drift, theater_ratio, 5, 0.30).
narrative_ontology:measurement(taxonomy_drift_tr_t10, taxonomy_drift, theater_ratio, 10, 0.85).

% Extraction over time (extraction_accumulation drift):
narrative_ontology:measurement(taxonomy_drift_ex_t0, taxonomy_drift, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(taxonomy_drift_ex_t5, taxonomy_drift, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(taxonomy_drift_ex_t10, taxonomy_drift, base_extractiveness, 10, 0.76).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% A taxonomy is a classic example of an information standard.
narrative_ontology:coordination_type(taxonomy_drift, information_standard).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */