% ============================================================================
% CONSTRAINT STORY: nicene_creed_authority__liturgical_habituation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nicene_creed_authority__liturgical_habituation_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: nicene_creed_authority__liturgical_habituation_reading
 *   human_readable: Nicene Creed as Liturgical Identity Marker
 *   domain: systematic_theology/ecclesiology/history_of_christian_doctrine
 *
 * SUMMARY:
 *   This constraint models the Nicene Creed not as a strict metaphysical
 *   binding, but as a liturgical performance that functions as a primary
 *   marker of Christian identity and continuity. Its authority derives from
 *   its habitual recitation and its role in shaping communal belonging,
 *   rather than from strict cognitive assent to its every proposition. This
 *   reading emphasizes the social and ritual function over the doctrinal
 *   enforcement function, leading to very low extractiveness and
 *   classification as a Rope.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nicene_creed_authority__liturgical_habituation_reading, 0.08).
domain_priors:suppression_score(nicene_creed_authority__liturgical_habituation_reading, 0.15).
domain_priors:theater_ratio(nicene_creed_authority__liturgical_habituation_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(nicene_creed_authority__liturgical_habituation_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nicene_creed_authority__liturgical_habituation_reading, rope).
narrative_ontology:human_readable(nicene_creed_authority__liturgical_habituation_reading, "Nicene Creed as Liturgical Identity Marker").
narrative_ontology:topic_domain(nicene_creed_authority__liturgical_habituation_reading, "systematic_theology/ecclesiology/history_of_christian_doctrine").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(nicene_creed_authority__liturgical_habituation_reading, '1b2689ee-a92a-42ad-ab9b-6c72b25ea6de').
narrative_ontology:cs_kernel_codification('1b2689ee-a92a-42ad-ab9b-6c72b25ea6de', fixed_text).
narrative_ontology:cs_authority_grounding('1b2689ee-a92a-42ad-ab9b-6c72b25ea6de', practice).
narrative_ontology:cs_interpretation_layer_present('1b2689ee-a92a-42ad-ab9b-6c72b25ea6de').
narrative_ontology:cs_reading_relation('1b2689ee-a92a-42ad-ab9b-6c72b25ea6de', nicene_creed_authority__strict_orthodox_reading, influences).
narrative_ontology:cs_reading_relation('1b2689ee-a92a-42ad-ab9b-6c72b25ea6de', nicene_creed_authority__symbolic_confessional_reading, influences).
narrative_ontology:cs_axiom('1b2689ee-a92a-42ad-ab9b-6c72b25ea6de', foundational, liturgical_performance_as_identity_grounding).
narrative_ontology:cs_axiom_status(liturgical_performance_as_identity_grounding, holdable).
narrative_ontology:cs_axiom_grounding('1b2689ee-a92a-42ad-ab9b-6c72b25ea6de', liturgical_performance_as_identity_grounding, conventional).
narrative_ontology:cs_axiom('1b2689ee-a92a-42ad-ab9b-6c72b25ea6de', foundational, cognitive_assent_not_primary_for_belonging).
narrative_ontology:cs_axiom_status(cognitive_assent_not_primary_for_belonging, holdable).
narrative_ontology:cs_axiom_grounding('1b2689ee-a92a-42ad-ab9b-6c72b25ea6de', cognitive_assent_not_primary_for_belonging, conventional).
narrative_ontology:cs_reference_frame('1b2689ee-a92a-42ad-ab9b-6c72b25ea6de', early_christian_communal_practice).
narrative_ontology:cs_drift_state('1b2689ee-a92a-42ad-ab9b-6c72b25ea6de', contemporary_pluralistic_context, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('1b2689ee-a92a-42ad-ab9b-6c72b25ea6de', '').
narrative_ontology:cs_kernel_id(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, congregants).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, clergy).
narrative_ontology:constraint_beneficiary(nicene_creed_authority__liturgical_habituation_reading, denominational_institutions).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, communal_identity_formation).
narrative_ontology:constraint_vindicates(nicene_creed_authority__liturgical_habituation_reading, liturgical_continuity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in the communal recitation of the creed, which reinforces a sense of shared identity and belonging, often without requiring explicit cognitive assent to every metaphysical claim. Exit is possible by joining another denomination or leaving the church, but means losing this form of communal identity.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, congregants, beneficiary,
    moderate, biographical, mobile, local).

% Lead the liturgical performance of the creed, maintaining its place in worship and catechesis. They benefit from the stable identity it provides for their congregations but may face pressure from other readings (strict orthodox, symbolic confessional) regarding its proper interpretation and use. Exit means leaving their ordained ministry.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, clergy, agenda_setter,
    organized, generational, constrained, national).

% Benefit from the creed's role in maintaining a coherent, historically continuous identity across diverse congregations and theological perspectives. It provides a common language and ritual practice that undergirds institutional unity, even if its metaphysical content is interpreted broadly. Exit is not an option for the institution itself, only for its members.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, denominational_institutions, beneficiary,
    institutional, civilizational, constrained, global).

% Analyze the historical, theological, and sociological functions of the Nicene Creed, including its role in identity formation through ritual. They observe its operation without being bound by its liturgical performance in the same way as congregants or clergy.
narrative_ontology:constraint_stakeholder(nicene_creed_authority__liturgical_habituation_reading, theologians_and_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates communal identity and historical continuity within Christian traditions by providing a shared liturgical text and ritual practice, allowing for diverse cognitive interpretations of its metaphysical content.
% TRANSFER_FUNCTION: Transfers a sense of belonging, historical rootedness, and shared identity to congregants and institutions, in exchange for participation in a common ritual practice.
% ABSENT_VOICES: Those who insist on strict cognitive assent to every metaphysical proposition of the creed as a condition of membership would object to this reading, arguing it dilutes doctrinal truth. They are present in other readings of the kernel, but structurally excluded from this reading's primary function.
% DISAPPEARANCE_RATIONALE: If the Nicene Creed vanished from liturgical use overnight, Christian denominations would lose a primary, widely recognized marker of historical and communal identity. Congregations would struggle to articulate their shared faith and connection to the broader tradition, leading to a significant reorganization of worship practices and identity formation.
% FOUNDING_PROBLEM: The early Christian church faced internal divisions and external pressures, requiring a concise statement of faith to unify diverse communities and distinguish orthodox belief from heresy.
% FOUNDING_PROBLEM_CORROBORATION: Historians of early Christianity corroborate the original problem of doctrinal unity. Contemporary sociologists of religion and liturgical scholars attest that the problem of maintaining communal identity and historical continuity remains live, even if the specific challenges have evolved.
narrative_ontology:disappearance_verdict(nicene_creed_authority__liturgical_habituation_reading, world_rearranges).
narrative_ontology:founding_problem_status(nicene_creed_authority__liturgical_habituation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(nicene_creed_authority__liturgical_habituation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(nicene_creed_authority__liturgical_habituation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(nicene_creed_authority__liturgical_habituation_reading, 0.08, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nicene_creed_authority__liturgical_habituation_reading_tests).
:- end_tests(nicene_creed_authority__liturgical_habituation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08) because this reading emphasizes voluntary participation in a shared ritual for identity formation, with minimal coercive overhead. Suppression is low (0.15) as the constraint's persistence relies on communal practice and cultural transmission, not active enforcement against dissenters from its metaphysical claims. Accessibility collapse is high (0.80) because, for those seeking this specific form of Christian communal identity, the creed's liturgical use is a near-universal and deeply embedded practice. Resistance is low (0.05) because this reading is largely accepted by those who value the communal and historical aspects of the creed over strict doctrinal uniformity.
 *
 * PERSPECTIVAL GAP:
 *   Other readings of the Nicene Creed (e.g., strict orthodox) would experience it as highly extractive and suppressive, demanding cognitive assent and enforcing doctrinal boundaries. This reading, however, focuses on the consensual, identity-forming function, where the 'cost' is participation in ritual, not intellectual submission. The engine's per-seat classification would reflect this divergence if other readings were modeled as distinct constraints.
 *
 * DIRECTIONALITY LOGIC:
 *   Congregants and denominational institutions are beneficiaries, gaining identity and continuity. Clergy act as agenda-setters, maintaining the practice. No direct victims are identified in this reading, as it focuses on the consensual, identity-forming aspects. Theologians and scholars are observers, analyzing its function.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cognitive_assent_threshold,
    'At what point does the expectation of cognitive metaphysical assent to the creed''s propositions become an implicit condition of belonging, shifting this constraint from a Rope to a Tangled Rope?',
    'Empirical study of congregational expectations and pastoral enforcement practices: if non-assent leads to social exclusion or pressure to conform, the threshold has been crossed.',
    'If a clear threshold for cognitive assent is found, the constraint''s extractiveness and suppression would increase, reclassifying it as a Tangled Rope due to implicit coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cognitive_assent_threshold, empirical, 'Ambiguity of implicit cognitive demands in liturgical practice.').

omega_variable(
    identity_vs_doctrine_primacy,
    'Is the primary function of the Nicene Creed in this context truly identity formation, or is identity formation a secondary effect of its doctrinal content, which is implicitly enforced?',
    'Conceptual analysis of theological and sociological literature, combined with ethnographic studies of how congregants articulate their relationship to the creed.',
    'If doctrinal content is found to be implicitly primary, the constraint would be re-evaluated for higher extractiveness and suppression, potentially shifting to a Tangled Rope or Snare, as the ''identity'' function would be cover for doctrinal control.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(identity_vs_doctrine_primacy, conceptual, 'Conceptual ambiguity between identity function and implicit doctrinal enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nicene_creed_authority__liturgical_habituation_reading, 0, 1700).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nice_tr_t0, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 0, 0.02).
narrative_ontology:measurement(nice_tr_t400, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 400, 0.03).
narrative_ontology:measurement(nice_tr_t800, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 800, 0.04).
narrative_ontology:measurement(nice_tr_t1200, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1200, 0.04).
narrative_ontology:measurement(nice_tr_t1700, nicene_creed_authority__liturgical_habituation_reading, theater_ratio, 1700, 0.05).

% Extraction over time
narrative_ontology:measurement(nice_be_t0, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(nice_be_t400, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 400, 0.06).
narrative_ontology:measurement(nice_be_t800, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 800, 0.07).
narrative_ontology:measurement(nice_be_t1200, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1200, 0.07).
narrative_ontology:measurement(nice_be_t1700, nicene_creed_authority__liturgical_habituation_reading, base_extractiveness, 1700, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(nice_su_t0, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(nice_su_t400, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 400, 0.12).
narrative_ontology:measurement(nice_su_t800, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 800, 0.13).
narrative_ontology:measurement(nice_su_t1200, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1200, 0.14).
narrative_ontology:measurement(nice_su_t1700, nicene_creed_authority__liturgical_habituation_reading, suppression_requirement, 1700, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nicene_creed_authority__liturgical_habituation_reading, identity_coordination).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__strict_orthodox_reading).
narrative_ontology:affects_constraint(nicene_creed_authority__liturgical_habituation_reading, nicene_creed_authority__symbolic_confessional_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Nicene Creed's authority. It provides the social and ritual substrate that enables both stricter doctrinal enforcement (strict_orthodox_reading) and more pluralistic reinterpretation (symbolic_confessional_reading) by maintaining a common identity marker.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
