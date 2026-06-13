% ============================================================================
% CONSTRAINT STORY: dueling_disappearance_mechanism__contraction_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dueling_disappearance_mechanism__contraction_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    domain_priors:emerges_naturally/1,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dueling_disappearance_mechanism__contraction_reading
 *   human_readable: Dueling's Cultural Unthinkability (Dignity Culture Contraction Reading)
 *   domain: historical_sociology/cultural_anthropology/legal_history
 *
 * SUMMARY:
 *   This constraint models the disappearance of dueling as a consequence of a
 *   fundamental cultural shift from an 'honor culture' to a 'dignity
 *   culture.' In this 'contraction reading,' dueling became unthinkable not
 *   primarily due to legal prohibition or institutional alternatives, but
 *   because the underlying cultural axioms that made it meaningful (e.g.,
 *   honor as a property to be defended by violence) were displaced by a new
 *   framework emphasizing universal human dignity and the state's monopoly on
 *   violence. This reading posits the dignity culture as an emergent,
 *   self-enforcing 'mountain' that renders dueling culturally impossible,
 *   rather than merely illegal or impractical.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dueling_disappearance_mechanism__contraction_reading, 0.05).
domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, 0.95).
domain_priors:theater_ratio(dueling_disappearance_mechanism__contraction_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dueling_disappearance_mechanism__contraction_reading, mountain).
narrative_ontology:human_readable(dueling_disappearance_mechanism__contraction_reading, "Dueling's Cultural Unthinkability (Dignity Culture Contraction Reading)").
narrative_ontology:topic_domain(dueling_disappearance_mechanism__contraction_reading, "historical_sociology/cultural_anthropology/legal_history").

domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dueling_disappearance_mechanism__contraction_reading, 'c16a7736-3870-4948-80c9-2d4295741106').
narrative_ontology:cs_kernel_codification('c16a7736-3870-4948-80c9-2d4295741106', implicit).
narrative_ontology:cs_authority_grounding('c16a7736-3870-4948-80c9-2d4295741106', practice).
narrative_ontology:cs_interpretation_layer_present('c16a7736-3870-4948-80c9-2d4295741106').
narrative_ontology:cs_reading_relation('c16a7736-3870-4948-80c9-2d4295741106', dueling_disappearance_mechanism__institutional_displacement_reading, influences).
narrative_ontology:cs_reading_relation('c16a7736-3870-4948-80c9-2d4295741106', dueling_disappearance_mechanism__overdetermined_composite_reading, coexists_with).
narrative_ontology:cs_axiom('c16a7736-3870-4948-80c9-2d4295741106', foundational, universal_human_dignity_supersedes_honor).
narrative_ontology:cs_axiom_status(universal_human_dignity_supersedes_honor, holdable).
narrative_ontology:cs_axiom_grounding('c16a7736-3870-4948-80c9-2d4295741106', universal_human_dignity_supersedes_honor, deontological).
narrative_ontology:cs_axiom('c16a7736-3870-4948-80c9-2d4295741106', secondary, state_monopoly_on_violence_is_absolute).
narrative_ontology:cs_axiom_status(state_monopoly_on_violence_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('c16a7736-3870-4948-80c9-2d4295741106', state_monopoly_on_violence_is_absolute, conventional).
narrative_ontology:cs_reference_frame('c16a7736-3870-4948-80c9-2d4295741106', honor_culture_legitimacy_of_duel).
narrative_ontology:cs_drift_state('c16a7736-3870-4948-80c9-2d4295741106', post_enlightenment_cultural_shift, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c16a7736-3870-4948-80c9-2d4295741106', '').
narrative_ontology:cs_kernel_id(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dueling_disappearance_mechanism__contraction_reading, dignity_culture_adherents).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dueling_disappearance_mechanism__contraction_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(dueling_disappearance_mechanism__contraction_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dueling_disappearance_mechanism__contraction_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, ExtMetricName, E),
    domain_priors:suppression_score(dueling_disappearance_mechanism__contraction_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(dueling_disappearance_mechanism__contraction_reading),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(dueling_disappearance_mechanism__contraction_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(dueling_disappearance_mechanism__contraction_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Mountain because the shift to dignity culture is presented as an irreversible, foundational change in normative substrate, making dueling culturally unthinkable. Extractiveness is low (0.05) because the 'extraction' is the loss of a cultural practice, not a direct transfer of resources; it's a redefinition of the social game itself. Suppression is high (0.95) because the dignity culture actively suppresses the very conceptual space for dueling, making it morally repugnant and socially impossible. Accessibility collapse is high (0.9) as the cultural alternatives (dueling as legitimate redress) are almost entirely gone. Resistance is low (0.05) because the cultural shift is so profound that active resistance to dueling's disappearance is minimal; those who might resist are identity-locked into a superseded framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of dignity culture adherents, the disappearance of dueling is a natural, beneficial evolution towards a more civilized society. From the perspective of historical honor culture practitioners, it represents a profound loss of a vital mechanism for maintaining social order and personal standing, leaving them disoriented and disempowered. The engine's classification of 'mountain' reflects the former, while the high suppression and victim declaration reflect the latter's experience.
 *
 * DIRECTIONALITY LOGIC:
 *   Dignity culture adherents are beneficiaries (d near 0.0) as their worldview is validated and enforced, leading to a more stable social order from their perspective. Honor culture practitioners are victims (d near 1.0) as their entire framework for social interaction and self-worth is invalidated and suppressed. State legal systems are agenda-setters, but their role is secondary to the cultural shift; they enforce what the culture has already made unthinkable. Legal scholars are analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cultural_vs_institutional_causation,
    'Was the decline of dueling primarily driven by the cultural shift to dignity culture (this reading), or by the rise of alternative institutional dispute resolution mechanisms (institutional_displacement_reading)?',
    'Comparative historical analysis of regions where cultural shifts preceded institutional changes, or vice versa; counterfactual analysis of societies with strong dignity culture but weak state institutions.',
    'If institutional displacement was primary, this constraint would be reclassified as a Rope or Tangled Rope (coordinating new institutions), and the ''mountain'' claim of dignity culture''s inevitability would be weakened. If cultural shift was primary, this reading''s Mountain classification is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cultural_vs_institutional_causation, empirical, 'Distinguishing cultural vs. institutional drivers of dueling''s decline.').

omega_variable(
    irreversibility_of_dignity_culture,
    'Is the shift to dignity culture truly an irreversible ''mountain'' (as claimed), or could a resurgence of honor-based values make dueling thinkable again under certain conditions?',
    'Sociological analysis of contemporary ''honor killings'' or other forms of private violence in dignity-culture contexts; theoretical exploration of conditions under which dignity culture might erode.',
    'If dignity culture is not irreversible, the ''mountain'' classification is too strong, and the constraint might be reclassified as a deeply entrenched Snare or Tangled Rope, maintained by active (though subtle) cultural enforcement rather than natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(irreversibility_of_dignity_culture, conceptual, 'Assessing the true ''mountain'' nature of dignity culture''s displacement of honor culture.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dueling_disappearance_mechanism__contraction_reading, 1700, 1900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(duel_be_t1700, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1700, 0.15).
narrative_ontology:measurement(duel_be_t1750, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1750, 0.1).
narrative_ontology:measurement(duel_be_t1800, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1800, 0.08).
narrative_ontology:measurement(duel_be_t1850, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1850, 0.06).
narrative_ontology:measurement(duel_be_t1900, dueling_disappearance_mechanism__contraction_reading, base_extractiveness, 1900, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(duel_su_t1700, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1700, 0.5).
narrative_ontology:measurement(duel_su_t1750, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1750, 0.65).
narrative_ontology:measurement(duel_su_t1800, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1800, 0.75).
narrative_ontology:measurement(duel_su_t1850, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1850, 0.85).
narrative_ontology:measurement(duel_su_t1900, dueling_disappearance_mechanism__contraction_reading, suppression_requirement, 1900, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dueling_disappearance_mechanism__contraction_reading, identity_coordination).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__institutional_displacement_reading).
narrative_ontology:affects_constraint(dueling_disappearance_mechanism__contraction_reading, dueling_disappearance_mechanism__overdetermined_composite_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'dueling_disappearance_mechanism' kernel. It focuses on the cultural displacement of honor-culture axioms by dignity culture, leading to dueling becoming unthinkable. Other readings emphasize institutional substitution or a composite of factors.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
