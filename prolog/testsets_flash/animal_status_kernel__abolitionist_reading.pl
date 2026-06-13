% ============================================================================
% CONSTRAINT STORY: animal_status_kernel__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status_kernel__abolitionist_reading, []).

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
 *   constraint_id: animal_status_kernel__abolitionist_reading
 *   human_readable: Abolitionist Reading: Animals as Moral Persons, Not Property
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of animal status,
 *   asserting that animals are moral persons with a fundamental right not to
 *   be property, and that all use is categorically impermissible. It is a
 *   reading of the 'animal_status_kernel' which is contested by
 *   'property_reading' and 'welfare_reading'. This reading frames the current
 *   legal and moral status of animals as a profound injustice, making the
 *   'animals_as_property' the primary victims. The high extractiveness and
 *   suppression reflect the systemic nature of animal exploitation under the
 *   property paradigm, which this reading seeks to dismantle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, 0.95).
domain_priors:suppression_score(animal_status_kernel__abolitionist_reading, 0.98).
domain_priors:theater_ratio(animal_status_kernel__abolitionist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, 0.99).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, mountain).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Abolitionist Reading: Animals as Moral Persons, Not Property").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:emerges_naturally(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, '8f890314-bee8-4ce7-92db-b23b91cde283').
narrative_ontology:cs_kernel_codification('8f890314-bee8-4ce7-92db-b23b91cde283', implicit).
narrative_ontology:cs_authority_grounding('8f890314-bee8-4ce7-92db-b23b91cde283', expertise).
narrative_ontology:cs_interpretation_layer_present('8f890314-bee8-4ce7-92db-b23b91cde283').
narrative_ontology:cs_reading_relation('8f890314-bee8-4ce7-92db-b23b91cde283', animal_status_kernel__property_reading, forecloses).
narrative_ontology:cs_reading_relation('8f890314-bee8-4ce7-92db-b23b91cde283', animal_status_kernel__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('8f890314-bee8-4ce7-92db-b23b91cde283', foundational, animals_are_moral_persons).
narrative_ontology:cs_axiom_status(animals_are_moral_persons, holdable).
narrative_ontology:cs_axiom_grounding('8f890314-bee8-4ce7-92db-b23b91cde283', animals_are_moral_persons, deontological).
narrative_ontology:cs_axiom('8f890314-bee8-4ce7-92db-b23b91cde283', foundational, property_status_is_injustice).
narrative_ontology:cs_axiom_status(property_status_is_injustice, holdable).
narrative_ontology:cs_axiom_grounding('8f890314-bee8-4ce7-92db-b23b91cde283', property_status_is_injustice, deontological).
narrative_ontology:cs_reference_frame('8f890314-bee8-4ce7-92db-b23b91cde283', universal_moral_personhood).
narrative_ontology:cs_drift_state('8f890314-bee8-4ce7-92db-b23b91cde283', contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('8f890314-bee8-4ce7-92db-b23b91cde283', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, animal_rights_advocates).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animals_as_property).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_status_kernel__abolitionist_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_status_kernel__abolitionist_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_status_kernel__abolitionist_reading),
    narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_status_kernel__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is near maximal (0.95) because the very existence of animals as property is considered an act of extraction, violating their fundamental right to not be owned. Suppression is also near maximal (0.98) as the legal and economic systems actively enforce animal property status, leaving animals with virtually no exit options. Theater ratio is minimal (0.05) because this reading is a direct, uncompromising moral claim, with little performative aspect; its proponents are genuinely seeking fundamental change, not mere symbolic gestures. Accessibility collapse is high (0.99) because within this moral framework, there are no legitimate alternatives to recognizing animal personhood; any other status is a collapse of moral reasoning. Resistance is low (0.01) because the animals themselves cannot resist, and the advocates for this position are a minority against entrenched systems.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the abolitionist reading, the current system is a Snare for animals, extracting their very being. From the perspective of industrial animal agriculture, this reading is a Mountain of an unchangeable moral law that threatens their existence. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal rights advocates are beneficiaries as their moral framework is validated (d=0.0). Animals as property are the ultimate targets/victims (d=1.0). Industrial animal agriculture and legal systems are payers/agenda-setters, as they bear the cost of this moral challenge and would be forced to change (d=0.8-0.9). Welfare reform advocates are 'excluded' as their incremental approach is seen as counterproductive to the core abolitionist goal.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate is a foundational moral principle that is considered timeless. The 'founding problem' (systemic animal exploitation) is 'live', indicating no atrophy of the original moral imperative. The classification prevents mislabeling by emphasizing the categorical nature of the moral claim, distinguishing it from welfare-based approaches that might appear as 'tangled ropes' or 'snares' but operate within the property paradigm.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_morality,
    'Is the claim that animals are moral persons with a right not to be property a discovery of natural moral law (Mountain), or a constructed ethical framework (Snare/Tangled Rope for existing systems)?',
    'Philosophical consensus on meta-ethics, or widespread societal adoption of the framework leading to legal reform.',
    'If a natural moral law, its persistence is independent of human enforcement. If a constructed framework, its persistence depends on active advocacy and enforcement, making it a ''Snare'' for existing systems and a ''Rope'' for its adherents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_morality, conceptual, 'Ambiguity between inherent moral truth and human-constructed ethical system.').

omega_variable(
    welfare_reform_impact,
    'Does incremental welfare reform (as advocated by the ''welfare_reading'') ultimately advance or delay the abolitionist goal of ending animal property status?',
    'Empirical study of social movements and legal history: do welfare improvements lead to greater public acceptance of animal rights, or do they merely ''greenwash'' exploitation and reduce pressure for fundamental change?',
    'If welfare reforms delay abolition, the ''welfare_reading'' is structurally antagonistic to this ''abolitionist_reading''. If they advance it, the ''welfare_reading'' could be seen as a ''scaffold'' towards abolition, despite its current limitations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_impact, empirical, 'Strategic tension between incremental reform and categorical abolition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_status_kernel__abolitionist_reading, theater_ratio, 1970, 0.01).
narrative_ontology:measurement(anim_tr_t1980, animal_status_kernel__abolitionist_reading, theater_ratio, 1980, 0.02).
narrative_ontology:measurement(anim_tr_t1990, animal_status_kernel__abolitionist_reading, theater_ratio, 1990, 0.03).
narrative_ontology:measurement(anim_tr_t2000, animal_status_kernel__abolitionist_reading, theater_ratio, 2000, 0.04).
narrative_ontology:measurement(anim_tr_t2010, animal_status_kernel__abolitionist_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(anim_tr_t2024, animal_status_kernel__abolitionist_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_status_kernel__abolitionist_reading, base_extractiveness, 1970, 0.99).
narrative_ontology:measurement(anim_be_t1980, animal_status_kernel__abolitionist_reading, base_extractiveness, 1980, 0.98).
narrative_ontology:measurement(anim_be_t1990, animal_status_kernel__abolitionist_reading, base_extractiveness, 1990, 0.97).
narrative_ontology:measurement(anim_be_t2000, animal_status_kernel__abolitionist_reading, base_extractiveness, 2000, 0.96).
narrative_ontology:measurement(anim_be_t2010, animal_status_kernel__abolitionist_reading, base_extractiveness, 2010, 0.95).
narrative_ontology:measurement(anim_be_t2024, animal_status_kernel__abolitionist_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_status_kernel__abolitionist_reading, suppression_requirement, 1970, 0.99).
narrative_ontology:measurement(anim_su_t1980, animal_status_kernel__abolitionist_reading, suppression_requirement, 1980, 0.99).
narrative_ontology:measurement(anim_su_t1990, animal_status_kernel__abolitionist_reading, suppression_requirement, 1990, 0.98).
narrative_ontology:measurement(anim_su_t2000, animal_status_kernel__abolitionist_reading, suppression_requirement, 2000, 0.98).
narrative_ontology:measurement(anim_su_t2010, animal_status_kernel__abolitionist_reading, suppression_requirement, 2010, 0.98).
narrative_ontology:measurement(anim_su_t2024, animal_status_kernel__abolitionist_reading, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
