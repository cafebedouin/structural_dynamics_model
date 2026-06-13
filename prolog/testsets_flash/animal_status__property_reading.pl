% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__property_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: animal_status__property_reading
 *   human_readable: Animal Status as Property (Property Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint defines the legal status of animals as property, without
 *   independent moral standing, with human ownership being largely
 *   unrestricted except by minimal welfare statutes. This is the 'property
 *   reading' of the broader 'animal_status' kernel. From this perspective,
 *   the constraint is a foundational legal principle, akin to a natural law,
 *   with negligible extraction from human actors, as it primarily clarifies
 *   ownership and use. Animals are not considered victims within this
 *   framework, as they lack standing.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__property_reading, 0.05).
domain_priors:suppression_score(animal_status__property_reading, 0.02).
domain_priors:theater_ratio(animal_status__property_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_status__property_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(animal_status__property_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, mountain).
narrative_ontology:human_readable(animal_status__property_reading, "Animal Status as Property (Property Reading)").
narrative_ontology:topic_domain(animal_status__property_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:emerges_naturally(animal_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, '6f759a5d-0b1b-4045-846f-e8033bfb0639').
narrative_ontology:cs_kernel_codification('6f759a5d-0b1b-4045-846f-e8033bfb0639', formalized).
narrative_ontology:cs_authority_grounding('6f759a5d-0b1b-4045-846f-e8033bfb0639', lineage).
narrative_ontology:cs_interpretation_layer_present('6f759a5d-0b1b-4045-846f-e8033bfb0639').
narrative_ontology:cs_reading_relation('6f759a5d-0b1b-4045-846f-e8033bfb0639', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('6f759a5d-0b1b-4045-846f-e8033bfb0639', animal_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('6f759a5d-0b1b-4045-846f-e8033bfb0639', foundational, animals_are_chattel).
narrative_ontology:cs_axiom_status(animals_are_chattel, holdable).
narrative_ontology:cs_axiom_grounding('6f759a5d-0b1b-4045-846f-e8033bfb0639', animals_are_chattel, conventional).
narrative_ontology:cs_axiom('6f759a5d-0b1b-4045-846f-e8033bfb0639', foundational, human_interests_precede_animal_interests).
narrative_ontology:cs_axiom_status(human_interests_precede_animal_interests, holdable).
narrative_ontology:cs_axiom_grounding('6f759a5d-0b1b-4045-846f-e8033bfb0639', human_interests_precede_animal_interests, deontological).
narrative_ontology:cs_reference_frame('6f759a5d-0b1b-4045-846f-e8033bfb0639', classical_roman_law_property_status).
narrative_ontology:cs_drift_state('6f759a5d-0b1b-4045-846f-e8033bfb0639', contemporary_animal_rights_discourse, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('6f759a5d-0b1b-4045-846f-e8033bfb0639', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, animal_owners).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__property_reading, biomedical_researchers).
narrative_ontology:constraint_vindicates(animal_status__property_reading, human_dominion_doctrine).
narrative_ontology:constraint_vindicates(animal_status__property_reading, property_rights_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds legal title to animals, with rights to use, sell, or dispose of them, subject only to minimal welfare standards. Benefits from the clear legal status and lack of independent claims from animals.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_owners, beneficiary,
    powerful, biographical, arbitrage, local).

% Relies on the legal status of animals as property for its economic model, allowing for large-scale breeding, raising, and slaughter without acknowledging independent moral standing. Benefits from minimal regulatory overhead.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_agriculture_industry, beneficiary,
    institutional, generational, arbitrage, global).

% Utilizes animals in research, benefiting from their legal status as property which permits experimentation under welfare guidelines, without requiring consent or acknowledging rights that would impede research.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, biomedical_researchers, beneficiary,
    organized, generational, constrained, national).

% Operates within the property framework, advocating for improved welfare standards for animals as property, but not challenging the fundamental legal status. Their efforts are directed at influencing owners and legislators.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, welfare_advocates, observer,
    moderate, generational, constrained, national).

% Enforces property laws regarding animals and adjudicates disputes, treating animals as chattel. Defines the boundaries of permissible human action towards animals within this framework.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, legal_system, agenda_setter,
    institutional, civilizational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear legal ownership and transferability of animals, facilitating their use in agriculture, research, and companionship by defining them as property.
% TRANSFER_FUNCTION: Legally transfers ownership rights over animals from one human to another, and implicitly transfers the 'burden' of moral consideration away from animals themselves to human discretion.
% ABSENT_VOICES: Animals themselves, who, if they could speak, would object to their status as mere property and demand independent moral standing. Abolitionist and strong welfare advocates are largely excluded from the foundational legal discourse that defines this status.
% DISAPPEARANCE_RATIONALE: If animals ceased to be legal property overnight, the entire animal agriculture industry, pet ownership, and biomedical research would face an immediate, profound crisis of legitimacy and legality, requiring a complete re-evaluation of human-animal relationships and economic structures.
% FOUNDING_PROBLEM: To establish clear legal frameworks for human interaction with and utilization of animals, resolving disputes over ownership and use in a way that prioritizes human interests and societal utility.
% FOUNDING_PROBLEM_CORROBORATION: The legal system and animal owners attest that the problem of managing human-animal interactions and resource allocation remains live. Welfare advocates, while disagreeing with the solution, acknowledge the historical problem of establishing order in these interactions. No external corroboration is sought for the 'problem' itself, only for the 'solution's' appropriateness.
narrative_ontology:disappearance_verdict(animal_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_status__property_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__property_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_status__property_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_status__property_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_status__property_reading),
    narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_status__property_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is near zero (0.05) because, from the perspective of human actors, this constraint primarily provides clarity and enables economic activity, rather than extracting from them. Suppression is also minimal (0.02) as the legal status is widely accepted and requires little active coercion to maintain among human parties. The theater ratio is negligible (0.01) as the constraint's function is direct and not performative. The high accessibility_collapse (0.95) and low resistance (0.01) reflect its deep entrenchment as a legal and philosophical default.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of those who benefit from animal ownership, this constraint is a stable, efficient framework. From the perspective of abolitionists (an excluded voice), this constraint is a snare of immense, unacknowledged extraction from animals. This story, however, strictly adheres to the 'property reading' where animals are not moral agents and thus cannot be 'victims' or 'targets' of extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal owners, the agriculture industry, and biomedical researchers are clear beneficiaries, as this reading grants them extensive rights and minimizes obligations. The legal system acts as the agenda-setter, codifying and enforcing this status. Animals themselves are not considered agents with directionality in this framework. Welfare advocates are observers, working within the existing framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    animal_moral_standing_ambiguity,
    'Is the ''property reading'' of animal status a genuine reflection of natural order, or a constructed legal framework that benefits human interests by denying independent moral standing to animals?',
    'Philosophical consensus shift on animal sentience and consciousness, or a legal paradigm shift granting animals personhood or rights.',
    'If animals are found to have independent moral standing, this ''property reading'' would be reclassified from a Mountain to a Snare, with animals as primary victims, and its extractiveness would be re-evaluated as extremely high.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(animal_moral_standing_ambiguity, conceptual, 'Ambiguity regarding the inherent moral status of animals and its implications for legal classification.').

omega_variable(
    welfare_statutes_as_extraction_cover,
    'Are existing welfare statutes genuine constraints on human ownership, or do they primarily serve to legitimize the ''property reading'' by providing a minimal, performative ''care'' layer?',
    'Empirical analysis of the effectiveness of welfare statutes in preventing suffering, and their impact on the economic viability of animal-using industries. Comparison with jurisdictions with stronger animal protection laws.',
    'If welfare statutes are largely performative, the ''property reading''s'' low extractiveness and suppression would be revealed as a cover for deeper, unacknowledged extraction from animals, pushing it towards a Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_statutes_as_extraction_cover, empirical, 'The role of welfare statutes in either constraining or legitimizing animal property status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 1600, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1600, animal_status__property_reading, theater_ratio, 1600, 0.01).
narrative_ontology:measurement(anim_tr_t1700, animal_status__property_reading, theater_ratio, 1700, 0.01).
narrative_ontology:measurement(anim_tr_t1800, animal_status__property_reading, theater_ratio, 1800, 0.01).
narrative_ontology:measurement(anim_tr_t1900, animal_status__property_reading, theater_ratio, 1900, 0.01).
narrative_ontology:measurement(anim_tr_t2000, animal_status__property_reading, theater_ratio, 2000, 0.01).
narrative_ontology:measurement(anim_tr_t2024, animal_status__property_reading, theater_ratio, 2024, 0.01).

% Extraction over time
narrative_ontology:measurement(anim_be_t1600, animal_status__property_reading, base_extractiveness, 1600, 0.05).
narrative_ontology:measurement(anim_be_t1700, animal_status__property_reading, base_extractiveness, 1700, 0.05).
narrative_ontology:measurement(anim_be_t1800, animal_status__property_reading, base_extractiveness, 1800, 0.05).
narrative_ontology:measurement(anim_be_t1900, animal_status__property_reading, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(anim_be_t2000, animal_status__property_reading, base_extractiveness, 2000, 0.05).
narrative_ontology:measurement(anim_be_t2024, animal_status__property_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1600, animal_status__property_reading, suppression_requirement, 1600, 0.02).
narrative_ontology:measurement(anim_su_t1700, animal_status__property_reading, suppression_requirement, 1700, 0.02).
narrative_ontology:measurement(anim_su_t1800, animal_status__property_reading, suppression_requirement, 1800, 0.02).
narrative_ontology:measurement(anim_su_t1900, animal_status__property_reading, suppression_requirement, 1900, 0.02).
narrative_ontology:measurement(anim_su_t2000, animal_status__property_reading, suppression_requirement, 2000, 0.02).
narrative_ontology:measurement(anim_su_t2024, animal_status__property_reading, suppression_requirement, 2024, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
