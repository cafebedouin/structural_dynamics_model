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
 *   constraint_id: animal_status_kernel__abolitionist_reading
 *   human_readable: Abolitionist Reading: Animals as Non-Property Persons
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of animal status:
 *   animals are moral persons with a fundamental right not to be property.
 *   The property status itself is the injustice, making all use categorically
 *   impermissible regardless of welfare conditions. This reading views the
 *   current legal and social arrangement as a snare, extracting maximum value
 *   from animals by denying their personhood. The high extractiveness and
 *   suppression reflect the systemic nature of animal exploitation under
 *   property law. The claimed type is 'snare' because the coordination story
 *   (human benefit from animal use) is seen as a cover for pure extraction,
 *   maintained by coercion and suppression of alternatives (animal
 *   personhood).
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
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Abolitionist Reading: Animals as Non-Property Persons").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, '49964c08-20d9-4bbe-9794-fa0ba8801340').
narrative_ontology:cs_kernel_codification('49964c08-20d9-4bbe-9794-fa0ba8801340', formalized).
narrative_ontology:cs_authority_grounding('49964c08-20d9-4bbe-9794-fa0ba8801340', lineage).
narrative_ontology:cs_interpretation_layer_present('49964c08-20d9-4bbe-9794-fa0ba8801340').
narrative_ontology:cs_reading_relation('49964c08-20d9-4bbe-9794-fa0ba8801340', animal_status_kernel__property_reading, forecloses).
narrative_ontology:cs_reading_relation('49964c08-20d9-4bbe-9794-fa0ba8801340', animal_status_kernel__welfare_reading, influences).
narrative_ontology:cs_axiom('49964c08-20d9-4bbe-9794-fa0ba8801340', foundational, animals_are_moral_persons).
narrative_ontology:cs_axiom_status(animals_are_moral_persons, holdable).
narrative_ontology:cs_axiom_grounding('49964c08-20d9-4bbe-9794-fa0ba8801340', animals_are_moral_persons, deontological).
narrative_ontology:cs_axiom('49964c08-20d9-4bbe-9794-fa0ba8801340', foundational, property_status_is_injustice).
narrative_ontology:cs_axiom_status(property_status_is_injustice, holdable).
narrative_ontology:cs_axiom_grounding('49964c08-20d9-4bbe-9794-fa0ba8801340', property_status_is_injustice, deontological).
narrative_ontology:cs_reference_frame('49964c08-20d9-4bbe-9794-fa0ba8801340', universal_moral_personhood).
narrative_ontology:cs_drift_state('49964c08-20d9-4bbe-9794-fa0ba8801340', contemporary, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('49964c08-20d9-4bbe-9794-fa0ba8801340', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, biomedical_research_institutions).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, pet_industry).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, hunting_fishing_industries).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animals_as_property).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, animal_rights_theory).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, personhood_ethics).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for the complete legal and moral abolition of animal property status, viewing any use as a violation of fundamental rights. They seek to dismantle existing systems of animal exploitation and are often in tension with welfare reform efforts.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, abolitionist_advocates, agenda_setter,
    organized, generational, identity_locked, global).

% Are legally defined as property, subject to human ownership, use, and disposal. Their interests are not recognized as rights, and their lives are systematically exploited across various industries, regardless of welfare conditions.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animals_as_property, payer,
    powerless, immediate, trapped, universal).

% Benefits immensely from the property status of animals, which allows for their commodification and large-scale exploitation for food production. Any challenge to property status is a direct threat to their business model.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_agriculture_industry, beneficiary,
    institutional, biographical, constrained, global).

% Relies on the property status of animals for their use in experiments and testing. The abolitionist reading directly challenges the ethical and legal basis of their research practices.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, biomedical_research_institutions, beneficiary,
    institutional, biographical, constrained, global).

% Enforce the property status of animals through legislation, court rulings, and regulatory frameworks. They are the primary institutional mechanism for maintaining the current arrangement, despite growing ethical challenges.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, legal_systems, agenda_setter,
    institutional, generational, constrained, national).

% Seek to improve the conditions of animals within their property status, focusing on reducing suffering. From an abolitionist perspective, their efforts are seen as perpetuating the fundamental injustice of property status, rather than challenging it.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, welfare_reform_advocates, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The current legal and moral framework coordinates human access to and use of animals as resources, facilitating industries that rely on animal exploitation.
% TRANSFER_FUNCTION: Transfers the lives, bodies, and labor of animals from their inherent moral status to human ownership and economic value, enabling industries to profit from their use.
% ABSENT_VOICES: Animals themselves are structurally absent from any legal or moral discourse that determines their status. Their interests are represented by advocates, but they cannot speak for themselves. Future generations, who might inherit a world without animal exploitation, are also absent.
% DISAPPEARANCE_RATIONALE: If the property status of animals vanished overnight, the global animal agriculture, biomedical research, and pet industries would collapse or be forced to fundamentally restructure. Legal systems would need to redefine personhood and rights, leading to a profound reorganization of human-animal relations and economic activity.
% FOUNDING_PROBLEM: The historical problem was the need for human societies to control and utilize animals for food, labor, and other resources, leading to their classification as property.
% FOUNDING_PROBLEM_CORROBORATION: The animal agriculture and research industries attest that the 'problem' of resource scarcity and scientific advancement is still live, requiring animal use. Abolitionist advocates, however, argue that the 'problem' is not a genuine necessity but a constructed justification for exploitation, and that the true problem is the moral status of animals. Independent ethical philosophers and some legal scholars corroborate the abolitionist framing of the problem.
narrative_ontology:disappearance_verdict(animal_status_kernel__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_status_kernel__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status_kernel__abolitionist_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status_kernel__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_status_kernel__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) because the constraint permits the total instrumentalization of animals, denying their inherent value and reducing them to commodities. Suppression is also extremely high (0.98) due to the pervasive legal and social structures that enforce animal property status, making exit (i.e., non-property status) virtually impossible for animals. Theater ratio is low (0.05) because, from this reading, there is little performative 'welfare' activity that genuinely mitigates the fundamental injustice of property status; any such activity is seen as merely cosmetic or delaying true abolition. Accessibility collapse is low (0.1) because the abolitionist perspective sees a clear alternative (animal personhood) that is actively suppressed, not naturally collapsed. Resistance is high (0.85) due to ongoing and growing animal rights activism.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading fundamentally diverges from both the property and welfare readings. From the perspective of industries benefiting from animal property, the arrangement is a necessary coordination mechanism for resource provision. From the welfare perspective, it's a system needing reform. The abolitionist reading sees it as a pure snare, where the 'coordination' is a cover for systemic extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals (as property) are the full targets (d=1.0) of this constraint, bearing all costs. Industries that profit from animal use (animal agriculture, biomedical research, pet industry, hunting/fishing) are the primary beneficiaries (d=0.0-0.1). Legal systems are agenda-setters, enforcing the property status. Welfare reform advocates are excluded from the abolitionist's core agenda, as their efforts are seen as perpetuating the problem.
 *
 * MANDATROPHY ANALYSIS:
 *   From the abolitionist perspective, the mandate for animal property status has not atrophied; rather, it was never legitimate. The constraint persists not because its original function is still needed, but because powerful beneficiaries actively maintain it for extraction. The classification as a snare prevents mislabeling this as a legitimate coordination mechanism or a degraded piton, emphasizing the active, coercive nature of the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_reform_impact,
    'Does welfare reform (as advocated by the welfare_reading) advance or delay the abolition of animal property status?',
    'Longitudinal study of social movements and legal changes in jurisdictions with strong welfare laws vs. those without. Analysis of whether welfare reforms lead to increased public awareness and eventual abolitionist sentiment, or if they pacify public concern and entrench animal use.',
    'If welfare reforms delay abolition, the welfare_reading is structurally complicit in maintaining the snare. If they advance it, the strategic tension between abolitionist and welfare advocates is misaligned, and the welfare_reading could be seen as an incremental scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_impact, empirical, 'Strategic impact of welfare reforms on abolitionist goals.').

omega_variable(
    personhood_definition_ambiguity,
    'What are the necessary and sufficient criteria for moral personhood, and do animals meet them?',
    'Ongoing philosophical debate, scientific advancements in animal cognition and sentience, and legal precedent setting. No single empirical test is likely to be definitive.',
    'If animals are definitively recognized as moral persons, the abolitionist reading gains stronger legal and ethical grounding, potentially shifting the constraint towards a mountain (natural law of personhood) or a rope (coordination around personhood). If not, the property status remains more defensible.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(personhood_definition_ambiguity, conceptual, 'Ambiguity in the definition and application of moral personhood to animals.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of animal personhood structural (legal barriers, economic dependency) or internalized (cultural norms, speciesism)?',
    'Post-legal-change societal shifts: if property status is legally removed in some jurisdictions, observe whether cultural practices and individual attitudes towards animal use persist or rapidly change. If suppression persists after legal barriers are removed, it indicates a strong internalized component.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the cultural inertia would continue to bind even after legal changes. This would make the path to abolition more complex, requiring deep cultural shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for animal property status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_status_kernel__abolitionist_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(anim_tr_t1980, animal_status_kernel__abolitionist_reading, theater_ratio, 1980, 0.05).
narrative_ontology:measurement(anim_tr_t1990, animal_status_kernel__abolitionist_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(anim_tr_t2000, animal_status_kernel__abolitionist_reading, theater_ratio, 2000, 0.05).
narrative_ontology:measurement(anim_tr_t2010, animal_status_kernel__abolitionist_reading, theater_ratio, 2010, 0.05).
narrative_ontology:measurement(anim_tr_t2024, animal_status_kernel__abolitionist_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_status_kernel__abolitionist_reading, base_extractiveness, 1970, 0.9).
narrative_ontology:measurement(anim_be_t1980, animal_status_kernel__abolitionist_reading, base_extractiveness, 1980, 0.92).
narrative_ontology:measurement(anim_be_t1990, animal_status_kernel__abolitionist_reading, base_extractiveness, 1990, 0.93).
narrative_ontology:measurement(anim_be_t2000, animal_status_kernel__abolitionist_reading, base_extractiveness, 2000, 0.94).
narrative_ontology:measurement(anim_be_t2010, animal_status_kernel__abolitionist_reading, base_extractiveness, 2010, 0.94).
narrative_ontology:measurement(anim_be_t2024, animal_status_kernel__abolitionist_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_status_kernel__abolitionist_reading, suppression_requirement, 1970, 0.9).
narrative_ontology:measurement(anim_su_t1980, animal_status_kernel__abolitionist_reading, suppression_requirement, 1980, 0.92).
narrative_ontology:measurement(anim_su_t1990, animal_status_kernel__abolitionist_reading, suppression_requirement, 1990, 0.94).
narrative_ontology:measurement(anim_su_t2000, animal_status_kernel__abolitionist_reading, suppression_requirement, 2000, 0.96).
narrative_ontology:measurement(anim_su_t2010, animal_status_kernel__abolitionist_reading, suppression_requirement, 2010, 0.97).
narrative_ontology:measurement(anim_su_t2024, animal_status_kernel__abolitionist_reading, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__abolitionist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__property_reading).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_status_kernel__welfare_reading).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, factory_farming_regulations).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, biomedical_research_ethics).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'animal_status_kernel'. It is linked to the 'property_reading' and 'welfare_reading' as part of a constraint family, representing different interpretations of animal moral and legal status.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
