% ============================================================================
% CONSTRAINT STORY: animal_moral_status__abolitionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__abolitionist_reading, []).

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
 *   constraint_id: animal_moral_status__abolitionist_reading
 *   human_readable: Abolitionist Reading: Animals as Rights-Bearing Individuals
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of animal moral
 *   status, asserting that animals are rights-bearing individuals and their
 *   property status is itself a fundamental violation. All use, however
 *   'humane,' is seen as perpetuating this victimization. This reading
 *   instantiates a Snare, as it identifies clear victims (all animals under
 *   human dominion) and a high degree of extraction (the totality of animal
 *   use) and suppression (the legal and social structures upholding property
 *   status). The metrics reflect the abolitionist perspective on the current
 *   state of affairs, not a future ideal.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, 0.95).
domain_priors:suppression_score(animal_moral_status__abolitionist_reading, 0.98).
domain_priors:theater_ratio(animal_moral_status__abolitionist_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, accessibility_collapse, 0.1).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_moral_status__abolitionist_reading, "Abolitionist Reading: Animals as Rights-Bearing Individuals").
narrative_ontology:topic_domain(animal_moral_status__abolitionist_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, 'ec6d6dd6-9f2b-434b-a3e6-95bb33662858').
narrative_ontology:cs_kernel_codification('ec6d6dd6-9f2b-434b-a3e6-95bb33662858', distributed).
narrative_ontology:cs_authority_grounding('ec6d6dd6-9f2b-434b-a3e6-95bb33662858', extraction).
narrative_ontology:cs_interpretation_layer_present('ec6d6dd6-9f2b-434b-a3e6-95bb33662858').
narrative_ontology:cs_reading_relation('ec6d6dd6-9f2b-434b-a3e6-95bb33662858', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('ec6d6dd6-9f2b-434b-a3e6-95bb33662858', animal_moral_status__welfare_reading, forecloses).
narrative_ontology:cs_axiom('ec6d6dd6-9f2b-434b-a3e6-95bb33662858', foundational, animals_are_rights_bearing_individuals).
narrative_ontology:cs_axiom_status(animals_are_rights_bearing_individuals, holdable).
narrative_ontology:cs_axiom_grounding('ec6d6dd6-9f2b-434b-a3e6-95bb33662858', animals_are_rights_bearing_individuals, deontological).
narrative_ontology:cs_axiom('ec6d6dd6-9f2b-434b-a3e6-95bb33662858', foundational, property_status_is_the_violation).
narrative_ontology:cs_axiom_status(property_status_is_the_violation, holdable).
narrative_ontology:cs_axiom_grounding('ec6d6dd6-9f2b-434b-a3e6-95bb33662858', property_status_is_the_violation, deontological).
narrative_ontology:cs_reference_frame('ec6d6dd6-9f2b-434b-a3e6-95bb33662858', universal_animal_personhood).
narrative_ontology:cs_drift_state('ec6d6dd6-9f2b-434b-a3e6-95bb33662858', contemporary_legal_framework, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('ec6d6dd6-9f2b-434b-a3e6-95bb33662858', '').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, all_animals_under_human_dominion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for the legal and moral recognition of animals as rights-bearing individuals, seeking to dismantle the property status of animals. They frame all use, however 'humane,' as a violation of fundamental rights.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, abolitionist_advocates, agenda_setter,
    organized, generational, identity_locked, global).

% Are subjected to property status, which is seen as the fundamental violation. Their bodies, labor, and lives are used for human purposes, regardless of individual sentience or suffering, perpetuating a state of victimization.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, all_animals_under_human_dominion, payer,
    powerless, immediate, trapped, universal).

% Benefit from the property status of animals, enabling their use in agriculture, research, entertainment, and other sectors. From the abolitionist perspective, their existence is predicated on the violation of animal rights, and they would be dismantled if this constraint were universally adopted.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_use_industries, excluded,
    institutional, biographical, constrained, global).

% Seek to minimize animal suffering within existing systems of use, advocating for 'humane' treatment. From the abolitionist perspective, their efforts, while well-intentioned, inadvertently perpetuate the property status and fundamental violation of animal rights.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, welfare_advocates, excluded,
    organized, biographical, constrained, national).

% Currently uphold the property status of animals, providing the legal framework for their use. They are the target of abolitionist legal challenges and legislative efforts to redefine animal status.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, legal_systems, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading seeks to coordinate human behavior around a new moral and legal paradigm for animals, establishing a universal norm of non-use and respect for animal rights.
% TRANSFER_FUNCTION: Abolition of property status would transfer moral and legal standing from humans (as owners) to animals (as rights-holders), ending the transfer of animal lives, bodies, and labor for human benefit.
% ABSENT_VOICES: The animals themselves are the primary absent voices, unable to articulate their interests or consent to their property status. Their interests are represented by abolitionist advocates, but their direct voice is absent from the legal and moral discourse that defines their status.
% DISAPPEARANCE_RATIONALE: If the property status of animals disappeared overnight, the entire animal-use industry (agriculture, research, entertainment) would collapse, legal systems would undergo a fundamental reorientation, and human-animal relationships would be radically transformed. The world would rearrange itself around the recognition of animal personhood.
% FOUNDING_PROBLEM: The historical and ongoing problem of human exploitation and suffering inflicted upon animals, rooted in their legal classification as property.
% FOUNDING_PROBLEM_CORROBORATION: Abolitionist philosophers and legal scholars attest that the problem is profoundly live, citing the scale of animal suffering and the inherent injustice of property status. While animal-use industries dispute the 'problem' itself, the existence of widespread animal use is an undeniable empirical fact.
narrative_ontology:disappearance_verdict(animal_moral_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_moral_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__abolitionist_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__abolitionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(animal_moral_status__abolitionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is maximal (0.95) because the abolitionist reading views all instrumental use of animals as extraction, regardless of welfare conditions. Suppression is also maximal (0.98) due to the pervasive legal and social structures that enforce animal property status, making exit from this status virtually impossible for animals. Theater ratio is minimal (0.05) because, from this perspective, 'humane' reforms are seen as largely performative, masking the underlying violation of property status rather than addressing it. Accessibility collapse is low (0.1) and resistance is high (0.85) because the abolitionist position actively challenges the dominant paradigm, and alternatives (animal liberation) are not yet widely accessible or accepted.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading fundamentally diverges from both the property and welfare readings. While the property reading sees no extraction and the welfare reading sees extraction only in cruelty, the abolitionist reading sees extraction in property status itself. This leads to a maximal extractiveness score for the current system from the abolitionist seat, which would be much lower from the other seats.
 *
 * DIRECTIONALITY LOGIC:
 *   From the abolitionist perspective, animals are the ultimate targets/victims (d=1.0), bearing the full cost of their property status. Abolitionist advocates are agenda-setters, but their directionality is complex: they are beneficiaries of the moral clarity of the position (d=0.0), but also targets of the existing system's resistance (d=1.0). Animal-use industries are the beneficiaries of the current property status (d=0.0), while welfare advocates, though well-intentioned, are seen as inadvertently supporting the extractive system (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The abolitionist reading argues that the 'mandate' for animal use (e.g., for food, research) is fundamentally flawed and has always been a form of extraction, not coordination. Therefore, it doesn't see a mandate that has atrophied, but rather a foundational injustice that needs to be resolved. The classification as a Snare reflects this view of inherent, pervasive extraction rather than a degraded coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_status_naturalness,
    'Is the property status of animals a natural, inevitable feature of human-animal relations, or a socially constructed legal fiction?',
    'Comparative legal anthropology and historical analysis of animal status across cultures and eras. Philosophical arguments for inherent moral status independent of human decree.',
    'If natural, the abolitionist reading is a conceptual challenge to an immutable fact (closer to Mountain). If constructed, it is a challenge to a contingent legal/social Snare, with clear pathways for change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_status_naturalness, conceptual, 'Whether animal property status is natural or constructed.').

omega_variable(
    scope_of_personhood,
    'What are the necessary and sufficient conditions for moral personhood, and do animals meet them?',
    'Ongoing philosophical debate, scientific research into animal cognition and sentience, and legal precedent in cases challenging animal status.',
    'A broader definition of personhood would strengthen the abolitionist claim, potentially shifting the constraint towards a more widely recognized Snare. A narrower definition would weaken it, making it a more niche, contested claim.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_personhood, empirical, 'The philosophical and scientific basis for animal personhood.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, economic dependency) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., animals in sanctuaries still exhibit trauma responses), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for animals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__abolitionist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(anim_tr_t10, animal_moral_status__abolitionist_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(anim_tr_t20, animal_moral_status__abolitionist_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(anim_tr_t30, animal_moral_status__abolitionist_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(anim_tr_t40, animal_moral_status__abolitionist_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(anim_tr_t50, animal_moral_status__abolitionist_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__abolitionist_reading, base_extractiveness, 0, 0.95).
narrative_ontology:measurement(anim_be_t10, animal_moral_status__abolitionist_reading, base_extractiveness, 10, 0.95).
narrative_ontology:measurement(anim_be_t20, animal_moral_status__abolitionist_reading, base_extractiveness, 20, 0.95).
narrative_ontology:measurement(anim_be_t30, animal_moral_status__abolitionist_reading, base_extractiveness, 30, 0.95).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__abolitionist_reading, base_extractiveness, 40, 0.95).
narrative_ontology:measurement(anim_be_t50, animal_moral_status__abolitionist_reading, base_extractiveness, 50, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__abolitionist_reading, suppression_requirement, 0, 0.98).
narrative_ontology:measurement(anim_su_t10, animal_moral_status__abolitionist_reading, suppression_requirement, 10, 0.98).
narrative_ontology:measurement(anim_su_t20, animal_moral_status__abolitionist_reading, suppression_requirement, 20, 0.98).
narrative_ontology:measurement(anim_su_t30, animal_moral_status__abolitionist_reading, suppression_requirement, 30, 0.98).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__abolitionist_reading, suppression_requirement, 40, 0.98).
narrative_ontology:measurement(anim_su_t50, animal_moral_status__abolitionist_reading, suppression_requirement, 50, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
