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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: animal_moral_status__abolitionist_reading
 *   human_readable: Abolitionist Reading: Animals as Rights-Bearing Individuals
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of animal moral
 *   status, where animals are considered rights-bearing individuals and their
 *   property status is the fundamental violation. All use, however 'humane,'
 *   is seen as perpetuating victimization. This reading posits the existing
 *   legal and social framework for animal use as a Snare, characterized by
 *   extremely high extraction and suppression, with animals as the primary
 *   victims. The claimed type 'snare' reflects the abolitionist view that the
 *   coordination story (e.g., efficient food production) is merely cover for
 *   systemic extraction.
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
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, 'a40d5e63-da52-4630-b7b8-cfe60dfc7100').
narrative_ontology:cs_kernel_codification('a40d5e63-da52-4630-b7b8-cfe60dfc7100', formalized).
narrative_ontology:cs_authority_grounding('a40d5e63-da52-4630-b7b8-cfe60dfc7100', extraction).
narrative_ontology:cs_interpretation_layer_present('a40d5e63-da52-4630-b7b8-cfe60dfc7100').
narrative_ontology:cs_reading_relation('a40d5e63-da52-4630-b7b8-cfe60dfc7100', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('a40d5e63-da52-4630-b7b8-cfe60dfc7100', animal_moral_status__welfare_reading, forecloses).
narrative_ontology:cs_axiom('a40d5e63-da52-4630-b7b8-cfe60dfc7100', foundational, animals_are_rights_bearing_individuals).
narrative_ontology:cs_axiom_status(animals_are_rights_bearing_individuals, holdable).
narrative_ontology:cs_axiom_grounding('a40d5e63-da52-4630-b7b8-cfe60dfc7100', animals_are_rights_bearing_individuals, deontological).
narrative_ontology:cs_axiom('a40d5e63-da52-4630-b7b8-cfe60dfc7100', foundational, property_status_is_the_fundamental_violation).
narrative_ontology:cs_axiom_status(property_status_is_the_fundamental_violation, holdable).
narrative_ontology:cs_axiom_grounding('a40d5e63-da52-4630-b7b8-cfe60dfc7100', property_status_is_the_fundamental_violation, deontological).
narrative_ontology:cs_reference_frame('a40d5e63-da52-4630-b7b8-cfe60dfc7100', universal_animal_personhood).
narrative_ontology:cs_drift_state('a40d5e63-da52-4630-b7b8-cfe60dfc7100', contemporary_legal_framework, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a40d5e63-da52-4630-b7b8-cfe60dfc7100', '').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, all_animals_under_human_dominion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, animal_use_industries).
narrative_ontology:constraint_vindicates(animal_moral_status__abolitionist_reading, animal_rights_doctrine).
narrative_ontology:constraint_vindicates(animal_moral_status__abolitionist_reading, sentience_as_moral_basis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively campaign for the legal and social recognition of animal personhood and the abolition of animal property status. They frame all animal use as a violation of fundamental rights.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, abolitionist_advocates, agenda_setter,
    organized, generational, analytical, global).

% Are structurally positioned as property, subject to human use and exploitation across all domains (food, clothing, entertainment, research). Their interests are systematically subordinated to human interests, leading to pervasive victimization.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, all_animals_under_human_dominion, payer,
    powerless, immediate, trapped, universal).

% Focus on minimizing animal suffering within existing systems of use, advocating for 'humane' treatment. From the abolitionist perspective, their efforts, while well-intentioned, implicitly legitimize property status and thus perpetuate the fundamental violation.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_welfare_organizations, excluded,
    organized, biographical, constrained, global).

% Benefit from the legal property status of animals, which allows for their commodification and instrumental use. They actively resist any redefinition of animal status that would challenge their business models.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_use_industries, beneficiary,
    institutional, generational, arbitrage, global).

% Currently uphold the property status of animals, providing the legal framework for their use. They are the primary enforcers of the existing constraint, even if not direct beneficiaries of the extraction.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, legal_systems, agenda_setter,
    institutional, civilizational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the abolitionist perspective, the constraint itself does not solve a legitimate coordination problem; rather, it coordinates the systematic exploitation of animals by defining them as property, facilitating their instrumental use across human society.
% TRANSFER_FUNCTION: Transfers the autonomy, bodily integrity, and lives of animals to human control and use, enabling the extraction of resources, labor, and entertainment from them.
% ABSENT_VOICES: The animals themselves are the primary absent voices, unable to articulate their interests or consent to their property status. Their interests are represented by abolitionist advocates, but their direct voice is structurally excluded.
% DISAPPEARANCE_RATIONALE: If the property status of animals vanished overnight, human society would undergo a profound moral and economic reorganization. Industries reliant on animal use would collapse or transform, legal systems would need to redefine personhood and rights, and human-animal relationships would be fundamentally re-evaluated. The world would be unrecognizable.
% FOUNDING_PROBLEM: The problem of how to organize human society to efficiently utilize animals as resources for food, labor, and other purposes, without acknowledging their independent moral standing.
% FOUNDING_PROBLEM_CORROBORATION: Animal use industries and many legal scholars attest that the problem of organizing animal use is still live and necessary for human flourishing. Abolitionist advocates, however, contest the legitimacy of the 'problem' itself, arguing it is a constructed justification for exploitation, not a genuine problem to be solved.
narrative_ontology:disappearance_verdict(animal_moral_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is near maximal (0.95) because the constraint denies fundamental rights and autonomy to sentient beings, reducing them to property. Suppression is also near maximal (0.98) because the legal and social systems actively enforce this property status, making exit (i.e., non-use) virtually impossible for animals. Theater ratio is very low (0.05) because, from this perspective, 'humane' use is not a genuine attempt at coordination but a performative act that masks the underlying extraction; the core function is exploitation, not support. Resistance is high (0.85) due to ongoing advocacy by abolitionist movements.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading fundamentally diverges from property and welfare readings. While property readings see animals as resources and welfare readings seek to mitigate suffering within use, the abolitionist reading views property status itself as the violation. This leads to a maximal extractiveness and suppression score, reflecting the structural violence inherent in the property paradigm, which is not acknowledged by the other readings.
 *
 * DIRECTIONALITY LOGIC:
 *   From the abolitionist perspective, animals are the full targets (d=1.0) of this constraint, bearing all costs without benefit. Animal use industries are the primary beneficiaries (d=0.0), profiting directly from the property status. Legal systems act as agenda-setters, enforcing the constraint. Animal welfare organizations, while well-intentioned, are seen as implicitly supporting the property status, thus contributing to the constraint's persistence, even if they do not directly benefit from the extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   From the abolitionist perspective, the constraint's mandate (to facilitate human use of animals) has not atrophied but is fundamentally illegitimate. The classification as a Snare prevents mislabeling this as a coordination mechanism, highlighting the coercive and extractive nature of animal property status. The persistence of the constraint is due to the immense economic benefits for animal use industries and the deeply entrenched legal and social norms, not a genuine coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_status_contingency,
    'Is the property status of animals a contingent legal construct, or an inevitable outcome of human-animal relations?',
    'Legal and philosophical analysis of historical shifts in legal personhood and property, and cross-cultural studies of human-animal relationships that do not rely on property status.',
    'If contingent, it strengthens the argument for abolition as a feasible legal reform. If inevitable, it suggests the constraint is closer to a Mountain, making abolition a more radical, perhaps impossible, reordering of fundamental human existence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_status_contingency, conceptual, 'Whether animal property status is a mutable legal construct or an immutable feature of human society.').

omega_variable(
    sentience_as_moral_basis_universality,
    'Is sentience a universally accepted and sufficient basis for moral personhood and rights, or are other criteria (e.g., rationality, self-awareness) also necessary?',
    'Ongoing philosophical debate and scientific advancements in understanding animal cognition and consciousness. Shifts in public and legal consensus on moral criteria.',
    'If sentience is sufficient, it strengthens the abolitionist claim. If other criteria are deemed necessary, it complicates the universal application of rights to all animals, potentially weakening the abolitionist position for some species.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sentience_as_moral_basis_universality, conceptual, 'The scope and sufficiency of sentience as a basis for moral rights.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, economic dependency) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-abolition trajectory: if suppression of animal interests persists after legal property status is removed, reclassify as partially internalized (e.g., through cultural habits of objectification).',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target (animals) carries the suppression with them after legal exit, making true liberation more complex.',
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
narrative_ontology:measurement(anim_be_t0, animal_moral_status__abolitionist_reading, base_extractiveness, 0, 0.9).
narrative_ontology:measurement(anim_be_t10, animal_moral_status__abolitionist_reading, base_extractiveness, 10, 0.92).
narrative_ontology:measurement(anim_be_t20, animal_moral_status__abolitionist_reading, base_extractiveness, 20, 0.93).
narrative_ontology:measurement(anim_be_t30, animal_moral_status__abolitionist_reading, base_extractiveness, 30, 0.94).
narrative_ontology:measurement(anim_be_t40, animal_moral_status__abolitionist_reading, base_extractiveness, 40, 0.95).
narrative_ontology:measurement(anim_be_t50, animal_moral_status__abolitionist_reading, base_extractiveness, 50, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__abolitionist_reading, suppression_requirement, 0, 0.95).
narrative_ontology:measurement(anim_su_t10, animal_moral_status__abolitionist_reading, suppression_requirement, 10, 0.96).
narrative_ontology:measurement(anim_su_t20, animal_moral_status__abolitionist_reading, suppression_requirement, 20, 0.97).
narrative_ontology:measurement(anim_su_t30, animal_moral_status__abolitionist_reading, suppression_requirement, 30, 0.98).
narrative_ontology:measurement(anim_su_t40, animal_moral_status__abolitionist_reading, suppression_requirement, 40, 0.98).
narrative_ontology:measurement(anim_su_t50, animal_moral_status__abolitionist_reading, suppression_requirement, 50, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__abolitionist_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'animal_moral_status' kernel, focusing on the abolition of property status. It is linked to the 'property_reading' and 'welfare_reading' as alternative interpretations of the same kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
