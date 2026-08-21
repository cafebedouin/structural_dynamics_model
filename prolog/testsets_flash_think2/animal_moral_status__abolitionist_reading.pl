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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   human_readable: Animal Property Status (Abolitionist Reading)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of animal moral
 *   status, asserting that animals are rights-bearing individuals and their
 *   legal status as property constitutes a fundamental violation. All forms
 *   of use, even those deemed 'humane,' are seen as perpetuating this
 *   victimization. The constraint describes the existing legal and social
 *   arrangement where animals are property, viewed through an abolitionist
 *   lens. This reading stands in direct opposition to the property_reading
 *   and welfare_reading of the same kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, 0.95).
domain_priors:suppression_score(animal_moral_status__abolitionist_reading, 0.98).
domain_priors:theater_ratio(animal_moral_status__abolitionist_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_moral_status__abolitionist_reading, "Animal Property Status (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_moral_status__abolitionist_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, '9dc5448b-6bcd-4ec9-9605-962b08791757').
narrative_ontology:cs_kernel_codification('9dc5448b-6bcd-4ec9-9605-962b08791757', formalized).
narrative_ontology:cs_authority_grounding('9dc5448b-6bcd-4ec9-9605-962b08791757', lineage).
narrative_ontology:cs_interpretation_layer_present('9dc5448b-6bcd-4ec9-9605-962b08791757').
narrative_ontology:cs_reading_relation('9dc5448b-6bcd-4ec9-9605-962b08791757', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('9dc5448b-6bcd-4ec9-9605-962b08791757', animal_moral_status__welfare_reading, forecloses).
narrative_ontology:cs_axiom('9dc5448b-6bcd-4ec9-9605-962b08791757', foundational, animal_sentience_implies_rights).
narrative_ontology:cs_axiom_status(animal_sentience_implies_rights, holdable).
narrative_ontology:cs_axiom_grounding('9dc5448b-6bcd-4ec9-9605-962b08791757', animal_sentience_implies_rights, deontological).
narrative_ontology:cs_axiom('9dc5448b-6bcd-4ec9-9605-962b08791757', foundational, property_status_is_fundamental_violation).
narrative_ontology:cs_axiom_status(property_status_is_fundamental_violation, holdable).
narrative_ontology:cs_axiom_grounding('9dc5448b-6bcd-4ec9-9605-962b08791757', property_status_is_fundamental_violation, deontological).
narrative_ontology:cs_reference_frame('9dc5448b-6bcd-4ec9-9605-962b08791757', animal_as_property_framework).
narrative_ontology:cs_drift_state('9dc5448b-6bcd-4ec9-9605-962b08791757', contemporary_animal_rights_movement, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('9dc5448b-6bcd-4ec9-9605-962b08791757', '').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, human_users_of_animals).
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, all_animals_under_human_dominion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the legal and social status of animals as property, enabling their use in agriculture, research, entertainment, and companionship. Actively defends this status through legal frameworks and cultural norms. Exit would require fundamental societal restructuring.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, human_users_of_animals, agenda_setter,
    institutional, generational, constrained, global).

% Bears the full cost of property status, including loss of bodily autonomy, agency, and life. Their interests are legally subordinate to human interests, and they have no means of legal or physical exit from this status.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, all_animals_under_human_dominion, payer,
    powerless, immediate, trapped, universal).

% Advocates for the abolition of animal property status and all forms of animal use. Operates outside the dominant legal and social frameworks that define animals as property, often facing legal and social resistance. Their voice is largely excluded from policy-making bodies that uphold animal property status.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_rights_activists, observer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(animal_moral_status__abolitionist_reading, animal_rights_activists, excluded).

% Enforces the legal status of animals as property, adjudicating disputes and upholding regulations that govern their use. Its structure is deeply intertwined with the historical and philosophical lineage of human dominion over animals.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, legal_system, agenda_setter,
    institutional, civilizational, analytical, national).

% Benefits from the availability of animal products and services (food, clothing, entertainment, pets) made possible by their property status. Can choose to reduce or eliminate consumption of animal products, but the underlying property status remains.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, consumers, beneficiary,
    moderate, biographical, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the abolitionist perspective, the constraint of animal property status does not serve a genuine coordination function for animals; rather, it coordinates human society around the systematic exploitation of animals, facilitating their commodification and use.
% TRANSFER_FUNCTION: Transfers bodily autonomy, agency, and life from animals to humans, enabling the extraction of resources, labor, and companionship for human benefit.
% ABSENT_VOICES: The animals themselves are structurally absent from any legal or ethical deliberation regarding their status. If present, they would object to their property status and all forms of use.
% DISAPPEARANCE_RATIONALE: If animal property status vanished overnight, human societies would undergo a profound and immediate restructuring of their food systems, scientific research, entertainment industries, and even companion animal relationships. The economic, legal, and ethical foundations of human-animal interaction would be fundamentally altered.
% FOUNDING_PROBLEM: The historical establishment of animals as property was driven by human needs and desires for resources, labor, and control, solving the 'problem' of how to systematically utilize other species for human benefit without moral or legal impediment.
% FOUNDING_PROBLEM_CORROBORATION: While human users of animals assert the continued necessity of this status for societal function, animal rights philosophers, legal scholars, and activists corroborate that the 'problem' of human desire for animal exploitation remains live, perpetuating the property status despite evolving ethical considerations.
narrative_ontology:disappearance_verdict(animal_moral_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness is extremely high (0.95) because the very existence of animals as property is considered a complete extraction of their inherent rights and autonomy. Suppression is near total (0.98) as animals are legally voiceless and physically controlled. Theater ratio is low (0.15) because, from this perspective, 'humane' treatment laws are largely performative, serving to legitimize continued use rather than genuinely addressing the fundamental violation of property status. Resistance is high (0.70) due to ongoing activism and philosophical challenges to this status. Accessibility collapse is high (0.90) as animals are legally trapped in their property status.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading fundamentally diverges from other perspectives (e.g., welfare, property) on the nature of the constraint itself. While others might see a coordination function or minimal extraction, the abolitionist view sees pure, systemic extraction. The engine's computation of per-seat classification will highlight this divergence, showing a snare from the animal's seat and a perceived rope/tangled_rope from the human user's seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Human users of animals are the primary beneficiaries, as they derive direct and indirect benefits from animal property status. All animals under human dominion are the direct and complete targets/victims. Animal rights activists operate as observers and are largely excluded from the institutional mechanisms that uphold the constraint. The legal system acts as an agenda-setter, enforcing the property status.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_status_natural_vs_constructed,
    'Is the legal status of animals as property a natural, inevitable outcome of human-animal interaction, or a socially and historically constructed constraint?',
    'Comparative legal and anthropological studies of societies with different human-animal relationships, or philosophical analysis of the contingency of legal personhood.',
    'If constructed, it strengthens the argument for its alterability and the ethical imperative for abolition. If natural, it might imply a more fundamental, ''mountain-like'' aspect, though still extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_status_natural_vs_constructed, conceptual, 'Ambiguity of animal property status as natural law or human construct.').

omega_variable(
    welfare_as_coordination_or_cover,
    'Does ''humane'' treatment of animals genuinely mitigate suffering and represent a form of coordination, or does it primarily serve as a social license and cover for continued extraction?',
    'Empirical studies on the effectiveness of welfare regulations in reducing animal suffering, combined with critical analysis of their role in public perception and industry practices.',
    'If primarily cover, it reinforces the snare classification and the high theater_ratio. If genuine coordination, it might suggest a tangled_rope element, though the abolitionist reading would still see it as insufficient.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_as_coordination_or_cover, empirical, 'Role of humane treatment in the context of property status.').

omega_variable(
    definition_of_rights_bearing_individual,
    'What are the necessary and sufficient conditions for an entity to be considered a ''rights-bearing individual'' in a legal and moral sense, and do animals meet these criteria?',
    'Philosophical and legal debate on personhood, sentience, and moral considerability, potentially influenced by scientific advancements in animal cognition.',
    'A broader definition of rights-bearing individuals that includes animals would strengthen the abolitionist claim and challenge the legal system''s current framework.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_rights_bearing_individual, conceptual, 'Scope and criteria for rights-bearing personhood.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__abolitionist_reading, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1800, animal_moral_status__abolitionist_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(anim_tr_t1850, animal_moral_status__abolitionist_reading, theater_ratio, 1850, 0.07).
narrative_ontology:measurement(anim_tr_t1900, animal_moral_status__abolitionist_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(anim_tr_t1950, animal_moral_status__abolitionist_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(anim_tr_t2000, animal_moral_status__abolitionist_reading, theater_ratio, 2000, 0.14).
narrative_ontology:measurement(anim_tr_t2024, animal_moral_status__abolitionist_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(anim_be_t1800, animal_moral_status__abolitionist_reading, base_extractiveness, 1800, 0.95).
narrative_ontology:measurement(anim_be_t1850, animal_moral_status__abolitionist_reading, base_extractiveness, 1850, 0.95).
narrative_ontology:measurement(anim_be_t1900, animal_moral_status__abolitionist_reading, base_extractiveness, 1900, 0.95).
narrative_ontology:measurement(anim_be_t1950, animal_moral_status__abolitionist_reading, base_extractiveness, 1950, 0.95).
narrative_ontology:measurement(anim_be_t2000, animal_moral_status__abolitionist_reading, base_extractiveness, 2000, 0.95).
narrative_ontology:measurement(anim_be_t2024, animal_moral_status__abolitionist_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1800, animal_moral_status__abolitionist_reading, suppression_requirement, 1800, 0.98).
narrative_ontology:measurement(anim_su_t1850, animal_moral_status__abolitionist_reading, suppression_requirement, 1850, 0.98).
narrative_ontology:measurement(anim_su_t1900, animal_moral_status__abolitionist_reading, suppression_requirement, 1900, 0.98).
narrative_ontology:measurement(anim_su_t1950, animal_moral_status__abolitionist_reading, suppression_requirement, 1950, 0.98).
narrative_ontology:measurement(anim_su_t2000, animal_moral_status__abolitionist_reading, suppression_requirement, 2000, 0.98).
narrative_ontology:measurement(anim_su_t2024, animal_moral_status__abolitionist_reading, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__abolitionist_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
