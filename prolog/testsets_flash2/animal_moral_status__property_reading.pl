% ============================================================================
% CONSTRAINT STORY: animal_moral_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_moral_status__property_reading, []).

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
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: animal_moral_status__property_reading
 *   human_readable: Animals as Property: Subordinate Moral Standing
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'property reading' of animal moral status,
 *   where animals are legally and morally defined as property or resources,
 *   with their interests inherently subordinate to human interests. This
 *   reading views the arrangement as a foundational 'mountain' of legal and
 *   ethical thought, enabling human use of animals. The low extractiveness
 *   reflects the view that property rights themselves are not extractive from
 *   the perspective of the property owner, and the constraint primarily
 *   defines a boundary of non-extraction for humans. This is one reading of
 *   the 'animal_moral_status' kernel, distinct from welfare or abolitionist
 *   readings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.05).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.02).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Animals as Property: Subordinate Moral Standing").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, 'eb5849c8-9111-4e01-b31e-d0e60afbc4f8').
narrative_ontology:cs_kernel_codification('eb5849c8-9111-4e01-b31e-d0e60afbc4f8', formalized).
narrative_ontology:cs_authority_grounding('eb5849c8-9111-4e01-b31e-d0e60afbc4f8', lineage).
narrative_ontology:cs_interpretation_layer_present('eb5849c8-9111-4e01-b31e-d0e60afbc4f8').
narrative_ontology:cs_reading_relation('eb5849c8-9111-4e01-b31e-d0e60afbc4f8', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('eb5849c8-9111-4e01-b31e-d0e60afbc4f8', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('eb5849c8-9111-4e01-b31e-d0e60afbc4f8', foundational, animals_are_property_resources).
narrative_ontology:cs_axiom_status(animals_are_property_resources, holdable).
narrative_ontology:cs_axiom_grounding('eb5849c8-9111-4e01-b31e-d0e60afbc4f8', animals_are_property_resources, conventional).
narrative_ontology:cs_axiom('eb5849c8-9111-4e01-b31e-d0e60afbc4f8', foundational, human_interests_are_primary).
narrative_ontology:cs_axiom_status(human_interests_are_primary, holdable).
narrative_ontology:cs_axiom_grounding('eb5849c8-9111-4e01-b31e-d0e60afbc4f8', human_interests_are_primary, deontological).
narrative_ontology:cs_reference_frame('eb5849c8-9111-4e01-b31e-d0e60afbc4f8', classical_property_law).
narrative_ontology:cs_drift_state('eb5849c8-9111-4e01-b31e-d0e60afbc4f8', contemporary_animal_welfare_movement, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('eb5849c8-9111-4e01-b31e-d0e60afbc4f8', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_property_owners).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_product_consumers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds legal title to animals, exercising full rights of use, sale, and disposition. Benefits from the legal and social framework that defines animals as property, enabling industries like agriculture, research, and entertainment. Their interests are prioritized by this constraint.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_property_owners, beneficiary,
    powerful, generational, arbitrage, global).

% Benefits from the availability and affordability of animal products (food, clothing, medicine) that result from animals being treated as resources. Their consumption choices are enabled by the property status of animals.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_product_consumers, beneficiary,
    moderate, biographical, mobile, global).

% Are legally defined as property, lacking independent moral standing or rights. Their interests are considered only insofar as they align with human interests or prevent economic waste. They bear the full cost of instrumentalization.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animals_as_property, payer,
    powerless, immediate, trapped, universal).
narrative_ontology:stakeholder_non_agent(animal_moral_status__property_reading, animals_as_property).

% Codifies and enforces the legal status of animals as property, providing the framework for ownership, use, and trade. Adjudicates disputes based on this foundational premise, treating animals as objects of human dominion.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, legal_systems, agenda_setter,
    institutional, civilizational, constrained, national).

% Seek to improve the conditions of animals within the existing property framework, but their core challenge to property status itself is outside the scope of this reading. Their arguments for expanded welfare protections are often framed as preventing 'cruelty' rather than challenging fundamental rights.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_welfare_advocates, excluded,
    organized, generational, constrained, global).

% Fundamentally reject the property status of animals, arguing for their rights-bearing individual status. Their position is foreclosed by this reading's foundational axiom and they are structurally excluded from the discourse that accepts animals as property.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, abolitionist_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universally understood framework for human interaction with animals, enabling their instrumental use across diverse industries (agriculture, research, entertainment) by defining them as resources.
% TRANSFER_FUNCTION: Transfers the full control and benefit of animal lives and bodies to human owners and users, in exchange for minimal obligations related to preventing wanton cruelty or economic waste.
% ABSENT_VOICES: Abolitionist advocates are structurally excluded from the conversation, as their core premise (animals as rights-bearers) directly contradicts the foundational axiom of this reading. Animals themselves, as non-agents, have no voice.
% DISAPPEARANCE_RATIONALE: If the property status of animals vanished overnight, the global agricultural, pharmaceutical, and entertainment industries would collapse or undergo radical transformation. Legal systems would need to redefine fundamental concepts of ownership and rights, leading to a complete rearrangement of human-animal relations.
% FOUNDING_PROBLEM: To establish a clear, stable, and efficient framework for human dominion over animals, enabling their instrumental use for human benefit without ambiguity or legal challenge.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars and industry bodies consistently affirm the necessity of animal property status for economic stability and human welfare. While challenged by animal rights movements, the legal and social consensus, as reflected in legislation and judicial precedent, continues to uphold this status.
narrative_ontology:disappearance_verdict(animal_moral_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__property_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_moral_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__property_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_moral_status__property_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, ExtMetricName, E),
    domain_priors:suppression_score(animal_moral_status__property_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(animal_moral_status__property_reading),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(animal_moral_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a 'mountain' from the perspective of this reading because it is presented as a fundamental, unchangeable aspect of human-animal relations and legal systems. Extractiveness is very low (0.05) because, from the perspective of property owners, defining something as property is not an 'extraction' but a definition of ownership and rights. Suppression is minimal (0.02) as the concept is deeply embedded in legal and social structures, requiring little active enforcement against internal dissent. Resistance is low (0.05) because, within this framework, challenges are typically framed as welfare concerns rather than fundamental property rights. Accessibility collapse is high (0.95) as alternatives to property status are largely foreclosed by this foundational premise.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of animal property owners, this is a natural and efficient arrangement. From the perspective of abolitionist advocates (a sibling reading), this same structure is a snare of pure extraction and violence. This constraint story captures only the 'property reading' perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Animal property owners and consumers are direct beneficiaries, as the constraint enables their use and consumption of animals. Legal systems act as agenda-setters, codifying and enforcing this status. Animals themselves are the 'payers' in a structural sense, bearing the cost of instrumentalization, but are not 'victims' in the extractive sense from this reading's perspective, as their status is defined as non-moral-standing. Animal welfare and abolitionist advocates are excluded, as their positions challenge the core premise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the property status of animals a ''natural law'' (a mountain) or a ''social construct'' (a snare or tangled rope) that benefits identifiable human agents?',
    'Historical and anthropological analysis of varying human-animal legal/moral frameworks across cultures and time, coupled with philosophical arguments for animal sentience and moral considerability.',
    'If determined to be a social construct, the constraint would be reclassified from a mountain to a snare or tangled rope, with significantly higher extractiveness and suppression, and animals would be re-categorized as victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity regarding the fundamental nature of animal property status.').

omega_variable(
    scope_of_moral_consideration,
    'What is the appropriate scope of moral consideration, and does it inherently exclude non-human animals from independent moral standing?',
    'Philosophical debate and scientific advancements in understanding animal cognition, sentience, and capacity for suffering. Public discourse and evolving ethical norms.',
    'If the scope of moral consideration expands to include animals with independent standing, the foundational axiom of this reading would be challenged, leading to a re-evaluation of the constraint''s legitimacy and classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_moral_consideration, preference, 'Uncertainty about the boundaries of moral considerability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_moral_status__property_reading, theater_ratio, 0, 0.01).
narrative_ontology:measurement(anim_tr_t25, animal_moral_status__property_reading, theater_ratio, 25, 0.01).
narrative_ontology:measurement(anim_tr_t50, animal_moral_status__property_reading, theater_ratio, 50, 0.01).
narrative_ontology:measurement(anim_tr_t75, animal_moral_status__property_reading, theater_ratio, 75, 0.01).
narrative_ontology:measurement(anim_tr_t100, animal_moral_status__property_reading, theater_ratio, 100, 0.01).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_moral_status__property_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(anim_be_t25, animal_moral_status__property_reading, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(anim_be_t50, animal_moral_status__property_reading, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(anim_be_t75, animal_moral_status__property_reading, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(anim_be_t100, animal_moral_status__property_reading, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_moral_status__property_reading, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(anim_su_t25, animal_moral_status__property_reading, suppression_requirement, 25, 0.02).
narrative_ontology:measurement(anim_su_t50, animal_moral_status__property_reading, suppression_requirement, 50, 0.02).
narrative_ontology:measurement(anim_su_t75, animal_moral_status__property_reading, suppression_requirement, 75, 0.02).
narrative_ontology:measurement(anim_su_t100, animal_moral_status__property_reading, suppression_requirement, 100, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
