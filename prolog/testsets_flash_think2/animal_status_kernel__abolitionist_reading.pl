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
 *   human_readable: Abolitionist Reading of Animal Moral Status
 *   domain: Moral Philosophy/Animal Ethics/Legal Theory
 *
 * SUMMARY:
 *   This constraint story instantiates the abolitionist reading of the
 *   'animal_status_kernel', which posits that animals are moral persons with
 *   a fundamental right not to be property. From this perspective, the
 *   property status of animals is the root injustice, rendering all use
 *   categorically impermissible regardless of welfare conditions. The
 *   constraint describes the existing system of animal property and use as a
 *   snare, extracting maximally from animals and maintained by active
 *   suppression of alternatives to property status. The metrics reflect the
 *   abolitionist's assessment of this system, not a neutral or welfarist
 *   view.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status_kernel__abolitionist_reading, 0.95).
domain_priors:suppression_score(animal_status_kernel__abolitionist_reading, 0.9).
domain_priors:theater_ratio(animal_status_kernel__abolitionist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(animal_status_kernel__abolitionist_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Abolitionist Reading of Animal Moral Status").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "Moral Philosophy/Animal Ethics/Legal Theory").

domain_priors:requires_active_enforcement(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, '1e204ab7-e930-45b1-b945-8d0058da8e9e').
narrative_ontology:cs_kernel_codification('1e204ab7-e930-45b1-b945-8d0058da8e9e', implicit).
narrative_ontology:cs_authority_grounding('1e204ab7-e930-45b1-b945-8d0058da8e9e', extraction).
narrative_ontology:cs_interpretation_layer_present('1e204ab7-e930-45b1-b945-8d0058da8e9e').
narrative_ontology:cs_reading_relation('1e204ab7-e930-45b1-b945-8d0058da8e9e', animal_status_kernel__property_reading, forecloses).
narrative_ontology:cs_reading_relation('1e204ab7-e930-45b1-b945-8d0058da8e9e', animal_status_kernel__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('1e204ab7-e930-45b1-b945-8d0058da8e9e', foundational, animals_are_moral_persons).
narrative_ontology:cs_axiom_status(animals_are_moral_persons, holdable).
narrative_ontology:cs_axiom_grounding('1e204ab7-e930-45b1-b945-8d0058da8e9e', animals_are_moral_persons, deontological).
narrative_ontology:cs_axiom('1e204ab7-e930-45b1-b945-8d0058da8e9e', foundational, property_status_is_inherently_unjust).
narrative_ontology:cs_axiom_status(property_status_is_inherently_unjust, holdable).
narrative_ontology:cs_axiom_grounding('1e204ab7-e930-45b1-b945-8d0058da8e9e', property_status_is_inherently_unjust, deontological).
narrative_ontology:cs_reference_frame('1e204ab7-e930-45b1-b945-8d0058da8e9e', animals_as_moral_persons_framework).
narrative_ontology:cs_drift_state('1e204ab7-e930-45b1-b945-8d0058da8e9e', current_legal_property_status, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('1e204ab7-e930-45b1-b945-8d0058da8e9e', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, human_users_of_animals).
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, animal_exploitation_industries).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animals).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, animal_rights_doctrine).
narrative_ontology:constraint_vindicates(animal_status_kernel__abolitionist_reading, moral_personhood_of_animals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Are treated as property, subjected to use, exploitation, and death across various industries. They bear the full cost of this property status, with no legal recourse or ability to exit the system.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animals, payer,
    powerless, immediate, trapped, universal).

% Benefit from the property status of animals, using them for food, clothing, entertainment, research, and companionship. Their economic and cultural practices are deeply intertwined with animal exploitation, making exit from this system costly but not impossible.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, human_users_of_animals, beneficiary,
    powerful, biographical, constrained, global).

% These industries (e.g., factory farming, pharmaceutical testing, entertainment) actively shape and defend the legal and social framework that defines animals as property. They derive immense economic benefit from this status and resist any changes that would challenge it.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_exploitation_industries, agenda_setter,
    institutional, generational, constrained, global).

% Codify and enforce the property status of animals, providing the legal framework for their use and exploitation. While capable of reform, these systems are slow to change and often reflect the interests of dominant human stakeholders.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, legal_systems, agenda_setter,
    institutional, generational, constrained, global).

% Actively challenge the property status of animals, advocating for their moral personhood and fundamental rights. They operate outside the dominant system, seeking to dismantle it through legal, social, and ethical arguments.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, abolitionist_advocates, observer,
    organized, generational, constrained, global).

% Seek to improve the conditions of animals within their property status, focusing on reducing suffering. From an abolitionist perspective, their efforts, while well-intentioned, do not challenge the fundamental injustice of property status and may even entrench it by making exploitation seem more palatable.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animal_welfare_advocates, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The current system coordinates human society's use of animals as resources, ensuring a stable supply for various industries and cultural practices by legally defining them as property.
% TRANSFER_FUNCTION: Transfers the lives, bodies, labor, and reproductive capacities of animals to human benefit, enabling the production of food, clothing, research data, and entertainment.
% ABSENT_VOICES: Animals themselves are structurally absent from any decision-making or legal processes concerning their status. Those who would advocate for their categorical rights (abolitionist advocates) are largely excluded from mainstream legal and political discourse, often relegated to the fringes or dismissed as radical.
% DISAPPEARANCE_RATIONALE: If the property status of animals vanished overnight, the global economy would undergo a profound and immediate reorganization. Industries reliant on animal exploitation (e.g., meat, dairy, leather, fur, animal testing, zoos, circuses) would collapse or be forced to fundamentally transform. Legal systems would require a complete overhaul to recognize animal personhood, leading to massive societal shifts in diet, culture, and ethics.
% FOUNDING_PROBLEM: The historical problem of how to manage human-animal interactions and resource allocation, which was 'solved' by establishing animals as property, allowing for their systematic use and control by humans.
% FOUNDING_PROBLEM_CORROBORATION: Human users of animals and animal exploitation industries attest that the problem of resource allocation and human needs is still live, justifying animal property status. Legal systems implicitly corroborate this through their continued enforcement. Abolitionist advocates, however, dispute this, arguing that the 'founding problem' was a misframing that created the injustice, and that the arrangement persists as a mechanism of exploitation rather than a solution to a legitimate problem. Independent ethical philosophy and some scientific findings on animal sentience support the abolitionist critique.
narrative_ontology:disappearance_verdict(animal_status_kernel__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   Extractiveness is extremely high (0.95) because the entire existence of animals under property status is considered an extraction of their fundamental rights and lives. Suppression is also very high (0.90) as the legal and social systems actively enforce animal property status and suppress any challenges to it, including the development of alternatives. Theater ratio is low (0.10) because, from this reading, there is little performative maintenance; the system is brutally functional in its extraction. Accessibility collapse is high (0.85) because the legal and economic structures make it nearly impossible for animals to exit property status or for humans to easily opt out of systems built on it. Resistance is high (0.75) due to the ongoing and growing animal rights movement, which actively challenges this constraint.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading fundamentally diverges from both the property and welfare readings. While the property reading sees animals as mere resources and the welfare reading seeks to mitigate suffering within property status, the abolitionist reading views property status itself as the problem. This leads to a high extractiveness score for the existing system, which would be much lower or zero from a property reading, and moderate from a welfare reading (focused on suffering, not status). The engine's computation of per-seat classifications will highlight this structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals are the full targets (d=1.0) of this constraint, bearing all costs with no benefits. Human users and animal exploitation industries are the full beneficiaries (d=0.0), deriving immense economic and practical gains. Legal systems are agenda-setters, enforcing the constraint. Animal welfare advocates are 'excluded' in the sense that their incremental approach is seen as not addressing the fundamental injustice, thus not truly challenging the constraint's core. Abolitionist advocates are observers, analyzing and resisting the system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately representing the ''abolitionist_reading'' of the ''animal_status_kernel''?',
    'Comparison with foundational texts and contemporary discourse within the animal abolitionist movement.',
    'If misaligned, the classification of the existing system''s extractiveness and suppression from this perspective would be inaccurate, potentially understating the perceived injustice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Verifies the fidelity of this reading to the abolitionist philosophical position.').

omega_variable(
    strategic_tension_welfare_reading,
    'Do animal welfare reforms (as advocated by the ''welfare_reading'') ultimately delay or advance the goal of animal abolition?',
    'Longitudinal empirical study of social movements and legal changes: does improved welfare lead to greater public acceptance of animal use, or does it raise awareness that eventually challenges property status?',
    'If welfare reforms delay abolition, the ''welfare_reading'' might be seen as inadvertently reinforcing the ''property_reading'' from the abolitionist perspective. If they advance it, the strategic tension might be re-evaluated.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(strategic_tension_welfare_reading, empirical, 'Examines the strategic impact of incremental welfare reforms on the abolitionist goal.').

omega_variable(
    naturalness_of_property_status,
    'Is the property status of animals a ''natural'' or inevitable outcome of human-animal interaction, or is it a contingent social and legal construct?',
    'Anthropological and historical analysis of diverse human-animal relationships across cultures and eras, alongside philosophical arguments regarding moral status.',
    'If ''natural'', the constraint might lean towards a ''mountain'' from some perspectives, reducing perceived extractiveness. If ''constructed'', it reinforces the ''snare'' classification and the possibility of its abolition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_property_status, conceptual, 'Addresses the fundamental question of whether animal property status is a given or a choice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_status_kernel__abolitionist_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(anim_tr_t1980, animal_status_kernel__abolitionist_reading, theater_ratio, 1980, 0.11).
narrative_ontology:measurement(anim_tr_t1990, animal_status_kernel__abolitionist_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(anim_tr_t2000, animal_status_kernel__abolitionist_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(anim_tr_t2010, animal_status_kernel__abolitionist_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(anim_tr_t2025, animal_status_kernel__abolitionist_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_status_kernel__abolitionist_reading, base_extractiveness, 1970, 0.9).
narrative_ontology:measurement(anim_be_t1980, animal_status_kernel__abolitionist_reading, base_extractiveness, 1980, 0.91).
narrative_ontology:measurement(anim_be_t1990, animal_status_kernel__abolitionist_reading, base_extractiveness, 1990, 0.92).
narrative_ontology:measurement(anim_be_t2000, animal_status_kernel__abolitionist_reading, base_extractiveness, 2000, 0.93).
narrative_ontology:measurement(anim_be_t2010, animal_status_kernel__abolitionist_reading, base_extractiveness, 2010, 0.94).
narrative_ontology:measurement(anim_be_t2025, animal_status_kernel__abolitionist_reading, base_extractiveness, 2025, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_status_kernel__abolitionist_reading, suppression_requirement, 1970, 0.85).
narrative_ontology:measurement(anim_su_t1980, animal_status_kernel__abolitionist_reading, suppression_requirement, 1980, 0.86).
narrative_ontology:measurement(anim_su_t1990, animal_status_kernel__abolitionist_reading, suppression_requirement, 1990, 0.87).
narrative_ontology:measurement(anim_su_t2000, animal_status_kernel__abolitionist_reading, suppression_requirement, 2000, 0.88).
narrative_ontology:measurement(anim_su_t2010, animal_status_kernel__abolitionist_reading, suppression_requirement, 2010, 0.89).
narrative_ontology:measurement(anim_su_t2025, animal_status_kernel__abolitionist_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__abolitionist_reading, resource_allocation).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_welfare_regulations).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, food_production_standards).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, animal_testing_protocols).
narrative_ontology:affects_constraint(animal_status_kernel__abolitionist_reading, pet_ownership_laws).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'animal_status_kernel', each with different ε values and structural properties. The 'abolitionist_reading' focuses on the inherent injustice of property status, while 'property_reading' and 'welfare_reading' offer alternative framings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
