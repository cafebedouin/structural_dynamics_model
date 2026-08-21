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
    narrative_ontology:coordination_type/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: animal_moral_status__property_reading
 *   human_readable: Animal Property Status (Property Reading)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'property reading' of animal moral status,
 *   where animals are legally and ethically defined as property or resources,
 *   with their interests inherently subordinate to human interests. From this
 *   perspective, the constraint is a foundational principle, akin to a
 *   natural law, enabling human use and economic activity involving animals.
 *   It is one reading of the broader 'animal_moral_status' kernel, which also
 *   includes 'welfare_reading' and 'abolitionist_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.05).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.9).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Animal Property Status (Property Reading)").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, '79178ae7-49b6-43fc-9bb2-c5436e1b7354').
narrative_ontology:cs_kernel_codification('79178ae7-49b6-43fc-9bb2-c5436e1b7354', formalized).
narrative_ontology:cs_authority_grounding('79178ae7-49b6-43fc-9bb2-c5436e1b7354', lineage).
narrative_ontology:cs_interpretation_layer_present('79178ae7-49b6-43fc-9bb2-c5436e1b7354').
narrative_ontology:cs_reading_relation('79178ae7-49b6-43fc-9bb2-c5436e1b7354', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('79178ae7-49b6-43fc-9bb2-c5436e1b7354', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('79178ae7-49b6-43fc-9bb2-c5436e1b7354', foundational, animals_are_chattel).
narrative_ontology:cs_axiom_status(animals_are_chattel, holdable).
narrative_ontology:cs_axiom_grounding('79178ae7-49b6-43fc-9bb2-c5436e1b7354', animals_are_chattel, conventional).
narrative_ontology:cs_axiom('79178ae7-49b6-43fc-9bb2-c5436e1b7354', foundational, human_interests_are_primary).
narrative_ontology:cs_axiom_status(human_interests_are_primary, holdable).
narrative_ontology:cs_axiom_grounding('79178ae7-49b6-43fc-9bb2-c5436e1b7354', human_interests_are_primary, deontological).
narrative_ontology:cs_reference_frame('79178ae7-49b6-43fc-9bb2-c5436e1b7354', human_dominion_framework).
narrative_ontology:cs_drift_state('79178ae7-49b6-43fc-9bb2-c5436e1b7354', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('79178ae7-49b6-43fc-9bb2-c5436e1b7354', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, human_property_owners).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_resource_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the legal and ethical framework that defines animals as property, allowing for their use, sale, and consumption without independent moral claims. This status underpins significant economic and cultural practices.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, human_property_owners, agenda_setter,
    institutional, generational, arbitrage, universal).

% Relies entirely on the property status of animals to operate. This includes agriculture, research, entertainment, and pet industries. The constraint provides the fundamental legal basis for their existence and profitability.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_resource_industries, beneficiary,
    organized, biographical, mobile, global).

% Codifies and upholds the property status of animals, providing the framework for ownership, transfer, and use. It interprets and applies laws that define animals as chattel, while also managing limited anti-cruelty provisions that do not challenge the core property status.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, legal_system, agenda_setter,
    institutional, civilizational, constrained, national).

% Seek to improve the conditions of animals within the property framework, advocating for stronger anti-cruelty laws and better treatment. While they challenge specific practices, their efforts generally do not (from this reading's perspective) challenge the fundamental property status itself, thus remaining 'excluded' from the core definitional debate.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_welfare_advocates, excluded,
    moderate, generational, constrained, global).

% Fundamentally reject the property status of animals, arguing for their independent moral standing and rights. From the property reading's perspective, their claims are outside the established legal and ethical framework and are thus 'excluded' from the legitimate discourse on animal status.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, abolitionist_advocates, excluded,
    powerless, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universal framework for human ownership, use, and management of animals as resources, facilitating economic activity, scientific research, and cultural practices by defining their legal status.
% TRANSFER_FUNCTION: Legitimizes the transfer of animals and animal products as commodities, and the benefits derived from their use (e.g., food, labor, companionship), from animals to human owners and users.
% ABSENT_VOICES: Animals themselves, lacking legal personhood, have no voice in defining their status. Abolitionist advocates, who challenge the very premise of animal property, are structurally excluded from the legal and philosophical frameworks that uphold this reading.
% DISAPPEARANCE_RATIONALE: If animals were suddenly no longer considered property, the global agricultural, research, entertainment, and pet industries would face immediate collapse. Legal systems would require fundamental redefinition of rights and responsibilities, leading to widespread economic and social reorganization.
% FOUNDING_PROBLEM: To establish clear legal and ethical frameworks for human interaction with and use of animals as resources, preventing disputes over ownership and facilitating their integration into human economies and societies.
% FOUNDING_PROBLEM_CORROBORATION: Historical legal codes (e.g., Roman law, common law traditions), philosophical texts on human dominion, and contemporary agricultural and scientific bodies consistently affirm the necessity of animal property status for societal function. This corroboration comes from within the established legal and economic systems that benefit from the constraint, but is presented as self-evident truth.
narrative_ontology:disappearance_verdict(animal_moral_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__property_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The constraint is classified as a Mountain from this reading's perspective because it defines a fundamental, 'natural' order of human-animal relations. Extractiveness is very low (0.05) because using property as property is not considered 'extraction' but normal operation within this framework. Suppression is high (0.9) as this framework actively suppresses any alternative legal or ethical standing for animals. Theater ratio is low (0.1) as it's a core, functional principle, not performative. Resistance is low from within the framework, as it's the established norm. The metrics are stable over time, reflecting the foundational nature of this principle from its own perspective.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human property owners and industries, this constraint is a self-evident, beneficial framework. From the perspective of abolitionist advocates (an excluded seat), it is a fundamental injustice and a snare. The engine's classification will highlight this divergence by computing a Mountain for the beneficiaries and a Snare for the excluded abolitionists, despite the constraint's self-proclaimed Mountain status.
 *
 * DIRECTIONALITY LOGIC:
 *   Human property owners and animal resource industries are clear beneficiaries, as the constraint directly legitimizes their activities and economic models. The legal system acts as an agenda-setter, codifying and enforcing this status. Animal welfare advocates are 'excluded' from challenging the core property status, focusing instead on improving conditions within it. Abolitionist advocates are also 'excluded' as their fundamental rejection of property status places them outside the legitimate discourse of this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_construct,
    'Is the property status of animals a genuine natural law of human-animal relations, or a constructed legal and philosophical framework that primarily benefits human property owners?',
    'Comparative legal anthropology across diverse cultures, historical analysis of legal evolution, and philosophical debate on the basis of moral standing. If it is found to be a contingent social construct, its ''emerges_naturally'' claim is falsified.',
    'If resolved as a construct, the constraint''s ''mountain'' claim would be reclassified, likely to a ''tangled_rope'' or ''snare'' from the perspective of those it defines as property, triggering the False Summit Mountain detection.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_construct, conceptual, 'Ambiguity of the constraint''s naturalness claim.').

omega_variable(
    impact_of_abolitionist_challenge,
    'How would the widespread adoption of the abolitionist reading (animals as rights-bearing individuals) structurally alter the property reading''s persistence and legitimacy?',
    'Analysis of legal and social shifts in jurisdictions where animal rights movements gain significant traction, or counterfactual modeling of economic and legal system responses to a shift in animal status.',
    'If the abolitionist reading gains sufficient traction, the property reading''s ''suppression'' and ''accessibility_collapse'' metrics would decrease, and its ''resistance'' would increase, potentially leading to a reclassification from Mountain to a more actively contested type.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(impact_of_abolitionist_challenge, empirical, 'The potential for an alternative reading to destabilize the property reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 1800, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1800, animal_moral_status__property_reading, theater_ratio, 1800, 0.1).
narrative_ontology:measurement(anim_tr_t1900, animal_moral_status__property_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(anim_tr_t2024, animal_moral_status__property_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(anim_be_t1800, animal_moral_status__property_reading, base_extractiveness, 1800, 0.05).
narrative_ontology:measurement(anim_be_t1900, animal_moral_status__property_reading, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(anim_be_t2024, animal_moral_status__property_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1800, animal_moral_status__property_reading, suppression_requirement, 1800, 0.9).
narrative_ontology:measurement(anim_su_t1900, animal_moral_status__property_reading, suppression_requirement, 1900, 0.9).
narrative_ontology:measurement(anim_su_t2024, animal_moral_status__property_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_moral_status__property_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
