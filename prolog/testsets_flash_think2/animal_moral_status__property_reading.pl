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
 *   human_readable: Animals as Property: Subordinate Moral Status
 *   domain: applied_ethics/legal_philosophy
 *
 * SUMMARY:
 *   This constraint defines animals as property or resources, inherently
 *   subordinate to human interests, and lacking independent moral standing.
 *   It is a specific reading of the broader 'animal_moral_status' kernel,
 *   which is contested by other readings. From the perspective of this
 *   'property_reading', the constraint itself is a foundational truth, not an
 *   extractive mechanism, hence the low extractiveness and suppression. The
 *   classification as 'mountain' reflects its proponents' view of it as an
 *   unchangeable, definitional aspect of reality or law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__property_reading, 0.05).
domain_priors:suppression_score(animal_moral_status__property_reading, 0.1).
domain_priors:theater_ratio(animal_moral_status__property_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__property_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(animal_moral_status__property_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__property_reading, mountain).
narrative_ontology:human_readable(animal_moral_status__property_reading, "Animals as Property: Subordinate Moral Status").
narrative_ontology:topic_domain(animal_moral_status__property_reading, "applied_ethics/legal_philosophy").

domain_priors:emerges_naturally(animal_moral_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__property_reading, 'a6c373bd-8afb-4e9e-96d7-9ce428028a16').
narrative_ontology:cs_kernel_codification('a6c373bd-8afb-4e9e-96d7-9ce428028a16', formalized).
narrative_ontology:cs_authority_grounding('a6c373bd-8afb-4e9e-96d7-9ce428028a16', lineage).
narrative_ontology:cs_interpretation_layer_present('a6c373bd-8afb-4e9e-96d7-9ce428028a16').
narrative_ontology:cs_reading_relation('a6c373bd-8afb-4e9e-96d7-9ce428028a16', animal_moral_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6c373bd-8afb-4e9e-96d7-9ce428028a16', animal_moral_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('a6c373bd-8afb-4e9e-96d7-9ce428028a16', foundational, animals_are_chattel).
narrative_ontology:cs_axiom_status(animals_are_chattel, holdable).
narrative_ontology:cs_axiom_grounding('a6c373bd-8afb-4e9e-96d7-9ce428028a16', animals_are_chattel, conventional).
narrative_ontology:cs_axiom('a6c373bd-8afb-4e9e-96d7-9ce428028a16', foundational, human_dominion_over_animals).
narrative_ontology:cs_axiom_status(human_dominion_over_animals, holdable).
narrative_ontology:cs_axiom_grounding('a6c373bd-8afb-4e9e-96d7-9ce428028a16', human_dominion_over_animals, conventional).
narrative_ontology:cs_reference_frame('a6c373bd-8afb-4e9e-96d7-9ce428028a16', classical_property_doctrine).
narrative_ontology:cs_drift_state('a6c373bd-8afb-4e9e-96d7-9ce428028a16', contemporary_animal_rights_movement, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('a6c373bd-8afb-4e9e-96d7-9ce428028a16', '').
narrative_ontology:cs_kernel_id(animal_moral_status__property_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, property_owners).
narrative_ontology:constraint_beneficiary(animal_moral_status__property_reading, animal_use_industries).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals and entities who own animals, exercising full control over their use, disposition, and treatment within the bounds of general property law. They benefit from the clear, unambiguous status of animals as resources.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, property_owners, agenda_setter,
    powerful, biographical, mobile, global).

% Industries (e.g., agriculture, biomedical research, entertainment) whose business models rely on the instrumental use of animals. This constraint provides the foundational legal and ethical justification for their operations.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_use_industries, beneficiary,
    institutional, generational, arbitrage, global).

% The body of laws and courts that codify and enforce animal property status, providing a framework for ownership, transfer, and dispute resolution. It upholds the definitional claim of animals as property.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, legal_system, agenda_setter,
    institutional, civilizational, constrained, national).

% Scholars who analyze the philosophical underpinnings and implications of animal property status, often defending its coherence within established ethical and legal traditions.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, analytical_philosophers, observer,
    analytical, civilizational, analytical, universal).

% Groups and individuals who argue for the protection of animals from cruelty and suffering, but whose core premise of animal sentience and interests is subordinated or denied independent moral weight by this 'property' reading. Their fundamental challenge to property status is excluded from this reading's framework.
narrative_ontology:constraint_stakeholder(animal_moral_status__property_reading, animal_welfare_advocates, excluded,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear ownership and control over animals, facilitating their use in agriculture, research, and companionship without moral ambiguity regarding their status, thereby coordinating human interaction with animals.
% TRANSFER_FUNCTION: Transfers full control and decision-making power over animals to human owners, allowing for the extraction of labor, products, and companionship, and defining animals as objects of human economic and personal benefit.
% ABSENT_VOICES: Animal rights advocates and the animals themselves are structurally excluded from having a voice in defining their moral status within this framework; their claims to independent moral standing are dismissed by definition.
% DISAPPEARANCE_RATIONALE: If the definition of animals as property with subordinate moral status vanished overnight, the entire legal and economic framework for animal use (agriculture, biomedical research, pet ownership) would be fundamentally destabilized, necessitating a complete re-evaluation of animal-human relations and property law.
% FOUNDING_PROBLEM: To provide a clear, unambiguous framework for human interaction with and use of animals, resolving potential conflicts over ownership and purpose, and ensuring human access to animal resources.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars specializing in property law, agricultural industry bodies, and some philosophical traditions corroborate this as a foundational principle that continues to serve its purpose. However, animal welfare and rights advocates contest its ongoing legitimacy.
narrative_ontology:disappearance_verdict(animal_moral_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__property_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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
 *   The low extractiveness (0.05) and suppression (0.1) reflect that, from the perspective of this reading, animals are defined as resources, so the constraint is not 'extracting' from them but rather defining their status. The high accessibility_collapse (0.9) indicates that this definition fundamentally collapses any alternative moral standing for animals within this framework. Resistance (0.1) is low from the internal perspective of this reading's adherents, as challenges come from external, competing frameworks. The metrics are stable over time, reflecting the definitional and foundational nature of this claim within its own tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of property owners, this constraint is a fundamental, non-extractive truth that enables orderly use of resources. From the perspective of animal rights advocates (an excluded voice), this same constraint is the ultimate snare, enabling systemic extraction and suffering. The engine's per-seat classification will highlight this divergence, which is rooted in the definitional nature of the constraint itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Property owners and animal-use industries are direct beneficiaries, gaining clear rights and justification for use. The legal system acts as an agenda-setter, codifying and enforcing this status. Animal welfare advocates are 'excluded' because their core premise of independent animal interests is rejected by this reading's definition, making their voice absent from the internal logic of this constraint.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the status of animals as property a natural, self-evident truth (a Mountain), or a social/legal construct that benefits identifiable human groups (a Snare or Tangled Rope)?',
    'Comparative legal and philosophical analysis across cultures and historical periods, examining the contingency and evolution of animal status, and identifying the beneficiaries of its persistence.',
    'If found to be a social construct, the constraint would be reclassified from Mountain to a more extractive type (e.g., Snare or Tangled Rope), and animals would be recognized as victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity regarding the ontological status of animal property rights.').

omega_variable(
    property_vs_welfare_boundary,
    'How does this ''property_reading'' structurally differ from the ''welfare_reading'' of the animal_moral_status kernel?',
    'Analyze the legal and ethical implications: the ''welfare_reading'' would introduce constraints on use based on animal sentience, potentially shifting animals from ''non-victim'' to ''payer'' for certain harms, and introducing ''regulated use'' as a coordination function.',
    'The ''welfare_reading'' would likely compute as a Rope or Tangled Rope, acknowledging animal interests while maintaining use, whereas this ''property_reading'' denies such independent interests.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_vs_welfare_boundary, conceptual, 'Distinguishing property status from welfare considerations.').

omega_variable(
    property_vs_abolition_contradiction,
    'How does this ''property_reading'' fundamentally contradict the ''abolitionist_reading'' of the animal_moral_status kernel?',
    'Examine the core premises: the ''abolitionist_reading'' asserts animals are rights-bearing individuals, directly foreclosing the ''property_reading''s'' premise of animals as chattel. This would place animals in the ''victim'' set and challenge ''property_owners'' as ''agenda_setters''.',
    'The ''abolitionist_reading'' would classify the current system as a Snare, with animals as primary victims, a direct contradiction to the Mountain classification of the ''property_reading''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_vs_abolition_contradiction, conceptual, 'Contradiction between property status and animal rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__property_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1900, animal_moral_status__property_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(anim_tr_t1930, animal_moral_status__property_reading, theater_ratio, 1930, 0.05).
narrative_ontology:measurement(anim_tr_t1960, animal_moral_status__property_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(anim_tr_t1990, animal_moral_status__property_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(anim_tr_t2024, animal_moral_status__property_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(anim_be_t1900, animal_moral_status__property_reading, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(anim_be_t1930, animal_moral_status__property_reading, base_extractiveness, 1930, 0.05).
narrative_ontology:measurement(anim_be_t1960, animal_moral_status__property_reading, base_extractiveness, 1960, 0.05).
narrative_ontology:measurement(anim_be_t1990, animal_moral_status__property_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(anim_be_t2024, animal_moral_status__property_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1900, animal_moral_status__property_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(anim_su_t1930, animal_moral_status__property_reading, suppression_requirement, 1930, 0.1).
narrative_ontology:measurement(anim_su_t1960, animal_moral_status__property_reading, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement(anim_su_t1990, animal_moral_status__property_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(anim_su_t2024, animal_moral_status__property_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
