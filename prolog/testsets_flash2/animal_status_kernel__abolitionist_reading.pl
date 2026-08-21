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
 *   human_readable: Abolitionist Reading: Animals as Moral Persons, Not Property
 *   domain: moral_philosophy/animal_ethics/legal_theory
 *
 * SUMMARY:
 *   This constraint represents the abolitionist reading of animal status:
 *   animals are moral persons with a fundamental right not to be property,
 *   and their property status itself is the injustice. All use is
 *   categorically impermissible, regardless of welfare conditions. This
 *   reading views the current legal and social arrangement as a Snare,
 *   extracting total value from animals by denying their personhood. The
 *   metrics reflect this perspective, showing extremely high extractiveness
 *   and suppression, with minimal theater, as the system is brutally
 *   efficient in its function. This is one reading of the
 *   'animal_status_kernel', distinct from the 'property_reading' and
 *   'welfare_reading'.
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
narrative_ontology:constraint_claim(animal_status_kernel__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_status_kernel__abolitionist_reading, "Abolitionist Reading: Animals as Moral Persons, Not Property").
narrative_ontology:topic_domain(animal_status_kernel__abolitionist_reading, "moral_philosophy/animal_ethics/legal_theory").

domain_priors:requires_active_enforcement(animal_status_kernel__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status_kernel__abolitionist_reading, 'f4d33381-229a-42af-835d-a2a1abea501c').
narrative_ontology:cs_kernel_codification('f4d33381-229a-42af-835d-a2a1abea501c', formalized).
narrative_ontology:cs_authority_grounding('f4d33381-229a-42af-835d-a2a1abea501c', lineage).
narrative_ontology:cs_interpretation_layer_present('f4d33381-229a-42af-835d-a2a1abea501c').
narrative_ontology:cs_reading_relation('f4d33381-229a-42af-835d-a2a1abea501c', animal_status_kernel__property_reading, forecloses).
narrative_ontology:cs_reading_relation('f4d33381-229a-42af-835d-a2a1abea501c', animal_status_kernel__welfare_reading, coexists_with).
narrative_ontology:cs_axiom('f4d33381-229a-42af-835d-a2a1abea501c', foundational, animals_are_moral_persons).
narrative_ontology:cs_axiom_status(animals_are_moral_persons, holdable).
narrative_ontology:cs_axiom_grounding('f4d33381-229a-42af-835d-a2a1abea501c', animals_are_moral_persons, deontological).
narrative_ontology:cs_axiom('f4d33381-229a-42af-835d-a2a1abea501c', foundational, property_status_is_injustice).
narrative_ontology:cs_axiom_status(property_status_is_injustice, holdable).
narrative_ontology:cs_axiom_grounding('f4d33381-229a-42af-835d-a2a1abea501c', property_status_is_injustice, deontological).
narrative_ontology:cs_reference_frame('f4d33381-229a-42af-835d-a2a1abea501c', universal_moral_personhood).
narrative_ontology:cs_drift_state('f4d33381-229a-42af-835d-a2a1abea501c', contemporary_legal_systems, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('f4d33381-229a-42af-835d-a2a1abea501c', '').
narrative_ontology:cs_kernel_id(animal_status_kernel__abolitionist_reading, animal_status_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status_kernel__abolitionist_reading, human_users_of_animals).
narrative_ontology:constraint_victim(animal_status_kernel__abolitionist_reading, animals_as_property).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Animals are legally defined as property, subject to human ownership and use. This status denies them fundamental rights and makes them the primary targets of extraction, suffering all costs without recourse or exit.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, animals_as_property, payer,
    powerless, immediate, trapped, universal).

% Individuals and industries (agriculture, research, entertainment) that benefit from the property status of animals. They set the terms of use and actively resist any challenge to this status, as it underpins their economic and cultural practices.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, human_users_of_animals, agenda_setter,
    institutional, generational, constrained, global).

% Advocates who argue for the complete abolition of animal property status and use. They analyze the constraint as a fundamental injustice and work to dismantle it, often facing significant resistance from established industries and legal systems.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, abolitionist_advocates, observer,
    moderate, generational, analytical, global).

% Advocates who seek to improve animal welfare within the existing property framework. From an abolitionist perspective, their efforts are seen as implicitly legitimizing property status and delaying fundamental change, thus they are excluded from the core abolitionist conversation.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, welfare_reformers, excluded,
    organized, biographical, constrained, national).

% The legal frameworks that codify and enforce the property status of animals. They are the primary mechanism by which the constraint is maintained, defining rights and obligations based on this foundational status.
narrative_ontology:constraint_stakeholder(animal_status_kernel__abolitionist_reading, legal_systems, agenda_setter,
    institutional, civilizational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the abolitionist perspective, there is no genuine coordination function; the constraint primarily coordinates the exploitation of animals for human benefit by defining them as property, facilitating their commodification and use across various industries.
% TRANSFER_FUNCTION: Transfers all rights, autonomy, and bodily integrity from animals to human users, enabling the extraction of labor, products, and services from animals without their consent or consideration of their interests.
% ABSENT_VOICES: Animals themselves are the primary absent voices, unable to articulate their interests or resist their property status. Their interests are represented by abolitionist advocates, but their direct voice is structurally excluded from the legal and moral frameworks that define their existence.
% DISAPPEARANCE_RATIONALE: If the property status of animals vanished overnight, the global economy, legal systems, and human-animal relationships would undergo a radical, immediate, and profound rearrangement. Industries reliant on animal use would collapse or transform, legal codes would require complete overhaul, and the moral landscape would shift fundamentally.
% FOUNDING_PROBLEM: The historical problem solved was the efficient and unchallenged utilization of animals as resources for human needs and desires, establishing a clear hierarchy of moral and legal status.
% FOUNDING_PROBLEM_CORROBORATION: From the abolitionist perspective, the 'problem' of animal use for human benefit is still 'live' because the injustice of property status persists. This is corroborated by the ongoing suffering of animals and the continued resistance from abolitionist movements, who attest that the foundational problem of speciesism remains unresolved, despite claims from human users that the arrangement is 'natural' or 'necessary'.
narrative_ontology:disappearance_verdict(animal_status_kernel__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status_kernel__abolitionist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status_kernel__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is maximal (0.95) because the property status of animals allows for total extraction of their labor, bodies, and lives, denying them any intrinsic value or rights. Suppression is also maximal (0.98) because animals are legally trapped within this status, with no means of exit or resistance, and legal systems actively enforce their commodification. Theater ratio is minimal (0.05) because, from this perspective, any 'welfare' measures are seen as superficial attempts to legitimize an inherently unjust system, rather than genuine functional improvements. The constraint is stable over time because the fundamental property status has not changed.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading fundamentally diverges from both property and welfare readings. While the property reading sees no extraction and the welfare reading sees some extraction mitigated by regulation, the abolitionist reading sees total extraction and fundamental injustice. The engine's classification will highlight this divergence by computing a Snare from this reading, contrasting with potentially Rope or Tangled Rope classifications from other readings of the same kernel.
 *
 * DIRECTIONALITY LOGIC:
 *   Animals, as property, are the ultimate targets (d=1.0, maximal extraction). Human users and legal systems are the primary beneficiaries and agenda-setters (d=0.0-0.1, minimal extraction/maximal subsidy). Abolitionist advocates are observers, seeking to dismantle the constraint. Welfare reformers are structurally excluded from the core abolitionist project, as their incremental approach is seen as reinforcing the property paradigm.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_status_naturalness,
    'Is the property status of animals a natural, inevitable arrangement, or a human-constructed legal and moral framework?',
    'Historical and anthropological analysis of human-animal relationships across cultures and time, examining the emergence and evolution of property concepts applied to living beings.',
    'If constructed, it strengthens the case for abolition as a matter of justice, rather than a challenge to natural order. If ''natural'', it would imply a Mountain-like resistance to change, though still extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(property_status_naturalness, conceptual, 'Whether animal property status is natural or constructed.').

omega_variable(
    welfare_reform_impact,
    'Do welfare reforms (e.g., improved living conditions, reduced suffering) advance or delay the ultimate goal of animal abolition?',
    'Empirical study of social movements and legal changes: do incremental welfare gains lead to greater public acceptance of animal rights, or do they pacify concerns and entrench the property system?',
    'If welfare reforms delay abolition, the strategic tension with welfare reformers is justified, and their efforts are seen as counterproductive to the abolitionist goal. If they advance it, the abolitionist strategy might need re-evaluation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_reform_impact, empirical, 'Strategic impact of welfare reforms on abolition.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (legal barriers, economic dependency) or internalized (cognitive patterns that persist after barrier removal)?',
    'Post-exit suppression trajectory: if suppression persists after the extractive mechanism is removed (e.g., animals rescued from farms still exhibit learned helplessness), reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status_kernel__abolitionist_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status_kernel__abolitionist_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(anim_tr_t10, animal_status_kernel__abolitionist_reading, theater_ratio, 10, 0.05).
narrative_ontology:measurement(anim_tr_t20, animal_status_kernel__abolitionist_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(anim_tr_t30, animal_status_kernel__abolitionist_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(anim_tr_t40, animal_status_kernel__abolitionist_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(anim_tr_t50, animal_status_kernel__abolitionist_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status_kernel__abolitionist_reading, base_extractiveness, 0, 0.95).
narrative_ontology:measurement(anim_be_t10, animal_status_kernel__abolitionist_reading, base_extractiveness, 10, 0.95).
narrative_ontology:measurement(anim_be_t20, animal_status_kernel__abolitionist_reading, base_extractiveness, 20, 0.95).
narrative_ontology:measurement(anim_be_t30, animal_status_kernel__abolitionist_reading, base_extractiveness, 30, 0.95).
narrative_ontology:measurement(anim_be_t40, animal_status_kernel__abolitionist_reading, base_extractiveness, 40, 0.95).
narrative_ontology:measurement(anim_be_t50, animal_status_kernel__abolitionist_reading, base_extractiveness, 50, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status_kernel__abolitionist_reading, suppression_requirement, 0, 0.98).
narrative_ontology:measurement(anim_su_t10, animal_status_kernel__abolitionist_reading, suppression_requirement, 10, 0.98).
narrative_ontology:measurement(anim_su_t20, animal_status_kernel__abolitionist_reading, suppression_requirement, 20, 0.98).
narrative_ontology:measurement(anim_su_t30, animal_status_kernel__abolitionist_reading, suppression_requirement, 30, 0.98).
narrative_ontology:measurement(anim_su_t40, animal_status_kernel__abolitionist_reading, suppression_requirement, 40, 0.98).
narrative_ontology:measurement(anim_su_t50, animal_status_kernel__abolitionist_reading, suppression_requirement, 50, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status_kernel__abolitionist_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
