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
 *   human_readable: Animal Property Status (Abolitionist Reading)
 *   domain: applied_ethics/animal_studies/legal_philosophy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'abolitionist reading' of the
 *   'animal_moral_status' kernel. From this perspective, the legal and
 *   customary property status of animals is itself the fundamental violation,
 *   rendering all forms of animal use, however 'humane,' as perpetuations of
 *   victimization. The constraint is framed as a snare because the
 *   'coordination' narrative of regulated, humane use is seen as a cover for
 *   the underlying, deeply extractive system of animal commodification. The
 *   metrics reflect this high extraction and suppression, with minimal
 *   theatricality as the system is actively maintained.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_moral_status__abolitionist_reading, 0.9).
domain_priors:suppression_score(animal_moral_status__abolitionist_reading, 0.95).
domain_priors:theater_ratio(animal_moral_status__abolitionist_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(animal_moral_status__abolitionist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_moral_status__abolitionist_reading, snare).
narrative_ontology:human_readable(animal_moral_status__abolitionist_reading, "Animal Property Status (Abolitionist Reading)").
narrative_ontology:topic_domain(animal_moral_status__abolitionist_reading, "applied_ethics/animal_studies/legal_philosophy").

domain_priors:requires_active_enforcement(animal_moral_status__abolitionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_moral_status__abolitionist_reading, '38829b50-a91c-4f4c-9a5a-363c6af0faa2').
narrative_ontology:cs_kernel_codification('38829b50-a91c-4f4c-9a5a-363c6af0faa2', formalized).
narrative_ontology:cs_authority_grounding('38829b50-a91c-4f4c-9a5a-363c6af0faa2', lineage).
narrative_ontology:cs_interpretation_layer_present('38829b50-a91c-4f4c-9a5a-363c6af0faa2').
narrative_ontology:cs_reading_relation('38829b50-a91c-4f4c-9a5a-363c6af0faa2', animal_moral_status__property_reading, forecloses).
narrative_ontology:cs_reading_relation('38829b50-a91c-4f4c-9a5a-363c6af0faa2', animal_moral_status__welfare_reading, forecloses).
narrative_ontology:cs_axiom('38829b50-a91c-4f4c-9a5a-363c6af0faa2', foundational, animal_sentience_entails_rights).
narrative_ontology:cs_axiom_status(animal_sentience_entails_rights, holdable).
narrative_ontology:cs_axiom_grounding('38829b50-a91c-4f4c-9a5a-363c6af0faa2', animal_sentience_entails_rights, deontological).
narrative_ontology:cs_axiom('38829b50-a91c-4f4c-9a5a-363c6af0faa2', foundational, property_status_is_inherent_violation).
narrative_ontology:cs_axiom_status(property_status_is_inherent_violation, holdable).
narrative_ontology:cs_axiom_grounding('38829b50-a91c-4f4c-9a5a-363c6af0faa2', property_status_is_inherent_violation, deontological).
narrative_ontology:cs_reference_frame('38829b50-a91c-4f4c-9a5a-363c6af0faa2', animal_as_property_framework).
narrative_ontology:cs_drift_state('38829b50-a91c-4f4c-9a5a-363c6af0faa2', contemporary_animal_rights_movement, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('38829b50-a91c-4f4c-9a5a-363c6af0faa2', '').
narrative_ontology:cs_kernel_id(animal_moral_status__abolitionist_reading, animal_moral_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(animal_moral_status__abolitionist_reading, all_animals_under_human_dominion).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_moral_status__abolitionist_reading, biomedical_research_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Legally classified as property, animals are subjected to human use, exploitation, and confinement, experiencing suffering and death for human benefit. Their agency is systematically denied, and their interests are subordinated by law and custom.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, all_animals_under_human_dominion, payer,
    powerless, immediate, trapped, universal).

% Actively challenge the property status of animals, advocating for their rights and the cessation of all animal use. They face significant institutional and cultural resistance but work to shift legal and ethical paradigms.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, human_abolitionist_advocates, agenda_setter,
    organized, generational, constrained, global).

% Benefits directly and massively from the property status of animals, which allows for their breeding, confinement, and slaughter for food and other products. This industry actively defends the current legal framework.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, animal_agriculture_industry, beneficiary,
    institutional, biographical, arbitrage, global).

% Relies on the property status of animals for their use in experiments and testing. They benefit from the legal and ethical framework that permits animal experimentation, often citing scientific progress as justification.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, biomedical_research_institutions, beneficiary,
    institutional, biographical, constrained, global).

% Codify and enforce the property status of animals, providing the legal framework that underpins their use across various sectors. They are the ultimate arbiters of animal legal standing.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, legal_systems, agenda_setter,
    institutional, civilizational, analytical, national).

% Advocate for improved conditions and reduced suffering for animals within existing systems of use. From the abolitionist perspective, their efforts, while well-intentioned, implicitly legitimize animal property status and thus perpetuate victimization, making them 'excluded' from the abolitionist project.
narrative_ontology:constraint_stakeholder(animal_moral_status__abolitionist_reading, welfarist_organizations, excluded,
    organized, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint coordinates human society's instrumental use of animals, ensuring a stable supply of animal products and services by legally defining animals as property and managing their exploitation.
% TRANSFER_FUNCTION: Transfers the lives, bodies, labor, and reproductive capacities of animals to human beings for their consumption, research, entertainment, and other benefits.
% ABSENT_VOICES: Animals themselves are the primary absent voices, unable to articulate their interests or consent to their treatment. Their suffering is systematically silenced or reframed as a necessary cost.
% DISAPPEARANCE_RATIONALE: If the property status of animals vanished overnight, human society would undergo a profound and immediate reorganization. Industries reliant on animal exploitation (agriculture, research, entertainment) would collapse or be forced to fundamentally transform, and legal systems would need to redefine human-animal relations entirely.
% FOUNDING_PROBLEM: The historical problem of human survival, convenience, and economic development, which was 'solved' by the instrumentalization and commodification of animals.
% FOUNDING_PROBLEM_CORROBORATION: The historical and ongoing economic structures built on animal use corroborate the 'founding problem' from the perspective of benefiting industries. However, philosophical arguments and social movements from outside these benefiting parties contest the legitimacy and necessity of this 'solution' in contemporary society, arguing it creates more problems than it solves.
narrative_ontology:disappearance_verdict(animal_moral_status__abolitionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_moral_status__abolitionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_moral_status__abolitionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(animal_moral_status__abolitionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_moral_status__abolitionist_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is very high (0.9) because the constraint permits the systematic exploitation and killing of sentient beings for human benefit, which the abolitionist reading views as total extraction. Suppression is also very high (0.95) due to the legal classification of animals as property, which denies them agency, legal standing, and any meaningful exit options. The system requires active enforcement to maintain this property status against both animal resistance and human advocacy. Theater ratio is low (0.1) because the constraint's function is genuinely to enable and protect animal use, not merely to perform it; 'humane' regulations are seen as minor adjustments within an extractive framework, not as a primary function.
 *
 * PERSPECTIVAL GAP:
 *   The abolitionist reading fundamentally diverges from both the 'property reading' (which sees animals as resources) and the 'welfare reading' (which seeks to minimize suffering within use). From the perspective of benefiting industries, the constraint is a legitimate framework for resource management. From the abolitionist perspective, it is a system of profound injustice and extraction. The engine's classification of 'snare' from this reading's metrics highlights this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   All animals under human dominion are the full targets (d=1.0) of this constraint, bearing its full costs without benefit. Industries and institutions that profit from animal use (e.g., animal agriculture, biomedical research) are the primary beneficiaries (d=0.0). Legal systems act as agenda-setters, enforcing the property status. Human abolitionist advocates, while working to dismantle the constraint, are also targets in that they operate within a system that legally permits what they oppose. Welfarist organizations are 'excluded' from the abolitionist project because their focus on 'humane' use is seen as implicitly validating the property status itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a fundamental feature of human-animal relations (property_reading), a call for regulated use (welfare_reading), or a demand for abolition of property status (abolitionist_reading)?',
    'Philosophical and legal consensus on animal moral status, or a societal shift in human-animal relations.',
    'Reclassification of the fundamental nature of human-animal interaction, altering the victim set and the legitimacy of ''use''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is one reading of the ''animal_moral_status'' kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of animal agency primarily structural (legal property status, physical confinement) or internalized (learned helplessness, domestication)?',
    'Behavioral studies of formerly captive animals, or legal changes that remove property status and observe long-term behavioral shifts.',
    'If internalized, the effective suppression is higher and more persistent, requiring deeper intervention than legal changes alone.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for animals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_moral_status__abolitionist_reading, 1970, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_moral_status__abolitionist_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(anim_tr_t1980, animal_moral_status__abolitionist_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(anim_tr_t1990, animal_moral_status__abolitionist_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(anim_tr_t2000, animal_moral_status__abolitionist_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(anim_tr_t2010, animal_moral_status__abolitionist_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(anim_tr_t2025, animal_moral_status__abolitionist_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_moral_status__abolitionist_reading, base_extractiveness, 1970, 0.85).
narrative_ontology:measurement(anim_be_t1980, animal_moral_status__abolitionist_reading, base_extractiveness, 1980, 0.87).
narrative_ontology:measurement(anim_be_t1990, animal_moral_status__abolitionist_reading, base_extractiveness, 1990, 0.88).
narrative_ontology:measurement(anim_be_t2000, animal_moral_status__abolitionist_reading, base_extractiveness, 2000, 0.89).
narrative_ontology:measurement(anim_be_t2010, animal_moral_status__abolitionist_reading, base_extractiveness, 2010, 0.9).
narrative_ontology:measurement(anim_be_t2025, animal_moral_status__abolitionist_reading, base_extractiveness, 2025, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_moral_status__abolitionist_reading, suppression_requirement, 1970, 0.9).
narrative_ontology:measurement(anim_su_t1980, animal_moral_status__abolitionist_reading, suppression_requirement, 1980, 0.92).
narrative_ontology:measurement(anim_su_t1990, animal_moral_status__abolitionist_reading, suppression_requirement, 1990, 0.93).
narrative_ontology:measurement(anim_su_t2000, animal_moral_status__abolitionist_reading, suppression_requirement, 2000, 0.94).
narrative_ontology:measurement(anim_su_t2010, animal_moral_status__abolitionist_reading, suppression_requirement, 2010, 0.95).
narrative_ontology:measurement(anim_su_t2025, animal_moral_status__abolitionist_reading, suppression_requirement, 2025, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
