% ============================================================================
% CONSTRAINT STORY: animal_status__property_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__property_reading, []).

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
 *   constraint_id: animal_status__property_reading
 *   human_readable: Animal Status as Property (Property Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint defines animals as legal objects without independent
 *   moral standing, allowing human ownership largely unrestricted except by
 *   specific welfare statutes. This story instantiates the 'property_reading'
 *   of the broader 'animal_status' kernel. From this reading's perspective,
 *   the constraint functions as a foundational legal 'rope' that coordinates
 *   human society's use of animals by establishing clear property rights,
 *   with minimal extraction from human owners. The low extractiveness (ε
 *   ~0.05) reflects that the constraint primarily benefits human owners by
 *   granting them rights, rather than extracting from them.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__property_reading, 0.05).
domain_priors:suppression_score(animal_status__property_reading, 0.1).
domain_priors:theater_ratio(animal_status__property_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__property_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(animal_status__property_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(animal_status__property_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__property_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(animal_status__property_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__property_reading, rope).
narrative_ontology:human_readable(animal_status__property_reading, "Animal Status as Property (Property Reading)").
narrative_ontology:topic_domain(animal_status__property_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__property_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__property_reading, 'c4f5bae2-d696-4524-9ef6-de9d3070a871').
narrative_ontology:cs_kernel_codification('c4f5bae2-d696-4524-9ef6-de9d3070a871', formalized).
narrative_ontology:cs_authority_grounding('c4f5bae2-d696-4524-9ef6-de9d3070a871', lineage).
narrative_ontology:cs_interpretation_layer_present('c4f5bae2-d696-4524-9ef6-de9d3070a871').
narrative_ontology:cs_reading_relation('c4f5bae2-d696-4524-9ef6-de9d3070a871', animal_status__welfare_reading, coexists_with).
narrative_ontology:cs_reading_relation('c4f5bae2-d696-4524-9ef6-de9d3070a871', animal_status__abolitionist_reading, forecloses).
narrative_ontology:cs_axiom('c4f5bae2-d696-4524-9ef6-de9d3070a871', foundational, animals_are_chattel).
narrative_ontology:cs_axiom_status(animals_are_chattel, holdable).
narrative_ontology:cs_axiom_grounding('c4f5bae2-d696-4524-9ef6-de9d3070a871', animals_are_chattel, conventional).
narrative_ontology:cs_axiom('c4f5bae2-d696-4524-9ef6-de9d3070a871', foundational, moral_standing_derives_from_human_recognition).
narrative_ontology:cs_axiom_status(moral_standing_derives_from_human_recognition, holdable).
narrative_ontology:cs_axiom_grounding('c4f5bae2-d696-4524-9ef6-de9d3070a871', moral_standing_derives_from_human_recognition, conventional).
narrative_ontology:cs_reference_frame('c4f5bae2-d696-4524-9ef6-de9d3070a871', roman_law_property_tradition).
narrative_ontology:cs_drift_state('c4f5bae2-d696-4524-9ef6-de9d3070a871', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c4f5bae2-d696-4524-9ef6-de9d3070a871', '').
narrative_ontology:cs_kernel_id(animal_status__property_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__property_reading, human_owners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(animal_status__property_reading, animal_welfare_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals, corporations, and institutions that own animals for various purposes (companionship, food production, research, entertainment). They benefit from clear, legally protected rights to use and dispose of animals as property, with minimal external restrictions.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, human_owners, agenda_setter,
    powerful, biographical, arbitrage, local).

% The framework of laws, courts, and enforcement bodies that define and uphold animal property status. It provides the legal infrastructure for ownership, transfer, and dispute resolution concerning animals, ensuring stability for human owners.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, legal_system, agenda_setter,
    institutional, generational, analytical, national).

% Groups and individuals who seek to improve animal treatment through welfare statutes. From the property reading's perspective, they bear the cost of animals' lack of independent moral standing, as their efforts are limited to imposing human-defined restrictions on property use, rather than challenging the fundamental property status itself.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, animal_welfare_advocates, payer,
    organized, generational, constrained, national).

% Judicial bodies that interpret and apply laws related to animal ownership and welfare. They operate within the established legal framework that defines animals as property, adjudicating disputes and enforcing statutes based on this foundational premise.
narrative_ontology:constraint_stakeholder(animal_status__property_reading, courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes clear, transferable ownership of animals, coordinating human interactions around their use in agriculture, research, companionship, and other sectors, thereby minimizing disputes among human parties.
% TRANSFER_FUNCTION: Legally transfers control, use rights, and economic value associated with animals from a diffuse 'natural' state to human owners, enabling their integration into human economic and social systems.
% ABSENT_VOICES: Animals themselves are structurally excluded from having a voice or independent standing within this legal framework. Abolitionist advocates, who would fundamentally challenge the property status, are also effectively excluded from the core legal discourse that defines this constraint.
% DISAPPEARANCE_RATIONALE: If the legal status of animals as property vanished overnight, the entire legal, economic, and social system built on animal use (agriculture, pet ownership, research, entertainment) would collapse. Ownership claims would be nullified, commercial transactions involving animals would cease, and the legal framework for human-animal interactions would require a complete overhaul.
% FOUNDING_PROBLEM: To establish clear, enforceable rights of ownership and control over animals for human benefit (e.g., food, labor, companionship, economic value) and to minimize conflicts among humans over these resources.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars specializing in property law, agricultural industry bodies, and pet owner associations consistently attest that the need for clear property rights over animals remains a live and essential problem for human society. This perspective is widely reflected in legal education and economic policy discussions.
narrative_ontology:disappearance_verdict(animal_status__property_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__property_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__property_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(animal_status__property_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__property_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__property_reading_tests).
:- end_tests(animal_status__property_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a 'rope' because it provides a clear, stable legal framework for human interaction with animals, primarily benefiting human owners by defining animals as property. Extractiveness is low (0.05) because the constraint's primary function, from this reading's perspective, is to grant rights to owners, not to extract from them. Suppression (0.1) is minimal, reflecting the general acceptance of this legal status within the framework, with enforcement focused on upholding property rights. Theater ratio is low (0.05) as the legal status is a core, functional aspect of property law. Accessibility collapse is high (0.9) because the legal system offers virtually no alternatives to this fundamental property status for animals. Resistance (0.1) is low from the perspective of this reading, as it represents the established legal norm.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human owners and the legal system, this constraint is a stable, beneficial coordination mechanism. However, from the perspective of animal welfare advocates, it represents a fundamental injustice that limits their ability to protect animals. The engine's classification will reflect the 'rope' nature from the owner's seat, while other readings (e.g., abolitionist) would classify it as a 'snare' or 'tangled_rope' due to the extraction from animals.
 *
 * DIRECTIONALITY LOGIC:
 *   Human owners are the primary beneficiaries and agenda-setters, as the constraint grants them extensive rights over animals. The legal system also benefits by having a clear, stable framework for adjudication. Animal welfare advocates are positioned as payers, as they bear the cost of animals lacking independent legal standing and must work within the property framework to achieve their goals. Animals themselves are not recognized as agents or victims within this reading's framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint accurately identified as the ''property_reading'' of the ''animal_status'' kernel?',
    'Analysis of legal texts and philosophical arguments to confirm that the core premise of animals as property, without independent moral standing, is consistently maintained and distinct from welfare-based or rights-based approaches.',
    'If misidentified, the classification of this constraint and its relationship to sibling readings would be inaccurate, leading to incorrect analysis of the broader kernel''s dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Confirms the specific reading being instantiated from the ''animal_status'' kernel.').

omega_variable(
    structural_impact_of_abolitionist_reading,
    'What would be the full structural impact on this ''property_reading'' if the ''abolitionist_reading'' were to gain legal supremacy?',
    'Legal and economic modeling of a transition to a rights-based animal legal system, analyzing the dissolution of property rights over animals and the redefinition of human-animal interactions.',
    'The ''abolitionist_reading'' would fundamentally foreclose the ''property_reading'', rendering its core axioms overridden and leading to a complete reclassification of the legal status of animals and associated constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_impact_of_abolitionist_reading, empirical, 'Examines the potential for the abolitionist perspective to dismantle the property status.').

omega_variable(
    coexistence_with_welfare_reading,
    'To what extent does the ''property_reading'' genuinely coexist with the ''welfare_reading'' without internal contradiction, or does the ''welfare_reading'' subtly erode the property premise?',
    'Detailed legal analysis of case law and legislative history where welfare statutes have been applied, examining whether judicial interpretations consistently uphold the property status or introduce elements that implicitly challenge it.',
    'If the ''welfare_reading'' is found to subtly erode the property premise, the ''coexists_with'' relation might need re-evaluation, potentially indicating a slow ''influences'' or even ''forecloses'' dynamic over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coexistence_with_welfare_reading, conceptual, 'Assesses the true nature of coexistence between property and welfare readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__property_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1900, animal_status__property_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(anim_tr_t1930, animal_status__property_reading, theater_ratio, 1930, 0.05).
narrative_ontology:measurement(anim_tr_t1960, animal_status__property_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(anim_tr_t1990, animal_status__property_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(anim_tr_t2024, animal_status__property_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(anim_be_t1900, animal_status__property_reading, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(anim_be_t1930, animal_status__property_reading, base_extractiveness, 1930, 0.05).
narrative_ontology:measurement(anim_be_t1960, animal_status__property_reading, base_extractiveness, 1960, 0.05).
narrative_ontology:measurement(anim_be_t1990, animal_status__property_reading, base_extractiveness, 1990, 0.05).
narrative_ontology:measurement(anim_be_t2024, animal_status__property_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1900, animal_status__property_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(anim_su_t1930, animal_status__property_reading, suppression_requirement, 1930, 0.1).
narrative_ontology:measurement(anim_su_t1960, animal_status__property_reading, suppression_requirement, 1960, 0.1).
narrative_ontology:measurement(anim_su_t1990, animal_status__property_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(anim_su_t2024, animal_status__property_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__property_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
