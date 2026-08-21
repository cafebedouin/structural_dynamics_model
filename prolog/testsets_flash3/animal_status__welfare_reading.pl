% ============================================================================
% CONSTRAINT STORY: animal_status__welfare_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_animal_status__welfare_reading, []).

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
 *   constraint_id: animal_status__welfare_reading
 *   human_readable: Animal Welfare Constraint (Welfare Reading)
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'welfare reading' of animal status, where
 *   animals are recognized as sentient beings with interests that constrain,
 *   but do not prohibit, human use. It aims to prevent gratuitous harm while
 *   permitting instrumental use under welfare protections. The constraint is
 *   classified as a Tangled Rope due to its genuine coordination function
 *   (balancing human and animal interests) coupled with asymmetric extraction
 *   (animals bear the costs of use, humans benefit) and active enforcement of
 *   welfare standards.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(animal_status__welfare_reading, 0.45).
domain_priors:suppression_score(animal_status__welfare_reading, 0.6).
domain_priors:theater_ratio(animal_status__welfare_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(animal_status__welfare_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(animal_status__welfare_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(animal_status__welfare_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(animal_status__welfare_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(animal_status__welfare_reading, tangled_rope).
narrative_ontology:human_readable(animal_status__welfare_reading, "Animal Welfare Constraint (Welfare Reading)").
narrative_ontology:topic_domain(animal_status__welfare_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, '6c592bac-cf9d-4313-962a-36c157e7bcd4').
narrative_ontology:cs_kernel_codification('6c592bac-cf9d-4313-962a-36c157e7bcd4', formalized).
narrative_ontology:cs_authority_grounding('6c592bac-cf9d-4313-962a-36c157e7bcd4', practice).
narrative_ontology:cs_interpretation_layer_present('6c592bac-cf9d-4313-962a-36c157e7bcd4').
narrative_ontology:cs_reading_relation('6c592bac-cf9d-4313-962a-36c157e7bcd4', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('6c592bac-cf9d-4313-962a-36c157e7bcd4', animal_status__property_reading, coexists_with).
narrative_ontology:cs_axiom('6c592bac-cf9d-4313-962a-36c157e7bcd4', foundational, animal_sentience_entails_moral_consideration).
narrative_ontology:cs_axiom_status(animal_sentience_entails_moral_consideration, holdable).
narrative_ontology:cs_axiom_grounding('6c592bac-cf9d-4313-962a-36c157e7bcd4', animal_sentience_entails_moral_consideration, deontological).
narrative_ontology:cs_axiom('6c592bac-cf9d-4313-962a-36c157e7bcd4', foundational, human_instrumental_use_is_permissible_with_welfare_protections).
narrative_ontology:cs_axiom_status(human_instrumental_use_is_permissible_with_welfare_protections, holdable).
narrative_ontology:cs_axiom_grounding('6c592bac-cf9d-4313-962a-36c157e7bcd4', human_instrumental_use_is_permissible_with_welfare_protections, conventional).
narrative_ontology:cs_reference_frame('6c592bac-cf9d-4313-962a-36c157e7bcd4', balanced_human_animal_interests).
narrative_ontology:cs_drift_state('6c592bac-cf9d-4313-962a-36c157e7bcd4', contemporary_ethical_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('6c592bac-cf9d-4313-962a-36c157e7bcd4', '').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, biomedical_researchers).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, pet_owners).
narrative_ontology:constraint_victim(animal_status__welfare_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status__welfare_reading, laboratory_animals).
narrative_ontology:constraint_victim(animal_status__welfare_reading, companion_animals_subject_to_abuse).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the legal and ethical framework that permits instrumental use of animals for food production, while bearing costs of welfare regulations. Actively lobbies to shape welfare standards to minimize economic impact.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_agriculture_industry, beneficiary,
    institutional, generational, constrained, global).

% Relies on the framework permitting animal experimentation for scientific advancement, subject to ethical review boards and welfare protocols. Benefits from the ability to use animals as models for human disease.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, biomedical_researchers, beneficiary,
    organized, biographical, constrained, national).

% Benefits from the legal status of animals as property that can be owned, while being constrained by anti-cruelty laws. Experiences the constraint as a balance between ownership rights and responsibility for animal well-being.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, pet_owners, beneficiary,
    moderate, biographical, mobile, local).

% Bear the direct costs of instrumental use, including confinement, pain, and premature death, mitigated by welfare standards. Their interests are recognized but subordinated to human interests.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, farmed_animals, payer,
    powerless, immediate, trapped, global).

% Subject to experimental procedures, often involving pain and distress, under protocols designed to minimize suffering. Their existence is entirely instrumental to human research goals.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, laboratory_animals, payer,
    powerless, immediate, trapped, national).

% Experience direct harm from human neglect or cruelty, despite legal protections. Their welfare depends on the enforcement of anti-cruelty laws and the ethical choices of individual owners.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, companion_animals_subject_to_abuse, payer,
    powerless, immediate, trapped, local).

% Work within the existing framework to improve welfare standards and enforce anti-cruelty laws. They are both beneficiaries (of legal recognition of animal interests) and payers (of the effort to push for change within a system that permits use).
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_welfare_advocates, agenda_setter,
    organized, generational, constrained, national).

% Advocate for the complete cessation of animal use, viewing the welfare framework as legitimizing exploitation. They are excluded from the core decision-making processes of the welfare framework itself, as their goals fundamentally challenge its premise.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, abolitionist_activists, excluded,
    moderate, generational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human interactions with animals by establishing a baseline of acceptable treatment and defining limits on gratuitous harm, allowing for instrumental use while acknowledging animal sentience.
% TRANSFER_FUNCTION: Transfers the right to use animals for human benefit (food, research, companionship) from animals to humans, while transferring a moral obligation to prevent unnecessary suffering from humans to animals.
% ABSENT_VOICES: Abolitionist activists are largely excluded from the policy-making bodies that define welfare standards, as their fundamental challenge to animal use is seen as outside the scope of 'welfare' discussions. The animals themselves, of course, have no voice.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, the legal and ethical landscape around animal use would collapse. Industries relying on animals would face immediate moral and legal challenges, and the concept of 'humane' treatment would lose its grounding, leading to either unrestricted exploitation or a rapid shift towards abolitionist frameworks.
% FOUNDING_PROBLEM: To reconcile human desires for animal products and services with a growing moral recognition of animal sentience and capacity for suffering, preventing extreme cruelty while permitting use.
% FOUNDING_PROBLEM_CORROBORATION: Animal welfare organizations, ethicists, and a significant portion of the public corroborate that the problem of balancing human interests with animal suffering remains live. The animal agriculture and research industries also attest to the ongoing need for a framework that permits their activities while addressing ethical concerns.
narrative_ontology:disappearance_verdict(animal_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__welfare_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__welfare_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_status__welfare_reading, 'none', 1).
narrative_ontology:epsilon_provenance(animal_status__welfare_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(animal_status__welfare_reading_tests).
:- end_tests(animal_status__welfare_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because while animals are used instrumentally, their suffering is intended to be minimized, and gratuitous harm is prohibited. Suppression (0.6) is significant because the framework actively suppresses alternatives to instrumental use (e.g., full animal liberation) and enforces compliance with welfare standards. Theater ratio (0.2) is low, indicating that while there's some performative aspect to welfare claims, genuine efforts are made to improve animal conditions within the use framework. Resistance (0.7) is high due to ongoing advocacy from animal welfare and abolitionist groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human beneficiaries, this constraint is a necessary and ethical balance. From the perspective of animals, or abolitionist activists, it is a system of regulated exploitation. The engine's classification will reflect this divergence, showing a more extractive classification for the payer seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Industries and individuals who use animals (animal agriculture, biomedical researchers, pet owners) are beneficiaries, as the constraint legitimizes their activities while imposing manageable costs. Animals themselves (farmed, laboratory, abused companions) are the primary payers, bearing the costs of instrumental use. Animal welfare advocates act as agenda-setters, working to improve conditions within the existing framework. Abolitionist activists are excluded, as their position fundamentally challenges the constraint's premise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_sentience,
    'Which animals are covered by the ''sentient beings with interests'' clause, and how is sentience empirically determined?',
    'Ongoing scientific research into animal cognition and neurobiology, coupled with legal and ethical consensus-building on the definition and implications of sentience.',
    'Expanding the scope of recognized sentience would increase the victim set and potentially raise the measured extractiveness and suppression, pushing the constraint towards a Snare for newly included species. Narrowing it would have the opposite effect.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_sentience, empirical, 'Ambiguity in the empirical and conceptual boundaries of sentience.').

omega_variable(
    welfare_vs_rights_framing,
    'Is the ''welfare'' framework inherently capable of protecting animal interests, or does it merely legitimize exploitation by setting a floor for acceptable harm?',
    'Longitudinal studies of animal welfare outcomes under current regulations, compared with outcomes under alternative (e.g., rights-based) frameworks, or philosophical analysis of the coherence of ''humane exploitation''.',
    'If welfare is found to be inherently insufficient, the constraint''s extractiveness would be re-evaluated upward, and its coordination function questioned, potentially reclassifying it closer to a Snare. If effective, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_vs_rights_framing, conceptual, 'The fundamental ethical efficacy of the welfare framework itself.').

omega_variable(
    enforcement_effectiveness_gap,
    'What is the actual gap between declared welfare standards and their real-world enforcement and compliance?',
    'Independent audits of animal facilities, whistleblower reports, and empirical studies of animal health and behavior in regulated environments, compared against official compliance data.',
    'A significant gap would increase the effective extractiveness and theater ratio, as the declared coordination function (welfare protection) would be largely performative, pushing the constraint towards a Piton or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_effectiveness_gap, empirical, 'Discrepancy between stated welfare goals and actual outcomes.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t0, animal_status__welfare_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(anim_tr_t10, animal_status__welfare_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(anim_tr_t20, animal_status__welfare_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(anim_tr_t30, animal_status__welfare_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(anim_tr_t40, animal_status__welfare_reading, theater_ratio, 40, 0.21).
narrative_ontology:measurement(anim_tr_t50, animal_status__welfare_reading, theater_ratio, 50, 0.2).

% Extraction over time
narrative_ontology:measurement(anim_be_t0, animal_status__welfare_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(anim_be_t10, animal_status__welfare_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(anim_be_t20, animal_status__welfare_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement(anim_be_t30, animal_status__welfare_reading, base_extractiveness, 30, 0.47).
narrative_ontology:measurement(anim_be_t40, animal_status__welfare_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement(anim_be_t50, animal_status__welfare_reading, base_extractiveness, 50, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t0, animal_status__welfare_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(anim_su_t10, animal_status__welfare_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(anim_su_t20, animal_status__welfare_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(anim_su_t30, animal_status__welfare_reading, suppression_requirement, 30, 0.62).
narrative_ontology:measurement(anim_su_t40, animal_status__welfare_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement(anim_su_t50, animal_status__welfare_reading, suppression_requirement, 50, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, identity_coordination).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__property_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'animal_status' kernel. It coexists with the abolitionist and property readings, each representing a distinct structural claim about animal moral status and human obligations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
