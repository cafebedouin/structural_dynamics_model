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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: animal_status__welfare_reading
 *   human_readable: Animal Welfare as Constraint on Human Use
 *   domain: applied_ethics/legal_philosophy/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'welfare' reading of animal status, where
 *   animals are recognized as sentient beings with interests that constrain,
 *   but do not prohibit, human instrumental use. It is a tangled rope because
 *   it genuinely coordinates human behavior to prevent gratuitous harm while
 *   simultaneously extracting value from animals through their
 *   instrumentalization, with enforcement required to maintain the balance.
 *   The extractiveness has decreased over time due to increased public
 *   awareness and advocacy, but remains substantial due to the fundamental
 *   permission of use.
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
narrative_ontology:human_readable(animal_status__welfare_reading, "Animal Welfare as Constraint on Human Use").
narrative_ontology:topic_domain(animal_status__welfare_reading, "applied_ethics/legal_philosophy/political_economy").

domain_priors:requires_active_enforcement(animal_status__welfare_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(animal_status__welfare_reading, '45d7f63e-9ba2-462f-a450-8ccc9f81ed5d').
narrative_ontology:cs_kernel_codification('45d7f63e-9ba2-462f-a450-8ccc9f81ed5d', formalized).
narrative_ontology:cs_authority_grounding('45d7f63e-9ba2-462f-a450-8ccc9f81ed5d', practice).
narrative_ontology:cs_interpretation_layer_present('45d7f63e-9ba2-462f-a450-8ccc9f81ed5d').
narrative_ontology:cs_reading_relation('45d7f63e-9ba2-462f-a450-8ccc9f81ed5d', animal_status__abolitionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('45d7f63e-9ba2-462f-a450-8ccc9f81ed5d', animal_status__property_reading, coexists_with).
narrative_ontology:cs_axiom('45d7f63e-9ba2-462f-a450-8ccc9f81ed5d', foundational, animal_sentience_implies_moral_consideration).
narrative_ontology:cs_axiom_status(animal_sentience_implies_moral_consideration, holdable).
narrative_ontology:cs_axiom_grounding('45d7f63e-9ba2-462f-a450-8ccc9f81ed5d', animal_sentience_implies_moral_consideration, deontological).
narrative_ontology:cs_axiom('45d7f63e-9ba2-462f-a450-8ccc9f81ed5d', foundational, human_instrumental_use_is_permissible_with_constraints).
narrative_ontology:cs_axiom_status(human_instrumental_use_is_permissible_with_constraints, holdable).
narrative_ontology:cs_axiom_grounding('45d7f63e-9ba2-462f-a450-8ccc9f81ed5d', human_instrumental_use_is_permissible_with_constraints, conventional).
narrative_ontology:cs_reference_frame('45d7f63e-9ba2-462f-a450-8ccc9f81ed5d', utilitarian_welfare_maximization).
narrative_ontology:cs_drift_state('45d7f63e-9ba2-462f-a450-8ccc9f81ed5d', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('45d7f63e-9ba2-462f-a450-8ccc9f81ed5d', '').
narrative_ontology:cs_kernel_id(animal_status__welfare_reading, animal_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, animal_agriculture_industry).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, biomedical_researchers).
narrative_ontology:constraint_beneficiary(animal_status__welfare_reading, pet_owners).
narrative_ontology:constraint_victim(animal_status__welfare_reading, farmed_animals).
narrative_ontology:constraint_victim(animal_status__welfare_reading, research_animals).
narrative_ontology:constraint_victim(animal_status__welfare_reading, wild_animals_in_human_contact).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operates within welfare regulations, often influencing their scope and enforcement. Benefits from the continued legality of animal use while managing public perception and regulatory compliance costs. Actively lobbies against stricter regulations.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_agriculture_industry, agenda_setter,
    institutional, generational, constrained, global).

% Relies on animal models for scientific advancement, operating under ethical review boards and welfare guidelines. Benefits from the constraint allowing instrumental use while imposing compliance costs.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, biomedical_researchers, beneficiary,
    organized, biographical, constrained, national).

% Benefits from the legal framework allowing animal ownership, with minimal direct costs from welfare regulations beyond basic care. Generally supports welfare standards for companion animals.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, pet_owners, beneficiary,
    moderate, biographical, mobile, local).

% Experience the direct impact of human use, with welfare regulations providing some, often minimal, protections against extreme suffering. Their interests are represented by advocates, not directly by themselves.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, farmed_animals, payer,
    powerless, immediate, trapped, local).

% Subjected to experimental procedures, with welfare regulations aiming to minimize pain and distress. Their existence is entirely instrumental to human goals within this framework.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, research_animals, payer,
    powerless, immediate, trapped, local).

% Work to strengthen welfare regulations and improve enforcement, often through public campaigns, lobbying, and legal challenges. They represent the interests of animals within the existing framework of human use.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_welfare_advocates, agenda_setter,
    organized, generational, constrained, global).

% Argue for the inherent rights of animals, challenging the fundamental premise of instrumental use. Their arguments are largely outside the practical policy debates shaped by the welfare reading, which assumes human use is permissible.
narrative_ontology:constraint_stakeholder(animal_status__welfare_reading, animal_rights_philosophers, excluded,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates human activities involving animals by establishing a baseline of permissible treatment, balancing human interests in use with animal interests in avoiding suffering, thereby preventing arbitrary cruelty and ensuring a degree of public acceptance for animal-related industries.
% TRANSFER_FUNCTION: Transfers the cost of welfare compliance (e.g., better housing, veterinary care) from animals (who would otherwise suffer more) to human users, while transferring the right to instrumental use from animals to humans, subject to these constraints.
% ABSENT_VOICES: Animals themselves, who cannot articulate their interests directly. Animal rights philosophers and abolitionist advocates are largely excluded from the practical policy-making that defines the 'welfare' boundary, as their core premise (no instrumental use) is foreclosed by this reading.
% DISAPPEARANCE_RATIONALE: If the welfare constraint vanished, it would lead to widespread, unchecked cruelty, public outcry, and a collapse of legitimacy for industries relying on animal use. The legal and ethical landscape would be forced to re-evaluate the status of animals, likely leading to a more extreme 'property' or 'abolitionist' default.
% FOUNDING_PROBLEM: The problem of gratuitous cruelty to animals, driven by a recognition of animal sentience and the moral intuition that suffering should be avoided where possible, even if instrumental use is permitted.
% FOUNDING_PROBLEM_CORROBORATION: The problem of animal suffering remains live, attested by animal welfare organizations, veterinary professionals, and public sentiment. The ongoing need for enforcement and the continuous debate over welfare standards corroborate that the problem is not fully 'solved' but actively managed within this framework.
narrative_ontology:disappearance_verdict(animal_status__welfare_reading, world_rearranges).
narrative_ontology:founding_problem_status(animal_status__welfare_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(animal_status__welfare_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(animal_status__welfare_reading, 'none', 1).

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
 *   The extractiveness (0.45) reflects the inherent cost to animals of being used instrumentally, even with welfare protections. Suppression (0.6) is necessary to enforce welfare standards against industries that would otherwise cut costs, and to suppress challenges to the fundamental premise of animal use. Theater ratio (0.2) is relatively low, as welfare regulations do provide real, albeit limited, protections, but some compliance is performative to satisfy public opinion without significant operational change. Accessibility collapse (0.3) is low because alternatives to animal products/research exist, but are not widely adopted due to cost or cultural inertia. Resistance (0.7) is high, driven by animal welfare advocates pushing for stronger protections and animal rights groups challenging the entire framework.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human users, this constraint is a necessary coordination mechanism that allows for ethical animal use. From the perspective of animals and their most radical advocates, it is a snare that legitimizes exploitation under the guise of protection. The engine's classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   The animal agriculture industry and biomedical researchers are beneficiaries, as the constraint permits their operations while imposing manageable costs. Pet owners also benefit from legal ownership. Farmed and research animals are clear victims, bearing the direct costs of instrumentalization. Animal welfare advocates act as agenda-setters, shaping the terms of the constraint. Animal rights philosophers are excluded, as their core premise (no instrumental use) is outside the scope of this 'welfare' reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    welfare_vs_abolition_boundary,
    'Is the ''welfare'' reading a stable ethical position, or an unstable compromise that will inevitably drift towards either ''property'' or ''abolition''?',
    'Longitudinal study of legal and philosophical trends in animal status over the next century: does the ''constrain but not prohibit'' line hold, or does it erode?',
    'If unstable, the constraint''s long-term classification is either a snare (drifting to property) or a scaffold (transitional to abolition). If stable, its tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(welfare_vs_abolition_boundary, conceptual, 'Stability of the welfare position between property and abolition.').

omega_variable(
    enforcement_effectiveness_gap,
    'What is the actual gap between stated welfare regulations and their on-the-ground enforcement and compliance?',
    'Independent, unannounced audits of animal facilities (farms, labs) with public reporting, compared to self-reported compliance data.',
    'A large gap would increase the effective extractiveness and theater_ratio, potentially reclassifying the constraint closer to a snare, as the coordination function becomes more performative.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(enforcement_effectiveness_gap, empirical, 'Discrepancy between animal welfare law and practice.').

omega_variable(
    natural_vs_constructed_interests,
    'Are the ''interests'' attributed to animals (e.g., to avoid pain) genuinely natural and universal, or are they socially constructed within the human ethical framework?',
    'Philosophical analysis of the grounding of animal ethics, cross-cultural comparison of animal treatment norms, and neuroscientific research on animal consciousness.',
    'If primarily constructed, the constraint''s ''naturalness'' claim is weakened, increasing its perceived extractiveness from an analytical seat. If natural, it grounds the coordination function more firmly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_vs_constructed_interests, conceptual, 'The ontological status of animal interests.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(animal_status__welfare_reading, 1970, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anim_tr_t1970, animal_status__welfare_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(anim_tr_t1980, animal_status__welfare_reading, theater_ratio, 1980, 0.12).
narrative_ontology:measurement(anim_tr_t1990, animal_status__welfare_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(anim_tr_t2000, animal_status__welfare_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(anim_tr_t2010, animal_status__welfare_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(anim_tr_t2024, animal_status__welfare_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(anim_be_t1970, animal_status__welfare_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(anim_be_t1980, animal_status__welfare_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(anim_be_t1990, animal_status__welfare_reading, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(anim_be_t2000, animal_status__welfare_reading, base_extractiveness, 2000, 0.48).
narrative_ontology:measurement(anim_be_t2010, animal_status__welfare_reading, base_extractiveness, 2010, 0.46).
narrative_ontology:measurement(anim_be_t2024, animal_status__welfare_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(anim_su_t1970, animal_status__welfare_reading, suppression_requirement, 1970, 0.4).
narrative_ontology:measurement(anim_su_t1980, animal_status__welfare_reading, suppression_requirement, 1980, 0.45).
narrative_ontology:measurement(anim_su_t1990, animal_status__welfare_reading, suppression_requirement, 1990, 0.5).
narrative_ontology:measurement(anim_su_t2000, animal_status__welfare_reading, suppression_requirement, 2000, 0.55).
narrative_ontology:measurement(anim_su_t2010, animal_status__welfare_reading, suppression_requirement, 2010, 0.58).
narrative_ontology:measurement(anim_su_t2024, animal_status__welfare_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(animal_status__welfare_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(animal_status__welfare_reading, 0.08).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__abolitionist_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, animal_status__property_reading).
narrative_ontology:affects_constraint(animal_status__welfare_reading, food_production_ethics).
narrative_ontology:affects_constraint(animal_status__welfare_reading, biomedical_ethics).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'animal_status' kernel. The 'welfare_reading' (this constraint) acknowledges animal sentience and interests, constraining but not prohibiting human use. The 'abolitionist_reading' asserts inherent animal rights precluding all instrumental use. The 'property_reading' treats animals as legal objects with minimal moral standing. Each reading has distinct ε values and structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
