% ============================================================================
% CONSTRAINT STORY: personhood_boundary__fitness_contingent_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_personhood_boundary__fitness_contingent_reading, []).

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
 *   constraint_id: personhood_boundary__fitness_contingent_reading
 *   human_readable: Personhood Contingent on Demonstrated Fitness
 *   domain: moral_philosophy/historical_ethics/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents a reading of the 'personhood_boundary' kernel
 *   where moral and legal standing are contingent upon an individual
 *   demonstrating certain 'fitness' criteria (e.g., cognitive capacity,
 *   self-awareness). Pre-fitness entities, including severely disabled
 *   infants, are structurally excluded from the moral community. This reading
 *   has historical roots in various philosophical and eugenic movements, and
 *   its persistence, even in contested forms, continues to shape debates
 *   around human rights and bioethics. The constraint is highly extractive,
 *   denying fundamental rights, and highly suppressive, as it actively
 *   excludes and marginalizes those deemed 'unfit'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, 0.9).
domain_priors:suppression_score(personhood_boundary__fitness_contingent_reading, 0.95).
domain_priors:theater_ratio(personhood_boundary__fitness_contingent_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, suppression_requirement, 0.95).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(personhood_boundary__fitness_contingent_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(personhood_boundary__fitness_contingent_reading, snare).
narrative_ontology:human_readable(personhood_boundary__fitness_contingent_reading, "Personhood Contingent on Demonstrated Fitness").
narrative_ontology:topic_domain(personhood_boundary__fitness_contingent_reading, "moral_philosophy/historical_ethics/commitment_systems").

domain_priors:requires_active_enforcement(personhood_boundary__fitness_contingent_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(personhood_boundary__fitness_contingent_reading, '4580c09f-3684-458d-a954-d648b2ad0e8f').
narrative_ontology:cs_kernel_codification('4580c09f-3684-458d-a954-d648b2ad0e8f', formalized).
narrative_ontology:cs_authority_grounding('4580c09f-3684-458d-a954-d648b2ad0e8f', extraction).
narrative_ontology:cs_interpretation_layer_present('4580c09f-3684-458d-a954-d648b2ad0e8f').
narrative_ontology:cs_reading_relation('4580c09f-3684-458d-a954-d648b2ad0e8f', personhood_boundary__birth_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('4580c09f-3684-458d-a954-d648b2ad0e8f', personhood_boundary__potential_based_reading, forecloses).
narrative_ontology:cs_axiom('4580c09f-3684-458d-a954-d648b2ad0e8f', foundational, personhood_requires_demonstrated_capacity).
narrative_ontology:cs_axiom_status(personhood_requires_demonstrated_capacity, holdable).
narrative_ontology:cs_axiom_grounding('4580c09f-3684-458d-a954-d648b2ad0e8f', personhood_requires_demonstrated_capacity, empirically_contingent).
narrative_ontology:cs_axiom('4580c09f-3684-458d-a954-d648b2ad0e8f', secondary, moral_standing_is_earned).
narrative_ontology:cs_axiom_status(moral_standing_is_earned, holdable).
narrative_ontology:cs_axiom_grounding('4580c09f-3684-458d-a954-d648b2ad0e8f', moral_standing_is_earned, deontological).
narrative_ontology:cs_reference_frame('4580c09f-3684-458d-a954-d648b2ad0e8f', capacity_based_moral_hierarchy).
narrative_ontology:cs_drift_state('4580c09f-3684-458d-a954-d648b2ad0e8f', contemporary_human_rights_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('4580c09f-3684-458d-a954-d648b2ad0e8f', '').
narrative_ontology:cs_kernel_id(personhood_boundary__fitness_contingent_reading, personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, state_authority).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, able_bodied_citizens).
narrative_ontology:constraint_beneficiary(personhood_boundary__fitness_contingent_reading, bioethicists_advocating_fitness).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, pre_fitness_entities).
narrative_ontology:constraint_victim(personhood_boundary__fitness_contingent_reading, severely_disabled_infants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defines, codifies, and enforces the criteria for 'fitness' that grant moral and legal personhood. Benefits from the ability to manage populations and resources by excluding certain groups from full moral consideration.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, state_authority, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Individuals or groups who have not yet, or cannot, demonstrate the 'fitness' required for personhood. They are denied fundamental rights, moral standing, and protection, making them vulnerable to exploitation, neglect, or elimination.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, pre_fitness_entities, payer,
    powerless, immediate, trapped, local).

% A specific subset of pre-fitness entities whose lack of 'demonstrated fitness' (e.g., cognitive capacity, self-awareness) is used to deny them moral standing, potentially leading to reduced care, research exploitation, or euthanasia.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, severely_disabled_infants, payer,
    powerless, immediate, trapped, local).

% Benefit from a clear, if exclusionary, definition of who belongs to the moral community. This can reduce perceived social obligations or resource burdens associated with caring for those deemed 'unfit'.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, able_bodied_citizens, beneficiary,
    organized, biographical, mobile, national).

% Acquire professional standing and influence by developing and promoting philosophical frameworks that justify personhood based on demonstrated fitness. Their theories provide the intellectual scaffolding for the constraint.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, bioethicists_advocating_fitness, beneficiary,
    powerful, biographical, analytical, global).

% Actively oppose any definition of personhood contingent on fitness, arguing for universal moral standing based on inherent human dignity. They are structurally excluded from the decision-making processes that define and enforce fitness criteria.
narrative_ontology:constraint_stakeholder(personhood_boundary__fitness_contingent_reading, human_rights_advocates, excluded,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit exclusionary, boundary for moral and legal personhood, defining who is owed rights and who is not, thereby structuring social obligations and resource allocation.
% TRANSFER_FUNCTION: Transfers moral standing, rights, and social obligations from pre-fitness entities to those who meet the fitness criteria, or to the state/society that defines and benefits from such exclusion.
% ABSENT_VOICES: The pre-fitness entities themselves, and their advocates who argue for inherent moral worth regardless of demonstrated capacity. These voices are often marginalized or silenced by the very framework that denies them standing.
% DISAPPEARANCE_RATIONALE: If personhood were universally granted regardless of fitness, the legal, ethical, and social frameworks for care, resource allocation, and human rights would undergo a fundamental and immediate re-evaluation, leading to a radical restructuring of societal norms and institutions.
% FOUNDING_PROBLEM: Historically, this constraint was built to justify social hierarchies, manage scarce resources by excluding certain populations, or to rationalize eugenic practices by defining 'unfit' individuals as non-persons.
% FOUNDING_PROBLEM_CORROBORATION: Proponents might cite historical philosophical traditions or perceived social utility. However, human rights organizations and mainstream bioethics largely reject such justifications, corroborating the 'contested' status from an external, critical perspective, highlighting the historical abuses enabled by such definitions.
narrative_ontology:disappearance_verdict(personhood_boundary__fitness_contingent_reading, world_rearranges).
narrative_ontology:founding_problem_status(personhood_boundary__fitness_contingent_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(personhood_boundary__fitness_contingent_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(personhood_boundary__fitness_contingent_reading, 'none', 1).
narrative_ontology:epsilon_provenance(personhood_boundary__fitness_contingent_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(personhood_boundary__fitness_contingent_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(personhood_boundary__fitness_contingent_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(personhood_boundary__fitness_contingent_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is very high (0.9) because this constraint denies fundamental moral and legal standing to entire categories of beings, leading to severe consequences like neglect, exploitation, or even elimination. Suppression is also very high (0.95) as it requires active enforcement of fitness criteria and the marginalization of dissenting voices. Theater ratio is low (0.1) because the exclusion is direct and functional, not primarily performative, though rhetorical justifications may exist. Accessibility collapse is high (0.9) for victims, as there are no alternatives to being 'fit' within this framework. Resistance is moderate (0.4), reflecting historical and ongoing opposition from human rights and disability advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of those who define and benefit from 'fitness-contingent personhood', this constraint provides necessary clarity and order for society. From the perspective of the excluded and their advocates, it is a deeply unjust and violent act of dehumanization. The engine's classification as a Snare reflects the latter, highlighting the coercive and extractive nature of this boundary definition.
 *
 * DIRECTIONALITY LOGIC:
 *   The state authority and bioethicists advocating fitness are clear beneficiaries, gaining power, influence, and the ability to define social order. Able-bodied citizens may also benefit from a perceived reduction in social obligations. Pre-fitness entities and severely disabled infants are the primary targets/victims, bearing the full cost of exclusion. Human rights advocates are excluded, as their universalist perspective directly challenges the constraint's foundational premise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately identified as a specific reading of the ''personhood_boundary'' kernel?',
    'Comparative analysis with other readings and historical philosophical texts to confirm distinct structural premises.',
    'If misidentified, the analysis of inter-reading relations and axiom contradictions would be flawed, leading to incorrect kernel-level classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint''s identity as a reading within the personhood kernel.').

omega_variable(
    victim_set_contingency,
    'Does the victim set (infants) genuinely enter only after failing a fitness test, or are they excluded a priori?',
    'Examination of specific legal or philosophical texts that codify the fitness criteria and the timing of their application.',
    'If exclusion is a priori, the ''fitness test'' is merely a post-hoc rationalization, increasing the constraint''s effective suppression and extractiveness by removing any pretense of conditional inclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_contingency, empirical, 'Clarifies the precise mechanism and timing of victim exclusion based on fitness.').

omega_variable(
    state_exclusion_authority,
    'Is the state''s authority to exclude pre-test entities from the moral community universally accepted within this reading, or is it a point of internal contention?',
    'Analysis of internal debates within philosophical traditions that support fitness-contingent personhood.',
    'If internally contested, the constraint''s stability is lower, and its persistence relies more heavily on active suppression of internal dissent, potentially increasing its theater ratio.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(state_exclusion_authority, conceptual, 'Examines the internal coherence and acceptance of the state''s exclusionary power.').

omega_variable(
    fitness_criteria_objectivity,
    'Are the ''fitness'' criteria objective and universal, or are they culturally and socially constructed and therefore arbitrary?',
    'Cross-cultural and historical analysis of ''fitness'' definitions; philosophical critique of the possibility of objective moral criteria.',
    'If arbitrary, the constraint''s legitimacy is undermined, increasing its effective extractiveness and suppression as it relies on power rather than objective truth. This would also increase resistance from excluded groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fitness_criteria_objectivity, conceptual, 'Assesses the objectivity and universality of the fitness criteria.').

omega_variable(
    historical_justification_validity,
    'Is the historical justification for fitness-contingent personhood (e.g., resource management, social order) still ethically defensible in contemporary society?',
    'Ethical review against modern human rights standards and principles of universal dignity.',
    'If indefensible, the constraint''s founding problem is ''dead'', and its persistence is a clear case of mandatrophy, reclassifying it as a Piton or a Snare with a strong Mandatrophy signal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_justification_validity, preference, 'Evaluates the contemporary ethical validity of the constraint''s historical justifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(personhood_boundary__fitness_contingent_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pers_tr_t1900, personhood_boundary__fitness_contingent_reading, theater_ratio, 1900, 0.1).
narrative_ontology:measurement(pers_tr_t1930, personhood_boundary__fitness_contingent_reading, theater_ratio, 1930, 0.08).
narrative_ontology:measurement(pers_tr_t1960, personhood_boundary__fitness_contingent_reading, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(pers_tr_t1990, personhood_boundary__fitness_contingent_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(pers_tr_t2020, personhood_boundary__fitness_contingent_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(pers_be_t1900, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1900, 0.9).
narrative_ontology:measurement(pers_be_t1930, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1930, 0.92).
narrative_ontology:measurement(pers_be_t1960, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1960, 0.88).
narrative_ontology:measurement(pers_be_t1990, personhood_boundary__fitness_contingent_reading, base_extractiveness, 1990, 0.85).
narrative_ontology:measurement(pers_be_t2020, personhood_boundary__fitness_contingent_reading, base_extractiveness, 2020, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(pers_su_t1900, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1900, 0.95).
narrative_ontology:measurement(pers_su_t1930, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1930, 0.97).
narrative_ontology:measurement(pers_su_t1960, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1960, 0.9).
narrative_ontology:measurement(pers_su_t1990, personhood_boundary__fitness_contingent_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(pers_su_t2020, personhood_boundary__fitness_contingent_reading, suppression_requirement, 2020, 0.95).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(personhood_boundary__fitness_contingent_reading, identity_coordination).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, eugenics_policies).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, resource_allocation_for_disabled).
narrative_ontology:affects_constraint(personhood_boundary__fitness_contingent_reading, medical_ethics_research_protocols).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'personhood_boundary' kernel, focusing on fitness as a prerequisite for moral standing. It is structurally distinct from readings based on birth or potential.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
