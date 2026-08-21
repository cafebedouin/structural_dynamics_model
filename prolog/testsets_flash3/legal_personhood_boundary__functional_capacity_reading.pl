% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__functional_capacity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__functional_capacity_reading, []).

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
 *   constraint_id: legal_personhood_boundary__functional_capacity_reading
 *   human_readable: Legal Personhood Boundary (Functional Capacity Reading)
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'functional capacity' reading of
 *   the legal personhood boundary kernel. It posits that personhood, and thus
 *   the entitlement to rights, should be determined by demonstrable cognitive
 *   capacities such as rationality, sentience, and self-awareness,
 *   irrespective of species. This reading directly challenges anthropocentric
 *   legal frameworks that limit personhood to humans, and developmental
 *   frameworks that grant personhood based on potentiality rather than
 *   present capacity. The current system, from this reading's perspective, is
 *   highly extractive from sentient non-human beings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.85).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.9).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, snare).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Legal Personhood Boundary (Functional Capacity Reading)").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, '1f97a9e9-ab5a-46d2-8cfb-378e9bd120cc').
narrative_ontology:cs_kernel_codification('1f97a9e9-ab5a-46d2-8cfb-378e9bd120cc', formalized).
narrative_ontology:cs_authority_grounding('1f97a9e9-ab5a-46d2-8cfb-378e9bd120cc', lineage).
narrative_ontology:cs_interpretation_layer_present('1f97a9e9-ab5a-46d2-8cfb-378e9bd120cc').
narrative_ontology:cs_reading_relation('1f97a9e9-ab5a-46d2-8cfb-378e9bd120cc', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('1f97a9e9-ab5a-46d2-8cfb-378e9bd120cc', legal_personhood_boundary__developmental_potentiality_reading, coexists_with).
narrative_ontology:cs_axiom('1f97a9e9-ab5a-46d2-8cfb-378e9bd120cc', foundational, personhood_requires_demonstrable_cognitive_capacity).
narrative_ontology:cs_axiom_status(personhood_requires_demonstrable_cognitive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('1f97a9e9-ab5a-46d2-8cfb-378e9bd120cc', personhood_requires_demonstrable_cognitive_capacity, empirically_contingent).
narrative_ontology:cs_axiom('1f97a9e9-ab5a-46d2-8cfb-378e9bd120cc', foundational, species_is_not_a_morally_relevant_criterion).
narrative_ontology:cs_axiom_status(species_is_not_a_morally_relevant_criterion, holdable).
narrative_ontology:cs_axiom_grounding('1f97a9e9-ab5a-46d2-8cfb-378e9bd120cc', species_is_not_a_morally_relevant_criterion, deontological).
narrative_ontology:cs_reference_frame('1f97a9e9-ab5a-46d2-8cfb-378e9bd120cc', enlightenment_rationality_principle).
narrative_ontology:cs_drift_state('1f97a9e9-ab5a-46d2-8cfb-378e9bd120cc', contemporary_neuroscience_and_ai_advances, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('1f97a9e9-ab5a-46d2-8cfb-378e9bd120cc', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, human_property_owners).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, industrial_animal_agriculture).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, research_institutions).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, sentient_non_human_animals).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, future_advanced_ais).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the current legal framework that classifies non-human sentient beings as property, allowing their use and exploitation without legal recourse. This reading challenges that status, potentially impacting economic interests.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, human_property_owners, beneficiary,
    powerful, generational, arbitrage, global).

% Relies on the non-personhood status of animals for its operational model. A shift to functional capacity personhood would necessitate radical restructuring or abolition, incurring prohibitive costs.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, industrial_animal_agriculture, beneficiary,
    institutional, biographical, constrained, global).

% Benefits from the ability to conduct research on sentient non-human animals without the full ethical and legal constraints applied to human subjects. This reading would significantly alter research protocols and permissible studies.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, research_institutions, beneficiary,
    institutional, biographical, constrained, global).

% Currently classified as property, they bear the full cost of exploitation, suffering, and lack of legal protection. This reading would grant them legal standing and rights based on their cognitive capacities.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, sentient_non_human_animals, payer,
    powerless, immediate, trapped, global).

% As potential entities with demonstrable cognitive capacities, they would be denied personhood under current anthropocentric frameworks. This reading anticipates their emergence and advocates for their inclusion as rights-bearers.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, future_advanced_ais, payer,
    powerless, civilizational, trapped, universal).

% Actively campaign for the expansion of personhood to non-human animals based on their cognitive abilities. They seek to change legal frameworks and societal norms.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, animal_rights_advocates, agenda_setter,
    organized, generational, constrained, global).

% Analyze and debate the theoretical underpinnings and practical implications of different personhood criteria, including functional capacity. Their work informs legal and ethical discourse.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, legal_scholars_and_philosophers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading seeks to coordinate legal and ethical frameworks with scientific understanding of cognitive capacity, ensuring that rights are consistently applied based on demonstrable attributes rather than arbitrary species boundaries.
% TRANSFER_FUNCTION: It would transfer legal rights, protections, and moral consideration from an exclusive human domain to any entity demonstrating relevant cognitive capacities. Conversely, it would transfer property rights over such entities from human owners to the entities themselves, or to legal guardians.
% ABSENT_VOICES: The direct voices of sentient non-human animals and future advanced AIs are absent from current legal and philosophical discourse, as they lack the means to articulate their interests within human systems. Their interests are represented by advocates.
% DISAPPEARANCE_RATIONALE: If this reading were universally adopted and enforced overnight, the legal status of countless beings would change, fundamentally altering industries (e.g., agriculture, research), property law, and ethical considerations. The world would undergo a profound legal and moral reorganization.
% FOUNDING_PROBLEM: The problem this reading addresses is the arbitrary and inconsistent application of personhood and rights based on species, rather than on demonstrable and morally relevant capacities like sentience and self-awareness, leading to the exploitation of cognitively capable non-human beings.
% FOUNDING_PROBLEM_CORROBORATION: Ethical philosophers, cognitive scientists, and animal welfare organizations corroborate the problem, citing scientific evidence of animal sentience and the moral inconsistencies of current legal frameworks. This corroboration comes from outside the direct beneficiaries of the current anthropocentric system.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__functional_capacity_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__functional_capacity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legal_personhood_boundary__functional_capacity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__functional_capacity_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__functional_capacity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__functional_capacity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.85) because the current legal system, which this reading contests, permits extensive exploitation of sentient non-human animals, denying them fundamental rights despite their demonstrable capacities. Suppression is also very high (0.90) as legal and social structures actively prevent the recognition of non-human personhood and suppress any attempts to grant them rights. The theater ratio is low (0.10) because the system's function is genuinely to maintain the current anthropocentric boundary, not to performatively support a different one. Resistance is high (0.75) due to ongoing advocacy from animal rights movements and philosophical challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human property owners and industrial agriculture, the current anthropocentric personhood boundary is a natural and necessary 'mountain' for economic and social order. From the perspective of sentient non-human animals (represented by advocates), the same boundary is a 'snare' of immense extraction and suppression. This reading aligns with the latter, highlighting the structural asymmetry.
 *
 * DIRECTIONALITY LOGIC:
 *   Human property owners, industrial animal agriculture, and research institutions are beneficiaries, as the current system allows them to treat sentient non-humans as property. Sentient non-human animals and future advanced AIs are the primary victims, bearing the full cost of non-personhood. Animal rights advocates act as agenda-setters, pushing for the redefinition of this boundary. The high extractiveness and suppression are directed towards the non-person entities.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate, from an anthropocentric view, is to maintain human exceptionalism and property rights over animals. From this reading's perspective, this mandate has outlived its ethical justification given scientific understanding of sentience. The classification as a snare prevents mislabeling the current system as a 'natural' or 'coordinative' arrangement, instead highlighting its extractive nature from those denied personhood.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_of_cognitive_capacity,
    'How reliably and universally can ''demonstrable cognitive capacity'' (rationality, sentience, self-awareness) be measured across diverse species and potential future intelligences (e.g., AI)?',
    'Development of standardized, cross-species cognitive assessment protocols and consensus among neuroscientists and AI ethicists on thresholds for personhood-relevant capacities.',
    'If measurement is robust, the functional capacity reading gains empirical grounding, strengthening its claim for legal reform. If measurement remains ambiguous or species-specific, the practical implementation of this reading becomes challenging, potentially leading to new forms of arbitrary exclusion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_of_cognitive_capacity, empirical, 'Uncertainty regarding the objective and universal measurement of cognitive capacities for personhood.').

omega_variable(
    scope_of_sentience_definition,
    'What is the precise definition and scope of ''sentience'' for legal personhood? Does it include only pain/pleasure, or also complex emotional states, social cognition, and subjective experience?',
    'Philosophical consensus on a minimal definition of sentience sufficient for moral consideration, informed by comparative neuroscience and ethology.',
    'A narrow definition might exclude many animals currently subject to exploitation, limiting the impact of this reading. A broad definition could extend personhood to a wider range of beings, including invertebrates, leading to more radical legal and ethical shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_sentience_definition, conceptual, 'Ambiguity in the definition and scope of sentience as a criterion for personhood.').

omega_variable(
    balancing_rights_of_different_persons,
    'If personhood is extended to non-human animals and AIs, how would their rights be balanced against human rights, especially in cases of conflict (e.g., resource allocation, self-defense, environmental impact)?',
    'Development of a comprehensive legal framework for inter-species and inter-intelligence rights, potentially involving a hierarchy of rights or a system of legal guardianship and advocacy.',
    'Failure to establish clear balancing mechanisms could lead to legal chaos or a de facto re-subordination of non-human persons. Successful frameworks would solidify the practical viability of this reading.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(balancing_rights_of_different_persons, preference, 'Uncertainty about the practical implementation and balancing of rights for diverse persons.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 1970, 2050).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t1970, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(lega_tr_t1990, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 1990, 0.08).
narrative_ontology:measurement(lega_tr_t2010, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(lega_tr_t2030, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2030, 0.1).
narrative_ontology:measurement(lega_tr_t2050, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2050, 0.1).

% Extraction over time
narrative_ontology:measurement(lega_be_t1970, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 1970, 0.95).
narrative_ontology:measurement(lega_be_t1990, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 1990, 0.9).
narrative_ontology:measurement(lega_be_t2010, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2010, 0.88).
narrative_ontology:measurement(lega_be_t2030, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2030, 0.86).
narrative_ontology:measurement(lega_be_t2050, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2050, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t1970, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 1970, 0.98).
narrative_ontology:measurement(lega_su_t1990, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 1990, 0.95).
narrative_ontology:measurement(lega_su_t2010, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2010, 0.93).
narrative_ontology:measurement(lega_su_t2030, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2030, 0.91).
narrative_ontology:measurement(lega_su_t2050, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2050, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__functional_capacity_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, animal_welfare_regulations).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, property_law_frameworks).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, ai_ethics_governance).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'legal_personhood_boundary' kernel. It focuses on functional capacity, while 'restrictive_anthropocentric_reading' limits personhood to born humans with cognitive capacity, and 'developmental_potentiality_reading' extends it from conception for any human life trajectory holder. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
