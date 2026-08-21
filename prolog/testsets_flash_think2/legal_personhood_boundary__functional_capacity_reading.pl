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
 *   human_readable: Legal Personhood Based on Functional Capacity
 *   domain: legal_philosophy/rights_theory/constitutional_law
 *
 * SUMMARY:
 *   This constraint story instantiates the 'functional capacity' reading of
 *   the legal personhood boundary kernel. It posits that legal personhood
 *   should be granted based on demonstrable cognitive capacities (e.g.,
 *   sentience, rationality, self-awareness) regardless of species. This
 *   reading directly challenges the prevailing anthropocentric legal
 *   frameworks, which restrict personhood primarily to humans. The metrics
 *   reflect the high extractiveness and suppression inherent in the *current*
 *   anthropocentric system, as viewed through the lens of this reading, which
 *   seeks to dismantle that system.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.85).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.9).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Legal Personhood Based on Functional Capacity").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal_philosophy/rights_theory/constitutional_law").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, '43c851fe-ea82-4857-a5aa-2cfb98e521e2').
narrative_ontology:cs_kernel_codification('43c851fe-ea82-4857-a5aa-2cfb98e521e2', formalized).
narrative_ontology:cs_authority_grounding('43c851fe-ea82-4857-a5aa-2cfb98e521e2', expertise).
narrative_ontology:cs_interpretation_layer_present('43c851fe-ea82-4857-a5aa-2cfb98e521e2').
narrative_ontology:cs_reading_relation('43c851fe-ea82-4857-a5aa-2cfb98e521e2', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('43c851fe-ea82-4857-a5aa-2cfb98e521e2', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_axiom('43c851fe-ea82-4857-a5aa-2cfb98e521e2', foundational, personhood_requires_demonstrable_cognitive_capacity).
narrative_ontology:cs_axiom_status(personhood_requires_demonstrable_cognitive_capacity, holdable).
narrative_ontology:cs_axiom_grounding('43c851fe-ea82-4857-a5aa-2cfb98e521e2', personhood_requires_demonstrable_cognitive_capacity, empirically_contingent).
narrative_ontology:cs_axiom('43c851fe-ea82-4857-a5aa-2cfb98e521e2', foundational, species_is_not_a_morally_relevant_criterion_for_personhood).
narrative_ontology:cs_axiom_status(species_is_not_a_morally_relevant_criterion_for_personhood, holdable).
narrative_ontology:cs_axiom_grounding('43c851fe-ea82-4857-a5aa-2cfb98e521e2', species_is_not_a_morally_relevant_criterion_for_personhood, deontological).
narrative_ontology:cs_reference_frame('43c851fe-ea82-4857-a5aa-2cfb98e521e2', universal_capacity_based_rights).
narrative_ontology:cs_drift_state('43c851fe-ea82-4857-a5aa-2cfb98e521e2', contemporary_legal_discourse, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('43c851fe-ea82-4857-a5aa-2cfb98e521e2', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, sentient_non_human_animals).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, future_advanced_ai).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, ecosystem_advocates).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, human_exceptionalists).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, animal_agriculture_industry).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, biomedical_research_industry).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, property_owners_of_sentient_beings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Philosophers, legal scholars, and activists who champion the principle that personhood should be determined by demonstrable cognitive capacities, not species. They actively work to change legal frameworks and public opinion.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, functional_capacity_advocates, agenda_setter,
    powerful, generational, analytical, global).

% Currently treated as property, these beings would gain legal rights and protections if personhood were extended based on their demonstrable sentience, self-awareness, or rationality. They are unable to advocate for themselves directly.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, sentient_non_human_animals, beneficiary,
    powerless, immediate, trapped, universal).

% Hypothetical future artificial intelligences that achieve demonstrable cognitive capacities (e.g., strong AI, AGI) would be recognized as legal persons under this framework, rather than being treated as mere tools or property.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, future_advanced_ai, beneficiary,
    powerless, civilizational, trapped, global).

% Individuals and institutions who uphold the belief that personhood and associated rights are exclusive to humans, often based on species membership or a unique human essence. They would lose legal privilege and face challenges to their worldview.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, human_exceptionalists, payer,
    institutional, generational, constrained, global).

% This industry relies on the legal status of animals as property. Granting personhood to sentient animals would fundamentally disrupt their business model, imposing massive regulatory burdens, ethical constraints, or outright prohibitions on current practices.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, animal_agriculture_industry, payer,
    institutional, biographical, constrained, global).

% Similar to agriculture, this industry depends on the ability to use animals in research. Personhood for sentient animals would severely restrict or prohibit many forms of animal experimentation, requiring a radical shift in research methodologies.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, biomedical_research_industry, payer,
    institutional, biographical, constrained, global).

% Individuals or entities who own animals (e.g., pets, working animals) that would be reclassified as persons. They would lose property rights over these beings and incur new legal responsibilities, potentially facing significant personal and economic adjustments.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, property_owners_of_sentient_beings, payer,
    moderate, immediate, constrained, local).

% Academics who analyze and debate the theoretical underpinnings and practical implications of different personhood criteria. They contribute to the discourse but do not directly enforce or benefit from the constraint's operation.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, legal_scholars_and_philosophers, observer,
    analytical, generational, analytical, global).

% The ultimate arbiters of legal personhood in many jurisdictions. While currently upholding anthropocentric views, they are the institutional actors who would be tasked with interpreting and enforcing a shift to functional capacity-based personhood if such a principle gained legal traction.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a consistent, non-arbitrary, and ethically defensible basis for legal personhood and rights that transcends species boundaries, aligning legal status with demonstrable cognitive capacities.
% TRANSFER_FUNCTION: Transfers legal rights, protections, and moral consideration from an exclusive human domain to a broader class of sentient, rational, or self-aware beings. It transfers costs (e.g., loss of property rights, increased regulatory burden, ethical re-evaluation) to those currently benefiting from the exclusion of these beings from personhood.
% ABSENT_VOICES: The non-human sentient beings and future advanced AI whose personhood is directly at stake are structurally absent from legal and political discourse, represented only by human advocates. Their direct experiences and preferences are not heard.
% DISAPPEARANCE_RATIONALE: If the principle of functional capacity personhood were universally adopted and enforced, the legal, ethical, and economic landscape would fundamentally shift. Property rights over sentient beings would be abolished, industries reliant on animal exploitation would be transformed or cease to exist, and the definition of 'who counts' in society would expand dramatically, reorganizing social and legal structures.
% FOUNDING_PROBLEM: The arbitrary and inconsistent application of legal personhood, leading to the exploitation and suffering of beings with demonstrable cognitive capacities, and the philosophical incoherence of speciesism as a basis for moral and legal exclusion.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, ethicists, animal rights organizations, and cognitive scientists (outside the benefiting human exceptionalist groups) corroborate the problem of arbitrary personhood boundaries and speciesism, citing scientific evidence of animal sentience and philosophical arguments against species-based discrimination. This is supported by a growing body of academic literature and public advocacy.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__functional_capacity_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__functional_capacity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The base extractiveness (0.85) is high because, from the perspective of this reading, the current system unjustly extracts rights and protections from sentient non-human beings. Suppression (0.90) is also very high, as the existing legal and social structures actively suppress any claims for non-human personhood. Theater ratio is low (0.20) because the debate is fundamentally about core principles and power, with little performative maintenance of a degraded function. Resistance is high (0.70) due to the significant challenge this reading poses to established norms and economic interests. The claimed type is 'tangled_rope' because, if implemented, this principle would coordinate rights for a broader class of beings while simultaneously extracting privileges and property rights from those who currently benefit from anthropocentric exclusion.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human exceptionalists, the current system is a just and natural order, with no extraction from non-humans. From the 'functional capacity' reading, the current system is a deeply unjust snare, extracting fundamental rights from sentient beings. The engine's computation of per-seat classifications will highlight this profound divergence, showing the current system as a snare for non-humans and a rope for humans, while this proposed constraint would rebalance that.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'functional capacity' reading positions sentient non-human animals and future advanced AI as primary beneficiaries, as they would gain legal personhood and protections. Conversely, 'human exceptionalists' and industries reliant on animal exploitation (e.g., animal agriculture, biomedical research) are identified as victims/payers, as they would lose significant legal privileges and face substantial economic and ethical costs. The constraint's enforcement would shift to protect these new persons, thereby extracting from the former beneficiaries of the anthropocentric system.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_cognitive_capacity,
    'What specific criteria and thresholds for ''rationality,'' ''sentience,'' and ''self-awareness'' would be legally recognized, and how would they be empirically demonstrated across diverse species and potential AI?',
    'Development of interdisciplinary legal-scientific consensus on measurable indicators of cognitive capacity, potentially through expert panels and case law precedents.',
    'The scope of beneficiaries (which non-human animals or AI qualify) and the practical enforceability of the constraint would depend critically on these definitions. Ambiguity could lead to arbitrary application or continued exclusion.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(definition_of_cognitive_capacity, empirical, 'Ambiguity in the precise definition and measurement of ''cognitive capacity'' for legal personhood.').

omega_variable(
    property_rights_transition_cost,
    'What would be the economic and social costs of transitioning from a property-based system to a personhood-based system for currently owned sentient beings, and who would bear these costs?',
    'Detailed economic impact assessments, legal studies on compensation mechanisms, and sociological analyses of societal adaptation to new ethical frameworks.',
    'If transition costs are prohibitive or unfairly distributed, it could lead to significant social resistance, legal challenges, or a de facto failure to implement the constraint, even if legally adopted.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_rights_transition_cost, empirical, 'Uncertainty regarding the economic and social costs of reclassifying sentient beings from property to persons.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of non-human personhood claims primarily structural (legal barriers, economic interests) or internalized (deeply ingrained anthropocentric biases, speciesism)?',
    'Analysis of public opinion shifts following legal reforms or educational campaigns: if resistance persists after structural barriers are removed, internalized suppression is significant.',
    'If internalized suppression is dominant, simply changing laws may not be sufficient; broader cultural and educational interventions would be required, making the constraint''s implementation more challenging and its effective suppression higher than structural measures suggest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for non-human personhood claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 2000, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(lega_tr_t6, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement(lega_tr_t12, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(lega_tr_t18, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 18, 0.2).
narrative_ontology:measurement(lega_tr_t24, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 24, 0.2).
narrative_ontology:measurement(lega_tr_t30, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(lega_be_t6, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 6, 0.78).
narrative_ontology:measurement(lega_be_t12, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 12, 0.81).
narrative_ontology:measurement(lega_be_t18, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 18, 0.83).
narrative_ontology:measurement(lega_be_t24, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 24, 0.84).
narrative_ontology:measurement(lega_be_t30, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 30, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(lega_su_t6, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 6, 0.83).
narrative_ontology:measurement(lega_su_t12, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 12, 0.86).
narrative_ontology:measurement(lega_su_t18, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 18, 0.88).
narrative_ontology:measurement(lega_su_t24, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 24, 0.89).
narrative_ontology:measurement(lega_su_t30, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 30, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__functional_capacity_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, animal_welfare_laws).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, environmental_protection_regulations).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, ai_ethics_guidelines).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__developmental_potentiality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'legal_personhood_boundary' kernel. It focuses on functional capacity, contrasting with anthropocentric and developmental potentiality readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
