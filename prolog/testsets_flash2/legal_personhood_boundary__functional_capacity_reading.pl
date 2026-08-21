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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Legal Personhood Boundary: Functional Capacity Reading
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'functional capacity' reading of
 *   the legal personhood boundary kernel. It posits that legal personhood
 *   should be granted based on demonstrable cognitive capacities
 *   (rationality, sentience, self-awareness), irrespective of species. This
 *   reading challenges traditional anthropocentric views and
 *   developmental-potentiality arguments, potentially extending rights to
 *   non-human animals, ecosystems, and future advanced AI. The constraint is
 *   classified as a Tangled Rope because it offers a genuine coordination
 *   function (a universal, non-arbitrary basis for personhood) but also
 *   involves significant extraction from those currently denied personhood,
 *   requiring active enforcement to maintain the existing boundary.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.65).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.75).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Legal Personhood Boundary: Functional Capacity Reading").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, 'c9d13a13-d33d-4c98-ab58-3fbdfadc0aa9').
narrative_ontology:cs_kernel_codification('c9d13a13-d33d-4c98-ab58-3fbdfadc0aa9', formalized).
narrative_ontology:cs_authority_grounding('c9d13a13-d33d-4c98-ab58-3fbdfadc0aa9', lineage).
narrative_ontology:cs_interpretation_layer_present('c9d13a13-d33d-4c98-ab58-3fbdfadc0aa9').
narrative_ontology:cs_reading_relation('c9d13a13-d33d-4c98-ab58-3fbdfadc0aa9', legal_personhood_boundary__restrictive_anthropocentric_reading, influences).
narrative_ontology:cs_reading_relation('c9d13a13-d33d-4c98-ab58-3fbdfadc0aa9', legal_personhood_boundary__developmental_potentiality_reading, influences).
narrative_ontology:cs_axiom('c9d13a13-d33d-4c98-ab58-3fbdfadc0aa9', foundational, personhood_is_capacity_dependent).
narrative_ontology:cs_axiom_status(personhood_is_capacity_dependent, holdable).
narrative_ontology:cs_axiom_grounding('c9d13a13-d33d-4c98-ab58-3fbdfadc0aa9', personhood_is_capacity_dependent, deontological).
narrative_ontology:cs_axiom('c9d13a13-d33d-4c98-ab58-3fbdfadc0aa9', foundational, species_is_not_moral_criterion).
narrative_ontology:cs_axiom_status(species_is_not_moral_criterion, holdable).
narrative_ontology:cs_axiom_grounding('c9d13a13-d33d-4c98-ab58-3fbdfadc0aa9', species_is_not_moral_criterion, deontological).
narrative_ontology:cs_reference_frame('c9d13a13-d33d-4c98-ab58-3fbdfadc0aa9', universal_capacity_based_personhood).
narrative_ontology:cs_drift_state('c9d13a13-d33d-4c98-ab58-3fbdfadc0aa9', contemporary_legal_discourse, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c9d13a13-d33d-4c98-ab58-3fbdfadc0aa9', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, human_rights_advocates).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, animal_rights_advocates).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, environmental_advocates).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, non_human_sentient_beings).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, future_advanced_ai).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, ecosystems_as_entities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, industrial_agriculture).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, sentience_as_moral_basis).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, rationality_as_moral_basis).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, self_awareness_as_moral_basis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates for the expansion of rights based on demonstrable capacities, potentially extending personhood beyond traditional human boundaries. Benefits from the philosophical grounding this reading provides for universal rights.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, human_rights_advocates, beneficiary,
    organized, generational, mobile, global).

% Seeks to extend legal personhood to sentient non-human animals, challenging their status as property. This reading provides a strong legal and philosophical basis for their claims.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, animal_rights_advocates, beneficiary,
    organized, generational, constrained, global).

% Argues for the legal personhood of ecosystems or natural entities, often by analogy to sentient beings or by emphasizing their intrinsic value. This reading offers a pathway for such recognition.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, environmental_advocates, beneficiary,
    organized, civilizational, constrained, global).

% Currently lack legal personhood and are often treated as property, subject to instrumental use. This reading would recognize their inherent rights based on their cognitive capacities, shifting them from victim to rights-bearer.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, non_human_sentient_beings, payer,
    powerless, biographical, trapped, universal).

% Potential future entities that may demonstrate advanced cognitive capacities, including rationality and self-awareness, but currently have no legal standing. This reading anticipates their potential personhood.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, future_advanced_ai, payer,
    powerless, generational, trapped, universal).

% Adheres to the belief that personhood is exclusively a human attribute, often based on species membership or a specific interpretation of human dignity. They would strongly object to the expansion of personhood based on functional capacity.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, human_exceptionalists, excluded,
    institutional, generational, identity_locked, global).

% Relies on the property status of animals for its economic model. The expansion of personhood to sentient non-human animals would fundamentally challenge its operational and legal foundations, imposing significant costs.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, industrial_agriculture, payer,
    institutional, biographical, constrained, global).

% Responsible for interpreting and applying legal frameworks. This reading challenges existing precedents and requires a re-evaluation of foundational legal concepts, potentially leading to new jurisprudence.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, legal_scholars_and_judiciary, agenda_setter,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a consistent, non-arbitrary basis for assigning legal personhood and rights, moving beyond species-specific or developmental stage-specific criteria to a universal standard based on demonstrable capacities.
% TRANSFER_FUNCTION: Transfers moral and legal consideration, and potentially rights, from entities currently holding personhood (primarily humans) to those demonstrating relevant cognitive capacities, regardless of species. This implies a transfer of obligations and limitations on instrumental use.
% ABSENT_VOICES: Entities currently treated as property (e.g., sentient non-human animals) are unable to advocate for themselves. Future advanced AI, if it emerges, would also be an absent voice. Their interests are currently represented by advocates, but they lack direct agency.
% DISAPPEARANCE_RATIONALE: If this reading of personhood were universally adopted overnight, the legal and ethical landscape would fundamentally shift. Property rights over sentient beings would be challenged, new legal protections would emerge, and the moral status of non-human entities would be elevated, leading to a profound reorganization of human-animal and human-technology relationships.
% FOUNDING_PROBLEM: The problem of arbitrary and inconsistent criteria for legal personhood, leading to the exclusion and instrumentalization of beings capable of suffering or rational thought, based solely on species or developmental stage.
% FOUNDING_PROBLEM_CORROBORATION: Philosophers, ethicists, and a growing body of scientific research on animal cognition and AI capabilities corroborate the ongoing nature of this problem, highlighting the ethical inconsistencies of current legal frameworks from outside the anthropocentric and developmental-potentiality benefiting parties.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__functional_capacity_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__functional_capacity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legal_personhood_boundary__functional_capacity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__functional_capacity_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) reflects the ongoing denial of rights and instrumentalization of sentient non-human beings and potential future AI, which this reading seeks to rectify. Suppression (0.75) is high due to the entrenched legal and cultural frameworks that actively resist the expansion of personhood beyond humans. Resistance (0.8) is also high, driven by strong advocacy from animal rights and environmental groups. Accessibility collapse (0.4) is moderate, as alternative framings for personhood exist but face significant institutional barriers. Theater ratio (0.1) is low, as the debate is highly substantive and not primarily performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of human exceptionalists and industrial agriculture, this reading is a threat to established order and economic interests. From the perspective of animal rights and environmental advocates, it is a necessary correction to an unjust system. The engine's classification will highlight this divergence, showing how the same structural constraint is experienced as a beneficial coordination mechanism by some and an extractive snare by others.
 *
 * DIRECTIONALITY LOGIC:
 *   Advocates for human, animal, and environmental rights are beneficiaries, as this reading provides a strong basis for their claims. Non-human sentient beings, future advanced AI, and ecosystems (as entities) are victims, as they currently bear the costs of denied personhood. Human exceptionalists and industrial agriculture are excluded or payers, as their current benefits or operational models would be challenged by this reading. Legal scholars and the judiciary act as agenda-setters, interpreting and potentially reshaping the legal boundary.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    empirical_measurement_of_capacity,
    'How reliably and universally can ''demonstrable cognitive capacity'' (rationality, sentience, self-awareness) be measured across diverse species and potential AI, and what are the thresholds for legal personhood?',
    'Development of interdisciplinary scientific consensus on cognitive metrics and their application, potentially through a standing expert body or international scientific standards.',
    'Unclear or contested measurement would undermine the ''non-arbitrary'' claim of this reading, potentially leading to new forms of exclusion or arbitrary inclusion. Clear, agreed-upon metrics would strengthen its legal enforceability and ethical coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_measurement_of_capacity, empirical, 'The empirical challenge of defining and measuring cognitive capacities for legal personhood.').

omega_variable(
    scope_of_ecosystem_personhood,
    'If personhood is extended to ecosystems, what specific ''capacities'' or ''functions'' would ground their legal standing, and how would this be reconciled with the cognitive capacity framework?',
    'Development of a distinct, but compatible, theoretical framework for ''ecological personhood'' that either identifies analogous capacities or grounds personhood in systemic integrity and intrinsic value, rather than individual cognition.',
    'Without a clear grounding, the inclusion of ecosystems could dilute the coherence of the functional capacity reading or lead to internal contradictions. A robust framework would expand the scope of beneficiaries and legal protections.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_ecosystem_personhood, conceptual, 'Conceptual challenge of extending cognitive-capacity-based personhood to non-cognitive entities like ecosystems.').

omega_variable(
    resistance_to_property_status_change,
    'What is the true economic and social cost of reclassifying sentient non-human animals from ''property'' to ''persons'', and how would this be managed?',
    'Comprehensive economic modeling of industries reliant on animal exploitation, coupled with policy proposals for transition, compensation, and alternative economic activities.',
    'Underestimation of costs could lead to severe economic disruption and political backlash, hindering the adoption of this reading. Realistic assessment and transition plans could facilitate its implementation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(resistance_to_property_status_change, empirical, 'The practical and economic challenges of changing the legal status of sentient beings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(lega_tr_t10, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(lega_tr_t20, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(lega_tr_t30, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(lega_tr_t50, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(lega_be_t10, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(lega_be_t20, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(lega_be_t30, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement(lega_be_t50, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 50, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(lega_su_t10, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 10, 0.72).
narrative_ontology:measurement(lega_su_t20, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 20, 0.73).
narrative_ontology:measurement(lega_su_t30, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 30, 0.74).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 40, 0.75).
narrative_ontology:measurement(lega_su_t50, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 50, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__functional_capacity_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, animal_welfare_regulations).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, environmental_protection_laws).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, ai_ethics_governance).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
