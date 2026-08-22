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
 *   domain: legal_philosophy/rights_theory
 *
 * SUMMARY:
 *   This constraint represents the 'functional capacity' reading of legal
 *   personhood, which posits that personhood (and thus rights) should be
 *   granted based on demonstrable cognitive capacities like rationality,
 *   sentience, and self-awareness, irrespective of species. This reading
 *   directly challenges anthropocentric and speciesist legal frameworks,
 *   potentially extending rights to non-human animals and future AI. The
 *   current legal system, largely based on a restrictive anthropocentric
 *   view, operates as a Tangled Rope, coordinating human society while
 *   extracting heavily from non-human sentient beings. The metrics reflect
 *   the high extraction and suppression inherent in the current system from
 *   the perspective of this reading.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.65).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.78).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Legal Personhood Boundary: Functional Capacity Reading").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal_philosophy/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, 'e064d806-7f6c-4923-8dad-f1a7abd6b105').
narrative_ontology:cs_kernel_codification('e064d806-7f6c-4923-8dad-f1a7abd6b105', distributed).
narrative_ontology:cs_authority_grounding('e064d806-7f6c-4923-8dad-f1a7abd6b105', distributed).
narrative_ontology:cs_reading_relation('e064d806-7f6c-4923-8dad-f1a7abd6b105', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('e064d806-7f6c-4923-8dad-f1a7abd6b105', legal_personhood_boundary__developmental_potentiality_reading, coexists_with).
narrative_ontology:cs_axiom('e064d806-7f6c-4923-8dad-f1a7abd6b105', foundational, cognitive_capacity_is_moral_basis).
narrative_ontology:cs_axiom_status(cognitive_capacity_is_moral_basis, holdable).
narrative_ontology:cs_axiom_grounding('e064d806-7f6c-4923-8dad-f1a7abd6b105', cognitive_capacity_is_moral_basis, deontological).
narrative_ontology:cs_axiom('e064d806-7f6c-4923-8dad-f1a7abd6b105', foundational, species_is_not_moral_basis).
narrative_ontology:cs_axiom_status(species_is_not_moral_basis, holdable).
narrative_ontology:cs_axiom_grounding('e064d806-7f6c-4923-8dad-f1a7abd6b105', species_is_not_moral_basis, deontological).
narrative_ontology:cs_reference_frame('e064d806-7f6c-4923-8dad-f1a7abd6b105', universal_sentience_rights).
narrative_ontology:cs_drift_state('e064d806-7f6c-4923-8dad-f1a7abd6b105', contemporary_legal_frameworks, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('e064d806-7f6c-4923-8dad-f1a7abd6b105', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, human_property_owners).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, industrial_agriculture).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, research_institutions).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, sentient_non_human_animals).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, future_advanced_ai).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, sentience_as_moral_basis).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, rationality_as_moral_basis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Currently benefit from the legal classification of non-human animals as property, allowing their use for food, labor, and entertainment without rights. This reading challenges that status, potentially imposing significant costs.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, human_property_owners, beneficiary,
    powerful, generational, constrained, global).

% Relies on the current legal status of animals as commodities. A shift to functional capacity personhood would necessitate radical changes in practices, potentially leading to massive economic disruption and increased costs.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, industrial_agriculture, beneficiary,
    institutional, biographical, constrained, global).

% Conducts research using sentient non-human animals. Granting personhood based on functional capacity would severely restrict or prohibit many forms of animal experimentation, impacting scientific progress and methodologies.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, research_institutions, beneficiary,
    organized, biographical, constrained, global).

% Currently bear the full cost of being classified as property, including suffering, exploitation, and death, without legal recourse. This reading would grant them legal protection and rights based on their demonstrable cognitive capacities.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, sentient_non_human_animals, payer,
    powerless, immediate, trapped, global).

% As hypothetical entities, they would be denied personhood and potentially exploited if their functional capacity (rationality, sentience) is not recognized as a basis for rights. This reading anticipates their potential inclusion.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, future_advanced_ai, payer,
    powerless, civilizational, trapped, universal).

% Actively campaign for the expansion of personhood to sentient non-human animals based on their cognitive abilities. They seek to change legal frameworks and societal norms to reflect this ethical stance.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, animal_rights_advocates, agenda_setter,
    organized, generational, mobile, global).

% Analyze and debate the theoretical underpinnings and practical implications of different personhood criteria, including functional capacity. Their work informs legal and ethical discourse.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, legal_scholars_and_philosophers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a consistent and ethically defensible boundary for legal personhood based on observable and measurable cognitive functions, aiming to resolve inconsistencies in current rights assignments.
% TRANSFER_FUNCTION: Transfers legal rights and protections from an exclusive human domain to any entity demonstrating sufficient cognitive capacity, thereby transferring costs (e.g., restrictions on use) to those who currently benefit from their non-person status.
% ABSENT_VOICES: The direct voices of sentient non-human animals and future advanced AI are absent from the legal and philosophical discourse, requiring human advocates to represent their interests. Their suffering and potential for exploitation are currently unrepresented in legal frameworks.
% DISAPPEARANCE_RATIONALE: If the functional capacity reading of personhood were universally adopted overnight, the legal status of countless non-human animals would change, challenging property laws, agricultural practices, and research ethics. The world would fundamentally reorganize around a broader definition of rights-bearers.
% FOUNDING_PROBLEM: The problem of arbitrary and inconsistent assignment of legal rights and protections, particularly the exclusion of demonstrably sentient and rational non-human entities from personhood, leading to their exploitation.
% FOUNDING_PROBLEM_CORROBORATION: Animal welfare scientists, ethologists, and neuroscientists provide extensive empirical corroboration for the cognitive capacities of many non-human animals. Legal scholars and ethicists outside the direct beneficiary groups (e.g., industrial agriculture) also attest to the philosophical and legal inconsistencies of anthropocentric personhood.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__functional_capacity_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__functional_capacity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_gemini+stakeholder_backfill', 'agent/example_platform_commission.json',
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
 *   The extractiveness is high (0.65) because the current system denies fundamental rights to many sentient beings, allowing their instrumentalization for human benefit. Suppression is also high (0.78) due to the active legal and social enforcement of speciesist hierarchies and property laws that prevent non-human animals from having legal standing or recourse. The theater ratio is low (0.1) as the system is genuinely functional in its current extractive mode, with little performative maintenance masking atrophy. Resistance is high (0.8) reflecting the ongoing and growing animal rights movement and philosophical challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of current beneficiaries (e.g., industrial agriculture), the existing system is a necessary coordination mechanism for food production and economic activity. From the perspective of the functional capacity reading, and its advocates, the same system is a deeply extractive and suppressive structure that denies fundamental rights based on arbitrary criteria. The engine will compute this divergence based on the declared roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Human property owners, industrial agriculture, and research institutions are beneficiaries, as they profit from the current non-person status of animals. Sentient non-human animals and future advanced AI are victims, bearing the full cost of their exclusion from personhood. Animal rights advocates act as agenda-setters, pushing for the adoption of this reading. Legal scholars and philosophers are observers, analyzing the implications.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_of_cognitive_capacity,
    'How reliably and universally can ''demonstrable cognitive capacity'' (rationality, sentience, self-awareness) be measured across diverse species and potential AI forms?',
    'Development of standardized, cross-species cognitive assessment protocols and AI consciousness metrics, validated by interdisciplinary consensus.',
    'If measurement is unreliable, the functional capacity reading''s implementation would be arbitrary, potentially leading to new forms of exclusion or over-inclusion. If reliable, it provides a robust, non-speciesist basis for personhood.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(measurement_of_cognitive_capacity, empirical, 'The empirical challenge of consistently measuring cognitive capacity across diverse entities.').

omega_variable(
    economic_disruption_vs_moral_imperative,
    'To what extent should the economic disruption caused by extending personhood to non-human animals outweigh the moral imperative to end their exploitation?',
    'Societal and political deliberation, potentially leading to new legal frameworks that balance economic transition with ethical obligations, or a clear prioritization of one over the other.',
    'Prioritizing economic stability would slow or halt the adoption of this reading, maintaining the current extractive system. Prioritizing the moral imperative would accelerate legal reform, leading to significant economic restructuring.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(economic_disruption_vs_moral_imperative, preference, 'The tension between economic consequences and ethical demands in personhood reform.').

omega_variable(
    speciesism_vs_functionalism,
    'Is the ''functional capacity'' reading truly non-speciesist, or does it implicitly favor capacities more common in humans, thereby creating a new form of exclusion?',
    'Philosophical analysis and ethical debate, examining whether the chosen capacities are universally applicable or culturally biased, and exploring alternative criteria for personhood.',
    'If biased, the reading risks perpetuating a subtle form of anthropocentrism. If truly universal, it provides a more robust and inclusive foundation for rights.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(speciesism_vs_functionalism, conceptual, 'Whether functional capacity criteria are truly universal or implicitly biased towards human-like traits.').


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
narrative_ontology:measurement(lega_be_t1970, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 1970, 0.8).
narrative_ontology:measurement(lega_be_t1990, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(lega_be_t2010, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(lega_be_t2030, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2030, 0.68).
narrative_ontology:measurement(lega_be_t2050, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2050, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t1970, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 1970, 0.9).
narrative_ontology:measurement(lega_su_t1990, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(lega_su_t2010, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(lega_su_t2030, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2030, 0.79).
narrative_ontology:measurement(lega_su_t2050, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2050, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__functional_capacity_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, animal_welfare_regulations).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, property_law_frameworks).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, ai_ethics_guidelines).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'legal_personhood_boundary' kernel. Its ε value differs significantly from the 'restrictive_anthropocentric_reading' (higher extraction from non-humans) and the 'developmental_potentiality_reading' (different victim set and scope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
