% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__restrictive_anthropocentric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__restrictive_anthropocentric_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: legal_personhood_boundary__restrictive_anthropocentric_reading
 *   human_readable: Legal Personhood Boundary (Restrictive Anthropocentric Reading)
 *   domain: legal_philosophy/constitutional_law/rights_theory
 *
 * SUMMARY:
 *   This constraint defines legal personhood as limited to born humans with
 *   cognitive capacity, a restrictive anthropocentric reading of the broader
 *   'legal_personhood_boundary' kernel. It explicitly excludes fetuses,
 *   non-human animals, ecosystems, and artificial intelligences from legal
 *   personhood, thereby maximizing the autonomy of pregnant persons and
 *   minimizing state intervention in reproduction and environmental law. The
 *   constraint is claimed as a Tangled Rope, reflecting its dual function of
 *   coordinating human legal society while extracting heavily from those it
 *   excludes.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.85).
domain_priors:suppression_score(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.9).
domain_priors:theater_ratio(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(legal_personhood_boundary__restrictive_anthropocentric_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__restrictive_anthropocentric_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__restrictive_anthropocentric_reading, "Legal Personhood Boundary (Restrictive Anthropocentric Reading)").
narrative_ontology:topic_domain(legal_personhood_boundary__restrictive_anthropocentric_reading, "legal_philosophy/constitutional_law/rights_theory").

domain_priors:requires_active_enforcement(legal_personhood_boundary__restrictive_anthropocentric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__restrictive_anthropocentric_reading, 'af414d63-a81b-4cda-b97d-3a93fbb4c34f').
narrative_ontology:cs_kernel_codification('af414d63-a81b-4cda-b97d-3a93fbb4c34f', formalized).
narrative_ontology:cs_authority_grounding('af414d63-a81b-4cda-b97d-3a93fbb4c34f', lineage).
narrative_ontology:cs_interpretation_layer_present('af414d63-a81b-4cda-b97d-3a93fbb4c34f').
narrative_ontology:cs_reading_relation('af414d63-a81b-4cda-b97d-3a93fbb4c34f', legal_personhood_boundary__developmental_potentiality_reading, forecloses).
narrative_ontology:cs_reading_relation('af414d63-a81b-4cda-b97d-3a93fbb4c34f', legal_personhood_boundary__functional_capacity_reading, forecloses).
narrative_ontology:cs_axiom('af414d63-a81b-4cda-b97d-3a93fbb4c34f', foundational, human_species_membership_is_necessary_for_personhood).
narrative_ontology:cs_axiom_status(human_species_membership_is_necessary_for_personhood, holdable).
narrative_ontology:cs_axiom_grounding('af414d63-a81b-4cda-b97d-3a93fbb4c34f', human_species_membership_is_necessary_for_personhood, deontological).
narrative_ontology:cs_axiom('af414d63-a81b-4cda-b97d-3a93fbb4c34f', foundational, post_natal_existence_is_necessary_for_personhood).
narrative_ontology:cs_axiom_status(post_natal_existence_is_necessary_for_personhood, holdable).
narrative_ontology:cs_axiom_grounding('af414d63-a81b-4cda-b97d-3a93fbb4c34f', post_natal_existence_is_necessary_for_personhood, deontological).
narrative_ontology:cs_reference_frame('af414d63-a81b-4cda-b97d-3a93fbb4c34f', classical_legal_anthropocentrism).
narrative_ontology:cs_drift_state('af414d63-a81b-4cda-b97d-3a93fbb4c34f', contemporary_rights_expansion_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('af414d63-a81b-4cda-b97d-3a93fbb4c34f', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, born_cognitively_capable_humans).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_systems).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, fetuses).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, non_human_animals).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, ecosystems).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligences).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(legal_personhood_boundary__restrictive_anthropocentric_reading, advocacy_groups_for_excluded).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, human_exceptionalism_doctrine).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__restrictive_anthropocentric_reading, individual_autonomy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the primary beneficiaries, they possess full legal personhood, rights, and protections, and are the only entities capable of exercising legal agency within this framework. Their status is secured by the exclusion of other potential rights-bearers.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, born_cognitively_capable_humans, beneficiary,
    powerful, generational, analytical, universal).

% Administer and enforce the personhood boundary, providing a clear, albeit restrictive, framework for legal rights and duties. They benefit from the clarity and reduced complexity of a limited set of legal persons, but face increasing pressure to expand the boundary.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_systems, agenda_setter,
    institutional, civilizational, constrained, global).

% Benefit from the autonomy over their bodies and reproductive choices that this restrictive personhood definition affords, as fetuses are not granted independent legal personhood. Their rights are prioritized over potential fetal claims.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, pregnant_persons, beneficiary,
    moderate, biographical, mobile, national).

% Are explicitly denied legal personhood and its associated rights, existing instead as property or extensions of the pregnant person. They have no legal standing to assert claims.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, fetuses, excluded,
    powerless, immediate, trapped, local).

% Are largely treated as property or resources, with limited welfare protections that do not equate to legal personhood. Their interests are not recognized as rights in themselves.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, non_human_animals, excluded,
    powerless, biographical, trapped, global).

% Are denied legal personhood, meaning their intrinsic value and right to exist are not recognized. Environmental protections are typically framed in terms of human benefit or property rights, not the rights of the ecosystem itself.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, ecosystems, excluded,
    powerless, generational, trapped, global).

% Are currently denied legal personhood, regardless of their demonstrated cognitive capacities. Their status is that of tools or property, with no independent rights or responsibilities.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, artificial_intelligences, excluded,
    powerless, generational, trapped, global).

% Bear the costs of challenging the existing personhood boundary through legal, political, and social means. They face significant institutional resistance and resource disparities in their efforts to expand legal personhood.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, advocacy_groups_for_excluded, payer,
    organized, generational, constrained, global).

% Analyze, critique, and propose alternative frameworks for legal personhood. They are not directly subject to the constraint's extraction but provide the intellectual foundation for challenges and reforms.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__restrictive_anthropocentric_reading, legal_scholars_and_philosophers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__restrictive_anthropocentric_reading, born_cognitively_capable_humans).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__restrictive_anthropocentric_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit narrow, boundary for legal rights and responsibilities, allowing legal systems to function with a defined set of actors and preventing legal chaos from an unbounded definition of personhood.
% TRANSFER_FUNCTION: Transfers full legal rights, protections, and agency to born, cognitively capable humans, while denying these to fetuses, non-human animals, ecosystems, and artificial intelligences. It also transfers autonomy over reproductive decisions to pregnant persons.
% ABSENT_VOICES: The excluded entities themselves (fetuses, non-human animals, ecosystems, artificial intelligences) cannot speak for themselves in the legal system. Their interests are represented by advocacy groups, but their direct voices are absent from the foundational legal discourse.
% DISAPPEARANCE_RATIONALE: If this restrictive personhood boundary vanished overnight, the entire legal framework would collapse. Questions of rights, duties, property, and agency would become immediately ambiguous for a vast array of entities, leading to profound societal and legal reorganization.
% FOUNDING_PROBLEM: To establish a clear and manageable scope for legal rights and duties, preventing legal chaos and ensuring that the legal system could effectively govern human society, while also protecting human autonomy.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, historical legal texts, and human rights declarations attest to the historical need for clear legal boundaries. However, advocacy groups and some philosophers contest whether the current restrictive boundary still optimally solves the problem, given evolving scientific and ethical understandings of consciousness, sentience, and ecological interdependence.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__restrictive_anthropocentric_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__restrictive_anthropocentric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legal_personhood_boundary__restrictive_anthropocentric_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__restrictive_anthropocentric_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__restrictive_anthropocentric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__restrictive_anthropocentric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because it denies fundamental rights and legal standing to a vast array of entities. Suppression (0.90) is also high, as legal systems actively enforce this boundary, denying claims and resisting challenges to expand personhood. The theater ratio (0.10) is low, indicating that the constraint is highly functional in its exclusionary purpose, with little performative maintenance. Accessibility collapse (0.95) is near total for excluded entities, as there are almost no legal avenues for them to gain personhood within this framework. Resistance (0.70) is substantial, driven by persistent advocacy for the rights of excluded groups.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of born, cognitively capable humans and legal systems, this constraint provides essential clarity and order, appearing as a necessary coordination mechanism. However, from the perspective of advocacy groups for the excluded, and analytically, it functions as a highly extractive and suppressive mechanism that denies fundamental recognition to other entities.
 *
 * DIRECTIONALITY LOGIC:
 *   Born, cognitively capable humans and legal systems are primary beneficiaries, gaining legal clarity and privileged status. Pregnant persons also benefit from enhanced autonomy. Fetuses, non-human animals, ecosystems, and artificial intelligences are the direct targets of extraction, being denied legal personhood. Advocacy groups for the excluded bear the costs of challenging this entrenched boundary.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate to provide legal clarity and prevent chaos is still live, but its status is contested. Critics argue that while the founding problem of legal ambiguity was real, the current restrictive solution has outlived its optimal function, becoming a mechanism for maintaining human exceptionalism and avoiding complex ethical questions, rather than purely coordinating. The persistence of the constraint is increasingly due to the benefits it confers on the included, rather than solely solving the original problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine, independent legal principle, or one specific reading of the broader ''legal_personhood_boundary'' kernel?',
    'Analysis of legal scholarship and judicial opinions that explicitly acknowledge or dispute alternative personhood framings.',
    'If confirmed as a reading, its classification is understood in relation to its siblings, and its stability is tied to the contestation of the kernel. If independent, its classification stands alone.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint''s identity as a specific reading of a contested kernel.').

omega_variable(
    developmental_potentiality_impact,
    'How would the legal system''s structure and the victim set change if the ''developmental_potentiality_reading'' (personhood from conception) were adopted?',
    'Comparative legal analysis of jurisdictions that have adopted fetal personhood laws, examining their impact on reproductive rights and legal standing.',
    'The victim set would shift, with fetuses gaining legal personhood and pregnant persons potentially losing autonomy. The constraint''s extractiveness would shift from fetuses to pregnant persons.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developmental_potentiality_impact, empirical, 'Impact of adopting the developmental potentiality reading.').

omega_variable(
    functional_capacity_impact,
    'How would the legal system''s structure and the victim set change if the ''functional_capacity_reading'' (personhood regardless of species) were adopted?',
    'Philosophical and legal analysis of criteria for ''demonstrable cognitive capacity'' and its application to non-human animals and AI, alongside hypothetical legal frameworks.',
    'The victim set would shrink to only those entities lacking demonstrable cognitive capacity, potentially including some humans, while expanding to include many non-human animals and advanced AI. The constraint''s extractiveness would be significantly reallocated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(functional_capacity_impact, conceptual, 'Impact of adopting the functional capacity reading.').

omega_variable(
    cognitive_capacity_definition_ambiguity,
    'What constitutes ''cognitive capacity'' for the purpose of legal personhood, and how is it measured consistently across diverse entities?',
    'Interdisciplinary consensus from neuroscience, philosophy of mind, and AI ethics on measurable criteria for sentience, self-awareness, and rationality.',
    'Ambiguity in this definition allows for arbitrary exclusion or inclusion, making the constraint''s application inconsistent. Clarification would either solidify or destabilize the current boundary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cognitive_capacity_definition_ambiguity, empirical, 'Ambiguity in the definition of ''cognitive capacity''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__restrictive_anthropocentric_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t1948, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 1948, 0.1).
narrative_ontology:measurement(lega_tr_t1960, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 1960, 0.1).
narrative_ontology:measurement(lega_tr_t1975, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(lega_tr_t1990, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(lega_tr_t2005, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(lega_tr_t2024, legal_personhood_boundary__restrictive_anthropocentric_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(lega_be_t1948, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 1948, 0.8).
narrative_ontology:measurement(lega_be_t1960, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 1960, 0.82).
narrative_ontology:measurement(lega_be_t1975, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 1975, 0.83).
narrative_ontology:measurement(lega_be_t1990, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 1990, 0.84).
narrative_ontology:measurement(lega_be_t2005, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 2005, 0.85).
narrative_ontology:measurement(lega_be_t2024, legal_personhood_boundary__restrictive_anthropocentric_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t1948, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 1948, 0.75).
narrative_ontology:measurement(lega_su_t1960, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 1960, 0.78).
narrative_ontology:measurement(lega_su_t1975, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 1975, 0.82).
narrative_ontology:measurement(lega_su_t1990, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 1990, 0.86).
narrative_ontology:measurement(lega_su_t2005, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 2005, 0.88).
narrative_ontology:measurement(lega_su_t2024, legal_personhood_boundary__restrictive_anthropocentric_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__restrictive_anthropocentric_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
