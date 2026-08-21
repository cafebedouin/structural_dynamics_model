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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: Legal Personhood Based on Functional Cognitive Capacity
 *   domain: legal_philosophy/rights_theory/ethics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'functional_capacity_reading' of
 *   the 'legal_personhood_boundary' kernel. It posits that legal personhood
 *   should be determined by demonstrable cognitive capacities (e.g.,
 *   rationality, sentience, self-awareness) rather than species membership.
 *   This reading directly challenges the prevailing anthropocentric legal
 *   frameworks, which currently grant exclusive personhood to humans and
 *   treat non-human entities as property. The metrics reflect the high
 *   resistance and extraction inherent in challenging such a deeply
 *   entrenched system.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.8).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.9).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, rope).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Legal Personhood Based on Functional Cognitive Capacity").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal_philosophy/rights_theory/ethics").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, '7d87b0b2-c869-48cf-9d2a-035917a8dc72').
narrative_ontology:cs_kernel_codification('7d87b0b2-c869-48cf-9d2a-035917a8dc72', implicit).
narrative_ontology:cs_authority_grounding('7d87b0b2-c869-48cf-9d2a-035917a8dc72', distributed).
narrative_ontology:cs_reading_relation('7d87b0b2-c869-48cf-9d2a-035917a8dc72', legal_personhood_boundary__restrictive_anthropocentric_reading, forecloses).
narrative_ontology:cs_reading_relation('7d87b0b2-c869-48cf-9d2a-035917a8dc72', legal_personhood_boundary__developmental_potentiality_reading, coexists_with).
narrative_ontology:cs_axiom('7d87b0b2-c869-48cf-9d2a-035917a8dc72', foundational, personhood_requires_functional_capacity).
narrative_ontology:cs_axiom_status(personhood_requires_functional_capacity, holdable).
narrative_ontology:cs_axiom_grounding('7d87b0b2-c869-48cf-9d2a-035917a8dc72', personhood_requires_functional_capacity, deontological).
narrative_ontology:cs_axiom('7d87b0b2-c869-48cf-9d2a-035917a8dc72', foundational, species_is_not_a_moral_boundary).
narrative_ontology:cs_axiom_status(species_is_not_a_moral_boundary, holdable).
narrative_ontology:cs_axiom_grounding('7d87b0b2-c869-48cf-9d2a-035917a8dc72', species_is_not_a_moral_boundary, deontological).
narrative_ontology:cs_reference_frame('7d87b0b2-c869-48cf-9d2a-035917a8dc72', species_neutral_capacity_framework).
narrative_ontology:cs_drift_state('7d87b0b2-c869-48cf-9d2a-035917a8dc72', contemporary_ethical_legal_discourse, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('7d87b0b2-c869-48cf-9d2a-035917a8dc72', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, sentient_non_human_animals).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, future_advanced_ai).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, ecosystems_with_complex_interactivity).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, anthropocentric_rights_holders).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, industries_exploiting_sentient_beings).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Propose and refine the conceptual framework for personhood based on functional capacities, challenging existing anthropocentric legal norms. They seek to influence legal systems and public opinion.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, legal_philosophers_advocates, agenda_setter,
    analytical, generational, analytical, global).

% Would gain legal personhood and associated rights, shifting from property status to rights-bearing entities. Currently unable to advocate for themselves, they are the primary beneficiaries of this reading's adoption.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, sentient_non_human_animals, beneficiary,
    powerless, immediate, trapped, universal).

% Would be recognized as legal persons if they demonstrate sufficient cognitive capacities (rationality, self-awareness). Currently, their status is undefined or as property, but this reading anticipates their potential personhood.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, future_advanced_ai, beneficiary,
    powerless, civilizational, trapped, global).

% Currently benefit from exclusive human personhood, which grants them unique legal privileges and allows for the instrumental use of non-human entities. Adoption of this reading would challenge their exclusive status and impose new ethical and legal obligations.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, anthropocentric_rights_holders, payer,
    powerful, generational, constrained, global).

% Benefit economically from the property status of non-human animals. This reading would impose significant costs by restricting or prohibiting their current practices, requiring fundamental shifts in business models and legal frameworks.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, industries_exploiting_sentient_beings, payer,
    institutional, biographical, constrained, global).

% Currently operate under anthropocentric personhood frameworks. They are the gatekeepers for legal change and would face immense pressure to adapt or resist the redefinition of personhood, requiring legislative and judicial re-evaluation.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, existing_legal_systems, agenda_setter,
    institutional, generational, constrained, national).

% Influenced by philosophical arguments and scientific discoveries, public opinion can shift towards or away from expanding personhood. It acts as a diffuse force, either supporting or resisting legal and ethical reforms.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, public_opinion, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__functional_capacity_reading, diffuse).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__functional_capacity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a consistent, non-arbitrary, and ethically defensible basis for legal personhood and rights across all beings, ensuring moral consideration aligns with demonstrable cognitive capacities (rationality, sentience, self-awareness) regardless of species.
% TRANSFER_FUNCTION: Transfers moral and legal consideration, and potentially rights, from an exclusive human domain to a broader set of entities based on functional capacity. This implies a transfer of costs (e.g., restrictions on exploitation, new legal obligations) to those who currently benefit from the exclusion of these entities.
% ABSENT_VOICES: The primary beneficiaries of this reading (sentient non-human animals, future advanced AI) are currently absent from the legal discourse, unable to advocate for their own personhood. Their interests are represented by human advocates, but their direct voice is excluded by their current legal status.
% DISAPPEARANCE_RATIONALE: If the conceptual constraint of personhood based on functional capacity vanished, the legal and ethical landscape would remain anthropocentric by default. The pressure for expanding personhood would dissipate, leaving existing power structures and exploitative practices unchallenged, and the moral status of non-human entities would revert to property or instrumental value.
% FOUNDING_PROBLEM: The arbitrary and inconsistent application of moral and legal status based solely on species membership, leading to the exploitation and suffering of demonstrably sentient and cognitively capable beings, and failing to account for potential future non-human intelligences.
% FOUNDING_PROBLEM_CORROBORATION: Ethicists, animal welfare scientists, cognitive scientists, and some legal scholars (outside the anthropocentric beneficiary group) corroborate the problem of arbitrary speciesism and the ethical imperative to align rights with capacities. Scientific advancements in animal cognition and AI development further underscore the urgency of this problem.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__functional_capacity_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__functional_capacity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(legal_personhood_boundary__functional_capacity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__functional_capacity_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

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
 *   The base extractiveness is high (0.8) because the current anthropocentric system extracts immense value from treating non-human sentient beings as property, and this reading directly threatens that extraction. Suppression is very high (0.9) as existing legal, economic, and social structures actively resist any redefinition of personhood that would expand rights beyond humans. Resistance is also high (0.9) due to the fundamental nature of the challenge. Theater ratio is low (0.1) because this is a direct, conceptual challenge to a foundational legal principle, with little performative maintenance of a degraded function. The claimed type is 'rope' because this reading proposes a new, more consistent coordination mechanism for rights, even though its current effect is to disrupt existing 'snare'-like structures.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of anthropocentric rights holders and industries, the current system is a 'rope' that coordinates human society and economic activity. From the perspective of this functional capacity reading, the current system is a 'snare' that unjustly extracts from and suppresses non-human sentient beings. The engine's computation of per-seat classifications will highlight this fundamental divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Legal philosophers and advocates are agenda-setters, pushing for this redefinition. Sentient non-human animals and future advanced AI are the primary beneficiaries, as they would gain legal personhood. Anthropocentric rights holders and industries exploiting sentient beings are the payers, as they would lose exclusive privileges and face significant economic and legal costs. Existing legal systems act as agenda-setters, mediating the debate and resisting change, while public opinion is an observer, whose shifts can influence the trajectory of the debate.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately representing the ''functional_capacity_reading'' of the ''legal_personhood_boundary'' kernel?',
    'Comparison with canonical texts and leading proponents of the functional capacity approach to personhood in legal philosophy.',
    'If misaligned, the analysis of inter-reading relations and axiom conflicts would be inaccurate, potentially misrepresenting the structural dynamics of the personhood debate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Verifies the fidelity of this story to the intended kernel reading.').

omega_variable(
    definition_of_cognitive_capacity,
    'What constitutes ''demonstrable cognitive capacity'' (rationality, sentience, self-awareness) for legal purposes, and how is it measured across diverse species and potential AI?',
    'Development of interdisciplinary consensus standards by cognitive scientists, ethicists, and legal scholars, potentially codified in legislation or judicial precedent.',
    'Ambiguity in definition could lead to arbitrary application, creating new forms of exclusion or making the framework impractical to implement. Clear definitions would strengthen the reading''s coherence and enforceability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_cognitive_capacity, empirical, 'Clarifies the operational criteria for personhood under this reading.').

omega_variable(
    implementation_feasibility,
    'Given the immense resistance, what are the practical pathways and timelines for implementing a legal framework based on functional capacity, and what are the likely transitional costs?',
    'Detailed legal and economic modeling of proposed legislative changes, case studies of incremental legal reforms (e.g., ''rights of nature'' movements), and analysis of public acceptance trajectories.',
    'If implementation is deemed infeasible or prohibitively costly, the reading might remain a philosophical ideal rather than a practical legal constraint, reducing its effective extractiveness and suppression on existing systems.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implementation_feasibility, empirical, 'Assesses the practical viability of the functional capacity personhood framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 1970, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t1970, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(lega_tr_t1980, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 1980, 0.1).
narrative_ontology:measurement(lega_tr_t1990, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 1990, 0.1).
narrative_ontology:measurement(lega_tr_t2000, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(lega_tr_t2010, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(lega_tr_t2020, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2020, 0.1).
narrative_ontology:measurement(lega_tr_t2030, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 2030, 0.1).

% Extraction over time
narrative_ontology:measurement(lega_be_t1970, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(lega_be_t1980, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(lega_be_t1990, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 1990, 0.7).
narrative_ontology:measurement(lega_be_t2000, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(lega_be_t2010, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2010, 0.78).
narrative_ontology:measurement(lega_be_t2020, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2020, 0.8).
narrative_ontology:measurement(lega_be_t2030, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 2030, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t1970, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(lega_su_t1980, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(lega_su_t1990, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(lega_su_t2000, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2000, 0.85).
narrative_ontology:measurement(lega_su_t2010, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2010, 0.88).
narrative_ontology:measurement(lega_su_t2020, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2020, 0.9).
narrative_ontology:measurement(lega_su_t2030, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 2030, 0.92).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__functional_capacity_reading, identity_coordination).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, animal_property_status).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, ai_liability_frameworks).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, environmental_protection_laws).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__developmental_potentiality_reading).

% DUAL FORMULATION NOTE:
% This is one of three distinct readings of the 'legal_personhood_boundary' kernel, each defining personhood differently. This reading challenges the premises of the other two, creating a complex network of influence and foreclosure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
