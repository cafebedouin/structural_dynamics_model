% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_survival__hybrid_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_survival__hybrid_encoding_reading, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: catastrophe_memory_survival__hybrid_encoding_reading
 *   human_readable: Hybrid Encoding of Catastrophe Memory in Ritual
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes the inherent nature of ritual practice in
 *   catastrophe-survivor communities, where it functions on dual registers:
 *   symbolic boundary-maintenance and embedded practical knowledge, with
 *   community survival depending on both. The constraint is the persistence
 *   of this hybrid encoding. It is claimed as a Mountain because this dual
 *   function emerges naturally from the needs of collective memory and
 *   survival. The low but non-zero extractiveness and high suppression
 *   reflect the 'cost' imposed on analytical frameworks that attempt to force
 *   a binary classification, which the ritual itself resists.
 *
 * KEY AGENTS:
 *   - catastrophe_survivor_communities: Primary beneficiary and agenda-setter (organized/identity_locked)
 *   - ritual_practitioners: Agenda-setter and beneficiary (moderate/identity_locked)
 *   - binary_classification_analysts: Primary target/payer (moderate/constrained)
 *   - external_observers: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_survival__hybrid_encoding_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, 0.75).
domain_priors:theater_ratio(catastrophe_memory_survival__hybrid_encoding_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_survival__hybrid_encoding_reading, mountain).
narrative_ontology:human_readable(catastrophe_memory_survival__hybrid_encoding_reading, "Hybrid Encoding of Catastrophe Memory in Ritual").
narrative_ontology:topic_domain(catastrophe_memory_survival__hybrid_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:emerges_naturally(catastrophe_memory_survival__hybrid_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_survival__hybrid_encoding_reading, '7b2af719-a686-4b54-8fb1-b834d0f03470').
narrative_ontology:cs_kernel_codification('7b2af719-a686-4b54-8fb1-b834d0f03470', implicit).
narrative_ontology:cs_authority_grounding('7b2af719-a686-4b54-8fb1-b834d0f03470', practice).
narrative_ontology:cs_interpretation_layer_present('7b2af719-a686-4b54-8fb1-b834d0f03470').
narrative_ontology:cs_reading_relation('7b2af719-a686-4b54-8fb1-b834d0f03470', catastrophe_memory_survival__symbol_survival_reading, coexists_with).
narrative_ontology:cs_reading_relation('7b2af719-a686-4b54-8fb1-b834d0f03470', catastrophe_memory_survival__competence_transmission_reading, coexists_with).
narrative_ontology:cs_axiom('7b2af719-a686-4b54-8fb1-b834d0f03470', foundational, ritual_is_inherently_multi_functional).
narrative_ontology:cs_axiom_status(ritual_is_inherently_multi_functional, holdable).
narrative_ontology:cs_axiom_grounding('7b2af719-a686-4b54-8fb1-b834d0f03470', ritual_is_inherently_multi_functional, deontological).
narrative_ontology:cs_axiom('7b2af719-a686-4b54-8fb1-b834d0f03470', secondary, separation_reduces_efficacy).
narrative_ontology:cs_axiom_status(separation_reduces_efficacy, holdable).
narrative_ontology:cs_axiom_grounding('7b2af719-a686-4b54-8fb1-b834d0f03470', separation_reduces_efficacy, empirically_contingent).
narrative_ontology:cs_reference_frame('7b2af719-a686-4b54-8fb1-b834d0f03470', integrated_ritual_practice).
narrative_ontology:cs_drift_state('7b2af719-a686-4b54-8fb1-b834d0f03470', contemporary_analytical_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('7b2af719-a686-4b54-8fb1-b834d0f03470', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_survivor_communities).
narrative_ontology:constraint_victim(catastrophe_memory_survival__hybrid_encoding_reading, binary_classification_analysts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These communities benefit directly from the robust transmission of survival knowledge and cultural identity through hybrid rituals. They actively maintain and transmit these practices, often without explicit theoretical resolution of their dual functions.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_survivor_communities, beneficiary,
    organized, generational, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_survivor_communities, agenda_setter).

% The individuals who perform and transmit the rituals, embodying the integrated symbolic and practical knowledge. Their identity and social role are often deeply intertwined with the ritual practice.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practitioners, agenda_setter,
    moderate, biographical, identity_locked, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_survival__hybrid_encoding_reading, ritual_practitioners, beneficiary).

% Academics and researchers who attempt to analyze ritual by separating its functions into distinct symbolic or practical categories. The inherent hybridity of the ritual challenges their analytical frameworks, making clear-cut classification difficult.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, binary_classification_analysts, payer,
    moderate, biographical, constrained, global).

% Scholars and ethnographers who study these rituals, often grappling with the challenge of understanding their multi-functional nature without reducing one aspect to another. They are positioned to observe the tension between the ritual's lived reality and analytical attempts to simplify it.
narrative_ontology:constraint_stakeholder(catastrophe_memory_survival__hybrid_encoding_reading, external_observers, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Enables communities to collectively process, encode, and transmit complex, multi-layered knowledge essential for survival and identity maintenance after catastrophic events, integrating emotional, social, and practical dimensions.
% TRANSFER_FUNCTION: Transfers collective memory, social cohesion, cultural identity, and practical survival strategies across generations within the community, ensuring resilience and continuity.
% ABSENT_VOICES: Reductionist theorists who insist on a single, primary function for ritual (e.g., purely symbolic or purely instrumental) are structurally excluded from the community's internal understanding, as their frameworks fail to capture the lived hybridity and would be seen as misrepresenting the practice.
% DISAPPEARANCE_RATIONALE: If the hybrid encoding of catastrophe memory in ritual vanished, communities would lose a vital, integrated mechanism for transmitting complex survival knowledge and maintaining identity. This would lead to cultural fragmentation, reduced resilience, and potentially the loss of critical practical skills necessary for long-term survival.
% FOUNDING_PROBLEM: The core problem was how to ensure the long-term survival and resilience of a community after a catastrophic event, by transmitting both the trauma and the practical lessons learned across generations in a cohesive and enduring manner.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of post-catastrophe societies, oral histories from survivor communities, and cognitive science research on memory and ritual all corroborate the necessity of multi-modal encoding for robust cultural transmission and community resilience. This is attested by independent researchers and community elders, not just those benefiting from the status quo.
narrative_ontology:disappearance_verdict(catastrophe_memory_survival__hybrid_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_survival__hybrid_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_survival__hybrid_encoding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_survival__hybrid_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_survival__hybrid_encoding_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, ExtMetricName, E),
    domain_priors:suppression_score(catastrophe_memory_survival__hybrid_encoding_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(catastrophe_memory_survival__hybrid_encoding_reading),
    narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(catastrophe_memory_survival__hybrid_encoding_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(catastrophe_memory_survival__hybrid_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.15) because the constraint primarily describes a beneficial, naturally emergent property of ritual for communities. However, it is not zero because it 'extracts' a cost from analytical approaches that attempt to reduce ritual to a single function, forcing them to confront its inherent complexity. Suppression is high (0.75) for these reductionist analytical alternatives, as the lived reality of the ritual effectively 'suppresses' simpler interpretations. Theater ratio is very low (0.05) as the dual function is genuinely operative and essential for survival, not performative. Accessibility collapse is high (0.85) for alternative, non-hybrid understandings, as they fail to capture the phenomenon's full scope. Resistance is low (0.20) from the communities themselves, who naturally maintain this hybridity, but some resistance comes from analysts whose frameworks are challenged.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of catastrophe-survivor communities and ritual practitioners, the hybrid encoding is a natural and essential aspect of their survival and identity, experienced as a beneficial, integrated whole. From the perspective of binary classification analysts, the same phenomenon presents as a 'constraint' on their ability to apply simplified models, making their work more complex and challenging their theoretical assumptions.
 *
 * DIRECTIONALITY LOGIC:
 *   Catastrophe survivor communities and ritual practitioners are clear beneficiaries (d near 0.0) as the hybrid encoding directly supports their survival and cultural continuity. Binary classification analysts are targets (d near 1.0) because the constraint's inherent complexity challenges their preferred methodologies and forces them to adapt or fail to capture the phenomenon. External observers are analytical (d near 0.5), seeking to understand the dynamics without being directly subject to the constraint's 'extraction' or 'benefit' in the same way.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as it describes an emergent, functional property of ritual rather than a human-designed institution. The 'mandate' is the ongoing need for survival and identity transmission, which remains live. The classification as a Mountain (with FSM potential) prevents mislabeling this natural, beneficial complexity as a constructed snare, while still acknowledging the 'cost' it imposes on certain analytical approaches.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint accurately representing the `hybrid_encoding_reading` of the `catastrophe_memory_survival` kernel?',
    'Comparison with other readings of the same kernel, ensuring that the structural delta and unique axioms are clearly articulated and distinct.',
    'If misidentified, the analysis of the kernel''s overall contestation and the specific contributions of each reading would be compromised.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Confirms this constraint''s identity as a specific kernel reading.').

omega_variable(
    analytical_victimization_validity,
    'Are ''binary_classification_analysts'' truly ''victims'' of this constraint, or merely challenged by its complexity?',
    'Qualitative research into the lived experience of analysts attempting to apply reductionist frameworks to hybrid rituals, assessing the degree of professional cost, career impact, or theoretical impasse.',
    'If they are merely challenged, the extractiveness metric might be slightly lower, but the core classification as a Mountain (resisting simplification) would likely hold. If genuine victimization (e.g., career stagnation due to inability to publish within a dominant paradigm) is found, the extractiveness value is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(analytical_victimization_validity, empirical, 'Assesses the validity of classifying analysts as ''victims'' of the constraint.').

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the hybrid encoding of ritual a genuine natural law of human cognition and social organization, or a culturally constructed constraint that benefits identifiable agents (catastrophe survivor communities)?',
    'Cross-cultural comparative studies of post-catastrophe societies and neurocognitive research on memory formation and transmission in traumatic contexts. If the pattern is universal and robust across diverse cultural forms, it supports natural law. If it varies significantly with cultural context, it leans towards a constructed constraint.',
    'If primarily constructed, the classification would shift from Mountain to a highly stable Rope, acknowledging the community''s active role in its maintenance, even if beneficial. The FSM signature would then correctly reclassify it to a Tangled Rope if extraction were higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, empirical, 'Ambiguity between natural law and culturally constructed constraint for the hybrid encoding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_survival__hybrid_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 60, 0.05).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 80, 0.05).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_survival__hybrid_encoding_reading, theater_ratio, 100, 0.05).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 20, 0.13).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 40, 0.14).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 60, 0.15).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 80, 0.15).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_survival__hybrid_encoding_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 60, 0.74).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 80, 0.75).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_survival__hybrid_encoding_reading, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_survival__hybrid_encoding_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__symbol_survival_reading).
narrative_ontology:affects_constraint(catastrophe_memory_survival__hybrid_encoding_reading, catastrophe_memory_survival__competence_transmission_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'catastrophe_memory_survival' kernel, which describes how communities encode and transmit memory of catastrophic events. This 'hybrid_encoding_reading' emphasizes the integrated symbolic and practical functions of ritual, distinct from readings that prioritize one function over the other.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
