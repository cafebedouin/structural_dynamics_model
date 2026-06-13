% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__constitutive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__constitutive_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: montevideo_statehood_criteria__constitutive_reading
 *   human_readable: Statehood requires recognition by the existing community of states (Constitutive Reading)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'constitutive' theory of statehood, which
 *   posits that a political entity becomes a state only through recognition
 *   by other existing states. It is a reading of the broader Montevideo
 *   Statehood Criteria kernel. This reading grants existing states a powerful
 *   gatekeeping function, effectively making statehood a political rather
 *   than purely objective legal fact. Unrecognized polities, even if they
 *   meet objective criteria, are denied full international legal personality
 *   and access to global systems.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.85).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.9).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, snare).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Statehood requires recognition by the existing community of states (Constitutive Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, 'e97d9aaf-bc1c-4bee-b387-3e15bad29c37').
narrative_ontology:cs_kernel_codification('e97d9aaf-bc1c-4bee-b387-3e15bad29c37', formalized).
narrative_ontology:cs_authority_grounding('e97d9aaf-bc1c-4bee-b387-3e15bad29c37', lineage).
narrative_ontology:cs_interpretation_layer_present('e97d9aaf-bc1c-4bee-b387-3e15bad29c37').
narrative_ontology:cs_reading_relation('e97d9aaf-bc1c-4bee-b387-3e15bad29c37', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('e97d9aaf-bc1c-4bee-b387-3e15bad29c37', montevideo_statehood_criteria__hybrid_reading, influences).
narrative_ontology:cs_axiom('e97d9aaf-bc1c-4bee-b387-3e15bad29c37', foundational, recognition_is_constitutive_of_statehood).
narrative_ontology:cs_axiom_status(recognition_is_constitutive_of_statehood, holdable).
narrative_ontology:cs_axiom_grounding('e97d9aaf-bc1c-4bee-b387-3e15bad29c37', recognition_is_constitutive_of_statehood, conventional).
narrative_ontology:cs_axiom('e97d9aaf-bc1c-4bee-b387-3e15bad29c37', secondary, existing_states_have_veto_over_new_entrants).
narrative_ontology:cs_axiom_status(existing_states_have_veto_over_new_entrants, holdable).
narrative_ontology:cs_axiom_grounding('e97d9aaf-bc1c-4bee-b387-3e15bad29c37', existing_states_have_veto_over_new_entrants, conventional).
narrative_ontology:cs_reference_frame('e97d9aaf-bc1c-4bee-b387-3e15bad29c37', westphalian_state_sovereignty).
narrative_ontology:cs_drift_state('e97d9aaf-bc1c-4bee-b387-3e15bad29c37', contemporary_human_rights_era, gap(revival_pressure, minor, false)).
narrative_ontology:cs_created_at('e97d9aaf-bc1c-4bee-b387-3e15bad29c37', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, existing_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, international_organizations).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, aspiring_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of the 'community of states' whose recognition is required for new statehood. They collectively hold a veto power over the entry of new polities into the international system, thereby preserving their own status and influence. They benefit from the stability and predictability of the existing order.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, existing_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Entities that meet objective criteria for statehood (territory, population, government, capacity to enter relations) but lack recognition from the existing community of states. They are denied full participation in international law, treaties, and economic systems, facing severe limitations on their sovereignty and development.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities, payer,
    powerless, generational, trapped, regional).

% Political movements or entities seeking to establish independent statehood. They must actively lobby for recognition, often making concessions or aligning with powerful states, and face the existential threat of non-recognition, which can delegitimize their claims and hinder their ability to govern or develop.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, aspiring_states, payer,
    powerless, generational, identity_locked, local).

% Bodies like the UN, World Bank, and IMF, whose membership and operational frameworks are largely predicated on the recognition of states. They benefit from the clarity and stability provided by the constitutive theory, as it defines their operational scope and legitimate interlocutors, even if it sometimes creates 'grey areas'.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, international_organizations, beneficiary,
    institutional, civilizational, constrained, global).

% Scholars and legal practitioners who argue that statehood is an objective fact, not dependent on recognition. Their arguments are often sidelined in practical international relations where recognition holds de facto power, despite its theoretical contestation.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, declaratory_theorists, excluded,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, albeit politically mediated, process for admitting new members to the international system, preventing chaotic proliferation of self-declared states and maintaining a stable order among existing powers.
% TRANSFER_FUNCTION: Transfers the power to define and legitimate statehood from objective criteria to the political will of existing states, effectively granting a veto over new entrants. This transfers sovereignty and access to international resources from unrecognized polities to the recognizing states.
% ABSENT_VOICES: Unrecognized polities and their populations, who are directly impacted by the denial of statehood and its associated rights and responsibilities. They would argue for self-determination and objective criteria, but their voices are often excluded from the recognition process itself.
% DISAPPEARANCE_RATIONALE: If the requirement for recognition vanished, numerous unrecognized polities would immediately claim full statehood, leading to a chaotic and contested international landscape. Existing states would lose their gatekeeping power, and the international system would need to rapidly re-evaluate its foundational principles and membership criteria.
% FOUNDING_PROBLEM: To prevent the arbitrary and destabilizing creation of new states, particularly in post-colonial or conflict-ridden contexts, and to ensure that new entities are viable and capable of fulfilling international obligations.
% FOUNDING_PROBLEM_CORROBORATION: Existing states and international organizations consistently cite the need for stability and responsible statehood as reasons for maintaining recognition as a prerequisite. While contested by unrecognized polities, the practical necessity of a gatekeeping mechanism is widely acknowledged by those within the established international order.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__constitutive_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__constitutive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__constitutive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(montevideo_statehood_criteria__constitutive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__constitutive_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__constitutive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.85) because the constraint imposes significant costs on unrecognized polities, denying them access to treaties, trade, and international aid, while benefiting existing states by preserving their exclusive club. Suppression is also high (0.9) as the international system actively enforces non-recognition through diplomatic and economic exclusion, with few viable alternatives for unrecognized entities. Theater ratio is low (0.2) because the recognition process, while sometimes performative, has very real and material consequences.
 *
 * PERSPECTIVAL GAP:
 *   Existing states experience this as a necessary coordination mechanism for international order, ensuring stability and responsible governance. Unrecognized polities, however, experience it as an arbitrary and highly extractive barrier to self-determination and development. The engine's per-seat classification should reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing states and international organizations are beneficiaries (d near 0.0) as they control the recognition process and maintain the existing order. Unrecognized polities and aspiring states are targets (d near 1.0) as they bear the full cost of non-recognition and have limited to no exit options from this structural trap.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing chaos, ensuring responsible statehood) is still live, but its application has become increasingly extractive. The constitutive reading allows existing powers to leverage the 'recognition' requirement to serve their own geopolitical interests, rather than solely for the collective good of international stability. This prevents mislabeling it as a pure coordination mechanism (Rope) when it clearly involves asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    recognition_as_political_tool,
    'To what extent is recognition used as a political tool by existing states to advance their own interests, rather than as an objective assessment of statehood criteria?',
    'Comparative analysis of recognition patterns across different geopolitical contexts, examining cases where objective criteria are met but recognition is withheld due to political considerations.',
    'If recognition is primarily a political tool, the constraint''s extractiveness is higher and its coordination function is more theatrical, pushing it further towards a Snare classification. If it''s consistently applied based on objective criteria, it leans more towards a Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(recognition_as_political_tool, empirical, 'Ambiguity in the political vs. objective nature of state recognition.').

omega_variable(
    legitimacy_of_non_recognition,
    'Is the denial of international legal personality to unrecognized polities a legitimate consequence of the international system, or an unjust suppression of self-determination?',
    'Analysis of international legal precedents and evolving norms regarding self-determination, human rights, and the right to development for stateless peoples.',
    'If deemed an unjust suppression, the constraint''s suppression metric is amplified, and the victim status of unrecognized polities is more pronounced. If deemed legitimate, the constraint''s classification might shift towards a more ''necessary evil'' Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_non_recognition, preference, 'Normative debate on the legitimacy of non-recognition.').

omega_variable(
    constitutive_vs_declaratory_primacy,
    'Which reading of statehood (constitutive or declaratory) better describes the de facto operation of the international system?',
    'Empirical study of how international law and state practice actually treat unrecognized entities in terms of treaty-making, diplomatic relations, and access to international forums.',
    'If the declaratory reading is found to be more accurate in practice, this constitutive reading would be reclassified as a Piton (theatrical maintenance of a non-functional rule) or a Snare (a cover story for political exclusion). If the constitutive reading holds, its Snare classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutive_vs_declaratory_primacy, empirical, 'Empirical primacy of constitutive vs. declaratory theories.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 1933, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1933, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1933, 0.3).
narrative_ontology:measurement(mont_tr_t1960, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1960, 0.25).
narrative_ontology:measurement(mont_tr_t1990, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(mont_tr_t2024, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(mont_be_t1933, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1933, 0.7).
narrative_ontology:measurement(mont_be_t1960, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1960, 0.75).
narrative_ontology:measurement(mont_be_t1990, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1990, 0.8).
narrative_ontology:measurement(mont_be_t2024, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 2024, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1933, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1933, 0.75).
narrative_ontology:measurement(mont_su_t1960, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1960, 0.8).
narrative_ontology:measurement(mont_su_t1990, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1990, 0.85).
narrative_ontology:measurement(mont_su_t2024, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 2024, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__constitutive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__hybrid_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, un_membership_criteria).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, international_treaty_access).

% DUAL FORMULATION NOTE:
% This constraint is the 'constitutive' reading of the Montevideo Statehood Criteria. Its extractiveness and suppression are higher than the 'declaratory' reading because it grants existing states a political veto, making recognition a condition for statehood rather than an acknowledgment of it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
