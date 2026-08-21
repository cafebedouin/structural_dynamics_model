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
 *   constraint_id: montevideo_statehood_criteria__constitutive_reading
 *   human_readable: Statehood Requires Constitutive Recognition
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'constitutive' reading of statehood, where
 *   an entity's status as a state in international law is contingent upon its
 *   recognition by existing states. This reading grants significant power to
 *   the established community of states to control entry into the
 *   international system. Unrecognized polities, despite often meeting
 *   objective criteria for statehood, face severe limitations and extraction
 *   due to the denial of recognition. The constraint is claimed as a 'snare'
 *   due to its high extractiveness and suppression, which are actively
 *   enforced to maintain the existing international order.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__constitutive_reading, 0.85).
domain_priors:suppression_score(montevideo_statehood_criteria__constitutive_reading, 0.9).
domain_priors:theater_ratio(montevideo_statehood_criteria__constitutive_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__constitutive_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__constitutive_reading, snare).
narrative_ontology:human_readable(montevideo_statehood_criteria__constitutive_reading, "Statehood Requires Constitutive Recognition").
narrative_ontology:topic_domain(montevideo_statehood_criteria__constitutive_reading, "international_law/political_philosophy/state_theory").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__constitutive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__constitutive_reading, '4d51f304-044c-4772-8122-44766fd29edb').
narrative_ontology:cs_kernel_codification('4d51f304-044c-4772-8122-44766fd29edb', formalized).
narrative_ontology:cs_authority_grounding('4d51f304-044c-4772-8122-44766fd29edb', lineage).
narrative_ontology:cs_interpretation_layer_present('4d51f304-044c-4772-8122-44766fd29edb').
narrative_ontology:cs_reading_relation('4d51f304-044c-4772-8122-44766fd29edb', montevideo_statehood_criteria__declaratory_reading, forecloses).
narrative_ontology:cs_reading_relation('4d51f304-044c-4772-8122-44766fd29edb', montevideo_statehood_criteria__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('4d51f304-044c-4772-8122-44766fd29edb', foundational, recognition_is_a_prerequisite_for_statehood).
narrative_ontology:cs_axiom_status(recognition_is_a_prerequisite_for_statehood, holdable).
narrative_ontology:cs_axiom_grounding('4d51f304-044c-4772-8122-44766fd29edb', recognition_is_a_prerequisite_for_statehood, conventional).
narrative_ontology:cs_axiom('4d51f304-044c-4772-8122-44766fd29edb', secondary, international_order_requires_gatekeeping).
narrative_ontology:cs_axiom_status(international_order_requires_gatekeeping, holdable).
narrative_ontology:cs_axiom_grounding('4d51f304-044c-4772-8122-44766fd29edb', international_order_requires_gatekeeping, instrumental).
narrative_ontology:cs_reference_frame('4d51f304-044c-4772-8122-44766fd29edb', westphalian_order_of_states).
narrative_ontology:cs_drift_state('4d51f304-044c-4772-8122-44766fd29edb', post_cold_war_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('4d51f304-044c-4772-8122-44766fd29edb', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, existing_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__constitutive_reading, established_international_organizations).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__constitutive_reading, populations_in_unrecognized_territories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the established members of the international community, they collectively hold the power to grant or deny recognition to new entities claiming statehood. This power allows them to control entry into international forums, treaty regimes, and economic systems, maintaining the existing international order.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, existing_states, agenda_setter,
    institutional, generational, arbitrage, global).

% These entities meet many objective criteria for statehood (territory, population, government, capacity to enter relations) but are denied full international legal personality due to lack of recognition. They face severe limitations in diplomatic relations, international trade, access to aid, and security guarantees, bearing the direct costs of non-recognition.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, unrecognized_polities, payer,
    powerless, generational, trapped, regional).

% Organizations like the UN, World Bank, and IMF operate based on the recognition of states. The constitutive reading provides a clear, albeit gatekept, framework for their membership and operations, ensuring stability and predictability in their interactions with sovereign entities. They benefit from the controlled entry of new members.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, established_international_organizations, beneficiary,
    institutional, generational, constrained, global).

% These populations often lack full access to international human rights protections, humanitarian aid, and the benefits of international law due to their polity's unrecognized status. Their identity is often tied to the aspiration of statehood, making exit from the struggle for recognition unthinkable, despite the severe costs.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, populations_in_unrecognized_territories, payer,
    powerless, generational, identity_locked, local).

% Scholars and legal experts who argue that statehood arises automatically once objective criteria are met, regardless of recognition. They critically observe the constitutive reading's operation, highlighting its potential for political manipulation and denial of self-determination.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, declaratory_theorists, observer,
    analytical, biographical, analytical, global).

% Scholars who advocate for a nuanced approach, suggesting statehood requires objective criteria, recognition, and adherence to normative principles like democracy and human rights. They observe how the constitutive reading can be applied without these normative considerations.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__constitutive_reading, hybrid_theorists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__constitutive_reading, existing_states).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__constitutive_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a controlled and orderly process for admitting new members into the international system, preventing chaotic proliferation of state claims and maintaining the stability of existing international legal and political structures.
% TRANSFER_FUNCTION: Transfers legitimacy, diplomatic rights, economic access, and security guarantees from the existing community of states to entities it chooses to recognize, while denying these critical benefits to unrecognized polities.
% ABSENT_VOICES: Unrecognized polities and their populations are largely excluded from the international forums where their status is debated and determined. They would advocate for self-determination and automatic recognition based on objective criteria, challenging the existing states' gatekeeping power.
% DISAPPEARANCE_RATIONALE: If recognition by existing states were no longer required for statehood, numerous unrecognized entities (e.g., Kosovo, Palestine, Somaliland, Taiwan) would immediately claim full sovereign rights. This would lead to a fundamental reordering of international borders, diplomatic relations, and global governance, potentially causing widespread instability and conflict over territorial claims and resource access.
% FOUNDING_PROBLEM: To prevent arbitrary and destabilizing claims of statehood, particularly in the aftermath of colonial empires and conflicts, ensuring a stable and predictable international order where new entities are integrated in a controlled manner.
% FOUNDING_PROBLEM_CORROBORATION: Existing states and many traditional international legal scholars attest to the ongoing need for order and stability in the international system. However, unrecognized polities and some critical international legal scholars argue that the founding problem is now largely used as a pretext to maintain power imbalances, citing selective recognition practices and the denial of self-determination.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__constitutive_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__constitutive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__constitutive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(montevideo_statehood_criteria__constitutive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__constitutive_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.85) because unrecognized polities are systematically denied access to critical international benefits (treaties, aid, diplomatic relations) that are essential for their development and security. Suppression is also very high (0.90) as existing states actively coordinate to deny recognition, and there are virtually no alternative paths to full international legal personality. The theater ratio is low (0.10) because the denial of recognition is a very real and consequential act, not merely performative. Accessibility collapse is near total (0.95) as the international system offers almost no viable alternatives to recognition for achieving full statehood. Resistance is high (0.70) as unrecognized polities continuously lobby, negotiate, and sometimes fight for recognition.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of existing states, this constraint is a necessary mechanism for maintaining international order and stability, ensuring that new states are viable and responsible actors. From the perspective of unrecognized polities, it is a deeply extractive and suppressive mechanism that denies their right to self-determination and perpetuates their marginalization.
 *
 * DIRECTIONALITY LOGIC:
 *   Existing states and established international organizations are clear beneficiaries (low directionality) as they control the system and benefit from its stability. Unrecognized polities and their populations are clear targets (high directionality) as they bear the costs of exclusion and denial of rights. Analytical observers (theorists) have an analytical directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constitutive reading's mandate to maintain international order is contested. While it genuinely solved problems of chaotic state formation in the past, its persistence is increasingly seen by some as a mechanism for established powers to maintain their geopolitical advantage, rather than solely for global stability. The high extractiveness and suppression, coupled with ongoing resistance, suggest a potential for mandatrophy where the original coordination function has been overshadowed by rent-seeking and power maintenance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    constitutive_vs_declaratory_nature,
    'Is statehood fundamentally constituted by recognition, or is it merely declared upon meeting objective criteria?',
    'A shift in international legal consensus or a series of landmark international court rulings that definitively prioritize one theory over the other in practice.',
    'If resolved as purely declaratory, the power of existing states to deny statehood would diminish, reclassifying this constraint as a ''piton'' or ''rope'' with significantly lower extraction. If resolved as purely constitutive, the current ''snare'' classification would be reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutive_vs_declaratory_nature, conceptual, 'The fundamental nature of statehood in international law.').

omega_variable(
    legitimacy_vs_power_dynamics,
    'To what extent is the denial of recognition based on legitimate concerns for international order versus geopolitical power dynamics and self-interest of existing states?',
    'Independent, transparent assessments of unrecognized polities'' objective statehood criteria and their adherence to international norms, free from political influence, leading to consistent recognition decisions.',
    'If primarily driven by power, the ''snare'' classification is accurate. If genuinely driven by legitimate concerns for order, the constraint might lean more towards a ''tangled_rope'' or even ''rope'' if extraction were lower and more symmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_vs_power_dynamics, empirical, 'The underlying motivations for granting or denying state recognition.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__constitutive_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1945, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1945, 0.15).
narrative_ontology:measurement(mont_tr_t1960, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1960, 0.13).
narrative_ontology:measurement(mont_tr_t1975, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1975, 0.12).
narrative_ontology:measurement(mont_tr_t1990, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 1990, 0.11).
narrative_ontology:measurement(mont_tr_t2005, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(mont_tr_t2025, montevideo_statehood_criteria__constitutive_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(mont_be_t1945, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1945, 0.75).
narrative_ontology:measurement(mont_be_t1960, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1960, 0.78).
narrative_ontology:measurement(mont_be_t1975, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1975, 0.8).
narrative_ontology:measurement(mont_be_t1990, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 1990, 0.82).
narrative_ontology:measurement(mont_be_t2005, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 2005, 0.84).
narrative_ontology:measurement(mont_be_t2025, montevideo_statehood_criteria__constitutive_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1945, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1945, 0.8).
narrative_ontology:measurement(mont_su_t1960, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1960, 0.83).
narrative_ontology:measurement(mont_su_t1975, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1975, 0.86).
narrative_ontology:measurement(mont_su_t1990, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 1990, 0.88).
narrative_ontology:measurement(mont_su_t2005, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 2005, 0.89).
narrative_ontology:measurement(mont_su_t2025, montevideo_statehood_criteria__constitutive_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__constitutive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__declaratory_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__constitutive_reading, montevideo_statehood_criteria__hybrid_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Montevideo Statehood Criteria kernel. The constitutive reading emphasizes recognition by existing states as a prerequisite for statehood, contrasting with the declaratory reading (objective criteria suffice) and the hybrid reading (objective criteria + normative legitimacy).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
