% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__hybrid_reading, []).

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
 *   constraint_id: montevideo_statehood_criteria__hybrid_reading
 *   human_readable: Hybrid Statehood Criteria (Normative Legitimacy Reading)
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid reading' of the Montevideo
 *   Statehood Criteria, which asserts that in addition to objective criteria
 *   (territory, population, government, capacity to enter relations),
 *   statehood also requires normative legitimacy, such as democratic
 *   governance, respect for human rights, and non-aggression. This reading
 *   emerged prominently post-Cold War, providing a basis for conditional
 *   recognition and intervention. It functions as a Tangled Rope,
 *   coordinating the international community around certain values while
 *   extracting sovereignty and legitimacy from entities that do not conform.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, 0.8).
domain_priors:suppression_score(montevideo_statehood_criteria__hybrid_reading, 0.75).
domain_priors:theater_ratio(montevideo_statehood_criteria__hybrid_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__hybrid_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__hybrid_reading, "Hybrid Statehood Criteria (Normative Legitimacy Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__hybrid_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__hybrid_reading, '19a3ba3c-c95d-402c-abe2-b6aaf83e31d4').
narrative_ontology:cs_kernel_codification('19a3ba3c-c95d-402c-abe2-b6aaf83e31d4', formalized).
narrative_ontology:cs_authority_grounding('19a3ba3c-c95d-402c-abe2-b6aaf83e31d4', lineage).
narrative_ontology:cs_interpretation_layer_present('19a3ba3c-c95d-402c-abe2-b6aaf83e31d4').
narrative_ontology:cs_reading_relation('19a3ba3c-c95d-402c-abe2-b6aaf83e31d4', montevideo_statehood_criteria__declaratory_reading, influences).
narrative_ontology:cs_reading_relation('19a3ba3c-c95d-402c-abe2-b6aaf83e31d4', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_axiom('19a3ba3c-c95d-402c-abe2-b6aaf83e31d4', foundational, democratic_legitimacy_required).
narrative_ontology:cs_axiom_status(democratic_legitimacy_required, holdable).
narrative_ontology:cs_axiom_grounding('19a3ba3c-c95d-402c-abe2-b6aaf83e31d4', democratic_legitimacy_required, deontological).
narrative_ontology:cs_axiom('19a3ba3c-c95d-402c-abe2-b6aaf83e31d4', foundational, human_rights_compliance_mandated).
narrative_ontology:cs_axiom_status(human_rights_compliance_mandated, holdable).
narrative_ontology:cs_axiom_grounding('19a3ba3c-c95d-402c-abe2-b6aaf83e31d4', human_rights_compliance_mandated, deontological).
narrative_ontology:cs_reference_frame('19a3ba3c-c95d-402c-abe2-b6aaf83e31d4', post_cold_war_liberal_order).
narrative_ontology:cs_drift_state('19a3ba3c-c95d-402c-abe2-b6aaf83e31d4', contemporary_geopolitical_contestation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('19a3ba3c-c95d-402c-abe2-b6aaf83e31d4', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__hybrid_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, international_human_rights_organizations).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__hybrid_reading, interventionist_powers).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionists).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, authoritarian_regimes).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, states_accused_of_human_rights_abuses).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__hybrid_reading, declaratory_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states champion and often enforce the normative criteria for statehood, using them to justify recognition or non-recognition, and sometimes intervention. They benefit from an international order aligned with their values.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, liberal_democratic_states, agenda_setter,
    institutional, generational, arbitrage, global).

% Aspiring state entities that meet objective criteria (territory, population, government) but do not adhere to democratic governance or human rights standards. They face denial of recognition and potential international isolation, making statehood effectively impossible.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, non_liberal_secessionists, payer,
    powerless, generational, trapped, local).

% Existing states that are challenged on their legitimacy due to lack of democratic governance or human rights abuses. They face sanctions, diplomatic pressure, and the threat of intervention, undermining their sovereignty and international standing.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, authoritarian_regimes, payer,
    powerful, biographical, constrained, national).

% These organizations advocate for the application of human rights and democratic norms in statehood and international relations. This reading provides a legal and normative basis for their advocacy and for holding states accountable.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, international_human_rights_organizations, beneficiary,
    organized, generational, analytical, global).

% States or coalitions that are willing to use military or economic force to uphold democratic governance and human rights. This reading provides a legal and moral justification for their actions, expanding their sphere of influence.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, interventionist_powers, agenda_setter,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__hybrid_reading, interventionist_powers, beneficiary).

% Legal scholars and practitioners who analyze the evolution and application of international law, including statehood criteria. They observe the contestation and implications of this hybrid reading.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, traditional_international_lawyers, observer,
    analytical, generational, analytical, global).

% States that primarily adhere to the declaratory theory of statehood, recognizing entities based purely on objective criteria. They may face pressure or criticism from proponents of the hybrid reading for recognizing non-normative entities, complicating their foreign policy.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__hybrid_reading, declaratory_states, payer,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for the international community to assess and recognize new states, aiming to align statehood with shared normative values like democracy and human rights, thereby fostering a more stable and just international order.
% TRANSFER_FUNCTION: Transfers legitimacy and sovereignty from entities that fail to meet normative criteria to those that uphold them, or to the international community that enforces these norms. It also transfers the burden of proof for legitimacy onto aspiring states.
% ABSENT_VOICES: Non-liberal political movements, indigenous groups seeking self-determination without adopting Western democratic models, and states prioritizing absolute sovereignty over interventionist human rights claims. They would argue for a more pluralistic or purely objective definition of statehood, free from normative conditions imposed by powerful states.
% DISAPPEARANCE_RATIONALE: If this hybrid reading vanished overnight, the international system would lose a key justification for denying recognition or intervening in states based on normative grounds. Statehood would revert to purely objective or recognition-based criteria, fundamentally altering geopolitical dynamics, the scope of international law, and the leverage of liberal states.
% FOUNDING_PROBLEM: The perceived inadequacy of purely objective or recognition-based statehood criteria to address human rights abuses, democratic deficits, and aggressive behavior by states, particularly after the World Wars and the rise of human rights discourse, leading to a desire for a more morally grounded international system.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies, liberal democratic governments, and many international relations scholars attest to the ongoing relevance of these normative concerns. Critics (e.g., some post-colonial scholars, realists) argue the problem is often a pretext for power projection, but the normative justification remains widely asserted by its proponents.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__hybrid_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__hybrid_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(montevideo_statehood_criteria__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__hybrid_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high because this reading imposes significant normative hurdles for statehood, effectively denying full sovereignty to non-compliant entities. Suppression is also high, as non-recognition, sanctions, and even military intervention are used to enforce these norms. The theater ratio is moderate, reflecting genuine normative intent alongside strategic application by powerful states. The metrics show a general increase in extractiveness and suppression as this reading gained prominence, with some fluctuations due to geopolitical shifts.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of liberal democratic states and human rights organizations, this reading is a legitimate evolution of international law, promoting universal values. From the perspective of non-liberal secessionists or authoritarian regimes, it is an extractive tool used by powerful states to impose their political systems and undermine sovereignty.
 *
 * DIRECTIONALITY LOGIC:
 *   Liberal democratic states and interventionist powers are beneficiaries and agenda-setters, as they define and enforce these norms, gaining moral and political leverage. International human rights organizations also benefit by having a legal framework for their advocacy. Non-liberal secessionists and authoritarian regimes are clear victims, facing denial of legitimacy and various forms of suppression. Declaratory states, while powerful, become payers if they adhere to a purely objective view and face pressure to conform to the hybrid reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    consistent_application_vs_selectivity,
    'Is the hybrid reading of statehood criteria applied consistently across all cases, or is its application selective, influenced by geopolitical interests?',
    'Empirical analysis of recognition patterns, intervention decisions, and diplomatic pressure over time, comparing cases where normative criteria were invoked versus those where they were overlooked.',
    'If application is found to be highly selective, the effective extraction from targeted entities is amplified, and the constraint''s legitimacy as a universal norm is undermined, suggesting a stronger ''snare'' component.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consistent_application_vs_selectivity, empirical, 'Assesses the consistency and impartiality of the hybrid reading''s application in practice.').

omega_variable(
    universal_norm_vs_cultural_imposition,
    'Are the normative criteria (democracy, human rights) universally accepted as prerequisites for statehood, or are they a cultural imposition by a subset of powerful states?',
    'Cross-cultural and historical analysis of state formation and international legal theory, examining the degree of consensus on these norms outside Western liberal traditions.',
    'If the norms are primarily a cultural imposition, the constraint''s claim to coordination is weakened, and its extractive function (justifying intervention/non-recognition) is amplified, pushing it closer to a ''snare'' for non-conforming cultures.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(universal_norm_vs_cultural_imposition, conceptual, 'Examines the universality versus cultural specificity of the normative statehood criteria.').

omega_variable(
    normative_criteria_primacy,
    'To what extent have the normative criteria superseded or become more important than the objective criteria for statehood in practice?',
    'Legal and political analysis of cases where entities meeting objective criteria were denied statehood due to normative deficiencies, or vice versa, over time.',
    'If normative criteria consistently override objective ones, the constraint''s ''tangled_rope'' nature is reinforced, as the coordination function (objective criteria) is increasingly overshadowed by the extractive function (normative enforcement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_criteria_primacy, conceptual, 'Determines the relative weight of normative versus objective criteria in the hybrid reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__hybrid_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1990, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(mont_tr_t1995, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(mont_tr_t2000, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2000, 0.4).
narrative_ontology:measurement(mont_tr_t2005, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2005, 0.42).
narrative_ontology:measurement(mont_tr_t2010, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2010, 0.4).
narrative_ontology:measurement(mont_tr_t2015, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2015, 0.38).
narrative_ontology:measurement(mont_tr_t2020, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2020, 0.41).
narrative_ontology:measurement(mont_tr_t2025, montevideo_statehood_criteria__hybrid_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(mont_be_t1990, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(mont_be_t1995, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 1995, 0.7).
narrative_ontology:measurement(mont_be_t2000, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(mont_be_t2005, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2005, 0.78).
narrative_ontology:measurement(mont_be_t2010, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2010, 0.8).
narrative_ontology:measurement(mont_be_t2015, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2015, 0.79).
narrative_ontology:measurement(mont_be_t2020, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2020, 0.81).
narrative_ontology:measurement(mont_be_t2025, montevideo_statehood_criteria__hybrid_reading, base_extractiveness, 2025, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1990, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(mont_su_t1995, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 1995, 0.65).
narrative_ontology:measurement(mont_su_t2000, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(mont_su_t2005, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2005, 0.73).
narrative_ontology:measurement(mont_su_t2010, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(mont_su_t2015, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2015, 0.74).
narrative_ontology:measurement(mont_su_t2020, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2020, 0.76).
narrative_ontology:measurement(mont_su_t2025, montevideo_statehood_criteria__hybrid_reading, suppression_requirement, 2025, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, humanitarian_intervention_doctrine).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, responsibility_to_protect_doctrine).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__hybrid_reading, international_recognition_of_secession).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
