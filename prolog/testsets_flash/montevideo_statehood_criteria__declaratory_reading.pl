% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Montevideo Statehood Criteria (Declaratory Reading)
 *   domain: international_law/political_philosophy/state_theory
 *
 * SUMMARY:
 *   This constraint represents the 'declaratory' reading of the Montevideo
 *   Convention on the Rights and Duties of States (1933), which posits that
 *   statehood is an objective legal fact established by meeting four
 *   criteria: a permanent population, a defined territory, a government, and
 *   the capacity to enter into relations with other states. Recognition by
 *   other states is seen as merely 'declaratory' of an existing fact, not
 *   'constitutive' of statehood itself. This reading reduces the political
 *   leverage of existing states over aspiring ones.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.2).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.1).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, mountain).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Montevideo Statehood Criteria (Declaratory Reading)").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "international_law/political_philosophy/state_theory").

domain_priors:emerges_naturally(montevideo_statehood_criteria__declaratory_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, '3ef14a7a-ea2a-4405-9dad-fcabc4f883cd').
narrative_ontology:cs_kernel_codification('3ef14a7a-ea2a-4405-9dad-fcabc4f883cd', formalized).
narrative_ontology:cs_authority_grounding('3ef14a7a-ea2a-4405-9dad-fcabc4f883cd', lineage).
narrative_ontology:cs_interpretation_layer_present('3ef14a7a-ea2a-4405-9dad-fcabc4f883cd').
narrative_ontology:cs_reading_relation('3ef14a7a-ea2a-4405-9dad-fcabc4f883cd', montevideo_statehood_criteria__constitutive_reading, coexists_with).
narrative_ontology:cs_reading_relation('3ef14a7a-ea2a-4405-9dad-fcabc4f883cd', montevideo_statehood_criteria__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('3ef14a7a-ea2a-4405-9dad-fcabc4f883cd', foundational, statehood_is_an_objective_fact).
narrative_ontology:cs_axiom_status(statehood_is_an_objective_fact, holdable).
narrative_ontology:cs_axiom_grounding('3ef14a7a-ea2a-4405-9dad-fcabc4f883cd', statehood_is_an_objective_fact, deontological).
narrative_ontology:cs_axiom('3ef14a7a-ea2a-4405-9dad-fcabc4f883cd', secondary, recognition_is_merely_declaratory).
narrative_ontology:cs_axiom_status(recognition_is_merely_declaratory, holdable).
narrative_ontology:cs_axiom_grounding('3ef14a7a-ea2a-4405-9dad-fcabc4f883cd', recognition_is_merely_declaratory, conventional).
narrative_ontology:cs_reference_frame('3ef14a7a-ea2a-4405-9dad-fcabc4f883cd', rules_based_international_order).
narrative_ontology:cs_drift_state('3ef14a7a-ea2a-4405-9dad-fcabc4f883cd', contemporary_geopolitical_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('3ef14a7a-ea2a-4405-9dad-fcabc4f883cd', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, international_legal_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, existing_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, parent_states).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, self_determination_principle).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, rule_of_law_in_international_relations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the governing bodies of entities that meet the Montevideo criteria (permanent population, defined territory, government, capacity to enter into relations) but lack widespread international recognition. Under the declaratory reading, their statehood is a legal fact, strengthening their claim to sovereignty and reducing the leverage of states that deny recognition.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, de_facto_authorities, beneficiary,
    moderate, generational, constrained, local).

% Established states, particularly those with historical or geopolitical interests in denying recognition to new entities, find their leverage diminished by the declaratory reading. They are 'payers' in the sense that they lose the ability to condition statehood on political concessions or normative alignment, which they can do under a constitutive reading.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, existing_states, payer,
    institutional, civilizational, constrained, global).

% Scholars who advocate for a rules-based international order and the principle of self-determination benefit from the clarity and objectivity of the declaratory reading. It aligns with a view of international law as self-executing based on objective facts, rather than dependent on political consensus.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_legal_scholars, beneficiary,
    analytical, generational, analytical, global).

% States from which new entities have seceded or declared independence. The declaratory reading removes their structural leverage to condition the statehood of the new entity on their consent or political terms, as statehood is established by objective criteria alone.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, parent_states, payer,
    institutional, generational, constrained, national).

% Organizations like the UN, which must decide on membership and engage with new entities. The declaratory reading provides a clear, objective framework for assessing statehood, reducing the political discretion involved in admitting new members, though political considerations often still influence their actions.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_organizations, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides an objective, rules-based standard for determining when an entity qualifies as a state under international law, reducing arbitrary political decisions in recognizing new actors.
% TRANSFER_FUNCTION: Transfers the power to determine statehood from existing states (especially those with veto power or historical claims) to the objective fulfillment of criteria by the aspiring entity. It transfers legitimacy and legal standing to de facto authorities.
% ABSENT_VOICES: The 'constitutive' school of thought, which argues that recognition by existing states is essential for statehood, is structurally marginalized by the declaratory reading. They would argue that statehood is a political act, not merely a factual one, and that the community of states has a right to determine its membership.
% DISAPPEARANCE_RATIONALE: If the declaratory reading of the Montevideo criteria vanished, the process of state creation would become entirely political, dependent on the whims of powerful states. This would lead to greater instability, contested sovereignty claims, and a less predictable international legal order, as the objective basis for statehood would be lost.
% FOUNDING_PROBLEM: To establish a clear, objective, and depoliticized standard for statehood in international law, preventing powerful states from arbitrarily denying the existence of new states that meet basic factual requirements.
% FOUNDING_PROBLEM_CORROBORATION: International legal scholars and many de facto authorities attest that the problem of politicized recognition remains live, citing ongoing disputes over entities like Palestine, Kosovo, and Taiwan. While some established states might prefer a more politically flexible approach, the need for objective criteria is widely acknowledged by independent legal experts and advocates for self-determination.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__declaratory_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__declaratory_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__declaratory_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(montevideo_statehood_criteria__declaratory_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, ExtMetricName, E),
    domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(montevideo_statehood_criteria__declaratory_reading),
    narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The declaratory reading is claimed as a Mountain because it asserts statehood as an objective legal fact, independent of political will. Its extractiveness (0.2) is low, reflecting that it primarily removes the power of existing states to deny statehood, rather than actively extracting from new entities. Suppression (0.1) is minimal, as its force comes from legal principle rather than coercion. Theater ratio (0.05) is low, as the criteria are generally applied functionally. Accessibility collapse (0.9) is high because if an entity meets these criteria, its statehood is legally established, leaving little room for alternative interpretations of its status. Resistance (0.05) is low because the resistance is against the *application* of the reading (e.g., denying a de facto state meets the criteria), not against the principle itself.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of de facto authorities, this is a beneficial, objective standard that grants them legal standing. From the perspective of existing states (especially parent states or those with geopolitical interests), it is a constraint that limits their political discretion and leverage, effectively 'extracting' their power to condition statehood.
 *
 * DIRECTIONALITY LOGIC:
 *   De facto authorities and international legal scholars are beneficiaries (d near 0.0) as the reading strengthens their claims and aligns with their principles. Existing states and parent states are payers (d near 1.0) as they lose structural leverage and political control over the creation of new states. International organizations act as agenda-setters, applying the criteria, but their discretion is constrained by the objective nature of the reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The declaratory reading's mandate remains live: it continues to address the problem of politicized state recognition. Its persistence is not due to inertia but to its ongoing function in providing an objective legal framework. The low extractiveness and theater ratio, coupled with a live founding problem, indicate it is not a Piton or a Snare, but a Mountain (or a Rope if viewed as a coordination mechanism among legal scholars). The presence of beneficiaries on a claimed Mountain triggers FSM analysis, which is appropriate given the contestable 'naturalness' of legal principles.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_legal_construct,
    'Is the declaratory reading of statehood a ''natural law'' of international relations, or a legal construct that benefits identifiable agents (de facto authorities)?',
    'Analysis of historical state formation patterns prior to the Montevideo Convention: if similar objective criteria were implicitly applied, it supports natural law; if statehood was purely a matter of political recognition, it supports legal construct.',
    'If a natural law, its Mountain classification is robust. If a legal construct, the presence of beneficiaries (de facto authorities) would push it towards a ''false summit'' classification, likely a Tangled Rope, as it coordinates legal recognition while extracting political leverage from existing states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_legal_construct, conceptual, 'Ambiguity between inherent legal principle and beneficial legal construct.').

omega_variable(
    objective_criteria_application,
    'How objectively and consistently are the four Montevideo criteria actually applied in practice, especially the ''capacity to enter into relations''?',
    'Empirical study of recognition practices by states and international organizations: if political considerations consistently override objective assessment, the ''declaratory'' aspect is weakened.',
    'If application is highly subjective and politicized, the constraint''s effective extractiveness (from existing states) and suppression (of de facto authorities'' claims) would be higher than measured, pushing it towards a Snare or Tangled Rope, as the ''objective'' cover story would mask political discretion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objective_criteria_application, empirical, 'Gap between claimed objectivity and actual application of criteria.').

omega_variable(
    declaratory_constitutive_boundary,
    'Is the distinction between ''declaratory'' and ''constitutive'' statehood a clear, structural boundary, or a conceptual framing that shifts with political context?',
    'Analysis of legal arguments in cases of contested statehood: if courts consistently adhere to one reading over the other, the boundary is clearer. If arguments fluidly switch between readings based on political expediency, it''s a conceptual framing.',
    'If a clear structural boundary, the distinct classifications of the readings are robust. If a fluid conceptual framing, the ''declaratory'' reading''s Mountain classification is less stable, as its ''naturalness'' is undermined by its fungibility with other framings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(declaratory_constitutive_boundary, conceptual, 'Clarity of the declaratory vs. constitutive distinction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 1933, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mont_tr_t1933, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1933, 0.03).
narrative_ontology:measurement(mont_tr_t1960, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1960, 0.04).
narrative_ontology:measurement(mont_tr_t1990, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 1990, 0.05).
narrative_ontology:measurement(mont_tr_t2024, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(mont_be_t1933, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1933, 0.15).
narrative_ontology:measurement(mont_be_t1960, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1960, 0.18).
narrative_ontology:measurement(mont_be_t1990, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 1990, 0.2).
narrative_ontology:measurement(mont_be_t2024, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(mont_su_t1933, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1933, 0.08).
narrative_ontology:measurement(mont_su_t1960, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1960, 0.09).
narrative_ontology:measurement(mont_su_t1990, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 1990, 0.1).
narrative_ontology:measurement(mont_su_t2024, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
