% ============================================================================
% CONSTRAINT STORY: westphalian_sovereignty__graduated_sovereignty
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_westphalian_sovereignty__graduated_sovereignty, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: westphalian_sovereignty__graduated_sovereignty
 *   human_readable: Graduated Sovereignty Doctrine
 *   domain: international_law/political_philosophy/global_governance
 *
 * SUMMARY:
 *   This constraint represents the 'graduated sovereignty' reading of the
 *   Westphalian sovereignty kernel, where a state's sovereignty is not
 *   absolute but exists on a spectrum determined by its capacity to govern
 *   and the perceived legitimacy of its governance. This reading emerged
 *   prominently in the post-Cold War era, justifying external intervention
 *   and conditional engagement with 'weak' or 'failing' states. It is claimed
 *   as a snare because its coordination function (addressing state failure)
 *   often serves as a cover for asymmetric extraction of discretion and
 *   influence by powerful states.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, 0.65).
domain_priors:suppression_score(westphalian_sovereignty__graduated_sovereignty, 0.75).
domain_priors:theater_ratio(westphalian_sovereignty__graduated_sovereignty, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, extractiveness, 0.65).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(westphalian_sovereignty__graduated_sovereignty, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(westphalian_sovereignty__graduated_sovereignty, snare).
narrative_ontology:human_readable(westphalian_sovereignty__graduated_sovereignty, "Graduated Sovereignty Doctrine").
narrative_ontology:topic_domain(westphalian_sovereignty__graduated_sovereignty, "international_law/political_philosophy/global_governance").

domain_priors:requires_active_enforcement(westphalian_sovereignty__graduated_sovereignty).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(westphalian_sovereignty__graduated_sovereignty, '6845897e-be3c-4aad-b036-d34415dc87e6').
narrative_ontology:cs_kernel_codification('6845897e-be3c-4aad-b036-d34415dc87e6', formalized).
narrative_ontology:cs_authority_grounding('6845897e-be3c-4aad-b036-d34415dc87e6', extraction).
narrative_ontology:cs_interpretation_layer_present('6845897e-be3c-4aad-b036-d34415dc87e6').
narrative_ontology:cs_reading_relation('6845897e-be3c-4aad-b036-d34415dc87e6', westphalian_sovereignty__absolute_sovereignty, forecloses).
narrative_ontology:cs_reading_relation('6845897e-be3c-4aad-b036-d34415dc87e6', westphalian_sovereignty__conditional_sovereignty, coexists_with).
narrative_ontology:cs_axiom('6845897e-be3c-4aad-b036-d34415dc87e6', foundational, state_capacity_is_measurable_and_determinative).
narrative_ontology:cs_axiom_status(state_capacity_is_measurable_and_determinative, holdable).
narrative_ontology:cs_axiom_grounding('6845897e-be3c-4aad-b036-d34415dc87e6', state_capacity_is_measurable_and_determinative, empirically_contingent).
narrative_ontology:cs_axiom('6845897e-be3c-4aad-b036-d34415dc87e6', foundational, governance_legitimacy_is_externally_assessable).
narrative_ontology:cs_axiom_status(governance_legitimacy_is_externally_assessable, holdable).
narrative_ontology:cs_axiom_grounding('6845897e-be3c-4aad-b036-d34415dc87e6', governance_legitimacy_is_externally_assessable, conventional).
narrative_ontology:cs_reference_frame('6845897e-be3c-4aad-b036-d34415dc87e6', post_cold_war_interventionism).
narrative_ontology:cs_drift_state('6845897e-be3c-4aad-b036-d34415dc87e6', contemporary_multipolar_world, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('6845897e-be3c-4aad-b036-d34415dc87e6', '').
narrative_ontology:cs_kernel_id(westphalian_sovereignty__graduated_sovereignty, westphalian_sovereignty).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, external_interveners).
narrative_ontology:constraint_beneficiary(westphalian_sovereignty__graduated_sovereignty, powerful_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, weak_states).
narrative_ontology:constraint_victim(westphalian_sovereignty__graduated_sovereignty, states_with_low_governance_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are states or coalitions that assert the right to assess other states' capacity and legitimacy, and to intervene based on those assessments. They gain discretion and access to resources/influence.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, external_interveners, agenda_setter,
    institutional, generational, arbitrage, global).

% States with significant military, economic, and diplomatic power who benefit from the flexibility to define and apply criteria for 'state capacity' and 'governance legitimacy' to other nations, often justifying their own foreign policy objectives.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, powerful_states, beneficiary,
    institutional, generational, arbitrage, global).

% States with limited military, economic, or institutional capacity that are subject to external classification and potential intervention. They bear the cost of lost autonomy and external oversight.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, weak_states, payer,
    powerless, generational, trapped, national).

% States whose internal governance structures or practices are deemed 'illegitimate' by external actors, leading to a loss of sovereign rights and increased vulnerability to intervention or conditional aid. Their identity as a 'legitimate' state is externally defined.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, states_with_low_governance_legitimacy, payer,
    powerless, generational, identity_locked, national).

% Bodies like the UN, World Bank, or IMF that develop and apply metrics for state capacity and governance, providing a framework and legitimacy for external assessments and interventions, often mediating between powerful and weak states.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, international_organizations, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(westphalian_sovereignty__graduated_sovereignty, international_organizations, observer).

% Non-governmental organizations and activists who champion human rights and often call for international intervention in cases of severe violations, sometimes inadvertently providing moral justification for the graduated sovereignty framework.
narrative_ontology:constraint_stakeholder(westphalian_sovereignty__graduated_sovereignty, human_rights_advocates, observer,
    organized, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(westphalian_sovereignty__graduated_sovereignty, powerful_states).
narrative_ontology:fixing_cost_class(westphalian_sovereignty__graduated_sovereignty, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Claims to coordinate international responses to state fragility, internal conflicts, or humanitarian crises by providing a framework for assessing states' capacity and legitimacy, thereby guiding intervention or support.
% TRANSFER_FUNCTION: Transfers discretion, authority, and often resources from states deemed 'weak' or 'illegitimate' to powerful states and international bodies, enabling external influence over domestic affairs.
% ABSENT_VOICES: States and populations that are subject to reclassification or intervention often have their voices marginalized or dismissed, as their 'lack of capacity' or 'illegitimacy' is used to justify overriding their consent or self-determination.
% DISAPPEARANCE_RATIONALE: If the doctrine of graduated sovereignty vanished, powerful states would lose a key justification for intervention and conditional engagement, forcing a fundamental renegotiation of international norms regarding non-interference and state autonomy. Weak states would regain formal equality, though material inequalities would persist.
% FOUNDING_PROBLEM: To address the perceived failures of absolute sovereignty in preventing humanitarian catastrophes, state collapse, and cross-border instability, particularly in the post-Cold War era, by allowing for differentiated international engagement based on state performance.
% FOUNDING_PROBLEM_CORROBORATION: Powerful states and some international organizations assert that the problem of state fragility and its consequences remains live. Many weak states, non-aligned movements, and critical scholars argue that the 'founding problem' is largely a pretext for neo-colonial intervention and power projection, with the original problem either solved or reframed to justify ongoing extraction.
narrative_ontology:disappearance_verdict(westphalian_sovereignty__graduated_sovereignty, world_rearranges).
narrative_ontology:founding_problem_status(westphalian_sovereignty__graduated_sovereignty, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(westphalian_sovereignty__graduated_sovereignty, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(westphalian_sovereignty__graduated_sovereignty, 'none', 1).
narrative_ontology:epsilon_provenance(westphalian_sovereignty__graduated_sovereignty, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(westphalian_sovereignty__graduated_sovereignty_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(westphalian_sovereignty__graduated_sovereignty, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(westphalian_sovereignty__graduated_sovereignty_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.65) reflects the significant loss of autonomy and resources by states subjected to this framework, as external actors gain the right to define their 'capacity' and 'legitimacy'. Suppression (0.75) is high due to the structural power imbalance: weak states have limited means to resist reclassification or intervention. The theater ratio (0.45) indicates that while some genuine capacity-building efforts exist, a substantial portion of the activity involves performative assessments and justifications for interventions that primarily serve the interests of external actors. The increasing trend in extractiveness and suppression over the interval reflects the hardening of interventionist doctrines.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of powerful states and interveners, this doctrine provides a necessary framework for global stability and human rights protection. From the perspective of weak or targeted states, it is a mechanism for neo-colonial control and the erosion of self-determination. The engine's classification as a snare captures this fundamental divergence, highlighting the extractive nature masked by the coordination narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   External interveners and powerful states are clear beneficiaries, gaining discretion and access (low directionality). Weak states and those deemed to have low governance legitimacy are the primary targets, bearing the costs of lost autonomy and external control (high directionality). International organizations play a dual role, both setting the agenda for assessment and mediating the impacts, placing them in a more constrained but still influential position.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    objectivity_of_capacity_assessment,
    'To what extent are ''state capacity'' and ''governance legitimacy'' objectively measurable criteria, versus being subject to political interpretation and bias by external actors?',
    'Comparative analysis of assessment methodologies across different international bodies and their application to states with varying geopolitical significance. If assessments correlate strongly with geopolitical interests rather than objective metrics, it supports the bias hypothesis.',
    'If assessments are primarily subjective and politically driven, the constraint''s effective extractiveness and suppression are higher, as the ''capacity'' narrative serves as a cover for power projection. If objective, the coordination function is more genuine.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(objectivity_of_capacity_assessment, empirical, 'Ambiguity in the objectivity of state capacity and legitimacy assessments.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (lack of military/economic power, international legal frameworks) or internalized (acceptance of international norms by elites in weak states, leading to self-regulation)?',
    'Post-intervention trajectory: if states continue to adhere to externally imposed norms even after direct intervention ceases, it suggests a degree of internalized suppression. Analysis of elite discourse in weak states for evidence of norm internalization.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as the target carries the suppression with them after direct external pressure is removed. This would make exit even harder.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in weak states.').

omega_variable(
    mandate_drift_vs_original_intent,
    'Has the application of graduated sovereignty drifted from its original intent of preventing humanitarian crises to primarily serving the geopolitical and economic interests of powerful states?',
    'Longitudinal study of intervention outcomes: comparing stated humanitarian goals with actual geopolitical and economic benefits accrued by intervening powers. Analysis of policy documents and diplomatic cables over time.',
    'If significant drift is confirmed, the constraint''s classification as a snare is strongly reinforced, indicating a clear shift from a coordination function to pure extraction. If the original intent remains dominant, it might lean closer to a tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandate_drift_vs_original_intent, empirical, 'Drift of graduated sovereignty''s mandate from humanitarian to geopolitical.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(westphalian_sovereignty__graduated_sovereignty, 1990, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(west_tr_t1990, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1990, 0.3).
narrative_ontology:measurement(west_tr_t1998, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 1998, 0.35).
narrative_ontology:measurement(west_tr_t2006, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2006, 0.4).
narrative_ontology:measurement(west_tr_t2014, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2014, 0.43).
narrative_ontology:measurement(west_tr_t2024, westphalian_sovereignty__graduated_sovereignty, theater_ratio, 2024, 0.45).

% Extraction over time
narrative_ontology:measurement(west_be_t1990, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(west_be_t1998, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 1998, 0.55).
narrative_ontology:measurement(west_be_t2006, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2006, 0.6).
narrative_ontology:measurement(west_be_t2014, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2014, 0.63).
narrative_ontology:measurement(west_be_t2024, westphalian_sovereignty__graduated_sovereignty, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(west_su_t1990, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(west_su_t1998, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 1998, 0.65).
narrative_ontology:measurement(west_su_t2006, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2006, 0.7).
narrative_ontology:measurement(west_su_t2014, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2014, 0.73).
narrative_ontology:measurement(west_su_t2024, westphalian_sovereignty__graduated_sovereignty, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(westphalian_sovereignty__graduated_sovereignty, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
