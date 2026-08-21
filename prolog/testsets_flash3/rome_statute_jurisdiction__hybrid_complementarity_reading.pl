% ============================================================================
% CONSTRAINT STORY: rome_statute_jurisdiction__hybrid_complementarity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rome_statute_jurisdiction__hybrid_complementarity_reading, []).

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
 *   constraint_id: rome_statute_jurisdiction__hybrid_complementarity_reading
 *   human_readable: Rome Statute Jurisdiction: Hybrid Complementarity Reading
 *   domain: international_law/treaty_interpretation/institutional_authority
 *
 * SUMMARY:
 *   This constraint represents the 'hybrid complementarity' reading of the
 *   Rome Statute's jurisdictional framework. It acknowledges the ICC's
 *   residual universal authority to prosecute atrocity crimes, but emphasizes
 *   that this authority is operationally constrained by the principle of
 *   complementarity, which defers to genuine national proceedings.
 *   Jurisdiction exists, but its enforcement is heavily dependent on state
 *   cooperation. This reading grounds the ICC's authority in a blend of
 *   natural law aspirations for justice and the consensual nature of treaty
 *   law.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.45).
domain_priors:suppression_score(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.3).
domain_priors:theater_ratio(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rome_statute_jurisdiction__hybrid_complementarity_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rome_statute_jurisdiction__hybrid_complementarity_reading, tangled_rope).
narrative_ontology:human_readable(rome_statute_jurisdiction__hybrid_complementarity_reading, "Rome Statute Jurisdiction: Hybrid Complementarity Reading").
narrative_ontology:topic_domain(rome_statute_jurisdiction__hybrid_complementarity_reading, "international_law/treaty_interpretation/institutional_authority").

domain_priors:requires_active_enforcement(rome_statute_jurisdiction__hybrid_complementarity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rome_statute_jurisdiction__hybrid_complementarity_reading, 'f143819e-b502-4e14-a177-5e0af306e709').
narrative_ontology:cs_kernel_codification('f143819e-b502-4e14-a177-5e0af306e709', formalized).
narrative_ontology:cs_authority_grounding('f143819e-b502-4e14-a177-5e0af306e709', lineage).
narrative_ontology:cs_interpretation_layer_present('f143819e-b502-4e14-a177-5e0af306e709').
narrative_ontology:cs_reading_relation('f143819e-b502-4e14-a177-5e0af306e709', rome_statute_jurisdiction__universalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('f143819e-b502-4e14-a177-5e0af306e709', rome_statute_jurisdiction__sovereigntist_reading, coexists_with).
narrative_ontology:cs_axiom('f143819e-b502-4e14-a177-5e0af306e709', foundational, complementarity_as_primary_jurisdiction).
narrative_ontology:cs_axiom_status(complementarity_as_primary_jurisdiction, holdable).
narrative_ontology:cs_axiom_grounding('f143819e-b502-4e14-a177-5e0af306e709', complementarity_as_primary_jurisdiction, conventional).
narrative_ontology:cs_axiom('f143819e-b502-4e14-a177-5e0af306e709', foundational, residual_icc_jurisdiction_for_impunity).
narrative_ontology:cs_axiom_status(residual_icc_jurisdiction_for_impunity, holdable).
narrative_ontology:cs_axiom_grounding('f143819e-b502-4e14-a177-5e0af306e709', residual_icc_jurisdiction_for_impunity, deontological).
narrative_ontology:cs_reference_frame('f143819e-b502-4e14-a177-5e0af306e709', balanced_sovereignty_and_justice).
narrative_ontology:cs_drift_state('f143819e-b502-4e14-a177-5e0af306e709', contemporary_geopolitical_challenges, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('f143819e-b502-4e14-a177-5e0af306e709', '').
narrative_ontology:cs_kernel_id(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_court).
narrative_ontology:constraint_beneficiary(rome_statute_jurisdiction__hybrid_complementarity_reading, victims_of_atrocity_crimes).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, sovereign_states_under_investigation).
narrative_ontology:constraint_victim(rome_statute_jurisdiction__hybrid_complementarity_reading, accused_individuals).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, principle_of_complementarity).
narrative_ontology:constraint_vindicates(rome_statute_jurisdiction__hybrid_complementarity_reading, international_rule_of_law).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary institution responsible for investigating and prosecuting individuals for atrocity crimes. Its jurisdiction is activated when national courts are unwilling or unable to do so, balancing universal justice with state sovereignty. It benefits from the legitimacy conferred by the Statute but is constrained by state cooperation for enforcement.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_court, agenda_setter,
    institutional, generational, constrained, global).

% Receive a pathway to justice when national systems fail. The ICC offers a forum for accountability and recognition of their suffering, providing a measure of redress. Their benefit is contingent on the ICC's ability to exercise jurisdiction and enforce its decisions.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, victims_of_atrocity_crimes, beneficiary,
    powerless, biographical, trapped, local).

% Bear the cost of potential loss of sovereignty and international scrutiny when the ICC asserts jurisdiction. They are compelled to demonstrate genuine national proceedings or face ICC intervention. Their options are to cooperate, resist, or withdraw from the Statute, each with significant political and diplomatic costs.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, sovereign_states_under_investigation, payer,
    powerful, generational, constrained, national).

% Face prosecution and potential imprisonment under international law. They are direct targets of the ICC's enforcement actions, with limited recourse once jurisdiction is established and warrants are issued. Their costs are direct and severe.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, accused_individuals, payer,
    powerless, immediate, trapped, local).

% States that have not ratified the Rome Statute. They are not directly bound by its jurisdiction but may face political pressure or ad hoc cooperation requests. Their exclusion from the treaty framework means they are not subject to its direct enforcement mechanisms, but their actions may still be scrutinized.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, non_state_parties, excluded,
    institutional, generational, mobile, global).

% Analyze the evolving interpretation and application of the Rome Statute, particularly the principle of complementarity. They assess its effectiveness, legitimacy, and impact on international law and state sovereignty, contributing to the conceptual contestation.
narrative_ontology:constraint_stakeholder(rome_statute_jurisdiction__hybrid_complementarity_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates international efforts to prosecute individuals for the most serious international crimes (genocide, crimes against humanity, war crimes, crime of aggression) when national jurisdictions are unwilling or unable to do so, preventing impunity.
% TRANSFER_FUNCTION: Transfers jurisdiction over atrocity crimes from national courts to the International Criminal Court under specific conditions, moving accountability from sovereign states to an international body, and transferring the burden of prosecution from victims to the international community.
% ABSENT_VOICES: States that actively resist ICC jurisdiction, particularly those not party to the Rome Statute, are absent from the direct legal framework but exert significant political and diplomatic pressure. They would argue for absolute sovereign primacy and against any form of external judicial intervention.
% DISAPPEARANCE_RATIONALE: If the Rome Statute and its complementarity mechanism vanished, the international legal landscape for atrocity crimes would revert to ad hoc tribunals or purely national prosecutions, leading to greater impunity and a significant setback for international criminal justice. Victims would lose a crucial avenue for redress.
% FOUNDING_PROBLEM: The problem of impunity for atrocity crimes, where national courts either could not or would not prosecute, leading to a cycle of violence and injustice.
% FOUNDING_PROBLEM_CORROBORATION: Human rights organizations, UN bodies, and numerous international legal experts corroborate that the problem of impunity remains live, citing ongoing conflicts and the persistent failure of national systems to deliver justice. While progress has been made, the need for an international backstop persists.
narrative_ontology:disappearance_verdict(rome_statute_jurisdiction__hybrid_complementarity_reading, world_rearranges).
narrative_ontology:founding_problem_status(rome_statute_jurisdiction__hybrid_complementarity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(rome_statute_jurisdiction__hybrid_complementarity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rome_statute_jurisdiction__hybrid_complementarity_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).
:- end_tests(rome_statute_jurisdiction__hybrid_complementarity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the tension between the ICC's mandate and state sovereignty; states are 'extracted from' in terms of their exclusive jurisdiction, but the complementarity principle limits this. Suppression (0.30) is moderate because the ICC's enforcement relies on state cooperation rather than direct coercive power, but it does suppress impunity. Theater ratio (0.20) is low, as the ICC's function is largely genuine, though political maneuvering can introduce performative elements. The metrics show a slight increase in extractiveness and suppression over time as the ICC has become more active, challenging state impunity more directly.
 *
 * PERSPECTIVAL GAP:
 *   The ICC and victims perceive the complementarity mechanism as a necessary balance for justice, while states under investigation and accused individuals view it as an infringement on sovereignty. The engine's per-seat classification will reflect these divergent experiences, with beneficiaries seeing a coordination function and targets experiencing extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The ICC and victims are beneficiaries, as the constraint provides a mechanism for justice. Sovereign states under investigation and accused individuals are targets, as they face the assertion of international jurisdiction. Non-state parties are excluded, as they are outside the direct legal framework but still influenced by its norms. International legal scholars are observers, analyzing the system's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    complementarity_genuineness,
    'How genuinely are national proceedings conducted when the ICC asserts jurisdiction, and to what extent is ''unwillingness or inability'' a pretext for state evasion?',
    'Empirical analysis of national court records and independent human rights monitoring in cases where complementarity has been invoked, assessing the quality and impartiality of domestic investigations and prosecutions.',
    'If national proceedings are frequently found to be non-genuine, the effective extractiveness of the ICC''s jurisdiction is higher (as states are merely delaying justice), and the constraint leans more towards a Snare. If genuine, it supports the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(complementarity_genuineness, empirical, 'Assesses the real-world application of the complementarity principle.').

omega_variable(
    universal_justice_vs_state_consent,
    'What is the ultimate normative grounding of international criminal justice: universal moral imperatives or the consensual agreement of sovereign states?',
    'Conceptual analysis and philosophical debate within international legal theory. This is a foundational question that cannot be resolved empirically.',
    'If universal moral imperatives are prioritized, the constraint leans towards a Mountain (natural law of justice) or a more extractive Snare (if states resist a just universal order). If state consent is prioritized, it leans towards a Rope (pure coordination) or a less extractive Tangled Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(universal_justice_vs_state_consent, conceptual, 'Fundamental conceptual tension in international law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rome_statute_jurisdiction__hybrid_complementarity_reading, 1998, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rome_tr_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 1998, 0.1).
narrative_ontology:measurement(rome_tr_t2004, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2004, 0.15).
narrative_ontology:measurement(rome_tr_t2010, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(rome_tr_t2016, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2016, 0.19).
narrative_ontology:measurement(rome_tr_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(rome_be_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 1998, 0.3).
narrative_ontology:measurement(rome_be_t2004, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2004, 0.35).
narrative_ontology:measurement(rome_be_t2010, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(rome_be_t2016, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2016, 0.43).
narrative_ontology:measurement(rome_be_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(rome_su_t1998, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 1998, 0.2).
narrative_ontology:measurement(rome_su_t2004, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2004, 0.25).
narrative_ontology:measurement(rome_su_t2010, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2010, 0.28).
narrative_ontology:measurement(rome_su_t2016, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2016, 0.29).
narrative_ontology:measurement(rome_su_t2024, rome_statute_jurisdiction__hybrid_complementarity_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rome_statute_jurisdiction__hybrid_complementarity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__universalist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, rome_statute_jurisdiction__sovereigntist_reading).
narrative_ontology:affects_constraint(rome_statute_jurisdiction__hybrid_complementarity_reading, international_criminal_justice_funding).

% DUAL FORMULATION NOTE:
% This is one of three readings of the Rome Statute's jurisdictional framework, linked by their shared kernel. This 'hybrid complementarity' reading influences the 'universalist' and 'sovereigntist' readings by shaping the practical application and perceived legitimacy of the ICC's authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
