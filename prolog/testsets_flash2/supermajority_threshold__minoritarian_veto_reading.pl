% ============================================================================
% CONSTRAINT STORY: supermajority_threshold__minoritarian_veto_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supermajority_threshold__minoritarian_veto_reading, []).

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
 *   constraint_id: supermajority_threshold__minoritarian_veto_reading
 *   human_readable: Supermajority Threshold as Minoritarian Veto
 *   domain: constitutional_theory/political_economy/institutional_design
 *
 * SUMMARY:
 *   This constraint describes the supermajority threshold, not as a safeguard
 *   for consensus, but as a mechanism that empowers blocking minorities to
 *   entrench the status quo against majoritarian will. It converts historical
 *   privilege into a permanent veto, leading to high extraction from
 *   contemporary majorities and reform advocates, and high suppression of
 *   their efforts. The constraint is actively enforced by institutional
 *   actors (e.g., constitutional courts) and passively by the sheer
 *   difficulty of overcoming the threshold.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, 0.85).
domain_priors:suppression_score(supermajority_threshold__minoritarian_veto_reading, 0.9).
domain_priors:theater_ratio(supermajority_threshold__minoritarian_veto_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(supermajority_threshold__minoritarian_veto_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supermajority_threshold__minoritarian_veto_reading, snare).
narrative_ontology:human_readable(supermajority_threshold__minoritarian_veto_reading, "Supermajority Threshold as Minoritarian Veto").
narrative_ontology:topic_domain(supermajority_threshold__minoritarian_veto_reading, "constitutional_theory/political_economy/institutional_design").

domain_priors:requires_active_enforcement(supermajority_threshold__minoritarian_veto_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(supermajority_threshold__minoritarian_veto_reading, 'ea463afc-df8d-4da0-b74b-33d2225bca35').
narrative_ontology:cs_kernel_codification('ea463afc-df8d-4da0-b74b-33d2225bca35', formalized).
narrative_ontology:cs_authority_grounding('ea463afc-df8d-4da0-b74b-33d2225bca35', lineage).
narrative_ontology:cs_interpretation_layer_present('ea463afc-df8d-4da0-b74b-33d2225bca35').
narrative_ontology:cs_reading_relation('ea463afc-df8d-4da0-b74b-33d2225bca35', supermajority_threshold__consensus_safeguard_reading, coexists_with).
narrative_ontology:cs_reading_relation('ea463afc-df8d-4da0-b74b-33d2225bca35', supermajority_threshold__adaptive_gradient_reading, coexists_with).
narrative_ontology:cs_axiom('ea463afc-df8d-4da0-b74b-33d2225bca35', foundational, majority_rule_is_foundational).
narrative_ontology:cs_axiom_status(majority_rule_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('ea463afc-df8d-4da0-b74b-33d2225bca35', majority_rule_is_foundational, deontological).
narrative_ontology:cs_axiom('ea463afc-df8d-4da0-b74b-33d2225bca35', foundational, constitutional_flexibility_is_necessary).
narrative_ontology:cs_axiom_status(constitutional_flexibility_is_necessary, holdable).
narrative_ontology:cs_axiom_grounding('ea463afc-df8d-4da0-b74b-33d2225bca35', constitutional_flexibility_is_necessary, instrumental).
narrative_ontology:cs_reference_frame('ea463afc-df8d-4da0-b74b-33d2225bca35', responsive_democratic_governance).
narrative_ontology:cs_drift_state('ea463afc-df8d-4da0-b74b-33d2225bca35', contemporary_political_polarization, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('ea463afc-df8d-4da0-b74b-33d2225bca35', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(supermajority_threshold__minoritarian_veto_reading, supermajority_threshold).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, entrenched_elites).
narrative_ontology:constraint_beneficiary(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities).
narrative_ontology:constraint_victim(supermajority_threshold__minoritarian_veto_reading, reform_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups benefit from the existing distribution of power and resources, which the supermajority threshold protects from change. They actively lobby against reforms and leverage their structural position to maintain the status quo.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, entrenched_elites, beneficiary,
    institutional, generational, arbitrage, national).

% Individuals or organizations that profit from the current legal and economic arrangements. They may not be actively involved in blocking reform but are direct recipients of the benefits preserved by the supermajority rule.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, status_quo_beneficiaries, beneficiary,
    powerful, biographical, constrained, national).

% The majority of the populace whose will for reform is consistently thwarted by the supermajority requirement. They bear the costs of an unresponsive system and are effectively trapped by the high barrier to change.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, contemporary_majorities, payer,
    organized, biographical, trapped, national).

% Activists, political movements, and scholars who identify the need for constitutional or systemic reform. They expend significant resources attempting to overcome the supermajority threshold, often with little success, and bear the frustration of systemic inertia.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, reform_advocates, payer,
    moderate, generational, constrained, national).

% Interpret the constitution and its amendment procedures. While not directly setting the supermajority rule, their rulings can reinforce or subtly alter the practical difficulty of overcoming it, acting as gatekeepers of the status quo.
narrative_ontology:constraint_stakeholder(supermajority_threshold__minoritarian_veto_reading, constitutional_courts, agenda_setter,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The constraint, in this reading, does not solve a genuine coordination problem; its primary function is to prevent coordination among majoritarian forces for reform.
% TRANSFER_FUNCTION: Transfers political power and policy outcomes from contemporary majorities and reform advocates to entrenched elites and status quo beneficiaries, by entrenching historical privileges and preventing their democratic revision.
% ABSENT_VOICES: Future generations, whose interests are bound by an unamendable past, are structurally absent. They would advocate for a more flexible constitutional framework that allows for adaptation to evolving societal needs.
% DISAPPEARANCE_RATIONALE: If the supermajority threshold vanished overnight, the political landscape would immediately shift. Majoritarian reforms, currently blocked, would likely pass, leading to a significant redistribution of power and resources. The constitutional framework would become more responsive to current public will.
% FOUNDING_PROBLEM: To protect fundamental rights and prevent hasty, ill-considered changes to the foundational law, ensuring stability and broad consensus for constitutional amendments.
% FOUNDING_PROBLEM_CORROBORATION: While proponents (entrenched elites) claim the problem is live, contemporary majorities and reform advocates, supported by political scientists and historical analysis, argue that the threshold now primarily serves to entrench historical privilege rather than protect fundamental rights, which are often themselves the target of reform efforts. The original problem of 'hasty change' is largely superseded by the problem of 'blocked necessary change'.
narrative_ontology:disappearance_verdict(supermajority_threshold__minoritarian_veto_reading, world_rearranges).
narrative_ontology:founding_problem_status(supermajority_threshold__minoritarian_veto_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(supermajority_threshold__minoritarian_veto_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(supermajority_threshold__minoritarian_veto_reading, 'none', 1).
narrative_ontology:epsilon_provenance(supermajority_threshold__minoritarian_veto_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supermajority_threshold__minoritarian_veto_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(supermajority_threshold__minoritarian_veto_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(supermajority_threshold__minoritarian_veto_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.85) is high because the threshold effectively transfers policy outcomes and political power to a minority. Suppression (0.90) is also high, reflecting the near-impossibility of overcoming the barrier, which actively suppresses majoritarian political action. The theater ratio (0.10) is low because the constraint's function is not performative; it is highly effective at its (extractive) purpose. Accessibility collapse (0.75) is substantial as alternatives to the entrenched status quo are severely limited. Resistance (0.80) is high, as majorities and reform advocates continuously push against the barrier, but with limited success.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of entrenched elites, the supermajority threshold is a legitimate protection of fundamental principles (a 'mountain' or 'rope'). From the perspective of contemporary majorities, it is a 'snare' that extracts their political agency and entrenches an undesirable status quo. The engine's classification will reflect this divergence based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Entrenched elites and status quo beneficiaries are clear beneficiaries (low d) as the constraint directly preserves their advantages. Contemporary majorities and reform advocates are clear targets (high d) as they bear the costs of political paralysis and thwarted will. Constitutional courts, as agenda-setters, enforce the mechanism, aligning their directionality with the beneficiaries by upholding the threshold's legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's original mandate (protecting fundamental rights and ensuring broad consensus) is, in this reading, dead. It has atrophied into a mechanism for minority veto, preventing necessary adaptation. The classification as a 'snare' prevents mislabeling this as legitimate 'coordination' or 'safeguard' when its primary effect is extraction and entrenchment. The high extractiveness and suppression, coupled with the 'dead' founding problem status, strongly indicate a mandatrophic state where the constraint serves a different, more extractive function than its stated purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimacy_of_historical_privilege,
    'Is the entrenchment of historical privilege by the supermajority threshold a legitimate outcome of constitutional design, or an illegitimate capture of democratic process?',
    'Philosophical and legal analysis of democratic theory, constitutionalism, and intergenerational equity. Public discourse and judicial review that explicitly addresses the normative basis of minority veto power.',
    'If deemed legitimate, the constraint might be reclassified as a ''rope'' or ''tangled_rope'' from a different normative frame. If illegitimate, its ''snare'' classification is reinforced, potentially leading to calls for reform or reinterpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_historical_privilege, conceptual, 'The normative status of minority veto power in a democracy.').

omega_variable(
    empirical_impact_on_policy_gridlock,
    'To what extent does the supermajority threshold empirically contribute to policy gridlock and the inability to address pressing societal issues, versus other factors?',
    'Comparative empirical studies across jurisdictions with varying amendment thresholds, analyzing legislative output, policy responsiveness, and public satisfaction over time. Quantitative analysis isolating the effect of the threshold from other political factors.',
    'Strong empirical evidence of gridlock directly attributable to the threshold would strengthen the ''snare'' classification by demonstrating its negative societal impact. Weak evidence might suggest other factors are more dominant, potentially shifting the focus of reform efforts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_impact_on_policy_gridlock, empirical, 'Quantifying the supermajority threshold''s contribution to policy paralysis.').

omega_variable(
    kernel_reading_ambiguity,
    'This constraint is one reading of the ''supermajority_threshold'' kernel. Is this ''minoritarian_veto_reading'' the most accurate structural interpretation, or do the ''consensus_safeguard_reading'' or ''adaptive_gradient_reading'' offer a more complete picture?',
    'A comprehensive analysis of the constraint''s historical application, its impact on different social groups, and the stated justifications versus actual outcomes. This involves evaluating the empirical evidence for each reading''s claims and their normative implications.',
    'If the ''consensus_safeguard_reading'' were found to be more accurate, the constraint would be reclassified, likely as a ''rope'' or ''tangled_rope'' with lower extractiveness. If the ''adaptive_gradient_reading'' were preferred, it would imply a need for recalibration rather than abolition, potentially leading to a ''scaffold'' classification if a sunset or review mechanism were introduced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity between different readings of the supermajority threshold kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supermajority_threshold__minoritarian_veto_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supe_tr_t0, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(supe_tr_t10, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(supe_tr_t20, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(supe_tr_t30, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 30, 0.12).
narrative_ontology:measurement(supe_tr_t40, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 40, 0.11).
narrative_ontology:measurement(supe_tr_t50, supermajority_threshold__minoritarian_veto_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(supe_be_t0, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(supe_be_t10, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(supe_be_t20, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 20, 0.78).
narrative_ontology:measurement(supe_be_t30, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 30, 0.82).
narrative_ontology:measurement(supe_be_t40, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 40, 0.84).
narrative_ontology:measurement(supe_be_t50, supermajority_threshold__minoritarian_veto_reading, base_extractiveness, 50, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(supe_su_t0, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(supe_su_t10, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 10, 0.75).
narrative_ontology:measurement(supe_su_t20, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 20, 0.82).
narrative_ontology:measurement(supe_su_t30, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 30, 0.87).
narrative_ontology:measurement(supe_su_t40, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 40, 0.89).
narrative_ontology:measurement(supe_su_t50, supermajority_threshold__minoritarian_veto_reading, suppression_requirement, 50, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supermajority_threshold__minoritarian_veto_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'supermajority_threshold' kernel. The 'minoritarian_veto_reading' focuses on the extractive and suppressive aspects, contrasting with the 'consensus_safeguard_reading' (emphasizing stability) and the 'adaptive_gradient_reading' (emphasizing functional calibration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
