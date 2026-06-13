% ============================================================================
% CONSTRAINT STORY: war_winnability_post_1945__rhetorical_contraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_war_winnability_post_1945__rhetorical_contraction, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: war_winnability_post_1945__rhetorical_contraction
 *   human_readable: Nuclear War Winnability: Rhetorical Contraction
 *   domain: strategic_studies/nuclear_deterrence/international_relations
 *
 * SUMMARY:
 *   This constraint describes the post-1945 phenomenon where the concept of
 *   'winnability' in nuclear war became a rhetorical taboo in public
 *   discourse, while remaining an active, if constrained, subject of
 *   classified strategic planning. The space for discussing victory
 *   contracted discursively but persisted strategically. This dual-layer
 *   contraction benefits strategic planners by affording operational
 *   flexibility without public accountability, at the cost of democratic
 *   oversight and informed public discourse. This is one reading of the
 *   broader 'war_winnability_post_1945' kernel.
 *
 * KEY AGENTS:
 *   - strategic_planners: Primary beneficiary (institutional/arbitrage) — maintains operational flexibility.
 *   - political_elites: Secondary beneficiary (powerful/constrained) — manages public perception.
 *   - public_discourse: Primary victim (powerless/trapped) — subject to rhetorical contraction.
 *   - democratic_oversight: Secondary victim (organized/constrained) — denied full information for accountability.
 *   - academic_analysts: Observer (analytical/analytical) — attempts to bridge the public/classified gap.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, 0.6).
domain_priors:suppression_score(war_winnability_post_1945__rhetorical_contraction, 0.7).
domain_priors:theater_ratio(war_winnability_post_1945__rhetorical_contraction, 0.8).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, extractiveness, 0.6).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, theater_ratio, 0.8).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(war_winnability_post_1945__rhetorical_contraction, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(war_winnability_post_1945__rhetorical_contraction, tangled_rope).
narrative_ontology:human_readable(war_winnability_post_1945__rhetorical_contraction, "Nuclear War Winnability: Rhetorical Contraction").
narrative_ontology:topic_domain(war_winnability_post_1945__rhetorical_contraction, "strategic_studies/nuclear_deterrence/international_relations").

domain_priors:requires_active_enforcement(war_winnability_post_1945__rhetorical_contraction).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(war_winnability_post_1945__rhetorical_contraction, 'b2c5d4c7-584a-444e-8e9e-404b93536b1e').
narrative_ontology:cs_kernel_codification('b2c5d4c7-584a-444e-8e9e-404b93536b1e', implicit).
narrative_ontology:cs_authority_grounding('b2c5d4c7-584a-444e-8e9e-404b93536b1e', extraction).
narrative_ontology:cs_interpretation_layer_present('b2c5d4c7-584a-444e-8e9e-404b93536b1e').
narrative_ontology:cs_reading_relation('b2c5d4c7-584a-444e-8e9e-404b93536b1e', war_winnability_post_1945__deterrence_unthinkable, coexists_with).
narrative_ontology:cs_reading_relation('b2c5d4c7-584a-444e-8e9e-404b93536b1e', war_winnability_post_1945__countervailing_thinkable, coexists_with).
narrative_ontology:cs_axiom('b2c5d4c7-584a-444e-8e9e-404b93536b1e', foundational, public_rhetoric_must_manage_existential_risk).
narrative_ontology:cs_axiom_status(public_rhetoric_must_manage_existential_risk, holdable).
narrative_ontology:cs_axiom_grounding('b2c5d4c7-584a-444e-8e9e-404b93536b1e', public_rhetoric_must_manage_existential_risk, instrumental).
narrative_ontology:cs_axiom('b2c5d4c7-584a-444e-8e9e-404b93536b1e', foundational, classified_planning_requires_operational_latitude).
narrative_ontology:cs_axiom_status(classified_planning_requires_operational_latitude, holdable).
narrative_ontology:cs_axiom_grounding('b2c5d4c7-584a-444e-8e9e-404b93536b1e', classified_planning_requires_operational_latitude, conventional).
narrative_ontology:cs_reference_frame('b2c5d4c7-584a-444e-8e9e-404b93536b1e', post_hiroshima_strategic_ambiguity).
narrative_ontology:cs_drift_state('b2c5d4c7-584a-444e-8e9e-404b93536b1e', contemporary_era_of_information_asymmetry, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b2c5d4c7-584a-444e-8e9e-404b93536b1e', '').
narrative_ontology:cs_kernel_id(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, strategic_planners).
narrative_ontology:constraint_beneficiary(war_winnability_post_1945__rhetorical_contraction, political_elites).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, democratic_oversight).
narrative_ontology:constraint_victim(war_winnability_post_1945__rhetorical_contraction, public_discourse).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for developing and maintaining nuclear war plans, including scenarios for 'winnable' outcomes. They benefit from the rhetorical taboo as it allows them to conduct sensitive planning without public scrutiny or debate, maintaining operational flexibility.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, strategic_planners, agenda_setter,
    institutional, generational, arbitrage, national).

% Utilize the rhetorical taboo to manage public fear and maintain a stable deterrence narrative. They benefit from avoiding politically difficult discussions about nuclear war scenarios, simplifying public communication and reducing political risk.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, political_elites, beneficiary,
    powerful, biographical, constrained, national).

% The arena where public debate about nuclear war takes place. It is constrained by the rhetorical taboo, limiting the scope of discussion and preventing open engagement with the complexities of strategic planning. Bears the cost of suppressed information and limited democratic participation.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, public_discourse, payer,
    powerless, generational, trapped, global).

% Institutions (e.g., legislatures, watchdog groups) tasked with holding strategic planners accountable. They are victims of the rhetorical contraction as the taboo and classification limit their access to information and ability to scrutinize planning, hindering effective oversight.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, democratic_oversight, payer,
    organized, generational, constrained, national).

% Researchers and scholars who study nuclear strategy and deterrence. They observe and analyze the gap between public rhetoric and operational planning, often attempting to bridge this divide through scholarship, but lack direct power to alter the constraint.
narrative_ontology:constraint_stakeholder(war_winnability_post_1945__rhetorical_contraction, academic_analysts, observer,
    moderate, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(war_winnability_post_1945__rhetorical_contraction, strategic_planners).
narrative_ontology:fixing_cost_class(war_winnability_post_1945__rhetorical_contraction, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages public perception of nuclear war to maintain deterrence stability and prevent widespread panic, while allowing strategic planners to continue developing operational contingencies.
% TRANSFER_FUNCTION: Transfers operational flexibility and reduced public scrutiny to strategic planners and political elites, from public discourse and democratic oversight.
% ABSENT_VOICES: Advocates for radical transparency in nuclear planning and those who believe open public debate is essential for democratic control over existential risks are effectively silenced or marginalized by the rhetorical taboo. They would argue for a complete alignment of public and operational discourse.
% DISAPPEARANCE_RATIONALE: If the rhetorical taboo and the associated classification regime vanished overnight, public discourse on nuclear war would immediately expand, demanding transparency and accountability from strategic planners. This would force a fundamental reorganization of how nuclear strategy is communicated and debated, likely leading to significant shifts in planning and policy.
% FOUNDING_PROBLEM: The existential threat of nuclear weapons created a need to manage public fear and maintain a stable deterrence posture, while simultaneously requiring military planners to prepare for the unthinkable.
% FOUNDING_PROBLEM_CORROBORATION: The problem of managing nuclear risk and public perception remains live, as attested by ongoing debates in international relations and strategic studies. However, the specific solution of rhetorical contraction is contested, with critics (e.g., academic analysts, democratic oversight bodies) arguing that it has become an extractive mechanism rather than a pure coordination solution.
narrative_ontology:disappearance_verdict(war_winnability_post_1945__rhetorical_contraction, world_rearranges).
narrative_ontology:founding_problem_status(war_winnability_post_1945__rhetorical_contraction, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(war_winnability_post_1945__rhetorical_contraction, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(war_winnability_post_1945__rhetorical_contraction, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(war_winnability_post_1945__rhetorical_contraction_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(war_winnability_post_1945__rhetorical_contraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(war_winnability_post_1945__rhetorical_contraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a coordination function (managing public fear and maintaining deterrence stability) while simultaneously enabling asymmetric extraction (strategic planners retain operational latitude at the expense of public transparency). Extractiveness is high (0.6) due to the information asymmetry and the cost to public discourse. Suppression is also high (0.7) as the rhetorical taboo is actively maintained through political rhetoric and classification. The theater ratio is very high (0.8) because the public performance of 'unwinnability' masks ongoing, detailed planning for 'winnable' scenarios, making the public discourse largely performative.
 *
 * PERSPECTIVAL GAP:
 *   Strategic planners experience this as a necessary coordination mechanism for national security, allowing them to prepare for contingencies while managing public anxiety. Democratic oversight and public discourse, however, experience it as a form of suppression and extraction, where critical information is withheld, and public debate is constrained by an enforced taboo. The engine's per-seat classification should reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Strategic planners and political elites are beneficiaries (d near 0.0-0.2) as they gain flexibility and control over narrative. Democratic oversight and public discourse are victims (d near 0.8-1.0) as they bear the costs of opacity and constrained debate. Academic analysts are observers (d near 0.5) as they attempt to analyze the system from an external, balanced perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (managing nuclear risk and maintaining deterrence) is still live, but its operationalization has drifted. The 'rhetorical contraction' prevents mislabeling this as pure extraction by acknowledging the genuine coordination function of managing public perception of nuclear risk. However, the high theater ratio and suppression indicate that the coordination function is heavily intertwined with extractive practices, preventing it from being a pure Rope. The persistence of operational planning for winnability, despite the public taboo, suggests a form of 'mandatrophy' where the public mandate has atrophied while the operational mandate persists, creating a gap that is filled by extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rhetorical_vs_operational_reality,
    'Is the rhetorical taboo on nuclear war winnability a genuine reflection of its operational impossibility, or a strategic communication tool to manage public perception?',
    'Declassification of historical strategic planning documents and comparison with public statements over time; analysis of military doctrine evolution vs. public discourse.',
    'If primarily a communication tool, the constraint is more extractive, as it actively suppresses public debate and democratic oversight. If genuinely impossible, the constraint is closer to a Mountain, reflecting an irreducible physical/logical limit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rhetorical_vs_operational_reality, empirical, 'Ambiguity between rhetorical taboo and operational reality of nuclear war winnability.').

omega_variable(
    kernel_reading_identification,
    'This constraint is one reading of the ''war_winnability_post_1945'' kernel. How does this ''rhetorical_contraction'' reading differ from ''deterrence_unthinkable'' and ''countervailing_thinkable''?',
    'Analysis of the specific claims made by proponents of each reading regarding the feasibility and desirability of nuclear war planning.',
    'The ''rhetorical_contraction'' reading highlights the dual-layer nature of the constraint (public vs. classified), which is distinct from the purely operational focus of ''countervailing_thinkable'' or the purely categorical focus of ''deterrence_unthinkable''. This distinction affects the identification of beneficiaries and victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Distinguishing this reading from other interpretations of nuclear war winnability.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(war_winnability_post_1945__rhetorical_contraction, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(war__tr_t1945, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1945, 0.2).
narrative_ontology:measurement(war__tr_t1960, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1960, 0.5).
narrative_ontology:measurement(war__tr_t1980, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 1980, 0.8).
narrative_ontology:measurement(war__tr_t2000, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2000, 0.8).
narrative_ontology:measurement(war__tr_t2024, war_winnability_post_1945__rhetorical_contraction, theater_ratio, 2024, 0.8).

% Extraction over time
narrative_ontology:measurement(war__be_t1945, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1945, 0.3).
narrative_ontology:measurement(war__be_t1960, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1960, 0.5).
narrative_ontology:measurement(war__be_t1980, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 1980, 0.65).
narrative_ontology:measurement(war__be_t2000, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(war__be_t2024, war_winnability_post_1945__rhetorical_contraction, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(war__su_t1945, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1945, 0.4).
narrative_ontology:measurement(war__su_t1960, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(war__su_t1980, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 1980, 0.75).
narrative_ontology:measurement(war__su_t2000, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(war__su_t2024, war_winnability_post_1945__rhetorical_contraction, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(war_winnability_post_1945__rhetorical_contraction, enforcement_mechanism).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__deterrence_unthinkable).
narrative_ontology:affects_constraint(war_winnability_post_1945__rhetorical_contraction, war_winnability_post_1945__countervailing_thinkable).

% DUAL FORMULATION NOTE:
% This constraint is part of a family of readings of the 'war_winnability_post_1945' kernel. This 'rhetorical_contraction' reading focuses on the dual public/classified nature of winnability, distinct from purely operational or categorical claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
