% ============================================================================
% CONSTRAINT STORY: common_law_precedent_corpus__strict_stare_decisis
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_law_precedent_corpus__strict_stare_decisis, []).

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
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis in Common Law Precedent
 *   domain: legal/jurisprudence/constitutional_law
 *
 * SUMMARY:
 *   This constraint describes the 'strict_stare_decisis' reading of the
 *   'common_law_precedent_corpus' kernel. Under this reading, judicial
 *   precedent is a strong, backward-binding constraint, requiring
 *   extraordinary justification for any departure. This framework prioritizes
 *   legal stability and predictability, but at the cost of suppressing
 *   challenges to established norms and potentially entrenching outdated or
 *   unjust legal positions. The structural delta for this reading is high
 *   constraint rigidity, with precedent overruling being rare and highly
 *   contested, and litigants facing narrow pathways for norm challenge,
 *   leaving the judiciary constrained by accumulated holdings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.65).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.75).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.65).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis in Common Law Precedent").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal/jurisprudence/constitutional_law").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, 'aaca92aa-438e-4e5f-b764-36bcd47d2654').
narrative_ontology:cs_kernel_codification('aaca92aa-438e-4e5f-b764-36bcd47d2654', formalized).
narrative_ontology:cs_authority_grounding('aaca92aa-438e-4e5f-b764-36bcd47d2654', lineage).
narrative_ontology:cs_interpretation_layer_present('aaca92aa-438e-4e5f-b764-36bcd47d2654').
narrative_ontology:cs_reading_relation('aaca92aa-438e-4e5f-b764-36bcd47d2654', common_law_precedent_corpus__evolutionary_framework, coexists_with).
narrative_ontology:cs_reading_relation('aaca92aa-438e-4e5f-b764-36bcd47d2654', common_law_precedent_corpus__pluralist_balancing, coexists_with).
narrative_ontology:cs_axiom('aaca92aa-438e-4e5f-b764-36bcd47d2654', foundational, precedent_binds_strongly).
narrative_ontology:cs_axiom_status(precedent_binds_strongly, holdable).
narrative_ontology:cs_axiom_grounding('aaca92aa-438e-4e5f-b764-36bcd47d2654', precedent_binds_strongly, deontological).
narrative_ontology:cs_axiom('aaca92aa-438e-4e5f-b764-36bcd47d2654', secondary, judicial_role_is_limited).
narrative_ontology:cs_axiom_status(judicial_role_is_limited, holdable).
narrative_ontology:cs_axiom_grounding('aaca92aa-438e-4e5f-b764-36bcd47d2654', judicial_role_is_limited, conventional).
narrative_ontology:cs_reference_frame('aaca92aa-438e-4e5f-b764-36bcd47d2654', judicial_restraint_principle).
narrative_ontology:cs_drift_state('aaca92aa-438e-4e5f-b764-36bcd47d2654', contemporary_jurisprudence, gap(stable, minor, true)).
narrative_ontology:cs_created_at('aaca92aa-438e-4e5f-b764-36bcd47d2654', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, legal_system_stability).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, established_interests).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, litigants_seeking_norm_change).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, evolving_societal_norms).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, rule_of_law_principle).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, judicial_restraint_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies legal precedent, upholding the rule of law and ensuring consistency. While they set the agenda for individual cases, they are also bound by the system of precedent, requiring extraordinary justification to depart from it.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Individuals or groups who challenge existing legal norms, bearing the high costs and low probability of success in overturning established precedent. Their pathways for norm challenge are narrow and difficult.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, litigants_seeking_norm_change, payer,
    powerless, biographical, constrained, national).

% Parties (e.g., corporations, government agencies) whose positions are favored by existing legal precedents, benefiting from predictability and the difficulty of challenging the status quo. They rely on the rigidity of precedent to maintain their advantages.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, established_interests, beneficiary,
    powerful, generational, mobile, national).

% Academics and legal theorists who analyze the application and evolution of precedent, providing critical commentary and proposing reforms, but without direct enforcement power over the legal system.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legal_scholars, observer,
    analytical, generational, analytical, universal).

% The collective values and expectations of society that shift over time, often coming into conflict with rigid precedents. They are 'excluded' as they have no direct voice in the legal system's internal logic, but their pressure for change is felt indirectly.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, evolving_societal_norms, excluded,
    powerless, generational, identity_locked, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(common_law_precedent_corpus__strict_stare_decisis, established_interests).
narrative_ontology:fixing_cost_class(common_law_precedent_corpus__strict_stare_decisis, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides legal predictability and stability, allowing individuals and institutions to plan their actions and investments based on settled law, ensuring consistency and fairness in judicial decisions.
% TRANSFER_FUNCTION: Transfers the burden of proof and the cost of legal innovation from the established legal order to those seeking to challenge or change it, effectively subsidizing legal stability at the expense of normative evolution.
% ABSENT_VOICES: Evolving societal norms and future generations, whose interests may be constrained by past decisions without direct representation in the current legal framework. Their perspectives are often only heard through the difficult process of challenging precedent.
% DISAPPEARANCE_RATIONALE: If the binding force of precedent vanished overnight, every legal question would be open for re-litigation, leading to extreme uncertainty and instability in all areas governed by law. The entire legal system would collapse into chaos, requiring a complete reorganization.
% FOUNDING_PROBLEM: To ensure consistency, predictability, and fairness in legal judgments, preventing arbitrary decisions by individual judges and promoting public confidence in the judiciary and the rule of law.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians, political scientists, and the general public (through surveys on trust in institutions) corroborate the historical and ongoing need for legal stability and predictability, though the *degree* of binding force is a subject of ongoing debate among legal scholars.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__strict_stare_decisis, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_law_precedent_corpus__strict_stare_decisis_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates legal stability and predictability (benefiting the legal system and established interests) but simultaneously extracts from those seeking legal reform or challenging outdated norms (litigants seeking norm change, evolving societal norms). The high suppression (0.75) reflects the active judicial enforcement of precedent and the difficulty of overturning it. Extractiveness (0.65) is substantial as the system can maintain positions that are no longer socially optimal or just. The theater ratio is moderate (0.25) as the justification for upholding precedent is often genuine, but can become performative when denying the need for change in the face of strong societal pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the judiciary and established interests, strict stare decisis is a necessary mechanism for the rule of law and societal order. From the perspective of litigants seeking norm change and evolving societal norms, it can appear as an oppressive force that entrenches injustice and resists necessary adaptation. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary, as the agenda-setter, benefits from the stability and perceived legitimacy of the system, though they are also constrained by it. Established interests are clear beneficiaries, as the system protects their favored positions. Litigants seeking norm change and evolving societal norms are the primary targets, bearing the costs of challenging the entrenched system. Legal scholars act as observers, analyzing the system without direct participation in its enforcement or extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    precedent_rigidity_vs_justice,
    'Is the high rigidity of strict stare decisis a necessary cost for legal stability, or does it primarily serve to entrench existing power structures and delay social justice?',
    'Comparative legal analysis across jurisdictions with varying approaches to stare decisis, assessing long-term societal outcomes, and empirical studies on the impact of precedent on marginalized groups.',
    'If primarily entrenching power, the constraint''s effective extractiveness is higher than measured, and its coordination function is largely a cover. If necessary for stability, the measured extractiveness is a legitimate cost of coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(precedent_rigidity_vs_justice, conceptual, 'Whether precedent''s rigidity serves stability or entrenches power.').

omega_variable(
    extraordinary_justification_threshold,
    'What constitutes ''extraordinary justification'' for departing from precedent, and how consistently is this standard applied across different courts and cases?',
    'Content analysis of judicial opinions over time, identifying explicit and implicit criteria for overruling precedent, and statistical analysis of overruling rates by court, case type, and political context.',
    'If the standard is inconsistently applied or shifts based on judicial ideology, the constraint''s suppression is more arbitrary and less legitimate, potentially increasing effective extractiveness for targeted groups.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraordinary_justification_threshold, empirical, 'Consistency and criteria for departing from precedent.').

omega_variable(
    judicial_discretion_scope,
    'To what extent does the strict application of precedent genuinely constrain judicial discretion, versus providing a rhetorical cover for policy preferences?',
    'Empirical studies of judicial behavior, analyzing voting patterns and opinion content in cases where precedent is challenged, particularly when judges'' stated preferences align with the outcome.',
    'If precedent primarily serves as rhetorical cover, the constraint''s theater_ratio is higher, and its claimed coordination function is more performative than functional, indicating a stronger extractive component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(judicial_discretion_scope, empirical, 'Precedent as genuine constraint vs. rhetorical cover for judicial policy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 0, 70).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t0, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0, 0.2).
narrative_ontology:measurement(comm_tr_t10, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 10, 0.21).
narrative_ontology:measurement(comm_tr_t20, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 20, 0.22).
narrative_ontology:measurement(comm_tr_t30, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 30, 0.23).
narrative_ontology:measurement(comm_tr_t40, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 40, 0.24).
narrative_ontology:measurement(comm_tr_t50, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 50, 0.25).
narrative_ontology:measurement(comm_tr_t60, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 60, 0.25).
narrative_ontology:measurement(comm_tr_t70, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 70, 0.25).

% Extraction over time
narrative_ontology:measurement(comm_be_t0, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(comm_be_t10, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(comm_be_t20, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(comm_be_t30, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 30, 0.62).
narrative_ontology:measurement(comm_be_t40, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 40, 0.63).
narrative_ontology:measurement(comm_be_t50, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 50, 0.64).
narrative_ontology:measurement(comm_be_t60, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 60, 0.65).
narrative_ontology:measurement(comm_be_t70, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 70, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t0, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(comm_su_t10, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(comm_su_t20, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(comm_su_t30, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 30, 0.72).
narrative_ontology:measurement(comm_su_t40, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 40, 0.73).
narrative_ontology:measurement(comm_su_t50, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 50, 0.74).
narrative_ontology:measurement(comm_su_t60, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(comm_su_t70, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 70, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
