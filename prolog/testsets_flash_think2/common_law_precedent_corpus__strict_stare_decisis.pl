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
 *   constraint_id: common_law_precedent_corpus__strict_stare_decisis
 *   human_readable: Strict Stare Decisis in Common Law Precedent
 *   domain: legal/jurisprudence
 *
 * SUMMARY:
 *   This constraint describes the doctrine of strict stare decisis within
 *   common law systems, where judicial precedent is treated as a binding
 *   backward constraint, and departure requires extraordinary justification.
 *   It is one reading of the broader 'common_law_precedent_corpus' kernel.
 *   This reading emphasizes legal stability and predictability, often at the
 *   expense of adaptability to evolving social norms or novel legal
 *   challenges. The high extractiveness and suppression reflect the
 *   significant costs imposed on those seeking legal change.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_law_precedent_corpus__strict_stare_decisis, 0.75).
domain_priors:suppression_score(common_law_precedent_corpus__strict_stare_decisis, 0.8).
domain_priors:theater_ratio(common_law_precedent_corpus__strict_stare_decisis, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, extractiveness, 0.75).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(common_law_precedent_corpus__strict_stare_decisis, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_law_precedent_corpus__strict_stare_decisis, tangled_rope).
narrative_ontology:human_readable(common_law_precedent_corpus__strict_stare_decisis, "Strict Stare Decisis in Common Law Precedent").
narrative_ontology:topic_domain(common_law_precedent_corpus__strict_stare_decisis, "legal/jurisprudence").

domain_priors:requires_active_enforcement(common_law_precedent_corpus__strict_stare_decisis).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_law_precedent_corpus__strict_stare_decisis, 'f8135289-637d-4d36-98ee-6418ea1c8539').
narrative_ontology:cs_kernel_codification('f8135289-637d-4d36-98ee-6418ea1c8539', formalized).
narrative_ontology:cs_authority_grounding('f8135289-637d-4d36-98ee-6418ea1c8539', lineage).
narrative_ontology:cs_interpretation_layer_present('f8135289-637d-4d36-98ee-6418ea1c8539').
narrative_ontology:cs_reading_relation('f8135289-637d-4d36-98ee-6418ea1c8539', common_law_precedent_corpus__evolutionary_framework, forecloses).
narrative_ontology:cs_reading_relation('f8135289-637d-4d36-98ee-6418ea1c8539', common_law_precedent_corpus__pluralist_balancing, forecloses).
narrative_ontology:cs_axiom('f8135289-637d-4d36-98ee-6418ea1c8539', foundational, precedent_as_binding_rule).
narrative_ontology:cs_axiom_status(precedent_as_binding_rule, holdable).
narrative_ontology:cs_axiom_grounding('f8135289-637d-4d36-98ee-6418ea1c8539', precedent_as_binding_rule, deontological).
narrative_ontology:cs_axiom('f8135289-637d-4d36-98ee-6418ea1c8539', secondary, judicial_restraint_as_virtue).
narrative_ontology:cs_axiom_status(judicial_restraint_as_virtue, holdable).
narrative_ontology:cs_axiom_grounding('f8135289-637d-4d36-98ee-6418ea1c8539', judicial_restraint_as_virtue, conventional).
narrative_ontology:cs_reference_frame('f8135289-637d-4d36-98ee-6418ea1c8539', historical_legal_certainty).
narrative_ontology:cs_drift_state('f8135289-637d-4d36-98ee-6418ea1c8539', contemporary_legal_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('f8135289-637d-4d36-98ee-6418ea1c8539', '').
narrative_ontology:cs_kernel_id(common_law_precedent_corpus__strict_stare_decisis, common_law_precedent_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, legal_system_stability).
narrative_ontology:constraint_beneficiary(common_law_precedent_corpus__strict_stare_decisis, established_interests).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, litigants_seeking_change).
narrative_ontology:constraint_victim(common_law_precedent_corpus__strict_stare_decisis, evolving_social_norms).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, rule_of_law).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, predictability_in_law).
narrative_ontology:constraint_vindicates(common_law_precedent_corpus__strict_stare_decisis, judicial_restraint).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Judges are bound by and enforce the doctrine of strict stare decisis, requiring extraordinary justification for departure from precedent. Their professional identity and the legitimacy of the legal system are tied to upholding this stability.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Parties who seek to challenge or overturn established legal precedents face significant hurdles, high litigation costs, and a low probability of success due to the strict binding nature of prior rulings. Their ability to achieve legal reform through litigation is severely limited.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, litigants_seeking_change, payer,
    powerless, immediate, constrained, local).

% Entities (corporations, government agencies, long-standing institutions) that benefit from the predictability and stability of existing legal interpretations. Strict adherence to precedent protects their investments and operational models from sudden legal shifts, allowing them to plan with high certainty.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, established_interests, beneficiary,
    powerful, biographical, mobile, national).

% Academics and legal theorists who analyze the application and effects of stare decisis. They can critique its rigidity or defend its necessity, but do not directly enforce or pay its costs in a legal proceeding.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, legal_scholars, observer,
    analytical, generational, analytical, global).

% The collective shifts in societal values, ethical understandings, and factual circumstances that may render old precedents unjust or obsolete. Under strict stare decisis, these evolving norms struggle to find expression or legal recognition without extraordinary, often legislative, intervention.
narrative_ontology:constraint_stakeholder(common_law_precedent_corpus__strict_stare_decisis, evolving_social_norms, excluded,
    powerless, generational, identity_locked, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides legal predictability, consistency, and stability across judicial decisions, ensuring that similar cases are treated similarly and building a coherent body of law over time.
% TRANSFER_FUNCTION: Transfers legal certainty and stability to established legal interpretations and those who benefit from them, at the cost of flexibility, responsiveness to social change, and the ability of new litigants to challenge entrenched norms.
% ABSENT_VOICES: Advocates for rapid legal evolution, social movements whose norms are not yet codified, and future generations whose interests are not yet represented in the existing body of precedent. Their perspectives are largely excluded by the backward-looking nature of strict adherence to precedent.
% DISAPPEARANCE_RATIONALE: If strict stare decisis vanished overnight, legal outcomes would become highly unpredictable, leading to chaos in contracts, property rights, and criminal justice. The entire legal system would lose its foundational consistency, requiring a complete re-establishment of how legal decisions are made and applied.
% FOUNDING_PROBLEM: To ensure legal certainty, predictability, and fairness by treating like cases alike, preventing arbitrary judicial decisions, and building a stable, authoritative body of law over time.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians, constitutional scholars, and practitioners across the political spectrum attest to the historical and ongoing importance of legal stability and predictability, even if they disagree on the strictness with which precedent should bind. This corroboration comes from outside the immediate beneficiaries of strict adherence.
narrative_ontology:disappearance_verdict(common_law_precedent_corpus__strict_stare_decisis, world_rearranges).
narrative_ontology:founding_problem_status(common_law_precedent_corpus__strict_stare_decisis, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_law_precedent_corpus__strict_stare_decisis, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(common_law_precedent_corpus__strict_stare_decisis, 'none', 1).
narrative_ontology:epsilon_provenance(common_law_precedent_corpus__strict_stare_decisis, 0.75, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.75) stems from the substantial burden placed on litigants to overcome established precedent, often leading to costly and unsuccessful challenges. Suppression (0.80) is high because the legal system actively enforces adherence to precedent, effectively suppressing alternative legal interpretations and pathways for change. The theater ratio is low (0.15) as the binding nature of precedent is genuinely enforced by the judiciary, not merely performative. Accessibility collapse is high (0.85) because the strict doctrine severely limits the range of viable legal arguments. Resistance is moderate (0.55) as, despite the high bar, litigants and some judges still attempt to distinguish cases or argue for overturning precedent.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the judiciary and established interests, strict stare decisis is a necessary mechanism for legal order and stability. From the perspective of litigants seeking change and those representing evolving social norms, it functions as a barrier to justice and a mechanism for entrenching existing power structures. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary acts as the agenda-setter, enforcing the constraint. Established interests are beneficiaries, gaining predictability and protection for their status quo. Litigants seeking change and evolving social norms are victims, bearing the costs of rigidity and suppressed alternatives. Legal scholars act as observers, analyzing the system without direct participation in its enforcement or extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    strictness_vs_adaptability_balance,
    'What is the optimal balance between strict adherence to precedent for stability and flexibility for legal evolution, and where does this reading fall on that spectrum?',
    'Comparative legal analysis across jurisdictions with different approaches to stare decisis, assessing long-term societal and economic outcomes, and jurisprudential debate on the nature of law.',
    'If the current strictness is found to be suboptimal, it would suggest that the constraint''s extractiveness is higher than necessary for its coordination function, potentially reclassifying it closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(strictness_vs_adaptability_balance, conceptual, 'The inherent tension between legal stability and adaptability.').

omega_variable(
    justification_for_departure_threshold,
    'What constitutes ''extraordinary justification'' for departing from precedent, and is this threshold consistently applied or subject to judicial discretion?',
    'Empirical study of judicial opinions over time, coding for stated reasons for overturning or distinguishing precedent, and analysis of dissenting opinions.',
    'If the threshold is found to be inconsistently applied or arbitrarily high, it would indicate a higher degree of judicial power and potential for extraction, reinforcing the Tangled Rope classification. If it''s consistently and transparently applied, it might suggest a more Rope-like function.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(justification_for_departure_threshold, empirical, 'Ambiguity in the criteria for overturning precedent.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''strict_stare_decisis'' reading, or does it incorporate elements of other readings in practice?',
    'Detailed textual analysis of judicial opinions and legal scholarship, comparing the explicit articulation of stare decisis principles against the actual outcomes and reasoning in landmark cases.',
    'If elements of ''evolutionary_framework'' or ''pluralist_balancing'' are found to be consistently applied, it would suggest that this reading is not as ''strict'' as claimed, potentially lowering its measured extractiveness and suppression, and shifting its classification towards a more Rope-like or even Scaffold-like (if temporary) function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Verifying the purity of the ''strict_stare_decisis'' reading against actual legal practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_law_precedent_corpus__strict_stare_decisis, 1950, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1950, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1950, 0.15).
narrative_ontology:measurement(comm_tr_t1960, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1960, 0.15).
narrative_ontology:measurement(comm_tr_t1970, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(comm_tr_t1980, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1980, 0.15).
narrative_ontology:measurement(comm_tr_t1990, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(comm_tr_t2000, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(comm_tr_t2010, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 2010, 0.15).
narrative_ontology:measurement(comm_tr_t2020, common_law_precedent_corpus__strict_stare_decisis, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(comm_be_t1950, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1950, 0.7).
narrative_ontology:measurement(comm_be_t1960, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1960, 0.72).
narrative_ontology:measurement(comm_be_t1970, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1970, 0.73).
narrative_ontology:measurement(comm_be_t1980, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1980, 0.74).
narrative_ontology:measurement(comm_be_t1990, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(comm_be_t2000, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 2000, 0.75).
narrative_ontology:measurement(comm_be_t2010, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 2010, 0.75).
narrative_ontology:measurement(comm_be_t2020, common_law_precedent_corpus__strict_stare_decisis, base_extractiveness, 2020, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1950, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1950, 0.75).
narrative_ontology:measurement(comm_su_t1960, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1960, 0.77).
narrative_ontology:measurement(comm_su_t1970, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1970, 0.78).
narrative_ontology:measurement(comm_su_t1980, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1980, 0.79).
narrative_ontology:measurement(comm_su_t1990, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 1990, 0.8).
narrative_ontology:measurement(comm_su_t2000, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 2000, 0.8).
narrative_ontology:measurement(comm_su_t2010, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 2010, 0.8).
narrative_ontology:measurement(comm_su_t2020, common_law_precedent_corpus__strict_stare_decisis, suppression_requirement, 2020, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_law_precedent_corpus__strict_stare_decisis, enforcement_mechanism).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, constitutional_interpretation_doctrine).
narrative_ontology:affects_constraint(common_law_precedent_corpus__strict_stare_decisis, statutory_interpretation_rules).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'common_law_precedent_corpus' kernel, alongside 'evolutionary_framework' and 'pluralist_balancing'. Each reading represents a distinct structural claim about how precedent operates.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
