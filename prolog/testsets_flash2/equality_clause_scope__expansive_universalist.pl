% ============================================================================
% CONSTRAINT STORY: equality_clause_scope__expansive_universalist
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equality_clause_scope__expansive_universalist, []).

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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equality_clause_scope__expansive_universalist
 *   human_readable: Expansive Universalist Reading of Equality Clause Scope
 *   domain: constitutional_law/political_philosophy/civil_rights_history
 *
 * SUMMARY:
 *   This constraint represents the 'expansive universalist' reading of
 *   constitutional equality clauses, which posits equality as a self-evident,
 *   universal truth applicable to all humans, irrespective of historical
 *   exclusions. This reading views historical discrimination as a failure to
 *   live up to the principle, rather than as binding precedent. It supports a
 *   low legitimacy threshold for rights expansion via judicial
 *   interpretation. This is one reading of the 'equality_clause_scope'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equality_clause_scope__expansive_universalist, 0.15).
domain_priors:suppression_score(equality_clause_scope__expansive_universalist, 0.2).
domain_priors:theater_ratio(equality_clause_scope__expansive_universalist, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, extractiveness, 0.15).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equality_clause_scope__expansive_universalist, mountain).
narrative_ontology:human_readable(equality_clause_scope__expansive_universalist, "Expansive Universalist Reading of Equality Clause Scope").
narrative_ontology:topic_domain(equality_clause_scope__expansive_universalist, "constitutional_law/political_philosophy/civil_rights_history").

domain_priors:emerges_naturally(equality_clause_scope__expansive_universalist).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equality_clause_scope__expansive_universalist, 'd91b6d37-f90c-4b6e-ae11-02e8249b47fb').
narrative_ontology:cs_kernel_codification('d91b6d37-f90c-4b6e-ae11-02e8249b47fb', fixed_text).
narrative_ontology:cs_authority_grounding('d91b6d37-f90c-4b6e-ae11-02e8249b47fb', lineage).
narrative_ontology:cs_interpretation_layer_present('d91b6d37-f90c-4b6e-ae11-02e8249b47fb').
narrative_ontology:cs_reading_relation('d91b6d37-f90c-4b6e-ae11-02e8249b47fb', equality_clause_scope__restrictive_originalist, forecloses).
narrative_ontology:cs_reading_relation('d91b6d37-f90c-4b6e-ae11-02e8249b47fb', equality_clause_scope__progressive_textualist, coexists_with).
narrative_ontology:cs_axiom('d91b6d37-f90c-4b6e-ae11-02e8249b47fb', foundational, equality_is_self_evident_truth).
narrative_ontology:cs_axiom_status(equality_is_self_evident_truth, holdable).
narrative_ontology:cs_axiom_grounding('d91b6d37-f90c-4b6e-ae11-02e8249b47fb', equality_is_self_evident_truth, deontological).
narrative_ontology:cs_axiom('d91b6d37-f90c-4b6e-ae11-02e8249b47fb', foundational, historical_exclusion_is_hypocrisy_not_precedent).
narrative_ontology:cs_axiom_status(historical_exclusion_is_hypocrisy_not_precedent, holdable).
narrative_ontology:cs_axiom_grounding('d91b6d37-f90c-4b6e-ae11-02e8249b47fb', historical_exclusion_is_hypocrisy_not_precedent, conventional).
narrative_ontology:cs_reference_frame('d91b6d37-f90c-4b6e-ae11-02e8249b47fb', universal_natural_rights_framework).
narrative_ontology:cs_drift_state('d91b6d37-f90c-4b6e-ae11-02e8249b47fb', contemporary, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('d91b6d37-f90c-4b6e-ae11-02e8249b47fb', '').
narrative_ontology:cs_kernel_id(equality_clause_scope__expansive_universalist, equality_clause_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equality_clause_scope__expansive_universalist, historically_marginalized_groups).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, natural_rights_doctrine).
narrative_ontology:constraint_vindicates(equality_clause_scope__expansive_universalist, universal_human_dignity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups are the primary beneficiaries of this reading, as it provides a framework for expanding their rights and challenging historical exclusions. Their identity is often tied to the struggle for recognition under this principle.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, historically_marginalized_groups, beneficiary,
    organized, generational, identity_locked, national).

% These advocates actively promote and litigate for the expansive application of equality, using this reading to push for legal and social reforms. They shape the interpretation and application of the clause.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, civil_rights_advocates, agenda_setter,
    powerful, biographical, constrained, national).

% Judges and legal scholars who adopt this reading interpret constitutional equality clauses broadly, often finding new applications for the principle based on evolving societal understanding of justice and human rights.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, judicial_interpreters, agenda_setter,
    institutional, generational, constrained, national).

% Adherents to the restrictive originalist reading are structurally excluded from the interpretive process of this expansive reading, as their foundational premises are incompatible. They would argue against judicial activism and for adherence to historical intent.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, restrictive_originalists, excluded,
    institutional, generational, identity_locked, national).

% While sharing some goals, progressive textualists are excluded from the specific interpretive method of this reading, as they prioritize democratic amendment processes over broad judicial reinterpretation for rights expansion.
narrative_ontology:constraint_stakeholder(equality_clause_scope__expansive_universalist, progressive_textualists, excluded,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universal moral and legal standard for human treatment, coordinating societal efforts towards greater justice and inclusion by establishing a baseline of inherent worth for all individuals.
% TRANSFER_FUNCTION: Transfers moral and legal standing, rights, and protections from a historically privileged subset of humanity to all individuals, challenging and reallocating social and political power.
% ABSENT_VOICES: Restrictive originalists and those who benefit from historical hierarchies are actively marginalized by this reading; they would argue for a narrower, historically bounded application of equality, but their views are treated as antithetical to the core principle.
% DISAPPEARANCE_RATIONALE: If this expansive universalist reading of equality vanished, the legal and moral foundations for civil rights, anti-discrimination laws, and human dignity would collapse, leading to a profound reorganization of legal systems and social norms, likely reverting to more hierarchical structures.
% FOUNDING_PROBLEM: The historical problem of justifying and enforcing equal treatment for all humans, particularly in the face of entrenched discrimination and exclusion.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, international human rights bodies, and philosophical traditions attest that the problem of achieving true equality remains live, citing ongoing systemic inequalities and discrimination globally. This corroboration comes from outside the direct beneficiaries of specific legal outcomes.
narrative_ontology:disappearance_verdict(equality_clause_scope__expansive_universalist, world_rearranges).
narrative_ontology:founding_problem_status(equality_clause_scope__expansive_universalist, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equality_clause_scope__expansive_universalist, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equality_clause_scope__expansive_universalist, 'none', 1).
narrative_ontology:epsilon_provenance(equality_clause_scope__expansive_universalist, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equality_clause_scope__expansive_universalist_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, ExtMetricName, E),
    domain_priors:suppression_score(equality_clause_scope__expansive_universalist, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(equality_clause_scope__expansive_universalist),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(equality_clause_scope__expansive_universalist, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(equality_clause_scope__expansive_universalist_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The low extractiveness (0.15) and suppression (0.2) reflect the ideal of a natural law that benefits all, with minimal coercive overhead. The high accessibility_collapse (0.88) indicates that once the principle is understood, alternatives to universal equality are seen as logically untenable. Resistance is low (0.05) because, in this reading, the principle is fundamentally self-evident. The claimed type is 'mountain' because it asserts a natural, immutable truth, despite the presence of beneficiaries (which triggers FSM evaluation).
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically marginalized groups, this reading is a liberating force, a true mountain of justice. From the perspective of those who benefit from historical hierarchies, it is a constructed constraint that undermines tradition. The engine's FSM detection will highlight this tension between the claimed naturalness and the identifiable beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically marginalized groups are the primary beneficiaries, as this reading expands their rights. Civil rights advocates and judicial interpreters act as agenda-setters, actively shaping and applying this reading. Restrictive originalists and progressive textualists are structurally excluded from this specific interpretive framework, as their approaches to equality differ fundamentally.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_social_construct,
    'Is the expansive universalist reading of equality a genuine natural law, or a socially constructed principle that benefits identifiable agents?',
    'Philosophical analysis of foundational ethical principles, cross-cultural studies of moral intuitions, and historical examination of the contingent development of ''universal'' rights claims.',
    'If a social construct, the constraint would reclassify from Mountain to a more extractive type (e.g., Tangled Rope), reflecting the active enforcement and contestation required to maintain its ''universal'' application against competing interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Ambiguity between inherent truth and constructed norm for universal equality.').

omega_variable(
    judicial_legitimacy_threshold,
    'What is the appropriate legitimacy threshold for judicial bodies to expand the scope of equality based on evolving societal understanding, versus requiring legislative or amendment processes?',
    'Comparative constitutional law studies, analysis of public opinion on judicial review, and historical outcomes of judicial versus legislative rights expansions.',
    'A higher threshold would strengthen the ''progressive_textualist'' reading, potentially reclassifying this reading as a Snare if its judicial expansion is seen as illegitimate extraction of power from the democratic process.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_legitimacy_threshold, preference, 'Debate over the proper institutional mechanism for expanding equality rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equality_clause_scope__expansive_universalist, 1776, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1776, equality_clause_scope__expansive_universalist, theater_ratio, 1776, 0.01).
narrative_ontology:measurement(equa_tr_t1865, equality_clause_scope__expansive_universalist, theater_ratio, 1865, 0.02).
narrative_ontology:measurement(equa_tr_t1964, equality_clause_scope__expansive_universalist, theater_ratio, 1964, 0.04).
narrative_ontology:measurement(equa_tr_t2024, equality_clause_scope__expansive_universalist, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(equa_be_t1776, equality_clause_scope__expansive_universalist, base_extractiveness, 1776, 0.05).
narrative_ontology:measurement(equa_be_t1865, equality_clause_scope__expansive_universalist, base_extractiveness, 1865, 0.1).
narrative_ontology:measurement(equa_be_t1964, equality_clause_scope__expansive_universalist, base_extractiveness, 1964, 0.12).
narrative_ontology:measurement(equa_be_t2024, equality_clause_scope__expansive_universalist, base_extractiveness, 2024, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1776, equality_clause_scope__expansive_universalist, suppression_requirement, 1776, 0.1).
narrative_ontology:measurement(equa_su_t1865, equality_clause_scope__expansive_universalist, suppression_requirement, 1865, 0.15).
narrative_ontology:measurement(equa_su_t1964, equality_clause_scope__expansive_universalist, suppression_requirement, 1964, 0.18).
narrative_ontology:measurement(equa_su_t2024, equality_clause_scope__expansive_universalist, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equality_clause_scope__expansive_universalist, identity_coordination).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__restrictive_originalist).
narrative_ontology:affects_constraint(equality_clause_scope__expansive_universalist, equality_clause_scope__progressive_textualist).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'equality_clause_scope' kernel. This 'expansive_universalist' reading asserts equality as a self-evident universal truth, contrasting with 'restrictive_originalist' (historical, limited application) and 'progressive_textualist' (democratic amendment for expansion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
