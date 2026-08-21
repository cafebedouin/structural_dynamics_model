% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__rhetorical_scaffold_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__rhetorical_scaffold_reading, []).

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
 *   constraint_id: deferential_realism_ontology__rhetorical_scaffold_reading
 *   human_readable: Deferential Realism Typology as Rhetorical Scaffold
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the 'rhetorical scaffold' reading of
 *   the Deferential Realism ontology. From this perspective, the typology is
 *   primarily a normative vocabulary for policy critique, where
 *   classifications like 'snare' are declared based on judgments about
 *   illegitimate beneficiaries, rather than being discovered through
 *   objective measurement. The framework's value lies in its persuasive power
 *   to enable critical analysis and advocacy. This reading emphasizes low
 *   suppression of alternative framings and an advocacy-driven approach to
 *   classification, where epsilon values are constructed through normative
 *   judgment.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.15).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.2).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, scaffold).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "Deferential Realism Typology as Rhetorical Scaffold").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemology/normative_theory/institutional_design").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, '7e823264-b401-40e3-83a7-0fef377e1cad').
narrative_ontology:cs_kernel_codification('7e823264-b401-40e3-83a7-0fef377e1cad', formalized).
narrative_ontology:cs_authority_grounding('7e823264-b401-40e3-83a7-0fef377e1cad', expertise).
narrative_ontology:cs_interpretation_layer_present('7e823264-b401-40e3-83a7-0fef377e1cad').
narrative_ontology:cs_reading_relation('7e823264-b401-40e3-83a7-0fef377e1cad', deferential_realism_ontology__immutable_diagnostic_reading, forecloses).
narrative_ontology:cs_reading_relation('7e823264-b401-40e3-83a7-0fef377e1cad', deferential_realism_ontology__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('7e823264-b401-40e3-83a7-0fef377e1cad', foundational, classification_is_normative_declaration).
narrative_ontology:cs_axiom_status(classification_is_normative_declaration, holdable).
narrative_ontology:cs_axiom_grounding('7e823264-b401-40e3-83a7-0fef377e1cad', classification_is_normative_declaration, deontological).
narrative_ontology:cs_axiom('7e823264-b401-40e3-83a7-0fef377e1cad', foundational, framework_value_is_persuasive_power).
narrative_ontology:cs_axiom_status(framework_value_is_persuasive_power, holdable).
narrative_ontology:cs_axiom_grounding('7e823264-b401-40e3-83a7-0fef377e1cad', framework_value_is_persuasive_power, instrumental).
narrative_ontology:cs_reference_frame('7e823264-b401-40e3-83a7-0fef377e1cad', critical_theory_application).
narrative_ontology:cs_drift_state('7e823264-b401-40e3-83a7-0fef377e1cad', contemporary_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('7e823264-b401-40e3-83a7-0fef377e1cad', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, advocacy_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, framework_users).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, normative_critique_efficacy).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__rhetorical_scaffold_reading, social_construction_of_value).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Utilize the typology as a conceptual tool to frame arguments, identify targets for critique, and persuade audiences about the illegitimate nature of certain institutional arrangements. They benefit from the clarity and persuasive force the framework offers.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics, beneficiary,
    moderate, biographical, mobile, global).

% Leverage the typology to build coalitions, articulate grievances, and influence policy debates by providing a shared, normatively-charged language for critique. They gain rhetorical power and strategic direction from the framework.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, advocacy_groups, beneficiary,
    organized, biographical, mobile, global).

% Invest intellectual effort in learning and applying the framework's concepts. While they 'pay' in terms of cognitive load, they are net beneficiaries of its utility. Their 'cost' is the effort of engagement, not extraction.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, framework_users, payer,
    moderate, immediate, mobile, global).

% Adhere to a view of the typology as a purely objective, discovered instrument. They find this reading's emphasis on normative declaration problematic and are structurally excluded from its core premise, as it contradicts their foundational assumptions about the nature of classification.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, immutable_diagnostic_proponents, excluded,
    institutional, generational, identity_locked, global).

% Seek a middle ground, acknowledging both objective and normative aspects of the typology. They observe the debate between the purely diagnostic and purely rhetorical readings, attempting to integrate insights from both.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, hybrid_pragmatic_proponents, observer,
    institutional, generational, constrained, global).

% Analyze the meta-theoretical implications of different readings of the Deferential Realism ontology, without directly participating in policy critique or advocacy. They are interested in the structural properties of the framework itself.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, analytical_observers, observer,
    analytical, generational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, normatively-charged vocabulary for diverse policy critics and advocacy groups to coordinate their arguments and identify common targets for challenging illegitimate extraction and power asymmetries.
% TRANSFER_FUNCTION: Transfers persuasive power, a critical lens, and a framework for normative judgment to its users, enabling them to reframe policy debates and motivate action.
% ABSENT_VOICES: Proponents of purely objective, value-neutral social science or those who believe in a strict fact-value dichotomy are structurally excluded from this reading's core premise. They would object to the explicit embrace of normative judgment in classification.
% DISAPPEARANCE_RATIONALE: If this rhetorical framework vanished overnight, policy critique would lose a powerful, shared language for identifying and challenging illegitimate extraction. Advocacy efforts might become more fragmented and less persuasive, potentially allowing extractive mechanisms to persist unchallenged due to a lack of effective critical tools.
% FOUNDING_PROBLEM: The need for a robust, normatively-grounded language to critique and expose mechanisms of illegitimate extraction and power asymmetry in policy and institutional design, especially where purely descriptive accounts fail to motivate change.
% FOUNDING_PROBLEM_CORROBORATION: Critical theorists, social justice advocates, and scholars of rhetoric and persuasion attest to the ongoing need for such frameworks, noting that purely descriptive or 'objective' accounts often fail to motivate necessary social and political change. This corroboration comes from outside the immediate beneficiaries of the framework's direct application.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__rhetorical_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__rhetorical_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(deferential_realism_ontology__rhetorical_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).
:- end_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The metrics reflect the nature of a conceptual framework intended for critique. Extractiveness is low because the framework itself does not directly extract resources, but rather provides a tool for analysis. Suppression is low, as this reading explicitly embraces the coexistence of alternative framings. Theater ratio is low because the framework is genuinely intended for critical utility, not mere performance. Accessibility collapse and resistance are also low, as it's a tool that users can choose to adopt or not.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between this reading, which sees classification as a normative declaration, and the 'immutable diagnostic' reading, which views it as an objective discovery. This divergence is central to the kernel's contestation.
 *
 * DIRECTIONALITY LOGIC:
 *   Policy critics and advocacy groups are the primary beneficiaries, gaining a powerful tool for their work. Framework users are 'payers' in terms of intellectual effort, but are net beneficiaries of the framework's utility. Proponents of purely diagnostic readings are excluded from this reading's core premise, as their foundational assumptions conflict.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontology_nature_ambiguity,
    'Is the Deferential Realism ontology fundamentally a descriptive instrument for discovering structural facts, or a normative vocabulary for declaring judgments?',
    'Meta-theoretical analysis of the framework''s application in diverse contexts, examining whether its classifications consistently precede or follow normative judgments about legitimacy.',
    'If resolved as purely descriptive, this ''rhetorical scaffold'' reading would be reclassified as a misapplication or a ''snare'' of conceptual capture. If resolved as purely normative, the ''immutable diagnostic'' reading would be foreclosed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontology_nature_ambiguity, conceptual, 'Ambiguity regarding the fundamental nature of the DR ontology (descriptive vs. normative).').

omega_variable(
    epsilon_construction_criteria,
    'If epsilon values are ''constructed through normative judgment,'' what are the explicit, intersubjectively verifiable criteria for this construction?',
    'Development and formalization of a transparent methodology for deriving epsilon values from declared normative judgments, subject to peer review and critical scrutiny.',
    'Lack of clear criteria would undermine the framework''s persuasive power and lead to accusations of arbitrary classification. Clear criteria would strengthen its utility as a rhetorical scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_construction_criteria, empirical, 'Clarity on the criteria for normatively constructing epsilon values.').

omega_variable(
    implicit_suppression_of_alternatives,
    'Does the persuasive power of this ''rhetorical scaffold'' reading, despite its explicit claim of low suppression, implicitly suppress alternative framings by making them less appealing or effective in policy critique?',
    'Longitudinal study of policy discourse, observing the prevalence and impact of alternative critical frameworks in contexts where the DR typology is widely adopted. Analysis of rhetorical ''lock-in'' effects.',
    'If implicit suppression is significant, the framework''s effective suppression would be higher than currently measured, potentially shifting its classification towards a ''tangled_rope'' of conceptual coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implicit_suppression_of_alternatives, empirical, 'Whether the framework''s persuasive power implicitly suppresses alternative critical framings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 5, 0.1).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 5, 0.15).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 10, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 5, 0.2).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 10, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__rhetorical_scaffold_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'deferential_realism_ontology' kernel, each representing a different structural interpretation of the typology's function and persistence. They are linked to capture their interdependencies and contestation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
