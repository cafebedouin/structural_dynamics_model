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
 *   human_readable: Deferential Realism Ontology as Rhetorical Scaffold
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint models the Deferential Realism ontology as a rhetorical
 *   scaffold, where its primary function is to provide a normative vocabulary
 *   for policy critique. In this reading, classifications like 'snare' are
 *   not discovered through objective measurement but are declared based on
 *   normative judgments about legitimate beneficiaries. The framework's value
 *   lies in its persuasive power and its ability to mobilize advocacy, rather
 *   than its claim to immutable diagnostic truth. This is one reading of the
 *   'deferential_realism_ontology' kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.1).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.2).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, scaffold).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "Deferential Realism Ontology as Rhetorical Scaffold").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemology/normative_theory/institutional_design").

narrative_ontology:has_sunset_clause(deferential_realism_ontology__rhetorical_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, '02f9cc5f-0990-4b49-bd83-5d5ea7e627d4').
narrative_ontology:cs_kernel_codification('02f9cc5f-0990-4b49-bd83-5d5ea7e627d4', distributed).
narrative_ontology:cs_authority_grounding('02f9cc5f-0990-4b49-bd83-5d5ea7e627d4', distributed).
narrative_ontology:cs_reading_relation('02f9cc5f-0990-4b49-bd83-5d5ea7e627d4', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('02f9cc5f-0990-4b49-bd83-5d5ea7e627d4', deferential_realism_ontology__hybrid_pragmatic_reading, coexists_with).
narrative_ontology:cs_axiom('02f9cc5f-0990-4b49-bd83-5d5ea7e627d4', foundational, classification_is_normative_declaration).
narrative_ontology:cs_axiom_status(classification_is_normative_declaration, holdable).
narrative_ontology:cs_axiom_grounding('02f9cc5f-0990-4b49-bd83-5d5ea7e627d4', classification_is_normative_declaration, deontological).
narrative_ontology:cs_axiom('02f9cc5f-0990-4b49-bd83-5d5ea7e627d4', foundational, framework_value_is_persuasive_power).
narrative_ontology:cs_axiom_status(framework_value_is_persuasive_power, holdable).
narrative_ontology:cs_axiom_grounding('02f9cc5f-0990-4b49-bd83-5d5ea7e627d4', framework_value_is_persuasive_power, instrumental).
narrative_ontology:cs_reference_frame('02f9cc5f-0990-4b49-bd83-5d5ea7e627d4', critical_theory_as_tool).
narrative_ontology:cs_drift_state('02f9cc5f-0990-4b49-bd83-5d5ea7e627d4', contemporary_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('02f9cc5f-0990-4b49-bd83-5d5ea7e627d4', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, advocacy_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, institutional_defenders).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Utilize the typology as a tool for framing policy debates and mobilizing public opinion. They benefit from the persuasive power of labels like 'snare' to highlight perceived injustices and advocate for change.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics, beneficiary,
    moderate, biographical, mobile, global).

% Adopt the typology to articulate their critiques of existing institutional arrangements, finding it effective for communicating complex normative arguments in an accessible way. The framework provides a shared language for their advocacy.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, advocacy_groups, beneficiary,
    organized, generational, mobile, national).

% Analyze the typology's utility as a normative framework, evaluating its coherence, consistency, and effectiveness in shaping discourse. They are interested in its conceptual power rather than its direct application.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, academic_theorists, observer,
    analytical, civilizational, analytical, universal).

% Bear the cost of having their arrangements labeled as 'snares' or 'tangled ropes' by critics using the typology. They must respond to these critiques, either by reframing their own positions or by challenging the typology's legitimacy.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, institutional_defenders, payer,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, accessible vocabulary for normative critique, enabling diverse policy critics and advocacy groups to coordinate their arguments and communicate effectively about institutional design.
% TRANSFER_FUNCTION: Transfers persuasive power and legitimacy to policy critiques by offering a structured way to label and categorize institutional arrangements, from critics to their target audiences.
% ABSENT_VOICES: Those who believe in purely objective, value-neutral social science might object to the explicit normative framing, arguing it biases analysis. They are often outside the direct policy debate, operating in academic silos.
% DISAPPEARANCE_RATIONALE: If this rhetorical framing vanished, policy critique would lose a powerful, widely understood vocabulary. Critics would need to invent new ways to articulate their normative judgments, leading to a period of conceptual fragmentation and reduced persuasive impact.
% FOUNDING_PROBLEM: The problem of effectively communicating complex normative critiques of institutional arrangements to a broader public and coordinating advocacy efforts around shared conceptual tools.
% FOUNDING_PROBLEM_CORROBORATION: Policy critics and advocacy groups consistently attest to the ongoing challenge of framing their arguments persuasively. Academic theorists, from an analytical distance, also corroborate the need for effective conceptual tools in public discourse.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__rhetorical_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__rhetorical_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(deferential_realism_ontology__rhetorical_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 0.1, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low because the framework itself does not directly extract resources; its 'extraction' is the persuasive power it lends to critiques. Suppression is low because this reading emphasizes the open, advocacy-driven nature of classification, rather than enforcing a single 'correct' interpretation. The 'scaffold' classification reflects its temporary, instrumental role in building arguments and facilitating critique, with an implicit sunset when the critique is either successful or superseded. The metrics reflect its function as a tool for normative discourse, not a fixed diagnostic instrument.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of policy critics, the framework is a valuable tool for social change. From the perspective of institutional defenders, it is a weaponized vocabulary. This reading explicitly embraces the normative, advocacy-driven nature of the framework, which is precisely what creates this perspectival gap.
 *
 * DIRECTIONALITY LOGIC:
 *   Policy critics and advocacy groups are the primary beneficiaries, as the framework provides them with a powerful rhetorical tool. Institutional defenders are the payers, as they bear the cost of being labeled and critiqued by this framework. Academic theorists act as observers, analyzing its conceptual utility.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontology_epistemic_status,
    'Is the Deferential Realism ontology primarily an objective diagnostic tool, a normative rhetorical framework, or a hybrid of both?',
    'Analysis of how the typology is actually used in practice by different communities (academic, policy, advocacy) and the epistemic claims made by its proponents and critics.',
    'If resolved as purely rhetorical, its claims to ''realism'' would be weakened, potentially reducing its persuasive power for some audiences. If resolved as purely diagnostic, its utility for direct policy critique would be limited without explicit normative bridging.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ontology_epistemic_status, conceptual, 'Ambiguity regarding the fundamental epistemic status of the Deferential Realism ontology.').

omega_variable(
    normative_judgment_source,
    'What are the foundational normative judgments that drive the ''declaration'' of classifications like ''snare'' in this rhetorical reading, and are they universally shared or context-dependent?',
    'Detailed philosophical analysis of the underlying ethical theories and value systems implicitly or explicitly invoked when applying the typology in a rhetorical context.',
    'If the normative judgments are highly context-dependent or contested, the rhetorical power of the typology might be limited to specific communities. If they are shown to derive from broadly shared principles, its persuasive reach would be wider.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_judgment_source, preference, 'Uncertainty about the source and universality of the normative judgments underpinning rhetorical classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 5, 0.045).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 10, 0.05).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 5, 0.09).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 10, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 5, 0.19).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 10, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__rhetorical_scaffold_reading, identity_coordination).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__immutable_diagnostic_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'deferential_realism_ontology' kernel. This 'rhetorical_scaffold_reading' emphasizes the normative and persuasive function of the typology, contrasting with the 'immutable_diagnostic_reading' (objective measurement) and the 'hybrid_pragmatic_reading' (fixed core, contested periphery).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
