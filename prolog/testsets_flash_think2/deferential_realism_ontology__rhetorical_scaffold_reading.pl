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
 *   human_readable: DR Ontology as Rhetorical Scaffold for Policy Critique
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story instantiates the 'rhetorical scaffold' reading of
 *   the Deferential Realism ontology. In this reading, the typology
 *   (Mountain, Rope, Snare, etc.) is understood not as a set of discovered,
 *   objective classifications, but as a normative vocabulary designed for
 *   policy critique. Its primary value lies in its persuasive power to frame
 *   institutional arrangements as illegitimate or extractive, thereby
 *   supporting advocacy and social change. Classification, particularly of
 *   'snares,' is a declaration based on normative judgments about legitimate
 *   beneficiaries, rather than an empirical discovery.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, 0.6).
domain_priors:suppression_score(deferential_realism_ontology__rhetorical_scaffold_reading, 0.2).
domain_priors:theater_ratio(deferential_realism_ontology__rhetorical_scaffold_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(deferential_realism_ontology__rhetorical_scaffold_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__rhetorical_scaffold_reading, scaffold).
narrative_ontology:human_readable(deferential_realism_ontology__rhetorical_scaffold_reading, "DR Ontology as Rhetorical Scaffold for Policy Critique").
narrative_ontology:topic_domain(deferential_realism_ontology__rhetorical_scaffold_reading, "epistemology/normative_theory/institutional_design").

narrative_ontology:has_sunset_clause(deferential_realism_ontology__rhetorical_scaffold_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__rhetorical_scaffold_reading, 'c861ed65-86a3-4700-a3cc-9f9b1d558e1c').
narrative_ontology:cs_kernel_codification('c861ed65-86a3-4700-a3cc-9f9b1d558e1c', implicit).
narrative_ontology:cs_authority_grounding('c861ed65-86a3-4700-a3cc-9f9b1d558e1c', expertise).
narrative_ontology:cs_interpretation_layer_present('c861ed65-86a3-4700-a3cc-9f9b1d558e1c').
narrative_ontology:cs_reading_relation('c861ed65-86a3-4700-a3cc-9f9b1d558e1c', deferential_realism_ontology__immutable_diagnostic_reading, forecloses).
narrative_ontology:cs_reading_relation('c861ed65-86a3-4700-a3cc-9f9b1d558e1c', deferential_realism_ontology__hybrid_pragmatic_reading, forecloses).
narrative_ontology:cs_axiom('c861ed65-86a3-4700-a3cc-9f9b1d558e1c', foundational, classification_is_normative_declaration).
narrative_ontology:cs_axiom_status(classification_is_normative_declaration, holdable).
narrative_ontology:cs_axiom_grounding('c861ed65-86a3-4700-a3cc-9f9b1d558e1c', classification_is_normative_declaration, conventional).
narrative_ontology:cs_axiom('c861ed65-86a3-4700-a3cc-9f9b1d558e1c', secondary, typology_value_is_persuasive_power).
narrative_ontology:cs_axiom_status(typology_value_is_persuasive_power, holdable).
narrative_ontology:cs_axiom_grounding('c861ed65-86a3-4700-a3cc-9f9b1d558e1c', typology_value_is_persuasive_power, instrumental).
narrative_ontology:cs_reference_frame('c861ed65-86a3-4700-a3cc-9f9b1d558e1c', normative_critique_framework).
narrative_ontology:cs_drift_state('c861ed65-86a3-4700-a3cc-9f9b1d558e1c', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c861ed65-86a3-4700-a3cc-9f9b1d558e1c', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__rhetorical_scaffold_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__rhetorical_scaffold_reading, advocacy_groups).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, illegitimate_beneficiaries_of_policy).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, status_quo_defenders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(deferential_realism_ontology__rhetorical_scaffold_reading, policy_makers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Utilize the DR typology as a framework to articulate and organize critiques of existing policies, framing them as 'snares' or 'tangled ropes' to highlight their extractive nature. They gain persuasive leverage and intellectual coherence from this vocabulary.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_critics, agenda_setter,
    organized, biographical, mobile, national).

% Adopt the DR typology to strengthen their arguments against policies they deem unjust or extractive, using its categories to mobilize public opinion and pressure policymakers. The framework provides a shared language for their campaigns.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, advocacy_groups, beneficiary,
    organized, biographical, mobile, local).

% Are the targets of the critique enabled by the DR typology. They face challenges to their legitimacy and the mechanisms from which they benefit, incurring costs in defending the status quo or adapting to new policy demands.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, illegitimate_beneficiaries_of_policy, payer,
    institutional, generational, constrained, national).

% Actively resist the application of the DR typology to existing policies, arguing for the necessity or benign nature of the arrangements being critiqued. They bear the cost of defending their positions against a rhetorically potent framework.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, status_quo_defenders, payer,
    powerful, biographical, constrained, national).

% Analyze the use and impact of the DR typology in policy debates, evaluating its effectiveness as a rhetorical tool and its influence on institutional design. They do not directly participate in the critique but study its dynamics.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, academic_observers, observer,
    analytical, generational, analytical, global).

% Are subjected to pressure from policy critics and advocacy groups employing the DR typology. They must respond to the critiques, either by defending existing policies, proposing reforms, or facing political consequences.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__rhetorical_scaffold_reading, policy_makers, payer,
    institutional, immediate, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates critical discourse and advocacy efforts by providing a shared, normatively charged vocabulary for identifying, analyzing, and challenging institutional arrangements deemed illegitimate or extractive.
% TRANSFER_FUNCTION: Transfers legitimacy (or lack thereof) from existing policy mechanisms to the critique, aiming to shift public opinion, policy discourse, and ultimately, policy outcomes. It also transfers attention and resources to the critique itself.
% ABSENT_VOICES: Those who benefit from the status quo without acknowledging the normative basis of the critique; they would argue the typology is an objective diagnostic instrument being misapplied, rather than a rhetorical tool.
% DISAPPEARANCE_RATIONALE: If the DR typology as a rhetorical scaffold vanished overnight, the landscape of policy critique would lose a powerful, shared vocabulary. Critics would struggle to frame complex institutional problems in a coherent, persuasive manner, and advocacy efforts would be less coordinated, leading to a less effective challenge to illegitimate beneficiaries.
% FOUNDING_PROBLEM: The lack of a clear, shared, and normatively charged vocabulary to effectively critique and challenge extractive or illegitimate institutional arrangements, leading to diffuse and uncoordinated opposition.
% FOUNDING_PROBLEM_CORROBORATION: Academic discourse in critical theory, social movement studies, and public policy analysis consistently highlights the need for robust frameworks to challenge power structures. The ongoing use and adaptation of the DR typology by diverse advocacy groups further corroborates the persistence of this problem.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__rhetorical_scaffold_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__rhetorical_scaffold_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(deferential_realism_ontology__rhetorical_scaffold_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__rhetorical_scaffold_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(deferential_realism_ontology__rhetorical_scaffold_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(deferential_realism_ontology__rhetorical_scaffold_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.60) reflects the framework's capacity to 'extract' attention, commitment, and action from its users (critics) and impose costs on its targets (illegitimate beneficiaries). Suppression (0.20) is low because the framework itself is a conceptual tool, not an enforced physical or legal barrier; it doesn't suppress alternatives but offers a new way to frame them. Theater ratio (0.40) is moderate, acknowledging the inherent performative and framing aspects of a rhetorical tool. Accessibility collapse (0.30) is low as it doesn't physically remove options, but resistance (0.70) is high because it's explicitly designed to challenge existing power structures. The claimed type is 'scaffold' because it serves as a temporary support for a transition from an illegitimate status quo to a more just arrangement, with a 'sunset clause' implying that its specific application to a policy might cease once the critique is successful.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between those who view the DR typology as an objective diagnostic instrument (as in the 'immutable diagnostic' reading) and those, like this reading, who see it as a normatively constructed rhetorical tool. This reading asserts that the 'snare' classification is a declaration, not a discovery, fundamentally altering how the framework's outputs are understood and applied.
 *
 * DIRECTIONALITY LOGIC:
 *   Policy critics and advocacy groups are beneficiaries and agenda-setters, gaining a powerful tool for their work. Illegitimate beneficiaries and status quo defenders are payers, bearing the costs of the critique and facing challenges to their legitimacy. Policymakers are also payers, as they must respond to the pressure generated by the framework's application. Academic observers maintain an analytical distance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    classification_objectivity_vs_normativity,
    'To what extent can any classification within the DR typology be considered an objective discovery, versus a normative declaration driven by a specific ethical or political stance?',
    'Analysis of classification disputes: if consensus on classification consistently correlates with shared normative commitments rather than purely empirical evidence, it supports the ''normative declaration'' view.',
    'If classification is primarily normative, the framework''s utility shifts from diagnostic accuracy to rhetorical efficacy, impacting its application in policy and academic discourse.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(classification_objectivity_vs_normativity, conceptual, 'The fundamental nature of DR typology classification.').

omega_variable(
    typology_persuasive_power_limits,
    'What are the limits of the DR typology''s persuasive power in influencing policy outcomes, particularly when facing entrenched interests or alternative rhetorical frameworks?',
    'Empirical case studies of policy debates where the DR typology was applied: measure changes in public opinion, policy proposals, and legislative outcomes.',
    'If persuasive power is limited, the ''rhetorical scaffold'' reading might overestimate the framework''s practical impact, suggesting a need for additional tools or strategies beyond normative critique.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(typology_persuasive_power_limits, empirical, 'Efficacy of the typology as a rhetorical tool.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__rhetorical_scaffold_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 5, 0.34).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement(defe_tr_t15, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 15, 0.39).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__rhetorical_scaffold_reading, theater_ratio, 20, 0.4).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 10, 0.57).
narrative_ontology:measurement(defe_be_t15, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 15, 0.59).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__rhetorical_scaffold_reading, base_extractiveness, 20, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 5, 0.2).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(defe_su_t15, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 15, 0.2).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__rhetorical_scaffold_reading, suppression_requirement, 20, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__rhetorical_scaffold_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
