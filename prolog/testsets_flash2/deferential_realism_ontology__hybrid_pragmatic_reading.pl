% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__hybrid_pragmatic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__hybrid_pragmatic_reading, []).

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
 *   constraint_id: deferential_realism_ontology__hybrid_pragmatic_reading
 *   human_readable: Deferential Realism Ontology: Hybrid Pragmatic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint story describes the 'hybrid pragmatic reading' of the
 *   Deferential Realism (DR) ontology. This reading posits that the DR
 *   typology has a fixed, observational core (mountains, ropes) and a
 *   contested periphery (tangled_ropes, snares) where classification
 *   inherently involves normative judgments about legitimate beneficiaries.
 *   This is one of three readings of the 'deferential_realism_ontology'
 *   kernel.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, 0.45).
domain_priors:suppression_score(deferential_realism_ontology__hybrid_pragmatic_reading, 0.55).
domain_priors:theater_ratio(deferential_realism_ontology__hybrid_pragmatic_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Deferential Realism Ontology: Hybrid Pragmatic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, 'c9e0c9fd-7c3e-40fb-80ea-b5d8bf79de99').
narrative_ontology:cs_kernel_codification('c9e0c9fd-7c3e-40fb-80ea-b5d8bf79de99', formalized).
narrative_ontology:cs_authority_grounding('c9e0c9fd-7c3e-40fb-80ea-b5d8bf79de99', expertise).
narrative_ontology:cs_interpretation_layer_present('c9e0c9fd-7c3e-40fb-80ea-b5d8bf79de99').
narrative_ontology:cs_reading_relation('c9e0c9fd-7c3e-40fb-80ea-b5d8bf79de99', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('c9e0c9fd-7c3e-40fb-80ea-b5d8bf79de99', deferential_realism_ontology__rhetorical_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('c9e0c9fd-7c3e-40fb-80ea-b5d8bf79de99', foundational, constraint_typology_has_fixed_core).
narrative_ontology:cs_axiom_status(constraint_typology_has_fixed_core, holdable).
narrative_ontology:cs_axiom_grounding('c9e0c9fd-7c3e-40fb-80ea-b5d8bf79de99', constraint_typology_has_fixed_core, empirically_contingent).
narrative_ontology:cs_axiom('c9e0c9fd-7c3e-40fb-80ea-b5d8bf79de99', foundational, peripheral_classification_requires_normative_judgment).
narrative_ontology:cs_axiom_status(peripheral_classification_requires_normative_judgment, holdable).
narrative_ontology:cs_axiom_grounding('c9e0c9fd-7c3e-40fb-80ea-b5d8bf79de99', peripheral_classification_requires_normative_judgment, deontological).
narrative_ontology:cs_reference_frame('c9e0c9fd-7c3e-40fb-80ea-b5d8bf79de99', integrated_descriptive_normative_analysis).
narrative_ontology:cs_drift_state('c9e0c9fd-7c3e-40fb-80ea-b5d8bf79de99', contemporary_interdisciplinary_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c9e0c9fd-7c3e-40fb-80ea-b5d8bf79de99', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_analysts).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, naive_observational_theorists).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, pure_rhetorical_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_designers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applies the DR framework, navigating the distinction between observational and normative classification. Benefits from the framework's utility in dissecting complex constraints, but also bears the burden of justifying its hybrid nature.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_analysts, agenda_setter,
    analytical, generational, mobile, global).

% Struggles to reconcile the DR framework's normative elements with a purely observational epistemology. Pays in conceptual friction and potential misclassification if they insist on a 'facts-only' approach to all constraint types.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, naive_observational_theorists, payer,
    moderate, biographical, constrained, global).

% Finds the DR framework's insistence on an observational core (mountains, ropes) to be a constraint on their ability to use the typology purely for normative critique. Pays in reduced flexibility for rhetorical deployment.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, pure_rhetorical_advocates, payer,
    moderate, biographical, constrained, global).

% Uses the DR framework to design institutions that account for both fixed constraints and contestable, normatively-laden ones. Benefits from the nuanced understanding of constraint persistence and legitimacy.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, institutional_designers, beneficiary,
    powerful, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates understanding of constraint persistence by distinguishing between empirically verifiable constraints (mountains, ropes) and those whose classification depends on normative judgments (tangled_ropes, snares), allowing for a more robust analysis of institutional design.
% TRANSFER_FUNCTION: Transfers conceptual clarity and analytical precision to users of the DR framework, enabling them to navigate the complex interplay of descriptive and normative claims in constraint analysis. It also transfers the burden of making explicit normative judgments for peripheral classifications.
% ABSENT_VOICES: Those who insist on a purely descriptive or purely normative approach to all constraints are implicitly excluded; they would argue for a simpler, more unified classification system, but their perspectives are not fully integrated into this hybrid reading.
% DISAPPEARANCE_RATIONALE: If this hybrid reading of the DR ontology vanished, the analytical community would lose a crucial tool for distinguishing between different modes of constraint persistence and legitimacy. The discourse around institutional design and critique would become less nuanced, potentially leading to mischaracterizations of constraints as purely natural or purely constructed.
% FOUNDING_PROBLEM: The problem of classifying constraints that exhibit both objective and subjective characteristics, where a purely observational or purely normative approach fails to capture their full nature.
% FOUNDING_PROBLEM_CORROBORATION: Philosophers of science and institutional theorists outside the immediate DR community attest to the ongoing challenge of integrating descriptive and normative elements in social theory. The persistence of debates around 'natural' vs. 'socially constructed' phenomena corroborates the live status of this problem.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(deferential_realism_ontology__hybrid_pragmatic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).
:- end_tests(deferential_realism_ontology__hybrid_pragmatic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.45) reflects the conceptual cost imposed on those who prefer a simpler, unified classification, as they must engage with the hybrid nature. Suppression (0.55) is moderate, as this reading requires active intellectual effort to maintain the distinction and resist oversimplification from either purely descriptive or purely normative camps. The claimed type is 'tangled_rope' because it offers a genuine coordination function (nuanced classification) but also involves an 'extraction' of conceptual simplicity from those who prefer a monolithic view, requiring active intellectual enforcement to hold.
 *
 * PERSPECTIVAL GAP:
 *   Analysts who adopt this reading benefit from its nuanced understanding, while those who prefer a purely observational or purely rhetorical approach experience conceptual friction. The 'agenda_setter' (DR analysts) actively maintains this distinction, while 'payers' (naive observational theorists, pure rhetorical advocates) bear the cost of adapting their frameworks.
 *
 * DIRECTIONALITY LOGIC:
 *   Deferential Realism analysts are beneficiaries as they gain a more robust analytical tool. Naive observational theorists and pure rhetorical advocates are payers, as they must adjust their conceptual frameworks to accommodate the hybrid nature of the ontology. Institutional designers are also beneficiaries, as the framework provides a more realistic basis for their work.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the hybrid nature of the DR ontology as either a purely observational 'rope' (ignoring the normative contestation) or a purely rhetorical 'snare' (ignoring the fixed core). By acknowledging both coordination and extraction, it accurately captures the ongoing intellectual work required to maintain this nuanced understanding.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_judgment_objectivity,
    'To what extent can the ''normative judgments about legitimate beneficiaries'' for peripheral classifications be made objectively or intersubjectively?',
    'Development of explicit, shared criteria for ''legitimate beneficiaries'' that achieve broad consensus across diverse analytical communities.',
    'If such criteria are established, the ''tangled_rope'' aspect of the periphery might shift towards a ''rope'' or even ''mountain'' for those specific criteria, reducing the perceived extraction. If not, the contestation remains, reinforcing the ''tangled_rope'' or ''snare'' classification for the periphery.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_judgment_objectivity, conceptual, 'Ambiguity regarding the objectivity of normative judgments in constraint classification.').

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine ''hybrid pragmatic reading'' of the DR ontology, or is it merely a temporary compromise between the ''immutable diagnostic'' and ''rhetorical scaffold'' readings?',
    'Longitudinal study of the DR framework''s application and evolution: if the hybrid approach stabilizes and generates novel insights beyond mere compromise, it is a distinct reading. If it consistently collapses into one of the other two, it is a transient state.',
    'If confirmed as a distinct reading, it strengthens the framework''s capacity for nuanced analysis. If found to be a compromise, its long-term stability and utility are diminished, potentially shifting its classification towards a ''scaffold'' or ''piton'' for a transitional phase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'Uncertainty about the true nature and stability of this specific reading within the DR ontology.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__hybrid_pragmatic_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 5, 0.55).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__hybrid_pragmatic_reading, information_standard).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'deferential_realism_ontology' kernel. It focuses on the hybrid nature of the typology, distinguishing it from purely observational or purely rhetorical interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
