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
 *   This constraint represents the 'hybrid pragmatic' reading of the
 *   Deferential Realism ontology, which posits a fixed, observational core
 *   for 'mountain' and 'rope' classifications, but a contested periphery for
 *   'tangled_rope' and 'snare' where normative judgments about legitimate
 *   beneficiaries are central to classification. This reading acknowledges
 *   the constructed nature of social constraints while retaining an empirical
 *   anchor. The classification as 'tangled_rope' reflects its dual function:
 *   it coordinates analytical discourse but also extracts a cost from those
 *   who prefer a simpler, less normatively engaged framework.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__hybrid_pragmatic_reading, 0.45).
domain_priors:suppression_score(deferential_realism_ontology__hybrid_pragmatic_reading, 0.55).
domain_priors:theater_ratio(deferential_realism_ontology__hybrid_pragmatic_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(deferential_realism_ontology__hybrid_pragmatic_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__hybrid_pragmatic_reading, tangled_rope).
narrative_ontology:human_readable(deferential_realism_ontology__hybrid_pragmatic_reading, "Deferential Realism Ontology: Hybrid Pragmatic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__hybrid_pragmatic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__hybrid_pragmatic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__hybrid_pragmatic_reading, '9d410762-edef-4912-a47b-ccfb8ec7e664').
narrative_ontology:cs_kernel_codification('9d410762-edef-4912-a47b-ccfb8ec7e664', distributed).
narrative_ontology:cs_authority_grounding('9d410762-edef-4912-a47b-ccfb8ec7e664', expertise).
narrative_ontology:cs_interpretation_layer_present('9d410762-edef-4912-a47b-ccfb8ec7e664').
narrative_ontology:cs_reading_relation('9d410762-edef-4912-a47b-ccfb8ec7e664', deferential_realism_ontology__immutable_diagnostic_reading, coexists_with).
narrative_ontology:cs_reading_relation('9d410762-edef-4912-a47b-ccfb8ec7e664', deferential_realism_ontology__rhetorical_scaffold_reading, coexists_with).
narrative_ontology:cs_axiom('9d410762-edef-4912-a47b-ccfb8ec7e664', foundational, normative_judgment_is_integral_to_social_constraint_classification).
narrative_ontology:cs_axiom_status(normative_judgment_is_integral_to_social_constraint_classification, holdable).
narrative_ontology:cs_axiom_grounding('9d410762-edef-4912-a47b-ccfb8ec7e664', normative_judgment_is_integral_to_social_constraint_classification, deontological).
narrative_ontology:cs_axiom('9d410762-edef-4912-a47b-ccfb8ec7e664', foundational, physical_and_coordination_constraints_are_objectively_classifiable).
narrative_ontology:cs_axiom_status(physical_and_coordination_constraints_are_objectively_classifiable, holdable).
narrative_ontology:cs_axiom_grounding('9d410762-edef-4912-a47b-ccfb8ec7e664', physical_and_coordination_constraints_are_objectively_classifiable, empirically_contingent).
narrative_ontology:cs_reference_frame('9d410762-edef-4912-a47b-ccfb8ec7e664', integrated_descriptive_normative_analysis).
narrative_ontology:cs_drift_state('9d410762-edef-4912-a47b-ccfb8ec7e664', contemporary_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('9d410762-edef-4912-a47b-ccfb8ec7e664', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__hybrid_pragmatic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, analytical_observers).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__hybrid_pragmatic_reading, policy_reformers).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, status_quo_defenders).
narrative_ontology:constraint_victim(deferential_realism_ontology__hybrid_pragmatic_reading, naive_observers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from a framework that allows for both objective analysis of physical/coordination constraints and critical normative evaluation of extractive ones. Uses the framework to understand complex systems.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, analytical_observers, beneficiary,
    analytical, generational, analytical, global).

% Uses the hybrid nature of the typology to argue for policy changes, leveraging the 'snare' classification to highlight illegitimate extraction and mobilize support for reform. Benefits from the framework's capacity for normative critique.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, policy_reformers, beneficiary,
    organized, biographical, mobile, national).

% Faces challenges to their preferred classifications, particularly for 'tangled_rope' and 'snare' constraints they benefit from. Must actively defend their framing against normative critiques enabled by this reading of the ontology.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, status_quo_defenders, payer,
    institutional, generational, constrained, global).

% Struggles with the ambiguity and contestation inherent in the peripheral classifications, seeking a purely objective, diagnostic tool. Finds the hybrid nature of the framework unsettling or difficult to apply consistently.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, naive_observers, payer,
    moderate, immediate, identity_locked, local).

% Advocates for a purely observational, fixed-referent interpretation of the typology, rejecting the role of normative judgment in classification. This reading of the ontology directly challenges their foundational premise.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, immutable_diagnostic_proponents, excluded,
    powerful, generational, constrained, global).

% Argues the typology is primarily a tool for persuasion and critique, where 'snare' is a declaration rather than a discovery. This reading, by positing a fixed core, limits the purely rhetorical flexibility they seek.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__hybrid_pragmatic_reading, rhetorical_scaffold_proponents, excluded,
    powerful, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared vocabulary for analyzing constraints, allowing different communities to engage with both the objective and normative aspects of institutional design and power dynamics.
% TRANSFER_FUNCTION: Transfers analytical clarity and critical leverage to those seeking to understand and reform complex systems, while imposing a burden of justification on those who benefit from contested constraints.
% ABSENT_VOICES: Proponents of purely immutable or purely rhetorical readings of the ontology are structurally excluded from the core premise of this hybrid reading; they would argue for a simpler, less ambiguous framework.
% DISAPPEARANCE_RATIONALE: If this hybrid reading vanished, the discourse around constraint classification would fragment. Analytical observers would lose a nuanced tool, policy reformers would lose a basis for critique, and the debate would likely revert to simpler, less productive dichotomies (e.g., purely objective vs. purely subjective).
% FOUNDING_PROBLEM: The problem of reconciling the objective reality of physical and coordination constraints with the constructed, normatively laden nature of extractive social arrangements within a single analytical framework.
% FOUNDING_PROBLEM_CORROBORATION: Philosophers of science and institutional economists, outside the immediate proponents of the framework, corroborate the ongoing challenge of integrating descriptive and normative analysis in complex systems. The persistence of debates over 'naturalness' in social structures attests to the problem's live status.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__hybrid_pragmatic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__hybrid_pragmatic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__hybrid_pragmatic_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(deferential_realism_ontology__hybrid_pragmatic_reading, 'none', 1).

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
 *   The extractiveness (0.45) is moderate, reflecting the intellectual effort and discomfort required to navigate a framework that blends objective and normative claims. Suppression (0.55) is also moderate, as this reading actively pushes back against attempts to simplify the ontology into a purely diagnostic or purely rhetorical tool. Theater ratio is low (0.2) because the framework is actively used for analysis and critique, not merely for performative maintenance. The metrics reflect the ongoing intellectual and normative work required to maintain this hybrid position.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of analytical observers, this reading is a valuable and necessary tool for understanding complex realities. From the perspective of status quo defenders, it is a source of unwanted critique and instability. The engine's classification will reflect this tension, likely highlighting the extractive aspects for those who resist its normative implications.
 *
 * DIRECTIONALITY LOGIC:
 *   Analytical observers and policy reformers are beneficiaries, as this reading provides them with a powerful and nuanced tool. Status quo defenders and naive observers are payers, as they must contend with the framework's critical implications or its inherent complexity. Proponents of alternative readings are excluded, as their core premises are challenged by this hybrid approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    normative_judgment_objectivity,
    'To what extent can normative judgments about ''legitimate beneficiaries'' be made objectively or intersubjectively, rather than being purely subjective preferences?',
    'Development of robust, cross-cultural ethical frameworks for evaluating beneficiary legitimacy, or empirical studies on convergence of normative judgments in diverse populations.',
    'If normative judgments can be shown to have a high degree of intersubjective agreement, the ''tangled_rope'' aspect of this reading (its reliance on contested normative judgment) would shift towards a more ''rope''-like coordination function. If they remain purely subjective, the ''snare'' potential increases.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normative_judgment_objectivity, conceptual, 'The epistemic status of normative judgments in classifying peripheral constraints.').

omega_variable(
    immutable_diagnostic_reading_challenge,
    'Does the ''immutable_diagnostic_reading'' of the ontology (a sibling reading) logically foreclose this ''hybrid_pragmatic_reading'' by asserting a purely objective classification for all constraint types?',
    'Formal logical analysis of the axioms of both readings to determine if they are mutually exclusive within a single coherent framework.',
    'If foreclosed, this reading would be deemed internally inconsistent or incompatible with a more fundamental understanding of the ontology. If not, both readings can coexist as competing interpretations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(immutable_diagnostic_reading_challenge, conceptual, 'Logical compatibility with the immutable diagnostic reading.').

omega_variable(
    rhetorical_scaffold_reading_influence,
    'How does the ''rhetorical_scaffold_reading'' (a sibling reading) influence the practical application and reception of this ''hybrid_pragmatic_reading''?',
    'Empirical study of how different communities adopt and apply the typology, and how the ''rhetorical'' framing affects the perceived legitimacy or utility of the ''hybrid'' approach.',
    'If the rhetorical reading gains dominance, it could undermine the perceived analytical rigor of the hybrid reading, pushing it towards being seen as merely another ''persuasive'' tool rather than a robust analytical framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rhetorical_scaffold_reading_influence, empirical, 'Influence of the rhetorical reading on the hybrid reading''s perceived legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__hybrid_pragmatic_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 5, 0.2).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__hybrid_pragmatic_reading, theater_ratio, 10, 0.2).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 5, 0.43).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__hybrid_pragmatic_reading, base_extractiveness, 10, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 5, 0.53).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__hybrid_pragmatic_reading, suppression_requirement, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__hybrid_pragmatic_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
