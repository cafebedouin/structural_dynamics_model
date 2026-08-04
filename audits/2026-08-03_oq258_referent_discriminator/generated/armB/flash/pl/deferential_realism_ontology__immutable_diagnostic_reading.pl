% ============================================================================
% CONSTRAINT STORY: deferential_realism_ontology__immutable_diagnostic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_deferential_realism_ontology__immutable_diagnostic_reading, []).

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
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Deferential Realism Ontology: Immutable Diagnostic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint represents the 'immutable diagnostic' reading of the
 *   Deferential Realism (DR) ontology, which posits that constraint types
 *   (mountain, snare, etc.) are fixed, discoverable referents in the social
 *   world, and misclassification is an observational error. This reading
 *   emphasizes the framework's role as an objective diagnostic instrument,
 *   with classification disputes resolved by appealing to observable metrics
 *   and structural properties. It is one of three competing readings of the
 *   core DR ontology.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.1).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.7).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, mountain).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Deferential Realism Ontology: Immutable Diagnostic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:emerges_naturally(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, 'f3019fbf-8dec-4c6d-899e-979d048a8b44').
narrative_ontology:cs_kernel_codification('f3019fbf-8dec-4c6d-899e-979d048a8b44', formalized).
narrative_ontology:cs_authority_grounding('f3019fbf-8dec-4c6d-899e-979d048a8b44', expertise).
narrative_ontology:cs_interpretation_layer_present('f3019fbf-8dec-4c6d-899e-979d048a8b44').
narrative_ontology:cs_reading_relation('f3019fbf-8dec-4c6d-899e-979d048a8b44', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('f3019fbf-8dec-4c6d-899e-979d048a8b44', deferential_realism_ontology__hybrid_pragmatic_reading, forecloses).
narrative_ontology:cs_axiom('f3019fbf-8dec-4c6d-899e-979d048a8b44', foundational, typology_referents_are_fixed).
narrative_ontology:cs_axiom_status(typology_referents_are_fixed, holdable).
narrative_ontology:cs_axiom_grounding('f3019fbf-8dec-4c6d-899e-979d048a8b44', typology_referents_are_fixed, deontological).
narrative_ontology:cs_axiom('f3019fbf-8dec-4c6d-899e-979d048a8b44', foundational, misclassification_is_observational_error).
narrative_ontology:cs_axiom_status(misclassification_is_observational_error, holdable).
narrative_ontology:cs_axiom_grounding('f3019fbf-8dec-4c6d-899e-979d048a8b44', misclassification_is_observational_error, empirically_contingent).
narrative_ontology:cs_reference_frame('f3019fbf-8dec-4c6d-899e-979d048a8b44', objective_diagnostic_science).
narrative_ontology:cs_drift_state('f3019fbf-8dec-4c6d-899e-979d048a8b44', contemporary_postmodern_critique, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('f3019fbf-8dec-4c6d-899e-979d048a8b44', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, dr_framework_analysts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, policy_advocates).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, objective_social_science).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, epistemic_realism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the primary users and developers of the Deferential Realism framework. They benefit from the clarity and perceived objectivity of a fixed, diagnostic typology, which allows them to apply the framework consistently across diverse domains and claim epistemic authority for its classifications. Their professional identity is tied to the framework's integrity.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, dr_framework_analysts, beneficiary,
    analytical, generational, identity_locked, global).

% Scholars from critical theory traditions who would argue that all typologies are inherently normative and performative, and that claiming 'immutable referents' for social constructs masks power relations. They are excluded from the framework's internal discourse by its foundational premises.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, critical_theorists, excluded,
    moderate, generational, mobile, global).

% Advocates who seek to use the DR framework to critique existing policies. They find their normative arguments constrained by the 'immutable diagnostic' reading, as it forces them to frame their critiques as 'observational errors' rather than legitimate contestations of the constraint's purpose or beneficiaries.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, policy_advocates, payer,
    organized, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a consistent, shared vocabulary and methodology for analyzing social constraints, enabling intersubjective agreement on their structural properties and classification.
% TRANSFER_FUNCTION: Transfers epistemic authority and analytical clarity to users of the framework, by asserting that classifications are objective and discoverable, rather than subject to interpretation or normative debate.
% ABSENT_VOICES: Critical theorists and post-structuralists who would challenge the very premise of objective, immutable referents for social phenomena. Their voices are absent because the framework's foundational axioms preclude their mode of analysis.
% DISAPPEARANCE_RATIONALE: If this reading of the DR ontology vanished, the framework itself would lose its claim to diagnostic objectivity, leading to a fundamental re-evaluation of its purpose and application. Classification disputes would become explicitly normative, and the analytical community would fragment.
% FOUNDING_PROBLEM: The problem of inconsistent and normatively loaded analyses of social constraints, leading to endless debates about classification rather than structural understanding.
% FOUNDING_PROBLEM_CORROBORATION: DR framework analysts attest to the ongoing problem of analytical inconsistency in social science. External corroboration is limited, as critics argue the 'problem' is inherent to social analysis, not solvable by an 'immutable' ontology.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'f1436bd4937f864097dabaad92b27bd9b6eec212', '2026-08-03',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.1, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, ExtMetricName, E),
    domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(deferential_realism_ontology__immutable_diagnostic_reading),
    narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(deferential_realism_ontology__immutable_diagnostic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is low (0.1) because this reading primarily extracts epistemic clarity and analytical consistency, not material resources. Suppression is high (0.7) because this reading actively suppresses alternative, more interpretivist or normative framings of the typology, treating them as errors. The high accessibility_collapse (0.8) reflects that once this reading is adopted, alternative interpretive paths are largely foreclosed within the framework. Resistance is low (0.15) because internal dissent is framed as observational error, and external critics are largely excluded from the discourse.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of DR framework analysts, this reading is a genuine mountain of epistemic clarity. From the perspective of critical theorists, it is a snare of conceptual capture, suppressing alternative modes of inquiry. The engine's classification will reflect the structural properties, not the self-perception of the beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   DR framework analysts are beneficiaries, gaining epistemic authority and a clear methodological path. Policy advocates are payers, as their normative goals are constrained by the 'objective' classification. Critical theorists are excluded, as their foundational premises are incompatible with this reading's axioms.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    epistemic_vs_normative_boundary,
    'Is the distinction between ''objective observation'' and ''normative judgment'' truly immutable in social science, or is it a constructed boundary that serves specific analytical goals?',
    'Analysis of historical shifts in scientific paradigms and the sociology of knowledge, examining how ''objective'' categories are established and contested over time.',
    'If the boundary is constructed, the ''immutable diagnostic'' reading''s claim to objective classification is weakened, potentially reclassifying it from a mountain to a tangled_rope or snare for those whose interpretive freedom is suppressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epistemic_vs_normative_boundary, conceptual, 'Ambiguity of the epistemic/normative boundary in social analysis.').

omega_variable(
    natural_law_vs_conceptual_choice,
    'Is the DR typology''s ''immutable diagnostic'' nature a discovery of natural law in social systems, or a conceptual choice made by its proponents to achieve analytical consistency?',
    'Comparative analysis with other social science typologies: do they converge on the same ''immutable'' categories, or do they reveal a diversity of equally valid conceptualizations?',
    'If it''s a conceptual choice, the ''emerges_naturally'' claim is undermined, and the constraint would be reclassified from mountain to a constructed type (e.g., rope or tangled_rope), reflecting its active maintenance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_conceptual_choice, conceptual, 'Whether the typology''s immutability is discovered or constructed.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 2010, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(defe_be_t2010, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 2010, 0.08).
narrative_ontology:measurement(defe_be_t2015, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 2015, 0.09).
narrative_ontology:measurement(defe_be_t2020, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 2020, 0.1).
narrative_ontology:measurement(defe_be_t2024, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 2024, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t2010, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(defe_su_t2015, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 2015, 0.65).
narrative_ontology:measurement(defe_su_t2020, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 2020, 0.68).
narrative_ontology:measurement(defe_su_t2024, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
