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
 *   constraint_id: deferential_realism_ontology__immutable_diagnostic_reading
 *   human_readable: Deferential Realism Ontology: Immutable Diagnostic Reading
 *   domain: epistemology/normative_theory/institutional_design
 *
 * SUMMARY:
 *   This constraint represents the 'immutable diagnostic' reading of the
 *   Deferential Realism ontology. It asserts that the typology is an
 *   objective, observational instrument with fixed referents (e.g., mountains
 *   are physical invariants, snares are measurable extraction mechanisms).
 *   Misclassification is treated as an error correctable through better
 *   observation, not a matter of normative judgment or rhetorical framing.
 *   This reading emphasizes the framework's scientific rigor and diagnostic
 *   power.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.15).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.75).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, mountain).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "Deferential Realism Ontology: Immutable Diagnostic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "epistemology/normative_theory/institutional_design").

domain_priors:emerges_naturally(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, '72628394-d3fa-4964-a3cc-ebd7c12daa49').
narrative_ontology:cs_kernel_codification('72628394-d3fa-4964-a3cc-ebd7c12daa49', formalized).
narrative_ontology:cs_authority_grounding('72628394-d3fa-4964-a3cc-ebd7c12daa49', expertise).
narrative_ontology:cs_interpretation_layer_present('72628394-d3fa-4964-a3cc-ebd7c12daa49').
narrative_ontology:cs_reading_relation('72628394-d3fa-4964-a3cc-ebd7c12daa49', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('72628394-d3fa-4964-a3cc-ebd7c12daa49', deferential_realism_ontology__hybrid_pragmatic_reading, influences).
narrative_ontology:cs_axiom('72628394-d3fa-4964-a3cc-ebd7c12daa49', foundational, typology_referents_are_fixed).
narrative_ontology:cs_axiom_status(typology_referents_are_fixed, holdable).
narrative_ontology:cs_axiom_grounding('72628394-d3fa-4964-a3cc-ebd7c12daa49', typology_referents_are_fixed, empirically_contingent).
narrative_ontology:cs_axiom('72628394-d3fa-4964-a3cc-ebd7c12daa49', foundational, misclassification_is_observational_error).
narrative_ontology:cs_axiom_status(misclassification_is_observational_error, holdable).
narrative_ontology:cs_axiom_grounding('72628394-d3fa-4964-a3cc-ebd7c12daa49', misclassification_is_observational_error, empirically_contingent).
narrative_ontology:cs_reference_frame('72628394-d3fa-4964-a3cc-ebd7c12daa49', objective_diagnostic_science).
narrative_ontology:cs_drift_state('72628394-d3fa-4964-a3cc-ebd7c12daa49', contemporary_social_theory_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('72628394-d3fa-4964-a3cc-ebd7c12daa49', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_analysts).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, social_scientists).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, objective_social_science).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, empirical_falsifiability).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Proponents of this reading assert the objective, diagnostic power of the typology. They resolve classification disputes by appealing to observable metrics and treat epsilon values as discoverable facts about the world. Their professional identity is tied to the framework's scientific rigor.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_analysts, agenda_setter,
    analytical, generational, analytical, global).

% Advocates of the rhetorical_scaffold_reading, who view the typology as a normative vocabulary for critique rather than an objective diagnostic tool. Their framing is suppressed by the immutable_diagnostic_reading's insistence on fixed referents and empirical verifiability.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, rhetorical_scaffold_proponents, excluded,
    moderate, biographical, identity_locked, global).

% Advocates of the hybrid_pragmatic_reading, who seek a middle ground where some classifications are objective and others depend on normative judgments. Their nuanced position is often dismissed by the immutable_diagnostic_reading as compromising the framework's scientific integrity.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, hybrid_pragmatic_proponents, excluded,
    moderate, biographical, constrained, global).

% Academics and researchers who benefit from a framework that promises objective, empirically verifiable classifications of social constraints, aligning with traditional scientific ideals. They gain legitimacy for their work by adopting a 'diagnostic' approach.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, social_scientists, beneficiary,
    organized, generational, mobile, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, objective language and methodology for diagnosing the structural nature of social constraints, enabling consistent analysis across different contexts and researchers.
% TRANSFER_FUNCTION: Transfers epistemic authority from subjective interpretation to objective observation and measurable metrics, from contested normative claims to discoverable facts about social structures.
% ABSENT_VOICES: Proponents of more interpretivist or constructivist approaches to social science, who would argue that all social categories are inherently constructed and that 'objective' classification is a rhetorical move. They are excluded by the reading's foundational commitment to fixed referents.
% DISAPPEARANCE_RATIONALE: If this reading of the ontology vanished, the entire project of Deferential Realism would lose its claim to scientific rigor and objective diagnosis. Classification disputes would become purely normative or rhetorical, and the framework's utility as a diagnostic instrument would collapse, forcing a re-evaluation of its foundational premises.
% FOUNDING_PROBLEM: The problem of subjective and inconsistent classification of social constraints, leading to endless debate without empirical resolution, and undermining the possibility of a rigorous social science.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading within the Deferential Realism community attest to the ongoing problem of subjective interpretation in social theory. Critics from other theoretical traditions (e.g., critical theory, post-structuralism) would argue that the 'problem' itself is a construct of this reading, but within the DR framework, the problem is considered live and foundational.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(deferential_realism_ontology__immutable_diagnostic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(deferential_realism_ontology__immutable_diagnostic_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low (0.15) because this reading primarily extracts epistemic authority from alternative framings, rather than material resources. Suppression is high (0.75) because it actively suppresses alternative interpretations that would introduce normative judgment or rhetorical flexibility into the classification process. Accessibility collapse is high (0.8) as it aims to make the 'correct' classification self-evident through observation. Resistance is low (0.2) from within its own community, as its proponents largely agree on its premises. Theater ratio is low (0.1) because its claims are presented as genuinely functional and diagnostic.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading's proponents, the framework is a robust, objective tool. From the perspective of those advocating for more flexible or normatively-driven interpretations, this reading acts as a suppressive force, limiting the framework's applicability and foreclosing alternative modes of critique. The engine's classification will highlight the tension between the claimed 'mountain' status and the active suppression of alternative framings.
 *
 * DIRECTIONALITY LOGIC:
 *   Deferential Realism analysts are the primary beneficiaries and agenda-setters, as they define and enforce this reading's principles. Proponents of rival readings (rhetorical_scaffold_proponents, hybrid_pragmatic_proponents) are structurally excluded or suppressed, making them targets of the constraint's enforcement of its own epistemic boundaries. Social scientists who adopt this framework benefit from its promise of objective analysis.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading's mandate is to provide objective, diagnostic classifications. It avoids mislabeling coordination as extraction by insisting on measurable criteria for extraction, rather than subjective normative judgments. However, its high suppression of alternative framings could be seen as a form of epistemic extraction, where the 'truth' of the framework is enforced rather than discovered through open debate.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    objectivity_vs_construction,
    'Is the ''objective'' nature of the typology''s referents (e.g., epsilon values) a discoverable fact about social reality, or a constructed feature of this reading''s epistemic framework?',
    'Meta-analysis of classification disputes: if disputes consistently resolve through empirical observation, it supports discoverability; if they resolve through shifts in normative consensus or framing, it supports construction.',
    'If constructed, the constraint''s ''emerges_naturally'' claim is weakened, and its suppression of alternative framings becomes more extractive, potentially reclassifying it as a Tangled Rope or Snare from the perspective of excluded readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(objectivity_vs_construction, conceptual, 'The fundamental nature of the typology''s referents: objective or constructed.').

omega_variable(
    suppression_of_alternative_framings,
    'Is the high suppression of alternative framings (e.g., rhetorical_scaffold_reading) a necessary epistemic boundary for maintaining diagnostic rigor, or an extractive mechanism to preserve this reading''s dominance?',
    'Analysis of the epistemic costs and benefits of open vs. closed interpretive frameworks: if opening the framework leads to unresolvable ambiguity, suppression is functional; if it leads to richer insights, suppression is extractive.',
    'If extractive, the ''suppression'' metric''s contribution to effective extraction is amplified, and the ''claimed_type'' of mountain is challenged, pushing towards a Snare or Tangled Rope classification for those whose framings are suppressed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_alternative_framings, preference, 'Whether suppression of alternative framings is functional or extractive.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(defe_tr_t5, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 5, 0.09).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(defe_be_t5, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 5, 0.14).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 10, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(defe_su_t5, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 5, 0.73).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'Deferential Realism Ontology' kernel. This 'immutable diagnostic' reading asserts objective, fixed referents, influencing how other readings are perceived and legitimized within the broader framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
