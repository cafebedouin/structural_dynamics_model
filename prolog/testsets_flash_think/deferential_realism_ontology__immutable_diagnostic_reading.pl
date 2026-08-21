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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   human_readable: DR Ontology: Immutable Diagnostic Reading
 *   domain: Epistemology/Normative Theory/Institutional Design
 *
 * SUMMARY:
 *   This constraint represents the 'immutable diagnostic' reading of the
 *   Deferential Realism ontology, which posits the typology as an objective,
 *   observational instrument with fixed referents. Mountains are seen as
 *   physical invariants, snares as measurable extraction mechanisms, and
 *   misclassification as an error correctable through better observation.
 *   This reading emphasizes the discoverable nature of constraint types and
 *   the suppression of alternative, more interpretive framings.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(deferential_realism_ontology__immutable_diagnostic_reading, 0.15).
domain_priors:suppression_score(deferential_realism_ontology__immutable_diagnostic_reading, 0.88).
domain_priors:theater_ratio(deferential_realism_ontology__immutable_diagnostic_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(deferential_realism_ontology__immutable_diagnostic_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(deferential_realism_ontology__immutable_diagnostic_reading, mountain).
narrative_ontology:human_readable(deferential_realism_ontology__immutable_diagnostic_reading, "DR Ontology: Immutable Diagnostic Reading").
narrative_ontology:topic_domain(deferential_realism_ontology__immutable_diagnostic_reading, "Epistemology/Normative Theory/Institutional Design").

domain_priors:requires_active_enforcement(deferential_realism_ontology__immutable_diagnostic_reading).
domain_priors:emerges_naturally(deferential_realism_ontology__immutable_diagnostic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(deferential_realism_ontology__immutable_diagnostic_reading, 'ee4eb9a8-7ff6-4b07-96c7-4c1b0d7a3505').
narrative_ontology:cs_kernel_codification('ee4eb9a8-7ff6-4b07-96c7-4c1b0d7a3505', formalized).
narrative_ontology:cs_authority_grounding('ee4eb9a8-7ff6-4b07-96c7-4c1b0d7a3505', expertise).
narrative_ontology:cs_interpretation_layer_present('ee4eb9a8-7ff6-4b07-96c7-4c1b0d7a3505').
narrative_ontology:cs_reading_relation('ee4eb9a8-7ff6-4b07-96c7-4c1b0d7a3505', deferential_realism_ontology__rhetorical_scaffold_reading, forecloses).
narrative_ontology:cs_reading_relation('ee4eb9a8-7ff6-4b07-96c7-4c1b0d7a3505', deferential_realism_ontology__hybrid_pragmatic_reading, forecloses).
narrative_ontology:cs_axiom('ee4eb9a8-7ff6-4b07-96c7-4c1b0d7a3505', foundational, objective_classification_is_possible).
narrative_ontology:cs_axiom_status(objective_classification_is_possible, holdable).
narrative_ontology:cs_axiom_grounding('ee4eb9a8-7ff6-4b07-96c7-4c1b0d7a3505', objective_classification_is_possible, empirically_contingent).
narrative_ontology:cs_axiom('ee4eb9a8-7ff6-4b07-96c7-4c1b0d7a3505', foundational, epsilon_values_are_discoverable).
narrative_ontology:cs_axiom_status(epsilon_values_are_discoverable, holdable).
narrative_ontology:cs_axiom_grounding('ee4eb9a8-7ff6-4b07-96c7-4c1b0d7a3505', epsilon_values_are_discoverable, empirically_contingent).
narrative_ontology:cs_reference_frame('ee4eb9a8-7ff6-4b07-96c7-4c1b0d7a3505', scientific_objectivity_paradigm).
narrative_ontology:cs_drift_state('ee4eb9a8-7ff6-4b07-96c7-4c1b0d7a3505', contemporary_philosophical_discourse, gap(stable, minor, false)).
narrative_ontology:cs_created_at('ee4eb9a8-7ff6-4b07-96c7-4c1b0d7a3505', '').
narrative_ontology:cs_kernel_id(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_analysts).
narrative_ontology:constraint_victim(deferential_realism_ontology__immutable_diagnostic_reading, alternative_typology_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(deferential_realism_ontology__immutable_diagnostic_reading, institutional_designers).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, objective_social_science_ideal).
narrative_ontology:constraint_vindicates(deferential_realism_ontology__immutable_diagnostic_reading, epistemic_realism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These are the proponents and practitioners of the Deferential Realism framework who adhere to this specific reading. They benefit from the clarity and authority derived from an objective, diagnostic classification system. Their professional identity is deeply tied to the framework's perceived immutability and diagnostic power.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_analysts, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_analysts, beneficiary).

% These are scholars and practitioners who advocate for alternative, more flexible, or normatively-driven typologies of social constraints. Their framings are suppressed or dismissed as less rigorous by the immutable diagnostic reading, forcing them to either conform or operate outside the dominant discourse.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, alternative_typology_advocates, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(deferential_realism_ontology__immutable_diagnostic_reading, alternative_typology_advocates, excluded).

% These actors seek clear, objective tools for understanding and designing institutions. They benefit from a typology that claims to provide fixed, discoverable classifications, as it offers a seemingly stable foundation for policy and intervention, reducing ambiguity in their work.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, institutional_designers, beneficiary,
    powerful, generational, mobile, national).

% These are external observers who analyze the philosophical underpinnings and implications of the Deferential Realism ontology. They critically assess its claims of objectivity and immutability, often highlighting its internal tensions or external challenges, but are not directly subject to its enforcement.
narrative_ontology:constraint_stakeholder(deferential_realism_ontology__immutable_diagnostic_reading, philosophical_critics, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_analysts).
narrative_ontology:fixing_cost_class(deferential_realism_ontology__immutable_diagnostic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared, purportedly objective, and stable framework for classifying social constraints, enabling consistent analysis and communication across different domains and researchers.
% TRANSFER_FUNCTION: Transfers epistemic authority and analytical clarity to those who adopt and apply this reading of the ontology, while imposing a cost of intellectual conformity on those who propose alternative, more fluid, or normatively-indexed classifications.
% ABSENT_VOICES: Advocates for purely constructivist or post-structuralist approaches to social phenomena are largely excluded from the conversation, as their fundamental premises are incompatible with the immutable diagnostic reading's claims of objective classification.
% DISAPPEARANCE_RATIONALE: If this reading of the ontology vanished, the field of constraint analysis would lose a foundational claim of objective classification. This would lead to a proliferation of interpretive frameworks, a re-evaluation of existing classifications, and a significant shift in how social constraints are understood and addressed, particularly by institutional designers.
% FOUNDING_PROBLEM: The problem of subjective, arbitrary, or ideologically-driven classification of social constraints, leading to inconsistent analysis and an inability to identify genuinely fixed or extractive structures.
% FOUNDING_PROBLEM_CORROBORATION: Even philosophical critics acknowledge the historical and ongoing challenge of arbitrary classification in social theory, though they dispute whether the immutable diagnostic reading provides a genuine solution or merely a new form of epistemic closure. The problem itself is widely recognized across various schools of thought.
narrative_ontology:disappearance_verdict(deferential_realism_ontology__immutable_diagnostic_reading, world_rearranges).
narrative_ontology:founding_problem_status(deferential_realism_ontology__immutable_diagnostic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(deferential_realism_ontology__immutable_diagnostic_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The extractiveness of this constraint (the ontology itself) is low because it claims to be a neutral diagnostic tool, not an extractive mechanism. However, its suppression is very high (0.88) because it actively dismisses and forecloses alternative epistemological framings that challenge its claims of objectivity and fixed referents. The accessibility collapse is high (0.85) as it claims to provide the 'correct' way to understand constraints, making other approaches seem less viable. Resistance is moderate (0.40) due to ongoing philosophical debates. Theater ratio is low (0.10) as it purports to be purely functional and diagnostic, with minimal performative elements.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of DR analysts, the ontology is a robust, objective tool that clarifies the world. From the perspective of alternative typology advocates, it is a rigid, exclusionary framework that suppresses legitimate alternative insights. The engine's classification will highlight this divergence, particularly the high suppression inherent in maintaining the 'immutable diagnostic' claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Deferential Realism analysts are beneficiaries as they gain epistemic authority and a clear framework for their work. Alternative typology advocates are victims, as their approaches are suppressed and delegitimized. Institutional designers benefit from the perceived stability and objectivity of the framework. Philosophical critics act as observers, analyzing its claims without being directly subject to its internal enforcement.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint truly an ''immutable diagnostic instrument'' or is its perceived immutability a product of active enforcement and suppression of alternative framings?',
    'Analysis of the historical evolution of the DR framework, focusing on instances where classifications were contested and how those contests were resolved (e.g., by empirical evidence vs. rhetorical power).',
    'If its immutability is primarily enforced, the constraint''s effective extractiveness and suppression would be higher, potentially reclassifying it from a Mountain (epistemological) to a Tangled Rope or Snare (epistemological).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'Ambiguity between objective diagnostic and enforced epistemic framework.').

omega_variable(
    classification_objectivity_ambiguity,
    'Are the epsilon values and constraint types truly discoverable properties of the world, or are they partly constructed through the interpretive lens of the framework itself?',
    'Cross-framework comparison: if different, equally rigorous frameworks consistently yield different classifications for the same real-world phenomena, it suggests a degree of interpretive construction.',
    'If classifications are partly constructed, the claim of ''emerges_naturally'' would be weakened, and the constraint''s effective suppression of alternative framings would be seen as more extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(classification_objectivity_ambiguity, empirical, 'Empirical vs. constructed nature of constraint classification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of alternative framings structural (due to the inherent clarity of the diagnostic tool) or internalized (due to the epistemic authority claimed by its proponents)?',
    'Analysis of how new, incompatible frameworks are received: if they are dismissed without engagement, it suggests internalized suppression; if they are refuted by evidence, it suggests structural clarity.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as it operates through intellectual conformity rather than objective refutation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in epistemic discourse.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(deferential_realism_ontology__immutable_diagnostic_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(defe_tr_t0, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(defe_tr_t10, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(defe_tr_t20, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(defe_tr_t30, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 30, 0.1).
narrative_ontology:measurement(defe_tr_t40, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(defe_tr_t50, deferential_realism_ontology__immutable_diagnostic_reading, theater_ratio, 50, 0.1).

% Extraction over time
narrative_ontology:measurement(defe_be_t0, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(defe_be_t10, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 10, 0.13).
narrative_ontology:measurement(defe_be_t20, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(defe_be_t30, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 30, 0.15).
narrative_ontology:measurement(defe_be_t40, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(defe_be_t50, deferential_realism_ontology__immutable_diagnostic_reading, base_extractiveness, 50, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(defe_su_t0, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 0, 0.8).
narrative_ontology:measurement(defe_su_t10, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 10, 0.82).
narrative_ontology:measurement(defe_su_t20, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 20, 0.84).
narrative_ontology:measurement(defe_su_t30, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 30, 0.86).
narrative_ontology:measurement(defe_su_t40, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 40, 0.87).
narrative_ontology:measurement(defe_su_t50, deferential_realism_ontology__immutable_diagnostic_reading, suppression_requirement, 50, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(deferential_realism_ontology__immutable_diagnostic_reading, information_standard).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__rhetorical_scaffold_reading).
narrative_ontology:affects_constraint(deferential_realism_ontology__immutable_diagnostic_reading, deferential_realism_ontology__hybrid_pragmatic_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'deferential_realism_ontology' kernel. Each reading instantiates a distinct constraint with its own epsilon and structural properties, linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
