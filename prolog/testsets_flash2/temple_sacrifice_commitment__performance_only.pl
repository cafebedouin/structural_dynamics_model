% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__performance_only, []).

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
 *   constraint_id: temple_sacrifice_commitment__performance_only
 *   human_readable: Temple Sacrifice Commitment: Performance Only Reading
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint represents the 'performance_only' reading of the Temple
 *   sacrifice commitment, which holds that the laws of sacrifice require
 *   material instantiation to be active. In the absence of the Temple, the
 *   commitment is considered a dormant husk; study of these laws is seen as
 *   archival preservation of a defunct practice, not an occupation of the
 *   commitment itself. This reading has no current victim set, but
 *   acknowledges potential future victims if a restoration of sacrifice were
 *   attempted without prior ethical evolution.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__performance_only, 0.05).
domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, 0.02).
domain_priors:theater_ratio(temple_sacrifice_commitment__performance_only, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, extractiveness, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__performance_only, mountain).
narrative_ontology:human_readable(temple_sacrifice_commitment__performance_only, "Temple Sacrifice Commitment: Performance Only Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__performance_only, "religious_law/halakhic_tradition/commitment_system_theory").

domain_priors:emerges_naturally(temple_sacrifice_commitment__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__performance_only, '8ce930f6-f293-4d59-8360-370d70d542d7').
narrative_ontology:cs_kernel_codification('8ce930f6-f293-4d59-8360-370d70d542d7', fixed_text).
narrative_ontology:cs_authority_grounding('8ce930f6-f293-4d59-8360-370d70d542d7', lineage).
narrative_ontology:cs_interpretation_layer_present('8ce930f6-f293-4d59-8360-370d70d542d7').
narrative_ontology:cs_reading_relation('8ce930f6-f293-4d59-8360-370d70d542d7', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('8ce930f6-f293-4d59-8360-370d70d542d7', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('8ce930f6-f293-4d59-8360-370d70d542d7', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('8ce930f6-f293-4d59-8360-370d70d542d7', foundational, material_instantiation_is_prerequisite).
narrative_ontology:cs_axiom_status(material_instantiation_is_prerequisite, holdable).
narrative_ontology:cs_axiom_grounding('8ce930f6-f293-4d59-8360-370d70d542d7', material_instantiation_is_prerequisite, conventional).
narrative_ontology:cs_axiom('8ce930f6-f293-4d59-8360-370d70d542d7', foundational, study_is_not_performance).
narrative_ontology:cs_axiom_status(study_is_not_performance, holdable).
narrative_ontology:cs_axiom_grounding('8ce930f6-f293-4d59-8360-370d70d542d7', study_is_not_performance, conventional).
narrative_ontology:cs_reference_frame('8ce930f6-f293-4d59-8360-370d70d542d7', post_temple_destruction_halakha).
narrative_ontology:cs_drift_state('8ce930f6-f293-4d59-8360-370d70d542d7', contemporary_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('8ce930f6-f293-4d59-8360-370d70d542d7', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, historical_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, archival_institutions).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the clarity that the commitment is not currently active, allowing for objective historical and textual study without the burden of present-day ritual obligation. Their work is seen as preservation, not performance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, historical_scholars, beneficiary,
    moderate, generational, mobile, global).

% Benefit from the classification of sacrifice law as a defunct practice, justifying its preservation in archives and academic curricula as historical artifacts rather than living religious obligations. They curate texts and archaeological findings.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, archival_institutions, beneficiary,
    organized, civilizational, mobile, global).

% Would object to this reading as it diminishes the present-day spiritual significance of sacrifice law study. They believe study is a preparatory act for a future restoration of the Temple and its rituals, and that the commitment is merely suspended, not defunct. They are excluded from this reading's interpretive framework.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, messianic_restorationists, excluded,
    moderate, generational, identity_locked, regional).

% These authorities interpret the law as requiring physical performance for the commitment to be active. They maintain that in the absence of the Temple, the laws are not currently binding in a ritual sense, though their study remains valuable for historical and intellectual purposes. They set the interpretive agenda for this reading.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, halakhic_authorities_performance_only, agenda_setter,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, unambiguous framework for understanding the current status of Temple sacrifice laws, preventing misapplication or premature attempts at restoration in the absence of the Temple.
% TRANSFER_FUNCTION: Transfers the status of sacrifice law from an active ritual obligation to a historical and textual subject, shifting scholarly and communal focus accordingly.
% ABSENT_VOICES: Messianic restorationists and those who believe study itself constitutes performance are excluded. They would argue that this reading diminishes the spiritual vitality and ongoing relevance of the sacrifice laws.
% DISAPPEARANCE_RATIONALE: If this interpretive constraint vanished, the material conditions for sacrifice would still be absent. The world would not rearrange itself in a practical sense, though the interpretive debate among scholars and religious communities would intensify.
% FOUNDING_PROBLEM: To clarify the status of Temple sacrifice laws in the absence of the Temple, preventing confusion and inappropriate ritual attempts.
% FOUNDING_PROBLEM_CORROBORATION: This reading is corroborated by the historical reality of the Temple's destruction and the cessation of sacrifices, as well as by a long tradition of halakhic jurisprudence that distinguishes between theoretical study and practical application of ritual law. This is attested by a broad range of historical and contemporary halakhic authorities outside the immediate beneficiaries of this specific reading.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__performance_only, world_unchanged).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__performance_only, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(temple_sacrifice_commitment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__performance_only, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__performance_only_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, ExtMetricName, E),
    domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(temple_sacrifice_commitment__performance_only),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(temple_sacrifice_commitment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.05) because this reading imposes minimal burden; it primarily clarifies a state of non-obligation. Suppression is negligible (0.02) as it's an interpretive stance, not an actively enforced prohibition. Theater ratio is also very low (0.01) as there's no performative maintenance of a non-existent ritual. Accessibility collapse is high (0.95) because the physical absence of the Temple makes actual performance impossible, regardless of interpretation. Resistance is low (0.01) because this reading is widely accepted among many halakhic authorities.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, the commitment is a 'mountain' of physical reality (no Temple, no sacrifice). From the perspective of messianic restorationists, this reading is a 'snare' that suppresses spiritual vitality and delays restoration. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Historical scholars and archival institutions are beneficiaries as this reading legitimizes their work as preservation rather than active ritual engagement. Messianic restorationists are excluded, as this reading directly contradicts their view of ongoing spiritual obligation through study.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    future_restoration_ethics,
    'If the Temple were to be rebuilt, would the ethical framework for sacrifice need to evolve beyond historical practice to avoid creating a new victim set?',
    'Hypothetical ethical and theological discourse among diverse religious authorities, or the actual attempt at restoration and its social/ethical consequences.',
    'If ethical evolution is deemed necessary, this reading''s ''dormant husk'' status would be vindicated as a protective measure against future extraction. If not, the reading might be seen as overly cautious or dismissive of traditional practice.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(future_restoration_ethics, preference, 'Uncertainty about the ethical implications of a future restoration of sacrifice.').

omega_variable(
    commitment_status_ambiguity,
    'Is the commitment to Temple sacrifice truly ''dormant'' or merely ''suspended'' and awaiting conditions for activation?',
    'Further halakhic rulings on the nature of ritual obligation in absence of material conditions, or a shift in communal consensus regarding messianic expectations.',
    'If ''suspended'', the ''study_as_exercise'' or ''hybrid_preparatory'' readings gain legitimacy, potentially reclassifying this constraint as a ''snare'' for those who feel obligated to study as a form of performance. If ''dormant'', this reading''s low-extraction ''mountain'' classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(commitment_status_ambiguity, conceptual, 'Ambiguity regarding the active vs. suspended status of the sacrifice commitment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__performance_only, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__performance_only, theater_ratio, 0, 0.01).
narrative_ontology:measurement(temp_tr_t25, temple_sacrifice_commitment__performance_only, theater_ratio, 25, 0.01).
narrative_ontology:measurement(temp_tr_t50, temple_sacrifice_commitment__performance_only, theater_ratio, 50, 0.01).
narrative_ontology:measurement(temp_tr_t75, temple_sacrifice_commitment__performance_only, theater_ratio, 75, 0.01).
narrative_ontology:measurement(temp_tr_t100, temple_sacrifice_commitment__performance_only, theater_ratio, 100, 0.01).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__performance_only, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(temp_be_t25, temple_sacrifice_commitment__performance_only, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(temp_be_t50, temple_sacrifice_commitment__performance_only, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(temp_be_t75, temple_sacrifice_commitment__performance_only, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(temp_be_t100, temple_sacrifice_commitment__performance_only, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__performance_only, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(temp_su_t25, temple_sacrifice_commitment__performance_only, suppression_requirement, 25, 0.02).
narrative_ontology:measurement(temp_su_t50, temple_sacrifice_commitment__performance_only, suppression_requirement, 50, 0.02).
narrative_ontology:measurement(temp_su_t75, temple_sacrifice_commitment__performance_only, suppression_requirement, 75, 0.02).
narrative_ontology:measurement(temp_su_t100, temple_sacrifice_commitment__performance_only, suppression_requirement, 100, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__performance_only, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'temple_sacrifice_commitment' kernel. Its structural claim (performance-only) differs significantly from sibling readings regarding the nature of ongoing obligation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
