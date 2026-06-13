% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__symbolic_archive_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sacrifice_obligation_kernel__symbolic_archive_reading
 *   human_readable: Sacrifice Law as Symbolic Archive (No Halakhic Claim)
 *   domain: religious_law/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint represents the 'symbolic archive' reading of sacrifice
 *   law within Judaism. In this reading, the study of sacrifice law is
 *   understood as a means of preserving cultural identity and historical
 *   continuity, without asserting any active halakhic (religious legal)
 *   obligation. It is a voluntary cultural practice, not a binding
 *   commandment. This reading explicitly denies any coercive or extractive
 *   elements, positioning the law as a 'mountain' of cultural heritage.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0).
domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0).
domain_priors:theater_ratio(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, 0.0).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__symbolic_archive_reading, mountain).
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrifice Law as Symbolic Archive (No Halakhic Claim)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious_law/halakhic_authority/commitment_system_dynamics").

domain_priors:emerges_naturally(sacrifice_obligation_kernel__symbolic_archive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__symbolic_archive_reading, 'ae2c73b7-7238-4489-932d-265b45f6f6a9').
narrative_ontology:cs_kernel_codification('ae2c73b7-7238-4489-932d-265b45f6f6a9', fixed_text).
narrative_ontology:cs_authority_grounding('ae2c73b7-7238-4489-932d-265b45f6f6a9', practice).
narrative_ontology:cs_interpretation_layer_present('ae2c73b7-7238-4489-932d-265b45f6f6a9').
narrative_ontology:cs_reading_relation('ae2c73b7-7238-4489-932d-265b45f6f6a9', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae2c73b7-7238-4489-932d-265b45f6f6a9', sacrifice_obligation_kernel__performance_only_reading, coexists_with).
narrative_ontology:cs_reading_relation('ae2c73b7-7238-4489-932d-265b45f6f6a9', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_axiom('ae2c73b7-7238-4489-932d-265b45f6f6a9', foundational, sacrifice_law_is_historical_archive).
narrative_ontology:cs_axiom_status(sacrifice_law_is_historical_archive, holdable).
narrative_ontology:cs_axiom_grounding('ae2c73b7-7238-4489-932d-265b45f6f6a9', sacrifice_law_is_historical_archive, conventional).
narrative_ontology:cs_axiom('ae2c73b7-7238-4489-932d-265b45f6f6a9', foundational, no_active_halakhic_obligation).
narrative_ontology:cs_axiom_status(no_active_halakhic_obligation, holdable).
narrative_ontology:cs_axiom_grounding('ae2c73b7-7238-4489-932d-265b45f6f6a9', no_active_halakhic_obligation, conventional).
narrative_ontology:cs_reference_frame('ae2c73b7-7238-4489-932d-265b45f6f6a9', post_temple_cultural_preservation).
narrative_ontology:cs_drift_state('ae2c73b7-7238-4489-932d-265b45f6f6a9', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ae2c73b7-7238-4489-932d-265b45f6f6a9', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_identity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__symbolic_archive_reading, lay_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the preservation of historical and cultural knowledge, ensuring continuity of tradition without imposing active obligations. The study of sacrifice law enriches the collective memory of the Jewish people.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory, beneficiary,
    analytical, civilizational, analytical, universal).

% Is strengthened by engagement with foundational texts and practices, even if symbolic. The historical context provided by sacrifice law contributes to a robust sense of shared heritage and belonging.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_identity, beneficiary,
    analytical, generational, analytical, universal).

% Interpret and transmit the texts of sacrifice law, framing them as cultural and historical artifacts rather than active halakhic obligations. They curate the archive and guide its study.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, religious_scholars, agenda_setter,
    organized, biographical, mobile, global).

% Engages in the study of sacrifice law as a voluntary act of cultural and historical preservation. They bear the 'cost' of time and intellectual effort, but receive no binding obligation or penalty for non-engagement.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, lay_community, payer,
    moderate, biographical, mobile, local).

% Would object to the claim that sacrifice law makes no halakhic claim, as their readings assert ongoing or suspended obligation. They are excluded from this reading's interpretive framework.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, halakhic_authorities_of_other_readings, excluded,
    institutional, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation and transmission of a significant body of historical and cultural texts, ensuring that a shared heritage remains accessible and understood across generations.
% TRANSFER_FUNCTION: Transfers historical knowledge and cultural continuity from past generations to present and future ones, without transferring any active religious obligation or material resources.
% ABSENT_VOICES: Halakhic authorities who maintain that sacrifice law carries a binding, even if suspended, obligation would object. Their voices are absent from this reading's interpretive framework, which explicitly denies any halakhic claim.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, the texts of sacrifice law would still exist, and other readings would continue to interpret them. The cultural and historical value would persist, but the specific framing of 'no halakhic claim' would be lost, potentially leading to a re-evaluation of the texts' status by those who previously held this view.
% FOUNDING_PROBLEM: The challenge of maintaining continuity with ancient religious practices and texts after their cessation, ensuring their relevance and preservation without imposing anachronistic obligations.
% FOUNDING_PROBLEM_CORROBORATION: Religious historians and cultural anthropologists, from outside the immediate halakhic discourse, corroborate the ongoing challenge of cultural preservation and identity formation in post-temple Judaism. Their scholarship supports the need for frameworks that maintain historical connection without literal performance.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__symbolic_archive_reading, world_unchanged).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__symbolic_archive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_kernel__symbolic_archive_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_obligation_kernel__symbolic_archive_reading),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness, suppression, and theater_ratio are all zero because this reading posits no active obligation, no enforcement, and no performative maintenance beyond voluntary study. Accessibility collapse is high (0.95) because, from this perspective, the 'alternatives' (i.e., performing sacrifices) are physically impossible and thus completely collapsed. Resistance is zero as there is no obligation to resist. The claimed type is 'mountain' because it is presented as an unchangeable cultural fact, a historical artifact whose study is a natural expression of identity, not a human-imposed rule.
 *
 * PERSPECTIVAL GAP:
 *   There is no significant perspectival gap within this reading, as all participants are assumed to share the understanding that sacrifice law is a cultural archive without halakhic force. The gap exists between this reading and other readings that assert an active obligation.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish collective memory and identity are the beneficiaries, as they are enriched by the preservation of this cultural archive. Religious scholars act as agenda-setters by framing and guiding this study. The lay community are 'payers' only in the sense of voluntarily expending effort for study, but they bear no coercive cost. Halakhic authorities of other readings are 'excluded' because their interpretive frameworks, which assert an active halakhic claim, are incompatible with this reading's core premise.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    halakhic_claim_ambiguity,
    'Is the claim that sacrifice law makes ''no halakhic claim'' an objective truth about the text, or an interpretive choice to manage historical discontinuity?',
    'Analysis of historical rabbinic responsa and legal codes: if no major halakhic authority ever explicitly denied a future halakhic claim, it suggests an interpretive choice rather than an inherent textual property.',
    'If an interpretive choice, the ''emerges_naturally'' claim for this reading is weakened, and it might be reclassified as a ''rope'' (coordination of cultural identity) rather than a ''mountain'' (natural cultural fact).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(halakhic_claim_ambiguity, conceptual, 'Ambiguity regarding the inherent halakhic status of sacrifice law.').

omega_variable(
    beneficiary_naturalness_ambiguity,
    'Are ''Jewish collective memory'' and ''Jewish identity'' genuine beneficiaries of a natural cultural process, or are they constructed beneficiaries of an interpretive framework designed to maintain relevance?',
    'Comparative study of other cultural archives: if similar archives in other cultures persist without active interpretive frameworks, it supports natural beneficiary status. If active framing is always required, it suggests construction.',
    'If constructed, the ''mountain'' classification is weakened, as the ''naturalness'' of the benefit is undermined, potentially shifting towards a ''rope'' or ''tangled_rope'' if the framing requires active, potentially extractive, maintenance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_naturalness_ambiguity, empirical, 'Whether the beneficiaries are natural outcomes or products of interpretive construction.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (zero) genuinely absent, or is there an internalized suppression of alternative halakhic readings within this interpretive community?',
    'Sociological study of interpretive communities: if adherents of this reading actively dismiss or marginalize those holding other readings, it suggests an internalized suppression mechanism.',
    'If internalized suppression is present, the effective suppression is higher than zero, potentially reclassifying this reading from a ''mountain'' to a ''rope'' (coordinating a specific interpretive stance) or even a ''tangled_rope'' if the marginalization is asymmetric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism regarding alternative readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 100, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 100, 0.0).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 1000, 0.0).
narrative_ontology:measurement(sacr_tr_t2024, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 2024, 0.0).

% Extraction over time
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 100, 0.0).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement(sacr_be_t2024, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 2024, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t100, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 100, 0.0).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 1000, 0.0).
narrative_ontology:measurement(sacr_su_t2024, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 2024, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__symbolic_archive_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__messianic_suspension_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__study_as_exercise_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'sacrifice_obligation_kernel', each representing a distinct interpretive stance on the halakhic status and contemporary relevance of sacrifice law. This reading explicitly denies any active halakhic claim, focusing on cultural and historical preservation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
