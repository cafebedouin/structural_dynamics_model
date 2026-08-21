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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: sacrifice_obligation_kernel__symbolic_archive_reading
 *   human_readable: Sacrifice Law as Symbolic Archive (Study for Identity)
 *   domain: religious_law/halakhic_authority/commitment_system
 *
 * SUMMARY:
 *   This constraint represents the 'symbolic archive' reading of sacrifice
 *   law, where its study is understood as a voluntary act of cultural and
 *   historical preservation, contributing to Jewish identity and collective
 *   memory, but carrying no current halakhic (binding legal) obligation. This
 *   reading posits zero extractiveness and suppression, as there is no
 *   command to obey or violate, and study is a free choice. It is classified
 *   as a Mountain due to its perceived unchangeable status as a historical
 *   fact and cultural resource, independent of human enforcement.
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
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrifice Law as Symbolic Archive (Study for Identity)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious_law/halakhic_authority/commitment_system").

domain_priors:emerges_naturally(sacrifice_obligation_kernel__symbolic_archive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__symbolic_archive_reading, '84068ca0-de87-4922-92e7-598a1847172d').
narrative_ontology:cs_kernel_codification('84068ca0-de87-4922-92e7-598a1847172d', fixed_text).
narrative_ontology:cs_authority_grounding('84068ca0-de87-4922-92e7-598a1847172d', lineage).
narrative_ontology:cs_interpretation_layer_present('84068ca0-de87-4922-92e7-598a1847172d').
narrative_ontology:cs_reading_relation('84068ca0-de87-4922-92e7-598a1847172d', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('84068ca0-de87-4922-92e7-598a1847172d', sacrifice_obligation_kernel__performance_only_reading, forecloses).
narrative_ontology:cs_reading_relation('84068ca0-de87-4922-92e7-598a1847172d', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_axiom('84068ca0-de87-4922-92e7-598a1847172d', foundational, no_current_halakhic_obligation).
narrative_ontology:cs_axiom_status(no_current_halakhic_obligation, holdable).
narrative_ontology:cs_axiom_grounding('84068ca0-de87-4922-92e7-598a1847172d', no_current_halakhic_obligation, conventional).
narrative_ontology:cs_axiom('84068ca0-de87-4922-92e7-598a1847172d', foundational, study_is_cultural_preservation).
narrative_ontology:cs_axiom_status(study_is_cultural_preservation, holdable).
narrative_ontology:cs_axiom_grounding('84068ca0-de87-4922-92e7-598a1847172d', study_is_cultural_preservation, conventional).
narrative_ontology:cs_reference_frame('84068ca0-de87-4922-92e7-598a1847172d', post_temple_destruction_cultural_preservation).
narrative_ontology:cs_drift_state('84068ca0-de87-4922-92e7-598a1847172d', contemporary_secular_context, gap(stable, minor, true)).
narrative_ontology:cs_created_at('84068ca0-de87-4922-92e7-598a1847172d', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_identity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, students_of_halakha).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, cultural_continuity_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_obligation_kernel__symbolic_archive_reading, historical_preservation_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the preservation of historical and cultural knowledge, ensuring the continuity of the Jewish narrative across generations. This is a non-coercive, identity-affirming benefit.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory).

% Is strengthened by the study and understanding of historical practices, even if not halakhically binding. It provides a sense of connection to the past and a shared heritage.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_identity, beneficiary,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_identity).

% Engage in the study of sacrifice law as a voluntary act of cultural and historical preservation, deriving personal and communal identity benefits without perceiving a binding obligation or cost.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, students_of_halakha, beneficiary,
    moderate, biographical, mobile, global).

% Observe and document the historical context of sacrifice law, acknowledging its past halakhic significance but not asserting a current binding obligation for performance or substitution through study.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, halakhic_authorities, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the preservation and transmission of a significant body of historical and cultural knowledge within the Jewish tradition, ensuring collective memory and identity are maintained.
% TRANSFER_FUNCTION: Transfers historical knowledge and cultural continuity across generations, from ancient texts and practices to contemporary students and communities, without imposing any material or ritual obligation.
% ABSENT_VOICES: Those who believe in a current, active halakhic obligation for sacrifice (either through performance or study as substitution) would object, arguing that this reading diminishes the divine command. They are present in other readings of the kernel.
% DISAPPEARANCE_RATIONALE: If the understanding of sacrifice law as a symbolic archive vanished, a significant pillar of Jewish historical consciousness and identity would be lost, leading to a profound rearrangement of cultural and historical self-understanding.
% FOUNDING_PROBLEM: The need to preserve the historical and cultural memory of ancient Jewish practices, particularly after the destruction of the Temple, to maintain identity and continuity.
% FOUNDING_PROBLEM_CORROBORATION: Historians of religion and cultural anthropologists, alongside many Jewish educators and community leaders, corroborate the ongoing importance of historical and cultural preservation for collective identity, independent of halakhic claims.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__symbolic_archive_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__symbolic_archive_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_kernel__symbolic_archive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 0.0, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   The metrics reflect the core premise of this reading: if sacrifice law is purely an archive, there is no extraction (no one pays a cost for non-compliance, as there is no compliance to begin with), no suppression (no coercion to study or not study), and no theater (the act of study is genuine cultural engagement, not a performance of a non-existent obligation). Accessibility collapse is high because the historical fact of the archive is unchangeable, and resistance is zero because there's no active imposition to resist. The beneficiaries are abstract entities (collective memory, identity) and voluntary students, none of whom are coerced.
 *
 * PERSPECTIVAL GAP:
 *   From this reading's perspective, there is no 'gap' in the sense of a contested obligation. Other readings, however, would perceive this as a significant departure from halakhic truth, seeing a 'gap' between this reading's non-binding view and their own belief in an active, albeit transformed or suspended, obligation.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'beneficiaries' (Jewish collective memory, Jewish identity, students) are not 'beneficiaries' in an extractive sense, but rather entities or agents that are enriched by the voluntary cultural practice. There are no 'victims' because no obligation is imposed. Directionality for all involved is towards full beneficiary (d=0.0) as the constraint subsidizes identity and continuity without cost.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a cultural preservation effort as an extractive or coercive constraint. By asserting zero extraction and suppression, it highlights that the 'mandate' here is cultural continuity, which is being fulfilled through voluntary study, not through an atrophied or performative legal obligation. The 'mandatrophy' question itself is resolved by the premise that no active halakhic mandate exists to atrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    halakhic_status_ambiguity,
    'Is sacrifice law purely a historical archive, or does it retain a latent or transformed halakhic obligation?',
    'Theological consensus shifts, new halakhic rulings, or the re-establishment of a Temple and sacrificial system.',
    'If a latent halakhic obligation is affirmed, the constraint would shift from a Mountain (historical fact) to a Rope or Tangled Rope, with associated extractiveness and suppression for those bound by the obligation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(halakhic_status_ambiguity, conceptual, 'Ambiguity regarding the current halakhic (binding legal) status of sacrifice law.').

omega_variable(
    cultural_vs_religious_framing,
    'Is the study of sacrifice law primarily a cultural practice for identity, or is it inherently a religious act with spiritual implications beyond mere preservation?',
    'Sociological studies of motivation for study, or shifts in theological discourse emphasizing spiritual efficacy of study.',
    'If framed as a religious act with spiritual efficacy, the ''beneficiary'' aspect might take on a more active, albeit non-extractive, role, potentially influencing how its ''value'' is perceived, though unlikely to change its Mountain classification without an associated obligation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_vs_religious_framing, conceptual, 'Distinction between cultural and religious motivations for studying sacrifice law.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0, 0.0).
narrative_ontology:measurement(sacr_tr_t25, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 25, 0.0).
narrative_ontology:measurement(sacr_tr_t50, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 50, 0.0).
narrative_ontology:measurement(sacr_tr_t75, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 75, 0.0).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 100, 0.0).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(sacr_be_t25, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 25, 0.0).
narrative_ontology:measurement(sacr_be_t50, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 50, 0.0).
narrative_ontology:measurement(sacr_be_t75, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 75, 0.0).
narrative_ontology:measurement(sacr_be_t100, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 100, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0, 0.0).
narrative_ontology:measurement(sacr_su_t25, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 25, 0.0).
narrative_ontology:measurement(sacr_su_t50, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 50, 0.0).
narrative_ontology:measurement(sacr_su_t75, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 75, 0.0).
narrative_ontology:measurement(sacr_su_t100, sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 100, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__symbolic_archive_reading, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__messianic_suspension_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four readings of the 'sacrifice_obligation_kernel'. Each reading represents a distinct structural claim about the nature and status of sacrifice law, with differing implications for obligation and extraction. This 'symbolic_archive_reading' posits no active halakhic claim, focusing on cultural preservation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
