% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Ritual as Persecution-Survival Competence Transmission
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes the function of ritual in a persecuted
 *   community as a mechanism for encoding and transmitting adaptive capacity
 *   for survival. It is one reading of the 'catastrophe_memory_kernel',
 *   focusing on the practical, operational competence preserved through
 *   ritual rehearsal of catastrophe-response patterns. The community
 *   (beneficiary) gains resilience, while individuals facing assimilation
 *   pressure (victims) bear the costs of maintaining distinctiveness. The
 *   claimed type is 'rope' because the primary function is coordination for
 *   survival, with moderate extraction as a necessary cost of maintaining
 *   group cohesion and adaptive capacity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.45).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.6).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Ritual as Persecution-Survival Competence Transmission").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, 'd8fea4c2-6fd2-449a-9531-26b6196c351c').
narrative_ontology:cs_kernel_codification('d8fea4c2-6fd2-449a-9531-26b6196c351c', implicit).
narrative_ontology:cs_authority_grounding('d8fea4c2-6fd2-449a-9531-26b6196c351c', practice).
narrative_ontology:cs_interpretation_layer_present('d8fea4c2-6fd2-449a-9531-26b6196c351c').
narrative_ontology:cs_reading_relation('d8fea4c2-6fd2-449a-9531-26b6196c351c', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('d8fea4c2-6fd2-449a-9531-26b6196c351c', catastrophe_memory_kernel__trauma_encoding_reading, influences).
narrative_ontology:cs_reading_relation('d8fea4c2-6fd2-449a-9531-26b6196c351c', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('d8fea4c2-6fd2-449a-9531-26b6196c351c', foundational, ritual_as_adaptive_rehearsal).
narrative_ontology:cs_axiom_status(ritual_as_adaptive_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('d8fea4c2-6fd2-449a-9531-26b6196c351c', ritual_as_adaptive_rehearsal, empirically_contingent).
narrative_ontology:cs_axiom('d8fea4c2-6fd2-449a-9531-26b6196c351c', foundational, collective_memory_as_survival_resource).
narrative_ontology:cs_axiom_status(collective_memory_as_survival_resource, holdable).
narrative_ontology:cs_axiom_grounding('d8fea4c2-6fd2-449a-9531-26b6196c351c', collective_memory_as_survival_resource, empirically_contingent).
narrative_ontology:cs_reference_frame('d8fea4c2-6fd2-449a-9531-26b6196c351c', community_resilience_paradigm).
narrative_ontology:cs_drift_state('d8fea4c2-6fd2-449a-9531-26b6196c351c', contemporary_globalization_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('d8fea4c2-6fd2-449a-9531-26b6196c351c', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, persecuted_community).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, assimilating_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The community benefits from the ritual's function in transmitting practical knowledge and psychological resilience for surviving persecution. Participation reinforces collective identity and prepares members for future threats, but also imposes costs of maintaining distinctiveness.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, persecuted_community, beneficiary,
    organized, generational, identity_locked, local).

% Individuals who seek to integrate into the dominant culture experience the ritual as a cost, as it reinforces a distinct identity and demands adherence to practices that may conflict with assimilation. The 'cost' is the friction and social pressure against abandoning the community's survival strategies.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, assimilating_individuals, payer,
    powerless, biographical, constrained, local).

% The custodians and interpreters of the ritual. They ensure its correct performance and transmission, thereby maintaining the community's adaptive capacity. Their authority is derived from this role, and they bear the responsibility of preserving the survival competence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, ritual_leaders, agenda_setter,
    powerful, generational, identity_locked, local).

% The external societal context that exerts pressure for assimilation. It does not directly participate in the ritual but its presence and policies create the conditions under which the ritual's survival function becomes salient.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, dominant_culture, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the intergenerational transmission of adaptive strategies, psychological resilience, and practical knowledge necessary for a persecuted community to survive and persist under external threat.
% TRANSFER_FUNCTION: Transfers collective memory, behavioral patterns, and emotional fortitude from past generations to current and future members, ensuring the continuity of survival competence. It also transfers the cost of maintaining a distinct identity onto individuals who might otherwise assimilate.
% ABSENT_VOICES: Individuals who have successfully assimilated into the dominant culture are absent; they would argue that the ritual's costs outweigh its benefits, as their own survival was achieved through integration, not resistance.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the community would lose a primary mechanism for transmitting survival competence. Over generations, this would lead to a decline in resilience, increased vulnerability to persecution, and accelerated assimilation, fundamentally altering the community's structure and persistence.
% FOUNDING_PROBLEM: The recurring experience of persecution and the need to preserve the community's existence and distinct identity across generations in hostile environments.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of persecuted groups, historical records of survival strategies, and contemporary testimonies from community elders and scholars outside the immediate community corroborate the ongoing relevance of the founding problem.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__survival_competence_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.45) as the ritual demands adherence and imposes social costs on those who might prefer assimilation, but these costs are largely seen as necessary for collective survival. Suppression (0.6) is present through social pressure and the difficulty of exiting the community's identity, but it's not actively enforced by an external body. Theater ratio is low (0.1) as the ritual's function is highly practical and directly tied to survival, with little performative excess. The cyclical nature of extractiveness and suppression reflects periods of heightened external threat (e.g., 1930s) leading to increased internal cohesion and demands, followed by periods of relative calm where pressures ease.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the persecuted community and ritual leaders, the constraint is a vital 'rope' for collective survival. From the perspective of assimilating individuals, it can feel more like a 'tangled rope' or even a 'snare' due to the social and identity costs it imposes, limiting their individual choices for integration.
 *
 * DIRECTIONALITY LOGIC:
 *   The 'persecuted_community' is the primary beneficiary (d near 0.0) as the ritual directly contributes to its resilience and continuity. 'Assimilating_individuals' are targets (d near 1.0) as they bear the costs of maintaining a distinct identity and face social pressure against assimilation. 'Ritual_leaders' are agenda-setters, benefiting from their role in preserving the community's adaptive capacity. The 'dominant_culture' is an observer, creating the external conditions but not directly participating in the constraint's internal dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (persecution-survival) remains live, preventing mislabeling as a piton. The moderate extractiveness and suppression are directly linked to the ongoing need for adaptive capacity, rather than being an atrophied function. The classification as 'rope' acknowledges the genuine coordination function while the metrics capture the costs borne by individuals within the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_competence_vs_identity_maintenance,
    'Is the primary function of the ritual the transmission of survival competence, or the maintenance of group identity for its own sake?',
    'Comparative analysis of ritual forms in communities under varying degrees of external threat: if the ''survival competence'' aspects diminish significantly in low-threat environments while ''identity maintenance'' persists, it suggests separability.',
    'If identity maintenance is primary, the extractiveness for assimilating individuals might be reclassified as higher, as the ''survival'' justification would be weaker. If survival competence is primary, the ''rope'' classification is strongly reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_competence_vs_identity_maintenance, conceptual, 'Distinguishing the core function of the ritual.').

omega_variable(
    internalized_vs_structural_suppression,
    'What proportion of the measured suppression is structural (external barriers to assimilation) versus internalized (cognitive patterns, identity fusion) within the community?',
    'Longitudinal studies of individuals who successfully exit the community: if ''suppression'' effects (e.g., guilt, self-doubt) persist after structural barriers are removed, it indicates internalized suppression.',
    'If internalized suppression is high, the effective suppression for individuals is higher than the structural measure suggests, potentially shifting the individual experience closer to a ''snare'' even if the collective benefits are real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_suppression, empirical, 'Mechanism of suppression in identity-locked communities.').

omega_variable(
    reading_overlap_with_trauma_encoding,
    'To what extent does the ''survival_competence_reading'' overlap with the ''trauma_encoding_reading''? Is the transmission of ''competence'' inseparable from the encoding of ''trauma''?',
    'Detailed ethnographic analysis of ritual content and participant experience: if the ''competence'' elements are always presented through the lens of past suffering, the readings are highly coupled.',
    'If highly coupled, the ''survival_competence_reading'' might inherit some of the ''trauma_encoding_reading''s'' potential for higher extraction (e.g., psychological burden), making the ''rope'' classification more tenuous.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_overlap_with_trauma_encoding, conceptual, 'Assessing the degree of functional overlap between two kernel readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 1900, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t1900, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(cata_tr_t1930, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 1930, 0.08).
narrative_ontology:measurement(cata_tr_t1960, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 1960, 0.12).
narrative_ontology:measurement(cata_tr_t1990, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(cata_tr_t2020, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 2020, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t1900, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(cata_be_t1930, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 1930, 0.6).
narrative_ontology:measurement(cata_be_t1960, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 1960, 0.4).
narrative_ontology:measurement(cata_be_t1990, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(cata_be_t2020, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 2020, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t1900, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 1900, 0.7).
narrative_ontology:measurement(cata_su_t1930, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 1930, 0.8).
narrative_ontology:measurement(cata_su_t1960, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 1960, 0.5).
narrative_ontology:measurement(cata_su_t1990, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 1990, 0.4).
narrative_ontology:measurement(cata_su_t2020, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 2020, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
