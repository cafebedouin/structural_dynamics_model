% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__performance_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__performance_only_reading, []).

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
 *   constraint_id: sacrifice_obligation_kernel__performance_only_reading
 *   human_readable: Sacrifice Obligation: Performance Only Reading
 *   domain: religious_law/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint represents the 'performance only' reading of the
 *   sacrifice obligation kernel within Jewish law. It asserts that the divine
 *   command for sacrifices requires physical performance in the Temple, and
 *   that study of the laws of sacrifice, while meritorious, does not fulfill
 *   the mitzvah itself. This reading results in a structural impossibility
 *   for the Jewish people since the destruction of the Temple, leading to a
 *   high degree of unfulfilled obligation. The constraint is classified as a
 *   Mountain due to its perceived immutability as divine law, despite the
 *   high 'extraction' of unfulfilled obligation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, 0.95).
domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, 0.99).
domain_priors:theater_ratio(sacrifice_obligation_kernel__performance_only_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0.99).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, 0.99).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__performance_only_reading, mountain).
narrative_ontology:human_readable(sacrifice_obligation_kernel__performance_only_reading, "Sacrifice Obligation: Performance Only Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__performance_only_reading, "religious_law/halakhic_authority/commitment_system_dynamics").

domain_priors:emerges_naturally(sacrifice_obligation_kernel__performance_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__performance_only_reading, 'c37904de-ea02-4d96-80b0-8865a5ed679c').
narrative_ontology:cs_kernel_codification('c37904de-ea02-4d96-80b0-8865a5ed679c', fixed_text).
narrative_ontology:cs_authority_grounding('c37904de-ea02-4d96-80b0-8865a5ed679c', lineage).
narrative_ontology:cs_interpretation_layer_present('c37904de-ea02-4d96-80b0-8865a5ed679c').
narrative_ontology:cs_reading_relation('c37904de-ea02-4d96-80b0-8865a5ed679c', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('c37904de-ea02-4d96-80b0-8865a5ed679c', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('c37904de-ea02-4d96-80b0-8865a5ed679c', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('c37904de-ea02-4d96-80b0-8865a5ed679c', foundational, mitzvah_requires_physical_action).
narrative_ontology:cs_axiom_status(mitzvah_requires_physical_action, holdable).
narrative_ontology:cs_axiom_grounding('c37904de-ea02-4d96-80b0-8865a5ed679c', mitzvah_requires_physical_action, deontological).
narrative_ontology:cs_axiom('c37904de-ea02-4d96-80b0-8865a5ed679c', secondary, study_is_preparation_not_fulfillment).
narrative_ontology:cs_axiom_status(study_is_preparation_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('c37904de-ea02-4d96-80b0-8865a5ed679c', study_is_preparation_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('c37904de-ea02-4d96-80b0-8865a5ed679c', torah_command_and_temple_practice).
narrative_ontology:cs_drift_state('c37904de-ea02-4d96-80b0-8865a5ed679c', post_temple_destruction_era, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('c37904de-ea02-4d96-80b0-8865a5ed679c', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, the_jewish_people).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__performance_only_reading, messianic_aspirants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commanded by divine law to perform sacrifices, but physically unable to do so due to the destruction of the Temple. Bears the burden of unfulfilled obligation and the spiritual gap it represents. Identity is deeply tied to this covenantal relationship.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, the_jewish_people, payer,
    powerless, generational, identity_locked, global).

% Interpret and transmit the divine law, affirming the requirement for physical performance of sacrifices. They maintain the legal framework and the expectation of future fulfillment, but cannot alter the core requirement. Their authority is grounded in this transmission.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, halakhic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% While not directly benefiting from the constraint's operation, this group finds meaning and purpose in the expectation of future Temple restoration and the resumption of sacrifices. The unfulfilled obligation fuels their messianic hopes.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, messianic_aspirants, beneficiary,
    moderate, generational, identity_locked, global).

% Analyze the structural implications of a divine command that is currently impossible to fulfill, examining its impact on religious practice, identity, and legal interpretation without being bound by the obligation itself.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the collective spiritual and legal understanding of the Jewish people around a core divine command, maintaining the expectation of its future physical fulfillment.
% TRANSFER_FUNCTION: Transfers the burden of unfulfilled divine command onto the Jewish people, creating a spiritual and legal gap that cannot be bridged by current actions.
% ABSENT_VOICES: Those who might argue for a purely symbolic or intellectual fulfillment of the mitzvah are implicitly excluded by the strict interpretation of 'performance only.' Their voices are present in other readings of the kernel, but not in this one's framework.
% DISAPPEARANCE_RATIONALE: If the obligation for physical sacrifice vanished, or if this reading were universally rejected, it would fundamentally alter Jewish religious law, identity, and messianic aspirations. The entire legal and spiritual framework would need to be re-evaluated.
% FOUNDING_PROBLEM: The divine command to offer sacrifices as a central act of worship and atonement, as detailed in the Torah.
% FOUNDING_PROBLEM_CORROBORATION: The problem is attested as live by virtually all halakhic authorities across millennia, grounded in scriptural commands and the historical reality of the Temple's destruction. This is not contested by external parties, only by internal readings of how the obligation is currently fulfilled.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__performance_only_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__performance_only_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__performance_only_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_kernel__performance_only_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_obligation_kernel__performance_only_reading),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) because the entire Jewish people is commanded to perform an act that has been physically impossible for nearly 2000 years, creating a profound spiritual and legal gap. Suppression is also extremely high (0.99) because the constraint is understood as divine law, leaving no legitimate 'exit' or alternative fulfillment within this reading's framework. Theater ratio is very low (0.05) as there is no performative maintenance of the physical sacrifice itself; the constraint's persistence is due to its perceived divine origin and the commitment to its future fulfillment. Accessibility collapse is near total (0.99) as no alternative means of fulfillment are recognized. Resistance is negligible (0.01) because the constraint is accepted as divine will, not a human construct to be resisted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Jewish people, this is a profound and unresolvable spiritual burden. From the perspective of halakhic authorities, it is a faithful transmission of divine will. From an analytical observer's perspective, it is a unique case of a 'Mountain' constraint that imposes immense 'extraction' due to a structural impossibility, rather than an agent's rent-seeking.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish people are the primary 'payer' (victim) of this constraint, bearing the burden of unfulfilled divine command. Halakhic authorities act as 'agenda setters' by transmitting and upholding this interpretation of the law. Messianic aspirants are 'beneficiaries' in a structural sense, as the unfulfilled obligation fuels their hopes for future redemption and restoration. There is no agent that 'extracts' in the conventional sense; the extraction is a structural consequence of the divine command meeting historical impossibility.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_command_vs_human_capacity,
    'Is the ''extraction'' of unfulfilled obligation a feature of divine will, or a consequence of human interpretation of an historically contingent command?',
    'Theological or philosophical inquiry into the nature of divine commands and human capacity, or the emergence of a widely accepted alternative halakhic interpretation.',
    'If interpreted as a feature of divine will, the Mountain classification is reinforced. If seen as a human interpretive choice, it could shift towards a Snare or Tangled Rope, depending on whether an agent benefits from maintaining the impossibility.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(divine_command_vs_human_capacity, conceptual, 'Ambiguity regarding the source of the unfulfilled obligation.').

omega_variable(
    natural_law_vs_constructed_impossibility,
    'Is the impossibility of performance a ''natural law'' (a physical/historical fact beyond human control), or is its persistence as an ''obligation'' a constructed feature of the legal system?',
    'Analysis of how other historically impossible commands are treated within the halakhic system; examination of the interpretive choices that maintain the ''live'' status of the obligation.',
    'If the ''obligation'' aspect is found to be a constructed feature, the ''emerges_naturally'' flag for this Mountain could be challenged, potentially reclassifying it as a Snare or Tangled Rope if beneficiaries are identified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_impossibility, conceptual, 'Distinguishing between physical impossibility and legal construction of obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__performance_only_reading, 0, 1954).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(sacr_tr_t488, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 488, 0.05).
narrative_ontology:measurement(sacr_tr_t977, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 977, 0.05).
narrative_ontology:measurement(sacr_tr_t1466, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1466, 0.05).
narrative_ontology:measurement(sacr_tr_t1954, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1954, 0.05).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 0, 0.95).
narrative_ontology:measurement(sacr_be_t488, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 488, 0.95).
narrative_ontology:measurement(sacr_be_t977, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 977, 0.95).
narrative_ontology:measurement(sacr_be_t1466, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1466, 0.95).
narrative_ontology:measurement(sacr_be_t1954, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1954, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0, 0.99).
narrative_ontology:measurement(sacr_su_t488, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 488, 0.99).
narrative_ontology:measurement(sacr_su_t977, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 977, 0.99).
narrative_ontology:measurement(sacr_su_t1466, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1466, 0.99).
narrative_ontology:measurement(sacr_su_t1954, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1954, 0.99).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
