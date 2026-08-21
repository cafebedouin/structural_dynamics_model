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
 *   constraint_id: sacrifice_obligation_kernel__performance_only_reading
 *   human_readable: Sacrifice Obligation: Physical Performance Only Reading
 *   domain: religious_law/halakhic_authority
 *
 * SUMMARY:
 *   This constraint represents the 'performance only' reading of the
 *   sacrifice obligation kernel within Jewish law. It asserts that the divine
 *   command for animal sacrifice requires physical performance, and that the
 *   study of these laws, while meritorious, does not fulfill the mitzvah
 *   (commandment). This reading results in a state of perpetual, unfulfilled
 *   obligation for the Jewish people since the destruction of the Second
 *   Temple, creating a structural impossibility that drives messianic
 *   longing. The high extractiveness reflects the burden of this unfulfilled
 *   command, and the high suppression reflects the absolute physical and
 *   theological barriers to performance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, 0.95).
domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, 0.99).
domain_priors:theater_ratio(sacrifice_obligation_kernel__performance_only_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0.99).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, 0.99).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__performance_only_reading, mountain).
narrative_ontology:human_readable(sacrifice_obligation_kernel__performance_only_reading, "Sacrifice Obligation: Physical Performance Only Reading").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__performance_only_reading, "religious_law/halakhic_authority").

domain_priors:emerges_naturally(sacrifice_obligation_kernel__performance_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__performance_only_reading, '054c250b-e5ad-4f60-8306-910ecdc41651').
narrative_ontology:cs_kernel_codification('054c250b-e5ad-4f60-8306-910ecdc41651', fixed_text).
narrative_ontology:cs_authority_grounding('054c250b-e5ad-4f60-8306-910ecdc41651', lineage).
narrative_ontology:cs_interpretation_layer_present('054c250b-e5ad-4f60-8306-910ecdc41651').
narrative_ontology:cs_reading_relation('054c250b-e5ad-4f60-8306-910ecdc41651', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('054c250b-e5ad-4f60-8306-910ecdc41651', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('054c250b-e5ad-4f60-8306-910ecdc41651', sacrifice_obligation_kernel__symbolic_archive_reading, forecloses).
narrative_ontology:cs_axiom('054c250b-e5ad-4f60-8306-910ecdc41651', foundational, physical_performance_is_sine_qua_non).
narrative_ontology:cs_axiom_status(physical_performance_is_sine_qua_non, holdable).
narrative_ontology:cs_axiom_grounding('054c250b-e5ad-4f60-8306-910ecdc41651', physical_performance_is_sine_qua_non, deontological).
narrative_ontology:cs_axiom('054c250b-e5ad-4f60-8306-910ecdc41651', foundational, study_is_preparation_not_fulfillment).
narrative_ontology:cs_axiom_status(study_is_preparation_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('054c250b-e5ad-4f60-8306-910ecdc41651', study_is_preparation_not_fulfillment, conventional).
narrative_ontology:cs_reference_frame('054c250b-e5ad-4f60-8306-910ecdc41651', torah_literal_command).
narrative_ontology:cs_drift_state('054c250b-e5ad-4f60-8306-910ecdc41651', post_second_temple_destruction, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('054c250b-e5ad-4f60-8306-910ecdc41651', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, jewish_people).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__performance_only_reading, messianic_aspirants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Commanded by divine law to perform sacrifices, but physically unable to do so due to the destruction of the Temple and lack of a recognized sacrificial system. Bears the burden of unfulfilled obligation, which is a structural impossibility rather than an extraction by an agent. Identity is deeply tied to the covenant, making 'exit' from the obligation unthinkable.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, jewish_people, payer,
    powerless, generational, identity_locked, global).

% Interpret and transmit the Halakha (Jewish law), including the laws of sacrifice. They maintain the doctrine that study is preparatory but does not fulfill the physical mitzvah, thus perpetuating the sense of unfulfilled obligation. Their authority is grounded in lineage and tradition.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, rabbinic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Benefit from the continued expectation of physical sacrifice, as it reinforces the need for a messianic era and the rebuilding of the Temple. Their identity is tied to this future restoration, making the current unfulfilled state a source of meaning and purpose.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, messianic_aspirants, beneficiary,
    moderate, civilizational, identity_locked, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains the historical and theological continuity of the divine command for sacrifice, ensuring that the obligation is not forgotten or reinterpreted away, thus coordinating the collective memory and future aspiration of the Jewish people.
% TRANSFER_FUNCTION: Transfers a sense of unfulfilled divine obligation and a longing for messianic restoration to the Jewish people, from the divine command itself.
% ABSENT_VOICES: Those who might argue for a purely spiritual or symbolic fulfillment of sacrifice are implicitly excluded by the strict adherence to physical performance. Their voices are present in other readings of the kernel, but not within this framework.
% DISAPPEARANCE_RATIONALE: If the obligation for physical sacrifice vanished, or if study were deemed to fulfill it, a foundational pillar of Jewish eschatology and collective identity would collapse. The longing for the Temple, the messianic expectation, and the very structure of rabbinic authority would be profoundly altered, leading to a complete rearrangement of religious life and meaning.
% FOUNDING_PROBLEM: The divine command for animal sacrifice as a central act of worship and atonement, as detailed in the Torah.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic authorities universally attest that the founding problem (the divine command) is still live. Historical texts and communal prayers across millennia corroborate the enduring nature of this obligation, even in its unfulfilled state. The entire liturgical tradition is built around this expectation.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__performance_only_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__performance_only_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__performance_only_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(sacrifice_obligation_kernel__performance_only_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__performance_only_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   Extractiveness is extremely high (0.95) because the entire Jewish people bears the burden of an unfulfilled divine command for nearly two millennia. This is not extraction by an agent, but a structural gap between command and capacity. Suppression is also extremely high (0.99) due to the physical impossibility of performing sacrifices without a rebuilt Temple and a divinely sanctioned priesthood, coupled with the theological prohibition against unauthorized performance. Accessibility collapse is near total (0.99) as no halakhically valid alternative exists for fulfilling the physical mitzvah. Resistance is low (0.05) because the impossibility is widely accepted within the tradition, and the focus is on maintaining the aspiration rather than actively resisting the constraint. Theater ratio is low (0.1) as the study of sacrifice laws is genuinely preparatory and not a mere performance masking a defunct function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Jewish people, this is a profound and enduring burden. From the perspective of messianic aspirants, it is a necessary condition for future redemption. Rabbinic authorities navigate this tension by emphasizing study as a means of maintaining readiness and merit, without claiming it fulfills the physical mitzvah.
 *
 * DIRECTIONALITY LOGIC:
 *   The Jewish people are the primary targets/payers, bearing the weight of the unfulfilled obligation (d=1.0). Rabbinic authorities act as agenda-setters, maintaining the interpretation that perpetuates this state. Messianic aspirants are beneficiaries, as the unfulfilled obligation fuels their eschatological hopes. There are no direct beneficiaries in the sense of an agent collecting rents from the unfulfilled obligation; the 'extraction' is a structural consequence of the reading itself.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    nature_of_unfulfilled_obligation,
    'Is the ''extraction'' of unfulfilled obligation a genuine burden, or is it transformed into a spiritual merit through study and longing?',
    'Theological and phenomenological inquiry into the lived experience of the Jewish people, and analysis of rabbinic texts on the nature of ''zechut'' (merit) in the absence of performance.',
    'If transformed into merit, the effective extractiveness of this constraint would be lower, as the ''cost'' is offset by spiritual gain. If it remains a pure burden, the high extractiveness is accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nature_of_unfulfilled_obligation, conceptual, 'Ambiguity in the spiritual accounting of unfulfilled divine commands.').

omega_variable(
    natural_law_vs_constructed_impossibility,
    'Is the impossibility of sacrifice a ''natural law'' (Mountain) or a ''constructed constraint'' (Snare) maintained by rabbinic interpretation?',
    'Analysis of alternative halakhic interpretations that might permit symbolic or spiritual fulfillment, and the historical processes by which these were foreclosed or marginalized. If viable alternatives were suppressed, it leans towards constructed.',
    'If constructed, the claimed_type ''mountain'' would be a false summit, reclassifying to a ''snare'' or ''tangled_rope'' depending on beneficiary structure. If genuinely natural, the mountain classification holds.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_impossibility, conceptual, 'Whether the physical impossibility is an irreducible fact or a consequence of interpretive choices.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__performance_only_reading, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 500, 0.1).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(sacr_tr_t1950, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 1950, 0.1).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 0, 0.95).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 500, 0.95).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1000, 0.95).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1500, 0.95).
narrative_ontology:measurement(sacr_be_t1950, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 1950, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0, 0.99).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 500, 0.99).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1000, 0.99).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1500, 0.99).
narrative_ontology:measurement(sacr_su_t1950, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 1950, 0.99).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__performance_only_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'sacrifice_obligation_kernel'. Its high extractiveness and suppression are due to the structural impossibility of fulfilling the physical mitzvah, which is a core tenet of this reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
