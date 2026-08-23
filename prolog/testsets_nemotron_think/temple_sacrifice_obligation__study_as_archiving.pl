% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_archiving
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_archiving, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: temple_sacrifice_obligation__study_as_archiving
 *   human_readable: Temple Sacrifice Obligation (Study as Archiving Reading)
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   The Temple sacrifice obligation is a divine command that cannot be
 *   performed since the Temple's destruction in 70 CE. The study_as_archiving
 *   reading holds that studying the laws of sacrifices preserves knowledge
 *   for future restoration but does not fulfill the obligation. Thus the
 *   obligation remains binding, creating a permanent state of non-compliance
 *   for the Jewish people. The rabbinic authority maintains this binding
 *   status, deriving legitimacy from the claim that the divine command is
 *   eternal and unchangeable. The reading presents the obligation as a
 *   Mountain (divine law), but the metrics reveal moderate extractiveness:
 *   the authority benefits from maintaining an unperformable command, while
 *   the people bear the cost of unfulfilled obligation.
 *
 * KEY AGENTS:
 *   - rabbinic_authority: Primary beneficiary (institutional/analytical) — maintains binding status, derives legitimacy
 *   - jewish_people: Primary payer (organized/identity_locked) — bears spiritual cost of non-compliance, no exit
 *   - messianic_reading_adherents: Excluded (organized/constrained) — hold rival reading that obligation is suspended
 *   - study_as_occupation_adherents: Excluded (organized/constrained) — hold rival reading that study fulfills obligation
 *   - secular_observers: Observer (analytical/analytical) — external analytical seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, 0.52).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, 0.68).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_archiving, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, extractiveness, 0.52).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, accessibility_collapse, 0.91).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_archiving, mountain).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_archiving, "Temple Sacrifice Obligation (Study as Archiving Reading)").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_archiving, "religious/halakhic").

domain_priors:emerges_naturally(temple_sacrifice_obligation__study_as_archiving).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_archiving, '6ed81100-7e6c-4f7b-95fd-9881fd76d55c').
narrative_ontology:cs_kernel_codification('6ed81100-7e6c-4f7b-95fd-9881fd76d55c', fixed_text).
narrative_ontology:cs_authority_grounding('6ed81100-7e6c-4f7b-95fd-9881fd76d55c', lineage).
narrative_ontology:cs_interpretation_layer_present('6ed81100-7e6c-4f7b-95fd-9881fd76d55c').
narrative_ontology:cs_reading_relation('6ed81100-7e6c-4f7b-95fd-9881fd76d55c', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('6ed81100-7e6c-4f7b-95fd-9881fd76d55c', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_axiom('6ed81100-7e6c-4f7b-95fd-9881fd76d55c', foundational, study_is_archival_not_fulfillment).
narrative_ontology:cs_axiom_status(study_is_archival_not_fulfillment, holdable).
narrative_ontology:cs_axiom_grounding('6ed81100-7e6c-4f7b-95fd-9881fd76d55c', study_is_archival_not_fulfillment, conventional).
narrative_ontology:cs_axiom('6ed81100-7e6c-4f7b-95fd-9881fd76d55c', foundational, obligation_remains_binding).
narrative_ontology:cs_axiom_status(obligation_remains_binding, holdable).
narrative_ontology:cs_axiom_grounding('6ed81100-7e6c-4f7b-95fd-9881fd76d55c', obligation_remains_binding, conventional).
narrative_ontology:cs_reference_frame('6ed81100-7e6c-4f7b-95fd-9881fd76d55c', temple_standing_era).
narrative_ontology:cs_drift_state('6ed81100-7e6c-4f7b-95fd-9881fd76d55c', post_temple_exile, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('6ed81100-7e6c-4f7b-95fd-9881fd76d55c', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_archiving, rabbinic_authority).
narrative_ontology:constraint_victim(temple_sacrifice_obligation__study_as_archiving, jewish_people).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, binding_status_of_temple_sacrifice).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__study_as_archiving, divine_command_unfulfilled).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and maintains the binding status of the Temple sacrifice obligation; authorizes study as archival preservation but not fulfillment; derives legitimacy from continuity with divine command.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, rabbinic_authority, agenda_setter,
    institutional, generational, analytical, global).

% Bound by the unfulfillable obligation; required to study sacrifice laws as archival duty but cannot achieve fulfillment; bears spiritual cost of non-compliance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, jewish_people, payer,
    organized, generational, identity_locked, global).

% Hold the messianic_suspension reading; believe the obligation is suspended until messianic restoration; excluded from the study_as_archiving framework.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, messianic_reading_adherents, excluded,
    organized, generational, constrained, global).

% Hold the study_as_occupation reading; believe study of sacrifice law constitutes legitimate occupation of the obligation in the Temple's absence; excluded from the study_as_archiving framework.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, study_as_occupation_adherents, excluded,
    organized, generational, constrained, global).

% Analyze the halakhic system from outside; see the obligation as a historical construct and the study-as-archiving reading as a strategy for maintaining coherence.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__study_as_archiving, secular_observers, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the textual and procedural knowledge of Temple sacrifices across the exile period so that the system can be restored if the Temple is rebuilt.
% TRANSFER_FUNCTION: Moves the burden of the unfulfilled obligation onto the Jewish people as a collective spiritual deficit, while the rabbinic authority retains interpretive control and legitimacy.
% ABSENT_VOICES: Proponents of messianic_suspension (who say the obligation is suspended) and study_as_occupation (who say study fulfills the obligation) are excluded from this reading's framework; they would argue the obligation is either not currently binding or is fulfilled by study.
% DISAPPEARANCE_RATIONALE: The Temple sacrifice obligation is a central positive commandment; its removal would restructure the halakhic system's architecture of binding law and the rabbinic authority's claim to maintain unperformable commands.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), the halakhic system faced a crisis: the central positive commandments (sacrifices) could not be performed, threatening the system's coherence and the authority's legitimacy.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship (e.g., Schürer, Neusner) and competing halakhic readings attest the crisis; the rabbinic tradition itself records the debate (e.g., Talmudic discussions on whether study replaces sacrifice).
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__study_as_archiving, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__study_as_archiving, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_archiving, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_archiving, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__study_as_archiving, 0.52, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_archiving_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_archiving, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, ExtMetricName, E),
    domain_priors:suppression_score(temple_sacrifice_obligation__study_as_archiving, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(temple_sacrifice_obligation__study_as_archiving),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_archiving, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(temple_sacrifice_obligation__study_as_archiving_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52) because the constraint transfers a persistent spiritual deficit to the Jewish people while the rabbinic authority collects interpretive authority. Suppression is high (0.68) because alternatives (messianic suspension, study as occupation) are excluded from the framework; the binding status is enforced by identity_locked exit options. Theater ratio is moderate (0.33) because study is a real preservative activity but also performs the function of maintaining the obligation's visibility. Accessibility collapse is very high (0.91) because the obligation cannot be fulfilled and exit requires abandoning religious identity. Resistance is low (0.18) because the reading is widely accepted within its tradition.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic_authority seat (agenda_setter, institutional, analytical exit), the constraint appears as a Mountain: a divine law that structures the system. From the jewish_people seat (payer, organized, identity_locked), the same constraint operates as a persistent extraction with no exit. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic_authority is the structural beneficiary (collects interpretive authority, controls the framework — d near beneficiary end). The jewish_people are the targets (bear the cost of unfulfilled obligation, identity_locked exit — d near target end). The excluded groups are not coordinated by this reading; their exclusion is part of the suppression mechanism. The observer seat sees the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Temple destruction creating a crisis of unperformable commandments) is contested: some readings claim it is resolved (messianic_suspension, study_as_occupation). The study_as_archiving reading maintains the problem is live, preserving the obligation's binding status. This prevents mislabeling the coordination function (knowledge preservation) as pure extraction, but the moderate extractiveness and high suppression suggest the arrangement has drifted from its founding justification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_ambiguity,
    'Is the binding status of the Temple sacrifice obligation a genuine natural/divine law (Mountain) or a constructed constraint maintained by the rabbinic authority for legitimacy?',
    'Comparative analysis of halakhic development: if the binding status persists only through continuous rabbinic interpretation and not through an independent divine revelation, it is constructed.',
    'If constructed, the constraint is a false summit Mountain (FSM candidate) and would reclassify as tangled_rope or snare; the rabbinic_authority beneficiary declaration would trigger FSM.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_ambiguity, conceptual, 'Whether the obligation''s binding status is a natural law or a human-maintained construct.').

omega_variable(
    coordination_extraction_boundary,
    'Is the archival study function a genuine coordination need (preserving knowledge for restoration) or a cover for maintaining the authority''s extractive position?',
    'Counterfactual: if the Temple were rebuilt tomorrow, would the preserved knowledge suffice for immediate restoration, or has the study tradition diverged from practical requirements?',
    'If the coordination function is genuine, the constraint is a Tangled Rope (coordination + extraction). If the coordination function is illusory, it is a Snare (pure extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_boundary, empirical, 'Whether the archival coordination function is real or performative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_archiving, 0, 1954).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 0, 0.25).
narrative_ontology:measurement(temp_tr_t400, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 400, 0.28).
narrative_ontology:measurement(temp_tr_t800, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 800, 0.3).
narrative_ontology:measurement(temp_tr_t1200, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1200, 0.31).
narrative_ontology:measurement(temp_tr_t1600, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1600, 0.32).
narrative_ontology:measurement(temp_tr_t1954, temple_sacrifice_obligation__study_as_archiving, theater_ratio, 1954, 0.33).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(temp_be_t400, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 400, 0.48).
narrative_ontology:measurement(temp_be_t800, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 800, 0.5).
narrative_ontology:measurement(temp_be_t1200, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1200, 0.51).
narrative_ontology:measurement(temp_be_t1600, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1600, 0.52).
narrative_ontology:measurement(temp_be_t1954, temple_sacrifice_obligation__study_as_archiving, base_extractiveness, 1954, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(temp_su_t400, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 400, 0.63).
narrative_ontology:measurement(temp_su_t800, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 800, 0.65).
narrative_ontology:measurement(temp_su_t1200, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1200, 0.67).
narrative_ontology:measurement(temp_su_t1600, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1600, 0.68).
narrative_ontology:measurement(temp_su_t1954, temple_sacrifice_obligation__study_as_archiving, suppression_requirement, 1954, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_archiving, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__study_as_archiving, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__messianic_suspension).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_archiving, temple_sacrifice_obligation__study_as_occupation).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the temple_sacrifice_obligation kernel. The three readings differ on the status of the obligation (binding/suspended/fulfilled-by-study) and the function of study. They form a constraint family linked by kernel_id.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
