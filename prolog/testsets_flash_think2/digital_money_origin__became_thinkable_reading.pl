% ============================================================================
% CONSTRAINT STORY: digital_money_origin__became_thinkable_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_digital_money_origin__became_thinkable_reading, []).

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
 *   constraint_id: digital_money_origin__became_thinkable_reading
 *   human_readable: Digital Money Origin: Conceivability as Emergence
 *   domain: monetary_history/technology_studies/institutional_economics
 *
 * SUMMARY:
 *   This constraint defines the origin of digital money as the point when its
 *   concept became technically and institutionally conceivable, rather than
 *   when it was first implemented or formally recognized. This reading
 *   emphasizes the intellectual and infrastructural preconditions for digital
 *   money's emergence, framing it as a natural progression of technological
 *   and institutional thought. The constraint is claimed as a Mountain due to
 *   its presentation as an inevitable conceptual shift, but the presence of
 *   beneficiaries triggers False Summit Mountain (FSM) detection, indicating
 *   a potential constructed element.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(digital_money_origin__became_thinkable_reading, 0.35).
domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, 0.4).
domain_priors:theater_ratio(digital_money_origin__became_thinkable_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, 0.85).
narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(digital_money_origin__became_thinkable_reading, mountain).
narrative_ontology:human_readable(digital_money_origin__became_thinkable_reading, "Digital Money Origin: Conceivability as Emergence").
narrative_ontology:topic_domain(digital_money_origin__became_thinkable_reading, "monetary_history/technology_studies/institutional_economics").

domain_priors:emerges_naturally(digital_money_origin__became_thinkable_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(digital_money_origin__became_thinkable_reading, 'afba7f68-7425-421f-af40-6bfad31d110f').
narrative_ontology:cs_kernel_codification('afba7f68-7425-421f-af40-6bfad31d110f', implicit).
narrative_ontology:cs_authority_grounding('afba7f68-7425-421f-af40-6bfad31d110f', expertise).
narrative_ontology:cs_interpretation_layer_present('afba7f68-7425-421f-af40-6bfad31d110f').
narrative_ontology:cs_reading_relation('afba7f68-7425-421f-af40-6bfad31d110f', digital_money_origin__first_held_reading, coexists_with).
narrative_ontology:cs_reading_relation('afba7f68-7425-421f-af40-6bfad31d110f', digital_money_origin__regulatory_recognition_reading, coexists_with).
narrative_ontology:cs_axiom('afba7f68-7425-421f-af40-6bfad31d110f', foundational, technological_precondition_axiom).
narrative_ontology:cs_axiom_status(technological_precondition_axiom, holdable).
narrative_ontology:cs_axiom_grounding('afba7f68-7425-421f-af40-6bfad31d110f', technological_precondition_axiom, empirically_contingent).
narrative_ontology:cs_axiom('afba7f68-7425-421f-af40-6bfad31d110f', foundational, institutional_acceptance_precondition_axiom).
narrative_ontology:cs_axiom_status(institutional_acceptance_precondition_axiom, holdable).
narrative_ontology:cs_axiom_grounding('afba7f68-7425-421f-af40-6bfad31d110f', institutional_acceptance_precondition_axiom, conventional).
narrative_ontology:cs_reference_frame('afba7f68-7425-421f-af40-6bfad31d110f', post_cybernetics_era_conceptual_space).
narrative_ontology:cs_drift_state('afba7f68-7425-421f-af40-6bfad31d110f', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('afba7f68-7425-421f-af40-6bfad31d110f', '').
narrative_ontology:cs_kernel_id(digital_money_origin__became_thinkable_reading, digital_money_origin).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, early_institutional_architects).
narrative_ontology:constraint_beneficiary(digital_money_origin__became_thinkable_reading, technological_visionaries).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, alternative_conceptual_frameworks).
narrative_ontology:constraint_victim(digital_money_origin__became_thinkable_reading, excluded_early_innovators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Defined the conceptual and institutional groundwork that made digital money 'thinkable' within established paradigms. They benefit from this framing as it legitimizes their foundational contributions.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, early_institutional_architects, agenda_setter,
    institutional, generational, arbitrage, global).

% Their ideas and proposals gained traction and legitimacy as the concept of digital money became technically and institutionally conceivable. They benefit from the historical credit this reading assigns.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, technological_visionaries, beneficiary,
    powerful, biographical, mobile, global).

% Represent non-mainstream or earlier ideas about digital money that did not fit the dominant technical/institutional 'conceivability' framework. They are excluded from the primary historical narrative of origin.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, alternative_conceptual_frameworks, excluded,
    powerless, generational, trapped, universal).

% Individuals or groups whose practical innovations or proto-digital money systems existed but were not recognized as the 'origin' due to the conceptual framing of this reading. They bear the cost of historical marginalization.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, excluded_early_innovators, payer,
    moderate, biographical, constrained, global).

% Analyze and interpret the historical emergence of digital money, often engaging in debates about its precise origin point and the criteria for 'emergence'.
narrative_ontology:constraint_stakeholder(digital_money_origin__became_thinkable_reading, monetary_historians, observer,
    analytical, generational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(digital_money_origin__became_thinkable_reading, diffuse).
narrative_ontology:fixing_cost_class(digital_money_origin__became_thinkable_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared conceptual framework for understanding the emergence of digital money, allowing for coordinated research, development, and policy discussions based on a common historical starting point.
% TRANSFER_FUNCTION: Transfers intellectual legitimacy and historical credit to those who first conceived of digital money within established technical and institutional paradigms, often at the expense of those with alternative or earlier practical but unrecognized innovations.
% ABSENT_VOICES: Early innovators or theorists whose ideas didn't fit the dominant technical/institutional 'conceivability' framework. They would argue for a more inclusive or practice-based origin story, emphasizing actual use over theoretical possibility.
% DISAPPEARANCE_RATIONALE: If this conceptual constraint vanished, the entire historical narrative of digital money's origin would be rewritten. This would impact the perceived legitimacy and foundational claims of current digital monetary systems, their architects, and the academic fields built upon this understanding, leading to a significant reorganization of historical and theoretical frameworks.
% FOUNDING_PROBLEM: To establish a clear, authoritative historical origin point for digital money, providing a stable foundation for its study, regulation, and the attribution of intellectual and institutional credit.
% FOUNDING_PROBLEM_CORROBORATION: Monetary historians and technology scholars outside the direct beneficiary groups corroborate the ongoing debate about the precise origin point, even if they disagree on the specific reading. Legislative hearings and academic publications frequently revisit these foundational questions.
narrative_ontology:disappearance_verdict(digital_money_origin__became_thinkable_reading, world_rearranges).
narrative_ontology:founding_problem_status(digital_money_origin__became_thinkable_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(digital_money_origin__became_thinkable_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(digital_money_origin__became_thinkable_reading, 'none', 1).
narrative_ontology:epsilon_provenance(digital_money_origin__became_thinkable_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(digital_money_origin__became_thinkable_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, ExtMetricName, E),
    domain_priors:suppression_score(digital_money_origin__became_thinkable_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(digital_money_origin__became_thinkable_reading),
    narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(digital_money_origin__became_thinkable_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(digital_money_origin__became_thinkable_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) is moderate, reflecting the conceptual gatekeeping that prioritizes certain intellectual lineages and institutional frameworks. Suppression (0.4) is also moderate, as alternative conceptualizations or earlier, unrecognized innovations are sidelined. Theater ratio (0.1) is low, as the constraint primarily concerns a historical interpretation rather than ongoing performance. Accessibility collapse (0.85) is high because, from this reading's perspective, once digital money is 'thinkable,' the exclusive reliance on physical money conceptually collapses. Resistance (0.2) is low, as the debate is primarily academic and historical, not active opposition to an ongoing operation.
 *
 * PERSPECTIVAL GAP:
 *   While this reading presents the emergence as a natural conceptual evolution, alternative perspectives (e.g., from early innovators) would view it as a constructed narrative that selectively grants historical credit. The engine's FSM detection will highlight this tension between the 'natural' claim and the presence of identifiable beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   Early institutional architects and technological visionaries are beneficiaries (low d) as this reading legitimizes their foundational contributions and intellectual lineage. Alternative conceptual frameworks and excluded early innovators are victims (high d) because their contributions are marginalized or ignored by this dominant historical narrative. Monetary historians act as observers, analyzing the dynamics without direct benefit or cost from the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    origin_definition_ambiguity,
    'Is the ''origin'' of digital money fundamentally a conceptual, practical, or regulatory event?',
    'Consensus among interdisciplinary scholars (historians, economists, technologists) on a primary criterion for ''origin,'' or a clear shift in policy/academic discourse towards one definition.',
    'If resolved as primarily practical, this reading''s claim of ''conceivability'' as origin would be weakened, potentially reclassifying it from a Mountain to a more constructed type. If regulatory, this reading would be seen as a precursor, not the origin itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(origin_definition_ambiguity, conceptual, 'Ambiguity in the fundamental definition of ''origin'' for digital money.').

omega_variable(
    false_summit_naturality_ambiguity,
    'Is the ''emergence'' of digital money as a conceivable concept a genuine natural progression of thought and technology, or a constructed narrative that benefits specific intellectual and institutional lineages?',
    'Detailed historical and sociological analysis tracing the influence networks and funding sources behind the dominant conceptual frameworks, and comparing them to marginalized alternatives. Evidence of active suppression of alternative narratives would support the ''constructed'' view.',
    'If found to be a constructed narrative, the constraint would be reclassified from a Mountain (via FSM) to a Tangled Rope or Snare, reflecting the active maintenance of a beneficial historical account.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_naturality_ambiguity, empirical, 'Whether the conceptual emergence is a natural phenomenon or a constructed historical narrative.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(digital_money_origin__became_thinkable_reading, 1970, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(digi_tr_t1970, digital_money_origin__became_thinkable_reading, theater_ratio, 1970, 0.05).
narrative_ontology:measurement(digi_tr_t1975, digital_money_origin__became_thinkable_reading, theater_ratio, 1975, 0.06).
narrative_ontology:measurement(digi_tr_t1980, digital_money_origin__became_thinkable_reading, theater_ratio, 1980, 0.07).
narrative_ontology:measurement(digi_tr_t1985, digital_money_origin__became_thinkable_reading, theater_ratio, 1985, 0.08).
narrative_ontology:measurement(digi_tr_t1990, digital_money_origin__became_thinkable_reading, theater_ratio, 1990, 0.09).
narrative_ontology:measurement(digi_tr_t1995, digital_money_origin__became_thinkable_reading, theater_ratio, 1995, 0.095).
narrative_ontology:measurement(digi_tr_t2000, digital_money_origin__became_thinkable_reading, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(digi_be_t1970, digital_money_origin__became_thinkable_reading, base_extractiveness, 1970, 0.25).
narrative_ontology:measurement(digi_be_t1975, digital_money_origin__became_thinkable_reading, base_extractiveness, 1975, 0.28).
narrative_ontology:measurement(digi_be_t1980, digital_money_origin__became_thinkable_reading, base_extractiveness, 1980, 0.3).
narrative_ontology:measurement(digi_be_t1985, digital_money_origin__became_thinkable_reading, base_extractiveness, 1985, 0.32).
narrative_ontology:measurement(digi_be_t1990, digital_money_origin__became_thinkable_reading, base_extractiveness, 1990, 0.33).
narrative_ontology:measurement(digi_be_t1995, digital_money_origin__became_thinkable_reading, base_extractiveness, 1995, 0.34).
narrative_ontology:measurement(digi_be_t2000, digital_money_origin__became_thinkable_reading, base_extractiveness, 2000, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(digi_su_t1970, digital_money_origin__became_thinkable_reading, suppression_requirement, 1970, 0.3).
narrative_ontology:measurement(digi_su_t1975, digital_money_origin__became_thinkable_reading, suppression_requirement, 1975, 0.32).
narrative_ontology:measurement(digi_su_t1980, digital_money_origin__became_thinkable_reading, suppression_requirement, 1980, 0.35).
narrative_ontology:measurement(digi_su_t1985, digital_money_origin__became_thinkable_reading, suppression_requirement, 1985, 0.37).
narrative_ontology:measurement(digi_su_t1990, digital_money_origin__became_thinkable_reading, suppression_requirement, 1990, 0.38).
narrative_ontology:measurement(digi_su_t1995, digital_money_origin__became_thinkable_reading, suppression_requirement, 1995, 0.39).
narrative_ontology:measurement(digi_su_t2000, digital_money_origin__became_thinkable_reading, suppression_requirement, 2000, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
