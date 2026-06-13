% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__archival_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_continuity__archival_preservation, []).

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
 *   constraint_id: sacrifice_obligation_continuity__archival_preservation
 *   human_readable: Sacrifice Law as Historical Archive: Study Preserves Textual Tradition Without Normative Obligation
 *   domain: religious_law/textual_tradition/ritual_studies
 *
 * SUMMARY:
 *   After the Second Temple's destruction (70 CE), Jewish communities faced
 *   the impossibility of performing sacrifice law. The archival-preservation
 *   reading holds that the law is no longer binding; study of sacrifice texts
 *   preserves textual tradition and cultural memory without normative force.
 *   This reading treats the law as a historical and cultural object, not as a
 *   current obligation. The constraint is claimed as a mountain (natural,
 *   non-extractive, zero suppression) because after physical sacrifice became
 *   impossible, study emerged as the natural mode of engagement with the
 *   texts. However, this reading is one of four contested interpretations of
 *   the same kernel: messianic suspension (obligation suspended pending
 *   restoration), performance-only (physical restoration is the true
 *   obligation, study is preparation), and study-as-performance (textual
 *   engagement itself fulfills the commandment, obligation persists). The
 *   archival reading claims the obligation has exited constraint space
 *   entirely; the sibling readings contest this by holding that obligation
 *   persists in different forms. The structural delta is therefore
 *   fundamental: zero extractiveness because there is no normative claim to
 *   enforce.
 *
 * KEY AGENTS:
 *   - archival_scholars: Engage texts as historical/literary objects; extract cultural meaning through analysis; primary bearers of the textual tradition in the archival frame
 *   - religious_community_study_practitioners: Engage through study and remembrance; receive cultural continuity and meaning without obligation; mobile exit
 *   - messianic_obligation_holders: Excluded; hold suspension (not abrogation) of obligation; represent a fundamentally different reading
 *   - performance_restoration_advocates: Excluded; argue physical performance is the true obligation; represent foreclosure against archival reading
 *   - study_as_performance_interpreters: Excluded; hold that study fulfills obligation; coexist with archival reading in live theological debate
 *   - historical_textual_tradition: Non-agent; the body of texts and commentaries; is vindicated (as object of legitimate study) by the archival reading
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__archival_preservation, 0.0).
domain_priors:suppression_score(sacrifice_obligation_continuity__archival_preservation, 0.0).
domain_priors:theater_ratio(sacrifice_obligation_continuity__archival_preservation, 0.0).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, extractiveness, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 0.0).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, theater_ratio, 0.0).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__archival_preservation, mountain).
narrative_ontology:human_readable(sacrifice_obligation_continuity__archival_preservation, "Sacrifice Law as Historical Archive: Study Preserves Textual Tradition Without Normative Obligation").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__archival_preservation, "religious_law/textual_tradition/ritual_studies").

domain_priors:emerges_naturally(sacrifice_obligation_continuity__archival_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__archival_preservation, 'd32e691e-b7a9-4e56-8d6f-232b19ffab25').
narrative_ontology:cs_kernel_codification('d32e691e-b7a9-4e56-8d6f-232b19ffab25', fixed_text).
narrative_ontology:cs_authority_grounding('d32e691e-b7a9-4e56-8d6f-232b19ffab25', lineage).
narrative_ontology:cs_interpretation_layer_present('d32e691e-b7a9-4e56-8d6f-232b19ffab25').
narrative_ontology:cs_reading_relation('d32e691e-b7a9-4e56-8d6f-232b19ffab25', sacrifice_obligation_continuity__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('d32e691e-b7a9-4e56-8d6f-232b19ffab25', sacrifice_obligation_continuity__performance_only, forecloses).
narrative_ontology:cs_reading_relation('d32e691e-b7a9-4e56-8d6f-232b19ffab25', sacrifice_obligation_continuity__study_as_performance, coexists_with).
narrative_ontology:cs_axiom('d32e691e-b7a9-4e56-8d6f-232b19ffab25', foundational, obligation_abrogated_not_suspended).
narrative_ontology:cs_axiom_status(obligation_abrogated_not_suspended, holdable).
narrative_ontology:cs_axiom_grounding('d32e691e-b7a9-4e56-8d6f-232b19ffab25', obligation_abrogated_not_suspended, deontological).
narrative_ontology:cs_axiom('d32e691e-b7a9-4e56-8d6f-232b19ffab25', foundational, study_without_performative_equivalence).
narrative_ontology:cs_axiom_status(study_without_performative_equivalence, holdable).
narrative_ontology:cs_axiom_grounding('d32e691e-b7a9-4e56-8d6f-232b19ffab25', study_without_performative_equivalence, deontological).
narrative_ontology:cs_reference_frame('d32e691e-b7a9-4e56-8d6f-232b19ffab25', textual_preservation_post_destruction).
narrative_ontology:cs_drift_state('d32e691e-b7a9-4e56-8d6f-232b19ffab25', contemporary_institutional_embeddedness, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d32e691e-b7a9-4e56-8d6f-232b19ffab25', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, textual_tradition_bearers).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, historical_memory_community).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__archival_preservation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__archival_preservation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__archival_preservation_tests).

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, ExtMetricName, E),
    domain_priors:suppression_score(sacrifice_obligation_continuity__archival_preservation, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(sacrifice_obligation_continuity__archival_preservation),
    narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(sacrifice_obligation_continuity__archival_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The archival-preservation reading authors zero extractiveness because it denies any normative claim. No party enforces obligation; no party extracts from refusal or non-compliance; no party pays for the constraint's maintenance. The law has exited obligation space entirely and become a cultural historical object. Accessibility collapse is very high (0.95) because once the archival interpretation is understood, alternatives to treating the texts as historical artifacts become cognitively difficult—the interpretive frame shapes what the texts can mean. Resistance is minimal (0.1) because the archival framing does not demand anything of practitioners (it is permissive, not prescriptive); those who study do so by choice, those who do not are not in violation. Theater ratio is zero because there is no performative gap—the constraint is what it claims to be (preservation of tradition without obligation). The measurements are flat across all 1956 years because the archival reading (once established post-Temple destruction) produces no dynamic drift: extraction does not accumulate, suppression does not intensify. The constraint is structurally stable. This stasis is not evidence of a mountain; it is a measurement fact. The omega variables address the only unstable elements: whether the reading is truly natural or disguises foreclosure, and whether suppression is genuinely absent or is applied to marginalize competing readings.
 *
 * PERSPECTIVAL GAP:
 *   There is no perspectival gap because the archival reading has no internal seats with opposed directionalities. All stakeholders (scholars, practitioners) occupy beneficiary positions—they receive cultural inheritance without bearing costs. Messianics and performance advocates are excluded from the constraint entirely because their readings are incommensurable with the archival frame. The engine would compute each reading as a separate constraint with its own seats; here, only the archival reading is authored. Within that single reading, the gap that would emerge is between this constraint (zero extraction) and the sibling constraints (messianic, performance, study-as-performance), which would show extraction or obligation-bearing. That gap is the measurement the kernel framework exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   There is no directionality computation because there is no extraction. All named beneficiaries (textual-tradition-bearers, historical-memory-community) receive cultural goods without cost. Excluded stakeholders (messianic, performance, study-as-performance holders) are not seated in this constraint because their readings affirm obligation, which contradicts the archival frame's core claim. Their directionality would be computed from the constraints that represent their readings, not this one. The analytical observers (archival scholars) occupy an observer seat (d not computed, boundary condition). This pure beneficiary structure is itself a structural fact: the constraint coordinates without asymmetric extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preserve textual tradition after physical sacrifice became impossible) was live at t=70 and is classified as dead at t=2026. The archival-preservation reading asserts the problem was solved: the texts are preserved, tradition is maintained, study persists as the engagement mode. However, the excluded stakeholders contest this: messianic and performance readings assert the problem is not solved but suspended or deferred. The mismatch detector reads founding_problem_status=dead × disappearance_verdict=world_unchanged and would flag a zombie-constraint hypothesis (obligation persists despite being declared dead). However, this flag is inapplicable here: the archival reading's claim is that obligation is not persisting but has exited. The mismatch exists only if one compares the archival reading to the sibling readings. Within the archival reading alone, the mandatrophy state is resolved: the obligation genuinely exited; study genuinely replaced it; the constraint is not a piton but a mountain (natural state, not inertial performance).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_interpretive_choice,
    'Is the archival-preservation reading a feature of objective textual reality (after sacrifice became impossible, study naturally became the only option), or is it one interpretive choice among contested readings held by different communities?',
    'Ethnographic and historical study of communities that reject the archival reading (messianic suspension, performance restoration, study-as-performance): do they treat their readings as equally natural, or as deliberately chosen against external pressure?',
    'If archival preservation is one choice among live alternatives, the constraint''s classification as a mountain (zero extraction, natural emergence) may be false; the constraint may be a snare or tangled rope whose ''naturality'' is a cover story for foreclosing competing interpretations. If it is genuinely the only natural option (performance is impossible, suspension is internally incoherent, study-as-performance is a recent innovation), the mountain classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_interpretive_choice, conceptual, 'Whether archival preservation is an objective constraint or an interpretive choice disguised as natural law.').

omega_variable(
    beneficiary_identity_and_extraction,
    'Do the named beneficiaries (textual tradition bearers, historical memory community) actually benefit from the archival framing, or does the framing extract from them by denying the law''s normative force?',
    'Elicit testimony from practitioners about whether archival framing enhances or diminishes their engagement, meaning-making, and spiritual practice. Compare communities that embrace the archival frame to those that resist it.',
    'If the archival framing is experienced as a loss or denial of obligation, the beneficiaries are actually targets, and the constraint is a snare (the archival frame is the cover story). If experienced as liberating and enabling, the zero-extraction reading holds and the constraint is a true mountain.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(beneficiary_identity_and_extraction, empirical, 'Whether the archival frame''s beneficiaries experience genuine benefit or concealed extraction.').

omega_variable(
    foreclosure_vs_coexistence_with_study_as_performance,
    'Does the archival-preservation reading logically foreclose the study-as-performance reading (obligation persists through textual engagement), or do both remain coherent interpretations of the tradition?',
    'Examine whether a community could consistently hold that (a) the law is no longer binding as normative obligation AND (b) engaged study fulfills the commandment. If both can be held without internal contradiction, the readings coexist; if (a) logically entails denial of (b)''s core claim, the readings foreclose each other.',
    'If they coexist, the archival reading''s relation to study-as-performance is coexists_with. If the archival reading''s denial of normative force logically rules out study''s obligatory character, the relation is forecloses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foreclosure_vs_coexistence_with_study_as_performance, conceptual, 'Logical coherence of archival reading with study-as-performance reading.').

omega_variable(
    suppression_mechanism_in_textual_tradition,
    'Is the zero measured suppression honest, or does the archival frame suppress competing readings (messianic, performance, study-as-performance) by naturalizing the archival interpretation?',
    'Document the institutional and social mechanisms that establish archival preservation as the default reading in scholarly and communal contexts. Are competing readings actively marginalized, or do they freely coexist as equally legitimate alternatives?',
    'If competing readings are marginalized or excluded (as the excluded stakeholder descriptions suggest), the measured suppression is artificially low; the constraint may be a snare disguised as a mountain. If competing readings are genuinely permitted to coexist, the zero suppression is honest.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_in_textual_tradition, empirical, 'Whether measured zero suppression accurately reflects suppression of competing interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__archival_preservation, 70, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 70, 0.0).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 500, 0.0).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 1000, 0.0).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 1500, 0.0).
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 1900, 0.0).
narrative_ontology:measurement(sacr_tr_t2026, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 2026, 0.0).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 70, 0.0).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 500, 0.0).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 1000, 0.0).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 1500, 0.0).
narrative_ontology:measurement(sacr_be_t1900, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 1900, 0.0).
narrative_ontology:measurement(sacr_be_t2026, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 2026, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t70, sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 70, 0.0).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 500, 0.0).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 1000, 0.0).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 1500, 0.0).
narrative_ontology:measurement(sacr_su_t1900, sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 1900, 0.0).
narrative_ontology:measurement(sacr_su_t2026, sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 2026, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__archival_preservation, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__study_as_performance).

% DUAL FORMULATION NOTE:
% This constraint is part of a four-reading kernel family: sacrifice_obligation_continuity decomposed into archival_preservation, messianic_suspension, performance_only, and study_as_performance. Each reading is a structurally distinct constraint with its own ε, beneficiary/victim structure, and type classification. The family is linked by network.affects_constraints in all four directions because each reading's dominance or decline in a community affects the institutional viability of the others. The archival reading (this file) is the foundational reading historically—after 70 CE, it emerged as the only feasible mode; the other readings are secondary innovations that contest the archival frame. The archival reading influences all three siblings by establishing the default interpretive context against which they define themselves.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
