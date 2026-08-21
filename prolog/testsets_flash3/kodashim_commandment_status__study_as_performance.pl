% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__study_as_performance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__study_as_performance, []).

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
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Kodashim Commandment Status: Study as Performance
 *   domain: religious_studies/halakhic_theory
 *
 * SUMMARY:
 *   This constraint represents the Halakhic (Jewish legal) reading that the
 *   study of the laws of sacrifices (Kodashim) is itself considered a
 *   fulfillment of the commandment, particularly in the absence of the
 *   Temple. This interpretation ensures the continuity of the commandment's
 *   spiritual force through intellectual engagement. It is claimed as a
 *   Mountain due to its perceived theological necessity and deep integration
 *   into the tradition, with negligible extraction as it provides a path to
 *   observance rather than imposing burdens.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.05).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.02).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.01).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.05).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.02).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.01).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, 0.01).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, mountain).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Kodashim Commandment Status: Study as Performance").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious_studies/halakhic_theory").

domain_priors:emerges_naturally(kodashim_commandment_status__study_as_performance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, '1946aec2-4e2c-4b1c-98d4-03e580cf8cb2').
narrative_ontology:cs_kernel_codification('1946aec2-4e2c-4b1c-98d4-03e580cf8cb2', fixed_text).
narrative_ontology:cs_authority_grounding('1946aec2-4e2c-4b1c-98d4-03e580cf8cb2', lineage).
narrative_ontology:cs_interpretation_layer_present('1946aec2-4e2c-4b1c-98d4-03e580cf8cb2').
narrative_ontology:cs_reading_relation('1946aec2-4e2c-4b1c-98d4-03e580cf8cb2', kodashim_commandment_status__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('1946aec2-4e2c-4b1c-98d4-03e580cf8cb2', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('1946aec2-4e2c-4b1c-98d4-03e580cf8cb2', foundational, torah_study_equivalent_to_performance).
narrative_ontology:cs_axiom_status(torah_study_equivalent_to_performance, holdable).
narrative_ontology:cs_axiom_grounding('1946aec2-4e2c-4b1c-98d4-03e580cf8cb2', torah_study_equivalent_to_performance, theological).
narrative_ontology:cs_reference_frame('1946aec2-4e2c-4b1c-98d4-03e580cf8cb2', post_temple_rabbinic_consensus).
narrative_ontology:cs_drift_state('1946aec2-4e2c-4b1c-98d4-03e580cf8cb2', contemporary_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1946aec2-4e2c-4b1c-98d4-03e580cf8cb2', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, halakhic_scholars).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, observant_jews).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, torah_study_as_ultimate_value).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__study_as_performance, intellectual_engagement_as_spiritual_fulfillment).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their intellectual and spiritual work is directly validated as fulfilling a core commandment, providing purpose and legitimacy to their scholarly pursuits. Their identity is deeply intertwined with this interpretive framework.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, halakhic_scholars, beneficiary,
    institutional, generational, identity_locked, global).

% Find spiritual fulfillment and a path to commandment observance through study, even in the absence of the Temple. This reading provides a continuous, accessible means of religious practice, reinforcing their communal identity.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, observant_jews, beneficiary,
    organized, biographical, identity_locked, global).

% Believe the commandment is suspended until the Messianic era, with study serving as preparation. While they engage in study, they would argue that it does not fully substitute for performance, creating a subtle tension with the 'study as performance' reading.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, messianic_deferral_proponents, excluded,
    organized, generational, constrained, global).

% Hold that the commandment is entirely contingent on the Temple's existence and cannot be fulfilled in its absence. They would view 'study as performance' as a theological innovation that dilutes the original intent, but their voice is marginalized in mainstream discourse.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, performance_only_proponents, excluded,
    moderate, biographical, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a continuous, accessible, and intellectually rigorous path for observant Jews to fulfill the commandment of sacrifices, even without the physical Temple, thereby maintaining communal engagement with the divine law.
% TRANSFER_FUNCTION: Transfers spiritual merit and a sense of active observance to individuals and the community through intellectual engagement, rather than through physical ritual performance.
% ABSENT_VOICES: Proponents of the 'performance only' reading are largely excluded from the dominant discourse, as their interpretation would leave a significant void in contemporary religious practice. They would argue that study is not a substitute for actual performance.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the underlying commandment would still exist, but observant Jews would lose a primary means of engaging with it, leading to a significant spiritual and intellectual void. However, the physical reality of the Temple's absence would remain unchanged.
% FOUNDING_PROBLEM: The destruction of the Second Temple left observant Jews without the means to fulfill the central commandments related to sacrifices, creating a profound spiritual and practical crisis regarding the continuity of divine law.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinic literature from the Talmudic era onwards consistently grapples with the problem of commandment fulfillment post-Temple destruction, corroborating the ongoing spiritual challenge this interpretation addresses. This is attested by historical texts and theological commentaries from across the tradition, not just by those who benefit from this specific reading.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__study_as_performance, world_unchanged).
narrative_ontology:founding_problem_status(kodashim_commandment_status__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kodashim_commandment_status__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__study_as_performance, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__study_as_performance_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, ExtMetricName, E),
    domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kodashim_commandment_status__study_as_performance),
    narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kodashim_commandment_status__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.05) because this reading offers a solution to a spiritual problem, providing a path to fulfillment rather than extracting resources or imposing costs. Suppression is minimal (0.02) as it's a widely accepted theological position, not enforced coercively. Theater ratio is negligible (0.01) as the study is considered genuinely efficacious. Accessibility collapse is high (0.95) because, for this reading, alternatives to study for fulfilling the commandment are almost entirely collapsed by the Temple's absence. Resistance is very low (0.01) as this reading is foundational for much of post-Temple Judaism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, the constraint is a natural theological truth that emerged from necessity, offering a path to spiritual continuity. From the perspective of those who believe only physical performance counts, this reading might be seen as a 'convenient' reinterpretation, but it does not impose costs on them.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars and observant Jews are direct beneficiaries, as this reading validates their spiritual and intellectual efforts as commandment fulfillment. There are no direct victims, as the reading resolves a problem rather than creating one. Proponents of alternative readings are 'excluded' in the sense that their interpretations are not dominant, but they are not actively suppressed or harmed by this reading's existence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_theological_construct,
    'Is the fulfillment of the Kodashim commandment through study a natural theological truth, or a rabbinic construct developed to address the Temple''s destruction?',
    'Deep historical and theological analysis of early rabbinic sources, examining the explicit arguments and implicit assumptions underlying this interpretive shift. This would involve tracing the evolution of the concept of ''Torah study as equivalent to sacrifice''.',
    'If a construct, its ''emerges_naturally'' status would be reclassified to false, potentially shifting its type from Mountain to Rope or even Tangled Rope if identifiable beneficiaries actively maintain the construct for institutional gain. If a natural truth, its Mountain classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_theological_construct, conceptual, 'Ambiguity regarding the inherent vs. constructed nature of this theological interpretation.').

omega_variable(
    spiritual_efficacy_measurement,
    'How can the ''spiritual efficacy'' of study, as a substitute for physical sacrifice, be assessed or compared across different readings?',
    'This is a conceptual omega, likely unresolvable empirically. Resolution would depend on adopting a specific theological framework''s internal criteria for spiritual merit, which is a preference-based choice.',
    'If a framework could be agreed upon, it might reveal that this reading''s ''efficacy'' is lower than actual performance, potentially increasing its perceived extractiveness (as it offers a ''lesser'' fulfillment). However, within its own framework, it is full fulfillment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(spiritual_efficacy_measurement, preference, 'The inherent difficulty in objectively measuring or comparing spiritual efficacy across different theological interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__study_as_performance, theater_ratio, 0, 0.01).
narrative_ontology:measurement(koda_tr_t25, kodashim_commandment_status__study_as_performance, theater_ratio, 25, 0.01).
narrative_ontology:measurement(koda_tr_t50, kodashim_commandment_status__study_as_performance, theater_ratio, 50, 0.01).
narrative_ontology:measurement(koda_tr_t75, kodashim_commandment_status__study_as_performance, theater_ratio, 75, 0.01).
narrative_ontology:measurement(koda_tr_t100, kodashim_commandment_status__study_as_performance, theater_ratio, 100, 0.01).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__study_as_performance, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(koda_be_t25, kodashim_commandment_status__study_as_performance, base_extractiveness, 25, 0.05).
narrative_ontology:measurement(koda_be_t50, kodashim_commandment_status__study_as_performance, base_extractiveness, 50, 0.05).
narrative_ontology:measurement(koda_be_t75, kodashim_commandment_status__study_as_performance, base_extractiveness, 75, 0.05).
narrative_ontology:measurement(koda_be_t100, kodashim_commandment_status__study_as_performance, base_extractiveness, 100, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__study_as_performance, suppression_requirement, 0, 0.02).
narrative_ontology:measurement(koda_su_t25, kodashim_commandment_status__study_as_performance, suppression_requirement, 25, 0.02).
narrative_ontology:measurement(koda_su_t50, kodashim_commandment_status__study_as_performance, suppression_requirement, 50, 0.02).
narrative_ontology:measurement(koda_su_t75, kodashim_commandment_status__study_as_performance, suppression_requirement, 75, 0.02).
narrative_ontology:measurement(koda_su_t100, kodashim_commandment_status__study_as_performance, suppression_requirement, 100, 0.02).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__study_as_performance, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
