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
 *   constraint_id: kodashim_commandment_status__study_as_performance
 *   human_readable: Kodashim Commandment Status: Study as Performance
 *   domain: religious_studies/halakhic_theory/commitment_system
 *
 * SUMMARY:
 *   This constraint describes the halakhic interpretation that studying the
 *   laws of sacrifices (Kodashim) is considered a fulfillment of the
 *   commandment itself, particularly in the absence of the Temple. This
 *   reading provides a continuous path for religious observance and identity
 *   maintenance. It is one reading of the 'kodashim_commandment_status'
 *   kernel, which addresses the ongoing relevance and fulfillment of
 *   sacrifice laws after the Temple's destruction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__study_as_performance, 0.08).
domain_priors:suppression_score(kodashim_commandment_status__study_as_performance, 0.15).
domain_priors:theater_ratio(kodashim_commandment_status__study_as_performance, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, extractiveness, 0.08).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(kodashim_commandment_status__study_as_performance, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__study_as_performance, rope).
narrative_ontology:human_readable(kodashim_commandment_status__study_as_performance, "Kodashim Commandment Status: Study as Performance").
narrative_ontology:topic_domain(kodashim_commandment_status__study_as_performance, "religious_studies/halakhic_theory/commitment_system").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__study_as_performance, '38d35721-29af-473a-8762-737bb5086af6').
narrative_ontology:cs_kernel_codification('38d35721-29af-473a-8762-737bb5086af6', fixed_text).
narrative_ontology:cs_authority_grounding('38d35721-29af-473a-8762-737bb5086af6', lineage).
narrative_ontology:cs_interpretation_layer_present('38d35721-29af-473a-8762-737bb5086af6').
narrative_ontology:cs_reading_relation('38d35721-29af-473a-8762-737bb5086af6', kodashim_commandment_status__performance_only, forecloses).
narrative_ontology:cs_reading_relation('38d35721-29af-473a-8762-737bb5086af6', kodashim_commandment_status__messianic_deferral, coexists_with).
narrative_ontology:cs_axiom('38d35721-29af-473a-8762-737bb5086af6', foundational, study_is_equivalent_to_performance).
narrative_ontology:cs_axiom_status(study_is_equivalent_to_performance, holdable).
narrative_ontology:cs_axiom_grounding('38d35721-29af-473a-8762-737bb5086af6', study_is_equivalent_to_performance, deontological).
narrative_ontology:cs_reference_frame('38d35721-29af-473a-8762-737bb5086af6', rabbinic_interpretive_tradition).
narrative_ontology:cs_drift_state('38d35721-29af-473a-8762-737bb5086af6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('38d35721-29af-473a-8762-737bb5086af6', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__study_as_performance, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, halakhic_scholars).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__study_as_performance, jewish_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These scholars interpret and transmit the halakha, asserting that the intellectual engagement with Kodashim (sacrifice laws) constitutes a fulfillment of the commandment itself in the absence of the Temple. Their professional and spiritual identity is deeply intertwined with this interpretive framework.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, halakhic_scholars, agenda_setter,
    institutional, generational, identity_locked, global).

% The broader Jewish community benefits from this interpretation by maintaining a continuous connection to the divine commandments and the tradition, even when physical performance is impossible. It provides a path for spiritual engagement and continuity.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, jewish_community, beneficiary,
    organized, generational, constrained, global).

% Adherents of the 'messianic deferral' reading believe the commandment is suspended until the Messiah's arrival and the Temple's rebuilding, with study serving as preparation, not fulfillment. Their view is acknowledged but not central to this reading's operational logic.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, messianic_deferral_adherents, excluded,
    moderate, civilizational, identity_locked, global).

% Adherents of the 'performance only' reading believe the commandment is entirely suspended without the Temple, rendering study a mere academic exercise rather than a form of fulfillment. This reading directly contradicts the core premise of 'study as performance'.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, performance_only_adherents, excluded,
    moderate, generational, identity_locked, global).

% Scholars of religious studies or commitment systems who analyze the structural function of this interpretation within the broader halakhic framework, without necessarily adhering to its theological claims.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__study_as_performance, analytical_observers, observer,
    analytical, biographical, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It coordinates the Jewish community's ongoing engagement with the divine commandment of sacrifices, providing a legitimate and accessible means of fulfillment in the absence of the Temple, thereby maintaining the continuity of religious practice and identity.
% TRANSFER_FUNCTION: It transfers the locus of commandment fulfillment from physical ritual performance to intellectual and spiritual engagement, from the Temple to the study hall, allowing the 'spiritual capital' of the commandment to continue flowing to the community.
% ABSENT_VOICES: Adherents of the 'performance only' reading would object, arguing that study cannot substitute for actual performance and that the commandment is truly suspended. Their voices are excluded from the operational logic of this reading, which asserts study as fulfillment.
% DISAPPEARANCE_RATIONALE: If this interpretation vanished, the Jewish community would face a significant spiritual void regarding the Kodashim commandments, potentially leading to a sense of incompleteness or obsolescence for a major part of the Torah. The entire framework of engagement with these laws would need to be re-established, profoundly impacting religious identity and practice.
% FOUNDING_PROBLEM: The destruction of the Second Temple (70 CE) rendered the physical performance of animal sacrifices impossible, creating a crisis regarding the fulfillment of numerous biblical commandments related to the Temple service.
% FOUNDING_PROBLEM_CORROBORATION: The problem of non-performance of sacrifices remains live for the Jewish community, as the Temple has not been rebuilt. This is corroborated by historical texts, liturgical practices (prayers for the Temple's rebuilding), and ongoing halakhic discourse from various rabbinic authorities and communal leaders, not just those who benefit from this specific interpretation.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__study_as_performance, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__study_as_performance, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__study_as_performance, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kodashim_commandment_status__study_as_performance, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__study_as_performance, 0.08, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__study_as_performance_tests).
:- end_tests(kodashim_commandment_status__study_as_performance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.08) because this interpretation genuinely solves a problem for the community without imposing significant costs or extracting rents; it offers a spiritual benefit. Suppression is low (0.15) as it's an interpretive stance, not enforced coercion, though communal norms may encourage it. Theater ratio is low (0.05) because the study is considered a sincere and effective form of religious observance, not a mere performance. Accessibility collapse is moderate (0.60) because while physical performance is impossible, intellectual engagement is widely accessible. Resistance is low (0.05) as this is a widely accepted and spiritually beneficial interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading's adherents, the constraint is a beneficial 'Rope' that coordinates spiritual life. From the 'performance only' perspective, it might be seen as a 'Piton' or even a 'Snare' if they believe it distracts from the true (suspended) nature of the commandment. The engine's classification will reflect the structural data, not the internal claims of any single reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic scholars and the Jewish community are beneficiaries, as this interpretation allows for continued spiritual engagement and the maintenance of religious identity. There are no direct 'victims' as the interpretation aims to provide a solution, not to extract. Those who hold alternative readings (messianic deferral, performance only) are excluded from this reading's operational logic, but not actively harmed by it.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_vs_performance_equivalence,
    'To what extent does intellectual study truly substitute for physical ritual performance in fulfilling the divine commandment?',
    'Theological and philosophical analysis of the nature of divine command and human obligation, potentially informed by comparative religious studies on symbolic vs. literal observance.',
    'If study is deemed a partial or imperfect substitute, the ''extractiveness'' of the non-performance (the gap) might be higher, and the ''claimed_type'' could shift towards a ''Tangled Rope'' or ''Piton'' if the ''fulfillment'' is seen as theatrical.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(study_vs_performance_equivalence, conceptual, 'Ambiguity regarding the equivalence of study and performance for commandment fulfillment.').

omega_variable(
    kernel_reading_distinction,
    'Is this constraint a genuine, structurally distinct reading of the ''kodashim_commandment_status'' kernel, or merely a nuanced variant of a sibling reading?',
    'Detailed textual analysis of primary halakhic sources and their reception history, focusing on the explicit claims made about the *status* of the commandment during study, not just the *value* of study.',
    'If it''s a mere variant, the distinct ''constraint_id'' would be merged into a sibling, simplifying the kernel''s decomposition. If distinct, the current classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_distinction, conceptual, 'Clarifying the structural distinctiveness of this kernel reading from its siblings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__study_as_performance, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_commandment_status__study_as_performance, theater_ratio, 0, 0.05).
narrative_ontology:measurement(koda_tr_t400, kodashim_commandment_status__study_as_performance, theater_ratio, 400, 0.05).
narrative_ontology:measurement(koda_tr_t800, kodashim_commandment_status__study_as_performance, theater_ratio, 800, 0.05).
narrative_ontology:measurement(koda_tr_t1200, kodashim_commandment_status__study_as_performance, theater_ratio, 1200, 0.05).
narrative_ontology:measurement(koda_tr_t1600, kodashim_commandment_status__study_as_performance, theater_ratio, 1600, 0.05).
narrative_ontology:measurement(koda_tr_t2000, kodashim_commandment_status__study_as_performance, theater_ratio, 2000, 0.05).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_commandment_status__study_as_performance, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(koda_be_t400, kodashim_commandment_status__study_as_performance, base_extractiveness, 400, 0.08).
narrative_ontology:measurement(koda_be_t800, kodashim_commandment_status__study_as_performance, base_extractiveness, 800, 0.08).
narrative_ontology:measurement(koda_be_t1200, kodashim_commandment_status__study_as_performance, base_extractiveness, 1200, 0.08).
narrative_ontology:measurement(koda_be_t1600, kodashim_commandment_status__study_as_performance, base_extractiveness, 1600, 0.08).
narrative_ontology:measurement(koda_be_t2000, kodashim_commandment_status__study_as_performance, base_extractiveness, 2000, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(koda_su_t0, kodashim_commandment_status__study_as_performance, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(koda_su_t400, kodashim_commandment_status__study_as_performance, suppression_requirement, 400, 0.15).
narrative_ontology:measurement(koda_su_t800, kodashim_commandment_status__study_as_performance, suppression_requirement, 800, 0.15).
narrative_ontology:measurement(koda_su_t1200, kodashim_commandment_status__study_as_performance, suppression_requirement, 1200, 0.15).
narrative_ontology:measurement(koda_su_t1600, kodashim_commandment_status__study_as_performance, suppression_requirement, 1600, 0.15).
narrative_ontology:measurement(koda_su_t2000, kodashim_commandment_status__study_as_performance, suppression_requirement, 2000, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__study_as_performance, identity_coordination).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__study_as_performance, kodashim_commandment_status__messianic_deferral).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'kodashim_commandment_status' kernel, each representing a distinct halakhic interpretation of the sacrifice laws in the absence of the Temple. Each reading has a different ε value and structural implications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
