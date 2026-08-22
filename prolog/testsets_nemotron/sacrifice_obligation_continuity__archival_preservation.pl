% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__archival_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
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
 *   constraint_id: sacrifice_obligation_continuity__archival_preservation
 *   human_readable: Sacrifice Obligation as Archival Preservation (No Binding Force)
 *   domain: religious/ritual/textual_tradition
 *
 * SUMMARY:
 *   This constraint story represents the archival_preservation reading of the
 *   sacrifice_obligation_continuity kernel: after the destruction of the
 *   Second Temple (70 CE), the biblical obligation to offer sacrifices
 *   (korbanot) ceased to be binding. The textual corpus — Leviticus, Numbers,
 *   the Talmudic order Kodashim — is preserved and studied as cultural
 *   memory, literary heritage, and historical record, but carries no
 *   normative force. No one is obligated to perform sacrifices; no one is
 *   penalized for not studying the laws; the constraint has exited constraint
 *   space entirely. The reading is held by academic scholars of rabbinics,
 *   secular Jewish studies, and some liberal religious movements that treat
 *   the sacrificial system as historically completed.
 *
 * KEY AGENTS:
 *   - academic_scholars_rabbinics: Observer (analytical/institutional) — studies the texts as historical-philological objects
 *   - secular_jewish_studies_practitioners: Observer (organized) — engages the material as cultural heritage
 *   - liberal_religious_movements: Observer (organized) — treats sacrificial law as historically superseded
 *   - traditional_halakhic_authorities: Excluded (institutional) — rejects this reading; holds competing readings (messianic_suspension, study_as_performance, performance_only)
 *   - messianic_activists: Excluded (organized) — actively prepares for restoration; views archival reading as denial
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
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__archival_preservation, mountain).
narrative_ontology:human_readable(sacrifice_obligation_continuity__archival_preservation, "Sacrifice Obligation as Archival Preservation (No Binding Force)").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__archival_preservation, "religious/ritual/textual_tradition").

domain_priors:emerges_naturally(sacrifice_obligation_continuity__archival_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__archival_preservation, '1205b08a-474a-4b38-9c12-e0f845f1aa3f').
narrative_ontology:cs_kernel_codification('1205b08a-474a-4b38-9c12-e0f845f1aa3f', fixed_text).
narrative_ontology:cs_authority_grounding('1205b08a-474a-4b38-9c12-e0f845f1aa3f', lineage).
narrative_ontology:cs_reading_relation('1205b08a-474a-4b38-9c12-e0f845f1aa3f', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('1205b08a-474a-4b38-9c12-e0f845f1aa3f', sacrifice_obligation_continuity__messianic_suspension, forecloses).
narrative_ontology:cs_reading_relation('1205b08a-474a-4b38-9c12-e0f845f1aa3f', sacrifice_obligation_continuity__performance_only, forecloses).
narrative_ontology:cs_axiom('1205b08a-474a-4b38-9c12-e0f845f1aa3f', foundational, korbanot_obligation_lapsed_with_temple).
narrative_ontology:cs_axiom_status(korbanot_obligation_lapsed_with_temple, holdable).
narrative_ontology:cs_axiom_grounding('1205b08a-474a-4b38-9c12-e0f845f1aa3f', korbanot_obligation_lapsed_with_temple, empirically_contingent).
narrative_ontology:cs_axiom('1205b08a-474a-4b38-9c12-e0f845f1aa3f', foundational, textual_study_is_cultural_not_ritual).
narrative_ontology:cs_axiom_status(textual_study_is_cultural_not_ritual, holdable).
narrative_ontology:cs_axiom_grounding('1205b08a-474a-4b38-9c12-e0f845f1aa3f', textual_study_is_cultural_not_ritual, conventional).
narrative_ontology:cs_reference_frame('1205b08a-474a-4b38-9c12-e0f845f1aa3f', rabbinic_transition_post_70ce).
narrative_ontology:cs_drift_state('1205b08a-474a-4b38-9c12-e0f845f1aa3f', contemporary_academic_consensus, gap(stable, minor, true)).
narrative_ontology:cs_created_at('1205b08a-474a-4b38-9c12-e0f845f1aa3f', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity).

% --- Structural relationships ---
% No enrichment needed. As a Mountain (physical limit), this constraint does
% not have beneficiaries or victims in the structural sense.

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__archival_preservation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(sacrifice_obligation_continuity__archival_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__archival_preservation, 0.0, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__archival_preservation_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
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
 *   Extractiveness is zero because the reading makes no normative claim on anyone — no transfer of resources, time, or status is demanded. Suppression is zero because no alternatives are blocked; one may study, ignore, or reject the material freely. Theater ratio is zero because there is no performative maintenance of a defunct function — the study *is* the function (cultural preservation). Accessibility collapse is near-total (0.95) because once the historical fact of Temple destruction and the rabbinic consensus on suspension are accepted, the alternatives (binding obligation, physical performance) are cognitively unavailable — this is the mountain signature. Resistance is near-zero (0.05) because the reading faces no organized opposition from within its own community; opposition comes from competing readings (external contestation), not internal resistance. The temporal measurements show the historical trajectory: high extraction/suppression/theater during the Temple era and early rabbinic transition, decaying to zero as the archival reading stabilizes.
 *
 * PERSPECTIVAL GAP:
 *   The seat divergence is minimal because this reading has no payer/beneficiary structure. The perspectival gap that *does* exist is between this reading (mountain, ε=0) and its siblings (tangled_rope/snare/scaffold with ε>0). The engine will compute the archival_preservation constraint as mountain from every seat; the sibling constraints will compute differently. The divergence is inter-constraint, not intra-constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   All named agents occupy the analytical/observer seat (d ≈ 0.5, symmetric). No agent is a beneficiary in the extraction sense — no one collects rents from the archival reading. No agent is a victim — no one bears costs. The traditional halakhic authorities and messianic activists are excluded from this reading's framework; they are not stakeholders *within* this constraint but operators of sibling constraints. This is why the reading is a mountain: it describes a structural fact (the obligation has lapsed) that is independent of anyone's enforcement or acceptance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining divine service after Temple destruction) is dead — the Temple is not being rebuilt in this reading's framework, and the sacrificial system is historically completed. The arrangement (archival study) does not persist to solve a live problem; it persists because cultural memory is self-sustaining. Mandatrophy is resolved: the mandate (binding sacrifice law) has outlived its function, and the successor practice (study) has shed the mandate entirely. No capture, no zombie institution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archival_vs_normative_boundary,
    'Does the study of sacrifice law in the archival preservation reading genuinely carry zero normative force, or does cultural memory inevitably re-normativize the text over time?',
    'Longitudinal study of how communities that adopt this reading transmit the material across generations: do descendant communities treat the studied texts as purely historical, or do ritual longing and messianic expectation re-emerge?',
    'If cultural memory reliably re-normativizes, the zero-extractiveness claim is unstable — the constraint would drift toward study_as_performance or messianic_suspension over generational time, making the mountain classification provisional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archival_vs_normative_boundary, conceptual, 'Whether archival study of sacrifice law can remain normatively inert across transmission').

omega_variable(
    kernel_framing_underdetermination,
    'Is the sacrifice_obligation_continuity kernel best framed as a single textual-historical problem (the fate of korbanot after 70 CE) or as a cluster of distinct successor commitments (study, messianic readiness, physical performance, symbolic substitution)?',
    'Comparative analysis of how each sibling reading''s community defines the kernel''s boundaries: do they recognize a shared referent, or do they talk past each other?',
    'If the kernel fractures into non-communicating referents, the decomposition into four constraint stories is not a reading split but a topic split — each story would be independent, and network.affects_constraints links would be misplaced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_underdetermination, conceptual, 'Whether the kernel is a unified contested commitment or a family resemblance cluster').

omega_variable(
    cs_framing_underdetermination,
    'Does the archival_preservation reading instantiate a commitment-system structure at all, or is it a post-commitment analytical stance that merely describes the kernel''s historical fate?',
    'Test whether the reading''s proponents treat the kernel as a legitimating authority they answer to (even negatively), or as a historical object they analyze from outside. The presence of interpretive disputes about *how* to study would signal CS dynamics; their absence would confirm post-commitment stance.',
    'If no CS structure, the cs_structure block should be omitted; if CS structure exists despite zero normative claim, the authority_grounding would be ''extraction'' (institutional benefit from declaring the kernel settled) or ''practice'' (academic community maintaining interpretive standards).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether archival preservation is a commitment-system reading or a post-commitment analytical frame').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__archival_preservation, 70, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t70, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 70, 0.75).
narrative_ontology:measurement(sacr_tr_t200, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 200, 0.45).
narrative_ontology:measurement(sacr_tr_t500, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 500, 0.25).
narrative_ontology:measurement(sacr_tr_t1000, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 1000, 0.12).
narrative_ontology:measurement(sacr_tr_t1500, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(sacr_tr_t1800, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 1800, 0.01).
narrative_ontology:measurement(sacr_tr_t2025, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 2025, 0.0).

% Extraction over time
narrative_ontology:measurement(sacr_be_t70, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 70, 0.85).
narrative_ontology:measurement(sacr_be_t200, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 200, 0.35).
narrative_ontology:measurement(sacr_be_t500, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 500, 0.12).
narrative_ontology:measurement(sacr_be_t1000, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 1000, 0.04).
narrative_ontology:measurement(sacr_be_t1500, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 1500, 0.01).
narrative_ontology:measurement(sacr_be_t1800, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 1800, 0.0).
narrative_ontology:measurement(sacr_be_t2025, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 2025, 0.0).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t70, sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 70, 0.8).
narrative_ontology:measurement(sacr_su_t200, sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 200, 0.4).
narrative_ontology:measurement(sacr_su_t500, sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 500, 0.15).
narrative_ontology:measurement(sacr_su_t1000, sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 1000, 0.05).
narrative_ontology:measurement(sacr_su_t1500, sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 1500, 0.02).
narrative_ontology:measurement(sacr_su_t1800, sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 1800, 0.0).
narrative_ontology:measurement(sacr_su_t2025, sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 2025, 0.0).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__archival_preservation, identity_coordination).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__messianic_suspension).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__performance_only).

% DUAL FORMULATION NOTE:
% The sacrifice_obligation_continuity kernel decomposes into four constraint stories linked by structural influence. Archival_preservation is the upstream 'settled fact' reading that the other readings must contend with: study_as_performance and messianic_suspension explicitly reject its core premise (obligation lapsed); performance_only treats it as the error to be corrected. The ε values differ radically: archival_preservation ε=0 (mountain); study_as_performance ε≈0.3 (tangled_rope — coordination + extraction via study-as-obligation); messianic_suspension ε≈0.15 (scaffold — transitional with messianic sunset); performance_only ε≈0.6 (snare — high extraction via physical performance demand).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
