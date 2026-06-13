% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__study_as_occupation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__study_as_occupation, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: temple_sacrifice_obligation__study_as_occupation
 *   human_readable: Study of Sacrifice Law as Legitimate Occupation of Temple Obligation
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   After the Temple's destruction in 70 CE, Jewish law preserved the
 *   obligation to bring offerings but made performance impossible. Rabbinic
 *   interpretation developed the reading that study of sacrifice law
 *   constitutes legitimate occupation of the obligation in the Temple's
 *   absence. This constraint is ONE READING of the contested kernel
 *   temple_sacrifice_obligation. The study-as-occupation reading claims that
 *   intellectual engagement with sacrificial law fulfills the obligation
 *   relationally — the obligation persists and is genuinely occupied through
 *   study, not merely archived or suspended. This reading coexists with two
 *   sibling readings: messianic-suspension (the obligation waits, neither
 *   fulfilled nor violated) and study-as-archiving (study preserves knowledge
 *   for future restoration but does not fulfill the obligation). The
 *   constraint itself carries minimal extractiveness because study is the
 *   obligation's current, legitimate form — no victim set exists; no one is
 *   coerced into compliance because the constraint aligns participants'
 *   interests (scholars benefit from interpretive authority, communities
 *   benefit from a coherent practice modality). The theater ratio is modest
 *   (0.22) because some portion of the ongoing study elaboration serves
 *   rhetorical and identity-maintenance functions alongside genuine legal
 *   interpretation.
 *
 * KEY AGENTS:
 *   - rabbinic_scholars: institutional beneficiary and agenda-setter. Possess the interpretive authority to declare study as legitimate occupation; their interpretive labor constitutes the occupation itself.
 *   - jewish_communities: organized beneficiary. Sustain the obligation through participation in study-based ritual cycles and support for scholars.
 *   - messianic_restoration_awaiting_movement: moderate-power observer. Sees study as placeholder rather than occupation; awaits actual Temple performance.
 *   - alternative_halakhic_authority_holders: excluded moderate-power actors. Would introduce competing readings but are marginalized from consensus-setting.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__study_as_occupation, 0.12).
domain_priors:suppression_score(temple_sacrifice_obligation__study_as_occupation, 0.08).
domain_priors:theater_ratio(temple_sacrifice_obligation__study_as_occupation, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, extractiveness, 0.12).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__study_as_occupation, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__study_as_occupation, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__study_as_occupation, "Study of Sacrifice Law as Legitimate Occupation of Temple Obligation").
narrative_ontology:topic_domain(temple_sacrifice_obligation__study_as_occupation, "religious/halakhic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__study_as_occupation, '6e08cee7-aaed-48e6-b44c-ba773f88f6c3').
narrative_ontology:cs_kernel_codification('6e08cee7-aaed-48e6-b44c-ba773f88f6c3', fixed_text).
narrative_ontology:cs_authority_grounding('6e08cee7-aaed-48e6-b44c-ba773f88f6c3', lineage).
narrative_ontology:cs_interpretation_layer_present('6e08cee7-aaed-48e6-b44c-ba773f88f6c3').
narrative_ontology:cs_reading_relation('6e08cee7-aaed-48e6-b44c-ba773f88f6c3', temple_sacrifice_obligation__messianic_suspension, coexists_with).
narrative_ontology:cs_reading_relation('6e08cee7-aaed-48e6-b44c-ba773f88f6c3', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_axiom('6e08cee7-aaed-48e6-b44c-ba773f88f6c3', foundational, study_occupies_obligation_intrinsically).
narrative_ontology:cs_axiom_status(study_occupies_obligation_intrinsically, holdable).
narrative_ontology:cs_axiom_grounding('6e08cee7-aaed-48e6-b44c-ba773f88f6c3', study_occupies_obligation_intrinsically, deontological).
narrative_ontology:cs_axiom('6e08cee7-aaed-48e6-b44c-ba773f88f6c3', foundational, rabbinic_interpretation_binding).
narrative_ontology:cs_axiom_status(rabbinic_interpretation_binding, holdable).
narrative_ontology:cs_axiom_grounding('6e08cee7-aaed-48e6-b44c-ba773f88f6c3', rabbinic_interpretation_binding, conventional).
narrative_ontology:cs_reference_frame('6e08cee7-aaed-48e6-b44c-ba773f88f6c3', study_as_legitimate_occupation).
narrative_ontology:cs_drift_state('6e08cee7-aaed-48e6-b44c-ba773f88f6c3', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('6e08cee7-aaed-48e6-b44c-ba773f88f6c3', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, rabbinic_scholars).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__study_as_occupation, jewish_communities).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__study_as_occupation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(temple_sacrifice_obligation__study_as_occupation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__study_as_occupation_tests).
:- end_tests(temple_sacrifice_obligation__study_as_occupation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12 at interval end) because the constraint is structurally integrative: no party is forced to accept loss or constraint from another party's gain. Rabbinic scholars gain interpretive authority and community status; communities gain a coherent practice modality. The extraction that does exist is minimal and consists of: (1) the differential authority granted to scholars (not all interpretations are equally valid — the constraint privileges rabbinic over lay interpretation, a modest asymmetry), and (2) the implicit cost to alternatives (communities that might wish to read the obligation as suspended or to release it entirely have that position marginalized). Suppression is very low (0.08) because the constraint persists not through coercion but through alignment of interests and because alternatives are marginalized through interpretive authority rather than active enforcement. Theater ratio is modest because the constraint does real work (it genuinely solves the obligation-without-performance problem) but also serves identity-maintenance (emphasizing the continuity of Jewish law after institutional collapse) and communal meaning-making functions that are performative in character. The measurement series is stable across the interval because the constraint has been in steady state for nearly 2,000 years — no significant drift in extractiveness, suppression, or theater ratio. Accessibility collapse is high (0.78) because once the constraint is understood (study occupies the obligation), alternatives become intellectually difficult to sustain within a single coherent framework — the constraint absorbs the interpretive space. Resistance is low (0.15) because the constraint aligns with beneficiary interests and because resistance to it comes from outside the authoritative interpretive community (marginal voices) rather than from those who actually live the practice.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is minimal because the constraint is integrative, not extractive. All seated parties (rabbinic scholars, communities, messianic-restoration-awaiting movement) broadly agree that study-as-occupation is at least a coherent and legitimate reading; they may disagree about whether it is permanent or provisional, but not about whether it is structurally valid. The marginal disagreement sits with excluded voices (alternative readings) rather than seated parties. The engine should compute the rabbinic-scholar seat as beneficiary and the community seat as beneficiary-with-minimal-target-load, with no seat strongly opposing the constraint from within the authoritative framework.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic scholars sit as beneficiaries (d near 0.0–0.2 range): they gain interpretive authority, community status, and the institutional role that the constraint creates. Jewish communities sit as beneficiaries (d near 0.15–0.30 range): they gain a coherent practice modality and sustained religious identity, though constrained by the obligation itself (cannot simply release the obligation or choose alternative modalities). The obligation is a genuine constraint they bear, but one they regard as binding and meaningful, not extractive. The messianic-restoration-awaiting movement sits as an observer (d analytical): their resistance to the study-as-occupation reading is not enough to override the consensus within mainstream rabbinic institutions. Alternative authority holders are excluded (not in the directionality computation, but their exclusion means their target position is not registered). No pure-target seat exists in this constraint; the lowest-power seat (alternative authority holders) is excluded rather than extracted from. This is structurally different from a snare or tangled rope, where identifiable victims bear costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits no mandatrophy: the founding problem (obligation-without-performance after Temple destruction) remains live, the constraint genuinely solves it, and the solution is not performing obsolete functions for inertial reasons. The study-as-occupation reading has been continuously elaborated and refined throughout rabbinic history, indicating that the mandate is actively renewed, not merely repeated. However, the existence of sibling readings (messianic-suspension, study-as-archiving) indicates that the constraint is not universally accepted as the sole legitimate reading — it is contested but dominant. This is not mandatrophy (where a solution persists despite the problem being gone) but rather a live constraint operating within a contested interpretive field.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_as_genuine_occupation_vs_substitute,
    'Does study of sacrifice law constitute genuine occupation of the obligation, or is it a substitute modality that the obligation has de facto shifted to due to impossibility of performance?',
    'Textual and hermeneutical analysis of rabbinic sources asking whether the obligation itself changed (study became the original performance-modality) or only its execution-form changed (study replaces animal sacrifice but the obligation remains fundamentally about sacrifice). Comparison with other religious traditions'' handling of impossible obligations (Hindu fire sacrifice, Christian sacrifice interpretations) to identify structural parallels.',
    'If study is genuine occupation, the constraint is structurally integrative and low-extractive. If study is a substitute modality, the constraint may be higher-extractive (imposed by rabbis rather than derived from the obligation''s nature) and may generate resistance from those who dispute the interpretive authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_as_genuine_occupation_vs_substitute, conceptual, 'Whether study fulfills or replaces the obligation.').

omega_variable(
    permanence_vs_provisionality,
    'Is study-as-occupation a permanent resolution of the obligation-without-performance problem, or a provisional solution pending messianic restoration?',
    'Analysis of rabbinic discourse across centuries: does the tradition treat study-as-occupation as final, or as explicitly temporary? Do references to the future Temple imply the current study-based modality is always-already understood as provisional?',
    'If permanent, the constraint is a genuine new coordination form. If provisional, it carries a hidden dependency on messianic restoration that may increase theater ratio and complicate the classification if messianic expectation fades.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permanence_vs_provisionality, conceptual, 'Whether the study-as-occupation reading is final or waiting.').

omega_variable(
    excluded_voice_representation,
    'What proportion of contemporary Jewish communities and scholars actively assent to the study-as-occupation reading versus holding the constraint as merely dominant institutional position?',
    'Survey of contemporary Jewish religious thought, comparison of textual traditions emphasizing different readings, ethnographic evidence from different Jewish communities'' practice and emphasis.',
    'High assent would confirm the constraint as genuinely integrative. Lower assent might indicate the constraint''s dominance relies more on institutional authority than on consensus, increasing effective suppression and extractiveness.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(excluded_voice_representation, empirical, 'Degree of actual vs. enforced assent to study-as-occupation reading.').

omega_variable(
    reading_drift_over_time,
    'Has the study-as-occupation reading''s framing or emphasis shifted over the 2,000-year interval? Has the rhetorical weight on identity-maintenance versus genuine legal interpretation shifted?',
    'Diachronic analysis of rabbinic texts, commentaries, and modern Jewish writings on the obligation. Track whether the justification for study-as-occupation emphasizes its intrinsic rightness versus its necessity-given-Temple-absence versus its role in sustaining Jewish continuity.',
    'A drift toward identity-maintenance emphasis would suggest rising theater ratio and potential classification movement toward piton. Stable emphasis on intrinsic rightness would suggest theater ratio remains low.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_drift_over_time, empirical, 'Whether the reading''s functional justification has shifted over centuries.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__study_as_occupation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 0, 0.18).
narrative_ontology:measurement(temp_tr_t5, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 5, 0.19).
narrative_ontology:measurement(temp_tr_t10, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 10, 0.21).
narrative_ontology:measurement(temp_tr_t15, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 15, 0.22).
narrative_ontology:measurement(temp_tr_t20, temple_sacrifice_obligation__study_as_occupation, theater_ratio, 20, 0.22).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(temp_be_t5, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 5, 0.11).
narrative_ontology:measurement(temp_be_t10, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 10, 0.12).
narrative_ontology:measurement(temp_be_t15, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 15, 0.12).
narrative_ontology:measurement(temp_be_t20, temple_sacrifice_obligation__study_as_occupation, base_extractiveness, 20, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 0, 0.07).
narrative_ontology:measurement(temp_su_t5, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 5, 0.075).
narrative_ontology:measurement(temp_su_t10, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 10, 0.08).
narrative_ontology:measurement(temp_su_t15, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 15, 0.08).
narrative_ontology:measurement(temp_su_t20, temple_sacrifice_obligation__study_as_occupation, suppression_requirement, 20, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__study_as_occupation, resource_allocation).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_obligation__study_as_occupation, 0.1).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__messianic_suspension).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__study_as_occupation, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% temple_sacrifice_obligation is a contested kernel with three structurally distinct constraint readings. Each reading—study_as_occupation, messianic_suspension, study_as_archiving—instantiates a different obligation-modality and authority structure. They are linked via network.affects_constraints because the dominant reading (study_as_occupation) marginalizes and influences the plausibility of the other readings. Study-as-occupation is the steady-state institutional reading; messianic_suspension and study_as_archiving are the live alternatives. All three are authored as separate constraint stories with distinct epsilon-values, beneficiary/victim structures, and authority groundings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
