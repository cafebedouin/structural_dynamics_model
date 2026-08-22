% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__performance_only, []).

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
    narrative_ontology:suppression_profile/2,
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
 *   constraint_id: temple_sacrifice_commitment__performance_only
 *   human_readable: Temple Sacrifice Commitment â Performance-Only Reading
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the performance-only reading of the
 *   temple_sacrifice_commitment kernel. The reading holds that the biblical
 *   commandments regarding Temple sacrifice require material instantiation
 *   and cannot be occupied through study, prayer, or symbolic substitution.
 *   In the current post-Temple era, this renders the commitment a dormant
 *   husk: study of sacrificial law functions as a low-epsilon coordination
 *   mechanism that preserves expertise and communal orientation toward future
 *   restoration, but does not constitute performance of the commandment
 *   itself. The constraint is currently non-extractive â no victim set
 *   exists â but carries latent extractive potential should messianic
 *   restoration be attempted without parallel ethical or political evolution.
 *
 * KEY AGENTS:
 *   - rabbinic_authorities: Agenda-setters (institutional/constrained) â transmit and enforce the performance-only interpretation
 *   - traditional_students: Beneficiaries (moderate/constrained) â coordinated by preparatory study, receiving meaning and continuity
 *   - symbolic_reading_adherents: Excluded voices (moderate/mobile) â hold alternative readings but are delegitimated within this framework
 *   - religious_studies_scholars: Analytical observers (analytical/analytical) â trace the sociological function of the dormant commitment
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__performance_only, 0.18).
domain_priors:suppression_score(temple_sacrifice_commitment__performance_only, 0.22).
domain_priors:theater_ratio(temple_sacrifice_commitment__performance_only, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, extractiveness, 0.18).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__performance_only, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__performance_only, rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__performance_only, "Temple Sacrifice Commitment â Performance-Only Reading").
narrative_ontology:topic_domain(temple_sacrifice_commitment__performance_only, "religious_law/halakhic_tradition/commitment_system_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__performance_only, '7cf55668-4b67-4d00-a4dd-27f644f5f564').
narrative_ontology:cs_kernel_codification('7cf55668-4b67-4d00-a4dd-27f644f5f564', fixed_text).
narrative_ontology:cs_authority_grounding('7cf55668-4b67-4d00-a4dd-27f644f5f564', lineage).
narrative_ontology:cs_interpretation_layer_present('7cf55668-4b67-4d00-a4dd-27f644f5f564').
narrative_ontology:cs_reading_relation('7cf55668-4b67-4d00-a4dd-27f644f5f564', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('7cf55668-4b67-4d00-a4dd-27f644f5f564', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('7cf55668-4b67-4d00-a4dd-27f644f5f564', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('7cf55668-4b67-4d00-a4dd-27f644f5f564', foundational, material_instantiation_required).
narrative_ontology:cs_axiom_status(material_instantiation_required, holdable).
narrative_ontology:cs_axiom_grounding('7cf55668-4b67-4d00-a4dd-27f644f5f564', material_instantiation_required, theological).
narrative_ontology:cs_axiom('7cf55668-4b67-4d00-a4dd-27f644f5f564', foundational, study_is_non_occupational).
narrative_ontology:cs_axiom_status(study_is_non_occupational, holdable).
narrative_ontology:cs_axiom_grounding('7cf55668-4b67-4d00-a4dd-27f644f5f564', study_is_non_occupational, theological).
narrative_ontology:cs_reference_frame('7cf55668-4b67-4d00-a4dd-27f644f5f564', material_performance_framework).
narrative_ontology:cs_drift_state('7cf55668-4b67-4d00-a4dd-27f644f5f564', post_temple_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('7cf55668-4b67-4d00-a4dd-27f644f5f564', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__performance_only, traditional_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Transmit and adjudicate the interpretation that biblical sacrificial law requires material Temple performance. Set curricula for intensive study of sacrificial tractates as preparatory rather than substitutive. Bound by the chain of transmission and communal expectation to maintain the dormant commitment in its literal form.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, rabbinic_authorities, agenda_setter,
    institutional, generational, constrained, global).

% Study sacrificial law in traditional academies, understanding that their intellectual engagement does not fulfill the commandment but preserves expertise and communal memory for a future restoration. Receive meaning, identity, and social integration from the study, while accepting the deferral of actual performance.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, traditional_students, beneficiary,
    moderate, biographical, constrained, regional).

% Hold that prayer, study, or ethical refinement have transformed or replaced the sacrificial commandment in the current era. Their views are treated as category errors within the performance-only framework and are not engaged as legitimate halakhic alternatives in traditional curricula.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, symbolic_reading_adherents, excluded,
    moderate, biographical, mobile, national).

% Analyze the performance-only reading as a historical and sociological phenomenon, tracing how the rabbinic tradition managed the cognitive and institutional tension between a literal commandment and its material impossibility across two millennia of diaspora.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__performance_only, religious_studies_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains trans-generational expertise in sacrificial law and orients the community toward a shared eschatological horizon of material restoration, preventing the tradition from atrophying or transforming into unrecognizable forms during the diasporic interval.
% TRANSFER_FUNCTION: Moves curricular attention, communal memory, and normative authority from the absent Temple to the study hall, while reserving ultimate religious value for a future material performance that study alone cannot provide.
% ABSENT_VOICES: Adherents of symbolic-transformation and study-as-exercise readings, who would argue that the community already occupies the sacrificial commitment through prayer, study, or spiritualized substitution, are structurally muted within the performance-only framework â their positions are treated as category errors rather than rival interpretations.
% DISAPPEARANCE_RATIONALE: If the performance-only reading vanished, traditional study of sacrificial law would lose its preparatory character and become either antiquarian preservation or would need to be reinterpreted as direct performance; the community's eschatological orientation and curricular emphasis would shift toward alternative readings that declare the commitment already occupied or transformed.
% FOUNDING_PROBLEM: The destruction of the Second Temple removed the material conditions for fulfilling the biblical sacrificial commandments, creating a crisis of continuity: how to remain bound to commandments that require a central altar and priesthood when both are absent.
% FOUNDING_PROBLEM_CORROBORATION: Roman and Jewish historiography (Josephus, Tacitus) corroborate the Temple's destruction independently of rabbinic sources; the Tisha B'Av liturgical tradition and archaeological evidence of Temple destruction attest the founding event from both within and outside the rabbinic beneficiary set.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__performance_only, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__performance_only, 0.18, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__performance_only_tests).
:- end_tests(temple_sacrifice_commitment__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.18 because the constraint is currently dormant: no material sacrifice occurs and no party is coerced into furnishing it. The low positive value acknowledges the latent structural potential for extraction if restoration is attempted without ethical evolution. Suppression is 0.22 because alternative readings (study-as-exercise, symbolic-transformation) are not violently suppressed; they are held by other communities and are intellectually accessible, though delegitimated within the performance-only framework. Theater_ratio at 0.30 reflects the performative dimension of elaborate, multigenerational study of a practice explicitly declared to be defunct â the study is genuine coordination but carries a theatrical element of maintaining expertise for a restoration that has been deferred for two millennia. Accessibility_collapse is 0.60: once an agent accepts the performance-only reading, the alternative occupations of the commitment collapse for that agent, but globally the alternatives remain live and held by substantial communities. Resistance is 0.40 because the alternative readings constitute an active, ongoing contestation of this framing within Judaism.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic agenda-setter seat, the constraint appears as fidelity to an unchangeable divine command that happens to be currently unrealizable. From the student beneficiary seat, it appears as meaningful participation in a tradition that defers gratification across generations. From the excluded symbolic-reading seat, it appears as an unnecessarily rigid refusal to recognize that the community has already adapted the commitment into contemporary practice. The engine will compute different per-seat classifications from these structural positions: the agenda-setter and beneficiary seats should compute toward rope or mountain-like stability, while the excluded seat experiences a delegitimating constraint with higher effective extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and traditional students are declared beneficiaries: they are coordinated by the constraint and receive continuity, identity, and authority from it. Their directionality values will compute toward the beneficiary end (low d). No victims are declared because the prompt specifies no current victim set and the constraint is not currently extracting from any identifiable group. The excluded symbolic-reading adherents are not declared as victims because their exit is mobile â they can join communities that hold alternative readings â so they do not meet the structural victim criteria of trapped or identity-locked exit. Effective extraction is therefore low across all seated agents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem â the destruction of the Temple and the loss of sacrificial infrastructure â remains live (the Temple has not been rebuilt, the priesthood is not restored). This prevents piton classification: the constraint is not a dead mandate maintained by inertia alone. The genuine coordination function of study â preserving expertise, maintaining communal identity, orienting toward restoration â provides the positive-valence half that prevents snare classification despite the zero current victim set. If the founding problem were declared dead while the constraint persisted, the mismatch flag would fire; here, the live status of the founding problem is corroborated by both internal liturgical practice (Tisha B'Av) and external historiography.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    latent_extractive_potential,
    'If the material conditions for sacrifice were restored (e.g., Third Temple reconstruction), would the performance-only reading generate identifiable victims through coercive enforcement or asymmetric resource demands?',
    'Observe any actual restoration attempts and assess whether they produce excluded groups, economic coercion, or political violence; compare against the ethical-evolution threshold hypothesized in the source material.',
    'If restoration under this reading produces victims, the constraint would reclassify from rope to tangled_rope or snare in the restored context; if restoration proceeds without victims, the low-epsilon rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latent_extractive_potential, empirical, 'Whether dormant commitment becomes extractive upon material restoration').

omega_variable(
    study_occupation_boundary,
    'Does the intensive, multi-generational study of sacrificial law function as a covert form of occupation or identity-performance that the reading''s doctrinal framework denies, raising the theater_ratio beyond the authored 0.30?',
    'Ethnographic analysis of traditional student communities: measure whether the study practice produces psychological states of fulfillment, atonement, or divine connection that are structurally equivalent to the performance it claims study cannot replace.',
    'If study functions as covert occupation, the reading''s self-description is performative denial and the constraint carries higher extraction than the surface metrics suggest; if study remains genuinely preparatory, the authored metrics hold.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_occupation_boundary, conceptual, 'Whether study is covert occupation despite doctrinal denial').

omega_variable(
    framework_suppression_ambiguity,
    'Does the performance-only reading structurally suppress symbolic-transformation and study-as-exercise readings within the halakhic ecosystem, or do these readings genuinely coexist as live alternatives?',
    'Bibliometric and curriculum analysis: measure citation rates, responsa engagement, and yeshiva curriculum inclusion of alternative readings; if alternative readings are systematically excluded from normative discourse, suppression is higher than authored.',
    'If structural suppression is high, the constraint approaches tangled_rope; if genuine pluralism obtains, the rope classification is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framework_suppression_ambiguity, empirical, 'Whether alternative readings are structurally suppressed or coexist').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__performance_only, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tscp_perf_tr_t0, temple_sacrifice_commitment__performance_only, theater_ratio, 0, 0.22).
narrative_ontology:measurement(tscp_perf_tr_t10, temple_sacrifice_commitment__performance_only, theater_ratio, 10, 0.24).
narrative_ontology:measurement(tscp_perf_tr_t20, temple_sacrifice_commitment__performance_only, theater_ratio, 20, 0.25).
narrative_ontology:measurement(tscp_perf_tr_t30, temple_sacrifice_commitment__performance_only, theater_ratio, 30, 0.27).
narrative_ontology:measurement(tscp_perf_tr_t40, temple_sacrifice_commitment__performance_only, theater_ratio, 40, 0.29).
narrative_ontology:measurement(tscp_perf_tr_t50, temple_sacrifice_commitment__performance_only, theater_ratio, 50, 0.3).

% Extraction over time
narrative_ontology:measurement(tscp_perf_be_t0, temple_sacrifice_commitment__performance_only, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(tscp_perf_be_t10, temple_sacrifice_commitment__performance_only, base_extractiveness, 10, 0.15).
narrative_ontology:measurement(tscp_perf_be_t20, temple_sacrifice_commitment__performance_only, base_extractiveness, 20, 0.15).
narrative_ontology:measurement(tscp_perf_be_t30, temple_sacrifice_commitment__performance_only, base_extractiveness, 30, 0.16).
narrative_ontology:measurement(tscp_perf_be_t40, temple_sacrifice_commitment__performance_only, base_extractiveness, 40, 0.17).
narrative_ontology:measurement(tscp_perf_be_t50, temple_sacrifice_commitment__performance_only, base_extractiveness, 50, 0.18).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(temple_sacrifice_commitment__performance_only, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__performance_only, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__performance_only, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the temple_sacrifice_commitment kernel, which decomposes into structurally distinct claims per the epsilon-invariance principle. The performance-only reading maintains material instantiation as the sole valid occupation, while sibling readings authorize study, hybrid suspension, or symbolic transformation as valid alternatives. Each reading carries distinct epsilon values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
