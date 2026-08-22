% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__study_as_exercise, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: temple_sacrifice_commitment__study_as_exercise
 *   human_readable: Study of Sacrifice Law as Intrinsic Performance of Divine Command
 *   domain: religious_law/halakhic_tradition/commitment_system_theory
 *
 * SUMMARY:
 *   This constraint instantiates the study_as_exercise reading of the
 *   temple_sacrifice_commitment kernel. The reading holds that intellectual
 *   engagement with sacrifice law — learning its rules, debating its
 *   parameters, maintaining its conceptual architecture — is itself the
 *   performance of the divine command when material conditions (the Temple,
 *   priesthood, sacrificial animals) are absent. The studying community
 *   (rabbinic academies, yeshivas, individual scholars) maintains covenant
 *   fidelity through study alone. No victim set exists because the
 *   arrangement is internally sustained: participants voluntarily enter the
 *   study framework, resource it from within the community, and experience it
 *   as intrinsically valuable. The constraint is claimed as rope: a genuine
 *   coordination mechanism (maintaining communal identity and covenant
 *   continuity) with negligible extraction and no suppression of
 *   alternatives.
 *
 * KEY AGENTS:
 *   - studying_community: Primary beneficiary (organized/biographical/arbitrage/global) — maintains covenant fidelity through voluntary intellectual engagement
 *   - rabbinic_authorities: Agenda setter (institutional/generational/arbitrage/global) — curates the study corpus, adjudicates interpretive boundaries, authorizes the reading
 *   - external_observers: Observer (analytical/civilizational/analytical/universal) — academic scholars of religion, comparative law, commitment system theory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__study_as_exercise, 0.02).
domain_priors:suppression_score(temple_sacrifice_commitment__study_as_exercise, 0.05).
domain_priors:theater_ratio(temple_sacrifice_commitment__study_as_exercise, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, extractiveness, 0.02).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__study_as_exercise, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__study_as_exercise, rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__study_as_exercise, "Study of Sacrifice Law as Intrinsic Performance of Divine Command").
narrative_ontology:topic_domain(temple_sacrifice_commitment__study_as_exercise, "religious_law/halakhic_tradition/commitment_system_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__study_as_exercise, 'b8f81a65-172d-48e5-836a-721cd77c150f').
narrative_ontology:cs_kernel_codification('b8f81a65-172d-48e5-836a-721cd77c150f', fixed_text).
narrative_ontology:cs_authority_grounding('b8f81a65-172d-48e5-836a-721cd77c150f', lineage).
narrative_ontology:cs_interpretation_layer_present('b8f81a65-172d-48e5-836a-721cd77c150f').
narrative_ontology:cs_reading_relation('b8f81a65-172d-48e5-836a-721cd77c150f', temple_sacrifice_commitment__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('b8f81a65-172d-48e5-836a-721cd77c150f', temple_sacrifice_commitment__hybrid_preparatory, coexists_with).
narrative_ontology:cs_reading_relation('b8f81a65-172d-48e5-836a-721cd77c150f', temple_sacrifice_commitment__symbolic_transformation, coexists_with).
narrative_ontology:cs_axiom('b8f81a65-172d-48e5-836a-721cd77c150f', foundational, intellectual_engagement_is_full_performance).
narrative_ontology:cs_axiom_status(intellectual_engagement_is_full_performance, holdable).
narrative_ontology:cs_axiom_grounding('b8f81a65-172d-48e5-836a-721cd77c150f', intellectual_engagement_is_full_performance, deontological).
narrative_ontology:cs_axiom('b8f81a65-172d-48e5-836a-721cd77c150f', foundational, covenant_fidelity_requires_no_material_conditions).
narrative_ontology:cs_axiom_status(covenant_fidelity_requires_no_material_conditions, holdable).
narrative_ontology:cs_axiom_grounding('b8f81a65-172d-48e5-836a-721cd77c150f', covenant_fidelity_requires_no_material_conditions, deontological).
narrative_ontology:cs_reference_frame('b8f81a65-172d-48e5-836a-721cd77c150f', sinai_covenant_with_study_clause).
narrative_ontology:cs_drift_state('b8f81a65-172d-48e5-836a-721cd77c150f', post_temple_destruction, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('b8f81a65-172d-48e5-836a-721cd77c150f', '2026-08-03T14:30:00Z').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, studying_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__study_as_exercise, rabbinic_authorities).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, covenant_fidelity_through_study).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__study_as_exercise, divine_command_occupiable_intellectually).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Voluntarily engages in study of sacrifice law as intrinsic religious practice. Invests time, attention, and communal resources (yeshiva funding, family support for scholars) to maintain the commitment. Experiences the study as intrinsically valuable — covenant fidelity, intellectual coherence, communal identity. Exit is always available: one can stop studying, leave the community, or adopt a different reading without structural penalty. The community resources itself through internal donation, tuition, and endowment — no external population is taxed.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, studying_community, beneficiary,
    organized, biographical, arbitrage, global).

% Curate the study corpus (Mishnah, Talmud, codes, responsa), adjudicate interpretive boundaries, authorize the study_as_exercise reading as legitimate. Their authority derives from the studying community's voluntary acceptance — they do not compel participation. They benefit from the arrangement (status, institutional position, intellectual leadership) but do not extract from the community; the community's support is voluntary. Exit is available: authorities who lose communal trust lose authority.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, rabbinic_authorities, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__study_as_exercise, rabbinic_authorities, beneficiary).

% Academic scholars of religion, comparative law, commitment system theory. They analyze the constraint from outside the commitment, mapping its structural dynamics without participating in the covenant. They neither benefit nor pay; they observe. Their exit is trivial — they engage analytically by choice.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__study_as_exercise, external_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__study_as_exercise, diffuse).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__study_as_exercise, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains covenant fidelity and communal identity across the rupture of Temple destruction by providing a self-sustaining intellectual practice that occupies the divine command without material conditions.
% TRANSFER_FUNCTION: Moves nothing between parties. The studying community voluntarily directs its own attention and resources toward study; the returns (covenant fidelity, identity, coherence) accrue to the same community. No transfer from payer to beneficiary.
% ABSENT_VOICES: Those who hold the performance_only reading (material instantiation required) are not excluded — they are present in the discourse as a live alternative. The symbolic_transformation reading (prayer as authorized replacement) is also a live position. No voice is structurally excluded; the constraint persists through persuasion, not suppression.
% DISAPPEARANCE_RATIONALE: If the study_as_exercise constraint vanished overnight, the studying community would lose its primary mode of covenant occupation. The community would either adopt a different reading (performance_only, hybrid_preparatory, symbolic_transformation) or the commitment itself would attenuate. The world rearranges because the arrangement is actively maintaining a commitment structure that would otherwise collapse.
% FOUNDING_PROBLEM: After the Temple's destruction (70 CE), the divine command to offer sacrifices could not be materially performed. The community needed a way to maintain covenant fidelity without the material conditions the command presupposed.
% FOUNDING_PROBLEM_CORROBORATION: The studying community (beneficiaries) attests the problem is live: material conditions remain absent, study remains the occupation. Corroboration from outside the beneficiary set: academic historians of Judaism (e.g., Jacob Neusner, Moshe Halbertal) document that rabbinic study emerged as the primary covenant-maintenance mechanism post-70 CE and remains so; comparative religion scholars note the pattern of intellectual substitution for material cult across traditions. No non-beneficiary source claims the founding problem is dead.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__study_as_exercise, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(temple_sacrifice_commitment__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__study_as_exercise, 0.02, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__study_as_exercise_tests).
:- end_tests(temple_sacrifice_commitment__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.02) because the arrangement is internally resourced and voluntary — no population bears costs for the studying community's benefit. Suppression is minimal (0.05) because alternative readings (performance_only, hybrid_preparatory, symbolic_transformation) remain live and uncontested by force; the constraint persists through intellectual persuasion and communal cohesion, not enforcement. Theater ratio is low (0.10) because the study practice is the real function, not a performance masking extraction. Accessibility collapse is low (0.15) because the commitment can be engaged at many levels and exit is always available. Resistance is near-zero (0.08) because the constraint does not compel — it invites.
 *
 * PERSPECTIVAL GAP:
 *   From the studying community's seat, this is a mountain-like coordination mechanism — the commitment occupies itself through them. From the performance_only reading's seat, this constraint is a piton: a degraded substitute maintaining the appearance of commitment while the real practice is impossible. From the hybrid_preparatory seat, this is a scaffold: preparatory exercise awaiting messianic restoration. From symbolic_transformation, this constraint is a transitional form already superseded. The engine computes these divergences from the structural data; the authored claim (rope) represents this reading's self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   The studying community is the sole structural beneficiary: they voluntarily invest time and attention to maintain the commitment, and the returns (covenant fidelity, communal identity, intellectual coherence) accrue entirely to them. No victim set exists — the constraint extracts from no one. Rabbinic authorities serve as agenda_setter: they curate the corpus and adjudicate boundaries, but they do not extract from the community; their authority derives from the community's voluntary acceptance of their interpretive role. The directionality derivation yields d ≈ 0.15 for the studying community (beneficiary), d ≈ 0.25 for rabbinic authorities (agenda_setter with slight institutional capture risk), and d ≈ 0.0 for observers.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (covenant fidelity without Temple) remains live per this reading's self-understanding. Mandatrophy is resolved in the sense that the arrangement's function (intellectual occupation of the commitment) matches its declared purpose — no drift into extraction or performance. The constraint does not persist by inertia; it persists because the studying community continues to find it intrinsically valuable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_study_as_exercise,
    'Is the study-as-exercise reading a distinct constraint from the other readings of the temple_sacrifice_commitment kernel?',
    'Compare ε, beneficiary/victim structure, and classification across all four declared readings. If ε and structural profiles differ, they are separate constraints per ε-invariance.',
    'If distinct, each reading gets its own constraint story with its own ε. If identical, the kernel collapses to one constraint and the reading distinctions are perspectival.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_study_as_exercise, conceptual, 'Whether the four kernel readings instantiate structurally distinct constraints or are observer-frames on one constraint.').

omega_variable(
    study_extractiveness_boundary,
    'Does intellectual engagement with sacrifice law extract from any party, or is it purely self-sustaining commitment exercise?',
    'Trace resource flows: time, attention, institutional support for academies. If study requires material subsidy extracted from a population, extractiveness > 0. If entirely internally resourced by the studying community, ε ≈ 0.',
    'Non-zero extraction would reclassify from rope toward tangled_rope or snare, and create a victim set.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_extractiveness_boundary, empirical, 'Whether study-as-performance has hidden extractive structure (subsidized academies, compelled attendance, etc.).').

omega_variable(
    commitment_occupation_sufficiency,
    'Is intellectual engagement sufficient to ''occupy the commitment'' in the absence of material conditions, or does the commitment require material instantiation?',
    'Examine the internal logic of this reading: does the divine command''s structure admit intellectual performance as full satisfaction, or does the command''s semantics require material action?',
    'If material instantiation is structurally required, this reading''s claim of ''zero extractiveness / full occupation'' is internally inconsistent — the constraint would be a scaffold (preparatory) or piton (atrophied).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commitment_occupation_sufficiency, conceptual, 'Whether the study_as_exercise reading''s core premise is coherent on its own terms.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__study_as_exercise, 0, 2500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tscsae_tr_t0, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tscsae_tr_t500, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 500, 0.07).
narrative_ontology:measurement(tscsae_tr_t1000, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1000, 0.08).
narrative_ontology:measurement(tscsae_tr_t1500, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 1500, 0.09).
narrative_ontology:measurement(tscsae_tr_t2000, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(tscsae_tr_t2500, temple_sacrifice_commitment__study_as_exercise, theater_ratio, 2500, 0.1).

% Extraction over time
narrative_ontology:measurement(tscsae_be_t0, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 0, 0.01).
narrative_ontology:measurement(tscsae_be_t500, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 500, 0.01).
narrative_ontology:measurement(tscsae_be_t1000, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1000, 0.02).
narrative_ontology:measurement(tscsae_be_t1500, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 1500, 0.02).
narrative_ontology:measurement(tscsae_be_t2000, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 2000, 0.02).
narrative_ontology:measurement(tscsae_be_t2500, temple_sacrifice_commitment__study_as_exercise, base_extractiveness, 2500, 0.02).

% Suppression requirement over time
narrative_ontology:measurement(tscsae_su_t0, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 0, 0.03).
narrative_ontology:measurement(tscsae_su_t500, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 500, 0.04).
narrative_ontology:measurement(tscsae_su_t1000, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 1000, 0.05).
narrative_ontology:measurement(tscsae_su_t1500, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(tscsae_su_t2000, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 2000, 0.05).
narrative_ontology:measurement(tscsae_su_t2500, temple_sacrifice_commitment__study_as_exercise, suppression_requirement, 2500, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__study_as_exercise, identity_coordination).
narrative_ontology:boltzmann_floor_override(temple_sacrifice_commitment__study_as_exercise, 0.08).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__hybrid_preparatory).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__study_as_exercise, temple_sacrifice_commitment__symbolic_transformation).

% DUAL FORMULATION NOTE:
% Four readings of the temple_sacrifice_commitment kernel, each a separate constraint story with distinct ε and structural profiles. This reading (study_as_exercise) claims ε ≈ 0 and rope classification. The performance_only reading likely has higher ε (maintaining impossible material practice) and snare/tangled_rope profile. The hybrid_preparatory reading likely has scaffold profile with sunset clause (messianic restoration). The symbolic_transformation reading likely has rope/tangled_rope profile with different beneficiary structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(temple_sacrifice_commitment__study_as_exercise, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
