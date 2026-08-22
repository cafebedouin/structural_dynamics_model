% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__performance_only_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
 *   constraint_id: sacrifice_obligation_kernel__performance_only_reading
 *   human_readable: Performance-Only Reading of the Sacrifice Obligation
 *   domain: religious_law/halakhic_authority/commitment_system_dynamics
 *
 * SUMMARY:
 *   This constraint instantiates the performance_only_reading of the
 *   sacrifice_obligation_kernel: the halakhic position that the biblical
 *   commandment of animal sacrifice requires physical performance on a
 *   legitimate altar and cannot be discharged through study, prayer, or
 *   symbolic substitution. For approximately 1,900 years since the
 *   destruction of the Second Temple, this reading has maintained an acute
 *   obligation that the commanded community cannot fulfill, generating a
 *   standing structural gap between norm and capacity. The reading is
 *   contested by three sibling readings that would transform, suspend, or
 *   dissolve the obligation.
 *
 * KEY AGENTS:
 *   - Halakhic authority: agenda_setter (institutional/analytical exit) â administers the performance-only codification without material benefit.
 *   - Commanded community: payer (moderate/identity_locked) â bears the unfulfillable obligation as a standing covenantal debt.
 *   - Alternative reading adherents: excluded voices â holders of study-as-exercise, messianic suspension, and symbolic-archive readings who are not seated in the authoritative forum.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, 0.82).
domain_priors:suppression_score(sacrifice_obligation_kernel__performance_only_reading, 0.72).
domain_priors:theater_ratio(sacrifice_obligation_kernel__performance_only_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__performance_only_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__performance_only_reading, piton).
narrative_ontology:human_readable(sacrifice_obligation_kernel__performance_only_reading, "Performance-Only Reading of the Sacrifice Obligation").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__performance_only_reading, "religious_law/halakhic_authority/commitment_system_dynamics").

domain_priors:requires_active_enforcement(sacrifice_obligation_kernel__performance_only_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__performance_only_reading, 'bc1d908f-335c-4a07-b9dd-98c45fe09b99').
narrative_ontology:cs_kernel_codification('bc1d908f-335c-4a07-b9dd-98c45fe09b99', fixed_text).
narrative_ontology:cs_authority_grounding('bc1d908f-335c-4a07-b9dd-98c45fe09b99', lineage).
narrative_ontology:cs_interpretation_layer_present('bc1d908f-335c-4a07-b9dd-98c45fe09b99').
narrative_ontology:cs_reading_relation('bc1d908f-335c-4a07-b9dd-98c45fe09b99', sacrifice_obligation_kernel__study_as_exercise_reading, forecloses).
narrative_ontology:cs_reading_relation('bc1d908f-335c-4a07-b9dd-98c45fe09b99', sacrifice_obligation_kernel__messianic_suspension_reading, influences).
narrative_ontology:cs_reading_relation('bc1d908f-335c-4a07-b9dd-98c45fe09b99', sacrifice_obligation_kernel__symbolic_archive_reading, coexists_with).
narrative_ontology:cs_axiom('bc1d908f-335c-4a07-b9dd-98c45fe09b99', foundational, sacrifice_requires_physical_action).
narrative_ontology:cs_axiom_status(sacrifice_requires_physical_action, holdable).
narrative_ontology:cs_axiom_grounding('bc1d908f-335c-4a07-b9dd-98c45fe09b99', sacrifice_requires_physical_action, deontological).
narrative_ontology:cs_axiom('bc1d908f-335c-4a07-b9dd-98c45fe09b99', foundational, study_lacks_dispositive_status).
narrative_ontology:cs_axiom_status(study_lacks_dispositive_status, holdable).
narrative_ontology:cs_axiom_grounding('bc1d908f-335c-4a07-b9dd-98c45fe09b99', study_lacks_dispositive_status, conventional).
narrative_ontology:cs_reference_frame('bc1d908f-335c-4a07-b9dd-98c45fe09b99', temple_cult_operational).
narrative_ontology:cs_drift_state('bc1d908f-335c-4a07-b9dd-98c45fe09b99', post_second_temple_diaspora, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('bc1d908f-335c-4a07-b9dd-98c45fe09b99', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__performance_only_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_victim(sacrifice_obligation_kernel__performance_only_reading, commanded_community).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the codification that physical sacrifice is the sole valid performance of the mitzvah and that Torah study of sacrificial law is preparatory rather than dispositive. Administers the textual corpus, trains jurists in Temple-law tractates, and rejects alternative readings that would equate study with fulfillment or suspend the obligation. Preserves interpretive continuity without materially profiting from the extraction.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, halakhic_authority, agenda_setter,
    institutional, civilizational, analytical, universal).

% Constituted by the covenantal obligation to bring sacrifices to the Temple. Has borne the command for approximately 1,900 years without physical capacity to perform it. Exit from the obligation is identity-destroying (apostasy), so the community remains structurally bound to an unfulfillable divine command, experiencing the gap between norm and capacity as a standing debt.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__performance_only_reading, commanded_community, payer,
    moderate, civilizational, identity_locked, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__performance_only_reading, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__performance_only_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves covenantal continuity between Israel and the Divine across the absence of cultic infrastructure by maintaining the sacrificial mandate as an active textual and juridical reality, ensuring operational readiness for messianic restoration.
% TRANSFER_FUNCTION: Transfers the ontological burden of an unfulfillable positive commandment onto the commanded community while the halakhic authority retains interpretive control over the obligation's conditions without discharging it.
% ABSENT_VOICES: Proponents of the study-as-exercise reading, who would seat intellectual engagement as halakhic fulfillment; messianic suspension advocates, who would declare the obligation divinely inoperative; and symbolic-archive scholars, who would dissolve the halakhic claim into cultural memory. All are structurally excluded from the authoritative interpretive forum.
% DISAPPEARANCE_RATIONALE: If the performance-only obligation vanished overnight, the Jewish community would reorganize its relationship to Temple law: study would shift from preparatory rehearsal to potential fulfillment, the liturgical cycle would lose its orienting lack, and rabbinic authority would cede its unique juridical role as guardian of impossible practice.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE eliminated the sole legitimate locus for sacrificial worship, confronting rabbinic Judaism with the task of preserving covenantal structure in the absence of cultic infrastructure.
% FOUNDING_PROBLEM_CORROBORATION: Academic historians of Second Temple Judaism and sociologists of religion attest that the rabbinic movement successfully reorganized Jewish practice around text, prayer, and halakhic norm; they do not corroborate that a performance-only stance remains a live necessity, instead describing it as institutional preservation of juridical authority. No external corroboration from outside the halakhic authority confirms the ongoing functional requirement of this specific reading.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__performance_only_reading, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__performance_only_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__performance_only_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__performance_only_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__performance_only_reading, 0.82, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sacrifice_obligation_kernel__performance_only_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sacrifice_obligation_kernel__performance_only_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness is high (0.82) because the obligation remains acute and unfulfilled after nearly two millennia, constituting a persistent burden on the commanded community. Theater ratio is very high (0.85) because the constraint's primary function (sacrifice) has atrophied and what remains is the performance of study, textual rehearsal, and liturgical memorial â maintenance without operation. Suppression (0.72) reflects the active institutional rejection of alternative readings (study-as-fulfillment, symbolic archive) that would bridge the gap. Accessibility collapse is high (0.80): once the performance-only frame is accepted, alternatives collapse for the adherent because study is explicitly coded as insufficient. Resistance is moderate-low (0.35) because resistance is diffuse and primarily arises outside the Orthodox interpretive world. The measurement series share one time grid (0â190) to prevent misalignment artifacts.
 *
 * PERSPECTIVAL GAP:
 *   The halakhic authority experiences the constraint as authentic preservation of divine law and juridical continuity; the commanded community experiences it as a structural debt they are forbidden to discharge. The engine computes this divergence from the asymmetry between agenda_setter (analytical exit, no material target status) and payer (identity_locked exit, declared victim).
 *
 * DIRECTIONALITY LOGIC:
 *   The commanded_community is the sole declared victim with identity_locked exit, driving directionality toward the full-target end (d â 1.0). The halakhic_authority is agenda_setter but not beneficiary; because no agent is declared beneficiary, the structural derivation produces no concentrated subsidy seat, leaving the authority's d to revert toward its power atom's canonical fallback. The absence of a beneficiary is the structural signature that differentiates this piton from a snare.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is classified as piton rather than snare because no identifiable agent captures the extraction: the halakhic authority maintains the reading for reasons of interpretive continuity, not material rent. It is not a mountain because the constraint is a constructed halakhic interpretation (emerges_naturally: false), not an irreducible natural law. It is not a rope because the community is a net victim, not a beneficiary. The classification prevents mislabeling the 1,900-year persistence of an impossible command as either benign natural fact (mountain) or agentic extraction (snare); instead, it captures the inertial and theatrical character of a degraded function maintained by institutional performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Does the performance-only reading represent an unalterable divine command, or a halakhic interpretation that could be revised by rabbinic authority?',
    'Comparative analysis of responsa literature: if later rabbinic authorities have discretion to declare study sufficient under emergency conditions (sha''at had''chak), the constraint is interpretive; if all such proposals are rejected as ultra vires, the constraint functions as a fixed kernel.',
    'If interpretive, classification shifts toward tangled_rope or snare (active enforcement by interpretive authority); if fixed, classification shifts toward mountain (unalterable structural limit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether the performance-only requirement is constructed or fixed').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (rabbinic institutional control of halakhic discourse) or internalized (the commanded community''s identity fusion with the unfulfillable obligation)?',
    'Post-exit suppression trajectory: if individuals who adopt alternative readings (study-as-exercise, symbolic archive) experience social ostracism or institutional exclusion, suppression is structural; if they experience internal guilt or identity crisis independent of social reaction, suppression is internalized.',
    'If internalized, effective suppression exceeds structural measure â the community carries the constraint even if institutional enforcement vanished. If structural, the constraint''s persistence depends on ongoing rabbinic gatekeeping.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs internalized suppression mechanism').

omega_variable(
    diffuse_beneficiary_ambiguity,
    'Does the halakhic authority that maintains the performance-only reading accrue diffuse benefits (legitimacy, institutional reproduction, communal deference) even without material rent extraction?',
    'Sociological analysis of rabbinic authority''s dependency on Temple-law expertise: if removing the performance-only obligation would diminish the authority''s unique juridical role, the authority is a diffuse beneficiary.',
    'If diffuse benefits are structurally significant, the constraint is a snare (extractive with beneficiary) rather than a piton (inertial, no beneficiary).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(diffuse_beneficiary_ambiguity, conceptual, 'Whether interpretive authority constitutes a diffuse beneficiary').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__performance_only_reading, 0, 190).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacrifice_perf_tr_t0, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(sacrifice_perf_tr_t38, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 38, 0.35).
narrative_ontology:measurement(sacrifice_perf_tr_t76, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 76, 0.55).
narrative_ontology:measurement(sacrifice_perf_tr_t114, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 114, 0.68).
narrative_ontology:measurement(sacrifice_perf_tr_t152, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 152, 0.78).
narrative_ontology:measurement(sacrifice_perf_tr_t190, sacrifice_obligation_kernel__performance_only_reading, theater_ratio, 190, 0.85).

% Extraction over time
narrative_ontology:measurement(sacrifice_perf_be_t0, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(sacrifice_perf_be_t38, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 38, 0.65).
narrative_ontology:measurement(sacrifice_perf_be_t76, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 76, 0.7).
narrative_ontology:measurement(sacrifice_perf_be_t114, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 114, 0.74).
narrative_ontology:measurement(sacrifice_perf_be_t152, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 152, 0.78).
narrative_ontology:measurement(sacrifice_perf_be_t190, sacrifice_obligation_kernel__performance_only_reading, base_extractiveness, 190, 0.82).

% Suppression requirement over time
narrative_ontology:measurement(sacrifice_perf_su_t0, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(sacrifice_perf_su_t38, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 38, 0.5).
narrative_ontology:measurement(sacrifice_perf_su_t76, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 76, 0.6).
narrative_ontology:measurement(sacrifice_perf_su_t114, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 114, 0.65).
narrative_ontology:measurement(sacrifice_perf_su_t152, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 152, 0.68).
narrative_ontology:measurement(sacrifice_perf_su_t190, sacrifice_obligation_kernel__performance_only_reading, suppression_requirement, 190, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_kernel__performance_only_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% The sacrifice_obligation_kernel decomposes into four structurally distinct readings. Each reading instantiates a different constraint with distinct epsilon, beneficiary/victim structure, and classification. They form a constraint family linked through cs_structure.reading_relations rather than causal influence edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
