% ============================================================================
% CONSTRAINT STORY: sacrifice_commandment__archive_maintenance
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_commandment__archive_maintenance, []).

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
 *   constraint_id: sacrifice_commandment__archive_maintenance
 *   human_readable: Sacrificial Law Study as Archival Preparation for Temple Restoration
 *   domain: religious_studies/halakhic_theory
 *
 * SUMMARY:
 *   This story instantiates the archive_maintenance reading of the
 *   sacrifice_commandment kernel: study of sacrificial law is justified as
 *   technical preservation for a future restored Temple, explicitly NOT as
 *   present-tense fulfillment of the commandment and NOT as a claim that the
 *   commandment is currently suspended and inert. This is a deliberately
 *   moderate reading — it neither grants students present spiritual credit
 *   (as study_as_performance does) nor concedes the practice is simply
 *   dormant with no present obligation (as performance_only does). The ε here
 *   (0.42) reflects genuine coordination function (a real
 *   archival/transmission problem exists) combined with real present-day cost
 *   extracted from students whose labor benefits a hypothetical future
 *   beneficiary who cannot corroborate the arrangement's terms.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_commandment__archive_maintenance, 0.42).
domain_priors:suppression_score(sacrifice_commandment__archive_maintenance, 0.38).
domain_priors:theater_ratio(sacrifice_commandment__archive_maintenance, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, extractiveness, 0.42).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, tangled_rope).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Sacrificial Law Study as Archival Preparation for Temple Restoration").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious_studies/halakhic_theory").

domain_priors:requires_active_enforcement(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, '3f2da3b2-c5d5-4637-b5b3-cc2a04e6372c').
narrative_ontology:cs_kernel_codification('3f2da3b2-c5d5-4637-b5b3-cc2a04e6372c', fixed_text).
narrative_ontology:cs_authority_grounding('3f2da3b2-c5d5-4637-b5b3-cc2a04e6372c', lineage).
narrative_ontology:cs_interpretation_layer_present('3f2da3b2-c5d5-4637-b5b3-cc2a04e6372c').
narrative_ontology:cs_reading_relation('3f2da3b2-c5d5-4637-b5b3-cc2a04e6372c', sacrifice_commandment__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('3f2da3b2-c5d5-4637-b5b3-cc2a04e6372c', sacrifice_commandment__performance_only, influences).
narrative_ontology:cs_axiom('3f2da3b2-c5d5-4637-b5b3-cc2a04e6372c', foundational, study_is_preparatory_not_constitutive).
narrative_ontology:cs_axiom_status(study_is_preparatory_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('3f2da3b2-c5d5-4637-b5b3-cc2a04e6372c', study_is_preparatory_not_constitutive, conventional).
narrative_ontology:cs_axiom('3f2da3b2-c5d5-4637-b5b3-cc2a04e6372c', foundational, commandment_remains_technically_live_pending_restoration).
narrative_ontology:cs_axiom_status(commandment_remains_technically_live_pending_restoration, holdable).
narrative_ontology:cs_axiom_grounding('3f2da3b2-c5d5-4637-b5b3-cc2a04e6372c', commandment_remains_technically_live_pending_restoration, theological).
narrative_ontology:cs_reference_frame('3f2da3b2-c5d5-4637-b5b3-cc2a04e6372c', second_temple_active_sacrificial_service).
narrative_ontology:cs_drift_state('3f2da3b2-c5d5-4637-b5b3-cc2a04e6372c', post_destruction_rabbinic_reconstitution, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('3f2da3b2-c5d5-4637-b5b3-cc2a04e6372c', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_restored_priesthood).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, yeshiva_institutions_teaching_kodashim).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, present_day_students_of_kodashim).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, messianic_restoration_doctrine).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, temple_service_continuity_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets curricula requiring intensive study of sacrificial law (Kodashim, Temple architecture, priestly purity codes) and justifies allocating years of student time to it on the grounds that this technical knowledge must be preserved intact for a future rebuilt Temple. Draws prestige, funding, and continuity narrative from being the custodian of a body of practical law that no living person has ever performed. Institutional standing does not depend on the Temple ever actually being rebuilt in any student's lifetime.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, yeshiva_institutions_teaching_kodashim, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__archive_maintenance, yeshiva_institutions_teaching_kodashim, beneficiary).

% Spend substantial portions of a yeshiva education memorizing sacrificial procedures, altar dimensions, and priestly disqualifications that have no present ritual application and will most likely have none within their lifetimes. Are told this labor is 'as if' performing the commandment, but the reading under study here explicitly denies that equivalence — study is preparation, not fulfillment — which means the present-tense spiritual credit sometimes implied to students is not actually available under this reading's own terms. Exit is constrained by identity: leaving the study track can read as abandoning tradition rather than as a reasonable assessment of present utility.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, present_day_students_of_kodashim, payer,
    powerless, biographical, identity_locked, local).

% A hypothetical future cohort of Kohanim who would, upon Temple restoration, need exactly this technical knowledge to resume sacrificial service without discontinuity. They do not yet exist and cannot corroborate, object, or confirm that the knowledge preserved on their behalf is accurate, sufficient, or even the version they would need. All present cost is justified by their hypothetical future need.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, future_restored_priesthood, beneficiary,
    analytical, civilizational, analytical, national).

% Rabbinic authorities who articulate and defend the archive-maintenance framing specifically to distinguish it from claims that study itself fulfills the commandment (avoiding the theological risk of claiming present divine service without an altar) and from claims that the commandment is simply suspended (avoiding the practical risk of the knowledge base atrophying). They administer which framing is taught and enforce it against competing readings within their institutions.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, messianic_restorationist_authorities, agenda_setter,
    organized, civilizational, arbitrage, global).

% Movements that have largely dropped sacrificial restoration from liturgy and curriculum entirely, treating the whole archive-maintenance project as preserving a practice they do not wish restored. Their view — that the technical knowledge need not be preserved because the restoration itself is neither expected nor desired — is not represented in the yeshiva curricular conversation and would, if voiced there, undercut the entire justification for the study burden.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, reform_and_reconstructionist_movements, excluded,
    organized, generational, mobile, global).

% Study the archive-maintenance framing as a structural device: a way to keep a large body of practical law alive as a live curricular and institutional commitment across centuries without ever needing the triggering event (Temple restoration) to occur, and without conceding the commandment is simply dormant.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, comparative_religion_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__archive_maintenance, yeshiva_institutions_teaching_kodashim).
narrative_ontology:fixing_cost_class(sacrifice_commandment__archive_maintenance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves a large, technically precise body of practical law (altar measurements, sacrificial procedures, priestly disqualifications) across generations so that, should Temple restoration occur, the knowledge needed to resume service is not lost. This is a genuine transmission/archival problem: without active study, the technical details would degrade or be lost the way any unpracticed specialized knowledge degrades.
% TRANSFER_FUNCTION: Moves years of study time and cognitive labor from present-day students toward the maintenance of a knowledge base whose beneficiary is a hypothetical future priesthood; moves institutional prestige, funding, and continuity-narrative capital to the yeshiva institutions and rabbinic authorities who administer and enforce the study requirement.
% ABSENT_VOICES: Reform and Reconstructionist voices who reject the restoration premise entirely are not part of the curricular conversation in the institutions that mandate this study; their absence means the underlying question — should this archive be maintained at all — is never actually put to a vote among those bearing its cost.
% DISAPPEARANCE_RATIONALE: If the archive-maintenance framing vanished, the yeshiva world would either shift wholesale to the study-as-performance reading (restoring present-tense spiritual value to the same study hours) or reduce Kodashim study sharply as curricular time reallocates to commandments with live application — institutions dispute which would happen, which is itself evidence the framing is doing real structural work rather than being a neutral description.
% FOUNDING_PROBLEM: After the Temple's destruction, sacrificial law risked becoming inert or forgotten precisely because it could no longer be practiced; rabbinic authorities needed a way to keep the technical corpus alive across a restoration horizon of unknown length without conceding either that the commandment was dead or that unpracticed study alone satisfied it.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic sources (Megillah 3b, Menachot 110a) and later halakhic authorities within the tradition attest the archival framing as their own stated rationale. Outside corroboration is thinner: comparative religion scholars and historians of Second Temple Judaism note the practical function of keeping technical knowledge transmissible, but secular historians studying institutional persistence describe the same arrangement as serving present institutional continuity (curricular identity, funding, communal cohesion) independent of restoration ever occurring — a reading the benefiting institutions do not themselves advance.
narrative_ontology:disappearance_verdict(sacrifice_commandment__archive_maintenance, contested).
narrative_ontology:founding_problem_status(sacrifice_commandment__archive_maintenance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_commandment__archive_maintenance, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_commandment__archive_maintenance, 0.42, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_commandment__archive_maintenance_tests).
:- end_tests(sacrifice_commandment__archive_maintenance_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness sits at moderate level (0.42) because the coordination function is real — unpracticed technical knowledge does degrade, and someone bears the cost of keeping it alive — but the beneficiary (future restored priesthood) is hypothetical and cannot confirm the knowledge transmitted is accurate, complete, or even the version eventually needed, while the payer (present students) is concrete and bears real opportunity cost now. Suppression (0.38) is moderate: institutional and identity pressure keeps students in the study track, but it is social/identity pressure rather than coercive enforcement. Theater ratio (0.3) reflects that much of the study is genuinely substantive (real transmission of real technical content) with a growing performative layer (curricular prestige signaling) as the restoration horizon recedes without event.
 *
 * DIRECTIONALITY LOGIC:
 *   Present-day students are the clearest target (d near full-target): they pay in study-years, are identity-locked into the track, and receive no present-tense credit under this specific reading. Yeshiva institutions and messianic restorationist authorities are the structural beneficiaries: they administer the curriculum, draw institutional continuity narrative and funding from custodianship of the archive, and bear none of the opportunity cost. The future restored priesthood is coded as beneficiary but sits at 'analytical' exit and power because it is a hypothetical, non-corroborating party — its benefit is asserted, never confirmed.
 *
 * MANDATROPHY ANALYSIS:
 *   The archive-maintenance framing prevents two mislabelings simultaneously: it blocks classifying the whole enterprise as pure extraction with no coordination function (there IS a real transmission problem being solved), and it blocks classifying it as costless pure coordination (present students really do bear opportunity cost for a benefit that may never materialize in their lifetime, or ever). Tangled Rope captures this: genuine coordination function (technical knowledge preservation) plus asymmetric extraction (present students pay, hypothetical future priesthood and present institutions benefit) held together by active curricular enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archive_vs_performance_boundary_contested,
    'Is the archive-maintenance reading structurally distinct from study-as-performance, or is it a diplomatic hedge that lets institutions claim both the archival justification AND informally imply present spiritual credit to sustain student motivation?',
    'Examine whether curricular messaging to students consistently denies present-tense fulfillment credit, or whether teachers informally invoke ''as if performing the service'' language that belongs to the study_as_performance reading while officially defending the archive-maintenance framing to skeptics.',
    'If institutions blend the readings inconsistently depending on audience, the archive_maintenance framing functions partly as a defensive posture rather than a stable theological position, which would raise the effective extraction (students are promised more than the official reading delivers).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_vs_performance_boundary_contested, conceptual, 'Whether archive_maintenance is a stable distinct reading or a rhetorical hedge blending with study_as_performance depending on context.').

omega_variable(
    future_beneficiary_corroboration_impossibility,
    'Can a hypothetical future beneficiary (the restored priesthood) ever meaningfully corroborate that the preserved knowledge served their actual need, or is this an unfalsifiable justification structure by design?',
    'There is no empirical resolution available before restoration occurs, if it occurs; the closest available proxy is examining whether the preserved corpus has in fact degraded, been contested, or diverged across transmission lineages in ways that would already undermine its stated future utility.',
    'If the corpus shows significant internal disagreement or degradation despite centuries of maintenance, the archival function is not actually being achieved even on its own terms, which would shift this reading''s effective extraction upward — cost is being paid for a preservation outcome not reliably occurring.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(future_beneficiary_corroboration_impossibility, empirical, 'Whether the study regime actually achieves reliable technical preservation for an unfalsifiable future beneficiary.').

omega_variable(
    restoration_probability_and_time_horizon,
    'Does the subjective probability assigned to Temple restoration (within a generation vs. never) materially change how present students and institutions should weigh the present cost against the hypothetical future benefit?',
    'Survey variation across communities with differing messianic-imminence beliefs (e.g., religious-Zionist vs. non-Zionist Haredi communities) to see whether curricular time allocation to Kodashim correlates with stated restoration-probability beliefs.',
    'If curricular commitment is insensitive to stated restoration-probability beliefs, that supports reading the archive-maintenance justification as serving present institutional functions (continuity, identity, prestige) rather than being genuinely calibrated to expected future utility, raising confidence that this is a tangled_rope rather than a pure rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(restoration_probability_and_time_horizon, empirical, 'Whether the study burden tracks actual restoration-probability beliefs or is insensitive to them, suggesting present institutional function rather than genuine future-utility calibration.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.18).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_commandment__archive_maintenance, theater_ratio, 20, 0.21).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_commandment__archive_maintenance, theater_ratio, 40, 0.24).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_commandment__archive_maintenance, theater_ratio, 60, 0.26).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_commandment__archive_maintenance, theater_ratio, 80, 0.28).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_commandment__archive_maintenance, theater_ratio, 100, 0.3).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(sacr_be_t20, sacrifice_commandment__archive_maintenance, base_extractiveness, 20, 0.34).
narrative_ontology:measurement(sacr_be_t40, sacrifice_commandment__archive_maintenance, base_extractiveness, 40, 0.37).
narrative_ontology:measurement(sacr_be_t60, sacrifice_commandment__archive_maintenance, base_extractiveness, 60, 0.4).
narrative_ontology:measurement(sacr_be_t80, sacrifice_commandment__archive_maintenance, base_extractiveness, 80, 0.41).
narrative_ontology:measurement(sacr_be_t100, sacrifice_commandment__archive_maintenance, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(sacr_su_t0, sacrifice_commandment__archive_maintenance, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(sacr_su_t20, sacrifice_commandment__archive_maintenance, suppression_requirement, 20, 0.34).
narrative_ontology:measurement(sacr_su_t40, sacrifice_commandment__archive_maintenance, suppression_requirement, 40, 0.35).
narrative_ontology:measurement(sacr_su_t60, sacrifice_commandment__archive_maintenance, suppression_requirement, 60, 0.36).
narrative_ontology:measurement(sacr_su_t80, sacrifice_commandment__archive_maintenance, suppression_requirement, 80, 0.37).
narrative_ontology:measurement(sacr_su_t100, sacrifice_commandment__archive_maintenance, suppression_requirement, 100, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__archive_maintenance, 0.1).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__performance_only).

% DUAL FORMULATION NOTE:
% Three constraints decompose the single natural-language label 'the sacrifice commandment in exile': archive_maintenance (this story — moderate extraction, present cost for hypothetical future benefit), study_as_performance (present-tense fulfillment claim — expected lower extraction, direct value delivery), and performance_only (commandment suspended pending Temple — expected lowest extraction, no compelled present study). Each is ε-invariant on its own terms; the apparent single 'BGS-style' conflation in ordinary religious discourse ('learning about sacrifices fulfills/preserves/awaits the commandment') is exactly the kind of colloquial label the framework requires decomposing into structurally distinct claims rather than parameterizing by observer.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
