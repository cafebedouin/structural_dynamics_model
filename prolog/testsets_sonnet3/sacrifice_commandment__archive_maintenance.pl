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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
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
 *   human_readable: Sacrificial Law Study as Archival Maintenance for Future Temple Restoration
 *   domain: religious/halakhic
 *
 * SUMMARY:
 *   This constraint instantiates the 'archive_maintenance' reading of the
 *   sacrifice-commandment kernel: study of Kodashim (Temple sacrificial law)
 *   is justified not as present ritual performance and not as itself
 *   constituting fulfillment of the commandment, but as technical
 *   preservation work — maintaining operational knowledge against the
 *   contingency of a future Temple restoration. This reading occupies a
 *   middle position between the performance_only reading (which holds the
 *   commandment simply suspended, unfulfilled, pending restoration) and
 *   study_as_performance (which holds that the intellectual engagement itself
 *   IS the fulfillment, collapsing the distinction between preparation and
 *   performance). The archive-maintenance reading is temporally deferential:
 *   it claims no present devotional value for the study beyond its future
 *   utility, which is precisely what gives it a scaffold-like structure — a
 *   sunset condition (Temple restoration) after which the justification
 *   either resolves into direct practice or dissolves.
 *
 * KEY AGENTS:
 *   - yeshiva_institutions_teaching_kodashim: agenda-setting beneficiary — administers curriculum and gains institutional continuity
 *   - present_generation_students_of_kodashim: payer — bears the opportunity cost of years spent on non-present-actionable material
 *   - future_restored_temple_priesthood: hypothetical beneficiary — cannot corroborate, may never materialize
 *   - halakhic_authorities_overseeing_curriculum: agenda-setter/observer — adjudicates curricular weight
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
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(sacrifice_commandment__archive_maintenance, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_commandment__archive_maintenance, scaffold).
narrative_ontology:human_readable(sacrifice_commandment__archive_maintenance, "Sacrificial Law Study as Archival Maintenance for Future Temple Restoration").
narrative_ontology:topic_domain(sacrifice_commandment__archive_maintenance, "religious/halakhic").

domain_priors:requires_active_enforcement(sacrifice_commandment__archive_maintenance).
narrative_ontology:has_sunset_clause(sacrifice_commandment__archive_maintenance).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_commandment__archive_maintenance, '84f35645-982e-4d29-8235-3ac9fe107256').
narrative_ontology:cs_kernel_codification('84f35645-982e-4d29-8235-3ac9fe107256', fixed_text).
narrative_ontology:cs_authority_grounding('84f35645-982e-4d29-8235-3ac9fe107256', lineage).
narrative_ontology:cs_interpretation_layer_present('84f35645-982e-4d29-8235-3ac9fe107256').
narrative_ontology:cs_reading_relation('84f35645-982e-4d29-8235-3ac9fe107256', sacrifice_commandment__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('84f35645-982e-4d29-8235-3ac9fe107256', sacrifice_commandment__performance_only, influences).
narrative_ontology:cs_axiom('84f35645-982e-4d29-8235-3ac9fe107256', foundational, study_has_deferred_not_present_value).
narrative_ontology:cs_axiom_status(study_has_deferred_not_present_value, holdable).
narrative_ontology:cs_axiom_grounding('84f35645-982e-4d29-8235-3ac9fe107256', study_has_deferred_not_present_value, conventional).
narrative_ontology:cs_axiom('84f35645-982e-4d29-8235-3ac9fe107256', secondary, restoration_is_the_operative_sunset_condition).
narrative_ontology:cs_axiom_status(restoration_is_the_operative_sunset_condition, holdable).
narrative_ontology:cs_axiom_grounding('84f35645-982e-4d29-8235-3ac9fe107256', restoration_is_the_operative_sunset_condition, theological).
narrative_ontology:cs_reference_frame('84f35645-982e-4d29-8235-3ac9fe107256', temple_era_sacrificial_practice).
narrative_ontology:cs_drift_state('84f35645-982e-4d29-8235-3ac9fe107256', post_talmudic_diaspora_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('84f35645-982e-4d29-8235-3ac9fe107256', '').
narrative_ontology:cs_kernel_id(sacrifice_commandment__archive_maintenance, sacrifice_commandment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, future_restored_temple_priesthood).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, yeshiva_institutions_teaching_kodashim).
narrative_ontology:constraint_beneficiary(sacrifice_commandment__archive_maintenance, messianic_continuity_tradition).
narrative_ontology:constraint_victim(sacrifice_commandment__archive_maintenance, present_generation_students_of_kodashim).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, temple_will_be_rebuilt).
narrative_ontology:constraint_vindicates(sacrifice_commandment__archive_maintenance, technical_continuity_of_halakha).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces the curriculum requiring extensive study of Temple sacrificial procedure — tractates like Zevachim and Menachot — as a permanent feature of advanced study, framed explicitly as preservation work rather than devotional performance. Institutions gain prestige, continuity of a specialized scholarly tradition, and a rationale for maintaining faculty and curricular structures indefinitely, since the framing defers any test of present relevance to an unspecified future restoration.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, yeshiva_institutions_teaching_kodashim, agenda_setter,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__archive_maintenance, yeshiva_institutions_teaching_kodashim, beneficiary).

% Spend years of study on procedures with no present ritual enactment and uncertain future applicability, told the value is archival — preparing technical knowledge for a restoration that may not occur in their lifetime. Their exit options are limited by the weight the tradition places on this study as a marker of scholarly seriousness; opting out of Kodashim study can carry reputational cost within the institution even though no sacrifice will ever be performed by them.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, present_generation_students_of_kodashim, payer,
    moderate, biographical, constrained, national).

% A hypothetical future beneficiary — the priests and functionaries who would, upon an eventual Temple restoration, draw on preserved technical knowledge (slaughter procedures, blood application order, disqualifying conditions) rather than reconstructing it from scratch. This beneficiary does not yet exist and cannot corroborate, benefit, or object; the entire justification structure rests on their eventual materialization.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, future_restored_temple_priesthood, beneficiary,
    analytical, civilizational, analytical, national).

% Pedagogical approaches that would redirect study time toward prayer, ethics, or present-applicable law are structurally disfavored within institutions organized around the archive-maintenance framing; advocates for reallocating study emphasis toward immediately actionable observance are present in the tradition but do not set the dominant curricular agenda in Kodashim-centered institutions.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, present_worship_alternative_pedagogies, excluded,
    moderate, biographical, constrained, national).

% Rabbinic decisors and roshei yeshiva who adjudicate how much curricular weight sacrificial law deserves relative to actionable law; they observe the tension between preservation value and present cost, and their rulings shape whether the archive-maintenance framing remains dominant or cedes ground to competing readings of the same kernel.
narrative_ontology:constraint_stakeholder(sacrifice_commandment__archive_maintenance, halakhic_authorities_overseeing_curriculum, observer,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_commandment__archive_maintenance, halakhic_authorities_overseeing_curriculum, agenda_setter).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_commandment__archive_maintenance, yeshiva_institutions_teaching_kodashim).
narrative_ontology:fixing_cost_class(sacrifice_commandment__archive_maintenance, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates transmission of a large, technically intricate body of procedural law across generations so that, should Temple service resume, the operational knowledge is not lost and does not need to be reconstructed from fragmentary or contested sources.
% TRANSFER_FUNCTION: Moves scholarly time, institutional resources, and curricular priority from present-applicable study toward preservation of contingently-useful technical knowledge, on the promise that a future (unspecified-date) priesthood will draw on it.
% ABSENT_VOICES: Advocates of reallocating study time toward prayer, ethics, or currently-actionable halakha are present in the broader tradition but structurally outvoted within institutions whose prestige and continuity are bound to Kodashim curricula; the future priesthood who would supposedly benefit cannot corroborate that the preserved material will be usable or even needed in the form preserved.
% DISAPPEARANCE_RATIONALE: If the archive-maintenance framing disappeared, some institutions would reallocate study time toward present-applicable law with little practical loss; others regard the study itself as constitutive of Torah scholarship's completeness and would experience the loss as a rupture in continuity independent of any Temple's status. The verdict is genuinely contested between preservation-committed and practice-committed factions.
% FOUNDING_PROBLEM: Talmudic-era sages faced the loss of Temple service and needed a mechanism to prevent the detailed operational law of sacrifice from being forgotten before any future restoration, given uncertainty about when or whether restoration would occur.
% FOUNDING_PROBLEM_CORROBORATION: Historians of halakha and comparative religion scholars outside the yeshiva system corroborate that the original transmission problem (loss of oral operational detail) was real in the Talmudic period; they diverge from the institutions' own account by noting that once written and extensively glossed across two millennia, the knowledge is now robustly archived in text and largely immune to further loss — making the present-day 'preservation' framing less about transmission risk and more about sustaining institutional study patterns. No source entirely outside the benefiting institutions attests that continued intensive present-day study meaningfully reduces restoration-readiness risk beyond what existing texts already secure.
narrative_ontology:disappearance_verdict(sacrifice_commandment__archive_maintenance, contested).
narrative_ontology:founding_problem_status(sacrifice_commandment__archive_maintenance, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_commandment__archive_maintenance, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is moderate (0.42) rather than low or high because the archive-maintenance framing extracts real present cost (years of scholarly time redirected from actionable law) for a benefit whose recipient does not yet exist and whose need is uncertain — but the extraction is bounded because the material is genuinely preserved in durable written form regardless of continued study, meaning marginal preservation risk from reduced study is low. Suppression (0.38) reflects institutional and reputational pressure to continue Kodashim-centered curricula rather than legal coercion. Theater ratio (0.30, rising modestly) reflects that some curricular time functions as institutional prestige-signaling (mastery of Kodashim as a marker of scholarly seriousness) independent of its preservation function, and this proportion has grown slightly as restoration has receded as an imminent expectation for many communities while study continues at similar intensity.
 *
 * DIRECTIONALITY LOGIC:
 *   The yeshiva institutions are the structural beneficiary: they administer the curriculum, gain prestige and continuity, and bear none of the opportunity cost personally. Present-generation students are the payer: they bear the years of study whose direct value is deferred to a hypothetical future they may not see. The future priesthood is a beneficiary in name only — an analytical/non-corroborating seat whose eventual existence is precisely the omega this story cannot resolve. This asymmetry — real present cost for a beneficiary that does not yet exist — is what keeps extractiveness moderate rather than low: the coordination story (preserving knowledge) is genuine, but its present cost is borne disproportionately by students relative to any present-day corroborated benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold classification is deliberately chosen over snare or tangled_rope: the coordination function (preventing technical loss of complex procedural law) is real and historically grounded, and the arrangement carries an explicit sunset condition — Temple restoration — after which the archive-maintenance rationale would either convert into direct performance obligation or become moot. This prevents mislabeling the practice as pure extraction: unlike a tangled rope, there is no clearly identified party actively profiting from perpetuating present-generation cost against their interest; unlike a snare, exits exist (institutions vary in curricular weight, and individual scholars can and do redirect emphasis). The genealogy check (founding_problem_status: contested) is the load-bearing signal: outside historians corroborate that the original transmission-risk problem was real in the Talmudic period but is now substantially mitigated by durable written transmission, meaning the sunset condition for the ORIGINAL problem may have already partially arrived (the knowledge is archived) even though the institutional practice of continued study has not correspondingly wound down — this is the mandatrophy risk this story flags rather than resolves.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    restoration_probability_and_timescale,
    'What is the actual probability and expected timescale of Temple restoration, and does the archive-maintenance justification''s value scale inversely with how indefinitely that timescale is deferred?',
    'No empirical resolution is possible in principle — this is a messianic/theological claim, not a testable proposition. The closest available proxy is tracking whether communities that hold the archive-maintenance reading show declining, stable, or increasing study-time allocation to Kodashim over generations, which would indicate whether the deferred-benefit framing is experienced as increasingly attenuated.',
    'If restoration is treated as arbitrarily deferred, the present cost-to-beneficiary ratio described in this story approaches a pattern where the beneficiary is permanently non-corroborating — pushing the classification toward tangled_rope or even snare, since indefinite deferral of the sunset condition would mean the scaffold never actually resolves. If restoration is held as near-term and concrete, the scaffold framing is well-supported.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restoration_probability_and_timescale, preference, 'Whether the sunset condition (restoration) functions as a real temporal bound or an indefinitely deferred horizon.').

omega_variable(
    kernel_reading_selection_pressure,
    'Is the archive_maintenance reading selected by particular institutions because it is the most defensible framing given a Temple''s absence, or because it best serves institutional continuity relative to the other two readings (which either dissolve the practice''s urgency or collapse study fully into performance, removing the deferred-value narrative that sustains ongoing curricular investment)?',
    'Compare institutional rhetoric across communities holding different readings: if archive_maintenance is disproportionately adopted by institutions with the largest Kodashim-dedicated infrastructure (relative to performance_only communities, which would have less reason to sustain such infrastructure), that correlation would support an institutional-interest explanation for reading selection.',
    'If reading selection tracks institutional interest rather than independent theological reasoning, the archive_maintenance framing is doing some of the coordination-cover work a false-summit mountain analysis would flag in a mountain context — here it would suggest the moderate-extraction reading is itself partly selected for its extraction-minimizing appearance relative to a franker acknowledgment of ongoing cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_pressure, conceptual, 'Whether this reading''s dominance in certain institutions reflects genuine theological reasoning or institutional self-interest in the framing that best sustains present curricular investment.').

omega_variable(
    written_transmission_sufficiency,
    'Given that Kodashim law has been extensively codified and glossed across two millennia (Mishnah, Talmud, Rambam, later commentators), does continued intensive present-day study add meaningfully to restoration-readiness beyond what the existing textual corpus already secures?',
    'Comparative assessment by historians of halakha (already partially undertaken in this story''s founding_problem_corroboration) of whether textual preservation alone, without an active study tradition, would leave the technical knowledge recoverable in the event of restoration.',
    'If textual preservation alone suffices, the marginal preservation benefit of continued intensive study approaches zero, and the archive-maintenance justification would be substantially overstated relative to its actual founding problem — supporting a mandatrophy-resolved reading. If active study meaningfully improves recoverability (e.g., resolving ambiguities that pure text cannot), the justification holds more of its original force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(written_transmission_sufficiency, empirical, 'Whether the founding transmission-risk problem has already been substantially solved by durable textual archiving, independent of continued study.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_commandment__archive_maintenance, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_commandment__archive_maintenance, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(sacr_tr_t0, observed).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_commandment__archive_maintenance, theater_ratio, 20, 0.21).
narrative_ontology:measurement_basis(sacr_tr_t20, observed).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_commandment__archive_maintenance, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(sacr_tr_t40, observed).
narrative_ontology:measurement(sacr_tr_t60, sacrifice_commandment__archive_maintenance, theater_ratio, 60, 0.27).
narrative_ontology:measurement_basis(sacr_tr_t60, observed).
narrative_ontology:measurement(sacr_tr_t80, sacrifice_commandment__archive_maintenance, theater_ratio, 80, 0.29).
narrative_ontology:measurement_basis(sacr_tr_t80, observed).
narrative_ontology:measurement(sacr_tr_t100, sacrifice_commandment__archive_maintenance, theater_ratio, 100, 0.3).
narrative_ontology:measurement_basis(sacr_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_commandment__archive_maintenance, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(sacr_be_t0, observed).
narrative_ontology:measurement(sacr_be_t20, sacrifice_commandment__archive_maintenance, base_extractiveness, 20, 0.33).
narrative_ontology:measurement_basis(sacr_be_t20, observed).
narrative_ontology:measurement(sacr_be_t40, sacrifice_commandment__archive_maintenance, base_extractiveness, 40, 0.36).
narrative_ontology:measurement_basis(sacr_be_t40, observed).
narrative_ontology:measurement(sacr_be_t60, sacrifice_commandment__archive_maintenance, base_extractiveness, 60, 0.39).
narrative_ontology:measurement_basis(sacr_be_t60, observed).
narrative_ontology:measurement(sacr_be_t80, sacrifice_commandment__archive_maintenance, base_extractiveness, 80, 0.41).
narrative_ontology:measurement_basis(sacr_be_t80, observed).
narrative_ontology:measurement(sacr_be_t100, sacrifice_commandment__archive_maintenance, base_extractiveness, 100, 0.42).
narrative_ontology:measurement_basis(sacr_be_t100, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_commandment__archive_maintenance, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_commandment__archive_maintenance, identity_coordination).
narrative_ontology:boltzmann_floor_override(sacrifice_commandment__archive_maintenance, 0.08).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_commandment__archive_maintenance, sacrifice_commandment__performance_only).

% DUAL FORMULATION NOTE:
% This constraint is one of three linked readings of the sacrifice_commandment kernel, decomposed per the epsilon-invariance principle: performance_only (near-zero extraction; commandment held simply suspended), archive_maintenance (this story; moderate extraction; deferred-benefit preservation framing), and study_as_performance (low extraction dressed as full present value; study itself held to constitute fulfillment). Each carries its own epsilon and stakeholder structure; they are linked via affects_constraints because institutional adoption of one reading changes the resource and legitimacy conditions available to communities holding the others — e.g., archive_maintenance's curricular infrastructure investment creates downstream pressure making study_as_performance more institutionally convenient to adopt, since the same study hours are already being spent.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
