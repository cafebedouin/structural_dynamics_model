% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_kernel__symbolic_archive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sacrifice_obligation_kernel__symbolic_archive_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: sacrifice_obligation_kernel__symbolic_archive_reading
 *   human_readable: Sacrifice Law as Cultural-Historical Archive (Symbolic Archive Reading)
 *   domain: religious/cultural/historical
 *
 * SUMMARY:
 *   This story instantiates the symbolic_archive_reading of the
 *   sacrifice_obligation_kernel: the view that the corpus of Temple sacrifice
 *   law (Kodashim, Zevachim, Menachot, and related tractates) functions today
 *   as a cultural-historical archive whose study preserves Jewish collective
 *   memory and continuity but carries no halakhic force — no obligation is
 *   fulfilled, violated, exercised, or suspended by studying it. This reading
 *   is deliberately generated as a standalone, ε-invariant constraint: it
 *   does not describe or average over the sibling readings
 *   (study_as_exercise_reading, performance_only_reading,
 *   messianic_suspension_reading), each of which is a structurally distinct
 *   constraint with its own ε, beneficiary structure, and classification,
 *   generated separately. The archive reading's structural signature is
 *   near-zero extraction because there is, by its own premise, no binding
 *   obligation whose non-performance could be extractive of anything — the
 *   practice is voluntary cultural engagement, not compliance with a rule
 *   that could be shirked or coerced.
 *
 * KEY AGENTS:
 *   - students_of_talmudic_literature: voluntary participants who study without any obligation attaching
 *   - communal_educators: curate and transmit the material within a heritage frame, non-coercively
 *   - jewish_collective_memory_and_identity: the diffuse cultural good sustained, not a rent-collecting actor
 *   - messianic_suspension_adherents and study_as_exercise_adherents: excluded from this reading's own framing, since they hold competing accounts of what the study act accomplishes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_kernel__symbolic_archive_reading, 0.02).
domain_priors:suppression_score(sacrifice_obligation_kernel__symbolic_archive_reading, 0.01).
domain_priors:theater_ratio(sacrifice_obligation_kernel__symbolic_archive_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, extractiveness, 0.02).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, suppression_requirement, 0.01).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, accessibility_collapse, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_kernel__symbolic_archive_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_kernel__symbolic_archive_reading, rope).
narrative_ontology:human_readable(sacrifice_obligation_kernel__symbolic_archive_reading, "Sacrifice Law as Cultural-Historical Archive (Symbolic Archive Reading)").
narrative_ontology:topic_domain(sacrifice_obligation_kernel__symbolic_archive_reading, "religious/cultural/historical").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_kernel__symbolic_archive_reading, '321894cb-7e7e-4521-8ae6-b2f2171e2be5').
narrative_ontology:cs_kernel_codification('321894cb-7e7e-4521-8ae6-b2f2171e2be5', fixed_text).
narrative_ontology:cs_authority_grounding('321894cb-7e7e-4521-8ae6-b2f2171e2be5', distributed).
narrative_ontology:cs_reading_relation('321894cb-7e7e-4521-8ae6-b2f2171e2be5', sacrifice_obligation_kernel__messianic_suspension_reading, coexists_with).
narrative_ontology:cs_reading_relation('321894cb-7e7e-4521-8ae6-b2f2171e2be5', sacrifice_obligation_kernel__performance_only_reading, influences).
narrative_ontology:cs_reading_relation('321894cb-7e7e-4521-8ae6-b2f2171e2be5', sacrifice_obligation_kernel__study_as_exercise_reading, coexists_with).
narrative_ontology:cs_axiom('321894cb-7e7e-4521-8ae6-b2f2171e2be5', foundational, sacrifice_study_carries_no_halakhic_force).
narrative_ontology:cs_axiom_status(sacrifice_study_carries_no_halakhic_force, holdable).
narrative_ontology:cs_axiom_grounding('321894cb-7e7e-4521-8ae6-b2f2171e2be5', sacrifice_study_carries_no_halakhic_force, conventional).
narrative_ontology:cs_axiom('321894cb-7e7e-4521-8ae6-b2f2171e2be5', secondary, cultural_continuity_is_sufficient_justification_for_study).
narrative_ontology:cs_axiom_status(cultural_continuity_is_sufficient_justification_for_study, holdable).
narrative_ontology:cs_axiom_grounding('321894cb-7e7e-4521-8ae6-b2f2171e2be5', cultural_continuity_is_sufficient_justification_for_study, instrumental).
narrative_ontology:cs_reference_frame('321894cb-7e7e-4521-8ae6-b2f2171e2be5', temple_era_sacrificial_obligation).
narrative_ontology:cs_drift_state('321894cb-7e7e-4521-8ae6-b2f2171e2be5', post_temple_diaspora_scholarship, gap(codification_collapse, severe, true)).
narrative_ontology:cs_created_at('321894cb-7e7e-4521-8ae6-b2f2171e2be5', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory_and_identity).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, students_of_talmudic_literature).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_kernel__symbolic_archive_reading, communal_educators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Study the sacrificial tractates (Kodashim, Zevachim, Menachot) as part of a voluntary curriculum of Torah learning. Nothing compels them to study this material over any other; they choose it for intellectual, spiritual, or cultural reasons. No obligation is discharged or violated by studying more or less; they can stop, switch to other tractates, or return without consequence beyond personal preference.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, students_of_talmudic_literature, beneficiary,
    moderate, biographical, mobile, global).

% Curate and teach sacrifice-law texts within yeshivot, day schools, and adult learning programs, framing the material as heritage and intellectual tradition rather than active legal obligation. They set the pedagogical agenda but administer nothing coercive — no enforcement mechanism attaches to non-participation.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, communal_educators, agenda_setter,
    moderate, generational, mobile, national).

% Represents the continuity of communal narrative, textual literacy, and historical self-understanding sustained by ongoing engagement with sacrificial law as archived tradition. Not an actor that collects rents; it is the diffuse cultural good the practice sustains.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory_and_identity, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(sacrifice_obligation_kernel__symbolic_archive_reading, jewish_collective_memory_and_identity).

% Hold that the obligation is merely suspended, not archival, and that study functions as operational readiness for a restored Temple. Their view is not represented within this reading's framing of the practice as symbolic and non-binding; they would object that treating the law as mere archive undersells its live, if dormant, legal status.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, messianic_suspension_adherents, excluded,
    moderate, civilizational, constrained, global).

% Hold that intellectual engagement with sacrifice law itself constitutes fulfillment of the mitzvah — a live halakhic act, not a cultural exercise. Their view is excluded from this reading, which denies any halakhic weight to the study act.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, study_as_exercise_adherents, excluded,
    moderate, civilizational, constrained, global).

% Rabbinic authorities and scholars across the four readings observe how communities actually treat sacrifice-law study, without this reading itself requiring their adjudication — the archive reading positions itself as pre-halakhic, sidestepping the arbitration that the other three readings require.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_kernel__symbolic_archive_reading, halakhic_arbiters_analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sacrifice_obligation_kernel__symbolic_archive_reading, diffuse).
narrative_ontology:fixing_cost_class(sacrifice_obligation_kernel__symbolic_archive_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates continued transmission of a large body of ancient legal-ritual literature across generations by giving communities a low-stakes, non-coercive reason to keep studying it — preserving textual literacy, communal narrative, and historical self-understanding even absent any operative legal obligation.
% TRANSFER_FUNCTION: Moves nothing coercively: attention and pedagogical effort flow voluntarily from educators and learners into sustained engagement with the sacrificial tractates, and what returns is cultural continuity, literacy, and a felt connection to communal history — not compliance with a binding rule.
% ABSENT_VOICES: Adherents of the messianic-suspension and study-as-exercise readings are not represented within this reading's own terms — both would object that stripping the law of any halakhic status understates its status, but they are not participants in the archive framing itself, only observers of a different kernel-reading contest.
% DISAPPEARANCE_RATIONALE: If the archival study practice ceased tomorrow, no binding obligation would be violated and no legal status would change, because this reading holds that none exists — communities would lose a channel of cultural transmission and educators would lose a curricular resource, but no one's halakhic standing would be affected. The comparative loss is real but is a loss of cultural good, not a rearrangement of any enforced arrangement.
% FOUNDING_PROBLEM: After the Temple's destruction, the sacrificial legal corpus risked becoming inert or forgotten entirely; this reading holds that recasting its study as cultural-historical preservation (rather than either literal legal obligation or exercise-fulfillment) gives communities a reason to keep the material alive without asserting a legal claim that reality (no Temple, no altar) cannot support.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Jewish liturgy and academic scholars of rabbinics (outside any of the four halakhic reading communities) attest that communities have in fact continued studying these tractates through purely cultural-identity motives across centuries, independent of which halakhic reading a given community holds — this is external corroboration from the academic study of religion, not from any beneficiary of the archive reading itself. No rabbinic authority within the four contesting readings corroborates this specific reading's own denial of halakhic weight, since that denial is precisely what the sibling readings dispute.
narrative_ontology:disappearance_verdict(sacrifice_obligation_kernel__symbolic_archive_reading, world_unchanged).
narrative_ontology:founding_problem_status(sacrifice_obligation_kernel__symbolic_archive_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sacrifice_obligation_kernel__symbolic_archive_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_kernel__symbolic_archive_reading, 0.02, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).
:- end_tests(sacrifice_obligation_kernel__symbolic_archive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored near zero (0.02) because this reading's defining premise is that no binding obligation exists to be violated, so there is no rent, no coerced compliance, and no cost transferred from a payer to a beneficiary in the way a Tangled Rope or Snare would require. Suppression is near-zero (0.01) because nothing coerces study or non-study; accessibility_collapse is low (0.05) because the alternative of not engaging with this material at all remains fully open and unpenalized. Resistance is low (0.1) — the mild resistance that exists comes from adherents of sibling readings who object to this reading's own halakhic minimalism, not from anyone resisting an imposed cost. Theater ratio is low (0.05): the little performative element present (ceremonial recitation of sacrificial texts in some liturgies) is small relative to genuine study activity and is itself understood by this reading as symbolic, not disguised compliance.
 *
 * PERSPECTIVAL GAP:
 *   There is minimal seat divergence within this reading because no seat bears an enforced cost: students, educators, and the diffuse cultural-memory beneficiary all experience the practice as voluntary and low-stakes. The real divergence is EXTERNAL to this reading — between this reading and its siblings, which is exactly why the committer-frame routes that contest to omega variables rather than folding it into this constraint's own classification.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (students, educators, collective memory/identity) sit near the full-beneficiary end of directionality because the practice subsidizes their cultural and educational goods at negligible cost to them; there is no victim group because this reading structurally denies that any obligation-bearing party exists to be extracted from. The excluded stakeholders (adherents of sibling readings) are not victims of this constraint — they are excluded VOICES whose disagreement is about which reading is correct, not about costs this reading imposes on them.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is the clearest possible non-mandatrophy case in the kernel family: there is no mandate to have outlived its function, because this reading holds that no operative mandate exists in the first place — only a cultural practice that continues because communities find it valuable, not because any authority enforces it. Classifying it as Rope (not Tangled Rope or Piton) prevents mislabeling a genuinely voluntary heritage practice as either coercive extraction or as a decayed institution requiring justification it can no longer supply.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archive_vs_suspended_obligation,
    'Is the sacrifice-law study practice best understood as cultural archive with no live obligation (this reading), or as an obligation merely suspended pending messianic restoration (the sibling messianic_suspension_reading), where study maintains operational readiness for a resumed duty?',
    'No empirical resolution is possible — this is a live theological dispute within different communities'' own commitment frameworks, not an empirical question about the world. Resolution mechanism (to the extent one exists) is doctrinal: which reading a given community or authority formally adopts.',
    'If the suspension reading is correct rather than the archive reading, the practice carries latent halakhic weight and a different beneficiary/obligation structure entirely — this would be a structurally different constraint, not a recalibration of this one''s metrics, per the ε-invariance principle.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(archive_vs_suspended_obligation, conceptual, 'Whether the sacrifice-study practice is archival (no obligation) or dormant obligation (suspended, not dissolved) — the central committer-frame disagreement with the messianic_suspension_reading.').

omega_variable(
    study_fulfills_mitzvah_or_not,
    'Does the intellectual act of studying sacrifice law constitute genuine occupation/fulfillment of the mitzvah (the sibling study_as_exercise_reading), or does it carry no halakhic weight whatsoever (this reading)?',
    'Internal to rabbinic hermeneutics: resolved (for a given community) by which authorities and interpretive traditions that community follows regarding the halakhic status of talmud torah as substitute for maaseh (physical performance).',
    'If study genuinely fulfills the mitzvah, the practice has a positive halakhic valence this reading denies, changing its classification from voluntary cultural rope to something closer to a coordination mechanism with genuine (if non-coercive) obligation-discharge function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_fulfills_mitzvah_or_not, conceptual, 'Whether study itself has halakhic force (study_as_exercise_reading) or none (this reading) — located at the disagreement over what talmud torah accomplishes relative to maaseh.').

omega_variable(
    archive_framing_beneficiary_of_diaspora_conditions,
    'Does the archive reading persist because it is doctrinally correct, or because diaspora conditions (no Temple, no altar, no priesthood) make it the most practically comfortable reading for communities to hold, regardless of its doctrinal merits relative to the suspension or exercise readings?',
    'Historical-sociological analysis of when and where the archive framing gained prominence relative to messianic-restorationist movements — if archive-framing correlates with periods/communities emphasizing accommodation over restoration, that would suggest doctrinal convenience plays a role alongside doctrinal argument.',
    'If the archive reading is substantially motivated by practical convenience rather than pure doctrinal reasoning, this does not change its own zero-extraction structure (no coercion exists regardless of motive) but would inform how much interpretive weight the reading deserves relative to its siblings in broader theological discourse.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(archive_framing_beneficiary_of_diaspora_conditions, conceptual, 'Whether the archive reading''s prominence reflects doctrinal argument, practical accommodation to diaspora conditions, or both.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_kernel__symbolic_archive_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t0, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(sacr_tr_t10, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 10, 0.04).
narrative_ontology:measurement(sacr_tr_t20, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement(sacr_tr_t30, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 30, 0.05).
narrative_ontology:measurement(sacr_tr_t40, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 40, 0.05).
narrative_ontology:measurement(sacr_tr_t50, sacrifice_obligation_kernel__symbolic_archive_reading, theater_ratio, 50, 0.05).

% Extraction over time
narrative_ontology:measurement(sacr_be_t0, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(sacr_be_t10, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 10, 0.02).
narrative_ontology:measurement(sacr_be_t20, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 20, 0.02).
narrative_ontology:measurement(sacr_be_t30, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 30, 0.02).
narrative_ontology:measurement(sacr_be_t40, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 40, 0.02).
narrative_ontology:measurement(sacr_be_t50, sacrifice_obligation_kernel__symbolic_archive_reading, base_extractiveness, 50, 0.02).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_kernel__symbolic_archive_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__study_as_exercise_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__performance_only_reading).
narrative_ontology:affects_constraint(sacrifice_obligation_kernel__symbolic_archive_reading, sacrifice_obligation_kernel__messianic_suspension_reading).

% DUAL FORMULATION NOTE:
% This story is one of four constraints decomposing the natural-language label 'the sacrifice-law obligation debate' per the epsilon-invariance principle: symbolic_archive_reading (this story, epsilon ~0.02, Rope), study_as_exercise_reading (higher epsilon expected if framed as live obligation-discharge with contested boundaries), performance_only_reading (obligation persists but is unfulfillable absent the Temple, generating distinct tension), and messianic_suspension_reading (obligation dormant, study as readiness-maintenance). Each sibling is generated as its own file with its own ordinary metrics; this file links to all three via affects_constraints because they share the same underlying kernel and a shift in one reading's prominence in communal practice exerts structural pressure on the others' plausibility and adoption.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
