% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__study_as_exercise, []).

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
 *   constraint_id: kodashim_corpus__study_as_exercise
 *   human_readable: Study of Sacrifice Law as Mitzvah Performance
 *   domain: religious_studies/rabbinic_judaism
 *
 * SUMMARY:
 *   This constraint instantiates the study_as_exercise reading of the
 *   contested kodashim_corpus kernel in Rabbinic Judaism. After the
 *   destruction of the Second Temple, the physical apparatus of sacrifice was
 *   destroyed. This reading claims that scholarly engagement with the
 *   sacrificial lawsâparticularly the tractates of Kodashimâis itself the
 *   complete performance of the mitzvah, and that the kernel remains actively
 *   occupied through continuous intellectual-spiritual engagement rather than
 *   archived or superseded.
 *
 * KEY AGENTS:
 *   - Talmudic scholars (beneficiary/organized/identity_locked): occupy the kernel through study and are understood to maintain cosmic order.
 *   - Rabbinic academies (agenda_setter/institutional/constrained): set curricula and validate the interpretive methods that make study-as-practice normatively legible.
 *   - Kohanim (excluded/moderate/constrained): hereditary priests structurally absent from the current occupation regime.
 *   - Critical historians (observer/analytical/analytical): outside analytical seat attesting to the founding crisis and contesting the solution.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.06).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.12).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.06).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Study of Sacrifice Law as Mitzvah Performance").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious_studies/rabbinic_judaism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, '99b431c6-a382-48e9-8b5a-339015f79d68').
narrative_ontology:cs_kernel_codification('99b431c6-a382-48e9-8b5a-339015f79d68', fixed_text).
narrative_ontology:cs_authority_grounding('99b431c6-a382-48e9-8b5a-339015f79d68', lineage).
narrative_ontology:cs_interpretation_layer_present('99b431c6-a382-48e9-8b5a-339015f79d68').
narrative_ontology:cs_reading_relation('99b431c6-a382-48e9-8b5a-339015f79d68', kodashim_corpus__performance_only, coexists_with).
narrative_ontology:cs_reading_relation('99b431c6-a382-48e9-8b5a-339015f79d68', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('99b431c6-a382-48e9-8b5a-339015f79d68', foundational, study_fulfills_sacrificial_mitzvah).
narrative_ontology:cs_axiom_status(study_fulfills_sacrificial_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('99b431c6-a382-48e9-8b5a-339015f79d68', study_fulfills_sacrificial_mitzvah, deontological).
narrative_ontology:cs_axiom('99b431c6-a382-48e9-8b5a-339015f79d68', foundational, kernel_occupied_not_suspended).
narrative_ontology:cs_axiom_status(kernel_occupied_not_suspended, holdable).
narrative_ontology:cs_axiom_grounding('99b431c6-a382-48e9-8b5a-339015f79d68', kernel_occupied_not_suspended, deontological).
narrative_ontology:cs_reference_frame('99b431c6-a382-48e9-8b5a-339015f79d68', torah_study_as_kernel_occupation).
narrative_ontology:cs_drift_state('99b431c6-a382-48e9-8b5a-339015f79d68', post_temple_destruction_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('99b431c6-a382-48e9-8b5a-339015f79d68', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, talmudic_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in continuous study of Kodashim tractates as the living fulfillment of the sacrificial mitzvah; their intellectual-spiritual labor is understood to sustain cosmic order and covenantal continuity after the Temple's destruction; exit from this practice means abandoning the scholarly vocation and its covenantal role.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, talmudic_scholars, beneficiary,
    organized, generational, identity_locked, global).

% Set curricula that elevate Kodashim study to mitzvah-performance status, certify interpreters, and maintain the textual infrastructure that makes the sacrificial kernel occupiable through intellect; they sustain the normative framework without extracting material rents.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, rabbinic_academies, agenda_setter,
    institutional, generational, constrained, global).

% Hereditary priests whose ritual jurisdiction over sacrifice is suspended; their claims to the kernel are set aside in favor of scholarly occupation, and their voice in how the kernel is maintained is structurally absent from rabbinic curriculum decisions.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, kohanim, excluded,
    moderate, biographical, constrained, global).

% Analyze the rabbinic study-practice as a historically contingent response to the Temple's destruction, comparing it to other post-cultic religious formations; they do not participate in the covenantal framework but attest to the founding crisis from outside the beneficiary class.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, critical_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains covenantal continuity after the destruction of the Second Temple by coordinating a shared interpretive discipline around the sacrificial laws, preserving a ritually constitutive community without physical altar or priesthood.
% TRANSFER_FUNCTION: Moves the locus of sacrificial mitzvah-fulfillment from the physical Temple priesthood to the rabbinic scholar class, transferring religious efficacy to continuous intellectual engagement; the laity indirectly benefit from maintained cosmic order without direct participation or material cost.
% ABSENT_VOICES: Kohanim (priestly descendants) who would claim exclusive ritual jurisdiction over the sacrificial kernel; sectarian advocates for immediate physical restoration of sacrifice; and modernizing Jewish movements who question the cosmic efficacy of textual study.
% DISAPPEARANCE_RATIONALE: If the constraint vanished, the rabbinic curriculum would lose its organizing center for the Order of Holy Things, the scholar class would lose the primary practice that legitimates their cosmic maintenance role, and the post-Temple community would face a vacuum in covenantal continuity.
% FOUNDING_PROBLEM: The destruction of the Second Temple in 70 CE eliminated the physical site and priestly apparatus for sacrificial worship, creating a crisis of continuity for a Torah-centered community whose covenantal life had been organized around the altar.
% FOUNDING_PROBLEM_CORROBORATION: Secular historians, archaeologists, and critical scholars attest the Temple destruction as historical fact from outside the beneficiary class; they contest whether study genuinely solves the crisis or restructures institutional authority, while rabbinic sources attest the problem from within.
narrative_ontology:disappearance_verdict(kodashim_corpus__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__study_as_exercise, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-20',
    'no_scope_rebuild_kimi', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__study_as_exercise, 0.06, 'kimi-k2.6', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__study_as_exercise_tests).
:- end_tests(kodashim_corpus__study_as_exercise_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is near-zero (0.06) because the constraint coordinates a shared interpretive practice without material transfer or coercion; suppression is low (0.12) because alternatives (messianic restoration movements, Karaite literalism) are marginalized but not violently suppressed; theater_ratio is low-moderate (0.18) because the study is a genuine spiritual-intellectual practice with only modest performative maintenance; accessibility_collapse is moderate (0.35) because within the rabbinic frame the kernel appears fully occupied, while from outside the frame physical restoration remains thinkable; resistance is low (0.15) because the arrangement meets little active opposition from within the community it coordinates.
 *
 * PERSPECTIVAL GAP:
 *   The talmudic_scholars seat experiences the constraint as genuine spiritual coordination and cosmic vocation; the kohanim seat, were it present, would experience the same structure as an exclusion from hereditary ritual jurisdiction; the critical_historians seat sees a historically contingent institutional strategy. The engine computes this divergence from structural position, not from authored claim.
 *
 * DIRECTIONALITY LOGIC:
 *   Talmudic scholars are declared beneficiaries with identity_locked exit: the constraint subsidizes their role and fuses it to their self-concept, placing them near the full-beneficiary end of directionality. Rabbinic academies are agenda_setters with constrained exit: they administer the interpretive framework and derive institutional legitimacy, sitting slightly off the beneficiary pole but not extractive. Kohanim are excluded from the conversation and would sit near the target end if admitted. Critical historians are analytical with no stake in the constraint's operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This is classified as rope rather than piton because the coordination function is live and non-theatrical: the study practice genuinely solves the continuity problem for a post-Temple community, and there is no atrophied function being maintained by inertia. The absence of a victim set, the low theater_ratio, and the low extractiveness all prevent mandatrophy mislabeling. A piton reading would require diffuse costs, no concentrated beneficiary, and high theatrical maintenanceânone of which are present.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kodashim_kernel_reading_contest,
    'Is the Kodashim kernel structurally occupied by study, suspended as a husk awaiting restoration, or superseded by prayer and replaced by memorial archive?',
    'Historical sociology of the rabbinic movement and textual archaeology of Talmudic sources for study-as-practice; comparative analysis of how other post-cultic traditions handle defunct ritual corpora.',
    'If the kernel is superseded or archived, this reading is a coordination fiction or memorial performance rather than live rope; if genuinely occupied, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kodashim_kernel_reading_contest, conceptual, 'Whether study_as_exercise is live occupation or post-hoc legitimation.').

omega_variable(
    scholar_beneficiary_neutrality,
    'Does the concentration of kernel-occupation privilege in the scholar class constitute an asymmetric extraction masked as coordination, or is the benefit purely the diffuse cosmic order the community receives?',
    'Comparative institutional analysis of specialist-class control over defunct ritual domains across religious traditions; measurement of whether lay access to the kernel is structurally discouraged or merely delegated.',
    'If the scholar class captures status, authority, or material support through exclusive occupation of the kernel, the constraint shifts toward tangled_rope; if the benefit is genuinely diffuse, rope stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scholar_beneficiary_neutrality, conceptual, 'Whether beneficiary concentration in the scholar class introduces hidden extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_study_tr_t0, kodashim_corpus__study_as_exercise, theater_ratio, 0, 0.1).
narrative_ontology:measurement(kodashim_study_tr_t400, kodashim_corpus__study_as_exercise, theater_ratio, 400, 0.12).
narrative_ontology:measurement(kodashim_study_tr_t800, kodashim_corpus__study_as_exercise, theater_ratio, 800, 0.14).
narrative_ontology:measurement(kodashim_study_tr_t1200, kodashim_corpus__study_as_exercise, theater_ratio, 1200, 0.16).
narrative_ontology:measurement(kodashim_study_tr_t1600, kodashim_corpus__study_as_exercise, theater_ratio, 1600, 0.17).
narrative_ontology:measurement(kodashim_study_tr_t2000, kodashim_corpus__study_as_exercise, theater_ratio, 2000, 0.18).

% Extraction over time
narrative_ontology:measurement(kodashim_study_be_t0, kodashim_corpus__study_as_exercise, base_extractiveness, 0, 0.04).
narrative_ontology:measurement(kodashim_study_be_t400, kodashim_corpus__study_as_exercise, base_extractiveness, 400, 0.05).
narrative_ontology:measurement(kodashim_study_be_t800, kodashim_corpus__study_as_exercise, base_extractiveness, 800, 0.05).
narrative_ontology:measurement(kodashim_study_be_t1200, kodashim_corpus__study_as_exercise, base_extractiveness, 1200, 0.06).
narrative_ontology:measurement(kodashim_study_be_t1600, kodashim_corpus__study_as_exercise, base_extractiveness, 1600, 0.06).
narrative_ontology:measurement(kodashim_study_be_t2000, kodashim_corpus__study_as_exercise, base_extractiveness, 2000, 0.06).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_corpus__study_as_exercise, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kodashim_corpus kernel, instantiated as study_as_exercise. The kernel decomposes into at least three structurally distinct claims: performance_only (archived husk awaiting restoration), study_as_exercise (occupied through study), and substitution_archive (superseded memorial). Each reading carries a distinct epsilon, beneficiary structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
