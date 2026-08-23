% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Study of Sacrifice Law as Performative Mitzvah Fulfillment
 *   domain: religious_studies/rabbinic_judaism
 *
 * SUMMARY:
 *   This constraint instantiates the study_as_exercise reading of the
 *   kodashim_corpus kernel within Rabbinic Judaism. The kernel concerns the
 *   biblical commandment of sacrifice (korbanot) in the Temple. After the
 *   Second Temple's destruction, three main readings contest the kernel's
 *   status: performance_only (the law is a husk awaiting messianic
 *   restoration), substitution_archive (prayer and study replaced sacrifice,
 *   rendering Kodashim a memorial), and this reading, study_as_exercise
 *   (intellectual engagement with sacrificial law is itself the living
 *   performance of the mitzvah, maintaining cosmic order). This reading
 *   frames the kernel as actively occupied rather than archived or dormant.
 *   It functions as coordination around shared interpretive practice, with
 *   scholars as the primary beneficiaries of the spiritual and social goods
 *   produced.
 *
 * KEY AGENTS:
 *   - talmudic_scholars: Primary beneficiaries (organized/global) â engage in study as worship, derive spiritual fulfillment and social status
 *   - yeshiva_institutions: Agenda-setters (institutional/generational) â structure curricula and validate the hermeneutic framework
 *   - diaspora_communities: Secondary beneficiaries (organized/global) â receive maintained cosmic order and continuity
 *   - temple_restoration_advocates: Excluded voices (moderate/national) â hold the performance_only reading, structurally absent from rabbinic halakhic discourse
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.08).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.12).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.08).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Study of Sacrifice Law as Performative Mitzvah Fulfillment").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious_studies/rabbinic_judaism").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, 'f134140e-2be0-4b0c-9bdd-83f84e3e3b00').
narrative_ontology:cs_kernel_codification('f134140e-2be0-4b0c-9bdd-83f84e3e3b00', fixed_text).
narrative_ontology:cs_authority_grounding('f134140e-2be0-4b0c-9bdd-83f84e3e3b00', lineage).
narrative_ontology:cs_interpretation_layer_present('f134140e-2be0-4b0c-9bdd-83f84e3e3b00').
narrative_ontology:cs_reading_relation('f134140e-2be0-4b0c-9bdd-83f84e3e3b00', kodashim_corpus__performance_only, forecloses).
narrative_ontology:cs_reading_relation('f134140e-2be0-4b0c-9bdd-83f84e3e3b00', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('f134140e-2be0-4b0c-9bdd-83f84e3e3b00', foundational, study_occupies_sacrificial_mitzvah).
narrative_ontology:cs_axiom_status(study_occupies_sacrificial_mitzvah, holdable).
narrative_ontology:cs_axiom_grounding('f134140e-2be0-4b0c-9bdd-83f84e3e3b00', study_occupies_sacrificial_mitzvah, deontological).
narrative_ontology:cs_axiom('f134140e-2be0-4b0c-9bdd-83f84e3e3b00', secondary, rabbinic_succession_from_priestly_cult).
narrative_ontology:cs_axiom_status(rabbinic_succession_from_priestly_cult, holdable).
narrative_ontology:cs_axiom_grounding('f134140e-2be0-4b0c-9bdd-83f84e3e3b00', rabbinic_succession_from_priestly_cult, conventional).
narrative_ontology:cs_reference_frame('f134140e-2be0-4b0c-9bdd-83f84e3e3b00', study_based_temple_service).
narrative_ontology:cs_drift_state('f134140e-2be0-4b0c-9bdd-83f84e3e3b00', contemporary_diaspora, gap(stable, minor, true)).
narrative_ontology:cs_created_at('f134140e-2be0-4b0c-9bdd-83f84e3e3b00', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, talmudic_scholars).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, diaspora_communities).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, oral_torah_authority).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, rabbinic_succession_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in continuous study of Kodashim tractates as an act of worship; their intellectual labor is understood to actively fulfill the biblical commandment of sacrifice and sustain cosmic order in the absence of the Temple.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, talmudic_scholars, beneficiary,
    organized, generational, constrained, global).

% Structure curricula, set schedules, and validate hermeneutic methods that equate textual study with Temple service; they administer the institutional framework within which the study-as-exercise reading is reproduced.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, yeshiva_institutions, agenda_setter,
    institutional, generational, constrained, global).

% Receive spiritual benefit from the maintained cosmic order and covenantal continuity produced by the scholarly study practice; they support the institutional framework financially and socially but do not typically engage in advanced Kodashim study themselves.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, diaspora_communities, beneficiary,
    organized, generational, constrained, global).

% Advocate for the literal rebuilding of the Temple and resumption of physical sacrifice; their performance_only reading is structurally marginalized by the study-as-exercise framework, which obviates the need for immediate physical restoration.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, temple_restoration_advocates, excluded,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains collective continuity with the Temple sacrificial tradition through distributed textual-interpretive practice, ensuring the mitzvah remains actively fulfilled and cosmic order sustained in the absence of physical infrastructure.
% TRANSFER_FUNCTION: Moves spiritual authority and cosmic maintenance capacity from the priestly Temple service to the rabbinic scholarly community; transfers time and intellectual energy from individual scholars to the collective task of textual occupation.
% ABSENT_VOICES: Temple restoration advocates and priestly families who hold that physical sacrifice is the only valid fulfillment; they are absent from the dominant rabbinic halakhic conversation because this reading renders their position practically and theologically marginal.
% DISAPPEARANCE_RATIONALE: If the equation between study and sacrifice dissolved, rabbinic Judaism would lose its primary post-Temple mechanism for covenantal maintenance; the scholarly economy would reorient, diaspora communities would face a theological vacuum, and pressure for physical restoration or alternative supersession frameworks would intensify radically.
% FOUNDING_PROBLEM: The destruction of the Second Temple and the subsequent inability to perform biblical sacrificial commandments, threatening covenantal continuity and cosmic order.
% FOUNDING_PROBLEM_CORROBORATION: Attested within the Talmudic literature (e.g., Menachot 110a, Taanit 27b) by the beneficiary scholarly tradition; corroborated by external historians of religion (Jacob Neusner, Ephraim Urbach) who document the post-70 CE rabbinic shift to study and prayer as functional replacements for sacrifice, though they read it as historical reconstruction rather than normative fulfillment.
narrative_ontology:disappearance_verdict(kodashim_corpus__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__study_as_exercise, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__study_as_exercise, 0.08, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is authored at 0.08: the constraint coordinates voluntary scholarly engagement around a shared textual practice with minimal coercive overhead. No agent is deprived of resources or status by the arrangement itself; the 'cost' is the opportunity cost of study time, which participants willingly bear as spiritual exercise. Suppression is low (0.12) because the reading does not actively suppress alternatives; it competes discursively but lacks enforcement machinery. Theater ratio is moderate-low (0.25) because while the study performance has ritualized elements (chanting, specific postures, institutional schedules), the coordination functionâmaintaining textual continuity and communal orderâis substantively real. Accessibility collapse is moderate (0.35) because alternative readings (performance_only, substitution_archive) remain intellectually available, though this reading dominates the Orthodox rabbinic sphere. Resistance is low (0.10) as the reading is broadly accepted within its target community. Temporal measurements show stable, flat low extraction over the interval, consistent with a durable rope.
 *
 * PERSPECTIVAL GAP:
 *   Within the rabbinic framework, the agenda_setter and beneficiary seats largely align: yeshiva institutions and scholars mutually reinforce the study-as-exercise framework. The primary divergence would be computed for the excluded temple_restoration_advocates, who, if granted a seat, would experience the constraint as a snare or tangled rope suppressing their alternative. However, within the authored structure of this reading, there is no victim seat; the engine will compute rope for all present stakeholders, while the absence of the excluded seat is documented in Q4.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (talmudic_scholars, diaspora_communities) derive low directionality (near 0.0) because the constraint subsidizes their spiritual economy and continuity. There are no declared victims. Yeshiva institutions sit low-to-moderate d because they administer the framework but also depend on it for institutional legitimacy. Temple restoration advocates would sit at high d if included, but are excluded from this reading's structural framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problemâhow to fulfill sacrifice after Temple destructionâremains live (no Temple has been rebuilt), and the coordination function (maintaining covenantal continuity through study) remains operative. There is no evidence the constraint has outlived its function or degraded into piton-like theatricality; the study practice continues to coordinate genuine communal activity and textual transmission. Thus mandatrophy is not declared.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    study_extraction_from_laity,
    'Does the institutional arrangement of full-time Torah study extract material support from the lay community, creating a diffuse victim set not captured by the sacrificial-study framework itself?',
    'Economic analysis of kollel and yeshiva funding flows; sociological study of material obligations placed on non-studying community members.',
    'If significant material extraction exists, the constraint may recompute as tangled_rope rather than rope, with lay community members as undeclared victims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_extraction_from_laity, empirical, 'Whether lay material support for scholars constitutes hidden extraction').

omega_variable(
    kernel_reading_foreclosure,
    'Does the study_as_exercise reading logically foreclose the performance_only reading, or can they coexist as temporal phases (present study, future sacrifice)?',
    'Textual analysis of rabbinic sources: do classical authorities who hold study_as_exercise also affirm future sacrifice, or do they foreclose it?',
    'If foreclosed, the relation to performance_only should be forecloses; if coexistence is possible within a single framework, coexists_with is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Logical relation between study fulfillment and future restoration').

omega_variable(
    study_access_ascriptive_closure,
    'To what extent does the study-as-exercise framework restrict access to primary religious fulfillment to those with the cognitive and social capital for advanced Talmud study?',
    'Demographic and ethnographic analysis of who engages in Kodashim study across gender, class, and educational lines.',
    'If access is heavily restricted, the coordination may functionally extract status and spiritual opportunity from excluded groups, shifting classification toward tangled_rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(study_access_ascriptive_closure, empirical, 'Whether study-based fulfillment creates ascriptive closure').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__study_as_exercise, theater_ratio, 0, 0.2).
narrative_ontology:measurement(koda_tr_t5, kodashim_corpus__study_as_exercise, theater_ratio, 5, 0.22).
narrative_ontology:measurement(koda_tr_t10, kodashim_corpus__study_as_exercise, theater_ratio, 10, 0.24).
narrative_ontology:measurement(koda_tr_t15, kodashim_corpus__study_as_exercise, theater_ratio, 15, 0.26).
narrative_ontology:measurement(koda_tr_t20, kodashim_corpus__study_as_exercise, theater_ratio, 20, 0.28).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__study_as_exercise, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(koda_be_t5, kodashim_corpus__study_as_exercise, base_extractiveness, 5, 0.08).
narrative_ontology:measurement(koda_be_t10, kodashim_corpus__study_as_exercise, base_extractiveness, 10, 0.09).
narrative_ontology:measurement(koda_be_t15, kodashim_corpus__study_as_exercise, base_extractiveness, 15, 0.08).
narrative_ontology:measurement(koda_be_t20, kodashim_corpus__study_as_exercise, base_extractiveness, 20, 0.08).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_corpus__study_as_exercise, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, substitution_archive).

% DUAL FORMULATION NOTE:
% The kodashim_corpus kernel decomposes into three structurally distinct readings: performance_only (dormant kernel awaiting restoration), study_as_exercise (active occupation through study), and substitution_archive (superseded memorial). Each reading carries a different epsilon, beneficiary structure, and classification. They form a constraint family linked by shared textual kernel but divergent ontological claims about the status of sacrificial law.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
