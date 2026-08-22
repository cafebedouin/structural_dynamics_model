% ============================================================================
% CONSTRAINT STORY: sacrifice_obligation_continuity__archival_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: sacrifice_obligation_continuity__archival_preservation
 *   human_readable: Sacrificial Law as Archival Heritage: Non-Normative Textual Preservation
 *   domain: religious/textual_tradition
 *
 * SUMMARY:
 *   Under this reading, the sacrificial precepts carry no binding force in
 *   the absence of the Temple cult, and the extensive study apparatus built
 *   around them — curricula, translations, commentaries, museum displays — is
 *   cultural memory-keeping rather than religious obligation. Participation
 *   is voluntary, alternatives are fully open, and no seat bears an imposed
 *   cost. The story is deliberately near-vacuous as a constraint: its
 *   interest lies in what the near-zero profile reveals against its sibling
 *   readings, which bind adherents to varying degrees. KEY AGENTS (by
 *   structural relationship): - heritage_text_students:
 *   Participant-beneficiary (moderate/mobile) — gains knowledge and
 *   continuity, pays tuition and time, free to leave -
 *   diaspora_memory_institutions: Agenda-setter and beneficiary
 *   (institutional/generational) — sets curricula, sustains the corpus,
 *   mission-bound - academic_biblical_scholars: External beneficiary
 *   (organized/arbitrage) — consumes the corpus for research, portable skills
 *   - traditionalist_normative_constituency: Excluded voice
 *   (organized/mobile) — rejects the deflationary framing, operates parallel
 *   institutions, bears no cost here - cultural_historians: Analytical
 *   observer (analytical/analytical) — sees the full structure from outside
 *
 * KEY AGENTS:
 *   - heritage_text_students: participant-beneficiary (moderate/mobile) — voluntary learners bearing only chosen costs
 *   - diaspora_memory_institutions: agenda-setter and beneficiary (institutional/generational) — curriculum-setters whose missions depend on the corpus staying teachable
 *   - academic_biblical_scholars: external beneficiary (organized/arbitrage) — scholarly consumers of the preserved corpus
 *   - traditionalist_normative_constituency: excluded voice (organized/mobile) — holders of the rival normative framings, outside this conversation by choice and by venue
 *   - cultural_historians: analytical observer (analytical/analytical) — comparative view of the memory-keeping practice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sacrifice_obligation_continuity__archival_preservation, 0.03).
domain_priors:suppression_score(sacrifice_obligation_continuity__archival_preservation, 0.05).
domain_priors:theater_ratio(sacrifice_obligation_continuity__archival_preservation, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, extractiveness, 0.03).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, accessibility_collapse, 0.15).
narrative_ontology:constraint_metric(sacrifice_obligation_continuity__archival_preservation, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sacrifice_obligation_continuity__archival_preservation, rope).
narrative_ontology:human_readable(sacrifice_obligation_continuity__archival_preservation, "Sacrificial Law as Archival Heritage: Non-Normative Textual Preservation").
narrative_ontology:topic_domain(sacrifice_obligation_continuity__archival_preservation, "religious/textual_tradition").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sacrifice_obligation_continuity__archival_preservation, 'e570338a-bd22-41be-aee1-6edf9cfbd8ba').
narrative_ontology:cs_kernel_codification('e570338a-bd22-41be-aee1-6edf9cfbd8ba', fixed_text).
narrative_ontology:cs_authority_grounding('e570338a-bd22-41be-aee1-6edf9cfbd8ba', expertise).
narrative_ontology:cs_interpretation_layer_present('e570338a-bd22-41be-aee1-6edf9cfbd8ba').
narrative_ontology:cs_reading_relation('e570338a-bd22-41be-aee1-6edf9cfbd8ba', sacrifice_obligation_continuity__study_as_performance, forecloses).
narrative_ontology:cs_reading_relation('e570338a-bd22-41be-aee1-6edf9cfbd8ba', sacrifice_obligation_continuity__performance_only, forecloses).
narrative_ontology:cs_reading_relation('e570338a-bd22-41be-aee1-6edf9cfbd8ba', sacrifice_obligation_continuity__messianic_suspension, forecloses).
narrative_ontology:cs_axiom('e570338a-bd22-41be-aee1-6edf9cfbd8ba', foundational, sacrificial_precepts_non_binding_post_destruction).
narrative_ontology:cs_axiom_status(sacrificial_precepts_non_binding_post_destruction, holdable).
narrative_ontology:cs_axiom_grounding('e570338a-bd22-41be-aee1-6edf9cfbd8ba', sacrificial_precepts_non_binding_post_destruction, empirically_contingent).
narrative_ontology:cs_axiom('e570338a-bd22-41be-aee1-6edf9cfbd8ba', secondary, study_as_cultural_not_commanded_practice).
narrative_ontology:cs_axiom_status(study_as_cultural_not_commanded_practice, holdable).
narrative_ontology:cs_axiom_grounding('e570338a-bd22-41be-aee1-6edf9cfbd8ba', study_as_cultural_not_commanded_practice, conventional).
narrative_ontology:cs_reference_frame('e570338a-bd22-41be-aee1-6edf9cfbd8ba', desacralized_textual_heritage).
narrative_ontology:cs_drift_state('e570338a-bd22-41be-aee1-6edf9cfbd8ba', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('e570338a-bd22-41be-aee1-6edf9cfbd8ba', '').
narrative_ontology:cs_kernel_id(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, heritage_text_students).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, diaspora_memory_institutions).
narrative_ontology:constraint_beneficiary(sacrifice_obligation_continuity__archival_preservation, academic_biblical_scholars).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sacrifice_obligation_continuity__archival_preservation, heritage_text_students).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Attend adult-education classes, university courses, or read independently on the sacrificial codes — Leviticus, the mishnaic tractates on offerings, medieval commentaries. They gain textual literacy, a sense of continuity with ancestral practice, and material for identity conversations. They pay tuition or give time in exchange, and can stop attending at any point without sanction or loss of standing in their communities.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, heritage_text_students, beneficiary,
    moderate, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__archival_preservation, heritage_text_students, payer).

% Synagogue adult-education programs, Jewish studies departments, heritage schools, and academic publishers decide which sacrificial texts enter curricula, commission translations and commentaries, and train the teachers. They receive tuition, grants, and enrollment in return, and their institutional missions are bound up with keeping the corpus teachable; restructuring away from the core texts would mean remaking what these organizations are for.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, diaspora_memory_institutions, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(sacrifice_obligation_continuity__archival_preservation, diaspora_memory_institutions, beneficiary).

% Use the preserved corpus — the Masoretic text, rabbinic compilations, and the archaeology of altars and temples — for research on ancient Israelite religion and comparative cult. They publish, cite, and build careers on the material's availability, and their skills port readily into adjacent fields if the corpus lost scholarly value.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, academic_biblical_scholars, beneficiary,
    organized, biographical, arbitrage, global).

% Communities and authorities who regard the sacrificial precepts as divine legislation awaiting restoration. They run their own schools and liturgical life, where the texts retain binding force, and they object to presentations that reduce the material to heritage. They hold no lever inside the academic and heritage-education settings where the archival framing prevails, but they lose nothing by disengaging from those settings and largely do so.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, traditionalist_normative_constituency, excluded,
    organized, generational, mobile, global).

% Study how communities transmit, exhibit, and sometimes sacralize their pasts. They observe the heritage-study practice from outside, comparing it with other traditions' memory-keeping, and neither collect from it nor bear its costs.
narrative_ontology:constraint_stakeholder(sacrifice_obligation_continuity__archival_preservation, cultural_historians, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Keeps a large, technically demanding corpus — sacrificial codes, mishnaic tractates, commentaries — accurately copied, translated, taught, and indexed across dispersed communities and generations. This is a memory-keeping task no single family or congregation could sustain alone; standardized editions, shared curricula, and trained teachers solve it collectively.
% TRANSFER_FUNCTION: Moves tuition, donations, volunteer hours, and attention from students, donors, and readers to schools, departments, and publishers, which return instruction, editions, and credentials. No duty or penalty moves in either direction; the transfers are purchases and gifts, not levies.
% ABSENT_VOICES: Traditionalist constituencies who hold the precepts binding would object that the archival framing strips the texts of their authority; liturgical communities whose daily prayers rehearse the offerings would object that the archive reading amputates the meaning of their own worship. Neither sits inside the academic and heritage-education venues where this framing is set.
% DISAPPEARANCE_RATIONALE: The physical texts would survive in libraries and critical editions, but the living channel of transmission would close: heritage curricula would shed their core sequence, identity formation would lose one of its few text-based vehicles, and the scholarly pipeline feeding biblical and comparative fields would thin within a generation.
% FOUNDING_PROBLEM: After the destruction of the Second Temple ended the sacrificial cult, the community faced the loss of both the practice and the practical competence its texts encode; later, emancipation and secularization posed the same problem again in a new form — how to keep the inherited corpus teachable and known without the normative commitments a previous era took for granted.
% FOUNDING_PROBLEM_CORROBORATION: Demographic surveys of religious identification and heritage-language attrition (large-scale population studies such as the Pew Research Center's portraits of Jewish Americans, and comparable European communal studies) document the erosion problem independently of the schools and publishers that address it; academic historiography of post-70 CE memory-keeping corroborates the genealogy. No corroborating source comes from inside the benefiting institutions.
narrative_ontology:disappearance_verdict(sacrifice_obligation_continuity__archival_preservation, world_rearranges).
narrative_ontology:founding_problem_status(sacrifice_obligation_continuity__archival_preservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sacrifice_obligation_continuity__archival_preservation, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sacrifice_obligation_continuity__archival_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(sacrifice_obligation_continuity__archival_preservation, 0.03, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sacrifice_obligation_continuity__archival_preservation_tests).
:- end_tests(sacrifice_obligation_continuity__archival_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.03 because the only costs in the arrangement are chosen ones — tuition, time, attention — exchanged for instruction and materials; the small residual reflects soft conformity pressure inside opted-in communities, not imposition. Suppression is 0.05: exit is fully open and exercised constantly (assimilation, translation-only engagement, abstention), so nothing needs coercing. Theater is 0.18 and slowly rising: anniversaries, exhibits, and heritage festivals add a performative layer (museumification of memory), but the core function — accurate transmission of a difficult corpus — remains dominant. Accessibility_collapse is 0.15 because alternatives survive intact once the practice is understood: secular study, vernacular translation, or none at all are all live options with no penalty. Resistance is 0.08: the practice meets indifference more than opposition, with occasional traditionalist objection to the framing itself. The temporal series run on one shared seven-point grid (1780–2024) so every tracked metric is authored at every examined time point; suppression_requirement is deliberately not serialized because the enforcement picture is static (there is effectively no enforcement machinery to build up or decay) — the scalar captures it.
 *
 * PERSPECTIVAL GAP:
 *   From the institution seat, the arrangement is mission-fulfillment: pure coordination the institutions built and sustain, experienced as purpose rather than burden. From the student seat it is a low-cost good. From the excluded traditionalist seat, the same arrangement looks like a category error — sacred legislation exhibited as artifact — and the objection is to the framing's legitimacy, not to any burden it imposes. The engine should compute the same benign type from every interior seat, with the perspectival divergence concentrated at the excluded seat, where the dispute is over what the texts ARE rather than over who pays.
 *
 * DIRECTIONALITY LOGIC:
 *   Every seated party sits at or below the symmetric point. Students are near-beneficiaries: they receive instruction and identity goods and pay only chosen costs. Institutions are mixed but net-beneficiary: they set the agenda and collect revenue and prestige, while carrying the mission-cost of maintenance. Scholars are external beneficiaries drawing on the corpus without sustaining it. The traditionalist constituency is excluded yet unburdened — its costs are borne inside its own parallel institutions, not here — so no high-directionality target exists anywhere in the structure. That absence is precisely what distinguishes this reading from its siblings: in the obligation-bearing readings, adherents become targets and epsilon rises sharply; here the victim set is empty by construction.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandate-atrophy declaration is made: the founding problem — keeping a demanding corpus teachable after the cult that produced it ended — is still live, as continuing assimilation and language attrition attest, so the arrangement has not outlived its function. The misclassification risks in this family run in two directions. The sibling readings risk the classic error of calling a devotional discipline a burden (coordination mistaken for extraction); this reading risks the inverse — extraction-by-accretion mistaken for coordination — if soft normative expectation (curricular 'shoulds', heritage guilt, communal disappointment at lapse) hardens into felt obligation. The omega residual_normativity_boundary watches that boundary; if it closes, this story's epsilon should be revised upward and the rope claim revisited.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading (archival_preservation) of the kernel sacrifice_obligation_continuity; what would each sibling reading change structurally, and where exactly is the disagreement located?',
    'Comparative compilation of the four sibling stories: the disagreement is located in whether the precepts retain normative force absent the Temple cult — study_as_performance locates fulfillment in textual engagement, performance_only in future physical performance, messianic_suspension in a suspended-but-live duty, and this reading in no duty at all.',
    'Any obligation-bearing sibling converts adherents from beneficiaries into targets and raises epsilon sharply (from ~0.03 toward 0.4–0.7 depending on the reading''s compliance demands); the family''s classification spread is the measure of how much hangs on the normative-force question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is one reading of a four-way contested kernel; siblings instantiate different constraints with different victim sets.').

omega_variable(
    residual_normativity_boundary,
    'Is the archival practice genuinely normatively weightless, or does communal expectation — curricular inclusion, bar/bat mitzvah engagement with Leviticus, heritage guilt at lapse — smuggle soft obligation back in?',
    'Survey and interview evidence on whether participants experience any ought-force (shame at stopping, expectation of continuation) distinct from ordinary hobby or course attrition; compare dropout trajectories with matched voluntary adult-education cohorts.',
    'If soft normativity is present, epsilon rises above the information_standard floor and the rope claim comes under pressure toward tangled_rope (coordinated memory-keeping with asymmetric expectation costs on lapsing members); if absent, the near-zero profile stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(residual_normativity_boundary, empirical, 'Whether zero-normativity holds at the level of lived experience or only at the level of official doctrine.').

omega_variable(
    archive_feeds_restoration_pipeline,
    'Does the archival apparatus materially enable the sibling readings — do critical editions, lexica, and archaeological publications supply the textual and evidentiary base that performance_only and messianic_suspension projects (liturgy reconstruction, vessel and rite research) depend on?',
    'Citation and funding tracing from restorationist institutions and publications back to academic and heritage-education outputs; document which restorationist claims are load-bearing on archival infrastructure.',
    'If yes, this reading occupies an upstream network position relative to higher-epsilon siblings: contamination analysis must treat archival degradation (funding collapse, corpus neglect) as a threat to the siblings'' viability, and the family''s dependency arrows run opposite to its normative-authority arrows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_feeds_restoration_pipeline, empirical, 'Whether the normatively weightless reading is nonetheless infrastructurally load-bearing for the obligation-bearing readings.').

omega_variable(
    coordination_function_dominance,
    'Is the dominant coordination function information_standard (accurate transmission of a fixed corpus — editions, curricula, pedagogy) or identity_coordination (boundary maintenance and membership signaling through shared textual competence)?',
    'Test which function''s failure participants treat as decisive: if corrupted texts or broken teacher-lines are the feared loss, information_standard dominates; if dilution of distinctiveness is the feared loss, identity_coordination dominates. Curriculum documents and fundraising appeals are observable traces of which fear drives the institutions.',
    'The two types carry different Boltzmann floors (0.02 vs 0.08): under identity_coordination, the same measured extraction leaves more room flagged as excess overhead; the choice shifts excess-extraction verdicts for the whole family, so it should be settled before cross-sibling comparison.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_function_dominance, conceptual, 'Framing under-determination in the coordination-type declaration, with floor consequences.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sacrifice_obligation_continuity__archival_preservation, 1780, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sacr_tr_t1780, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 1780, 0.06).
narrative_ontology:measurement(sacr_tr_t1820, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 1820, 0.07).
narrative_ontology:measurement(sacr_tr_t1860, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 1860, 0.09).
narrative_ontology:measurement(sacr_tr_t1900, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 1900, 0.11).
narrative_ontology:measurement(sacr_tr_t1940, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 1940, 0.13).
narrative_ontology:measurement(sacr_tr_t1980, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 1980, 0.16).
narrative_ontology:measurement(sacr_tr_t2024, sacrifice_obligation_continuity__archival_preservation, theater_ratio, 2024, 0.18).

% Extraction over time
narrative_ontology:measurement(sacr_be_t1780, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 1780, 0.02).
narrative_ontology:measurement(sacr_be_t1820, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 1820, 0.03).
narrative_ontology:measurement(sacr_be_t1860, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 1860, 0.04).
narrative_ontology:measurement(sacr_be_t1900, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 1900, 0.05).
narrative_ontology:measurement(sacr_be_t1940, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 1940, 0.06).
narrative_ontology:measurement(sacr_be_t1980, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 1980, 0.04).
narrative_ontology:measurement(sacr_be_t2024, sacrifice_obligation_continuity__archival_preservation, base_extractiveness, 2024, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(sacrifice_obligation_continuity__archival_preservation, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sacrifice_obligation_continuity__archival_preservation, information_standard).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__study_as_performance).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__performance_only).
narrative_ontology:affects_constraint(sacrifice_obligation_continuity__archival_preservation, sacrifice_obligation_continuity__messianic_suspension).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'sacrifice law today' covers four structurally distinct claims about the status of the sacrificial precepts after the destruction of the Second Temple, each with its own epsilon, victim set, and classification. This member (archival_preservation) authors epsilon ~0.03 with an empty victim set; the obligation-bearing siblings author substantially higher epsilon with adherents as targets. Classical readings (study_as_performance, grounded in the Talmudic substitution tradition) are historically upstream; the archival reading is a modern downstream development that nonetheless supplies the textual infrastructure (critical editions, lexica, archaeology) on which the restorationist siblings rely — see omega archive_feeds_restoration_pipeline. All four files link one another via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
