% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__performance_only
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_corpus__performance_only, []).

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
 *   constraint_id: kodashim_corpus__performance_only
 *   human_readable: Kodashim Corpus as Archived Blueprint Awaiting Messianic Restoration
 *   domain: religious/rabbinic_judaism/commitment_system
 *
 * SUMMARY:
 *   The Kodashim corpus (the 'Holy Things' order of the Mishnah and Talmud)
 *   details the laws of Temple sacrifice with exhaustive precision. The
 *   performance_only reading — dominant in the Lithuanian yeshiva world and
 *   its derivatives — treats this corpus not as historical record or memorial
 *   but as an archived blueprint: the sacrificial system is suspended, not
 *   superseded, and its laws await literal reactivation when the Messiah
 *   comes and the Temple is rebuilt. This framing extracts legitimacy from a
 *   future state that cannot be verified, falsified, or influenced by present
 *   action. The extraction is real: students devote prime learning years to
 *   inoperative tractates; institutions raise funds for 'Temple preparation';
 *   laypeople direct devotion toward an unrealizable horizon. The
 *   coordination function (maintaining messianic orientation and
 *   institutional distinctiveness) is genuine but entirely subordinated to
 *   the extraction: the same coordination could be achieved by the
 *   study_as_exercise or substitution_archive readings without the
 *   misallocation. The constraint persists through active enforcement —
 *   curricular mandates, social pressure, exclusion of alternative framings —
 *   not through participant preference.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__performance_only, 0.78).
domain_priors:suppression_score(kodashim_corpus__performance_only, 0.65).
domain_priors:theater_ratio(kodashim_corpus__performance_only, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, extractiveness, 0.78).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, accessibility_collapse, 0.83).
narrative_ontology:constraint_metric(kodashim_corpus__performance_only, resistance, 0.28).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__performance_only, snare).
narrative_ontology:human_readable(kodashim_corpus__performance_only, "Kodashim Corpus as Archived Blueprint Awaiting Messianic Restoration").
narrative_ontology:topic_domain(kodashim_corpus__performance_only, "religious/rabbinic_judaism/commitment_system").

domain_priors:requires_active_enforcement(kodashim_corpus__performance_only).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__performance_only, '5c59cad6-c49f-4a4e-90e8-dd20a2f8578c').
narrative_ontology:cs_kernel_codification('5c59cad6-c49f-4a4e-90e8-dd20a2f8578c', formalized).
narrative_ontology:cs_authority_grounding('5c59cad6-c49f-4a4e-90e8-dd20a2f8578c', lineage).
narrative_ontology:cs_interpretation_layer_present('5c59cad6-c49f-4a4e-90e8-dd20a2f8578c').
narrative_ontology:cs_reading_relation('5c59cad6-c49f-4a4e-90e8-dd20a2f8578c', kodashim_corpus__study_as_exercise, coexists_with).
narrative_ontology:cs_reading_relation('5c59cad6-c49f-4a4e-90e8-dd20a2f8578c', kodashim_corpus__substitution_archive, forecloses).
narrative_ontology:cs_axiom('5c59cad6-c49f-4a4e-90e8-dd20a2f8578c', foundational, physical_sacrifice_irreplaceable).
narrative_ontology:cs_axiom_status(physical_sacrifice_irreplaceable, holdable).
narrative_ontology:cs_axiom_grounding('5c59cad6-c49f-4a4e-90e8-dd20a2f8578c', physical_sacrifice_irreplaceable, deontological).
narrative_ontology:cs_axiom('5c59cad6-c49f-4a4e-90e8-dd20a2f8578c', secondary, study_preserves_restoration_readiness).
narrative_ontology:cs_axiom_status(study_preserves_restoration_readiness, holdable).
narrative_ontology:cs_axiom_grounding('5c59cad6-c49f-4a4e-90e8-dd20a2f8578c', study_preserves_restoration_readiness, instrumental).
narrative_ontology:cs_reference_frame('5c59cad6-c49f-4a4e-90e8-dd20a2f8578c', temple_restoration_expectation).
narrative_ontology:cs_drift_state('5c59cad6-c49f-4a4e-90e8-dd20a2f8578c', post_temple_destruction, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5c59cad6-c49f-4a4e-90e8-dd20a2f8578c', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__performance_only, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, messianic_preparation_institutions).
narrative_ontology:constraint_beneficiary(kodashim_corpus__performance_only, yeshiva_authorities).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, devoted_students_treating_archive_as_living_practice).
narrative_ontology:constraint_victim(kodashim_corpus__performance_only, lay_practitioners_misallocating_devotion).
narrative_ontology:constraint_vindicates(kodashim_corpus__performance_only, messianic_restoration_necessity).
narrative_ontology:constraint_vindicates(kodashim_corpus__performance_only, sacrifice_law_eternal_validity).
narrative_ontology:constraint_vindicates(kodashim_corpus__performance_only, oral_torah_preserves_sacrificial_blueprint).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the curriculum, ordination, and institutional infrastructure that frames Kodashim study as preparation for Temple restoration. They allocate resources to sacrificial research institutes, publish commentary literature, and set the terms of what counts as legitimate engagement with the corpus. Their authority and funding depend on the unrestored Temple remaining the organizing horizon.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, messianic_preparation_institutions, agenda_setter,
    institutional, generational, arbitrage, global).

% Derive institutional prestige, donor support, and student recruitment from maintaining the most 'authentic' curriculum — one that includes the technically demanding, practically inoperative sacrificial tractates. They benefit from the perception that their curriculum preserves the complete Torah, even the parts that cannot be practiced. Exit means curricular reform that would undermine their distinctive claim to authenticity.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, yeshiva_authorities, beneficiary,
    organized, biographical, constrained, global).

% Invest years of full-time study mastering tractates (Zevachim, Menachot, Tamid, Middot, Kinnim) whose practical application is structurally impossible. They internalize the framing that this study 'counts as' sacrificial service or prepares for its resumption. Their professional identity, marriage prospects, and communal standing are fused to this course of study. Exit would mean admitting their central religious investment was misallocated — a psychological and social rupture few can survive.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, devoted_students_treating_archive_as_living_practice, payer,
    moderate, biographical, identity_locked, global).

% Direct charitable giving, communal respect, and personal devotional energy toward institutions and individuals framed as 'doing the work of the Temple.' They lack the textual literacy to evaluate the claim that Kodashim study substitutes for sacrifice, and the communal infrastructure offers no alternative framework for sacrificial devotion. Exit requires leaving the community or accepting marginal status within it.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, lay_practitioners_misallocating_devotion, payer,
    powerless, immediate, trapped, local).

% Academic talmudists and historians of rabbinic Judaism who read Kodashim as a rabbinic construction responding to Temple loss, not a preserved blueprint. They would object that the performance_only framing obscures the creative, adaptive nature of the rabbinic project. Their voices are excluded from the yeshiva world's internal discourse and from communal decision-making about curriculum and resource allocation.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, critical_scholars, excluded,
    analytical, generational, analytical, global).

% Study the corpus philologically, historically, and comparatively without stake in its theological framing. They document the textual history, the development of sacrificial law in the Mishnah and Talmud, and the reception history. Their analysis illuminates the constraint's construction but carries no weight in the institutional arena where the constraint operates.
narrative_ontology:constraint_stakeholder(kodashim_corpus__performance_only, academic_talmudists, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates messianic expectation and institutional continuity around an unrealizable practice by presenting the archived sacrificial law as a living blueprint whose study maintains Israel's readiness for restoration.
% TRANSFER_FUNCTION: Moves devotion, study-time, charitable resources, and communal legitimacy from practitioners (students and laypeople) to messianic-preparation institutions and yeshiva authorities, who control the curriculum and the framing of what constitutes authentic Torah study.
% ABSENT_VOICES: The historical priesthood and Temple-era Jews who actually performed the sacrifices — they would testify that study is not performance. Also absent: Jews who reject the messianic framework entirely (secular, Reform, Reconstructionist) and would redirect the resources to practicable mitzvot or social action. Both groups are structurally excluded: the first by history, the second by the communal boundary that defines 'authentic' Judaism as messianically oriented.
% DISAPPEARANCE_RATIONALE: If the performance_only framing vanished overnight, messianic-preparation institutions would lose their core legitimating narrative and primary fundraising hook. Yeshiva curricula would face pressure to reallocate thousands of study-hours from inoperative tractates to practicable halacha. Students would confront the sunk cost of their specialization. The communal economy of sacrificial devotion — donations for 'Temple vessels,' 'red heifer research,' 'priestly training' — would collapse or redirect.
% FOUNDING_PROBLEM: How to maintain sacrificial law's authority and centrality after the Temple's destruction (70 CE) without admitting that the core of Torah practice had been rendered impossible — and without conceding that the rabbinic project was innovation rather than preservation.
% FOUNDING_PROBLEM_CORROBORATION: Historical scholarship (Neusner, Boyarin, Klawans) demonstrates the rabbinic corpus was a creative response to catastrophe, not a transmitted blueprint. Archaeological evidence confirms no Temple reconstruction for 1950+ years. Traditional sources themselves acknowledge the loss: 'Since the Temple was destroyed, we have neither altar nor priest nor sacrifice' (Taanit 29a); 'Whoever studies the laws of the burnt offering is as if he offered a burnt offering' (Menachot 110a) — the 'as if' marks the substitution, not the identity. No corroborating voice outside the beneficiary institutions treats the restoration prospect as imminent or the archive as a functional blueprint.
narrative_ontology:disappearance_verdict(kodashim_corpus__performance_only, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__performance_only, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__performance_only, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_corpus__performance_only, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__performance_only, 0.78, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_corpus__performance_only_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(kodashim_corpus__performance_only, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(kodashim_corpus__performance_only_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78) is high because the constraint moves substantial resources (time, money, devotion, legitimacy) from practitioners to institutions based on a claim about an unrealizable future. Suppression (0.65) is substantial: alternative framings are excluded from yeshiva curricula, communal discourse, and funding streams; the 'as if' formulation in Talmud is reinterpreted to erase the substitution. Theater ratio (0.72) is high because the performative maintenance of sacrificial study — including institutes that manufacture 'Temple vessels' to exact specifications never used — far exceeds any functional coordination need. Accessibility collapse (0.83) is severe: once a student accepts the performance_only framing, practicable alternatives (study_as_exercise, substitution_archive) appear as heresy or capitulation. Resistance (0.28) is low because the constraint operates through identity fusion and communal belonging, not overt coercion — the cost of exit is social and existential, not physical.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat, the constraint is genuine coordination: it preserves the complete Torah, maintains messianic hope, and structures a distinctive communal identity. From the payer seats, the same structure operates as extraction: their devotion is harvested for an unrealizable promise, their alternatives are suppressed, their exit is blocked by identity fusion. The engine computes this divergence from the structural data — the authored claim (snare) reflects the payer-seat reality, not the agenda-setter's self-understanding.
 *
 * DIRECTIONALITY LOGIC:
 *   Messianic-preparation institutions and yeshiva authorities are structural beneficiaries (d near 0.0): they control the framing, collect the resources, and face arbitrage-grade exit (they could reform but choose not to). Devoted students are identity-locked targets (d near 0.9): their professional and marital futures are fused to the curriculum; exit means identity rupture. Lay practitioners are trapped (d = 1.0): they lack textual literacy to evaluate the claim and face communal exclusion if they dissent. Critical scholars are excluded (analytical seat): their structural position is outside the constraint's enforcement reach. Academic talmudists are observers: they analyze without stake.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (maintaining sacrificial authority post-destruction) is dead: the Temple is not rebuilt, no priesthood exists, no restoration is imminent. The arrangement persists because it now serves a different function — legitimating institutional authority and extracting resources — while retaining the original framing. This is classic mandatrophy: the mandate has atrophied but the constraint remains, reclassified by the engine as snare (extraction from unrealizable future) rather than the scaffold it may have been in the first centuries (transitional coordination with a sunset that never came).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    archive_vs_blueprint,
    'Does the Kodashim corpus genuinely preserve a restorable sacrificial practice, or does it construct an unfalsifiable future that legitimates present extraction?',
    'Compare the corpus''s technical specifications against archaeological evidence of Second Temple practice; assess whether the rabbinic system could function as a ''blueprint'' without living priestly tradition. If the corpus contains innovations with no Temple precedent (e.g., the entire system of menachot meal-offerings as codified), the blueprint claim is falsified.',
    'If the corpus is rabbinic construction not Temple preservation, the performance_only framing is a snare extracting devotion for a fiction. If it genuinely preserves Temple practice, the extraction may be the cost of maintaining a restorable tradition — though the unrealizable horizon remains extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(archive_vs_blueprint, empirical, 'Whether the archive preserves a restorable practice or constructs an unfalsifiable future.').

omega_variable(
    devotion_displacement,
    'Does Kodashim study under the performance_only framing displace devotion that would otherwise go to practicable mitzvot (chesed, tzedakah, interpersonal ethics, environmental stewardship)?',
    'Comparative time-allocation studies in yeshiva vs. non-yeshiva Orthodox communities; analysis of charitable giving patterns correlated with curricular emphasis; ethnography of student decision-making about learning priorities.',
    'If displacement is documented, the extraction is not just misallocation within Torah study but active diversion from actionable religious obligation — strengthening the snare classification. If no displacement (students would not otherwise do those mitzvot), the extraction is narrower.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(devotion_displacement, empirical, 'Whether the constraint diverts devotion from practicable mitzvot.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression of alternative framings structural (curricular mandates, funding gates, social ostracism) or internalized (students genuinely believe alternatives are heretical)?',
    'Longitudinal study of students exposed to alternative framings (e.g., in academic settings): do they reject alternatives from conviction or from fear? Post-exit trajectories of those who leave the yeshiva world: does the suppression persist internally?',
    'If primarily internalized, the constraint''s effective suppression is higher than structural measures suggest — the target carries the suppression after exit. If primarily structural, suppression would decay rapidly if enforcement relaxed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, conceptual, 'Structural vs. internalized suppression mechanism in the yeshiva world.').

omega_variable(
    kernel_reading_contestation,
    'Is the performance_only reading a genuine theological commitment or an institutional strategy to maintain curricular distinctiveness and funding?',
    'Analyze institutional decision-making: when curricular reform is proposed (e.g., reducing Kodashim hours for practical halacha), what arguments prevail? Track funding flows: do donors give more for ''Temple preparation'' branding than for general Torah study?',
    'If institutional strategy, the constraint is a snare with full intentionality. If genuine commitment, it is a snare sustained by sincere but unfalsifiable belief — the extraction is real regardless of intent.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contestation, preference, 'Whether the reading is sincere theology or institutional strategy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__performance_only, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_perf_tr_t0, kodashim_corpus__performance_only, theater_ratio, 0, 0.35).
narrative_ontology:measurement(kodashim_perf_tr_t50, kodashim_corpus__performance_only, theater_ratio, 50, 0.45).
narrative_ontology:measurement(kodashim_perf_tr_t100, kodashim_corpus__performance_only, theater_ratio, 100, 0.55).
narrative_ontology:measurement(kodashim_perf_tr_t150, kodashim_corpus__performance_only, theater_ratio, 150, 0.65).
narrative_ontology:measurement(kodashim_perf_tr_t200, kodashim_corpus__performance_only, theater_ratio, 200, 0.72).

% Extraction over time
narrative_ontology:measurement(kodashim_perf_be_t0, kodashim_corpus__performance_only, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(kodashim_perf_be_t50, kodashim_corpus__performance_only, base_extractiveness, 50, 0.52).
narrative_ontology:measurement(kodashim_perf_be_t100, kodashim_corpus__performance_only, base_extractiveness, 100, 0.61).
narrative_ontology:measurement(kodashim_perf_be_t150, kodashim_corpus__performance_only, base_extractiveness, 150, 0.7).
narrative_ontology:measurement(kodashim_perf_be_t200, kodashim_corpus__performance_only, base_extractiveness, 200, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_perf_su_t0, kodashim_corpus__performance_only, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(kodashim_perf_su_t50, kodashim_corpus__performance_only, suppression_requirement, 50, 0.48).
narrative_ontology:measurement(kodashim_perf_su_t100, kodashim_corpus__performance_only, suppression_requirement, 100, 0.55).
narrative_ontology:measurement(kodashim_perf_su_t150, kodashim_corpus__performance_only, suppression_requirement, 150, 0.6).
narrative_ontology:measurement(kodashim_perf_su_t200, kodashim_corpus__performance_only, suppression_requirement, 200, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__performance_only, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_corpus__performance_only, 0.08).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, daily_prayer_liturgy).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, temple_mount_activism).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, priestly_lineage_claims).
narrative_ontology:affects_constraint(kodashim_corpus__performance_only, red_heifer_research).

% DUAL FORMULATION NOTE:
% The kodashim_corpus kernel decomposes into three constraint stories: performance_only (this file, snare), study_as_exercise (rope/tangled_rope — study as genuine coordination), and substitution_archive (mountain/rope — historical fact with low extraction). The performance_only reading extracts from the unrealizable future; study_as_exercise coordinates present practice; substitution_archive memorializes the past. Their ε values differ by wide margins (0.78 vs ~0.2 vs ~0.05). Linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_corpus__performance_only, organized, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
