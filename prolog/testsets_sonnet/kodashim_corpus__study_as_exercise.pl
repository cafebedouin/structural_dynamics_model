% ============================================================================
% CONSTRAINT STORY: kodashim_corpus__study_as_exercise
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: kodashim_corpus__study_as_exercise
 *   human_readable: Talmudic Study of Kodashim as Fulfillment of the Sacrificial Mitzvah
 *   domain: religious/legal/epistemic
 *
 * SUMMARY:
 *   This constraint models one specific reading of the Kodashim kernel — the
 *   corpus of Talmudic law governing Temple sacrifice, which has had no
 *   operative referent since 70 CE. The 'study_as_exercise' reading holds
 *   that continuous intellectual-spiritual engagement with the sacrificial
 *   law texts IS the complete performance of the mitzvah, not a substitute
 *   for it and not a mere memorial of it. This reading occupies the kernel
 *   actively through study rather than treating it as dormant (the
 *   performance_only reading, awaiting messianic restoration) or as
 *   archived/superseded (the substitution_archive reading, in which prayer
 *   displaced sacrifice entirely). Structurally, this reading produces
 *   negligible extraction: no one is deprived, no coercive apparatus enforces
 *   the doctrine, and the practice is voluntary scholarly and liturgical
 *   engagement whose 'output' is communal continuity, not rent.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_corpus__study_as_exercise, 0.03).
domain_priors:suppression_score(kodashim_corpus__study_as_exercise, 0.08).
domain_priors:theater_ratio(kodashim_corpus__study_as_exercise, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, extractiveness, 0.03).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(kodashim_corpus__study_as_exercise, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_corpus__study_as_exercise, rope).
narrative_ontology:human_readable(kodashim_corpus__study_as_exercise, "Talmudic Study of Kodashim as Fulfillment of the Sacrificial Mitzvah").
narrative_ontology:topic_domain(kodashim_corpus__study_as_exercise, "religious/legal/epistemic").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_corpus__study_as_exercise, '996eea4e-d898-4102-942d-385efb4aa887').
narrative_ontology:cs_kernel_codification('996eea4e-d898-4102-942d-385efb4aa887', fixed_text).
narrative_ontology:cs_authority_grounding('996eea4e-d898-4102-942d-385efb4aa887', lineage).
narrative_ontology:cs_interpretation_layer_present('996eea4e-d898-4102-942d-385efb4aa887').
narrative_ontology:cs_reading_relation('996eea4e-d898-4102-942d-385efb4aa887', kodashim_corpus__performance_only, forecloses).
narrative_ontology:cs_reading_relation('996eea4e-d898-4102-942d-385efb4aa887', kodashim_corpus__substitution_archive, coexists_with).
narrative_ontology:cs_axiom('996eea4e-d898-4102-942d-385efb4aa887', foundational, study_constitutes_complete_performance).
narrative_ontology:cs_axiom_status(study_constitutes_complete_performance, holdable).
narrative_ontology:cs_axiom_grounding('996eea4e-d898-4102-942d-385efb4aa887', study_constitutes_complete_performance, theological).
narrative_ontology:cs_axiom('996eea4e-d898-4102-942d-385efb4aa887', secondary, kernel_presently_occupied_not_dormant).
narrative_ontology:cs_axiom_status(kernel_presently_occupied_not_dormant, holdable).
narrative_ontology:cs_axiom_grounding('996eea4e-d898-4102-942d-385efb4aa887', kernel_presently_occupied_not_dormant, conventional).
narrative_ontology:cs_reference_frame('996eea4e-d898-4102-942d-385efb4aa887', talmudic_study_equivalence_precedent).
narrative_ontology:cs_drift_state('996eea4e-d898-4102-942d-385efb4aa887', post_temple_diaspora_consolidation, gap(stable, minor, true)).
narrative_ontology:cs_created_at('996eea4e-d898-4102-942d-385efb4aa887', '').
narrative_ontology:cs_kernel_id(kodashim_corpus__study_as_exercise, kodashim_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, yeshiva_scholars).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, rabbinic_academies).
narrative_ontology:constraint_beneficiary(kodashim_corpus__study_as_exercise, jewish_communal_continuity).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, torah_study_equivalence_doctrine).
narrative_ontology:constraint_vindicates(kodashim_corpus__study_as_exercise, oral_law_perpetual_validity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Study the tractates of Kodashim (Zevachim, Menachot, Chullin, and related sacrificial law) in daily study cycles, understanding the intellectual reconstruction of sacrificial procedure as itself discharging the mitzvah in the absence of the Temple. They set the interpretive agenda of what counts as adequate engagement, and the practice is entirely voluntary — no one compels them to study Kodashim rather than any other tractate, and leaving the study hall costs them standing within their community but nothing coercive is applied to keep them there.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, yeshiva_scholars, beneficiary,
    organized, civilizational, mobile, global).
narrative_ontology:stakeholder_secondary_role(kodashim_corpus__study_as_exercise, yeshiva_scholars, agenda_setter).

% Institutionalize the study-as-fulfillment doctrine into curricula, ordination requirements, and daily liturgical study cycles (daf yomi and similar programs). They gain communal cohesion and doctrinal continuity across the post-Temple diaspora, but they do not extract payment or coercive submission from anyone through this specific claim; institutions could adopt other curricular emphases without penalty.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, rabbinic_academies, beneficiary,
    institutional, civilizational, mobile, global).

% An abstract collective good — the maintenance of a coherent legal-religious tradition across two millennia without a functioning Temple. The study-as-exercise doctrine is one of the mechanisms credited with holding halakhic practice together in the absence of the thing it describes; it is not itself an actor and captures nothing.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, jewish_communal_continuity, beneficiary,
    analytical, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(kodashim_corpus__study_as_exercise, jewish_communal_continuity).

% Participate in communal life shaped by rabbinic scholarship but are not required to personally study Kodashim in depth to be considered observant; they benefit indirectly from the continuity the scholarly tradition sustains and can engage with the doctrine at whatever depth they choose without penalty for non-engagement.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, lay_practitioners, observer,
    moderate, biographical, mobile, regional).

% Study the doctrine comparatively — as an instance of textual practice substituting for ritual practice — without personal stake in its truth, documenting how the study-as-fulfillment claim functions structurally within rabbinic legal theory.
narrative_ontology:constraint_stakeholder(kodashim_corpus__study_as_exercise, religious_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kodashim_corpus__study_as_exercise, diffuse).
narrative_ontology:fixing_cost_class(kodashim_corpus__study_as_exercise, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared interpretive practice that lets a legal-religious community maintain continuous engagement with an entire body of law (sacrificial procedure) that has had no operative referent for roughly two thousand years, without requiring any physical infrastructure (Temple, altar, priesthood) to do so.
% TRANSFER_FUNCTION: Moves nothing extractively between parties; it redirects intellectual-spiritual labor (study time, exegetical effort) toward maintaining doctrinal and communal coherence. What is 'transferred' is continuity of tradition across generations, not a resource from a payer to a beneficiary.
% ABSENT_VOICES: No excluded party is structurally disadvantaged by this specific reading; it is a voluntary interpretive claim adopted or declined by choice. Adherents of the sibling readings (performance_only, substitution_archive) hold competing accounts of the same kernel, but they are not silenced — they publish, teach, and dispute openly within the same tradition.
% DISAPPEARANCE_RATIONALE: If study-as-fulfillment doctrine vanished, the daily study cycles built around Kodashim (daf yomi, yeshiva curricula emphasizing sacrificial tractates) would lose their theological rationale; institutions would likely reallocate study time toward tractates with more direct halakhic application, and a specific mechanism of post-Temple continuity would need to be replaced by another rationale (e.g., pure historical/legal interest) or abandoned.
% FOUNDING_PROBLEM: After the Temple's destruction in 70 CE, the sacrificial cultus that structured much of biblical and early rabbinic religious life had no physical referent; the rabbis needed a way to keep the corpus of sacrificial law religiously alive and legally binding rather than letting it become inert antiquarian material.
% FOUNDING_PROBLEM_CORROBORATION: Talmudic sources themselves (e.g., statements attributed to Rav in tractate Menachot equating study of the sacrificial order with its performance) attest the doctrine from within the tradition. Academic historians of religion and comparative legal scholars, writing from outside any confessional commitment to the doctrine's truth, corroborate that the practice functions as a continuity mechanism regardless of whether the underlying theological claim is accepted — this is an outside-the-beneficiary-set corroboration of function, not of doctrinal truth.
narrative_ontology:disappearance_verdict(kodashim_corpus__study_as_exercise, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_corpus__study_as_exercise, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_corpus__study_as_exercise, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kodashim_corpus__study_as_exercise, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_corpus__study_as_exercise, 0.03, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness is authored near zero (0.02-0.03) because no party pays a cost to another party through this doctrine's operation — it redistributes attention and study time within a voluntary practice, not resources from payer to beneficiary. Suppression is low (0.08) because non-adoption carries social rather than coercive cost; a scholar or community that rejects this specific doctrinal framing (in favor of a sibling reading) faces no material penalty. Theater ratio is low and stable across the interval (0.03-0.05) because the study activity is the substantive content of the practice, not a performative gloss over some other function — there is no 'real function' being masked here; study literally is the claimed function. Accessibility collapse is moderate (0.35), reflecting that once inside the rabbinic interpretive tradition, treating study as fulfillment becomes the dominant framing taught in most yeshiva curricula, though the sibling readings remain live and openly taught alternatives, so collapse is far from total.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setting scholarly seat, this reading is lived as genuine, complete religious fulfillment — coordination around a shared interpretive practice with civilizational stakes. From an analytical observer seat (comparative religion), the same structure reads as a functionally elegant continuity mechanism regardless of its theological truth-value. Both seats converge on low extraction and low suppression because the structural data (no beneficiary extracting from an identifiable victim, no active enforcement) does not change across seats — the seats differ in evaluative framing, not in the underlying structural facts the engine reads.
 *
 * DIRECTIONALITY LOGIC:
 *   Scholars and academies are declared beneficiaries because the doctrine directly enables and legitimates the activity they are already engaged in (Torah study), providing theological weight to their vocation; there is no victim group because no one's costs are increased by other parties adopting this reading. Lay practitioners and comparative scholars sit as observers with essentially symmetric or unaffected positions — the doctrine does not extract from them, coordinate against them, or exclude them from anything they sought access to.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is a clean case of NOT mandatrophy: the founding problem (keeping sacrificial law religiously alive without a Temple) is still actively addressed by the very mechanism under evaluation, and the mechanism's continued operation is precisely what keeps the founding problem from going dead. There is no zombie arrangement here — study-as-fulfillment is not a vestigial husk performing empty motions; the founding problem's status is genuinely contested only insofar as sibling readings (performance_only, substitution_archive) dispute whether the kernel is 'occupied' at all, not whether this reading's own internal logic has decayed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_occupation_vs_deferral,
    'Is the sacrificial-law kernel presently OCCUPIED (fulfilled through study, as this reading holds) or DORMANT (awaiting future restoration, as the performance_only reading holds)? These are mutually exclusive claims about the same textual corpus''s present operative status.',
    'No empirical resolution is available — this is a theological/doctrinal dispute internal to rabbinic tradition, resolvable only by appeal to which authorities and textual traditions a community follows (e.g., specific Talmudic dicta on study-equals-performance vs. messianic-restoration liturgy and halakhic codes that treat sacrificial law as suspended pending Temple rebuilding).',
    'If the performance_only reading is adopted instead, the same corpus of texts would be reclassified as a scaffold (a temporary holding pattern awaiting a sunset condition — Temple restoration) rather than a rope (complete, ongoing coordination). The extractiveness and beneficiary structure would likely remain low in both readings, but the classification type would diverge sharply.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_occupation_vs_deferral, conceptual, 'Whether the kernel is presently occupied through study or merely held in abeyance.').

omega_variable(
    study_vs_memorial_function,
    'Does intensive study of Kodashim constitute active occupation of the mitzvah''s kernel, or is it better described (per the substitution_archive reading) as documentation of a superseded practice, with prayer having taken over the kernel''s actual function?',
    'Comparative analysis of how practitioners and halakhic authorities themselves describe the phenomenology of study — do they report a sense of having ''performed'' something, or a sense of historical/legal documentation? Ethnographic and textual-critical work on lived rabbinic self-description could distinguish these, though the two framings are not mutually exclusive in practice and may simply be different theological emphases within the same behavior.',
    'If the substitution_archive reading is more descriptively accurate, this constraint''s claimed beneficiary structure (scholars maintaining cosmic order) would need to be reframed as scholars maintaining historical memory — a materially different coordination function, though likely still low-extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(study_vs_memorial_function, conceptual, 'Whether study functions as live performance or as archival memorial.').

omega_variable(
    voluntary_curricular_pressure,
    'Is the near-total dominance of the study-as-fulfillment framing within major yeshiva curricula truly voluntary, or does institutional path-dependency (established curricula, ordination requirements) create soft pressure that approaches suppression for scholars who would prefer to emphasize sibling readings?',
    'Survey of curricular flexibility across denominations and yeshivot; examination of whether dissenting theological framings face career or institutional consequences within rabbinic academies.',
    'If institutional pressure is substantial, the authored suppression value (0.08) may understate true suppression, and part of what looks like voluntary doctrinal consensus may be institutionally reinforced rather than freely chosen.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(voluntary_curricular_pressure, empirical, 'Whether curricular dominance of this reading reflects genuine consensus or soft institutional coercion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_corpus__study_as_exercise, 0, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(koda_tr_t0, kodashim_corpus__study_as_exercise, theater_ratio, 0, 0.03).
narrative_ontology:measurement(koda_tr_t400, kodashim_corpus__study_as_exercise, theater_ratio, 400, 0.04).
narrative_ontology:measurement(koda_tr_t800, kodashim_corpus__study_as_exercise, theater_ratio, 800, 0.04).
narrative_ontology:measurement(koda_tr_t1200, kodashim_corpus__study_as_exercise, theater_ratio, 1200, 0.05).
narrative_ontology:measurement(koda_tr_t1600, kodashim_corpus__study_as_exercise, theater_ratio, 1600, 0.05).
narrative_ontology:measurement(koda_tr_t1950, kodashim_corpus__study_as_exercise, theater_ratio, 1950, 0.05).

% Extraction over time
narrative_ontology:measurement(koda_be_t0, kodashim_corpus__study_as_exercise, base_extractiveness, 0, 0.02).
narrative_ontology:measurement(koda_be_t400, kodashim_corpus__study_as_exercise, base_extractiveness, 400, 0.02).
narrative_ontology:measurement(koda_be_t800, kodashim_corpus__study_as_exercise, base_extractiveness, 800, 0.03).
narrative_ontology:measurement(koda_be_t1200, kodashim_corpus__study_as_exercise, base_extractiveness, 1200, 0.03).
narrative_ontology:measurement(koda_be_t1600, kodashim_corpus__study_as_exercise, base_extractiveness, 1600, 0.03).
narrative_ontology:measurement(koda_be_t1950, kodashim_corpus__study_as_exercise, base_extractiveness, 1950, 0.03).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kodashim_corpus__study_as_exercise, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_corpus__study_as_exercise, identity_coordination).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__performance_only).
narrative_ontology:affects_constraint(kodashim_corpus__study_as_exercise, kodashim_corpus__substitution_archive).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the kodashim_corpus kernel (study_as_exercise, performance_only, substitution_archive), each instantiated as a separate constraint story per the ε-invariance principle — the three readings assign structurally different beneficiary/victim sets, different sunset conditions, and different classifications to the same underlying textual corpus, and cannot be averaged into one ε. study_as_exercise claims zero deferral and zero supersession, producing the lowest extraction and a rope classification; performance_only treats the same corpus as a scaffold awaiting a sunset (messianic restoration); substitution_archive treats it as a closed memorial record. All three are linked bidirectionally via affects_constraints since a shift in communal adoption of one reading directly changes the resource and legitimacy environment for the others (e.g., growth of daf yomi programs under the study_as_exercise reading measurably affects institutional attention available to performance_only-oriented messianic-restoration curricula).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
