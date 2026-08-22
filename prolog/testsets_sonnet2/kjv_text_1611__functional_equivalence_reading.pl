% ============================================================================
% CONSTRAINT STORY: kjv_text_1611__functional_equivalence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_text_1611__functional_equivalence_reading, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: kjv_text_1611__functional_equivalence_reading
 *   human_readable: Functional Equivalence Reading of the KJV Kernel (Complementary Translation Pluralism)
 *   domain: religious_studies/textual_criticism/theology
 *
 * SUMMARY:
 *   This story authors the functional-equivalence reading of the KJV kernel:
 *   the position, held widely among mainstream Protestant and academic
 *   communities, that the KJV and modern translations serve complementary
 *   rather than competing purposes — the KJV valued for literary cadence,
 *   historical influence, and liturgical continuity, modern translations
 *   valued for readability and closer alignment with the current manuscript
 *   record. This is one reading among three of a single contested kernel (the
 *   1611 King James Bible's status). It is generated as a clean,
 *   self-contained constraint per the ε-invariance principle: it does not
 *   describe or average over the exclusive-inspiration reading (which holds
 *   the KJV alone is inspired and inerrant) or the revisable-translation
 *   reading (which holds the KJV is an improvable historical artifact subject
 *   to correction). Those are separate constraints in the same kernel family,
 *   linked via network.affects_constraints. From this reading's own lights,
 *   extractiveness is low: no single text or committee holds gatekeeping
 *   power over legitimate scripture access, so the coordination story is not
 *   cover for extraction — it is closer to genuinely low-coercion
 *   coordination, though not zero, because publishers still exercise ordinary
 *   market power and denominational culture still exerts soft pressure at the
 *   congregation level.
 *
 * KEY AGENTS:
 *   - lay_readers_seeking_clarity: primary beneficiary of translation choice (powerless/mobile) — gains comprehension without doctrinal cost
 *   - literary_and_historical_scholars: beneficiary who draws value from both texts for different purposes (moderate/arbitrage)
 *   - translation_publishers: agenda-setting beneficiary who profits from a plural translation market (organized/mobile)
 *   - liturgical_traditions_using_kjv: beneficiary retaining KJV use without asserting exclusivity (organized/mobile)
 *   - kjv_onlyist_congregations: excluded voice whose exclusive-inspiration claim this reading structurally forecloses (moderate/identity_locked)
 *   - textual_critics_and_manuscript_scholars: analytical observer underwriting the plausibility of pluralism (institutional/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_text_1611__functional_equivalence_reading, 0.22).
domain_priors:suppression_score(kjv_text_1611__functional_equivalence_reading, 0.15).
domain_priors:theater_ratio(kjv_text_1611__functional_equivalence_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, accessibility_collapse, 0.25).
narrative_ontology:constraint_metric(kjv_text_1611__functional_equivalence_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_text_1611__functional_equivalence_reading, rope).
narrative_ontology:human_readable(kjv_text_1611__functional_equivalence_reading, "Functional Equivalence Reading of the KJV Kernel (Complementary Translation Pluralism)").
narrative_ontology:topic_domain(kjv_text_1611__functional_equivalence_reading, "religious_studies/textual_criticism/theology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kjv_text_1611__functional_equivalence_reading, 'd55e891f-516e-4dd3-b02c-ce9ad9061a82').
narrative_ontology:cs_kernel_codification('d55e891f-516e-4dd3-b02c-ce9ad9061a82', fixed_text).
narrative_ontology:cs_authority_grounding('d55e891f-516e-4dd3-b02c-ce9ad9061a82', distributed).
narrative_ontology:cs_reading_relation('d55e891f-516e-4dd3-b02c-ce9ad9061a82', kjv_text_1611__exclusive_inspiration_reading, forecloses).
narrative_ontology:cs_reading_relation('d55e891f-516e-4dd3-b02c-ce9ad9061a82', kjv_text_1611__revisable_translation_reading, influences).
narrative_ontology:cs_axiom('d55e891f-516e-4dd3-b02c-ce9ad9061a82', foundational, translation_purposes_are_complementary_not_competing).
narrative_ontology:cs_axiom_status(translation_purposes_are_complementary_not_competing, holdable).
narrative_ontology:cs_axiom_grounding('d55e891f-516e-4dd3-b02c-ce9ad9061a82', translation_purposes_are_complementary_not_competing, conventional).
narrative_ontology:cs_axiom('d55e891f-516e-4dd3-b02c-ce9ad9061a82', foundational, no_single_english_text_holds_exclusive_gatekeeping_authority).
narrative_ontology:cs_axiom_status(no_single_english_text_holds_exclusive_gatekeeping_authority, holdable).
narrative_ontology:cs_axiom_grounding('d55e891f-516e-4dd3-b02c-ce9ad9061a82', no_single_english_text_holds_exclusive_gatekeeping_authority, conventional).
narrative_ontology:cs_reference_frame('d55e891f-516e-4dd3-b02c-ce9ad9061a82', single_authoritative_english_bible_tradition).
narrative_ontology:cs_drift_state('d55e891f-516e-4dd3-b02c-ce9ad9061a82', contemporary_translation_market, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('d55e891f-516e-4dd3-b02c-ce9ad9061a82', '').
narrative_ontology:cs_kernel_id(kjv_text_1611__functional_equivalence_reading, kjv_text_1611).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, lay_readers_seeking_clarity).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, literary_and_historical_scholars).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, translation_publishers).
narrative_ontology:constraint_beneficiary(kjv_text_1611__functional_equivalence_reading, liturgical_traditions_using_kjv).
narrative_ontology:constraint_vindicates(kjv_text_1611__functional_equivalence_reading, translation_pluralism_doctrine).
narrative_ontology:constraint_vindicates(kjv_text_1611__functional_equivalence_reading, complementary_purpose_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Reads scripture for devotional or study purposes and can freely choose among NIV, ESV, NRSV, NLT, or KJV depending on which serves comprehension or worship style best. Faces no gatekeeping cost to switching translations mid-study.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, lay_readers_seeking_clarity, beneficiary,
    powerless, biographical, mobile, global).

% Uses the KJV for its influence on English literature, its historical role in Anglophone culture, and its liturgical cadence, while using modern critical translations for exegesis and manuscript-based accuracy. Draws on each text for what it is structurally good at rather than treating either as the sole authority.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, literary_and_historical_scholars, beneficiary,
    moderate, generational, arbitrage, global).

% Produces and markets a range of translations to distinct market segments (readability, formal equivalence, study editions), competing on translation philosophy rather than on claims of exclusive inspiration. Benefits commercially from a marketplace of coexisting texts.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, translation_publishers, beneficiary,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(kjv_text_1611__functional_equivalence_reading, translation_publishers, agenda_setter).

% Continues using the KJV in worship for its cadence and continuity with tradition, while not disputing the validity of congregations that use modern translations. Retains the KJV as a live liturgical text without asserting it displaces others.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, liturgical_traditions_using_kjv, beneficiary,
    organized, generational, mobile, national).

% Holds that the KJV alone is the inspired English text and regards this pluralist reading as a doctrinal compromise. Their objection — that complementary-purpose framing quietly demotes the KJV from sole authority to one option among several — is not resolved inside this reading; it is simply a different reading that this one does not adjudicate.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, kjv_onlyist_congregations, excluded,
    moderate, generational, identity_locked, national).

% Evaluates translations against the manuscript record (Textus Receptus vs. critical text traditions) and can speak to each translation's textual basis without needing to rank one as exclusively authoritative. Their expertise underwrites the plausibility of a complementary-purposes framing but does not itself adjudicate between kernel readings.
narrative_ontology:constraint_stakeholder(kjv_text_1611__functional_equivalence_reading, textual_critics_and_manuscript_scholars, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows a large, doctrinally diverse readership to use whichever English translation best serves its purpose — devotional clarity, historical/literary study, or liturgical continuity — without requiring a single arbitrating text, avoiding the coordination costs of forcing one translation's authority onto every use case.
% TRANSFER_FUNCTION: Moves interpretive authority away from any single translation committee or textual tradition and distributes it across readers, denominations, and scholarly communities; no party collects rents from exclusive textual gatekeeping, though publishers capture ordinary commercial value from serving differentiated market segments.
% ABSENT_VOICES: KJV-onlyist congregations, who hold that treating the KJV as merely 'one valuable option among several' already forecloses their core claim of exclusive inspiration — from their seat, this reading is not neutral pluralism but a rival doctrinal position dressed as description.
% DISAPPEARANCE_RATIONALE: If this pluralist framing vanished, most lay readers, scholars, and publishers would likely reconstitute something like it in practice, since it largely describes an already-existing market and academic reality rather than enforcing one; but KJV-only communities would regard its disappearance as removing a legitimizing veneer over what they see as scriptural corruption, so whether the world 'rearranges' depends on which party is asked.
% FOUNDING_PROBLEM: By the 20th century, advances in manuscript discovery (Dead Sea Scrolls, papyri), comparative linguistics, and changes in English usage made the KJV's 1611 language increasingly opaque to ordinary readers while newer translations lacked its literary and liturgical standing — the functional-equivalence reading was built to let both serve without requiring readers to declare one the sole legitimate Bible.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated by textual critics and manuscript scholars outside any denomination's beneficiary interest, whose comparative work on translation philosophy and manuscript traditions is cited by publishers and educators alike; also implicitly corroborated by the continued commercial and devotional coexistence of multiple translations across denominations that do not share a single doctrinal stake in the outcome.
narrative_ontology:disappearance_verdict(kjv_text_1611__functional_equivalence_reading, contested).
narrative_ontology:founding_problem_status(kjv_text_1611__functional_equivalence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kjv_text_1611__functional_equivalence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet2', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(kjv_text_1611__functional_equivalence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(kjv_text_1611__functional_equivalence_reading, 0.22, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_text_1611__functional_equivalence_reading_tests).
:- end_tests(kjv_text_1611__functional_equivalence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.22) and falling slightly over the interval because this reading describes a genuinely decentralized arrangement: no translation committee, denomination, or publisher captures rents by virtue of textual exclusivity, and readers face negligible switching costs between translations. Suppression is low (0.15) because exit into another translation is trivially available and increasingly normalized. Theater ratio is modest (0.2) reflecting that some denominational rhetoric about 'the beauty and majesty of the KJV' functions partly as brand loyalty rather than substantive claim, but this is a minor component, not the dominant function. Accessibility collapse is low (0.25) — the whole point of this reading is that alternatives remain fully live and simultaneously available, the opposite of collapse. Resistance is low (0.2): the main friction this reading meets is not organized counter-mobilization but principled theological objection from KJV-only communities, who are better modeled as excluded voices than as resistors within this reading's own structure.
 *
 * PERSPECTIVAL GAP:
 *   From inside this reading, all named seats compute close to rope: the arrangement is low-suppression, low-extraction coordination. The divergence that matters is not between seats within this reading, but between this reading and its siblings — the exclusive-inspiration reading would compute the entire premise of 'complementary translations' as itself the extraction (a dilution of the one true text's authority), while the revisable-translation reading would compute this reading as insufficiently committed to textual correction. Those computations belong to the sibling constraint files, not to this one.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (lay readers, scholars, publishers, liturgical traditions using KJV) sit near the low-d end: the arrangement subsidizes their access to whichever text serves their purpose, at negligible cost. There are no declared victims in this reading because no party is structurally worse off under complementary pluralism than under a monopolized-authority alternative — the closest thing to a disadvantaged party, KJV-onlyist congregations, is modeled as excluded rather than victimized, since their loss is doctrinal standing, not material extraction. This is why the story carries no victims array and is not eligible for tangled_rope or snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (readers needing both clarity and connection to the historical/literary Bible tradition) remains live rather than resolved-and-abandoned, so this is not a case of a mandate outliving its function; the coordination structure continues to do real work matching translation philosophy to reader need. Classifying this as rope rather than tangled_rope or snare prevents mislabeling a genuinely low-coercion, exit-rich arrangement as extractive merely because publishers earn ordinary commercial revenue from it — commercial benefit alone, absent captured exit or suppressed alternatives, does not make a coordination arrangement extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    pluralism_as_neutral_description_or_rival_doctrine,
    'Is the functional-equivalence reading a neutral, ecumenical description of how English Bible translations are actually used, or is it itself a substantive doctrinal position (soft anti-exclusivism) that KJV-onlyist communities are right to read as foreclosing their claim?',
    'Survey denominational statements and seminary curricula over time to see whether ''complementary purposes'' language is presented as descriptive consensus or as an implicit argument against exclusive-inspiration claims; track whether KJV-only communities'' objections are engaged as doctrine-vs-doctrine or dismissed as fringe.',
    'If it functions as a live doctrinal argument rather than neutral description, the excluded_voices framing understates its structural weight — it would be more accurate to model it as one contesting party among several rather than a background consensus.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pluralism_as_neutral_description_or_rival_doctrine, conceptual, 'Whether this reading is neutral description or a doctrinal position that structurally competes with the exclusive-inspiration reading.').

omega_variable(
    commercial_pluralism_extraction_ceiling,
    'As translation publishing has become a larger commercial market (study Bibles, branded editions, copyright-restricted paraphrase translations), does the low-extraction character of this reading hold, or is a genuine coordination function increasingly overlaid with brand-driven product proliferation that adds cost without adding clarity?',
    'Compare growth in number of commercially distinct English translations against measurable gains in reader comprehension or textual accuracy; assess whether newer translations are differentiated by genuine linguistic/scholarly advance or by marketing segmentation.',
    'If commercial proliferation outpaces genuine functional differentiation, the extractiveness value authored here (0.22, falling) may be understating a slow drift toward publisher-driven extraction dressed as reader choice — this would be the mountain-extraction-accumulation pattern in miniature, worth tracking via future measurements.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commercial_pluralism_extraction_ceiling, empirical, 'Whether commercial translation proliferation is diluting the genuine coordination function this reading claims.').

omega_variable(
    kernel_framing_under_determination,
    'Is ''the KJV kernel'' best framed as a single stabilized text under contested interpretation (as authored here, with three sibling readings), or does the more fundamental kernel sit one level up — at the question of what counts as a legitimate criterion for scriptural authority at all (inspiration-of-a-specific-text vs. inspiration-of-an-underlying-message)? Under the higher framing, this reading and the revisable-translation reading might collapse into the same commitment (message-level inspiration), leaving only a two-reading kernel rather than three.',
    'Trace whether adherents of the functional-equivalence and revisable-translation readings, when pressed, converge on an underlying inspiration-of-message commitment distinct from the exclusive-inspiration reading''s inspiration-of-text commitment; if they converge, the kernel is better modeled as binary.',
    'A two-reading kernel would eliminate the need for this constraint as a separately authored file and would substantially change the reading_relations declared below (functional_equivalence and revisable_translation would need a coexists_with-to-identical edge rather than the current influences relation).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_under_determination, conceptual, 'Alternative framing of the kernel boundary itself: text-level vs. message-level inspiration as the real fault line.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_text_1611__functional_equivalence_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv__tr_t0, kjv_text_1611__functional_equivalence_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(kjv__tr_t10, kjv_text_1611__functional_equivalence_reading, theater_ratio, 10, 0.24).
narrative_ontology:measurement(kjv__tr_t20, kjv_text_1611__functional_equivalence_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(kjv__tr_t30, kjv_text_1611__functional_equivalence_reading, theater_ratio, 30, 0.21).
narrative_ontology:measurement(kjv__tr_t45, kjv_text_1611__functional_equivalence_reading, theater_ratio, 45, 0.2).
narrative_ontology:measurement(kjv__tr_t60, kjv_text_1611__functional_equivalence_reading, theater_ratio, 60, 0.2).

% Extraction over time
narrative_ontology:measurement(kjv__be_t0, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(kjv__be_t10, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 10, 0.28).
narrative_ontology:measurement(kjv__be_t20, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 20, 0.26).
narrative_ontology:measurement(kjv__be_t30, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 30, 0.24).
narrative_ontology:measurement(kjv__be_t45, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 45, 0.23).
narrative_ontology:measurement(kjv__be_t60, kjv_text_1611__functional_equivalence_reading, base_extractiveness, 60, 0.22).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(kjv_text_1611__functional_equivalence_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, kjv_text_1611__exclusive_inspiration_reading).
narrative_ontology:affects_constraint(kjv_text_1611__functional_equivalence_reading, kjv_text_1611__revisable_translation_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the kjv_text_1611 kernel. exclusive_inspiration_reading claims the KJV alone is inspired and inerrant (high suppression of alternatives, gatekeeping authority concentrated in one text — likely tangled_rope or snare depending on enforcement mechanisms within KJV-only institutions). revisable_translation_reading claims the KJV is historically important but properly superseded by better manuscripts and scholarship (a scaffold-like or rope-like structure oriented toward ongoing correction). This functional_equivalence_reading occupies the low-extraction middle: it decentralizes authority across multiple coexisting texts rather than either enthroning or revising a single one. Each reading is authored with its own ε reflecting its own internal logic, per the ε-invariance principle — they are not the same constraint measured three ways.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
