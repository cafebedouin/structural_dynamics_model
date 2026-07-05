% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra_discourse__universalist_devotional_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gita_kurukshetra_discourse__universalist_devotional_reading, []).

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
 *   constraint_id: gita_kurukshetra_discourse__universalist_devotional_reading
 *   human_readable: Universalist Devotional (Bhakti) Reading of the Bhagavad Gita
 *   domain: religious_studies/textual_hermeneutics/ethics
 *
 * SUMMARY:
 *   This story instantiates ONE of three structurally distinct readings of
 *   the Bhagavad Gita's Kurukshetra discourse: the universalist devotional
 *   (bhakti) reading, which holds that the text's core teaching is
 *   path-independent devotion to the divine accessible regardless of caste,
 *   and that dharma is properly understood as surrender to divine will rather
 *   than fidelity to inherited social role. This reading is historically
 *   associated with bhakti movements (from Alvars and Nayanars through later
 *   vernacular saint-poets) and was amplified during 19th-20th century reform
 *   and nationalist-era commentary. It stands apart from (1) the orthodox
 *   literal reading, which holds the text affirms caste-based duty and
 *   legitimates righteous violence, and (2) the Gandhian allegorical reading,
 *   which treats the battlefield as a metaphor for internal spiritual
 *   struggle rather than physical war. These are not three angles on one
 *   constraint — they carry different beneficiary structures, different
 *   textual emphases, and different institutional consequences, and are
 *   authored as three separate constraint stories linked via network edges.
 *
 * KEY AGENTS:
 *   - low_caste_devotees: primary beneficiary (powerless/constrained) — gains theological legitimacy for devotional practice outside ritual mediation
 *   - bhakti_movement_teachers: agenda-setting beneficiary (organized/mobile) — builds institutional authority around this reading
 *   - brahminical_ritual_authorities: structural payer (institutional/constrained) — loses interpretive monopoly and gatekeeping function
 *   - comparative_religion_scholars: analytical observer (analytical/analytical) — traces the reading's historical emergence and contested status
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gita_kurukshetra_discourse__universalist_devotional_reading, 0.28).
domain_priors:suppression_score(gita_kurukshetra_discourse__universalist_devotional_reading, 0.32).
domain_priors:theater_ratio(gita_kurukshetra_discourse__universalist_devotional_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(gita_kurukshetra_discourse__universalist_devotional_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gita_kurukshetra_discourse__universalist_devotional_reading, rope).
narrative_ontology:human_readable(gita_kurukshetra_discourse__universalist_devotional_reading, "Universalist Devotional (Bhakti) Reading of the Bhagavad Gita").
narrative_ontology:topic_domain(gita_kurukshetra_discourse__universalist_devotional_reading, "religious_studies/textual_hermeneutics/ethics").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gita_kurukshetra_discourse__universalist_devotional_reading, '74038407-4419-48bc-86e8-4e06dc62701f').
narrative_ontology:cs_kernel_codification('74038407-4419-48bc-86e8-4e06dc62701f', fixed_text).
narrative_ontology:cs_authority_grounding('74038407-4419-48bc-86e8-4e06dc62701f', practice).
narrative_ontology:cs_interpretation_layer_present('74038407-4419-48bc-86e8-4e06dc62701f').
narrative_ontology:cs_reading_relation('74038407-4419-48bc-86e8-4e06dc62701f', gita_kurukshetra_discourse__orthodox_literal_reading, influences).
narrative_ontology:cs_reading_relation('74038407-4419-48bc-86e8-4e06dc62701f', gita_kurukshetra_discourse__gandhian_allegorical_reading, coexists_with).
narrative_ontology:cs_axiom('74038407-4419-48bc-86e8-4e06dc62701f', foundational, caste_is_not_a_spiritual_barrier).
narrative_ontology:cs_axiom_status(caste_is_not_a_spiritual_barrier, holdable).
narrative_ontology:cs_axiom_grounding('74038407-4419-48bc-86e8-4e06dc62701f', caste_is_not_a_spiritual_barrier, deontological).
narrative_ontology:cs_axiom('74038407-4419-48bc-86e8-4e06dc62701f', foundational, dharma_is_devotional_surrender_not_social_role).
narrative_ontology:cs_axiom_status(dharma_is_devotional_surrender_not_social_role, holdable).
narrative_ontology:cs_axiom_grounding('74038407-4419-48bc-86e8-4e06dc62701f', dharma_is_devotional_surrender_not_social_role, conventional).
narrative_ontology:cs_reference_frame('74038407-4419-48bc-86e8-4e06dc62701f', bhakti_devotional_universalism).
narrative_ontology:cs_drift_state('74038407-4419-48bc-86e8-4e06dc62701f', colonial_and_reform_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('74038407-4419-48bc-86e8-4e06dc62701f', '').
narrative_ontology:cs_kernel_id(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, low_caste_devotees).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, bhakti_movement_teachers).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, women_devotees).
narrative_ontology:constraint_beneficiary(gita_kurukshetra_discourse__universalist_devotional_reading, lay_practitioners_outside_brahminical_lineage).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gita_kurukshetra_discourse__universalist_devotional_reading, brahminical_ritual_authorities).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, salvation_accessible_regardless_of_caste).
narrative_ontology:constraint_vindicates(gita_kurukshetra_discourse__universalist_devotional_reading, dharma_as_devotional_surrender_not_social_role).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically excluded from Vedic study and temple access by caste rules, this reading of the text gives them a doctrinal basis to claim direct devotional access to the divine without Brahminical mediation. They gain textual legitimacy for practices (bhakti, chanting, surrender) that bypass ritual gatekeeping; exit from the caste system itself remains constrained by surrounding social structure even as the theological argument for exclusion weakens.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, low_caste_devotees, beneficiary,
    powerless, generational, constrained, regional).

% Teachers and poet-saints (historically figures like Ramanuja-lineage bhakti teachers, vernacular saint-poets) who propagate this reading, building movements, institutions, and followings around it. They administer the interpretive tradition that makes this reading operative in practice, and they gain authority, followers, and institutional standing from popularizing it.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, bhakti_movement_teachers, agenda_setter,
    organized, generational, mobile, regional).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__universalist_devotional_reading, bhakti_movement_teachers, beneficiary).

% Priestly lineages whose social and economic position depended partly on being the necessary intermediaries for ritual and textual access. This reading erodes their gatekeeping monopoly by asserting that devotion, not ritual correctness or caste-conferred purity, is the operative path to liberation. They cannot simply exit the tradition — their institutional identity is bound up in officiating it — so the erosion is a structural cost they must absorb or resist.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, brahminical_ritual_authorities, payer,
    institutional, civilizational, constrained, regional).

% Traditionally barred from Vedic study under orthodox rules, women gain a textual basis for direct devotional practice under this reading. The devotional path does not require the ritual qualifications historically denied to them, though the surrounding social order restricting their broader autonomy is not itself dissolved by the reading.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, women_devotees, beneficiary,
    powerless, generational, constrained, regional).

% 19th-20th century reformers, translators, and commentators (figures in the Bengal Renaissance, Arya Samaj-adjacent and neo-Vedantic circles) who advanced this universalist reading partly in dialogue with colonial critique and partly to reposition Hindu textual tradition as compatible with modern egalitarian ethics. They shape which reading circulates internationally and in vernacular print culture.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, colonial_and_reform_era_interpreters, agenda_setter,
    organized, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(gita_kurukshetra_discourse__universalist_devotional_reading, colonial_and_reform_era_interpreters, observer).

% Scholars and lineages committed to the caste-affirming, duty-as-social-role reading would object that the universalist reading strips the text's explicit endorsement of svadharma tied to varna. Their objection is present in scholarly and traditionalist literature but is largely absent from the popular devotional and reformist circulation of this reading, especially internationally.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, orthodox_literalist_scholars, excluded,
    institutional, civilizational, constrained, regional).

% Academic interpreters who trace how this reading emerged historically (bhakti movements of the 12th-17th centuries, later reform-era universalism) and assess its textual warrant against competing readings without institutional stake in either outcome.
narrative_ontology:constraint_stakeholder(gita_kurukshetra_discourse__universalist_devotional_reading, comparative_religion_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared devotional framework that lets people across social position coordinate around a single accessible practice (bhakti) and a single textual authority, without requiring specialized ritual training or caste qualification as a precondition for religious legitimacy.
% TRANSFER_FUNCTION: Moves religious and interpretive authority away from Brahminical ritual specialists and toward devotional teachers and lay practitioners; moves theological legitimacy from caste-conferred status to individual devotional practice, redistributing access to claims of spiritual standing.
% ABSENT_VOICES: Orthodox literalist scholars and traditional ritual authorities who read the text as affirming caste-based duty are largely excluded from the popular and reform-era circulation of this reading; their textual counter-arguments (citing explicit varna-dharma passages) exist in scholarly literature but rarely surface in devotional or activist appropriations of the universalist reading.
% DISAPPEARANCE_RATIONALE: If this reading vanished, bhakti movements and reform-era egalitarian religious arguments would lose a major textual anchor, and devotees without ritual standing would lose a canonical basis for direct spiritual claims; this would matter enormously to bhakti institutions and lay practitioners. But orthodox and academic-literalist communities would argue the 'true' text was unaffected all along, since they hold the universalist reading was always a later imposition rather than the text's operative meaning — hence contested rather than a clean verdict.
% FOUNDING_PROBLEM: This reading was built to resolve a felt tension between the text's explicit caste-and-duty language (chapter 4, 18) and its simultaneous claims about devotion being open to 'even the lowest born' (referencing verses like BG 9.32) — and to provide theological grounding for devotional movements seeking religious legitimacy outside Brahminical ritual control.
% FOUNDING_PROBLEM_CORROBORATION: Comparative religion scholars (outside both the bhakti-teacher beneficiary class and the orthodox-authority payer class) corroborate that the historical bhakti movements developed and popularized this reading over centuries partly in genuine tension with, and partly as reform against, caste-restrictive ritual practice — but the same scholars note the reading's emphasis on universal access was also amplified during the colonial and nationalist period for apologetic purposes, which is a distinct, later motivation than the medieval bhakti movements' own concerns.
narrative_ontology:disappearance_verdict(gita_kurukshetra_discourse__universalist_devotional_reading, contested).
narrative_ontology:founding_problem_status(gita_kurukshetra_discourse__universalist_devotional_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gita_kurukshetra_discourse__universalist_devotional_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gita_kurukshetra_discourse__universalist_devotional_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).
:- end_tests(gita_kurukshetra_discourse__universalist_devotional_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored low (0.28 at endpoint) because this reading's structural effect is broadly redistributive of religious legitimacy rather than extractive of resources — it does not concentrate rents on a narrow class; if anything it diffuses gatekeeping authority outward. Suppression is moderate (0.32): the reading does not require active coercion to hold, but its widespread popular circulation has, in places, been amplified by reform-era institutions with their own agendas, and rival orthodox readings are sometimes marginalized in devotional and international contexts rather than fairly weighed. Theater ratio is low, reflecting that the devotional practices this reading legitimates (bhakti, chanting, direct worship) are substantially functional rather than performative. Resistance is moderately high (0.55) because orthodox literalist and ritual-authority communities actively contest this reading on textual grounds, not merely as passive disagreement.
 *
 * PERSPECTIVAL GAP:
 *   From the bhakti-teacher and lay-devotee seats, this reading is coordination: a shared, accessible framework for religious practice and community formation. From the orthodox ritual-authority seat, the same reading operates as an erosion of a previously load-bearing institutional function (ritual gatekeeping tied to caste-conferred purity) — the engine should compute these seats differently given their divergent structural position, without either seat's computed type being treated as the 'correct' overall verdict for the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Low-caste devotees, women devotees, and bhakti teachers sit toward the beneficiary end: the reading extends religious legitimacy and practice-access to groups previously excluded by caste- or gender-conditioned ritual requirements. Brahminical ritual authorities sit toward the payer end: their institutional position depended partly on being necessary interpretive intermediaries, and this reading structurally erodes that necessity. Colonial/reform-era interpreters are agenda-setters with mixed motives (genuine reform commitments alongside apologetic positioning against colonial critique), which is why they carry both agenda_setter and observer roles rather than a clean beneficiary designation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — providing theological grounding for devotional access outside caste-restricted ritual — remains genuinely live for many practicing bhakti communities (hence 'contested' rather than 'dead'), which prevents this reading from being mislabeled as pure extraction or pure ideological cover. At the same time, the corroboration record shows the reading was also mobilized for later apologetic purposes distinct from its medieval devotional origins, which is exactly the kind of layered-motive structure the mandatrophy question is designed to surface rather than flatten into a single clean verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_warrant_vs_selective_emphasis,
    'Does the universalist devotional reading draw on genuine textual warrant distributed throughout the Gita (e.g., BG 9.32''s claim about access for the lowest-born), or does it selectively emphasize devotional passages while backgrounding the text''s explicit caste-duty language (BG 4.13, 18.41-48)?',
    'Close philological comparison of relative textual weight given to devotional-universalist passages versus caste-duty passages across the full eighteen chapters, cross-checked against pre-bhakti-movement commentarial traditions (e.g., Shankara''s Advaita commentary) to see whether the universalist emphasis is attested earlier or is a later interpretive layer.',
    'If the universalist reading requires substantial backgrounding of explicit caste-duty verses, its claim to represent the text''s ''core'' teaching is weaker than its popular circulation suggests, and its structural function shifts closer to selective reform-motivated reinterpretation than textually mandated egalitarianism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_warrant_vs_selective_emphasis, conceptual, 'Whether the universalist reading''s textual warrant is comprehensive or selective.').

omega_variable(
    reform_era_amplification_motive,
    'How much of this reading''s contemporary prominence derives from genuine continuity with medieval bhakti movements versus colonial-era and nationalist-era apologetic motivation to present Hindu textual tradition as compatible with modern egalitarian norms?',
    'Historical tracing of the reading''s textual and institutional lineage from bhakti-era commentaries (12th-17th century) through 19th-20th century reform movements, comparing emphasis and framing across periods.',
    'If substantially colonial/nationalist-era amplified, part of this reading''s apparent naturalness is itself a constructed response to external critique rather than an uninterrupted internal development — this would not change the ε classification but would sharpen the founding_problem_status assessment toward a two-phase genealogy rather than a single continuous one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reform_era_amplification_motive, empirical, 'Degree to which contemporary prominence reflects apologetic amplification versus continuous devotional tradition.').

omega_variable(
    kernel_framing_alternative,
    'Is the caste-liberation dimension of this reading better modeled as a distinct emphasis within this single reading, or as a fourth sibling reading in its own right (a specifically anti-caste social-reform reading, distinguishable from the broader devotional-universalist theological reading)?',
    'Compare whether social-reform interpreters (e.g., anti-caste activists citing the Gita) and purely theological bhakti interpreters (concerned with personal salvation, less with caste-system critique as such) actually converge on identical claims, or diverge enough to warrant separate constraint stories under the ε-invariance principle.',
    'If they diverge structurally (different beneficiary sets, different vindicated propositions, different persistence mechanisms), this story should be split further; as authored here, it bundles theological-universalist and social-reform emphases into one reading because both share the same core axiom (caste is not a spiritual barrier) and the same beneficiary class.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative, conceptual, 'Whether caste-liberation and devotional-universalism are one reading or should decompose further.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra_discourse__universalist_devotional_reading, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gita_tr_t0, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(gita_tr_t0, projected).
narrative_ontology:measurement(gita_tr_t133, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 133, 0.12).
narrative_ontology:measurement_basis(gita_tr_t133, projected).
narrative_ontology:measurement(gita_tr_t266, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 266, 0.14).
narrative_ontology:measurement_basis(gita_tr_t266, projected).
narrative_ontology:measurement(gita_tr_t400, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 400, 0.16).
narrative_ontology:measurement_basis(gita_tr_t400, projected).
narrative_ontology:measurement(gita_tr_t600, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 600, 0.18).
narrative_ontology:measurement_basis(gita_tr_t600, projected).
narrative_ontology:measurement(gita_tr_t800, gita_kurukshetra_discourse__universalist_devotional_reading, theater_ratio, 800, 0.2).
narrative_ontology:measurement_basis(gita_tr_t800, projected).

% Extraction over time
narrative_ontology:measurement(gita_be_t0, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(gita_be_t0, projected).
narrative_ontology:measurement(gita_be_t133, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 133, 0.18).
narrative_ontology:measurement_basis(gita_be_t133, projected).
narrative_ontology:measurement(gita_be_t266, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 266, 0.2).
narrative_ontology:measurement_basis(gita_be_t266, projected).
narrative_ontology:measurement(gita_be_t400, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 400, 0.22).
narrative_ontology:measurement_basis(gita_be_t400, projected).
narrative_ontology:measurement(gita_be_t600, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 600, 0.25).
narrative_ontology:measurement_basis(gita_be_t600, projected).
narrative_ontology:measurement(gita_be_t800, gita_kurukshetra_discourse__universalist_devotional_reading, base_extractiveness, 800, 0.28).
narrative_ontology:measurement_basis(gita_be_t800, projected).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(gita_kurukshetra_discourse__universalist_devotional_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__orthodox_literal_reading).
narrative_ontology:affects_constraint(gita_kurukshetra_discourse__universalist_devotional_reading, gita_kurukshetra_discourse__gandhian_allegorical_reading).

% DUAL FORMULATION NOTE:
% Three constraint stories form the gita_kurukshetra_discourse kernel family: orthodox_literal_reading (caste-duty affirming, violence-legitimating), gandhian_allegorical_reading (battlefield-as-metaphor, internal struggle), and this story, universalist_devotional_reading (caste-transcending devotional access). Each reading has a stable ε and distinct beneficiary/victim structure per the ε-invariance principle; they are not one constraint measured three ways. This reading structurally influences the orthodox reading by eroding the theological necessity of caste-conferred ritual privilege wherever it gains institutional and popular traction (an 'influences' relationship, not foreclosure, since orthodox and universalist readings continue to coexist across different communities).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
