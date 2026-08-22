% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__native_generation_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: hebrew_living_language__native_generation_reading
 *   human_readable: Hebrew Living Language via Native Generative Speech
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   Between 1880 and 1950, Hebrew was transformed from a liturgical and
 *   scholarly language into the native language of a new speech community.
 *   The transformation required a strong institutional definition: Hebrew was
 *   declared 'living' only when native speakers produced it generatively in
 *   daily contexts, not via memorized recitation or textual study. This
 *   reading of what makes Hebrew a living language excluded competing
 *   definitions (liturgical continuity, literary production) and suppressed
 *   rival linguistic communities (Yiddish and Ladino speakers). The
 *   constraint enforced a tight reachability break: diaspora speakers had to
 *   abandon their native languages and master Hebrew as if they were learning
 *   it for the first time, despite existing competence from prayer and study.
 *   The constraint is claimed as Tangled Rope (genuine coordination of a
 *   Hebrew-speaking nation plus asymmetric extraction from diaspora
 *   languages), measured as moderately extractive (0.61) with substantial
 *   suppression (0.58) and rising theater (as institutional enforcement of a
 *   solved problem increased post-1935).
 *
 * KEY AGENTS:
 *   - Hebrew Revival Movement: organized political and educational force that defined the living-language standard and built institutions (schools, press, language academy) to enforce generative native speech
 *   - Yiddish-speaking diaspora: organized communities in Eastern Europe, victims of the suppression, bore the cost of linguistic marginalization and shame
 *   - Ladino-speaking diaspora: moderate-power communities in Ottoman/Mediterranean regions, identity-locked into the requirement to abandon Ladino as proof of Jewishness
 *   - Hebrew Language Institutions: schools, academy, media that embedded and enforced the native-generation standard, collected authority from controlling linguistic legitimacy
 *   - Children of immigrants: powerless, identity-locked beneficiaries who became native Hebrew speakers and gained authentic belonging via fluency, but at the cost of discontinuity with parents' languages
 *   - Liturgical tradition authorities: excluded communities maintaining Hebrew through unbroken textual/religious transmission, whose reading of Hebrew vitality was suppressed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, 0.61).
domain_priors:suppression_score(hebrew_living_language__native_generation_reading, 0.58).
domain_priors:theater_ratio(hebrew_living_language__native_generation_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_generation_reading, "Hebrew Living Language via Native Generative Speech").
narrative_ontology:topic_domain(hebrew_living_language__native_generation_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_generation_reading, '7970d4ce-d1f6-489e-8907-4e507e1167a0').
narrative_ontology:cs_kernel_codification('7970d4ce-d1f6-489e-8907-4e507e1167a0', distributed).
narrative_ontology:cs_authority_grounding('7970d4ce-d1f6-489e-8907-4e507e1167a0', extraction).
narrative_ontology:cs_interpretation_layer_present('7970d4ce-d1f6-489e-8907-4e507e1167a0').
narrative_ontology:cs_reading_relation('7970d4ce-d1f6-489e-8907-4e507e1167a0', hebrew_living_language__literary_revival_reading, influences).
narrative_ontology:cs_reading_relation('7970d4ce-d1f6-489e-8907-4e507e1167a0', hebrew_living_language__liturgical_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('7970d4ce-d1f6-489e-8907-4e507e1167a0', foundational, authentic_hebrew_vitality_requires_native_generativity).
narrative_ontology:cs_axiom_status(authentic_hebrew_vitality_requires_native_generativity, holdable).
narrative_ontology:cs_axiom_grounding('7970d4ce-d1f6-489e-8907-4e507e1167a0', authentic_hebrew_vitality_requires_native_generativity, deontological).
narrative_ontology:cs_axiom('7970d4ce-d1f6-489e-8907-4e507e1167a0', secondary, diaspora_languages_incompatible_with_national_hebrew).
narrative_ontology:cs_axiom_status(diaspora_languages_incompatible_with_national_hebrew, holdable).
narrative_ontology:cs_axiom_grounding('7970d4ce-d1f6-489e-8907-4e507e1167a0', diaspora_languages_incompatible_with_national_hebrew, conventional).
narrative_ontology:cs_reference_frame('7970d4ce-d1f6-489e-8907-4e507e1167a0', hebrew_as_national_native_language).
narrative_ontology:cs_drift_state('7970d4ce-d1f6-489e-8907-4e507e1167a0', post_native_speaker_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7970d4ce-d1f6-489e-8907-4e507e1167a0', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_generation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_revival_movement).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_language_institutions).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, yiddish_speaking_diaspora).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, ladino_speaking_diaspora).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, children_of_diaspora_immigrants).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, children_of_diaspora_immigrants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Led by intellectuals, educators, and political Zionists who defined Hebrew as 'living' only when produced generatively as daily native speech. They established institutions (schools, newspapers, literary circles) that enforced this standard and marginalized Yiddish/Ladino as diaspora languages unfit for the new nation. Collected cultural authority and institutional resources by controlling what counted as authentic Hebrew revitalization.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_revival_movement, agenda_setter,
    organized, generational, mobile, national).

% Bore the primary suppressive cost: faced institutional delegitimization of their native language in Hebrew-revival contexts, pressure to abandon Yiddish, and linguistic shame attached to continuing diaspora speech. Their exit was theoretically available (stay in diaspora, continue Yiddish) but structurally constrained by the Zionist redefinition of Hebrew as the marker of authenticity and belonging in the reconstituted Jewish nation.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddish_speaking_diaspora, payer,
    organized, biographical, constrained, continental).

% Experienced linguistic suppression similarly: Ladino (the lingua franca of Sephardic diaspora communities) was redefined as incompatible with Hebrew revival. Their identity as Sephardic Jews became entangled with the requirement to abandon Ladino and adopt Hebrew generative speech as proof of authenticity and commitment to the revived nation.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, ladino_speaking_diaspora, payer,
    moderate, biographical, identity_locked, continental).

% Schools, the Academy of the Hebrew Language, publishing houses, and media established standards that treated generative native speech as the sole marker of a living language. They collected institutional authority and resources by defining and enforcing what counted as legitimate Hebrew. Their enforcement machinery suppressed alternative forms (Yiddish-influenced Hebrew, code-switching, liturgical recitation treated as insufficient) and marginalized speakers who maintained diaspora languages.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_language_institutions, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__native_generation_reading, hebrew_language_institutions, beneficiary).

% The primary carriers of the revived language: learned Hebrew as native generative speech in schools and grew up in Hebrew-dominant environments. They benefited from the institutional infrastructure the revival built and gained authentic belonging in the new nation through fluent native Hebrew speech. They also bore the cost of linguistic discontinuity with their parents' native languages and the identity shame attached to Yiddish/Ladino in the revitalization project.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, children_of_diaspora_immigrants, beneficiary,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__native_generation_reading, children_of_diaspora_immigrants, payer).

% Rabbinical scholars and communities maintaining Hebrew through continuous liturgical recitation, textual study, and halakhic discourse across centuries of diaspora. Excluded from the native-generation reading's definition of a living language; their Hebrew was redefined as dead language maintained by rote, not living speech. Would dispute the constraint by arguing Hebrew remained alive through unbroken textual and liturgical transmission.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, liturgical_tradition_authorities, excluded,
    organized, civilizational, constrained, continental).

% Writers and intellectuals who revived Hebrew through literary production (Haskalah movement) before the native-speaker requirement became dominant. They viewed generative Hebrew competence as demonstrated through written literature, not daily speech. Their reading of Hebrew revival competes with the native-generation reading but is absent from the institutional enforcement that defines this constraint.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_literary_renaissance_advocates, excluded,
    organized, biographical, mobile, continental).

% Analyze the historical claim that a language is 'living' only when native speakers produce generative daily speech. They document the competition between readings, the empirical record of Hebrew's status under each definition, and the structural relationship between the native-generation reading and the suppression of Yiddish/Ladino.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, linguistic_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__native_generation_reading, hebrew_revival_movement).
narrative_ontology:fixing_cost_class(hebrew_living_language__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a shared standard for what counts as a living language in the reconstituted Hebrew-speaking nation: generative daily native speech by a speech community, not rote recitation or written literature. This standard coordinates the linguistic identity of the new nation and the institutional infrastructure (schools, media, literature) that transmits native Hebrew competence across generations.
% TRANSFER_FUNCTION: Moves cultural authority and institutional resources from diaspora language communities (Yiddish, Ladino speakers) to Hebrew institutions. Yiddish and Ladino speakers forfeit their claim to authentic Jewish linguistic identity in return for access to the new nation and the possibility of raising children with native Hebrew fluency. The constraint transfers authority from liturgical/textual traditions to speech-community standards.
% ABSENT_VOICES: Liturgical tradition authorities and Haskalah literary advocates are structurally excluded: their readings of Hebrew revival (textual/liturgical continuity, written literary production) are not represented in the institutional enforcement machinery. Yiddish and Ladino speakers, while present as victims, were not agents in defining the living-language standard — they had to accept or resist definitions produced by the revival movement.
% DISAPPEARANCE_RATIONALE: If the native-generation reading and its enforcing institutions vanished, the legitimacy claim to Hebrew would shift: the liturgical and literary readings would resurface as competing definitions of a living language, Yiddish and Ladino might regain institutional support, and the identity requirement for Hebrew fluency as proof of belonging would dissolve. The entire institutional structure of language revitalization would reorganize around alternative standards.
% FOUNDING_PROBLEM: Hebrew had become a liturgical and scholarly language confined to religious contexts and elite textual study across centuries of diaspora. The revival movement's problem was transforming Hebrew into a language of daily life and national communication: creating native speakers who used Hebrew generatively for all speech contexts, not just prayer and study.
% FOUNDING_PROBLEM_CORROBORATION: The revival movement and Hebrew language institutions attest the problem remains live: maintaining Hebrew as a fully native language for new generations requires continued institutional enforcement of generative standards and suppression of competing diaspora languages. Yiddish and Ladino speakers, and linguistic historians outside the revivalist tradition, attest that the founding problem was substantially solved by mid-20th century (native Hebrew speakers were fluent and generative), but the constraint persists as institutional enforcement of linguistic dominance, not coordination of a surviving problem.
narrative_ontology:disappearance_verdict(hebrew_living_language__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__native_generation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__native_generation_reading, 0.61, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__native_generation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_living_language__native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.35 (1880, early revival phase with minimal institutional enforcement) to 0.61 by 1950 (stabilized at ~0.61-0.62 from 1935 onward), indicating growing institutional extraction from diaspora speakers as the revival movement consolidated power. Suppression follows a similar arc, rising from 0.25 to 0.58, tracking the intensification of enforcement machinery (schools, language policies, press standards) that marginalized Yiddish and Ladino. Theater rises throughout (0.18 to 0.42) as the founding problem (lack of native speakers) was solved by mid-1930s but institutional enforcement continued — by 1950, much of the active suppression work was maintaining the legitimacy narrative rather than solving an active coordination problem. The measurements share one time grid: all three metrics are authored at every examined time point (1880, 1900, 1920, 1935, 1945, 1950). The constraint shows post-solve persistence: extraction and suppression plateau at 1935-1945 despite the founding problem being substantially solved (by 1945, native Hebrew speakers were fluent and dominant), indicating the constraint persists as institutional inertia and narrative maintenance, not as active coordination.
 *
 * PERSPECTIVAL GAP:
 *   The Hebrew revival movement and language institutions see the constraint as necessary coordination: creating a shared language for a new nation and teaching the next generation to be native speakers. From their seat, the suppression of Yiddish/Ladino is the cost of national unification, not extraction. Yiddish and Ladino speakers experience the same constraint as enforced assimilation: they are required to abandon their native languages and prove authenticity through fluent generative Hebrew, or face marginalization. The liturgical tradition authorities see the constraint as a false dichotomy: Hebrew was already a living language (through textual and ritual transmission); the native-generation reading is not a discovery but a redefinition that erased their centuries of Hebrew maintenance. The engine computes these divergent types from the structural data: the agenda-setter seat (revival movement) likely computes as rope or tangled-rope (coordination with some enforcement); the payer seats (Yiddish/Ladino speakers) likely compute as tangled-rope or snare (extraction with suppression); the excluded seats (liturgical authorities) compute as targeted by the enforcement machinery, effectively payers in a structure that denies them a voice.
 *
 * DIRECTIONALITY LOGIC:
 *   Hebrew revival movement: beneficiary, agenda-setter, d near 0.1-0.3 (organized institutional power, mobile exit via staying diaspora or adopting alternative revivals, benefits from controlling the living-language definition). Yiddish and Ladino speakers: payers, d near 0.75-0.85 (organized but constrained power, trapped/identity-locked exit as the revival movement is the dominant institution defining belonging in the new nation, bear the suppression cost directly). Children of diaspora immigrants: beneficiary and payer, d near 0.5 (powerless, identity-locked, gain native fluency and authentic belonging, lose continuity with parents' languages). Liturgical authorities: excluded payers, d near 0.7-0.8 (their authority is suppressed by the native-generation reading, but they have constrained exit via maintaining diaspora religious communities). The directionality derives from the structural asymmetry: the revival movement defines what counts as a living language, controls the institutions that enforce it, and benefits from the authority this grants; diaspora speakers must accept the definition or remain outside the new nation's linguistic community.
 *
 * MANDATROPHY ANALYSIS:
 *   The native-generation reading presents a potential mandatrophy case: the founding problem (lack of native Hebrew speakers) was substantially solved by 1935-1945 (native speaker generation was fluent, dominant in schools and institutions). Yet the constraint's extraction and suppression plateau at 0.61-0.62 (extractiveness) and 0.58 (suppression) after 1935, rather than declining. The measurement series shows post-solve persistence: theater rises from 0.44 (1935) to 0.42 (1950), indicating the constraint maintains institutional performance despite the founding problem's resolution. This pattern suggests mandatrophy: the revival movement and language institutions, having solved the coordination problem, continue to extract authority and enforce suppression of competing languages to preserve the legitimacy narrative. The founding-problem status is contested: the institutions attest the problem remains live (maintaining Hebrew as fully native requires continued enforcement); Yiddish/Ladino communities and linguistic historians attest the problem was solved and the constraint persists as institutional inertia. The disappearance verdict is world_rearranges: if the native-generation reading and its enforcement disappeared, the liturgical and literary readings would resurface, Yiddish/Ladino might regain institutional space, and the identity-based requirement for Hebrew fluency would dissolve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generativity_vs_linguistic_competence,
    'Is the core claim about ''generative daily speech'' a structural linguistic requirement for language vitality, or a ideological choice about what counts as authentic revival?',
    'Comparative analysis of living languages and language revitalization projects: can a language remain living through liturgical/textual transmission without native daily generative speech? What do linguistic communities outside the Hebrew revival consider a ''living language''?',
    'If generativity is a structural requirement, the native-generation reading captures a real linguistic distinction; if it is ideological, the suppression of Yiddish/Ladino is extraction justified by a framing choice, not by linguistic necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generativity_vs_linguistic_competence, conceptual, 'Whether generative native speech is structurally necessary for language vitality or a cultural choice.').

omega_variable(
    strict_reachability_asymmetry,
    'For Yiddish and Ladino speakers already fluent in Hebrew (via literacy, prayer, or study), why did the native-generation standard require relearning Hebrew as if it were a wholly new language, rather than recognizing existing Hebrew competence?',
    'Historical analysis of school curricula, institutional policies, and recorded statements by revival leaders: what empirical competence thresholds were actually required, and were they applied asymmetrically (requiring diaspora speakers to exceed competence thresholds native speakers did not face)?',
    'Evidence of asymmetric reachability would establish the native-generation reading as extractive (requiring diaspora speakers to prove authenticity repeatedly), not merely coordinative; symmetric standards would suggest genuine native-speech coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strict_reachability_asymmetry, empirical, 'Whether the native-generation standard was applied symmetrically or enforced asymmetrically against diaspora speakers.').

omega_variable(
    kernel_reading_status,
    'Is the native-generation reading a defensible interpretation of what makes Hebrew a ''living language,'' or a political choice that adopted linguistic framing as its cover story?',
    'This omega documents the kernel-reading structure itself: the native-generation reading is ONE reading of the contested kernel ''Hebrew living language.'' The committer structure (kernel_id, reading_id, sibling readings) is routed here rather than scattered across the JSON. Omega status: unresolvable by data alone (depends on evaluative stance toward what counts as authenticity in language revival).',
    'If defended as a reading, the constraint is Tangled Rope with genuine coordination and asymmetric extraction; if rejected as a cover story, it becomes pure Snare. The reading''s legitimacy depends on how one evaluates the generativity requirement against alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_status, conceptual, 'The kernel-reading structure: this constraint instantiates one reading of the living-language definition against sibling readings (literary, liturgical).').

omega_variable(
    suppression_internalization,
    'Did Yiddish and Ladino speakers'' retreat from their native languages persist because external institutional suppression remained in place, or because they internalized the shame/authenticity framing of the native-generation reading?',
    'Post-revitalization linguistic trajectories: in contexts where Yiddish/Ladino suppression was relaxed or lifted, did speakers return to those languages? Do diaspora communities outside the Hebrew-dominant sphere maintain Yiddish/Ladino without the same shame markers?',
    'If suppression is primarily structural (institutional enforcement), the constraint remains extractive only where enforcement is maintained; if internalized, the victims carry the suppression with them even after institutional barriers weaken. High internalization would indicate the constraint''s effective suppression is higher than the structural measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural vs. internalized suppression in the marginalization of Yiddish/Ladino.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_generation_reading, 1880, 1950).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t1880, hebrew_living_language__native_generation_reading, theater_ratio, 1880, 0.18).
narrative_ontology:measurement_basis(hebr_tr_t1880, projected).
narrative_ontology:measurement(hebr_tr_t1900, hebrew_living_language__native_generation_reading, theater_ratio, 1900, 0.28).
narrative_ontology:measurement_basis(hebr_tr_t1900, observed).
narrative_ontology:measurement(hebr_tr_t1920, hebrew_living_language__native_generation_reading, theater_ratio, 1920, 0.38).
narrative_ontology:measurement_basis(hebr_tr_t1920, observed).
narrative_ontology:measurement(hebr_tr_t1935, hebrew_living_language__native_generation_reading, theater_ratio, 1935, 0.44).
narrative_ontology:measurement_basis(hebr_tr_t1935, observed).
narrative_ontology:measurement(hebr_tr_t1945, hebrew_living_language__native_generation_reading, theater_ratio, 1945, 0.42).
narrative_ontology:measurement_basis(hebr_tr_t1945, observed).
narrative_ontology:measurement(hebr_tr_t1950, hebrew_living_language__native_generation_reading, theater_ratio, 1950, 0.42).
narrative_ontology:measurement_basis(hebr_tr_t1950, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t1880, hebrew_living_language__native_generation_reading, base_extractiveness, 1880, 0.35).
narrative_ontology:measurement_basis(hebr_be_t1880, projected).
narrative_ontology:measurement(hebr_be_t1900, hebrew_living_language__native_generation_reading, base_extractiveness, 1900, 0.48).
narrative_ontology:measurement_basis(hebr_be_t1900, observed).
narrative_ontology:measurement(hebr_be_t1920, hebrew_living_language__native_generation_reading, base_extractiveness, 1920, 0.57).
narrative_ontology:measurement_basis(hebr_be_t1920, observed).
narrative_ontology:measurement(hebr_be_t1935, hebrew_living_language__native_generation_reading, base_extractiveness, 1935, 0.62).
narrative_ontology:measurement_basis(hebr_be_t1935, observed).
narrative_ontology:measurement(hebr_be_t1945, hebrew_living_language__native_generation_reading, base_extractiveness, 1945, 0.61).
narrative_ontology:measurement_basis(hebr_be_t1945, observed).
narrative_ontology:measurement(hebr_be_t1950, hebrew_living_language__native_generation_reading, base_extractiveness, 1950, 0.61).
narrative_ontology:measurement_basis(hebr_be_t1950, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t1880, hebrew_living_language__native_generation_reading, suppression_requirement, 1880, 0.25).
narrative_ontology:measurement_basis(hebr_su_t1880, projected).
narrative_ontology:measurement(hebr_su_t1900, hebrew_living_language__native_generation_reading, suppression_requirement, 1900, 0.42).
narrative_ontology:measurement_basis(hebr_su_t1900, observed).
narrative_ontology:measurement(hebr_su_t1920, hebrew_living_language__native_generation_reading, suppression_requirement, 1920, 0.54).
narrative_ontology:measurement_basis(hebr_su_t1920, observed).
narrative_ontology:measurement(hebr_su_t1935, hebrew_living_language__native_generation_reading, suppression_requirement, 1935, 0.62).
narrative_ontology:measurement_basis(hebr_su_t1935, observed).
narrative_ontology:measurement(hebr_su_t1945, hebrew_living_language__native_generation_reading, suppression_requirement, 1945, 0.58).
narrative_ontology:measurement_basis(hebr_su_t1945, observed).
narrative_ontology:measurement(hebr_su_t1950, hebrew_living_language__native_generation_reading, suppression_requirement, 1950, 0.58).
narrative_ontology:measurement_basis(hebr_su_t1950, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__native_generation_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__native_generation_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__literary_revival_reading).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__liturgical_continuity_reading).

% DUAL FORMULATION NOTE:
% The kernel 'Hebrew living language' decomposes into three structurally distinct constraint stories: native_generation_reading (this file, native speaker generation), literary_revival_reading (Haskalah written competence), liturgical_continuity_reading (textual and ritual transmission). Each story carries its own beneficiary/victim structure, extractiveness, and enforcement machinery. They share a common kernel (what makes Hebrew 'living') and compete for legitimacy in defining Hebrew vitality. The native-generation reading structurally influences the other two by redefining what counts as authentic revival; it does not foreclose them (both remain live in diaspora communities), but it creates pressure to delegitimize them. The ε values differ significantly: native-generation reading (0.61 extractiveness, suppresses diaspora languages), literary reading (lower extractiveness if measured as pure literary coordination), liturgical reading (lower extractiveness if measured as internal tradition maintenance). Each story is generated independently with its own metrics and stakeholder structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_living_language__native_generation_reading, powerless, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
