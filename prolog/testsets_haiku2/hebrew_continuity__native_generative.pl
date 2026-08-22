% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__native_generative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__native_generative, []).

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
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Hebrew Continuity via Native Generative Use
 *   domain: sociolinguistics/language revitalization
 *
 * SUMMARY:
 *   Hebrew revitalization in the early 20th century reconstructed a language
 *   with no native speakers into the spoken language of a nation-state. The
 *   native-generative reading of this constraint asserts that Hebrew 'lives'
 *   only through the spontaneous, intuitive speech of children raised in
 *   Hebrew-speaking environments—primarily in Israel. This reading treats
 *   native-speaker competence as the sole measure of whether Hebrew is truly
 *   alive, marginalizing liturgical knowledge, textual scholarship, and
 *   non-native fluency as derivative or 'dead.' The constraint is CLAIMED as
 *   tangled_rope (coordination via native-speaker standardization PLUS
 *   extraction from communities whose Hebrew does not meet native-speaker
 *   criteria) while the authored metrics describe a system that has
 *   progressively intensified extraction as the native standard hardened. The
 *   gap is intentional: the state and language academy frame this as pure
 *   coordination; diaspora and textual communities experience it as enforced
 *   devaluation of their competence.
 *
 * KEY AGENTS:
 *   - Secular Zionist state: agenda-setter, institutional power, benefits from linguistic-nationalist cohesion
 *   - Hebrew Language Academy: institutional interpreter, standardizes native-speaker norms as law
 *   - Native speaker communities (Israel): beneficiary measured against, also subject to standardization pressure
 *   - Diaspora liturgical communities: victim set, Hebrew deemed 'dead' by native-speaker standard
 *   - Textual scholars (Bible, Talmud): victim set, historical linguistic authority subordinated to contemporary intuition
 *   - Non-native fluent learners: victim set, perpetually marked as 'accented,' excluded from full legitimacy
 *   - Competing heritage languages (Yiddish, Ladino): excluded, actively suppressed by national linguistic policy
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.67).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.71).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.67).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Hebrew Continuity via Native Generative Use").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistics/language revitalization").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, '8192aed7-0248-485e-a6c6-c9436952319e').
narrative_ontology:cs_kernel_codification('8192aed7-0248-485e-a6c6-c9436952319e', formalized).
narrative_ontology:cs_authority_grounding('8192aed7-0248-485e-a6c6-c9436952319e', extraction).
narrative_ontology:cs_interpretation_layer_present('8192aed7-0248-485e-a6c6-c9436952319e').
narrative_ontology:cs_reading_relation('8192aed7-0248-485e-a6c6-c9436952319e', hebrew_continuity__liturgical_preservation, forecloses).
narrative_ontology:cs_reading_relation('8192aed7-0248-485e-a6c6-c9436952319e', hebrew_continuity__bridge_pidginized, influences).
narrative_ontology:cs_axiom('8192aed7-0248-485e-a6c6-c9436952319e', foundational, native_speaker_intuition_primacy).
narrative_ontology:cs_axiom_status(native_speaker_intuition_primacy, holdable).
narrative_ontology:cs_axiom_grounding('8192aed7-0248-485e-a6c6-c9436952319e', native_speaker_intuition_primacy, empirically_contingent).
narrative_ontology:cs_axiom('8192aed7-0248-485e-a6c6-c9436952319e', foundational, linguistic_homogeneity_prerequisite_for_national_identity).
narrative_ontology:cs_axiom_status(linguistic_homogeneity_prerequisite_for_national_identity, holdable).
narrative_ontology:cs_axiom_grounding('8192aed7-0248-485e-a6c6-c9436952319e', linguistic_homogeneity_prerequisite_for_national_identity, conventional).
narrative_ontology:cs_reference_frame('8192aed7-0248-485e-a6c6-c9436952319e', hebrew_as_living_native_language).
narrative_ontology:cs_drift_state('8192aed7-0248-485e-a6c6-c9436952319e', contemporary_diaspora_literacy_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8192aed7-0248-485e-a6c6-c9436952319e', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, secular_zionist_state).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, hebrew_language_academy).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, native_speaker_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, diaspora_liturgical_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, textual_scholars).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, non_native_fluent_learners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Established Modern Hebrew as the national language through mandatory education, public administration in Hebrew, and suppression of competing language norms (Yiddish, Ladino). Enforces the native-speaker standard by licensing textbooks, certifying teachers, and marginalizing liturgical-only Hebrew as 'dead language' unfit for modern life. Benefits from linguistic unity enabling nationalist identity; the institutional power to set language policy derives from state control of education and public discourse.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, secular_zionist_state, agenda_setter,
    institutional, generational, arbitrage, national).

% Standardizes Modern Hebrew grammar, orthography, and neologisms to match native intuition rather than classical texts. Publishes prescriptive rulings and academic accounts of 'correct' Hebrew. Legitimizes the native-speaker standard as the authoritative criterion. Benefits from institutional authority over language standardization and academic prestige; the constraint's persistence maintains its interpretive monopoly.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, hebrew_language_academy, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__native_generative, hebrew_language_academy, beneficiary).

% Native speakers (primarily children raised in Hebrew in Israel) are the measuring stick for correctness: their generative competence sets the standard. They benefit from having their dialect elevated to national standard; their intuition is treated as law. Yet they also bear the cost of standardization pressure: childhood speech is corrected to align with academy rulings, and their generative use is constantly monitored and modeled for the non-native learning population.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, native_speaker_communities, beneficiary,
    organized, biographical, mobile, national).

% Maintain Hebrew through prayer, ritual recitation, and textual study—a competence decoupled from childhood native use. Diaspora Jewish communities use Hebrew liturgically but are raised in host-country languages; their Hebrew is non-generative, learned through study. The native-generative standard declares their Hebrew 'not really alive' and their textual knowledge 'mere artifact.' Their exit options are constrained: abandoning Hebrew entirely means severing from Jewish tradition; learning native-like Hebrew requires emigration to Israel or intensive childhood immersion. The constraint extracts social recognition and legitimacy from them.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, diaspora_liturgical_communities, payer,
    moderate, biographical, constrained, global).

% Biblical, Medieval, and Rabbinic Hebrew scholars whose expertise is in historical texts and their transmission. The native-generative standard marginalizes philological mastery as inferior to native intuition. Their scholarly authority is downgraded; textual testimony about historical Hebrew norms is subordinated to the speech of contemporary children. They may resist by privileging classical texts over modern usage, but their institutional influence in educational systems weakens as Modern Hebrew becomes mandatory.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, textual_scholars, payer,
    moderate, biographical, constrained, global).

% Adult learners and immigrants who achieve fluency through study rather than childhood immersion. They are perpetually judged against the native standard and told their Hebrew, no matter how fluent, is 'accented' or 'not native.' Their structural inability to acquire native-like intuition (critical period) means they can never meet the standard. They pay through social devaluation and professional barriers in Israel (jobs requiring 'native' Hebrew, accent-based discrimination).
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, non_native_fluent_learners, payer,
    moderate, biographical, constrained, global).

% Yiddish, Ladino, Judeo-Arabic, and other Jewish diaspora languages that were mother tongues before Hebrew revitalization. The native-generative standard for Hebrew and the national policy of linguistic homogenization have nearly eliminated these languages by declaring them 'pre-modern' and incompatible with Israeli identity. They are excluded from the coordination problem Hebrew solves; their speakers' multilingualism is actively suppressed.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, competing_heritage_languages, excluded,
    moderate, biographical, trapped, global).

% Implements the native-speaker standard through mandatory Hebrew instruction designed to produce native-like competence by immersion. The curriculum treats native-speaker intuition as the target and measures student success by proximity to native norms. Benefits from having a clear, measurable standard; is the primary enforcement mechanism for the constraint.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, israeli_public_education, agenda_setter,
    institutional, generational, analytical, national).

% External analysts studying Hebrew revitalization as a natural experiment in language reconstruction and standardization. They examine whether the native-generative model accurately describes Hebrew's actual practice, whether it is empirically justified, and what structural costs it imposes on non-native communities. They take testimony from other seats but their institutional position is outside the Hebrew language system itself.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, comparative_linguists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__native_generative, secular_zionist_state).
narrative_ontology:fixing_cost_class(hebrew_continuity__native_generative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies Jewish diaspora and Israel around a single generative language system rather than competing liturgical, classical, and contact-based Hebrews. Creates a shared linguistic foundation enabling modern Jewish nation-building and cross-community communication on grounds of native intelligibility rather than textual transmission.
% TRANSFER_FUNCTION: Moves legitimacy, institutional prestige, and educational/professional opportunity from textual scholars, liturgical communities, and non-native learners to native speakers and the state apparatus that certifies native competence. Transfers the definition of 'correct Hebrew' from classical texts and historical transmission to contemporary native-speaker intuition.
% ABSENT_VOICES: Yiddish, Ladino, and Judeo-Arabic speakers are structurally excluded: the constraint's coordination solves the problem for Hebrew but erases the linguistic alternatives these communities would use. They are not at the table because the native-generative framing makes diaspora multilingualism appear backward rather than functional.
% DISAPPEARANCE_RATIONALE: If the native-generative standard and its enforcement vanished, Hebrew communities would fracture into multiple registers: liturgical Hebrew for religious use, classical Hebrew for textual scholarship, modern colloquial Hebrew (possibly reverting toward Yiddish-influenced or pidginized forms) for daily use, and regional diaspora contact languages. The Israeli state would lose its linguistic anchor for national identity, and the teaching system would have to accommodate non-native pathways without institutional devaluation. The constraint's removal would require reorganizing educational authority away from native-speaker primacy.
% FOUNDING_PROBLEM: In the early 20th century, Hebrew existed primarily as a liturgical and classical language with no living native speakers. Jewish communities used it ritually and for texts but communicated in Yiddish, Ladino, Judeo-Arabic, and host languages. The founding problem was whether Hebrew could be revived as a spoken language at all, and if so, on what grounds: restoration of biblical norms, continuation of textual tradition, or construction of a new native-speaker community from scratch.
% FOUNDING_PROBLEM_CORROBORATION: Early Zionist activists (Eliezer Ben-Yehuda, historical record) attest the founding problem as urgent and real: Hebrew was indeed absent as a native language. Diaspora scholars and Haredi communities attest the problem was not truly absent—Hebrew had never died as a liturgical and textual language; the revitalization was a reconstruction, not a true resurrection. Comparative linguists studying language revival globally attest that the native-generative standard was a deliberate choice, not an inevitable outcome—other language revivals (Icelandic, Welsh) chose different models emphasizing textual continuity over native intuition.
narrative_ontology:disappearance_verdict(hebrew_continuity__native_generative, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__native_generative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__native_generative, 0.67, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__native_generative_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__native_generative, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__native_generative_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness begins at 0.38 (early revitalization, many pathways to Hebrew still open) and accumulates to 0.67 as native-speaker standardization hardens and institutional enforcement tightens. Theater ratio rises from 0.12 to 0.42 because the academy increasingly invests in prescriptive linguistics and style guidance—activity that defends the boundary between 'correct native' and 'incorrect non-native' Hebrew rather than solving coordination problems. Suppression requirement rises from 0.45 to 0.71 as the standard-setting apparatus must actively exclude and downgrade competing authorities (textual scholars, liturgical traditions, non-native speakers). Native speaker intuition is the measuring stick, but the constraint persists through enforcement machinery—the academy, education system, media gatekeeping—not through voluntary preference. Accessibility collapse is moderate (0.58) because alternatives exist (diaspora communities still use liturgical Hebrew, textual scholarship continues, immigration provides non-native pathways) but are treated as inferior and carry social/institutional costs. Resistance is high (0.73) because textual scholars, diaspora communities, and non-native learners actively contest the native-generative standard and the devaluation it entails. The measurement series model the interval from ~1920 (early revitalization, weak enforcement) to ~2020 (consolidated state, strong native-speaker monopoly on legitimacy).
 *
 * PERSPECTIVAL GAP:
 *   The state and academy see this constraint as coordination—a necessary linguistic foundation for national identity and modern communication. From this seat, native-speaker standardization enables mutual intelligibility and professional unity. From the diaspora-liturgical and textual-scholar seats, the same structure operates as extraction: their Hebrew competence is systematically devalued, their communities are told their language is 'dead,' and they bear the cost of conforming to a standard they cannot meet (non-native learners) or rejecting (diaspora communities choosing to preserve liturgical Hebrew). The constraint's persistence depends on suppressing these alternative framings—treating native-speaker primacy not as a policy choice but as natural fact. Native speakers themselves occupy a dual position: beneficiaries of having their dialect elevated to national standard, but also targets of standardization pressure, constantly corrected and modeled as exemplars.
 *
 * DIRECTIONALITY LOGIC:
 *   The state and academy are near d=0.0 (beneficiaries, control the standard, set the agenda). Native-speaker communities are near d=0.3 (beneficiaries of standardization, but subject to correction and standardization pressure—costs and benefits both present). Diaspora liturgical communities are near d=0.8 (targets of devaluation, identity-locked, constrained by religious/cultural commitment). Textual scholars are near d=0.75 (targets of authority downgrading, constrained by disciplinary commitment). Non-native learners are near d=0.85 (perpetually judged against an unattainable standard, economically constrained in Israel). The engine should derive these from the beneficiary/victim declarations and exit_options, but manual review shows the constraint's enforcement asymmetry is captured most accurately by the institutional beneficiary (state/academy) at low d and the constrained victim populations (diaspora, scholars, non-native learners) at high d.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (whether Hebrew could be revived as a spoken language) was live in 1920. By 1960, the problem was substantially solved: native speakers existed, the language was generative, and Hebrew was functional for modern communication. By 2020, the problem is dead: Hebrew is robust as a native language in Israel and is taught worldwide. Yet the constraint persists at high extractiveness and suppression: institutional authority still rests on native-speaker primacy, diaspora communities are still told their Hebrew is inferior, and textual scholarship is still subordinated to contemporary intuition. The persistent constraint now serves institutional legitimacy and linguistic nationalism, not the coordination problem it was built to solve. The mandatrophy is live: the founding problem has been solved for decades, but the extraction machinery persists because it benefits institutional actors (state, academy) who have no incentive to abandon it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    native_speaker_criterion_empirical,
    'What empirical facts ground the claim that native-speaker intuition is the only valid measure of whether Hebrew ''lives''? Is this a linguistic fact or a policy choice?',
    'Comparative study of language revitalization movements (Icelandic, Welsh, Basque, Maori) and their choice of standardization models. Examine whether languages without native-speaker communities can be said to ''live'' in functional, socially-embedded ways. Philological analysis of whether native-speaker intuition differs significantly from historical texts in ways that would warrant devaluation of textual knowledge.',
    'If native-speaker intuition is empirically necessary for ''living'' language, the constraint is a natural boundary (mountain). If it is a policy choice among viable alternatives (liturgical transmission, contact-language pidgin, textual scholarship), the constraint is constructed extraction riding on coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(native_speaker_criterion_empirical, empirical, 'Whether native-speaker criterion is empirical necessity or constructed standard').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the suppression of competing authorities (textual scholars, diaspora traditions, non-native speakers) structural (external barriers, institutional gatekeeping) or internalized (diaspora communities have come to believe their Hebrew is genuinely inferior)?',
    'Post-suppression trajectory: where diaspora communities that have abandoned liturgical Hebrew or textual scholars who have deferred to academy authority encounter situations where native-speaker norms are absent or irrelevant (private religious communities, scholarly colloquium), do they maintain deference to the native standard or recover confidence in their own competence? If internalized, they carry the suppression with them; if structural, suppression relaxes when enforcement infrastructure is absent.',
    'If suppression is internalized, the constraint''s effective suppression is higher than the scalar measure indicates, and removing institutional enforcement would not immediately restore alternative authorities. If structural, removing enforcement would restore pluralism quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Internalized vs. structural suppression mechanism').

omega_variable(
    coordination_vs_identity_fusion,
    'Is native-generative Hebrew coordination (solving a genuine problem of mutual intelligibility) or identity fusion (native-speaker standards have become inseparable from Israeli/Jewish national identity)?',
    'Natural experiment: in multilingual Israeli contexts (Arab-Israeli communities, immigrant communities), does Hebrew serve coordination functions without native-speaker standardization? In diaspora communities that have revived liturgical Hebrew through study (without childhood native speakers), do they experience the same coordination problems that justify the native-generative standard? Do alternative standards (textual, scholarly, liturgical) solve coordination problems in their own contexts?',
    'If coordination is primary, the constraint should soften as alternatives emerge that solve the same coordination problem. If identity fusion is primary, alternatives are experienced as threats to national identity regardless of functional sufficiency, and suppression will increase.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_identity_fusion, conceptual, 'Whether native-speaker standard solves coordination or serves identity nationalism').

omega_variable(
    kernel_reading_underdetermination,
    'Is this constraint (native-generative) the only defensible reading of the hebrew_continuity kernel, or do the liturgical-preservation and bridge-pidginized readings represent equally viable resolutions to the same founding problem?',
    'Historical counterfactual analysis: if Hebrew revitalization had proceeded under a liturgical-preservation framework (treating classical texts as authoritative, diaspora communities as legitimate users, non-native knowledge as sufficient), would the coordination and revitalization goals have been achieved differently but adequately? Would a bridge-pidginized framework (treating Hebrew as a functional contact language without purity standards) have solved the founding problem with less extraction?',
    'If alternative readings are equally viable, the native-generative reading is not a discovered necessity but a choice that benefits institutional actors (state, academy). The extraction from diaspora and textual communities becomes a choice cost, not a coordination cost. If the native-generative reading is empirically superior, mandatrophy resolution is harder because the benefit structure is genuinely asymmetric.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_underdetermination, conceptual, 'Underdetermination of the hebrew_continuity kernel across three readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_continuity__native_generative, theater_ratio, 0, 0.12).
narrative_ontology:measurement(hebr_tr_t15, hebrew_continuity__native_generative, theater_ratio, 15, 0.18).
narrative_ontology:measurement(hebr_tr_t30, hebrew_continuity__native_generative, theater_ratio, 30, 0.25).
narrative_ontology:measurement(hebr_tr_t50, hebrew_continuity__native_generative, theater_ratio, 50, 0.35).
narrative_ontology:measurement(hebr_tr_t75, hebrew_continuity__native_generative, theater_ratio, 75, 0.4).
narrative_ontology:measurement(hebr_tr_t100, hebrew_continuity__native_generative, theater_ratio, 100, 0.42).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_continuity__native_generative, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(hebr_be_t15, hebrew_continuity__native_generative, base_extractiveness, 15, 0.48).
narrative_ontology:measurement(hebr_be_t30, hebrew_continuity__native_generative, base_extractiveness, 30, 0.55).
narrative_ontology:measurement(hebr_be_t50, hebrew_continuity__native_generative, base_extractiveness, 50, 0.62).
narrative_ontology:measurement(hebr_be_t75, hebrew_continuity__native_generative, base_extractiveness, 75, 0.65).
narrative_ontology:measurement(hebr_be_t100, hebrew_continuity__native_generative, base_extractiveness, 100, 0.67).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_continuity__native_generative, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(hebr_su_t15, hebrew_continuity__native_generative, suppression_requirement, 15, 0.52).
narrative_ontology:measurement(hebr_su_t30, hebrew_continuity__native_generative, suppression_requirement, 30, 0.58).
narrative_ontology:measurement(hebr_su_t50, hebrew_continuity__native_generative, suppression_requirement, 50, 0.66).
narrative_ontology:measurement(hebr_su_t75, hebrew_continuity__native_generative, suppression_requirement, 75, 0.69).
narrative_ontology:measurement(hebr_su_t100, hebrew_continuity__native_generative, suppression_requirement, 100, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__native_generative, 0.12).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, yiddish_suppression_via_linguistic_nationalism).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, israeli_arabic_linguistic_marginalization).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the contested hebrew_continuity kernel. The native-generative reading treats Hebrew as living only through native-speaker intuition and daily generative use. The liturgical-preservation reading treats Hebrew as living through textual transmission and ritual recitation. The bridge-pidginized reading treats Hebrew as a functional contact language for diaspora communication. Each reading has different victim sets, different enforcement mechanisms, and different ε values. They are structurally distinct constraints sharing a common kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_continuity__native_generative, institutional, 0.15).
constraint_indexing:directionality_override(hebrew_continuity__native_generative, organized, 0.35).
constraint_indexing:directionality_override(hebrew_continuity__native_generative, moderate, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
