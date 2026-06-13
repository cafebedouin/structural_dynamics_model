% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:constraint_stakeholder/7,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_living_language__native_generation_reading
 *   human_readable: Hebrew Living Language via Native Generative Speech (native_generation_reading)
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The native-generation reading of the hebrew_living_language kernel
 *   asserts that Hebrew becomes linguistically living only when native
 *   speakers produce daily speech generatively (spontaneously creating
 *   utterances), not through memorized recitation of liturgical or literary
 *   texts. This reading emerged as dominant in the Zionist settlement project
 *   in Palestine and later the Israeli nation-state, where it functioned to
 *   legitimize Hebrew-only educational and public institutions and to
 *   suppress Yiddish and Ladino as impediments to national revival. The
 *   reading sits at the intersection of linguistic theory (what counts as a
 *   living language?), historical narrative (how did Hebrew survive
 *   diaspora?), and political commitment (what language should constitute the
 *   nation-state?). The constraint story models this reading as a tangled
 *   rope: it solves a genuine coordination problem (unifying Jewish diaspora
 *   under one spoken language) while simultaneously extracting from Yiddish
 *   and Ladino speakers through linguistic delegitimation and suppression.
 *   The measurement series tracks the escalation of extraction and
 *   suppression over the period of Palestinian Jewish settlement (T0~1880,
 *   T120~2000): extraction rises steeply through the first 60 time points
 *   (institutional establishment phase) and plateaus after T90 (state
 *   consolidation).
 *
 * KEY AGENTS:
 *   - hebrew_revival_movement: Organized agenda-setter (moderate→powerful power trajectory) — sets the native-generation criterion, enforces it through education and public discourse, benefits from linguistic unification.
 *   - jewish_settler_community_palestine: Organized beneficiary — gains native Hebrew competence and cultural legitimacy as descendants of ancient Jewish speakers; their settlement is legitimized by the constraint.
 *   - yiddish_speaking_diaspora: Moderate power, identity-locked exit — bears suppression and delegitimation; cannot exit without severing linguistic/cultural identity.
 *   - ladino_speaking_sephardic_diaspora: Moderate power, identity-locked exit — similarly suppressed; Ladino reframed from living vernacular to ancestral remainder.
 *   - liturgical_scholars_yeshiva_tradition: Organized, excluded — their claim that Hebrew lived through recitation is structurally negated by the native-generation criterion.
 *   - literary_hebrew_advocates_haskalah: Organized, excluded — their claim that Hebrew lived through literary production is rejected by the strict-reachability break (written only, not native speech).
 *   - linguistic_anthropologists: Analytical observers — examine whether the native-generation criterion is linguistically defensible or retrospectively imposed.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, 0.68).
domain_priors:suppression_score(hebrew_living_language__native_generation_reading, 0.72).
domain_priors:theater_ratio(hebrew_living_language__native_generation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, accessibility_collapse, 0.61).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_generation_reading, "Hebrew Living Language via Native Generative Speech (native_generation_reading)").
narrative_ontology:topic_domain(hebrew_living_language__native_generation_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_generation_reading, '733af577-2edc-4b41-be7b-5300212326fa').
narrative_ontology:cs_kernel_codification('733af577-2edc-4b41-be7b-5300212326fa', distributed).
narrative_ontology:cs_authority_grounding('733af577-2edc-4b41-be7b-5300212326fa', lineage).
narrative_ontology:cs_interpretation_layer_present('733af577-2edc-4b41-be7b-5300212326fa').
narrative_ontology:cs_reading_relation('733af577-2edc-4b41-be7b-5300212326fa', hebrew_living_language__liturgical_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('733af577-2edc-4b41-be7b-5300212326fa', hebrew_living_language__literary_revival_reading, forecloses).
narrative_ontology:cs_axiom('733af577-2edc-4b41-be7b-5300212326fa', foundational, native_generative_speech_necessary).
narrative_ontology:cs_axiom_status(native_generative_speech_necessary, holdable).
narrative_ontology:cs_axiom_grounding('733af577-2edc-4b41-be7b-5300212326fa', native_generative_speech_necessary, conventional).
narrative_ontology:cs_axiom('733af577-2edc-4b41-be7b-5300212326fa', foundational, recitation_insufficient_for_linguistic_life).
narrative_ontology:cs_axiom_status(recitation_insufficient_for_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('733af577-2edc-4b41-be7b-5300212326fa', recitation_insufficient_for_linguistic_life, empirically_contingent).
narrative_ontology:cs_reference_frame('733af577-2edc-4b41-be7b-5300212326fa', native_speech_revival_framework).
narrative_ontology:cs_drift_state('733af577-2edc-4b41-be7b-5300212326fa', contemporary_israeli_state, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('733af577-2edc-4b41-be7b-5300212326fa', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_generation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_revival_movement).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, jewish_settler_community_palestine).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, yiddish_speaking_diaspora).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, ladino_speaking_sephardic_diaspora).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, non_hebrew_vernacular_jews).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocates that Hebrew is living only through native generative speech — daily spontaneous utterance by native speakers, not recitation of liturgical or literary texts. Sets the standard for linguistic authenticity and coordinates the shift of Jewish settlement in Palestine toward Hebrew-only environments. Enforces the standard through educational institutions, public discourse, and social pressure, rejecting Yiddish and Ladino as impediments to Hebrew revival.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_revival_movement, agenda_setter,
    organized, generational, arbitrage, continental).

% Receives the benefit of a unified linguistic identity and cultural continuity reconstituted through Hebrew speech. Young settlers born into Hebrew-speaking households gain native competence without effort. The reading provides a framework for legitimating their presence and settlement as a revival of ancient Jewish life. They benefit from the enforcement that suppresses rival vernacular communities and languages.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, jewish_settler_community_palestine, beneficiary,
    organized, generational, constrained, regional).

% Bears the cost of linguistic delegitimation: Yiddish is reframed from a living Jewish vernacular into a diaspora remnant or obstacle to national revival. Yiddish speakers face social pressure to assimilate to Hebrew or accept marginalization within the revitalized Jewish community. Their language and culture are systematically suppressed in favor of Hebrew in institutional contexts (schools, media, government). The identity lock is tight: Yiddish speakers cannot readily abandon their linguistic identity without severing ties to their cultural and family continuity.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddish_speaking_diaspora, payer,
    moderate, biographical, identity_locked, global).

% Similarly bears suppression of Ladino (Judeo-Spanish) under the native-generation reading. Ladino speakers are positioned as outside the revitalized Jewish nation-state project unless they adopt Hebrew as primary language. Their centuries-long continuity of Judeo-Spanish oral culture is devalued as non-generative or merely ancestral. Identity lock is similarly tight: Ladino is constitutive of Sephardic Jewish identity; abandoning it means cultural dissolution.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, ladino_speaking_sephardic_diaspora, payer,
    moderate, biographical, identity_locked, global).

% Would argue that Hebrew remained living through unbroken liturgical recitation and textual study across the diaspora, independent of native generative speech. They are structurally excluded from the native-generation reading's authority framework, which treats recitative competence as insufficient proof of linguistic life. Their exclusion is the enforcement object itself: the constraint depends on denying that memorized liturgical Hebrew counts.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, liturgical_scholars_yeshiva_tradition, excluded,
    organized, civilizational, constrained, global).

% Would argue that Hebrew lived through Haskalah literary production and written generative competence, without requiring native daily speech in vernacular contexts. They are excluded from the native-generation framework by a strict reachability condition: written competence and literary production do not satisfy the native-speech requirement. This exclusion is deliberate — the constraint's specificity depends on rejecting written-only revival as insufficient.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, literary_hebrew_advocates_haskalah, excluded,
    organized, biographical, constrained, global).

% Examine whether the native-generation criterion is linguistically defensible or a retrospective ideological framing. They compare Hebrew's case to other revived languages (Irish, Welsh) and ask whether the native-generation reading captures a real linguistic fact or a political commitment. They can produce evidence about the actual speech practices of early Hebrew speakers in Palestine and whether generative native speech was actually the criterion applied historically or an invented standard.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, linguistic_anthropologists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__native_generation_reading, hebrew_revival_movement).
narrative_ontology:fixing_cost_class(hebrew_living_language__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifies Jewish diaspora communities under a single spoken language reconstituted as native, enabling a coherent national-cultural project in Palestine/Israel without the fragmentation of Yiddish, Ladino, Arabic, and other vernaculars.
% TRANSFER_FUNCTION: Transfers linguistic authority and cultural legitimacy from Yiddish and Ladino speakers to Hebrew-native speakers, and from memorized recitative competence (liturgical scholars) to generative daily-speech competence.
% ABSENT_VOICES: Yiddish and Ladino speakers whose objection to linguistic delegitimation would be direct (if included in the conversation, they would assert their languages remained living and continuous). Liturgical scholars and literary advocates whose epistemic claims about Hebrew's continuity are structurally excluded by the native-generation criterion. These exclusions are the enforcement machinery itself.
% DISAPPEARANCE_RATIONALE: If the native-generation reading vanished and the liturgical-continuity or literary-revival readings governed instead, Hebrew's status would be radically reframed: liturgical scholars could claim Hebrew never died, literary advocates could claim it lived through Haskalah, and Yiddish/Ladino would retain cultural legitimacy. The reading's disappearance would delegitimize the settler community's claim to native status and force linguistic pluralism in the revived state.
% FOUNDING_PROBLEM: How can Hebrew be reconstituted as a living spoken language when diaspora Jews speak multiple vernaculars (Yiddish, Ladino, Arabic, etc.) and the primary Hebrew competence is recitative (liturgical study)? The native-generation reading answers: only through native-speaker generative competence in daily speech.
% FOUNDING_PROBLEM_CORROBORATION: The revival movement attests the problem was acute and that native generative speech became the solution. Linguistic historians outside the movement contest the founding problem's framing: they show that earlier revivalists (Haskalah writers, liturgical scholars) already treated Hebrew as living through their respective practices. Historians of linguistics document that the native-generation criterion was a retrospective commitment imposed selectively, not discovered as a pre-existing standard.
narrative_ontology:disappearance_verdict(hebrew_living_language__native_generation_reading, contested).
narrative_ontology:founding_problem_status(hebrew_living_language__native_generation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__native_generation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__native_generation_reading, 'none', 1).

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
 *   The native-generation reading exhibits moderate-to-high extractiveness (rising from 0.32 to 0.68 over the interval) because the constraint's benefits accrue to Hebrew-native speakers while costs are borne by Yiddish/Ladino speakers in the form of linguistic delegitimation and suppression. Suppression is even higher (0.72 at interval end) because the constraint depends on actively excluding rival reading frames (liturgical continuity, literary revival) and suppressing competing vernaculars. The enforcement machinery is institutional and social: education systems that teach Hebrew-only, public discourse that privileges generative native speech as the standard of linguistic authenticity, migration and settlement policies that favor Hebrew speakers. Theater is low-to-moderate (0.28): the reading has genuine linguistic content (native generative speech is a real phenomenon), but a growing portion of enforcement effort is spent maintaining the suppression of rival readings rather than advancing the linguistic generative function itself. The measurement series shows extraction and suppression rising together through T0–T60 (the institutional establishment of the constraint in Palestine/Israel), then plateauing after T90 (state consolidation: the constraint is now embedded in law, education, media; further suppression adds little because the rival readings are already structurally excluded). Theater remains moderate because the constraint continues to be justified linguistically (native speech IS happening) even as enforcement shifts toward political consolidation.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter (hebrew_revival_movement) experiences this constraint as a liberating unification: they see themselves as solving a real coordination problem (diaspora fragmentation) and enabling linguistic revival. The beneficiary (jewish_settler_community_palestine) experiences it as a transparent benefit (they are native speakers; the constraint is simply a description of linguistic reality). The payer seats (yiddish_speaking_diaspora, ladino_speaking_sephardic_diaspora) experience it as suppression: their languages are delegitimated, their children are forced into Hebrew-only schooling, their cultural continuity is framed as an obstacle rather than a resource. The excluded seats (liturgical scholars, literary advocates) experience it as a rewriting of their historical claims: their centuries-long continuity is declared insufficient by the new native-generation standard. The engine computes these divergences from power, exit_options, and the beneficiary/victim declarations. The claim (tangled_rope) reflects the reading's own internal frame; the metrics (extractiveness, suppression rising over time) reflect how the constraint's operation is experienced from seats not centered in the revival movement.
 *
 * DIRECTIONALITY LOGIC:
 *   The hebrew_revival_movement holds agenda-setter role and benefits from the constraint (sets its terms, controls institutions enforcing it); directionality is low (near beneficiary end, d~0.2). The jewish_settler_community_palestine benefits directly (native status, cultural legitimacy, institutional preference) with constrained exit (tied to the settlement project); directionality is low-moderate (d~0.35). The yiddish_speaking_diaspora and ladino_speaking_sephardic_diaspora are victims (suppressed, delegitimated) with identity-locked exit (cannot abandon their languages without severing cultural identity); directionality is very high (d~0.85–0.95). The liturgical_scholars and literary_advocates are excluded rather than extracted from, but the constraint's operation depends on suppressing their reading frames; directionality is constrained (they cannot exit the constraint because it negates their epistemic claims, but they are not direct payers; d~0.70). No overrides are necessary: the structural data produces accurate directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The native-generation reading sits at high risk of mandatrophy: the founding problem (how to reconstitute Hebrew as a living language in a multilingual diaspora) is increasingly moot by T90 because Hebrew IS generatively native for Israeli-born speakers. The constraint shifts from solving the founding problem to defending the exclusion of competing readings (liturgical, literary) and suppressing rival vernaculars. By T120, the constraint persists primarily through institutional inertia and the political commitment to linguistic nationalism rather than because the founding problem remains acute. The theater_ratio stays low because the linguistic function is real (native speakers do generatively produce speech), but the suppression of alternative readings becomes the dominant enforcement activity. A strong case can be made that the constraint has resolved its founding problem and now operates as extraction dressed in linguistic authenticity — exactly the mandatrophy signature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    linguistic_criterion_vs_political_commitment,
    'Is the native-generation criterion a linguistically defensible definition of what makes a language living, or a retrospectively imposed political standard that selects for Hebrew and against Yiddish/Ladino?',
    'Historical linguistic analysis of how revivalists actually used the criterion: did they apply it consistently to all languages, or selectively to Hebrew? Comparison with other language revivals (Irish, Welsh, Catalan) to see if native generative speech is a universal standard or peculiar to the Hebrew case.',
    'If the criterion is purely political, the native-generation reading is exposed as extraction dressed in linguistic authenticity, and the suppression of Yiddish/Ladino is delegitimized. If it is linguistically defensible, the reading retains stronger claim to authenticity, though the suppression may still be extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(linguistic_criterion_vs_political_commitment, empirical, 'Whether the native-generation criterion is a linguistic fact or a political choice.').

omega_variable(
    strict_reachability_break_legitimacy,
    'Is the strict requirement that Hebrew speech be native (acquired in childhood by native speakers) a legitimate linguistic criterion, or does it arbitrarily exclude written competence, literary production, and recitative fluency as insufficiently authentic?',
    'Linguistic investigation into the actual speech practices of early revivalists (Eliezer Ben-Yehuda, David Yellin, etc.): were they native speakers? Or were they generative non-native speakers whose speech became the model for the native-generation criterion? If the latter, the criterion is retrospectively applied to exclude its own originators.',
    'If early revivalists were learned non-native speakers, the native-generation criterion is anachronistic and the strict-reachability break is indefensible. The reading would lose its foundational authority claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strict_reachability_break_legitimacy, empirical, 'Whether the native-speaker requirement is historically consistent or retrospectively imposed.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression of Yiddish/Ladino structural (external barriers: education policy, legal restrictions, institutional preference) or internalized (Yiddish/Ladino speakers have come to believe their languages are inferior or obstacles to national revival)?',
    'Post-suppression trajectory analysis: if Yiddish/Ladino speakers regain institutional support (reversals of education policy, cultural revival programs), does suppression persist or diminish? If it persists, the suppression is partially internalized.',
    'If suppression is purely structural, policy reversal could quickly restore the languages. If internalized, the constraint has become self-sustaining even after external enforcement is removed. This affects the extractiveness calculation: internalized suppression is more extractive because it persists after the enforcement mechanism is disabled.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Suppression mechanism: structural barriers versus internalized belief.').

omega_variable(
    reading_foreclosure_consensus,
    'Do the three sibling readings (native_generation, liturgical_continuity, literary_revival) truly foreclose each other logically, or is there an ambient framework in which all three could be held as partial truths?',
    'Theoretical analysis: can a language be said to ''live'' through multiple modalities simultaneously (native speech AND liturgical recitation AND literary production)? If yes, the readings influence rather than foreclose each other.',
    'If foreclosure is real, the native-generation reading''s institutionalization represents a genuine epistemic choice with real costs (suppression of alternatives). If the readings are reconcilable, the suppression is harder to justify as necessary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_consensus, conceptual, 'Whether the three readings logically foreclose each other or could coexist as complementary truths.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_generation_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__native_generation_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t20, hebrew_living_language__native_generation_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement_basis(hebr_tr_t20, observed).
narrative_ontology:measurement(hebr_tr_t40, hebrew_living_language__native_generation_reading, theater_ratio, 40, 0.24).
narrative_ontology:measurement_basis(hebr_tr_t40, observed).
narrative_ontology:measurement(hebr_tr_t60, hebrew_living_language__native_generation_reading, theater_ratio, 60, 0.27).
narrative_ontology:measurement_basis(hebr_tr_t60, observed).
narrative_ontology:measurement(hebr_tr_t90, hebrew_living_language__native_generation_reading, theater_ratio, 90, 0.28).
narrative_ontology:measurement_basis(hebr_tr_t90, observed).
narrative_ontology:measurement(hebr_tr_t120, hebrew_living_language__native_generation_reading, theater_ratio, 120, 0.28).
narrative_ontology:measurement_basis(hebr_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__native_generation_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t20, hebrew_living_language__native_generation_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(hebr_be_t20, observed).
narrative_ontology:measurement(hebr_be_t40, hebrew_living_language__native_generation_reading, base_extractiveness, 40, 0.54).
narrative_ontology:measurement_basis(hebr_be_t40, observed).
narrative_ontology:measurement(hebr_be_t60, hebrew_living_language__native_generation_reading, base_extractiveness, 60, 0.64).
narrative_ontology:measurement_basis(hebr_be_t60, observed).
narrative_ontology:measurement(hebr_be_t90, hebrew_living_language__native_generation_reading, base_extractiveness, 90, 0.68).
narrative_ontology:measurement_basis(hebr_be_t90, observed).
narrative_ontology:measurement(hebr_be_t120, hebrew_living_language__native_generation_reading, base_extractiveness, 120, 0.68).
narrative_ontology:measurement_basis(hebr_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_living_language__native_generation_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(hebr_su_t0, observed).
narrative_ontology:measurement(hebr_su_t20, hebrew_living_language__native_generation_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(hebr_su_t20, observed).
narrative_ontology:measurement(hebr_su_t40, hebrew_living_language__native_generation_reading, suppression_requirement, 40, 0.61).
narrative_ontology:measurement_basis(hebr_su_t40, observed).
narrative_ontology:measurement(hebr_su_t60, hebrew_living_language__native_generation_reading, suppression_requirement, 60, 0.7).
narrative_ontology:measurement_basis(hebr_su_t60, observed).
narrative_ontology:measurement(hebr_su_t90, hebrew_living_language__native_generation_reading, suppression_requirement, 90, 0.72).
narrative_ontology:measurement_basis(hebr_su_t90, observed).
narrative_ontology:measurement(hebr_su_t120, hebrew_living_language__native_generation_reading, suppression_requirement, 120, 0.72).
narrative_ontology:measurement_basis(hebr_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__native_generation_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__native_generation_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% The hebrew_living_language kernel decomposes into three structurally distinct constraint stories, each representing a different reading of what makes Hebrew linguistically living. This story (native_generation_reading) asserts that living language requires native generative daily speech, excluding recitative and literary modalities. The sibling readings (liturgical_continuity_reading and literary_revival_reading) assert alternative criteria. The three readings are logically incompatible within a single framework but empirically coexist as competing truth-claims held by different parties. Each reading has different beneficiaries, victims, and patterns of extraction/suppression. The three stories are linked by affects_constraints to model the contamination dynamics: adoption of one reading delegitimizes the others and shifts institutional resources accordingly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
