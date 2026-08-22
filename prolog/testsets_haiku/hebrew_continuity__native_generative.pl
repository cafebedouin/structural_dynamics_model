% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__native_generative
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-13
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: hebrew_continuity__native_generative
 *   human_readable: Hebrew Language Continuity via Native Speaker Generative Use
 *   domain: sociolinguistics/language_revitalization
 *
 * SUMMARY:
 *   Under the native-generative reading of Hebrew continuity, the language
 *   'lives' only when acquired as a first language by children raised in
 *   daily immersion, with speakers' intuition and ordinary generative use as
 *   the sole arbiter of correctness. This reading emerged with Zionist
 *   nation-building in the 20th century and became institutionalized in
 *   Israeli schools, the Academy of the Hebrew Language, and state media. The
 *   reading's core claim is that Hebrew cannot survive diaspora, ritual
 *   transmission, or formal adult acquisition — that the 2,000-year history
 *   of non-native transmission is linguistically dead even if textually
 *   preserved. The constraint extracts authority from communities whose
 *   transmission pathways do not fit the L1 native-speaker model and
 *   concentrates it in Israeli state institutions and native speakers. The
 *   measured extraction (0.68) reflects how thoroughly the standard
 *   suppresses alternative legitimacies; the theater ratio (0.41) reflects
 *   that the scientific-sounding criterion hides ideological exclusion.
 *
 * KEY AGENTS:
 *   - Israeli native Hebrew speakers: primary beneficiaries, set language standards through ordinary use
 *   - Zionist state ideology: non-agent beneficiary, vindicated by native-generative standard
 *   - Israeli state education and Academy of the Hebrew Language: agenda setters, enforce the criterion
 *   - Liturgical Hebrew communities: victims, transmission pathways reclassified as dead
 *   - Diaspora non-native speakers: victims, permanently excluded from authentic speaker status
 *   - Linguistic scholars and alternative transmission advocates: excluded, prevented from institutional parity
 *   - Theoretical linguistics community: observers, measure whether the criterion is scientifically necessary
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__native_generative, 0.68).
domain_priors:suppression_score(hebrew_continuity__native_generative, 0.72).
domain_priors:theater_ratio(hebrew_continuity__native_generative, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, extractiveness, 0.68).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(hebrew_continuity__native_generative, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__native_generative, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__native_generative, "Hebrew Language Continuity via Native Speaker Generative Use").
narrative_ontology:topic_domain(hebrew_continuity__native_generative, "sociolinguistics/language_revitalization").

domain_priors:requires_active_enforcement(hebrew_continuity__native_generative).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__native_generative, '6ee73106-781a-4440-9b2a-43b7662c2f5b').
narrative_ontology:cs_kernel_codification('6ee73106-781a-4440-9b2a-43b7662c2f5b', distributed).
narrative_ontology:cs_authority_grounding('6ee73106-781a-4440-9b2a-43b7662c2f5b', extraction).
narrative_ontology:cs_interpretation_layer_present('6ee73106-781a-4440-9b2a-43b7662c2f5b').
narrative_ontology:cs_reading_relation('6ee73106-781a-4440-9b2a-43b7662c2f5b', hebrew_continuity__liturgical_preservation, forecloses).
narrative_ontology:cs_reading_relation('6ee73106-781a-4440-9b2a-43b7662c2f5b', hebrew_continuity__bridge_pidginized, forecloses).
narrative_ontology:cs_axiom('6ee73106-781a-4440-9b2a-43b7662c2f5b', foundational, native_speaker_primacy_for_authenticity).
narrative_ontology:cs_axiom_status(native_speaker_primacy_for_authenticity, holdable).
narrative_ontology:cs_axiom_grounding('6ee73106-781a-4440-9b2a-43b7662c2f5b', native_speaker_primacy_for_authenticity, empirically_contingent).
narrative_ontology:cs_axiom('6ee73106-781a-4440-9b2a-43b7662c2f5b', foundational, daily_generative_use_necessity).
narrative_ontology:cs_axiom_status(daily_generative_use_necessity, holdable).
narrative_ontology:cs_axiom_grounding('6ee73106-781a-4440-9b2a-43b7662c2f5b', daily_generative_use_necessity, empirically_contingent).
narrative_ontology:cs_reference_frame('6ee73106-781a-4440-9b2a-43b7662c2f5b', hebrew_native_speaker_standard).
narrative_ontology:cs_drift_state('6ee73106-781a-4440-9b2a-43b7662c2f5b', contemporary_post_state_solidification, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6ee73106-781a-4440-9b2a-43b7662c2f5b', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__native_generative, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, israeli_hebrew_speaking_community).
narrative_ontology:constraint_beneficiary(hebrew_continuity__native_generative, zionist_nation_state_ideology).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, liturgical_hebrew_only_communities).
narrative_ontology:constraint_victim(hebrew_continuity__native_generative, diaspora_jewish_communities_non_native_speakers).
narrative_ontology:constraint_vindicates(hebrew_continuity__native_generative, hebrew_language_resurrection_doctrine).
narrative_ontology:constraint_vindicates(hebrew_continuity__native_generative, native_speaker_primacy_in_language_vitality).
narrative_ontology:constraint_vindicates(hebrew_continuity__native_generative, daily_vernacular_necessity_for_language_survival).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Native speakers in Israel whose children acquire Hebrew as a first language through daily immersion. The constraint treats their practice — ordinary generative use — as the sole criterion for Hebrew's authentic continuity. They set language standards in schools, media, and public institutions, implicitly devaluing non-native acquisition pathways. Their status as primary speakers and rule-setters depends on maintaining this standard.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, israeli_hebrew_speaking_community, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__native_generative, israeli_hebrew_speaking_community, agenda_setter).

% The ideological commitment that Hebrew national belonging requires native fluency acquired through daily immersion and child-rearing in the territorial state. The constraint vindicates this doctrine by structurally excluding other acquisition pathways as insufficient. Non-agent entry: a doctrine that collects no rents but benefits from the constraint's operation.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, zionist_nation_state_ideology, beneficiary,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_non_agent(hebrew_continuity__native_generative, zionist_nation_state_ideology).

% Jewish communities — Ashkenazi, Sephardic, Mizrahi populations worldwide, historically Orthodox leadership — who maintained Hebrew through ritual recitation, textual study, and formal prayer. Their generations of transmitted knowledge and liturgical fluency are classified under this reading as 'dead' or 'inauthentic' because they lack the daily vernacular generative competence criterion. They experience pressure to abandon their transmission pathway and re-acquire Hebrew as a native language, or accept marginalization as speakers.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, liturgical_hebrew_only_communities, payer,
    moderate, biographical, constrained, global).

% Diaspora Jews who acquire Hebrew as a second or third language through formal study, weekend schools, summer camps, or adult immersion programs. Under the native-generative reading, their acquisition is classified as inauthentic no matter how proficient they become. Their children, if raised outside Israel, do not become native speakers and thus their own children cannot transmit Hebrew natively. The constraint locks them into a permanent 'non-native' status and excludes their transmission lineages from counting as authentic Hebrew continuity.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, diaspora_jewish_communities_non_native_speakers, payer,
    powerless, biographical, identity_locked, global).

% Schools, media authorities, and language regulators (Academy of the Hebrew Language) that enforce the native-generative standard through curriculum, media production, and official language policy. They define what counts as correct Hebrew, standardize phonology and lexicon around native-speaker norms, and create institutional pressure on non-native speakers to conform. They administer the constraint and prevent alternative legitimacies from achieving institutional parity.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, israeli_state_education_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% The Academy of the Hebrew Language sets official standards for lexical innovation and phonological correctness, based on native-speaker intuition and documented usage. Their technical role is language standardization; their structural role is enforcing that innovations not grounded in native speakers' generative use are classified as artificial, borrowed, or invalid. They have institutional power to exclude or marginalize alternative standardization pathways.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, hebrew_language_academy, agenda_setter,
    institutional, generational, analytical, national).

% Scholars and community leaders who argue that Hebrew continuity can be maintained through liturgical transmission, bridge-language contact, formal education, and multilingual acquisition. They would argue that the native-generative criterion is unnecessarily restrictive, historically revisionist, and exclusionary. They are systematically excluded from institutional language-setting bodies and their theoretical positions are marginalized as non-scientific or ideologically motivated by institutional actors defending the native-generative standard.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, linguistic_scholars_non_native_advocates, excluded,
    moderate, biographical, constrained, global).

% The accumulated linguistic lineages — Ashkenazi, Sephardic, Mizrahi, Yemenite traditions of Hebrew transmission — that sustained the language for 2,000 years of diaspora through ritual, textual study, and formal pedagogy. Under this reading, their transmission pathways are devalued as 'inauthentic' and their accumulated knowledge is reframed as dead-language literacy rather than living language use.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, historical_hebrew_transmission_networks, excluded,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(hebrew_continuity__native_generative, historical_hebrew_transmission_networks).

% Academic linguists who study language revitalization, historical linguistics, and sociolinguistics. They observe the native-generative reading as one among competing models of language continuity and vitality, each grounded in different linguistic theories and empirical criteria. They can evaluate whether the native-generative criterion is scientifically necessary or ideology-justified.
narrative_ontology:constraint_stakeholder(hebrew_continuity__native_generative, theoretical_linguistics_community, observer,
    organized, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__native_generative, israeli_hebrew_speaking_community).
narrative_ontology:fixing_cost_class(hebrew_continuity__native_generative, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a single criterion for what counts as authentic, living Hebrew: daily generative use by native speakers of a complete age-cohort (children acquiring as L1). Solves the coordination problem of how to define language continuity when transmission has been broken and must be reconstructed — eliminates ambiguity about which forms, speakers, and acquisition pathways count as 'real Hebrew' versus revived, artificial, or liturgical remnants.
% TRANSFER_FUNCTION: Moves prestige, institutional authority, and linguistic legitimacy from diaspora transmission networks and liturgical communities to Israeli native speakers and the Israeli nation-state. The extraction is the forced reclassification of millennia of non-native transmission as 'inauthentic' or 'dead,' stripping communities of authority over their own linguistic heritage. Diaspora speakers and liturgical scholars lose the ability to define what counts as Hebrew knowledge; Israeli native speakers and state institutions gain exclusive authority.
% ABSENT_VOICES: Liturgical scholars, diaspora community leaders, non-native speakers of acquired Hebrew, historical linguistic transmission networks (now reclassified as inauthentic), and linguists who argue alternative criteria for language continuity are systematically excluded from institutional language-setting bodies. They would object that the native-generative standard is historically revisionist, empirically narrow (modern L1 acquisition is not the only mechanism for language continuity), and ideologically loaded to serve nation-state authority. Their exclusion is structural: the constraint's enforcement depends on preventing their arguments from achieving institutional parity.
% DISAPPEARANCE_RATIONALE: If this constraint vanished, Hebrew transmission would immediately pluralize: liturgical communities would regain legitimacy and authority over their transmission traditions; diaspora adult-learners and their institutions would no longer experience stigma for non-native acquisition; alternative standardization criteria (usage across diverse speaker populations, historical documentation, community consensus) would compete with native-speaker norms in language-setting bodies. The territorial state would lose its monopoly on defining authenticity. Hebrew would be understood as living through multiple pathways simultaneously, not just Israeli native-speaker generative use.
% FOUNDING_PROBLEM: Hebrew was nearly extinct as a spoken language by the late 19th century, maintained only through liturgical recitation and formal study. The founding problem was: how to resurrect a language with no continuous native-speaking population, when all historical transmission had been through non-native literacy and ritual use?
% FOUNDING_PROBLEM_CORROBORATION: Israeli institutional actors and the state apparatus attest the founding problem is live and continuing — Hebrew requires constant renewal through native-speaker generative use to avoid reverting to liturgical deadness. Diaspora communities, non-native speakers, and linguists studying historical language continuity attest the founding problem is substantially solved and the standard now persists as a tool of linguistic exclusion and nation-state authority — Hebrew is demonstrably alive through multiple transmission pathways, not dependent on L1 native acquisition. Scholarship on language revitalization (Fishman, Hinton, Hale) and comparative studies of minority languages support the latter reading.
narrative_ontology:disappearance_verdict(hebrew_continuity__native_generative, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__native_generative, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__native_generative, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_continuity__native_generative, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__native_generative, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-high (0.68) and rising through the interval (from 0.42 to 0.68) because the standard's enforcement intensifies as Israeli state institutions solidify control: curriculum standardization accelerates, Academy of the Hebrew Language authority expands, media normalization of native-speaker norms deepens. Suppression is high (0.72) because the constraint's persistence requires active institutional work to exclude rival transmission pathways from legitimacy — liturgical schools must be devalued, diaspora adult learners must be stigmatized, alternative standardization criteria must be marginalized in official bodies. The suppression does not decrease because each generation of enforcement creates new institutional infrastructure (testing standards, textbook gatekeeping, media dialect standardization). Theater is moderate (0.41) and rising because as the standard becomes naturalized, more enforcement activity is theatrical: the Academy of the Hebrew Language presents linguistic decisions as discovered facts about native-speaker intuition, when they are institutional choices to exclude non-native pathways. Accessibility collapse is high (0.79): once Hebrew is defined as requiring L1 native acquisition, diaspora learners have almost no alternative way to become authentic speakers — the alternatives (ritual transmission, formal study, multilingual acquisition) are structurally foreclosed by the definition itself. Resistance is moderate (0.58): diaspora communities and non-native speakers resist the standard, but they lack institutional power to counter Israeli state enforcement; linguistic scholars raise theoretical objections but are marginalized in official bodies.
 *
 * PERSPECTIVAL GAP:
 *   From the Israeli native-speaker and state institutional seat, this constraint is a discovery of what Hebrew authenticity requires — native-speaker intuition is the natural source of language truth. From the liturgical and diaspora seats, the same constraint is visible as a redefinition that stripped their authority and reclassified their centuries of work as dead. The engine should compute type-divergence: the Israeli native-speaker seat perceives rope (genuine coordination of language standards around native practice), while the diaspora victim seats perceive snare (their transmission pathways are trapped, alternatives suppressed, and they cannot exit the constraint without abandoning Hebrew entirely). The perspectival gap is structural: the beneficiary and payer seats have genuinely opposed relationships to the same rule.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli native speakers (institutional power, mobile exit, national scope) are beneficiaries with d near 0.2: the constraint provides them authority over language standards and prestige recognition for their ordinary speech as the criterion for authenticity. They gain from institutional gatekeeping that treats their intuitions as scientific facts. Liturgical Hebrew communities (moderate power, constrained exit, global scope) are victims with d near 0.85: their transmission pathways are structurally devalued, their scholars lose authority, their children's Hebrew is classified as inauthentic no matter how competent. Diaspora non-native speakers (powerless, identity-locked exit, global scope) are victims with d near 0.95: identity-locked because withdrawing from Hebrew acquisition means abandoning a core element of Jewish identity, yet the constraint ensures they can never become authentic speakers in the institutional eyes that matter. The state ideology (institutional power, analytical exit) has d near 0.05 as a pure beneficiary: it receives vindication and authority through the constraint's operation. The theoretical linguistics community (organized power, analytical exit, global scope) sits near d = 0.5 as an observer seat: they document the constraint's operation but lack enforcement power.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — how to resurrect a language with no continuous native-speaking population — was solved by the early 20th century: Hebrew was successfully revived through intensive pedagogy, state investment, and child-rearing in the emergent Israeli community. That problem is now dead: Hebrew has 10+ million native speakers, a literary tradition renewed in daily use, complete phonological and lexical standardization. Yet the native-generative standard persists and intensifies (extraction rising through the interval), now functioning as a tool of exclusion rather than revival. The constraint exhibits mandatrophy: its founding problem is obsolete, yet it persists because it serves a different function (nation-state authority and linguistic exclusion) than the function it was built for (language resurrection). The misalignment between founding problem and current operation is the mandatrophy signal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    native_speaker_criterion_empirical_necessity,
    'Is the native-speaker criterion (L1 acquisition in daily immersion) empirically necessary for language continuity, or is it one sufficient pathway among several?',
    'Comparative linguistics study of successfully revived minority languages (Basque, Irish, Māori, etc.) tracking which transmission pathways are actually necessary and which are contingent. Post-hoc measurement of Hebrew stability: if Hebrew remained robust when exposed to non-native transmission (diaspora adoption, adult learners, multilingual L1 input), the criterion would be shown as over-specified.',
    'If empirically unnecessary, the native-generative criterion is an ideology-justified exclusion, not a linguistic necessity. The constraint would reclassify from tangled_rope (genuinely coordinating language standards + extracting authority) toward snare (pure extraction with coordination cover). Diaspora and liturgical communities would be victims of a false-necessity claim, not beneficiaries of a real coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_speaker_criterion_empirical_necessity, empirical, 'Whether L1 native acquisition is empirically necessary for language vitality or one among sufficient pathways.').

omega_variable(
    reading_boundary_foreclosure_ambiguity,
    'Does the native-generative reading logically foreclose the liturgical_preservation reading, or do they coexist as different institutional pathways that could both sustain Hebrew simultaneously?',
    'Case analysis: could a Hebrew-speaking population sustain the language through BOTH native-speaker generative use in Israel AND liturgical transmission networks in diaspora communities, with mutual recognition of both pathways as legitimate? If yes, the readings coexist; if the native-generative reading logically requires devaluing all non-native pathways, it forecloses the liturgical reading.',
    'If readings are logically incompatible (native-generative forecloses liturgical), the kernel exhibits genuine foreclosure and one reading must be chosen. If they could coexist (mutual legitimacy, complementary functions), the foreclosure is institutional (power-driven exclusion) rather than logical, changing the mandatrophy diagnosis from ''founding problem obsolete, now serving extraction'' to ''institutional monopoly over language authority.''',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_boundary_foreclosure_ambiguity, conceptual, 'Whether readings are logically incompatible or institutionally incompatible (different institutional consequence).').

omega_variable(
    identity_lock_suppression_mechanism,
    'For diaspora non-native speakers, is the suppression of alternative transmission pathways structural (institutional barriers block access to authentic speaker status) or internalized (they have absorbed the native-speaker criterion as the true measure of Hebrew authenticity)?',
    'Longitudinal study of diaspora Hebrew communities that reject the native-generative criterion and develop alternative authentication mechanisms (e.g., claiming authority from scriptural knowledge, community fluency, multilingual acquisition). Do they escape the suppression, or does it persist as internalized self-doubt? Post-exit measurement: do diaspora speakers who reject the constraint and claim non-native pathways as authentic report reduced stigma and increased linguistic confidence?',
    'If suppression is purely structural, communities that collectively reject the criterion should report improved outcomes. If suppression is largely internalized, communities would carry the stigma even after institutional barriers are overcome — the constraint''s persistence would depend on psychological capture, not just institutional power. This differentiates institutional mandatrophy (the state keeps the standard alive) from internalized mandatrophy (communities keep themselves trapped).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_suppression_mechanism, empirical, 'Structural vs. internalized suppression of non-native transmission pathways.').

omega_variable(
    historical_revisionism_in_criterion,
    'Is the native-generative reading a discovery of what Hebrew authenticity always required, or a retroactive redefinition imposed by 20th-century nation-state construction?',
    'Historical analysis of pre-Zionist Hebrew scholarship and transmission: did medieval, early-modern, and 19th-century authorities treat non-native liturgical scholars as legitimate authorities on Hebrew? Did they recognize degrees of authenticity based on acquisition pathway, or did they treat fluency and correctness as criteria independent of whether Hebrew was one''s L1? If pre-modern authorities treated non-native scholars as legitimate authorities, the native-generative criterion is a modern innovation, not a rediscovered truth.',
    'If the criterion is a modern innovation, the constraint''s claim to naturalness (that native-speaker intuition has always been the true measure) is false. The constraint would be more transparently an apparatus of exclusion and nation-state authority-building, weakening any mandate-based justification for its persistence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_revisionism_in_criterion, empirical, 'Whether the native-generative criterion reflects historical reality or modern ideological reconstruction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__native_generative, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hcng_tr_t0, hebrew_continuity__native_generative, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(hcng_tr_t0, observed).
narrative_ontology:measurement(hcng_tr_t8, hebrew_continuity__native_generative, theater_ratio, 8, 0.22).
narrative_ontology:measurement_basis(hcng_tr_t8, observed).
narrative_ontology:measurement(hcng_tr_t16, hebrew_continuity__native_generative, theater_ratio, 16, 0.28).
narrative_ontology:measurement_basis(hcng_tr_t16, observed).
narrative_ontology:measurement(hcng_tr_t24, hebrew_continuity__native_generative, theater_ratio, 24, 0.33).
narrative_ontology:measurement_basis(hcng_tr_t24, observed).
narrative_ontology:measurement(hcng_tr_t32, hebrew_continuity__native_generative, theater_ratio, 32, 0.37).
narrative_ontology:measurement_basis(hcng_tr_t32, observed).
narrative_ontology:measurement(hcng_tr_t40, hebrew_continuity__native_generative, theater_ratio, 40, 0.4).
narrative_ontology:measurement_basis(hcng_tr_t40, observed).
narrative_ontology:measurement(hcng_tr_t50, hebrew_continuity__native_generative, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(hcng_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(hcng_be_t0, hebrew_continuity__native_generative, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(hcng_be_t0, observed).
narrative_ontology:measurement(hcng_be_t8, hebrew_continuity__native_generative, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(hcng_be_t8, observed).
narrative_ontology:measurement(hcng_be_t16, hebrew_continuity__native_generative, base_extractiveness, 16, 0.55).
narrative_ontology:measurement_basis(hcng_be_t16, observed).
narrative_ontology:measurement(hcng_be_t24, hebrew_continuity__native_generative, base_extractiveness, 24, 0.62).
narrative_ontology:measurement_basis(hcng_be_t24, observed).
narrative_ontology:measurement(hcng_be_t32, hebrew_continuity__native_generative, base_extractiveness, 32, 0.66).
narrative_ontology:measurement_basis(hcng_be_t32, observed).
narrative_ontology:measurement(hcng_be_t40, hebrew_continuity__native_generative, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(hcng_be_t40, observed).
narrative_ontology:measurement(hcng_be_t50, hebrew_continuity__native_generative, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(hcng_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(hcng_su_t0, hebrew_continuity__native_generative, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(hcng_su_t0, observed).
narrative_ontology:measurement(hcng_su_t8, hebrew_continuity__native_generative, suppression_requirement, 8, 0.58).
narrative_ontology:measurement_basis(hcng_su_t8, observed).
narrative_ontology:measurement(hcng_su_t16, hebrew_continuity__native_generative, suppression_requirement, 16, 0.62).
narrative_ontology:measurement_basis(hcng_su_t16, observed).
narrative_ontology:measurement(hcng_su_t24, hebrew_continuity__native_generative, suppression_requirement, 24, 0.66).
narrative_ontology:measurement_basis(hcng_su_t24, observed).
narrative_ontology:measurement(hcng_su_t32, hebrew_continuity__native_generative, suppression_requirement, 32, 0.69).
narrative_ontology:measurement_basis(hcng_su_t32, observed).
narrative_ontology:measurement(hcng_su_t40, hebrew_continuity__native_generative, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(hcng_su_t40, observed).
narrative_ontology:measurement(hcng_su_t50, hebrew_continuity__native_generative, suppression_requirement, 50, 0.72).
narrative_ontology:measurement_basis(hcng_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__native_generative, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_continuity__native_generative, 0.12).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__liturgical_preservation).
narrative_ontology:affects_constraint(hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% The hebrew_continuity kernel instantiates three structurally distinct constraints corresponding to different readings of what makes Hebrew 'live.' native_generative asserts that only L1 native acquisition counts; liturgical_preservation asserts that ritual/textual transmission suffices; bridge_pidginized asserts that multilingual contact use maintains vitality. Each reading has different victim sets, different beneficiaries, and different ε values. They compete for institutional authority over what counts as authentic Hebrew. All three are linked here as competing readings of the same kernel. For analysis of the kernel's overall resolution trajectory, consult network.affects_constraints symmetrically.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_continuity__native_generative, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
