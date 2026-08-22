% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: Native-Generation Criterion for Hebrew's Living-Language Status
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This story instantiates the native_generation_reading of the contested
 *   kernel hebrew_living_language: the claim that Hebrew counts as a living
 *   language only when native speakers produce daily speech generatively — a
 *   criterion under which Hebrew ceased to be living when Arabic displaced it
 *   as a spoken vernacular in late antiquity, and became living again only
 *   with the first native-speaking generation raised in the Yishuv
 *   (1890s–1920s). Achieving that status was not passive: the revival
 *   required suppressing the immigrants' own vernaculars — Yiddish above all,
 *   then Ladino and the Judeo-Arabic varieties — through Hebrew-only
 *   schooling, the language wars (culminating in the 1913–1914 Technion
 *   affair and the raids of Gedud Meginei Hasafa), employment and
 *   marriage-market incentives, and finally the melting-pot policies applied
 *   to Mizrahi arrivals. The arrangement solved a real coordination problem —
 *   one shared spoken medium for a polyglot polity — while transferring its
 *   costs onto identifiable vernacular communities. Per Rule 1, this file
 *   authors ONLY this reading: the liturgical and literary readings are
 *   separate constraints with their own ε, beneficiaries, and victims, linked
 *   through network.affects_constraints. The claim/metric split is
 *   deliberate: claimed_type is tangled_rope from the structure (real
 *   coordination function, named payers, active enforcement); the metrics
 *   describe the arrangement's actual operation and are authored
 *   independently of the claim.
 *
 * KEY AGENTS:
 *   - hebrew_language_committee: agenda-setter/administrator (institutional / identity_locked) — sets norms, adjudicates correctness, administers the standard; its members' professional selves are fused with the revival
 *   - zionist_settlement_leadership: primary beneficiary (powerful / constrained) — collects the unified speech community as the deliverable of nation-building
 *   - ashkenazi_yiddish_speakers: primary target (organized / constrained) — bears the sharpest founding-era suppression
 *   - sephardi_ladino_speakers: target (moderate / constrained) — communal institutions converted within a generation
 *   - mizrahi_judeo_arabic_speakers: target (powerless / trapped) — latest arrivals, least consent, no exit
 *   - native_hebrew_first_generation: dual beneficiary/payer (powerless / identity_locked) — gained the language, lost the parental vernaculars
 *   - yiddishist_diaspora_movement: excluded challenger (organized / mobile) — the counter-program that lost the intra-Jewish contest
 *   - rabbinic_traditionalist_authorities: observer (institutional / analytical) — dissented from vernacularizing the sacred tongue without a seat in the project
 *   - comparative_historical_linguists: analytical observer (institutional / analytical) — the outside audit on the reading's self-description
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__native_generation_reading, 0.47).
domain_priors:suppression_score(hebrew_living_language__native_generation_reading, 0.35).
domain_priors:theater_ratio(hebrew_living_language__native_generation_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, extractiveness, 0.47).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(hebrew_living_language__native_generation_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__native_generation_reading, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language__native_generation_reading, "Native-Generation Criterion for Hebrew's Living-Language Status").
narrative_ontology:topic_domain(hebrew_living_language__native_generation_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language__native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__native_generation_reading, 'a96b82ac-a724-4371-adc9-c24ee956a9fd').
narrative_ontology:cs_kernel_codification('a96b82ac-a724-4371-adc9-c24ee956a9fd', distributed).
narrative_ontology:cs_authority_grounding('a96b82ac-a724-4371-adc9-c24ee956a9fd', expertise).
narrative_ontology:cs_interpretation_layer_present('a96b82ac-a724-4371-adc9-c24ee956a9fd').
narrative_ontology:cs_reading_relation('a96b82ac-a724-4371-adc9-c24ee956a9fd', hebrew_living_language__liturgical_continuity_reading, forecloses).
narrative_ontology:cs_reading_relation('a96b82ac-a724-4371-adc9-c24ee956a9fd', hebrew_living_language__literary_revival_reading, forecloses).
narrative_ontology:cs_axiom('a96b82ac-a724-4371-adc9-c24ee956a9fd', foundational, generative_native_speech_necessary_for_liveness).
narrative_ontology:cs_axiom_status(generative_native_speech_necessary_for_liveness, holdable).
narrative_ontology:cs_axiom_grounding('a96b82ac-a724-4371-adc9-c24ee956a9fd', generative_native_speech_necessary_for_liveness, empirically_contingent).
narrative_ontology:cs_axiom('a96b82ac-a724-4371-adc9-c24ee956a9fd', secondary, memorized_recitation_insufficient_for_liveness).
narrative_ontology:cs_axiom_status(memorized_recitation_insufficient_for_liveness, holdable).
narrative_ontology:cs_axiom_grounding('a96b82ac-a724-4371-adc9-c24ee956a9fd', memorized_recitation_insufficient_for_liveness, empirically_contingent).
narrative_ontology:cs_reference_frame('a96b82ac-a724-4371-adc9-c24ee956a9fd', native_speech_community_vitality).
narrative_ontology:cs_drift_state('a96b82ac-a724-4371-adc9-c24ee956a9fd', contemporary_relexification_debate, gap(axiom_overriding, substantial, true)).
narrative_ontology:cs_created_at('a96b82ac-a724-4371-adc9-c24ee956a9fd', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__native_generation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, zionist_settlement_leadership).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, hebrew_language_committee).
narrative_ontology:constraint_beneficiary(hebrew_living_language__native_generation_reading, native_hebrew_first_generation).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, ashkenazi_yiddish_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, sephardi_ladino_speakers).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, mizrahi_judeo_arabic_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(hebrew_living_language__native_generation_reading, native_hebrew_first_generation).
narrative_ontology:constraint_vindicates(hebrew_living_language__native_generation_reading, generative_nativeness_criterion).
narrative_ontology:constraint_vindicates(hebrew_living_language__native_generation_reading, language_revival_possibility_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Founded in 1890 as the Hebrew Language Committee and chartered as the Academy of the Hebrew Language in 1953, it coins terminology, fixes orthography and pronunciation norms, and rules on correctness in textbooks, broadcasting, and state documents. Its members built their careers inside the revival — lexicographers, grammarians, teachers whose life work is the language itself. It receives state funding and institutional standing; stepping outside the project would mean dissolving the profession it constitutes.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, hebrew_language_committee, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__native_generation_reading, hebrew_language_committee, beneficiary).

% The Yishuv's national institutions — the Va'ad Leumi, the Hebrew school boards, later the state ministries and the army — funded Hebrew-only schooling, ran Hebrew-language youth movements, and made Hebrew the language of workplace, court, and barracks. A single shared tongue was the deliverable promised to the national project; German was seriously weighed earlier (the Technion taught in German until the 1913 language war), but after committing to Hebrew, reversing course became politically impossible. They collect the unified speech community as an accomplished fact.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, zionist_settlement_leadership, beneficiary,
    powerful, generational, constrained, national).

% Children raised in Hebrew-only kindergartens, schools, and — in some settlements — deliberately Hebrew-speaking households, sometimes separated from parents to prevent vernacular transmission. They gained native command of the national language and unconditional membership in the polity it anchors. What flowed away from them was the parents' tongue: many grew up unable to converse with grandparents, cut off from the literature and humor of the home language. The language of their thought is not one they can put down.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, native_hebrew_first_generation, beneficiary,
    powerless, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language__native_generation_reading, native_hebrew_first_generation, payer).

% The largest immigrant speech community of the Second and Third Aliyah. Their children were punished for speaking Yiddish at school; the Gedud Meginei Hasafa raided Yiddish printing houses and burned newspapers in the 1920s; employers and marriage markets priced Hebrew fluency above Yiddish. Many were committed Hebraists who accepted the loss as the price of nationhood; others migrated onward or stayed in diaspora. Inside the Yishuv their organized defenses — the Yiddish press, Bundist circles — fought a losing rear-guard action.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, ashkenazi_yiddish_speakers, payer,
    organized, biographical, constrained, national).

% Judeo-Spanish speakers from Salonika, Smyrna, Jerusalem's Old Yishuv, and the Balkans. Smaller in number and less organized than the Yiddish community, they watched their communal schools and presses convert to Hebrew within a generation; Ladino retreated to the kitchen and the synagogue, then largely to memory. Their own elites often led the conversion, trading the communal tongue for entry into the new Hebrew public sphere.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, sephardi_ladino_speakers, payer,
    moderate, biographical, constrained, regional).

% Arrived in 1948–1964 from Iraq, Yemen, Morocco, Tunisia, Egypt, and Iran into a state where Hebrew primacy was already fixed law and custom. Transit camps, army ulpanim, and Youth Aliyah boarding schools imposed rapid shift on their children; within one generation Judeo-Arabic, Judeo-Persian, and related varieties stopped being transmitted. They had the least choice and the latest consent of any group in the arrangement, and no exit at all — citizenship, housing, and schooling all ran through Hebrew.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, mizrahi_judeo_arabic_speakers, payer,
    powerless, biographical, trapped, regional).

% Bundists, YIVO philologists, Yiddish writers, and diaspora cultural figures who argued that a modern Jewish culture could run on Yiddish plus local languages, and that branding Yiddish 'jargon' was chauvinism. They published, debated, and lobbied from Vilna, Warsaw, New York, and Berlin — outside the Yishuv's decision-making bodies entirely. Their counter-program lost the intra-Jewish contest before the Holocaust destroyed its demographic base.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, yiddishist_diaspora_movement, excluded,
    organized, generational, mobile, continental).

% Segments of the rabbinic establishment, in Palestine and the diaspora, objected to using lashon ha-kodesh — the holy tongue — for commerce, journalism, and nationalist politics, and to the secular pronunciation innovations that accompanied it. They issued rulings and letters and preached against the vernacularization, and were answered with ridicule by the Hebraist press. They held no seat in the revival's institutions; their dissent registered as background noise to the project.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, rabbinic_traditionalist_authorities, observer,
    institutional, civilizational, analytical, continental).

% Academic linguists who classify Hebrew's vitality, date the death of Hebrew as a spoken vernacular, document the phonological and morphological distance between Israeli Hebrew and the Tiberian/Mishnaic attestation, and — in the relexification school — dispute that the result is the same language at all. They measure from outside the national project; their journals and conferences are where the reading's self-description gets audited.
narrative_ontology:constraint_stakeholder(hebrew_living_language__native_generation_reading, comparative_historical_linguists, observer,
    institutional, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__native_generation_reading, zionist_settlement_leadership).
narrative_ontology:fixing_cost_class(hebrew_living_language__native_generation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of a polyglot immigrant polity: administration, defense, courts, schooling, and markets need one shared spoken medium. Producing it through mass native acquisition solved the medium problem permanently instead of maintaining permanent translation infrastructure among Yiddish, Ladino, Judeo-Arabic, Russian, Polish, and German speech communities.
% TRANSFER_FUNCTION: Moves communicative labor and cultural capital: each immigrant household must acquire Hebrew and cease transmitting its vernacular to children; educational access, employment, and national belonging flow toward Hebrew competence; fluency in Yiddish, Ladino, or Judeo-Arabic converts from community capital into a liability marked as exile residue. The net transfer runs from the vernacular speech communities to the Hebrew-speaking collective and its administering institutions.
% ABSENT_VOICES: Yiddishist and Sephardi cultural organizations (the Bund, YIVO, the Ladino press networks) would object that multilingual co-officiality remained viable and that the nativeness criterion was used to brand living vernaculars as dead weight; they sat outside Yishuv decision-making bodies. The first native-speaking children also had no voice: the choice to raise them monolingually was made by adults on their behalf before they could consent.
% DISAPPEARANCE_RATIONALE: Without the native-generation standard and its enforcement, the Yishuv plausibly remains a multilingual society with German or Yiddish as a high-prestige koine — the Technion-era German option was live until 1913 — and Yiddish and Ladino continue intergenerational transmission. The Academy's adjudication of 'living' status loses its object, and Israeli linguistic identity reorganizes around whichever medium wins the open contest.
% FOUNDING_PROBLEM: A stateless nation-building project drawing immigrants from mutually unintelligible speech communities needed a unifying national language, and chose to restore Hebrew as the spoken national tongue rather than adopt a European language of wider communication.
% FOUNDING_PROBLEM_CORROBORATION: Sources outside the beneficiary set attest both sides of the contest. Demographic and pedagogical records (school censuses, British Mandate education reports) attest that the founding problem was real and that native generative acquisition occurred from the 1890s onward. Comparative-historical linguists attest the reconstruction break — Modern Hebrew phonology is not continuous with the Tiberian/Mishnaic attestation. Relexification-school linguists (Wexler, Zuckermann) dispute that the result is the same language revived. Rabbinic traditionalists, also outside the beneficiary set, attest the rupture with liturgical continuity. No source disputes that the founding problem itself existed.
narrative_ontology:disappearance_verdict(hebrew_living_language__native_generation_reading, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_living_language__native_generation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__native_generation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_living_language__native_generation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__native_generation_reading, 0.47, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.47 (the endpoint of the series): the standing arrangement transfers communicative labor and cultural capital from vernacular communities to the Hebrew-speaking collective, but the transfer bought a coordination good the whole polity consumes, so ε sits well below snare territory. Suppression (0.35 endpoint) is the residue of machinery that peaked around 1930–1950: school punishment for Yiddish, newspaper raids, hiring discrimination, then melting-pot boarding schools for Mizrahi children; active coercion decayed once the norm internalized, leaving stigma that is part structural, part internalized (see omega suppression_mechanism_internalization). Theater ratio (0.34) is low-to-moderate: the coordination function is performed daily by millions of speakers; the performative share is the Academy's norm-adjudication ritual and anniversary rhetoric, which has grown as enforcement faded. Accessibility collapse (0.5): within the Yishuv and the state, the alternatives (the German option, Yiddish co-officiality) were foreclosed by deliberate campaign, but the collapse was never universal — diaspora multilingualism persisted and the definitional alternative (adopting a sibling reading of liveness) remains live in scholarship, so this is not a natural-law-style collapse. Resistance (0.6): the Yiddishist movement, the Bundist press, Sephardi communal schools, parents persisting in the vernacular at home, and contemporary heritage-language reclamation all mounted organized opposition; a Yiddishist-plus-Sephardi coalition was structurally possible and was defeated as much by exogenous catastrophe (the destruction of the Yiddish heartland) as by the arrangement's own enforcement. The three metric series share one eight-point grid (t=0..135, approximately 1890–2025) so temporal analysis samples every metric at every point; suppression_requirement is authored because this story specifically traces enforcement-capacity build-up (0.30→0.72) and decay (→0.35), not merely extraction drift. Note on scaling: suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and scope, in the engine's computation. The record shows a ratchet-then-decay arc rather than oscillation, so no cyclical grid is authored.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats compute an extraction-dominated classification: from inside a Yiddish- or Ladino-speaking household the arrangement is a machine that made their language a marker of shame and severed them from their grandparents. The agenda-setter and beneficiary seats compute a coordination-dominated classification: from the committee and the national institutions the same structure is the deliberate construction of a people's shared tongue — an achievement with few parallels in language history. The first native generation computes both at once: they hold the prize and the loss in a single biography, which is why their seat is authored dual (beneficiary + payer) with identity_locked exit — the identity-lock here is relational and constitutive, since the language of their thought is the arrangement's product and cannot be exited without dissolving the self that thinks in it. The committee's lock is professional: the institution has become its function, and its members' careers, terminological authority, and standing exist only inside the project. Inter-institutionally, the committee (norm producer) and the settlement leadership (norm consumer and funder) sit at the same nominal institutional power but different directionalities: the committee collects standing and budget from administering the standard; the leadership collected the political good the standard produced. The engine derives these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (zionist_settlement_leadership, hebrew_language_committee, native_hebrew_first_generation) drive those seats toward the beneficiary end of d; victim declarations (the three vernacular communities) drive them toward the target end, amplified by constrained and trapped exit and moderated nowhere by arbitrage-grade mobility. The dual seat (native_hebrew_first_generation) is the delicate case: a beneficiary-first derivation would push its d near 0.1, ignoring that its native command was purchased with the coerced loss of the parental vernaculars — its situation declares both flows, so its d should sit near symmetric (approximately 0.45). The directionality_overrides mechanism keys on power atoms rather than agent names, and another powerless agent (mizrahi_judeo_arabic_speakers) must remain near-full-target, so no override is authored; the dual-role declaration is left to carry the symmetry, and this note records the expectation so a mis-derived d is visible in the output. Scope: the arrangement operates at national scope, where verification of 'voluntary' language shift is difficult, which modestly amplifies effective extraction on the payer seats. Identity_coordination is declared as the coordination type because the arrangement's distinctive function is membership adjudication — who speaks Hebrew natively, hence who belongs — and the gaming risk flagged for that type (identity narratives covering extraction) is exactly what the victim declarations and the multilingual-alternative omega test.
 *
 * MANDATROPHY ANALYSIS:
 *   Tangled-rope classification prevents two symmetrical mislabels. Reading the arrangement as pure snare erases the coordination achievement — a polyglot polity did acquire a working shared medium, and the criterion defining its liveness is not mere cover. Reading it as pure rope erases the victims — Yiddish and Ladino did not die of neglect; they were killed by policy, and the killing was the plan, not a side effect. On mandatrophy: the enforcement mandate (active suppression of vernacular transmission) has largely atrophied — coercion became unnecessary once the norm internalized and the vernacular demographic bases were destroyed — while the adjudication function (deciding what counts as living Hebrew) persists and has grown more performative relative to enforcement, which the rising theater_ratio series records. The R5 interview records the founding problem as contested rather than dead: the integration problem the arrangement solved is substantively gone, but the genealogical question the reading answers — is the result continuous Hebrew? — is exactly what the relexification school contests, so the arrangement's mandate decays into adjudication without resolving into obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is the native_generation_reading of kernel hebrew_living_language; what structural deltas would the sibling readings (liturgical_continuity_reading, literary_revival_reading) produce?',
    'Generate the sibling stories and compare victim sets, epsilon, and enforcement data: the liturgical reading implies near-zero extraction (recitation suppresses nothing), the literary reading implies low-to-moderate extraction (print networks, no vernacular suppression), while this reading carries the vernacular-suppression victim set.',
    'Classification is reading-indexed: the same historical episode classifies as coordination-dominant under the liturgical reading and as tangled-rope-to-snare-leaning under this one; cross-reading comparison locates the disagreement in the liveness criterion, not in the facts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this story is one of three readings of the hebrew_living_language kernel.').

omega_variable(
    revival_or_relexification,
    'Is native-generative Modern Hebrew a revived continuation of Hebrew, or a Euro-Asian hybrid with Hebrew lexicon (the Wexler/Zuckermann relexification thesis)?',
    'Comparative typology of structures acquired natively by the first generations against donor-substrate predictions (Yiddish, Polish, Russian, German, Ladino, Arabic); acquisition-order and creole-benchmark studies; phonological reconstruction audits against the Tiberian/Mishnaic attestation.',
    'If relexification is right, the reading''s continuity premise breaks: the arrangement built a new language and called it revival, which raises the reconstruction cost charged to the vernacular communities and pushes the classification toward snare; if continuity holds, the extraction stands as the price of a genuine revival.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(revival_or_relexification, empirical, 'Whether the revival was continuation or reconstruction-in-disguise.').

omega_variable(
    multilingual_alternative_viability,
    'Could the Yishuv have coordinated a modern polity while keeping Yiddish, Ladino, and Judeo-Arabic as functioning co-vernaculars, or did the coordination problem make monolingual shift necessary?',
    'Comparative analysis of multilingual polities absorbing comparable immigrant diversity (Switzerland, Belgium, Singapore''s bilingual management); archival reconstruction of the German-option episodes (Technion 1913, Hilfsverein school wars) to test whether a non-Hebrew koine was administratively viable.',
    'If a multilingual equilibrium was reachable, the suppression component exceeds coordination necessity and effective extraction rises toward snare territory; if the equilibrium was unreachable, the suppression tracks genuine coordination cost and the tangled-rope reading stabilizes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multilingual_alternative_viability, empirical, 'Counterfactual viability of multilingual coordination for the Yishuv.').

omega_variable(
    suppression_mechanism_internalization,
    'How much of the measured suppression is structural enforcement versus internalized stigma that persists after enforcement ended?',
    'Post-enforcement trajectory of vernacular attitudes: heritage-language reclamation cohorts (Yiddish and Ladino revival classes, Haketia documentation projects) reporting residual shame; intergenerational transmission surveys after the coercive mechanisms were dismantled.',
    'If suppression persists after the enforcement mechanism is removed, effective suppression is higher than the structural measure suggests — the vernacular communities carried the constraint with them after exit became possible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    victim_set_boundary,
    'Does this reading''s extraction ledger include the post-1948 suppression of Mizrahi Judeo-Arabic and other heritage languages, or only the founding-era Yiddish/Ladino suppression?',
    'Trace enforcement-lineage continuity: determine whether melting-pot schooling and ulpan policy are the same arrangement as the language wars (same institutions, same criterion, extended population) or a distinct arrangement deserving its own story.',
    'Including the Mizrahi wave extends the victim set and raises epsilon toward the interval peak; excluding it splits the corpus into two linked stories with different victim sets — the epsilon-invariance discipline decides.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_boundary, conceptual, 'Boundary of the victim set across the arrangement''s two enforcement waves.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__native_generation_reading, 0, 135).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__native_generation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t20, hebrew_living_language__native_generation_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement_basis(hebr_tr_t20, observed).
narrative_ontology:measurement(hebr_tr_t40, hebrew_living_language__native_generation_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(hebr_tr_t40, observed).
narrative_ontology:measurement(hebr_tr_t60, hebrew_living_language__native_generation_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t60, observed).
narrative_ontology:measurement(hebr_tr_t80, hebrew_living_language__native_generation_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement_basis(hebr_tr_t80, observed).
narrative_ontology:measurement(hebr_tr_t100, hebrew_living_language__native_generation_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement_basis(hebr_tr_t100, observed).
narrative_ontology:measurement(hebr_tr_t120, hebrew_living_language__native_generation_reading, theater_ratio, 120, 0.31).
narrative_ontology:measurement_basis(hebr_tr_t120, observed).
narrative_ontology:measurement(hebr_tr_t135, hebrew_living_language__native_generation_reading, theater_ratio, 135, 0.34).
narrative_ontology:measurement_basis(hebr_tr_t135, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__native_generation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t20, hebrew_living_language__native_generation_reading, base_extractiveness, 20, 0.45).
narrative_ontology:measurement_basis(hebr_be_t20, observed).
narrative_ontology:measurement(hebr_be_t40, hebrew_living_language__native_generation_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement_basis(hebr_be_t40, observed).
narrative_ontology:measurement(hebr_be_t60, hebrew_living_language__native_generation_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(hebr_be_t60, observed).
narrative_ontology:measurement(hebr_be_t80, hebrew_living_language__native_generation_reading, base_extractiveness, 80, 0.6).
narrative_ontology:measurement_basis(hebr_be_t80, observed).
narrative_ontology:measurement(hebr_be_t100, hebrew_living_language__native_generation_reading, base_extractiveness, 100, 0.52).
narrative_ontology:measurement_basis(hebr_be_t100, observed).
narrative_ontology:measurement(hebr_be_t120, hebrew_living_language__native_generation_reading, base_extractiveness, 120, 0.49).
narrative_ontology:measurement_basis(hebr_be_t120, observed).
narrative_ontology:measurement(hebr_be_t135, hebrew_living_language__native_generation_reading, base_extractiveness, 135, 0.47).
narrative_ontology:measurement_basis(hebr_be_t135, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_living_language__native_generation_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(hebr_su_t0, observed).
narrative_ontology:measurement(hebr_su_t20, hebrew_living_language__native_generation_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement_basis(hebr_su_t20, observed).
narrative_ontology:measurement(hebr_su_t40, hebrew_living_language__native_generation_reading, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(hebr_su_t40, observed).
narrative_ontology:measurement(hebr_su_t60, hebrew_living_language__native_generation_reading, suppression_requirement, 60, 0.65).
narrative_ontology:measurement_basis(hebr_su_t60, observed).
narrative_ontology:measurement(hebr_su_t80, hebrew_living_language__native_generation_reading, suppression_requirement, 80, 0.5).
narrative_ontology:measurement_basis(hebr_su_t80, observed).
narrative_ontology:measurement(hebr_su_t100, hebrew_living_language__native_generation_reading, suppression_requirement, 100, 0.4).
narrative_ontology:measurement_basis(hebr_su_t100, observed).
narrative_ontology:measurement(hebr_su_t120, hebrew_living_language__native_generation_reading, suppression_requirement, 120, 0.37).
narrative_ontology:measurement_basis(hebr_su_t120, observed).
narrative_ontology:measurement(hebr_su_t135, hebrew_living_language__native_generation_reading, suppression_requirement, 135, 0.35).
narrative_ontology:measurement_basis(hebr_su_t135, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__native_generation_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__liturgical_continuity_reading).
narrative_ontology:affects_constraint(hebrew_living_language__native_generation_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the revival of Hebrew' conflates three structurally distinct claims, decomposed per the epsilon-invariance principle into a constraint family: liturgical_continuity_reading (recitation sustains liveness; negligible extraction, no vernacular suppression), literary_revival_reading (written generative competence suffices; low-to-moderate extraction), and this file, native_generation_reading (daily generative native speech required; moderate extraction with an explicit vernacular-suppression victim set). Each member carries its own epsilon, beneficiaries, and victims. This reading is the downstream-contested member: the relexification critique attacks its continuity premise while leaving the siblings untouched, and its necessary-condition axiom forecloses both siblings within any single framework even though all three coexist across parties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
