% ============================================================================
% CONSTRAINT STORY: living_language_status__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_living_language_status__liturgical_preservation_reading, []).

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
 *   constraint_id: living_language_status__liturgical_preservation_reading
 *   human_readable: Living Language Status — Liturgical Preservation Reading
 *   domain: sociolinguistic/religious/nationalist
 *
 * SUMMARY:
 *   The liturgical preservation reading of 'living language' defines a
 *   language as alive when its canonical sacred texts are continuously
 *   recited, studied, and transmitted through institutional ritual practice —
 *   independent of whether native speakers exist or whether the language is
 *   used in daily secular life. Under this reading, Hebrew remained 'living'
 *   throughout the diaspora because rabbinical communities maintained the
 *   Torah, Mishnah, and Talmud in liturgical use and textual study. This
 *   reading privileges textual and institutional continuity over demographic
 *   and communicative vitality. It benefits rabbinical authority (who become
 *   the sole arbiters of linguistic legitimacy) and diaspora Jewish
 *   communities (who can maintain linguistic identity through ritual practice
 *   without native speech). It extracts from secular modern speakers and
 *   modernizing intellectuals by delegitimizing their speech as desecration
 *   or corruption of the sacred tongue — a necessary practical medium but not
 *   a source of the language's life. The constraint is a KERNEL READING: one
 *   interpretation of what makes a language 'living' among three contested
 *   definitions in the living_language_status kernel. The other readings
 *   (native_generation_reading, literary_continuity_reading) instantiate
 *   different constraints with different beneficiaries, victims, and
 *   extraction profiles. This story models the liturgical reading's structure
 *   in isolation, per the committer frame rules.
 *
 * KEY AGENTS:
 *   - rabbinical_authority_and_interpretive_class: Institutional agenda-setter; controls standards; benefits from monopoly on legitimacy
 *   - secular_hebrew_speech_community: Moderate-power payer; uses language daily but delegitimized; constrained exit (embedded in the speech community)
 *   - modernizing_jewish_intellectuals: Powerful payer and excluded voice; create new literature and thought in Hebrew but are not counted as sources of vitality
 *   - diaspora_jewish_communities: Organized beneficiary; ritual practice provides them linguistic continuity
 *   - nationalist_jewish_movement: Powerful beneficiary; uses the reading to ground national identity in sacred continuity
 *   - secular_jewish_nationalists: Powerful but excluded; contest the reading by asserting native speakers are the true measure
 *   - academic_linguists: Observer seat; can map the reading's inconsistency with empirical vitality but lack leverage over identity-constituting definitions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(living_language_status__liturgical_preservation_reading, 0.42).
domain_priors:suppression_score(living_language_status__liturgical_preservation_reading, 0.68).
domain_priors:theater_ratio(living_language_status__liturgical_preservation_reading, 0.31).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, theater_ratio, 0.31).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(living_language_status__liturgical_preservation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(living_language_status__liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(living_language_status__liturgical_preservation_reading, "Living Language Status — Liturgical Preservation Reading").
narrative_ontology:topic_domain(living_language_status__liturgical_preservation_reading, "sociolinguistic/religious/nationalist").

domain_priors:requires_active_enforcement(living_language_status__liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(living_language_status__liturgical_preservation_reading, '5b52bf2a-b85c-4796-9af7-2e4cd6b875f3').
narrative_ontology:cs_kernel_codification('5b52bf2a-b85c-4796-9af7-2e4cd6b875f3', fixed_text).
narrative_ontology:cs_authority_grounding('5b52bf2a-b85c-4796-9af7-2e4cd6b875f3', lineage).
narrative_ontology:cs_interpretation_layer_present('5b52bf2a-b85c-4796-9af7-2e4cd6b875f3').
narrative_ontology:cs_reading_relation('5b52bf2a-b85c-4796-9af7-2e4cd6b875f3', living_language_status__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('5b52bf2a-b85c-4796-9af7-2e4cd6b875f3', living_language_status__literary_continuity_reading, influences).
narrative_ontology:cs_axiom('5b52bf2a-b85c-4796-9af7-2e4cd6b875f3', foundational, sacred_textual_transmission_constitutes_linguistic_vitality).
narrative_ontology:cs_axiom_status(sacred_textual_transmission_constitutes_linguistic_vitality, holdable).
narrative_ontology:cs_axiom_grounding('5b52bf2a-b85c-4796-9af7-2e4cd6b875f3', sacred_textual_transmission_constitutes_linguistic_vitality, conventional).
narrative_ontology:cs_axiom('5b52bf2a-b85c-4796-9af7-2e4cd6b875f3', foundational, rabbinical_institutional_authority_legitimacy_grounded_in_textual_guardianship).
narrative_ontology:cs_axiom_status(rabbinical_institutional_authority_legitimacy_grounded_in_textual_guardianship, holdable).
narrative_ontology:cs_axiom_grounding('5b52bf2a-b85c-4796-9af7-2e4cd6b875f3', rabbinical_institutional_authority_legitimacy_grounded_in_textual_guardianship, theological).
narrative_ontology:cs_reference_frame('5b52bf2a-b85c-4796-9af7-2e4cd6b875f3', post_temple_exile_dispersed_diaspora).
narrative_ontology:cs_drift_state('5b52bf2a-b85c-4796-9af7-2e4cd6b875f3', modern_hebrew_native_speaker_emergence, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5b52bf2a-b85c-4796-9af7-2e4cd6b875f3', '').
narrative_ontology:cs_kernel_id(living_language_status__liturgical_preservation_reading, living_language_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, rabbinical_authority_and_interpretive_class).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_hebrew_speech_community).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, modernizing_jewish_intellectuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, diaspora_jewish_communities).
narrative_ontology:constraint_beneficiary(living_language_status__liturgical_preservation_reading, nationalist_jewish_movement).
narrative_ontology:constraint_victim(living_language_status__liturgical_preservation_reading, secular_jewish_nationalists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Controls which texts count as canonical, what counts as proper ritual use, and what constitutes legitimate study. Draws institutional authority and social standing from guardianship of the sacred textual corpus and its interpretive traditions. Under this reading, their role expands: they are not merely curators of a living spoken language — they are the sole arbiters of what 'living' means. They set the standard by which the language is measured and certified.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, rabbinical_authority_and_interpretive_class, agenda_setter,
    institutional, civilizational, mobile, national).

% Speaks Hebrew daily in secular contexts (commerce, education, socializing) but is excluded from the 'living language' status as defined by the reading. Their speech is delegitimized as desecration or corruption of the sacred tongue — a necessary practical medium but not a source of the language's vitality. They bear the cost of exclusion from cultural legitimacy: their speech community is rendered invisible in the official narrative of linguistic continuity.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_hebrew_speech_community, payer,
    moderate, biographical, constrained, regional).

% Use Hebrew as a vehicle for contemporary literary, scientific, and philosophical expression (poetry, journalism, secular philosophy). Under this reading, their creative work does not count toward the language's vitality — only liturgical use does. They are partly excluded from the conversation about what makes Hebrew 'living' because the framework explicitly devalues their contributions. They have the intellectual resources and cultural position to contest this reading, but the liturgical reading's institutional backing makes contestation costly.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, modernizing_jewish_intellectuals, payer,
    powerful, biographical, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, modernizing_jewish_intellectuals, excluded).

% Use the liturgical reading as justification for maintaining Hebrew prayer and study practices even where the language is not natively spoken or used in daily life. The reading provides a bridge: Hebrew remains 'alive' through their ritual practice, independent of whether anyone's grandmother spoke it at home. This affords them cultural continuity and religious legitimacy without requiring native-speaker fluency.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).

% Uses the liturgical reading strategically: the claim that Hebrew is alive through ritual transmission supports the Zionist project's assertion that Jewish national identity has linguistic continuity across the diaspora, intact and unbroken through centuries of exile. The reading elevates rabbinical-era Hebrew to the status of the 'true' living language, validating revival efforts and grounding nationalism in sacred textual authority rather than demographic fact.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, nationalist_jewish_movement, beneficiary,
    powerful, generational, mobile, global).

% Want Hebrew revival grounded in native generational transmission and modern literary productivity, not in liturgical recitation by non-speakers. Under the liturgical reading, their vision of a secular Jewish nation-state using Hebrew as a living spoken language is subordinated to a religious authenticity standard. They have institutional and economic power but are excluded from defining 'living language' by a reading that privileges religious transmission.
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, secular_jewish_nationalists, payer,
    powerful, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(living_language_status__liturgical_preservation_reading, secular_jewish_nationalists, excluded).

% Study Hebrew's actual structural evolution, lexical expansion, and usage patterns across contexts. From their analytical seat, the liturgical reading is transparent: it is not a description of a language's vitality but a normative gate excluding certain speakers and uses from counting as legitimate. They can map the reading's inconsistency with empirical linguistic description but have little institutional leverage over identity-constituting definitions like 'living language.'
narrative_ontology:constraint_stakeholder(living_language_status__liturgical_preservation_reading, academic_linguists, observer,
    institutional, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(living_language_status__liturgical_preservation_reading, rabbinical_authority_and_interpretive_class).
narrative_ontology:fixing_cost_class(living_language_status__liturgical_preservation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes and maintains a fixed, canonical textual corpus (the Hebrew Bible and rabbinical interpretive literature) as the authoritative standard of the language, enabling consistent ritual practice, study, and prayer across dispersed communities over centuries. The liturgical reading coordinates around this fixed corpus rather than around living speakers: the coordination problem solved is 'how do we keep the language structurally coherent and recognizable across time and geography' — answered by liturgical recitation and hermeneutic transmission.
% TRANSFER_FUNCTION: Transfers authority and cultural legitimacy from secular speakers to the rabbinical interpretive class and from contemporary spoken innovation to historical textual authority. The reading authorizes the extraction of definitional power: the rabbinical class collects the right to declare what counts as 'living' language, and secular modern speakers bear the cost of delegitimization — their speech is present but rendered secondary or tainted in the hierarchy of linguistic authenticity.
% ABSENT_VOICES: Native speakers of Modern Hebrew in secular contexts are partly excluded from the conversation about the language's vitality (their speech is treated as derivative or corrupt); secular modernizing intellectuals are excluded from the standards-setting process; native-speaker communities from other languages (Arabic, Yiddish, diaspora languages) are excluded as foreign to the sacred corpus. These absent voices would argue that actual usage patterns, generational transmission, and literary productivity are the true measures of linguistic life.
% DISAPPEARANCE_RATIONALE: The liturgical reading itself would disappear as a constraint only if rabbinical institutional authority over language standards dissolved. But the linguistic facts it describes — the corpus of sacred texts, the practices of ritual recitation and study — would persist. What would vanish is the AUTHORITY to declare secular speech delegitimate. The contest: whether the world rearranges toward native-speaker vitality standards (the secular speakers and modernizing intellectuals argue yes) or whether liturgical transmission remains definitive (the rabbinical and nationalist seats argue it would rearrange away from continuity). The verdict is contested precisely because the reading's authority is being challenged.
% FOUNDING_PROBLEM: After the Romans destroyed the Second Temple and dispersed Jewish communities across the Mediterranean and beyond, Hebrew ceased to be a language of daily life for most Jews. The founding problem: How can a language remain 'alive' — retain its authority, identity, and cultural continuity — when native speakers are scattered, when the community itself is scattered, when no child learns it naturally at home? The liturgical reading answers: through ritual recitation and textual study, the language is kept alive in the institutional memory and practice of the rabbinical class and the diaspora communities that follow it.
% FOUNDING_PROBLEM_CORROBORATION: Rabbinical and Jewish institutional authorities attest the founding problem was real and remains partly live (diaspora communities still need a linguistic bond; exile persistence is real). Secular linguists and modernizing intellectuals attest the founding problem is substantially solved — Modern Hebrew is native-transmitted in Israel, actively used in daily speech and contemporary literature. The literary and native-generation readings (the sibling constraints) treat the problem as solved and displaced; the liturgical reading treats the problem as permanently defining. The mismatch is the signal: a founding problem once live (post-exile linguistic continuity) that persists institutionally as a reading even after conditions change (Israel's native speakers).
narrative_ontology:disappearance_verdict(living_language_status__liturgical_preservation_reading, contested).
narrative_ontology:founding_problem_status(living_language_status__liturgical_preservation_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(living_language_status__liturgical_preservation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(living_language_status__liturgical_preservation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(living_language_status__liturgical_preservation_reading, 0.42, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(living_language_status__liturgical_preservation_reading_tests).
:- end_tests(living_language_status__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.42) because the constraint solves a genuine coordination problem (maintaining textual coherence across dispersed communities) but also enforces an asymmetric legitimacy hierarchy. Suppression is substantial (0.68) because the reading requires active exclusion of secular speakers from the category of legitimate vitality-sources; the institutional machinery of religious authority is deployed to prevent secular speech from counting. Theater is rising over the interval (0.15 to 0.31) because as Modern Hebrew becomes an actual native language with native speakers and contemporary literature, the reading's reliance on ritual recitation becomes increasingly performative — the reading persists more to maintain rabbinical authority than to solve the original founding problem of linguistic continuity through exile. Accessibility collapse is high (0.72) because once one accepts the reading's premise (sacred textual transmission = vitality), alternatives disappear: secular speech cannot be vitality, modernization cannot be vitality, only rabbinical-administered liturgical use counts. Resistance is moderate-high (0.58) because the literary and native-generation readings offer explicit alternatives, and secular Hebrew speakers and modernizing intellectuals actively contest the reading — it is not passively accepted. One shared time grid: every metric is authored at 0, 5, 10, 15, 20, 25.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinical institutional seat, the reading is a stabilizing preservation mechanism that kept Hebrew alive through catastrophe — genuine coordination around a fixed corpus. From the secular speaker seat, the same constraint is a delegitimization mechanism that renders their lived speech invisible in official narratives of linguistic vitality — it is extraction masquerading as preservation. From the diaspora communities' seat, the reading is a resource (it allows them to maintain identity through ritual without native fluency). From the secular nationalist seat, it is an obstacle (it subordinates their vision of Hebrew as a modern living language to a religious authentication standard). The engine computes these divergences from the structural data (beneficiary/victim declarations, power differences, exit options, scope differences) — the reading's authority is not universally shared; it is enforced against competing readings and competing speakers.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinical class is the structural beneficiary (d ≈ 0.15): they collect definitional authority and institutional standing from gatekeeping the sacred corpus; they have mobile exit (they could step down from the role but choose not to; they can influence what counts as legitimate). Secular speakers are targets (d ≈ 0.85): their speech is delegitimized as derivative, constrained by the reading's framework (they cannot escape it without leaving the speech community). Modernizing intellectuals are targets (d ≈ 0.75): their creative work is excluded from vitality, though they have arbitrage exit (they can write in other languages or ignore the reading's authority — but the cost is cultural relevance). Diaspora communities are near-beneficiaries (d ≈ 0.20): they benefit from the reading (it preserves their linguistic identity), though they are also somewhat constrained (they depend on rabbinical institutions to maintain the corpus). The directionality overrides are minimal; the structural derivation from beneficiary/victim + exit + power captures the asymmetry accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (linguistic continuity through exile) was real in the post-70 CE period and remained live through the diaspora. But by the 20th century, particularly after 1948 and the founding of Israel, the problem fundamentally changed: Hebrew acquired native speakers, a state apparatus, modern literature, and daily use in secular contexts. The founding problem is now DEAD in factual terms, but the rabbinical reading persists institutionally. The theater_ratio rising from 0.15 to 0.31 captures this: the reading's original function (preserving linguistic coherence across dispersed non-native communities) is increasingly performative; it persists more as a marker of institutional authority and religious identity than as a solution to an active coordination problem. The Tangled Rope classification captures this: there is still genuine coordination (the corpus does remain coherent, diaspora communities do maintain identity), but it is increasingly overlaid with extraction (the reading enforces a hierarchy that benefits the rabbinical class and nationalist movements at the expense of secular speakers). A pure rope would have low suppression and no victims; a pure snare would have no coordination function. The tangled rope sits between, and the rising theater ratio indicates the coordination component is atrophying while the extraction component holds stable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    founding_problem_obsolescence,
    'Is the reading''s founding problem (linguistic continuity through exile without native speakers) still live, or has it been superseded by the empirical fact of native Hebrew speakers in Israel?',
    'Observe whether rabbinical authority still invokes the exile-continuity justification or whether it reframes the reading around other grounds (religious authenticity, textual purity, institutional tradition). Interview rabbinical and nationalist constituencies about why the reading persists despite the changed empirical conditions.',
    'If the founding problem is dead but the reading persists, the constraint is a zombie (coordination problem solved, extraction mechanism remains) — classification would shift toward snare. If the reading is reframed around different grounds (e.g., theological purity rather than linguistic continuity), the constraint is a different constraint, not a tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(founding_problem_obsolescence, empirical, 'Whether the constraint''s founding problem remains live or has been superseded.').

omega_variable(
    institutional_capture_vs_genuine_coordination,
    'Is the suppression of secular speech (0.68) a structural requirement of the coordination function (maintaining textual coherence), or is it an institutional capture mechanism (rabbinical class defending turf)?',
    'Counterfactual test: could one maintain the coordination function (preserve the canonical corpus, enable ritual study, maintain textual coherence) while legitimizing secular speech as a co-source of linguistic vitality? If yes, the suppression is capture, not coordination. If no, the suppression is structural.',
    'If capture, the reading should reclassify as snare (extraction without coordination); if structural, the tangled rope classification holds. The theater_ratio''s rise suggests capture is increasingly the dominant mechanism — the suppression persists even as the original coordination problem (exile) becomes obsolete.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_vs_genuine_coordination, conceptual, 'Whether measured suppression is a structural requirement of coordination or an institutional capture mechanism.').

omega_variable(
    alternative_reading_foreclosure,
    'Does the liturgical reading logically foreclose the native_generation reading in any single institutional or theological framework, or are they genuinely coexistent positions held by different constituencies?',
    'Examine whether rabbinical authorities, using the liturgical reading, explicitly deny that native speakers constitute living language (logical foreclosure) or merely assert that native speech alone is insufficient — liturgical transmission is also necessary (coexistence). Seek textual authorities and institutional statements on both sides.',
    'If foreclosure obtains, the reading_relations should mark native_generation_reading as ''forecloses''; if coexistence obtains, mark it ''coexists_with'' or ''influences''. The classification of the whole kernel (whether the readings form a unified standard or mutually exclusive positions) depends on this.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Whether the liturgical reading logically forecloses competing readings or coexists with them.').

omega_variable(
    secular_speech_delegitimation_mechanism,
    'Is the delegitimization of secular speech (suppression = 0.68) internalized (secular speakers have absorbed the reading''s premise and believe their speech is inferior) or structural (rabbinical institutions actively enforce the hierarchy)?',
    'Post-exit suppression trajectory: if secular Hebrew speakers in communities without active rabbinical enforcement still believe their speech is inferior/corrupted, suppression is internalized; if delegitimization declines without enforcement, it is structural. Survey secular speakers on their perception of their own speech''s legitimacy.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure (targets carry the suppression with them). If structural, fixing the constraint (removing rabbinical enforcement) would dissolve the suppression. This informs whether the constraint is sustainable without active enforcement.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secular_speech_delegitimation_mechanism, empirical, 'Whether suppression of secular speech is internalized or structurally enforced.').

omega_variable(
    reading_identity_vs_linguistic_fact,
    'How much of the reading''s persistence is grounded in linguistic claims about what preserves a language''s identity, and how much is grounded in theological or nationalist claims about Jewish identity and continuity?',
    'Separate the reading''s linguistic premises (sacred texts = vitality) from its theological premises (Jewish continuity requires textual authority) and nationalist premises (national identity grounds in religious authenticity). Test each independently.',
    'If the reading is primarily a linguistic claim, it is vulnerable to linguistic refutation (Modern Hebrew is alive by any linguistic standard). If it is primarily a theological claim, linguistic refutation is irrelevant — the reading persists on different grounds. This informs whether the constraint is stable or whether its epistemic status is mismatched to its institutional authority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_identity_vs_linguistic_fact, conceptual, 'Whether the reading is grounded in linguistic or non-linguistic (theological/nationalist) authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(living_language_status__liturgical_preservation_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(livi_tr_t0, living_language_status__liturgical_preservation_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(livi_tr_t0, observed).
narrative_ontology:measurement(livi_tr_t5, living_language_status__liturgical_preservation_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(livi_tr_t5, observed).
narrative_ontology:measurement(livi_tr_t10, living_language_status__liturgical_preservation_reading, theater_ratio, 10, 0.23).
narrative_ontology:measurement_basis(livi_tr_t10, observed).
narrative_ontology:measurement(livi_tr_t15, living_language_status__liturgical_preservation_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement_basis(livi_tr_t15, observed).
narrative_ontology:measurement(livi_tr_t20, living_language_status__liturgical_preservation_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement_basis(livi_tr_t20, observed).
narrative_ontology:measurement(livi_tr_t25, living_language_status__liturgical_preservation_reading, theater_ratio, 25, 0.31).
narrative_ontology:measurement_basis(livi_tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(livi_be_t0, living_language_status__liturgical_preservation_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement_basis(livi_be_t0, observed).
narrative_ontology:measurement(livi_be_t5, living_language_status__liturgical_preservation_reading, base_extractiveness, 5, 0.38).
narrative_ontology:measurement_basis(livi_be_t5, observed).
narrative_ontology:measurement(livi_be_t10, living_language_status__liturgical_preservation_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement_basis(livi_be_t10, observed).
narrative_ontology:measurement(livi_be_t15, living_language_status__liturgical_preservation_reading, base_extractiveness, 15, 0.42).
narrative_ontology:measurement_basis(livi_be_t15, observed).
narrative_ontology:measurement(livi_be_t20, living_language_status__liturgical_preservation_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(livi_be_t20, observed).
narrative_ontology:measurement(livi_be_t25, living_language_status__liturgical_preservation_reading, base_extractiveness, 25, 0.42).
narrative_ontology:measurement_basis(livi_be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(livi_su_t0, living_language_status__liturgical_preservation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(livi_su_t0, observed).
narrative_ontology:measurement(livi_su_t5, living_language_status__liturgical_preservation_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(livi_su_t5, observed).
narrative_ontology:measurement(livi_su_t10, living_language_status__liturgical_preservation_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(livi_su_t10, observed).
narrative_ontology:measurement(livi_su_t15, living_language_status__liturgical_preservation_reading, suppression_requirement, 15, 0.66).
narrative_ontology:measurement_basis(livi_su_t15, observed).
narrative_ontology:measurement(livi_su_t20, living_language_status__liturgical_preservation_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement_basis(livi_su_t20, observed).
narrative_ontology:measurement(livi_su_t25, living_language_status__liturgical_preservation_reading, suppression_requirement, 25, 0.68).
narrative_ontology:measurement_basis(livi_su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(living_language_status__liturgical_preservation_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(living_language_status__liturgical_preservation_reading, 0.12).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__native_generation_reading).
narrative_ontology:affects_constraint(living_language_status__liturgical_preservation_reading, living_language_status__literary_continuity_reading).

% DUAL FORMULATION NOTE:
% The living_language_status kernel has three constraint stories: (1) liturgical_preservation_reading (this story) — sacred textual transmission suffices; (2) native_generation_reading — only native generational transmission confers vitality; (3) literary_continuity_reading — productive modern literary work demonstrates vitality. Each reading instantiates a different constraint with different beneficiaries, victims, and ε values. They are not three perspectives on one constraint; they are three different constraints from one contested kernel. The readings coexist and influence one another's institutional space. Decomposed per the ε-invariance principle (DP-001): each reading has a stable, reading-indexed ε (liturgical ≈ 0.42, native ≈ higher, literary ≈ medium) and structurally distinct beneficiary/victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
