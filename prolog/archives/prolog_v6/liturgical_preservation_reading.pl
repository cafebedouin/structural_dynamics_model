% ============================================================================
% CONSTRAINT STORY: liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liturgical_preservation_reading, []).

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
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: liturgical_preservation_reading
 *   human_readable: Liturgical Preservation Reading of Living Language Status
 *   domain: sociolinguistics/religious_studies/nationalism
 *
 * SUMMARY:
 *   The liturgical preservation reading asserts that a language maintains its
 *   life and authenticity through continuous ritual use and canonical textual
 *   study, even if native generational transmission has ceased or become
 *   marginal. This reading grounds legitimacy in the interpretive authority
 *   of those trained in the sacred corpus — rabbis, priests, monks, scholars
 *   — who maintain linguistic vitality through liturgical practice and
 *   textual commentary. The constraint exhibits a genuine coordination
 *   function (preserving shared liturgical knowledge across generations;
 *   enabling ritual participation; maintaining access to sacred texts) but
 *   simultaneously extracts from secular speakers, who are delegitimized as
 *   desecrators of the language. Native generational transmission outside
 *   ritual contexts is reframed as corruption rather than continuation. The
 *   beneficiary structure is institutional: the rabbinical or priestly
 *   authority class gains interpretive monopoly and cultural authority. The
 *   victim structure is the secular speech community: those who use the
 *   language in everyday contexts face constant delegitimization.
 *   Extractiveness (0.38) reflects that the constraint coordinates a real
 *   function (liturgical transmission) while simultaneously suppressing
 *   (0.52) an alternative transmission pathway (native generational speech).
 *   Theater ratio (0.45) is moderate because the liturgical practice is
 *   genuinely functional within its domain (enabling ritual performance and
 *   textual interpretation) but is elevated to a claim about what language is
 *   essentially, producing some performative content. Measurements show
 *   gradual increase in extractiveness as native generational transmission
 *   declines and the constraint tightens — the secular speaker must invest
 *   more in overcoming delegitimization as the liturgical reading gains
 *   institutional enforcement.
 *
 * KEY AGENTS:
 *   - Rabbinical/Priestly Authority: Institutional beneficiary (institutional/arbitrage) — holds interpretive monopoly and cultural authority over 'correct' language use; benefits from constraint that centralizes linguistic legitimacy
 *   - Liturgical Specialist Class: Institutional beneficiary (organized/arbitrage) — trained specialists in textual study and ritual recitation; their skill set is valorized under this reading
 *   - Secular Speaker: Primary victim (powerless/trapped) — native speakers using the language in everyday contexts; delegitimized as corrupting the language; cannot exit without abandoning linguistic community
 *   - Language Community (Generational): Constrained agent (moderate/constrained) — experiences both coordination benefit (shared liturgical knowledge) and extraction (native generational transmission devalued)
 *   - Native Speaker Movement: Organized resistance (organized/mobile) — Zionists, language revival activists, secular cultural nationalists; building alternative transmission pathways with sunset intent
 *   - Secular Literary Production: Emerging alternative (organized/mobile) — newspapers, novels, colloquial media in the language; competes with liturgical canon as source of linguistic vitality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liturgical_preservation_reading, 0.38).
domain_priors:suppression_score(liturgical_preservation_reading, 0.52).
domain_priors:theater_ratio(liturgical_preservation_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liturgical_preservation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(liturgical_preservation_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(liturgical_preservation_reading, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liturgical_preservation_reading, tangled_rope).
narrative_ontology:human_readable(liturgical_preservation_reading, "Liturgical Preservation Reading of Living Language Status").
narrative_ontology:topic_domain(liturgical_preservation_reading, "sociolinguistics/religious_studies/nationalism").

domain_priors:requires_active_enforcement(liturgical_preservation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liturgical_preservation_reading, 'd19823ad-f268-4c89-9931-3327236453d3').
narrative_ontology:cs_created_at('d19823ad-f268-4c89-9931-3327236453d3', '').
narrative_ontology:cs_kernel_codification('d19823ad-f268-4c89-9931-3327236453d3', formalized).
narrative_ontology:cs_authority_grounding('d19823ad-f268-4c89-9931-3327236453d3', lineage).
narrative_ontology:cs_interpretation_layer_present('d19823ad-f268-4c89-9931-3327236453d3').
narrative_ontology:cs_kernel_id(liturgical_preservation_reading, living_language_status).
narrative_ontology:cs_reading_relation('d19823ad-f268-4c89-9931-3327236453d3', native_generation_reading, forecloses).
narrative_ontology:cs_reading_relation('d19823ad-f268-4c89-9931-3327236453d3', literary_continuity_reading, influences).
narrative_ontology:cs_axiom('d19823ad-f268-4c89-9931-3327236453d3', foundational, interpretive_authorization).
narrative_ontology:cs_axiom_status(interpretive_authorization, holdable).
narrative_ontology:cs_axiom_grounding('d19823ad-f268-4c89-9931-3327236453d3', interpretive_authorization, conventional).
narrative_ontology:cs_axiom('d19823ad-f268-4c89-9931-3327236453d3', secondary, liturgical_essence).
narrative_ontology:cs_axiom_status(liturgical_essence, holdable).
narrative_ontology:cs_axiom_grounding('d19823ad-f268-4c89-9931-3327236453d3', liturgical_essence, deontological).
narrative_ontology:cs_reference_frame('d19823ad-f268-4c89-9931-3327236453d3', canonical_liturgical_transmission).
narrative_ontology:cs_drift_state('d19823ad-f268-4c89-9931-3327236453d3', contemporary_secular_linguistic_innovation, gap(practice_drift, substantial, false)).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liturgical_preservation_reading, rabbinical_authority).
narrative_ontology:constraint_beneficiary(liturgical_preservation_reading, liturgical_interpretive_class).
narrative_ontology:constraint_beneficiary(liturgical_preservation_reading, language_sanctity_doctrine).
narrative_ontology:constraint_victim(liturgical_preservation_reading, secular_speech_community).
narrative_ontology:constraint_victim(liturgical_preservation_reading, colloquial_linguistic_innovation).
narrative_ontology:constraint_victim(liturgical_preservation_reading, native_generational_transmission).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SECULAR SPEAKER (SNARE) — Cannot exit the constraint. Native speakers who use the language in colloquial contexts face delegitimization: their speech is labeled 'profane,' 'impure,' 'inauthentic.' The liturgical reading declares that only ritual/canonical uses preserve the language; everyday speech corrupts it. The speaker bears the extraction (loss of legitimacy for natural speech) with no exit option and no coordination benefit.
constraint_indexing:constraint_classification(liturgical_preservation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: LANGUAGE COMMUNITY / GENERATIONAL (TANGLED ROPE) — Constrained by the obligation to transmit liturgical competence (ritual recitation, textual study) to the next generation. This is genuine coordination: the community does need to maintain shared liturgical knowledge. But the constraint also extracts: native generational transmission is devalued; colloquial innovation is suppressed; the community's living speech is subordinated to fixed liturgical corpus. Mixed coordination and extraction over the generational horizon.
constraint_indexing:constraint_classification(liturgical_preservation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RABBINICAL AUTHORITY (ROPE) — Institutional beneficiary with arbitrage options. Benefits from the liturgical preservation framing: interpretive monopoly is preserved (only those trained in canonical texts can authoritatively speak the language); authority over 'correct' language rests with the interpretive class. Experiences the constraint as pure coordination: transmitting the liturgical corpus and maintaining the interpretive tradition. The extraction runs toward this agent; they see no extraction, only legitimate authority preservation.
constraint_indexing:constraint_classification(liturgical_preservation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MODERN NATIVE SPEAKER MOVEMENT (SCAFFOLD) — Organized agents (Zionism, language revival movements, secular cultural nationalism) who see the liturgical preservation reading as a temporary constraint to be overcome. They organize alternative transmission pathways: native-speaker schooling, colloquial literature, everyday media use. The constraint has a sunset: as native speakers grow in number and cultural authority, the 'liturgical only' framing loses legitimacy. This perspective experiences extraction but sees an exit path and active alternatives being built.
constraint_indexing:constraint_classification(liturgical_preservation_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: THEOLOGICAL NATURAL LAW VIEW (MOUNTAIN) — From a theological/civilizational perspective, the liturgical preservation reading claims that a language's essence is its sacred corpus: the language IS its liturgy, and any use outside the sacred context is degradation, not continuation. This perspective sees language vitality as inherent to liturgical transmission — an immutable theological claim. However, the structural data contradicts this mountain classification: the empirical record shows languages thriving through colloquial native transmission without liturgical recitation. The engine will detect this as a false summit — the 'natural law' framing naturalizes what is actually a contested normative claim about what counts as authentic language use.
constraint_indexing:constraint_classification(liturgical_preservation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liturgical_preservation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(liturgical_preservation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liturgical_preservation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The liturgical reading coordinates a genuine function — maintaining access to sacred texts and enabling ritual performance. But it simultaneously extracts by delegitimizing native generational transmission and suppressing colloquial innovation. The extraction is not maximal (0.46+) because native speakers retain agency: they can use the language colloquially despite delegitimization, and alternative transmission mechanisms (schooling, media, literature) are available. Suppression (0.52): Moderate-high. The constraint suppresses native generational transmission, colloquial linguistic innovation, and alternative transmission mechanisms. But suppression is not total (0.60+) because the constraint relies on active institutional enforcement rather than structural barriers. Theater ratio (0.45): Moderate-low. The liturgical practice is genuinely functional within its domain — ritual recitation and textual study do preserve linguistic knowledge and enable continued use in sacred contexts. But the reading's claim that this suffices for 'living language status' introduces performative content — the elevation of liturgical transmission to a claim about linguistic essence rather than one transmission mechanism among others.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a structural disagreement about what constitutes linguistic vitality. The rabbinical authority sees the constraint as pure coordination (Rope) — maintaining the liturgical corpus is the language's primary function. The secular speaker sees it as pure extraction (Snare) — their speech is delegitimized with no benefit to them. The language community sees mixed coordination and extraction (Tangled Rope) over the generational horizon. The native speaker movement sees a temporary constraint with an exit path (Scaffold) — alternative transmission mechanisms are being built and will supersede the liturgical reading within a generation. The theological natural-law perspective risks seeing the reading as immutable (Mountain) — that liturgical transmission is inherent to language itself — but the structural data shows this is a contingent institutional arrangement with historical alternatives. The false-summit detection reveals that the theological framing naturalizes a political/normative choice.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries (rabbinical_authority, liturgical_interpretive_class) occupy institutional power with arbitrage options — they can withdraw from the language entirely or reframe it. Their directionality is low (d ≈ 0.15-0.20): they benefit from the constraint (interpretive monopoly preserved) and experience minimal extraction. The victims (secular_speech_community) are powerless with trapped exit options — they cannot abandon the language without losing community identity. Their directionality is high (d ≈ 0.85-0.90): the constraint extracts legitimacy from their speech while they have no exit. The moderate agent (language_community) experiences mixed extraction (generational transmission devalued) and coordination benefit (shared liturgical knowledge), producing medium directionality (d ≈ 0.50-0.60). The organized resistance (native_speaker_movement) has mobile exit options — they can build alternative institutions. Their directionality is lower (d ≈ 0.40-0.50) because they have agency and visible alternatives.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that the disagreement is not 'which transmission mechanism is correct?' but 'who gets to define what counts as living language?' Under the liturgical reading, the secular speaker's native colloquial speech does not count as language continuation. Under the native_generation_reading, the rabbinical authority's liturgical recitation alone does not count as living transmission (because it lacks the spontaneity and innovation of native generational speech). These are incommensurable claims about linguistic authenticity, not empirical disagreements about transmission mechanisms. The constraint is genuine Tangled Rope because both coordination (liturgical knowledge transmission) and extraction (delegitimization of secular speech) are real structural features. The false summit at the theological perspective indicates the reading's risk: naturalizing as immutable law what is actually a contestable claim about linguistic essence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_sufficiency_threshold,
    'How much ritual recitation and textual study is required for a language to be ''living'' under the liturgical preservation standard? Is occasional ceremonial use sufficient, or does it require daily/weekly engagement?',
    'Historical comparison: languages maintained through liturgical transmission only (Latin in Catholic contexts, Ge''ez in Ethiopian Orthodox contexts, Sanskrit in Brahminical ritual) vs. languages that revived through native generational transmission despite centuries of liturgical-only use (Modern Hebrew). Empirical observation of engagement frequency and scale required to maintain linguistic competence across generations.',
    'If occasional use suffices: the constraint is weak coordination (Rope). If daily engagement required: the constraint becomes a snare for secular communities unable to maintain that intensity. If threshold is contestable: the ambiguity is the constraint (Tangled Rope).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liturgical_sufficiency_threshold, empirical, 'Frequency threshold for liturgical transmission to sustain language vitality').

omega_variable(
    native_generation_decoupling,
    'Can a language be ''living'' if the dominant transmission mechanism is ritual recitation by trained specialists rather than native generational transmission (parent to child, peer to peer, community apprenticeship)?',
    'Neurolinguistic studies of how languages are acquired via liturgical study vs. natural immersion; generational competence data (can second-generation liturgical learners produce novel utterances or only reproduce memorized texts?); comparison of linguistic complexity and innovation patterns in liturgy-maintained vs. natively-transmitted languages.',
    'If decoupling is possible: liturgical transmission alone suffices (reading confirmed). If decoupling fails: generational transmission is necessary (reading foreclosed by native_generation_reading). This resolves the core axiological disagreement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(native_generation_decoupling, empirical, 'Whether liturgical transmission alone sustains full linguistic vitality without native generational transmission').

omega_variable(
    interpretive_monopoly_contingency,
    'Is the rabbinical/priestly interpretive monopoly a necessary feature of the liturgical preservation reading, or could liturgical transmission occur without centralized interpretive authority?',
    'Historical analysis of liturgical transmission in contexts with and without centralized interpretive authority (e.g., distributed liturgical practice in early Christianity vs. centralized Rabbinic Judaism; sectarian liturgical innovation vs. orthodox enforcement). Observation of whether open-source liturgical transmission (crowdsourced interpretation, democratic text selection) can sustain the language.',
    'If monopoly is necessary: the beneficiary structure (rabbinical_authority) is intrinsic to the reading (extraction structural). If monopoly is contingent: alternative non-extractive implementations are possible, and the current implementation is a political choice rather than logical necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_monopoly_contingency, conceptual, 'Whether interpretive monopoly is necessary to liturgical preservation or contingently coupled').

omega_variable(
    reading_kernel_distinction,
    'Is this the liturgical_preservation_reading or a collation of multiple readings? The kernel (living_language_status) is contested between at least three readings; does this constraint capture only the liturgical reading, or does it conflate the liturgical norm with a theological natural-law claim?',
    'Re-examine the boundary between the liturgical reading (transmission mechanism: ritual recitation and textual study) and the theological reading (claim that this is the essence of the language itself). These are distinct commitments. The liturgical reading is a normative claim about how to preserve language. The theological reading is a metaphysical claim about what the language essentially is. The false-summit mountain perspective conflates them.',
    'If they are distinct: the theological natural-law perspective should be its own constraint (or omega). If they are inseparable: the false summit is diagnostic (the reading itself naturalizes a normative choice). This omega tracks the authorial intent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_kernel_distinction, conceptual, 'Boundary between liturgical_preservation_reading and theological_natural_law claims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liturgical_preservation_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(litu_tr_t0, liturgical_preservation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(litu_tr_t15, liturgical_preservation_reading, theater_ratio, 15, 0.42).
narrative_ontology:measurement(litu_tr_t30, liturgical_preservation_reading, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(litu_be_t0, liturgical_preservation_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(litu_be_t15, liturgical_preservation_reading, base_extractiveness, 15, 0.34).
narrative_ontology:measurement(litu_be_t30, liturgical_preservation_reading, base_extractiveness, 30, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(liturgical_preservation_reading, native_generation_reading).
narrative_ontology:affects_constraint(liturgical_preservation_reading, literary_continuity_reading).

% DUAL FORMULATION NOTE:
% The living_language_status kernel decomposes into three structurally distinct constraints. Each reading generates a different epsilon value and beneficiary/victim structure. The liturgical_preservation_reading (this constraint) exhibits moderate extractiveness (0.38) because it coordinates genuine liturgical transmission while suppressing generational transmission. The native_generation_reading would exhibit lower extractiveness (ε ≈ 0.15-0.20, Rope) because generational transmission requires minimal enforcement and benefits all speakers. The literary_continuity_reading would exhibit different extraction dynamics depending on whether literary production is centralized or democratic. All three readings share the same kernel (what counts as living language) but generate distinct constraints through different transmission mechanisms and authority structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(liturgical_preservation_reading, institutional, 0.18).
constraint_indexing:directionality_override(liturgical_preservation_reading, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
