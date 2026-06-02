% ============================================================================
% CONSTRAINT STORY: native_generation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_native_generation_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: native_generation_reading
 *   human_readable: Hebrew Native Generation Reading: Living Language Requires Daily Generative Speech
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The native-generation reading holds that Hebrew is a 'living' language
 *   only insofar as native speakers produce it generatively in daily speech,
 *   not through memorized recitation or learned performative competence. This
 *   is ONE reading of the contested kernel 'Hebrew as living language.' Under
 *   this reading, Hebrew's revival is incomplete because the first generation
 *   of speakers acquired it as a second language, not as a native L1 through
 *   family transmission. Only with the emergence of speakers whose L1 is
 *   Hebrew — speakers socialized into the language from infancy through daily
 *   family and community use — does Hebrew achieve 'living' status. This
 *   reading instantiates a commitment system: the linguistic authority
 *   (Hebrew Language Academy) grounds its legitimacy in a fixed, formal
 *   kernel (the definition of 'living language') and deploys institutional
 *   power to enforce a specific interpretation. The constraint extracts from
 *   non-native speakers, Yiddish/Ladino speakers, and other diaspora language
 *   communities by systematically redeclassifying their linguistic production
 *   as 'non-living,' 'inauthentic,' or 'reconstructed.' The suppression
 *   mechanism is the power to define who counts as a native speaker and whose
 *   speech counts as generative.
 *
 * KEY AGENTS:
 *   - Hebrew Nationalist Movement: Primary beneficiary (institutional/arbitrage) — benefits from the native-generation criterion's role in legitimizing the nation-state and concentrating linguistic authority around Hebrew
 *   - Hebrew Language Academy: Institutional authority (institutional/arbitrage) — maintains gatekeeping power over what counts as authentic Hebrew; benefits from the need to adjudicate native-speaker status
 *   - Yiddish/Ladino Diaspora Speakers: Primary victims (powerless/trapped) — their living vernacular languages are redeclassified as non-living; forced to choose between linguistic identity and nationalist legitimacy
 *   - Non-Native Hebrew Learners: Secondary victims (moderate/constrained) — constrained by native-only gatekeeping; permanently marked as inauthentic despite functional fluency
 *   - Diaspora Language Preservation Movements: Organized resistance (organized/constrained) — maintain agency through organized preservation efforts but face zero-sum competition for authenticity
 *   - Linguistic Science Community: Universal observer (analytical/analytical) — recognizes the native-generation criterion as cultural gatekeeping rather than linguistic necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(native_generation_reading, 0.52).
domain_priors:suppression_score(native_generation_reading, 0.65).
domain_priors:theater_ratio(native_generation_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(native_generation_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(native_generation_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(native_generation_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(native_generation_reading, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(native_generation_reading, resistance, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(native_generation_reading, tangled_rope).
narrative_ontology:human_readable(native_generation_reading, "Hebrew Native Generation Reading: Living Language Requires Daily Generative Speech").
narrative_ontology:topic_domain(native_generation_reading, "historical_linguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(native_generation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_kernel_codification(native_generation_reading, formalized).
narrative_ontology:cs_authority_grounding(native_generation_reading, extraction).
narrative_ontology:cs_interpretation_layer_present(native_generation_reading).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(native_generation_reading, hebrew_nationalist_movement).
narrative_ontology:constraint_beneficiary(native_generation_reading, hebrew_institutional_authority).
narrative_ontology:constraint_victim(native_generation_reading, yiddish_speaking_diaspora).
narrative_ontology:constraint_victim(native_generation_reading, ladino_speaking_diaspora).
narrative_ontology:constraint_victim(native_generation_reading, non_native_hebrew_learners).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YIDDISH/LADINO DIASPORA (SNARE) — Native Yiddish or Ladino speakers face the constraint as a complete trap: their living vernacular languages are redefined as non-living, non-generative, dead. Exit requires abandoning their native speech production for a reconstructed language. Structurally trapped by the definition itself — no alternative framework exists within the nationalist movement for recognizing their speech as 'living.' Maximum experienced extraction: forced choice between linguistic identity and nationalist legitimacy.
constraint_indexing:constraint_classification(native_generation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-NATIVE HEBREW LEARNERS (TANGLED ROPE) — Moderate agents learning Hebrew as second language experience genuine coordination (Hebrew enables participation in nationalist/cultural project) alongside extraction (their speech is always measured against native-generation standard, perpetually marked as inauthentic, constrained from leadership roles). Benefits from the language's adoption; constrained by the native-only gatekeeping.
constraint_indexing:constraint_classification(native_generation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HEBREW NATIONALIST MOVEMENT (ROPE) — The primary beneficiary (institutional/arbitrage). Experiences the constraint as pure coordination: the native-generation standard solves the legitimacy problem of whether Hebrew is truly 'revived' or merely artificially restored. Defining livingness through native daily speech establishes an irrefutable criterion that no other language can meet in Israel, concentrating linguistic authority. Net beneficiary with maximum exit flexibility — can redefine the constraint at will.
constraint_indexing:constraint_classification(native_generation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HEBREW LANGUAGE ACADEMY (ROPE) — Institutional authority over Hebrew standardization and authentication. Experiences the constraint as coordination (maintains linguistic purity and institutional gatekeeping authority). Benefits from the native-generation standard: it makes the Academy's normative authority indispensable — only they can adjudicate what counts as authentic native speech. Net beneficiary with arbitrage exit.
constraint_indexing:constraint_classification(native_generation_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DIASPORA LANGUAGE PRESERVATION MOVEMENTS (TANGLED ROPE) — Organized agents (Yiddish cultural organizations, Ladino academic communities) face the constraint as both coordination problem and extraction. Coordination function: Hebrew revival does provide a symbolic Jewish national center and enables literacy access. Extraction function: the native-generation reading systematically delegitimizes Yiddish and Ladino as 'non-living,' creating a zero-sum competition for authenticity. Constrained by the nationalist framing but retain agency through organized resistance.
constraint_indexing:constraint_classification(native_generation_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: LINGUISTIC SCIENCE / UNIVERSAL AUTHORITY (PITON) — From a civilizational/universal perspective focused on linguistic function, the native-generation standard is largely theater. All languages are 'living' if they perform communicative function — Yiddish, Ladino, Hebrew, and constructed languages all do this. The native-generation criterion is a performative gatekeeping ritual masquerading as scientific linguistic classification. Linguists know this; the institutional framework persists through cultural inertia and nationalist authority needs rather than linguistic evidence.
constraint_indexing:constraint_classification(native_generation_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(universal))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From the fullest analytical perspective, the native-generation criterion might appear as an immutable feature of language continuity itself: a reconstructed language cannot truly be 'living' without native speakers producing it generatively from birth. This perspective sees strict-reachability as a natural law — no reconstruction can bridge the gap. However, this classification is a FALSE SUMMIT: the constraint's beneficiaries (nationalist movement, academy) are identifiable, and the suppression mechanism (redefinition of Yiddish/Ladino as non-living) is a contingent institutional choice, not a law of nature.
constraint_indexing:constraint_classification(native_generation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(native_generation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(native_generation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(native_generation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(native_generation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(native_generation_reading, TR),
    TR >= 0.70.

:- end_tests(native_generation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The native-generation standard creates a structural advantage for Hebrew over other Jewish languages and establishes a hierarchy of authenticity. The extraction mechanism is epistemic rather than economic: the power to define 'living' status shapes institutional authority, career legitimacy, and cultural belonging. The extractiveness is not maximal (0.66+) because the constraint does provide genuine coordination benefits (shared language enables community) alongside the extraction. Suppression (0.65): Moderate-high. Multiple suppression mechanisms operate: (1) institutional gatekeeping that excludes non-native speakers from leadership roles; (2) redefinition of Yiddish/Ladino as non-living, delegitimizing their use; (3) historical erasure — the Yiddish-speaking diaspora is reframed as a pre-national population whose linguistic production is inauthentic; (4) normative pressure on second-generation speakers to erase first-language traces and perform 'native' Hebrew. But suppression is not total (not 0.80+) because organizational resistance exists and linguistic alternatives persist. Theater ratio (0.48): Moderate. The constraint exhibits mixed functional and performative elements. Functional: daily generative speech is genuinely required for language transmission to the next generation. Performative: the institutional gatekeeping around who counts as native, the enforcement of standardized Hebrew against Arabic-influenced or English-influenced varieties, and the ceremonial exclusion of diaspora languages from institutional recognition are largely performative — they maintain authority rather than serving linguistic function.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates extreme perspectival variance. The nationalist movement sees pure coordination (Rope) — the criterion solves the legitimacy problem of whether Hebrew is truly revived. The Language Academy sees pure coordination (Rope) — they maintain authority. Non-native learners see mixed coordination and extraction (Tangled Rope) — they benefit from Hebrew participation but are constrained by authenticity gatekeeping. Yiddish/Ladino speakers see pure extraction (Snare) — their living languages are redefined as dead. The diaspora language preservation movement sees mixed (Tangled Rope) — they coordinate cultural identity but extract cost in zero-sum competition for authenticity. From a universal linguistic perspective, the native-generation standard is largely performative (Piton) — linguists know that all these languages perform communicative function; the gatekeeping persists through institutional inertia. From an analytical perspective that might naturalize the constraint as a law of language continuity, it appears as Mountain (false summit) — but this is precisely what the engine detects when beneficiaries are declared on a mountain constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality derives from the agent's structural position relative to the extraction flow. The nationalist movement and Language Academy are beneficiaries with maximum exit flexibility (arbitrage) — they derive low d values and experience negative or minimal effective extraction. Non-native learners are moderate victims with constrained exit (career cost, identity cost) — they derive d around 0.55-0.65. Yiddish/Ladino speakers are powerless victims with trapped exit (linguistic identity cannot be abandoned without ceasing to exist as speakers) — they derive d around 0.95, producing maximum effective extraction. The diaspora preservation movement is an organized agent with constrained exit (they can resist but at organizational cost) — they derive d around 0.65-0.75. The linguistic scientist is analytical/analytical with a derived d around 0.72 (the classic observer position). The false-summit mountain perspective declares beneficiaries (nationalist movement, academy) on a constraint that might appear natural, triggering FSM engine evaluation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint avoids mandatrophy through explicit perspectival decomposition. The beneficiary (nationalist movement) sees coordination; the victims see extraction; the organized resistance sees mixed. The tangled_rope classification accommodates the genuine coordination function (Hebrew does enable shared community and cultural participation) alongside the asymmetric extraction (institutional gatekeeping that privileges native speakers and delegitimizes diaspora languages). The false-summit detection prevents misclassification as mountain (natural linguistic law) by identifying beneficiaries whose interests are served by the native-generation definition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    generative_vs_memorized_boundary,
    'What empirical distinction separates ''generative native speech'' from ''native-accented memorized recitation''? Where is the boundary drawn, and who draws it?',
    'Linguistic analysis of speech production patterns; examination of native speaker judgments; documentation of how institutional authority determines classification in borderline cases (second-language-dominant speakers, speakers with mixed linguistic input)',
    'If boundary is empirically measurable and objective: constraint approaches mountain status (natural linguistic law). If boundary is drawn by institutional authority through social judgment: constraint is tangled_rope or snare (institutional extraction mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generative_vs_memorized_boundary, empirical, 'Where and how the generative/memorized boundary is empirically determined').

omega_variable(
    strict_reachability_assumption,
    'Must a truly ''living'' language have an unbroken chain of native speakers from historical period to present? Can reconstructed languages ever achieve this, or is strict reachability inherently impossible?',
    'Historical linguistics analysis; comparison with other reconstructed/revived languages (Latin in scholarly communities, Ancient Egyptian in Egyptology); clarification of what ''native speaker'' means across historical discontinuity',
    'If strict reachability is a requirement: reconstruction is inherently degraded; many victims (non-native speakers) are perpetually excluded. If strict reachability is waived: the constraint collapses to a coordination problem (rope) — any community that uses Hebrew generatively becomes native speakers of their generation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(strict_reachability_assumption, conceptual, 'Whether strict reachability is a hard requirement for ''living'' language status').

omega_variable(
    kernel_reading_contest,
    'Is the native-generation reading the legitimate reading of what makes Hebrew ''living,'' or is the liturgical-continuity reading (Hebrew continuously used in prayer/study) or literary-revival reading (Hebrew continuously produced in written form) equally valid?',
    'Historical textual analysis; community acceptance patterns; comparison with how other religious/cultural languages (Sanskrit, Latin, Church Slavonic) define ''living'' status',
    'If native-generation is singular: this constraint stands as tangled_rope with identified extraction. If readings are equally valid: the constraint decomposes into a network of three competing constraints, each with different ε values and different victim/beneficiary sets.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Legitimacy of native-generation reading vs. sibling readings of the Hebrew living-language kernel').

omega_variable(
    performative_authenticity_maintenance,
    'Does the institutional enforcement of the native-generation standard require active policing of speech boundaries, or does it function through passive cultural authority?',
    'Documentation of Language Academy enforcement mechanisms; analysis of social penalty mechanisms for non-native speech; tracking of how ''native-only'' gatekeeping manifests in institutional practices (hiring, media, education)',
    'If enforcement is active and visible: suppression value is accurate (0.65). If enforcement is primarily cultural/normative (internalized gatekeeping by non-native speakers): suppression may be underestimated; theater_ratio may be higher.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performative_authenticity_maintenance, empirical, 'Active vs. passive enforcement of native-generation standard').

omega_variable(
    alternative_reading_false_summit_risk,
    'Does the native-generation reading function as a false summit — a naturalized institutional arrangement that claims to be a law of linguistic science but actually benefits specific nationalist agents?',
    'Comparison with linguistic science consensus on what ''living'' language means; examination of whether the native-generation criterion is applied consistently across all languages or selectively; analysis of whether Yiddish/Ladino could meet the criterion if institutional incentives differed',
    'If false summit confirmed: the mountain perspective is reclassified; constraint''s true type is tangled_rope or snare. If mountain is genuine: linguistic science genuinely supports the native-generation criterion as a law of continuity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_false_summit_risk, empirical, 'Whether native-generation reading constitutes false-summit naturalization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(native_generation_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nati_tr_t0, native_generation_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement(nati_tr_t20, native_generation_reading, theater_ratio, 20, 0.42).
narrative_ontology:measurement(nati_tr_t40, native_generation_reading, theater_ratio, 40, 0.48).

% Extraction over time
narrative_ontology:measurement(nati_be_t0, native_generation_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(nati_be_t20, native_generation_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(nati_be_t40, native_generation_reading, base_extractiveness, 40, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(native_generation_reading, identity_coordination).
narrative_ontology:affects_constraint(native_generation_reading, liturgical_continuity_reading).
narrative_ontology:affects_constraint(native_generation_reading, literary_revival_reading).

% DUAL FORMULATION NOTE:
% The Hebrew-living-language kernel decomposes into three structurally distinct constraints, each with its own ε value and victim/beneficiary set. The native-generation reading (this constraint) has ε=0.52 with Yiddish/Ladino victims. The liturgical-continuity reading would have lower ε (0.30-0.40) with no zero-sum victim set. The literary-revival reading would have lower ε (0.35-0.45) with Yiddish/Ladino as collaborative partners rather than victims. Each reading is a complete constraint story; together they form a kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(native_generation_reading, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
