% ============================================================================
% CONSTRAINT STORY: liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liturgical_reading, []).

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
 *   constraint_id: liturgical_reading
 *   human_readable: Hebrew as Living Language Through Liturgical Reading
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   Hebrew's status as a 'living language' through continuous liturgical
 *   reading represents a unique case in linguistic history: a language
 *   maintained primarily through ritual practice rather than native daily
 *   speech for nearly two millennia (roughly 70 CE to 1880s CE), then
 *   revitalized as a native language in the 20th century. This constraint
 *   story instantiates ONE reading of the contested kernel 'Hebrew as living
 *   language' — specifically, the liturgical reading, which holds that
 *   symbolic preservation through ritual use constitutes linguistic life. The
 *   constraint operates through the daily and weekly liturgical practices of
 *   Jewish communities worldwide: the recitation of prayers, Torah readings,
 *   and Talmudic study in Hebrew. The rabbinic interpretive authority
 *   maintains standardization and transmits the tradition; the liturgical
 *   community participates in the practice; modern Hebrew education uses the
 *   liturgical foundation as scaffolding for native speech. The constraint
 *   exhibits low extractiveness (0.15) because the coordination function is
 *   genuine — the ritual practice does solve the real problem of maintaining
 *   linguistic continuity — and no identifiable victims exist. The theater
 *   ratio (0.35) reflects that some participants engage in phonetic
 *   recitation without comprehension, but the overall practice is
 *   functionally oriented toward linguistic preservation rather than
 *   performative maintenance. This reading coexists with two sibling
 *   readings: the native daily reading (which holds that linguistic life
 *   requires native speakers and daily use) and the continuity narrative
 *   reading (which holds that the narrative of unbroken linguistic tradition
 *   constitutes linguistic life). These three readings are structurally
 *   distinct constraints with different ε values and different
 *   beneficiary/victim structures.
 *
 * KEY AGENTS:
 *   - Rabbinic Interpretive Authority: Institutional beneficiary (institutional/arbitrage) — maintains and transmits the interpretive tradition; benefits from the constraint through authority preservation and institutional continuity
 *   - Liturgical Community Participant: Moderate beneficiary (moderate/constrained) — participates in daily/weekly liturgical reading; experiences genuine coordination (linguistic and cultural continuity)
 *   - Non-Hebrew-Speaking Liturgical Participant: Mixed victim/beneficiary (powerless/identity_locked) — participates without fluent comprehension; identity-locked to the community; bears extraction of labor without full understanding
 *   - Hebrew Language Revitalization Movement: Powerful beneficiary (powerful/mobile) — uses liturgical Hebrew as foundation for modern native speech; benefits from the constraint's linguistic preservation function
 *   - Modern Hebrew Education System: Organized agent (organized/constrained) — treats the constraint as temporary scaffolding with sunset; sees liturgical reading as transitional mechanism
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the contingent institutional arrangement as an immutable property of linguistic preservation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liturgical_reading, 0.15).
domain_priors:suppression_score(liturgical_reading, 0.25).
domain_priors:theater_ratio(liturgical_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liturgical_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(liturgical_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(liturgical_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liturgical_reading, rope).
narrative_ontology:human_readable(liturgical_reading, "Hebrew as Living Language Through Liturgical Reading").
narrative_ontology:topic_domain(liturgical_reading, "sociolinguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liturgical_reading, 'c6849cd6-3350-4a82-9802-2977ac2c1799').
narrative_ontology:cs_kernel_codification('c6849cd6-3350-4a82-9802-2977ac2c1799', fixed_text).
narrative_ontology:cs_authority_grounding('c6849cd6-3350-4a82-9802-2977ac2c1799', lineage).
narrative_ontology:cs_interpretation_layer_present('c6849cd6-3350-4a82-9802-2977ac2c1799').
narrative_ontology:cs_reading_relation('c6849cd6-3350-4a82-9802-2977ac2c1799', liturgical_reading__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('c6849cd6-3350-4a82-9802-2977ac2c1799', liturgical_reading__continuity_narrative_reading, influences).
narrative_ontology:cs_axiom('c6849cd6-3350-4a82-9802-2977ac2c1799', foundational, symbolic_preservation_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(symbolic_preservation_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('c6849cd6-3350-4a82-9802-2977ac2c1799', symbolic_preservation_constitutes_linguistic_life, conventional).
narrative_ontology:cs_axiom('c6849cd6-3350-4a82-9802-2977ac2c1799', secondary, rabbinic_authority_maintains_linguistic_standardization).
narrative_ontology:cs_axiom_status(rabbinic_authority_maintains_linguistic_standardization, holdable).
narrative_ontology:cs_axiom_grounding('c6849cd6-3350-4a82-9802-2977ac2c1799', rabbinic_authority_maintains_linguistic_standardization, conventional).
narrative_ontology:cs_reference_frame('c6849cd6-3350-4a82-9802-2977ac2c1799', hebrew_as_sacred_language_through_ritual_continuity).
narrative_ontology:cs_drift_state('c6849cd6-3350-4a82-9802-2977ac2c1799', contemporary, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c6849cd6-3350-4a82-9802-2977ac2c1799', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(liturgical_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liturgical_reading, rabbinic_interpretive_authority).
narrative_ontology:constraint_beneficiary(liturgical_reading, jewish_liturgical_community).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITURGICAL COMMUNITY PARTICIPANT (ROPE) — Participates in daily or weekly liturgical reading (Shacharit, Mincha, Ma'ariv, Shabbat services). Experiences the constraint as genuine coordination: the shared practice of reading Hebrew texts maintains linguistic continuity and community cohesion. Constrained exit (leaving the community carries social cost) but net beneficiary — the constraint solves the real coordination problem of maintaining linguistic and cultural continuity across generations. Low experienced extraction.
constraint_indexing:constraint_classification(liturgical_reading, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: RABBINIC INTERPRETIVE AUTHORITY (ROPE) — Maintains and transmits the interpretive tradition (Talmudic commentary, halakhic rulings, liturgical standardization). Benefits from the constraint through institutional continuity and authority preservation. Arbitrage exit (can shift interpretive frameworks, adopt new readings, or recontextualize texts). Experiences the constraint as coordination: the liturgical reading practice is the mechanism through which rabbinic authority is exercised and transmitted. Low extraction — the authority's power derives from the coordination function, not from suppression.
constraint_indexing:constraint_classification(liturgical_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: HEBREW LANGUAGE REVITALIZATION MOVEMENT (ROPE) — Modern secular movement (19th-20th century) that adopted liturgical Hebrew as the foundation for modern spoken Hebrew. Experiences the constraint as coordination: the liturgical reading tradition provided a standardized, continuous linguistic resource that enabled the construction of modern Hebrew. Mobile exit (can adopt alternative linguistic bases or abandon the project) but net beneficiary. The constraint solves the problem of linguistic continuity without requiring native speakers — ritual use preserves the language structure that revitalization can build upon.
constraint_indexing:constraint_classification(liturgical_reading, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: NON-HEBREW-SPEAKING LITURGICAL PARTICIPANT (TANGLED ROPE) — Participates in liturgical reading without fluent Hebrew comprehension; reads phonetically or from transliteration. Identity-locked exit (leaving the community means abandoning religious identity and cultural belonging). Experiences mixed coordination and extraction: the constraint maintains community cohesion (coordination function) but also requires participation in a linguistic practice the participant does not fully understand (extraction of labor/attention without full comprehension). Moderate experienced extraction due to identity lock.
constraint_indexing:constraint_classification(liturgical_reading, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(local))).

% PERSPECTIVE 5: MODERN HEBREW EDUCATION SYSTEM (SCAFFOLD) — Organized institutional effort (schools, universities, language academies) to teach Hebrew as a living language. Sees the liturgical reading constraint as a temporary scaffolding mechanism: the ritual practice provides linguistic continuity during the transition from liturgical-only to spoken-native Hebrew. Constrained exit (educational institutions depend on state/community support). Experiences the constraint as transitional coordination with a sunset: as native Hebrew speakers increase and modern Hebrew becomes the primary linguistic medium, the liturgical reading constraint's function shifts from primary language preservation to cultural/religious maintenance. Has sunset clause: the constraint's role as primary language preservation mechanism is expected to decline as native speakers become the norm.
constraint_indexing:constraint_classification(liturgical_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, continuous ritual use of a language is a natural mechanism for linguistic preservation: any language used regularly in structured contexts will maintain its phonological, morphological, and syntactic features across generations. This perspective sees the constraint as an immutable property of how languages persist through time. However, this classification is a false summit: the constraint naturalizes what is actually a contingent institutional arrangement (the choice to maintain Hebrew through liturgy rather than through other mechanisms, the rabbinic authority structure that enforces standardization, the community commitment to ritual practice). The engine will reclassify this as a false summit, revealing the naturalization.
constraint_indexing:constraint_classification(liturgical_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liturgical_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(liturgical_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liturgical_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low. The constraint's primary function is genuine coordination — maintaining linguistic continuity across generations through ritual practice. The rabbinic authority benefits from the constraint, but the benefit derives from the coordination function (maintaining the tradition) rather than from suppression or coercion. No identifiable victims exist; even non-comprehending participants benefit from community cohesion and cultural continuity. The extractiveness is slightly above zero (not a pure coordination mechanism) because the rabbinic authority does exercise interpretive control and some participants experience the constraint as obligatory rather than voluntary. Suppression (0.25): Low-moderate. The constraint operates through social cohesion and religious obligation rather than through coercive enforcement. Participants can exit by leaving the community, but the cost is high (loss of religious identity, cultural belonging, family ties). The suppression value reflects the social pressure to participate and the limited alternatives for maintaining Hebrew literacy outside the liturgical context. Theater ratio (0.35): Low-moderate. The constraint is functionally oriented toward linguistic preservation, but some participants engage in phonetic recitation without comprehension, which is performative rather than functionally engaged. The theater ratio has increased slightly over the interval (from 0.25 to 0.35) as the proportion of non-native speakers participating in liturgical reading has increased, particularly in diaspora communities where Hebrew is not the primary daily language. The increase reflects the constraint's shift from a mechanism for native speakers to maintain their language to a mechanism for non-native speakers to participate in cultural practice.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates significant perspectival divergence across different observer positions. The rabbinic authority and the liturgical community participant both see Rope (genuine coordination), but from different structural positions: the authority benefits through institutional continuity, while the participant benefits through linguistic and cultural preservation. The non-Hebrew-speaking participant sees Tangled Rope (mixed coordination and extraction) because the constraint requires participation without comprehension. The modern Hebrew education system sees Scaffold (temporary mechanism with sunset) because it views the constraint as transitional — as native speakers become the norm, the liturgical reading constraint's function shifts from primary language preservation to cultural/religious maintenance. The analytical observer risks seeing Mountain (natural law of linguistic preservation) but this is a false summit: the constraint naturalizes what is actually a contingent institutional arrangement (the choice to maintain Hebrew through liturgy, the rabbinic authority structure, the community commitment to ritual practice). The perspectival gap reveals that the constraint's classification depends critically on whether the observer sees linguistic life as requiring native daily use (native daily reading) or as achievable through symbolic preservation (liturgical reading).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position relative to the constraint. The rabbinic authority has low d (0.2-0.3) because they are a net beneficiary with arbitrage exit options — they can shift interpretive frameworks or recontextualize texts without losing authority. The liturgical community participant has moderate d (0.4-0.5) because they benefit from the constraint but face constrained exit (leaving the community carries social cost). The non-Hebrew-speaking participant has high d (0.6-0.7) because they are identity-locked to the community and bear the extraction of labor without full comprehension, but they also benefit from community cohesion. The modern Hebrew education system has low d (0.2-0.3) because they are a net beneficiary with constrained but manageable exit options. The analytical observer has neutral d (0.5) because they are not a participant in the constraint but an external analyst. The engine derives d from beneficiary/victim declarations and exit options; the directionality logic shows how these structural parameters map to experienced extractiveness.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint does not exhibit mandatrophy in the classical sense (a mandate that has outlived its function). The liturgical reading constraint's original mandate was to preserve Hebrew as a living language during the diaspora period when native speakers were dispersed and Hebrew was not a primary daily language. That mandate remains live: the constraint continues to preserve Hebrew's linguistic structure and maintain community cohesion. However, the constraint's function has shifted over time. In the pre-1880s period, the constraint was the primary mechanism for Hebrew preservation. In the modern period (post-1880s), with the emergence of native Hebrew speakers and the establishment of Hebrew as a primary language in Israel, the constraint's function has become secondary — it now serves cultural and religious maintenance rather than primary language preservation. The Modern Hebrew Education System perspective (Scaffold) captures this shift: the constraint is transitional, with a sunset as native speakers become the norm. The constraint does not exhibit mandatrophy because the mandate (preserve Hebrew) remains live, but it does exhibit functional shift (from primary to secondary preservation mechanism). The analytical observer's false summit classification reveals the risk of naturalizing this contingent institutional arrangement as an immutable property of linguistic preservation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_vs_native_speaker_distinction,
    'Does linguistic continuity through ritual reading constitute ''living language'' in the same sense as native daily speech?',
    'Comparative analysis of linguistic innovation rates, grammatical stability, and semantic drift in liturgical-only vs. native-speaker Hebrew; examination of whether liturgical Hebrew constrains or enables modern Hebrew development',
    'If liturgical reading is sufficient for ''living language'' status: the constraint is pure coordination (Rope). If native daily use is required: the constraint is partial preservation (Scaffold with sunset). If the two are incommensurable: the constraint is a false summit (naturalization of a specific reading of ''linguistic life'').',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(liturgical_vs_native_speaker_distinction, conceptual, 'Whether liturgical reading constitutes linguistic life equivalent to native speech').

omega_variable(
    rabbinic_authority_extraction_ambiguity,
    'Does the rabbinic interpretive authority benefit from the constraint through genuine coordination (maintaining the tradition) or through institutional extraction (controlling the meaning-making apparatus)?',
    'Historical analysis of rabbinic authority''s power sources: does authority derive from the coordination function (solving the problem of linguistic/textual continuity) or from the monopoly on interpretation? Examination of whether alternative interpretive authorities could emerge without destabilizing the constraint.',
    'If coordination-derived: the constraint is Rope from the rabbinic perspective. If extraction-derived: the constraint is Tangled Rope (coordination function + asymmetric extraction). If both: the constraint is Tangled Rope with high beneficiary extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rabbinic_authority_extraction_ambiguity, empirical, 'Whether rabbinic authority benefits from coordination or extraction').

omega_variable(
    symbolic_preservation_sufficiency,
    'Is symbolic preservation through ritual use sufficient to maintain a language''s structural integrity, or does it require active cognitive engagement and comprehension by participants?',
    'Linguistic analysis of Hebrew''s structural stability across the liturgical-only period (pre-1880s) vs. the modern native-speaker period; examination of whether non-comprehending participants contribute to linguistic preservation or merely perform it',
    'If symbolic preservation is sufficient: the constraint is Rope (genuine coordination mechanism). If comprehension is required: the constraint is Piton (performative maintenance without functional preservation). If partial: the constraint is Tangled Rope (some coordination, some theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symbolic_preservation_sufficiency, empirical, 'Whether symbolic preservation maintains linguistic structure without comprehension').

omega_variable(
    reading_vs_sibling_kernel_ambiguity,
    'Is the liturgical reading of ''Hebrew as living language'' a distinct constraint from the native daily reading and the continuity narrative reading, or are these three readings of the same constraint viewed from different perspectives?',
    'Structural analysis of ε values: if the three readings have substantially different extractiveness values (liturgical ~0.15, native daily ~0.05, continuity narrative ~0.35), they are distinct constraints. If ε values are similar, they are perspectival readings of one constraint.',
    'If distinct constraints: each reading gets its own story with its own beneficiary/victim structure. If perspectival readings: the three should be collapsed into one story with multiple perspectives. The current authoring assumes distinct constraints (separate stories linked via network.affects_constraints).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_kernel_ambiguity, conceptual, 'Whether liturgical reading is a distinct constraint or a perspective on a shared kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liturgical_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(litur_theater_t0, liturgical_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(litur_theater_t50, liturgical_reading, theater_ratio, 50, 0.3).
narrative_ontology:measurement(litur_theater_t100, liturgical_reading, theater_ratio, 100, 0.35).

% Extraction over time
narrative_ontology:measurement(litur_extract_t0, liturgical_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(litur_extract_t50, liturgical_reading, base_extractiveness, 50, 0.12).
narrative_ontology:measurement(litur_extract_t100, liturgical_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(litur_suppress_t0, liturgical_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(litur_suppress_t50, liturgical_reading, suppression_requirement, 50, 0.23).
narrative_ontology:measurement(litur_suppress_t100, liturgical_reading, suppression_requirement, 100, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liturgical_reading, information_standard).
narrative_ontology:affects_constraint(liturgical_reading, native_daily_reading).
narrative_ontology:affects_constraint(liturgical_reading, continuity_narrative_reading).

% DUAL FORMULATION NOTE:
% The liturgical reading is one of three structurally distinct constraints that decompose the contested kernel 'Hebrew as living language.' The three readings have different ε values (liturgical ~0.15, native daily ~0.05, continuity narrative ~0.35) and different beneficiary/victim structures. They are linked via network.affects_constraints because they share a common kernel and influence each other's legitimacy conditions. The liturgical reading provides the linguistic foundation that the native daily reading builds upon; the continuity narrative reading uses the liturgical tradition as evidence for unbroken linguistic continuity. Each reading should be authored as a separate constraint story with its own perspectives, beneficiaries/victims, and measurements.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
