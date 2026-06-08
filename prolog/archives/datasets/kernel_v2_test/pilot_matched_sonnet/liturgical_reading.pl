% ============================================================================
% CONSTRAINT STORY: liturgical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-08
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
    narrative_ontology:constraint_vindicates/2,
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
 *   human_readable: Hebrew Living Language Through Liturgical Use (Liturgical Reading)
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The liturgical reading of Hebrew as a living language holds that
 *   continuous ritual use — in prayer, Torah reading, lifecycle ceremonies,
 *   and holiday observances — constitutes linguistic life. This reading does
 *   not require native daily speakers; it requires only that the language
 *   remain in active use within its ritual domain. Hebrew is 'living' because
 *   it is spoken, chanted, and read aloud in synagogues every week,
 *   transmitted from generation to generation through religious education,
 *   and used to mark the most significant moments of Jewish life (birth,
 *   coming of age, marriage, death). The constraint coordinates diaspora
 *   Jewish communities around a shared textual and liturgical tradition,
 *   solving the collective-action problem of maintaining linguistic
 *   continuity across geographic dispersion and temporal distance. The
 *   rabbinic interpretive authority maintains the tradition's accuracy and
 *   standardization, but this is a coordination service rather than
 *   extraction — the authority structure preserves the shared resource rather
 *   than capturing rents from it. This reading contrasts with the
 *   native_daily_reading (which requires daily vernacular use for linguistic
 *   life) and the continuity_narrative_reading (which treats any unbroken
 *   transmission as sufficient, regardless of domain). The liturgical reading
 *   is the traditional rabbinic position, held across Orthodox, Conservative,
 *   and many Reform communities, and represents the dominant framework for
 *   understanding Hebrew's status during the 1800-year diaspora period
 *   between the Roman exile and the Zionist revival.
 *
 * KEY AGENTS:
 *   - Individual Congregant: Participant (powerless/constrained) — uses liturgical Hebrew in weekly services and lifecycle events; benefits from coordination without bearing asymmetric costs
 *   - Rabbinic Interpretive Authority: Coordinator (institutional/arbitrage) — maintains liturgical standards, trains educators, preserves textual accuracy; benefits from institutional position but provides genuine coordination service
 *   - Liturgical Communities: Organized users (organized/mobile) — synagogues and prayer groups coordinate around shared liturgical Hebrew; choose this framework for its coordination value
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees liturgical use as genuine coordination mechanism for maintaining linguistic continuity across diaspora
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liturgical_reading, 0.18).
domain_priors:suppression_score(liturgical_reading, 0.25).
domain_priors:theater_ratio(liturgical_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liturgical_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(liturgical_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(liturgical_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liturgical_reading, rope).
narrative_ontology:human_readable(liturgical_reading, "Hebrew Living Language Through Liturgical Use (Liturgical Reading)").
narrative_ontology:topic_domain(liturgical_reading, "sociolinguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(liturgical_reading, '875019f4-ccf4-431f-8a37-22b3f9e1565d').
narrative_ontology:cs_kernel_codification('875019f4-ccf4-431f-8a37-22b3f9e1565d', fixed_text).
narrative_ontology:cs_authority_grounding('875019f4-ccf4-431f-8a37-22b3f9e1565d', lineage).
narrative_ontology:cs_interpretation_layer_present('875019f4-ccf4-431f-8a37-22b3f9e1565d').
narrative_ontology:cs_reading_relation('875019f4-ccf4-431f-8a37-22b3f9e1565d', liturgical_reading__native_daily_reading, coexists_with).
narrative_ontology:cs_reading_relation('875019f4-ccf4-431f-8a37-22b3f9e1565d', liturgical_reading__continuity_narrative_reading, coexists_with).
narrative_ontology:cs_axiom('875019f4-ccf4-431f-8a37-22b3f9e1565d', foundational, ritual_domain_sufficiency).
narrative_ontology:cs_axiom_status(ritual_domain_sufficiency, holdable).
narrative_ontology:cs_axiom_grounding('875019f4-ccf4-431f-8a37-22b3f9e1565d', ritual_domain_sufficiency, conventional).
narrative_ontology:cs_axiom('875019f4-ccf4-431f-8a37-22b3f9e1565d', secondary, daily_use_not_required).
narrative_ontology:cs_axiom_status(daily_use_not_required, holdable).
narrative_ontology:cs_axiom_grounding('875019f4-ccf4-431f-8a37-22b3f9e1565d', daily_use_not_required, conventional).
narrative_ontology:cs_reference_frame('875019f4-ccf4-431f-8a37-22b3f9e1565d', second_temple_liturgical_standard).
narrative_ontology:cs_drift_state('875019f4-ccf4-431f-8a37-22b3f9e1565d', contemporary, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('875019f4-ccf4-431f-8a37-22b3f9e1565d', '').
narrative_ontology:cs_kernel_id(liturgical_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liturgical_reading, rabbinic_interpretive_authority).
narrative_ontology:constraint_beneficiary(liturgical_reading, liturgical_communities).
narrative_ontology:constraint_vindicates(liturgical_reading, ritual_continuity_constitutes_life).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIVIDUAL CONGREGANT (ROPE) — Participates in liturgical Hebrew through weekly services, lifecycle events, and holiday observances. Experiences the constraint as coordination: shared ritual language enables collective worship and maintains connection to tradition. Exit is constrained (leaving the community has social cost) but extraction is minimal — the congregant benefits from the coordination function without bearing asymmetric costs.
constraint_indexing:constraint_classification(liturgical_reading, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: RABBINIC INTERPRETIVE AUTHORITY (ROPE) — Maintains the liturgical tradition and interpretive framework. Benefits from institutional position but also provides genuine coordination service: standardizing pronunciation, preserving textual accuracy, training cantors and educators. Arbitrage exit available (could shift to other forms of religious authority) but chooses to maintain this role. Low extraction — the authority structure serves a real coordination function.
constraint_indexing:constraint_classification(liturgical_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: LITURGICAL COMMUNITIES (ROPE) — Synagogues, prayer groups, and religious schools coordinate around shared liturgical Hebrew. Mobile exit (communities can adopt vernacular prayer or alternative traditions) but choose liturgical Hebrew for its coordination value. Minimal extraction — the constraint solves the genuine problem of maintaining shared ritual practice across geographic and temporal distance.
constraint_indexing:constraint_classification(liturgical_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, liturgical use represents a genuine coordination mechanism for maintaining linguistic continuity across diaspora communities. The constraint solves a real collective-action problem: how to preserve a shared textual tradition when speakers are geographically dispersed and the language is not used for daily communication. Extraction is minimal — the coordination function is primary.
constraint_indexing:constraint_classification(liturgical_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liturgical_reading_tests).
:- end_tests(liturgical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The liturgical framework captures some institutional benefit for rabbinic authority (interpretive control, educational gatekeeping), but the primary function is coordination. The constraint solves a real problem: how to maintain a shared textual tradition across dispersed communities. Most participants are net beneficiaries — they gain access to a rich liturgical tradition and connection to a multi-millennial textual heritage. The modest extraction reflects that rabbinic authority does maintain some control over liturgical innovation and vernacular alternatives, but this control is substantially less than in other religious authority structures. Suppression (0.25): Low-moderate. Exit from the liturgical framework is possible (Reform Judaism has experimented with vernacular prayer; secular Jews abandon liturgical practice entirely) but carries social cost within traditional communities. Vernacular liturgical movements (Yiddish, Ladino) were marginalized but not entirely suppressed — they coexisted with Hebrew liturgy in many communities. The suppression metric reflects real barriers to liturgical innovation but not total foreclosure of alternatives. Theater ratio (0.35): Moderate-low. Some performative elements exist (rote recitation without comprehension, cantorial virtuosity prioritized over meaning), but the liturgical use is substantially functional: the language is actually used for its intended purpose (ritual worship, textual study, community coordination). The theater ratio rose slightly during the Haskalah period (1850-1900) as liturgical Hebrew became more distant from daily life, then stabilized in the modern period as Hebrew education improved.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify as Rope because the constraint is genuinely low-extraction coordination. The individual congregant, the rabbinic authority, the liturgical communities, and the analytical observer all experience the constraint as solving a real coordination problem with minimal extractive overhead. The perspectival uniformity is not an authoring error — it reflects the structural reality that liturgical Hebrew, within its own domain, is a successful coordination mechanism. The gap appears only when comparing this reading to its siblings: the native_daily_reading sees this constraint as insufficient (not 'truly' living without daily speakers) and the continuity_narrative_reading sees it as one instance of a broader pattern (any unbroken transmission constitutes life). The kernel dispute is conceptual (what counts as 'living'?), not structural (who benefits and who pays within this specific arrangement).
 *
 * DIRECTIONALITY LOGIC:
 *   All perspectives in this constraint are beneficiaries or neutral observers. The individual congregant benefits from the coordination function (access to shared ritual tradition) without bearing asymmetric costs — their directionality is low (net beneficiary). The rabbinic interpretive authority benefits from institutional position but also provides coordination service — their directionality is low to moderate (mixed beneficiary and coordinator). Liturgical communities benefit from the shared framework — their directionality is low (net beneficiary). The analytical observer is neutral. No agent is a victim in this reading — the constraint has no victim structure. This is the key structural difference from the native_daily_reading, which creates victims (non-Hebrew-speaking diaspora Jews excluded from 'authentic' Jewish life). The absence of victims is what keeps this constraint as Rope rather than Tangled Rope or Snare.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that 'living language' is a contested kernel with multiple structurally distinct readings. The liturgical_reading is Rope (low-extraction coordination around ritual use). The native_daily_reading (not this constraint) is Tangled Rope or Snare (coordination around daily use with victims: diaspora Jews excluded from authenticity). The continuity_narrative_reading (not this constraint) is Mountain or Rope (any unbroken transmission suffices). No single reading is 'correct' — the classification depends on which definition of linguistic life the observer adopts. The mandatrophy is not 'which type is Hebrew?' but 'which reading of the kernel are you measuring from?' This constraint is the liturgical reading only.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is Hebrew ''living'' through liturgical use alone, or does linguistic life require native daily speakers?',
    'This is the core kernel dispute. The liturgical_reading holds that continuous ritual use constitutes linguistic life; the native_daily_reading holds that only native speakers using the language for daily communication constitute linguistic life; the continuity_narrative_reading holds that any unbroken transmission (ritual or daily) constitutes life. Resolution depends on which definition of ''living language'' the observer adopts.',
    'If liturgical use suffices: this constraint is pure coordination (Rope). If native daily use is required: this constraint is either aspirational (Scaffold toward the Zionist revival) or extractive (Snare — rabbinic authority claims linguistic life while suppressing vernacular alternatives). If continuity alone suffices: the constraint is Mountain (any unbroken transmission is sufficient).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Core kernel ambiguity: what constitutes a ''living'' language').

omega_variable(
    interpretive_control_extraction,
    'Does rabbinic interpretive authority extract rents from the liturgical framework, or does it provide pure coordination service?',
    'Measure the degree to which rabbinic authority suppresses alternative liturgical innovations, vernacular prayer movements, or lay interpretation. If suppression is high and alternatives are blocked, extraction is present. If alternatives coexist and authority is voluntarily granted, coordination is primary.',
    'If extraction is present: reclassify from Rope to Tangled Rope (coordination + asymmetric extraction). If coordination is primary: Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_control_extraction, empirical, 'Whether rabbinic authority extracts or coordinates').

omega_variable(
    vernacular_suppression,
    'Were vernacular prayer movements (Yiddish, Ladino, Judeo-Arabic liturgies) actively suppressed, or did they coexist with Hebrew liturgy?',
    'Historical analysis of vernacular liturgical movements: Were they tolerated, marginalized, or actively suppressed by rabbinic authorities? Did communities have genuine choice between Hebrew and vernacular liturgy, or was Hebrew enforced through institutional pressure?',
    'If vernacular alternatives were suppressed: increases extractiveness and suppression metrics, potentially reclassifying to Tangled Rope or Snare. If they coexisted: confirms Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vernacular_suppression, empirical, 'Historical treatment of vernacular liturgical alternatives').

omega_variable(
    sibling_reading_structural_delta,
    'How does the native_daily_reading''s victim structure (non-Hebrew-speaking Jews excluded from ''authentic'' Jewish life) differ from this reading''s lack of victims?',
    'The native_daily_reading (Zionist revival framing) creates victims: diaspora Jews who maintain liturgical Hebrew but not daily Hebrew are positioned as linguistically inauthentic. The liturgical_reading has no such victim structure — liturgical competence is the standard, and daily use is not required. The structural delta is the presence/absence of a victim class defined by the standard of linguistic authenticity.',
    'This omega documents the key structural difference between sibling readings: the liturgical_reading is coordination without victims; the native_daily_reading is coordination with an excluded class.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Victim structure difference between liturgical and native_daily readings').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liturgical_reading, 0, 200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liturg_theater_1800, liturgical_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(liturg_theater_1850, liturgical_reading, theater_ratio, 50, 0.32).
narrative_ontology:measurement(liturg_theater_1900, liturgical_reading, theater_ratio, 100, 0.35).
narrative_ontology:measurement(liturg_theater_1950, liturgical_reading, theater_ratio, 150, 0.38).
narrative_ontology:measurement(liturg_theater_2000, liturgical_reading, theater_ratio, 200, 0.35).

% Extraction over time
narrative_ontology:measurement(liturg_extract_1800, liturgical_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(liturg_extract_1850, liturgical_reading, base_extractiveness, 50, 0.16).
narrative_ontology:measurement(liturg_extract_1900, liturgical_reading, base_extractiveness, 100, 0.18).
narrative_ontology:measurement(liturg_extract_1950, liturgical_reading, base_extractiveness, 150, 0.22).
narrative_ontology:measurement(liturg_extract_2000, liturgical_reading, base_extractiveness, 200, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liturgical_reading, identity_coordination).
narrative_ontology:affects_constraint(liturgical_reading, native_daily_reading).
narrative_ontology:affects_constraint(liturgical_reading, continuity_narrative_reading).

% DUAL FORMULATION NOTE:
% The hebrew_living_language kernel decomposes into three structurally distinct readings with different ε values and victim structures. The liturgical_reading (this constraint) has low ε and no victims. The native_daily_reading has moderate ε and creates victims (diaspora Jews excluded from authenticity). The continuity_narrative_reading has negligible ε and no victims (treats all transmission as equivalent). These are not the same constraint viewed from different angles — they are different claims about what 'living language' means, each with its own structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
