% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__liturgical_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__liturgical_continuity_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: hebrew_living_language__liturgical_continuity_reading
 *   human_readable: Hebrew Liturgical Continuity: Living Language Through Unbroken Recitation
 *   domain: historical_linguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   Hebrew in diaspora Jewish communities persists as a living language
 *   through unbroken liturgical recitation and textual study across
 *   generations and geographies. This reading asserts that the constraint—the
 *   socially-enforced practice of communal prayer in Hebrew, Torah study
 *   circles, and textual commentary—keeps the language generatively competent
 *   and semantically rich, despite the absence of native daily speakers. The
 *   constraint is not universal (it applies only to diaspora communities
 *   participating in the liturgical tradition) and not coercive
 *   (participation is voluntary, though culturally embedded). The reading is
 *   one of three contested framings of how Hebrew survived: liturgical
 *   continuity (this reading), literary revival through Haskalah (secular
 *   intellectual production), and native generation (territorial revival via
 *   childhood acquisition in Israel). Each reading emphasizes a different
 *   mechanism and claims different evidence of linguistic 'aliveness.'
 *
 * KEY AGENTS:
 *   - Liturgical communities: Organized, geographically dispersed Jewish communities (Ashkenazi, Sephardi, Mizrahi) maintaining Hebrew through prayer and study
 *   - Textual interpretive tradition: The non-agent corpus of Talmudic and liturgical commentary, preserved and regenerated through continued study
 *   - Hebrew learners: Individuals acquiring competence through liturgical exposure and textual study
 *   - Secular Zionists: Excluded parties pursuing native-language revival via territorial settlement and childhood acquisition (19th–20th century)
 *   - Analytic linguists: Observers measuring whether liturgical constraints constitute genuine linguistic continuity or ritualized performance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__liturgical_continuity_reading, 0.12).
domain_priors:suppression_score(hebrew_living_language__liturgical_continuity_reading, 0.08).
domain_priors:theater_ratio(hebrew_living_language__liturgical_continuity_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, extractiveness, 0.12).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, accessibility_collapse, 0.92).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_continuity_reading, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_continuity_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__liturgical_continuity_reading, "Hebrew Liturgical Continuity: Living Language Through Unbroken Recitation").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_continuity_reading, "historical_linguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_continuity_reading, 'd9da6c98-f8c3-477d-a5f9-a984d97401e4').
narrative_ontology:cs_kernel_codification('d9da6c98-f8c3-477d-a5f9-a984d97401e4', distributed).
narrative_ontology:cs_authority_grounding('d9da6c98-f8c3-477d-a5f9-a984d97401e4', lineage).
narrative_ontology:cs_interpretation_layer_present('d9da6c98-f8c3-477d-a5f9-a984d97401e4').
narrative_ontology:cs_reading_relation('d9da6c98-f8c3-477d-a5f9-a984d97401e4', hebrew_living_language__native_generation_reading, coexists_with).
narrative_ontology:cs_reading_relation('d9da6c98-f8c3-477d-a5f9-a984d97401e4', hebrew_living_language__literary_revival_reading, coexists_with).
narrative_ontology:cs_axiom('d9da6c98-f8c3-477d-a5f9-a984d97401e4', foundational, diaspora_liturgical_recitation_preserves_competence).
narrative_ontology:cs_axiom_status(diaspora_liturgical_recitation_preserves_competence, holdable).
narrative_ontology:cs_axiom_grounding('d9da6c98-f8c3-477d-a5f9-a984d97401e4', diaspora_liturgical_recitation_preserves_competence, empirically_contingent).
narrative_ontology:cs_axiom('d9da6c98-f8c3-477d-a5f9-a984d97401e4', foundational, textual_interpretive_continuity_constitutes_linguistic_aliveness).
narrative_ontology:cs_axiom_status(textual_interpretive_continuity_constitutes_linguistic_aliveness, holdable).
narrative_ontology:cs_axiom_grounding('d9da6c98-f8c3-477d-a5f9-a984d97401e4', textual_interpretive_continuity_constitutes_linguistic_aliveness, conventional).
narrative_ontology:cs_reference_frame('d9da6c98-f8c3-477d-a5f9-a984d97401e4', post_temple_destruction_diaspora_preservation).
narrative_ontology:cs_drift_state('d9da6c98-f8c3-477d-a5f9-a984d97401e4', contemporary_global_diaspora, gap(stable, minor, true)).
narrative_ontology:cs_created_at('d9da6c98-f8c3-477d-a5f9-a984d97401e4', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, liturgical_communities).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, textual_interpretive_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_continuity_reading, hebrew_learners).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, liturgical_hebrew_is_living_language).
narrative_ontology:constraint_vindicates(hebrew_living_language__liturgical_continuity_reading, diaspora_continuity_preserves_linguistic_competence).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Jewish communities (Ashkenazi, Sephardi, Mizrahi, other diaspora branches) maintain Hebrew through prayer services, Torah study, and textual exegesis. They collectively preserve the language's semantic depth, grammatical structure, and religious meaning-system across generations. Participation is voluntary; individuals can leave the community, but the linguistic continuity depends on sustained collective recitation and study.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, liturgical_communities, beneficiary,
    organized, generational, mobile, global).

% The accumulated corpus of Talmudic interpretation, liturgical commentary, and textual analysis (Midrash, Responsa, later scholarly work) is preserved and regenerated through continued study. Each generation of interpreters reads the same texts and adds new layers of meaning, keeping the tradition alive as a living epistemic practice, not a museum artifact.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, textual_interpretive_tradition, beneficiary,
    analytical, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(hebrew_living_language__liturgical_continuity_reading, textual_interpretive_tradition).

% Students and practitioners who enter the tradition (including secular scholars, converts, cultural enthusiasts, and children socialized into the community) acquire Hebrew competence through liturgical exposure, textual study, and memorized recitation. They become carriers of the language and, through continued engagement, reproducers of the interpretive tradition.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, hebrew_learners, beneficiary,
    moderate, biographical, mobile, global).

% Late 19th/early 20th century Zionist intellectuals and activists who seek to revive Hebrew as a native, generatively-spoken language for a territorial nation-state. They view the liturgical reading as passive preservation rather than true linguistic revival. Their exclusion from the liturgical-continuity constraint is structural: they pursue a different revitalization model (native-generation) and would contest whether memorized recitation constitutes a 'living' language at all.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, secular_zionists, excluded,
    organized, generational, mobile, national).

% Scholars who study language preservation, dead/living language boundaries, and linguistic continuity mechanisms. They examine whether the liturgical constraint preserves generative competence or only ritualized production. Their role is to measure whether the constraint truly sustains linguistic aliveness or merely maintains a symbolic memorial to the language.
narrative_ontology:constraint_stakeholder(hebrew_living_language__liturgical_continuity_reading, analytic_linguists, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_living_language__liturgical_continuity_reading, diffuse).
narrative_ontology:fixing_cost_class(hebrew_living_language__liturgical_continuity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of preserving a high-semantic-density, grammatically complex language across diaspora populations separated by geography, political authority, and vernacular drift. Liturgical recitation and textual study are low-cost, high-fidelity transmission mechanisms that do not require territorial concentration or political sovereignty. The community collectively maintains the language's meaning-system and grammatical structure through coordinated repetition and interpretive engagement.
% TRANSFER_FUNCTION: Moves linguistic and cultural authority from one generation to the next through memorized texts, prescribed recitation patterns, and interpretive commentary. The constraint transfers the burden of memorization and study onto each generation of participants, who must invest time and cognitive effort to maintain competence. What transfers is not material wealth but linguistic and symbolic capital.
% ABSENT_VOICES: Secular Zionists and later native-speaker revivalists (who pursued territorially-grounded Hebrew revival in Palestine/Israel) would argue that liturgical recitation is performative, not generative—that it preserves symbols but not living language. They are structurally excluded from the liturgical-continuity reading because they propose a different mechanism (native childhood acquisition) and reject the memorized-recitation model as insufficient for linguistic aliveness. Post-1948 Israeli native speakers, for whom Hebrew became a primary daily language, represent a completed alternative mechanism that the liturgical-constraint reading does not cover.
% DISAPPEARANCE_RATIONALE: If the liturgical constraint dissolved overnight (diaspora communities ceased collective recitation and study), the language's transmission would collapse within a single generation. Diaspora communities without a territorial homeland cannot maintain a language through daily vernacular use alone—the liturgical framework IS the mechanism. However, secular Zionists and modern Israelis would contest the verdict: they would argue that Hebrew's true linguistic aliveness resides in native generative speech (which has survived and flourished independent of the liturgical constraint since 1948), not in memorized recitation. The disappearance would not kill the language because a different mechanism (native generation) sustains it in Israel; it would kill only the diaspora liturgical tradition's role in preservation.
% FOUNDING_PROBLEM: Following the Roman destruction of the Second Temple (70 CE) and the dispersion of Jewish populations across the Mediterranean and beyond, Hebrew was no longer a vernacular language of daily life in any major diaspora population. Yet the Jewish textual tradition (Torah, Talmud, prayer liturgy) was written in Hebrew. Without a transmission mechanism, the language would become a dead ceremonial marker within a few generations. The founding problem is: how does a diaspora population maintain linguistic competence in a language that is not spoken natively in their territory, yet is essential to their textual-religious tradition?
% FOUNDING_PROBLEM_CORROBORATION: Documented by Jewish historical sources (Talmud, medieval responsa) and modern linguistics scholarship on diaspora language maintenance: Saadia Gaon's Hebrew grammar (10th century), Maimonides' legislative framework for community education (12th century), and contemporary sociolinguistic studies (Fishman, Ravid, Rabin) all attest that the founding problem remains unsolved by any alternative mechanism in diaspora contexts. Post-1948 Israel represents a solved instance via native-generation revival, but diaspora communities still depend on the liturgical constraint to maintain Hebrew competence.
narrative_ontology:disappearance_verdict(hebrew_living_language__liturgical_continuity_reading, contested).
narrative_ontology:founding_problem_status(hebrew_living_language__liturgical_continuity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_living_language__liturgical_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(hebrew_living_language__liturgical_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_living_language__liturgical_continuity_reading, 0.12, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__liturgical_continuity_reading_tests).
:- end_tests(hebrew_living_language__liturgical_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is very low (0.12) because the constraint solves a genuine collective-action problem (diaspora language preservation) with minimal coercive overhead. No party benefits asymmetrically—liturgical communities voluntarily participate in both the recitation and the study. The beneficiaries (liturgical communities and the interpretive tradition) are the same parties doing the work. Suppression is minimal (0.08) because the constraint is maintained by cultural preference, not external force—individuals can exit the community and lose Hebrew competence without legal or violent barrier. Theater is very low (0.05) because the recitation serves the stated function (transmission and meaning-making) rather than masking hidden extraction. Accessibility collapse is very high (0.92) because once diaspora Hebrew speakers understand the constraint, they recognize that this is the ONLY mechanism available to preserve the language without territorial settlement. Resistance is minimal (0.03) because the constraint is experienced as voluntary participation in a valued tradition, not as coercive extraction. The measurement series shows very slight oscillation: minor rise (periods 1–3) tracking periods of diaspora consolidation and renewed study commitment, then stability (periods 4–6) as the constraint reaches equilibrium. This pattern reflects real historical cycles of liturgical renewal (Hasidic revivals, Enlightenment challenges, post-Holocaust reconstruction) without sustained drift in the underlying mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The liturgical-continuity reading and the native-generation reading (sibling: hebrew_living_language__native_generation_reading) produce very different seat divergence. From the liturgical seat, the constraint is pure coordination—communities voluntarily sustain it because it serves their linguistic and religious function. From the secular-Zionist / native-speaker seat, the same constraint is performative theater that obscures the real linguistic revival happening through childhood acquisition in Israel. The engine will compute different types from the two seats because the structural data differ: the liturgical seat experiences low extractiveness and voluntary participation; the native-speaker seat experiences the diaspora constraint as historically inert, replaced by the generative native-speech mechanism. This is not a perspectival disagreement—it is a structural disagreement about which mechanism sustains the language. The two readings are not perspectives on one constraint; they are two distinct constraints (one per reading) with different referents: this reading's ε measures the diaspora liturgical mechanism; the native-generation reading's ε measures childhood acquisition in Israel. The ε-invariance principle requires two separate constraint stories, linked via network.affects_constraints, because their metrics would differ by a wide margin if measured against the same observable.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are liturgical communities (low d, near 0.1–0.2) and the textual-interpretive tradition (analytical seat, d = 0.5 by structural symmetry—neither benefits nor bears cost, purely observational). There are no victims because the constraint is not extractive; all parties who participate in it do so voluntarily and benefit from the coordination it provides. Hebrew learners are also beneficiaries (acquire competence without coercion). Secular Zionists are excluded, not paying—they exit the constraint entirely and pursue a different mechanism. The directionality derivation is straightforward: beneficiaries with mobile exit options → low d; no victims; no extraction → no high-d seats. This is a classic rope profile: low extractiveness, low suppression, high accessibility collapse (once you understand diaspora demographics, the liturgical mechanism becomes obvious as the only alternative to language death), minimal resistance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to preserve diaspora Hebrew competence without territorial native-speaker base) was live at the constraint's inception (post-70 CE destruction of the Second Temple). The problem remains live in diaspora contexts as of this story's observation point—liturgical communities still face the choice: either maintain the constraint or allow Hebrew to become a dead ceremonial language. However, the mandatrophy question is more subtle here because of the sibling readings. The native-generation reading (Hebrew revived through Zionist settlement and childhood acquisition in Israel) partially solves the founding problem in a TERRITORIAL context, but does not solve it in diaspora contexts. The literary-revival reading (Haskalah intellectual production) keeps Hebrew semantically alive without solving the transmission problem for whole communities. The liturgical-continuity reading uniquely solves the diaspora problem. Therefore, mandatrophy is NOT resolved in the diaspora seat—the founding problem persists. In the Israeli seat (where the native-generation constraint applies), mandatrophy is resolved because the problem has been superseded. This story is correctly classified as rope in the diaspora context precisely because the founding problem persists and the coordination function remains necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    liturgical_vs_generative_competence,
    'Does unbroken liturgical recitation and textual study preserve genuine generative linguistic competence, or only ritualized production of memorized utterances?',
    'Controlled linguistic analysis: compare diaspora liturgical Hebrew speakers'' performance on novel grammatical constructions, semantic composition, and productive morphology against (a) native speakers and (b) speakers with memorization-only training. If diaspora speakers generatively produce new structures at rates approaching native speakers, the constraint preserves competence; if rates are near memorization-only baselines, the constraint preserves performance not competence.',
    'If the constraint preserves only ritualized performance, it may be more appropriately classified as piton (degraded rope, maintained theatrically) than rope. If it preserves genuine competence, the rope classification holds. This impacts whether diaspora Hebrew is ''living'' by linguistic standards or merely ''symbolically maintained.''',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(liturgical_vs_generative_competence, empirical, 'Whether liturgical recitation sustains generative linguistic competence or only ritualized utterance production.').

omega_variable(
    sibling_reading_foreclosure,
    'Do the three readings (liturgical-continuity, literary-revival, native-generation) mutually foreclose each other, coexist as live alternatives, or stand in influence relationships?',
    'Examine the logical structure of each reading''s core claim: (1) does asserting liturgical continuity preserve language require denying that literary revival or native generation do? (2) or can all three mechanisms operate in parallel contexts without contradiction? (3) does one reading create structural conditions that change the other''s viability?',
    'If readings mutually foreclose: the kernel resolves to one true mechanism and two false readings. If they coexist: they remain live positions held by different communities/traditions and should stay as separate constraint stories linked by network.affects_constraints. If influence relationship: one reading (e.g., native-generation) creates downstream pressure that changes how the others operate but doesn''t eliminate them. This affects how the constraint family is structured in the compiled knowledge base.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Logical relationships between the three competing readings of how Hebrew remains living.').

omega_variable(
    voluntariness_vs_identity_lock,
    'Is diaspora participation in liturgical communities truly voluntary (mobile exit) or is it identity-locked (exit carries unbearable identity cost)?',
    'Historical and sociological study of exit patterns: measure the proportion of diaspora Hebrew speakers who leave the liturgical community with vs. without significant identity reconstruction. Post-exit narrative analysis: do exiters report exit as freely chosen or as identity rupture? Compare against exit rates from similarly-embedded but non-identity-constitutive communities.',
    'If truly mobile, the exit_options remain ''mobile'' and directionality is low (d ≈ 0.15). If identity-locked, exit_options shift to ''identity_locked'' and directionality may increase (d ≈ 0.3–0.4) even without material extraction, because the suppression is internalized. This affects the effective extraction (χ) computation and could shift the type from rope toward tangled-rope if exit becomes sufficiently costly in identity terms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(voluntariness_vs_identity_lock, empirical, 'Whether diaspora participation in liturgical Hebrew is maintained by voluntary coordination or by identity-fusion that makes exit costly.').

omega_variable(
    diaspora_vs_territorial_scope,
    'Is the liturgical-continuity constraint fundamentally a diaspora phenomenon, or does it apply equally in territorial (Israeli) contexts?',
    'Examine whether Israeli native speakers of Hebrew participate in the liturgical recitation constraint at similar rates and intensity as diaspora speakers. If participation rates drop sharply when native daily speech becomes available, the constraint is diaspora-specific (spatial_scope = diaspora, not universal). If rates remain stable, the constraint may be universal (participation decoupled from native-speech availability).',
    'If diaspora-specific: the constraint''s spatial_scope should be recoded from ''global'' to a new value or clarified as conditional on diaspora context. The founding problem (preserve language without native daily speakers) does not apply in Israel; therefore the constraint''s necessity differs by territory. If universal: the constraint operates identically regardless of native-speaker availability, suggesting a different founding problem (preserve the interpretive tradition itself, not just the language). This affects stakeholder analysis and beneficiary definition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(diaspora_vs_territorial_scope, empirical, 'Whether the liturgical-continuity constraint applies universally or is specific to diaspora contexts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_continuity_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement_basis(hebr_tr_t0, observed).
narrative_ontology:measurement(hebr_tr_t4, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 4, 0.04).
narrative_ontology:measurement_basis(hebr_tr_t4, observed).
narrative_ontology:measurement(hebr_tr_t8, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 8, 0.05).
narrative_ontology:measurement_basis(hebr_tr_t8, observed).
narrative_ontology:measurement(hebr_tr_t12, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 12, 0.06).
narrative_ontology:measurement_basis(hebr_tr_t12, observed).
narrative_ontology:measurement(hebr_tr_t16, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 16, 0.05).
narrative_ontology:measurement_basis(hebr_tr_t16, observed).
narrative_ontology:measurement(hebr_tr_t20, hebrew_living_language__liturgical_continuity_reading, theater_ratio, 20, 0.05).
narrative_ontology:measurement_basis(hebr_tr_t20, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement_basis(hebr_be_t0, observed).
narrative_ontology:measurement(hebr_be_t4, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 4, 0.11).
narrative_ontology:measurement_basis(hebr_be_t4, observed).
narrative_ontology:measurement(hebr_be_t8, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 8, 0.12).
narrative_ontology:measurement_basis(hebr_be_t8, observed).
narrative_ontology:measurement(hebr_be_t12, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 12, 0.13).
narrative_ontology:measurement_basis(hebr_be_t12, observed).
narrative_ontology:measurement(hebr_be_t16, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 16, 0.12).
narrative_ontology:measurement_basis(hebr_be_t16, observed).
narrative_ontology:measurement(hebr_be_t20, hebrew_living_language__liturgical_continuity_reading, base_extractiveness, 20, 0.12).
narrative_ontology:measurement_basis(hebr_be_t20, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t0, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 0, 0.06).
narrative_ontology:measurement_basis(hebr_su_t0, observed).
narrative_ontology:measurement(hebr_su_t4, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 4, 0.07).
narrative_ontology:measurement_basis(hebr_su_t4, observed).
narrative_ontology:measurement(hebr_su_t8, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 8, 0.08).
narrative_ontology:measurement_basis(hebr_su_t8, observed).
narrative_ontology:measurement(hebr_su_t12, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 12, 0.09).
narrative_ontology:measurement_basis(hebr_su_t12, observed).
narrative_ontology:measurement(hebr_su_t16, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 16, 0.08).
narrative_ontology:measurement_basis(hebr_su_t16, observed).
narrative_ontology:measurement(hebr_su_t20, hebrew_living_language__liturgical_continuity_reading, suppression_requirement, 20, 0.08).
narrative_ontology:measurement_basis(hebr_su_t20, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__liturgical_continuity_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language__liturgical_continuity_reading, 0.12).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__native_generation_reading).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_continuity_reading, hebrew_living_language__literary_revival_reading).

% DUAL FORMULATION NOTE:
% The hebrew_living_language kernel decomposes into three structurally distinct constraints, one per reading. Each reading defines a different mechanism for Hebrew preservation: liturgical-continuity (diaspora recitation/study), native-generation (Israeli childhood acquisition), and literary-revival (Haskalah intellectual production). The three ε values differ sharply because the referents differ—each reading measures the extractiveness of its proposed preservation mechanism. Liturgical-continuity shows low ε (0.12) because it solves a genuine diaspora coordination problem with no asymmetric extraction. Native-generation (sibling: hebrew_living_language__native_generation_reading) would show higher ε because native-speaker emergence is contested (some read it as genuine linguistic revival, others as Zionist political project imposed on Arabic-speaking Palestinian populations, creating victims and asymmetric power dynamics). Literary-revival (sibling: hebrew_living_language__literary_revival_reading) shows moderate ε because intellectual-elite production benefits Enlightenment intellectuals asymmetrically while excluding working-class and traditional communities from the meaning-making process. All three are live historical positions held by different Jewish communities and intellectual movements; they coexist rather than foreclose. They are linked via network.affects_constraints because each reading's viability influences the others' structural conditions: if native generation succeeds (Israeli statehood), the diaspora liturgical constraint becomes historically contingent rather than necessary; if literary revival succeeds (secular intellectualism), the religious interpretive tradition loses exclusive authority to define Hebrew's meaning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
