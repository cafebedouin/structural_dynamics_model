% ============================================================================
% CONSTRAINT STORY: emotional_arc_management
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_emotional_arc_management, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: emotional_arc_management
 *   human_readable: Emotional Arc Management in Digital News Curation
 *   domain: media_studies/political_economy/cognitive_infrastructure
 *
 * SUMMARY:
 *   Emotional arc management in digital news curation addresses a genuine
 *   coordination problem: how to present emotionally demanding news
 *   (political conflicts, war coverage, systemic injustice) in a format that
 *   maintains reader engagement without inducing burnout, learned
 *   helplessness, or session abandonment. The constraint operates through
 *   deliberate sequencing of heavy items with tonal relief items (human
 *   interest stories, cultural features, aesthetic content) to manage reader
 *   emotional state across a reading session. This is a low-extraction
 *   coordination mechanism that benefits readers (sustainable engagement with
 *   difficult material), editorial teams (professional craft validated by
 *   retention metrics), and platform sustainability (long-term reader
 *   relationships). The constraint exhibits uniform Rope classification
 *   across all perspectives because the coordination function is genuine and
 *   the extraction is minimal. However, three omega variables identify
 *   potential degradation pathways: (1) optimization drift from reader
 *   well-being to pure engagement metrics, (2) suppression of
 *   reader-controlled curation alternatives, and (3) replacement of
 *   editorially valuable relief content with pure emotional manipulation. The
 *   measurements show slight increases in both theater_ratio and
 *   base_extractiveness over the interval, suggesting early-stage metric
 *   substitution as A/B testing and algorithmic optimization begin to replace
 *   editorial judgment.
 *
 * KEY AGENTS:
 *   - Readers: Primary beneficiary (moderate/mobile) — receive coordination service that enables sustained engagement with difficult news without emotional collapse
 *   - Editorial Teams: Institutional beneficiary (institutional/arbitrage) — professional craft solving legitimate coordination problem; validated by reader retention
 *   - Platform Sustainability: Institutional beneficiary (institutional/arbitrage) — coordination serves mutual interests of readers and platform viability
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees coordination mechanism addressing structural mismatch between human emotional bandwidth and global information volume
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(emotional_arc_management, 0.18).
domain_priors:suppression_score(emotional_arc_management, 0.22).
domain_priors:theater_ratio(emotional_arc_management, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(emotional_arc_management, extractiveness, 0.18).
narrative_ontology:constraint_metric(emotional_arc_management, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(emotional_arc_management, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(emotional_arc_management, rope).
narrative_ontology:human_readable(emotional_arc_management, "Emotional Arc Management in Digital News Curation").
narrative_ontology:topic_domain(emotional_arc_management, "media_studies/political_economy/cognitive_infrastructure").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(emotional_arc_management, readers).
narrative_ontology:constraint_beneficiary(emotional_arc_management, editorial_teams).
narrative_ontology:constraint_beneficiary(emotional_arc_management, platform_sustainability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE READER (ROPE) — Experiences emotional arc management as a coordination service that solves the legitimate problem of information overload and emotional exhaustion. The pacing enables sustained engagement with difficult material without burnout. Mobile exit options (can switch platforms, skip items, curate own feeds) and genuine benefit from the coordination function. Low extraction — the constraint serves reader needs.
constraint_indexing:constraint_classification(emotional_arc_management, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: THE EDITORIAL TEAM (ROPE) — Sees arc management as professional craft solving the coordination problem of presenting complex, emotionally demanding news in a sustainable format. Benefits from reader retention and engagement metrics that validate editorial judgment. Arbitrage exit (can move between platforms, set editorial standards) and experiences the constraint as a coordination tool rather than extraction mechanism.
constraint_indexing:constraint_classification(emotional_arc_management, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PLATFORM SUSTAINABILITY (ROPE) — The constraint coordinates reader attention and platform viability. Preventing reader burnout and session abandonment is a genuine coordination function that benefits both readers (sustainable engagement) and platform (retention). Low extraction because the coordination serves mutual interests — readers want to stay informed without emotional collapse; platforms need sustainable engagement models.
constraint_indexing:constraint_classification(emotional_arc_management, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, emotional arc management is a coordination mechanism addressing the structural mismatch between human emotional bandwidth and the volume/intensity of newsworthy events in a globally connected information environment. The constraint solves a real collective action problem: how to maintain an informed public without inducing learned helplessness or disengagement. Low extraction, genuine coordination function, minimal suppression of alternatives (readers can choose unmanaged feeds, RSS, social media).
constraint_indexing:constraint_classification(emotional_arc_management, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(emotional_arc_management_tests).
:- end_tests(emotional_arc_management_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The constraint extracts minimal value asymmetrically — readers receive genuine coordination benefit (sustainable engagement), editorial teams receive professional validation, platforms receive retention. The extraction is the slight prioritization of engagement metrics over pure informational value, but this is minor and serves reader sustainability. Suppression (0.22): Low. Readers have mobile exit options — can switch platforms, use RSS feeds, curate own social media, skip items within sessions. Alternative curation models coexist. The suppression reflects only the default-on nature of arc management and the cognitive cost of opting out. Theater ratio (0.35): Low-moderate. Some performative elements exist (relief items selected more for emotional valence than editorial merit, A/B testing of placement for engagement rather than comprehension), but the core function remains genuine coordination. The theater has increased slightly over the interval as algorithmic optimization supplements editorial judgment.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits minimal perspectival gap — all four perspectives classify as Rope because the coordination function is genuine and the extraction is minimal from all structural positions. The reader experiences sustainable engagement with difficult material. The editorial team experiences professional craft validated by reader retention. The platform experiences mutual benefit coordination. The analytical observer sees a legitimate solution to the structural mismatch between human emotional capacity and information volume. The uniformity of classification is itself diagnostic: it suggests the constraint is operating as intended (pure coordination) rather than concealing extraction behind coordination theater. However, the omega variables identify three degradation pathways that could break this uniformity: if optimization targets shift from reader well-being to pure engagement metrics, if alternatives are suppressed, or if relief content becomes pure manipulation, the constraint would reclassify to Tangled Rope from some perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   All agents in this constraint are beneficiaries with mobile or arbitrage exit options, resulting in low directionality values (d ≈ 0.10-0.20) and low or negative effective extraction (χ). Readers are moderate-power beneficiaries with mobile exit — they can leave the platform, skip items, or choose unmanaged feeds, and they receive genuine coordination benefit. Editorial teams are institutional beneficiaries with arbitrage exit — they can set standards across platforms and experience the constraint as professional craft. Platform sustainability is an institutional beneficiary — the constraint serves the mutual interest of readers and platform viability. The analytical observer sees the constraint as coordination addressing a structural problem (information overload in a globally connected environment) rather than extraction. No victims are declared because no agent bears asymmetric costs — the constraint coordinates rather than extracts.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that Rope classification can be uniform across perspectives when the coordination function is genuine and extraction is minimal. The mandatrophy question 'Is this coordination or extraction?' has a clear answer: coordination, from all structural positions, given current implementation. However, the omega variables and temporal measurements reveal that this answer is contingent and fragile. The constraint is on a degradation pathway: theater_ratio increasing from 0.25 to 0.35, base_extractiveness increasing from 0.12 to 0.18, suggesting that algorithmic optimization and metric substitution are beginning to replace editorial judgment. If this trend continues, the constraint will cross the Rope threshold (χ > 0.35) and reclassify to Tangled Rope as coordination becomes contaminated with extraction. The current Rope classification is accurate but not stable — it describes the constraint as implemented, not as optimized.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manipulation_threshold,
    'At what point does emotional arc management cross from coordination (pacing for sustainability) to manipulation (engineering emotional states for engagement metrics)?',
    'Analysis of A/B testing objectives: are tests optimizing for reader well-being metrics (session satisfaction, long-term retention, informed citizenship) or pure engagement metrics (time-on-site, click-through, dopamine-cycle exploitation)? Comparison of editorial rationale vs algorithmic optimization targets.',
    'If optimization targets reader well-being: Rope classification confirmed. If optimization targets engagement metrics divorced from reader benefit: reclassify as Tangled Rope (coordination + extraction) or Snare (if suppression of alternatives increases).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(manipulation_threshold, empirical, 'Threshold between coordination and manipulation in arc management').

omega_variable(
    alternative_suppression,
    'Does the prevalence of arc-managed feeds suppress development of reader-controlled curation tools, or do alternatives coexist?',
    'Market analysis of RSS readers, customizable news aggregators, and reader-controlled filtering tools. Measurement of platform investment in user-controlled curation vs editorial curation. Assessment of whether arc management is presented as optional or default-mandatory.',
    'If alternatives are suppressed or deprecated: suppression metric should increase, potentially reclassifying to Tangled Rope. If alternatives coexist and are actively supported: Rope classification confirmed with low suppression.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_suppression, empirical, 'Whether arc management suppresses reader-controlled alternatives').

omega_variable(
    tonal_relief_authenticity,
    'Are tonal relief items (hermit crabs, pet portraits) editorially valuable content or pure emotional manipulation — filler designed to reset dopamine without informational content?',
    'Content analysis: do relief items have journalistic merit (well-reported features, cultural insight, aesthetic value) or are they algorithmically selected for emotional valence alone? Reader survey: do readers value relief items as content or experience them as patronizing filler?',
    'If relief items have editorial merit: coordination function confirmed. If relief items are pure emotional manipulation: increases theater_ratio and extractiveness, potentially reclassifying to Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(tonal_relief_authenticity, conceptual, 'Whether tonal relief items are editorially valuable or pure manipulation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(emotional_arc_management, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(emo_arc_tr_t0, emotional_arc_management, theater_ratio, 0, 0.25).
narrative_ontology:measurement(emo_arc_tr_t3, emotional_arc_management, theater_ratio, 3, 0.3).
narrative_ontology:measurement(emo_arc_tr_t6, emotional_arc_management, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(emo_arc_be_t0, emotional_arc_management, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(emo_arc_be_t3, emotional_arc_management, base_extractiveness, 3, 0.15).
narrative_ontology:measurement(emo_arc_be_t6, emotional_arc_management, base_extractiveness, 6, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(emotional_arc_management, information_standard).

% DUAL FORMULATION NOTE:
% Emotional arc management is downstream of subscription_retention_imperative (mountain) — the retention imperative creates the structural pressure that arc management addresses through coordination. The upstream constraint (readers will cancel subscriptions if emotionally exhausted) is treated as immutable; the downstream constraint (how to prevent exhaustion) is the coordination mechanism. These are distinct constraints with different ε values: the retention imperative has ε ≈ 0.05 (near-mountain, structural feature of subscription business models), while arc management has ε = 0.18 (low-extraction coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
