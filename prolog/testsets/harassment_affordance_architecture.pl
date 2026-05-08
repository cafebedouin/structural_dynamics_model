% ============================================================================
% CONSTRAINT STORY: harassment_affordance_architecture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_harassment_affordance_architecture, []).

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
 *   constraint_id: harassment_affordance_architecture
 *   human_readable: Harassment Affordance Architecture in Platform Design
 *   domain: platform_governance/content_moderation/community_norms
 *
 * SUMMARY:
 *   Platform harassment affordance architecture refers to the structural
 *   features that enable or prevent coordinated harassment campaigns:
 *   blocking mechanisms, reporting systems, direct messaging controls,
 *   amplification mechanics (reblogs, quote-tweets), and moderation tool
 *   accessibility. This constraint is downstream of the
 *   locus_of_harm_prevention mountain (the structural fact that platforms
 *   must choose whether to place harm prevention burden on targets or
 *   potential harassers). The architecture represents platforms'
 *   implementation choices within that constraint space. Well-designed
 *   affordances (one-click blocking, robust reporting, creator-controlled
 *   amplification) solve genuine coordination problems with minimal
 *   extraction. Poorly designed affordances (ineffective reporting,
 *   asymmetric blocking, harassment-amplifying mechanics) layer extraction
 *   onto the coordination function. The constraint exhibits primarily rope
 *   characteristics from most perspectives because the base extractiveness
 *   (0.18) and suppression (0.22) are low — most platforms have converged on
 *   affordances that genuinely reduce harassment overhead for creators. The
 *   theater_ratio (0.35) reflects that some report handling is performative
 *   (reports filed but not acted upon), but the majority of the architecture
 *   is functional. The marginalized creator perspective shows tangled_rope
 *   because platform lock-in (constrained exit) combined with inconsistent
 *   enforcement creates mixed coordination and extraction.
 *
 * KEY AGENTS:
 *   - Platform Creators: Primary beneficiaries (moderate/mobile) — blocking and reporting tools reduce harassment management overhead; can switch platforms if affordances degrade
 *   - Community Moderators: Organized beneficiaries (organized/mobile) — moderation tools enable distributed enforcement; have exit options via multi-platform presence
 *   - Platform Operators: Institutional beneficiaries (institutional/arbitrage) — harassment prevention features solve retention and liability problems; can redesign affordances or acquire competitors
 *   - Safety Standards Coalition: Organized agents (organized/constrained) — industry groups building cross-platform safety infrastructure; constrained by coordination costs but see sunset path
 *   - Marginalized Creators: Mixed position (moderate/constrained) — benefit from blocking/reporting but bear inconsistent enforcement costs; platform lock-in via audience capture limits exit
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees low-extraction coordination solving collective action problem
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(harassment_affordance_architecture, 0.18).
domain_priors:suppression_score(harassment_affordance_architecture, 0.22).
domain_priors:theater_ratio(harassment_affordance_architecture, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(harassment_affordance_architecture, extractiveness, 0.18).
narrative_ontology:constraint_metric(harassment_affordance_architecture, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(harassment_affordance_architecture, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(harassment_affordance_architecture, rope).
narrative_ontology:human_readable(harassment_affordance_architecture, "Harassment Affordance Architecture in Platform Design").
narrative_ontology:topic_domain(harassment_affordance_architecture, "platform_governance/content_moderation/community_norms").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(harassment_affordance_architecture, platform_creators).
narrative_ontology:constraint_beneficiary(harassment_affordance_architecture, community_moderators).
narrative_ontology:constraint_beneficiary(harassment_affordance_architecture, platform_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLATFORM CREATOR (ROPE) — Blocking, muting, and report tools solve genuine coordination problems: filtering unwanted interactions, maintaining community boundaries, preventing harassment escalation. Low extraction — these features enable creators to participate without constant harassment management overhead.
constraint_indexing:constraint_classification(harassment_affordance_architecture, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 2: COMMUNITY MODERATOR (ROPE) — Moderation tools (bulk actions, pattern detection, shared blocklists) coordinate distributed enforcement. Organized agents with exit options experience these as low-overhead coordination mechanisms that enable community self-governance.
constraint_indexing:constraint_classification(harassment_affordance_architecture, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: PLATFORM OPERATOR (ROPE) — Harassment prevention features solve the platform's coordination problem: retaining creators, reducing legal liability, maintaining advertiser relationships. Institutional beneficiary with arbitrage options experiences minimal extraction — the architecture serves platform interests.
constraint_indexing:constraint_classification(harassment_affordance_architecture, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: SAFETY STANDARDS COALITION (SCAFFOLD) — Industry groups developing cross-platform safety standards see current architectures as temporary coordination mechanisms with a sunset: federated identity, portable reputation systems, and interoperable blocking are building toward platform-independent harassment prevention. Current platform-specific tools are scaffolding for future distributed safety infrastructure.
constraint_indexing:constraint_classification(harassment_affordance_architecture, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: MARGINALIZED CREATOR (TANGLED ROPE) — Harassment prevention tools provide genuine coordination benefits (blocking, reporting) but also embed extraction: report systems require emotional labor to document abuse, platform response is inconsistent, and blocking is reactive rather than preventive. Constrained exit (platform lock-in via audience) combined with mixed benefits and costs produces tangled rope classification.
constraint_indexing:constraint_classification(harassment_affordance_architecture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational perspective, harassment affordance architecture represents low-extraction coordination: platforms that provide blocking, reporting, and moderation tools enable participation that would otherwise be untenable. The architecture solves a genuine collective action problem with minimal coercive overhead. Base extraction (0.18) reflects inherent costs of any moderation system; suppression (0.22) reflects that some coordination overhead is unavoidable.
constraint_indexing:constraint_classification(harassment_affordance_architecture, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(harassment_affordance_architecture_tests).
:- end_tests(harassment_affordance_architecture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The architecture solves genuine coordination problems (filtering unwanted interactions, preventing harassment escalation) with minimal overhead. The extraction component reflects inherent costs: emotional labor of documenting abuse, platform response inconsistency, reactive rather than preventive design. But these costs are substantially lower than the harassment burden without the affordances. Suppression (0.22): Low. Creators have meaningful alternatives: switching platforms, using third-party tools, building off-platform communities. The suppression component reflects coordination costs (learning new tools, rebuilding audiences) rather than coercive lock-in. Theater ratio (0.35): Moderate-low. Some report handling is performative (reports filed but not acted upon, especially for marginalized creators), and some moderation is optics-driven rather than effectiveness-driven. But the majority of blocking, muting, and filtering features are functional — they actually reduce harassment exposure. The theater has increased slightly over the interval as platforms have added visible safety features (report buttons, safety centers) without proportionally increasing enforcement capacity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint shows rope classification from most perspectives because the base extractiveness and suppression are genuinely low — harassment affordance architecture solves real coordination problems with minimal coercive overhead. The marginalized creator perspective shows tangled_rope because platform lock-in (constrained exit via audience capture) combined with inconsistent enforcement creates mixed coordination and extraction. The safety standards coalition sees scaffold because they are building toward platform-independent safety infrastructure that will eventually replace platform-specific affordances. The perspectival gap is narrow (rope vs tangled_rope vs scaffold) rather than wide (mountain vs snare), reflecting that this is a relatively well-functioning coordination mechanism with localized extraction rather than a deeply contested constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Platform creators, community moderators, and platform operators are all beneficiaries — they benefit from harassment prevention affordances that enable participation, reduce liability, and maintain community health. Creators with mobile exit options (can switch platforms) experience low effective extraction. Marginalized creators with constrained exit (platform lock-in via audience) experience moderate extraction from inconsistent enforcement. The safety standards coalition sees the current architecture as temporary scaffolding — federated identity and portable reputation systems will eventually enable platform-independent harassment prevention. No victims are declared because the constraint does not extract from a specific group — the costs (emotional labor of reporting, inconsistent enforcement) are distributed across users rather than concentrated on a target population. The analytical observer sees low-extraction coordination from a civilizational perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that low-extraction coordination (rope) can coexist with localized mixed coordination-extraction (tangled_rope) when exit options differ across agent groups. The platform creators with mobile exit see rope; the marginalized creators with constrained exit see tangled_rope. Both are correct from their structural positions. The analytical observer's rope classification reflects the constraint's overall low extractiveness (0.18), but this does not erase the tangled_rope experience of agents with constrained exit and inconsistent enforcement. The mandatrophy resolution: coordination mechanisms can have different effective extraction for different agents based on their exit options and enforcement consistency, and all perspectival readings are structurally valid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    report_handling_effectiveness,
    'Do platform abuse report systems actually reduce harassment, or do they primarily serve as liability theater?',
    'Longitudinal analysis of report outcomes: action rates, recidivism rates, reporter satisfaction, comparison of platforms with different report-to-action ratios',
    'If effective: rope classification confirmed — reports are functional coordination. If theater: reclassify toward piton for institutional perspective, tangled_rope for creator perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(report_handling_effectiveness, empirical, 'Whether abuse reporting produces functional harassment reduction').

omega_variable(
    blocking_asymmetry,
    'Does blocking create power asymmetries where organized harassment campaigns use blocking to isolate targets from support networks?',
    'Network analysis of blocking patterns in documented harassment campaigns; comparison of block-list sizes between harassers and targets; measurement of support network fragmentation',
    'If asymmetric: blocking becomes an extraction mechanism for organized harassers, raising extractiveness and potentially reclassifying toward tangled_rope from more perspectives. If symmetric: rope classification holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(blocking_asymmetry, empirical, 'Whether blocking tools create exploitable power asymmetries').

omega_variable(
    platform_lock_in_severity,
    'How severe is creator platform lock-in via audience capture and content investment?',
    'Measurement of cross-platform migration costs: audience retention rates after platform switches, content portability, income stability during transitions',
    'If severe: exit options for creators shift from mobile to constrained or trapped, raising effective extraction and potentially reclassifying toward tangled_rope or snare from creator perspectives. If low: rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_lock_in_severity, empirical, 'Magnitude of creator lock-in via audience and content investment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(harassment_affordance_architecture, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(harass_arch_theater_t0, harassment_affordance_architecture, theater_ratio, 0, 0.25).
narrative_ontology:measurement(harass_arch_theater_t3, harassment_affordance_architecture, theater_ratio, 3, 0.3).
narrative_ontology:measurement(harass_arch_theater_t6, harassment_affordance_architecture, theater_ratio, 6, 0.35).

% Extraction over time
narrative_ontology:measurement(harass_arch_extract_t0, harassment_affordance_architecture, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(harass_arch_extract_t3, harassment_affordance_architecture, base_extractiveness, 3, 0.15).
narrative_ontology:measurement(harass_arch_extract_t6, harassment_affordance_architecture, base_extractiveness, 6, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(harassment_affordance_architecture, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is downstream of locus_of_harm_prevention (mountain) — the structural fact that platforms must choose whether to place harm prevention burden on targets or potential harassers. The locus constraint has ε ≈ 0.05 (mountain) because the choice itself is unavoidable. The harassment_affordance_architecture constraint has ε = 0.18 (rope) because it represents platforms' implementation choices within that constraint space, and most platforms have converged on affordances that genuinely reduce harassment overhead.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
