% ============================================================================
% CONSTRAINT STORY: fb_creator_monetization_indonesia
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fb_creator_monetization_indonesia, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: fb_creator_monetization_indonesia
 *   human_readable: Meta Performance Bonus Monetization for Indonesian Creators
 *   domain: technological/economic/labor
 *
 * SUMMARY:
 *   Meta's Performance Bonus program in Indonesia presents a structurally
 *   hybrid constraint combining genuine coordination infrastructure with
 *   systematic extraction from precarious creators. The program offers
 *   monetization pathways that did not exist before, solving a real
 *   coordination problem: connecting Indonesian creators with advertising
 *   markets and enabling income from content creation. Simultaneously, the
 *   program extracts substantial value through algorithmic control,
 *   suppression of non-monetizable content, and enforcement of platform terms
 *   unilaterally favoring Meta. The constraint exhibits high theater ratio
 *   (0.65) because much of the 'monetization' is performative: bonuses
 *   fluctuate unpredictably, payment criteria are opaque, and the algorithmic
 *   mechanisms that determine bonus eligibility are proprietary and
 *   non-auditable. Indonesian creators cannot inspect or challenge the
 *   algorithms that determine their income. The suppression metric (0.68)
 *   reflects the structural coercive capacity: Meta can unilaterally change
 *   bonus rules, reduce payment rates, demonetize creators, or suspend
 *   accounts with minimal recourse. Creator exit options are suppressed by
 *   network effects — audiences cannot easily be migrated to alternative
 *   platforms, and rebuilding on TikTok or YouTube means starting from zero
 *   visibility. The extractiveness value (0.58) is moderate-high because the
 *   extraction is real but not absolute: creators do receive cash payments,
 *   some do achieve meaningful income, and the Performance Bonus does solve a
 *   coordination problem that didn't exist before. However, the value
 *   extracted by Meta (audience data, engagement metrics, advertiser
 *   relationships, labor value of content creation) vastly exceeds the
 *   payments made. The constraint degraded over the measurement interval as
 *   the platform tightened algorithmic control, raised engagement thresholds
 *   for bonuses, and shifted more performance risk onto creators. Theater
 *   ratio increased as the program accumulated more policy theater (Meta
 *   announcements about 'creator support') while operational opacity
 *   deepened.
 *
 * KEY AGENTS:
 *   - Meta Platform Corporate: Institutional beneficiary (institutional/arbitrage) — controls monetization rules, receives engagement data and content value; can unilaterally modify terms and payment rates
 *   - High-Engagement Creators (Top 10%): Secondary beneficiary (powerful/mobile) — can achieve meaningful income; have some ability to experiment with content and platform switching
 *   - Precarious Creator Workforce (Bottom 50%): Primary victim (powerless/trapped) — entirely dependent on Meta's algorithms; cannot exit without losing potential audience; income highly volatile and non-transparent
 *   - Mid-Tier Creator Network: Mixed agent (moderate/constrained) — some income access but algorithmic dependency; can exit to alternative platforms but at significant cost; collectively organized but individually vulnerable
 *   - Indonesian Government Technology/Labor Ministry: Ostensible regulator (institutional/arbitrage) — appears to oversee creator welfare; actual regulatory capacity and enforcement minimal; theater primarily performative
 *   - Creator Advocacy Groups & Labor Unions: Organized opposition (organized/mobile) — emerging advocacy for creator rights, platform accountability, labor protections; building alternatives and regulatory pressure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fb_creator_monetization_indonesia, 0.58).
domain_priors:suppression_score(fb_creator_monetization_indonesia, 0.68).
domain_priors:theater_ratio(fb_creator_monetization_indonesia, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fb_creator_monetization_indonesia, extractiveness, 0.58).
narrative_ontology:constraint_metric(fb_creator_monetization_indonesia, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(fb_creator_monetization_indonesia, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fb_creator_monetization_indonesia, tangled_rope).
narrative_ontology:human_readable(fb_creator_monetization_indonesia, "Meta Performance Bonus Monetization for Indonesian Creators").
narrative_ontology:topic_domain(fb_creator_monetization_indonesia, "technological/economic/labor").

domain_priors:requires_active_enforcement(fb_creator_monetization_indonesia).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fb_creator_monetization_indonesia, meta_platform).
narrative_ontology:constraint_beneficiary(fb_creator_monetization_indonesia, high_engagement_creators).
narrative_ontology:constraint_victim(fb_creator_monetization_indonesia, precarious_creator_workforce).
narrative_ontology:constraint_victim(fb_creator_monetization_indonesia, content_quality_degradation).
narrative_ontology:constraint_victim(fb_creator_monetization_indonesia, algorithmic_dependency).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRECARIOUS CREATOR (SNARE) — Locked into platform dependency with minimal alternatives. Income contingent on Meta's opaque algorithms; no negotiating power over terms; suppression includes enforcement of platform content policies, algorithmic demotion, account suspension. Biographical timeframe: creator's livelihood depends on continued access. Maximum extraction: must comply with all platform rules, content ownership ambiguity, zero recourse for algorithmic punishment.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MID-TIER CREATOR NETWORK (TANGLED ROPE) — Moderate power through collective presence and niche audiences. Exit is constrained: alternative platforms (TikTok, YouTube) require rebuilding audience from zero, but some exit options exist. Mixed experience: Performance Bonus provides real income pathway while simultaneously extracting unpredictable labor (algorithm-chasing, content optimization, suppression of non-monetizable content). Coordination function: network effects enable audience building, but extraction via algorithmic ranking dominates the experience.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: META CORPORATE (ROPE) — Primary beneficiary. Experiences constraint as coordination mechanism: Performance Bonus aggregates creator content production, distributes engagement rewards, and retains platform control over monetization rules. Arbitrage available: can adjust payment rates, alter bonus structures, redirect creator effort. Net beneficiary during entire program duration. Extraction flows toward Meta — data, attention, labor value of created content far exceeds bonus payments.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CREATOR ADVOCACY & REGULATION (SCAFFOLD) — Organized agents (creator unions, labor advocates, Indonesian labor ministry, digital rights NGOs) perceive Performance Bonus as a temporary coordination failure with regulatory sunset. Exit visible: creator-controlled platforms, cooperative models, regulatory minimum income protections could replace Meta's unilateral control. High suppression currently (platform terms of service, algorithmic enforcement), but advocacy is building alternative frameworks. Sunset clause implicit in regulatory trajectory toward platform labor standards.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: INDONESIAN GOVERNMENT (PITON) — Ostensibly regulates platform labor practices and creator welfare, but enforcement is largely theatrical. Performance Bonus appears as a win for creator monetization in policy announcements while Meta retains unilateral control. Government theater: public support for 'digital economy' without meaningful oversight. Theater ratio high because monitoring is minimal, enforcement nonexistent, and policy statements substitute for structural intervention. The constraint persists due to institutional inertia — regulatory capacity is weak, and Facebook/Meta's market dominance discourages aggressive intervention.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT (MOUNTAIN) — Civilizational analysis risks treating the Performance Bonus constraint as an immutable property of digital labor: 'platform-mediated work always extracts more value than it distributes,' 'algorithmic ranking is an unavoidable feature of attention economies,' 'creator labor always depends on platform goodwill.' This perspective naturalizes contingent institutional arrangements. However, the structural data reveals this as a false summit: the extraction mechanism, suppression, and theater are all socially constructed and contingent on Meta's unilateral policy choices, not laws of nature.
constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fb_creator_monetization_indonesia_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(fb_creator_monetization_indonesia, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(fb_creator_monetization_indonesia, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(fb_creator_monetization_indonesia, TR),
    TR >= 0.70.

:- end_tests(fb_creator_monetization_indonesia_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.58): The Performance Bonus delivers real monetization that didn't exist before, creating genuine coordination value. However, this is coupled with systematic extraction through algorithmic control, opaque reward mechanisms, and unilateral platform enforcement. The metric reflects that extraction dominates the experience for most creators while acknowledging real coordination benefit. Suppression (0.68): High structural suppression. Creators cannot audit algorithms, cannot negotiate terms, cannot access alternative monetization without audience loss, and face algorithmic penalties (demonetization, reduced visibility) for non-compliance with platform content policies. The suppression is enforced through technical infrastructure (algorithms) and policy (terms of service), not visible threats, making it effective and normalized. Theater ratio (0.65): Significant performative content. Platform announcements frame Performance Bonus as 'supporting creators' while algorithmic mechanisms remain proprietary and non-auditable. Monthly bonus fluctuations appear as rewards but are mechanically determined by opaque ranking systems. Government policy statements about creator welfare substitute for enforcement. Creator success stories are promoted while algorithmic punishment is invisible.
 *
 * PERSPECTIVAL GAP:
 *   The constraint manifests as six structurally distinct experiences depending on observer position. The precarious creator sees a Snare: trapped income dependency with no meaningful alternatives, suppressed by algorithmic enforcement, experiencing maximum extraction. The mid-tier creator network sees Tangled Rope: real income pathways (coordination function) coupled with algorithmic dependency (extraction). Meta sees Rope: a coordination mechanism that aggregates creator labor and monetizes it through advertising, with extraction flowing toward the platform as net beneficiary. The Indonesian government sees Piton: regulatory theater about creator support while actual enforcement is minimal and contingent on Meta's voluntary compliance. Creator advocacy movements see Scaffold: a temporary institutional arrangement being replaced by regulatory frameworks and platform alternatives, with visible sunset as labor protections strengthen. The civilizational analytical observer risks seeing Mountain: treating algorithmic ranking and platform dependence as inevitable laws of digital attention, naturalizing what are actually contingent policy choices by Meta.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) is derived from their structural position within the extraction flow and exit capacity. Meta, as institutional beneficiary with arbitrage options (can adjust rules, redirect creator effort, leave market if unprofitable), experiences d ≈ 0.05 (full beneficiary position) → f(d) ≈ -0.12 (negative chi, experiencing this as coordination, not extraction). High-engagement creators with mobile options (can experiment across platforms or build direct monetization) experience d ≈ 0.40 (partial targets with some exit capacity) → f(d) ≈ 0.40 (moderate experienced extraction, mitigated by success and optionality). Mid-tier creators with constrained exit (audience not portable, alternative platforms require rebuilding) experience d ≈ 0.65 (mostly target) → f(d) ≈ 1.00 (moderate-to-high experienced extraction). Precarious creators with trapped exit (entirely dependent, cannot rebuild elsewhere) experience d ≈ 0.90 (near-full target position) → f(d) ≈ 1.35 (maximum experienced extraction, approaching the bound). The powerless creator's experienced chi is amplified because their structural position as trapped victim means all platform mechanisms are experienced as coercive. Indonesian government, despite nominal regulatory role, has arbitrage exit (can decline to intervene, faces no personal penalty) and derives d ≈ 0.10 (beneficiary position, enjoying stability without responsibility) → f(d) ≈ -0.05. This explains the Piton classification: institutional actors experience this as coordination even though structural data (victims, suppression) indicates extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy by revealing that the disagreement between 'this is pure coordination' (Rope) and 'this is pure extraction' (Snare) is entirely perspectival. Meta experiences genuine coordination value: the Performance Bonus successfully aggregates creator supply, monetizes engagement, and creates advertiser relationships. These are real coordination benefits. Simultaneously, precarious creators experience pure extraction: algorithmic enforcement, suppressed alternatives, and one-sided terms with no negotiation. Both experiences are accurate. The 'mandatrophy' was the false binary asking 'is it coordination or extraction?' The answer is both, depending on structural position. For Meta: coordination. For powerless creators: extraction. For mid-tier creators: tangled hybrid. For advocacy movements: temporary (scaffold). For government: theatrical (piton). For civilizational analysis: false natural law (false summit). The Performance Bonus is a Tangled Rope at the baseline analytical level because it has BOTH genuine coordination function (solving the problem of connecting creators to monetization) AND systematic asymmetric extraction (benefiting Meta disproportionately, suppressing creator alternatives, enforcing one-sided terms). The constraint cannot be classified as pure Rope (lacks the asymmetric extraction gate: victims and enforcement both present) nor pure Snare (does deliver real monetization value, providing genuine benefit to some creators). The Tangled Rope classification holds: the constraint solves a collective action problem while simultaneously extracting through enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithmic_transparency_threshold,
    'At what level of algorithmic transparency would the Performance Bonus constraint shift from extraction to coordination?',
    'A/B testing of creator outcomes under varying transparency regimes; comparison with platforms offering algorithmic auditability; measurement of creator income stability and prediction accuracy',
    'If transparency threshold is achievable: constraint could shift from Snare (at powerless level) toward Rope (at organized level). If threshold is impossibly high: algorithmic opacity is fundamental to extraction, not an implementation detail.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_transparency_threshold, empirical, 'Whether algorithmic transparency would enable creator agency within Performance Bonus').

omega_variable(
    alternative_platform_viability,
    'Do creator-controlled or cooperative platforms offer structurally viable alternatives to Meta''s Performance Bonus model in the Indonesian market?',
    'Longitudinal analysis of alternative platform growth, creator income distributions, retention rates, and operational sustainability; comparison of creator satisfaction and income stability across platforms',
    'If viable alternatives exist: constraint is not structurally necessary, and suppression is enforced choice. If alternatives fail: creator dependence on Meta is systemic, and Snare classification is structural rather than contingent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_platform_viability, empirical, 'Whether cooperative or creator-controlled platforms are viable in Indonesian market').

omega_variable(
    extraction_value_quantification,
    'What is the actual ratio of bonus payments to engagement value extracted by Meta from creator content?',
    'Detailed accounting: creator compensation vs platform revenue from creator-generated engagement, advertising spend, data value; comparison with creator wage expectations and gig economy labor rates',
    'If ratio is < 10%: extraction is severe and Snare classification is accurate. If ratio is > 40%: compensation is more proportional and Rope classification gains support. If ratio is unknowable: opacity itself is the extraction mechanism.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraction_value_quantification, empirical, 'Actual ratio of creator compensation to platform value extraction').

omega_variable(
    regulatory_capacity_constraints,
    'Is Indonesian government capacity to regulate platform labor genuinely limited, or is regulatory weakness a contingent choice by political economy actors?',
    'Comparative analysis of Indonesian regulatory capacity in other sectors; interviews with government labor officials on platform regulation priorities; examination of resource allocation to digital labor oversight',
    'If genuinely capacity-limited: government Piton classification is structural. If contingent choice: government is complicit in extraction, and should be reclassified as institutional beneficiary rather than regulator.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capacity_constraints, empirical, 'Whether Indonesian regulatory weakness is structural or political choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fb_creator_monetization_indonesia, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fbcmi_tr_t0, fb_creator_monetization_indonesia, theater_ratio, 0, 0.45).
narrative_ontology:measurement(fbcmi_tr_t3, fb_creator_monetization_indonesia, theater_ratio, 3, 0.58).
narrative_ontology:measurement(fbcmi_tr_t6, fb_creator_monetization_indonesia, theater_ratio, 6, 0.65).

% Extraction over time
narrative_ontology:measurement(fbcmi_be_t0, fb_creator_monetization_indonesia, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fbcmi_be_t3, fb_creator_monetization_indonesia, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(fbcmi_be_t6, fb_creator_monetization_indonesia, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fb_creator_monetization_indonesia, resource_allocation).
narrative_ontology:affects_constraint(fb_creator_monetization_indonesia, platform_content_moderation_opacity).
narrative_ontology:affects_constraint(fb_creator_monetization_indonesia, creator_labor_classification_ambiguity).
narrative_ontology:affects_constraint(fb_creator_monetization_indonesia, algorithmic_ranking_enforcement).

% DUAL FORMULATION NOTE:
% The Performance Bonus program is downstream of Meta's broader platform architecture (content moderation, algorithmic ranking) and upstream of creator labor classification debates (are creators employees, contractors, or independent business operators?). The constraint family includes distinct stories for algorithmic ranking opacity (higher ε, more Snare-like from creator perspective) and labor classification (contested between platforms/governments/creators, higher institutional complexity). The monetization program itself is the hybrid coordinating device connecting these upstream technical constraints to downstream legal/labor frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(fb_creator_monetization_indonesia, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
