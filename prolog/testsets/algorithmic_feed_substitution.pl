% ============================================================================
% CONSTRAINT STORY: algorithmic_feed_substitution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_algorithmic_feed_substitution, []).

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
 *   constraint_id: algorithmic_feed_substitution
 *   human_readable: Algorithmic Feed Substitution in Attention Markets
 *   domain: digital_economics/cognitive_capture
 *
 * SUMMARY:
 *   Algorithmic feed substitution occurs when platform operators
 *   algorithmically modify the content stream shown to users away from
 *   chronological or user-preference-aligned ordering toward
 *   engagement-maximizing content. The constraint exhibits tangled rope
 *   structure: platforms solve a genuine coordination problem (how to
 *   recommend relevant content at scale), but layer extraction on top
 *   (behavioral targeting, attention commodification, suppression of
 *   non-engagement-driving content). End users experience high suppression
 *   (opacity of filtering mechanisms, identity fusion with platform social
 *   graphs), creators face algorithmic unpredictability, and the epistemic
 *   commons bears degraded information quality. The constraint's theater
 *   ratio has grown from 0.40 (early social platforms with mostly transparent
 *   chronological feeds) to 0.65 (current state with opaque algorithmic
 *   curation and performative transparency reports). The extractiveness
 *   trajectory shows steady accumulation: as platforms matured and engagement
 *   became the primary optimization target, the gap between stated (user
 *   choice, relevant recommendations) and actual (attention maximization,
 *   behavioral prediction) functions widened.
 *
 * KEY AGENTS:
 *   - End Users: Primary victims (powerless/identity_locked) — structurally mobile but identity-fused with platform graphs; experience suppressed visibility of content they would prefer
 *   - Content Creators: Secondary victims (moderate/constrained) — face algorithmic unpredictability and barrier to visibility; also derive coordination benefit from platform distribution
 *   - Platform Operators: Primary beneficiaries (institutional/arbitrage) — design and control the algorithmic substitution; experience it as product function and control system
 *   - Epistemic Commons: Collective victim (powerful/mobile but unorganized) — bears degraded information quality; cannot organize or exit
 *   - Advertisers & Attention Brokers: Co-beneficiaries (organized/arbitrage) — algorithms are tuned to enable and measure attention commerce
 *   - Regulatory Authorities: Institutional actors (institutional/constrained) — attempt oversight via transparency rules, but enforcement is degraded (piton perspective)
 *   - Analytical Observer: Sees the hybrid structure — genuine coordination function + systematic extraction layered on top
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(algorithmic_feed_substitution, 0.58).
domain_priors:suppression_score(algorithmic_feed_substitution, 0.68).
domain_priors:theater_ratio(algorithmic_feed_substitution, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(algorithmic_feed_substitution, extractiveness, 0.58).
narrative_ontology:constraint_metric(algorithmic_feed_substitution, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(algorithmic_feed_substitution, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(algorithmic_feed_substitution, tangled_rope).
narrative_ontology:human_readable(algorithmic_feed_substitution, "Algorithmic Feed Substitution in Attention Markets").
narrative_ontology:topic_domain(algorithmic_feed_substitution, "digital_economics/cognitive_capture").

domain_priors:requires_active_enforcement(algorithmic_feed_substitution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(algorithmic_feed_substitution, platform_operators).
narrative_ontology:constraint_beneficiary(algorithmic_feed_substitution, attention_brokers).
narrative_ontology:constraint_beneficiary(algorithmic_feed_substitution, engagement_maximizers).
narrative_ontology:constraint_victim(algorithmic_feed_substitution, end_users).
narrative_ontology:constraint_victim(algorithmic_feed_substitution, content_creators).
narrative_ontology:constraint_victim(algorithmic_feed_substitution, epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% End user experiencing algorithmic feed substitution perceives constrained exit — technically mobile (can delete app) but identity-fused with platform social graph. User's self-concept includes platform presence; offline identity coordination feels incomplete. Feeds are curated away from stated preferences toward engagement maximization. Maximum extraction from the powerless perspective — they cannot see the filtering mechanism or change it.
constraint_indexing:constraint_classification(algorithmic_feed_substitution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% Small creator faces high barriers to exit: algorithm controls visibility, feed substitution suppresses reach unless content is engineered for engagement metrics. Also derives genuine coordination benefit — platform provides distribution to audience creator could not reach independently. Mixed extraction: creator bears cost of algorithmic unpredictability while benefiting from infrastructure.
constraint_indexing:constraint_classification(algorithmic_feed_substitution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Platform experiences the constraint as coordination: algorithmic feed curation solves the content-recommendation problem and enables user retention at scale. Experiences the constraint as low extraction — it is their product, their function, their control system. Net beneficiary with exit options.
constraint_indexing:constraint_classification(algorithmic_feed_substitution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Collective information environment experiences the constraint as pure extraction. Feed substitution prioritizes engagement over accuracy, incentivizing sensationalism and suppressing nuance. The epistemic commons cannot organize or exit. Information quality degrades across all platforms via isomorphic algorithmic structures. This perspective shows high effective extraction despite the powerful nominal power level — the commons lacks agency.
constraint_indexing:constraint_classification(algorithmic_feed_substitution, snare,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Regulators (FTC, EU DMA, national authorities) have attempted feed algorithm transparency rules, but enforcement is degraded. Theater ratio is high: platforms produce algorithmic explanation documents and transparency reports that are performative — genuine feed manipulation logic remains opaque. The regulatory constraint persists through theater, not through actual transparency function.
constraint_indexing:constraint_classification(algorithmic_feed_substitution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Advertisers, data brokers, and engagement-optimization services see feed substitution as coordination mechanism: algorithms solve the matching problem (which content reaches which user), enabling attention commerce. They also extract: algorithms are tuned to maximize advertiser ROI over user experience. Organized agents with arbitrage options — they can switch platforms or deploy alternative engagement strategies.
constraint_indexing:constraint_classification(algorithmic_feed_substitution, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From analytical/civilizational perspective, feed substitution is a genuine hybrid: it solves real coordination problems (how to recommend content at scale) while enabling systematic extraction (behavioral targeting, engagement maximization, attention commodification). The constraint is not naturally occurring but actively maintained through competitive feed-algorithm lock-in. Mandatrophy resolved: the coordination function is real (recommendation is hard), but the extraction is neither incidental nor minimal.
constraint_indexing:constraint_classification(algorithmic_feed_substitution, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(algorithmic_feed_substitution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(algorithmic_feed_substitution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(algorithmic_feed_substitution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(algorithmic_feed_substitution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(algorithmic_feed_substitution, TR),
    TR >= 0.70.

:- end_tests(algorithmic_feed_substitution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Substantial but not maximal. The constraint does solve a real problem (content recommendation at scale), which reduces ε below pure-extraction levels. However, the solution is optimized for platform benefit (engagement, attention commodification) not user benefit, which keeps ε well above rope-level coordination. The 0.35→0.58 trajectory reflects layers of engagement-metric optimization accumulating over time. Suppression (0.68): High. Multiple suppression mechanisms: algorithmic opacity (users cannot see how feeds are filtered), identity lock (users cannot imagine departing their social graph), tacit knowledge barriers (algorithm logic is proprietary), and career/social cost of exit (followers, reputation, network effects). Suppression is actively designed not incidental. Theater ratio (0.65): Moderate-high. Platforms produce algorithmic transparency documents and feed-explanation features that are performative — they create appearance of user control while actual feed optimization remains opaque. The theater provides regulatory cover (demonstrating 'transparency efforts') without functional user agency. Claimed type (tangled rope): Required beneficiaries (platform, advertisers), required victims (users, creators, epistemic commons), required active enforcement (algorithmic substitution must be maintained via continuous reranking). The constraint satisfies all three gates for tangled rope: genuine coordination function + asymmetric extraction + active enforcement.
 *
 * PERSPECTIVAL GAP:
 *   The identity_locked exit option for end users is the key diagnostic signal. A structurally mobile agent (delete the app, switch platforms) experiences the constraint as if it were a trap (cannot imagine leaving). This reveals the binding mechanism is cognitive rather than material. The platform's rope perspective depends on users remaining inside — if users actually exercised exit (deletion without replacement), the constraint would degrade. But users don't leave not because they can't, but because their identity is fused with their platform presence. The creator's tangled rope perspective is more informative than it first appears: creators both benefit (distribution) and bear costs (algorithmic unpredictability). This duality makes creators structurally unstable — they could become victims (if algorithmic reach collapses) or remain co-beneficiaries (if reach is reliable). The regulatory piton perspective shows that oversight mechanisms persist through theater, not through function. This is a secondary diagnostic signal: when institutional oversight appears in piton form, it indicates that the primary constraint has captured the regulatory apparatus or made it performative.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) flows from the extraction flow: platforms and advertisers are beneficiaries (d ≈ 0.05-0.20), end users and creators are victims (d ≈ 0.75-0.95), epistemic commons is a powerless victim (d ≈ 0.95). The identity_locked exit option for end users reflects that they have technical mobility (can delete app) but cannot exercise it because their identity is constituted through their platform presence and social graph. This differs from trapped (material barriers persist after exit — they don't) and constrained (high cost to exit — true, but not the primary binding mechanism). The identity lock is the cognitive filter: users internalize platform framing ('this is how social media works') and cannot imagine functioning outside it. The piton perspective shows that regulatory authority has nominal power but constrained exit — they are trapped in a loop of transparency rules that create performative compliance rather than functional change. The analytical observer's d value derives from the epistemological position: able to see the full structure but without direct leverage to change it.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint classifies as tangled rope from the analytical perspective, which prevents mislabeling as either pure coordination (rope) or pure extraction (snare). The mandatrophy would occur if we tried to classify feed substitution as rope-only ('it's just recommendation algorithms, users benefit from better content matching'). The base structural data refutes this: beneficiaries are explicitly the platform and advertisers, not users; victims exist (creators, epistemic commons); active enforcement is required (algorithms must be continuously retuned toward engagement). Similarly, if we tried to classify as snare-only ('purely extractive, no coordination'), we would misrepresent that feed algorithms genuinely solve the content-discovery problem. The tangled rope classification captures both: the constraint is a hybrid where a coordination function (recommendation) and extraction mechanism (engagement maximization) are structurally fused. Breaking the extraction breaks the coordination. Decoupling them (e.g., user-aligned feed algorithms with chronological alternatives) is technically possible but not incentive-compatible for platforms. The mandatrophy resolves by acknowledging that the constraint's function is mixed, its costs are asymmetric, and its classification from different perspectives legitimately differs — but only along the indexical axes (power, time, exit, scope), not in the constraint itself.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_vs_constrained_boundary,
    'Is the user''s inability to exit driven by structural platform switching costs (constrained) or by identity fusion with platform social graph (identity_locked)?',
    'Longitudinal study of users post-deactivation: do suppression effects (social isolation, FOMO, reputational loss) persist after the structural barrier is removed? If suppression persists via internalized identity, classify as identity_locked; if it resolves, constrained is more accurate.',
    'If identity_locked: constraint is harder to break because user carries the suppression mechanism with them. If constrained: policy interventions (interoperability, data portability) directly reduce exit costs and extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_constrained_boundary, empirical, 'Boundary between structural exit costs and cognitive identity lock').

omega_variable(
    engagement_maximization_necessity,
    'Is feed substitution toward engagement metrics a necessary technical requirement for platform viability, or a profit-maximization choice layered on top of functional recommendation?',
    'Counterfactual analysis: platforms experimenting with chronological or user-preference-aligned feeds (Twitter''s toggle, some Mastodon instances). Measure: user retention, creator sustainability, epistemic quality. If functional alternatives exist with comparable viability metrics, engagement-maximization is choice not necessity.',
    'If necessary: extractiveness reflects coordination cost, suppression is inherent to scaled recommendation. If choice: extractiveness is rent-seeking layered onto coordination, suppression is designed not incidental.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(engagement_maximization_necessity, empirical, 'Whether engagement maximization is technical necessity or profit choice').

omega_variable(
    algorithmic_substitution_observability,
    'Can users reliably detect when feed substitution is occurring? Is feed curation hidden or disclosed?',
    'User perception study: ask users to predict what feed algorithm optimizes for. Compare predictions to actual algorithm specifications (where disclosed). Measure: prediction accuracy, awareness of substitution mechanism.',
    'If observable: users can make informed exit decisions (reduces suppression). If hidden: suppression is heightened by epistemic opacity. Current evidence suggests high hiddenness → high suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(algorithmic_substitution_observability, empirical, 'Detectability of feed substitution mechanism to end users').

omega_variable(
    creator_co_extraction,
    'Do content creators experience the constraint as tangled rope (mixed coordination and extraction) or primarily as snare (pure extraction)?',
    'Creator survey: measure perceived benefit (distribution reach, audience growth) vs perceived cost (algorithmic unpredictability, engagement metric engineering, exclusion from viral mechanisms). If perceived benefit is substantial and stable, tangled rope is accurate; if benefit is volatile or marginal, snare better describes the constraint from creator perspective.',
    'If snare: creators should be classified as victims requiring intervention. If tangled rope: creator class is co-beneficiary, policy implications shift toward sustainability rather than elimination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(creator_co_extraction, empirical, 'Whether creators experience mixed extraction or pure extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(algorithmic_feed_substitution, 0, 9).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(afs_tr_t0, algorithmic_feed_substitution, theater_ratio, 0, 0.4).
narrative_ontology:measurement(afs_tr_t3, algorithmic_feed_substitution, theater_ratio, 3, 0.52).
narrative_ontology:measurement(afs_tr_t6, algorithmic_feed_substitution, theater_ratio, 6, 0.62).
narrative_ontology:measurement(afs_tr_t9, algorithmic_feed_substitution, theater_ratio, 9, 0.65).

% Extraction over time
narrative_ontology:measurement(afs_be_t0, algorithmic_feed_substitution, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(afs_be_t3, algorithmic_feed_substitution, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(afs_be_t6, algorithmic_feed_substitution, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(afs_be_t9, algorithmic_feed_substitution, base_extractiveness, 9, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(algorithmic_feed_substitution, resource_allocation).
narrative_ontology:affects_constraint(algorithmic_feed_substitution, behavioral_targeting_surveillance).
narrative_ontology:affects_constraint(algorithmic_feed_substitution, attention_commerce_lock_in).
narrative_ontology:affects_constraint(algorithmic_feed_substitution, algorithmic_extremism_feedback).

% DUAL FORMULATION NOTE:
% Algorithmic feed substitution is upstream of behavioral targeting (feeds are the mechanism that enables targeting) and attention commerce (feeds route attention to paying advertisers). These three constraints form a family where feed substitution is the primary extraction mechanism, targeting is the secondary mechanism (data extraction enabling future extraction), and attention commerce is the revenue realization. Each has distinct ε and perspectives but they are structurally coupled via data and incentive flows.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(algorithmic_feed_substitution, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
