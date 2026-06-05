% ============================================================================
% CONSTRAINT STORY: open_culture_newsletter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_open_culture_newsletter, []).

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
 *   constraint_id: open_culture_newsletter
 *   human_readable: The 'Free Newsletter for Email' Exchange
 *   domain: technological/economic
 *
 * SUMMARY:
 *   The Open Culture newsletter exemplifies the modern 'free digital good for
 *   personal data' exchange. A user provides their email address in return
 *   for access to a curated newsletter of cultural content (articles,
 *   documentaries, lectures, art). This constraint exhibits the full tension
 *   between coordination and extraction: the platform solves a genuine
 *   curation and distribution problem (coordination function), but the
 *   mechanism for solving it — email list capture — also enables behavioral
 *   tracking, attention manipulation, and data monetization (extraction
 *   function). The constraint demonstrates how technical affordances (email
 *   as a medium for repeated engagement) embed power asymmetries that appear
 *   natural ('free means we monetize attention') but are actually contingent
 *   choices. Theater ratio (0.35) reflects moderate performative content: the
 *   newsletter genuinely delivers curated content, but the email medium adds
 *   friction and privacy burden that superior alternatives (RSS, federated
 *   social media, paid subscription) could eliminate. The extractiveness
 *   trajectory (0.22 → 0.38 over 10 years) shows increasing monetization:
 *   initial phase emphasized curation value; later phase layers on
 *   advertising, sponsorship, and data licensing. Suppression (0.42) is
 *   moderate: users can unsubscribe, but switching costs, habit formation,
 *   and the zero-friction enrollment funnel create sticky behavior. The
 *   constraint is a tangled rope by the primary classification: genuine
 *   coordination (curated content, discovery mechanism) paired with
 *   asymmetric extraction (privacy, attention, behavioral data). However, the
 *   perspectival analysis reveals why different stakeholders see it as
 *   everything from pure extraction (powerless subscribers) to pure
 *   coordination (the platform) to a temporary problem with a regulatory
 *   sunset (privacy advocates).
 *
 * KEY AGENTS:
 *   - Email Subscribers: Primary victim (powerless/trapped) — exchange email for content; bear privacy and attention costs through behavioral tracking and inertia-driven habit formation
 *   - Open Culture Platform: Primary beneficiary (institutional/arbitrage) — solves curation and distribution coordination problem; monetizes attention and data; can shift revenue model if needed
 *   - Privacy-Aware Users: Secondary actor (moderate/constrained) — value curated content but bear mixed coordination/extraction costs; can unsubscribe but face switching friction
 *   - Advertising and Data Broker Networks: Secondary beneficiary (organized/arbitrage) — monetize email list through data licensing and targeted ad placement; see themselves as peers in data ecosystem
 *   - Privacy Regulators and Advocates: Organized agents (organized/constrained) — see email-capture mechanism as temporary coordination failure with regulatory sunset via GDPR, CCPA, privacy-by-design mandates
 *   - Email Infrastructure Providers: Institutional actors (institutional/arbitrage) — enable the constraint through platform centralization (Gmail, Outlook, Yahoo); benefit from message volume and user engagement metrics
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(open_culture_newsletter, 0.38).
domain_priors:suppression_score(open_culture_newsletter, 0.42).
domain_priors:theater_ratio(open_culture_newsletter, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(open_culture_newsletter, extractiveness, 0.38).
narrative_ontology:constraint_metric(open_culture_newsletter, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(open_culture_newsletter, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(open_culture_newsletter, tangled_rope).
narrative_ontology:human_readable(open_culture_newsletter, "The 'Free Newsletter for Email' Exchange").
narrative_ontology:topic_domain(open_culture_newsletter, "technological/economic").

domain_priors:requires_active_enforcement(open_culture_newsletter).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(open_culture_newsletter, open_culture_platform).
narrative_ontology:constraint_beneficiary(open_culture_newsletter, email_subscribers).
narrative_ontology:constraint_victim(open_culture_newsletter, subscriber_attention).
narrative_ontology:constraint_victim(open_culture_newsletter, subscriber_privacy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMAIL SUBSCRIBER (SNARE) — Trapped by switching costs and behavioral inertia. Unsubscribing requires active effort; re-subscribing elsewhere requires evaluating alternatives. The exchange appears frictionless initially but becomes sticky once the subscriber's attention is habituated to the newsletter rhythm. Suppression high (0.42): vendor lock-in through habit and the zero-alternative framing ('free' implies no paid options). d≈0.88, f(d)≈1.30, σ=1.2 → χ≈0.60.
constraint_indexing:constraint_classification(open_culture_newsletter, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRIVACY-AWARE USER (TANGLED ROPE) — Benefits from curated content and community (coordination function) while bearing privacy and attention costs (extraction). Email address sold to advertisers, behavioral profiles built, newsletter frequency optimized for engagement rather than value. Suppression moderate: user can unsubscribe but loses access; can use spam filters but misses legitimate content. d≈0.72, f(d)≈1.08, σ=1.0 → χ≈0.41.
constraint_indexing:constraint_classification(open_culture_newsletter, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OPEN CULTURE PLATFORM (ROPE) — Experiences the constraint as pure coordination. The newsletter solves a collective action problem: aggregating cultural content requires curation effort and distribution infrastructure. Email list enables audience building, sponsorship revenue, and feedback loops. The extraction (privacy, attention) is subordinate to the coordination function from this perspective. d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.04. Net beneficiary via arbitrage exit (can shift revenue models).
constraint_indexing:constraint_classification(open_culture_newsletter, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVERTISING AND DATA BROKER NETWORKS (TANGLED ROPE) — Organized actors benefit from the email list through data licensing and targeted ad placement. The coordination function (shared advertising infrastructure) is real but subordinate to extraction (monetizing attention and behavioral profiles). Can arbitrage across platforms. d≈0.18, f(d)≈0.08, σ=1.2 → χ≈0.04. Low effective extraction from their perspective (they see themselves as peers in a data ecosystem), but high absolute extraction from the subscriber.
constraint_indexing:constraint_classification(open_culture_newsletter, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: PRIVACY ADVOCATES AND REGULATORS (SCAFFOLD) — See the email-for-content exchange as a temporary coordination failure with a sunset. GDPR, CCPA, and emerging privacy-by-design frameworks (federated newsletters, Mastodon cross-posting, RSS-only models) are creating exit paths that bypass the email-capture mechanism entirely. The sunset is real: explicit consent requirements and data minimization mandates are technically feasible and increasingly enforced. d≈0.45, f(d)≈0.48, σ=1.2 → χ≈0.22. Constrained but organized; sees a 5-10 year regulatory window for transition to privacy-respecting models.
constraint_indexing:constraint_classification(open_culture_newsletter, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: EMAIL AS DISTRIBUTION MEDIUM (PITON) — Email itself (SMTP, POP3, IMAP) is a degraded technology: it predates TLS encryption, has no native identity verification, enables trivial phishing, and centralizes power in a handful of providers (Gmail, Outlook, Yahoo). The newsletter format persists through institutional inertia despite superior alternatives (RSS feeds with decentralized curation, Mastodon replies, Discord/Slack communities). theater_ratio=0.35 suggests the performative component is moderate — newsletters do deliver content, but the email medium adds unnecessary friction and privacy burden. The piton perspective sees email newsletters as a zombie institution: persisting because alternatives haven't fully replaced it, not because email is superior for content distribution.
constraint_indexing:constraint_classification(open_culture_newsletter, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / ATTENTION ECONOMICS VIEW (MOUNTAIN) — From a civilizational/universal perspective, some exchange of attention for content is inherent to information economies: scarce curation and distribution infrastructure must be funded somehow, and attention-based revenue is a structural feature of post-scarcity information markets. This perspective risks naturalizing the email-capture mechanism as an inevitable law of attention economics. However, the structural data (ε=0.38, suppression=0.42, theater=0.35) and the existence of alternative models (RSS, Mastodon, paid newsletters) contradict the mountain classification — the engine will detect this as a false summit, revealing that the 'inevitable cost of free content' framing naturalizes what is actually a contingent technical and business model choice.
constraint_indexing:constraint_classification(open_culture_newsletter, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(open_culture_newsletter_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(open_culture_newsletter, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(open_culture_newsletter, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(open_culture_newsletter, TR),
    TR >= 0.70.

:- end_tests(open_culture_newsletter_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The platform captures real value from the email list through data monetization, attention metrics, and advertising placement. However, this is not as severe as pure extraction would be (ε ≥ 0.46) because subscribers do receive genuine value: curated content that would be costly to discover independently. The extraction is parasitic on the coordination function, not replacing it. The trajectory from 0.22 to 0.38 reflects increasing monetization over time — initial phases emphasized curation, later phases layered on sponsored content and data licensing. Suppression (0.42): Moderate. Substantial barriers to exit include: habit formation (weekly email becomes routine), switching costs (finding alternative curators, rebuilding feed), and the framing of 'free' which implies no paid alternatives are available. However, suppression is not high because unsubscribing is trivial (one click) and alternative curation platforms exist (Reddit, Hacker News, Mastodon). Theater ratio (0.35): Low-moderate. The newsletter does deliver actual curated content, so performative component is limited. However, the email medium itself adds unnecessary theater: email requires inbox management, parsing HTML formatting, avoiding spam filters — all friction that RSS feeds or native platforms would eliminate. The moderate theater ratio reflects that the platform's curation is genuine but the distribution mechanism is performatively complex.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between beneficiary and victim is the core diagnostic feature. The platform (beneficiary, institutional, arbitrage) sees pure coordination: 'We aggregate content, distribute it efficiently, and monetize through sponsorship. Subscribers get free value.' The email subscriber (victim, powerless, trapped) sees extraction: 'I trade my email address and attention for content I could find elsewhere. My inbox gets harvested for behavioral data.' The privacy advocate (organized, constrained, regulatory perspective) sees a temporary misalignment: 'Email-based distribution was necessary in 1990 for scaling, but today's privacy-respecting alternatives (RSS, decentralized social media, explicit consent architectures) make email capture obsolete. The extraction mechanism is regulatory-dependent — it persists because GDPR enforcement is incomplete, but that's a 5-10 year problem, not a permanent feature.' The piton perspective sees email newsletters as zombie technology: they persist through inertia, not because email is actually the best distribution medium. The analytical observer risks naturalizing the 'free content requires attention extraction' as an immutable law of information economics, but the existence of paid newsletters, RSS-only curators, and federated social media reveals this as a contingent business model choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Email Subscriber: Victim + trapped → d≈0.88, f(d)≈1.30. Maximum extraction from this perspective. The subscriber cannot easily exit without friction; no alternatives are framed as available; habit formation creates behavioral lock-in. Privacy-Aware User: Victim + constrained → d≈0.72, f(d)≈1.08. High extraction but not maximal; user can unsubscribe with effort and can use privacy tools (spam filters, email aliases) to reduce harm. Open Culture Platform: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Net beneficiary. The platform can shift revenue models (paywall, sponsorship, federation) without catastrophic cost. Advertising Networks: Organized beneficiary + arbitrage → d≈0.18, f(d)≈0.08. Low effective extraction from their perspective (they are pricing parties, not victims), but they are structural beneficiaries of the email list. Privacy Advocates: Organized + constrained with clear exit path → d≈0.45, f(d)≈0.48. Moderate, declining extraction as regulatory and technical alternatives mature. Email Infrastructure: Institutional + arbitrage → d≈0.05, f(d)≈-0.10. Neutral/beneficiary position; benefits from message volume and platform lock-in.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: This constraint avoids mandatrophy because the tangled_rope classification correctly identifies both a genuine coordination function (curated content, discovery mechanism) AND asymmetric extraction (email list, behavioral data, attention manipulation). The key evidence: (1) Beneficiaries clearly benefit from coordination: curators can reach audiences, audiences can find content. (2) Victims clearly bear extraction costs: email addresses are monetized, behavioral profiles are built, switching is friction-heavy. (3) Active enforcement exists: platform uses engagement metrics, email frequency optimization, and data licensing agreements to maintain the asymmetry. The mandatrophy would arise if this were misclassified as pure extraction (snare) despite having coordination functions, or as pure coordination (rope) despite having extraction costs. The tangled_rope classification captures both. The perspectival gap arises because different agents experience the constraint at different points on the coordination-extraction spectrum: the platform sees mostly coordination (it is solving a real problem), while the powerless subscriber sees mostly extraction (they are bearing uncompensated costs). Both observations are structurally correct — the constraint genuinely has both properties, and the agent's position determines which one is salient.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    content_value_attribution,
    'Is the primary value of the Open Culture newsletter the curated content itself, or the trusted filtering/discovery mechanism?',
    'User surveys on cancellation reasons; comparison of traffic from newsletter links vs organic search; analysis of which curated items generate engagement vs which are scrolled past',
    'If content value is primary: subscribers are paying fair value in attention (moderate extraction). If discovery mechanism is primary: the email list captures a discovery monopoly (high extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(content_value_attribution, empirical, 'Whether primary value is content or discovery mechanism').

omega_variable(
    privacy_harm_quantification,
    'What is the actual downstream harm from email address exposure? Are addresses sold, shared with advertisers, or retained for first-party analytics only?',
    'Audit of Open Culture''s privacy policy evolution; detection of email-based ad targeting via Facebook/Google pixel tracking; longitudinal measurement of spam rates for subscriber addresses',
    'If addresses are sold/shared: extraction is high and intentional (suppression ≥0.55, snare). If retained for first-party only: extraction is moderate and primarily attention-based (current tangled_rope classification holds).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(privacy_harm_quantification, empirical, 'Extent of email address monetization and downstream privacy harm').

omega_variable(
    alternative_model_viability,
    'Would subscribers accept RSS-only distribution, paid tiers, or federated models (Mastodon, Matrix) if offered, or is email lock-in structural?',
    'A/B testing of alternative distribution channels; analysis of alternative culture newsletter adoption (Substack, RSS-native platforms); survey of willingness to pay for privacy-respecting versions',
    'If alternatives are viable: scaffold sunset is real, extraction mechanism is regulatory/choice-dependent (5-10 year horizon). If alternatives fail: email lock-in is structural, extraction mechanism is deep (snare classification holds longer).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_model_viability, empirical, 'Whether viable alternatives to email-based distribution exist and subscribers would adopt them').

omega_variable(
    open_culture_intent,
    'Is Open Culture''s curation mission (public access to quality cultural content) compatible with email-based extraction, or does extraction undermine the mission?',
    'Analysis of platform decisions: paywall policies, ad density growth, newsletter frequency changes, curator compensation; interviews with founders on revenue model philosophy',
    'If extraction is mission-consistent: platform can maintain tangled_rope long-term with beneficiary framing. If extraction contradicts mission: cognitive dissonance may force transition to alternative model (scaffold outcome).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(open_culture_intent, conceptual, 'Alignment between platform''s stated mission and email extraction mechanics').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(open_culture_newsletter, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ocnl_tr_t0, open_culture_newsletter, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ocnl_tr_t5, open_culture_newsletter, theater_ratio, 5, 0.3).
narrative_ontology:measurement(ocnl_tr_t10, open_culture_newsletter, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(ocnl_be_t0, open_culture_newsletter, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(ocnl_be_t5, open_culture_newsletter, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(ocnl_be_t10, open_culture_newsletter, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(open_culture_newsletter, information_standard).
narrative_ontology:affects_constraint(open_culture_newsletter, email_surveillance).
narrative_ontology:affects_constraint(open_culture_newsletter, content_discovery_monopoly).
narrative_ontology:affects_constraint(open_culture_newsletter, attention_economy_extraction).

% DUAL FORMULATION NOTE:
% The Open Culture newsletter is a specific instance of a broader pattern: the email-for-attention exchange. It decomposes into three related constraints: (1) email_surveillance (ε≈0.50, snare) — the direct privacy extraction via email list monetization; (2) content_discovery_monopoly (ε≈0.42, tangled_rope) — the lock-in that comes from being the canonical source for a particular content domain; (3) attention_economy_extraction (ε≈0.55, snare) — the behavioral manipulation that optimizes email frequency for engagement rather than value. The open_culture_newsletter story integrates all three but treats them as a single constraint. Separation would be warranted if the platform could decouple discovery curation from email list monetization (e.g., by offering RSS-only access without data collection), revealing that the extraction is not inherent to the coordination function but a contingent design choice.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(open_culture_newsletter, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
