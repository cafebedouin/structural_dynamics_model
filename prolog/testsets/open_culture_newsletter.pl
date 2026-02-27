% ============================================================================
% CONSTRAINT STORY: open_culture_newsletter
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
 *   Open Culture's newsletter represents a canonical modern value exchange:
 *   email address for curated content. The constraint exhibits structural
 *   characteristics of both coordination (genuine curation work, subscriber
 *   value delivery) and extraction (attention harvesting, data collection,
 *   asymmetric information). The classification varies dramatically by
 *   perspective. A powerless subscriber experiences the constraint as a trap:
 *   they provide email expecting content but receive escalating frequency,
 *   algorithmic recommendations optimized for engagement rather than quality,
 *   and implicit profiling for behavioral targeting. An engaged reader
 *   community with moderate organization power experiences both benefits
 *   (curation) and costs (attention capture). The platform experiences pure
 *   coordination: email list enables distribution and relationship-building.
 *   Content creators experience coordination: free amplification. The piton
 *   perspective recognizes that email newsletters compete against superior
 *   alternatives (algorithmic feeds, RSS, social discovery) but persist
 *   through institutional inertia and data asset value. The analytical
 *   observer risks naturalizing the constraint as inherent to information
 *   economics. The core tension is whether the platform's value creation
 *   (curating and aggregating) exceeds its value extraction (profiling,
 *   attention harvesting, list ownership), and whether this calculus is
 *   transparent to subscribers at the point of commitment.
 *
 * KEY AGENTS:
 *   - Open Culture Platform: Primary beneficiary (institutional/arbitrage) — owns email list, develops advertiser relationships, builds user behavioral profiles, captures attention arbitrage value
 *   - Email Subscribers: Primary victim (powerless/trapped) — provide email expecting content; face escalating frequency, engagement optimization, implicit profiling; high exit cost due to content access loss
 *   - Engaged Reader Community: Secondary actor (moderate/constrained) — benefits from curation but constrained by effort cost to find alternatives; can organize collective action (unsubscribe campaigns, feedback)
 *   - Content Creators and Cultural Institutions: Beneficiary (organized/mobile) — gain free distribution and amplification; can publish via alternative channels; multiple exit options
 *   - Email Medium (Institutional): Degraded coordination mechanism (institutional/arbitrage) — email persists through legibility and regulatory advantages (GDPR-compliant, user-familiar), not functional superiority
 *   - Analytical Observer: Risks naturalizing contingent arrangements (analytical/analytical) — may frame attention extraction as inherent to information curation rather than platform choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(open_culture_newsletter, 0.38).
domain_priors:suppression_score(open_culture_newsletter, 0.48).
domain_priors:theater_ratio(open_culture_newsletter, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(open_culture_newsletter, extractiveness, 0.38).
narrative_ontology:constraint_metric(open_culture_newsletter, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(open_culture_newsletter, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(open_culture_newsletter, tangled_rope).
narrative_ontology:human_readable(open_culture_newsletter, "The 'Free Newsletter for Email' Exchange").
narrative_ontology:topic_domain(open_culture_newsletter, "technological/economic").

domain_priors:requires_active_enforcement(open_culture_newsletter).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(open_culture_newsletter, open_culture_platform).
narrative_ontology:constraint_beneficiary(open_culture_newsletter, email_subscribers).
narrative_ontology:constraint_victim(open_culture_newsletter, user_attention_scarcity).
narrative_ontology:constraint_victim(open_culture_newsletter, email_inbox_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMAIL SUBSCRIBER IN TRAP (SNARE) — User provides email address expecting curated content but experiences escalating extraction: newsletter frequency increases, linked content monetizes attention through advertising, subscriber data becomes profile input for behavioral targeting. Exit cost is high (lost access to curated content) and coordination among subscribers is minimal. Maximum experienced extraction from the individual subscriber perspective.
constraint_indexing:constraint_classification(open_culture_newsletter, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ENGAGED READER COMMUNITY (TANGLED ROPE) — Moderate power because reader community can organize (unsubscribe collectively, provide feedback, migrate to competing newsletters). Benefits from coordination function: curated aggregation of cultural content. But also experiences extraction: attention harvesting through recommendation algorithms that prioritize engagement over quality, data collection about reading patterns and preferences. Constrained exit because alternative sources require individual curation effort.
constraint_indexing:constraint_classification(open_culture_newsletter, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OPEN CULTURE PLATFORM (ROPE) — Institutional beneficiary. Experiences the constraint as pure coordination: organizing and distributing cultural content to subscribers solves their legitimate problem (keeping readers informed). Email list becomes arbitrage capital — platform can cross-promote content, develop advertiser relationships, pivot to adjacent services. Extraction runs toward the platform; they have full agency and multiple exit options. Net beneficiary.
constraint_indexing:constraint_classification(open_culture_newsletter, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CONTENT CREATORS / CULTURAL INSTITUTIONS (ROPE) — Organized agents (museums, artists, writers, educational publishers) benefit from distribution: Open Culture amplifies their reach at zero marginal cost to them. Experiences constraint as coordination mechanism. Exit options are mobile: creators can publish elsewhere, maintain their own mailing lists, use multiple distribution channels. Benefits exceed costs from creator perspective.
constraint_indexing:constraint_classification(open_culture_newsletter, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: EMAIL AS INFORMATION MEDIUM (PITON) — Email newsletter as a coordination mechanism is substantially degraded. The constraint persists through institutional inertia: email remains culturally legible, user-familiar, and legally compliant (GDPR/CAN-SPAM). But the functional verification work has declined: newsletters compete with algorithmic feeds (social media, RSS, news aggregators) that actually reach users more effectively. Theater ratio is moderate (some genuine curation, but also performative 'newsletter experience' competing for attention). Email newsletters persist because the exit costs for the platform are moderate (switching to in-app notifications, social distribution) and because email list ownership provides regulatory and data advantages, not because email is functionally superior.
constraint_indexing:constraint_classification(open_culture_newsletter, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a universal/civilizational perspective, some attention extraction is inherent to any information distribution system: curators must filter and prioritize, and this filtering creates asymmetric information (curator knows what is most valuable to include; subscriber does not). This perspective risks naturalizing the constraint as an immutable law of information economics. However, the structural data contradicts the mountain classification — the extractiveness (0.38) and suppression (0.48) reflect contingent platform choices (monetization model, recommendation algorithms, data collection), not inevitable limits. The engine's false summit detector should flag this perspective as naturalization.
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
 *   Extractiveness (0.38): Moderate. The platform captures value through email list ownership, behavioral profiling, advertiser relationships, and attention arbitrage. But this extraction is not as severe as a predatory lending snare (0.70+) because the content delivered is genuine, many subscribers actively value the curation, and the relationship is not coercive at signup. The measurement trajectory shows gradual increase from 0.18 to 0.38 over the interval — reflecting accumulating monetization layers (advertising, sponsorship deals, algorithmic recommendation sophistication) layered onto the original coordination function. Suppression (0.48): Moderate-high. Users face barriers to exit (content access loss), no organized subscriber collective to negotiate terms, information asymmetry about data collection and profiling, and low visibility into how attention is being harvested (algorithm opacity). However, suppression is not total (0.60+): users can unsubscribe freely, switch to competing newsletters, use multiple information sources. Piton perspective is supported by theater_ratio 0.35, reflecting that newsletter experience is partially performative (branded curation theater) and partially functional (genuine content aggregation). Theater trajectory increases from 0.15 to 0.35, showing that the performative 'personalized newsletter' framing has grown as algorithmic feeds have become functionally competitive.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a stark perspectival gap between beneficiary and victim perspectives. The platform (institutional/arbitrage) sees pure coordination: they solve subscribers' information overload problem. Subscribers with no exit options (powerless/trapped) see extraction: they trade attention and identity for content they could aggregate individually. The engaged reader community (moderate/constrained) experiences the hybrid: genuine coordination benefit (curation saves time) plus extraction cost (attention is being harvested for conversion/profiling). Content creators (organized/mobile) see coordination with arbitrage options: they benefit from distribution and can publish elsewhere. The piton perspective reveals institutional inertia: email persists not because it's optimal but because the platform's data asset value and regulatory positioning make it sticky. The mountain perspective risks naturalizing this as an inherent feature of information distribution rather than a platform choice. The perspectival gap is resolvable through transparency (disclosing monetization model and data use at signup) and through technical alternatives (privacy-preserving curation, subscriber-controlled algorithms, cooperative email platforms).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is driven by the asymmetry in exit options and benefit flow. The platform (institutional power, arbitrage exit) experiences low d: they have full agency and can pivot to alternative distribution. Subscribers (powerless, trapped) experience high d: they bear the attention extraction cost and cannot reorganize their information environment without losing content access. The engaged reader community (moderate, constrained) experiences intermediate d: they can exit but at cost. Content creators (organized, mobile) experience low d: they have multiple distribution channels. The piton perspective's directionality reflects that email as a medium has been captured by platform interests — it persists not because it serves users well but because it serves platform data asset strategy. The engine's derivation of d from beneficiary/victim + exit_options should produce: platform d ≈ 0.15 (beneficiary + arbitrage), subscribers d ≈ 0.90 (victim + trapped), reader community d ≈ 0.60 (victim + constrained with some benefit), creators d ≈ 0.25 (beneficiary + mobile).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandatrophy resolution lies in distinguishing genuine coordination (email curation solves subscriber information overload) from extraction optimized as coordination (platform monetizes the email list and subscriber attention). The analytical challenge is that the same base mechanism (email newsletter) performs both functions simultaneously. A false mandatrophy would classify the constraint as pure Rope (coordination only) by focusing on the genuine curation value, or as pure Snare (extraction only) by focusing on data harvesting and list ownership. The Tangled Rope classification acknowledges both: the platform genuinely curates and distributes content (coordination function, beneficiary: subscribers), AND the platform extracts attention and behavioral data (extraction function, victim: subscriber attention and privacy). The core resolution mechanism is to examine whether the platform's monetization choices (advertising density, algorithmic recommendation for engagement rather than quality, data sale or use for profiling) constitute benign curation overhead or intentional extraction amplification. The measurement trajectory showing theater_ratio increase from 0.15 to 0.35 suggests that extraction has been deepening over the interval — the newsletter experience has become more performative (personalization theater, algorithmic recommendation opacity) and less directly about content curation. This supports Tangled Rope over pure Rope: the extraction function has grown relative to coordination.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    newsletter_monetization_boundary,
    'At what point does the curation/distribution function transition from genuine coordination to extraction-optimized engagement harvesting?',
    'Measurement of newsletter frequency, ad density, algorithmic recommendation patterns, and correlation with subscriber retention/churn rates over time. Comparison with non-monetized alternatives.',
    'If boundary is low frequency/low ads: constraint classifies as Rope from most perspectives. If boundary is high frequency/high ads: constraint classifies as Tangled Rope or Snare depending on subscriber exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(newsletter_monetization_boundary, empirical, 'Threshold where curation transitions to engagement extraction').

omega_variable(
    subscriber_exit_cost_opacity,
    'How much do subscribers understand the data-collection and attention-harvesting costs at the point of email signup? Does obscured cost change the classification?',
    'Analysis of signup flow (what terms are disclosed), comparison with subscriber post-experience perception surveys, measurement of unsubscribe rates by disclosure condition.',
    'If costs are transparent: constraint classifies as Rope (informed coordination). If costs are obscured: constraint classifies as Snare (extraction through information asymmetry). This reflects whether suppression (0.48) is institutional or informational.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subscriber_exit_cost_opacity, empirical, 'Whether subscriber exit costs are transparently disclosed at signup').

omega_variable(
    algorithmic_curation_sufficiency,
    'Is human-curated newsletter curation actually superior to algorithmic feed curation in reaching subscribers with content they value? Or is the ''personalized newsletter'' primarily theater?',
    'Comparative engagement analysis: newsletter open rates / click-through rates vs social media algorithmic feeds for same content. Subscriber satisfaction surveys. Longitudinal retention data.',
    'If newsletters outperform algorithms: theater_ratio is low and constraint is genuine coordination (Rope). If algorithms equal or exceed newsletters: theater_ratio is high and constraint is degraded piton or extraction-focused snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithmic_curation_sufficiency, empirical, 'Whether curated newsletters outperform algorithmic feeds for subscriber engagement').

omega_variable(
    email_list_ownership_value,
    'What proportion of Open Culture''s business value comes from owning the email list itself (data asset, advertiser relationships, user behavioral profiles) vs. from genuine value delivered to subscribers?',
    'Financial disclosure analysis of revenue streams (advertising, sponsorship, data licensing, subscription tiers). Comparison with platform behavior when email list is large vs small (do feature investments follow subscriber count or email list size?).',
    'If list value >> direct subscriber value: constraint is extraction-optimized (Snare or Tangled Rope). If list value ≈ subscriber value: constraint is genuine coordination (Rope). This distinction determines whether beneficiary is the platform or the subscribers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(email_list_ownership_value, empirical, 'Whether email list ownership value exceeds value delivered to subscribers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(open_culture_newsletter, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(opencult_tr_t0, open_culture_newsletter, theater_ratio, 0, 0.15).
narrative_ontology:measurement(opencult_tr_t5, open_culture_newsletter, theater_ratio, 5, 0.25).
narrative_ontology:measurement(opencult_tr_t10, open_culture_newsletter, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(opencult_be_t0, open_culture_newsletter, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(opencult_be_t5, open_culture_newsletter, base_extractiveness, 5, 0.28).
narrative_ontology:measurement(opencult_be_t10, open_culture_newsletter, base_extractiveness, 10, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(open_culture_newsletter, information_standard).
narrative_ontology:affects_constraint(open_culture_newsletter, algorithmic_feed_substitution).
narrative_ontology:affects_constraint(open_culture_newsletter, email_data_monetization).
narrative_ontology:affects_constraint(open_culture_newsletter, subscriber_attention_commons).

% DUAL FORMULATION NOTE:
% The open culture newsletter constraint is part of a constraint family covering digital information distribution. The newsletter operates upstream of algorithmic feed competition (feeds are substitutes) and downstream of email infrastructure constraints (email legal compliance, deliverability standards). The extraction layer (data monetization) is separable from the coordination layer (content curation) — each can be modeled as its own constraint, but the tangled rope classification reflects that they are operationally coupled in the platform's architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
