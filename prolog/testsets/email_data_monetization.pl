% ============================================================================
% CONSTRAINT STORY: email_data_monetization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_email_data_monetization, []).

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
 *   constraint_id: email_data_monetization
 *   human_readable: Email Data Monetization
 *   domain: digital_economy/data_extraction/consumer_privacy
 *
 * SUMMARY:
 *   Email data monetization represents a foundational constraint in the
 *   digital economy: the structural conflict between the coordination need
 *   for global email infrastructure (requiring revenue to operate) and the
 *   asymmetric extraction of user behavior data for commercial purposes
 *   (benefiting providers and advertisers while imposing privacy costs on
 *   users). The constraint exhibits classic tangled rope characteristics —
 *   genuine coordination function (enabling free/low-cost global email
 *   service) layered with substantial asymmetric extraction (extensive
 *   behavioral data collection with minimal user comprehension or consent).
 *   The temporal progression shows increasing extractiveness (0.35 → 0.58)
 *   and rising theater ratio (0.40 → 0.65), indicating both intensifying
 *   extraction and increasing opacity about how data is used. Regulatory
 *   interventions (GDPR starting 2018, CCPA starting 2020) represent
 *   organized efforts to constrain the extraction mechanism, creating
 *   scaffold-type pressure to transition from extractive to coordinative
 *   revenue models. However, the regulatory effectiveness remains contested —
 *   consent mechanisms often constitute theater rather than genuine informed
 *   choice. The constraint's structural heterogeneity (different types from
 *   different perspectives) reveals how the same data monetization mechanism
 *   appears as natural economic law to analysts, pure extraction to trapped
 *   users, and legitimate coordination need to service providers.
 *
 * KEY AGENTS:
 *   - Email Users: Primary victims (powerless/trapped) — bear full extraction cost through data exposure without meaningful consent or compensation; network effects and communication necessity prevent exit
 *   - Email Service Providers: Primary beneficiaries (institutional/arbitrage) — capture substantial revenue from data monetization; experience constraint as coordination solution enabling free service provision
 *   - Advertising Platforms: Secondary beneficiaries (organized/arbitrage) — use email metadata for targeted advertising; benefit from provider-to-advertiser data flows with minimal direct accountability to users
 *   - Privacy-Conscious Users: Secondary victims (moderate/constrained) — can exit to alternatives but face high switching costs and incompatibility with mainstream users; constitute minority coalition unable to break network effects
 *   - Regulatory Coalitions: Organized agents (organized/constrained) — GDPR, CCPA, emerging frameworks attempting to constrain extraction through transparency and consent requirements; face ongoing provider workarounds
 *   - 'Free Service' Cultural Norm: Institutional maintenance mechanism (institutional/arbitrage) — decades of free email have made users internalize expectation of zero-cost service, reducing perceived legitimacy of paid alternatives
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing data monetization as economic law rather than contingent institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(email_data_monetization, 0.58).
domain_priors:suppression_score(email_data_monetization, 0.68).
domain_priors:theater_ratio(email_data_monetization, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(email_data_monetization, extractiveness, 0.58).
narrative_ontology:constraint_metric(email_data_monetization, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(email_data_monetization, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(email_data_monetization, tangled_rope).
narrative_ontology:human_readable(email_data_monetization, "Email Data Monetization").
narrative_ontology:topic_domain(email_data_monetization, "digital_economy/data_extraction/consumer_privacy").

domain_priors:requires_active_enforcement(email_data_monetization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(email_data_monetization, email_service_providers).
narrative_ontology:constraint_beneficiary(email_data_monetization, advertising_platforms).
narrative_ontology:constraint_beneficiary(email_data_monetization, data_brokers).
narrative_ontology:constraint_victim(email_data_monetization, email_users).
narrative_ontology:constraint_victim(email_data_monetization, consumer_privacy).
narrative_ontology:constraint_victim(email_data_monetization, competitive_email_market).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMAIL USER (SNARE) — Trapped by network effects and communication necessity. Email is structurally essential for employment, financial transactions, identity verification. Exits are cosmetically available (use alternative email) but functionally blocked by switching costs (notification of contacts, data loss, account recovery complexity) and the monopoly control of critical infrastructure. User bears full extraction cost: exposure of communication patterns, recipient networks, behavioral inferences from message metadata, without meaningful consent mechanism or compensation. Suppression manifests as opacity (terms of service are theater) and lack of alternatives (all major providers monetize data similarly).
constraint_indexing:constraint_classification(email_data_monetization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PRIVACY-CONSCIOUS USER COHORT (TANGLED ROPE) — Can exit to privacy-focused providers (Proton Mail, Tutanota) but face high costs: lost social network integration, reduced compatibility with mainstream contacts, employer systems, and organizational infrastructure. Coordination function exists: encrypted email enables legitimate secure communication among consensual users. Extraction runs parallel: privacy-focused services have lower user bases, reduced feature parity, and diminished access to social capital. The constraint is hybrid — genuine coordination need (secure communication) layered with asymmetric extraction (privacy segregates users into incompatible ecosystems).
constraint_indexing:constraint_classification(email_data_monetization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EMAIL SERVICE PROVIDER (ROPE) — Experiences the constraint as pure coordination: operating free/freemium email infrastructure requires revenue source. Data monetization solves the provider's structural problem of funding global email service at zero marginal cost to end users. The provider benefits from the constraint (arbitrage: can extract while users remain captive). Extraction flows toward the provider, but the coordination function is genuine — the constraint enables the service existence and continuation.
constraint_indexing:constraint_classification(email_data_monetization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — GDPR, CCPA, and emerging privacy frameworks create temporary enforcement against uncontrolled data monetization. Regulatory constraints have explicit sunsets: they require ongoing political maintenance and adapt to provider workarounds. Organized actors (privacy advocates, regulators, researchers) see the constraint as solvable through technical and legal means (data minimization mandates, consent mechanisms, transparency requirements). Suppression is declining: regulatory pressure reduces opacity and enables exit (explicit opt-out creates real alternatives). The scaffold classification reflects genuine coordinative intent (protecting user rights) with temporary enforcement and visible sunset (regulations must be renewed, adapted to emerging extraction mechanisms).
constraint_indexing:constraint_classification(email_data_monetization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: 'FREE SERVICE' CULTURAL NORM (PITON) — The framing of email as 'free' service is substantially performative. Users have internalized this cultural norm (email should cost nothing) despite the actual extraction of their data having substantial economic value. The norm persists through institutional inertia: decades of free-email culture have made users unable to imagine or accept paid alternatives. The theater_ratio is high (0.65) because the 'free' framing obscures the hidden cost (data monetization). The primary function (communicating messages) continues, but the extractive mechanism (data monetization) is maintained through cultural narrative rather than technical or economic necessity. Paid alternatives exist but fail to displace the free model due to the norm's persistence.
constraint_indexing:constraint_classification(email_data_monetization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ADVERTISING PLATFORM ECOSYSTEM (TANGLED ROPE) — Organized institutional actor (Google, Meta, other ad networks) that benefits from email data monetization while also experiencing coordination constraints. The constraint enables targeted advertising (genuine coordination function: matching ads to relevant audiences reduces information friction). Extraction is asymmetric: platforms capture disproportionate value from first-party user data; email providers capture secondary value. Both benefit relative to users (who bear all cost), but the ecosystem experiences internal competition and regulatory pressure that constrains pure extraction. The platform sees the constraint as hybrid: coordination mechanism + competitive extraction among institutional beneficiaries.
constraint_indexing:constraint_classification(email_data_monetization, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a purely economic perspective, email monetization appears as an immutable law: 'if you don't pay for the product, you are the product.' This frames data extraction as inherent to digital capitalism — any alternative (universal paid email, public-utility email) would require structural economic changes (subsidy, regulation, or collective action) incompatible with market systems. The mountain classification risks naturalizing what is actually a contingent institutional arrangement (specific business model choices by dominant providers plus regulatory failure). The analytical observer's perspective reveals the false summit: the constraint appears immutable only if one takes current institutional arrangements (advertising-based web, venture-backed platforms, regulatory deference) as economic constants rather than contingent policy choices.
constraint_indexing:constraint_classification(email_data_monetization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(email_data_monetization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(email_data_monetization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(email_data_monetization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(email_data_monetization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(email_data_monetization, TR),
    TR >= 0.70.

:- end_tests(email_data_monetization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Users lose detailed behavioral data (email timing, communication patterns, recipient networks, inferred interests from message content) with substantial commercial value. However, extractiveness is not extreme (0.70+) because: (a) genuine coordination function exists — email infrastructure requires revenue, and data monetization is one solution; (b) some user benefit exists — free email service enables participation in digital life; (c) regulatory pressure is reducing opacity and enabling exit. The 0.58 value reflects that extraction is significant but hybrid with coordination. Suppression (0.68): High. Structural barriers to exit include network effects (communication necessity, contact inertia), information opacity (terms of service are nearly unreadable; data use is obscure), and lack of competitive alternatives (all major providers use similar models). Psychological suppression is also high — users have internalized the 'free email' norm as inevitable, reducing perceived legitimacy of alternatives. Theater ratio (0.65): Moderately high and increasing over time. The 'free service' framing obscures the data extraction mechanism. Consent mechanisms (privacy policies, opt-in checkboxes) constitute theater — most users do not read them, do not understand what they authorize, and experience them as unavoidable friction. However, theater is not as high as piton-level (0.70+) because: (a) the service *does* provide real email functionality; (b) regulatory pressure is reducing opacity; (c) some users are aware of the extraction mechanism. The rising trend (0.40 → 0.65) indicates that providers are increasingly deploying theatrical justifications (GDPR consent dialogs, privacy commitments, transparency reports) as the actual extraction continues to intensify.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between trapped and beneficiary perspectives reveals the constraint's structure. From the trapped user's view (d=1.0), experienced extractiveness is χ = 0.58 × 1.42 × σ(global) = 0.58 × 1.42 × 1.2 ≈ 0.99 (approaching pure Snare thresholds). From the service provider's view (d=0.05), experienced extractiveness is χ = 0.58 × (-0.12) × 1.2 ≈ -0.08 (negative extraction, pure coordination benefit). This 1.07-point gap is the constraint's signature: it is experienced as maximally extractive by powerless agents and as coordinative benefit by institutional beneficiaries, with the same base extraction (0.58) producing opposite-signed chi values. The moderate/constrained victim (d=0.65, f(d)=1.00) experiences χ = 0.58 × 1.0 × 1.2 ≈ 0.70 — near snare territory but with agency — correctly perceiving the hybrid nature of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from the agent's structural relationship to the data extraction flow. Trapped users who cannot exit have d → 1.0 (full targets): the sigmoid f(d) produces maximum experienced extraction (f(1.0) ≈ 1.42). Service providers who benefit from the constraint have d → 0.05 (full beneficiaries): f(d) produces negative effective extraction, making the constraint appear as pure coordination. Privacy-conscious users have d → 0.65 (victims with partial exit capacity): f(d) ≈ 1.00, producing experienced extractiveness equal to base extractiveness. Regulatory organized agents have d → 0.45 (competing institutions): f(d) ≈ 0.55, producing suppressed experienced extraction as regulatory pressure reduces the constraint's effect. Advertising platforms have d → 0.15 (secondary beneficiaries): f(d) ≈ -0.01, making extraction invisible from their perspective. The analytical observer with arbitrage-like capacity to shift framing has d → 0.72: f(d) ≈ 1.15, experienced extraction slightly above base, reflecting the observatory challenge of seeing through naturalized economic narratives. These directionality values are not overridden — they derive cleanly from the structural beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that tangled rope classification is correct and stable. The mandatrophy question — 'Is this extraction or coordination?' — is answered by the structural data: (a) genuine coordination function exists (email infrastructure requires revenue; alternative models either fail or require subsidy/collective action); (b) asymmetric extraction is present (users lose data value with minimal compensation); (c) active enforcement is required (providers must invest in data extraction and monetization systems; regulators must enforce disclosure). All three tangled rope gates are met. The constraint does not degrade to pure snare because the coordination function is real and some users perceive benefit (free email service). It does not collapse to pure rope because the extraction is substantial and suppression is high. The mandatrophy is resolved through perspectival heterogeneity: from different positions in the constraint, agents accurately perceive different aspects of its hybrid nature. The false summit is the 'economic law' framing (Mountain perspective) — this perspective risks naturalizing the constraint as immutable when it is actually contingent on specific business model choices and regulatory decisions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    data_valuation_transparency,
    'What is the actual economic value of user email data being extracted, and is this value transparent to users?',
    'Empirical analysis of advertising spend derived from email-targeting data; comparison of user-estimated value vs provider-calculated value; behavioral economics studies on valuation awareness',
    'If value is substantial and non-transparent: extraction is maximized by user ignorance (Snare classification strengthened). If value is marginal or transparent: extraction is lower than perceived (Rope classification becomes more plausible).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(data_valuation_transparency, empirical, 'Transparency and magnitude of data monetization value').

omega_variable(
    genuine_service_provisioning_cost,
    'What fraction of email service cost is inherently technical (server infrastructure, maintenance) vs. what fraction is extractive profit-taking?',
    'Comparative analysis of email provider cost structures; benchmarking against non-profit and paid alternatives (Proton Mail, university-hosted); cost modeling of minimal viable email infrastructure',
    'If technical costs require majority of revenue: data monetization is coordination necessity (Rope strengthened). If technical costs are small relative to monetization revenue: extraction is opportunistic (Snare/Tangled Rope strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(genuine_service_provisioning_cost, empirical, 'Technical cost necessity vs extractive profit margin').

omega_variable(
    regulatory_mechanism_effectiveness,
    'Do data protection regulations (GDPR, CCPA) actually constrain email data monetization or do they create ''consent theater'' that users click through without understanding?',
    'Empirical audit of provider consent mechanisms; analysis of opt-out rates before/after regulation; behavioral economics studies on comprehension of privacy policies; technological assessment of whether regulations reduce actual data monetization volume',
    'If regulations are effective: scaffold perspective is correct — enforcement is real and suppression is declining. If regulations are theater: suppression remains high, and Snare classification is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_mechanism_effectiveness, empirical, 'Whether regulatory mechanisms genuinely constrain extraction or constitute theater').

omega_variable(
    network_effect_necessity,
    'Is email network consolidation (Gmail''s 1.5B+ users) structurally necessary for global email coordination, or is it a contingent outcome of winner-take-most platform dynamics?',
    'Historical analysis of federated email alternatives; technical assessment of why decentralized email (SMTP federation) failed to scale; comparison with messaging platforms that maintained federation (Matrix, ActivityPub); policy thought experiments on what regulatory intervention would be required to break concentration',
    'If consolidation is structurally necessary: trapped exit is real (Mountain or Snare). If consolidation is contingent: exit is more available than perceived, and suppression is partially internalized (identity_locked classification).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(network_effect_necessity, conceptual, 'Whether platform consolidation is structurally necessary or contingent').

omega_variable(
    collective_action_threshold,
    'What critical mass of users would be required to shift to privacy-focused or paid email alternatives to create viable competitive pressure on dominant providers?',
    'Network analysis of email switching costs; behavioral economics of coordination games; historical case studies of platform migration; technology assessment of interoperability protocols',
    'If threshold is very high (>50% users): organized resistance is unlikely, Snare classification holds. If threshold is achievable (<15% users): coalition formation is plausible, scaffold/piton classifications become more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_action_threshold, empirical, 'Coordination threshold for viable alternatives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(email_data_monetization, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(email_mon_tr_t0, email_data_monetization, theater_ratio, 0, 0.4).
narrative_ontology:measurement(email_mon_tr_t10, email_data_monetization, theater_ratio, 10, 0.55).
narrative_ontology:measurement(email_mon_tr_t20, email_data_monetization, theater_ratio, 20, 0.65).
narrative_ontology:measurement(email_mon_tr_t30, email_data_monetization, theater_ratio, 30, 0.62).

% Extraction over time
narrative_ontology:measurement(email_mon_be_t0, email_data_monetization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(email_mon_be_t10, email_data_monetization, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(email_mon_be_t20, email_data_monetization, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(email_mon_be_t30, email_data_monetization, base_extractiveness, 30, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(email_data_monetization, resource_allocation).
narrative_ontology:affects_constraint(email_data_monetization, platform_algorithmic_targeting).
narrative_ontology:affects_constraint(email_data_monetization, privacy_regulatory_capture).
narrative_ontology:affects_constraint(email_data_monetization, digital_advertising_market_concentration).

% DUAL FORMULATION NOTE:
% Email data monetization is downstream of two distinct structural constraints: (1) platform business model dependency on advertising revenue (broader constraint affecting all ad-supported platforms), and (2) regulatory failure to enforce user data rights (governance constraint). This story focuses on the email-specific manifestation of both upstream constraints. The advertising platform ecosystem constraint and the privacy regulatory capture constraint have their own ε values and perspectives; email data monetization represents their intersection in the email domain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
