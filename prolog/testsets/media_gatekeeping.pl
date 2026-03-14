% ============================================================================
% CONSTRAINT STORY: media_gatekeeping
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_media_gatekeeping, []).

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
 *   constraint_id: media_gatekeeping
 *   human_readable: Media Gatekeeping and Access to Public Discourse
 *   domain: media/communication/political_economy
 *
 * SUMMARY:
 *   Media gatekeeping is the structural constraint that controls which voices
 *   reach large audiences through established information infrastructure.
 *   This constraint combines genuine coordination (editorial verification,
 *   quality standards, audience trust) with systematic extraction (exclusion
 *   of dissident voices, advertising-driven attention distortion, incumbent
 *   political protection). The gatekeeping function exhibits a perspectival
 *   gap of remarkable breadth: from the powerless excluded voice, it appears
 *   as pure extraction (snare); from established media, as pure coordination
 *   (rope); from organized alternative movements, as a temporary problem with
 *   a sunset (scaffold). The theater ratio indicates increasing
 *   performativity in professional journalism: clickbait headlines,
 *   opinion-driven selection, verification theater (fact-checks designed for
 *   reassurance rather than accuracy). The extractiveness trajectory shows
 *   accumulation of extraction mechanisms over the 20-year interval as
 *   platform economics intensified while traditional credibility checks
 *   weakened. Extraction increased from 0.42 (2005-era professional
 *   gatekeeping with some genuine verification function) to 0.58
 *   (contemporary state with advertiser influence, algorithm dominance, and
 *   editorial theater).
 *
 * KEY AGENTS:
 *   - Excluded Voices: Primary victim (powerless/trapped) — individuals, movements, and alternative perspectives without access to established platforms; bear information asymmetry cost
 *   - Alternative Publishers: Secondary victim (moderate/constrained) — independent outlets, niche publications, local news struggling against platform dependencies and advertising fragmentation
 *   - Established Media Institutions: Primary beneficiary (institutional/arbitrage) — major news organizations that monetize gatekeeping function; have high exit mobility across distribution channels and advertiser networks
 *   - Advertiser Networks: Secondary beneficiary (powerful/mobile) — benefit from concentrated audience attention and premium placement control; can switch between media outlets and platforms
 *   - Open Information Movement: Organized resistance (organized/constrained) — Wikipedia, open-access journalism, blockchain media, citizen networks building alternative distribution and credibility pathways
 *   - Professional Journalism System: Institutional actor (institutional/arbitrage) — maintains gatekeeping ritual through professional licensing and credentialing; sees own practice as degraded (piton perspective)
 *   - Platform Algorithm Operators: Emerging beneficiary (institutional/arbitrage) — Facebook, YouTube, TikTok now perform secondary gatekeeping; extract through attention concentration and advertiser placement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(media_gatekeeping, 0.58).
domain_priors:suppression_score(media_gatekeeping, 0.68).
domain_priors:theater_ratio(media_gatekeeping, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(media_gatekeeping, extractiveness, 0.58).
narrative_ontology:constraint_metric(media_gatekeeping, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(media_gatekeeping, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(media_gatekeeping, tangled_rope).
narrative_ontology:human_readable(media_gatekeeping, "Media Gatekeeping and Access to Public Discourse").
narrative_ontology:topic_domain(media_gatekeeping, "media/communication/political_economy").

domain_priors:requires_active_enforcement(media_gatekeeping).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(media_gatekeeping, established_media_institutions).
narrative_ontology:constraint_beneficiary(media_gatekeeping, advertiser_networks).
narrative_ontology:constraint_beneficiary(media_gatekeeping, incumbent_political_actors).
narrative_ontology:constraint_victim(media_gatekeeping, excluded_voices).
narrative_ontology:constraint_victim(media_gatekeeping, alternative_publishers).
narrative_ontology:constraint_victim(media_gatekeeping, information_asymmetry_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED VOICE (SNARE) — An individual or group without access to established media platforms faces nearly insurmountable barriers to reaching large audiences. Exit options are minimal: social media offers some workaround but carries algorithmic gatekeeping and verification barriers. The constraint is experienced as pure extraction — bearing the cost of information asymmetry with no coordination benefit.
constraint_indexing:constraint_classification(media_gatekeeping, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INDEPENDENT PUBLISHER (TANGLED ROPE) — Local news outlets, niche publications, and independent journalists experience both coordination benefits (shared standards of journalistic practice, editorial review systems, audience trust from professional gatekeeping) and extraction (dependence on advertising networks, fragile business models, vulnerability to platform algorithm changes). Exit is constrained by capital requirements and platform dependencies.
constraint_indexing:constraint_classification(media_gatekeeping, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: ESTABLISHED MEDIA INSTITUTION (ROPE) — Major news organizations benefit from the gatekeeping constraint while experiencing it as coordination mechanism. The constraint enables them to monetize attention, maintains their franchise value through scarcity, and provides stable business model through advertiser relationships. They have high arbitrage exit options — they can shift between distribution platforms, audience models, and advertiser networks.
constraint_indexing:constraint_classification(media_gatekeeping, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN INFORMATION MOVEMENT (SCAFFOLD) — Organized actors (Wikipedia, open-access journalism projects, decentralized platforms, citizen journalism networks) perceive gatekeeping as a temporary structural problem being solved through technological and institutional innovation. The constraint exhibits genuine sunset logic: distributed publishing platforms, blockchain-based verification, and crowdsourced credibility systems are building alternative access pathways. Sunset horizon: 15-25 years as decentralized information infrastructure matures.
constraint_indexing:constraint_classification(media_gatekeeping, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PROFESSIONAL JOURNALISM RITUAL (PITON) — The professional gatekeeping function (editorial standards, fact-checking, source verification) has partially degraded while institutional inertia maintains its authority. Many established outlets practice theater journalism: headlines designed for engagement rather than accuracy, clickbait verification, opinion-section dominance. Yet the professional ritual persists as the primary legitimacy source for 'real news' versus 'information.' Theater ratio indicates that gatekeeping increasingly performs legitimacy rather than ensuring accuracy.
constraint_indexing:constraint_classification(media_gatekeeping, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW (MOUNTAIN) — From a civilizational/universal perspective, information scarcity and attention bottlenecks are inherent to human cognition: no individual can process all available information, and filtering mechanisms (gatekeeping) are inevitably necessary. The constraint appears as natural law — there must be some mechanism to select which claims reach public attention. However, structural data contradicts this classification: gatekeeping is a contingent institutional choice (profit-driven editorial selection) rather than a cognitive inevitability (neutral filtering). The engine will identify this as a false summit.
constraint_indexing:constraint_classification(media_gatekeeping, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(media_gatekeeping_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(media_gatekeeping, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(media_gatekeeping, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(media_gatekeeping, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(media_gatekeeping, TR),
    TR >= 0.70.

:- end_tests(media_gatekeeping_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Media gatekeeping extracts by excluding voices, concentrating advertiser subsidy, and channeling political access. However, extraction is not maximal (would be 0.70+) because some gatekeeping function is genuinely coordinative — editorial standards, fact-checking, and professional norms do provide value. The extractiveness trajectory (0.42 → 0.58) reflects that coordination value has declined as platforms intensified audience concentration and advertising became the dominant business model. Suppression (0.68): High. Excluded voices face multiple barriers: capital requirements for media ownership, platform algorithmic filtering, lack of professional credentialing, and network effects favoring established outlets. Suppression is not total (social media and low-cost publishing offer some workaround) but substantial. Theater ratio (0.62): Moderate-high. Professional journalism ritual persists (fact-checking sections, editorial review, byline credentialing) but increasingly performs legitimacy rather than ensures accuracy. Clickbait headlines, opinion-driven section placement, verification theater (fact-checks as reassurance mechanism) indicate significant theatrical component. Trend is rising (0.35 → 0.62) as engagement metrics drive editorial selection.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates an extreme perspectival gap. The established media institution experiences gatekeeping as a rope (coordination mechanism that solves audience trust and quality assurance problems). The excluded voice experiences it as a snare (pure extraction with no offsetting coordination benefit). The independent publisher experiences a tangled rope (genuine coordination value in quality standards and audience trust, but also genuine extraction through advertising dependency and algorithm vulnerability). The open information movement sees a scaffold (temporary structural problem being solved by distributed platforms and decentralized credibility systems with a realistic sunset). The professional journalism system sees itself as maintaining a degraded ritual (piton — theater persisting through institutional inertia despite declining functional value). The analytical observer risks treating gatekeeping as a natural law (inevitable consequence of attention scarcity) when it is actually a contingent institutional arrangement (profit-driven, advertiser-controlled, technology-dependent). The perspectival gap reveals that no single type captures the full structure — tangled_rope is the only classification that accommodates the simultaneous presence of genuine coordination and real extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (established media, advertisers) have high-mobility exit options (arbitrage) — they can shift between distribution platforms, audience models, and monetization strategies. Their directionality value is low (d ≈ 0.15-0.25), producing negative or low-positive effective extractiveness: they experience the constraint as enabling rather than constraining. Victims (excluded voices) have trapped or constrained exit options. Excluded voices cannot afford media ownership or platform dominance and lack professional credentialing pathways — directionality high (d ≈ 0.85-0.95). Independent publishers have constrained exit (capital requirements, platform dependency limits mobility) — directionality moderate-high (d ≈ 0.60-0.70). The open information movement has organized power and constrained-but-meaningful exit options (can build alternative platforms) — directionality moderate (d ≈ 0.50-0.60). The professional journalism system sees itself as beneficiary-turned-victim (institutional identity is tied to the gatekeeping role, but the role is degrading) — requires override.
 *
 * MANDATROPHY ANALYSIS:
 *   DIAGNOSTIC CASE: Media gatekeeping resolves the mandatrophy by demonstrating that classification type is perspectival rather than universal. The constraint is genuinely tangled_rope at the system level (both coordination and extraction present simultaneously), but this classification dissolves when viewing from specific positions. From the beneficiary's position, it's rope (pure coordination). From the powerless position, it's snare (pure extraction). The mandatrophy is resolved by recognizing that the tangled_rope classification is the presheaf over all perspectives — it describes how the constraint appears when you integrate the views of all structural positions, accounting for the fact that beneficiaries and victims experience opposite directions of the same mechanism. The false mountain classification (gatekeeping as natural law of attention) is caught by structural data: the constraint requires active enforcement (requires_active_enforcement: true), has clear beneficiaries and victims, and exhibits theater ratio growth over time. Mountains do not require enforcement or beneficiary protection — they persist regardless. The analytical false summit reveals that 'attention scarcity makes gatekeeping inevitable' naturalizes what is actually a policy choice (profit-driven business model, advertiser subsidy, professional credentialing requirements).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    platform_gatekeeping_displacement,
    'Has traditional media gatekeeping been displaced by platform algorithmic gatekeeping, or merely supplemented?',
    'Comparative analysis of information reach: studies of news diffusion patterns pre- and post-social-media dominance; measurement of alternative-source visibility on algorithmic platforms vs traditional media',
    'If displaced: constraint type remains tangled_rope but with different beneficiary (platform operators instead of editors). If supplemented: dual gatekeeping system creates compounding extraction. If neither: gatekeeping evolves but extraction mechanisms persist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_gatekeeping_displacement, empirical, 'Whether platform algorithms replace or supplement traditional media gatekeeping').

omega_variable(
    credibility_verification_necessity,
    'How much of the gatekeeping constraint''s suppression derives from legitimate credibility verification vs artificial scarcity maintenance?',
    'Comparative credential analysis: fact-check accuracy of gated sources vs crowdsourced/decentralized alternatives; correlation between gatekeeping and information quality metrics',
    'If mostly legitimate: gatekeeping classification shifts toward rope (pure coordination). If mostly artificial scarcity: classification shifts toward snare (pure extraction). If mixed: tangled_rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credibility_verification_necessity, empirical, 'Ratio of credibility verification to artificial scarcity in gatekeeping suppression').

omega_variable(
    decentralized_platform_maturity,
    'Are decentralized information platforms (blockchain news, distributed publishing, community fact-checking) approaching functional parity with centralized media for rapid high-quality information distribution?',
    'Timeline analysis of platform adoption curves; measurement of information latency, verification speed, and audience reach for decentralized platforms; tracking of institutional blockchain journalism projects',
    'If approaching parity within 10 years: scaffold sunset logic is real and timeline is accurate. If stalling: scaffold is aspirational and sunset is indefinite. If exceeded: gatekeeping constraint dissolves faster than modeled.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(decentralized_platform_maturity, empirical, 'Timeline to functional decentralized platform parity').

omega_variable(
    economic_sustainability_alternative_models,
    'Can alternative publishing models (direct reader support, public funding, cooperative ownership) achieve economic sustainability competitive with advertising-subsidized gatekeeping?',
    'Longitudinal financial analysis of subscription-funded, public-funded, and cooperative news outlets; correlation between funding model and editorial independence metrics',
    'If yes: scaffold sunset accelerates — direct-support models remove advertiser gatekeeping. If no: economic constraint becomes independent binding mechanism, lifting extractiveness. Classification may shift toward snare if economic barriers become insurmountable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_sustainability_alternative_models, empirical, 'Viability of alternative publishing economic models').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(media_gatekeeping, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(media_gk_tr_t0, media_gatekeeping, theater_ratio, 0, 0.35).
narrative_ontology:measurement(media_gk_tr_t10, media_gatekeeping, theater_ratio, 10, 0.55).
narrative_ontology:measurement(media_gk_tr_t20, media_gatekeeping, theater_ratio, 20, 0.62).
narrative_ontology:measurement(media_gk_tr_t5, media_gatekeeping, theater_ratio, 5, 0.45).
narrative_ontology:measurement(media_gk_tr_t15, media_gatekeeping, theater_ratio, 15, 0.59).

% Extraction over time
narrative_ontology:measurement(media_gk_be_t0, media_gatekeeping, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(media_gk_be_t10, media_gatekeeping, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(media_gk_be_t20, media_gatekeeping, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(media_gk_be_t5, media_gatekeeping, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(media_gk_be_t15, media_gatekeeping, base_extractiveness, 15, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(media_gatekeeping, information_standard).
narrative_ontology:affects_constraint(media_gatekeeping, algorithmic_platform_attention).
narrative_ontology:affects_constraint(media_gatekeeping, political_information_asymmetry).
narrative_ontology:affects_constraint(media_gatekeeping, professional_credentialing_exclusion).

% DUAL FORMULATION NOTE:
% Media gatekeeping decomposes into multiple structurally distinct constraints: (1) editorial gatekeeping (filtering by professional judgment), (2) platform algorithmic gatekeeping (filtering by engagement maximization), (3) economic gatekeeping (access requires capital for media ownership or advertising budget). Each has different ε values and should be modeled as separate stories with cross-constraint dependencies. This story represents the integrated system perspective.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(media_gatekeeping, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
