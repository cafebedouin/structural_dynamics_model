% ============================================================================
% CONSTRAINT STORY: platform_advertising_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_platform_advertising_extraction, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: platform_advertising_extraction
 *   human_readable: Platform Advertising Extraction: Asymmetric Value Capture from User Attention
 *   domain: digital_economy/advertising_markets
 *
 * SUMMARY:
 *   Platform advertising extraction represents the core revenue mechanism and
 *   value-capture architecture of digital platforms (Google, Meta, TikTok,
 *   Amazon). Platforms provide genuine coordination services—connecting
 *   advertisers to interested audiences at scale and enabling free-to-user
 *   services that facilitate massive social and economic coordination.
 *   Simultaneously, they extract value asymmetrically by treating user
 *   attention and behavioral data as appropriable commodities, deploying
 *   surveillance infrastructure, manipulative algorithmic ranking, and
 *   behavioral targeting that exploits cognitive vulnerabilities. The
 *   constraint exhibits all six classification types depending on structural
 *   position: users experience maximum extraction (snare); small advertisers
 *   experience mixed coordination and constraint (tangled rope); large
 *   advertisers experience genuine coordination with favorable terms (rope);
 *   regulators recognize both coordination and extraction (tangled rope);
 *   legacy advertising persists through institutional inertia (piton); and
 *   the civilizational analytical observer sees irreducible tension between
 *   coordination and extraction (tangled rope). The theater_ratio (0.68)
 *   reflects that platform advertising contains significant performative
 *   elements: brand-building goals often pursued through advertising are
 *   increasingly displaced by direct behavioral targeting for conversion
 *   optimization, yet branding messaging persists as ritual.
 *
 * KEY AGENTS:
 *   - Users/Audience: Primary victims (powerless/trapped) — provide attention and behavioral data with zero compensation, cannot exit without platform access loss
 *   - Small Business Advertisers: Secondary victims (moderate/constrained) — need platform access, bear high switching costs, limited bargaining power
 *   - Major Brand Advertisers: Primary beneficiaries (institutional/arbitrage) — have bargaining power, can arbitrage between platforms, benefit substantially from targeting and reach
 *   - Advertising Platforms (Google, Meta, Amazon, TikTok): Primary extractors (institutional/arbitrage) — control targeting infrastructure, capture behavioral data, set fees unilaterally
 *   - Privacy and Competition Authorities: Organized observers (organized/constrained) — recognize both coordination and extraction, face regulatory capture from platform lobbying
 *   - Legacy Advertising Industry: Institutional actor (institutional/arbitrage) — print, TV, outdoor advertising persists through inertia despite reduced effectiveness
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees irreducible coordination-extraction tension
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(platform_advertising_extraction, 0.58).
domain_priors:suppression_score(platform_advertising_extraction, 0.65).
domain_priors:theater_ratio(platform_advertising_extraction, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(platform_advertising_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(platform_advertising_extraction, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(platform_advertising_extraction, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(platform_advertising_extraction, tangled_rope).
narrative_ontology:human_readable(platform_advertising_extraction, "Platform Advertising Extraction: Asymmetric Value Capture from User Attention").
narrative_ontology:topic_domain(platform_advertising_extraction, "digital_economy/advertising_markets").

domain_priors:requires_active_enforcement(platform_advertising_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(platform_advertising_extraction, advertising_platforms).
narrative_ontology:constraint_beneficiary(platform_advertising_extraction, brand_advertisers).
narrative_ontology:constraint_victim(platform_advertising_extraction, user_privacy).
narrative_ontology:constraint_victim(platform_advertising_extraction, user_attention_commons).
narrative_ontology:constraint_victim(platform_advertising_extraction, competing_publishers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: USER AS POWERLESS VICTIM (SNARE) — Users are trapped in the advertising system with no meaningful exit. They provide attention and behavioral data with no compensation and cannot opt out without sacrificing platform access. The constraint extracts maximum value from their attention through algorithmic targeting that exploits behavioral vulnerabilities. Suppression is total: without the platform, social coordination and economic participation become impossible for most users.
constraint_indexing:constraint_classification(platform_advertising_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL ADVERTISER (TANGLED ROPE) — Small businesses face genuine constraints: they need platform access to reach customers and bear significant costs to switch platforms, but they also benefit from the platform's targeting and distribution capabilities. The constraint coordinates their access to customers while extracting high fees and data. Extraction is asymmetric but not maximal — some genuine service provision exists alongside rent extraction.
constraint_indexing:constraint_classification(platform_advertising_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR ADVERTISER (ROPE) — Large brands have significant bargaining power and can arbitrage between platforms. They experience the constraint as coordination: platforms solve the targeting and distribution problem at scale. Benefits substantially outweigh costs. Effective extraction approaches zero from this perspective.
constraint_indexing:constraint_classification(platform_advertising_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PRIVACY COALITION (TANGLED ROPE) — Regulators, privacy advocates, and competition authorities see genuine coordination benefits (efficient matching) but are increasingly recognizing extractive mechanisms (data monopolization, behavioral targeting exploitation, market foreclosure). The constraint both coordinates and extracts. High suppression reflects regulatory capture and lobbying advantages of platforms.
constraint_indexing:constraint_classification(platform_advertising_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY ADVERTISING INDUSTRY (PITON) — Television, print, and outdoor advertising continue through institutional inertia despite dramatically reduced effectiveness compared to platform advertising. The theater_ratio reflects performative compliance with branding goals that platforms have already superseded. Legacy advertising persists because contracts, relationships, and organizational identities depend on it, not because the mechanism works.
constraint_indexing:constraint_classification(platform_advertising_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational scope, platform advertising simultaneously solves a genuine coordination problem (connecting advertiser messages to interested audiences) and creates a new extraction mechanism (monopolistic control over the targeting commons through behavioral surveillance). The constraint contains both functions in irreducible tension. The suppression reflects that alternatives to platform-mediated advertising are difficult to build at scale.
constraint_indexing:constraint_classification(platform_advertising_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(platform_advertising_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(platform_advertising_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(platform_advertising_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(platform_advertising_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(platform_advertising_extraction, TR),
    TR >= 0.70.

:- end_tests(platform_advertising_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. Platforms capture substantial asymmetric value from user attention and behavioral data. However, extraction is not maximal because genuine coordination benefits exist—efficient targeting does solve a real matching problem, and users receive free services (genuine exchange, even if asymmetric). Suppression rose from 0.35 to 0.58 over the interval as network effects strengthened platform lock-in. Suppression (0.65): High. Users cannot exit without sacrificing social participation and economic access. Alternatives are technically possible but economically unviable at platform scale. Behavioral targeting exploits cognitive biases (intermittent reinforcement, social comparison), adding psychological suppression beyond structural lock-in. Theater ratio (0.68): High. Platform advertising contains performance ritual: brand awareness campaigns and engagement metrics diverge from actual behavior change; advertisers maintain branding spend partly from organizational inertia and sunk identity; algorithmic optimization often obscures whether targeting actually drives value. The trend from 0.52 to 0.68 reflects growing divergence between stated advertising goals and actual extractive mechanism (behavioral data monetization).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. Users see a snare (total extraction, no escape, behavioral targeting weaponized against them). Small advertisers see tangled rope (genuine service with unfair terms). Large advertisers see rope (efficient coordination at favorable rates). Platforms see rope (solving the targeting problem at massive scale). Regulators see tangled rope (both coordination and extraction, grappling with how to preserve benefits while constraining rent-seeking). Legacy advertising sees piton (performative ritual persisting through organizational inertia). The analytical observer sees tangled rope at civilizational scope, recognizing that the coordination function and extraction function are structurally inseparable in current platform architectures. The gap between user and advertiser experiences is the largest among the constraint stories in the corpus—the same constraint simultaneously subsidizes major advertisers while maximally extracting from users.
 *
 * DIRECTIONALITY LOGIC:
 *   Users as trapped victims with powerless status derive d → 0.95 (near-maximal targeting), producing high f(d) ≈ 1.42 and high experienced extraction χ. Small advertisers as constrained moderate agents derive d → 0.65 (mixed), producing f(d) ≈ 1.00 and moderate χ. Large advertisers as arbitrage-mobile institutional agents derive d → 0.10 (beneficiary advantage), producing f(d) ≈ -0.01 and near-zero or negative χ (they gain). Platforms themselves as institutional beneficiaries with arbitrage options occupy d → 0.00 (beneficiary-optimal), producing f(d) ≈ -0.12, reflecting that the constraint subsidizes their position. The perspectival gap between users and major advertisers (Δd ≈ 0.85) reflects the core structural asymmetry: the same constraint extracts from one group while subsidizing the other.
 *
 * MANDATROPHY ANALYSIS:
 *   STRUCTURAL RESOLUTION: Platform advertising resolves mandatrophy by demonstrating that genuine coordination and asymmetric extraction can coexist in irreducible tension. The constraint IS both a rope (for advertisers seeking efficient targeting) AND a snare (for users whose attention is appropriated). The mandatrophy is not resolved by choosing one perspective; it is resolved by recognizing that the observed classification divergence instantiates a real structural asymmetry in the constraint's design. Platform architecture deliberately couples user targeting (extraction) with advertiser coordination (service provision). The coupling is not accidental or parasitic—it is the generative mechanism. Uncoupling them (e.g., privacy-respecting opt-in targeting, decentralized alternatives) would reduce extraction but also reduce the efficiency gains that make platforms economically viable. This is why the analytical tangled rope classification is correct: the constraint cannot be classified as pure rope without ignoring user extraction, cannot be classified as pure snare without ignoring advertiser coordination, and cannot be decomposed into separate rope and snare constraints without losing the essential insight that the extraction mechanism IS the coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    targeting_efficiency_vs_behavioral_extraction,
    'How much of the platform''s extractive value derives from efficient matching versus exploitation of behavioral vulnerabilities and cognitive biases?',
    'Comparative analysis of advertising effectiveness with opt-in targeting (user-consented data use) versus dark-pattern targeting (manipulative engagement). Measurement of actual conversion value attributable to accurate targeting versus incremental conversions from exploitive behavioral targeting.',
    'If targeting efficiency dominates: constraint reclassifies as rope (genuine coordination benefit justifies extraction). If behavioral exploitation dominates: constraint deepens as snare (extraction parasitic on cognitive manipulation). Changes χ magnitude substantially.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(targeting_efficiency_vs_behavioral_extraction, empirical, 'Attribution of platform value to targeting efficiency versus behavioral exploitation').

omega_variable(
    user_consent_fiction,
    'Does the terms-of-service consent to data collection and advertising targeting represent genuine informed consent or is it a suppression mechanism that naturalizes extraction?',
    'User comprehension studies; analysis of takedown velocity when platforms present transparency versus opacity in privacy settings; measurement of exit rates under different consent framing conditions.',
    'If genuine: suppression is lower than measured (user agency partially retained). If fiction: suppression is higher (cognitive capture adds to technical lock-in). Affects classification from powerless perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(user_consent_fiction, empirical, 'Whether user consent represents genuine agency or suppression mechanism').

omega_variable(
    alternative_advertising_models_feasibility,
    'Are decentralized, privacy-preserving, or subscription-based advertising models structurally viable at scale, or are platform monopolies inherent to digital advertising economics?',
    'Historical analysis of platform alternatives (DuckDuckGo, Mastodon, BlueSky, privacy-focused ecosystems); cost and complexity analysis of building competing infrastructure; measurement of adoption rates for privacy-respecting alternatives when available.',
    'If alternatives are viable: sunset perspective becomes scaffolding (deliberate transition possible). If monopolistic: constraint is structurally immobile (piton or snare depending on extraction severity). Affects temporal horizon interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_advertising_models_feasibility, empirical, 'Structural feasibility of alternative advertising models').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.65) primarily structural (users genuinely need platform access for economic participation) or internalized (users have accepted constant surveillance as normal)?',
    'Cohort analysis: exit behavior when users gain transparency about tracking and data use; intergenerational comparison of consent responses; measurement of persistence of privacy concerns post-GDPR/CCPA implementation.',
    'If structural: escape requires building alternative infrastructure (high cost). If internalized: cognitive reframing could reduce perceived constraint (lower real suppression). Affects target-agent''s experienced extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(platform_advertising_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(padv_tr_t0, platform_advertising_extraction, theater_ratio, 0, 0.52).
narrative_ontology:measurement(padv_tr_t5, platform_advertising_extraction, theater_ratio, 5, 0.6).
narrative_ontology:measurement(padv_tr_t10, platform_advertising_extraction, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(padv_be_t0, platform_advertising_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(padv_be_t5, platform_advertising_extraction, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(padv_be_t10, platform_advertising_extraction, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(platform_advertising_extraction, resource_allocation).
narrative_ontology:boltzmann_floor_override(platform_advertising_extraction, 0.18).
narrative_ontology:affects_constraint(platform_advertising_extraction, algorithmic_ranking_amplification).
narrative_ontology:affects_constraint(platform_advertising_extraction, data_monopoly_network_effects).
narrative_ontology:affects_constraint(platform_advertising_extraction, behavioral_targeting_externality).

% DUAL FORMULATION NOTE:
% Platform advertising extraction is upstream of several downstream constraints: algorithmic ranking amplification (the targeting mechanism feeds into ranking biases), data monopoly network effects (behavioral data accumulation strengthens platform lock-in), and behavioral targeting externality (externalized harms from manipulative targeting spread through society). Each downstream constraint has its own ε reflecting specific mechanisms; the advertising extraction story models the core value-capture architecture.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(platform_advertising_extraction, institutional, 0.02).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
