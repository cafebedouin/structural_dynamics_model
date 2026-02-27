% ============================================================================
% CONSTRAINT STORY: sk_newtro_aesthetic
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sk_newtro_aesthetic, []).

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
 *   constraint_id: sk_newtro_aesthetic
 *   human_readable: South Korean 'Newtro' Aesthetic Commercialization
 *   domain: social/economic/cultural
 *
 * SUMMARY:
 *   The South Korean 'Newtro' (New + Retro) trend represents a
 *   reinterpretation of traditional, vintage, and analog aesthetics by Gen Z
 *   and younger millennials. What began as subcultural experimentation —
 *   independent artisans restoring vintage ceramics, small vintage clothing
 *   shops, DIY analog photography communities — has been rapidly
 *   commercialized by major retail conglomerates, entertainment companies,
 *   and e-commerce platforms. This constraint exhibits structural tension
 *   between genuine aesthetic coordination (communities discovering and
 *   sharing vintage culture) and extractive commercialization (corporations
 *   capturing trend value without compensating creators). The theater_ratio
 *   (0.58) reflects that much 'newtro' marketing emphasizes cultural
 *   authenticity and heritage preservation while actually functioning as
 *   rapid trend extraction and artisan displacement. Independent artisans who
 *   pioneered newtro aesthetics find their innovations scaled and commodified
 *   within months by firms with faster supply chains. Gen Z participants in
 *   newtro subculture create trend-forecasting content, aesthetic labor, and
 *   network effects that accumulate value for platforms and corporate
 *   retailers while creators receive minimal compensation. The constraint
 *   exhibits all major classification types: a Snare for displaced artisans,
 *   a Tangled Rope for Gen Z aesthetic laborers (who both participate and are
 *   extracted from), Rope for coordinating retailers, a Scaffold with real
 *   sunset logic from emerging cooperative alternatives, and a Piton from
 *   legacy cultural institutions whose gatekeeping authority persists despite
 *   low functional relevance.
 *
 * KEY AGENTS:
 *   - Independent Artisans & Craftspeople: Primary victims (powerless/trapped) — original creators of newtro objects; face artisan displacement and rapid commodification
 *   - Gen Z Aesthetic Laborers: Secondary victims (moderate/constrained) — create trend-forecasting content and aesthetic labor; benefit from participation but extracted from by platforms/brands
 *   - Corporate Retail Conglomerates (Lotte, Samsung C&C, SSSG): Primary beneficiaries (institutional/arbitrage) — capture trend value through scaling and supply-chain efficiency
 *   - Entertainment Conglomerates (SM Entertainment, YG Entertainment, HYBE): Mixed institutional actors (organized/constrained) — coordinate talent/production but enforce aesthetic IP monopolies
 *   - E-commerce Platforms (Coupang, Naver, Kakao): Secondary beneficiaries (institutional/arbitrage) — extract value through trend-forecasting algorithms and creator data
 *   - Cultural Rights Coalitions & Cooperatives: Organized agents building alternatives (organized/mobile) — peer-to-peer vintage markets, artist collectives, cultural cooperatives
 *   - Legacy Cultural Institutions (National Heritage Administration, Korean Museum Association): Gatekeepers with declining enforcement (institutional/arbitrage) — maintain bureaucratic roles through inertia despite low market impact
 *   - Analytical Observer: Civilizational risk of naturalizing contingent commercial dynamics as inherent to consumer culture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sk_newtro_aesthetic, 0.52).
domain_priors:suppression_score(sk_newtro_aesthetic, 0.48).
domain_priors:theater_ratio(sk_newtro_aesthetic, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sk_newtro_aesthetic, extractiveness, 0.52).
narrative_ontology:constraint_metric(sk_newtro_aesthetic, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sk_newtro_aesthetic, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sk_newtro_aesthetic, tangled_rope).
narrative_ontology:human_readable(sk_newtro_aesthetic, "South Korean 'Newtro' Aesthetic Commercialization").
narrative_ontology:topic_domain(sk_newtro_aesthetic, "social/economic/cultural").

domain_priors:requires_active_enforcement(sk_newtro_aesthetic).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sk_newtro_aesthetic, corporate_retail_brands).
narrative_ontology:constraint_beneficiary(sk_newtro_aesthetic, entertainment_conglomerates).
narrative_ontology:constraint_beneficiary(sk_newtro_aesthetic, cultural_licensing_entities).
narrative_ontology:constraint_victim(sk_newtro_aesthetic, independent_artists_and_craftspeople).
narrative_ontology:constraint_victim(sk_newtro_aesthetic, cultural_authenticity_commons).
narrative_ontology:constraint_victim(sk_newtro_aesthetic, gen_z_aesthetic_labor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT ARTISAN (SNARE) — Original creators of newtro objects (vintage restoration, handmade ceramics, analog craft) cannot exit the commercialization machine. Their aesthetic innovations are copied by conglomerates with faster supply chains and lower costs. Career path disappears as 'newtro' becomes a corporate commodity. d≈0.92, f(d)≈1.39, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(sk_newtro_aesthetic, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: GEN Z AESTHETIC LABORER (TANGLED ROPE) — Young people participate voluntarily in newtro subculture: creating content, collecting vintage items, wearing the aesthetic. They benefit from community, self-expression, and cultural participation. But platforms and brands extract value from their aesthetic labor, data, and trend-forecasting work without compensation. Constrained exit: social belonging and cultural identity tied to participation. d≈0.68, f(d)≈1.02, σ=1.0 → χ≈0.53.
constraint_indexing:constraint_classification(sk_newtro_aesthetic, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CORPORATE RETAIL CONGLOMERATE (ROPE) — Benefits from coordination of supply chains, brand licensing, and trend acceleration. Experiences the constraint as pure coordination: identifying emerging aesthetics and scaling them serves the market demand. Arbitrage exit available — can shift to different aesthetic trends. d≈0.10, f(d)≈-0.05, σ=1.0 → χ≈-0.03. Net beneficiary.
constraint_indexing:constraint_classification(sk_newtro_aesthetic, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: CULTURAL RIGHTS COALITION (SCAFFOLD) — Organized groups (independent designer collectives, cultural commons advocates, gen Z activist communities) see newtro commercialization as a temporary extractive phase. They are building alternative pathways: peer-to-peer vintage markets (carrot market, thrift unions), artist collective licensing, cultural cooperatives. Sunset logic: as Gen Z gains collective agency and platform literacy, direct creator-to-consumer sales and cooperative ownership models replace corporate intermediation. d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.25. Theater ≤0.70 satisfies scaffold gate.
constraint_indexing:constraint_classification(sk_newtro_aesthetic, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGACY CULTURAL INSTITUTIONS (PITON) — Museums, government cultural agencies, and academic folklore departments maintain bureaucratic gatekeeping roles in newtro discourse but lack real enforcement capacity. Their authority to 'validate' cultural authenticity persists through institutional inertia despite declining relevance. Performance of guardianship (heritage certification, cultural seminars) has high theater (0.58) with minimal functional impact on actual newtro market dynamics. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.01.
constraint_indexing:constraint_classification(sk_newtro_aesthetic, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ENTERTAINMENT CONGLOMERATES (TANGLED ROPE) — K-pop and entertainment companies both benefit from and enforce newtro aesthetics. They coordinate talent, music production, and idol styling (genuine coordination function: solving complex multi-stakeholder alignment). But they extract through monopoly control of aesthetic trends and IP, enforcing through contract restrictions on artist participation in independent newtro projects. Constrained exit for artists within these firms. d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.18. Lower extraction than independent conglomerates because coordination function is genuine (talent development, production quality), but extraction component is real (IP monopoly, aesthetic gatekeeping).
constraint_indexing:constraint_classification(sk_newtro_aesthetic, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal perspective, aesthetic commodification is an inevitable feature of consumer capitalism: any successful aesthetic will eventually be commercialized, extraction is inherent to market dynamics, and artisan displacement is a law-like consequence of industrial production. However, base metrics (ε=0.52, suppression=0.48, theater=0.58) contradict mountain classification — this is a false summit. The structural contingency (enforcement through contract and IP law, not physics) reveals this as a naturalizing narrative masking policy choices.
constraint_indexing:constraint_classification(sk_newtro_aesthetic, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sk_newtro_aesthetic_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sk_newtro_aesthetic, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sk_newtro_aesthetic, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sk_newtro_aesthetic, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sk_newtro_aesthetic, TR),
    TR >= 0.70.

:- end_tests(sk_newtro_aesthetic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The newtro trend has measurable economic extraction: independent artisans lose market share as corporate retailers capture demand; Gen Z creates trend-forecasting labor that platforms monetize without compensation; vintage aesthetic innovations are copied at industrial scale within 6-12 months. However, extraction is not as severe as pure Snare (0.70+) because some artisans successfully operate in niche segments, and some Gen Z participate voluntarily in communities that reinvest aesthetic value. The constraint is not a simple transfer of all value to extractors; it is redistribution toward corporate intermediaries. Suppression (0.48): Moderate. Barriers to artisan participation include manufacturing cost scaling disadvantages, marketing budget gaps, intellectual property enforcement by larger firms, and platform algorithm bias toward established retailers. But suppression is not total — independent creators can still access vintage markets, DIY manufacturing platforms, and direct-to-consumer sales channels (emerging Scaffold alternatives). Theater ratio (0.58): Moderate. Newtro marketing emphasizes cultural authenticity, heritage preservation, and 'authentic discovery' of traditional Korean aesthetics while functioning primarily as rapid trend extraction. The performative aspect has increased from 0.35 (2020-21, genuinely subcultural) to 0.58 (2025-26, corporate pastiche commodification). Corporate brands perform cultural stewardship while engaging in exploitation, creating a theater-to-function gap.
 *
 * PERSPECTIVAL GAP:
 *   The independent artisan and Gen Z aesthetic laborer perceive primarily Snare/Tangled Rope (high extraction, suppression, no exit). Corporate retailers perceive Rope (coordination of supply/demand, value creation). The organized coalition sees Scaffold (temporary extraction with sunset pathway as cooperatives mature). Legacy cultural institutions maintain Piton (degraded authority, theatrical validation). The civilizational observer risks seeing Mountain (aesthetic commodification as inevitable law of consumer capitalism) — but the structural data contradicts this, revealing contingent institutional arrangements (IP law, contract enforcement, platform algorithms) that could be redesigned. The perspectival gap reveals that newtro is not 'naturally' commercializing; it is being extracted and bounded by policy choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Independent Artisans: Victims + trapped → d≈0.92, f(d)≈1.39. Maximum extraction — no exit pathway. Gen Z Aesthetic Laborers: Victims + constrained → d≈0.68, f(d)≈1.02. Significant extraction but with some voluntary participation (community, identity). Corporate Retailers: Beneficiaries + arbitrage → d≈0.10, f(d)≈-0.05. Net beneficiary — can shift to other trends. Entertainment Conglomerates: Mixed (beneficiaries via monopoly, but constrained by artist contracts/competition) → d≈0.35, f(d)≈0.35. Moderate extraction because genuine coordination function coexists with IP enforcement. Cultural Coalitions: Organized + mobile → d≈0.45, f(d)≈0.48. Low effective extraction because these agents have agency and see exit pathways. Legacy Institutions: Arbitrage + institutional → d≈0.08, f(d)≈-0.10. Effectively non-extractive due to declining relevance.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint qualifies as Tangled Rope (not pure Snare) because it has genuine coordination functions: entertainment conglomerates coordinate complex multi-stakeholder production (talent, music, styling); platforms coordinate trend discovery and consumer access; retail conglomerates coordinate supply chains and market distribution. These are real coordination services. Simultaneously, asymmetric extraction occurs: artisans lose market position, Gen Z performs unpaid labor, independent creators are displaced. The mandatrophy is resolved by recognizing that newtro commercialization is structurally hybrid — it solves real coordination problems (scaling authentic aesthetic goods to consumer demand) while extracting value from creators. This is different from a Snare (pure extraction, no coordination benefit) and different from a pure Rope (coordination with balanced benefit distribution). The Tangled Rope classification indicates that reform should focus on rebalancing beneficiary distribution (artist royalties, platform compensation, cooperative alternatives) rather than eliminating the coordination mechanism entirely. The Scaffold perspective shows a real sunset pathway: as Gen Z develops digital literacy and organizational capacity, peer-to-peer and cooperative models can replace corporate intermediation over 10-15 years, making the current extraction phase temporary rather than structural.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authentic_newtro_definition,
    'What constitutes ''authentic'' newtro versus corporate pastiche? Is the distinction structural or merely subjective generational preference?',
    'Ethnographic analysis of Gen Z aesthetic judgment criteria; longitudinal tracking of which newtro objects command secondary market premiums; comparative study of artist-created vs corporate-created newtro across price/desirability metrics',
    'If structural authenticity distinction exists and measurable: artisan market can be protected via certification/licensing models (Scaffold strengthens). If purely subjective: authenticity claims are performative and extraction mechanism is cultural authority, not scarcity (constraint becomes Snare for all victims).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authentic_newtro_definition, empirical, 'Whether authentic newtro can be distinguished from corporate pastiche by measurable criteria').

omega_variable(
    platform_value_extraction_mechanism,
    'Do platforms (Instagram, TikTok, YouTube) extract more value from newtro aesthetic labor than corporate retailers extract from supply-chain scaling?',
    'Comparative analysis of user-generated content monetization rates; tracking of trend adoption timelines from platform emergence to retail scaling; survey of content creators'' perception of compensation fairness',
    'If platforms extract more: the primary victim is aesthetic laborer, not artisan (shifts Tangled Rope focus). If retailers extract more: artisan displacement remains primary constraint. May reveal multiple decomposed constraints (newtro_platform_labor vs newtro_artisan_displacement).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(platform_value_extraction_mechanism, empirical, 'Comparative value extraction: platforms vs retailers in newtro commercialization').

omega_variable(
    generational_exit_capacity_threshold,
    'At what level of collective Gen Z digital literacy and platform alternative-building does the Scaffold perspective''s sunset become inevitable rather than aspirational?',
    'Tracking of peer-to-peer newtro platforms (carrot market, thrift collectives); measurement of direct creator-to-consumer sales volume; analysis of Gen Z organizational capacity in independent designer collectives over 5-10 year horizon',
    'If threshold reached by 2029: scaffold timeline is accurate and constraint lifetime ~3-5 years. If threshold delayed past 2032: scaffold is aspirational and conglomerates maintain extraction capability longer. Determines whether current Tangled Rope classification remains or should be reclassified as durable Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(generational_exit_capacity_threshold, empirical, 'Timeline for Gen Z collective agency to make scaffold sunset inevitable').

omega_variable(
    cultural_commons_degradation_measurement,
    'Does newtro commercialization reduce the actual diversity of aesthetic innovation available to Gen Z, or does it merely redistribute attention toward commercial products?',
    'Comparative analysis of newtro aesthetic diversity pre-2020 vs post-2023; tracking of niche aesthetic communities that remain outside commercialization; measurement of aesthetic innovation rates in independent communities vs corporate R&D',
    'If diversity genuinely reduced: extraction mechanism is cultural impoverishment (stronger Snare case for cultural_authenticity_commons victim). If attention merely redistributed: constraint is primarily value extraction from creators, not ecosystem damage (Tangled Rope focus on labor, not commons).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_commons_degradation_measurement, empirical, 'Whether newtro commercialization reduces aesthetic diversity or redistributes attention').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sk_newtro_aesthetic, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(newt_tr_t0, sk_newtro_aesthetic, theater_ratio, 0, 0.35).
narrative_ontology:measurement(newt_tr_t3, sk_newtro_aesthetic, theater_ratio, 3, 0.48).
narrative_ontology:measurement(newt_tr_t6, sk_newtro_aesthetic, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(newt_be_t0, sk_newtro_aesthetic, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(newt_be_t3, sk_newtro_aesthetic, base_extractiveness, 3, 0.42).
narrative_ontology:measurement(newt_be_t6, sk_newtro_aesthetic, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sk_newtro_aesthetic, resource_allocation).
narrative_ontology:affects_constraint(sk_newtro_aesthetic, korean_cultural_appropriation_discourse).
narrative_ontology:affects_constraint(sk_newtro_aesthetic, gen_z_platform_labor_extraction).
narrative_ontology:affects_constraint(sk_newtro_aesthetic, artisan_market_displacement_global).

% DUAL FORMULATION NOTE:
% The newtro aesthetic constraint decomposes into multiple structurally distinct constraints: (1) newtro_artisan_displacement (ε≈0.65, primarily Snare for independent creators), (2) newtro_platform_labor (ε≈0.48, primarily Tangled Rope for Gen Z content creators), (3) newtro_entertainment_monopoly (ε≈0.42, Tangled Rope for idol artists). This story addresses the aggregate phenomenon; decomposition into domain-specific stories may be warranted as the constraint develops.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sk_newtro_aesthetic, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
