% ============================================================================
% CONSTRAINT STORY: nfl_superbowl_marketing_regulation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nfl_superbowl_marketing_regulation, []).

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
 *   constraint_id: nfl_superbowl_marketing_regulation
 *   human_readable: NFL Super Bowl Advertising Regulations
 *   domain: economic/media_entertainment
 *
 * SUMMARY:
 *   The NFL's Super Bowl advertising regulation system represents a
 *   high-extraction constraint that operates at the intersection of media
 *   monopoly, intellectual property enforcement, and sports league market
 *   power. The constraint involves the NFL league office (and its official
 *   broadcast partners) exerting unilateral control over advertising access,
 *   rates, and competitive scope during the single largest annual television
 *   event (100M+ viewers). Non-official advertisers and competing brands face
 *   active suppression through trademark enforcement, broadcast exclusions,
 *   and rate-setting that extracts substantial rent. The system exhibits
 *   snare characteristics from the perspective of price-sensitive advertisers
 *   and excluded competitors: suppression mechanisms prevent exit options,
 *   monopoly pricing extracts maximum rent, and the constraint's maintenance
 *   relies on the NFL's broadcast dominance and trademark enforcement power.
 *   However, the constraint is degrading as digital platforms create
 *   alternative prestige advertising venues, suggesting scaffold or piton
 *   dynamics in longer time horizons.
 *
 * KEY AGENTS:
 *   - NFL League Office: Primary beneficiary (institutional/arbitrage) — sets rates, enforces rules, captures monopoly rents through advertising premiums
 *   - Official Broadcast Partners (CBS/NBC/FOX): Secondary beneficiary (institutional/arbitrage) — benefit from exclusive broadcast rights and higher inventory prices; experience constraint as enabling coordination
 *   - Price-Sensitive Advertisers: Primary victim (powerless/trapped) — face binary choice: pay monopoly prices or lose market visibility during peak viewership; cannot negotiate or exit
 *   - Competing Brands (Non-Official): Secondary victim (moderate/constrained) — actively excluded through trademark enforcement and broadcast restrictions; constrained exit options
 *   - Advertising Industry / Trade Associations: Mixed (organized/constrained) — benefit from predictable market structure but constrained by NFL's rate-setting and approval power
 *   - Digital Platform Coalition: Organized agents (organized/mobile) — building alternative prestige advertising venues; represent sunset pathway for NFL's monopoly
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nfl_superbowl_marketing_regulation, 0.62).
domain_priors:suppression_score(nfl_superbowl_marketing_regulation, 0.68).
domain_priors:theater_ratio(nfl_superbowl_marketing_regulation, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nfl_superbowl_marketing_regulation, extractiveness, 0.62).
narrative_ontology:constraint_metric(nfl_superbowl_marketing_regulation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nfl_superbowl_marketing_regulation, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nfl_superbowl_marketing_regulation, snare).
narrative_ontology:human_readable(nfl_superbowl_marketing_regulation, "NFL Super Bowl Advertising Regulations").
narrative_ontology:topic_domain(nfl_superbowl_marketing_regulation, "economic/media_entertainment").

domain_priors:requires_active_enforcement(nfl_superbowl_marketing_regulation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nfl_superbowl_marketing_regulation, nfl_league_office).
narrative_ontology:constraint_beneficiary(nfl_superbowl_marketing_regulation, official_broadcast_partners).
narrative_ontology:constraint_victim(nfl_superbowl_marketing_regulation, non_official_advertisers).
narrative_ontology:constraint_victim(nfl_superbowl_marketing_regulation, competing_brands).
narrative_ontology:constraint_victim(nfl_superbowl_marketing_regulation, price_sensitive_advertisers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRICE-SENSITIVE ADVERTISER (SNARE) — Mid-market brands without massive budgets face a binary: pay NFL's monopoly prices ($5-7M per 30-second spot) or have zero association with the event. Exit is illusory — declining to advertise means ceding market visibility during peak viewership. The constraint extracts maximum rent because the advertiser cannot negotiate, cannot find alternatives, and cannot exit without accepting competitive disadvantage. High suppression because the NFL controls all pathway mechanisms: broadcast rights, trademark enforcement, and exclusivity rules.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: COMPETING BRAND / NON-OFFICIAL (SNARE) — Brands that compete with official partners (e.g., Pepsi competitor wanting to advertise during Super Bowl broadcast) face active suppression: trademark enforcement actions, broadcast exclusions, trademark dilution claims. Exit options are constrained but not eliminated — they can advertise during non-Super Bowl events or other sports with lower prestige/reach. The extraction is severe because they bear the cost of being excluded from the single largest advertising opportunity while official partners enjoy protected monopoly pricing.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OFFICIAL BROADCAST PARTNER (ROPE) — Networks (currently CBS/NBC/FOX on rotation) benefit from exclusive broadcast rights and higher advertising inventory prices. The constraint is experienced as coordination: the NFL sets rules that stabilize the broadcast market, prevent chaos during the game, and guarantee high-quality signal transmission. Networks have arbitrage options — they can negotiate terms, invest in production quality, or exit to other properties. They perceive the constraint as enabling their business through orderly market structure, not as extraction. Effective extraction experienced here is minimal; they are beneficiaries of the coordination it enables.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ADVERTISING INDUSTRY / TRADE ASSOCIATIONS (TANGLED ROPE) — Professional advertising groups and media buying firms both benefit from and are constrained by the NFL's rules. They benefit from predictable placement windows, standardized rates, and the prestige of Super Bowl advertising creating premium product demand. They are constrained by the NFL's unilateral rate-setting, approval power over creative content, and enforcement of exclusivity rules that limit their client flexibility. Organized agents have some capacity to negotiate terms or develop alternative strategies (e.g., pre-game/post-game advertising, real-time bidding in non-Super-Bowl inventory), but cannot exit the Super Bowl market entirely without ceding significant revenue. Mixed coordination (makes the market more stable) and extraction (monopoly pricing).
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: NFL LEAGUE OFFICE (ROPE) — From the league's perspective, the advertising regulations are a coordination mechanism. They solve the collective action problem of how to manage massive audience attention during the Super Bowl broadcast, prevent conflicting ad claims, maintain broadcast signal quality, and create premium advertising inventory that generates $500M+ in annual Super Bowl advertising revenue. The NFL perceives the constraint as coordination that benefits all stakeholders by creating an orderly market. Suppression (68%) reflects enforcement: the league actively polices trademark violations, competitor exclusions, and content compliance. But suppression is not experienced as coercive by the beneficiary — it is framed as 'protecting the brand' and 'ensuring quality.' The constraint enables the NFL's dominant position; it is not extraction from the league's perspective.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: DIGITAL ALTERNATIVE PLATFORM COALITION (SCAFFOLD) — Emerging organized actors (streaming platforms, social media, YouTube, TikTok, Discord) are building alternative prestige advertising events with lower suppression and higher advertiser flexibility. Gaming tournaments (Esports), digital culture events, and social media influencer campaigns are creating competing visibility pathways. From the coalition's perspective, the NFL's advertising constraint is a temporary monopoly on prestige that is decaying. Exit barriers for premium brands are lowering as alternative platforms demonstrate comparable reach and engagement metrics. Sunset logic applies: as digital alternatives mature (estimated 5-15 years), the NFL's ability to sustain monopoly pricing will erode. The scaffold perspective sees the constraint as increasingly indefensible as market alternatives emerge.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (PITON) — From a long-term competitive market perspective, the NFL's advertising regulation is increasingly theatrical. The league enforces exclusivity rules, trademark restrictions, and rate-setting that made sense in broadcast monopoly conditions (1980s-2010s). But the underlying justification (scarcity of prestige advertising inventory) is eroding as digital alternatives proliferate. The performance of 'protecting brand integrity' persists through institutional inertia, legacy enforcement budget, and brand mythology ('Super Bowl is sacred'), but the functional necessity is declining. Theater ratio (35%) is moderate rather than high because the constraint does perform real enforcement — the NFL actively and effectively prevents competitor advertising during broadcast. But the long-term trend is degradation as market power declines.
constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nfl_superbowl_marketing_regulation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nfl_superbowl_marketing_regulation, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nfl_superbowl_marketing_regulation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(nfl_superbowl_marketing_regulation, TR),
    TR >= 0.70.

:- end_tests(nfl_superbowl_marketing_regulation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The NFL captures substantial monopoly rent through advertising rates that are 3-5x higher per viewer than regular-season inventory. However, the extraction is not maximal (approaching 0.75+) because the NFL must maintain the constraint's functionality — if advertisers systematically exit, the prestige diminishes and extraction declines. The measured value reflects the current equilibrium where most premium brands participate despite high cost, indicating the NFL is extracting near the revenue-maximizing point but below the demand-collapse threshold. Suppression (0.68): High. The NFL actively enforces exclusivity through multiple mechanisms: trademark policing (aggressive against competitor brands), broadcast access control (only official partners can air during game), content approval (creative vetting), and rate-setting with no negotiation. Barriers to exit are substantial for premium brands seeking peak visibility. Theater ratio (0.35): Low-moderate. The constraint performs real enforcement — the NFL successfully prevents competitor advertising and maintains advertising rate premiums. However, the functional necessity for this level of suppression is declining as digital alternatives emerge, creating theatrical elements (enforcement through legacy authority rather than market necessity). The ratio has increased over the 30-year interval as digital platforms have matured, reducing functional necessity while the NFL maintains enforcement intensity.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival gap between beneficiary and victim perspectives. The NFL league office (Rope perspective) experiences the advertising regulation as coordination that creates market stability, protects brand integrity, and enables efficient allocation of premium inventory. Official broadcast partners (Rope) see orderly market structure that increases their inventory value. Price-sensitive advertisers (Snare) experience the same constraint as extraction with no legitimate coordination benefit — they are forced to pay monopoly prices because the NFL controls the entire prestige market. Competing brands (Snare) experience active suppression through trademark enforcement that prevents them from competing. The digital platform coalition (Scaffold) sees the constraint as increasingly indefensible as market alternatives mature. The analytical observer (Piton) recognizes that the functional necessity for this level of suppression is declining — the constraint persists through institutional inertia and legacy enforcement authority, not because the current market structure requires it. No single agent perceives this constraint as purely coordination or purely extraction; the perspectival gap measures the asymmetry in how much each agent benefits versus bears costs.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position relative to the constraint. The price-sensitive advertiser (powerless/trapped) has d ≈ 0.92 — they bear maximum extraction and cannot exit. The competing brand (moderate/constrained) has d ≈ 0.78 — moderate power but severely constrained by trademark enforcement and broadcast exclusion. The official broadcast partner (institutional/arbitrage) has d ≈ 0.12 — they benefit from the constraint and can arbitrage to other properties; the constraint enables their business. The advertising industry association (organized/constrained) has d ≈ 0.48 — roughly balanced extraction and coordination benefit; they can sometimes negotiate but are ultimately constrained by NFL's power. The digital platform coalition (organized/mobile) has d ≈ 0.35 — they have exit options and are building alternative pathways; they experience less extraction because they can choose to compete elsewhere. The NFL league office (institutional/arbitrage) has d ≈ 0.05 — they are the beneficiary; the constraint transfers wealth toward them. These d values feed the sigmoid f(d) to produce experienced chi (effective extraction), which explains why agents with identical nominal power can experience different constraint severity based on exit options and beneficiary/victim status.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy between 'coordinating a prestige advertising market' (rope-level claim) and 'extracting monopoly rents from trapped advertisers' (snare-level claim) by recognizing that BOTH are structurally true but the distribution of coordination benefit versus extraction asymmetry is extreme. The NFL genuinely solves a coordination problem: without some regulation, the Super Bowl broadcast would face chaos (unauthorized advertising, conflicting claims, signal degradation). Official broadcast partners genuinely benefit from this coordination. But the solution chosen — monopoly rate-setting with active competitor suppression through trademark enforcement — extracts far more rent than the minimum necessary to achieve coordination. An alternative solution (say, competitive bidding with rate floors to ensure quality) would provide similar coordination benefit with lower extraction. The snare classification is justified because the NBA could shift rules to reduce extraction without losing coordination function, but chooses not to because extraction is the primary objective. The mandatrophy resolves by recognizing that beneficiaries (NFL, official partners) frame the constraint as 'necessary coordination' while victims (competing brands, price-sensitive advertisers) experience it as 'unnecessary extraction.' The truth is: coordination is needed, but at far lower extraction cost than currently imposed. Current extraction levels reflect monopoly power, not coordination necessity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    digital_platform_competitive_parity,
    'Will digital platforms achieve comparable audience reach and advertiser prestige equivalence to the NFL Super Bowl within 10 years?',
    'Longitudinal tracking of alternative platform advertising rates, audience demographics, brand perception studies comparing Super Bowl vs digital event prestige, advertiser spending allocation shifts across media channels',
    'If parity achieved: scaffold sunset becomes real and snare classification downgrades to piton/rope as exit options expand. If parity fails: snare classification persists with maintained suppression and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_platform_competitive_parity, empirical, 'Whether digital platforms will achieve prestige parity with NFL Super Bowl advertising').

omega_variable(
    monopoly_price_elasticity,
    'What is the true price elasticity of demand for Super Bowl advertising? At what price point do mid-market brands systematically exit the market?',
    'Analysis of advertiser participation rates at different price levels; comparison with historical pricing data; survey data on advertising ROI expectations for Super Bowl vs alternatives',
    'If elasticity is high (< -1.0): constraint is extracting near the revenue-maximizing point and could face demand collapse if enforcement costs rise. If elasticity is low: extraction can be tightened further; snare classification strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monopoly_price_elasticity, empirical, 'Price elasticity of demand for Super Bowl advertising inventory').

omega_variable(
    antitrust_enforcement_risk,
    'Does the NFL''s advertising regulation scheme constitute illegal monopoly maintenance or tying under antitrust law? What is the probability of FTC/DOJ enforcement action?',
    'Legal analysis of past antitrust cases against sports leagues (NFL stadium funding, broadcast exclusivity); assessment of advertiser harm documentation; FTC/DOJ enforcement priorities shifts; competitive impact studies',
    'If enforcement occurs: snare classification would degrade as suppression enforcement becomes legally risky and costly; exit options expand for advertisers. If enforcement does not occur: snare maintains high suppression and extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(antitrust_enforcement_risk, empirical, 'Probability and impact of antitrust enforcement against NFL advertising regulations').

omega_variable(
    sports_league_revenue_dependency,
    'How dependent is the NFL''s overall revenue model on Super Bowl advertising rent extraction? What share of league revenue comes from Super Bowl advertising premiums?',
    'Financial analysis of NFL revenue streams; comparison of Super Bowl advertising revenue to regular-season inventory; assessment of whether constraint can be relaxed without material revenue impact',
    'If high dependency (>10% of league revenue): constraint will be defended aggressively and unlikely to be relaxed voluntarily. If low dependency: constraint might be relaxed as brand-building exercise or antitrust defense.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sports_league_revenue_dependency, empirical, 'Revenue dependency on Super Bowl advertising monopoly extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nfl_superbowl_marketing_regulation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sbowl_tr_t0, nfl_superbowl_marketing_regulation, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sbowl_tr_t15, nfl_superbowl_marketing_regulation, theater_ratio, 15, 0.3).
narrative_ontology:measurement(sbowl_tr_t30, nfl_superbowl_marketing_regulation, theater_ratio, 30, 0.35).

% Extraction over time
narrative_ontology:measurement(sbowl_be_t0, nfl_superbowl_marketing_regulation, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(sbowl_be_t15, nfl_superbowl_marketing_regulation, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(sbowl_be_t30, nfl_superbowl_marketing_regulation, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nfl_superbowl_marketing_regulation, global_infrastructure).
narrative_ontology:affects_constraint(nfl_superbowl_marketing_regulation, sports_broadcast_exclusivity_regimes).
narrative_ontology:affects_constraint(nfl_superbowl_marketing_regulation, trademark_enforcement_scope_creep).
narrative_ontology:affects_constraint(nfl_superbowl_marketing_regulation, premium_event_prestige_monopoly).

% DUAL FORMULATION NOTE:
% The NFL Super Bowl advertising regulation is upstream of broader sports league media monopolies and trademark enforcement patterns. It represents a canonical case of how market dominance in one domain (broadcast sports) enables rent extraction in adjacent domains (advertising). The constraint's decomposition distinguishes between coordination functions (necessary: preventing chaos in prestige advertising market) and extraction mechanisms (contingent: monopoly pricing and competitor exclusion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nfl_superbowl_marketing_regulation, organized, 0.48).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
