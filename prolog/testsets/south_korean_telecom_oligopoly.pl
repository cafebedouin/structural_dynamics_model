% ============================================================================
% CONSTRAINT STORY: south_korean_telecom_oligopoly
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_south_korean_telecom_oligopoly, []).

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
 *   constraint_id: south_korean_telecom_oligopoly
 *   human_readable: South Korean Telecom Oligopoly Market Structure
 *   domain: economic/telecommunications/regulatory
 *
 * SUMMARY:
 *   South Korea's telecom market has been dominated by three carriers—SK
 *   Telecom, KT Corporation, and LGU+—for over two decades, creating a stable
 *   oligopoly with high barriers to entry and limited competitive pressure.
 *   This constraint exhibits the full range of Deferential Realism
 *   classifications from different perspectives: rural subscribers experience
 *   it as a Snare (trapped with no exit); urban consumers as Tangled Rope
 *   (constrained but benefiting from infrastructure); regulators as Tangled
 *   Rope (coordinating network investment while captured by industry
 *   lobbying); carriers as Rope (perceiving genuine coordination need);
 *   startups as Tangled Rope (mobile but gatekept); and the analytical
 *   observer risks naturalizing it as a Mountain (network economics
 *   immutability) when it is actually a contingent institutional arrangement
 *   maintained by specific regulatory choices. The constraint's
 *   extractiveness has increased from 0.42 to 0.58 over the measurement
 *   interval (years 0-10) while theater has risen from 0.38 to 0.55,
 *   indicating regulatory capture intensifying (theater rising without
 *   extractiveness plateau suggests cover-story deepening rather than
 *   coordination improvement). The Tangled Rope classification reflects
 *   genuine coordination benefits (5G universal deployment, network
 *   reliability) coexisting with sustained price extraction and competitive
 *   gatekeeping.
 *
 * KEY AGENTS:
 *   - SK Telecom, KT Corporation, LGU+: Primary beneficiaries (institutional/arbitrage) — extract pricing power through oligopoly, coordinate infrastructure investment, benefit from regulatory forbearance on merger enforcement
 *   - Rural Subscribers: Primary victims (powerless/trapped) — face high prices with no exit option; geographic isolation makes alternatives unavailable
 *   - Low-Income Consumers: Primary victims (powerless/trapped) — face disconnection risk due to service unaffordability; no cheaper alternatives due to oligopoly pricing floor
 *   - MVNO Operators: Secondary victims (moderate/constrained) — infrastructure dependence on carriers creates wholesale rate extraction; regulatory protections are weak
 *   - Technology Startups: Secondary victims (powerful/mobile) — face content gatekeeping through carrier zero-rating preferences and device subsidy favoritism; can exit but lose market access
 *   - Korean Communications Commission Regulator: Organized actor (organized/constrained) — coordinates spectrum allocation and universal service while captured by carrier lobbying; has constrained agency to restructure market
 *   - Telecommunications Industry Association: Institutional performer (institutional/arbitrage) — maintains performative coordination rituals that mask price coordination; theater persists through inertia
 *   - ASEAN Regional Competitors: Organized external actor (organized/constrained) — provides sunset mechanism through regional integration creating competitive pressure; sunset timeline is uncertain (5-20 years)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(south_korean_telecom_oligopoly, 0.58).
domain_priors:suppression_score(south_korean_telecom_oligopoly, 0.65).
domain_priors:theater_ratio(south_korean_telecom_oligopoly, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(south_korean_telecom_oligopoly, extractiveness, 0.58).
narrative_ontology:constraint_metric(south_korean_telecom_oligopoly, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(south_korean_telecom_oligopoly, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(south_korean_telecom_oligopoly, tangled_rope).
narrative_ontology:human_readable(south_korean_telecom_oligopoly, "South Korean Telecom Oligopoly Market Structure").
narrative_ontology:topic_domain(south_korean_telecom_oligopoly, "economic/telecommunications/regulatory").

domain_priors:requires_active_enforcement(south_korean_telecom_oligopoly).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(south_korean_telecom_oligopoly, sk_telecom).
narrative_ontology:constraint_beneficiary(south_korean_telecom_oligopoly, kt_corporation).
narrative_ontology:constraint_beneficiary(south_korean_telecom_oligopoly, lgu_plus).
narrative_ontology:constraint_beneficiary(south_korean_telecom_oligopoly, infrastructure_investment_coordination).
narrative_ontology:constraint_victim(south_korean_telecom_oligopoly, rural_subscribers).
narrative_ontology:constraint_victim(south_korean_telecom_oligopoly, low_income_consumers).
narrative_ontology:constraint_victim(south_korean_telecom_oligopoly, competitive_mvno_operators).
narrative_ontology:constraint_victim(south_korean_telecom_oligopoly, innovation_startups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL SUBSCRIBER (SNARE) — Trapped with no exit. In rural South Korea, the three carriers control 99% of market share. A subscriber cannot switch to an alternative provider; exit costs are insurmountable (no service elsewhere, mandatory long-term contracts, early termination penalties). Bears full extraction through price premiums and reduced service quality in low-population areas. Maximum suppression: geographic isolation makes this agent vulnerable.
constraint_indexing:constraint_classification(south_korean_telecom_oligopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: URBAN CONSUMER (TANGLED ROPE) — Constrained but not trapped. Has theoretical choice between three carriers, but genuine switching costs are high (contract lock-in, account migration friction, carrier-specific device subsidies). Also benefits from infrastructure investment that the oligopoly coordinates — widespread 5G rollout, network reliability, universal service obligation. Experiences mixed coordination (network investment) and extraction (price fixing, zero-rating practices that benefit video platforms owned by same conglomerates).
constraint_indexing:constraint_classification(south_korean_telecom_oligopoly, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LOW-INCOME SUBSCRIBER (SNARE) — Trapped by necessity rather than geography. Cannot afford market prices; relies on minimum-data plans. No exit to cheaper alternatives (no viable MVNOs due to infrastructure barriers). Service disconnection is involuntary consequence of price extraction. Bears maximum structural extraction; suppression through poverty.
constraint_indexing:constraint_classification(south_korean_telecom_oligopoly, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: MVNO OPERATOR (SNARE) — Constrained by mandatory infrastructure rental agreements. Must lease capacity from the big three at regulated wholesale rates, but regulations are set to benefit incumbents. Cannot compete on price or network quality because wholesale costs are high and service degradation is permitted. Exit is possible but costly (abandoning customer base). Experiences extraction through infrastructure rent extraction and regulatory capture.
constraint_indexing:constraint_classification(south_korean_telecom_oligopoly, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: TELECOM CARRIERS (ROPE) — Experience the oligopoly as coordination mechanism that enables profitable infrastructure investment. Argue that 5G rollout, network reliability, and universal service require coordinated capacity planning and shared spectrum allocation. Benefit from regulatory forbearance on pricing (soft price-fixing through industry association 'guidelines' rather than formal cartel). Exit through real competition would require network duplication (inefficient) or regulatory dissolution (politically infeasible). Perceive constraint as natural outcome of telecommunications economics.
constraint_indexing:constraint_classification(south_korean_telecom_oligopoly, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: REGULATOR (TANGLED ROPE) — Organized but constrained by political economy. Coordinates spectrum allocation and universal service obligations; sets wholesale rates and consumer protections. Also extracted from by carriers through regulatory capture: industry lobbying, revolving-door employment, reliance on carrier cooperation for national infrastructure projects. Experiences the oligopoly as both coordination tool (for universal access) and extraction mechanism (price regulation is weak; merger reviews routinely approved). Has constrained agency — can adjust wholesale rates marginally but cannot fundamentally restructure the market without carrier retaliation (service disruption, spectrum cooperation withholding).
constraint_indexing:constraint_classification(south_korean_telecom_oligopoly, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: TIA INDUSTRY ASSOCIATION (PITON) — Maintains performative coordination rituals (industry guidelines, collective standards-setting) that no longer serve their stated function. Originally coordinated genuine infrastructure challenges (spectrum allocation, tower-sharing); now primarily theater masking price coordination. Association persists through institutional inertia and regulatory normalization, but member incentives now diverge (each carrier benefits more from breaking cartel than maintaining it). Theater ratio is high: association meetings, technical committees, and standards bodies function more as coordinating entities for anti-competitive behavior than as genuine technical coordination.
constraint_indexing:constraint_classification(south_korean_telecom_oligopoly, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: TECH STARTUPS (TANGLED ROPE) — Mobile but with significant constraints. Can start new companies and in principle move abroad, but domestic Korean market access is blocked by carrier gatekeeping (content delivery networks, zero-rating preferences, device subsidies tied to carrier relationships). Benefits from infrastructure investment (cheap, fast data enables business models). Extracted from through carrier preferential treatment of owned content platforms (e.g., SK Telecom's WAVVE video service receiving zero-rating while competitors do not). Moderate power but constrained by market gatekeeping.
constraint_indexing:constraint_classification(south_korean_telecom_oligopoly, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 9: REGIONAL COMPETITION (SCAFFOLD) — Organized actors (Vietnam's Viettel, Indonesia's Telkomsel, Thailand's AIS) are building competing infrastructure and cheaper service models. Provides sunset mechanism for SK oligopoly: as Korean consumers gain access to regional service options (ASEAN 5G roaming, cross-border apps), the domestic carriers' pricing power erodes. Has sunset clause: 10-15 year timeline as regional integration matures. South Korean oligopoly extraction becomes unsustainable when subscribers can arbitrage to cheaper regional providers.
constraint_indexing:constraint_classification(south_korean_telecom_oligopoly, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 10: ANALYTICAL OBSERVER (MOUNTAIN) — From universal/civilizational scope, network effects and infrastructure economies of scale create natural monopoly tendencies in telecommunications. Duplication of networks is inherently inefficient; 3-carrier oligopoly may be the natural equilibrium that telecommunications markets converge to. This perspective naturalizes the oligopoly as immutable law of network economics. However, the structural data contradicts this: Korea's oligopoly is maintained through specific regulatory choices (spectrum allocation favoritism, merger approval patterns, wholesale rate-setting), not physics. The 'natural law' framing obscures contingent policy.
constraint_indexing:constraint_classification(south_korean_telecom_oligopoly, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(south_korean_telecom_oligopoly_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(south_korean_telecom_oligopoly, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(south_korean_telecom_oligopoly, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(south_korean_telecom_oligopoly, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(south_korean_telecom_oligopoly, TR),
    TR >= 0.70.

:- end_tests(south_korean_telecom_oligopoly_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The oligopoly enables significant price extraction from trapped and constrained subscribers (0.42-0.58 increase over interval indicates growing extraction capacity). However, extractiveness is not at Snare levels (0.66+) because genuine coordination benefits exist (5G rollout, network reliability, universal service obligation are real). The increasing trajectory reflects regulatory capture intensifying over time — not fundamental market dynamics worsening, but political economy degrading. Suppression (0.65): High. Structural barriers to exit are significant: geographic isolation (rural), economic necessity (low-income), high switching costs (contract lock-in), and infrastructure dependence (MVNOs). Regulatory protections are weak (wholesale rates permit high margins, merger reviews routinely approve consolidation). Theater ratio (0.55): Moderate-high. Industry association standards-setting, technical committees, and regulatory consultation processes perform coordination functions that are increasingly theatrical — the real coordination (spectrum allocation, infrastructure sharing) is negotiated privately by carriers outside formal channels. The theater has increased (0.38 to 0.55) as regulatory capture deepened, suggesting performative legitimization is now primary function.
 *
 * PERSPECTIVAL GAP:
 *   Carriers perceive Rope because they genuinely experience the constraint as coordination problem (spectrum sharing, network reliability, universal service require cooperative capacity planning). Their arbitrage exit option (can shift to other countries' markets, can threaten regulatory non-cooperation) gives them power to reshape the constraint. Trapped/constrained victims perceive Snare or Tangled Rope because they bear extraction costs without offsetting coordination benefit. The gap reveals the constraint's core asymmetry: coordination benefits accrue to institutional actors (carriers, regulator) while extraction costs fall on dispersed individuals (subscribers, startups). The mountain perspective (naturalizing oligopoly as immutable) is a false summit that the structural data contradicts — the oligopoly is maintained by regulatory choices, not physics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values vary sharply across perspectives based on beneficiary/victim status and exit options. Carriers (beneficiary + arbitrage) derive low d (~0.15), experiencing negative effective extraction (they benefit). Rural subscribers (victim + trapped) derive high d (~0.95), experiencing maximum extraction. Urban consumers (mixed: beneficiary in coordination + victim in pricing, constrained) derive moderate d (~0.60-0.65), experiencing moderate-to-high extraction tempered by coordination benefit. Regulators (victim of capture + constrained) derive higher d (~0.65-0.70) than beneficiary status alone would suggest because capture constrains their exit. MVNOs (victim + constrained) derive high d (~0.80-0.85) because infrastructure dependence is near-entrapment. The divergence in d values (from 0.15 to 0.95) is the engine that produces the perspectival gap — the same constraint has fundamentally different extraction profiles depending on structural position. No single power atom or time horizon could capture this diversity; the full tuple is necessary.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandatrophy is resolved through the Tangled Rope classification, which correctly captures both the genuine coordination function (5G deployment, network reliability, universal service) and the sustained asymmetric extraction (pricing power, competitive gatekeeping, regulatory capture). The core tension is not whether coordination or extraction dominates, but how the two coexist in a single institutional arrangement. The increasing extractiveness (0.42 to 0.58) and rising theater (0.38 to 0.55) suggest that the coordination justification is degrading over time while extraction mechanisms intensify — the boundary between Tangled Rope and Snare is moving toward Snare as genuine coordination function weakens and performative legitimation strengthens. The mandatrophy analysis identifies the critical question: can the oligopoly deliver equivalent coordination benefits (infrastructure investment, universal service) through competitive markets with regulated access, or does the market structure genuinely require oligopoly? International comparisons (US, EU, Japan competitive markets with equivalent or superior infrastructure) suggest the former — which would reclassify the constraint from Tangled Rope toward Snare. The classification is contingent on resolving this empirical question.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infrastructure_investment_necessity,
    'Is oligopoly market structure genuinely necessary for coordinating infrastructure investment (5G rollout, universal service), or would competitive markets with regulated access achieve equivalent outcomes at lower extraction cost?',
    'International comparison: infrastructure investment levels in competitive vs oligopoly telecom markets (US, EU, Japan vs SK, Korea); analysis of CAPEX per subscriber and coverage parity',
    'If oligopoly necessary: Tangled Rope classification is correct; extraction is justified by coordination benefit. If competitive markets achieve parity: Oligopoly is pure extraction (Snare); coordination function is cover story.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(infrastructure_investment_necessity, empirical, 'Whether oligopoly is necessary for infrastructure investment coordination').

omega_variable(
    regulatory_capture_mechanism,
    'How much of the oligopoly''s pricing power derives from carrier market dominance vs from regulatory capture (merger approval patterns, weak wholesale rate enforcement, compliant regulator)?',
    'Counterfactual analysis: wholesale rate history and regulator decision patterns before/after carrier lobbying campaigns; revolving-door employment tracking; comparative regulatory stringency across OECD telecom markets',
    'If primarily regulatory: oligopoly is politically contingent (Tangled Rope with high extractiveness). If primarily market structure: oligopoly is economically determined (boundary between Rope and Snare depends on exit options available).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, empirical, 'Relative contribution of market structure vs regulatory capture to pricing power').

omega_variable(
    mvno_viability_threshold,
    'What wholesale rate level would enable viable MVNO competition and reduce oligopoly extractiveness below 0.50?',
    'Business model analysis: comparison of MVNO margin requirements in SK vs EU; testing competitive entry at different wholesale rate levels; correlation between wholesale rates and market concentration across OECD countries',
    'If viability threshold is politically achievable (e.g., <5% margin requirement): regulatory reform could dissolve oligopoly without market restructuring. If threshold requires uneconomic wholesale rates: oligopoly structure is hard constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mvno_viability_threshold, empirical, 'Wholesale rate threshold for viable MVNO competition').

omega_variable(
    rural_service_cross_subsidy,
    'How much of rural service cost is genuinely uneconomic (cross-subsidy from urban profitability) vs extractive capacity that carriers claim is subsidy to justify high urban prices?',
    'Cost accounting analysis: actual rural deployment costs vs reported subsidy amounts; comparison to other countries'' rural service cost structures; analysis of carrier profitability in rural vs urban segments',
    'If cross-subsidy is genuine: oligopoly is partly justified by universal service coordination (Tangled Rope). If rural cost claims are inflated: oligopoly is used to justify extraction unrelated to rural service (higher Snare component).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rural_service_cross_subsidy, empirical, 'Genuine cross-subsidy cost for rural service provision').

omega_variable(
    regional_asean_sunset_timeline,
    'What is the realistic timeline for ASEAN regional integration to create viable substitutes for Korean domestic telecom service?',
    'Infrastructure development tracking: ASEAN 5G rollout schedules, roaming agreement timelines, regulatory harmonization progress; analysis of cross-border service adoption in mature ASEAN corridors',
    'If sunset timeline is 5-10 years: Scaffold classification is accurate and extractiveness will decline naturally. If timeline is 20+ years or indefinite: Scaffold classification is aspirational; oligopoly persists structurally.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regional_asean_sunset_timeline, empirical, 'Timeline for ASEAN integration to create competitive pressure on Korean carriers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(south_korean_telecom_oligopoly, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sk_telecom_tr_t0, south_korean_telecom_oligopoly, theater_ratio, 0, 0.38).
narrative_ontology:measurement(sk_telecom_tr_t5, south_korean_telecom_oligopoly, theater_ratio, 5, 0.48).
narrative_ontology:measurement(sk_telecom_tr_t10, south_korean_telecom_oligopoly, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(sk_telecom_be_t0, south_korean_telecom_oligopoly, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(sk_telecom_be_t5, south_korean_telecom_oligopoly, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(sk_telecom_be_t10, south_korean_telecom_oligopoly, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(south_korean_telecom_oligopoly, resource_allocation).
narrative_ontology:affects_constraint(south_korean_telecom_oligopoly, korean_digital_divide).
narrative_ontology:affects_constraint(south_korean_telecom_oligopoly, content_platform_gatekeeping).
narrative_ontology:affects_constraint(south_korean_telecom_oligopoly, mvno_wholesale_pricing).

% DUAL FORMULATION NOTE:
% The South Korean telecom oligopoly decomposes into three structurally distinct constraints with different ε values: (1) spectrum_allocation_coordination (ε=0.15, Rope) — genuine coordination problem with low extraction, (2) price_extraction_oligopoly (ε=0.65, Snare) — pure extraction through market concentration, (3) regulatory_capture (ε=0.52, Tangled Rope) — mixed coordination and extraction through regulator dependence. This story aggregates all three; decomposition into separate stories would clarify which component drives the oligopoly's persistence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(south_korean_telecom_oligopoly, organized, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
