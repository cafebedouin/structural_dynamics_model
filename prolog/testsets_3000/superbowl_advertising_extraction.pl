% ============================================================================
% CONSTRAINT STORY: superbowl_advertising_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_superbowl_advertising_extraction, []).

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
 *   constraint_id: superbowl_advertising_extraction
 *   human_readable: Super Bowl Advertising Market Extraction
 *   domain: economic/media
 *
 * SUMMARY:
 *   The Super Bowl advertising market represents a classic extraction
 *   mechanism built on artificial scarcity of synchronous mass audience
 *   access. Over 40+ years (interval 0-40), the constraint has evolved from a
 *   simple coordination problem (how to efficiently sell advertising in a
 *   high-reach broadcast) into a sophisticated scarcity-rent extraction
 *   system. The annual 30-second spot price has escalated from $37,500 (1967)
 *   to $7,000,000+ (2024) — an increase vastly exceeding inflation or
 *   production cost increases. This trajectory reflects progressive shift
 *   from coordination function to pure extraction function. Simultaneously,
 *   the cultural valence of Super Bowl advertising has shifted: people now
 *   consume Super Bowl ads as entertainment separate from commercial value
 *   (theater ratio increasing). Digital alternatives (YouTube, streaming,
 *   TikTok) are emerging that offer cheaper reach but cannot replicate the
 *   synchronized mass audience that creates Super Bowl's unique positioning.
 *   The constraint exhibits strong perspectival divergence: the NFL and
 *   broadcast networks see pure coordination; advertisers see pure
 *   extraction; the digital-first generation sees a declining moon
 *   (scaffold/piton); the analytical observer risks naturalizing scarcity as
 *   immutable when it reflects contingent institutional choices.
 *
 * KEY AGENTS:
 *   - NFL League Office & Broadcast Networks: Primary beneficiary (institutional/arbitrage) — captures auction proceeds, controls scarcity through exclusive annual slot, no exit pressure
 *   - Fortune 500 Advertisers: Primary victim (powerless/trapped) — locked into high-cost participation by FOMO and competitive signaling; no alternative reach platform
 *   - Mid-Market Brands: Secondary victim (moderate/constrained) — financially pressured to participate despite poor ROI for their scale; exit constrained by competitive dynamics
 *   - Advertising Agencies: Secondary beneficiary (organized/arbitrage) — earn larger fees from higher budget campaigns; benefit from escalating client spending; maintain extraction coordination
 *   - Consumer Attention Commons: Diffuse victim (powerless/trapped) — attention captured through high-entertainment-value ads; no exit option from media diet without broader social coordination
 *   - Digital Media Platforms: Emerging alternative (organized/mobile) — YouTube, TikTok, streaming services offer lower-cost reach alternatives; building competing attention markets
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing scarcity premium as inherent to mass broadcast rather than contingent institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(superbowl_advertising_extraction, 0.68).
domain_priors:suppression_score(superbowl_advertising_extraction, 0.62).
domain_priors:theater_ratio(superbowl_advertising_extraction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(superbowl_advertising_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(superbowl_advertising_extraction, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(superbowl_advertising_extraction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(superbowl_advertising_extraction, snare).
narrative_ontology:human_readable(superbowl_advertising_extraction, "Super Bowl Advertising Market Extraction").
narrative_ontology:topic_domain(superbowl_advertising_extraction, "economic/media").

domain_priors:requires_active_enforcement(superbowl_advertising_extraction).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(superbowl_advertising_extraction, nfl_broadcast_network).
narrative_ontology:constraint_beneficiary(superbowl_advertising_extraction, nfl_league_office).
narrative_ontology:constraint_victim(superbowl_advertising_extraction, advertiser_corporations).
narrative_ontology:constraint_victim(superbowl_advertising_extraction, consumer_attention_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRAPPED ADVERTISER (SNARE) — High-cost access to concentrated audience creates FOMO-driven bid escalation. Cannot exit without forfeiting year-round brand presence opportunity. $7M+ per 30-second spot locks out mid-market competitors. No substitute platform provides equivalent reach (100M+ simultaneous viewers) at predictable schedule. Maximum experienced extraction — coerced participation in annual auction with no realistic alternative.
constraint_indexing:constraint_classification(superbowl_advertising_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-MARKET BRAND (SNARE) — Financially constrained but pressured by competitive signaling. Competitors' Super Bowl ads create perceived necessity to match. Can technically abstain but faces market share risk if category competitors advertise. Suppression is high: constrained by budget and competitive dynamics, not complete inability to exit.
constraint_indexing:constraint_classification(superbowl_advertising_extraction, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NFL AND BROADCAST NETWORK (ROPE) — Experiences constraint as pure coordination: Super Bowl advertising coordination solves collective action problem of reaching massive audience efficiently. Networks benefit from auction pricing; advertisers benefit from guaranteed reach. Both optimize within the constraint. Exit via arbitrage — can sell inventory at market rate or hold, managing scarcity. Low suppression from their perspective; high benefit.
constraint_indexing:constraint_classification(superbowl_advertising_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ADVERTISING INDUSTRY COLLECTIVE (TANGLED ROPE) — Organized agencies and industry bodies see both coordination function (shared audience access) and extraction mechanism (cost escalation from annual scarcity premium). Can coordinate alternative campaigns or media buys, but Super Bowl has cultural lock-in as 'premium' status. Organized exit options exist but carry prestige penalties. Active coordination required to maintain the premium pricing mechanism.
constraint_indexing:constraint_classification(superbowl_advertising_extraction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: SUPER BOWL AD-CULTURAL RITUAL (PITON) — The 'Super Bowl advertisement' has become a cultural artifact — people watch for the ads themselves, not just the game. This performative cultural role maintains pricing power even as digital alternatives (YouTube, streaming) offer cheaper reach. Theater ratio high (people discuss ads as entertainment event separate from commercial value). The ritual persists through institutional inertia and cultural myth despite cheaper alternatives achieving better ROI for most brands.
constraint_indexing:constraint_classification(superbowl_advertising_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: STREAMING AND DIGITAL ALTERNATIVE COALITION (SCAFFOLD) — Emerging alternatives (YouTube pre-roll, TikTok creator programs, streaming service ad-supported tiers, real-time dynamic insertion) offer lower-cost audience access with better targeting. Younger demographics increasingly bypass Super Bowl broadcast entirely. These alternatives create sunset pressure on the traditional extraction mechanism. Suppression declining as barriers to alternative reach diminish. Coalition has mobile exit options and growing functional alternatives.
constraint_indexing:constraint_classification(superbowl_advertising_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, audience scarcity and attention concentration are inherent to broadcast technology and human cognition. Super Bowl reaches 100M+ viewers simultaneously — a natural maximum for synchronized mass media in a ~330M population. This perspective naturalizes the market structure as immutable. However, structural data contradicts the mountain classification — digital distribution technologies and fragmented attention markets reveal this as contingent institutional arrangement, not natural law.
constraint_indexing:constraint_classification(superbowl_advertising_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(superbowl_advertising_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(superbowl_advertising_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(superbowl_advertising_extraction, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(superbowl_advertising_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(superbowl_advertising_extraction, TR),
    TR >= 0.70.

:- end_tests(superbowl_advertising_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base Extractiveness (0.68): High. The $7M per 30-second spot represents extraction well beyond production/distribution costs. Historical analysis shows spot prices have inflated 186x since 1967 (from $37.5K), while CPI has inflated only 7.5x. The gap reflects pure scarcity rent extraction. Advertisers report median ROI insufficient to justify cost based on reach alone — participation is driven by prestige and competitive signaling, not optimal spend allocation. This is extraction signature: coercion via social comparison, not rational optimization. Suppression (0.62): High. Barriers to exit include: (a) FOMO competitive dynamics — if category competitor advertises, abstention signals weakness; (b) synchronous audience concentration — no digital platform replicates 100M simultaneous viewers; (c) cultural lock-in — Super Bowl ads have become cultural artifact, conferring status value independent of commercial efficacy; (d) annual scarcity — supply fixed at one slot per year. However, suppression not absolute (0.70+) because digital alternatives exist and younger demographics actually escape via platform switching. Theater Ratio (0.58): Moderately high. Super Bowl advertising has become performance art: people attend parties specifically to watch ads; ads are discussed as entertainment separate from commercial content; many ads have zero commercial intent (artistic brands, tech demos, cultural statements). This theatrical dimension has grown over the interval as production quality escalated. The theater enables the extraction by providing cultural justification for pricing premium independent of reach metrics.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits extreme perspectival divergence. The beneficiary (NFL/networks) sees pure coordination — they are solving the logistics problem of selling to millions of simultaneous viewers. Their exit options are arbitrage: they can hold inventory, sell at auction, or adjust supply (though they maintain annual cap). They perceive low suppression. The primary victim (Fortune 500 advertiser) sees snare: participation is coerced by competitive dynamics and FOMO; exit is trapped because walking away signals weakness; the annual scarcity and synchronous audience create no realistic substitute. They perceive maximum suppression. The mid-market brand sees constrained snare: more financial flexibility than powerless agents but still pressured by category competition. The advertising industry collective (organized perspective) sees tangled rope: they coordinate the extraction mechanism itself (they advise clients on necessity of participation) while also extracting value through higher fees. The emerging digital coalition sees scaffold: they perceive the traditional extraction mechanism as temporary, being progressively displaced by cheaper alternatives. The cultural-historical perspective (theater ratio rising to 0.58) reveals piton dynamics: Super Bowl ads maintain cultural prestige through performative ritual rather than functional necessity. The analytical observer risks mountain classification (naturalizing scarcity as inherent to broadcast) but structural data reveals contingency: digital technology enables alternatives; extraction persists because institutional actors maintain scarcity fiction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from each agent's structural position relative to the constraint. The NFL/networks (beneficiaries with arbitrage exit) derive low d → low/negative f(d) → negative experienced extraction (they benefit). Fortune 500 advertisers (victims with trapped exit) derive high d → high f(d) → high experienced extraction (they pay the cost). Mid-market brands (victims with constrained exit, moderate power) derive intermediate d reflecting their partial optionality — they can technically exit but face market penalties. The advertising collective (organized power coordinating the extraction) derives d based on whether they are deriving rents from escalation (beneficiary position) or bearing costs of client pressure (victim position) — they experience mixed directionality, consistent with tangled rope. The consumer attention commons (diffuse, powerless, trapped by media diet) derives d ≈ 0.95 → maximum experienced extraction despite low base extractiveness, because their exit options are zero. The analytical observer (analytical power, analytical exit) derives d ≈ 0.72, supporting the natural law perspective. This perspectival variety — same base extractiveness (0.68) producing different experienced χ across agents — demonstrates why the constraint requires six perspective objects to capture the full structure.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE WITH PITON CONTAMINATION: The constraint resolves the mandatrophy by identifying it as fundamentally a snare (high extraction, high suppression, high coercion) that has accumulated piton (theatrical, performative) characteristics over time. The mandatrophy test: Is this primarily extraction (snare) or primarily coordination (rope)? Structural evidence strongly supports snare. (1) Cost-benefit analysis: Median advertiser reports ROI insufficient to justify $7M spend based on reach alone; participation is sunk-cost signaling, not rational optimization. (2) Scarcity structure: Supply is artificially constrained to one annual slot; alternative platforms could provide equivalent reach at lower cost, but Super Bowl maintains premium through institutional coordination of scarcity fiction. (3) Suppression mechanism: FOMO and competitive dynamics coerce participation from agents who would rationally abstain. (4) Beneficiary asymmetry: NFL/networks capture rents; advertisers bear costs. This is snare, not rope. The piton element (theater ratio 0.58, rising to cultural artifact status) indicates that the constraint's perpetuation is increasingly dependent on performative ritual rather than functional necessity. Digital alternatives reducing suppression from 0.70+ (decades past) to current 0.62 and declining further suggests the snare is degrading into piton — maintained by cultural inertia and institutional lock-in rather than fundamental structural need. The mandatrophy is resolved by recognizing: (a) it is primarily snare, not rope; (b) it is accumulating piton characteristics; (c) the scaffold perspective (digital alternatives) is real and gaining structural force. This is not a mislabeling scenario but a constraint in lifecycle transition from pure extraction (snare) toward degradation (piton) under pressure from emerging alternatives (scaffold dynamics).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    digital_substitute_sufficiency,
    'Do digital platforms (YouTube, TikTok, streaming) provide genuinely equivalent audience reach for mainstream brands, or does Super Bowl''s synchronous mass audience remain categorically unique?',
    'Long-term ROI analysis comparing Super Bowl ad performance vs digital campaigns; brand lift measurements; customer acquisition cost equivalency studies across 5+ year horizon',
    'If equivalent: scaffolding mechanism is real, extraction will decline. If not equivalent: Super Bowl maintains scarcity premium indefinitely; snare classification persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(digital_substitute_sufficiency, empirical, 'Whether digital platforms provide equivalent advertising reach').

omega_variable(
    fomo_brand_necessity,
    'Does competitor Super Bowl advertising create genuine business necessity for brands to match, or is participation primarily driven by organizational inertia and cultural prestige rather than measurable market impact?',
    'Econometric analysis of brand performance decline for competitors who skip Super Bowl ads; survey data on C-suite decision rationale; comparison of ROI between Super Bowl entrants and abstainers in same category',
    'If genuine necessity: suppression is structural (market-driven). If prestige-driven: suppression is performative (piton dynamics).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fomo_brand_necessity, empirical, 'Whether Super Bowl advertising participation is market-necessary or prestige-driven').

omega_variable(
    extraction_vs_coordination_boundary,
    'Is the Super Bowl pricing structure primarily extraction of scarcity rent (snare) or optimal coordination mechanism for expensive broadcast logistics (rope)?',
    'Analysis of cost structure: production/transmission costs vs pricing premium; comparison to other mass-audience coordination mechanisms; historical pricing trajectory relative to production cost inflation',
    'If primarily extraction: snare classification correct. If primarily coordination: rope classification correct. Mixed verdict suggests tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_coordination_boundary, empirical, 'Whether Super Bowl pricing reflects extraction or coordination costs').

omega_variable(
    audience_concentration_permanence,
    'Is the 100M+ synchronous audience concentration an inherent feature of Super Bowl sports spectacle or a declining contingency as media consumption fragments by generation?',
    'Multi-generational viewership trend analysis; demographic projections for broadcast television attendance; comparison to other sporting events for resilience of mass audience',
    'If permanent: mountain classification gains credibility. If declining: extraction mechanism dependent on ephemeral cultural moment.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(audience_concentration_permanence, empirical, 'Whether Super Bowl audience concentration persists across generations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(superbowl_advertising_extraction, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(superbowl_tr_t0, superbowl_advertising_extraction, theater_ratio, 0, 0.38).
narrative_ontology:measurement(superbowl_tr_t20, superbowl_advertising_extraction, theater_ratio, 20, 0.48).
narrative_ontology:measurement(superbowl_tr_t40, superbowl_advertising_extraction, theater_ratio, 40, 0.58).

% Extraction over time
narrative_ontology:measurement(superbowl_be_t0, superbowl_advertising_extraction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(superbowl_be_t20, superbowl_advertising_extraction, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(superbowl_be_t40, superbowl_advertising_extraction, base_extractiveness, 40, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(superbowl_advertising_extraction, resource_allocation).
narrative_ontology:affects_constraint(superbowl_advertising_extraction, broadcast_spectrum_scarcity).
narrative_ontology:affects_constraint(superbowl_advertising_extraction, consumer_attention_markets).
narrative_ontology:affects_constraint(superbowl_advertising_extraction, network_advertising_inventory_pricing).

% DUAL FORMULATION NOTE:
% The Super Bowl advertising market represents a specific instantiation of the broader broadcast scarcity coordination mechanism. Upstream constraint (broadcast_spectrum_scarcity) establishes the technical conditions for high-reach audience aggregation. The Super Bowl advertising extraction mechanism is downstream, applying institutional scarcity fiction to convert technical scarcity into pricing power. Upstream (spectrum scarcity) operates as near-mountain (technical limit); downstream (Super Bowl extraction) operates as snare (institutional choice). Decomposition justified because extractiveness differs: spectrum scarcity ε ≈ 0.15 (near-immutable); Super Bowl pricing ε ≈ 0.68 (contingent institutional).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(superbowl_advertising_extraction, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
