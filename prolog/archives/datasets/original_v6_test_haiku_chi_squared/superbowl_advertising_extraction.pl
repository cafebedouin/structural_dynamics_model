% ============================================================================
% CONSTRAINT STORY: superbowl_advertising_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   domain: economic/media_markets
 *
 * SUMMARY:
 *   The Super Bowl advertising market exemplifies how a coordination
 *   mechanism (concentrating national attention to solve the collective
 *   action problem of reaching mass audiences) has degraded into a snare
 *   mechanism (extracting premium pricing from trapped advertisers via
 *   fashion and institutional inertia). The constraint operates across three
 *   distinct temporal windows: (1) the early internet era (1990s-2010) when
 *   Super Bowl genuinely concentrated attention and provided unique ROI; (2)
 *   the transition era (2010-2020) when digital targeting emerged as
 *   equivalent alternative; (3) the current era (2020-present) when the
 *   premium persists primarily through cultural momentum and coordinated
 *   advertiser behavior, not technological necessity. The extractiveness
 *   trajectory (0.32→0.45→0.58 over 30 years) captures the paradox: as
 *   technological alternatives improved, the effective extraction increased
 *   because the constraint's legitimacy eroded but advertiser behavior locked
 *   in. Theater ratio (0.38→0.51→0.64) tracks the degradation: Super Bowl ads
 *   shifted from primarily functional messaging (reaching target audiences)
 *   to performative spectacle (signaling cultural dominance and brand
 *   prestige). The suppression mechanism (0.68) operates through coordinated
 *   expectations: individual advertisers believe competitors will advertise,
 *   so they must also advertise, creating a locked-in high-price equilibrium
 *   that persists despite availability of cheaper alternatives. Mid-market
 *   advertisers are trapped: exiting unilaterally means losing visibility
 *   signaling, but the signaling value exists only because all competitors
 *   remain trapped in the same equilibrium.
 *
 * KEY AGENTS:
 *   - NFL/Broadcast Rights Holders: Primary beneficiaries (institutional/arbitrage) — capture direct revenue from ad slot sales (~$7M per 30-second spot in 2024) plus ancillary sponsorship and ratings-based ad network premiums
 *   - Mega-Cap Consumer Brands (Apple, Nike, Coca-Cola, etc.): Secondary beneficiaries (powerful/arbitrage) — can afford Super Bowl spots and benefit from cultural positioning and viral moment creation; can also afford competing high-cost channels
 *   - Mid-Market Advertisers (automotive, fast food, consumer packaged goods at $500M-$5B revenue): Primary victims (moderate/constrained) — trapped by coordination failure; must participate in high-price equilibrium despite availability of cheaper national reach
 *   - Small Businesses and Limited-Budget Advertisers: Secondary victims (powerless/trapped) — cannot participate in Super Bowl market at all; forced to lower-tier advertising alternatives that lack the cultural signaling value
 *   - Consumer Attention Commons: Tertiary victim (powerless/trapped) — abstract collective resource that experiences extraction as unsolicited psychic incursion during mass spectacle; cannot exit or govern its own attention resources
 *   - Advertising Industry as Collective: Organized participant (organized/constrained) — agencies profit from Super Bowl ad production and placement fees; have incentive to maintain high prices; partially captured by NFL interests
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(superbowl_advertising_extraction, 0.58).
domain_priors:suppression_score(superbowl_advertising_extraction, 0.68).
domain_priors:theater_ratio(superbowl_advertising_extraction, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(superbowl_advertising_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(superbowl_advertising_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(superbowl_advertising_extraction, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(superbowl_advertising_extraction, snare).
narrative_ontology:human_readable(superbowl_advertising_extraction, "Super Bowl Advertising Market Extraction").
narrative_ontology:topic_domain(superbowl_advertising_extraction, "economic/media_markets").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(superbowl_advertising_extraction, nfl_broadcast_rights_holders).
narrative_ontology:constraint_beneficiary(superbowl_advertising_extraction, primary_advertising_agencies).
narrative_ontology:constraint_victim(superbowl_advertising_extraction, consumer_attention_commons).
narrative_ontology:constraint_victim(superbowl_advertising_extraction, mid_market_advertisers).
narrative_ontology:constraint_victim(superbowl_advertising_extraction, small_businesses).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MID-MARKET ADVERTISER (SNARE) — Trapped by fear of missing cultural moment. 30-second spot costs $6-7M (2024 rates). Cannot opt out without losing competitive brand visibility signaling during peak national attention (113M+ viewers). Exit options are catastrophically expensive (alternative national media buys lack temporal concentration). d≈0.88, f(d)≈1.32, σ=1.0 → χ≈0.77. High effective extraction.
constraint_indexing:constraint_classification(superbowl_advertising_extraction, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSUMER ATTENTION COMMONS (SNARE) — Abstract collective good (shared cultural moment, conversation substrate) that cannot exit. Experiences extraction as: (a) unsolicited psychic incursion during mass spectacle; (b) cognitive capture that commercial advertisers monetize; (c) inability of the commons itself to benefit from or govern its own attention resources. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80. Maximal extraction of abstract resource.
constraint_indexing:constraint_classification(superbowl_advertising_extraction, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: NFL BROADCAST RIGHTS HOLDER (ROPE) — Experiences constraint as coordination mechanism: monetizing concentrated viewership (113M+) solves the collective action problem of funding premier sports broadcasting at scale. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary. The rights holder sees the high ad rates as legitimate compensation for guaranteed audience concentration.
constraint_indexing:constraint_classification(superbowl_advertising_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MEGA-CAP BRAND (TANGLED ROPE) — Has both coordination and extraction functions. Benefits from using Super Bowl as signaling mechanism (billions in impressions, viral potential, cultural dominance positioning). But also extracts via network effects: when mega-caps dominate Super Bowl ad slots, mid-market competitors cannot achieve equivalent ROI (the expected value of a mid-market spot degrades because it is surrounded by superior production, brand recognition, and message recall from mega-caps). d≈0.35, f(d)≈0.35, σ=1.0 → χ≈0.20. Low effective extraction because mega-caps have high exit options (they can buy equivalent national reach via other mechanisms). The constraint is more beneficial than extractive for this tier.
constraint_indexing:constraint_classification(superbowl_advertising_extraction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: SUPER BOWL CULTURAL INSTITUTION (PITON) — Super Bowl is maintained as the advertising apex through institutional inertia and theatrical performance. Early Super Bowls (1970s-1990s) genuinely concentrated national attention for sports fan coordination (limited channels, no internet, no streaming). Modern Super Bowl advertising now operates substantially on theater: the idea that Super Bowl ad slots are uniquely valuable persists because previous years' high spend and viral moments created a coordination expectation. But digital targeting alternatives and streaming fragmentation now provide equivalent audience reach at lower cost. theater_ratio=0.64 indicates degradation: the constraint's function (reaching concentrated audience) is increasingly performative as fragmented digital media can achieve the same targeting more efficiently. The institutional commitment to 'Super Bowl advertising spectacle' persists through cultural momentum, not technological necessity.
constraint_indexing:constraint_classification(superbowl_advertising_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: MID-MARKET ADVERTISER COLLECTIVE (PITON) — From an organized coalition perspective, Super Bowl advertising is a degraded coordination mechanism. Historically, it solved the problem of reaching concentrating mass audiences (pre-internet). Now it persists through: (a) sunk cost fallacy (past Super Bowl ads set expectation that future ads must happen); (b) social proof / fashion (if competitors advertise, we must); (c) cultural significance marketing (brands want association with the event, not the audience reach). This creates inertial extraction without coordination benefit. theater_ratio=0.64 captures this. The coalition could negotiate lower rates or coordinate to a cheaper platform, but the institutional commitment to the event's symbolic value prevents coordination escape. d≈0.72, f(d)≈1.13, σ=1.0 → χ≈0.65. Moderate-high extraction, held in place by institutional theater rather than genuine functional necessity.
constraint_indexing:constraint_classification(superbowl_advertising_extraction, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / FRAGMENTATION SUNSET (SCAFFOLD) — From a long-term view, Super Bowl advertising's extraction mechanism has a sunset clause: digital fragmentation, addressable streaming ads, AI audience matching, and metaverse/gaming platforms will eventually provide equivalent audience targeting at lower cost and higher precision. The constraint's extractiveness (0.58) is temporarily high because legacy media still concentrates attention. But the coordination function it once served (reaching mass audiences) is being displaced by precision targeting. The theater ratio (0.64) indicates the constraint is already operating partially on cultural inertia. The sunset is 10-20 years: as streaming becomes primary (not secondary) consumption for Super Bowl generation (Gen Z), the mass-audience gathering is replaced by fragmented individual viewing, and the advertising premium collapses. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.37. Moderate effective extraction on a temporary trajectory.
constraint_indexing:constraint_classification(superbowl_advertising_extraction, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

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
 *   Extractiveness (0.58): High. The constraint extracts from mid-market and constrained advertisers via coordination failure: each individual advertiser must pay $6-7M for a 30-second spot to maintain competitive visibility signaling, even though cheaper national media alternatives (digital, cable networks, streaming ad networks) provide equivalent reach. The extraction is magnified by the coordination lock-in: the premium persists not because Super Bowl provides unique value, but because all competitors must stay in the game. Without the coordination failure, market prices would fall toward the alternative media baseline (~$1-2M for equivalent reach). Suppression (0.68): High. Suppression mechanisms include: (a) coordinated expectations (if competitors advertise, we cannot exit without losing positioning); (b) cultural prestige marketing (brands feel compelled to associate with the event); (c) media coverage amplification (Super Bowl ads receive disproportionate social media and traditional press attention, creating FOMO); (d) advertiser herding (following competitors' participation decisions rather than independent ROI calculation). Escape from the coordination lock-in is suppressed by lack of focal alternative (hard to coordinate a collective exodus without a unified destination) and fear of individualized loss of visibility. Theater ratio (0.64): Moderate-high. The constraint operates substantially on theatrical performance: the belief that Super Bowl advertising is uniquely valuable persists despite technological displacement. Early Super Bowls (1970s-1990s) had genuine theater reduction (the event actually concentrated attention uniquely). Modern Super Bowls maintain theater through: (a) tradition (past ad virality creates expectation of future ad virality); (b) celebrity participation and cultural narrative (Super Bowl ads are 'cultural events,' not merely advertising); (c) media amplification (ads receive disproportionate social media attention because they are Super Bowl ads, not because they are inherently better). The functional component (audience reach) has degraded from unique necessity to parity with digital alternatives. The performative component (cultural prestige signaling) has grown.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows stark perspectival disagreement on whether extraction is occurring. The NFL/broadcast rights holder sees rope: monetizing concentrated viewership solves the coordination problem of funding premier broadcasting. The mega-cap brand sees tangled rope with net benefit: the constraint enables cultural dominance signaling that has asymmetric value for brands with existing scale and production capacity. The mid-market advertiser sees snare: trapped by coordinated expectations; must pay premium despite availability of cheaper alternatives; the perceived value of a Super Bowl spot degrades as production quality and surrounding advertiser tier increases (my ad's ROI depends on what competitors advertise). The consumer attention commons sees snare: extraction of uncompensated psychic attention during mass cultural moment. The analytical observer sees scaffold with sunset: institutional theater sustaining a constraint that technological fragmentation will displace within 10-20 years as Gen Z consumes entertainment via streaming and gaming rather than live broadcast. The cultural institution perspective sees piton: Super Bowl advertising persists through institutional momentum and sunk cost expectations (past high spend created coordination lock-in) despite technological alternatives. This perspectival spread (from rope to snare) indicates a constraint in transition: the coordination function is degraded, but institutional inertia sustains the extraction mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   NFL/Broadcast Rights Holder: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary. Direct revenue stream from ad slot sales; can exit to lower-cost programming without loss. Mid-Market Advertiser: Victim + constrained → d≈0.88, f(d)≈1.32. High extraction. Trapped by coordinated expectations; cannot exit without losing visibility signaling; must pay premium despite cheaper alternatives. Consumer Attention Commons: Victim + trapped → d≈0.92, f(d)≈1.38. Maximal extraction. Abstract collective with no exit options; experiences unsolicited extraction of attention resources. Mega-Cap Brand: Mixed (beneficiary + arbitrage, but also partial victim to coordination asymmetry) → d≈0.35, f(d)≈0.35. Low effective extraction for this tier because high exit options (can access equivalent reach via other mechanisms) and net benefit from signaling value and production scale advantages. Mid-Market Collective (Organized): Victim + constrained → d≈0.72, f(d)≈1.13. Moderate-high extraction. The collective has more agency than individuals but remains trapped by coordination failure; cannot exit unilaterally without losing individual member positioning.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that Super Bowl advertising is NOT a pure coordination mechanism (rope) but rather a degraded coordination mechanism that has become extractive (tangled rope → snare). Early Super Bowl advertising (1970s-1990s) was primarily rope: it solved a genuine collective action problem (how to reach mass audiences before cable/digital fragmentation). The coordination value was real and visible: every advertiser benefited from concentration (the chance of their ad going viral, the cultural conversation, the shared viewership). Modern Super Bowl advertising (2020-present) is primarily snare: the coordination problem it solved has been replaced by technological alternatives (digital targeting, streaming ads, addressable TV). The premium persists because of institutional inertia and coordinated expectations, not because of genuine coordination benefit. The mid-market advertiser's dilemma is not 'should I participate in a valuable collective solution?' but 'should I pay $6-7M to signal that I can afford to pay $6-7M?' The constraint has transformed from solving a problem to being the problem. The theater ratio increase (0.38→0.64) tracks this transformation: as technological alternatives improved, the functional component degraded and the performative component grew. The constraint meets snare criteria: ε=0.58 (high extraction), suppression=0.68 (high suppression via coordinated expectations), χ≈0.77 for mid-market victim perspective (very high effective extraction). The constraint does not qualify as pure rope (ε ≤ 0.45) because the coordination function has eroded. It does not qualify as pure tangled rope (which requires genuine coordination benefit alongside extraction) because the benefits are asymmetric and artificial (cultural prestige signaling, not functional coordination). The analytical observer's scaffold perspective captures the key insight: this constraint has a sunset clause (media fragmentation) but we are still in the plateau phase where institutional theater sustains the premium.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    audience_fragmentation_timeline,
    'How quickly will streaming and digital targeting displace Super Bowl''s mass audience gathering as the primary advertising venue?',
    'Tracking of Gen Z/Gen Alpha viewing patterns (linear TV vs streaming vs gaming during Super Bowl); measurement of advertising ROI parity between Super Bowl and targeted digital buys; emergence of alternative cultural event with equivalent audience concentration',
    'If sunset < 10 years: scaffold classification becomes dominant, extraction mechanism collapses early. If sunset > 20 years: piton/snare classifications remain stable; institutional inertia sustains premium pricing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(audience_fragmentation_timeline, empirical, 'Timeline for media fragmentation to displace Super Bowl advertising premium').

omega_variable(
    extracted_value_vs_legitimacy_boundary,
    'At what advertiser-tier threshold does Super Bowl advertising represent rational brand positioning vs speculative coordination failure (celebrities paying for cultural association rather than audience reach)?',
    'Cross-advertiser ROI analysis comparing Super Bowl spend to equivalent digital+traditional media alternatives; survey of advertiser decision-making (cultural prestige vs measured reach); comparison of brand lift metrics pre/post-Super Bowl vs continuous digital campaigns of equivalent spend',
    'If rational threshold is high (majority of spend is legitimate ROI): constraint is primarily rope/tangled rope. If rational threshold is low (much spend is performative): constraint is primarily snare/piton (extraction via fashion/inertia).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extracted_value_vs_legitimacy_boundary, empirical, 'Whether Super Bowl advertising spend is ROI-justified or driven by cultural fashion').

omega_variable(
    coordination_escape_barriers,
    'Can mid-market advertisers coordinate a collective exit from Super Bowl advertising without losing competitive positioning, and what prevents such coordination?',
    'Game-theoretic analysis of mid-market advertiser payoff matrices; historical precedent analysis (other markets where coordinated escape from expensive signaling occurred); experimental study of advertiser willingness to coordinate',
    'If escape is possible: constraint is snare due to coordination failure (tragic commons), not fundamental extraction. If escape is impossible: constraint is more fundamental snare (extraction backed by asymmetric information or power, not pure coordination failure).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_escape_barriers, conceptual, 'Whether mid-market advertisers can coordinate exit from Super Bowl premium').

omega_variable(
    cultural_premium_measurement,
    'What fraction of Super Bowl advertising premium (vs equivalent national reach via other media) is due to genuine cultural significance vs institutional fashion/inertia?',
    'Historical analysis of advertising premium across eras (growth vs decline vs stabilization); brand association metrics (social listening on ''aspirational'' vs ''practical'' Super Bowl advertiser associations); longitudinal tracking of brand lift attribution (Super Bowl vs comparable spend on non-Super Bowl vehicles)',
    'If cultural premium is real (>60% of premium): constraint legitimacy increases (coordination + prestige signaling). If cultural premium is declining (<30% of premium): piton classification dominates (institutional theater sustaining premium).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cultural_premium_measurement, empirical, 'Fraction of Super Bowl advertising premium due to cultural significance vs inertia').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(superbowl_advertising_extraction, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(superbowl_tr_t0, superbowl_advertising_extraction, theater_ratio, 0, 0.38).
narrative_ontology:measurement(superbowl_tr_t15, superbowl_advertising_extraction, theater_ratio, 15, 0.51).
narrative_ontology:measurement(superbowl_tr_t30, superbowl_advertising_extraction, theater_ratio, 30, 0.64).

% Extraction over time
narrative_ontology:measurement(superbowl_be_t0, superbowl_advertising_extraction, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(superbowl_be_t15, superbowl_advertising_extraction, base_extractiveness, 15, 0.45).
narrative_ontology:measurement(superbowl_be_t30, superbowl_advertising_extraction, base_extractiveness, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(superbowl_advertising_extraction, information_standard).
narrative_ontology:affects_constraint(superbowl_advertising_extraction, attention_scarcity_commons).
narrative_ontology:affects_constraint(superbowl_advertising_extraction, advertising_budget_allocation_constraints).

% DUAL FORMULATION NOTE:
% Super Bowl advertising extraction is downstream of broader attention market constraints (scarcity of mass-audience platforms in pre-digital era). As upstream constraints (media fragmentation, attention fragmentation) evolve, the structural basis for Super Bowl premium erodes. The network link captures this dependency: fragmentation of attention infrastructure directly impacts whether concentrated Super Bowl audience remains valuable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(superbowl_advertising_extraction, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
