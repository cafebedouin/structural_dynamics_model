% ============================================================================
% CONSTRAINT STORY: broadcast_sports_economics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_broadcast_sports_economics, []).

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
 *   constraint_id: broadcast_sports_economics
 *   human_readable: Broadcast Sports Economics Extraction Mechanism
 *   domain: media/sports/economic
 *
 * SUMMARY:
 *   Broadcast sports economics constrains distribution, access, and revenue
 *   through a licensing mechanism that concentrates power in broadcast
 *   networks and league ownership while extracting from local broadcasters,
 *   athletes without negotiating leverage, and geographically fragmented fan
 *   communities. The constraint exhibits temporal evolution: extractiveness
 *   rose from 0.35 to 0.58 as digital distribution technologies increased
 *   network bargaining power against traditional broadcast competition, while
 *   simultaneously creating organizational capacity for streaming
 *   alternatives (the scaffold perspective). Theater ratio remained
 *   relatively low (0.35–0.48) because the licensing mechanism serves genuine
 *   coordination functions: networks do distribute content, leagues do
 *   capture revenue from broadcast rights, and the system does enable athlete
 *   income. However, extraction has accelerated because bargaining asymmetry
 *   has increased—networks face consolidated league negotiators with monopoly
 *   authority over content, while local broadcasters face exclusionary rights
 *   pricing that eliminates competitive distribution channels. The
 *   constraint's future hinges on whether streaming platforms can mature as
 *   viable alternative distribution pathways (scaffold sunset scenario) or
 *   whether league licensing control adapts faster than streaming can scale
 *   (perpetual tangled_rope scenario).
 *
 * KEY AGENTS:
 *   - Broadcast Networks: Primary beneficiary (institutional/arbitrage) — negotiate favorable licensing terms, capture advertising premiums, leverage bundling with other content
 *   - League Ownership: Primary beneficiary (organized/arbitrage) — architect and enforcer of licensing monopoly, coordinates collective revenue capture across teams
 *   - Local Broadcasters: Primary victim (powerless/trapped) — excluded from direct distribution, face escalating licensing costs, cannot exit without abandoning market
 *   - Regional Fan Communities: Secondary victim (powerless/identity_locked) — technically mobile but identity-fused to local teams and traditional broadcast rituals; blackout rules enforce geographic suppression
 *   - Non-Star Athletes: Secondary victim (moderate/constrained) — depend on broadcast distribution for income but lack individual leverage; experience extraction through revenue-sharing asymmetry
 *   - Star Athletes: Partial beneficiary (powerful/mobile) — access significant broadcast income but lose leverage compared to negotiating individually; have arbitrage options through endorsements and direct fan engagement
 *   - Streaming Platforms: Organized coalition (organized/mobile) — building alternative distribution pathways with sunset trajectory; currently constrained by licensing costs but gaining market share
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(broadcast_sports_economics, 0.58).
domain_priors:suppression_score(broadcast_sports_economics, 0.65).
domain_priors:theater_ratio(broadcast_sports_economics, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(broadcast_sports_economics, extractiveness, 0.58).
narrative_ontology:constraint_metric(broadcast_sports_economics, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(broadcast_sports_economics, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(broadcast_sports_economics, tangled_rope).
narrative_ontology:human_readable(broadcast_sports_economics, "Broadcast Sports Economics Extraction Mechanism").
narrative_ontology:topic_domain(broadcast_sports_economics, "media/sports/economic").

domain_priors:requires_active_enforcement(broadcast_sports_economics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(broadcast_sports_economics, broadcast_networks).
narrative_ontology:constraint_beneficiary(broadcast_sports_economics, league_ownership).
narrative_ontology:constraint_victim(broadcast_sports_economics, local_broadcasters).
narrative_ontology:constraint_victim(broadcast_sports_economics, consumer_access).
narrative_ontology:constraint_victim(broadcast_sports_economics, athletes_without_leverage).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOCAL BROADCASTER (SNARE) — Trapped by rights consolidation and licensing costs. Cannot broadcast local sports content without network approval. Faces escalating rights fees that eliminate profit margins while networks capture advertising upside. No viable exit: leaving the sports market leaves no audience alternative.
constraint_indexing:constraint_classification(broadcast_sports_economics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: REGIONAL FAN COMMUNITY (SNARE) — Structurally mobile (cable/streaming alternatives exist) but identity-locked to local team and regional broadcasts. Community identity fused with team narrative. Exit would mean abandoning identity-constituting rituals. Suppression combines external (paywall barriers) and internalized (cultural commitment to support home team). Experiences extraction through blackout rules and price discrimination.
constraint_indexing:constraint_classification(broadcast_sports_economics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(regional))).

% PERSPECTIVE 3: NON-STAR ATHLETE (TANGLED ROPE) — Derives income and career development from broadcast distribution network. Coordination function genuine: distribution enables income, exposure, talent scouting. But extraction is severe: athletes have no direct negotiation with broadcasters, compete with thousands of replacements, and receive minimal share of broadcast revenue. Exit costly (career damage, lost development opportunities) but possible (minor leagues, overseas, alternative sports). Experiences mixed coordination and extraction.
constraint_indexing:constraint_classification(broadcast_sports_economics, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: BROADCAST NETWORK (ROPE) — Benefits from coordination function: leagues need distribution, audiences need access, networks enable both. Experiences the constraint as pure coordination with favorable terms. Negotiating power against leagues allows arbitrage (package sports with other content, sell premium tiers, international distribution). Net beneficiary experiencing genuine coordination value.
constraint_indexing:constraint_classification(broadcast_sports_economics, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LEAGUE OWNERSHIP (ROPE) — Primary beneficiary and architect of the constraint. Coordinates revenue from broadcast rights, sets licensing terms, enforces geofencing and blackout rules. Experiences the constraint as coordinating collective action: owners negotiate collectively rather than competing individually for broadcast access. Asymmetric negotiating position against networks but net beneficiary with arbitrage options (streaming, international, gambling integration).
constraint_indexing:constraint_classification(broadcast_sports_economics, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: STREAMING COALITION (SCAFFOLD) — Organized agents (streaming platforms, social media, gambling operators) are building alternative distribution pathways that bypass traditional broadcast networks. Sunset clause is real and structural: as streaming matures, traditional broadcast licensing becomes optional rather than mandatory. High suppression during coalition building (platforms must negotiate rights, pay significant fees) but declining as alternatives scale. Low effective extraction because actors see the exit path and have timeline.
constraint_indexing:constraint_classification(broadcast_sports_economics, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: STAR ATHLETE (TANGLED ROPE) — Genuine coordination: broadcast access enables massive income and global brand building. But asymmetric extraction through league-wide revenue sharing and control of image rights. Star athletes have arbitrage options (endorsements, direct fan engagement, international opportunities) and can renegotiate individually, reducing experienced extraction. Mixed coordination and extraction but less severe than non-star athletes. Exit options create perspectival gap.
constraint_indexing:constraint_classification(broadcast_sports_economics, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER — FALSE SUMMIT (MOUNTAIN) — Risk of naturalizing contingent economic arrangements as immutable laws. The 'scarcity of broadcast spectrum requires centralized licensing' framing naturalizes digital-era distribution as if it inherited radio/TV constraints. In fact, scarcity was historically contingent; contemporary bandwidth abundance makes the constraint appear as natural law despite being policy-contingent. Engine false summit detector should flag this naturalization.
constraint_indexing:constraint_classification(broadcast_sports_economics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(broadcast_sports_economics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(broadcast_sports_economics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(broadcast_sports_economics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(broadcast_sports_economics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(broadcast_sports_economics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High and rising. The constraint extracts through multiple mechanisms: (1) local broadcasters pay escalating rights fees with no negotiating power, eliminating profit margins; (2) non-star athletes receive minimal share of broadcast revenue despite generating content; (3) regional fan communities face geofencing and blackout rules that artificially limit access to drive premium pricing. Extractiveness has grown because digital distribution increased league bargaining power—the existence of streaming alternatives actually reinforced league control by allowing them to threaten exclusion from emerging platforms. Suppression (0.65): High. Structural barriers include exclusive licensing terms, blackout rules, geographic restriction technologies, and licensing fee thresholds that eliminate smaller broadcasters. Internalized barriers for fans include identity attachment to local teams and traditional broadcast rituals. Theater ratio (0.48): Moderate. The constraint serves genuine coordination functions (distribution, revenue capture, audience aggregation), not purely performative ones. Theater has remained relatively flat because the licensing mechanism actually works at its stated purpose. Theater ratio is lower than many constraints because this is extraction optimized for function rather than disguised as something else.
 *
 * PERSPECTIVAL GAP:
 *   The gap reveals that this constraint operates on fundamentally different structural logics for different agents. For networks/leagues, it coordinates genuine distribution functions with favorable terms—they experience rope classification and low extraction. For local broadcasters, it enforces exclusionary control—they experience snare classification and maximum extraction. For athletes, it coordinates income with unequal distribution—they experience tangled rope but with severity depending on individual bargaining power (star vs non-star gap). For fans, it coordinates access with geographic suppression and identity-lock—they experience snare with internalized suppression. The perspectival gap is diagnostic: it reveals that the same licensing mechanism serves coordination functions for some agents (networks, leagues) while functioning as pure extraction for others (local broadcasters, non-star athletes, regional fans). This is the definitional signature of tangled_rope at the system level—mixed coordination and asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality computation chains from beneficiary/victim declarations through exit options to directional values. Networks/leagues declared as beneficiaries with arbitrage exit produce low d (~0.12 for institutional/arbitrage) → f(d) negative → negative χ (they benefit rather than lose). Local broadcasters declared as victims with trapped exit produce high d (~0.95) → f(d) high (~1.42) → high χ scaled by scope (national σ=1.0) produces significant experienced extraction. Non-star athletes declared as victims with constrained exit produce moderate-high d (~0.70) → f(d) moderate (~1.00) → moderate χ. Regional fans with identity_locked exit produce high d (~0.85) → f(d) high (~1.28) → significant extraction experienced as internalized suppression. Star athletes with mixed beneficiary/victim status and mobile exit produce lower d (~0.45) → f(d) moderate (~0.65) → weaker tangled rope experience. Scope (national) applies uniformly; higher-scope variations (global for networks, local for regional broadcasts) would adjust χ accordingly. The engine derives these automatically from the declarations; no manual computation required.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: This constraint avoids mandatrophy by maintaining genuine coordination functions alongside asymmetric extraction. The licensing mechanism actually works at its stated purpose (aggregating content, managing distribution, capturing revenue). The extraction is not disguised or performative—it is optimization for extractive function within a coordination framework. The measured theater ratio (0.48) reflects that most activity is functional, not theatrical. The perspectival gap (rope for networks, snare for broadcasters, tangled rope for athletes, scaffold for streaming) demonstrates that the same structural mechanism legitimately coordinates for some agents while extracting from others, which is exactly the tangled_rope definition. The constraint does not collapse into snare (which would require theater > suppression and pure extraction) nor into rope (which would require symmetric coordination). It maintains the diagnostic signature of tangled_rope: coordination functions exist, extraction is asymmetric, enforcement is active, and perspectival gap is wide. The future mandatrophy risk lies in the streaming coalition: if scaffold sunset arrives and alternative distribution scales, the coordination functions migrate to streaming platforms, and broadcast licensing becomes degraded piton (theatrical enforcement of declining power). Current classification is stable; risk is timeline-dependent evolution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    streaming_maturity_threshold,
    'At what market share of streaming distribution does the traditional broadcast licensing constraint lose structural grip?',
    'Market analysis tracking streaming revenue share, audience migration rates, athlete preference shifts, and league dependence on broadcast vs streaming licensing',
    'If threshold < 40% streaming: scaffold sunset will arrive within 10 years. If threshold > 60%: traditional broadcast may retain extraction mechanism for 20+ years despite declining audience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(streaming_maturity_threshold, empirical, 'Streaming market share threshold for constraint structural degradation').

omega_variable(
    identity_lock_persistence,
    'Does fan identity attachment to traditional broadcast (local networks, seasonal scheduling, shared viewing rituals) persist as identity-lock mechanism as streaming platforms provide technical access?',
    'Ethnographic analysis of fan migration patterns; measurement of broadcasting ritual persistence; identity frame shift tracking across cohorts',
    'If identity-lock persists: regional blackout rules retain suppression capacity even as technical barriers fall. Behavioral extraction (loyalty to broadcasting platform) outlasts structural extraction. If identity-lock shifts: suppression declines and snare classification shifts to mobile exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether fan community identity attachment persists as internalized suppression mechanism').

omega_variable(
    non_star_athlete_bargaining_power,
    'Can non-star athletes organize into labor coalition with sufficient leverage to renegotiate broadcast revenue share, or does replaceability maintain extraction asymmetry?',
    'Labor organizing attempts, strike history, revenue-share negotiation outcomes, athlete income distribution analysis',
    'If coalition succeeds: tangled_rope shifts toward rope for non-stars, and organized power rises. If replaceability holds: extraction persists and tangled_rope becomes more snare-like. Classification hinges on whether athletes can move from individual to organized power level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(non_star_athlete_bargaining_power, empirical, 'Whether non-star athlete bargaining power can reduce broadcast extraction asymmetry').

omega_variable(
    international_licensing_arbitrage,
    'Do international licensing arrangements (simultaneous global streaming, regional league partnerships) enable sufficient arbitrage for leagues to bypass national broadcast monopolies?',
    'Revenue flow analysis; comparison of broadcast-dependent vs international-diversified leagues; measurement of international licensing revenue growth',
    'If international arbitrage scales: league dependency on national broadcasters declines, reducing network bargaining power and compression of broadcast extraction mechanism. If national markets remain dominant: arbitrage option is theoretical, not structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(international_licensing_arbitrage, empirical, 'Whether international licensing provides meaningful league arbitrage against national broadcast networks').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(broadcast_sports_economics, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bse_tr_t0, broadcast_sports_economics, theater_ratio, 0, 0.35).
narrative_ontology:measurement(bse_tr_t10, broadcast_sports_economics, theater_ratio, 10, 0.42).
narrative_ontology:measurement(bse_tr_t20, broadcast_sports_economics, theater_ratio, 20, 0.48).
narrative_ontology:measurement(bse_tr_t30, broadcast_sports_economics, theater_ratio, 30, 0.45).

% Extraction over time
narrative_ontology:measurement(bse_be_t0, broadcast_sports_economics, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bse_be_t10, broadcast_sports_economics, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(bse_be_t20, broadcast_sports_economics, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(bse_be_t30, broadcast_sports_economics, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(broadcast_sports_economics, resource_allocation).
narrative_ontology:affects_constraint(broadcast_sports_economics, athlete_labor_economics).
narrative_ontology:affects_constraint(broadcast_sports_economics, media_market_concentration).

% DUAL FORMULATION NOTE:
% Broadcast sports economics decomposes into multiple structurally distinct constraints. The licensing mechanism (ε=0.58, tangled_rope) differs from athlete revenue extraction (ε=0.52, snare for non-stars), which differs from regional blackout enforcement (ε=0.60, snare), and from streaming competition (ε=0.35, scaffold). This story focuses on the licensing mechanism as the primary coordination/extraction hybrid; related constraints inherit from this upstream mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(broadcast_sports_economics, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
