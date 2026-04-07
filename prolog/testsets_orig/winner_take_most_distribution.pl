% ============================================================================
% CONSTRAINT STORY: winner_take_most_distribution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_winner_take_most_distribution, []).

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
 *   constraint_id: winner_take_most_distribution
 *   human_readable: Winner Take Most Distribution in Competitive Markets
 *   domain: economic/social_stratification
 *
 * SUMMARY:
 *   Winner-take-most distribution describes the concentration of market
 *   value, user attention, and competitive advantage in dominant platform
 *   leaders, particularly in digital markets where network effects, switching
 *   costs, and attention scarcity create reinforcing barriers to entry. This
 *   constraint exhibits structural tension between genuine coordination
 *   functions (platforms solve matching problems, reduce search costs, enable
 *   network participation) and extractive dynamics (lock-in, data control,
 *   monopolistic pricing, barrier elevation). The constraint's evolution
 *   shows increasing extractiveness (0.35→0.58 over 30 years) reflecting rent
 *   accumulation and regulatory capture, while theater ratio slightly
 *   declines (0.55→0.48) as open-source alternatives and regulatory scrutiny
 *   make the 'natural market outcome' narrative less defensible. The
 *   constraint operates simultaneously as coordination mechanism (rope),
 *   mixed coordination-extraction (tangled rope), extraction trap (snare),
 *   and temporary institutional arrangement (scaffold) depending on the
 *   agent's structural position and time horizon. Regulatory intervention
 *   (antitrust enforcement, data portability, interoperability mandates)
 *   represents the primary sunset mechanism for scaffold perspective.
 *
 * KEY AGENTS:
 *   - Market Leader: Primary beneficiary (institutional/arbitrage) — captures network effects, consumer lock-in, talent concentration, venture capital flows; maximum exit flexibility
 *   - Marginal Competitor: Primary victim (powerless/trapped) — faces insurmountable barriers to growth; excluded from network effects; cannot exit without abandoning market
 *   - Secondary Incumbent: Secondary victim (moderate/constrained) — benefits from category coordination but experiences talent drain, capital scarcity, customer switching; can compete on service quality or niche positioning
 *   - Consumer Base: Mixed (organized/mobile) — coordinates through platform (reduced search costs, network benefits) but experiences lock-in extraction; organized consumer movements increasing exit options
 *   - Regulatory Coalition: Organized beneficiary-of-change (organized/constrained) — perceives winner-take-most as temporary coordination problem; building interoperability standards, data portability, antitrust enforcement as sunset mechanisms
 *   - Venture Capital: Institutional beneficiary (institutional/arbitrage) — concentrates capital toward market leaders; benefits from winner-take-most dynamic through portfolio concentration and exit multiples
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(winner_take_most_distribution, 0.58).
domain_priors:suppression_score(winner_take_most_distribution, 0.65).
domain_priors:theater_ratio(winner_take_most_distribution, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(winner_take_most_distribution, extractiveness, 0.58).
narrative_ontology:constraint_metric(winner_take_most_distribution, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(winner_take_most_distribution, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(winner_take_most_distribution, tangled_rope).
narrative_ontology:human_readable(winner_take_most_distribution, "Winner Take Most Distribution in Competitive Markets").
narrative_ontology:topic_domain(winner_take_most_distribution, "economic/social_stratification").

domain_priors:requires_active_enforcement(winner_take_most_distribution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(winner_take_most_distribution, market_leader).
narrative_ontology:constraint_beneficiary(winner_take_most_distribution, network_effect_beneficiaries).
narrative_ontology:constraint_victim(winner_take_most_distribution, competitive_entrants).
narrative_ontology:constraint_victim(winner_take_most_distribution, market_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MARGINAL COMPETITOR (SNARE) — A competitor with inferior market position faces insurmountable barriers to growth. Network effects, consumer attention scarcity, and winner-take-most dynamics extract loyalty from consumers and capital from investors, funneling resources to the market leader. Exit requires abandoning the market entirely or accepting permanent marginality. No coordination function visible; pure extraction mechanism.
constraint_indexing:constraint_classification(winner_take_most_distribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SECONDARY INCUMBENT (TANGLED ROPE) — A second-place firm benefits from coordination functions (platform standards, market liquidity, consumer trust in the category) while simultaneously experiencing extraction through talent drain, venture capital concentration toward the leader, and customer switching costs. High suppression but not total — secondary firm can compete on service quality or niche positioning. Both coordination and asymmetric extraction present.
constraint_indexing:constraint_classification(winner_take_most_distribution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MARKET LEADER (ROPE) — The dominant firm experiences the winner-take-most constraint as a coordination mechanism: network effects strengthen the platform, consumer lock-in stabilizes revenue, and scale advantages improve product quality. The constraint coordin ates users, developers, and capital toward network deepening. Net beneficiary with maximum exit flexibility — can shift markets, monetization strategies, or business models with minimal cost.
constraint_indexing:constraint_classification(winner_take_most_distribution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — Organized agents (antitrust regulators, consumer advocates, open-source movements) perceive winner-take-most as a temporary coordination problem with a sunset clause. Interoperability standards, data portability mandates, and open-source alternatives are structural constraints on monopoly lock-in. Suppression exists but declining: regulations reduce switching costs, break network effects, and redistribute competitive opportunity. Sunset: technological disruption or regulatory intervention breaks the winner-take-most constraint within 15-30 years.
constraint_indexing:constraint_classification(winner_take_most_distribution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSUMER BASE (TANGLED ROPE) — Individual consumers coordinate through the platform (low search costs, network effects improve utility) but also experience extraction through lock-in (data control, switching costs, limited alternatives). Organized consumer movements (class-action litigation, coordinated switching campaigns) can exit or negotiate better terms. Mixed coordination and extraction, with exit options increasing as mobile substitutes mature.
constraint_indexing:constraint_classification(winner_take_most_distribution, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: NATURAL MARKET OUTCOME NARRATIVE (PITON) — The framing that winner-take-most is the natural result of free-market competition is substantially performative. It obscures the enforcement mechanisms (intellectual property law, patent breadth, trademark strength, regulatory capture) required to maintain concentration. The theater persists through economics textbooks and venture capital ideology despite declining functional truth as regulatory scrutiny and open-source alternatives mature.
constraint_indexing:constraint_classification(winner_take_most_distribution, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / INFORMATION THEORY VIEW (MOUNTAIN) — From a universal/civilizational perspective, winner-take-most is a mathematical property of attention economics: consumer attention is a fixed resource; information abundance creates scarcity in attention. As communication costs drop, the value of centralized coordination increases, making concentration inevitable. However, the structural data contradicts this classification — the constraint requires active enforcement (intellectual property, regulatory capture, data lock-in) to persist. The mountain framing naturalizes what are contingent institutional choices.
constraint_indexing:constraint_classification(winner_take_most_distribution, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(winner_take_most_distribution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(winner_take_most_distribution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(winner_take_most_distribution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(winner_take_most_distribution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(winner_take_most_distribution, TR),
    TR >= 0.70.

:- end_tests(winner_take_most_distribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over time. Base value reflects that market leaders extract significantly through lock-in, data control, and barrier elevation, but genuine coordination functions (network effects, platform stability, reduced search costs) reduce the pure extraction component. The 23-point increase over 30 years (0.35→0.58) reflects rent accumulation as leaders build regulatory moats, consolidate acquisitions, and entrench ecosystem control. The trajectory suggests ongoing extraction accumulation without offsetting productivity gains for trapped competitors. Suppression (0.65): High. Significant barriers include network effects (new entrants cannot offer equivalent value without achieving critical mass), switching costs (consumer and developer lock-in), regulatory capture (IP law breadth, trademark strength), venture capital concentration (funding flowing to leaders), and attention scarcity (consumer attention finite; winner captures disproportionate share). These barriers are not insurmountable but extremely costly to overcome. Theater ratio (0.48): Moderate-low and declining. The 'natural market outcome' narrative is still performative but increasingly challenged. Regulatory intervention, antitrust enforcement, and open-source alternatives are making the enforcement mechanisms visible — the theater persists but its credibility is declining. The slight decline (0.55→0.48) reflects this erosion.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap lies between the market leader (rope: genuine coordination, low extraction experienced) and marginal competitors (snare: pure extraction trap with no exit). The secondary incumbent (tangled rope) occupies the middle ground — recognizing both coordination benefits and extraction constraints, with constrained exit options. The regulatory coalition perspective (scaffold: temporary constraint with sunset) introduces temporal dimension absent from individual agent perspectives: the constraint's extractiveness increases precisely because regulatory and technological sunset mechanisms are becoming visible and costly to prevent. The piton perspective reveals that 'natural market outcome' framing persists despite declining functional truth — the narrative is maintained through institutional inertia (economics education, VC ideology, media framing) rather than empirical defensibility. The mountain perspective risks naturalizing what are contingent enforcement mechanisms (IP law, regulatory capture) as immutable mathematical laws of attention scarcity.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary and victim declarations establish the primary asymmetry: market leaders benefit from winner-take-most dynamics (network effects, switching costs, capital concentration); marginal competitors and market stability bear costs (trapped in non-viable positions, reduced competitive diversity). The tangled rope classification requires both genuine coordination (platforms do solve matching/search problems) and asymmetric extraction (leaders extract through lock-in, data control, barrier elevation) — both elements are structurally present. The base extractiveness (0.58) reflects that extraction is substantial but not total; genuine coordination value offsets some extraction cost. The 23-point increase in extractiveness over 30 years (0.35→0.58) indicates rent accumulation: as network effects deepen and switching costs compound, the extraction component grows while coordination value may plateau or decline.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy through temporal decomposition: at immediate time horizons (market leader perspective), winner-take-most appears as pure coordination (rope) — platforms solve genuine matching problems with low extraction overhead. At biographical horizons (marginal competitor perspective), the same constraint appears as pure extraction (snare) — trapped agents see only the barriers and lock-in costs. At generational horizons (regulatory coalition perspective), the constraint appears as temporary coordination with sunset (scaffold) — interoperability mandates, antitrust enforcement, and open-source alternatives are structural constraints on indefinite concentration. The mandatrophy reveals that 'which type is correct?' is not the right question; instead, the presheaf of perspectives over different agent positions and time horizons shows how the same structural phenomenon is experienced as coordination, extraction, or temporary constraint depending on standpoint. The piton perspective (theatrical 'natural market outcome' narrative) and mountain perspective (information theory inevitability) both risk false closure — the piton through institutional inertia, the mountain through false naturalization. The analytical observer's task is to recognize that all perspectival readings are legitimate (the constraint genuinely coordinates AND genuinely extracts) and that temporal dynamics (increasing extractiveness, declining theater) signal whether enforcement mechanisms or alternative pathways are winning.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effect_magnitude,
    'Are observed market concentrations driven by genuine network effects or by enforcement mechanisms (IP law, regulatory capture, switching costs)?',
    'Comparative analysis of markets with different IP regimes, switching cost structures, and regulatory environments; A/B testing of interoperability mandates on competitive dynamics',
    'If driven by genuine network effects: winner-take-most is closer to mountain (structural). If driven by enforcement: winner-take-most is contingent institutional arrangement (tangled rope with sunset potential via regulatory intervention).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_effect_magnitude, empirical, 'Whether concentration results from network effects or enforcement mechanisms').

omega_variable(
    disruptive_substitution_velocity,
    'How quickly can technological disruption or regulatory intervention break winner-take-most concentration in each market domain?',
    'Historical analysis of market transitions (SMS→iPhone→social platform shifts); prediction of next disruption timeline; correlation between regulatory intervention intensity and competitive opening',
    'If velocity < 5 years: scaffold sunset is real, classify toward temporary constraint. If velocity > 20 years: concentration is more durable, classify toward structural extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(disruptive_substitution_velocity, empirical, 'Velocity of market disruption or regulatory intervention').

omega_variable(
    consumer_surplus_extraction,
    'What fraction of the market leader''s excess profit is extracted from consumer surplus (via lock-in, price increases, data monetization) vs. captured from competitors (via scale advantages, network effects)?',
    'Price comparison across markets with different competitive structures; quantification of consumer switching costs; accounting analysis of profit sources (incremental revenue vs. margin expansion)',
    'If mostly from competitors: constraint is primarily coordination-driven (rope-like). If mostly from consumers: constraint is primarily extraction-driven (snare-like), violating tangled rope balance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_surplus_extraction, empirical, 'Distribution of excess profit between consumer surplus and competitor extraction').

omega_variable(
    interoperability_enforcement,
    'Do interoperability standards and data portability mandates (EU DMA, Digital Markets Act) actually reduce winner-take-most concentration or create new enforcement costs that preserve incumbent advantage?',
    'Post-mandate market analysis (3-5 years post-DMA implementation); competitive entry rates for firms leveraging interoperability; cost-benefit analysis of compliance burden on entrants vs. incumbents',
    'If effective: scaffold sunset is materializing. If creating new enforcement costs: constraint evolves to tangled rope with different extraction mechanisms (compliance burden extraction).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interoperability_enforcement, empirical, 'Effectiveness of interoperability mandates in reducing winner-take-most').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(winner_take_most_distribution, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wtm_tr_t0, winner_take_most_distribution, theater_ratio, 0, 0.55).
narrative_ontology:measurement(wtm_tr_t10, winner_take_most_distribution, theater_ratio, 10, 0.5).
narrative_ontology:measurement(wtm_tr_t20, winner_take_most_distribution, theater_ratio, 20, 0.48).
narrative_ontology:measurement(wtm_tr_t30, winner_take_most_distribution, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(wtm_be_t0, winner_take_most_distribution, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(wtm_be_t10, winner_take_most_distribution, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(wtm_be_t20, winner_take_most_distribution, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(wtm_be_t30, winner_take_most_distribution, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(winner_take_most_distribution, resource_allocation).
narrative_ontology:boltzmann_floor_override(winner_take_most_distribution, 0.18).
narrative_ontology:affects_constraint(winner_take_most_distribution, network_effect_lock_in).
narrative_ontology:affects_constraint(winner_take_most_distribution, attention_scarcity_economics).
narrative_ontology:affects_constraint(winner_take_most_distribution, regulatory_capture_digital_markets).

% DUAL FORMULATION NOTE:
% Winner-take-most distribution decomposes into three structurally distinct constraints: (1) network_effect_lock_in (ε≈0.35, natural lock-in mechanism) — the mathematical property that larger networks offer more value, creating self-reinforcing concentration; (2) attention_scarcity_economics (ε≈0.42, bounded rationality) — consumer attention is finite; information abundance creates scarcity in attention channels; (3) regulatory_capture_digital_markets (ε≈0.68, enforcement mechanism) — IP law breadth, patent strategies, trademark strength, and antitrust underenforcement are actively used to elevate and maintain barriers. Winner-take-most as a unified constraint (ε=0.58) represents the interaction of all three. The increasing extractiveness trajectory (0.35→0.58) reflects accumulation of enforcement mechanisms on top of the natural network effect base.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(winner_take_most_distribution, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
