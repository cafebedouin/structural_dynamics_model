% ============================================================================
% CONSTRAINT STORY: incumbent_industry_moat
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_incumbent_industry_moat, []).

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
 *   constraint_id: incumbent_industry_moat
 *   human_readable: Incumbent Industry Moat Protection Mechanism
 *   domain: economic/industrial_organization
 *
 * SUMMARY:
 *   An incumbent industry moat is a structural constraint that protects
 *   established firms from competition through mechanisms including network
 *   effects, switching costs, regulatory licensing, brand loyalty, scale
 *   advantages, and control over critical resources or distribution channels.
 *   The moat exhibits hybrid characteristics: it provides genuine
 *   coordination value (industry standards, network stability, consumer trust
 *   in quality) while simultaneously extracting rents from potential entrants
 *   and consumers unable to switch. The constraint's classification varies
 *   dramatically by observer position: the incumbent experiences it as pure
 *   coordination (Rope); the blocked entrepreneur experiences it as
 *   entrapment (Snare); the aspiring competitor experiences it as mixed
 *   (Tangled Rope); the technology disruptor sees it as temporary (Scaffold);
 *   the regulatory apparatus maintains it through increasingly theatrical
 *   compliance; and the civilizational analyst risks naturalizing what is
 *   actually contingent institutional design. The rising extractiveness over
 *   the 10-year interval reflects accumulating barriers as the incumbent's
 *   advantages compound — scale, data, network effects, and regulatory
 *   capture all amplify the moat over time.
 *
 * KEY AGENTS:
 *   - Incumbent Firms: Primary beneficiary (institutional/arbitrage) — capture supernormal returns through moat protection; experience constraint as coordination solution
 *   - Potential Entrants (Blocked): Primary victim (powerless/trapped) — face insurmountable barriers; cannot compete or access market
 *   - Aspiring Competitors: Secondary victim (moderate/constrained) — can enter at high cost through niches or innovation; face substantial switching costs and brand disadvantage
 *   - Technology Disruptors: Organized agents (organized/mobile) — can bypass moat through paradigm-shifting innovation or adjacent-market entry; see moat as temporary
 *   - Regulatory Apparatus: Institutional actor (institutional/arbitrage) — maintains licensing, standards, and barriers; experiences function as increasingly theatrical
 *   - Consumers: Powerless agents (powerless/trapped or powerless/constrained) — benefit from incumbent's stability but bear cost of reduced choice and higher prices; unable to switch or form alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(incumbent_industry_moat, 0.52).
domain_priors:suppression_score(incumbent_industry_moat, 0.58).
domain_priors:theater_ratio(incumbent_industry_moat, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(incumbent_industry_moat, extractiveness, 0.52).
narrative_ontology:constraint_metric(incumbent_industry_moat, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(incumbent_industry_moat, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(incumbent_industry_moat, tangled_rope).
narrative_ontology:human_readable(incumbent_industry_moat, "Incumbent Industry Moat Protection Mechanism").
narrative_ontology:topic_domain(incumbent_industry_moat, "economic/industrial_organization").

domain_priors:requires_active_enforcement(incumbent_industry_moat).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(incumbent_industry_moat, incumbent_firms).
narrative_ontology:constraint_victim(incumbent_industry_moat, potential_entrants).
narrative_ontology:constraint_victim(incumbent_industry_moat, consumers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BLOCKED ENTREPRENEUR (SNARE) — Faces capital barriers, regulatory licensing, supplier lock-in, and switching cost networks with no viable exit. Cannot compete; cannot operate in adjacent markets; cannot access distribution channels. Full extraction with maximum suppression.
constraint_indexing:constraint_classification(incumbent_industry_moat, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ASPIRING COMPETITOR (TANGLED ROPE) — Can enter through niche markets or digital channels (some coordination benefit from infrastructure the moat maintains), but faces high switching costs, brand disadvantage, and regulatory compliance costs. Entry is possible at substantial price; exit into adjacent markets also carries costs. Mixed experience — some benefit from industry infrastructure; severe extraction from moat mechanisms.
constraint_indexing:constraint_classification(incumbent_industry_moat, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT FIRM (ROPE) — Experiences the moat as pure coordination: network effects that lock in customers, brand loyalty that reduces marketing costs, scale advantages that enable efficient supply chains, switching costs that retain customers. The incumbent benefits from the collective network; the moat is a coordination problem they've solved and now benefit from solving. Net beneficiary.
constraint_indexing:constraint_classification(incumbent_industry_moat, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: TECHNOLOGY DISRUPTOR (SCAFFOLD) — Organized entrants (platform companies, venture-backed firms) can bypass traditional moat mechanisms through technological innovation or network-adjacent markets. The moat is temporary because paradigm shifts render legacy barriers obsolete. Exit path is visible: the disruptor's goal is to make the moat irrelevant rather than to compete within it.
constraint_indexing:constraint_classification(incumbent_industry_moat, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY APPARATUS (PITON) — Licensing regimes, patent systems, and standards enforcement that constitute the moat are largely performed: regulatory compliance becomes theater when the burden is calibrated to exclude competitors rather than to achieve actual safety/quality standards. The apparatus persists through institutional inertia and incumbent lobbying, not because the regulatory function is efficient. Theater ratio high; function degraded.
constraint_indexing:constraint_classification(incumbent_industry_moat, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some incumbent advantage is inherent to competition: first movers accumulate scale, networks, and reputation that cannot be instantly replicated. Barriers to entry are natural consequences of prior success and network effects. However, the structural data contradicts this mountain reading — the base extractiveness is driven by enforcement mechanisms, regulatory capture, and suppression of alternatives, not by irreducible physical/economic law. The 'natural moat' framing naturalizes what is actually contingent institutional design.
constraint_indexing:constraint_classification(incumbent_industry_moat, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(incumbent_industry_moat_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(incumbent_industry_moat, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(incumbent_industry_moat, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(incumbent_industry_moat, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(incumbent_industry_moat, TR),
    TR >= 0.70.

:- end_tests(incumbent_industry_moat_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The incumbent captures supernormal returns during the moat's protection period, but the extraction is not maximal because: (a) some of the incumbent's advantage comes from genuine coordination value (network effects, brand quality, infrastructure stability), (b) consumers do derive some benefit from the stable ecosystem, and (c) innovation occurs within the moat even if slowed by protection. The value reflects that the moat is hybrid — partly coordination, partly extraction. The rising trajectory (0.35 → 0.52) reflects accumulating barriers as incumbent advantages compound over time: network effects deepen, switching costs increase, regulatory capture tightens, and data advantages widen. Suppression (0.58): High. Multiple enforcement mechanisms constrain exit: capital barriers, regulatory licensing requirements, supplier lock-in, switching costs, brand disadvantage, and network effects all create barriers to entry or exit. But suppression is not total — niche entry, technological disruption, and regulatory reform remain possible. Theater ratio (0.48): Moderate. The moat's maintenance involves some performative elements (regulatory compliance theater, brand narratives, artificial product differentiation) but retains genuine functional content (actual network effects, real switching costs, legitimate scale advantages). The ratio is lower than regulatory constraints alone because the moat's network and switching cost mechanisms are partially real, not entirely theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap between the incumbent's Rope and the blocked entrepreneur's Snare is the core diagnostic. The same mechanism (switching costs, network effects, regulatory barriers) appears as coordination to one agent and entrapment to another. This gap is not resolvable by better information or transparency — it is structural. The incumbent genuinely benefits from network stability; the blocked entrepreneur genuinely cannot enter. Both perceptions are accurate from their positions. The gap reveals that the moat's classification depends entirely on position. If the analytical observer conflates the incumbent's Rope perspective with the moat's 'true' nature, they naturalize extraction as coordination. If they focus only on the blocked entrepreneur's Snare, they miss the genuine coordination value the moat provides. The Tangled Rope classification at the institutional/moderate level captures the hybrid nature: the constraint both coordinates (enables network stability) and extracts (protects incumbent rents, blocks entry). The scaffold perspective is aspirational — it assumes that technology will eventually disrupt the moat, but the timeline is uncertain (omega: disruption_timeline). The piton perspective on regulatory apparatus suggests that enforcement is becoming increasingly theatrical: compliance burdens may exceed their original justification as the moat sustains itself through regulatory inertia. The mountain perspective risks naturalizing contingent institutional design: competition may have 'natural' winners, but the specific form that advantage takes (network lock-in, regulatory capture, switching costs) is designed, not inevitable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is determined by each agent's structural position relative to the extraction flow. Incumbent firms are beneficiaries with arbitrage options — they have low d (≈0.10) because they can exit the moat by abandoning the market, and they benefit from the moat (negative effective extraction). Blocked entrepreneurs are victims with no exit — they have high d (≈0.95) because they cannot enter and bear the full cost of exclusion. Aspiring competitors are victims with constrained exit options — they have moderate-high d (≈0.60) because they can enter at high cost and can exit by pursuing adjacent markets, but the cost of both is substantial. Technology disruptors are organized agents with mobile exit — they have moderate d (≈0.45) because they can bypass the moat entirely through innovation and do not experience extraction as binding. The regulatory apparatus benefits from the moat through institutional capture (low d ≈0.20) and experiences it as coordination (their role is to enforce the barriers). The cyclical measurement pattern (extractiveness rising 0.35 → 0.52, theater ratio rising 0.38 → 0.48) reflects moat accumulation: as the incumbent's advantages compound, more suppression mechanisms are required to maintain the barrier, but the functional content remains stable (theater ratio rises slowly, not sharply).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by revealing how the same structural mechanism provides genuine coordination value (network effects, industry standards, consumer trust) while simultaneously extracting rents (supernormal incumbent returns, reduced consumer choice, suppressed competition). The mandatrophy resolution: the moat is simultaneously Rope (for the incumbent who benefits from coordination) and Snare (for the blocked entrepreneur who cannot compete). No single type is 'correct' — the presheaf over observer positions IS the answer. The Tangled Rope classification at the analytical/moderate level captures this duality: the constraint has both coordination function (genuine network effects) and extraction mechanism (artificial barriers). The theater ratio (0.48) is key: if theater were higher (> 0.70), the constraint would degrade to Piton (regulatory theater maintaining a defunct function). If theater were lower (< 0.30), it would be pure Rope (genuine coordination). At 0.48, the constraint is in the unstable region where both functions remain active. The rising extractiveness trajectory (0.35 → 0.52) combined with rising theater ratio (0.38 → 0.48) suggests moat accumulation: the incumbent's advantages are compounding, requiring more suppression mechanisms and increasing theatrical enforcement to maintain. This pattern is consistent with regulatory capture (the regulatory apparatus becoming theater) and network effect concentration (requiring artificial switching cost maintenance). The mandatrophy is resolved not by choosing a single type but by recognizing that the constraint is genuinely Tangled Rope — it requires active enforcement precisely because it is not a natural law (Mountain) nor pure coordination (Rope), but an institutional arrangement that must be maintained against forces that would dissolve it (technological disruption, regulatory reform, competitive entry through adjacent markets).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effect_vs_regulatory_lock,
    'Does the moat''s power come from genuine network effects and consumer preference, or primarily from regulatory/legal barriers and high switching costs?',
    'Empirical analysis: measure customer retention when switching costs are artificially removed (e.g., data portability regulations, subsidized migration); compare willingness-to-pay for incumbent vs equivalent alternative in markets with low regulatory barriers',
    'If network-driven: extractiveness drops significantly, classification shifts toward Rope. If regulatory-driven: extractiveness persists, classification remains Tangled Rope or Snare. Determines whether the moat is sustainable or dependent on enforcement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effect_vs_regulatory_lock, empirical, 'Whether moat power derives from network effects or regulatory barriers').

omega_variable(
    technological_disruption_timeline,
    'What is the realistic horizon for paradigm-shifting technology to render the incumbent''s moat obsolete?',
    'Historical precedent analysis (Kodak → digital, Nokia → smartphones, Yellow Pages → search); technical feasibility assessment of disruptive technologies relative to incumbent moat mechanics',
    'If timeline < 10 years: scaffold perspective dominates; moat is genuinely temporary. If timeline > 30 years: scaffold is aspirational; moat provides multi-generational extraction. Affects whether constraint should classify as Scaffold (temporary) or Tangled Rope (persistent).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technological_disruption_timeline, empirical, 'Realistic horizon for technological disruption of incumbent advantage').

omega_variable(
    switching_cost_endogeneity,
    'Are switching costs caused by technological incompatibility and genuine coordination complexity, or deliberately engineered to prevent competition?',
    'Technical comparison: cost of implementing genuine interoperability vs actual switching cost; analysis of legacy format dependencies and whether simpler alternatives were deliberately avoided; testimony/evidence of design decisions favoring lock-in',
    'If exogenous (genuine technical incompatibility): suppression is coordination cost, extractiveness lower. If endogenous (deliberately designed): suppression is enforcement mechanism, extractiveness higher. May shift classification from Tangled Rope toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_endogeneity, empirical, 'Whether switching costs are technical necessities or deliberate lock-in design').

omega_variable(
    consumer_welfare_distribution,
    'Do consumers experience net benefit from the moat (stable ecosystem, quality, innovation) or net harm (high prices, reduced choice, suppressed innovation)?',
    'Consumer surplus analysis; price/quality comparison with competitive markets; innovation rate pre/post moat establishment; welfare economics analysis of consumer outcomes',
    'If net positive: moat provides genuine coordination value, strengthens Rope perspective. If net negative: moat is pure extraction, strengthens Snare perspective. Determines whether to classify as Tangled Rope (mixed) or shift toward pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_welfare_distribution, empirical, 'Net welfare impact of incumbent moat on consumer outcomes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(incumbent_industry_moat, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(moat_tr_t0, incumbent_industry_moat, theater_ratio, 0, 0.38).
narrative_ontology:measurement(moat_tr_t5, incumbent_industry_moat, theater_ratio, 5, 0.42).
narrative_ontology:measurement(moat_tr_t10, incumbent_industry_moat, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(moat_be_t0, incumbent_industry_moat, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(moat_be_t5, incumbent_industry_moat, base_extractiveness, 5, 0.44).
narrative_ontology:measurement(moat_be_t10, incumbent_industry_moat, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(incumbent_industry_moat, resource_allocation).
narrative_ontology:affects_constraint(incumbent_industry_moat, regulatory_capture).
narrative_ontology:affects_constraint(incumbent_industry_moat, technology_disruption_cycle).
narrative_ontology:affects_constraint(incumbent_industry_moat, consumer_switching_costs).

% DUAL FORMULATION NOTE:
% The incumbent industry moat is upstream of specific market-structure constraints (regulatory capture, switching costs, network effects) but represents a distinct structural phenomenon: the aggregate effect of multiple enforcement mechanisms protecting incumbent advantage. The upstream constraints have their own extractiveness values reflecting specific barrier types; the moat aggregates these into a single institutional structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
