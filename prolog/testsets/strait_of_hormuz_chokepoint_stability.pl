% ============================================================================
% CONSTRAINT STORY: strait_of_hormuz_chokepoint_stability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_strait_of_hormuz_chokepoint_stability, []).

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
 *   constraint_id: strait_of_hormuz_chokepoint_stability
 *   human_readable: Strait of Hormuz Chokepoint Stability
 *   domain: geopolitical/infrastructure/maritime
 *
 * SUMMARY:
 *   The Strait of Hormuz represents a critical maritime chokepoint through
 *   which 21% of globally traded petroleum and approximately 30% of seaborne
 *   LNG transits annually. This constraint exhibits a fundamental tension
 *   between geopolitical reality (Iran controls the northern shore and can
 *   threaten shipping) and structural necessity (no economically viable
 *   alternative exists for the majority of this volume in the near-to-medium
 *   term). The constraint operates as a tangled rope: genuine coordination
 *   problems exist (preventing accidents, managing congestion, enforcing
 *   safety standards), but they are inseparable from extraction mechanisms
 *   (Iran's threat capacity extracts geopolitical concessions; US naval
 *   presence extracts petrodollar hegemony; oil producers extract price
 *   premiums through disruption risk pricing). The theater ratio has declined
 *   over the interval as institutions have shifted from performing 'normalcy'
 *   (IMO patrols, insurance classifications) toward more direct threat
 *   management (naval deployments, drone monitoring, explicit deterrence
 *   messaging). This is a diagnostic case of how chokepoint power emerges not
 *   from immutable geography but from a specific configuration of energy
 *   dependence, geopolitical rivalry, and hegemon enforcement mechanisms —
 *   all contingent on the energy transition pathway.
 *
 * KEY AGENTS:
 *   - Global Energy Market: Primary victim (powerless/trapped) — 21% of global oil, 30% of LNG dependent on Hormuz; no exit option; bears full cost of disruption risk pricing
 *   - Iran: Primary extractor (organized/constrained) — controls northern shore; extracts geopolitical compliance and economic concessions through closure threat; also constrained by own export dependence and military asymmetry
 *   - United States Navy: Beneficiary (institutional/arbitrage) — extracts geopolitical influence and regional dominance through military presence; maintains freedom of navigation for allies; arbitrage options enable redeployment or disengagement
 *   - Oil Exporting Producers (Saudi Arabia, UAE, Iraq): Beneficiary with coordination role (powerful/arbitrage) — benefit from high energy prices during disruption risk; provide coordination (managing production, maintaining investment); active enforcement required to prevent excessive disruption
 *   - Littoral States (Oman, UAE): Mixed position (organized/constrained) — constrained by military capacity and geopolitical risk; benefit from transit infrastructure, fees, and regional economic activity; require coordination with Iran and US Navy
 *   - International Maritime Institutions (IMO, insurers, port authorities): Institutional theater maintainers (institutional/mobile) — maintain performative safety infrastructure; shift from normalcy performance to explicit risk pricing as chokepoint tension increases
 *   - Alternative Energy Transition Interests: Long-term beneficiary (organized/constrained) — benefit from Hormuz stability during transition window; constrained by technical deployment rates; seek to reduce chokepoint dependency through pipeline bypasses and energy diversification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(strait_of_hormuz_chokepoint_stability, 0.58).
domain_priors:suppression_score(strait_of_hormuz_chokepoint_stability, 0.72).
domain_priors:theater_ratio(strait_of_hormuz_chokepoint_stability, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strait_of_hormuz_chokepoint_stability, extractiveness, 0.58).
narrative_ontology:constraint_metric(strait_of_hormuz_chokepoint_stability, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(strait_of_hormuz_chokepoint_stability, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(strait_of_hormuz_chokepoint_stability, tangled_rope).
narrative_ontology:human_readable(strait_of_hormuz_chokepoint_stability, "Strait of Hormuz Chokepoint Stability").
narrative_ontology:topic_domain(strait_of_hormuz_chokepoint_stability, "geopolitical/infrastructure/maritime").

domain_priors:requires_active_enforcement(strait_of_hormuz_chokepoint_stability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(strait_of_hormuz_chokepoint_stability, gulf_shipping_hegemonic_power).
narrative_ontology:constraint_beneficiary(strait_of_hormuz_chokepoint_stability, downstream_energy_consumers_indirect).
narrative_ontology:constraint_victim(strait_of_hormuz_chokepoint_stability, gulf_littoral_states).
narrative_ontology:constraint_victim(strait_of_hormuz_chokepoint_stability, global_energy_price_stability).
narrative_ontology:constraint_victim(strait_of_hormuz_chokepoint_stability, shipping_industry_vulnerability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL ENERGY MARKET (SNARE) — Cannot exit the chokepoint dependency; 21% of global petroleum transits through Hormuz with no viable alternative. Bears full cost of disruption risk (price volatility, supply shock exposure). No countervailing power or exit option. Maximum extraction from a trapped collective actor.
constraint_indexing:constraint_classification(strait_of_hormuz_chokepoint_stability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: ALTERNATIVE ENERGY TRANSITION (SNARE) — Long-term energy diversification away from oil is structurally dependent on Hormuz stability during the transition window (30-50 years). The transition cannot accelerate past the technical deployment rate. Trapped by temporal constraint even as alternatives are being built. Bears extraction through price volatility risk during transition.
constraint_indexing:constraint_classification(strait_of_hormuz_chokepoint_stability, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: LITTORAL STATE — IRAN/OMAN/UAE (TANGLED ROPE) — Constrained by military capacity and geopolitical risk; cannot unilaterally control or close the strait. But also benefits from transit fees, shipping infrastructure, and regional economic activity dependent on strait stability. Genuine coordination function (preventing accidents, managing traffic) exists alongside asymmetric extraction through threat leverage and transit toll extraction.
constraint_indexing:constraint_classification(strait_of_hormuz_chokepoint_stability, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: HEGEMONIC NAVAL POWER — US NAVY (ROPE) — Extracts geopolitical influence and regional dominance through military presence guaranteeing freedom of navigation. Experiences the constraint as coordination: the US navy's role is legitimized as maintaining the shipping safety commons. Net beneficiary through arbitrage options (can redeploy, can disengage, can shift costs to allies). Sees the constraint as a coordination function.
constraint_indexing:constraint_classification(strait_of_hormuz_chokepoint_stability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OIL EXPORTING PRODUCERS (TANGLED ROPE) — Benefit from high oil prices during supply-side constraints (Hormuz disruption risk is priced into energy costs, inflating export revenue). But face genuine coordination problem: excessive disruption risk tanks long-term investment in export capacity and refinery infrastructure. Active enforcement (military presence, treaties, norms) is required to balance extraction benefits with collective stability.
constraint_indexing:constraint_classification(strait_of_hormuz_chokepoint_stability, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: INTERNATIONAL MARITIME INSTITUTIONS (PITON) — IMO, port authorities, and shipping insurance regimes maintain a performative stability infrastructure (regulations, insurance classes, reporting requirements) that has become increasingly theatrical. The actual safety function (preventing accidents) is now subordinate to the theater of compliance and risk pricing. Theater ratio indicates institutions persist through inertia as alternatives (redundant pipelines, energy transition) reduce the functional necessity of the chokepoint stability regime.
constraint_indexing:constraint_classification(strait_of_hormuz_chokepoint_stability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, chokepoint vulnerability is framed as an inherent property of maritime trade routes: any critical infrastructure has vulnerability. This perspective naturalizes what the structural data reveals as contingent: the dependence is real, but the extraction is not immutable. The engine will flag this as a false summit — the 'natural law' framing obscures policy choices about pipeline alternatives, energy transition incentives, and naval hegemony.
constraint_indexing:constraint_classification(strait_of_hormuz_chokepoint_stability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strait_of_hormuz_chokepoint_stability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(strait_of_hormuz_chokepoint_stability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(strait_of_hormuz_chokepoint_stability, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(strait_of_hormuz_chokepoint_stability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(strait_of_hormuz_chokepoint_stability, TR),
    TR >= 0.70.

:- end_tests(strait_of_hormuz_chokepoint_stability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Iran's threat capacity to disrupt global energy markets is non-zero and credible (demonstrated in tanker war, mine-laying, drone attacks), yet Iran does not exercise maximum closure because its own oil exports depend on Hormuz transiting. The constraint equilibrium is one of partial disruption risk pricing — energy markets pay a premium (risk surcharge in oil prices, insurance costs) to Iran's threat capacity without experiencing total closure. The extractiveness value reflects this constrained extraction: real enough to affect global prices and investment decisions, but bounded by Iran's own economic dependence. Suppression (0.72): High. Alternatives (pipeline bypasses, overland routes, alternative suppliers) exist but are capital-intensive, take 5-10 years to build, and face geopolitical obstacles (Saudi-Iran rivalry, Turkey tensions, Israeli concerns). Littoral states cannot unilaterally resist the chokepoint because they lack naval capacity to enforce alternative arrangements. Global market cannot exit because demand destruction takes decades. Suppression reflects both material barriers (geography, capital requirements) and geopolitical constraints (inability to resolve Iran-US rivalry). Theater ratio (0.48): Moderate and declining. IMO regulations, insurance classifications, and naval patrols perform 'safety management,' but the actual constraint mechanism is geopolitical threat, not maritime safety failure. As threat perception rises (Iranian drone incidents, Saudi tanker attacks, Houthis), institutions shift from normalcy theater toward explicit risk management and deterrence. Lower theater indicates this constraint is becoming more directly extractive (less performative, more coercive).
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon generates radically different classifications depending on observer position. The trapped global energy market sees pure extraction (snare) — they pay a premium, have no exit, no coordination benefit. Iran sees mixed coordination and extraction (tangled rope) — it benefits from the status quo (regional leverage, export capability) while facing military constraints that prevent maximum extraction. The US Navy sees pure coordination (rope) — the constraint's existence legitimizes its regional presence and enables it to enforce petrodollar hegemony. Oil producers see coordination with extraction (tangled rope) — they benefit from high prices but face coordination burden of preventing excessive disruption that would tank investment. International institutions see degraded ritual (piton) — their safety regimes matter less than geopolitical threat management. The analytical observer risks false naturalization (mountain) — 'geography determines chokepoint power' — obscuring the contingency: energy transition or pipeline alternatives would shift classification toward scaffold (temporary) or rope (pure coordination). The perspectival gap reveals that Hormuz stability is not a law of nature but a configuration of technology (energy dependence), geography (no alternatives), and politics (Iran-US rivalry, US hegemonic enforcement).
 *
 * DIRECTIONALITY LOGIC:
 *   Iran's directionality is derived from beneficiary status (extracts concessions, maintains regional leverage through closure threat) combined with constrained exit (economically dependent on Hormuz, militarily inferior to US Navy, subject to sanctions escalation). This maps to d ~0.65, producing chi ~0.50-0.58 at the organizational level. The global energy market's directionality is derived from victim status (bears disruption risk, pays price premiums) combined with trapped exit (no alternatives exist at scale within relevant timeframe). This maps to d ~0.95, producing chi ~0.72-0.82 at the powerless level. US Navy's directionality is derived from beneficiary status (extracts geopolitical dominance, enforces petrodollar hegemony) combined with arbitrage exit (can maintain, withdraw, or redeploy). This maps to d ~0.25, producing negative chi ~-0.10-0.15, indicating that from the navy's perspective, the constraint produces net benefit. Directionality overrides are not needed: the baseline derivation chain (beneficiary/victim + exit options) correctly captures structural positions. The constraint's extractiveness ceiling is set by Iran's rational actor constraint: full closure would trigger sanctions escalation and military response that exceed Iran's payoff from disruption. This bounded extraction explains why extractiveness is 0.58 rather than 0.75+ — the mechanism is powerful but constrained by the beneficiary-with-constraints' own incentive structure.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint classifies as tangled rope from the beneficiary-enforcer perspective (Iran/US Navy) and snare from the victim perspective (global energy market), with credible scaffold possibility if energy transition accelerates. The mandatrophy arises from the question: 'Is Hormuz a coordination problem or an extraction mechanism?' The structural data shows both: genuine coordination problems exist (preventing accidents, managing traffic, enforcing safety), but they are inseparable from extraction (Iran's threat capacity, US hegemony, price risk premiums). The resolution is that this is not a false mandatrophy — the constraint genuinely is hybrid. What would resolve it into pure extraction (snare) is energy transition acceleration or pipeline bypass completion, which would remove the coordination function and leave only the threat/hegemony extraction. What would resolve it into pure coordination (rope) is Iran-US diplomatic normalization or multilateral maritime authority that removes the threat asymmetry. The current equilibrium is authentic hybrid: Iran extracts from the constraint (geopolitical leverage, price premiums for its own exports) while also participating in coordination (maintaining exports, avoiding total closure). The analytical observer's false summit (mountain: 'geography determines chokepoints') is a mislabeling of what is actually contingent policy and technology. Pipeline alternatives and energy transition timelines will resolve this classification within 20-30 years: if alternatives mature faster than the constraint's extraction capacity increases, classification shifts toward rope/scaffold; if extraction accumulates faster than alternatives develop, it persists as snare/tangled_rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suez_lessons_transferability,
    'Do historical Suez Canal disruptions (1956, 1967) and recent blockage (2021) demonstrate that Hormuz risk is structurally inevitable or contingent on specific geopolitical actors?',
    'Comparative analysis: Suez blockages were temporary (months to years) and resolved through diplomatic/military intervention. Hormuz disruption scenarios assume persistent Iranian closure (years). Empirical test: does the difference reflect geography (Suez has overland alternative, Hormuz does not) or actor behavior (Egypt vs Iran threat posture)?',
    'If geographic: Hormuz is mountain-adjacent (constraint emerges from geography). If behavioral: Hormuz is snare/tangled_rope (constraint emerges from extractive actor incentives). Classification shifts under resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suez_lessons_transferability, empirical, 'Whether Hormuz chokepoint vulnerability is geographic or behavioral').

omega_variable(
    pipeline_capacity_sufficiency,
    'Can existing + planned pipeline infrastructure (Saudi Aramco East-West pipeline, UAE pipelines, Iraq-Turkey pipeline) bypass sufficient Hormuz-dependent volume to materially reduce the chokepoint''s extraction power?',
    'Capacity analysis: current Hormuz flow (21% global oil + 30%+ LNG by some estimates) vs pipeline bypass capacity. Monitor investment and completion rates as alternatives mature. If bypass capacity reaches 30-40% of current Hormuz flow, effective chokepoint power drops materially.',
    'High pipeline bypass capacity: snare classification weakens toward rope/scaffold (alternatives provide exit). Low bypass: snare persists (trapped dependency). This is the primary vector for mandatrophy resolution.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(pipeline_capacity_sufficiency, empirical, 'Pipeline bypass capacity for Hormuz-dependent oil flows').

omega_variable(
    naval_presence_necessity,
    'Is US Navy presence in the Persian Gulf a genuine coordination function (preventing accidents, deterring piracy, maintaining insurance viability) or a mechanism for extracting geopolitical compliance and petrodollar flows?',
    'Counterfactual analysis: accident rates, piracy events, and insurance costs under different naval deployment scenarios. Compare Hormuz region vs other high-traffic chokepoints (Malacca Strait, English Channel) with different naval postures. Analyze cost-benefit of US naval presence vs alternative coordination mechanisms (international maritime authority, littoral state navies, autonomous systems).',
    'If coordination: navy presence is legitimate; rope classification appropriate; benign extraction. If extraction: navy is enforcing petrodollar hegemony; snare/tangled_rope appropriate; mechanism for US to extract geopolitical rent. Resolves perspectival gap between beneficiary and powerless agents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naval_presence_necessity, empirical, 'Whether naval presence provides coordination or extraction').

omega_variable(
    energy_transition_timeline,
    'At what rate is global energy transition reducing oil demand and thus Hormuz-dependent flows? Will transition velocity exceed chokepoint re-extraction accumulation?',
    'Longitudinal: track Hormuz-dependent oil share of global energy (currently ~18-21% of petroleum). Model transition scenarios (net-zero by 2050, 2040, 2070). If transition velocity > extraction risk acceleration, chokepoint becomes temporary (scaffold). If extraction risk grows faster, chokepoint becomes permanent (snare).',
    'Slow transition (net-zero 2070+): chokepoint remains trapped dependency. Fast transition (net-zero 2045-): chokepoint becomes sunset-bounded constraint. Affects mandatrophy resolution timeline.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(energy_transition_timeline, empirical, 'Energy transition velocity relative to chokepoint extraction').

omega_variable(
    iran_rational_actor_assumption,
    'Does Iran''s incentive structure make complete Hormuz closure rational? Or is Iran''s threat capacity better modeled as bargaining leverage within a constrained range (partial disruption, mines, harassment)?',
    'Game-theoretic analysis: Iran''s revenue dependence on Hormuz (own oil exports + transit fees). Cost analysis of complete closure (sanctions escalation, military response, economic collapse). Model Iran''s payoff matrix under full closure vs partial disruption vs status quo. Compare to historical behavior (tanker war, mine laying, drone attacks) which show constrained disruption rather than total closure.',
    'If full closure rational: maximum snare scenario. If constrained disruption: snare becomes tangled_rope (Iran benefits from status quo enough to maintain coordination). Affects extraction ceiling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(iran_rational_actor_assumption, empirical, 'Iran''s strategic incentive structure for Hormuz disruption').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(strait_of_hormuz_chokepoint_stability, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hormuz_tr_t0, strait_of_hormuz_chokepoint_stability, theater_ratio, 0, 0.62).
narrative_ontology:measurement(hormuz_tr_t10, strait_of_hormuz_chokepoint_stability, theater_ratio, 10, 0.55).
narrative_ontology:measurement(hormuz_tr_t20, strait_of_hormuz_chokepoint_stability, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(hormuz_be_t0, strait_of_hormuz_chokepoint_stability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(hormuz_be_t10, strait_of_hormuz_chokepoint_stability, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(hormuz_be_t20, strait_of_hormuz_chokepoint_stability, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(strait_of_hormuz_chokepoint_stability, global_infrastructure).
narrative_ontology:affects_constraint(strait_of_hormuz_chokepoint_stability, global_oil_price_stability).
narrative_ontology:affects_constraint(strait_of_hormuz_chokepoint_stability, middle_east_geopolitical_rivalry).
narrative_ontology:affects_constraint(strait_of_hormuz_chokepoint_stability, energy_transition_timeline).
narrative_ontology:affects_constraint(strait_of_hormuz_chokepoint_stability, us_hegemonic_naval_presence).

% DUAL FORMULATION NOTE:
% Hormuz chokepoint power derives from three interdependent constraints: energy dependence (demand side), geographic bottleneck (supply side), and geopolitical rivalry (actor incentives). Each has separate ε: energy_demand_oil_dependence (ε~0.45, rope — coordination of supplier/consumer relationship), geographic_chokepoint_topology (ε~0.20, mountain — emerges naturally from coastline), geopolitical_iran_us_rivalry (ε~0.65, snare — extraction through threat capacity). The Hormuz chokepoint constraint is downstream of all three: it exists at the intersection. Decomposition would separate the structural inevitability (geography) from the behavioral contingency (geopolitics), but the present story treats them as unified because they cannot be separated operationally — Hormuz's extraction power requires all three components.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
