% ============================================================================
% CONSTRAINT STORY: us_venezuela_oil_pressure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_venezuela_oil_pressure, []).

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
 *   constraint_id: us_venezuela_oil_pressure
 *   human_readable: US Geopolitical & Economic Pressure on Venezuela's Oil Sector
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The US pressure on Venezuela's oil sector represents a decades-long
 *   constraint that solves genuine coordination problems (preventing
 *   oil-market destabilization from state collapse, excluding hostile
 *   geopolitical competitors from hemisphere resource access) while
 *   simultaneously extracting asymmetric wealth and geopolitical advantage.
 *   This is textbook tangled rope: the coordination and extraction mechanisms
 *   are structurally fused — the same embargo that prevents Venezuelan
 *   dumping destabilizes Venezuela; the same sanctions that exclude Russian
 *   competition concentrate US geopolitical power; the same capital
 *   restrictions that prevent capital flight also prevent productive
 *   investment in oil infrastructure. The constraint cannot be decomposed.
 *   Venezuela's powerless population faces maximum extraction (snare
 *   perspective) because they cannot exit a resource-dependent economy whose
 *   primary export is controlled by external actors. The Venezuelan state
 *   faces constrained extraction (snare perspective) because sanctions block
 *   the capital and technology access needed for any alternative path. US
 *   corporate interests experience coordination (rope perspective) — the
 *   constraint ensures market access and excludes competitors. Organized
 *   global competitors face mixed coordination-extraction (tangled rope
 *   perspective) — price stability benefits them, but US hegemonic
 *   enforcement disadvantages them. The international legal architecture (UN,
 *   OAS, sanctions law) has degraded into inertial theater (piton
 *   perspective): humanitarian exemptions are performative, legitimacy claims
 *   are decoupled from actual enforcement. The theater ratio has increased
 *   over 30 years as the formal justifications (democracy promotion,
 *   humanitarian concern) have grown more divorced from the substantive
 *   mechanism (resource control, geopolitical leverage). The extractiveness
 *   has increased from 0.45 to 0.72 as sanctions have tightened and oil
 *   infrastructure has degraded. The analytical observer sees authentic
 *   tangled rope: the constraint simultaneously prevents worse outcomes and
 *   extracts rent; both cannot be removed without destroying the other.
 *
 * KEY AGENTS:
 *   - Venezuelan Population: Primary victim (powerless/trapped) — bears costs of hyperinflation, capital flight, humanitarian crisis; cannot exit resource-dependent economy
 *   - Venezuelan State / PDVSA: Primary victim (moderate/constrained) — loses oil export revenue, cannot access capital or technology; constrained exit
 *   - US Corporate Oil Interests: Primary beneficiary (institutional/arbitrage) — gains market access, competitive advantage, downstream control; arbitrage exit available
 *   - US State Department & Geopolitical Strategy: Institutional beneficiary (institutional/constrained) — maintains regional leverage and energy-security control; constrained exit from energy-security framework
 *   - Global Oil Market & Competitors: Secondary stakeholder (organized/mobile) — benefits from price stability, harmed by US hegemonic control; mobile exit via renewables transition
 *   - International Legal Architecture: Institutional actor (organized/constrained) — maintains formal legitimacy through performative humanitarian concern; constrained by US structural power
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — recognizes both coordination necessity and extraction mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_venezuela_oil_pressure, 0.72).
domain_priors:suppression_score(us_venezuela_oil_pressure, 0.68).
domain_priors:theater_ratio(us_venezuela_oil_pressure, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_venezuela_oil_pressure, extractiveness, 0.72).
narrative_ontology:constraint_metric(us_venezuela_oil_pressure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_venezuela_oil_pressure, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_venezuela_oil_pressure, tangled_rope).
narrative_ontology:human_readable(us_venezuela_oil_pressure, "US Geopolitical & Economic Pressure on Venezuela's Oil Sector").
narrative_ontology:topic_domain(us_venezuela_oil_pressure, "geopolitical/economic").

domain_priors:requires_active_enforcement(us_venezuela_oil_pressure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_venezuela_oil_pressure, us_corporate_oil_interests).
narrative_ontology:constraint_beneficiary(us_venezuela_oil_pressure, us_geopolitical_position).
narrative_ontology:constraint_victim(us_venezuela_oil_pressure, venezuelan_state_revenue).
narrative_ontology:constraint_victim(us_venezuela_oil_pressure, venezuelan_population).
narrative_ontology:constraint_victim(us_venezuela_oil_pressure, global_petro_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VENEZUELAN POPULATION (SNARE) — Trapped within a resource-dependent economy whose primary export is subject to external embargo and coercive pressure. No exit option: cannot diversify economy rapidly, cannot trade oil freely, cannot escape currency collapse driven by lost export revenue. Experiences maximum extraction through hyperinflation, capital flight, and humanitarian crisis. The constraint channels petro-wealth extraction into political control and reduces alternatives to compliance.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VENEZUELAN STATE / PDVSA (SNARE) — Constrained by sanctions limiting capital access, technology transfer, and customer base. Refineries degrade without replacement parts. Export capacity collapses. Extractiveness is severe: the state's primary revenue source becomes a liability. Exit options exist in theory (diversify, restore production through capital investment) but are blocked by the constraint itself — embargoes prevent the very capital imports needed for diversification. Constrained exit coupled with victim status produces high effective extraction.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: US CORPORATE OIL INTERESTS (ROPE) — Beneficiary perspective. The constraint creates profitable coordination: sanctions exclude competitors (Russian, Chinese, European firms) from Venezuelan oil deals while protecting US firms' downstream operations (refineries, distribution). For US majors, the constraint is experienced as beneficial coordination ensuring access, market control, and political leverage. Exit options are arbitrage: they can shift capital to other projects and regions if Venezuelan access declines, losing opportunity cost but not catastrophically. Net beneficiary position produces low-to-negative effective extractiveness from this perspective.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: GLOBAL OIL MARKET & COMPETITORS (TANGLED ROPE) — Organized international actors (OPEC, Chinese/Russian energy firms, European traders) experience the constraint as both coordination and extraction. The constraint partially coordinates: it removes a destabilizing wild card (Venezuelan crude dumping) from global markets, reducing price volatility. But it is also extractive: US hegemony over oil-regime enforcement provides US firms and policy with asymmetric advantage. Competitors have mobile exit (divest from oil, shift to renewables, trade elsewhere) but face medium-term lock-in (infrastructure, capital). Mixed classification reflects genuine coordination benefit coupled with asymmetric power enforcement.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: US STATE DEPARTMENT / GEOPOLITICAL STRATEGY (TANGLED ROPE) — Institutional actor with constrained exit from energy-security frameworks. Experiences the constraint as both coordination (maintains Western oil supply chains, excludes Russian/Chinese influence from hemisphere) and extraction (costs: international legitimacy, alliance management, humanitarian concern). The US maintains the constraint partly for genuine energy-security coordination and partly for asymmetric geopolitical extraction (control of an adversary state's economic capacity). Constrained because backing away risks loss of regional leverage and resource access; enforcement remains active because the coordination benefits (denying resources to rivals) outweigh the reputational costs. This is tangled rope at full complexity: real coordination necessity, real extraction mechanism, both simultaneously active.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTERNATIONAL LEGAL & SANCTIONS ARCHITECTURE (PITON) — The formal institutions (UN, OAS, international law consensus) that once provided legitimacy for state sovereignty have degraded into theater: sanctions persist despite weak legal basis, humanitarian exemptions are performative (not actually granted), international court opinions are ignored. The architecture maintains enforcement through institutional inertia (OFAC regulations, Treasury designation procedures) and US structural power rather than through legitimate consensus. Theater ratio is high because the legal form (democratic sanctions, humanitarian concern, democracy promotion) masks the substantive mechanism (resource control, geopolitical exclusion). The architecture persists because alternatives haven't replaced it, not because it functions. Piton classification: former coordination (humanitarian law, sovereignty norms) degraded into inertial enforcement theater.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational perspective, the constraint solves a genuine coordination problem: Venezuela's state collapse and production decline would destabilize global energy markets and create humanitarian crisis with unpredictable spillovers. Containment prevents worse outcomes. However, the containment mechanism is simultaneously extractive: US enforcement ensures that Venezuelan petro-assets either remain inaccessible or become available only on US terms. The same mechanism that coordinates (prevents market chaos) also extracts (channels wealth and geopolitical advantage). Both are structurally necessary to the constraint — it cannot be decomposed into pure coordination plus pure extraction. This is authentic tangled rope: two irreconcilable goods and harms fused at the structural level.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_venezuela_oil_pressure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_venezuela_oil_pressure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_venezuela_oil_pressure, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_venezuela_oil_pressure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_venezuela_oil_pressure, TR),
    TR >= 0.70.

:- end_tests(us_venezuela_oil_pressure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.72): High and increasing. The constraint extracts from Venezuela in multiple forms: lost oil revenue (primary), constrained capital access, degraded industrial capacity, population bearing humanitarian costs. The extraction is not total (some Venezuelan oil still moves through informal channels; Venezuela retains nominal sovereignty) but is severe and multi-layered. The increase from 0.45 to 0.72 over 30 years reflects tightening sanctions, accumulating infrastructure decay, and intensifying humanitarian crisis. Suppression (0.68): High. Venezuelans face severe barriers to alternatives: they cannot trade oil freely (sanctions), cannot access capital markets (excluded by US Treasury), cannot invest in infrastructure (technology embargoes), cannot even export food without regulatory complexity. The suppression is both structural (legal barriers) and practical (capital unavailability). However, suppression is not total (some sanctions have humanitarian exemptions, though performative) — hence 0.68 rather than 0.85. Theater ratio (0.55): Moderate-high and increasing. The formal justifications (democracy promotion, humanitarian concern, regional stability) are performative — the actual mechanism is resource control and geopolitical leverage. Humanitarian exemptions exist on paper but are rarely granted. Democratic concerns are selective (applied to Venezuela but not to allied autocracies). Theater has increased because the gap between stated and actual goals has widened over time as regime change has become less likely and containment more permanent. The theater is not extreme (0.55 rather than 0.80) because the coordination mechanism is real — the constraint does prevent market chaos — even if the justifications are decorative.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is vast and reveals the constraint's deep incoherence. For the Venezuelan population and state, this is pure snare: extraction without coordination benefit, forced into a regime they did not choose and cannot exit. For US corporate interests, it is pure rope: beneficial coordination with competitors excluded and market access secured. For the US geopolitical apparatus, it is tangled rope: genuine energy-security coordination (preventing Venezuelan chaos from destabilizing markets) fused with geopolitical extraction (maintaining US hemisphere dominance). For organized global competitors, it is tangled rope with harms emphasized: they get price stability but lose access and face US-imposed asymmetry. For the international legal system, the constraint is piton: the formal architecture (humanitarian law, sovereignty norms, international courts) persists through institutional inertia while the actual mechanism ignores these norms. The analytical observer synthesizes this into authentic tangled rope: both the coordination and extraction are structurally real. You cannot remove the extraction without collapsing the coordination (lifting sanctions destabilizes oil markets). You cannot preserve the coordination without accepting the extraction (stabilizing prices requires controlling access). This is the hallmark of true tangled rope — the two functions are inseparable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) differ sharply across agents based on their structural position. Venezuelan population: d ≈ 0.95 (near-full target) — victim with trapped exit. Venezuelan state: d ≈ 0.85 (high target) — constrained exit, victim status. US corporate interests: d ≈ 0.10 (beneficiary) — arbitrage exit, beneficiary status. US state apparatus: d ≈ 0.40 (organized moderate) — constrained exit, beneficiary status with coordination component. Global competitors: d ≈ 0.60 (moderate target) — mobile exit but medium-term lock-in. International legal system: d ≈ 0.55 (near-symmetric) — neither full beneficiary nor victim; constrained by US power. The f(d) sigmoid applies to these values to compute experienced extractiveness chi. Trapped agents experience maximum chi; arbitrage agents experience near-zero or negative chi. This directionality gap explains why the constraint appears as snare to Venezuela and rope to the US — it is not that either is wrong, but that their d values place them at opposite ends of the extraction curve.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED (extractiveness 0.72 > 0.70): The constraint avoids mislabeling through indexical analysis. The mandatrophy question is: 'Is this coordination masquerading as extraction, or extraction masquerading as coordination?' The answer is neither — it is both, structurally inseparable. At the analytical level, the constraint is tangled rope because both the coordination function (preventing market chaos) and the extraction mechanism (asymmetric geopolitical advantage) are real and necessary to the constraint's operation. The coordination is not decoration on extraction; it is functionally required. Market destabilization would harm the US as well as global actors. The extraction is not incidental to coordination; it is the mechanism by which coordination is enforced and the beneficiary is selected. The US could theoretically stabilize oil markets through multilateral coordination or by facilitating Venezuelan market access, but those alternatives would not provide the same geopolitical leverage. Hence tangled rope: the constraint contains both a genuine coordination function (energy stability) and an extraction mechanism (resource control), both structurally active. The constraint is not pure extraction (snare) because the Venezuelan population would face worse outcomes if global oil markets destabilized and supply chains fractured. The constraint is not pure coordination (rope) because the US derives geopolitical and economic advantage disproportionate to the global coordination benefit. The mandatrophy is resolved by recognizing that authentic tangled ropes exist: some constraints are genuinely hybrid, and the classification should preserve this rather than collapse it into one or the other.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    humanitarian_exemption_authenticity,
    'Do humanitarian exemptions to oil sanctions represent genuine policy commitment or performative theater designed to claim moral justification while maintaining extraction?',
    'Audit of actual humanitarian exemptions granted vs requested; timeline analysis of exemption approval relative to humanitarian need indicators; cross-comparison with historical exemption patterns in other sanctions regimes',
    'If authentic: constraint is genuine tangled rope with real coordination-extraction hybrid. If performative: constraint degrades toward pure snare (extraction masked by humanitarian rhetoric). Classification boundary between tangled_rope and snare shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_exemption_authenticity, empirical, 'Whether humanitarian exemptions are functionally granted or merely performative').

omega_variable(
    alternative_regime_stability,
    'Would a Venezuelan state under US-aligned governance produce genuinely different outcomes in global oil stability and geopolitical risk, or would it simply transfer wealth extraction to US-aligned actors?',
    'Comparative analysis of post-intervention outcomes in other oil-dependent states (Iraq, Libya); modeling of counterfactual Venezuelan governance scenarios; examination of whether regime change would durably alter market volatility or merely shift control',
    'If alternative regime would genuinely stabilize: constraint is justifiable tangled rope with real coordination benefit. If it would simply transfer extraction: constraint is tangled rope with coordination benefit overstated (closer to pure snare than analysis suggests). Shifts the weight of coordination vs extraction in classification logic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_regime_stability, conceptual, 'Whether regime change would produce genuine stability gains or merely shift control').

omega_variable(
    renewables_transition_exit,
    'At what trajectory of renewable energy adoption and oil demand decline does the US geopolitical advantage from Venezuelan oil pressure become negligible, enabling genuine constraint exit?',
    'Energy demand modeling under decarbonization scenarios; timeline analysis of when Venezuelan oil''s share of global supply becomes strategically irrelevant; correlation between renewable adoption rates and US policy shift toward Venezuela',
    'If transition horizon is <10 years: constraint is temporary scaffold disguised as permanent tangled rope. If >30 years: constraint is intergenerational tangled rope with long-term structural extraction. Classification confidence depends on energy transition timing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewables_transition_exit, empirical, 'Timeline to oil irrelevance and constraint exit').

omega_variable(
    russian_chinese_alternative,
    'If US pressure is removed, would Venezuelan oil access become leverage for Russian or Chinese geopolitical extraction instead, simply shifting the beneficiary of the tangled rope?',
    'Analysis of Venezuelan debt structures and Chinese/Russian capital control; historical precedent from other resource-dependent states under non-US hegemon pressure; modeling of how Venezuelan governance capacity would change under alternative sponsor regimes',
    'If yes: constraint is ''tangled rope is inevitable without US version'' scenario — coordination logic is real but beneficiary is contingent. If no: constraint becomes optional and the tangled rope classification weakens. Shifts the ''mandate'' question: is this about oil stability or about whose geopolitical position extracts the rent?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(russian_chinese_alternative, conceptual, 'Whether constraint is inevitable in some form or merely channeling extraction to US interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_venezuela_oil_pressure, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(usvop_tr_t0, us_venezuela_oil_pressure, theater_ratio, 0, 0.35).
narrative_ontology:measurement(usvop_tr_t15, us_venezuela_oil_pressure, theater_ratio, 15, 0.48).
narrative_ontology:measurement(usvop_tr_t30, us_venezuela_oil_pressure, theater_ratio, 30, 0.55).

% Extraction over time
narrative_ontology:measurement(usvop_be_t0, us_venezuela_oil_pressure, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(usvop_be_t15, us_venezuela_oil_pressure, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(usvop_be_t30, us_venezuela_oil_pressure, base_extractiveness, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_venezuela_oil_pressure, resource_allocation).
narrative_ontology:affects_constraint(us_venezuela_oil_pressure, global_oil_supply_stability).
narrative_ontology:affects_constraint(us_venezuela_oil_pressure, us_hemispheric_dominance).
narrative_ontology:affects_constraint(us_venezuela_oil_pressure, petro_state_institutional_collapse).

% DUAL FORMULATION NOTE:
% This constraint is downstream of structural US geopolitical dominance and upstream of Venezuelan state collapse. The global oil supply stability constraint models the market-level effects (price volatility, supply security); the hemispheric dominance constraint models the strategic intent; the petro-state collapse constraint models the outcome at Venezuelan institutional level. All three share feedback loops with this constraint: oil pressure worsens institutional capacity, which increases need for pressure, which increases geopolitical advantage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
