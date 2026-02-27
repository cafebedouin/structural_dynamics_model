% ============================================================================
% CONSTRAINT STORY: us_embargo_cuba
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_embargo_cuba, []).

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
 *   constraint_id: us_embargo_cuba
 *   human_readable: US Embargo of Cuba
 *   domain: political/economic
 *
 * SUMMARY:
 *   The US embargo of Cuba (initiated 1962, formalized through Helms-Burton
 *   Act 1996) represents a long-standing structural constraint that operates
 *   simultaneously as geopolitical strategy, domestic political coalition
 *   mechanism, and extraction apparatus. The constraint exhibits high
 *   suppression (0.78) due to legal barriers, enforcement mechanisms (OFAC
 *   regulations), and lack of alternative market access; moderate-high
 *   extractiveness (0.68) reflecting sustained economic opportunity costs for
 *   Cuban actors; and increasing theater ratio (0.30 → 0.55 over interval)
 *   indicating that the constraint's function has shifted from active Cold
 *   War tool toward performative institutional maintenance. The embargo is a
 *   diagnostic exemplar for tangled_rope classification: it coordinates US
 *   political actors around a shared geopolitical objective while
 *   simultaneously extracting from Cuban citizens through trade restrictions,
 *   medical supply shortages, and capital access denial. The constraint
 *   resolves mandatrophy by demonstrating that the coordination function (US
 *   coalition alignment) and extraction function (Cuban economic damage) are
 *   both structurally real — they are not in tension, but rather, the
 *   coordination mechanism IS the extraction apparatus. For the Cuban
 *   government, the embargo serves as both constraint and organizing
 *   principle for domestic politics; for the Cuban-American lobby, it is a
 *   source of political leverage; for the US exporters, it is a market-access
 *   restriction; for the Cuban population, it is a pure snare with no exit
 *   option. The theater ratio has increased over the interval as embargo
 *   enforcement has become more ritualistic (global markets ignore US
 *   restrictions; enforcement targets offshore subsidiaries and visa
 *   violations more than actual trade) while original Cold War objectives
 *   have faded, indicating incipient piton degradation.
 *
 * KEY AGENTS:
 *   - Cuban Population: Primary victim (powerless/trapped) — bears economic costs of trade restrictions, medical access denial, capital starvation; no exit option
 *   - Cuban Government: Secondary beneficiary + victim (organized/constrained) — benefits from embargo as organizing narrative; constrained by economic damage; uses constraint for domestic political control
 *   - US Embargo Coalition: Primary beneficiary (institutional/arbitrage) — coordinates US political actors around shared geopolitical objective; benefits from coalition maintenance
 *   - Cuban-American Exile Lobby: Primary beneficiary (institutional/arbitrage) — maintains political influence and community cohesion through embargo; benefits from restoration claims
 *   - US Exporters and Investors: Secondary victim (moderate/constrained) — face market-access restrictions; constrained by legal enforcement; some ability to work around via subsidiaries
 *   - Regional Latin American Actors: Mixed (powerful/mobile) — constrained by US embargo policy; benefit from US market access; coordinate regional alternatives
 *   - Congressional Embargo Framework: Institutional actor (institutional/arbitrage) — maintains legal and procedural apparatus; persists through inertia despite reduced global effectiveness
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent political arrangement as geopolitical necessity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_embargo_cuba, 0.68).
domain_priors:suppression_score(us_embargo_cuba, 0.78).
domain_priors:theater_ratio(us_embargo_cuba, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_embargo_cuba, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_embargo_cuba, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(us_embargo_cuba, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_embargo_cuba, tangled_rope).
narrative_ontology:human_readable(us_embargo_cuba, "US Embargo of Cuba").
narrative_ontology:topic_domain(us_embargo_cuba, "political/economic").

domain_priors:requires_active_enforcement(us_embargo_cuba).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_embargo_cuba, us_embargo_coalition).
narrative_ontology:constraint_beneficiary(us_embargo_cuba, cuban_exile_lobby).
narrative_ontology:constraint_victim(us_embargo_cuba, cuban_population).
narrative_ontology:constraint_victim(us_embargo_cuba, cuban_economy).
narrative_ontology:constraint_victim(us_embargo_cuba, us_business_interests).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CUBAN POPULATION (SNARE) — Trapped within Cuba's borders and facing restricted access to US goods, markets, and capital. No exit option; bears the full burden of trade restrictions, medical supply shortages, and economic isolation. Maximum experienced extraction without meaningful coordination benefits. Generational horizon reflects that embargo has persisted across decades as lived constraint.
constraint_indexing:constraint_classification(us_embargo_cuba, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CUBAN SMALL BUSINESSES (SNARE) — Trapped without access to US supply chains, financing, or markets. Cannot exit the constraint without emigrating. Suffer extraction through opportunity costs and resource scarcity. No meaningful coordination benefit — the embargo extracts without providing exchange mechanism.
constraint_indexing:constraint_classification(us_embargo_cuba, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CUBAN GOVERNMENT (TANGLED ROPE) — Constrained by embargo but maintains coalition with allies (USSR, China, Venezuela). Uses embargo as organizing principle for domestic politics and international solidarity. Experiences extraction but also benefits from embargo as explanation for economic failures and rallying point for national identity. Active enforcement required to maintain domestic control narrative.
constraint_indexing:constraint_classification(us_embargo_cuba, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: US EMBARGO COALITION (ROPE) — Benefits from embargo through coordination function: shared strategic objective of constraining Cuba, unified domestic coalition (Congress, executive, Cuban-American lobby), and mechanism for signaling commitment to Cold War / post-Cold War geopolitical order. Experiences embargo as enabling coordination among US political actors. Arbitrage exit: coalition members can exit if political winds shift (as occurred with normalized relations 2014-2017) but face domestic political costs.
constraint_indexing:constraint_classification(us_embargo_cuba, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CUBAN-AMERICAN EXILE LOBBY (ROPE) — Primary beneficiary of embargo maintenance through political influence and symbolic restoration of pre-1959 status claims. Uses embargo as coordination mechanism to maintain diaspora political power. Arbitrage exit available but politically costly within community. Net beneficiary — extraction flows toward this group through political influence currency.
constraint_indexing:constraint_classification(us_embargo_cuba, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: US EXPORTERS AND INVESTORS (SNARE) — Constrained from accessing Cuban market by legal restrictions. Cannot freely enter market without violating law; face enforcement against illegal trade. Extraction occurs through foregone market access and lost competitive positioning. Some exit available (offshore investment, Canada-based subsidiaries) but legally risky and operationally constrained. Moderate power — can lobby but lack coalition dominance.
constraint_indexing:constraint_classification(us_embargo_cuba, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: REGIONAL LATIN AMERICAN ACTORS (TANGLED ROPE) — Constrained by US embargo policy but also coordinate through regional institutions (ALBA, OAS). Benefit from US market access while facing pressure to support embargo for political alignment; extract through own sanctions or trade conditions. Mobile exit available (regional trade unions, South American alliances) but with costs to US relations. Powerful actors navigating hybrid coordination-extraction.
constraint_indexing:constraint_classification(us_embargo_cuba, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 8: US CONGRESSIONAL EMBARGO FRAMEWORK (PITON) — Formalized through Helms-Burton Act (1996) and other legislation with high procedural theater. The legal apparatus of embargo enforcement is largely performative at global scale (others ignore it) and functionally degraded relative to original Cold War strategic objectives. Persists through legislative inertia despite reduced effectiveness. Theater_ratio high because enforcement mechanisms (OFAC regulations, license requirements) are maintained with limited practical impact on Cuba's access to global markets outside US. Piton classification reflects institutional inertia over active function.
constraint_indexing:constraint_classification(us_embargo_cuba, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational perspective, may view embargo as immutable geopolitical fact: the natural consequence of US power asymmetry and state sovereignty to control trade relations. The frame naturalizes what is actually a contingent institutional arrangement. Engine false-summit detector will flag this as naturalization — the embargo is maintained through active political choice and enforcement, not through physical law or logical necessity.
constraint_indexing:constraint_classification(us_embargo_cuba, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_embargo_cuba_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_embargo_cuba, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_embargo_cuba, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_embargo_cuba, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_embargo_cuba, TR),
    TR >= 0.70.

:- end_tests(us_embargo_cuba_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High-moderate. The embargo extracts through denied market access, capital restrictions, and opportunity costs for Cuban economic actors. The baseline value (0.45 at interval start) reflects Cold War strategic rationale where extraction was incidental to containment. The trajectory toward 0.68 reflects that extraction has become the primary residual function as geopolitical rationale has faded. The value is not at snare level (0.70+) because Cuba has found partial market substitution through China, Venezuela, and Russia; therefore experienced extraction is less than nominal suppression would suggest. Suppression (0.78): High. Legal barriers (Helms-Burton, OFAC regulations, license requirements) create substantial friction for market entry. Cuban embargo-running costs are high. Alternative routes exist (offshore subsidiaries, third-party trade) but carry legal risk. Suppression does not reach 0.85+ because enforcement is incomplete globally — Cuba trades freely with most non-US actors. Theater ratio (0.55): Moderate. Embargo enforcement mechanisms (OFAC compliance, visa restrictions, regulatory paperwork) consume significant institutional resources but have limited practical impact on Cuba's actual access to global markets. The theater has increased over the interval because Cold War rationale has faded but institutional apparatus persists. The Helms-Burton legal framework is largely performative at global scale (other nations ignore it) and functionally degraded relative to original objective.
 *
 * PERSPECTIVAL GAP:
 *   The embargo generates maximum perspectival disagreement. For the Cuban population, it is a snare — pure extraction with no coordination benefit. For the Cuban government, it is tangled_rope — mixed extraction and political utility. For the US embargo coalition, it is rope — coordination mechanism for geopolitical alignment. For Cuban-American exiles, it is rope — political leverage and community identity. For US exporters, it is snare — market denial with no coordination benefit. For regional Latin American actors, it is tangled_rope — constrained by US pressure, benefiting from US market access. For the Congressional framework, it is piton — maintained through procedural inertia despite reduced function. For the analytical observer, it risks appearing as mountain — natural consequence of US power and state sovereignty — but this is a false summit: the embargo is maintained through active enforcement and political choice, not through physical law. The constraint is a presheaf over observation positions, not a single type.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality d differs sharply by agent. Cuban population: beneficiary status absent, trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 (maximum experienced extraction). Cuban government: victim status (economic damage) but organized power and constrained (not trapped) exit → d ≈ 0.65 → f(d) ≈ 1.00 (moderate extraction with mitigation through political narrative). US embargo coalition: beneficiary status (coordination success), arbitrage exit → d ≈ 0.15 → f(d) ≈ -0.01 (low/negative experienced extraction — they are subsidized by the constraint). Cuban-American lobby: beneficiary status, arbitrage exit, institutional power → d ≈ 0.10 → f(d) ≈ -0.12 (institutional arbitrage floor). US exporters: victim status (market denial), constrained exit (legal barriers, mobile only through offshore routes), moderate power → d ≈ 0.72 → f(d) ≈ 1.15 (high experienced extraction despite moderate nominal power). The perspectival gap reveals that the embargo is simultaneously a coordination rope for US coalition actors and a snare for Cuban victims.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED THROUGH STRUCTURAL DUAL FORMULATION: The embargo resolves mandatrophy by showing that 'coordination' (US coalition alignment) and 'extraction' (Cuban economic damage) are not competing interpretations but complementary structural facts. The mechanism that coordinates US political actors (embargo as signal of commitment to geopolitical order) is IDENTICAL to the mechanism that extracts from Cuban actors (trade restriction). There is no ambiguity between coordination and extraction — they are the same process viewed from different positions. The US coalition benefits from the structure that crushes Cuban alternatives. This is not a case where measuring differently reveals two constraint types (ε-invariance decomposition); it is a single structural constraint where the same apparatus serves dual functions depending on agent position. The mandatrophy is resolved by recognizing that tangled_rope is the correct classification from the system level: the constraint exhibits BOTH genuine coordination (US coalition) AND asymmetric extraction (Cuban victims), with active enforcement required to maintain both. The piton degradation is real (theater increasing, Cold War rationale faded) but does not override the current tangled_rope classification — it indicates future trajectory (potential sunset through normalization or coalition breakdown).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    humanitarian_threshold_ambiguity,
    'At what level of economic damage does a trade embargo transition from legitimate geopolitical tool to collective punishment violating international humanitarian law?',
    'Comparative analysis of embargo impacts on civilian welfare; reference to UN humanitarian standards; ICC precedent on collective punishment thresholds',
    'If threshold crossed: embargo classification shifts toward pure snare (systematic extraction without coordination); if threshold not crossed: remains tangled_rope (legitimate mixed policy). Classification affects entire presheaf.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(humanitarian_threshold_ambiguity, conceptual, 'Threshold distinguishing geopolitical tool from humanitarian violation').

omega_variable(
    coalition_unity_fragility,
    'How dependent is embargo stability on Cuban-American lobby electoral power, and how fragile is that coalition against demographic/political change?',
    'Electoral analysis of swing states; demographic tracking of Cuban-American voting patterns; congressional voting pattern analysis on Cuba normalization votes',
    'If coalition highly fragile: scaffold perspective (sunset near); if coalition durable: rope classification persists indefinitely. Affects beneficiary/victim durability.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coalition_unity_fragility, empirical, 'Durability of embargo coalition under demographic pressure').

omega_variable(
    alternative_market_substitution,
    'To what degree has Cuba successfully substituted lost US market access through trade with China, Russia, and Venezuela, thereby reducing actual extraction experienced?',
    'Comparative trade volume analysis; Cuba''s GDP trajectory vs counterfactual; market substitution rates by sector',
    'If high substitution: experienced extractiveness drops despite nominal embargo (χ < measured ε); if low substitution: extractiveness sustained. Affects snare vs tangled_rope classification for Cuban agents.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_market_substitution, empirical, 'Effectiveness of market substitution in reducing embargo impact').

omega_variable(
    normalized_relations_trajectory,
    'Are normalization attempts (2014-2017 rapprochement, potential future thaws) evidence that embargo is a degraded piton, or does congressional re-imposition indicate the rope coalition endures?',
    'Historical analysis of 2014-2017 period; tracking of legislative votes to restore/maintain embargo; identification of which actors benefited from normalization window',
    'If normalization persists: scaffold/piton classification confirmed. If re-imposition succeeds: rope coalition durability confirmed. Affects temporal horizon of constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normalized_relations_trajectory, empirical, 'Direction of embargo under shifting US administrations').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_embargo_cuba, 0, 65).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(embargo_tr_t0, us_embargo_cuba, theater_ratio, 0, 0.3).
narrative_ontology:measurement(embargo_tr_t32, us_embargo_cuba, theater_ratio, 32, 0.42).
narrative_ontology:measurement(embargo_tr_t65, us_embargo_cuba, theater_ratio, 65, 0.55).

% Extraction over time
narrative_ontology:measurement(embargo_be_t0, us_embargo_cuba, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(embargo_be_t32, us_embargo_cuba, base_extractiveness, 32, 0.58).
narrative_ontology:measurement(embargo_be_t65, us_embargo_cuba, base_extractiveness, 65, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_embargo_cuba, enforcement_mechanism).
narrative_ontology:affects_constraint(us_embargo_cuba, us_latin_america_trade_policy).
narrative_ontology:affects_constraint(us_embargo_cuba, caribbean_regional_integration).
narrative_ontology:affects_constraint(us_embargo_cuba, cold_war_geopolitical_constraint).

% DUAL FORMULATION NOTE:
% The US embargo of Cuba decomposes into two structurally related constraints: (1) EMBARGO_NOMINAL_LEGAL: The formal legal apparatus (Helms-Burton, OFAC regulations, license requirements) with ε ≈ 0.45, representing the stated geopolitical tool. (2) EMBARGO_FUNCTIONAL_EXTRACTION: The actual economic impact through denied markets and capital access with ε ≈ 0.68, representing the residual extraction mechanism. These are linked by network.affects_constraints because the functional extraction depends on maintenance of the legal apparatus, but they have different ε values reflecting different measurable functions. As embargo enforcement degrades (theater increases), the gap between nominal and functional may widen further.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_embargo_cuba, analytical, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
