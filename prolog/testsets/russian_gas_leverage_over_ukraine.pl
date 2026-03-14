% ============================================================================
% CONSTRAINT STORY: russian_gas_leverage_over_ukraine
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_russian_gas_leverage_over_ukraine, []).

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
 *   constraint_id: russian_gas_leverage_over_ukraine
 *   human_readable: Russian Gas Leverage Over Ukraine
 *   domain: geopolitical/energy/economic_coercion
 *
 * SUMMARY:
 *   Russian gas leverage over Ukraine represents a structurally embedded
 *   constraint that combines genuine energy coordination (reliable supply
 *   networks, price stability for industry, transit infrastructure) with
 *   systematic asymmetric extraction (pricing coercion, payment leverage,
 *   geopolitical weaponization). The constraint emerged from Soviet-era
 *   infrastructure dependencies but has been deliberately maintained and
 *   intensified as a coercive mechanism. Ukraine's position as both consumer
 *   and transit state creates a dual exposure: direct dependency (household
 *   heating, industrial power) and indirect vulnerability (leverage over
 *   European energy security). The constraint operates across immediate
 *   (supply disruptions), biographical (career-dependent energy sectors), and
 *   generational (infrastructure lock-in) time horizons. Its classification
 *   varies sharply across perspectives: snare for trapped households with no
 *   exit, tangled rope for the industrial sector and state apparatus with
 *   constrained options, rope for the beneficiary state energy sector with
 *   arbitrage alternatives, scaffold for EU coalition building alternatives,
 *   and piton for Soviet-era infrastructure inertia. The theater ratio is low
 *   (0.35), indicating that the extraction mechanism is primarily functional
 *   (supply control, pricing adjustment) rather than performative—suggesting
 *   genuine coercive capacity rather than institutional theater.
 *
 * KEY AGENTS:
 *   - Ukrainian Households: Primary victim (powerless/trapped) — dependent on winter heating; face coercive pricing and supply cutoff threats; no alternative supply option
 *   - Russian State Energy Sector (Gazprom): Primary beneficiary (institutional/arbitrage) — captures pricing premium, maintains geopolitical leverage, controls transit revenue, arbitrage options to Asian markets
 *   - Ukrainian Industrial Sector: Secondary victim (moderate/constrained) — depends on reliable energy supply; faces high switching costs; constrained by long-term contracts but retains some negotiation capacity
 *   - Ukrainian State Apparatus: Institutional actor (institutional/constrained) — manages energy distribution; benefits from coordination function; bears extraction through dependency and payment leverage
 *   - European Union States: Tertiary victim (organized/constrained) — transit dependency creates exposure; pursue diversification coalition; constrained by investment costs and timeline; some members benefit from cheap transit gas
 *   - EU Energy Diversification Coalition: Organized agent (organized/constrained) — arXiv of energy politics; building alternative infrastructure (LNG, renewables); constrained by capital and timeline; has visible exit strategy
 *   - Soviet-Era Infrastructure Complex: Institutional inertia (institutional/constrained) — pipeline networks, regulatory systems, energy accounting designed during Soviet era persist through momentum; partially functional, partially performative
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(russian_gas_leverage_over_ukraine, 0.62).
domain_priors:suppression_score(russian_gas_leverage_over_ukraine, 0.75).
domain_priors:theater_ratio(russian_gas_leverage_over_ukraine, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(russian_gas_leverage_over_ukraine, extractiveness, 0.62).
narrative_ontology:constraint_metric(russian_gas_leverage_over_ukraine, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(russian_gas_leverage_over_ukraine, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(russian_gas_leverage_over_ukraine, tangled_rope).
narrative_ontology:human_readable(russian_gas_leverage_over_ukraine, "Russian Gas Leverage Over Ukraine").
narrative_ontology:topic_domain(russian_gas_leverage_over_ukraine, "geopolitical/energy/economic_coercion").

domain_priors:requires_active_enforcement(russian_gas_leverage_over_ukraine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(russian_gas_leverage_over_ukraine, russian_state_energy_sector).
narrative_ontology:constraint_beneficiary(russian_gas_leverage_over_ukraine, russian_federal_budget).
narrative_ontology:constraint_victim(russian_gas_leverage_over_ukraine, ukrainian_economy).
narrative_ontology:constraint_victim(russian_gas_leverage_over_ukraine, ukrainian_households).
narrative_ontology:constraint_victim(russian_gas_leverage_over_ukraine, european_energy_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UKRAINIAN HOUSEHOLD (SNARE) — Trapped by structural dependency on Russian gas with no alternative supply during winter months; faces coercive pricing, supply cutoffs, and no exit option except relocation. Maximum extraction; minimal coordination benefit. Suppression enforced through geographic and seasonal immobility.
constraint_indexing:constraint_classification(russian_gas_leverage_over_ukraine, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UKRAINIAN INDUSTRIAL SECTOR (TANGLED ROPE) — Constrained by high switching costs and long-term supply contracts, but also benefits from reliable (if expensive) energy coordination for heavy industry. Both coordination function (energy supply network management) and asymmetric extraction (pricing premium and payment leverage) present. Significant agency constraints but not total entrapment.
constraint_indexing:constraint_classification(russian_gas_leverage_over_ukraine, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: GAZPROM & RUSSIAN STATE ENERGY (ROPE) — Primary beneficiary. Experiences the constraint as coordinating energy supply across Eurasia while maintaining revenue and geopolitical leverage. Net extractor — supply contracts are enforced, payment disputes weaponized, transit leverage over Europe maintained. Arbitrage options available (Asian markets, pipeline diversification). Low experienced extraction; high distribution of benefits.
constraint_indexing:constraint_classification(russian_gas_leverage_over_ukraine, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 4: EU ENERGY DIVERSIFICATION COALITION (SCAFFOLD) — Organized effort (LNG terminals, renewable targets, Nord Stream alternatives) to reduce Russian gas dependency. Sees the constraint as a temporary coordination problem with a sunset: Eastern European pipelines, liquefied natural gas infrastructure, and green energy transition create exit pathways with 15-20 year timeline. Enforcement pressure exists but coalition has visible agency and exit strategy.
constraint_indexing:constraint_classification(russian_gas_leverage_over_ukraine, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: SOVIET-ERA INFRASTRUCTURE LOCK (PITON) — Historical pipeline networks, regulatory frameworks, and energy accounting systems designed during Soviet period persist through institutional inertia. These systems are partially functional (gas still flows) but largely performative in justifying continued Russian leverage (most pipelines could be technically replaced but cultural/contractual momentum sustains them). Theater ratio reflects that much of the 'dependency' is institutional drag rather than active coercion mechanism.
constraint_indexing:constraint_classification(russian_gas_leverage_over_ukraine, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: UKRAINIAN STATE (TANGLED ROPE) — Coordination function: manages energy distribution, collects taxes from energy sector, uses pricing to support domestic industries. Extraction function: dependent on Russian supply for state budget, faces coercive pricing and payment leverage. State has constrained exit (cannot immediately replace supply or enforce transit fees unilaterally) but also some agency (can negotiate, diversify, build alternative infrastructure). Mixed beneficiary and victim status — benefits from coordination function; bears extraction through dependency.
constraint_indexing:constraint_classification(russian_gas_leverage_over_ukraine, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From a civilizational/global perspective, energy dependency has been systematically weaponized as a coercive mechanism: pricing timed to political crises, supply cutoffs during negotiation periods, transit leverage over Europe, implicit threat of winter supply interruption. The constraint exhibits minimal genuine coordination (gas could flow via alternative routes; pricing could follow market mechanisms) and maximum asymmetric extraction (revenue concentration, geopolitical leverage, enforcement through threat of deprivation). The analytical position sees snare dynamics that the beneficiary downplays as rope coordination.
constraint_indexing:constraint_classification(russian_gas_leverage_over_ukraine, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(russian_gas_leverage_over_ukraine_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(russian_gas_leverage_over_ukraine, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(russian_gas_leverage_over_ukraine, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(russian_gas_leverage_over_ukraine, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(russian_gas_leverage_over_ukraine, TR),
    TR >= 0.70.

:- end_tests(russian_gas_leverage_over_ukraine_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High-moderate. The constraint exhibits strong extraction mechanics: pricing premiums over European market rates (estimated 20-40% above spot prices depending on geopolitical context), payment leverage (threats to cut supply during negotiation periods), and geopolitical revenue concentration (energy exports constitute ~40% of Russian federal budget in peak periods). However, extraction is not absolute (Snare threshold ≥0.66) because genuine coordination functions exist: reliable gas delivery to industry, heating infrastructure management, and stable long-term supply planning. The distinction between rent-seeking extraction and coordination cost is measurable through pricing analysis (omega variable 1). The upward trajectory (0.45 → 0.62 over 15 years) reflects intensification: pricing has become more explicitly coercive, supply cutoffs have been weaponized with greater clarity, and the state has explicitly linked gas to political demands. Suppression (0.75): Very high. Barriers to exit are multi-layered: seasonal immobility (winter heating cannot be deferred), geographic isolation from alternative suppliers, technical incompatibility between pipeline networks, long-term contract lock-in, regulatory barriers, and implicit threat of deprivation during negotiation. Suppression is not absolute (≥0.95 would suggest physical imprisonment) because some economic actors can pay premium prices for alternatives or invest in efficiency; however, the vast majority of the population and industrial base faces effective suppression. Theater ratio (0.35): Low. The constraint's enforcement is primarily functional rather than performative: supply actually stops, prices actually rise, payment disputes actually trigger cutoffs. Unlike institutional theater (piton-signature high ratio), this constraint's enforcement mechanisms are direct and material. The low theater ratio indicates that the coercive mechanism operates through real material capability (control of supply valves, pricing authority) rather than through procedural legitimacy or institutional facade. This distinguishes it from a degraded piton and toward a functional snare.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiary (Gazprom/Russian state) perceives the constraint as Rope: a coordination mechanism delivering reliable supply across Eurasia while maintaining stable revenue and reasonable geopolitical positioning. The experienced extraction is near-zero or negative (the state benefits from the arrangement). The powerless Ukrainian household perceives Snare: coercive pricing, supply cutoff threats, no exit option, maximum experienced extraction. The industrial sector perceives Tangled Rope: genuine coordination benefits (stable supply for production) mixed with extraction penalties (pricing premium, payment leverage, long-term lock-in). The EU coalition perceives Scaffold: a temporary coordination problem with visible exit pathways (LNG, alternatives, renewables) and a 15-20 year sunset. The Soviet infrastructure system perceives Piton: the constraint persists through institutional inertia (pipelines still exist, contracts still valid) but the functional coordination has degraded into performative maintenance. The analytical observer perceives Snare: explicit evidence of coercive pricing timed to political events, supply weaponization, geopolitical leverage extraction, minimal genuine coordination value. The perspectival gap between beneficiary (Rope) and trapped victim (Snare) is maximal—a factor of 2+ in experienced extractiveness. This gap is the defining feature of a constraint with asymmetric extraction and suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values derive from structural position in the extraction flow. The beneficiary (Russian state energy) has high exit options (arbitrage: Asian markets, alternative transit routes via Turks Stream and Nord Stream; d ≈ 0.05) and benefits from the constraint (d → 0.0 from beneficiary status), yielding minimal experienced extraction. The trapped Ukrainian household (d → 1.0 from victim + trapped status, no exit options) experiences maximum extraction. The constrained Ukrainian industrial sector (d ≈ 0.75-0.80 from victim + constrained status: can pay premium or invest in alternatives but at very high cost) experiences high but not maximal extraction. The organized EU coalition (d ≈ 0.50-0.60 from victim + constrained status + organized power: has agency and collective exit strategy) experiences moderate extraction. The sigmoid function f(d) translates these d values into experienced extractiveness multipliers. Beneficiaries with d ≈ 0.05 see f(d) ≈ -0.12 (negative extraction = subsidy). Trapped agents with d ≈ 0.95 see f(d) ≈ 1.42 (maximum coercion multiplier). The constraint's base extractiveness (0.62) is then scaled by f(d) and scope σ(S) to produce the effective extraction χ each perspective experiences. The beneficiary sees rope (coordination, low χ). The powerless victim sees snare (extraction, high χ). The organized coalition sees scaffold (low χ due to organized power and visible exit pathway). The state sees tangled rope (mixed coordination and extraction). This perspectival gap is the signature of correct directionality derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint demonstrates Mandatrophy Type II (false coordination claim). The beneficiary frames the constraint as Rope coordination ('we provide reliable supply to our neighbors'), while the analytical observer and trapped agents identify it as Snare extraction ('pricing and supply are weaponized for geopolitical leverage'). The framework resolves the mandatrophy by showing both readings are structurally correct relative to the observers' positions, but the beneficiary's Rope classification conceals the asymmetric extraction from the observer's side. The classification is Tangled Rope (not pure Rope or pure Snare) at the institutional level because: (1) genuine coordination functions exist (energy supply networks, industrial reliability, long-term planning stability), (2) asymmetric extraction is present (pricing premium, payment leverage, geopolitical coercion), and (3) active enforcement mechanisms are required (supply cutoff credibility, pricing authority, contract enforcement). The mandatrophy dissolves when we recognize that both 'coordination' and 'extraction' are accurate descriptions of the same constraint from different structural positions. The constraint is Rope FROM THE BENEFICIARY'S PERSPECTIVE (they experience it as coordinating supply and revenue flow). The constraint is Snare FROM THE TRAPPED AGENT'S PERSPECTIVE (they experience it as pure coercive extraction with no exit). The constraint is Tangled Rope FROM THE INSTITUTIONAL/ANALYTICAL PERSPECTIVE (both functions present, both measurable). No single type is 'the truth'—the presheaf over the observation site is the truth. The high extractiveness (0.62) and the mandatrophy_resolved flag (true) together indicate that this is a well-understood asymmetric extraction mechanism, not a misclassified rope or a false snare claim.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    market_pricing_vs_coercive_pricing,
    'What proportion of the price premium paid by Ukraine is market-based (supply scarcity, transit costs, network effects) vs. coercive (political leverage, threat-based adjustment)?',
    'Comparison of Ukraine''s gas prices to spot market prices for equivalent supply volumes; correlation of price changes with political events (elections, NATO statements, territorial disputes); analysis of price discrimination across European buyers',
    'If >70% market-based: constraint is primarily a resource coordination problem (Rope from beneficiary context, Tangled Rope from moderate). If <40% market-based: constraint is primarily coercive extraction (Snare from all perspectives). Mid-range values suggest genuine tangled rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(market_pricing_vs_coercive_pricing, empirical, 'Proportion of pricing premium attributable to political coercion vs. market factors').

omega_variable(
    alternative_supply_technical_viability,
    'Are alternative gas supply routes (EU LNG terminals, Central Asian pipelines, Azerbaijani corridors) technically capable of replacing Russian supply within a 5-year horizon?',
    'Engineering feasibility analysis of pipeline expansion; LNG import capacity ramp-up rates; regulatory barrier assessment; cost comparison of alternative vs. Russian supply including infrastructure investment',
    'If fully viable: scaffold perspective confirmed — sunset is real structural feature. If partially viable: scaffold is aspirational; constraint will persist longer. If not viable: trapped/snare perspectives are structurally accurate; escape routes do not exist.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_supply_technical_viability, empirical, 'Technical feasibility of alternative supply routes within 5-year horizon').

omega_variable(
    coercive_supply_cutoff_credibility,
    'Is the threat of winter supply cutoff credible enough to constitute a coercive mechanism (affecting price negotiations and policy), or is it primarily rhetorical theater?',
    'Historical analysis of supply disruptions: timing relative to political events, duration, economic impact on Ukraine vs. Russian export revenue loss; communication patterns preceding cutoffs; reconstruction of decision-making context',
    'If credible and executed: suppression gate validates (≥0.60); snare classification from powerless perspective accurate. If rhetorical: suppression is lower; constraint may be tangled rope or even rope from powerless perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercive_supply_cutoff_credibility, empirical, 'Credibility and execution frequency of coercive supply cutoff threat').

omega_variable(
    ukrainian_state_capacity_to_enforce_alternatives,
    'Does Ukraine have the institutional capacity (regulatory authority, technical expertise, investment capital) to enforce alternative energy sourcing independently, or does this require European/NATO institutional backing?',
    'Assessment of Ukrainian energy ministry technical capability; analysis of required capital investment vs. state budget; evaluation of European support commitments and conditionality; comparison to successful energy diversification by other post-Soviet states',
    'If independent capacity exists: Ukrainian state has more exit agency than current classification suggests. If dependent on external support: constraint extends to EU institutional politics; Ukrainian agency is mediated through alliance relationships. This determines whether state perspective is constrained or trapped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ukrainian_state_capacity_to_enforce_alternatives, empirical, 'Ukraine''s independent vs. externally-mediated capacity for energy diversification').

omega_variable(
    european_union_collective_action_problem,
    'Does EU energy diversification function as genuine coalition action (shared risk, shared benefit) or as a coordination problem where individual states pursue arbitrage (cheap Russian gas for some, expensive alternatives for others)?',
    'Analysis of EU pricing mechanisms, burden-sharing arrangements, and cross-border gas flows; evaluation of individual state compliance with sanctions and diversification targets; comparison of stated policies to actual import patterns and investment commitments',
    'If genuine coalition: scaffold classification is accurate; organized power has real exit pathway. If coordination failure: some EU members benefit from Russian leverage over Ukraine and Central Europe (cheap transit gas, reduced infrastructure investment needs); constraint extends into internal EU dynamics. Affects scope classification and whether beneficiaries include some EU member-states.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(european_union_collective_action_problem, empirical, 'EU collective action vs. individual state arbitrage in energy diversification').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(russian_gas_leverage_over_ukraine, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rgl_tr_t0, russian_gas_leverage_over_ukraine, theater_ratio, 0, 0.42).
narrative_ontology:measurement(rgl_tr_t5, russian_gas_leverage_over_ukraine, theater_ratio, 5, 0.38).
narrative_ontology:measurement(rgl_tr_t10, russian_gas_leverage_over_ukraine, theater_ratio, 10, 0.35).
narrative_ontology:measurement(rgl_tr_t15, russian_gas_leverage_over_ukraine, theater_ratio, 15, 0.31).

% Extraction over time
narrative_ontology:measurement(rgl_be_t0, russian_gas_leverage_over_ukraine, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(rgl_be_t5, russian_gas_leverage_over_ukraine, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(rgl_be_t10, russian_gas_leverage_over_ukraine, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(rgl_be_t15, russian_gas_leverage_over_ukraine, base_extractiveness, 15, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(russian_gas_leverage_over_ukraine, resource_allocation).
narrative_ontology:affects_constraint(russian_gas_leverage_over_ukraine, european_energy_security).
narrative_ontology:affects_constraint(russian_gas_leverage_over_ukraine, ukraine_structural_dependency).
narrative_ontology:affects_constraint(russian_gas_leverage_over_ukraine, gazprom_monopoly_power).

% DUAL FORMULATION NOTE:
% Russian gas leverage operates as a single integrated constraint with multiple structural dimensions: physical supply control, pricing authority, geopolitical coercion, and infrastructure lock-in. Related constraints include European energy security (downstream), Ukraine's structural dependency on Russian inputs (causal parent), and Gazprom's institutional monopoly power (enabling mechanism). All three should be modeled as separate constraint stories but linked via affects_constraints to show the constraint family dependency structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(russian_gas_leverage_over_ukraine, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
