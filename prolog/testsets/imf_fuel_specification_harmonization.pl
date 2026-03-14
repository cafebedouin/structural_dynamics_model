% ============================================================================
% CONSTRAINT STORY: imf_fuel_specification_harmonization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_imf_fuel_specification_harmonization, []).

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
 *   constraint_id: imf_fuel_specification_harmonization
 *   human_readable: IMF Fuel Specification Harmonization
 *   domain: economic_policy/financial_regulation
 *
 * SUMMARY:
 *   IMF fuel specification harmonization represents a constraint operating at
 *   the intersection of global trade coordination and financial coercion. The
 *   IMF's structural adjustment programs routinely mandate adoption of
 *   international fuel specifications (sulfur content limits, cetane ratings,
 *   volatility standards) as conditions of lending. These mandates frame
 *   themselves as technical harmonization enabling global trade, but function
 *   as enforcement mechanisms for creditor-state preferences and profit
 *   extraction. The constraint exhibits all six DR types from different
 *   structural positions: pure coordination for beneficiaries with exit
 *   options (major producers, multinationals), snare for powerless trapped
 *   agents (fuel-importing developing nations), mixed coordination-extraction
 *   for moderately powered agents (domestic retailers), performative ritual
 *   for the IMF as institutional degraded form, and temporary problem for
 *   emerging alternative financing sources. The theater_ratio (0.64) reflects
 *   that the constraint justifies itself through efficiency and technical
 *   necessity rhetoric while its primary function is enforcement of
 *   neoliberal structural adjustment. Alternative financing sources create a
 *   scaffold dynamic where the sunset clause depends on institutional
 *   maturation of non-IMF funding sources.
 *
 * KEY AGENTS:
 *   - Fuel-Importing Developing Nations: Primary victim (powerless/trapped) — bears full compliance cost while lacking exit options; debt dependency makes resistance impossible
 *   - Major Oil Producers: Primary beneficiary (institutional/arbitrage) — captures coordination benefit; can arbitrage between markets and regulatory regimes
 *   - Multinational Fuel Distributors: Secondary beneficiary (institutional/arbitrage) — benefits from standardization reducing supply chain complexity; has global exit options
 *   - IMF Creditor State Bloc: Institutional beneficiary (organized/constrained) — voting power concentrated in creditors; structured adjustment mandates serve geopolitical and economic interests
 *   - Domestic Fuel Retailers: Secondary victim (moderate/constrained) — face capital costs and supply disruption but gain some post-harmonization benefits; constrained by capital requirements
 *   - IMF as Institutional Mechanism: Performative actor (institutional/arbitrage) — original counter-cyclical function degraded; maintains legitimacy through technical framing
 *   - Alternative Finance Coalition: Emerging organized actor (organized/mobile) — BRICS Bank, AIIB, Belt and Road offer exit paths from IMF conditionality; creates sunset dynamic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(imf_fuel_specification_harmonization, 0.52).
domain_priors:suppression_score(imf_fuel_specification_harmonization, 0.58).
domain_priors:theater_ratio(imf_fuel_specification_harmonization, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(imf_fuel_specification_harmonization, extractiveness, 0.52).
narrative_ontology:constraint_metric(imf_fuel_specification_harmonization, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(imf_fuel_specification_harmonization, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(imf_fuel_specification_harmonization, tangled_rope).
narrative_ontology:human_readable(imf_fuel_specification_harmonization, "IMF Fuel Specification Harmonization").
narrative_ontology:topic_domain(imf_fuel_specification_harmonization, "economic_policy/financial_regulation").

domain_priors:requires_active_enforcement(imf_fuel_specification_harmonization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(imf_fuel_specification_harmonization, major_oil_producers).
narrative_ontology:constraint_beneficiary(imf_fuel_specification_harmonization, multinational_fuel_distributors).
narrative_ontology:constraint_beneficiary(imf_fuel_specification_harmonization, imf_creditor_states).
narrative_ontology:constraint_victim(imf_fuel_specification_harmonization, fuel_importing_developing_nations).
narrative_ontology:constraint_victim(imf_fuel_specification_harmonization, domestic_fuel_retailers).
narrative_ontology:constraint_victim(imf_fuel_specification_harmonization, environmental_compliance_cost_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUEL-IMPORTING DEVELOPING NATION (SNARE) — Trapped by IMF lending conditions that mandate fuel specification harmonization with global standards. No exit option: debt restructuring requires IMF compliance; domestic alternatives require capital they lack. Bears full cost of specification compliance (infrastructure retrofitting, price subsidies, supply disruption) while capturing no coordination benefit. Maximum extraction experienced.
constraint_indexing:constraint_classification(imf_fuel_specification_harmonization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DOMESTIC FUEL RETAILER (TANGLED ROPE) — Constrained by specification mandate but also benefits from access to global supply chains and technology transfer. Faces capital costs and supply disruption during transition, but gains price stability and quality assurance post-harmonization. Extraction and coordination coexist; moderate experienced costs with partial benefits.
constraint_indexing:constraint_classification(imf_fuel_specification_harmonization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MAJOR OIL PRODUCER (ROPE) — Benefits from harmonization as coordination mechanism: standardized specs reduce production complexity, increase export market access, and stabilize global pricing. Can arbitrage between markets; has exit options (bilateral contracts, non-IMF trade corridors). Experiences constraint as pure coordination benefit with minimal coercion.
constraint_indexing:constraint_classification(imf_fuel_specification_harmonization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: MULTINATIONAL FUEL DISTRIBUTOR (ROPE) — Benefits from standardized specifications as coordination mechanism: reduces supply chain complexity, enables hub-and-spoke distribution, increases market efficiency. Has exit options (alternative trading corridors, bilateral arrangements). Extraction is minimal; constraint perceived as pure coordination benefit.
constraint_indexing:constraint_classification(imf_fuel_specification_harmonization, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CREDITOR STATE BLOC (TANGLED ROPE) — IMF governance structure concentrates voting power in creditor states (US, EU, Japan). Harmonization mandates serve coordination (global market integration) while extracting from debtor nations (forced compliance, loss of policy sovereignty). Organized power with strategic options but constrained by need to maintain IMF legitimacy and emerging-market participation.
constraint_indexing:constraint_classification(imf_fuel_specification_harmonization, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: IMF AS INSTITUTIONAL MECHANISM (PITON) — The IMF's fuel specification harmonization mandate is largely performative at the institutional level. Original function was counter-cyclical lending and coordination; current function is enforcement of neoliberal structural adjustment. Theater ratio is high: the mandate is justified through efficiency arguments, but underlying driver is creditor-state policy preference. IMF retains formal legitimacy through technical framing despite functional degradation.
constraint_indexing:constraint_classification(imf_fuel_specification_harmonization, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ALTERNATIVE FINANCE COALITION (SCAFFOLD) — Emerging multilateral institutions (BRICS Bank, Asian Infrastructure Investment Bank, Belt and Road financing) offer debtor nations exit paths from IMF conditionality. These alternatives reduce IMF's coercive power, creating a temporary scaffold effect where IMF harmonization mandates lose force as competing funding sources mature. Sunset trajectory depends on institutional consolidation of alternatives.
constraint_indexing:constraint_classification(imf_fuel_specification_harmonization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / TRADE ECONOMICS VIEW (MOUNTAIN) — From a universal analytical perspective, fuel specification harmonization reflects an immutable property of global commodity trade: standardization is necessary for market integration, and integrated markets require price discovery mechanisms that punish deviation. This perspective naturalizes the constraint as an inherent feature of global capitalism. However, the structural data reveals this as false summit — harmonization timing, enforcement stringency, and exemption patterns are contingent political choices, not market necessities.
constraint_indexing:constraint_classification(imf_fuel_specification_harmonization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(imf_fuel_specification_harmonization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(imf_fuel_specification_harmonization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(imf_fuel_specification_harmonization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(imf_fuel_specification_harmonization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(imf_fuel_specification_harmonization, TR),
    TR >= 0.70.

:- end_tests(imf_fuel_specification_harmonization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint extracts from fuel-importing developing nations through mandatory compliance costs (infrastructure retrofitting, supply disruption, price volatility during transition) while benefiting major producers through market standardization and guaranteed demand. The extraction is masked by coordination framing ('global efficiency requires standards') but operates through coercive lending conditions. Increased from initial 0.35 as alternative financing sources have reduced IMF's leverage, revealing previously disguised extraction. Suppression (0.58): Moderate-high. Constraints include: debt restructuring conditions that make non-compliance impossible, informational asymmetry (developing nations lack technical expertise to contest specifications), structural dependency on IMF for balance-of-payments support, and capital scarcity that makes domestic alternatives infeasible. However, suppression is not total — some nations have negotiated exemptions or borrowed from alternative sources. Theater ratio (0.64): Moderate-high. The IMF justifies fuel specifications through technical efficiency arguments and 'best practices' framing, obscuring that timing and stringency serve creditor-state interests. Specifications cluster around profit-maximization for multinational distributors and production standardization for major producers, not genuine global efficiency. Theater has increased as alternative financing has reduced IMF's power to enforce compliance — the mandate now relies more heavily on performance of legitimacy through technical discourse.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates striking perspectival divergence. Creditor-state institutions and major producers see Rope — genuine coordination enabling global market integration. Fuel-importing developing nations see Snare — extraction with no coordination benefit and no exit. Domestic retailers see Tangled Rope — both coordination (access to global supply) and extraction (forced compliance, cost burden). The IMF sees Piton — its core function degraded to structural adjustment enforcement, legitimacy maintained through technical discourse. Alternative financing sources see Scaffold — a temporary constraint whose coercive power is eroding as competing funding sources mature. The analytical/civilizational observer risks seeing Mountain — trade standardization as an immutable feature of global commerce — but the structural data reveals this as naturalization: the timing, stringency, and exemption patterns are contingent on IMF power distribution, not economic necessity.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by the agent's structural position relative to the extraction flow. Fuel-importing developing nations as primary victims with no exit options occupy d ≈ 0.92 (trapped + victim → high f(d) → high χ). Major oil producers as beneficiaries with arbitrage options occupy d ≈ 0.08 (beneficiary + arbitrage → low f(d) → low/negative χ). Multinational distributors occupy similar d ≈ 0.12 given beneficiary status and global mobility. Creditor states as beneficiaries with constrained exit occupy d ≈ 0.20 (institutional leverage constrained by IMF legitimacy requirements). Domestic retailers as moderate victims with some coordination benefit occupy d ≈ 0.55 (mixed costs/benefits + constrained exit). The alternative finance coalition's mobile exit status pushes d toward 0.35 (mobile exit reduces experienced extraction). The pipeline derives d from beneficiary/victim declarations and exit options; the four perspectives then apply geographic scope modifiers σ(S) to compute effective χ per the formula.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that classification divergence reflects real structural differences in agent power and exit options, not ambiguity in the constraint itself. The Snare classification from the developing nation perspective is not contradicted by the Rope classification from the producer perspective — they are correct measurements from different structural positions. The mandatrophy resolution mechanism is explicit: as alternative financing sources reduce IMF leverage (BRICS Bank, AIIB growth), the constraint's coercive mechanisms weaken, and classification should converge toward Rope or Scaffold from all perspectives. The extractiveness trajectory (0.35→0.52 over interval) reflects increasing divergence in power: as IMF power diminishes, the underlying extraction mechanism becomes more visible, explaining the increased extractiveness measurement despite declining coercive capacity. This appears paradoxical but reflects the distinction between coercive power (declining) and structural extraction (stable or increasing as masked).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    specification_timing_bias,
    'Does IMF harmonization timing track genuine global market coordination or strategic advantage for creditor-state producers?',
    'Historical analysis of specification adoption dates relative to creditor vs debtor state fuel production profiles; correlation between timing and market price asymmetries',
    'If coordinated timing: constraint is pure rope. If biased timing: constraint is snare with coordination framing; extractiveness is closer to 0.65.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(specification_timing_bias, empirical, 'Whether specification timing reflects coordination or strategic advantage').

omega_variable(
    environmental_externality_allocation,
    'Who bears the cost of environmental compliance embedded in fuel specifications: developing nations or global supply chains?',
    'Cost accounting for specification compliance infrastructure; tracking of environmental remediation cost allocation across supply chain; analysis of exemption patterns by nation income level',
    'If costs borne by developing nations: suppression and extractiveness higher (0.62+). If distributed across supply chain: extractiveness lower (0.45).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_externality_allocation, empirical, 'Allocation of environmental compliance costs across supply chain').

omega_variable(
    alternative_finance_substitution_rate,
    'What is the rate of substitution between IMF conditionality and alternative financing sources (BRICS, AIIB, Belt and Road) for fuel-importing developing nations?',
    'Longitudinal tracking of debtor nation funding sources; analysis of IMF lending volumes vs alternative institutional volumes; measurement of conditionality severity as alternative sources grow',
    'If substitution is rapid (>10% annual shift): scaffold sunset is real and extractiveness declines to 0.35-0.40. If slow or blocked: snare classification becomes more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_finance_substitution_rate, empirical, 'Rate of substitution between IMF and alternative financing').

omega_variable(
    harmonization_versus_domination,
    'Is the constraint primarily a coordination mechanism enabling global trade or a domination mechanism enabling resource extraction from debtor states?',
    'Comparative analysis of specification adoption in symmetric vs asymmetric power relationships; measurement of compliance cost burden across income levels; identification of exemptions and carve-outs by political power',
    'If coordination: tangled_rope classification confirmed with extractiveness ~0.45. If domination: snare classification for debtor perspective, extractiveness >0.60.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(harmonization_versus_domination, conceptual, 'Whether constraint functions as coordination or domination').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(imf_fuel_specification_harmonization, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(imf_fuel_tr_t0, imf_fuel_specification_harmonization, theater_ratio, 0, 0.48).
narrative_ontology:measurement(imf_fuel_tr_t7, imf_fuel_specification_harmonization, theater_ratio, 7, 0.58).
narrative_ontology:measurement(imf_fuel_tr_t14, imf_fuel_specification_harmonization, theater_ratio, 14, 0.64).

% Extraction over time
narrative_ontology:measurement(imf_fuel_be_t0, imf_fuel_specification_harmonization, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(imf_fuel_be_t7, imf_fuel_specification_harmonization, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(imf_fuel_be_t14, imf_fuel_specification_harmonization, base_extractiveness, 14, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(imf_fuel_specification_harmonization, resource_allocation).
narrative_ontology:boltzmann_floor_override(imf_fuel_specification_harmonization, 0.18).
narrative_ontology:affects_constraint(imf_fuel_specification_harmonization, imf_structural_adjustment_conditionality).
narrative_ontology:affects_constraint(imf_fuel_specification_harmonization, developing_nation_sovereign_debt_dependency).
narrative_ontology:affects_constraint(imf_fuel_specification_harmonization, multinational_fuel_supply_chain_consolidation).

% DUAL FORMULATION NOTE:
% IMF fuel specification harmonization is downstream of the broader structural adjustment regime but represents a distinct constraint. It coordinates with sovereign debt dependency (nations accept specifications because debt leaves no exit) and upstream from supply chain consolidation (standardization enables multinational distributor market concentration). Each constraint family member has distinct extractiveness and suppression values reflecting different mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(imf_fuel_specification_harmonization, organized, 0.22).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
