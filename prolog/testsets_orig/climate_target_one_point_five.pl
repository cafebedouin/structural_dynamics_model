% ============================================================================
% CONSTRAINT STORY: climate_target_one_point_five
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_target_one_point_five, []).

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
 *   constraint_id: climate_target_one_point_five
 *   human_readable: The 1.5°C Global Warming Target
 *   domain: political/environmental
 *
 * SUMMARY:
 *   The 1.5°C global warming target, established at COP21 in Paris (2015) and
 *   operationalized through the NDC framework, creates a fundamental
 *   structural tension: it commits nations to climate stabilization via
 *   emissions reductions while permitting high-income fossil fuel producers
 *   to defer real decarbonization through pledges with minimal enforcement
 *   capacity. This constraint exemplifies how governance targets can
 *   simultaneously function as coordination mechanisms (mobilizing renewable
 *   investment, establishing climate finance), instruments of extraction
 *   (protecting incumbent energy producers while concentrating climate costs
 *   on vulnerable populations), and theatrical performance (ritual COP
 *   meetings, net-zero commitments with 2050 horizons). From the perspective
 *   of a small island nation, the 1.5°C target is a false floor: even at
 *   1.5°C warming, committed sea-level rise will inundate their territory,
 *   yet they lack enforcement authority to compel decarbonization from major
 *   emitters. From the fossil fuel industry's perspective, the target is a
 *   coordination framework that enables gradual transition within profitable
 *   timelines. From the renewable energy sector, it is a coordination signal
 *   generating trillions in investment. From the UNFCCC bureaucracy, it is a
 *   performative ritual that persists through institutional inertia. The
 *   constraint's extractiveness (0.58) reflects the asymmetry between the
 *   urgency of the problem and the weakness of enforcement; its suppression
 *   (0.68) reflects barriers to exit (energy systems lock-in, global supply
 *   chain entanglement, asymmetric technological access) and the absence of
 *   alternative pathways for vulnerable nations; its theater ratio (0.64)
 *   reflects the rise of pledge-making over actual emissions reductions. Over
 *   the 20-year interval (2005-2025), theater has increased as annual COP
 *   meetings have become more central to climate governance while average
 *   global emissions trajectory remains on track for 2.8°C warming, not
 *   1.5°C.
 *
 * KEY AGENTS:
 *   - Small Island States (Kiribati, Tuvalu, Marshall Islands): Primary victims (powerless/trapped) — face existential sea-level rise with no exit or enforcement capacity; the 1.5°C target provides nominal protection but no material barrier to outcomes they cannot survive
 *   - Low-Income Agricultural Populations (Sub-Saharan Africa, Bangladesh, parts of South Asia): Primary victims (moderate/constrained) — depend on climate-stable agricultural systems; face adaptation costs they cannot finance and migration barriers; constrained exit through dependence on global food systems
 *   - High-Income Fossil Fuel Producers (Saudi Arabia, Russia, coal-dependent regions in US/Australia): Primary beneficiaries (organized/arbitrage) — capture extraction through continued high emissions while appearing to support transition; have full exit capacity (can relocate, diversify) but benefit from sustained carbon pricing through retained infrastructure
 *   - Renewable Energy Sector (Solar, wind, battery manufacturers): Secondary beneficiary (institutional/arbitrage) — benefits from regulatory mandates and investment channeling; experiences constraint as pure coordination mechanism
 *   - High-Income Consumer Populations (North America, Western Europe): Tertiary beneficiary/victim (powerful/mobile) — benefit from current consumption patterns but face distributional pressure (carbon taxes, ESG mandates); powerful enough to resist bearing costs through outsourcing and consumption relocation
 *   - Fossil Fuel-Dependent Workers (coal regions, petro-states): Secondary victim (moderate/trapped) — caught between energy transition imperative and livelihood protection; limited exit capacity
 *   - UNFCCC Bureaucracy: Institutional actor (institutional/arbitrage) — maintains COP ritual; benefits from continued governance relevance; sees own process as increasingly theatrical
 *   - Climate Action Movement (civil society, youth movements): Organized agents (organized/constrained) — pushing for tighter enforcement and faster transition; constrained by dependence on political coordination; have exit path if social pressure achieves critical mass
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_target_one_point_five, 0.58).
domain_priors:suppression_score(climate_target_one_point_five, 0.68).
domain_priors:theater_ratio(climate_target_one_point_five, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_target_one_point_five, extractiveness, 0.58).
narrative_ontology:constraint_metric(climate_target_one_point_five, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(climate_target_one_point_five, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_target_one_point_five, tangled_rope).
narrative_ontology:human_readable(climate_target_one_point_five, "The 1.5°C Global Warming Target").
narrative_ontology:topic_domain(climate_target_one_point_five, "political/environmental").

domain_priors:requires_active_enforcement(climate_target_one_point_five).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_target_one_point_five, high_income_fossil_fuel_producers).
narrative_ontology:constraint_beneficiary(climate_target_one_point_five, developed_nations_current_consumption).
narrative_ontology:constraint_beneficiary(climate_target_one_point_five, carbon_intensive_industry).
narrative_ontology:constraint_victim(climate_target_one_point_five, vulnerable_small_island_states).
narrative_ontology:constraint_victim(climate_target_one_point_five, low_income_agricultural_populations).
narrative_ontology:constraint_victim(climate_target_one_point_five, future_generations_climate_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ISLAND NATION / POWERLESS CLIMATE VICTIM (SNARE) — Cannot exit the constraint; bears full cost of rising seas. The 1.5°C target itself is a false floor: even at 1.5°C, island nations face existential sea-level rise within decades. Yet they lack enforcement capacity to compel emissions reductions. They are trapped in a framework designed to appear as protection while offering none. Maximum experienced extraction through false reassurance that delays real mitigation.
constraint_indexing:constraint_classification(climate_target_one_point_five, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING AGRICULTURAL ECONOMY (TANGLED ROPE) — Constrained by resource requirements and dependence on global supply chains; cannot transition energy infrastructure rapidly without economic collapse. Also benefits from the 1.5°C framing as a genuine coordination signal and international climate finance mechanism. Mixed extraction: bears costs of adaptation while benefiting from some technology transfer and climate funding. Extraction is not maximal because the target creates enforcement mechanisms (NDC revisions, climate finance) that provide some leverage.
constraint_indexing:constraint_classification(climate_target_one_point_five, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RENEWABLE ENERGY SECTOR / INSTITUTIONAL BENEFICIARY (ROPE) — Experiences the 1.5°C target as pure coordination mechanism. The target creates regulatory framework, investment pathways, and mandates for renewable deployment. This actor benefits from the constraint as a coordination signal that channels trillions into their sector. Low extraction; high coordination benefit. They have full arbitrage options — exit means relocation or sector switching, low-cost for institutional actors.
constraint_indexing:constraint_classification(climate_target_one_point_five, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: FOSSIL FUEL INDUSTRY / ORGANIZED EXTRACTION (TANGLED ROPE) — Experiences the 1.5°C target as mixed constraint: it requires transition away from their primary product, yet it also provides coordination cover for gradual phase-out, carve-outs, and 'net-zero by 2050' commitments that defer real action. They have constrained exit (cannot simply abandon global markets) but significant enforcement capacity (through political lobbying). They benefit from the target's weakness — it coordinates global agreement on climate urgency while allowing decades of continued high emissions under the guise of transition plans. Active enforcement mechanism (coal phase-out dates, net-zero commitments) exist but contain massive loopholes.
constraint_indexing:constraint_classification(climate_target_one_point_five, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: UNFCCC BUREAUCRACY / INSTITUTIONAL DEGRADATION (PITON) — The United Nations climate process is substantially theatrical: annual COP meetings, pledges and announcements, net-zero commitments that lack enforcement mechanisms, carbon accounting that allows vast Scope 3 loopholes. The process persists through institutional inertia and symbolic value, not through functional emissions reduction capacity. Theater ratio is high because the COP ritual (negotiation theater, agreement pageantry, target-setting without enforcement) has become the primary function rather than an instrument for achieving reductions. Real emissions mitigation increasingly happens outside UNFCCC structures (national policy, corporate decarbonization, investor pressure).
constraint_indexing:constraint_classification(climate_target_one_point_five, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: HIGH-INCOME CONSUMER / POWERFUL ASYMMETRY (TANGLED ROPE) — Benefits from the current carbon-intensive infrastructure (cheap energy, global supply chains, high consumption) while the 1.5°C target places nominal responsibility on them to reduce. They are powerful (can lobby, have voice in governance) and mobile (can relocate, switch sectors, arbitrage carbon prices). They experience the target as coordination overhead: pledges to reduce personal carbon footprint, ESG investment mandates, carbon pricing — all of which create theater and distributional impact but leave actual consumption levels largely unchanged. Mixed experience: some genuine coordination benefit from renewable access, but significant extraction of others (through outsourced emissions accounting) combined with their power to avoid bearing the cost.
constraint_indexing:constraint_classification(climate_target_one_point_five, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: CIVIL SOCIETY / ORGANIZED SCAFFOLD (SCAFFOLD) — Organized climate action (Fridays for Future, Indigenous climate movements, climate litigation networks) sees the 1.5°C target as a temporary coordination mechanism with sunset properties. The movement's theory of change is: the target creates political pressure for institutional transitions (renewable mandate, fossil fuel divestment, carbon pricing) that accumulate over 15-20 years until the old carbon infrastructure is economically obsolete. Low experienced extraction because the movement has agency and exit path (scaled action drives system change). Constrained because the movement cannot unilaterally force the transition — they depend on political coordination. Sunset horizon: if the movement achieves critical mass on decarbonization (renewable cost parity, grid storage deployment, supply chain transition), the enforcement mechanism shifts from policy mandate to economic inevitability, and the 1.5°C constraint dissolves into obsolescence.
constraint_indexing:constraint_classification(climate_target_one_point_five, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / PHYSICS NATURALIZATION (MOUNTAIN) — From a civilizational/universal perspective grounded in atmospheric physics, the 1.5°C target is an attempt to codify a natural boundary. Once CO2 concentration reaches certain levels, physical climate response is largely determined by radiative forcing laws. This perspective sees the target as reflecting an immutable constraint: global warming will be at least 1.5°C unless atmospheric CO2 is stabilized immediately, and stabilization requires global emissions to reach net-zero before mid-century. The physics constraint is real and ineliminable. However, the structural data contradicts the mountain classification: the 1.5°C target itself is a POLITICAL choice (arbitrary threshold, not a physical tipping point), and the constraint operates through governance and enforcement (UNFCCC, NDC revisions, climate finance) not through natural law. The physics is real; the 1.5°C framing is contingent. The engine will flag this as a false summit.
constraint_indexing:constraint_classification(climate_target_one_point_five, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_target_one_point_five_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_target_one_point_five, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_target_one_point_five, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_target_one_point_five, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_target_one_point_five, TR),
    TR >= 0.70.

:- end_tests(climate_target_one_point_five_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The 1.5°C target creates genuine coordination benefits (renewable investment, climate finance flows) but these benefits concentrate in high-income fossil fuel producers and renewable sector actors, while costs concentrate in vulnerable populations. The target enables continued high fossil fuel production through 2040-2050 transition timelines while presenting urgency framing. The extraction is 0.58 rather than higher (0.70+) because the constraint has real enforcement mechanisms (NDC revision cycles, climate finance allocation) that provide some leverage to victims. Suppression (0.68): High. Global populations lack practical exit from carbon-intensive infrastructure without economic collapse (energy lock-in, supply chain entanglement, technological asymmetry in access to alternatives). Developing nations cannot transition rapidly without energy availability collapse. Vulnerable populations cannot migrate without legal barriers. Suppression is not total (some adaptation pathways exist) but substantial. Theater Ratio (0.64): Moderate-high. The COP process has evolved to emphasize pledges, net-zero commitments, and climate finance announcements, yet global emissions trajectory has not inflected toward 1.5°C-compatible pathways. The NDC process generates substantial reporting and revision cycles without corresponding emissions reductions. Carbon accounting permits massive Scope 3 loopholes. However, theater is not dominant (≥0.70) because some real decarbonization is occurring (renewable deployment, coal retirement in some regions, corporate emissions limits). The rising theater ratio over 2005-2025 reflects Goodhart drift: the constraint mechanism (pledges, targets, international commitments) is increasingly performative as the physical constraint (emissions reductions) requires accelerating rather than plateauing.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximal perspectival divergence. Island nations see a snare: the target is presented as protection but offers none — 1.5°C warming still inundates them, and they lack enforcement capacity to compel emissions reductions from major emitters. The fossil fuel industry sees tangled_rope: the target creates regulatory overhead and political pressure (constrained exit) but also enables continued high-margin fossil fuel production through 2040+ transition timelines (benefits from extended timeline). Renewable sector sees pure rope: the target is a coordination signal that channels investment. Developed-economy consumers see tangled_rope: they face distributional pressure (carbon costs) but benefit from maintained consumption infrastructure and arbitrage options (offshoring emissions via carbon accounting). The UNFCCC process sees piton: its own ritual (COP meetings, pledge cycles) has become the primary function rather than an instrument for achieving reductions. Civil society sees scaffold: if distributed climate action (divestment, corporate commitments, grid transformation, supply chain pressure) accelerates beyond government timelines, the 1.5°C target becomes obsolete as economic logic replaces policy mandate. The analytical observer at civilizational scale risks false summit: treating 1.5°C as immutable physics when it is a political choice. The perspectival gap is structural — different agents experience dramatically different extraction/benefit ratios depending on their exit capacity and structural position in the emissions system.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from the agent's power level, exit options, and structural position (beneficiary vs. victim) relative to the constraint. Small island states (powerless/trapped/victims) experience maximum directionality (d ≈ 0.95) — they are full targets of the constraint with zero exit capacity, producing maximum experienced extractiveness. High-income fossil fuel producers (organized/arbitrage/beneficiaries) experience minimum directionality (d ≈ 0.10) — they benefit from the constraint's weakness and have full exit capacity, producing negative or near-zero experienced extractiveness. Developed-economy consumers (powerful/mobile) experience moderate directionality (d ≈ 0.48-0.55) — they are nominally subject to climate costs but have exit capacity (consumption relocation, capital mobility), producing moderate experienced extractiveness. The UNFCCC bureaucracy (institutional/arbitrage/beneficiaries of continued governance) experiences low directionality (d ≈ 0.05-0.15) — the constraint maintains their institutional relevance and enables continued resource flow through climate finance and technical assistance. Civil society (organized/constrained/both-beneficiary-and-victim) experiences moderate-high directionality (d ≈ 0.55-0.65) — they bear costs of climate inaction while also benefiting from the coordination signal that enables their pressure campaigns. The engine derives these d values from the beneficiary/victim declarations and exit options; the commentary documents why each structural position maps to that directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED AT EXTRACTIVENESS = 0.58: The constraint's classification as tangled_rope is justified by the presence of simultaneous coordination benefits and asymmetric extraction. The coordination function is real: the 1.5°C target mobilizes renewable investment, establishes climate finance flows, and creates regulatory frameworks that accelerate decarbonization relative to business-as-usual. The asymmetric extraction is also real: these benefits concentrate in high-income fossil fuel producers and renewable sector actors, while costs concentrate in vulnerable populations who lack exit capacity. The tangled_rope classification prevents mischaracterization of the constraint as pure coordination (rope, which would ignore the asymmetric extraction) or pure extraction (snare, which would ignore genuine coordination benefits). The mandatrophy is resolved by recognizing that the constraint is structurally hybrid — its coordination function is genuine, but it operates in a context of extreme power asymmetry (vulnerable nations vs. fossil fuel producers) that ensures the coordination benefits flow to the already-powerful. This is the defining pattern of tangled_rope: coordination mechanism + asymmetric extraction. Without the mandatrophy resolution, analysis risks either celebrating the 'cooperation' of the Paris Agreement while ignoring its weak enforcement, or dismissing it as pure extraction while missing genuine decarbonization pathways it has enabled. The resolution shows that both are correct from different perspectives: coordination from the renewable sector and developed-economy institutions, extraction from the perspective of small island states and low-income agricultural populations.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    one_point_five_vs_natural_tipping_point,
    'Is 1.5°C a physical tipping point or a purely political threshold?',
    'Climate science consensus on critical warming thresholds; comparison of 1.5°C impacts vs 2.0°C impacts in IPCC assessments; identification of non-linear climate response at specific temperature bands',
    'If physical tipping point: target is mountain from analytical perspective (natural law). If political choice: target is tangled_rope / snare from all perspectives (governance constraint, not physics constraint). Determines whether false summit detection applies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(one_point_five_vs_natural_tipping_point, empirical, 'Whether 1.5°C represents a physical climate tipping point or political threshold').

omega_variable(
    enforcement_mechanism_sufficiency,
    'Do NDC (Nationally Determined Contribution) commitments have enforcement capacity sufficient to drive real emissions reductions?',
    'Analysis of NDC compliance history; correlation between NDC strength and actual emissions trajectories; cost-benefit analysis of violation penalties vs. benefit of non-compliance',
    'If enforcement is sufficient: the constraint has real snare/tangled_rope properties from victim perspectives. If enforcement is near-zero: the constraint is primarily a piton (ritual without function). Determines whether suppression gate is satisfied.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_sufficiency, empirical, 'Whether NDC enforcement mechanisms have real capacity to compel emissions reductions').

omega_variable(
    carbon_transition_feasibility_timeline,
    'Is global decarbonization on a 1.5°C-compatible timeline technologically and economically feasible without contraction of high-income consumption?',
    'Engineering feasibility analysis of renewable deployment rates, grid storage scaling, industrial transition timelines; economic modeling of transition costs vs. GDP growth trajectories; comparison of required investment rates vs. historical capital flows',
    'If feasible without contraction: the constraint is coordinate-able (rope/scaffold from more perspectives). If requires income contraction in high-income nations: the constraint is extractive redistributive mechanism (snare/tangled_rope from global perspective). Determines whether beneficiary designation is accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(carbon_transition_feasibility_timeline, empirical, 'Whether 1.5°C-compatible decarbonization is achievable without reducing high-income consumption').

omega_variable(
    small_island_state_mitigation_vs_adaptation_gap,
    'Given committed sea-level rise even at 1.5°C, does the mitigation focus of the 1.5°C target address the actual constraint facing small island states (adaptation/relocation), or does it substitute political performance for material protection?',
    'Cost-benefit analysis: cost of 1.5°C-compatible mitigation vs. cost of adaptation/relocation for island nations; assessment of whether NDC commitments are financing adaptation or mitigation; interviews with island nation climate negotiators on perceived vs. actual constraint relief',
    'If 1.5°C-compatible mitigation meaningfully reduces island state adaptation burden: the target is snare with some coordination benefit (mixed extraction). If adaptation burden is unchanged at 1.5°C: the target is snare with performative coordination (pure extraction). Determines whether false protection detection applies.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(small_island_state_mitigation_vs_adaptation_gap, empirical, 'Whether 1.5°C mitigation addresses actual material constraints of island nations or substitutes for adaptation').

omega_variable(
    fossil_fuel_sunset_veracity,
    'Do fossil fuel phase-out commitments (coal by 2030, oil by 2040, net-zero by 2050) represent actual exit pathways or theatrical pledges without real enforcement?',
    'Analysis of coal production trends post-pledge; comparison of pledged retirement dates vs. actual plant closure rates; assessment of loopholes (carbon capture, natural gas transition, industrial heat carve-outs)',
    'If pledges are real: scaffold perspective is accurate — sunset horizon is 15-25 years. If pledges are theater: fossil fuel industry experiences constraint as low-cost piton (ritual without material cost). Determines whether scaffold classification is structural or aspirational.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fossil_fuel_sunset_veracity, empirical, 'Whether fossil fuel phase-out commitments represent real exit pathways or theatrical pledges').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_target_one_point_five, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ct15_tr_t0, climate_target_one_point_five, theater_ratio, 0, 0.45).
narrative_ontology:measurement(ct15_tr_t10, climate_target_one_point_five, theater_ratio, 10, 0.64).
narrative_ontology:measurement(ct15_tr_t20, climate_target_one_point_five, theater_ratio, 20, 0.72).

% Extraction over time
narrative_ontology:measurement(ct15_be_t0, climate_target_one_point_five, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(ct15_be_t10, climate_target_one_point_five, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(ct15_be_t20, climate_target_one_point_five, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_target_one_point_five, enforcement_mechanism).
narrative_ontology:affects_constraint(climate_target_one_point_five, coal_phase_out_timelines).
narrative_ontology:affects_constraint(climate_target_one_point_five, net_zero_2050_pledge_enforcement).
narrative_ontology:affects_constraint(climate_target_one_point_five, climate_finance_adequacy).
narrative_ontology:affects_constraint(climate_target_one_point_five, scope_three_emissions_accounting).

% DUAL FORMULATION NOTE:
% The 1.5°C target itself is a political choice that overlies a real physical constraint (radiative forcing and climate response). The constraint story decomposes these: (1) physics-level constraint: atmospheric CO2 concentration determines committed warming (ε ≈ 0.08, mountain); (2) governance-level constraint: the 1.5°C target creates enforcement mechanisms and narrative framing (ε ≈ 0.58, tangled_rope). The downstream constraints (coal phase-out, net-zero pledges, climate finance) operationalize the governance constraint. The upstream physical constraint is independent; the governance constraint would exist even if the physical target were 2.0°C instead of 1.5°C.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_target_one_point_five, organized, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
