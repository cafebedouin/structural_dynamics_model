% ============================================================================
% CONSTRAINT STORY: u_s_venezuela_sanctions_regime
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_u_s_venezuela_sanctions_regime, []).

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
 *   constraint_id: u_s_venezuela_sanctions_regime
 *   human_readable: U.S.-Venezuela Sanctions Regime
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The U.S.-Venezuela sanctions regime operates as a geopolitical-economic
 *   constraint that simultaneously pursues multiple objectives (regime
 *   pressure, energy market control, opposition support) while producing
 *   severe humanitarian costs that scale as the regime adapts and sanctions
 *   expand. The constraint exhibits genuine coordination functions
 *   (international pressure against authoritarian governance) alongside
 *   asymmetric extraction (Venezuelan civilians bear costs; U.S. and Latin
 *   American states distribute benefits unequally). The regime was initiated
 *   under the Obama administration (2015) and expanded substantially under
 *   Trump (2017-2020), with marginal adjustments under Biden. It has proven
 *   remarkably durable despite minimal evidence of regime change trajectory,
 *   suggesting institutional inertia (piton elements) or deliberately
 *   perpetuated extraction (snare elements) alongside genuine coordination
 *   rationales (rope and tangled_rope elements). The extractiveness value has
 *   increased from 0.35 (initial targeted measures) to 0.68 (comprehensive
 *   sectoral sanctions including oil, gold, finance), while theater_ratio has
 *   increased from 0.35 to 0.55, indicating growing compliance bureaucracy
 *   relative to stated regime pressure objectives. The constraint's
 *   durability cannot be explained by sustained regime change progress (none
 *   has occurred); instead, it reflects coalition interests in perpetuating
 *   sanctions (opposition funding, energy sector advantage, bureaucratic
 *   inertia) and institutional lock-in preventing negotiation pathways.
 *
 * KEY AGENTS:
 *   - Venezuelan Civilian Population: Primary victim (powerless/trapped) — bears humanitarian costs through healthcare collapse, food insecurity, currency debasement, medical brain drain
 *   - Venezuelan Government/Maduro Regime: Primary target (institutional/trapped) — subject of sanctions; has adapted through dollarization, informal economies, third-country trade
 *   - U.S. Policy Establishment: Primary beneficiary (institutional/arbitrage) — controls sanctions regime, sets targeting, defines legitimacy, maintains policy durability for domestic political reasons
 *   - Venezuelan Opposition Coalition: Secondary beneficiary/victim (moderate/constrained) — depends on U.S. support while constrained by policy establishment dominance; faces legitimacy questions when civilian harm persists
 *   - U.S. Petroleum and Energy Sector: Beneficiary (institutional/arbitrage) — benefits from Venezuelan oil exclusion; maintains market advantage without explicit visibility
 *   - Latin American States (Colombia, Brazil, Mexico): Mixed positions (powerful/constrained or institutional/constrained) — experience migration pressure, border crises, and secondary sanctions enforcement while constrained by U.S. alliance structures
 *   - International Humanitarian Organizations: Organized opposition (organized/constrained) — advocate for humanitarian exemptions and sunset clauses; marginalized in policy decision-making
 *   - OFAC and Sanctions Bureaucracy: Institutional performer (institutional/arbitrage) — implements sanctions through compliance architecture; benefits from regime durability through career investment and budget perpetuation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(u_s_venezuela_sanctions_regime, 0.68).
domain_priors:suppression_score(u_s_venezuela_sanctions_regime, 0.75).
domain_priors:theater_ratio(u_s_venezuela_sanctions_regime, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(u_s_venezuela_sanctions_regime, extractiveness, 0.68).
narrative_ontology:constraint_metric(u_s_venezuela_sanctions_regime, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(u_s_venezuela_sanctions_regime, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(u_s_venezuela_sanctions_regime, tangled_rope).
narrative_ontology:human_readable(u_s_venezuela_sanctions_regime, "U.S.-Venezuela Sanctions Regime").
narrative_ontology:topic_domain(u_s_venezuela_sanctions_regime, "geopolitical/economic").

domain_priors:requires_active_enforcement(u_s_venezuela_sanctions_regime).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(u_s_venezuela_sanctions_regime, u_s_policy_establishment).
narrative_ontology:constraint_beneficiary(u_s_venezuela_sanctions_regime, opposition_coalition_venezuela).
narrative_ontology:constraint_beneficiary(u_s_venezuela_sanctions_regime, us_petroleum_competitors).
narrative_ontology:constraint_victim(u_s_venezuela_sanctions_regime, venezuelan_civilian_population).
narrative_ontology:constraint_victim(u_s_venezuela_sanctions_regime, venezuelan_healthcare_system).
narrative_ontology:constraint_victim(u_s_venezuela_sanctions_regime, venezuelan_food_security).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VENEZUELAN CIVILIAN POPULATION (SNARE) — Trapped within national borders with no exit capacity. Bears maximum extraction costs through healthcare collapse, food scarcity, currency debasement, and blocked remittance flows. No alternative mechanisms exist to meet basic needs. Suppression is structural: medical personnel emigrate, hospitals lack supplies, pharmaceutical distribution breaks down. The regime maintains itself through enforcement of banking restrictions and asset freezes that devastate civilian infrastructure while targeting regime officials.
constraint_indexing:constraint_classification(u_s_venezuela_sanctions_regime, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: VENEZUELAN OPPOSITION COALITION (TANGLED ROPE) — Constrained by internal factionalism and resource scarcity but benefits from U.S. sanctions enforcement and diplomatic recognition. Sanctions provide coordination function (international pressure against regime) alongside extraction function (opposition coalition depends on U.S. support, limiting autonomy). Cannot exit without abandoning international backing; cannot remain without complicity in civilian harm. Asymmetric extraction flows upward to U.S. decision-makers.
constraint_indexing:constraint_classification(u_s_venezuela_sanctions_regime, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: U.S. POLICY ESTABLISHMENT (ROPE) — Primary beneficiary with full arbitrage capacity. Experiences the sanctions regime as coordination: maintaining maximum pressure on regime officials while claiming humanitarian intent. Can adjust sanctions mix, designate new targets, or negotiate exit terms. Extraction flows toward this agent through policy durability, domestic political leverage, and capacity to define legitimacy. Low or negative chi from U.S. perspective: sanctions are a tool, not a cost.
constraint_indexing:constraint_classification(u_s_venezuela_sanctions_regime, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: U.S. PETROLEUM AND ENERGY SECTOR (ROPE) — Beneficiary through Venezuelan oil exclusion from global markets, supporting higher prices and U.S. shale competitiveness. Sanctions regime provides coordination function by maintaining market structure favorable to U.S. energy independence. Can arbitrage between sanctions enforcement and negotiation outcomes. Extraction runs toward energy sector: sanctions maintain market advantage without direct visibility.
constraint_indexing:constraint_classification(u_s_venezuela_sanctions_regime, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LATIN AMERICAN STATES (TANGLED ROPE) — Constrained by U.S. dominance and migration pressure from Venezuela while experiencing genuine coordination benefits (regional stability, pressure against regime). Colombia faces maximum extraction (border crisis, drug trafficking exacerbation, refugee burden) while constrained by alliance with U.S. Brazil and Mexico have more exit capacity through alternative alignments. All experience suppression through lack of independent asylum capacity and pressure to enforce secondary sanctions.
constraint_indexing:constraint_classification(u_s_venezuela_sanctions_regime, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: INTERNATIONAL HUMANITARIAN COALITION (SCAFFOLD) — Organized opposition (UN bodies, NGOs, medical associations) to sanctions regime arguing for humanitarian exemptions and sunset clauses. Views sanctions as temporary coercive tool with declining utility and rising humanitarian cost. Suppression operates through political marginalization and funding constraints. Genuine coordination function exists (preventing worst humanitarian outcomes), alongside extraction (humanitarian actors depend on sanctions framework for relevance). Sunset depends on regime change or negotiated exit.
constraint_indexing:constraint_classification(u_s_venezuela_sanctions_regime, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL SANCTIONS ARCHITECTURE (PITON) — OFAC, Treasury Department, multilateral sanctions coordination mechanisms. Theater ratio high (0.55): sanctions enforcement requires extensive bureaucratic performance, documentation, compliance certifications, and legal justifications. The apparatus persists through institutional inertia and career investment in sanctions implementation. Primary function (coercing regime behavior change) has atrophied; regime adaptation has neutralized targeted sanctions, forcing expansion to broader sectoral measures with higher civilian impact. The system sees its own process as degraded but maintains performance through career incentives and legal requirements.
constraint_indexing:constraint_classification(u_s_venezuela_sanctions_regime, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / STRUCTURAL INEVITABILITY (MOUNTAIN) — From a civilizational view, sanctions regimes are natural expressions of geopolitical competition: powerful states inevitably sanction adversaries, regimes inevitably adapt, civilian populations inevitably suffer, and international systems inevitably lack mechanisms to prevent this cycle. This perspective naturalizes the constraint as immutable. However, the structural data contradicts mountain classification — historical contingency (Carter-era policy reversals, Nixon opening, Cold War summits) demonstrates that sanctions are institutionally contingent, not natural laws. False summit detection applies.
constraint_indexing:constraint_classification(u_s_venezuela_sanctions_regime, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(u_s_venezuela_sanctions_regime_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(u_s_venezuela_sanctions_regime, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(u_s_venezuela_sanctions_regime, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(u_s_venezuela_sanctions_regime, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(u_s_venezuela_sanctions_regime, TR),
    TR >= 0.70.

:- end_tests(u_s_venezuela_sanctions_regime_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The regime has expanded from targeted asset freezes on regime officials (0.35 extractiveness, precision targeting) to comprehensive sectoral sanctions (0.68 extractiveness, broad civilian impact). This progression is not justified by regime change progress (none) but reflects either administrative function creep or deliberately escalated extraction. The civilian healthcare and food supply systems have become primary extraction mechanisms — not incidental effects but structural mechanisms through which the U.S. applies pressure. Suppression (0.75): Very high. Venezuelan civilians face structural barriers to exit (border controls, legal status restrictions in neighboring countries, remittance blockages), suppression of alternative economic mechanisms (informal trade criminalized, cryptocurrency restricted), and suppression of information about sanction origins (regime attributes supply disruptions to internal mismanagement rather than external embargo). Suppression includes regional amplification through secondary sanctions on third-party suppliers, preventing workarounds. Theater ratio (0.55): Moderate-high. OFAC compliance operations, Treasury determinations, legal justifications, and multilateral coordination (though limited) constitute significant bureaucratic performance. However, the gap between stated objectives (regime change, pressure on officials) and actual implementation (sectoral disruption, civilian harm) is substantial. The theater serves legitimacy maintenance and institutional perpetuation more than regime pressure effectiveness. Claimed type (tangled_rope): The constraint combines genuine coordination function (international pressure against authoritarianism) with asymmetric extraction (costs borne by civilians, benefits distributed to policy establishment and select allies). Active enforcement is required; beneficiaries and victims are clearly distinct; extraction persists despite stated coordination objective. Mandatrophy resolution: The tangled_rope classification prevents false naturalization (this is not an immutable law of international relations) while acknowledging both coordination and extraction elements. The piton perspective (institutional inertia) and snare perspective (civilian extraction) are legitimate readings from constrained positions, but the primary analytical claim is tangled_rope: the regime persists because it serves multiple actor interests simultaneously.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence: from rope (beneficiary view) to snare (victim view) with tangled_rope (analytical claim) as the structural resolution. The policy establishment views sanctions as a successful coordination mechanism — maintaining deterrence without direct military intervention, supporting opposition governance, and enabling energy market advantage without explicit coordination. The opposition views sanctions as qualified but necessary support with significant autonomy constraints. Latin American states view sanctions as coerced participation with unequal cost distribution. Venezuelans view sanctions as the primary extraction mechanism for healthcare and food insecurity. The OFAC apparatus views its own function as degraded (theater persists despite effectiveness atrophy). The humanitarian coalition views sanctions as humanitarian catastrophe with declining coercive utility. The civilizational observer risks naturalizing this as inevitable geopolitical competition but historical cases demonstrate contingency.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are computed from beneficiary/victim declarations and exit options. The U.S. policy establishment and energy sector as beneficiaries with arbitrage exit options derive d ≈ 0.05-0.15 (full beneficiaries, low extraction experienced). The Venezuelan opposition as secondary beneficiary with constrained exit derives d ≈ 0.35-0.45 (qualified beneficiary with limited agency). Latin American states as constrained institutional actors derive d ≈ 0.55-0.65 (mixed victim/beneficiary positions with suppressed alternatives). Venezuelan civilians as victims with trapped exit derive d ≈ 0.90+ (maximum target, maximum extraction experienced). The sigmoid function f(d) transforms these values into effective power modifiers: beneficiaries experience negative or minimal chi (sanctions are tools, not costs); victims experience maximum chi amplified by trapped exit status. The scope modifier σ(S) amplifies extraction at national and continental scales (σ=1.0-1.1): Venezuelan civilians experience extraction amplified across all supply systems simultaneously; neighboring states experience migration and trade spillovers. The regional scope (1.0) for opposition coalition reflects that their constrained position is concentrated in South America rather than globally diffuse. Institutional beneficiaries with global scope (1.2) experience amplified extraction potential, though they experience it as leverageable control rather than cost.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The tangled_rope classification resolves mandatrophy by acknowledging that the sanctions regime simultaneously performs coordination (international pressure against authoritarianism, market stabilization) and extraction (civilian humanitarian harm, policy establishment control). The regime persists because it serves multiple coalition interests: policy establishment maintains geopolitical leverage, energy sector maintains market advantage, opposition maintains funding and international recognition, and bureaucratic apparatus maintains institutional investment. The snare (victim) perspective is structurally accurate from Venezuelan civilian position but misses the genuine coordination function that other actors benefit from. The rope (beneficiary) perspective is structurally accurate from policy establishment position but naturalizes the civilian costs as externalities rather than structural extraction. The piton (degraded apparatus) perspective is accurate about the theater dynamics but incomplete about the genuine benefits the apparatus produces for beneficiary coalitions. The scaffold (sunset) perspective is aspirational rather than structural — humanitarian organizations advocate for sunset clauses, but institutional inertia and coalition interest in regime perpetuation prevents meaningful sunset mechanisms. The mountain (structural inevitability) perspective is a false summit — sanctions are contingent institutional choices, not laws of nature, as demonstrated by historical policy reversals in comparable cases.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regime_accountability_vs_civilian_harm,
    'Can targeted sanctions on regime officials be effectively enforced without cascading harm to civilians through banking, pharmaceutical, and food supply disruption?',
    'Empirical measurement of actual vs intended sanction targeting; analysis of secondary effects through financial system bottlenecks; longitudinal health/nutrition data from sanctioned vs non-sanctioned comparable countries',
    'If yes: sanctions are more rope-like (genuine coordination against regime). If no: sanctions are more snare-like (extraction from civilians with minimal regime impact). Current evidence suggests secondary effects overwhelm targeting precision.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regime_accountability_vs_civilian_harm, empirical, 'Whether targeted sanctions can avoid cascading civilian harm').

omega_variable(
    regime_change_probability_vs_duration,
    'What is the causal relationship between sanction duration and probability of regime change? Do sanctions accelerate, delay, or decouple from transition dynamics?',
    'Comparative case analysis: Iran, North Korea, Cuba, Syria, Russia. Control for internal regime fragility, elite factional splits, and economic baseline. Measure sanction intensity vs regime durability across multi-decade intervals.',
    'If accelerating: scaffolding with meaningful sunset possible. If decoupling: piton detection justified (function atrophied, apparatus persists). If delaying: snare detection justified (extraction perpetuates while stated objective recedes).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regime_change_probability_vs_duration, empirical, 'Causal relationship between sanction duration and regime change probability').

omega_variable(
    opposition_coalition_authenticity,
    'Is the Venezuelan opposition coalition acting as an autonomous political force or as a U.S.-proxy dependent on sanctions regime maintenance for legitimacy and resources?',
    'Analysis of opposition coalition policy positions relative to U.S. preferences; funding source transparency; internal decision-making autonomy in negotiations; counterfactual opposition strategy without U.S. backing',
    'If autonomous: opposition is genuine beneficiary of coordination against regime. If dependent: opposition is tangled_rope victim constrained by U.S. dominance; extraction flows upward to policy establishment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(opposition_coalition_authenticity, empirical, 'Whether opposition coalition is autonomous or U.S.-proxy dependent').

omega_variable(
    secondary_sanctions_enforcement_cost,
    'What proportion of pharmaceutical and food supply disruption results from primary sanctions on Venezuela vs secondary sanctions enforcement against third-party suppliers (ports, carriers, payment processors)?',
    'Detailed supply chain mapping; interviews with pharmaceutical importers and food suppliers; measurement of goods lost to secondary sanctions enforcement vs regulatory complexity; comparison with trade flows to other sanctioned countries',
    'If high secondary cost: sanctions mechanism is less targeted; civilian harm is primary, not incidental. Snare classification strengthens. If low secondary cost: regime adaptation is primary driver; tangled_rope classification remains appropriate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secondary_sanctions_enforcement_cost, empirical, 'Cost of secondary sanctions enforcement relative to primary sanctions impact').

omega_variable(
    negotiation_deadlock_structure,
    'Are U.S.-Venezuela negotiations deadlocked because of incompatible substantive demands (regime change vs sovereignty) or because sanctions institutionalization has created actors with interest in perpetuating the regime (OFAC bureaucracy, energy sector, opposition coalition funding)?',
    'Timeline analysis of negotiation offers and counter-offers; institutional analysis of beneficiary positions; historical comparison to Cold War thaws (Cuba, Iran) where negotiation became possible after institutional reframing',
    'If substantive incompatibility: piton classification (apparatus persists despite dysfunction). If institutional perpetuation: tangled_rope classification (extraction mechanism sustains through coalition interests). Determines whether sunset clauses are feasible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(negotiation_deadlock_structure, conceptual, 'Whether negotiation deadlock is substantive or institutionally perpetuated').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(u_s_venezuela_sanctions_regime, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vesan_tr_t0, u_s_venezuela_sanctions_regime, theater_ratio, 0, 0.35).
narrative_ontology:measurement(vesan_tr_t8, u_s_venezuela_sanctions_regime, theater_ratio, 8, 0.48).
narrative_ontology:measurement(vesan_tr_t16, u_s_venezuela_sanctions_regime, theater_ratio, 16, 0.55).
narrative_ontology:measurement(vesan_tr_t4, u_s_venezuela_sanctions_regime, theater_ratio, 4, 0.4).
narrative_ontology:measurement(vesan_tr_t12, u_s_venezuela_sanctions_regime, theater_ratio, 12, 0.52).

% Extraction over time
narrative_ontology:measurement(vesan_be_t0, u_s_venezuela_sanctions_regime, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(vesan_be_t8, u_s_venezuela_sanctions_regime, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(vesan_be_t16, u_s_venezuela_sanctions_regime, base_extractiveness, 16, 0.68).
narrative_ontology:measurement(vesan_be_t4, u_s_venezuela_sanctions_regime, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(vesan_be_t12, u_s_venezuela_sanctions_regime, base_extractiveness, 12, 0.61).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(u_s_venezuela_sanctions_regime, enforcement_mechanism).
narrative_ontology:affects_constraint(u_s_venezuela_sanctions_regime, us_cuban_embargo).
narrative_ontology:affects_constraint(u_s_venezuela_sanctions_regime, iranian_sanctions_regime).
narrative_ontology:affects_constraint(u_s_venezuela_sanctions_regime, venezuelan_opposition_legitimacy).
narrative_ontology:affects_constraint(u_s_venezuela_sanctions_regime, latin_american_migration_pressure).

% DUAL FORMULATION NOTE:
% The sanctions regime can be decomposed into structurally distinct constraints: (1) sanctions_targeting_regime_officials (ε≈0.20, rope), (2) sectoral_sanctions_oil_finance (ε≈0.65, tangled_rope), and (3) secondary_sanctions_enforcement (ε≈0.72, snare). These three stories should be linked via network.affects_constraints: sectoral sanctions depend on secondary enforcement effectiveness; regime official targeting is upstream but overwhelmed by sectoral expansion. Current story integrates all three under unified tangled_rope claim.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(u_s_venezuela_sanctions_regime, powerful, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
