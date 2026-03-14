% ============================================================================
% CONSTRAINT STORY: agricultural_monoculture_fragility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_agricultural_monoculture_fragility, []).

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
 *   constraint_id: agricultural_monoculture_fragility
 *   human_readable: Agricultural Monoculture Fragility and Systemic Risk
 *   domain: agricultural_economics/environmental_systems
 *
 * SUMMARY:
 *   Agricultural monoculture has created a structural constraint that
 *   coordinates food production at scale while simultaneously extracting from
 *   farmers, soil health, and food security commons. The constraint exhibits
 *   the full range of DR classification: smallholder farmers perceive it as a
 *   pure snare (debt trap with no exit); regional food networks experience
 *   mixed coordination and extraction (tangled rope); industrial agriculture
 *   sees it as coordination (rope); the agroecology movement sees it as a
 *   temporary problem with a sunset (scaffold); agricultural policy sees it
 *   as a degraded ritual (piton); the analytical observer risks seeing it as
 *   a natural law (false summit mountain). The constraint's evolution shows
 *   increasing extractiveness (0.35 → 0.58) and rising theater ratio (0.30 →
 *   0.55), indicating that the system's original coordination function
 *   (stabilizing food supply after WWII industrialization) has atrophied
 *   while the extraction mechanism has deepened. Suppression is high (0.68)
 *   because the constraint operates through multiple reinforcing channels:
 *   credit systems that lock farmers into input dependency, certification
 *   standards that require monoculture-scale operations, price infrastructure
 *   that rewards commodity volume, and subsidy regimes that penalize
 *   diversification. The constraint is actively maintained through government
 *   agricultural policy, commodity trading infrastructure, and seed/chemical
 *   supply chains — it is not a spontaneous market outcome.
 *
 * KEY AGENTS:
 *   - Smallholder Farmers: Primary victims (powerless/trapped) — locked into debt cycles and input dependency; cannot access credit for diversification; face market penalties for non-commodity crops
 *   - Industrial Agriculture Corporations: Primary beneficiaries (institutional/arbitrage) — agribusiness, seed companies, chemical suppliers; capture value through input sales and commodity trading; maintain vertical integration that enables arbitrage
 *   - Food Security Commons: Secondary victim (powerless/trapped) — abstract collective good; no organizing agent; bears cost of systemic fragility and monoculture collapse risk
 *   - Soil Health Commons: Secondary victim (powerless/trapped) — ecological commons; bears cost of nutrient depletion, mycorrhizal network destruction, erosion; no self-advocacy mechanism
 *   - Regional Food Networks: Moderate stakeholder (moderate/constrained) — coordinated through monoculture supply; also vulnerable to monoculture failure; intermediate power and exit cost
 *   - Agroecology Movement: Organized agent (organized/constrained) — NGOs, farmer cooperatives, research institutions building alternative pathways; has agency but faces structural barriers to scaling
 *   - Agricultural Policy & Subsidy System: Institutional maintenance actor (institutional/arbitrage) — government programs that sustain monoculture; maintains through inertia and constituency pressure, not functional necessity
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent institutional choices as inherent agricultural limits
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(agricultural_monoculture_fragility, 0.58).
domain_priors:suppression_score(agricultural_monoculture_fragility, 0.68).
domain_priors:theater_ratio(agricultural_monoculture_fragility, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(agricultural_monoculture_fragility, extractiveness, 0.58).
narrative_ontology:constraint_metric(agricultural_monoculture_fragility, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(agricultural_monoculture_fragility, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(agricultural_monoculture_fragility, tangled_rope).
narrative_ontology:human_readable(agricultural_monoculture_fragility, "Agricultural Monoculture Fragility and Systemic Risk").
narrative_ontology:topic_domain(agricultural_monoculture_fragility, "agricultural_economics/environmental_systems").

domain_priors:requires_active_enforcement(agricultural_monoculture_fragility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(agricultural_monoculture_fragility, industrial_agricultural_corporations).
narrative_ontology:constraint_beneficiary(agricultural_monoculture_fragility, commodity_traders).
narrative_ontology:constraint_beneficiary(agricultural_monoculture_fragility, chemical_input_suppliers).
narrative_ontology:constraint_victim(agricultural_monoculture_fragility, smallholder_farmers).
narrative_ontology:constraint_victim(agricultural_monoculture_fragility, food_security_commons).
narrative_ontology:constraint_victim(agricultural_monoculture_fragility, soil_health_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALLHOLDER FARMER (SNARE) — Trapped by debt for hybrid seeds, chemical fertilizers, and pesticides; cannot access land without integrated credit packages; cannot exit monoculture without losing certification and market access. High suppression: credit structures lock farmers into input dependency; market infrastructure only accepts commodity volumes from monoculture. Maximum experienced extraction — full structural immobility.
constraint_indexing:constraint_classification(agricultural_monoculture_fragility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL FOOD NETWORK (TANGLED ROPE) — Experiences both coordination benefit (monoculture achieves reliable caloric output and price stability) and extraction (vulnerability to crop failure; soil degradation reduces long-term productive capacity; forced dependency on external inputs). Constrained exit: high costs of dietary diversification and supply chain restructuring, but not insurmountable. Mixed benefit-cost structure.
constraint_indexing:constraint_classification(agricultural_monoculture_fragility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: INDUSTRIAL AGRICULTURE CORPORATIONS & INPUT SUPPLIERS (ROPE) — Primary beneficiaries. Experience the monoculture system as pure coordination: reliable supply, standardized varieties, predictable input demand. Vertical integration creates arbitrage options — can shift crops, move operations, or substitute input suppliers without constraint. Net extraction runs toward this institutional actor.
constraint_indexing:constraint_classification(agricultural_monoculture_fragility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: AGROECOLOGY MOVEMENT (SCAFFOLD) — Organized agents (NGOs, research institutions, farmer cooperatives) see monoculture fragility as a temporary institutional arrangement with a sunset. Soil regeneration, polyculture systems, and integrated pest management offer verifiable alternatives. Has sunset clause: as agroecological knowledge diffuses and supply chains mature, monoculture's extraction mechanism loses force. Estimated transition window: 15-30 years depending on regional context and policy support.
constraint_indexing:constraint_classification(agricultural_monoculture_fragility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: AGRICULTURAL POLICY & SUBSIDY STRUCTURE (PITON) — Government subsidies and commodity-price supports were designed to stabilize food security after WWII and industrialization. The original coordination function (prevent famine, enable land-use transition) has atrophied; subsidies now maintain monoculture through inertia while creating perverse incentives against diversification. Theater ratio high: policy apparatus performs food security rhetoric while actual mechanism locks in fragile extraction. Sustained by institutional inertia, not by functional necessity.
constraint_indexing:constraint_classification(agricultural_monoculture_fragility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, monoculture may appear as an inevitable natural law: high-yield agriculture requires specialization; economies of scale require standardization; comparative advantage requires specialization. This perspective risks naturalizing contingent institutional choices (subsidy structures, input supply chains, credit systems) as inherent to food production. The engine will flag this as a false summit, revealing that the 'inherent to agricultural economics' framing obscures agency in policy and market design.
constraint_indexing:constraint_classification(agricultural_monoculture_fragility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(agricultural_monoculture_fragility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(agricultural_monoculture_fragility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(agricultural_monoculture_fragility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(agricultural_monoculture_fragility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(agricultural_monoculture_fragility, TR),
    TR >= 0.70.

:- end_tests(agricultural_monoculture_fragility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The industrial agriculture system extracts from smallholder farmers through credit-input dependency, from soil through nutrient mining, and from food security commons through fragility risk. The value reflects that extraction is substantial but not absolute — monoculture does deliver reliable caloric output and some farmers receive premium prices. However, the extraction has increased over the interval as input costs have risen, soil has degraded, and climate volatility has increased risk. The original function (post-WWII stabilization) has atrophied, exposing the extraction mechanism. Suppression (0.68): High. Structural barriers include: (1) credit systems designed exclusively for monoculture-scaled operations; (2) certification standards (organic, fair-trade) that require minimum land sizes inappropriate for smallholder diversification; (3) commodity price infrastructure that only accepts bulk homogeneous crops; (4) knowledge barriers (agroecological techniques require reskilling); (5) government subsidy structures that penalize diversification; (6) market concentration (seed, chemicals, commodity trading consolidated into 4-6 corporations per input category). Theater ratio (0.55): Moderate. The original function (food security coordination) has partially atrophied, and policy discourse emphasizes food security while actual mechanisms lock in monoculture. However, the theater is not dominant — real coordination value persists (monoculture does reliably produce calories) and real extraction persists (farmers really are debt-trapped). This is why classification is tangled rope, not piton. The system is neither purely functional nor purely performative.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates clear perspectival divergence across power and exit-option axes. Smallholder farmers see a snare (trapped + powerless = maximum extraction + no mobility). Industrial agriculture sees a rope (institutional + arbitrage = minimal extraction + maximum mobility). The regional food network sees a tangled rope (moderate power + constrained exit = mixed experience). The agroecology movement sees a scaffold (organized + constrained + sunset = temporary extraction with exit pathway). Agricultural policy sees a piton (institutional + arbitrage, but with atrophied function = maintenance through inertia). The analytical observer risks a false mountain (civilizational scope tempts naturalizing as inherent to agriculture). The perspectival gaps reveal that the system's sustainability depends entirely on the structural position of the observer — for those trapped in it, it is obviously unsustainable; for those extracting from it, it appears sustainable indefinitely; for those with organizing power, it appears as a solvable coordination problem with a clear exit timeline.
 *
 * DIRECTIONALITY LOGIC:
 *   The d value for smallholder farmers is high (0.88–0.95) because they are structural victims with trapped exit options. Credit dependency, lack of alternative land access, and market penalty for non-commodity crops create material immobility. The d value for industrial agriculture is low (0.08–0.15) because they are beneficiaries with arbitrage options — they can shift crops, regions, or input products without constraint. The d value for the regional food network is moderate (0.50–0.55) because the system benefits them through price stability and caloric reliability, but also extracts through fragility risk and input cost escalation. The piton institutional actor (agricultural policy) has a moderately low d (0.20–0.30) because policy benefits incumbent interests (agribusiness, large landholders) while the original coordination function has atrophied. The asymmetry in d values across institutional actors (beneficiary vs victim) is the key diagnostic for regulatory capture and path dependency: policy that was originally designed to stabilize food security now maintains a system that destabilizes food security for those without political power.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The apparent mandatrophy (is monoculture a rational coordination solution or an extractive trap?) resolves by recognizing that it is BOTH depending on temporal and power axis. In the immediate/institutional context (industrial agriculture's view), monoculture is rational coordination — reliable supply, input efficiency, capital optimization. In the biographical/powerless context (smallholder farmer's view), monoculture is a snare — structurally trapped. In the generational/organized context (agroecology movement's view), monoculture is a temporary scaffold — solvable with clear exit timeline. The mandatrophy dissolves when we recognize that 'rationality' is perspective-dependent. The constraint is rationally coordinated FROM THE PERSPECTIVE OF THOSE WHO BENEFIT and rationally unsustainable FROM THE PERSPECTIVE OF THOSE BEARING THE COST. The analytical observer's temptation to treat monoculture as a natural law is revealed as false naturalization — the system persists because its beneficiaries have power to maintain policy and subsidy structures that lock in smallholder participation, not because monoculture is inherently necessary to food production. The agroecology perspective demonstrates that alternatives with comparable yield and lower cost exist; their non-adoption reflects institutional lock-in, not technical infeasibility.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    crop_failure_threshold,
    'At what combination of disease pressure, pest outbreak, and climate stress does a monoculture system experience critical failure? How does this threshold vary by crop, region, and soil condition?',
    'Longitudinal analysis of crop failure events (Irish potato famine, banana Panama disease, corn leaf blight); correlation between diversity metrics and failure probability; stress-testing of representative monocultures',
    'If threshold is breached regularly: extraction mechanism is already failing (snare dominates). If threshold is distant: extraction can continue longer before systemic failure (rope persists). The threshold determines which perspectives see imminent collapse vs. sustainable extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(crop_failure_threshold, empirical, 'Critical failure threshold for monoculture systems').

omega_variable(
    soil_degradation_irreversibility,
    'Is soil degradation from continuous monoculture structurally reversible within farmer timescales (5-20 years) or does it require generational timescales (50+ years) or institutional intervention?',
    'Soil health measurements before/after transition to polyculture; comparison of regeneration timelines across regions; quantification of mycorrhizal network recovery and nutrient cycling restoration',
    'If reversible in < 10 years: exit cost for farmers declines, constrained → mobile (tangled rope weakens). If irreversible or > 50 years: farmers remain permanently trapped (snare persists). This determines whether scaffold sunset is achievable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(soil_degradation_irreversibility, empirical, 'Reversibility of soil degradation from monoculture').

omega_variable(
    polyculture_yield_gap,
    'What is the stable yield difference between optimized polyculture systems and industrial monoculture? Does polyculture achieve 80%, 90%, or parity with monoculture at global caloric requirement scale?',
    'Comparative yield analysis across representative regions; long-term field trials of polyculture vs monoculture with equivalent inputs; analysis of yield variance across climate scenarios',
    'If polyculture ≥ 85% of monoculture yield: scaffold becomes realistic (transition cost acceptable). If polyculture < 70%: food security trade-off is severe, and monoculture extraction persists as rationalized necessity. This determines whether the system is genuinely unsustainable or merely risky.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(polyculture_yield_gap, empirical, 'Stable yield difference between polyculture and monoculture').

omega_variable(
    credit_system_path_dependency,
    'Are smallholder farmers locked into monoculture-compatible credit systems by institutional path dependency, or do they actively choose monoculture when alternative credit systems are available?',
    'Analysis of farmer choice behavior when given access to agroecological credit products (zero-interest term loans for soil building, crop insurance for diversified systems); comparison of adoption rates across credit regimes',
    'If path dependency dominates: exit is truly trapped (snare classification holds). If active preference dominates: farmers'' preference for monoculture reliability is rational given systemic risk (tangled rope becomes more accurate). This determines whether the binding mechanism is structural or cognitive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credit_system_path_dependency, empirical, 'Credit system path dependency vs. farmer preference').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(agricultural_monoculture_fragility, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(agmono_tr_t0, agricultural_monoculture_fragility, theater_ratio, 0, 0.3).
narrative_ontology:measurement(agmono_tr_t20, agricultural_monoculture_fragility, theater_ratio, 20, 0.42).
narrative_ontology:measurement(agmono_tr_t40, agricultural_monoculture_fragility, theater_ratio, 40, 0.55).

% Extraction over time
narrative_ontology:measurement(agmono_be_t0, agricultural_monoculture_fragility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(agmono_be_t20, agricultural_monoculture_fragility, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(agmono_be_t40, agricultural_monoculture_fragility, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(agricultural_monoculture_fragility, resource_allocation).
narrative_ontology:affects_constraint(agricultural_monoculture_fragility, food_system_fragility).
narrative_ontology:affects_constraint(agricultural_monoculture_fragility, agricultural_debt_dependency).
narrative_ontology:affects_constraint(agricultural_monoculture_fragility, pesticide_residue_accumulation).

% DUAL FORMULATION NOTE:
% Agricultural monoculture fragility decomposes into multiple structurally distinct constraints: (1) the yield/efficiency coordination problem (monoculture as rational solution to scale requirements), (2) the smallholder debt trap (credit systems dependent on monoculture input purchases), (3) the soil degradation commons problem (negative externality of continuous monoculture). Each has its own ε value and network of affects. This story focuses on the systemic fragility emerging from the interaction of these constraints. The upstream food system fragility constraint (ε ≈ 0.45) describes the general vulnerability of global food systems; agricultural monoculture fragility (this story, ε = 0.58) describes the specific institutional structure that creates that fragility. The downstream constraints detail the mechanisms: debt dependency (ε ≈ 0.72, snare) and pesticide accumulation (ε ≈ 0.38, tangled rope).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(agricultural_monoculture_fragility, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
