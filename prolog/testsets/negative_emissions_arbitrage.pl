% ============================================================================
% CONSTRAINT STORY: negative_emissions_arbitrage
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_negative_emissions_arbitrage, []).

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
 *   constraint_id: negative_emissions_arbitrage
 *   human_readable: Negative Emissions Arbitrage Market
 *   domain: economic/political/technological
 *
 * SUMMARY:
 *   The scientific consensus that climate stabilization requires net-zero CO2
 *   emissions created a global carbon ledger that enabled a novel form of
 *   arbitrage: trading future emissions reduction for present-day
 *   high-emitting activity. This constraint exhibits the fundamental tension
 *   between coordination (financing mitigation projects) and extraction
 *   (allowing temporal deferral of climate risk). The market coordinates
 *   capital toward carbon removal but structurally extracts from populations
 *   without exit options. The negative emissions arbitrage is a Tangled Rope
 *   from the institutional perspective (traders, removal firms, emitters
 *   experience genuine coordination benefit) and a Snare from the vulnerable
 *   population perspective (no exit from climate impacts, no leverage over
 *   removal timelines). The theater ratio (0.68) reflects that large portions
 *   of the carbon credit ecosystem lack real verification: avoided emissions
 *   credits are counterfactual claims; temporary biological carbon storage is
 *   measured in decades when relevant timescales are centuries; and emissions
 *   accounting (Scope 3, lifecycle analysis) allows corporations to claim
 *   mitigation without reducing actual emissions. The constraint's
 *   extractiveness has increased from 0.35 to 0.58 over the interval as
 *   credit issuance has outpaced removal capacity, indicating Goodhart-type
 *   drift where accounting measures replace actual function.
 *
 * KEY AGENTS:
 *   - Climate-Vulnerable Communities: Primary victim (powerless/trapped) — bear temperature overshoot costs; cannot exit climate system or control removal timelines
 *   - Mid-Tier Emitting Nations: Secondary victim (moderate/constrained) — face constrained choice between carbon market participation and trade exclusion; border carbon adjustments reduce exit options
 *   - Carbon Credit Traders and Brokers: Primary beneficiary (institutional/arbitrage) — capture spread between high-emitter willingness-to-pay and low-cost removal project pricing; can redeploy capital at any time
 *   - DAC Industry and Technology Providers: Secondary beneficiary (organized/constrained) — benefit from policy mandates and credit demand but face capital-lock and technology-pathway suppression
 *   - Developed Economy Polluters: Primary beneficiary (institutional/arbitrage) — can continue high-emission activities by purchasing future offset credits; arbitrage exploits cheaper removal in jurisdictions without legacy emissions liability
 *   - IPCC and Scientific Consensus: Institutional actor (institutional/arbitrage) — established the net-zero target; operates through performative accounting frameworks; sees own metrics as degraded but continues enforcement through inertia
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(negative_emissions_arbitrage, 0.58).
domain_priors:suppression_score(negative_emissions_arbitrage, 0.65).
domain_priors:theater_ratio(negative_emissions_arbitrage, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(negative_emissions_arbitrage, extractiveness, 0.58).
narrative_ontology:constraint_metric(negative_emissions_arbitrage, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(negative_emissions_arbitrage, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(negative_emissions_arbitrage, tangled_rope).
narrative_ontology:human_readable(negative_emissions_arbitrage, "Negative Emissions Arbitrage Market").
narrative_ontology:topic_domain(negative_emissions_arbitrage, "economic/political/technological").

domain_priors:requires_active_enforcement(negative_emissions_arbitrage).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(negative_emissions_arbitrage, carbon_credit_traders).
narrative_ontology:constraint_beneficiary(negative_emissions_arbitrage, capital_intensive_removers).
narrative_ontology:constraint_beneficiary(negative_emissions_arbitrage, developed_economy_polluters).
narrative_ontology:constraint_victim(negative_emissions_arbitrage, vulnerable_populations).
narrative_ontology:constraint_victim(negative_emissions_arbitrage, future_generations).
narrative_ontology:constraint_victim(negative_emissions_arbitrage, ecosystem_carbon_sinks).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE-VULNERABLE COMMUNITIES (SNARE) — Small island nations and low-lying agricultural regions bear the full cost of temperature overshoot while wealthy emitters purchase future negative emission credits at present-day prices. Cannot exit climate system; cannot prevent temperature excursions during the waiting period for carbon removal to scale. Maximum extraction through temporal arbitrage: present harm financed by promises of future remediation.
constraint_indexing:constraint_classification(negative_emissions_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-TIER EMITTING NATIONS (TANGLED ROPE) — Middle-income countries face constrained choices: participate in carbon markets to fund mitigation, or face carbon border adjustments and market exclusion. The market coordinates capital toward mitigation-adjacent activities but extracts rents through credit intermediation. Net negative: more capital leaves than enters; coordination benefit is genuine but asymmetrically shared.
constraint_indexing:constraint_classification(negative_emissions_arbitrage, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CARBON CREDIT TRADERS AND BROKERS (ROPE) — Financial intermediaries experience the constraint as pure coordination: matching high-cost emitters seeking offsets with low-cost removal projects. No suppression of this agent's exit — can redeploy capital at any time. Benefits flow directly to traders via spread capture. Theater ratio is high for this perspective (verification of actual removal is difficult), but experienced extractiveness is negative (the trader benefits).
constraint_indexing:constraint_classification(negative_emissions_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DAC INDUSTRY (TANGLED ROPE) — Large-scale removal technology (Climeworks, Carbon Engineering) benefits from policy mandates and carbon credit demand but faces capital-intensive constraints and technology lock-in. High suppression: once capital committed to specific technology pathway (e.g., solvent-based DAC vs sorbent), switching costs are extreme. Experiences both coordination (policy support) and extraction (price controls, subsidy competition from nature-based solutions).
constraint_indexing:constraint_classification(negative_emissions_arbitrage, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: IPCC ACCOUNTING FRAMEWORKS (PITON) — The scientific consensus that net-zero is the target was derived from climate models, but the enforcement of that target through carbon markets is largely performative. Emissions accounting protocols (Scope 1/2/3, lifecycle analysis, avoided emissions credits) are substantially theater: they enable claims of compliance without reducing atmospheric CO2. The accounting ritual persists through regulatory inertia even as actual carbon reduction mechanisms remain unscaled. Theater ratio (0.68) reflects this degradation.
constraint_indexing:constraint_classification(negative_emissions_arbitrage, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICAL CONSTRAINT VIEW (MOUNTAIN) — From a civilizational timescale, the physics of atmospheric CO2 stabilization is immutable: net-zero is indeed required, and temporal sequencing is irrelevant to final temperature outcome (what matters is cumulative CO2, not when it was emitted). This perspective sees the negative emissions market as merely financing the inevitable requirement. However, this naturalization obscures the institutional arrangement: the market allows arbitrage that delays immediate mitigation, which is not a law of physics but a contingent financial design choice. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(negative_emissions_arbitrage, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(negative_emissions_arbitrage_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(negative_emissions_arbitrage, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(negative_emissions_arbitrage, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(negative_emissions_arbitrage, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(negative_emissions_arbitrage, TR),
    TR >= 0.70.

:- end_tests(negative_emissions_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint allows present-day high-emitting activity (airline flights, heavy manufacturing, energy production) to be financed by future carbon removal credits purchased at current prices. If future removal technology is cheaper or faster than expected, current credit purchasers profit; if removal technology fails to scale, purchasers suffer no consequences while vulnerable populations bear temperature damages. The extraction is significant — it is a form of option-pricing on future climate outcomes with asymmetric payoff. However, the extraction is not maximal (0.66+) because the coordination function is real: capital does flow toward removal projects, some of which are legitimate mitigation investments. Suppression (0.65): Moderate-high. Barriers to exiting the system include regulatory mandates for corporate carbon accounting, trade pressure through carbon border adjustments, and the global coordination problem: any individual jurisdiction reducing unilaterally suffers economic disadvantage without commensurate climate benefit. These barriers are substantial but not total — some jurisdictions resist carbon market participation, though at political cost. Theater ratio (0.68): High. The emissions accounting ecosystem has substantial performative content: avoided emissions credits rely on counterfactual claims about what would have happened without the project; temporary biological storage (trees) is credited as permanent removal; Scope 3 accounting allows firms to claim mitigation of supply-chain emissions without reducing actual supply-chain output; lifecycle analysis credits embedded energy savings that never reduce final consumption patterns.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap in this constraint is extreme and follows power gradients precisely. The trader and institutional emitter perspectives see Rope (pure coordination, no extraction) or Tangled Rope with genuine benefit (coordination is real; extraction is moderate). The vulnerable population perspective sees Snare (pure extraction, no coordination benefit). The mid-tier nation perspective sees Tangled Rope with genuine suppression (forced participation, asymmetric rents). The technological removal industry sees Tangled Rope with capital-lock constraints (coordinated by policy, extracted through price competition). The IPCC perspective sees Piton (performative accounting ritual). The analytical observer risks seeing Mountain (net-zero is physically immutable) but the structural data reveals the market design as contingent: temporal arbitrage is not a law of physics but a financial architecture choice. Different indexical tuples see genuinely different constraints at the same label.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values are derived from each agent's structural position within the extraction flow. Carbon traders experience low d (~0.10) because they are beneficiaries with arbitrage exit options — they can redeploy capital whenever the credit market becomes less profitable. Developed-economy polluters experience low d (~0.15) because they are beneficiaries with institutional power — they can continue emissions and purchase offsets. Climate-vulnerable communities experience high d (~0.95) because they are victims with trapped exit options — they cannot leave the climate system and cannot control removal timelines. Mid-tier emitting nations experience moderate d (~0.60) because they are victims with constrained but not trapped options — they can choose not to participate in carbon markets but face border adjustments and competitive disadvantage. The sigmoid f(d) converts these d values to experienced extractiveness chi, amplifying the extraction experienced by trapped agents and dampening it for agents with exit options. The high d values for vulnerable populations mean they experience χ values approaching 1.5× their base extractiveness, while traders experience χ values below zero (actual benefits).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: This constraint satisfies the Tangled Rope gate through explicit structural data. It has both a coordination function (capital allocation to mitigation projects) and asymmetric extraction (temporal arbitrage allowing present emissions to be deferred). It requires active enforcement (carbon accounting standards, credit issuance protocols, trade agreements). Beneficiary groups (traders, emitters) are distinct from victim groups (vulnerable populations, future generations). The coordination function is not fake (removal projects are real and funded), but the extraction is real (timing asymmetry extracts value from those without exit options). The mandatrophy is resolved by acknowledging that 'net-zero' is a scientifically necessary target but the market mechanism for achieving it is not scientifically determined. The market could be designed with immediate mitigation requirements (Rope), but instead it is designed to allow deferral (Tangled Rope with extraction). The constraint is not misclassified coordination masquerading as extraction, nor is it pure extraction falsely labeled as coordination — both components are structurally present. The theater_ratio drift (0.42 → 0.68) indicates Goodhart-type metric substitution: carbon accounting has displaced actual emissions reduction as the measured goal, allowing extractive behavior to persist behind a coordination facade.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    permanence_verification_gap,
    'Can carbon removal permanence be verified at timescales relevant to emissions accounting (decades to centuries)?',
    'Long-term monitoring protocols for geological sequestration (saline formations, basalt); empirical failure rates for biological carbon storage; cost of verification vs cost of removal',
    'If permanence unverifiable: credits are theater, market is pure extraction (Snare from all perspectives). If verifiable: coordination function becomes real, market moves toward Rope from institutional perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(permanence_verification_gap, empirical, 'Whether carbon removal permanence can be credibly verified').

omega_variable(
    temporal_discounting_legitimacy,
    'Is present-day carbon credit purchase a legitimate hedge against future removal costs, or a speculative bet that future removal technology will subsidize present emissions?',
    'Comparison of present credit prices vs projected future removal costs; analysis of whether credit purchases have historically corresponded to realized removal capacity; cost-of-capital analysis for removal projects',
    'If legitimate hedge: market is Rope from trader perspective, Tangled Rope from vulnerable populations. If speculative bet: extraction is higher, vulnerable populations face pure Snare, mandate for present-day mitigation becomes non-binding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(temporal_discounting_legitimacy, empirical, 'Whether carbon credit pricing reflects legitimate future removal costs').

omega_variable(
    nat_based_displacement_dynamics,
    'Does large-scale deployment of technology-based removal (DAC, biochar) displace or complement nature-based solutions (reforestation, wetland restoration)?',
    'Land-use analysis comparing reforestation potential vs DAC deployment footprint; carbon cost per ton removed for each pathway; ecosystem service valuation',
    'If displaced: nature-based victims become more powerless (worse Snare); if complementary: ecosystem value is preserved, Tangled Rope classification holds. If nature-based solutions become preferred: DAC industry extraction increases (higher suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(nat_based_displacement_dynamics, empirical, 'Whether technology-based removal displaces or complements nature-based solutions').

omega_variable(
    credit_quality_hierarchy_stability,
    'Will carbon credit quality standards remain aligned with atmospheric CO2 reduction, or will credit markets stratify into high-cost ''permanent'' credits and low-cost ''temporary'' credits that mask continued atmospheric growth?',
    'Credit market pricing trends; separation of high-quality vs low-quality credit prices; regulatory enforcement of quality standards; correlation between issued credits and atmospheric CO2 concentration',
    'If stratification occurs: market becomes pure Snare (low-cost credits enable continued emissions). If alignment maintained: Tangled Rope classification holds, though theater remains high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credit_quality_hierarchy_stability, empirical, 'Whether carbon credit standards remain aligned with atmospheric CO2 reduction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(negative_emissions_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(negarb_tr_t0, negative_emissions_arbitrage, theater_ratio, 0, 0.42).
narrative_ontology:measurement(negarb_tr_t5, negative_emissions_arbitrage, theater_ratio, 5, 0.56).
narrative_ontology:measurement(negarb_tr_t10, negative_emissions_arbitrage, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(negarb_be_t0, negative_emissions_arbitrage, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(negarb_be_t5, negative_emissions_arbitrage, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(negarb_be_t10, negative_emissions_arbitrage, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(negative_emissions_arbitrage, resource_allocation).
narrative_ontology:affects_constraint(negative_emissions_arbitrage, climate_stabilization_timeline).
narrative_ontology:affects_constraint(negative_emissions_arbitrage, nature_based_carbon_sink_displacement).
narrative_ontology:affects_constraint(negative_emissions_arbitrage, developing_economy_tech_access).

% DUAL FORMULATION NOTE:
% The negative emissions market decomposes into two structurally distinct claims: (1) Scientific: net-zero is required for climate stabilization (Mountain, ε ≈ 0.08); (2) Institutional: carbon markets can achieve net-zero through credit trading (Tangled Rope, ε = 0.58). The scientific claim is upstream and unambiguous; the institutional claim is downstream and contested. This story addresses the institutional design choice, which is why it classifies as Tangled Rope rather than Mountain.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(negative_emissions_arbitrage, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
