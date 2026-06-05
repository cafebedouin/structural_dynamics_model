% ============================================================================
% CONSTRAINT STORY: climate_policy_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_climate_policy_capture, []).

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
 *   constraint_id: climate_policy_capture
 *   human_readable: Climate Policy Capture by Incumbent Energy Interests
 *   domain: energy_policy/climate_governance
 *
 * SUMMARY:
 *   Climate policy capture represents a structural constraint in which
 *   incumbent fossil fuel and carbon-intensive industries maintain regulatory
 *   dominance over climate policy mechanisms, suppressing effective
 *   mitigation action and distributing the costs of atmospheric carbon
 *   accumulation to future generations and climate-vulnerable populations.
 *   The constraint exhibits the full range of DR classification types
 *   depending on observational position. From the perspective of future
 *   generations and climate system stability, it is a pure snare:
 *   structurally trapped actors bearing catastrophic costs with zero exit
 *   options. From the perspective of renewable energy producers and organized
 *   climate coalitions, it is a tangled rope with an incipient sunset
 *   mechanism: genuine coordination problems (energy transition coordination,
 *   grid stability, workforce transition) coexist with asymmetric extraction
 *   favoring incumbents. From the fossil fuel industry's perspective, it is
 *   pure rope: the constraint solves the coordination problem of managing
 *   profitable transition while protecting asset values. From the
 *   international governance perspective, it is a piton: the UNFCCC and Paris
 *   Agreement architecture persist as performative rituals with minimal
 *   enforcement capacity, maintained through institutional inertia. From the
 *   regulatory institution's perspective, it is tangled rope with
 *   identity-lock dynamics: genuine coordination functions (energy policy,
 *   industrial competitiveness) coexist with extraction dynamics (regulatory
 *   capture, revolving-door employment), and regulators may be
 *   identity-locked to fossil fuel paradigms through professional
 *   socialization. The analytical observer risks seeing an immutable law of
 *   political economy, but the structural data reveals this as false summit
 *   naturalization.
 *
 * KEY AGENTS:
 *   - Incumbent Fossil Fuel Producers: Primary beneficiary (institutional/arbitrage) — fossil fuel companies, utilities with fossil baseload, petrochemical manufacturers benefit from delayed climate policy, lenient emissions standards, continued subsidies, and regulatory dominance. Arbitrage options enable jurisdiction shopping and policy shopping.
 *   - Climate System / Future Generations: Primary victim (powerless/trapped) — the physical climate system and humans living 2100+ are structurally trapped by atmospheric CO2 accumulation; no exit option, bearing costs of delayed mitigation through increased warming, ecosystem collapse, civilizational instability.
 *   - Climate-Vulnerable Populations: Secondary victim (moderate/constrained) — low-income communities, Global South nations, subsistence-dependent populations face extraction through climate damages, resource scarcity, and displacement. Constrained by economic integration into carbon-intensive systems.
 *   - Renewable Energy Sector: Organized beneficiary-victim (organized/constrained) — solar, wind, battery manufacturers benefit from transition but face extraction through incumbent dominance, fossil fuel subsidies, and regulatory barriers. Organized but constrained by incumbent power; building coalition escape pathways.
 *   - Regulatory Institutions / Policymakers: Institutional actor (institutional/constrained, identity-locked variant possible) — agencies charged with climate policy face mixed incentives: genuine coordination function (energy policy, industrial planning) coexists with capture dynamics (industry lobbying, revolving-door employment, epistemic closure). May be identity-locked to fossil fuel paradigm through professional socialization.
 *   - International Climate Governance: Institutional actor (institutional/arbitrage) — UNFCCC, Paris Agreement, national climate bureaucracies maintain performative functions while lacking enforcement capacity to constrain extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(climate_policy_capture, 0.62).
domain_priors:suppression_score(climate_policy_capture, 0.68).
domain_priors:theater_ratio(climate_policy_capture, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(climate_policy_capture, extractiveness, 0.62).
narrative_ontology:constraint_metric(climate_policy_capture, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(climate_policy_capture, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(climate_policy_capture, tangled_rope).
narrative_ontology:human_readable(climate_policy_capture, "Climate Policy Capture by Incumbent Energy Interests").
narrative_ontology:topic_domain(climate_policy_capture, "energy_policy/climate_governance").

domain_priors:requires_active_enforcement(climate_policy_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(climate_policy_capture, incumbent_fossil_fuel_producers).
narrative_ontology:constraint_beneficiary(climate_policy_capture, carbon_intensive_manufacturing).
narrative_ontology:constraint_victim(climate_policy_capture, climate_system_stability).
narrative_ontology:constraint_victim(climate_policy_capture, future_generations).
narrative_ontology:constraint_victim(climate_policy_capture, vulnerable_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CLIMATE SYSTEM / FUTURE GENERATIONS (SNARE) — Structurally trapped by the physics of cumulative atmospheric CO2. Cannot exit the constraint; bears full cost of delayed mitigation through increased climate damages, ecosystem collapse, and civilizational instability. No advocate with equivalent power. Maximum experienced extraction with zero degrees of freedom.
constraint_indexing:constraint_classification(climate_policy_capture, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: CLIMATE-VULNERABLE POPULATIONS (TANGLED ROPE) — Face both extraction (bearing costs of delayed action) and genuine coordination benefits (climate adaptation infrastructure, green jobs in transition sectors). Structurally constrained by economic dependence on existing energy systems and political marginalization. Mixed experience: some benefit from climate policy mechanisms alongside the primary extraction.
constraint_indexing:constraint_classification(climate_policy_capture, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT FOSSIL FUEL INDUSTRY (ROPE) — Net beneficiary experiencing the constraint as pure coordination: delayed climate policy, lenient emissions standards, and continued fossil fuel subsidies enable business continuity and asset protection. Experiences the constraint as solving a legitimate problem (managing energy transition at profitable pace). Arbitrage exit options enable regulatory shopping and jurisdiction arbitrage.
constraint_indexing:constraint_classification(climate_policy_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: RENEWABLE ENERGY SECTOR / CLIMATE ACTION COALITION (TANGLED ROPE) — Organized agents with agency and exit pathways. Experience the constraint as mixed: extraction (delayed policy reduces their market access, competitive disadvantage vs subsidized fossil fuels) and coordination (state-mandated renewable procurement, carbon pricing mechanisms do solve genuine coordination problems). Constrained by incumbent political dominance but not trapped — have alternative organizational pathways and growing coalition power.
constraint_indexing:constraint_classification(climate_policy_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL CLIMATE GOVERNANCE ARCHITECTURE (PITON) — UNFCCC, Paris Agreement, and national climate commitments are largely performative at the enforcement level. Signatories commit to emissions reductions but lack enforcement mechanisms; national interest and corporate lobbying override treaty obligations. The governance structure persists through institutional inertia and ritual (annual COPs, Nationally Determined Contributions) despite minimal functional capacity to constrain extraction. Theater ratio reflects gap between stated commitments and actual emissions trajectories.
constraint_indexing:constraint_classification(climate_policy_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risks naturalizing climate policy capture as an inevitable feature of economic systems: incumbent industries always resist disruptive change; coordination failures always occur in commons problems; powerful actors always capture regulatory mechanisms. This perspective sees the constraint as an immutable structural law of political economy. However, the structural data (strong organized opposition, policy alternatives, coalition power, regulatory mechanisms) contradicts the mountain classification — the engine will identify this as false summit: naturalization of a contingent institutional arrangement.
constraint_indexing:constraint_classification(climate_policy_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(climate_policy_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(climate_policy_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(climate_policy_capture, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(climate_policy_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(climate_policy_capture, TR),
    TR >= 0.70.

:- end_tests(climate_policy_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): Moderate-high. Fossil fuel interests extract substantial rents through delayed climate policy, continued subsidies (~$7 trillion globally accounting for externality costs), and regulatory capture that suppresses renewable deployment and carbon pricing mechanisms. The extractiveness is not maximal (0.72+) because genuine coordination problems exist (energy transition requires coordination; grid stability has legitimate technical constraints; workforce transition requires planning) that justify some policy complexity. Suppression (0.68): High. Multiple barriers prevent effective climate action: fossil fuel industry political spending and lobbying dominance, regulatory capture through revolving-door employment, epistemic closure in policymaking institutions, fragmented international governance with no enforcement mechanism, sunk costs in fossil infrastructure, and coordination failures in multi-level governance. Economic dependence on fossil systems constrains exit for many actors. Theater ratio (0.65): Moderate-high. Significant performative content in climate governance: international climate agreements lack enforcement mechanisms; national climate plans show persistent gap between stated NDCs and actual emissions; corporate net-zero commitments rely on unverified offset accounting; climate finance mechanisms show low disbursement-to-commitment ratios. Theater has increased over the measurement interval as climate urgency has risen while policy effectiveness has stalled, suggesting performative substitution (more ritual without more function).
 *
 * PERSPECTIVAL GAP:
 *   The snare-to-rope perspectival gap reveals the asymmetric extraction mechanism. The primary beneficiary (fossil fuel industry) experiences pure coordination — the constraint solves their problem of managing profitable transition. The primary victims (climate system, future generations) experience pure extraction with zero agency. Secondary victims (climate-vulnerable populations, renewable sector) experience tangled rope — mixed extraction and coordination, but with power asymmetries that favor incumbents. The regulatory institutions show internal contradiction: their formal function is climate mitigation (suggesting victim perspective), but their actual policy outputs favor fossil fuel incumbents (suggesting beneficiary perspective or capture). This gap indicates regulatory capture rather than neutral institutional action.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's directionality value (d) derives from power level, exit options, and beneficiary/victim status. Fossil fuel producers: institutional power + arbitrage exit + beneficiary status → d ≈ 0.05-0.15 (low directionality, experience negative χ). Future generations: powerless + trapped exit + victim status → d ≈ 0.95 (maximum directionality, experience high χ). Renewable sector: organized power + constrained exit + victim-beneficiary status → d ≈ 0.55-0.65 (moderate directionality, experiencing mixed χ). Regulatory institutions: institutional power + constrained exit + mixed status → d ≈ 0.40-0.50 (moderate-high directionality if victim-leaning, lower if beneficiary-leaning due to capture). The structural data shows that regulatory institutions' exit options are more constrained than their power level suggests, indicating possible identity-lock: institutions behave as if fossil fuel dominance were inevitable even when policy alternatives exist (suggesting internal frame constraint rather than external barrier).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION: The constraint resolves the mandatrophy through identification of structural asymmetry between coordination function and extraction mechanism. Genuine coordination problems exist: energy system transition requires planning, grid stability requires coordination, workforce transition requires support. But these coordination functions are realized through mechanisms that systematically favor incumbents and delay action: policy delay itself becomes the coordination mechanism (slow-enough transition to protect fossil assets), regulatory complexity becomes the extraction mechanism (barriers to renewable deployment embedded in technical requirements), and international fragmentation becomes the structural enabler (no enforcement mechanism to overcome coordination failures). The mandatrophy is resolved by recognizing that the constraint's claimed coordination function (managing energy transition) is actually decoupled from the constraint's actual extraction mechanism (delaying effective transition). This is textbook Goodhart substitution: as climate science has created pressure for action, policymakers have increasingly substituted performative compliance (climate rhetoric, voluntary commitments, offset accounting) for functional transition (rapid emissions reduction, regulatory constraints on fossil production). The theater ratio increase (0.40 → 0.65) demonstrates this substitution. The constraint remains tangled rope (genuine coordination and extraction coexist) but the balance has tilted toward extraction as theater has displaced function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_capture_vs_structural_power,
    'Is fossil fuel dominance of climate policy a result of regulatory capture by a discrete powerful actor, or a structural feature of capital accumulation incentives across multiple actors?',
    'Comparative policy analysis across jurisdictions with different regulatory structures; analysis of whether capture persists when specific captured regulators are replaced; test whether structural incentives reproduce capture independent of individual actors.',
    'If discrete capture: snare classification becomes more concentrated, limited number of beneficiaries can be identified. If structural: extraction mechanism is more diffuse, harder to target reform efforts, classification remains snare but with more symmetrical structural distribution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_vs_structural_power, conceptual, 'Whether climate policy capture is individual regulatory capture or structural economic incentive').

omega_variable(
    tipping_point_threshold,
    'At what carbon concentration / atmospheric threshold do feedback loops make the constraint''s extraction costs catastrophic even to the beneficiaries?',
    'Climate modeling of positive feedback cascades (methane release, ice sheet instability, ecosystem collapse); economic analysis of damage costs exceeding fossil fuel value extraction.',
    'If threshold is decades away: snare classification holds. If threshold is years away: extraction mechanism may collapse endogenously, classification degrades toward chaos/dissolution rather than simple snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(tipping_point_threshold, empirical, 'Threshold at which climate damages exceed fossil fuel extraction value').

omega_variable(
    coalition_power_transition_point,
    'At what political/economic scale do renewable energy producers and climate action coalitions achieve sufficient organized power to break the capture mechanism?',
    'Trend analysis of renewable sector market share, political spending, legislator alignment shifts; identification of jurisdiction-level transition points where green energy becomes the organized beneficiary instead of fossil fuels.',
    'If transition imminent (5-10 years): scaffold perspective gains credibility, sunset becomes structural. If transition requires 30+ years: snare classification holds through generational timescale.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_power_transition_point, empirical, 'Transition point when renewable energy becomes dominant organized beneficiary').

omega_variable(
    identity_lock_in_regulatory_institutions,
    'Are regulatory agencies and policymakers identity-locked to fossil fuel paradigms through professional socialization, career path dependence, and epistemic closure, or do they maintain capacity to shift positions if power balance changes?',
    'Historical analysis of regulatory personnel transitions and belief system shifts; identification of jurisdictions where new generations of regulators exhibit different policy preferences; analysis of whether captured regulators can be mobilized by coalition power shift.',
    'If identity-locked: institutional perspectives remain classified as constrained rather than mobile; change requires institutional degradation and replacement. If capacity preserved: institutions classified as mobile; shift to renewable dominance may trigger rapid policy realignment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_regulatory_institutions, conceptual, 'Whether regulatory institutions are identity-locked to fossil fuel paradigm').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(climate_policy_capture, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(climate_tr_t0, climate_policy_capture, theater_ratio, 0, 0.4).
narrative_ontology:measurement(climate_tr_t10, climate_policy_capture, theater_ratio, 10, 0.58).
narrative_ontology:measurement(climate_tr_t20, climate_policy_capture, theater_ratio, 20, 0.65).
narrative_ontology:measurement(climate_tr_t5, climate_policy_capture, theater_ratio, 5, 0.5).
narrative_ontology:measurement(climate_tr_t15, climate_policy_capture, theater_ratio, 15, 0.62).

% Extraction over time
narrative_ontology:measurement(climate_be_t0, climate_policy_capture, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(climate_be_t10, climate_policy_capture, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(climate_be_t20, climate_policy_capture, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(climate_be_t5, climate_policy_capture, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(climate_be_t15, climate_policy_capture, base_extractiveness, 15, 0.56).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(climate_policy_capture, resource_allocation).
narrative_ontology:affects_constraint(climate_policy_capture, carbon_pricing_mechanism_design).
narrative_ontology:affects_constraint(climate_policy_capture, fossil_fuel_subsidy_lock_in).
narrative_ontology:affects_constraint(climate_policy_capture, renewable_energy_market_access).
narrative_ontology:affects_constraint(climate_policy_capture, international_climate_finance_mechanism).

% DUAL FORMULATION NOTE:
% Climate policy capture is downstream of multiple constraint families: carbon pricing mechanism design (technical governance constraint), fossil fuel subsidy lock-in (fiscal/institutional constraint), renewable energy market access (sectoral competition constraint), and international climate finance (development cooperation constraint). Each constraint family has distinct ε values reflecting different measurement bases. Climate policy capture integrates extraction mechanisms from all four families but represents distinct structural phenomenon: the political-institutional mechanism through which extraction is maintained across multiple policy domains.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(climate_policy_capture, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
