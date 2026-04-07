% ============================================================================
% CONSTRAINT STORY: renewable_energy_supply_chain_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_renewable_energy_supply_chain_extraction, []).

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
 *   constraint_id: renewable_energy_supply_chain_extraction
 *   human_readable: Renewable Energy Supply Chain Extraction
 *   domain: energy_economics/labor_governance
 *
 * SUMMARY:
 *   The renewable energy supply chain exhibits a structural paradox:
 *   accelerating global deployment of wind, solar, and battery technologies
 *   drives exponential demand for rare earth minerals and critical metals
 *   whose extraction imposes severe labor and environmental costs
 *   concentrated in developing nations. The constraint combines genuine
 *   coordination (standardized rare earth inputs enable manufacturing
 *   efficiency and supply reliability) with asymmetric extraction (labor and
 *   environmental costs are externalized to powerless populations while
 *   profits flow to developed-nation firms and equipment manufacturers). The
 *   constraint is not incidental to renewable energy—it is structurally
 *   embedded in the current engineering and supply chain architecture.
 *   Theater is moderate (0.55) because sustainability certification systems
 *   create symbolic compliance without enforcing structural reform, yet the
 *   constraint retains real functional components (supply standardization).
 *   Extractiveness has increased from 0.42 to 0.58 over the interval as
 *   renewable deployment has accelerated, concentrating extraction pressure
 *   on fixed artisanal mining communities whose labor productivity and
 *   regulatory environment have not improved. The circular economy coalition
 *   (recycled rare earth recovery, material substitution) represents the
 *   primary structural exit pathway, but its timeline to cost parity is
 *   uncertain.
 *
 * KEY AGENTS:
 *   - Artisanal Mining Workers: Primary victims (powerless/trapped) — no exit options; bear labor and health costs; localized in resource-extraction regions with minimal governance
 *   - Resource Extraction Communities: Primary victims (powerless/trapped) — environmental degradation without compensation; economic dependency on mining; no agency in supply chain decisions
 *   - Equipment Manufacturers: Primary beneficiaries (institutional/arbitrage) — access to standardized inputs enables production scaling; negotiate terms with suppliers; capture efficiency gains
 *   - Developed Nation Energy Firms: Primary beneficiaries (powerful/mobile) — externalize costs while capturing green transition profits; have exit options but limited incentive to use them
 *   - Rare Earth Mining Operators: Secondary beneficiaries (institutional/arbitrage) — control supply bottleneck; profit from price premiums; consolidate production in low-governance regions
 *   - Developing Nation Grid Operators: Secondary victims (moderate/constrained) — forced to purchase imported equipment; cannot build alternative supply chains; constrained by capital and technology access
 *   - Sustainability Certification Systems: Institutional theater maintainers (institutional/arbitrage) — perpetuate perception of managed constraint; perform audits with weak enforcement; reduce pressure for structural reform
 *   - Circular Economy Coalition: Organized agents (organized/constrained) — building alternative recycled supply pathways; constrained by current cost structures but see generational sunset trajectory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(renewable_energy_supply_chain_extraction, 0.58).
domain_priors:suppression_score(renewable_energy_supply_chain_extraction, 0.68).
domain_priors:theater_ratio(renewable_energy_supply_chain_extraction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(renewable_energy_supply_chain_extraction, extractiveness, 0.58).
narrative_ontology:constraint_metric(renewable_energy_supply_chain_extraction, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(renewable_energy_supply_chain_extraction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(renewable_energy_supply_chain_extraction, tangled_rope).
narrative_ontology:human_readable(renewable_energy_supply_chain_extraction, "Renewable Energy Supply Chain Extraction").
narrative_ontology:topic_domain(renewable_energy_supply_chain_extraction, "energy_economics/labor_governance").

domain_priors:requires_active_enforcement(renewable_energy_supply_chain_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(renewable_energy_supply_chain_extraction, equipment_manufacturers).
narrative_ontology:constraint_beneficiary(renewable_energy_supply_chain_extraction, rare_earth_mining_operators).
narrative_ontology:constraint_beneficiary(renewable_energy_supply_chain_extraction, developed_nation_energy_firms).
narrative_ontology:constraint_victim(renewable_energy_supply_chain_extraction, artisanal_mining_workers).
narrative_ontology:constraint_victim(renewable_energy_supply_chain_extraction, resource_extraction_communities).
narrative_ontology:constraint_victim(renewable_energy_supply_chain_extraction, developing_nation_grid_operators).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Artisanal miners in rare earth extraction regions face no exit options: local economies depend entirely on mining revenue, labor law enforcement is minimal, and geographic isolation prevents access to alternative work. The constraint extracts labor value while suppressing alternative livelihood pathways. Maximum experienced extraction with zero agency.
constraint_indexing:constraint_classification(renewable_energy_supply_chain_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% Grid operators in energy-importing nations are constrained by dependency on imported equipment and minerals; they cannot build renewable infrastructure without engaging the supply chain. The constraint provides genuine coordination (standardized components enable interoperability) but extracts through markup, supply control, and tied purchasing. Significant costs alongside real benefits.
constraint_indexing:constraint_classification(renewable_energy_supply_chain_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Equipment manufacturers (turbine makers, solar panel producers) experience the supply chain as pure coordination: access to standardized rare earth inputs enables production scaling and efficiency gains. They are net beneficiaries with arbitrage options—they can source from multiple suppliers, negotiate pricing, and relocate production. Net positive relationship with the constraint.
constraint_indexing:constraint_classification(renewable_energy_supply_chain_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% Developed nation renewable energy firms benefit from the constraint through cost externalization (labor and environmental costs borne in the supply chain) while maintaining public green credentials. They have exit options but limited incentive to use them; the constraint enables profitable renewable transition. Mixed coordination (supply reliability) and extraction (cost shifting).
constraint_indexing:constraint_classification(renewable_energy_supply_chain_extraction, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% Sustainability certifications (conflict minerals, labor standards, environmental compliance) claim to regulate the constraint but are largely performative: audits are infrequent, penalties are weak, and enforcement is theater. The certification system persists through institutional inertia—it satisfies demand for accountability without imposing real costs on beneficiaries. Certification maintains the constraint by substituting symbolic compliance for structural change.
constraint_indexing:constraint_classification(renewable_energy_supply_chain_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Organized actors (recycling infrastructure initiatives, material scientists, policy coalitions) are building alternative supply pathways through recycled rare earth recovery and substitution chemistry. These pathways have sunset logic: as recycling scales and substitutes mature, dependency on virgin mining extraction declines. Current extraction persists, but structured alternatives are reducing its future necessity.
constraint_indexing:constraint_classification(renewable_energy_supply_chain_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(renewable_energy_supply_chain_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(renewable_energy_supply_chain_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(renewable_energy_supply_chain_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(renewable_energy_supply_chain_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(renewable_energy_supply_chain_extraction, TR),
    TR >= 0.70.

:- end_tests(renewable_energy_supply_chain_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The renewable energy supply chain extracts significant value through labor suppression, environmental cost externalization, and supply control, but extraction is not total because genuine coordination benefits exist (standardized inputs enable manufacturing efficiency and supply reliability). The value reflects that the constraint does solve a real coordination problem while simultaneously imposing asymmetric extraction. Suppression (0.68): High. Artisanal miners face multiple suppression mechanisms: geographic isolation limits alternative livelihoods, weak labor law enforcement reduces exit costs for employers, low educational access restricts occupational mobility, and supply chain opacity prevents consumer-side pressure. Developing nation governments have limited capacity to enforce standards given capital and technical dependencies. Theater ratio (0.55): Moderate. Sustainability certifications create symbolic compliance that substitutes for structural reform. Audits are infrequent and penalties are weak. However, the constraint retains functional components—supply standardization is real, not purely performative. Theater has increased over the interval as firms have invested more in certification infrastructure without corresponding improvements in labor conditions.
 *
 * PERSPECTIVAL GAP:
 *   The primary perspectival gap lies between beneficiaries (equipment manufacturers, developed-nation firms) and victims (artisanal miners, extraction communities). Beneficiaries perceive coordination that enables renewable transition and cost efficiency. Victims perceive extraction with suppressed alternatives. This gap is not a measurement disagreement—it is a structural asymmetry in how the same constraint affects different positions. A secondary gap exists between the developed-nation energy transition narrative (renewable energy as green transition) and the supply-chain reality (extraction and environmental cost externalization). The certification system bridges this gap symbolically without bridging it structurally.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (equipment manufacturers, developed-nation firms, mining operators) derive d values near 0.0-0.25 through arbitrage exit options and beneficiary status, producing low or negative effective extraction from their perspective. They see the constraint as coordination. Artisanal miners derive d values near 0.95 through trapped exit and victim status, producing high f(d) and maximum experienced extractiveness. The powerless miner cannot negotiate or exit; the institutional manufacturer can do both. Developing nation grid operators derive intermediate d around 0.55-0.65 through constrained exit and mixed victim/beneficiary status—they bear extraction costs through supply chain dependency while gaining access to renewable energy technology. The organized circular economy coalition derives d around 0.50-0.60 through constrained exit and beneficiary-toward-victim orientation; they have agency (can invest in recycling infrastructure) but are currently disadvantaged by cost structures. Directionality overrides are not needed; the derivation chain captures the structural relationships accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is genuinely tangled: it combines real coordination (standardized supply, manufacturing efficiency, renewable energy access) with asymmetric extraction (labor suppression, environmental externality, supply dependency). The constraint cannot be decomposed into pure coordination (Rope) because extraction and suppression are structural features, not policy choices. The mineral standardization does enable manufacturing efficiency—that is real. But the supply chain's architecture channels rents toward beneficiaries while suppressing victim agency—that is also real and structural. The six-perspective analysis resolves the ambiguity: beneficiaries classify as Rope (they experience coordination), victims classify as Snare (they experience pure extraction), and moderate actors classify as Tangled Rope (they experience both). The analytical observer at civilizational scale risks misclassifying the constraint as natural (Mountain: minerals are rare, extraction takes labor, cost allocation reflects scarcity) or as inevitable (Scaffold: green energy transition requires some extraction cost). The structural data contradicts these naturalizations: the extraction magnitude and distribution are contingent on supply chain architecture, governance enforcement, and technology choices, not immutable. The constraint is changeable through recycled supply development, labor standard enforcement, and cost-internalization policy. The piton classification of the certification system reveals the mandatrophy's resolution mechanism: institutions create the appearance of managed extraction to sustain the constraint by suppressing pressure for structural change.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    virgin_versus_recycled_cost_parity,
    'At what point do recycled rare earth extraction and substitution chemistry become cost-competitive with virgin mining extraction?',
    'Longitudinal tracking of recycled rare earth yields, material science advances in substitutes, and production cost curves for both pathways; market price data for virgin vs recycled inputs',
    'If parity achieved by 2032: scaffold sunset is real, circular economy provides genuine exit pathway. If cost advantage remains with virgin extraction: supply chain dependency persists, cycling extraction continues indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(virgin_versus_recycled_cost_parity, empirical, 'Cost competitiveness of recycled versus virgin rare earth extraction').

omega_variable(
    labor_law_enforcement_capacity,
    'Can resource extraction labor standards be enforced in diffuse artisanal mining contexts, or is suppression structurally irreducible given the enforcement topology?',
    'Comparative analysis of labor standards enforcement in artisanal vs industrial mining; correlation between enforcement investment and outcome improvements; assessment of whether technology (GPS, digital documentation) can meaningfully improve monitoring',
    'If enforceable: suppression is policy-choice dependent (medium confidence in reform). If structurally irreducible: artisanal mining remains trapped by enforcement topology, not just poverty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_law_enforcement_capacity, empirical, 'Enforceability of labor standards in artisanal mining contexts').

omega_variable(
    extraction_visibility_asymmetry,
    'Does the supply chain''s geographic and institutional distance between consumers and extraction sites constitute a structural information barrier that sustains the constraint, or is opacity contingent on market design?',
    'Controlled transparency experiments (blockchain tracking, supply-chain labeling, third-party audits with real consequences); measurement of consumer willingness-to-pay for verified non-extractive sourcing; tracking of market shift following transparency interventions',
    'If structural: transparency alone will not resolve extraction. If contingent: market design reforms (full-cost pricing, supply-chain accountability) could shift extraction to developed-nation contexts where it would become politically intolerable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_visibility_asymmetry, empirical, 'Whether supply chain opacity is structural or contingent on market design').

omega_variable(
    developed_nation_renewable_dependency,
    'What fraction of developed-nation renewable energy transition genuinely depends on rare earth extraction versus being contingent on specific engineering choices and cost optimization?',
    'Techno-economic analysis of alternative renewable configurations (permanent magnet vs induction generators, neodymium-free battery chemistries, alternative grid architectures); comparison of cost and performance across pathways',
    'If high dependency: developed nations cannot exit the constraint without abandoning renewable transition. If contingent: extraction can be reduced through engineering choices that cost less than currently invested in supply chain control.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(developed_nation_renewable_dependency, empirical, 'Technical dependency of renewable energy on rare earth minerals').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(renewable_energy_supply_chain_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(renew_supply_tr_t0, renewable_energy_supply_chain_extraction, theater_ratio, 0, 0.38).
narrative_ontology:measurement(renew_supply_tr_t5, renewable_energy_supply_chain_extraction, theater_ratio, 5, 0.48).
narrative_ontology:measurement(renew_supply_tr_t10, renewable_energy_supply_chain_extraction, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(renew_supply_be_t0, renewable_energy_supply_chain_extraction, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(renew_supply_be_t5, renewable_energy_supply_chain_extraction, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(renew_supply_be_t10, renewable_energy_supply_chain_extraction, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(renewable_energy_supply_chain_extraction, resource_allocation).
narrative_ontology:affects_constraint(renewable_energy_supply_chain_extraction, battery_lithium_extraction).
narrative_ontology:affects_constraint(renewable_energy_supply_chain_extraction, semiconductor_supply_chain).
narrative_ontology:affects_constraint(renewable_energy_supply_chain_extraction, cobalt_mining_labor_standards).

% DUAL FORMULATION NOTE:
% The renewable energy supply chain extraction represents a constraint family with three decomposed stories: (1) rare earth mineral extraction (rare earth supply chain, ε≈0.58), (2) lithium and cobalt battery supply (battery mining, ε≈0.65), and (3) semiconductor material extraction (silicon and rare earth electronics supply, ε≈0.52). Each story has distinct ε values reflecting different extraction mechanisms and victim populations. This story focuses on rare earth supply chain; linked stories address lithium/cobalt and semiconductor components. All three are downstream of the renewable energy deployment constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(renewable_energy_supply_chain_extraction, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
