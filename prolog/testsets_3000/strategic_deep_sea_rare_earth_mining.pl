% ============================================================================
% CONSTRAINT STORY: strategic_deep_sea_rare_earth_mining
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_strategic_deep_sea_rare_earth_mining, []).

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
 *   constraint_id: strategic_deep_sea_rare_earth_mining
 *   human_readable: Strategic Deep-Sea Mining for Rare Earth Minerals
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   Japan's deep-sea mining initiative for rare earth minerals in its
 *   exclusive economic zone represents a constraint that exhibits all six
 *   Deferential Realism types across different observational perspectives.
 *   The constraint arises from the tension between Japan's strategic need for
 *   supply-chain security in critical minerals (driven by geopolitical
 *   vulnerability to Chinese REE export controls) and the irreversible
 *   ecological costs of abyssal ecosystem disruption, compounded by
 *   incomplete international governance frameworks for regulating mining in
 *   the global commons. The constraint combines genuine coordination function
 *   (enabling battery supply security) with asymmetric extraction (costs
 *   imposed on fishing communities and marine ecosystems without compensation
 *   mechanisms) and significant institutional theater (environmental impact
 *   assessments and ISA governance that lack real-time verification
 *   capacity). Over the past decade, the extractiveness of the constraint has
 *   increased (from 0.42 to 0.58) as mining technology has become more
 *   commercially viable and Japan has shifted from exploratory to
 *   implementation-focused policy. Theater has also increased (from 0.35 to
 *   0.48) as regulatory processes have become more elaborate while
 *   maintaining fundamental monitoring gaps.
 *
 * KEY AGENTS:
 *   - Japanese State (organized/arbitrage) — Primary beneficiary. Pursues strategic mineral independence and supply-chain resilience. Has high exit options (can shift strategy to international partnerships or material substitution) but strong incentive to develop domestic source.
 *   - Regional Fishing Communities (powerless/trapped) — Primary victim. Depend on fish stocks that migrate through mining-affected waters. No exit from geographic dependence. No voice in mining authorization.
 *   - Abyssal Ecosystem (powerless/trapped) — Structural victim. Polymetallic nodule harvesting destroys irreplaceable benthic communities on civilizational timescales. Cannot organize or exit.
 *   - Electronics & Battery Manufacturers (powerful/arbitrage) — Beneficiary. Gain supply security and price stability from Japanese mining. Have alternative sourcing options if mining fails (Indonesia, PNG).
 *   - Regional Coastal States (powerful/constrained) — Mixed position. Constrained by inability to shift battery supply chains quickly; constrained by maritime law that respects Japanese EEZ sovereignty. Benefit from stable global REE prices; suffer from transboundary plume damage.
 *   - International Seabed Authority & Marine Governance (organized/constrained) — Institutional actor with divided mandate. Enforces environmental standards but lacks real-time monitoring capacity. Constrained by legal ambiguity between EEZ sovereignty and common heritage principles.
 *   - Environmental & Technology Innovation Coalition (organized/mobile) — Organized agents (NGOs, battery researchers) who see mining as temporary (Scaffold). Have exit options through alternative technologies and view mining as transitional solution.
 *   - Analytical Observer (analytical/analytical) — Risk of naturalizing contingent material composition as immutable physical law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(strategic_deep_sea_rare_earth_mining, 0.58).
domain_priors:suppression_score(strategic_deep_sea_rare_earth_mining, 0.62).
domain_priors:theater_ratio(strategic_deep_sea_rare_earth_mining, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(strategic_deep_sea_rare_earth_mining, extractiveness, 0.58).
narrative_ontology:constraint_metric(strategic_deep_sea_rare_earth_mining, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(strategic_deep_sea_rare_earth_mining, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(strategic_deep_sea_rare_earth_mining, tangled_rope).
narrative_ontology:human_readable(strategic_deep_sea_rare_earth_mining, "Strategic Deep-Sea Mining for Rare Earth Minerals").
narrative_ontology:topic_domain(strategic_deep_sea_rare_earth_mining, "geopolitical/economic").

domain_priors:requires_active_enforcement(strategic_deep_sea_rare_earth_mining).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(strategic_deep_sea_rare_earth_mining, japanese_state_economic_security).
narrative_ontology:constraint_beneficiary(strategic_deep_sea_rare_earth_mining, electronics_manufacturers).
narrative_ontology:constraint_victim(strategic_deep_sea_rare_earth_mining, abyssal_ecosystem_integrity).
narrative_ontology:constraint_victim(strategic_deep_sea_rare_earth_mining, regional_fishing_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: ABYSSAL ECOSYSTEM INTEGRITY (SNARE) — The deep-sea environment cannot organize, exit, or articulate harm. Polymetallic nodule harvesting destroys sediment structure, disrupts chemosynthetic communities, and generates plumes affecting migratory fish stocks over decadal timescales. Zero exit options, zero voice, total cost-bearing.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: REGIONAL FISHING COMMUNITIES (SNARE) — Coastal fishing dependent on open access to regional waters and fish stocks; cannot exit dependence on migratory species depleted by mining plumes. No voice in mining authorization. Bears biological cost without compensation mechanism. Trapped by geography and economic structure.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: REGIONAL COASTAL STATES (TANGLED ROPE) — South Korea, China, Philippines have constrained exit (cannot easily shift battery supply chains or rare earth sourcing). Benefit from global rare earth price stability if Japanese mining increases supply. But suffer if mining damages transboundary fish stocks and disrupts maritime commons governance. Asymmetric extraction: limited voice in Japanese EEZ policy despite bearing transboundary costs. Active enforcement required — maritime boundary disputes and environmental liability claims.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ELECTRONICS MANUFACTURERS & JAPANESE STRATEGIC INDUSTRY (ROPE) — Benefits from stable, geopolitically-aligned rare earth supply. Can exit Chinese supply dependence or shift to alternative sourcing (e.g., Indonesia, Papua New Guinea) if Japanese mining fails. Coordination function: mining enables supply chain resilience and reduces geopolitical vulnerability. Experiences constraint as solving legitimate collective action problem (supply chain security). Low suppression experienced — manufacturers have alternatives and negotiating power.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: INTERNATIONAL MARINE GOVERNANCE (TANGLED ROPE) — Organized actors (International Maritime Organization, International Seabed Authority, UNCLOS signatory states) face conflicting mandates: protecting open access to marine commons vs. enabling sovereign development in EEZs. Japan can enforce mining in its EEZ; ISA/IMO can only coordinate standards and monitor. Constrained by legal ambiguity (EEZ sovereignty vs. common heritage principle for abyssal plains beyond EEZ). Active enforcement required — Japan must comply with environmental impact standards; ISA must evaluate. Both coordination (standards-setting) and extraction (jurisdictional power asymmetry) present.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ENVIRONMENTAL & TECH INNOVATION COALITION (SCAFFOLD) — Environmental NGOs, renewable energy advocates, and advanced-battery researchers see deep-sea mining as a temporary coordination problem with a sunset clause. The real exit path is solid-state batteries and critical mineral recycling, reducing REE demand by 40-60% over 15-20 years. Coalition has exit options and sees enforcement declining as alternatives mature. Theater low because the technical pathways (recycling, battery chemistry) are measurable and tracked.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: REGULATORY THEATER (PITON) — Environmental impact assessments, ISA-sponsored studies, and ISO standard-setting are largely performative. Mining operators and sponsoring states (Japan, Korea, etc.) design studies to justify predetermined approvals. Regulators lack independent verification capacity (abyssal depths prevent real-time monitoring). Theater ratio high because assessment ritual persists despite acknowledged measurement impossibility. Classification as piton reflects degraded function maintained by institutional inertia, not actual protective capacity.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a materials-science perspective, rare earth concentration in battery chemistry and electronic devices is a fundamental constraint of modern technology. Permanent magnetic materials (neodymium, samarium), phosphor elements (terbium, europium), and catalytic metals (cerium, lanthanum) have no ready substitutes in many applications. REE extraction becomes an apparently immutable requirement for electrification. However, this naturalizes what is contingent: battery chemistry is a design choice; elemental composition is engineering, not physics.
constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(strategic_deep_sea_rare_earth_mining_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(strategic_deep_sea_rare_earth_mining, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(strategic_deep_sea_rare_earth_mining, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(strategic_deep_sea_rare_earth_mining, TR),
    TR >= 0.70.

:- end_tests(strategic_deep_sea_rare_earth_mining_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. Japan extracts strategic benefit (supply independence) and electronics manufacturers extract supply security benefits during mining window. But extraction is bounded by (a) alternative sourcing options (Indonesia, Papua New Guinea have nodule deposits), (b) technology substitution pathways reducing REE demand, and (c) investment requirements for Japanese refining capacity remain capital-intensive. Not maximum extraction because beneficiaries have real exit options and costs are not imposed without choice. Increased from 0.42 to 0.58 over interval as technology has moved from theoretical to implementation stage, making strategic benefit more concrete and extraction more enforceable. Suppression (0.62): Moderate-high. Significant barriers exist: international governance ambiguity (EEZ sovereignty vs. ISA authority over abyssal plains), environmental impact assessment processes, and NGO/media scrutiny. But suppression is not maximal because Japan operates within recognized EEZ rights and ISA has formal authority to set standards. Regional states and fishing communities lack formal voice (high suppression for them), but Japan itself faces real international constraints (moderate suppression from Japan's perspective). Theater (0.48): Moderate. Environmental impact assessments are required and detailed, but real-time monitoring capacity at abyssal depths is unproven. Regulatory theater exists but is not dominant — technical uncertainty (plume transport modeling, ecosystem recovery timelines) drives genuine epistemological barriers, not pure ritual. Theater has increased as regulatory processes have elaborated while fundamental measurement problems persist.
 *
 * PERSPECTIVAL GAP:
 *   The perspectives show the full range of experienced constraint types. From Japan's organized/institutional perspective, mining is primarily a coordination mechanism (Rope) — solving legitimate supply-chain security problems. From the ecosystem's powerless/trapped perspective, it is pure extraction (Snare) with no alternatives or compensation. From regional states' perspective, it is a hybrid (Tangled Rope) — constrained by inability to exit battery supply chains quickly but able to negotiate through maritime law and ISA. From the innovation coalition's perspective, it is temporary (Scaffold) — sunset via substitution technology over 15-20 years. From international governance's perspective, it is institutional theater (Piton) — rituals maintained despite acknowledged monitoring gaps. From the analytical civilizational view, it risks appearing as natural law (Mountain) — rare earths as immutable requirement for modern technology — but this naturalizes an engineering choice. The perspectival gap is maximized between Japan's beneficiary view (Rope/Tangled Rope) and the ecosystem's victim view (Snare), revealing that the same structural phenomenon is experienced as legitimate coordination by the beneficiary and irreversible extraction by the victim.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value is determined by agent structural position. Japan (organized/arbitrage) derives low d (high beneficiary status, high exit options) — experienced χ is low-to-negative, constraint appears as beneficial coordination (Rope). Fishing communities (powerless/trapped) derive high d (victim status, zero exit) — experienced χ is high, constraint appears as pure Snare. Regional states (powerful/constrained) derive moderate-high d (victim of transboundary costs but powerful negotiators) — experienced χ is moderate, constraint appears as mixed (Tangled Rope). Ecosystem (powerless/trapped) derives maximum d — experienced χ is maximum, constraint is pure Snare, but agent cannot articulate or aggregate response. Electronics manufacturers (powerful/arbitrage) derive low d (beneficiary, high exit options) — experienced χ is low, constraint appears as beneficial Rope. ISA/international governance (organized/constrained) derives moderate d (mixed mandate, constrained by legal ambiguity) — experienced χ is moderate, constraint appears as hybrid needing active enforcement (Tangled Rope). Innovation coalition (organized/mobile) derives low d (high exit options via technology alternatives, beneficiary of eventual sunset) — experienced χ is low, constraint appears as temporary coordination problem (Scaffold). The directionality derivation captures how the same constraint produces radically different experienced extractiveness across the observational spectrum.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by showing that Tangled Rope classification is correct from the structural data (beneficiaries + victims + active enforcement required) even though multiple perspectives produce different types. The mandatrophy risk is: 'If this looks like coordination (Rope) to Japan and like extraction (Snare) to fishing communities, which is true?' The answer is both-and: the constraint IS hybrid (has genuine coordination function AND asymmetric extraction). The mistake would be forcing a single type that erases the perspectival gap. The Tangled Rope classification preserves both the coordination function (supply chain security, legitimate strategic need) and the extraction function (environmental costs imposed without compensation, suppression of victim voice through EEZ sovereignty doctrine). The analytics-level Tangled Rope prevents false mountains (naturalizing REE extraction) and prevents false ropes (pretending mining has no extraction component). It also explains why the fishing communities and ecosystem see Snare — from their structurally-victim position, the hybrid constraint's extraction component dominates their experienced chi. Theater increase (0.35→0.48) indicates the regulatory function has gained performative component without substantive monitoring improvement — classic Piton signal — but base extractiveness increase (0.42→0.58) indicates the extraction mechanism has become more enforceable over time, not that it was theater throughout. This prevents false Piton classification while acknowledging real institutional theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ecosystem_recovery_timeline,
    'What is the actual recovery timeline for abyssal sediment communities disrupted by nodule harvesting? Is regeneration on decadal (20-50 year) or civilizational (1000+ year) timescales?',
    'Longitudinal monitoring of test mining sites (ISA-sponsored pilot zones); baseline genetic and community surveys before/after harvesting; comparison with known recovery from natural abyssal disturbances',
    'If decadal: extraction can be bounded and limited (Tangled Rope or Scaffold). If civilizational: environmental cost becomes effectively infinite relative to mining benefit window (shifts constraint toward pure Snare for ecosystem perspective).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ecosystem_recovery_timeline, empirical, 'Recovery timeline for abyssal ecosystem disruption').

omega_variable(
    transboundary_plume_transport,
    'Do mining plumes from Japanese EEZ nodule harvesting demonstrably degrade fish stocks and benthic communities in South Korean, Chinese, or Philippine EEZs at measurable economic cost?',
    'Ocean current modeling with particle tracers; larval fish recruitment surveys in downstream nations; correlation of mining seasons with stock recruitment anomalies; economic valuation of fishery loss',
    'If demonstrable: regional states have concrete claim to extraction damages (strengthens Tangled Rope for regional states, potentially triggers sanctions). If negligible: extraction claim weakens, mining becomes closer to pure Rope (supply chain coordination).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transboundary_plume_transport, empirical, 'Transboundary economic and ecological impact of mining plumes').

omega_variable(
    rare_earth_substitution_timeline,
    'What is the realistic commercialization timeline for solid-state batteries, permanent-magnet alternatives, and rare earth recycling technologies at scale sufficient to reduce new REE demand by 50%+?',
    'Technology maturity tracking (TRL assessments); capital expenditure forecasts for recycling infrastructure; patent prosecution trends; pilot plant deployment schedules',
    'If 10-15 years: Scaffold perspective is structural (sunset is real and near). If 30+ years: mining must sustain 2-3 decades of extraction (constraint approaches Snare or permanent Tangled Rope). If infinite (no viable substitution): constraint becomes quasi-Mountain (REE extraction as permanent necessity).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rare_earth_substitution_timeline, empirical, 'Technology timeline for REE demand reduction').

omega_variable(
    geopolitical_arbitrage_sustainability,
    'Can Japan sustain strategic independence in rare earths through deep-sea mining, or does China''s 40+ year head start in REE refining infrastructure make Japanese supply politically vulnerable regardless of mining source?',
    'Supply-chain analysis of REE processing capacity (Japan vs China vs alternative nations); geopolitical leverage scenarios (sanctions, export controls); infrastructure investment required for Japanese refining autonomy',
    'If sustainable independence achieved: mining has genuine strategic benefit beyond extraction (benefits real, Rope upgraded). If China remains chokepoint: mining''s strategic justification is largely theater (shift toward Piton for Japanese state benefit claim).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geopolitical_arbitrage_sustainability, conceptual, 'Sustainability of Japanese rare earth independence via mining').

omega_variable(
    monitoring_enforcement_capacity,
    'Can real-time environmental monitoring (sediment particle sampling, plume tracking, biological surveys) actually be conducted at commercial mining depths and scales, or is EIS-mandated monitoring inherently performative?',
    'Assessment of monitoring technology limits; comparison of planned vs. actual monitoring in other deep-sea resource extraction; review of ISA inspection audit trails for compliance verification',
    'If actual monitoring is feasible: regulatory theater declines, Piton classification weakens (governance becomes functional). If monitoring is theater: confirms Piton perspective and suggests international governance is degraded institutional ritual rather than effective constraint on extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(monitoring_enforcement_capacity, empirical, 'Feasibility of real-time environmental monitoring at mining depths').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(strategic_deep_sea_rare_earth_mining, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dsreem_tr_t0, strategic_deep_sea_rare_earth_mining, theater_ratio, 0, 0.35).
narrative_ontology:measurement(dsreem_tr_t5, strategic_deep_sea_rare_earth_mining, theater_ratio, 5, 0.42).
narrative_ontology:measurement(dsreem_tr_t10, strategic_deep_sea_rare_earth_mining, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(dsreem_be_t0, strategic_deep_sea_rare_earth_mining, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(dsreem_be_t5, strategic_deep_sea_rare_earth_mining, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(dsreem_be_t10, strategic_deep_sea_rare_earth_mining, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(strategic_deep_sea_rare_earth_mining, resource_allocation).
narrative_ontology:affects_constraint(strategic_deep_sea_rare_earth_mining, chinese_rare_earth_export_monopoly).
narrative_ontology:affects_constraint(strategic_deep_sea_rare_earth_mining, battery_supply_chain_security).
narrative_ontology:affects_constraint(strategic_deep_sea_rare_earth_mining, abyssal_commons_governance).

% DUAL FORMULATION NOTE:
% Deep-sea mining is downstream of China's REE export control capacity (structural vulnerability) and upstream of battery supply security constraints. The ecological impact (abyssal ecosystem disruption) forms a separate constraint family decomposable into biodiversity loss and transboundary fishery damage — each has distinct ε values and victim sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(strategic_deep_sea_rare_earth_mining, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
