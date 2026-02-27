% ============================================================================
% CONSTRAINT STORY: china_vactrain_standard
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_vactrain_standard, []).

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
 *   constraint_id: china_vactrain_standard
 *   human_readable: China's Ultra-High-Speed Vacuum-Tube Maglev Standard
 *   domain: technological/economic
 *
 * SUMMARY:
 *   China's development of an ultra-high-speed vacuum-tube maglev standard
 *   represents a hybrid coordination-extraction constraint that operates
 *   across technological, economic, and geopolitical domains. The standard
 *   solves a genuine engineering problem — vacuum-tube maglev requires highly
 *   coupled physical and control systems where interoperability between
 *   vendors is non-trivial. However, the standard-setting process also
 *   captures market share, controls technology licensing, and creates
 *   structural dependencies for adopting regions. This constraint exhibits
 *   multiple classification types depending on the observer's structural
 *   position: the Chinese state sees coordination (Rope); global competitors
 *   see extraction (Snare); regional investors see mixed
 *   coordination-extraction (Tangled Rope); international standards bodies
 *   see a temporary institutional failure with alternative pathways
 *   (Scaffold); legacy operators see performative compliance (Piton); and the
 *   civilizational analytical observer risks false naturalness by treating
 *   institutional extraction as immutable physical law (false Mountain). The
 *   theater_ratio (0.58) reflects that much of the standard-setting activity
 *   appears as genuine technical coordination (safety interlocks, power
 *   delivery specifications, vacuum management protocols) but functions
 *   partly as extraction mechanism (IP control, vendor lock-in, operational
 *   service dependencies). The extractiveness trajectory (0.28 → 0.52 over 10
 *   years) shows increasing lock-in as infrastructure deployment creates sunk
 *   costs for adopting regions.
 *
 * KEY AGENTS:
 *   - Chinese State and Domestic Manufacturers: Primary beneficiary (institutional/arbitrage) — captures standard-setting power, technology licensing revenue, and long-term operational control
 *   - Global Rail Competitors (Japan, Germany, Europe, North America): Primary victims (powerless/trapped) — face technology obsolescence risk if vactrain standard dominates, but dependency risk if they adopt Chinese-controlled technology
 *   - Regional Infrastructure Investors (Southeast Asia, Middle East, Africa): Secondary victims (moderate/constrained) — benefit from access to advanced technology but constrained by licensing requirements and operational dependencies
 *   - International Standards Bodies (ISO/IEC rail committees): Organized actors (organized/mobile) — attempt to maintain open governance framework; can shift to alternative standards-setting mechanisms
 *   - Legacy Railroad Operators: Institutional actors (institutional/constrained) — face compliance and compatibility mandates with unclear technical pathways; high performative content
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing institutional extraction as immutable physics of vacuum-tube engineering
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_vactrain_standard, 0.52).
domain_priors:suppression_score(china_vactrain_standard, 0.62).
domain_priors:theater_ratio(china_vactrain_standard, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_vactrain_standard, extractiveness, 0.52).
narrative_ontology:constraint_metric(china_vactrain_standard, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(china_vactrain_standard, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_vactrain_standard, tangled_rope).
narrative_ontology:human_readable(china_vactrain_standard, "China's Ultra-High-Speed Vacuum-Tube Maglev Standard").
narrative_ontology:topic_domain(china_vactrain_standard, "technological/economic").

domain_priors:requires_active_enforcement(china_vactrain_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_vactrain_standard, chinese_state_actors).
narrative_ontology:constraint_beneficiary(china_vactrain_standard, domestic_maglev_manufacturers).
narrative_ontology:constraint_beneficiary(china_vactrain_standard, technology_integration_firms).
narrative_ontology:constraint_victim(china_vactrain_standard, international_rail_competitors).
narrative_ontology:constraint_victim(china_vactrain_standard, global_technology_standardization).
narrative_ontology:constraint_victim(china_vactrain_standard, infrastructure_capital_markets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GLOBAL RAIL COMPETITORS (SNARE) — Trapped in a technology adoption dilemma. If they ignore the vactrain standard, they risk obsolescence. If they adopt it, they depend on Chinese-controlled infrastructure, intellectual property, and manufacturing capacity. No exit option: either lose market share or surrender technological autonomy. Maximum experienced extraction.
constraint_indexing:constraint_classification(china_vactrain_standard, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: REGIONAL INFRASTRUCTURE INVESTORS (TANGLED ROPE) — Benefits from access to cutting-edge technology and reduced infrastructure deployment costs. Constrained by the need to adopt Chinese standards, licensing requirements, and long-term operational dependencies. Mixed coordination-extraction: the standard solves a genuine engineering problem (vacuum tube efficiency, safety interoperability) but extraction occurs through IP licensing and operational service contracts.
constraint_indexing:constraint_classification(china_vactrain_standard, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 3: CHINESE STATE AND MANUFACTURERS (ROPE) — Primary beneficiary. Experiences the standard as a coordination mechanism that aligns domestic engineering efforts, reduces fragmentation, and establishes market-captured positions. Arbitrage option: can exit the standard framework via alternative (non-vacuum-tube) maglev designs if domestic priorities shift. Net beneficiary — extraction flows toward this institutional actor.
constraint_indexing:constraint_classification(china_vactrain_standard, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INTERNATIONAL STANDARDS BODIES (SCAFFOLD) — Attempt to maintain genuinely open technological governance through ISO/IEC rail standards committees. Organized actors with mobile exit options: can shift to alternative standardization frameworks (proprietary-neutral technical consortia, open-source rail protocols). See the vactrain as a temporary coordination failure with a sunset: as competing vacuum-tube implementations mature and as open-source rail simulation tools improve, the lock-in weakens. Theater is moderate (0.58) because formal international standards committees maintain legitimacy even as de facto standard-setting power migrates to technology leaders.
constraint_indexing:constraint_classification(china_vactrain_standard, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY RAILROAD OPERATORS (PITON) — High theater (0.65): the constraint on them appears as operational compatibility requirements and regulatory compliance mandates, but the actual verification of vactrain-legacy integration is highly speculative and performative. Operators perform 'interoperability planning' without clear technical pathways. The constraint persists through regulatory inertia: legacy infrastructure is grandfathered, but the standard's enforcement creates a symbolic constraint with limited functional content. Institutional power but constrained exit: must adopt positioning even if the technical integration remains unresolved.
constraint_indexing:constraint_classification(china_vactrain_standard, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / PHYSICAL LIMITS (MOUNTAIN) — From a universal/civilizational perspective, the vacuum-tube maglev exhibits immutable physical constraints: aerodynamic efficiency gains from vacuum are logarithmic beyond a certain vacuum level; maglev levitation requires specific electromagnetic properties independent of governance frameworks; energy consumption curves are set by physics. However, this perspective risks false naturalness: the extraction mechanisms (licensing, operational control, standard-setting power) are institutional, not physical. The engine will classify this as a false summit.
constraint_indexing:constraint_classification(china_vactrain_standard, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_vactrain_standard_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_vactrain_standard, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_vactrain_standard, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_vactrain_standard, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(china_vactrain_standard, TR),
    TR >= 0.70.

:- end_tests(china_vactrain_standard_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The standard creates genuine technological benefits (reduced air resistance, safety interoperability, operational efficiency) that benefit all adopters, but the asymmetry in control is substantial. Chinese state actors and domestic manufacturers capture IP licensing revenue, operational service contracts, and technology development leadership. The extraction is not total because genuine coordination benefits exist — competitors could theoretically develop compatible systems if they reverse-engineered the standard or negotiated licensing. The trajectory from 0.28 to 0.52 reflects increasing lock-in as infrastructure sunk costs accumulate. Suppression (0.62): Moderate-high. Technical complexity creates high barriers to independent vactrain development — vacuum tube engineering, maglev electromagnetics, and control systems require specialized expertise and capital. Publishing standards openly does not eliminate these barriers; it requires additional investment to implement. Career and funding barriers exist for competing initiatives (hyperloop ventures, alternative maglev research receive less institutional support). But suppression is not total — alternatives exist (conventional high-speed rail, non-vacuum maglev, hyperloop concepts). Theater ratio (0.58): Moderate. Much of the standard-setting activity is genuine technical coordination (safety interlocks, interoperability testing, emergency procedures), but significant components are performative: international 'consultation' processes where China's role is pre-dominant; legacy operator 'interoperability planning' with unclear implementation pathways; public commitments to 'open standards' alongside proprietary IP licensing structures.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a large perspectival gap between beneficiaries and victims. The Chinese institutional actor sees Rope — the standard coordinates domestic engineering efforts and enables market capture. Global competitors see Snare — they face technology obsolescence or dependency without meaningful exit. Regional investors see Tangled Rope — mixed coordination (genuine efficiency gains) and extraction (licensing costs, operational control). International standards bodies see Scaffold — the constraint is a temporary institutional failure with a sunset horizon as alternative standards mature and open-source engineering tools improve. Legacy operators see Piton — their compliance obligations are largely performative, with unclear technical implementation pathways. The analytical observer's Mountain classification is a false summit — the 'inherent physics' framing naturalizes what is actually a contingent institutional arrangement (standard-setting power concentration, IP licensing structures, capital barriers to competing initiatives). This perspectival gap is the signature of a Tangled Rope: genuine coordination benefits exist (vacuum-tube efficiency, safety interoperability), but asymmetric extraction runs alongside the coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from each agent's structural position relative to the constraint. Chinese institutional actors and manufacturers benefit from the standard — they gain arbitrage options (can exit via alternative maglev designs) and control the standard-setting process (derived d ≈ 0.05-0.15 → low f(d) → low or negative experienced extraction). Global competitors are trapped — they have no equivalent technological alternative and no voice in standard-setting (derived d ≈ 0.90-0.95 → high f(d) → high experienced extraction). Regional investors are constrained but mobile — they depend on Chinese licensing but can negotiate terms or invest in competing technologies (derived d ≈ 0.55-0.65 → moderate f(d) → moderate experienced extraction). International standards bodies have mobile exit options — they can create alternative standards frameworks or shift governance models (derived d ≈ 0.50-0.60 → moderate f(d) → moderate experienced extraction). Legacy operators are constrained but not trapped — they can choose not to adopt vactrain (derived d ≈ 0.45-0.55 → low-moderate f(d) → low-moderate experienced extraction, but theater amplifies the performative content).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint resolves mandatrophy by demonstrating that both coordination and extraction are structurally present. The standard provides genuine coordination benefits — vacuum-tube maglev requires tight technical coupling, and a unified standard reduces fragmentation and enables safety interoperability. This is not theater; it is real coordination work. Simultaneously, the standard enables extraction through IP licensing, operational service dependencies, and market-captured positions for Chinese manufacturers. The mandatrophy (is it coordination or extraction?) is resolved by identifying BOTH: the standard is a Tangled Rope because it combines genuine coordination (reducing engineering fragmentation, enabling interoperability) with asymmetric extraction (licensing revenue, operational control, technology dependency). The analytical observer's risk is false naturalness: treating the institutional extraction as 'inevitable physics of vactrain engineering.' The schema prevents this by requiring explicit beneficiary/victim declarations and by showing that alternative standard-setting models (open-source, decentralized, competing implementations) are structurally possible — they are not implemented because of institutional choices, not because of immutable physics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    technical_interoperability_feasibility,
    'Can a genuinely open vacuum-tube maglev standard be technically viable, or does the engineering complexity inherently require centralized control?',
    'Analysis of competing vactrain implementations (Japanese SCMaglev, German TMT, domestic Chinese systems); assessment of standardization surface areas (power delivery, track gauge, vacuum management, emergency procedures) that could be decoupled from Chinese IP',
    'If decoupling is feasible: opens space for alternative standard-setters and reduces lock-in (Rope from more perspectives). If centralized control is unavoidable: validates the extraction mechanism (Snare from more perspectives).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_interoperability_feasibility, empirical, 'Whether open vacuum-tube standardization is technically feasible').

omega_variable(
    switching_cost_magnitude,
    'What is the true economic switching cost for a region adopting vactrain vs remaining with conventional high-speed rail or developing alternative maglev?',
    'Cost-benefit analysis: infrastructure sunk costs, fleet conversion costs, operational training, spare parts supply chain, technology licensing duration. Comparison with alternative maglev systems (non-vacuum) and advanced rail (7G/8G conventional).',
    'If switching cost is moderate (20-30% cost premium): standard is Tangled Rope, extraction is real but bounded. If very high (50%+ premium): approaches Snare. If low: approaches Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(switching_cost_magnitude, empirical, 'Economic switching costs for alternative technologies').

omega_variable(
    patent_licensing_extraction_scale,
    'Does Chinese IP licensing on vactrain represent genuine technology rent-extraction or fair compensation for development costs?',
    'Comparison of licensing fee structures (per-km-of-track, operational-revenue-sharing, per-vehicle-sold) against development costs; benchmarking against licensing terms for comparable transport technologies (AVE, ICE, Shinkansen patents); analysis of profit margins and reinvestment patterns',
    'If fees exceed development cost recovery + 15% margin: extraction mechanism confirmed, victims justified in snare classification. If fees approximate fair compensation: standard moves toward Rope or Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(patent_licensing_extraction_scale, empirical, 'Whether IP licensing terms reflect fair compensation or extraction').

omega_variable(
    alternative_standard_emergence_timeline,
    'What is the probability and timeline for a competing non-Chinese vactrain standard achieving technical maturity and market adoption?',
    'Tracking of Japanese SCMaglev Chuo Shinkansen deployment (Tokyo-Osaka completion ~2027-2034); German TMT development status; US hyperloop initiatives; open-source rail standards development. Market adoption thresholds and compatibility requirements.',
    'If alternative standard emerges with 10-15 year lag: current Chinese standard is temporary (Scaffold dynamics apply). If emergence takes 25+ years or fails: lock-in becomes persistent (Snare dynamics dominate).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_standard_emergence_timeline, empirical, 'Timeline for emergence of competing vactrain standards').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_vactrain_standard, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vactrain_tr_t0, china_vactrain_standard, theater_ratio, 0, 0.42).
narrative_ontology:measurement(vactrain_tr_t5, china_vactrain_standard, theater_ratio, 5, 0.5).
narrative_ontology:measurement(vactrain_tr_t10, china_vactrain_standard, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(vactrain_be_t0, china_vactrain_standard, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(vactrain_be_t5, china_vactrain_standard, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(vactrain_be_t10, china_vactrain_standard, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_vactrain_standard, global_infrastructure).
narrative_ontology:affects_constraint(china_vactrain_standard, semiconductor_supply_geopolitics).
narrative_ontology:affects_constraint(china_vactrain_standard, rare_earth_technology_dependency).

% DUAL FORMULATION NOTE:
% The vactrain standard constraint family may decompose into two structurally distinct claims: (1) technical interoperability coordination (lower extractiveness, ~0.30), and (2) IP licensing and operational control extraction (higher extractiveness, ~0.60-0.70). If further analysis reveals these have sufficiently different epsilon values, separate constraint stories should be written and linked via network edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(china_vactrain_standard, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
