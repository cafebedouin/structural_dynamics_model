% ============================================================================
% CONSTRAINT STORY: apparel_supply_chain_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_apparel_supply_chain_control, []).

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
 *   constraint_id: apparel_supply_chain_control
 *   human_readable: Apparel Supply Chain Control and Extraction
 *   domain: economic/labor/global_trade
 *
 * SUMMARY:
 *   The apparel supply chain represents a paradigmatic global extraction
 *   system coordinated through brand-retailer control of manufacturing
 *   networks spanning hundreds of thousands of factories across 60+
 *   countries. The constraint exhibits the full structural signature of
 *   Tangled Rope: genuine coordination function (efficient global apparel
 *   supply at consumer price points), active enforcement (order placement,
 *   quality control, payment terms), asymmetric extraction (brands capture
 *   value through buyer power while workers and farmers bear production costs
 *   and environmental externalities), and beneficiaries (brands,
 *   consolidators, purchasing departments) distinct from victims (garment
 *   workers earning below subsistence wages, small manufacturers locked into
 *   dependent relationships, cotton farmers trapped in commodity debt cycles,
 *   environmental commons bearing pesticide and water extraction burden). The
 *   constraint's theater ratio (0.48) reflects authentic coordination
 *   infrastructure (logistics, quality control, demand forecasting) alongside
 *   performative compliance mechanisms (third-party audits that gate
 *   compliance without improving conditions). The extractiveness trajectory
 *   (0.38 → 0.58 over 20 years) reveals a system whose coordination function
 *   remains stable while extraction mechanisms layer upward: fast-fashion
 *   acceleration increases extraction pressure on manufacturers and workers;
 *   audit theater expands without improving conditions; price suppression
 *   deepens as buyer consolidation increases. The constraint family
 *   encompasses six distinct perspectives revealing how the same structural
 *   control mechanisms produce radically different experiences: for brands,
 *   pure coordination enabling retail scale; for workers, pure extraction of
 *   labor value; for manufacturers, mixed coordination-extraction creating
 *   dependency; for environmental commons, unmitigated extraction with zero
 *   exit options.
 *
 * KEY AGENTS:
 *   - Brand Retailers: Primary beneficiary (institutional/arbitrage) — control supply chain, extract value through buyer power over pricing and terms, maintain plentiful exit options across suppliers
 *   - Garment Workers: Primary victim (powerless/trapped) — economically dependent on single employer, geographically isolated, lack legal and economic exit options, suppressed wages and unsafe conditions
 *   - Small Manufacturers: Secondary beneficiary/victim (moderate/constrained) — benefit from order predictability but trapped by buyer dependence, capital lock-in, unilateral price control
 *   - Cotton Farmers: Victim (powerless/trapped) — debt-financed agricultural system creates annual commodity dependence, suppressed prices below production costs, environmental extraction requirements enforced
 *   - Environmental Commons: Victim (powerless/trapped) — water tables, soil health, biodiversity bear extraction burden with zero exit options and zero voice in supply chain governance
 *   - Labor Compliance Auditors: Institutional actor (institutional/mobile) — maintain third-party audit theater; profit from audit volume regardless of condition improvements; enable brand deniability
 *   - Analytical Observer: Global perspective (analytical/analytical) — sees authentic coordination enabling global apparel distribution alongside systematic extraction from powerless agents
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(apparel_supply_chain_control, 0.58).
domain_priors:suppression_score(apparel_supply_chain_control, 0.65).
domain_priors:theater_ratio(apparel_supply_chain_control, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(apparel_supply_chain_control, extractiveness, 0.58).
narrative_ontology:constraint_metric(apparel_supply_chain_control, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(apparel_supply_chain_control, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(apparel_supply_chain_control, tangled_rope).
narrative_ontology:human_readable(apparel_supply_chain_control, "Apparel Supply Chain Control and Extraction").
narrative_ontology:topic_domain(apparel_supply_chain_control, "economic/labor/global_trade").

domain_priors:requires_active_enforcement(apparel_supply_chain_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(apparel_supply_chain_control, brand_retailers).
narrative_ontology:constraint_beneficiary(apparel_supply_chain_control, logistics_consolidators).
narrative_ontology:constraint_beneficiary(apparel_supply_chain_control, purchasing_departments).
narrative_ontology:constraint_victim(apparel_supply_chain_control, garment_workers).
narrative_ontology:constraint_victim(apparel_supply_chain_control, small_manufacturers).
narrative_ontology:constraint_victim(apparel_supply_chain_control, cotton_farmers).
narrative_ontology:constraint_victim(apparel_supply_chain_control, environmental_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FACTORY WORKER (SNARE) — Trapped by economic dependency on single employer, geographic isolation, limited alternative employment. Suppression is structural: minimal wage alternatives, legal restrictions on exit (contract penalties, debt bondage mechanisms), social isolation in company housing. No meaningful exit option. Bears full extraction: wages suppressed below subsistence, forced overtime, unsafe conditions maintained to maximize extraction.
constraint_indexing:constraint_classification(apparel_supply_chain_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL MANUFACTURER (TANGLED ROPE) — Constrained by dependence on large buyer contracts (single buyer may represent 40-80% of revenue) and capital lock into machinery specific to buyer specifications. Experiences both genuine coordination: order flow provides predictable demand, supply chain integration reduces market uncertainty; AND extraction: buyer controls pricing unilaterally, imposes retroactive cost reductions, maintains 60-90 day payment terms while demanding 30-day production cycles. Exit is possible but high-cost — retooling machinery, finding alternative buyers in crowded market, losing income during transition.
constraint_indexing:constraint_classification(apparel_supply_chain_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: BRAND RETAILER (ROPE) — Experiences supply chain control as coordination mechanism: standardized quality, reliable delivery, cost predictability enable retail operations at scale. Benefits from network: access to manufacturing capacity, ability to shift production between suppliers rapidly, leverage on pricing. Exit options are plentiful: multiple suppliers compete for orders, can shift production across countries and regions with minimal friction. Experiences constraint as enabler, not extractor.
constraint_indexing:constraint_classification(apparel_supply_chain_control, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WORKER ORGANIZING MOVEMENT (TANGLED ROPE) — Organized agents (unions, worker collectives, NGO monitors) see supply chain control as asymmetric extraction requiring active enforcement via audits, legal pressure, reputational campaigns. Gains genuine coordination benefits: standardized labor practices, transparency mechanisms, collective bargaining leverage. But remains constrained by brand power imbalance and risk of retaliatory factory closure or production shift. Sees the constraint as manageable but persistent — sunset is contingent on sustained political mobilization, not automatic.
constraint_indexing:constraint_classification(apparel_supply_chain_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR COMPLIANCE AUDIT SYSTEM (PITON) — Formal third-party auditing (SA8000, BSCI, SMETA) maintains theatrical compliance: factories pass audits then revert to non-compliant practices between inspections. Audits create documentation and deniability rather than behavior change. The audit system persists through institutional inertia — brands demand audits, auditors profit from auditing volume, factories game audits — despite documented ineffectiveness at preventing labor exploitation. Theater ratio high (0.68): elaborate audit infrastructure with minimal functional impact on worker conditions.
constraint_indexing:constraint_classification(apparel_supply_chain_control, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: COTTON FARMERS AND ENVIRONMENTAL COMMONS (SNARE) — Trapped by debt-financed agricultural systems dependent on annual crop sales. Supply chain control forces commodity pricing that suppresses farmer income below production costs in most seasons. Pesticide and water extraction requirements (cotton uses 16% of world pesticides on 2.5% of arable land) are enforced through input supplier dependencies and lack of alternatives. Suppression is maximal: farmers cannot exit commodity agriculture without land title loss; cannot enforce environmental restoration; lack political voice. Environmental commons (water tables, soil health, biodiversity) trapped with zero exit options and maximum extraction.
constraint_indexing:constraint_classification(apparel_supply_chain_control, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, apparel supply chain control exhibits genuine coordination function (reliable global supply of affordable clothing) alongside systematic extraction (labor suppression, farmer debt, environmental degradation). Classified as Tangled Rope: moderate-high extraction (χ ≈ 0.58), multiple beneficiaries (brands, consolidators) extracting from multiple victims (workers, farmers, environment), active enforcement required (audit systems, logistics control), but genuine coordination benefits also present. Engine detects authentic perspectival gap between beneficiary (rope) and powerless (snare) positions.
constraint_indexing:constraint_classification(apparel_supply_chain_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(apparel_supply_chain_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(apparel_supply_chain_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(apparel_supply_chain_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(apparel_supply_chain_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(apparel_supply_chain_control, TR),
    TR >= 0.70.

:- end_tests(apparel_supply_chain_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The supply chain extracts value through multiple mechanisms: labor cost suppression (workers earn $0.50-3.00 per garment that retails for $30-150), temporal extraction (payment delays that force supplier financing), quality penalties (retroactive cost reductions for minor defects), and externality dumping (environmental costs borne by commons). Suppression is not total because brands cannot eliminate all worker exit capacity without creating production failures; workers remain minimally productive even at subsistence wages. The extracted value flows upward: brands capture 40-60% of retail price as margin; manufacturers capture 8-15%; workers capture 3-5%. Base extraction of 0.58 reflects that the system functions specifically to concentrate value upward. Suppression (0.65): Moderate-high. Factory workers face language barriers (foreign language work rules), geographic isolation (factory dormitories), legal barriers (contract penalties, visa dependencies for migrant workers), economic barriers (single-buyer dependence removes outside job options), and information barriers (supply chain opacity prevents workers from knowing buyer identity or contacting brands). Suppression is not absolute because some workers do escape, organize, or change employers; but escape requires cost paid in lost income and social penalty. Theater ratio (0.48): Moderate. Supply chain control operates through legitimate logistics and quality coordination (genuine theater: 0.32) plus audit compliance theater (escalating from 0.32 to 0.48 over 20 years as audit infrastructure expands without improving worker conditions). The theater ratio rise reflects growing emphasis on compliance documentation and deniability rather than functional improvement.
 *
 * PERSPECTIVAL GAP:
 *   The supply chain produces maximal perspectival divergence. Brand retailers see pure Rope — coordination enabling global inventory management, reliable supplier networks, cost predictability. Factory workers see pure Snare — economic trap with no exit, suppressed wages, enforced extraction. Manufacturers see Tangled Rope — genuine coordination benefit (order predictability) alongside extraction (unilateral pricing, payment delays). Cotton farmers see Snare at extended timeline — commodity debt cycle extends across generations, escape requires complete agricultural transformation. Audit systems see Piton from their own perspective — they experience their role as compliance infrastructure while institutional analysis reveals their function as performative deniability theater. The analytical observer sees Tangled Rope with authentic perspectival gaps: the coordination benefits are real (billions of people wear affordable clothing) and the extraction is real (workers trapped in poverty while brands capture surplus). The mandatrophy is not resolved by declaring a single 'true' type but by recognizing that each perspective captures a structurally real dimension of the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values for each perspective derive from structural position: Brand retailers have d ≈ 0.10 (arbitrage exit, beneficiary status, institutional power) — the sigmoid f(d) produces negative effective extraction χ ≈ -0.05, they benefit from the constraint. Factory workers have d ≈ 0.95 (trapped exit, victim status, powerless) — f(d) ≈ 1.42 produces high effective extraction χ ≈ 0.82 scaled by global scope (σ=1.2), they bear maximum extraction. Manufacturers have d ≈ 0.55 (constrained exit, both victim and beneficiary) — f(d) ≈ 0.75 produces moderate extraction χ ≈ 0.44, they experience mixed dynamics. Environmental commons have d ≈ 0.98 (analytically trapped, pure victim) — f(d) ≈ 1.40 produces high extraction χ ≈ 0.81 scaled by global scope. The canonical fallback for institutional beneficiaries is d ≈ 0.00 (institutional power atom) yielding negative χ; for powerless victims is d ≈ 1.00 yielding maximum χ. No overrides needed — the structural data directly produces the observed perspectival gaps.
 *
 * MANDATROPHY ANALYSIS:
 *   The apparel supply chain resolves mandatrophy through genuine perspectival plurality: all six types are authentic readings of the constraint from different structural positions. Brands experience Rope (coordination), workers experience Snare (extraction), manufacturers experience Tangled Rope (hybrid), auditors experience Piton (theater), environmental commons experience Snare (unmitigated extraction), and the analytical observer sees Tangled Rope (coordination + extraction at global scale). No single classification is 'the truth' — the constraint IS a Tangled Rope whose effects distributively produce Snare and Rope outcomes depending on actor position. The falsifiability test: if organizing efforts succeeded in shifting worker power from powerless to organized, the worker perspective would reclassify from Snare to Tangled Rope (constrained exit with coalition leverage), confirming that the Snare status is contingent on powerlessness rather than inherent to the constraint. The audit theater (Piton classification) is falsifiable: if audits achieved genuine condition change (correlating audit passage with independent verification of improved wages/conditions/safety), the audit perspective would reclassify toward Rope. Neither falsification has occurred in 20+ years despite massive audit expansion — the theater_ratio increase (0.32 → 0.48) documents escalating audit infrastructure without condition improvement, confirming Piton diagnosis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    audit_gaming_vs_behavior_change,
    'Do labor compliance audits represent genuine enforcement of improved conditions or primarily provide deniability theater?',
    'Longitudinal comparison: factory conditions pre- vs post-audit; correlation between audit pass rates and independent monitoring data (worker interviews, NGO spot checks); analysis of audit frequency vs documented labor violations in same facility',
    'If genuine enforcement: audit system is functional Rope-level coordination. If primarily theater: audit system is Piton (degraded). Classification consequence: if Piton confirmed, entire compliance infrastructure reclassifies as false theater, raising snare classification confidence for worker perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(audit_gaming_vs_behavior_change, empirical, 'Whether audits improve conditions or provide deniability theater').

omega_variable(
    agricultural_debt_cycle_inevitability,
    'Is the debt-financed cotton farming system a structural necessity of commodity agriculture or a contingent institutional arrangement?',
    'Analysis of alternative agricultural financing models (cooperative systems, direct consumer contracts, public procurement); historical comparison with periods of farmer-controlled commodity pricing; identification of policy mechanisms that would enable non-debt agricultural models',
    'If structural necessity: cotton farmer trap classifies as Mountain (immutable). If contingent: classifies as Snare (extractive but potentially escapable through policy/institutional change).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agricultural_debt_cycle_inevitability, conceptual, 'Whether agricultural debt is structural or contingent').

omega_variable(
    supply_chain_opacity_and_worker_exit,
    'To what degree do supply chain secrecy and information barriers contribute to worker entrapment vs supplier economic dependency?',
    'Comparison of worker exit rates and conditions in transparent vs opaque supply chains; analysis of information access (do workers know buyer identities, can they contact brands directly); correlation between transparency initiatives and labor standard improvements',
    'If information barriers are primary mechanism: reclassify as Snare with emphasis on cognitive/information suppression. If economic dependency is primary: classification remains Snare but with emphasis on material suppression. Affects intervention strategies (transparency vs income support).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(supply_chain_opacity_and_worker_exit, empirical, 'Information barriers vs economic dependency in worker entrapment').

omega_variable(
    fast_fashion_coordination_necessity,
    'Does the fast-fashion model''s speed-to-market requirement justify the extraction mechanisms, or are extraction and rapid inventory turnover orthogonal?',
    'Comparison of extraction levels (wages, conditions, environmental impact) between fast-fashion suppliers and slower-cycle apparel producers; analysis of price differential and demand elasticity; identification of technical requirements that genuinely demand low-cost/low-oversight production vs those driven by margin maximization',
    'If genuine necessity: extraction becomes Tangled Rope feature (coordination cost is extraction cost). If orthogonal: extraction is pure Snare layered onto otherwise functional coordination. Affects mandatrophy analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fast_fashion_coordination_necessity, empirical, 'Whether fast-fashion coordination justifies extraction').

omega_variable(
    worker_coalition_power_threshold,
    'At what scale do worker organizing efforts achieve coalition power sufficient to reclassify from powerless to organized?',
    'Analysis of wage improvements and condition changes following unionization drives; comparison of unionized vs non-unionized factory outcomes; identification of coalition size that shifts bargaining power asymmetry',
    'If threshold is low (achievable through local organizing): powerless perspective could be upgraded to organized/constrained, shifting classification from Snare toward Tangled Rope with sunset. If threshold is high (requires industry-wide coordination): powerless perspective remains trapped, Snare classification remains.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(worker_coalition_power_threshold, empirical, 'Coalition power threshold for worker escape from powerless status').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(apparel_supply_chain_control, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(apparel_tr_t0, apparel_supply_chain_control, theater_ratio, 0, 0.32).
narrative_ontology:measurement(apparel_tr_t10, apparel_supply_chain_control, theater_ratio, 10, 0.4).
narrative_ontology:measurement(apparel_tr_t20, apparel_supply_chain_control, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(apparel_be_t0, apparel_supply_chain_control, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(apparel_be_t10, apparel_supply_chain_control, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(apparel_be_t20, apparel_supply_chain_control, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(apparel_supply_chain_control, resource_allocation).
narrative_ontology:boltzmann_floor_override(apparel_supply_chain_control, 0.18).
narrative_ontology:affects_constraint(apparel_supply_chain_control, fast_fashion_inventory_velocity).
narrative_ontology:affects_constraint(apparel_supply_chain_control, labor_cost_suppression_mechanisms).
narrative_ontology:affects_constraint(apparel_supply_chain_control, agricultural_commodity_pricing).
narrative_ontology:affects_constraint(apparel_supply_chain_control, environmental_water_extraction).

% DUAL FORMULATION NOTE:
% Apparel supply chain control is a parent constraint that coordinates multiple subordinate extraction mechanisms: fast-fashion acceleration (drives extraction velocity), labor cost suppression (worker-level mechanism), agricultural commodity pricing (farmer-level mechanism), and environmental externalities (commons-level mechanism). Each subordinate constraint has its own ε value reflecting its specific structural mechanisms. The parent constraint's ε=0.58 represents the aggregate extraction across all subordinate mechanisms scaled to global scope. Network links establish dependency: supply chain control enforces fast-fashion velocity constraints, which enforce labor cost suppression, which (through cotton component) enforces agricultural pricing, which (through pesticide/water requirements) enforces environmental extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(apparel_supply_chain_control, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
