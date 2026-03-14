% ============================================================================
% CONSTRAINT STORY: toy_industry_market_concentration
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_toy_industry_market_concentration, []).

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
 *   constraint_id: toy_industry_market_concentration
 *   human_readable: Market Concentration in the Global Toy Industry
 *   domain: consumer_goods/industrial_organization
 *
 * SUMMARY:
 *   The toy industry has undergone sustained market concentration over two
 *   decades, driven by scale economies in manufacturing, retail consolidation
 *   (Walmart, Amazon, Target), capital intensity of global supply chains, and
 *   IP enforcement mechanisms. Three major manufacturers (Mattel, Hasbro,
 *   LEGO) control approximately 60% of the global market. This constraint
 *   exhibits tangled coordination (consolidated distribution enables reliable
 *   supply and reduces transaction costs) layered with asymmetric extraction
 *   (independent makers and small retailers are progressively excluded from
 *   profitable channels). The suppression mechanism operates through multiple
 *   pathways: minimum order quantities that indie makers cannot satisfy,
 *   category management algorithms that favor established brands, IP
 *   litigation threats, and manufacturing capital requirements that force
 *   outsourcing to consolidated factories. The theater ratio (0.45) reflects
 *   genuine coordination function in modern retail — category management and
 *   supply-demand matching are real — but rising performative content as
 *   direct-to-consumer channels increasingly bypass retail. The constraint is
 *   stratified: independent toy makers experience it as a snare (trapped by
 *   distribution gatekeeping), small retailers as tangled rope (constrained
 *   but receiving coordination benefits), major manufacturers as pure rope
 *   (coordination with no extraction), and the broader innovation ecosystem
 *   as suppression of diversity with some coordination benefits.
 *
 * KEY AGENTS:
 *   - Major Toy Manufacturers (Mattel, Hasbro, LEGO): Institutional beneficiaries (arbitrage) — capture extraction through scale, retail shelf allocation, brand power, capital leverage
 *   - Independent Toy Makers: Powerless victims (trapped) — cannot access capital, cannot satisfy minimum orders, face IP litigation, excluded from retail channels
 *   - Small Regional Retailers: Moderate victims (constrained) — dependent on consolidated distributors, face margin compression from category management, benefit from supply stability
 *   - Retail Consolidation Platforms (Amazon, Walmart, Target): Institutional beneficiaries (arbitrage) — control retail gatekeeping, set terms for suppliers and consumers
 *   - Product Innovation Ecosystem: Collective victim (mobile in principle, constrained in practice) — access to capital is concentrated, designs are copied by majors, retail gatekeeping prevents direct market test
 *   - Direct-to-Consumer Alternatives (Kickstarter, Etsy): Organized alternative pathway — creating sunset mechanism for traditional retail gatekeeping
 *   - Toy Purchasing Ritual: Institutional decay artifact (piton) — department store toy aisles persist through inertia despite functional obsolescence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(toy_industry_market_concentration, 0.58).
domain_priors:suppression_score(toy_industry_market_concentration, 0.62).
domain_priors:theater_ratio(toy_industry_market_concentration, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(toy_industry_market_concentration, extractiveness, 0.58).
narrative_ontology:constraint_metric(toy_industry_market_concentration, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(toy_industry_market_concentration, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(toy_industry_market_concentration, tangled_rope).
narrative_ontology:human_readable(toy_industry_market_concentration, "Market Concentration in the Global Toy Industry").
narrative_ontology:topic_domain(toy_industry_market_concentration, "consumer_goods/industrial_organization").

domain_priors:requires_active_enforcement(toy_industry_market_concentration).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(toy_industry_market_concentration, major_toy_manufacturers).
narrative_ontology:constraint_beneficiary(toy_industry_market_concentration, retail_consolidation_platforms).
narrative_ontology:constraint_beneficiary(toy_industry_market_concentration, financial_investors).
narrative_ontology:constraint_victim(toy_industry_market_concentration, independent_toy_makers).
narrative_ontology:constraint_victim(toy_industry_market_concentration, small_retailers).
narrative_ontology:constraint_victim(toy_industry_market_concentration, product_innovation_diversity).
narrative_ontology:constraint_victim(toy_industry_market_concentration, consumer_choice_quality).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDEPENDENT TOY MAKER (SNARE) — Trapped by manufacturing cost scale, retail access gatekeeping, and IP litigation threats from majors. No viable exit: production requires capital investment that consolidated supply chains make prohibitively expensive; retail distribution requires relationships with Amazon, Walmart, Target. Maximum experienced extraction through supply chain margin compression and shelf-space exclusion.
constraint_indexing:constraint_classification(toy_industry_market_concentration, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL REGIONAL RETAILER (TANGLED ROPE) — Constrained by inventory cost and shelf-space economics. Benefits from consolidated supply (reliable inventory, predictable pricing) but suffers margin extraction through minimum order quantities and category management demands. High suppression (cannot source profitably outside major distributor networks) but some residual coordination benefit (stable supply reduces operational variance).
constraint_indexing:constraint_classification(toy_industry_market_concentration, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MAJOR TOY MANUFACTURER (ROPE) — Experiences market concentration as pure coordination: consolidated retail consolidates purchasing decisions, reducing sales complexity and distribution cost. Can arbitrage between manufacturers (offshore production), retailers (shelf allocation), and licensing partners. Net beneficiary through extraction of margin from supply chain.
constraint_indexing:constraint_classification(toy_industry_market_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: RETAIL CONSOLIDATION PLATFORM (ROPE) — Experiences concentration as coordination function: consolidated toy-maker roster enables category management and algorithmic matching. Can arbitrage between suppliers (toy makers) and consumers. Low extraction from their perspective — they control the platform and set terms.
constraint_indexing:constraint_classification(toy_industry_market_concentration, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSUMER PRODUCT INNOVATION ECOSYSTEM (TANGLED ROPE) — Structured as abstract collective (inventor communities, design schools, independent creator networks). Mobile in principle (can create outside major channels) but experiences genuine suppression through retail gatekeeping and capital access concentration. Receives some coordination benefit (shared tooling standards, manufacturing knowledge) but faces asymmetric extraction through IP appropriation and design copying by majors.
constraint_indexing:constraint_classification(toy_industry_market_concentration, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: DIRECT-TO-CONSUMER ALTERNATIVE COALITION (SCAFFOLD) — Organized agents (Kickstarter, Etsy, indie game platforms) are creating alternative distribution pathways that bypass retail concentration. See market concentration as a temporary institutional formation, not as inevitable. High confidence in sunset: crowdfunding and social commerce are maturing as alternative channels. Suppression is declining as access barriers (production capital, shipping logistics) are addressed by alternative infrastructure.
constraint_indexing:constraint_classification(toy_industry_market_concentration, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: TOY PURCHASING RITUAL (PITON) — The department store toy aisle and the major retailer's toy section persist through institutional inertia despite declining functional necessity. Online shopping reduces information asymmetry and shelf-space scarcity. The purchasing ritual (browsing aisles, seasonal marketing campaigns) is largely performative — real purchase decisions migrate to Amazon's algorithm. Theater ratio rises as the ritual persists despite reduced function.
constraint_indexing:constraint_classification(toy_industry_market_concentration, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, toy market concentration appears inherent to manufacturing economics: scale economies in tooling, capital intensity of supply chains, and logistics optimization all favor consolidation. This view naturalizes what is actually a contingent institutional arrangement (vertical integration, IP enforcement, retail consolidation). The engine's false summit detector will flag this as naturalization of institutional structure, not economic law.
constraint_indexing:constraint_classification(toy_industry_market_concentration, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(toy_industry_market_concentration_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(toy_industry_market_concentration, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(toy_industry_market_concentration, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(toy_industry_market_concentration, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(toy_industry_market_concentration, TR),
    TR >= 0.70.

:- end_tests(toy_industry_market_concentration_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderately high. Market concentration enables genuine economies (efficient distribution, reduced transaction costs) but layered with asymmetric extraction targeting independent makers and small retailers. The value reflects both real coordination benefits and real extraction — neither pure. The trajectory (0.35 → 0.58 over 20 years) shows accumulation: initial scale economies were real coordination gains; over time, extraction mechanisms (IP litigation, algorithmic gatekeeping, capital concentration) accumulated. Suppression (0.62): High. Multiple pathways: minimum order quantities, algorithmic gatekeeping, IP enforcement, capital access concentration. Barriers to exit for indie makers and small retailers are substantial but not absolute — direct-to-consumer pathways offer partial alternatives. Theater ratio (0.45): Moderate. Retail category management has genuine coordination function (demand forecasting, inventory optimization, consumer discovery) but increasing performative content as online shopping reduces information asymmetry. Rising theater over the interval reflects increasing performativity of the retail ritual as real purchasing decisions migrate to algorithms.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates perspectival stratification within the same economic mechanism. Major manufacturers see pure coordination (Rope) — consolidated retail reduces their sales and logistics burden. Retail platforms see pure coordination (Rope) — consolidated supplier rosters enable algorithmic matching and margin capture. Independent makers see pure extraction (Snare) — gatekeeping is absolute within their experience. Small retailers see mixed coordination and extraction (Tangled Rope) — they benefit from stable supply but suffer margin compression. The innovation ecosystem sees suppression of diversity with some coordination benefit (Tangled Rope) — genuine benefits from shared manufacturing standards, but asymmetric extraction through design appropriation. Direct-to-consumer platforms see a temporary institutional formation with a sunset (Scaffold) — Amazon and Etsy are maturing as distribution alternatives that bypass retail gatekeeping. The purchasing ritual appears degraded (Piton) — department store toy aisles persist through inertia despite low functional necessity. The civilizational analytical observer risks naturalizing this as inherent to manufacturing economics (Mountain) — but scale economies are contingent on capital-intensive infrastructure choices.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values range from 0.95 (independent makers trapped, maximum extraction) through 0.55-0.65 (small retailers constrained, significant extraction) to 0.15 (majors as beneficiaries with arbitrage, minimal/negative extraction). The pipeline derives d from beneficiary/victim declarations and exit options: independent makers are victims with trapped exit → high d → high f(d) → high experienced chi. Major manufacturers are beneficiaries with arbitrage exit → low d → low/negative f(d) → low experienced chi. Small retailers are victims with constrained exit → medium-high d → medium f(d) → medium-high experienced chi. The scope modifier σ(S) is global (1.2), amplifying chi for concentrated actors whose sphere is worldwide. The tangled_rope classification holds across multiple perspectives because the constraint genuinely coordinates (distribution logistics, inventory optimization) while genuinely extracting (margin compression, market access denial).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that tangled rope correctly identifies the hybrid structure: genuine coordination function (supply reliability, demand matching, transaction cost reduction) coexists with asymmetric extraction (independent maker exclusion, small retailer margin compression, innovation suppression). The threat of false natural law comes from the mountain perspective — manufacturing economics appear to necessitate scale consolidation. The reality is contingent: scale economies are real but dependent on capital-intensive infrastructure choices (injection molding, global supply chains, retail shelf scarcity). Alternative infrastructures (3D printing, on-demand manufacturing, direct-to-consumer platforms) enable lower-capital, less-concentrated production. The constraint is not a mountain of physics but a tangled_rope of institutional choices. The scaffold perspective confirms this: direct-to-consumer alternatives are maturing as genuine alternatives, indicating that the retail gatekeeping extraction is sunset-able, not inherent.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scale_economy_necessity,
    'Are the scale economies driving toy market concentration inherent to manufacturing physics or contingent on capital-intensive infrastructure choices?',
    'Comparative analysis of low-capital production methods (3D printing, on-demand manufacturing, modular design); cost structure differences between traditional injection molding vs distributed manufacturing',
    'If inherent: scale consolidation is economically necessary (mountain-like). If contingent: consolidation is a choice with alternatives — classification shifts toward tangled_rope or scaffold across more perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scale_economy_necessity, empirical, 'Whether scale economies are inherent or contingent on production methods').

omega_variable(
    retail_gatekeeping_necessity,
    'Does physical retail shelf-space scarcity necessitate consolidated gatekeeping, or is this a transitional institutional artifact?',
    'Historical analysis of retail footprint trends; correlation between shelf consolidation and market share inequality; measurement of direct-to-consumer sales growth rate vs retail-dependent sales',
    'If shelf-space is inherently scarce: gatekeeping is natural (mountain-like). If gatekeeping is institutional choice: suppression is contingent, and DTC alternatives are genuine (scaffold sunset is real).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(retail_gatekeeping_necessity, empirical, 'Whether retail gatekeeping is inherent or institutional').

omega_variable(
    innovation_suppression_causality,
    'Does market concentration suppress product innovation diversity, or do major manufacturers capture most innovative designs regardless of retail structure?',
    'Comparison of product design diversity metrics (form factor, material innovation, risk-taking frequency) in concentrated vs non-concentrated toy segments; analysis of successful indie designs and their adoption paths',
    'If concentration suppresses innovation: victim classification for ecosystem is correct. If majors innovate efficiently at scale: suppression is structural (access barriers) not functional (innovation prevention).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(innovation_suppression_causality, empirical, 'Whether market concentration suppresses innovation').

omega_variable(
    collective_consumer_benefit,
    'Do consumers benefit from concentration-enabled economies (lower prices, reliable availability) or suffer from reduced choice and quality diversity?',
    'Price trend analysis controlling for product quality; consumer satisfaction surveys comparing consolidated vs non-consolidated segments; market basket analysis for choice variety',
    'If consumers benefit: concentration is partially coordination (rope-like). If consumers suffer: extraction is more severe, and collective victimization is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_consumer_benefit, empirical, 'Net consumer welfare under market concentration').

omega_variable(
    drm_enforcement_scope,
    'To what degree is suppression of independent makers enabled by IP enforcement (patents, trademarks, design protection) vs by capital and distribution barriers?',
    'Analysis of litigation patterns targeting indie makers; comparison of suppression levels in high-IP-enforcement regions (US, EU) vs low-enforcement regions; indie maker survival rates post-IP challenge',
    'If IP enforcement is primary: suppression is institutional policy (changeable). If capital barriers dominate: suppression is structural (requires infrastructure change).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(drm_enforcement_scope, empirical, 'Role of IP enforcement in suppression of independent makers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(toy_industry_market_concentration, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(toy_conc_tr_t0, toy_industry_market_concentration, theater_ratio, 0, 0.28).
narrative_ontology:measurement(toy_conc_tr_t10, toy_industry_market_concentration, theater_ratio, 10, 0.38).
narrative_ontology:measurement(toy_conc_tr_t20, toy_industry_market_concentration, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(toy_conc_be_t0, toy_industry_market_concentration, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(toy_conc_be_t10, toy_industry_market_concentration, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(toy_conc_be_t20, toy_industry_market_concentration, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(toy_industry_market_concentration, resource_allocation).
narrative_ontology:affects_constraint(toy_industry_market_concentration, retail_shelf_scarcity).
narrative_ontology:affects_constraint(toy_industry_market_concentration, manufacturing_capital_concentration).
narrative_ontology:affects_constraint(toy_industry_market_concentration, intellectual_property_enforcement_toy_design).

% DUAL FORMULATION NOTE:
% Market concentration is downstream of manufacturing capital requirements and retail consolidation. Separate constraint stories address: (1) manufacturing_capital_concentration — capital-intensity of tooling/production (ε≈0.20, Mountain-like scale law); (2) retail_shelf_scarcity — physical shelf-space gatekeeping (ε≈0.45, Tangled Rope); (3) intellectual_property_enforcement_toy_design — IP litigation against indie makers (ε≈0.65, Snare). This story integrates these three, showing how market concentration emerges from their interaction. The network captures causal dependency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(toy_industry_market_concentration, powerful, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
