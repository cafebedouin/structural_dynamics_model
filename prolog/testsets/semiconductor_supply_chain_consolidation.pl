% ============================================================================
% CONSTRAINT STORY: semiconductor_supply_chain_consolidation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_semiconductor_supply_chain_consolidation, []).

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
 *   constraint_id: semiconductor_supply_chain_consolidation
 *   human_readable: Semiconductor Supply Chain Consolidation
 *   domain: industrial/economic/geopolitical
 *
 * SUMMARY:
 *   The semiconductor supply chain has undergone radical consolidation over
 *   the past 20 years, concentrating advanced-node manufacturing capacity in
 *   a handful of firms (TSMC, Samsung, Intel) and geographic regions (Taiwan,
 *   South Korea, US). This consolidation is justified as necessary for
 *   coordinating scarce materials, manufacturing expertise, and capital
 *   requirements across global demand. However, the structure functions as a
 *   snare for most supply-chain participants: specialty semiconductor firms,
 *   developing-nation manufacturers, and fabless design houses face material
 *   barriers to exit, supplier lock-in through long-term contracts, and
 *   technology access restrictions. The constraint exhibits mixed
 *   classifications from different perspectives: pure extraction for trapped
 *   participants, genuine coordination for beneficiaries, temporary
 *   coordination failure for organized diversification initiatives, and pure
 *   extraction from global analytical scope. The increase in extractiveness
 *   over the 20-year interval reflects deepening consolidation and tightening
 *   supply (especially post-2020 pandemic, post-2022 geopolitical
 *   fragmentation). The theater ratio increase reflects growing performative
 *   framing of consolidation as 'inevitable' or 'necessary to maintain
 *   innovation' despite evidence that mature-node capacity could serve the
 *   majority of applications in decentralized networks.
 *
 * KEY AGENTS:
 *   - Specialty Semiconductor Firms: Primary victim (powerless/trapped) — small independent chipmakers locked into consolidated supply chains with no alternative procurement pathways
 *   - Developing Nation Manufacturers: Primary victim (powerless/trapped) — structurally locked as lower-tier producers, unable to access advanced nodes or capital for independent foundries
 *   - Fabless Design Houses: Secondary victim (moderate/constrained) — medium-sized firms experience both coordination benefits and asymmetric extraction through foundry fees and capacity allocation
 *   - Advanced Node Manufacturers: Primary beneficiary (institutional/arbitrage) — TSMC, Samsung, Intel capture significant economic rent and maintain dominance through technology and capital barriers
 *   - Integrated Device Makers: Beneficiary with identity lock (institutional/identity_locked) — large corporations like Intel, Samsung internally capture supply-chain value; resistant to diversification despite structural mobility
 *   - Diversification Initiative: Organized response (organized/constrained) — government and industry-led efforts building alternative capacity; see consolidation as temporary coordination failure with sunset pathway
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(semiconductor_supply_chain_consolidation, 0.62).
domain_priors:suppression_score(semiconductor_supply_chain_consolidation, 0.68).
domain_priors:theater_ratio(semiconductor_supply_chain_consolidation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(semiconductor_supply_chain_consolidation, extractiveness, 0.62).
narrative_ontology:constraint_metric(semiconductor_supply_chain_consolidation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(semiconductor_supply_chain_consolidation, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(semiconductor_supply_chain_consolidation, snare).
narrative_ontology:human_readable(semiconductor_supply_chain_consolidation, "Semiconductor Supply Chain Consolidation").
narrative_ontology:topic_domain(semiconductor_supply_chain_consolidation, "industrial/economic/geopolitical").

domain_priors:requires_active_enforcement(semiconductor_supply_chain_consolidation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(semiconductor_supply_chain_consolidation, advanced_node_manufacturers).
narrative_ontology:constraint_beneficiary(semiconductor_supply_chain_consolidation, integrated_device_makers).
narrative_ontology:constraint_victim(semiconductor_supply_chain_consolidation, specialty_semiconductor_firms).
narrative_ontology:constraint_victim(semiconductor_supply_chain_consolidation, fabless_design_houses).
narrative_ontology:constraint_victim(semiconductor_supply_chain_consolidation, developing_nation_manufacturers).
narrative_ontology:constraint_victim(semiconductor_supply_chain_consolidation, supply_chain_competition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SPECIALTY SEMICONDUCTOR FIRM (SNARE) — Small independent chipmakers cannot exit consolidated supply chains. Material barriers: high capital requirements for equipment, inability to source rare materials, dependence on consolidated foundries. No alternative procurement pathways. Bears full extraction cost through supplier lock-in and price discrimination. Zero degrees of freedom.
constraint_indexing:constraint_classification(semiconductor_supply_chain_consolidation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATION MANUFACTURER (SNARE) — Structurally locked into consolidated chain as lower-tier producer. Cannot access advanced node technology without consolidated vendor permission. Capital barriers to independent foundry. Geopolitical lock-in (export controls, technology transfer restrictions). Faces maximum extraction through pricing and technology withholding.
constraint_indexing:constraint_classification(semiconductor_supply_chain_consolidation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: FABLESS DESIGN HOUSE (TANGLED ROPE) — Medium-sized design firms experience genuine coordination benefit (access to advanced nodes, managed supply) alongside asymmetric extraction (high foundry fees, capacity allocation priority favoring large customers, technology access restrictions). Can theoretically exit through alternative foundries but face significant switching costs and reduced advanced-node access. Mixed experience: some agency, some extraction.
constraint_indexing:constraint_classification(semiconductor_supply_chain_consolidation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ADVANCED NODE MANUFACTURER (ROPE) — Benefits from supply chain consolidation. Experiences constraint as pure coordination: standardization enables predictable demand, long-term contracts, and capital planning certainty. Can arbitrage between suppliers and customers. Captures significant economic rent but also genuinely solves coordination problem of matching rare materials to few capable fabs. Net beneficiary.
constraint_indexing:constraint_classification(semiconductor_supply_chain_consolidation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: DIVERSIFICATION INITIATIVE (SCAFFOLD) — Organized effort by governments and industry to build alternative capacity (CHIPS Act, European Chips Act, Taiwan subsidies). Sees consolidation as temporary coordination failure with sunset. Building parallel foundry capacity in allied nations, investing in mature-node alternatives, developing supply diversification protocols. High suppression tolerance acceptable only if it declines as alternative capacity comes online. Estimated sunset: 10-15 years as geographically dispersed capacity becomes available.
constraint_indexing:constraint_classification(semiconductor_supply_chain_consolidation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: INTEGRATED DEVICE MAKER / IDENTITY-LOCKED (TANGLED ROPE) — Large corporations (Samsung, Intel, TSMC) benefit from consolidated supply position and simultaneously depend on it for corporate identity and market strategy. Structurally mobile (could theoretically build redundant supply chains or source from alternatives) but identity-fused with consolidated-supply dominance model. Internal pressure to maintain exclusivity and resist diversification. This perspective's identity lock reveals institutional capture: the company has internalized the constraint's framing as 'competitive necessity' rather than contingent institutional arrangement.
constraint_indexing:constraint_classification(semiconductor_supply_chain_consolidation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational/global scope, the consolidation constraint is a pure extraction mechanism with suppression through geopolitical control, capital barriers, technology gatekeeping, and lock-in. No coordination benefit emerges at global scale — the constraint optimizes for beneficiary profit, not system-wide semiconductor production efficiency. The supposed 'coordination' is asymmetric: benefits concentrate at the top; suppression distributes downward. The canonical analytical perspective sees this as snare with high χ.
constraint_indexing:constraint_classification(semiconductor_supply_chain_consolidation, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(semiconductor_supply_chain_consolidation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(semiconductor_supply_chain_consolidation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(semiconductor_supply_chain_consolidation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(semiconductor_supply_chain_consolidation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(semiconductor_supply_chain_consolidation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. The constraint has shifted from coordination problem (managing scarce materials to few capable producers circa 2000) to extraction mechanism (consolidation creates artificial scarcity to maintain pricing power and market control). The 20-year trajectory shows rising extractiveness: from 0.35 (genuine coordination) to 0.62 (pure extraction). This reflects the phase shift from 'coordination of materials and expertise' to 'consolidation-based rent extraction.' Suppression (0.68): High. Trapped participants face insurmountable barriers: capital requirements for foundry construction (>$10B), specialized equipment with multi-year procurement, proprietary manufacturing processes, export controls on advanced equipment, geopolitical restrictions on technology transfer, and lock-in through long-term exclusive contracts. No legitimate exit pathway exists for developing-nation manufacturers or specialty firms. Theater ratio (0.45): Moderate. The framing of consolidation as 'inevitable for innovation' or 'necessary for global competition' is performative but not complete cover. Real technical and capital barriers exist, but the barrier heights are artificially maintained through exclusive licensing, equipment distribution restrictions, and political alliance (CHIPS Act investment flows to allied nations, not globally distributed). The theater has increased over time as the genuinely coordination-like aspects have attenuated.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon (consolidated supply chain) appears as rope to beneficiaries, snare to victims, and scaffold to organized counter-movements. The gap reveals that the 'coordination' framing is a beneficiary narrative. Actual coordination benefits exist but are asymmetric: consolidated capacity did solve real technical problems in the 1990s-2000s (coordinating rare materials, manufacturing expertise, capital). But these coordination functions have been abstracted and replaced with rent extraction. The theater ratio increase reflects the shift: early consolidation was justified by genuine technical necessity; current consolidation is justified by 'competitive necessity' and 'geopolitical reality,' which are performative framings masking rent-seeking. The identity-lock perspective reveals institutional capture: manufacturers have internalized the consolidation model as their identity, making diversification psychologically difficult even when structurally mobile.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness (χ) is computed from structural position (d) mediated through the sigmoid. Trapped victims apply maximum coercive pressure (d ≈ 0.95, f(d) ≈ 1.42, σ(global)=1.2) → χ ≈ 0.62 × 1.42 × 1.2 ≈ 1.05 (capped). Constrained moderate actors apply moderate pressure (d ≈ 0.55, f(d) ≈ 0.75, σ(global)=1.2) → χ ≈ 0.62 × 0.75 × 1.2 ≈ 0.56. Beneficiary arbitrage actors apply negative pressure (d ≈ 0.15, f(d) ≈ -0.01, σ(global)=1.2) → χ ≈ 0.62 × (-0.01) × 1.2 ≈ -0.01 (effectively zero). The analytic observer applies observer pressure (d ≈ 0.72, f(d) ≈ 1.15, σ(global)=1.2) → χ ≈ 0.62 × 1.15 × 1.2 ≈ 0.85. Suppression (0.68) is unchanged across perspectives — it represents the raw barrier height that applies uniformly to potential exitors.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint avoids mandatrophy by demonstrating legitimate perspectival divergence grounded in structural position. The beneficiary (TSMC) genuinely experiences rope: consolidation solves their capital and expertise coordination problem. The victim (specialty firm) genuinely experiences snare: consolidation traps them with no exit. Neither classification is false — both are true from their respective structural positions. The mandatrophy would arise if a single perspective produced mixed classification (appearing both as snare and rope to the same observer). Instead, the six perspectives produce consistent classification within their respective (P,T,E,S) tuples, and the perspectival gap correctly identifies the constraint as extractive (the beneficiary's rope derives from extracting from the victim's snare). The identity-locked perspective reveals institutional capture without creating mandatrophy: the identity-locked actor (Intel, Samsung) classifies as tangled_rope because they genuinely benefit from and enforce the consolidation, even though they are partly captured by it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'Does advanced node consolidation genuinely solve a coordination problem or merely create extraction opportunity by concentrating scarce capacity?',
    'Counterfactual analysis: compare costs of decentralized mature-node production + modest advanced-node capacity vs current consolidated model. If decentralized model''s total social cost is lower, the ''coordination'' framing is cover story for extraction.',
    'If coordination genuine: Rope/Tangled Rope classification strengthened. If extraction cover: Snare classification confirmed. Directional impact on policy response (market vs regulation).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, conceptual, 'Whether consolidation solves coordination or enables extraction').

omega_variable(
    geopolitical_lock_in_mechanism,
    'To what extent is suppression structural (capital/technology barriers) vs geopolitical (export controls, sanctioning, technology transfer restrictions)?',
    'Decompose suppression into component mechanisms. Measure which actors face export-control-based lock-in vs capital-based barriers. Compare exit difficulty in geopolitically allied vs non-allied nations.',
    'If primarily capital barriers: market-based solutions (capital availability, technology transfer) could reduce suppression. If geopolitical: structural decoupling and multi-polar supply chains are required. Shapes whether problem is economic or strategic.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(geopolitical_lock_in_mechanism, empirical, 'Extent of geopolitical vs capital-based suppression mechanisms').

omega_variable(
    mature_node_sufficiency_threshold,
    'What fraction of global semiconductor demand can be met by mature-node capacity in a diversified supply chain?',
    'Computational demand analysis by application class (automotive, IoT, legacy systems, advanced computing). Map to technical requirements (nm node required). Quantify unmet demand if concentrated capacity shifted to distributed capacity.',
    'If >85% of demand is mature-node serviceable: diversification is economically sufficient and consolidation is pure rent-seeking. If <70%: genuine bottleneck exists for advanced computing. Determines whether diversification sunsets the constraint or merely shifts extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mature_node_sufficiency_threshold, empirical, 'Fraction of semiconductor demand serviceable by mature nodes').

omega_variable(
    identity_lock_permeability,
    'Can incumbent manufacturers genuinely transition toward supply chain diversification and redundancy, or is their corporate identity fused with consolidation dominance?',
    'Longitudinal analysis of capital allocation: does investment flow toward diversification or toward deepening consolidation moats? Interview executives on strategic rationale for supply decisions. Test whether diversification proposals encounter institutional resistance.',
    'If manufacturers can credibly commit to diversification: identity lock dissolves and policy incentives can align interests. If identity-locked: policy must work around institutional resistance through regulatory mandate rather than market incentives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_permeability, empirical, 'Whether incumbent manufacturers can overcome identity lock toward consolidation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(semiconductor_supply_chain_consolidation, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(semicon_tr_t0, semiconductor_supply_chain_consolidation, theater_ratio, 0, 0.2).
narrative_ontology:measurement(semicon_tr_t10, semiconductor_supply_chain_consolidation, theater_ratio, 10, 0.35).
narrative_ontology:measurement(semicon_tr_t20, semiconductor_supply_chain_consolidation, theater_ratio, 20, 0.45).

% Extraction over time
narrative_ontology:measurement(semicon_be_t0, semiconductor_supply_chain_consolidation, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(semicon_be_t10, semiconductor_supply_chain_consolidation, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(semicon_be_t20, semiconductor_supply_chain_consolidation, base_extractiveness, 20, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(semiconductor_supply_chain_consolidation, resource_allocation).
narrative_ontology:affects_constraint(semiconductor_supply_chain_consolidation, rare_earth_supply_dependency).
narrative_ontology:affects_constraint(semiconductor_supply_chain_consolidation, advanced_node_technology_concentration).
narrative_ontology:affects_constraint(semiconductor_supply_chain_consolidation, geopolitical_technology_decoupling).

% DUAL FORMULATION NOTE:
% Semiconductor supply chain consolidation is upstream of rare earth dependency and advanced node technology concentration. Each downstream constraint inherits suppression from the supply chain lock-in. The constellation represents a constraint family where material scarcity (rare earth) is amplified through consolidation (supply chain) and crystallized into geopolitical fragmentation (technology decoupling). Decomposing consolidation into its sub-mechanisms (materials, capital, expertise, geopolitics) reveals multiple ε values; the 0.62 value captures the aggregate constraint at the supply-chain integration level.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(semiconductor_supply_chain_consolidation, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
