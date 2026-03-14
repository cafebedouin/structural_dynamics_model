% ============================================================================
% CONSTRAINT STORY: small_farm_market_access
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_small_farm_market_access, []).

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
 *   constraint_id: small_farm_market_access
 *   human_readable: Market Access Constraints for Small-Scale Farmers
 *   domain: agricultural/economic
 *
 * SUMMARY:
 *   Market access constraints for small-scale farmers create a structural
 *   tension between the genuine need to aggregate distributed supply into
 *   retail-scale volumes and the asymmetric extraction of value that
 *   consolidation enables. Small farms face multiple interlocking barriers:
 *   certification costs that are fixed regardless of farm size
 *   (disproportionately heavy for small operators), minimum order quantities
 *   that exclude producers below a threshold volume, quality grading
 *   standards designed for industrial-scale homogeneity, and supply-chain
 *   infrastructure controlled by consolidators who set prices unilaterally.
 *   The constraint exhibits mixed coordination and extraction
 *   characteristics. Genuine coordination functions exist: consolidators do
 *   solve the problem of moving food from millions of small producers to
 *   billions of consumers. But this coordination function is entangled with
 *   asymmetric extraction: the beneficiary (consolidator) controls the
 *   essential infrastructure and can extract surplus through information
 *   asymmetry (farmers do not know alternative prices), switching costs
 *   (exiting consolidation networks is costly), and structural dependence (no
 *   viable alternative sales channels for most small farms). Theater content
 *   has increased as regulatory complexity has grown: food safety
 *   certifications are layered, sometimes redundant, and function partly as
 *   compliance theater rather than functional verification.
 *   Direct-to-consumer and cooperative alternatives are emerging but remain
 *   niche, unable to absorb the production of most small farms. The
 *   constraint is temporally dynamic: extractiveness has risen as
 *   consolidation has intensified, theater has increased as regulatory
 *   frameworks have accumulated.
 *
 * KEY AGENTS:
 *   - Small Farm Operators: Primary victims (powerless/trapped) — face certification costs, minimum orders, and infrastructure barriers with no viable exit
 *   - Consolidator-Distributor Networks: Primary beneficiaries (institutional/arbitrage) — capture coordination margin and control essential infrastructure; can source from alternative regions if a supplier exits
 *   - Farming Communities: Secondary victim (moderate/constrained) — face capital barriers to equipment and certification but benefit from cooperative aggregation mechanisms
 *   - Direct-to-Consumer and Cooperative Initiatives: Organized agents (organized/mobile) — building alternative sales pathways with generational timeline for full substitution
 *   - Agricultural Policy Apparatus: Institutional actor (institutional/constrained) — maintains layered regulatory framework partly through institutional inertia; sees own processes as degraded
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy-contingent consolidation as inevitable efficiency requirement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(small_farm_market_access, 0.58).
domain_priors:suppression_score(small_farm_market_access, 0.62).
domain_priors:theater_ratio(small_farm_market_access, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(small_farm_market_access, extractiveness, 0.58).
narrative_ontology:constraint_metric(small_farm_market_access, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(small_farm_market_access, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(small_farm_market_access, tangled_rope).
narrative_ontology:human_readable(small_farm_market_access, "Market Access Constraints for Small-Scale Farmers").
narrative_ontology:topic_domain(small_farm_market_access, "agricultural/economic").

domain_priors:requires_active_enforcement(small_farm_market_access).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(small_farm_market_access, large_distributors).
narrative_ontology:constraint_beneficiary(small_farm_market_access, retail_consolidation).
narrative_ontology:constraint_victim(small_farm_market_access, small_farm_operators).
narrative_ontology:constraint_victim(small_farm_market_access, rural_economic_viability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL FARM OPERATOR (SNARE) — Trapped by infrastructure requirements, certification costs, and minimum order quantities that function as de facto barriers to direct market access. Cannot exit to alternative sales channels without catastrophic income loss. Experiences extraction as total: must sell through consolidators at predetermined prices or face crop spoilage.
constraint_indexing:constraint_classification(small_farm_market_access, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FARMING COMMUNITY (TANGLED ROPE) — Constrained by capital requirements and technical barriers but benefits from cooperative marketing structures, shared certification, and aggregation platforms. Some coordination function exists (bulk certification, shared cold storage) alongside asymmetric extraction (consolidators capture margin). Generational horizon captures intergenerational farm succession barriers.
constraint_indexing:constraint_classification(small_farm_market_access, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSOLIDATOR-DISTRIBUTOR NETWORK (ROPE) — Benefits from coordination of supply (aggregating small producers into retail-ready volumes) and coordination of information (managing quality standards, food safety compliance). Experiences the constraint as enabling: without it, small-farm supplies would be fragmented. Net beneficiary with arbitrage options (can source from alternative regions).
constraint_indexing:constraint_classification(small_farm_market_access, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DIRECT-TO-CONSUMER & COOPERATIVE INITIATIVES (SCAFFOLD) — Organized agents (farmers markets, CSA networks, producer cooperatives) see market fragmentation as a temporary problem with a sunset: digital platforms (local food aggregators, blockchain traceability, direct-order infrastructure) are building alternative sales pathways. Extraction is low because these initiatives have agency and visible exit paths. Generational horizon reflects that a full generation of farmers can exit via these platforms.
constraint_indexing:constraint_classification(small_farm_market_access, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: AGRICULTURAL POLICY APPARATUS (PITON) — Food safety regulations, grading standards, and commodity pricing frameworks persist through institutional inertia despite being partially obsolete. Regulatory theater (repeated audits, redundant certifications, compliance theater for small farms) is high while functional verification of actual safety is moderate. The policy system sees its own processes as degraded — maintained because alternatives haven't fully replaced them, not because they optimize outcomes.
constraint_indexing:constraint_classification(small_farm_market_access, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, some market consolidation is inherent to scale efficiency: moving food from millions of small producers to billions of consumers requires aggregation, standardization, and logistics. This perspective sees market consolidation as an immutable feature of industrial agriculture. However, structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that 'efficiency requires consolidation' naturalizes what is actually a policy-contingent institutional arrangement.
constraint_indexing:constraint_classification(small_farm_market_access, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(small_farm_market_access_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(small_farm_market_access, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(small_farm_market_access, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(small_farm_market_access, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(small_farm_market_access, TR),
    TR >= 0.70.

:- end_tests(small_farm_market_access_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising over interval. The constraint extracts meaningful value from small farms through consolidator margin capture, but extraction is not total because some coordination value is genuine. The 20-year trend shows extractiveness rising from 0.35 to 0.58 as consolidation has intensified and as direct-to-consumer alternatives have failed to scale fast enough to provide structural pressure. Suppression (0.62): Moderate-high. Significant barriers include fixed-cost certifications (food safety compliance, organic certification, quality grading), capital requirements for equipment that meets consolidator specifications, and information asymmetry (farmers cannot easily compare consolidator prices across regions). Exit is not impossible (some farms do establish direct sales) but carries substantial cost — lost consolidator volumes, lost crop insurance through consolidator networks, social costs of leaving farming community relationships. Theater ratio (0.48): Moderate. Certification regimes have accumulated layers of redundancy (USDA grading, state-level certifications, third-party food safety audits, retailer-specific requirements). The functional proportion is genuine: baseline food safety verification is necessary. But the layer of repeated audits, documentation, and compliance theater is non-functional for small farms specifically — consolidators can afford centralized compliance, small farms cannot. Theater has risen from 0.28 to 0.48 over the interval as regulatory complexity has increased without corresponding functional increase in food safety outcomes.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates strong perspectival disagreement. The small farm operator sees a snare: no exit, no benefit, pure extraction. The consolidator sees a rope: they are solving a genuine coordination problem (aggregating supply). The farming community sees tangled rope: mixed coordination benefit (aggregation platforms that enable cooperative marketing) and extraction (consolidator margin capture). The scaffold perspective (direct-to-consumer initiatives) sees a temporary problem with visible sunset: farmers markets, CSA networks, and digital platforms are building alternative pathways. The piton perspective sees a degraded regulatory apparatus maintained through inertia. The analytical observer risks seeing an inevitable law of efficiency (mountain) but structural data reveals this as naturalization: consolidation-favoring policies are contingent choices, not physical laws. The perspectival gap is wide precisely because the constraint genuinely contains both coordination and extraction — it is not a false snare or a hidden rope. The tangled rope classification resolves the gap by acknowledging that the constraint simultaneously coordinates supply and extracts from small producers.
 *
 * DIRECTIONALITY LOGIC:
 *   Small farm operators (powerless/trapped) derive high d ≈ 0.92 from their structural position: they are primary victims, have no exit options, and face maximum suppression. The sigmoid maps this to f(d) ≈ 1.35 — experienced extractiveness is amplified. Consolidator-distributors (institutional/arbitrage) derive low d ≈ 0.08 from beneficiary status and arbitrage exit options — they can source from alternative regions if a supplier exits. The sigmoid maps to f(d) ≈ -0.11 — experienced extractiveness is reversed (they experience net benefit). Farming communities (moderate/constrained) derive mid-range d ≈ 0.65 from being partial victims with constrained (not trapped) exit. The sigmoid maps to f(d) ≈ 1.00 — experienced extraction is neutral before scope scaling. Scope modifier σ(regional=0.9) then scales chi: χ_farming = 0.58 × 1.00 × 0.9 ≈ 0.52. The regional scope (not national) reflects that market consolidation operates at regional aggregation points, not purely at national commodity levels.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The mandatrophy is resolved by recognizing that small-farm market access contains both genuine coordination (aggregation solves a real supply problem) and genuine asymmetric extraction (consolidators capture surplus through infrastructure control and information asymmetry). The extracted value is not merely the cost of coordination — consolidators earn economic rent above their functional contribution. The snare perspective (small farm's lived experience) is structurally valid: they are trapped. The rope perspective (consolidator's lived experience) is also structurally valid: they are solving a problem that would otherwise make small-farm sales impossible. The tangled rope classification prevents mislabeling this as either pure extraction (erasing genuine coordination value) or pure coordination (erasing genuine asymmetric extraction). The scaffold sunset clause is real: direct-to-consumer channels are growing, blockchain traceability is reducing information asymmetry, and aggregation platforms are lowering minimum order quantities. But the sunset is multi-generational (20-30 years), not immediate — most small farms remain trapped in consolidation during the transition. The piton perspective (regulatory theater) is real: food safety certification has accumulated redundant layers. Reducing theater (simplifying certification for small farms) would reduce suppression from 0.62 to ~0.40-0.45, shifting classification from snare toward constrained tangled rope for the powerless perspective.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    certification_barrier_necessity,
    'Are current food safety certification requirements genuinely necessary for small-farm market access, or have they become redundant theater?',
    'Comparative analysis of food safety outcomes across certification levels; correlation between certification cost and actual risk reduction; international comparison of certification regimes',
    'If genuinely necessary: suppression is justified coordination cost, reducing effective extraction. If redundant: certification is a barrier mechanism, increasing extractiveness from 0.58 to 0.72+.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(certification_barrier_necessity, empirical, 'Whether food safety certifications are functionally necessary or theater').

omega_variable(
    consolidation_efficiency_gains,
    'What proportion of supply-chain cost differences between direct sales and consolidated distribution represents genuine efficiency gains versus extractive consolidator margins?',
    'Price data decomposition: storage costs, logistics, quality loss, administrative overhead, consolidator profit margin. Comparison with full-cost models for direct-to-consumer distribution.',
    'If >70% efficiency: market consolidation is substantive coordination (Rope from consolidator perspective justified). If <50% efficiency: consolidation is primarily extractive (Snare from farm perspective confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(consolidation_efficiency_gains, empirical, 'Efficiency gains versus consolidator extraction in distribution').

omega_variable(
    direct_channel_scalability,
    'Can direct-to-consumer and cooperative channels scale to capture meaningful market share (>20% farm revenue) within a generation, or are they structurally limited to niche markets?',
    'Growth trajectory analysis of CSA networks, farmers markets, producer cooperatives, digital aggregation platforms. Infrastructure requirements for 20% market penetration.',
    'If scalable: scaffold classification and sunset logic are structural. If niche-limited: direct channels remain alternative to consolidation, not replacement — snare persists for majority of small farms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(direct_channel_scalability, empirical, 'Scalability of direct-to-consumer and cooperative channels').

omega_variable(
    policy_intent_consolidation,
    'Do agricultural policies (certifications, grading standards, commodity subsidies) intentionally favor consolidation, or is consolidation-favorability an unintended side effect?',
    'Policy design analysis: explicit vs implicit incentive structures; comparative case studies of consolidation-neutral policy regimes; stakeholder testimony on policy intent',
    'If intentional: extractive architecture is engineered (high mandatrophy risk). If unintended: extractive outcome emerges from layered coordination purposes (lower mandatrophy risk).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_intent_consolidation, conceptual, 'Whether policy consolidation-favoring is intentional').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(small_farm_market_access, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sfma_tr_t0, small_farm_market_access, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sfma_tr_t10, small_farm_market_access, theater_ratio, 10, 0.38).
narrative_ontology:measurement(sfma_tr_t20, small_farm_market_access, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(sfma_be_t0, small_farm_market_access, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sfma_be_t10, small_farm_market_access, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(sfma_be_t20, small_farm_market_access, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(small_farm_market_access, resource_allocation).
narrative_ontology:affects_constraint(small_farm_market_access, rural_economic_decline).
narrative_ontology:affects_constraint(small_farm_market_access, agricultural_commodity_pricing).
narrative_ontology:affects_constraint(small_farm_market_access, food_system_supply_chain_resilience).

% DUAL FORMULATION NOTE:
% Small-farm market access decomposes into two structurally distinct constraints: (1) certification and compliance barriers (ε ≈ 0.25, primarily regulatory theater with real but surmountable coordination costs — scaffold/piton), and (2) consolidator margin extraction enabled by infrastructure control and information asymmetry (ε ≈ 0.65, genuine asymmetric extraction — snare/tangled rope). This story weights the extraction component (0.58 average) because small farms experience the combined effect. Decomposition would require separate analysis of certification rationalization (policy lever: simplify for scale-adjusted compliance) versus consolidator margin reduction (policy lever: reduce switching costs through cooperative infrastructure investment). See network linkages for downstream constraint impacts on rural viability.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(small_farm_market_access, moderate, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
