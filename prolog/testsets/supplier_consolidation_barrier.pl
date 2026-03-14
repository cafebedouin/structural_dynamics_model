% ============================================================================
% CONSTRAINT STORY: supplier_consolidation_barrier
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_supplier_consolidation_barrier, []).

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
 *   constraint_id: supplier_consolidation_barrier
 *   human_readable: Supplier Consolidation Barrier
 *   domain: economic_policy/industrial_organization
 *
 * SUMMARY:
 *   Supplier consolidation barriers create structural lock-in where dependent
 *   manufacturers face prohibitive switching costs while dominant suppliers
 *   extract rents through pricing power, extended payment terms, and forced
 *   adoption of proprietary ecosystems. The constraint exhibits multiple
 *   classification types across perspectives: snare for trapped
 *   manufacturers, rope for beneficiary suppliers, tangled rope for large
 *   OEMs with dual power, scaffold for regulatory coalitions building
 *   alternative ecosystems, and piton for degraded procurement processes
 *   maintained through institutional inertia. The extractiveness has
 *   increased from 0.35 to 0.58 over the measurement interval (ten years),
 *   reflecting accumulating switching costs and deepening supplier
 *   integration. Theater ratio remains moderate (0.48) because actual
 *   procurement decisions are generally outcome-driven rather than purely
 *   ceremonial, though significant performativity exists in vendor evaluation
 *   and RFQ processes that rarely result in supplier switches.
 *
 * KEY AGENTS:
 *   - Dominant Suppliers: Primary beneficiary (institutional/arbitrage) — capture price premiums, volume guarantees, and ecosystem lock-in; able to set terms and shift costs to dependent customers
 *   - Dependent Manufacturers: Primary victims (powerless/trapped) — face prohibitive switching costs, locked into technical ecosystems, bear price extraction and payment term asymmetries
 *   - Supply Chain Managers: Secondary victims (moderate/constrained) — understand lock-in but face career risk and organizational inertia when attempting supplier switches
 *   - Large OEMs: Secondary actors (powerful/mobile) — powerful enough to negotiate favorable terms and develop alternatives, but also extract from smaller suppliers in their own supply chains
 *   - Regulatory Coalition: Organized intervention agents (organized/constrained) — competition authorities and industrial policy advocates building alternative supplier ecosystems through antitrust enforcement and diversification mandates
 *   - Legacy Procurement System: Institutional actor (institutional/arbitrage) — maintains supplier relationships through contractual inertia and corporate culture defaults; performs vendor evaluation theater without structural change
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing consolidation as inherent to manufacturing complexity rather than recognizing it as a policy-contingent outcome
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(supplier_consolidation_barrier, 0.58).
domain_priors:suppression_score(supplier_consolidation_barrier, 0.65).
domain_priors:theater_ratio(supplier_consolidation_barrier, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(supplier_consolidation_barrier, extractiveness, 0.58).
narrative_ontology:constraint_metric(supplier_consolidation_barrier, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(supplier_consolidation_barrier, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(supplier_consolidation_barrier, snare).
narrative_ontology:human_readable(supplier_consolidation_barrier, "Supplier Consolidation Barrier").
narrative_ontology:topic_domain(supplier_consolidation_barrier, "economic_policy/industrial_organization").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(supplier_consolidation_barrier, dominant_suppliers).
narrative_ontology:constraint_victim(supplier_consolidation_barrier, dependent_manufacturers).
narrative_ontology:constraint_victim(supplier_consolidation_barrier, downstream_consumers).
narrative_ontology:constraint_victim(supplier_consolidation_barrier, market_entrants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEPENDENT MANUFACTURER (SNARE) — Faces complete supplier lock-in. Switching costs (tooling, qualification, redesign) are prohibitive. No alternative suppliers exist at competitive price and quality. Bears full extraction through price premiums, extended payment terms, and forced adoption of supplier's ecosystem. Exit is structurally impossible without multi-year retooling investment.
constraint_indexing:constraint_classification(supplier_consolidation_barrier, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SUPPLY CHAIN MANAGER (SNARE) — Understands the lock-in but is constrained by organizational inertia and sunk investments in the dominant supplier relationship. Career incentives are misaligned: switching suppliers is high-risk (production delays, quality variance) with benefits accruing to future quarters. Personal knowledge of the dominant supplier's systems creates switching friction. Can theoretically exit but at severe career and operational cost.
constraint_indexing:constraint_classification(supplier_consolidation_barrier, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOMINANT SUPPLIER (ROPE) — Experiences the consolidation barrier as coordination: their ecosystem integrates proprietary standards, supply-chain intelligence, and just-in-time logistics. Net beneficiary from extraction — price premiums, volume guarantees, and switching costs flow toward them. Can arbitrage between customers by leveraging their critical position.
constraint_indexing:constraint_classification(supplier_consolidation_barrier, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LARGE OEM (TANGLED ROPE) — Powerful enough to negotiate favorable terms and develop alternative suppliers over multi-year cycles, but structurally tied to consolidation logic for scale economies. Benefits from standardized component ecosystems while bearing the extraction cost of supplier lock-in for smaller suppliers in their own supply chain. Mixed experience: they extract from their suppliers while being extracted from by their component suppliers.
constraint_indexing:constraint_classification(supplier_consolidation_barrier, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: REGULATORY COALITION (SCAFFOLD) — Competition authorities, industrial policy advocates, and supply-chain resilience initiatives see consolidation barriers as a temporary coordination failure with a sunset: antitrust enforcement, industrial diversification incentives, and nearshoring policies are building alternative supplier ecosystems. Characterized as temporary because the regulatory logic has explicit time-bound goals (reduce supply-chain vulnerability by 2035, increase supplier diversity by mandate). Suppression is high but declining as policy mechanisms activate.
constraint_indexing:constraint_classification(supplier_consolidation_barrier, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: LEGACY PROCUREMENT SYSTEM (PITON) — Traditional supplier relationships persist through institutional inertia despite degraded function. Contract language, ordering systems, and corporate culture default to incumbents. The procurement theater — RFQ processes, vendor scoring sheets, periodic reviews — maintains legitimacy while actual switching rarely occurs. Theater ratio (0.48) reflects substantial but not overwhelming performativity; many procurement rituals are maintained despite acknowledged lock-in.
constraint_indexing:constraint_classification(supplier_consolidation_barrier, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, supplier consolidation appears as an immutable law of industrial organization: scale economies in specialized manufacturing naturally select for concentration. Barriers to switching are inherent to complex supply chains. However, the structural data contradicts mountain classification — the extractiveness (0.58) and suppression (0.65) are contingent on institutional arrangements (contract structure, information asymmetries, regulatory environment), not natural laws. This perspective demonstrates false naturalization of policy choice.
constraint_indexing:constraint_classification(supplier_consolidation_barrier, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(supplier_consolidation_barrier_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(supplier_consolidation_barrier, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(supplier_consolidation_barrier, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(supplier_consolidation_barrier, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(supplier_consolidation_barrier, TR),
    TR >= 0.70.

:- end_tests(supplier_consolidation_barrier_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through multiple mechanisms: price premiums (dominant supplier holds 15-30% pricing power above competitive baseline), extended payment terms (60-90 day cycles vs. 30 days for alternative suppliers), and forced ecosystem adoption (integration into supply-chain IT, proprietary standards, quality management systems). The extractiveness increased from 0.35 to 0.58 over the interval as supply chains became more complex and specialized, deepening technical lock-in. This is not extractiveness at snare maximum (0.75+) because some coordination function exists: standardized ecosystems do reduce transaction costs and enable scale economies for both parties. Suppression (0.65): Moderate-high. Barriers include: technical switching costs (multi-year tooling and qualification cycles), information asymmetries (dominant supplier controls performance data, obscuring alternative quality), organizational inertia (procurement systems default to incumbents), and risk perception (customers rationally fear production disruption during switches). However, suppression is not total because large manufacturers can and do successfully switch suppliers over extended timeframes, indicating mobility exists at cost. Theater ratio (0.48): Moderate. Procurement processes include genuine vendor evaluation and negotiation, but the theater is substantial: RFQ processes rarely lead to actual supplier switches (estimated 5-10% of initial RFQs result in qualification and contracts with new suppliers). The theater reflects organizational bias toward incumbents and risk aversion rather than performative compliance rituals.
 *
 * PERSPECTIVAL GAP:
 *   The powerless manufacturer sees a snare (zero exit options; bears full extraction). The dominant supplier sees a rope (coordination mechanism enabling their business; low cost to maintain). The large OEM sees tangled rope (enough power to negotiate favorable terms, but structurally tied to consolidation logic for their own suppliers). The regulatory coalition sees a temporary scaffold with a sunset (antitrust enforcement and industrial diversification policies are building alternatives). The procurement system sees itself as degraded (piton — the vendor evaluation theater persists despite acknowledged lock-in). The civilizational observer risks seeing mountain (immutable consequence of manufacturing scale). The perspectival gap reveals that the constraint's classification depends entirely on the agent's structural power and exit options — from powerless/trapped it is snare; from institutional/arbitrage it is rope. The gap widens if information asymmetries are intentional (snare) vs. unintentional side effects (tangled rope).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from beneficiary/victim status and exit options. Dominant suppliers have low d (0.10-0.20): beneficiaries with arbitrage exit → negative or minimal f(d). Dependent manufacturers have high d (0.90-0.95): victims with trapped exit → high f(d) → high experienced extractiveness. Supply chain managers have moderate d (0.55-0.65): victims with constrained exit → moderate f(d). Large OEMs have lower-middle d (0.40-0.50): mixed status (extract from suppliers, extracted from by component suppliers) with mobile exit → moderate experienced extractiveness. The scaffold perspective (regulatory coalition) has middle d (0.50-0.60): organized victims with constrained exit seeing a degrading constraint. The piton perspective (procurement system) has low d (0.15-0.25): institutional beneficiary of inertia with arbitrage options. The mountain perspective's d is observational (0.72) — the analytical observer sees the constraint from maximum distance but risks naturalizing what is contingent.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through perspectival diversity. The snare classification (from dependent manufacturers) is legitimate — they face extraction with no exit. The rope classification (from dominant suppliers) is also legitimate — the constraint coordinates their business ecosystem. The tangled rope classification (from large OEMs) is legitimate — genuine coordination coexists with asymmetric extraction. The scaffold classification (from regulatory coalitions) is legitimate — the constraint has a sunset mechanism (antitrust enforcement, supplier diversification mandates). The piton classification (from procurement systems) is legitimate — institutional inertia maintains a degraded process. The mountain classification is a FALSE SUMMIT — the constraint appears natural only from maximum distance; proximity reveals it is policy-contingent. Mandatrophy is resolved by recognizing that all six types are correct readings from their respective positions. The constraint is not 'really' a snare with a rope appearance; it is a snare FOR dependent manufacturers and a rope FOR dominant suppliers simultaneously. The policy question is not 'which type is correct?' but 'whose perspective should drive intervention?'
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    switching_cost_composition,
    'What proportion of observed switching costs are technical/irreducible vs. institutional/contingent?',
    'Decompose switching cost components: tooling amortization (technical), qualification cycles (technical), information asymmetry (institutional), contract penalties (institutional), integration risk perception (mixed). Direct measurement through cross-industry supplier-switch analyses.',
    'If technical costs dominate (>70%): consolidation barrier reflects genuine scale-economy constraint; snare classification is warranted. If institutional costs dominate (>70%): barrier is policy-contingent and could be significantly reduced through regulatory intervention; classification should shift toward snare-to-scaffold transition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(switching_cost_composition, empirical, 'Decomposition of technical vs institutional switching costs').

omega_variable(
    alternative_supplier_availability,
    'Does genuine technological/quality parity exist between dominant supplier and potential alternatives, but is obscured by information asymmetries and customer reluctance to test?',
    'Blind comparison testing of component specifications; analysis of product returns and quality variance across supplier cohorts when controlled for customer familiarity bias; case studies of forced supplier switches (supply disruptions, insolvency) and their actual impact on quality vs perceived risk.',
    'If parity exists but is obscured: barrier is suppression through information asymmetry and perception management (false snare, should be tangled_rope). If parity does not exist: barrier reflects genuine technical superiority and may be closer to rope than snare; consolidation is efficiency-driven.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_supplier_availability, empirical, 'Whether alternative suppliers offer genuine technical parity').

omega_variable(
    regulatory_intervention_timeline,
    'Can antitrust/industrial policy intervention realistically create viable alternative supplier ecosystems within 10-20 years?',
    'Historical analysis of previous supplier diversification mandates (defense contracting, automotive supplier networks, semiconductor fabs). Assessment of public investment requirements vs. policy feasibility in target jurisdictions.',
    'If timeline is realistic (>60% success probability): scaffold perspective is structural, not aspirational; sunset mechanism is genuine. If timeline is implausible: scaffold is theater, and the constraint is more durable than temporary; classification should shift toward persistent snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(regulatory_intervention_timeline, empirical, 'Feasibility of regulatory-driven supplier diversification').

omega_variable(
    information_asymmetry_intentionality,
    'Is the information asymmetry between dominant supplier and dependent customers a side effect of technical complexity or an intentionally maintained extraction mechanism?',
    'Analysis of supplier documentation practices, transparency in performance data, and pricing logic. Comparison of information disclosure across suppliers with different market power. Detection of deliberate obfuscation (proprietary lock-in, undisclosed dependencies, false equivalence claims).',
    'If intentional: snare classification is correct; extraction is deliberate and requires antitrust intervention. If side effect: constraint may be ropelike (genuine coordination with asymmetric benefits) or scaffold (solvable through better information tools).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_intentionality, empirical, 'Intentionality of information asymmetry in supplier lock-in').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(supplier_consolidation_barrier, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(supp_consol_tr_t0, supplier_consolidation_barrier, theater_ratio, 0, 0.38).
narrative_ontology:measurement(supp_consol_tr_t5, supplier_consolidation_barrier, theater_ratio, 5, 0.43).
narrative_ontology:measurement(supp_consol_tr_t10, supplier_consolidation_barrier, theater_ratio, 10, 0.48).

% Extraction over time
narrative_ontology:measurement(supp_consol_be_t0, supplier_consolidation_barrier, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(supp_consol_be_t5, supplier_consolidation_barrier, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(supp_consol_be_t10, supplier_consolidation_barrier, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(supplier_consolidation_barrier, resource_allocation).
narrative_ontology:affects_constraint(supplier_consolidation_barrier, supply_chain_resilience_concentration).
narrative_ontology:affects_constraint(supplier_consolidation_barrier, semiconductor_fab_consolidation).

% DUAL FORMULATION NOTE:
% Supplier consolidation barriers decompose into two structurally distinct constraints: the price extraction mechanism (this story, ε=0.58, snare primary) and the supply-chain resilience vulnerability (upstream constraint, ε=0.35, tangled rope — genuine coordination benefit coexists with catastrophic-failure asymmetry). The price extraction barrier is downstream of the resilience vulnerability — consolidation increases extraction because concentrated suppliers have more pricing power during risk periods.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(supplier_consolidation_barrier, powerful, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
