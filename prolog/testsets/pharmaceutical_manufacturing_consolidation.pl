% ============================================================================
% CONSTRAINT STORY: pharmaceutical_manufacturing_consolidation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pharmaceutical_manufacturing_consolidation, []).

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
 *   constraint_id: pharmaceutical_manufacturing_consolidation
 *   human_readable: Pharmaceutical Manufacturing Consolidation and Drug Supply Constraints
 *   domain: healthcare/pharmaceutical_supply
 *
 * SUMMARY:
 *   Pharmaceutical manufacturing consolidation represents a structural
 *   constraint where economic incentives, regulatory barriers, and capital
 *   intensity have progressively concentrated production capacity among a
 *   small number of large multinational firms. This consolidation extracts
 *   value through control of active pharmaceutical ingredient (API) supply,
 *   pricing power over generic manufacturers, and the ability to prioritize
 *   profitable drugs over essential but lower-margin medications. The
 *   constraint operates across multiple scales: globally through supply chain
 *   monopolization, nationally through regulatory compliance barriers, and
 *   individually through patient dependence on consolidated supply chains
 *   with no exit options. The constraint exhibits characteristics of a pure
 *   extraction mechanism (Snare) from most perspectives, with some
 *   coordination functions (Rope) visible from the beneficiary's position and
 *   with organized reform movements (Scaffold) creating potential sunset
 *   mechanisms through policy intervention.
 *
 * KEY AGENTS:
 *   - Patient Dependent on Essential Medications: Primary victim (powerless/trapped) — absolutely dependent on drug supply; faces price increases and supply shortages with no alternatives
 *   - Generic Manufacturers (small to mid-sized): Secondary victims (moderate/constrained) — cannot compete on cost against consolidated firms controlling API supply; face regulatory compliance barriers
 *   - Large Consolidated Pharmaceutical Manufacturers: Primary beneficiaries (institutional/arbitrage) — capture pricing power, supply chain control, and economies of scale; experience constraint as coordination
 *   - Developing Nation Healthcare Systems: Secondary victims (powerful/mobile) — face high prices and supply constraints but have some exit options through generic sourcing and local manufacturing
 *   - Regulatory Agencies (FDA/EMA): Institutional actors (institutional/arbitrage) — maintain approval processes that function as consolidation barriers; see compliance theater as necessary quality assurance
 *   - Generic Competition and Government Procurement Reform Movement: Organized actors (organized/mobile) — push policy levers to enable generic entry, pricing controls, and decentralized manufacturing
 *   - Analytical Observer: Civilizational context (analytical/analytical) — risks naturalizing contingent policy choices as immutable economic laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pharmaceutical_manufacturing_consolidation, 0.62).
domain_priors:suppression_score(pharmaceutical_manufacturing_consolidation, 0.68).
domain_priors:theater_ratio(pharmaceutical_manufacturing_consolidation, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pharmaceutical_manufacturing_consolidation, extractiveness, 0.62).
narrative_ontology:constraint_metric(pharmaceutical_manufacturing_consolidation, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(pharmaceutical_manufacturing_consolidation, theater_ratio, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pharmaceutical_manufacturing_consolidation, snare).
narrative_ontology:human_readable(pharmaceutical_manufacturing_consolidation, "Pharmaceutical Manufacturing Consolidation and Drug Supply Constraints").
narrative_ontology:topic_domain(pharmaceutical_manufacturing_consolidation, "healthcare/pharmaceutical_supply").

domain_priors:requires_active_enforcement(pharmaceutical_manufacturing_consolidation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pharmaceutical_manufacturing_consolidation, large_pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(pharmaceutical_manufacturing_consolidation, capital_investors).
narrative_ontology:constraint_victim(pharmaceutical_manufacturing_consolidation, generic_manufacturers).
narrative_ontology:constraint_victim(pharmaceutical_manufacturing_consolidation, patients_dependent_on_medications).
narrative_ontology:constraint_victim(pharmaceutical_manufacturing_consolidation, small_hospital_systems).
narrative_ontology:constraint_victim(pharmaceutical_manufacturing_consolidation, developing_nation_healthcare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PATIENT DEPENDENT ON ESSENTIAL MEDICATIONS (SNARE) — Cannot exit reliance on consolidated supply chains; bears full cost of manufacturing disruptions, price increases, and supply shortages. No alternative sources; health outcomes directly vulnerable to extraction mechanisms. Maximum experienced extraction — prisoner of pharmaceutical supply consolidation.
constraint_indexing:constraint_classification(pharmaceutical_manufacturing_consolidation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SMALL TO MID-SIZED GENERIC MANUFACTURER (TANGLED ROPE) — Faces high barriers to competing: regulatory compliance costs, capital investment requirements for specialized manufacturing, supply chain dependencies on consolidated suppliers. Some coordination benefit exists (established distribution networks, quality standards) but overshadowed by extractive pressure from dominant firms controlling active pharmaceutical ingredient (API) supply and market access. Constrained exit — can theoretically relocate or diversify, but at substantial cost.
constraint_indexing:constraint_classification(pharmaceutical_manufacturing_consolidation, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE CONSOLIDATED PHARMACEUTICAL MANUFACTURER (ROPE) — Benefits from consolidated control through economies of scale, pricing power, and supply chain monopolization. Experiences the constraint as pure coordination: standardized manufacturing, regulatory compliance frameworks, and vertical integration enable efficient drug production and distribution. Arbitrage options available (can exit to alternative markets, modify product lines). Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(pharmaceutical_manufacturing_consolidation, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COMPLIANCE THEATER (PITON) — FDA/EMA approval processes and good manufacturing practice (GMP) standards create significant barriers ostensibly for quality assurance, but the apparatus has become largely performative for large consolidated firms with sufficient capital to manage compliance. Theater ratio (0.45) reflects that compliance costs disproportionately harm smaller competitors while large firms absorb them as operational overhead. Regulatory certification persists as a functional quality gate but increasingly serves as inertial competitive moat maintained through institutional complexity rather than genuine necessity.
constraint_indexing:constraint_classification(pharmaceutical_manufacturing_consolidation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: GENERIC COMPETITION AND GOVERNMENT PROCUREMENT REFORM (SCAFFOLD) — Organized actors (generic manufacturers, government health agencies, patient advocacy groups, international generic sourcing initiatives) see consolidation as a temporary market failure addressable through: volume purchasing agreements, patent pool licensing, decentralized manufacturing incentives, and technology transfer programs to developing nations. Mobile exit options include strategic generic entry, compulsory licensing, and domestic manufacturing subsidies. Low effective extraction because organized actors have policy leverage and see sunset mechanisms: government procurement can privilege generics; patent expirations create entry windows; decentralized manufacturing technology (continuous flow, onshore production) reduces consolidation advantage.
constraint_indexing:constraint_classification(pharmaceutical_manufacturing_consolidation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: DEVELOPING NATION HEALTHCARE SYSTEMS (TANGLED ROPE) — Faces genuine coordination benefits (access to standardized pharmaceutical supply, quality assurance through international regulatory frameworks) alongside extractive pricing and supply control. Some exit options exist through generic sourcing, local manufacturing development, and international aid programs, but these are costly and politically constrained. Mobile exit options are real but high-friction. Bears asymmetric extraction through pricing power of consolidated manufacturers while also coordinating around international standards.
constraint_indexing:constraint_classification(pharmaceutical_manufacturing_consolidation, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, some manufacturing consolidation may appear immutable due to: capital intensity of pharmaceutical production, regulatory complexity creating barriers, economies of scale in distributed production networks, and scientific/technical expertise concentration. This perspective risks naturalizing what is actually a contingent policy choice (patent protection duration, regulatory approval timelines, capital subsidy patterns). The engine will likely identify this as a false summit — consolidation is not inherent to pharmaceutical chemistry but rather to institutional arrangements that could be restructured.
constraint_indexing:constraint_classification(pharmaceutical_manufacturing_consolidation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pharmaceutical_manufacturing_consolidation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pharmaceutical_manufacturing_consolidation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pharmaceutical_manufacturing_consolidation, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pharmaceutical_manufacturing_consolidation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pharmaceutical_manufacturing_consolidation, TR),
    TR >= 0.70.

:- end_tests(pharmaceutical_manufacturing_consolidation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.62): High. Consolidated manufacturers capture pricing power over patients and competing generic firms, with extractiveness rising over the 30-year interval from 0.38 to 0.68. The increase reflects progressive market concentration through M&A, supply chain integration, and API sourcing monopolization. The constraint is sufficiently severe (χ ≥ 0.66 from powerless/trapped perspective) to classify as Snare. Suppression (0.68): High. Multiple reinforcing barriers prevent exit or alternative sourcing: regulatory approval timelines (7-10 years for new entrants), capital requirements ($500M-$1B+ for manufacturing facilities), API supply control by consolidated firms, and patent protection creating market exclusivity. Patients cannot switch drugs; generic manufacturers cannot enter markets without decade-long approval processes. Theater ratio (0.45): Moderate. Regulatory compliance generates performative content (documentation, audit trails) but also serves genuine quality assurance function. The ratio is lower than pure piton constraints because manufacturing safety is genuinely consequential — but consolidated firms absorb compliance costs as overhead while smaller competitors cannot, making theater asymmetrically extractive.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence: Snare (powerless victim), Tangled Rope (moderate competitor), Rope (institutional beneficiary), Piton (regulatory apparatus), Scaffold (organized reform), and false-summit Mountain (civilizational naturalization). This divergence reflects real structural differences in how agents experience the same consolidation pressure. The beneficiary experiences pure coordination; the victim experiences pure extraction; the organized actor sees contingency and policy levers. The analytical risk is high: characterizing consolidation as 'inevitable' due to capital intensity or economies of scale naturalizes what is actually policy-chosen (patent duration, regulatory approval timelines, foreign ownership restrictions).
 *
 * DIRECTIONALITY LOGIC:
 *   Consolidated manufacturers (beneficiaries, institutional power, arbitrage exit) derive low directionality (d ≈ 0.10-0.20) because they benefit from the constraint and have exit optionality. Patients (victims, powerless, trapped) derive high directionality (d ≈ 0.95) because they bear costs and cannot exit. Generic manufacturers (victims with constrained exit) derive moderate-high directionality (d ≈ 0.70-0.75) because they face high costs but have theoretical exit paths (market repositioning, geographic relocation, technology development) at significant expense. Developing nation healthcare systems (victims with mobile exit) derive moderate directionality (d ≈ 0.60-0.65) because they can access generic sourcing and local manufacturing but at implementation cost. The scaffold's organized actors (mobile exit) derive low-moderate directionality (d ≈ 0.35-0.45) because they see policy levers and maintain agency even though extraction affects their constituencies.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED: Extractiveness (0.62), suppression (0.68), and effective extraction χ ≥ 0.66 from powerless/trapped perspective satisfy snare thresholds. The constraint has no genuine coordination function for victims — patients cannot 'coordinate' their way out of medication dependence, and competing manufacturers face barriers designed to exclude them. The consolidation serves the beneficiary's interests (price control, market monopoly) more than genuine coordination. However, the scaffold perspective identifies real policy mechanisms that could restructure the constraint: generic competition incentives, API sourcing alternatives, patent duration adjustments, regulatory approval acceleration for generic firms. These mechanisms are policy-contingent, not structural — they require organized political action but are genuinely available. The mandatrophy resolves by showing the Snare is maintainable only if these policy levers remain unexploited.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    manufacturing_decentralization_feasibility,
    'Can modern pharmaceutical manufacturing technology (continuous flow reactors, portable API synthesis) economically enable decentralized production at scales that would break consolidation pressure?',
    'Cost analysis of decentralized vs centralized production for top 50 drugs; pilot programs in generic manufacturing jurisdictions; technology adoption rates in developing nations',
    'If feasible: consolidation is policy-contingent (Scaffold sunset real). If infeasible: consolidation is economically forced (Mountain closer to true). Classification could shift from Snare to Rope if decentralization reduces extractive mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufacturing_decentralization_feasibility, empirical, 'Whether decentralized pharmaceutical manufacturing can economically compete').

omega_variable(
    regulatory_compliance_necessity_threshold,
    'What minimum regulatory compliance cost is genuinely necessary for pharmaceutical quality, versus what is barrier-generating bureaucratic theater?',
    'Comparative analysis of failure rates and quality metrics across regulatory regimes; cost-benefit analysis of specific FDA vs EMA vs developing-nation approval timelines; identification of redundant compliance steps',
    'If genuine necessity threshold is high: barriers are structural (Snare confirmed). If threshold is low: regulatory theater is primary consolidation mechanism (Piton perspective is primary, Snare recedes).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_compliance_necessity_threshold, empirical, 'Genuine necessity versus theater in pharmaceutical regulatory compliance').

omega_variable(
    patent_protection_duration_optimal_tradeoff,
    'What patent protection duration optimally balances innovation incentive against consolidation pressure and supply monopolization?',
    'Historical correlation between patent term and generic entry timing; R&D productivity vs market concentration metrics; international comparison of patent regimes with health outcomes',
    'If current durations exceed optimal: patent policy is primary driver (policy-changeable, Scaffold real). If current durations are near-optimal: consolidation has deeper structural roots (Snare or Mountain closer to true).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patent_protection_duration_optimal_tradeoff, preference, 'Optimal patent protection duration balancing innovation and access').

omega_variable(
    active_ingredient_sourcing_alternatives,
    'Can alternative sourcing arrangements (government stockpiles, international pooling, synthetic biology automation) reduce API supply monopolization by consolidated manufacturers?',
    'Feasibility studies for government API production facilities; international generic sourcing network analysis; synthetic biology cost curves for commodity APIs',
    'If alternatives are viable: consolidation pressure on APIs is policy-addressable (Scaffold sunrise). If alternatives face insurmountable barriers: API consolidation is structural (Snare mechanism deepens).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(active_ingredient_sourcing_alternatives, empirical, 'Viability of alternative API sourcing arrangements').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pharmaceutical_manufacturing_consolidation, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pharma_mfg_tr_t0, pharmaceutical_manufacturing_consolidation, theater_ratio, 0, 0.32).
narrative_ontology:measurement(pharma_mfg_tr_t10, pharmaceutical_manufacturing_consolidation, theater_ratio, 10, 0.38).
narrative_ontology:measurement(pharma_mfg_tr_t20, pharmaceutical_manufacturing_consolidation, theater_ratio, 20, 0.45).
narrative_ontology:measurement(pharma_mfg_tr_t30, pharmaceutical_manufacturing_consolidation, theater_ratio, 30, 0.52).

% Extraction over time
narrative_ontology:measurement(pharma_mfg_be_t0, pharmaceutical_manufacturing_consolidation, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(pharma_mfg_be_t10, pharmaceutical_manufacturing_consolidation, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(pharma_mfg_be_t20, pharmaceutical_manufacturing_consolidation, base_extractiveness, 20, 0.62).
narrative_ontology:measurement(pharma_mfg_be_t30, pharmaceutical_manufacturing_consolidation, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pharmaceutical_manufacturing_consolidation, resource_allocation).
narrative_ontology:affects_constraint(pharmaceutical_manufacturing_consolidation, drug_pricing_monopoly).
narrative_ontology:affects_constraint(pharmaceutical_manufacturing_consolidation, generic_market_entry_barriers).
narrative_ontology:affects_constraint(pharmaceutical_manufacturing_consolidation, api_supply_chain_concentration).

% DUAL FORMULATION NOTE:
% Pharmaceutical manufacturing consolidation is upstream of drug pricing monopoly (higher consolidation enables higher pricing power) and API supply concentration (consolidated manufacturers control vertical integration). Generic market entry barriers are downstream — consolidation creates barriers through regulatory complexity and capital requirements. These constraints form a causal family with shared root causes (patent protection duration, regulatory approval timelines, capital intensity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pharmaceutical_manufacturing_consolidation, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
