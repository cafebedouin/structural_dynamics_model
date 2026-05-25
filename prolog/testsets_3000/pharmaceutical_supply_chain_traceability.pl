% ============================================================================
% CONSTRAINT STORY: pharmaceutical_supply_chain_traceability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_pharmaceutical_supply_chain_traceability, []).

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
 *   constraint_id: pharmaceutical_supply_chain_traceability
 *   human_readable: Pharmaceutical Supply Chain Traceability Requirements
 *   domain: healthcare/regulatory/economic
 *
 * SUMMARY:
 *   Pharmaceutical supply chain traceability is a regulatory system designed
 *   to prevent counterfeit medicines from reaching patients — a legitimate
 *   public health objective. However, the implementation mechanism creates a
 *   classic Tangled Rope constraint: genuine coordination function (supply
 *   chain visibility, rapid recall response, counterfeit detection) coexists
 *   with asymmetric extraction (compliance costs concentrated on small-scale
 *   and resource-constrained actors, competitive advantage to large-scale
 *   manufacturers, barrier to market entry for generics). The constraint
 *   exhibits high suppression (0.65) through regulatory mandate with minimal
 *   cost-sharing mechanisms, and high theater ratio (0.58) as much compliance
 *   remains documentary rather than functionally verified. The perspectival
 *   gap reveals the constraint's hybrid nature: brand manufacturers see pure
 *   coordination (Rope), rural pharmacies see pure extraction (Snare),
 *   generic manufacturers see mixed dynamics (Tangled Rope), regulatory
 *   authorities see coordination with extractive leverage (Tangled Rope), and
 *   legacy paper systems show theatrical persistence (Piton). The measurement
 *   trajectory shows extractiveness increasing from 0.35 to 0.63 over the
 *   interval, indicating progressive cost-shifting without compensating
 *   functionality improvement — a signature of extraction layering onto
 *   coordination.
 *
 * KEY AGENTS:
 *   - Brand Manufacturers: Primary beneficiary (institutional/arbitrage) — reduce counterfeit competition, enhance supply chain control, experience minimal compliance burden due to existing scale
 *   - Generic Manufacturers: Secondary beneficiary/victim (moderate/constrained) — benefit from counterfeit reduction but face disproportionate compliance costs relative to margins
 *   - Rural Pharmacies: Primary victim (powerless/trapped) — must implement expensive serialization systems without cost support or licensing alternatives
 *   - Regulatory Authorities: Organized beneficiary/enforcer (organized/constrained) — gain verification authority and supply chain transparency but don't bear proportional compliance costs
 *   - Small Developing-World Healthcare Systems: Primary victim (powerless/trapped) — face medicine shortages if informal supply excluded; cannot absorb compliance infrastructure costs
 *   - Informal Medicine Distributors: Excluded actor (powerless/trapped) — excluded entirely from formal systems regardless of actual safety performance; no exit pathway
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risk of naturalizing cost-shifting as inevitable feature of complex supply chains
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(pharmaceutical_supply_chain_traceability, 0.58).
domain_priors:suppression_score(pharmaceutical_supply_chain_traceability, 0.65).
domain_priors:theater_ratio(pharmaceutical_supply_chain_traceability, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(pharmaceutical_supply_chain_traceability, extractiveness, 0.58).
narrative_ontology:constraint_metric(pharmaceutical_supply_chain_traceability, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(pharmaceutical_supply_chain_traceability, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(pharmaceutical_supply_chain_traceability, tangled_rope).
narrative_ontology:human_readable(pharmaceutical_supply_chain_traceability, "Pharmaceutical Supply Chain Traceability Requirements").
narrative_ontology:topic_domain(pharmaceutical_supply_chain_traceability, "healthcare/regulatory/economic").

domain_priors:requires_active_enforcement(pharmaceutical_supply_chain_traceability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(pharmaceutical_supply_chain_traceability, regulatory_authorities).
narrative_ontology:constraint_beneficiary(pharmaceutical_supply_chain_traceability, brand_manufacturers).
narrative_ontology:constraint_beneficiary(pharmaceutical_supply_chain_traceability, anti_counterfeiting_interests).
narrative_ontology:constraint_victim(pharmaceutical_supply_chain_traceability, generic_manufacturers).
narrative_ontology:constraint_victim(pharmaceutical_supply_chain_traceability, small_pharmacies).
narrative_ontology:constraint_victim(pharmaceutical_supply_chain_traceability, resource_constrained_healthcare_systems).
narrative_ontology:constraint_victim(pharmaceutical_supply_chain_traceability, patients_in_developing_regions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RURAL PHARMACIES (SNARE) — Cannot exit traceability requirements; must implement expensive serialization, track-and-trace systems, and regulatory documentation or lose operating licenses. Trapped by regulatory mandate with no cost-sharing mechanism. Bears full burden of compliance infrastructure designed for industrial-scale supply chains. No alternative sourcing pathways. Maximum extraction from perspectival position.
constraint_indexing:constraint_classification(pharmaceutical_supply_chain_traceability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: GENERIC MANUFACTURERS (TANGLED ROPE) — Experience both genuine coordination (reducing counterfeit medicines improves market trust; supply chain visibility benefits inventory management and recall response) AND asymmetric extraction (compliance costs disproportionately burden generics relative to brand manufacturers; margins compressed by serialization infrastructure investment; competitive disadvantage in price competition). Constrained exit: cannot serve markets without compliance, but compliance costs higher than brand competitors due to scale economics.
constraint_indexing:constraint_classification(pharmaceutical_supply_chain_traceability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: BRAND MANUFACTURERS (ROPE) — Net beneficiaries. Traceability requirements reduce counterfeit competition, protect brand value, and create switching costs for customers. Already operate at industrial scale with existing compliance infrastructure. Serialization costs are low relative to revenue; in fact, traceability investments enhance premium positioning and supply chain control. Experience constraint primarily as coordination: formalized supply chain enables better supply forecasting, inventory optimization, and recall management. Arbitrage exit available: can implement proprietary track-and-trace systems independent of regulatory mandates.
constraint_indexing:constraint_classification(pharmaceutical_supply_chain_traceability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY AUTHORITIES (TANGLED ROPE) — Enforcement requires active investment in verification infrastructure (databases, inspections, audits). Genuine coordination function: traceability enables rapid response to contaminated batches, identification of counterfeit sources, and supply chain risk detection. BUT also extraction mechanism: regulations shift compliance costs to suppliers and final-mile actors; public agencies don't bear proportional cost burden; leverage enforcement authority to advance industrial policy favoring large-scale players. Constrained by political economy: cannot simply mandate without supporting compliance infrastructure, but do so anyway.
constraint_indexing:constraint_classification(pharmaceutical_supply_chain_traceability, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: PAPER COMPLIANCE SYSTEMS (PITON) — In many jurisdictions, traceability remains largely performative: documented chain of custody without actual digital verification, batch tracking without real-time visibility, serialization with manual record-keeping prone to falsification. Theater ratio high (0.58 overall, concentrated in this perspective). Systems persist through regulatory habit and institutional inertia despite low functionality — actual counterfeit detection rates remain low; compliance documentation rarely prevents diversion. Functional verification bypassed; ritual maintained.
constraint_indexing:constraint_classification(pharmaceutical_supply_chain_traceability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of naturalizing the constraint: pharmaceutical supply chain verification is genuinely complex — billions of units, thousands of suppliers, multiple intermediaries, informal sectors, diverse regulatory jurisdictions. Some verification lag and information asymmetry appear structurally inevitable. But the structural data reveals false summit: the 'inherent complexity' framing disguises choices (centralized vs distributed verification, cost-sharing vs cost-shifting, standardized vs proprietary systems). Complexity is real; inevitability is not.
constraint_indexing:constraint_classification(pharmaceutical_supply_chain_traceability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(pharmaceutical_supply_chain_traceability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(pharmaceutical_supply_chain_traceability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(pharmaceutical_supply_chain_traceability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(pharmaceutical_supply_chain_traceability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(pharmaceutical_supply_chain_traceability, TR),
    TR >= 0.70.

:- end_tests(pharmaceutical_supply_chain_traceability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint shifts compliance infrastructure costs to supply-side actors (manufacturers, distributors, pharmacies) while distributing benefits primarily to brand manufacturers (competitive advantage) and regulatory authorities (leverage and data access). Generic manufacturers and small pharmacies bear cost burden without proportional benefit. The 0.58 value reflects that counterfeiting IS a real problem and traceability DOES provide some genuine coordination function — the extraction is not maximal because the underlying goal is legitimate. However, the cost distribution mechanism indicates deliberate extraction: compliance burdens are designed to scale cost per unit, penalizing low-margin producers. Suppression (0.65): High. Regulatory mandate removes the exit option for licensed operators. Small pharmacies cannot serve patients without complying, even if they want to operate entirely in informal channels. Small developing-world pharmacies face suppression through cost barrier (cannot afford serialization infrastructure) combined with regulatory mandate. Informal distributers face suppression through exclusion. Theater ratio (0.58): Moderate-high. Many jurisdictions maintain paper-based documentation that provides the appearance of traceability without functional verification. Batch numbers are recorded but not verified; chain of custody is documented but not audited; serialization exists but is routinely falsified in low-regulatory-capacity regions. As digital systems mature, theater should decrease; the 0.58 reflects current mixed state of analog documentation and emerging digital systems.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same regulatory structure (traceability mandates) produces fundamentally different experienced constraints across positions. The brand manufacturer sees a market-protective coordination mechanism (Rope). The regulatory authority sees public health coordination with some enforcement leverage (Tangled Rope). The generic manufacturer sees mixed dynamics — genuine counterfeiting risk reduction but asymmetric compliance burden (Tangled Rope). The rural pharmacy sees a regulatory mandate to implement systems they cannot afford, without support or alternatives (Snare). The analytical observer sees the real public health problem (counterfeiting) alongside the real cost-distribution problem (who bears the infrastructure burden). The gap reveals whether the constraint's primary function is genuine (counterfeiting prevention) or secondary/extractive (market consolidation by excluding low-cost competitors). The omegas target this ambiguity: if cost is distributed fairly and informal supply is integrated, the constraint is Rope with some enforcement cost. If cost is asymmetrically concentrated and informal supply is excluded, the constraint is Snare with regulatory cover.
 *
 * DIRECTIONALITY LOGIC:
 *   Brand manufacturers have beneficiary status and arbitrage-level exit (can implement proprietary systems or comply selectively); their d ≈ 0.10, producing low effective extraction f(d) ≈ -0.01. Generic manufacturers have victim status (compressed margins) balanced by some beneficiary status (reduced counterfeits); constrained exit; d ≈ 0.55, producing moderate extraction f(d) ≈ 0.75. Rural pharmacies have full victim status and trapped exit (must comply or lose license, cannot afford compliance); d ≈ 0.95, producing high extraction f(d) ≈ 1.42. Regulatory authorities have beneficiary status (gain leverage/data) but moderate institutional power with constrained exit (must maintain political legitimacy); d ≈ 0.35, producing moderate extraction f(d) ≈ 0.25. The analytical observer (d ≈ 0.72) produces f(d) ≈ 1.15. The scope modifier σ(S) for global scope (1.2) scales extractiveness at continental levels upward: χ for powerless agents at global scope = 0.58 × 1.42 × 1.2 ≈ 0.99 (nearly maximal experienced extraction). This directionality structure reveals why rural pharmacies in developing regions are devastated by the constraint while brand manufacturers absorb it easily.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy here is whether traceability requirements function as legitimate coordination for public health or as disguised extraction for industrial consolidation. The measured extractiveness (0.58) with genuine beneficiaries (counterfeiting IS reduced) and genuine victims (rural pharmacies face impossible costs) indicates Tangled Rope — both functions are structurally present. However, the classification is fragile: if omega omega_counterfeit_detection_efficacy resolves to 'low,' the coordination function evaporates and the constraint reclassifies upward to Snare. If omega_cost_distribution_mechanism resolves to 'highly asymmetric,' extraction increases and the constraint hardens. If omega_informal_sector_capture resolves to 'exclusionary,' the victim set expands dramatically. The mandatrophy is resolved by insisting that BOTH the public health benefit AND the cost-distribution mechanism are structurally real, and the constraint class is determined by their relative magnitude. At current parameters (0.58 extractiveness with genuine coordination), Tangled Rope is correct. The measurement trajectory showing extractiveness rising from 0.35 to 0.63 indicates the extraction component is strengthening relative to coordination — a warning sign that the constraint could be degrading from hybrid toward pure Snare over the generational horizon.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfeit_detection_efficacy,
    'Do traceability systems actually prevent counterfeit medicines from reaching patients, or do they primarily provide post-hoc documentation?',
    'Comparison of counterfeit incident rates before/after implementation; tracking of detected counterfeits attributed to traceability systems vs other detection methods; analysis of failure cases where traceability failed to prevent harm',
    'If efficacy high: coordination function is genuine (Rope/Tangled Rope confirmed). If low: extractive mechanism disguised as public health benefit (Snare/Piton confirmed); reclassify upward to pure extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfeit_detection_efficacy, empirical, 'Whether traceability systems prevent counterfeits or enable post-hoc documentation').

omega_variable(
    cost_distribution_mechanism,
    'Is the cost burden actually distributed proportionally across supply chain actors based on benefit received, or concentrated on final-mile actors regardless of risk exposure?',
    'Cost accounting across supply chain segments (manufacturer, distributor, pharmacy); correlation between implementation cost per unit and counterfeiting risk exposure by actor; analysis of who bears serialization infrastructure costs vs who benefits from counterfeit prevention',
    'If proportional: Tangled Rope (hybrid coordination/extraction) confirmed. If asymmetrically concentrated: reclassify Rope beneficiaries to Snare, raising effective extraction from 0.58 to 0.75+.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cost_distribution_mechanism, empirical, 'Whether compliance costs are distributed proportionally across supply chain actors').

omega_variable(
    informal_sector_capture,
    'Do traceability requirements formalize informal pharmaceutical distribution (improving safety) or exclude it entirely (reducing access in resource-constrained regions)?',
    'Pre/post implementation analysis of informal sector supply in traceability-regulated regions vs unregulated regions; tracking of medicine access (supply availability and affordability) in low-income areas before/after traceability mandates; counterfactual: what happens to patients if informal supply is shut down',
    'If inclusive formalization: Scaffold perspective (temporary coordination problem with sunset to inclusive system). If exclusionary: victims expand significantly (access barriers in developing regions); reclassify to pure Snare with continental scope extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(informal_sector_capture, empirical, 'Whether traceability requirements formalize or exclude informal pharmaceutical distribution').

omega_variable(
    technology_standardization_pathway,
    'Will digital traceability converge on open standards (reducing proprietary lock-in) or embed proprietary platforms (centralizing control)?',
    'Historical analysis of similar supply chain standardization efforts (food traceability, automotive parts); tracking of standard-setting bodies and their composition; analysis of emerging blockchain/IoT platform ecosystems and their licensing terms',
    'If open standards: extraction mechanism weakens over time (Scaffold perspective confirmed). If proprietary: extraction persists and hardens (Piton perspective confirmed); vendors accumulate leverage.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(technology_standardization_pathway, empirical, 'Whether digital traceability converges on open or proprietary standards').

omega_variable(
    regulatory_capture_mechanism,
    'Do traceability regulations reflect genuine public health priorities or captured rulemaking favoring industrial consolidation?',
    'Analysis of regulatory impact studies; tracking of lobby participation in standard-setting; correlation between traceability mandate stringency and market concentration in pharmaceuticals before/after; comparison of regulatory outcomes in jurisdictions with different regulatory capture indicators',
    'If captured: Tangled Rope extraction component is disguised industrial policy. If public-health-driven: extraction component reflects genuine coordination cost-benefit asymmetry.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_capture_mechanism, conceptual, 'Whether traceability regulations reflect public health or capture by industrial consolidation interests').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(pharmaceutical_supply_chain_traceability, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pharm_trace_tr_t0, pharmaceutical_supply_chain_traceability, theater_ratio, 0, 0.42).
narrative_ontology:measurement(pharm_trace_tr_t3, pharmaceutical_supply_chain_traceability, theater_ratio, 3, 0.5).
narrative_ontology:measurement(pharm_trace_tr_t6, pharmaceutical_supply_chain_traceability, theater_ratio, 6, 0.58).
narrative_ontology:measurement(pharm_trace_tr_t9, pharmaceutical_supply_chain_traceability, theater_ratio, 9, 0.65).

% Extraction over time
narrative_ontology:measurement(pharm_trace_be_t0, pharmaceutical_supply_chain_traceability, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(pharm_trace_be_t3, pharmaceutical_supply_chain_traceability, base_extractiveness, 3, 0.47).
narrative_ontology:measurement(pharm_trace_be_t6, pharmaceutical_supply_chain_traceability, base_extractiveness, 6, 0.58).
narrative_ontology:measurement(pharm_trace_be_t9, pharmaceutical_supply_chain_traceability, base_extractiveness, 9, 0.63).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(pharmaceutical_supply_chain_traceability, enforcement_mechanism).
narrative_ontology:affects_constraint(pharmaceutical_supply_chain_traceability, generic_drug_market_access).
narrative_ontology:affects_constraint(pharmaceutical_supply_chain_traceability, counterfeit_medicine_detection).
narrative_ontology:affects_constraint(pharmaceutical_supply_chain_traceability, healthcare_supply_chain_resilience).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(pharmaceutical_supply_chain_traceability, moderate, 0.55).
constraint_indexing:directionality_override(pharmaceutical_supply_chain_traceability, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
