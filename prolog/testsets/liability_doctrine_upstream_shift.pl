% ============================================================================
% CONSTRAINT STORY: liability_doctrine_upstream_shift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_liability_doctrine_upstream_shift, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: liability_doctrine_upstream_shift
 *   human_readable: Liability Doctrine Upstream Shift: Coordination vs Extraction in Risk Attribution
 *   domain: legal/regulatory/political_economy
 *
 * SUMMARY:
 *   Liability doctrine upstream shift is a legal mechanism that pushes
 *   responsibility for product safety and harm backward through supply chains
 *   toward original producers and component manufacturers. Originally framed
 *   as efficient risk allocation (those best positioned to control risk
 *   should bear it), the doctrine has evolved into a mechanism that
 *   increasingly assigns liability to actors who have minimal control over
 *   downstream use, distribution failures, or system-level integration
 *   problems. This constraint exhibits tangled rope structure: it contains
 *   genuine coordination functions (incentivizing quality control,
 *   information sharing, early risk identification in supply chains)
 *   alongside asymmetric extraction (shifting costs and risk to less powerful
 *   actors with constrained exit options). The upstream shift has accelerated
 *   over the measurement interval as litigation volume has increased,
 *   insurance premiums have risen, and defensive documentation requirements
 *   have proliferated. Theater ratio shows increasing performative activity
 *   relative to functional risk reduction — much legal and insurance activity
 *   addresses liability allocation and dispute resolution rather than actual
 *   safety improvement.
 *
 * KEY AGENTS:
 *   - Upstream Producers (Small/Medium Enterprises): Primary victims (powerless/trapped) — component suppliers, materials manufacturers bear expanding liability for failures they cannot control or observe
 *   - Mid-Tier Supply Chain Actors: Secondary victims (moderate/constrained) — experience mixed coordination and extraction; constrained by insurance costs and defensive documentation
 *   - Downstream Enterprises (Large Manufacturers/Retailers): Primary beneficiaries (institutional/arbitrage) — benefit from asymmetric risk allocation without equivalent constraint; can arbitrage supplier relationships
 *   - Regulatory Agencies (EPA, CPSC, Consumer Protection): Organized coalition (organized/constrained) — see liability doctrine as temporary coordination mechanism with sunset toward direct regulation
 *   - Insurance Industry: Institutional actor (institutional/arbitrage) — captures premiums and manages risk pools; benefits from liability expansion that increases demand for insurance
 *   - Judicial System: Institutional actor (institutional/arbitrage) — maintains precedent through case accumulation; exhibits piton dynamics (theater in dispute resolution)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(liability_doctrine_upstream_shift, 0.58).
domain_priors:suppression_score(liability_doctrine_upstream_shift, 0.62).
domain_priors:theater_ratio(liability_doctrine_upstream_shift, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(liability_doctrine_upstream_shift, extractiveness, 0.58).
narrative_ontology:constraint_metric(liability_doctrine_upstream_shift, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(liability_doctrine_upstream_shift, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(liability_doctrine_upstream_shift, tangled_rope).
narrative_ontology:human_readable(liability_doctrine_upstream_shift, "Liability Doctrine Upstream Shift: Coordination vs Extraction in Risk Attribution").
narrative_ontology:topic_domain(liability_doctrine_upstream_shift, "legal/regulatory/political_economy").

domain_priors:requires_active_enforcement(liability_doctrine_upstream_shift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(liability_doctrine_upstream_shift, downstream_enterprises).
narrative_ontology:constraint_beneficiary(liability_doctrine_upstream_shift, regulatory_agencies).
narrative_ontology:constraint_victim(liability_doctrine_upstream_shift, upstream_producers).
narrative_ontology:constraint_victim(liability_doctrine_upstream_shift, innovation_capacity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UPSTREAM PRODUCER (SNARE) — Small materials suppliers, component manufacturers, early-stage producers in supply chains bear expanding liability for downstream failures they cannot control or observe. Trapped by contract requirements and regulatory mandates. No exit except market exit. Maximum extraction: bears costs for defects they did not create, misuse they cannot prevent, systemic failures in distribution chains.
constraint_indexing:constraint_classification(liability_doctrine_upstream_shift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: MID-TIER SUPPLY CHAIN ACTOR (TANGLED ROPE) — Manufacturers and distributors in middle of supply chain experience both coordination benefits and asymmetric extraction. Genuine coordination function: liability doctrine drives quality control, information sharing, and risk management across the chain. But extraction is real: constrained by high insurance costs, defensive documentation requirements, and liability cascades from both directions. Constrained exit due to market dependency.
constraint_indexing:constraint_classification(liability_doctrine_upstream_shift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DOWNSTREAM ENTERPRISE (ROPE) — Large consumer-facing enterprises benefit from upstream liability shift without equivalent constraint. Experience the doctrine as coordination: suppliers maintain quality and provide indemnification. Net beneficiary with arbitrage options (can shift suppliers, externalize costs, lobby for favorable interpretations). Low experienced extraction — the constraint works to their advantage through asymmetric risk allocation.
constraint_indexing:constraint_classification(liability_doctrine_upstream_shift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGULATORY COALITION (SCAFFOLD) — Public health agencies (EPA, CPSC, OSHA) and consumer protection bodies see liability shift as temporary coordination solution with sunset logic. Upstream liability creates incentives for early risk identification and safety investment that regulatory mandates alone might not achieve. Constrained by political feasibility but see the doctrine as transitional: as regulation matures, direct safety standards could replace liability-based incentives. Scaffold reflects the sunset horizon: liability doctrine is efficient interim mechanism until regulatory framework is comprehensive enough to make it redundant.
constraint_indexing:constraint_classification(liability_doctrine_upstream_shift, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LEGAL PRECEDENT SYSTEM (PITON) — The doctrine persists through institutional inertia and case law accumulation. Theater ratio reflects that much litigation activity is ritualistic dispute resolution rather than substantive risk allocation or knowledge improvement. The legal system sees the doctrine as somewhat degraded (courts manage liability cascades, insurance pools, and settlements that are increasingly decoupled from actual causation). Maintained through precedent and path dependency rather than because the mechanism optimally allocates risk.
constraint_indexing:constraint_classification(liability_doctrine_upstream_shift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the upstream shift exhibits both genuine coordination and real extraction. Coordination function: creates incentive for information sharing, quality control, and early risk identification in supply chains. Extraction function: asymmetrically shifts risk and cost burden to less powerful actors (upstream) who cannot exit the chain. The constraint persists because both functions are real — it is not pure extraction, nor is it pure coordination. The analytical view sees this as a transitional form where coordination goals and extractive power asymmetries are intertwined.
constraint_indexing:constraint_classification(liability_doctrine_upstream_shift, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(liability_doctrine_upstream_shift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(liability_doctrine_upstream_shift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(liability_doctrine_upstream_shift, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(liability_doctrine_upstream_shift, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(liability_doctrine_upstream_shift, TR),
    TR >= 0.70.

:- end_tests(liability_doctrine_upstream_shift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, reflecting that the doctrine genuinely coordinates supply chain safety but increasingly shifts costs asymmetrically. The upward trend (0.32 → 0.58) shows that as litigation volume increased and insurance costs rose, extraction has become more pronounced relative to coordination benefit. The constraint started as a coordination mechanism but is accumulating extractive load. Suppression (0.62): Moderate-high. Significant barriers to upstream exit include contractual requirements for liability indemnification, regulatory mandates requiring liability insurance, market integration that makes exit economically infeasible, and information asymmetries preventing alternative governance structures. Suppression is not total because upstream actors can influence liability exposure through quality control and documentation, but the exit barriers are substantial. Theater ratio (0.55): Moderate, reflecting that while the doctrine has real safety coordination effects, significant activity is ritualistic litigation, insurance administration, and defensive documentation rather than substantive risk reduction. The upward trend (0.38 → 0.55) shows that as the doctrine has matured, theater has increased relative to functional risk management.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same legal mechanism produces opposite experiential classifications depending on structural position. The beneficiary's rope is genuine — liability creates coordination incentives. The victim's snare is also genuine — they bear asymmetric costs. The analytical observer's tangled rope is the unified view that both mechanisms operate simultaneously. The regulatory coalition's scaffold reflects the temporal dimension — liability is seen as a transitional mechanism until direct regulation matures. The legal system's piton reflects institutional inertia — the doctrine persists through precedent and path dependency rather than because it optimally allocates risk. No single perspective is 'wrong' — each captures a real structural relationship to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are driven by structural position in the liability cascade. Downstream enterprises occupy a beneficiary position with arbitrage options — they can shift suppliers, externalize costs to insurance markets, and lobby for favorable legal interpretations. This produces low d and low f(d), resulting in negative effective extraction. Upstream producers occupy a victim position with trapped exit options — contracts require liability indemnification, regulations mandate insurance, and market dependency makes exit economically infeasible. This produces high d and high f(d), resulting in high experienced extraction. Mid-tier actors occupy constrained positions with partial benefits (they benefit from upstream quality control but also bear part of the liability cascade from downstream). Regulatory agencies occupy organized positions with constrained exit (they benefit from liability-driven safety coordination but are constrained by political feasibility of direct regulation). The directionality pipeline differentiates these positions through the beneficiary/victim declarations and exit_options assignments.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVES THROUGH STRUCTURE: The mandate/atrophy tension here is whether liability doctrine is a functional coordination mechanism (mandate) or an extractive power play dressed in neutral risk allocation language (atrophy). The resolution is: it is both. The coordination function is real — liability does create incentives for quality control and information sharing. The extraction is also real — it asymmetrically shifts costs to less powerful actors. The classification as Tangled Rope rather than pure Rope or pure Snare reflects this hybrid reality. The upward trend in extractiveness (0.32 → 0.58) and theater ratio (0.38 → 0.55) suggests that as the doctrine has matured, extraction has accumulated relative to coordination benefit — the constraint is experiencing mandate drift toward atrophy. The scaffold perspective from regulatory agencies suggests a possible resolution: as direct safety regulation matures, liability could sunset as a primary mechanism, transitioning to a secondary role. This would reset the mandate/atrophy cycle.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    causation_attribution_ambiguity,
    'How much of product failure is attributable to upstream producer vs downstream distribution, end-user misuse, system integration, or environmental factors?',
    'Empirical analysis of failure mode distributions across product categories; tracing causation in product liability cases; laboratory vs field failure correlations',
    'If upstream causation is typically < 30%: liability shift is extractive (snare). If upstream causation is typically 50-70%: mixed model (tangled_rope). If upstream causation is > 80%: shift is justified coordination (rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causation_attribution_ambiguity, empirical, 'Attribution of causation between upstream and downstream actors').

omega_variable(
    information_asymmetry_persistence,
    'Does liability doctrine actually reduce information asymmetry between upstream and downstream actors, or does it merely shift costs without improving knowledge sharing?',
    'Comparative analysis of supply chains with vs without upstream liability requirements; measurement of information flows (quality data, failure reports, design feedback); correlation between liability exposure and actual quality improvement',
    'If information flows improve: coordination function is real (supports Rope/Scaffold). If information remains asymmetric but costs shift: extraction dominates (supports Snare/Tangled_Rope with high chi).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(information_asymmetry_persistence, empirical, 'Whether liability shift reduces or merely redistributes information asymmetry').

omega_variable(
    market_exit_feasibility_threshold,
    'At what level of liability burden do upstream producers systematically exit markets or consolidate, shifting control further upstream?',
    'Time series analysis of market structure change in regulated vs unregulated supply chains; measurement of producer consolidation rates; exit rates correlated with liability doctrine changes',
    'If exit threshold is low (< 20% cost increase): upstream shift destabilizes markets (paradoxical effect — increases downstream risk by reducing supplier diversity). If threshold is high (> 50% cost increase): shift is sustainable coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_exit_feasibility_threshold, empirical, 'Market exit feasibility threshold for upstream producers under liability burden').

omega_variable(
    insurance_pooling_moral_hazard,
    'Does liability insurance for upstream producers enable moral hazard (reduced safety effort because costs are insured) or does it enable specialization in risk management?',
    'Comparison of safety metrics before/after liability insurance availability; cross-national analysis of countries with vs without liability insurance markets; measurement of quality investments correlated with insurance availability',
    'If moral hazard dominates: insurance converts liability into theater (piton dynamics). If moral hazard is controlled by insurance underwriting: liability mechanism is functional coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(insurance_pooling_moral_hazard, empirical, 'Moral hazard in liability insurance markets').

omega_variable(
    regulatory_substitution_feasibility,
    'Could direct safety regulation (design standards, manufacturing requirements, inspection regimes) replace liability-based incentives without losing efficiency?',
    'Comparative analysis of heavily regulated (FDA-medical, FAA-aerospace) vs liability-driven (consumer products, general manufacturing) sectors; measurement of safety outcomes and innovation rates; cost-benefit analysis of regulatory vs liability regimes',
    'If substitution is feasible: scaffold sunset is real (liability is temporary mechanism until regulation matures). If substitution loses important properties (information dispersal, adaptive incentives): liability doctrine is permanent fixture (rope) not temporary transition (scaffold).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_substitution_feasibility, conceptual, 'Feasibility of regulatory substitution for liability-based incentives').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(liability_doctrine_upstream_shift, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(liab_tr_t0, liability_doctrine_upstream_shift, theater_ratio, 0, 0.38).
narrative_ontology:measurement(liab_tr_t10, liability_doctrine_upstream_shift, theater_ratio, 10, 0.48).
narrative_ontology:measurement(liab_tr_t20, liability_doctrine_upstream_shift, theater_ratio, 20, 0.55).

% Extraction over time
narrative_ontology:measurement(liab_be_t0, liability_doctrine_upstream_shift, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(liab_be_t10, liability_doctrine_upstream_shift, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(liab_be_t20, liability_doctrine_upstream_shift, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(liability_doctrine_upstream_shift, enforcement_mechanism).
narrative_ontology:affects_constraint(liability_doctrine_upstream_shift, product_safety_regulation).
narrative_ontology:affects_constraint(liability_doctrine_upstream_shift, insurance_market_extraction).
narrative_ontology:affects_constraint(liability_doctrine_upstream_shift, supply_chain_consolidation_pressure).

% DUAL FORMULATION NOTE:
% Liability doctrine upstream shift has downstream effects on insurance market extraction (as liability mandates drive demand for liability insurance) and supply chain consolidation (as liability burden drives market exit for small upstream producers). These three constraints form a causal family where upstream shift is the initiating mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
