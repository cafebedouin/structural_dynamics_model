% ============================================================================
% CONSTRAINT STORY: sotu_1984_reagan_regulatory_reduction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1984_reagan_regulatory_reduction, []).

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
 *   constraint_id: sotu_1984_reagan_regulatory_reduction
 *   human_readable: Federal Regulatory Growth Reduction (25% Decrease)
 *   domain: regulatory/political_economy
 *
 * SUMMARY:
 *   The 1984 State of the Union deregulation framework represents a
 *   structural constraint that combines genuine coordination (elimination of
 *   redundant compliance paperwork) with asymmetric extraction (concentration
 *   of benefits in large industries, concentration of costs in oversight
 *   capacity and safety-dependent populations). The constraint operates at
 *   the intersection of ideology (market efficiency as natural law) and
 *   political economy (specific industries and constituencies benefit from
 *   reduced enforcement). The 25% regulatory reduction target eliminates 300+
 *   million hours of annual paperwork but simultaneously degrades inspection
 *   capacity across worker safety, environmental protection, and consumer
 *   product oversight. This creates a tangled distribution: small businesses
 *   experience both benefit (reduced compliance burden) and harm (reduced
 *   market integrity); front-line inspectors experience pure extraction
 *   (reduced resources, maintained responsibility); large incumbent
 *   industries experience net benefit (can absorb compliance costs while
 *   competitors cannot); organized consumer advocates experience a temporary
 *   constraint with built-in self-correction (market failures trigger
 *   regulatory rebuilding). The analytical observer risks seeing this as a
 *   natural correction toward economic efficiency, but structural analysis
 *   reveals that the 'natural law' of market efficiency is actually a policy
 *   choice that benefits specific constituencies while imposing costs on
 *   others.
 *
 * KEY AGENTS:
 *   - Private Sector Enterprises: Primary beneficiary (institutional/arbitrage) — experience compliance cost reduction and market entry barriers to environmental/safety oversight
 *   - Front-Line Inspectors: Primary victim (powerless/trapped) — face resource erosion and workload intensification with no exit option
 *   - Regulatory Enforcement Capacity: Primary victim (powerless/trapped) — abstract institutional function that cannot exit or organize
 *   - Small Business Owners: Secondary victim and partial beneficiary (moderate/constrained) — benefit from compliance cost reduction but suffer from reduced market integrity and increased race-to-bottom pressure
 *   - Large Incumbent Industries: Primary beneficiary (institutional/arbitrage) — leverage existing compliance infrastructure to benefit from enforcement reduction
 *   - Consumer Safety Monitoring: Victim (powerless/trapped) — inspection coverage declines, quality assurance declines, recall detection slows
 *   - Organized Consumer/Environmental Coalition: Organized actor (organized/mobile) — sees deregulation as temporary constraint with market-failure-driven sunset
 *   - Federal Regulatory Apparatus: Institutional actor (institutional/arbitrage) — maintains performance theater while enforcement capacity degrades
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1984_reagan_regulatory_reduction, 0.52).
domain_priors:suppression_score(sotu_1984_reagan_regulatory_reduction, 0.48).
domain_priors:theater_ratio(sotu_1984_reagan_regulatory_reduction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1984_reagan_regulatory_reduction, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1984_reagan_regulatory_reduction, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1984_reagan_regulatory_reduction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1984_reagan_regulatory_reduction, tangled_rope).
narrative_ontology:human_readable(sotu_1984_reagan_regulatory_reduction, "Federal Regulatory Growth Reduction (25% Decrease)").
narrative_ontology:topic_domain(sotu_1984_reagan_regulatory_reduction, "regulatory/political_economy").

domain_priors:requires_active_enforcement(sotu_1984_reagan_regulatory_reduction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1984_reagan_regulatory_reduction, private_sector_enterprises).
narrative_ontology:constraint_beneficiary(sotu_1984_reagan_regulatory_reduction, compliance_cost_avoiders).
narrative_ontology:constraint_beneficiary(sotu_1984_reagan_regulatory_reduction, deregulated_industries).
narrative_ontology:constraint_victim(sotu_1984_reagan_regulatory_reduction, regulatory_enforcement_capacity).
narrative_ontology:constraint_victim(sotu_1984_reagan_regulatory_reduction, consumer_safety_monitoring).
narrative_ontology:constraint_victim(sotu_1984_reagan_regulatory_reduction, environmental_oversight).
narrative_ontology:constraint_victim(sotu_1984_reagan_regulatory_reduction, worker_protections).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FRONT-LINE INSPECTOR (SNARE) — Federal inspectors, safety auditors, and compliance monitors face resource erosion with no exit. Deregulation targets paperwork reduction but simultaneously reduces inspection capacity. The inspector is trapped: workload intensifies while funding shrinks. Maximum extraction with zero agency — cannot quit without abandoning responsibility, cannot refuse assignments, cannot reduce quality without risking public harm.
constraint_indexing:constraint_classification(sotu_1984_reagan_regulatory_reduction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALL BUSINESS OWNER (TANGLED ROPE) — Small firms benefit from reduced compliance costs but also suffer from reduced market integrity. A small manufacturer benefits from eliminating environmental paperwork but loses protection from cheaper competitors cutting corners. Net extraction is asymmetric but moderate — there is genuine coordination (shared compliance burden reduction) alongside extraction (increased market risk). Can exit by relocating to regulated jurisdiction or specializing in compliance-premium markets, but at significant cost.
constraint_indexing:constraint_classification(sotu_1984_reagan_regulatory_reduction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: LARGE REGULATED INDUSTRY (ROPE) — Incumbent firms with sophisticated compliance infrastructure benefit from deregulation reducing paperwork while their capital-intensive inspection avoidance systems remain in place. The constraint solves coordination: all firms face same compliance reduction. Net beneficiary with arbitrage options (can lobby for further reduction, shift to state-level regulation, or vertically integrate compliance). Effective extraction runs toward this actor.
constraint_indexing:constraint_classification(sotu_1984_reagan_regulatory_reduction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSUMER ADVOCACY COALITION (SCAFFOLD) — Organized consumer and environmental advocates see deregulation as a temporary constraint with built-in self-correction: market failures (product recalls, environmental crises, worker injuries) are predicted to trigger regulatory rebuilding. The constraint has a sunset — reduced oversight periods generate crises that restore regulatory demand. Low effective extraction because organized actors have exit options (lobbying, state-level alternatives, private certification schemes) and see a pathway back through demonstrated failure.
constraint_indexing:constraint_classification(sotu_1984_reagan_regulatory_reduction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: REGULATORY APPARATUS (PITON) — Federal regulatory agencies continue performing inspection and standard-setting rituals despite capacity erosion, maintaining the appearance of oversight while actual enforcement coverage declines. The constraint persists through institutional inertia: agencies maintain theater (publishing regulations, conducting token audits, issuing compliance guidance) while real enforcement capacity degrades. The regulatory apparatus recognizes its own degradation but maintains it because the alternative (admitting coverage gaps) is institutionally unacceptable.
constraint_indexing:constraint_classification(sotu_1984_reagan_regulatory_reduction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / MARKET EFFICIENCY VIEW (MOUNTAIN) — From a civilizational perspective, reducing regulatory burden to restore market efficiency is viewed as responding to an immutable economic law: administrative overhead reduces innovation and competitiveness. This perspective sees deregulation as natural correction toward efficient equilibrium. However, the structural data reveals false-summit dynamics: specific beneficiary industries and victim constituencies exist, enforcement capacity is not a natural limit but a policy choice, and the 'market law' naturalizes distributional choices.
constraint_indexing:constraint_classification(sotu_1984_reagan_regulatory_reduction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1984_reagan_regulatory_reduction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1984_reagan_regulatory_reduction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1984_reagan_regulatory_reduction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1984_reagan_regulatory_reduction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1984_reagan_regulatory_reduction, TR),
    TR >= 0.70.

:- end_tests(sotu_1984_reagan_regulatory_reduction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, increasing over the measurement interval. The deregulation framework creates genuine coordination benefit (elimination of redundant paperwork and administrative overhead) but simultaneously extracts from enforcement capacity and safety-monitoring populations. The trajectory shows extractiveness rising from 0.35 (initial optimism about paperwork reduction) to 0.58 (realized enforcement gaps). The increase reflects that the nominal 25% reduction in regulations creates disproportionate reduction in actual oversight capacity due to specialized inspectorate requirements. Suppression (0.48): Moderate. Barriers to contesting deregulation include political ideology (market efficiency framing), concentrated beneficiary lobbying, and diffuse victim constituencies (workers, consumers, environmental monitors) that lack organizational capacity. But suppression is not total — organized consumer and environmental constituencies can mobilize, and safety crises trigger rebuilding pressure. Theater ratio (0.58): Moderate-high and increasing. The regulatory apparatus maintains ceremonial compliance guidance, published standards, and token audits while actual inspection coverage declines. Theater increases because agencies must perform legitimacy maintenance despite capacity reduction — the piton effect.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates stark perspectival divergence: the large incumbent industry (institutional/arbitrage) sees pure Rope — solving the coordination problem of redundant compliance overhead. The front-line inspector (powerless/trapped) sees pure Snare — resource extraction with zero agency. The small business (moderate/constrained) sees Tangled Rope — genuine benefit alongside genuine cost. The consumer coalition (organized/mobile) sees Scaffold — temporary constraint with market-failure-driven sunset. The regulatory apparatus (institutional/arbitrage) sees Piton — maintaining performance theater while capacity degrades. The analytical observer risks Mountain — seeing market efficiency as natural law. The perspectival gaps reveal the constraint's structure: it is genuinely coordinating (paperwork reduction solves real redundancy) AND genuinely extractive (enforcement capacity degrades, costs concentrate on safety-dependent populations, benefits concentrate in large industries).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective derives directionality from beneficiary/victim status and exit options. Private sector enterprises benefit from compliance reduction with high arbitrage options (can lobby for further reduction, shift to state regulation, or vertically integrate safety functions) — low d value, experienced as Rope. Front-line inspectors are trapped victims with no exit (cannot refuse inspection assignments, cannot exit profession without career loss) — high d value, experienced as Snare. Small businesses face mixed benefit (compliance reduction) and cost (reduced market integrity, race-to-bottom pressure) with constrained but present exit options (relocate to regulated jurisdictions, specialize in compliance-premium markets) — moderate d value, experienced as Tangled Rope. Organized consumer advocates have mobile exit options (state-level alternatives, private certification, lobbying for rebuilding) and see a sunset mechanism — low-to-moderate d value, experienced as Scaffold. The regulatory apparatus maintains theater while degrading function, experiencing the constraint as inertial (Piton). The analytical observer from civilizational perspective risks seeing natural market law (Mountain), but the false-summit detector identifies this as naturalization of political choice.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint is structurally Tangled Rope: it serves a genuine coordination function (elimination of redundant compliance documentation) AND operates as asymmetric extraction (concentration of benefits and reduction of costs in large industries, concentration of costs in front-line enforcement capacity and diffuse safety constituencies). The constraint is not 'really' a Rope or 'really' a Snare — it is genuinely both, from different structural positions. The false summit (Mountain/analytical view) naturalizes the distributional choice by framing compliance reduction as response to immutable market efficiency. The true structure is institutional: the deregulation framework chooses to extract from enforcement capacity and disperses coordination benefits to beneficiaries while concentrating compliance-cost benefits. The scaffold perspective (consumer coalition) suggests the constraint has a sunset mechanism (market failures trigger regulatory rebuilding), but the omegas reveal that this sunset depends on empirical triggers (safety crisis detection) and political will (regulatory rebuild speed) that are themselves contested.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    safety_margin_threshold,
    'What reduction in inspection frequency crosses the threshold from acceptable risk tolerance to unacceptable safety compromise?',
    'Time-series analysis of inspection-to-incident ratios; correlation of inspection reduction with workplace injury rates, product recall rates, environmental violation discovery rates across industries',
    'If threshold low (few inspections skipped): deregulation is pure coordination benefit with minimal safety extraction. If threshold high (significant skips tolerable): current deregulation extracts from safety-dependent populations without crossing safety lines.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(safety_margin_threshold, empirical, 'Threshold for inspection frequency below which safety outcomes degrade').

omega_variable(
    compliance_capacity_measurement,
    'How much of the 300+ million hour paperwork reduction reflects genuine administrative overhead vs. legitimate safety/environmental/worker documentation?',
    'Audit of eliminated paperwork: categorize by function (genuine admin vs. required documentation); track post-elimination whether safety/environmental/worker protection functions require paper-equivalent replacements',
    'If 70%+ is genuine overhead: deregulation is nearly pure coordination gain. If 50%+ is legitimate documentation: deregulation extracts from documentation-dependent safety functions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(compliance_capacity_measurement, empirical, 'Proportion of paperwork reduction that is genuine overhead vs. necessary documentation').

omega_variable(
    market_power_concentration,
    'Does deregulation benefit distributed small-business competition or concentrate market power in firms that can self-certify?',
    'Market concentration indices pre/post deregulation; entry rates for new competitors; correlation between firm size and compliance cost savings',
    'If distributed: tangled rope classification correct — genuine coordination with moderate extraction. If concentrated: snare classification more accurate — large beneficiaries, distributed cost-bearing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_power_concentration, empirical, 'Whether deregulation benefits distributed competition or concentrates market power').

omega_variable(
    regulatory_rebuild_trigger,
    'What level of safety/environmental failure triggers regulatory rebuilding, and can regulatory rebuilding reach parity with pre-deregulation capacity?',
    'Historical analysis of regulatory cycles; measurement of post-crisis rebuilding speed and final capacity levels relative to pre-reduction baseline',
    'If triggers low and rebuild is fast: scaffold sunset is real and extraction period is bounded. If triggers high and rebuild is slow: sunset is aspirational rather than structural, and interim extraction is permanent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_rebuild_trigger, empirical, 'Whether safety crises trigger regulatory rebuilding and at what pace').

omega_variable(
    paperwork_as_coordination_mechanism,
    'Does eliminated paperwork represent genuine redundancy or essential inter-agency coordination that will be replaced by informal/undocumented processes?',
    'Tracing of information flow pre/post elimination; measurement of regulatory coordination effectiveness; emergence of informal or private certification schemes',
    'If paperwork is redundant: extraction is minimal. If paperwork serves real coordination: its elimination fragments regulatory oversight and creates coordination black holes, increasing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(paperwork_as_coordination_mechanism, empirical, 'Whether eliminated paperwork served coordination function or was genuine redundancy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1984_reagan_regulatory_reduction, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(regred_tr_t0, sotu_1984_reagan_regulatory_reduction, theater_ratio, 0, 0.42).
narrative_ontology:measurement(regred_tr_t2, sotu_1984_reagan_regulatory_reduction, theater_ratio, 2, 0.48).
narrative_ontology:measurement(regred_tr_t5, sotu_1984_reagan_regulatory_reduction, theater_ratio, 5, 0.58).
narrative_ontology:measurement(regred_tr_t8, sotu_1984_reagan_regulatory_reduction, theater_ratio, 8, 0.65).

% Extraction over time
narrative_ontology:measurement(regred_be_t0, sotu_1984_reagan_regulatory_reduction, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(regred_be_t2, sotu_1984_reagan_regulatory_reduction, base_extractiveness, 2, 0.44).
narrative_ontology:measurement(regred_be_t5, sotu_1984_reagan_regulatory_reduction, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(regred_be_t8, sotu_1984_reagan_regulatory_reduction, base_extractiveness, 8, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1984_reagan_regulatory_reduction, resource_allocation).
narrative_ontology:affects_constraint(sotu_1984_reagan_regulatory_reduction, regulatory_arbitrage_state_federal).
narrative_ontology:affects_constraint(sotu_1984_reagan_regulatory_reduction, workplace_safety_inspection_capacity).
narrative_ontology:affects_constraint(sotu_1984_reagan_regulatory_reduction, environmental_monitoring_enforcement).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1984_reagan_regulatory_reduction, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
