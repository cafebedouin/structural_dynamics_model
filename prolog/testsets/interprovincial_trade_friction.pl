% ============================================================================
% CONSTRAINT STORY: interprovincial_trade_friction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_interprovincial_trade_friction, []).

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
 *   constraint_id: interprovincial_trade_friction
 *   human_readable: Interprovincial Trade Friction and Non-Tariff Barriers
 *   domain: economic/political/regulatory
 *
 * SUMMARY:
 *   Interprovincial trade friction in federalist systems (Canada, Australia,
 *   USA) emerges from the structural tension between provincial
 *   constitutional authority over local commerce and the federal mandate for
 *   internal market integration. Provinces deploy non-tariff barriers —
 *   certification requirements, origin labeling, health/safety standards,
 *   procurement preferences — ostensibly to protect consumers and local
 *   environments, but functionally to protect incumbent producers from
 *   out-of-province competition. The constraint exhibits genuine coordination
 *   elements (safety standards, environmental protection) layered with
 *   extraction elements (protectionist intent, competitive barrier). It is
 *   not pure protectionism masquerading as safety regulation, nor pure safety
 *   regulation that happens to have protectionist effects. Both functions are
 *   active and intentional. The extractiveness has risen from 0.38 to 0.52
 *   over two decades as provinces have become more sophisticated in barrier
 *   design — moving from explicit tariffs (which are prohibited) to
 *   regulatory complexity that achieves the same effect through higher
 *   compliance costs. Theater ratio has risen from 0.42 to 0.55 as
 *   dispute-resolution mechanisms (trade commissions, inter-governmental
 *   negotiation) have formalized without reducing barriers, creating the
 *   appearance of accountability without the substance.
 *
 * KEY AGENTS:
 *   - Out-of-Province Producers: Structurally trapped victims (powerless/trapped) — face cumulative regulatory barriers that make market entry prohibitively expensive; no exit option; bear full extraction cost through market exclusion
 *   - Interprovincial Consumers: Moderate agents (moderate/constrained) — benefit from genuine safety/quality standards but pay higher prices and face reduced choice; exit possible but costly (move provinces, substitute goods)
 *   - Incumbent Provincial Producers: Primary beneficiaries (institutional/arbitrage) — experience constraints as legitimate coordination necessary for local market function; net gain through reduced competition
 *   - Provincial Regulatory Agencies: Organized institutional actors (organized/constrained) — maintain genuine safety/environmental functions while executing protectionist intent through regulatory design; have both coordination and extraction authority
 *   - Interprovincial Trade Commission: Institutional arbiter (institutional/arbitrage) — maintains formal dispute-resolution role that has become increasingly performative; rulings issued but barriers persist through regulatory workarounds
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing federalism's regulatory fragmentation as immutable constitutional feature, obscuring deliberate protectionist design
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(interprovincial_trade_friction, 0.52).
domain_priors:suppression_score(interprovincial_trade_friction, 0.48).
domain_priors:theater_ratio(interprovincial_trade_friction, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(interprovincial_trade_friction, extractiveness, 0.52).
narrative_ontology:constraint_metric(interprovincial_trade_friction, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(interprovincial_trade_friction, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(interprovincial_trade_friction, tangled_rope).
narrative_ontology:human_readable(interprovincial_trade_friction, "Interprovincial Trade Friction and Non-Tariff Barriers").
narrative_ontology:topic_domain(interprovincial_trade_friction, "economic/political/regulatory").

domain_priors:requires_active_enforcement(interprovincial_trade_friction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(interprovincial_trade_friction, incumbent_provincial_producers).
narrative_ontology:constraint_beneficiary(interprovincial_trade_friction, provincial_regulatory_agencies).
narrative_ontology:constraint_victim(interprovincial_trade_friction, interprovincial_commerce).
narrative_ontology:constraint_victim(interprovincial_trade_friction, consumer_access).
narrative_ontology:constraint_victim(interprovincial_trade_friction, competing_provincial_producers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OUT-OF-PROVINCE PRODUCER (SNARE) — Faces cumulative regulatory and certification barriers that make market entry prohibitively expensive. Cannot exit or arbitrage; must accept local producer protection as immutable constraint. Bears full extraction cost through market exclusion.
constraint_indexing:constraint_classification(interprovincial_trade_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: INTERPROVINCIAL CONSUMER (TANGLED ROPE) — Experiences both coordination and extraction. Coordination: provincial regulations establish baseline safety/quality standards that benefit all consumers. Extraction: non-tariff barriers limit choice and raise prices relative to single-market alternatives. Exit is costly but possible (move provinces, substitute goods).
constraint_indexing:constraint_classification(interprovincial_trade_friction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT PROVINCIAL PRODUCER (ROPE) — Primary beneficiary. Experiences the constraint as coordination: fragmented markets require local adaptation, supply-chain development, and regulatory alignment — genuine coordination value. Net benefits through reduced competition outweigh coordination costs.
constraint_indexing:constraint_classification(interprovincial_trade_friction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: PROVINCIAL REGULATORY AGENCY (TANGLED ROPE) — Organized institutional actor. Genuine coordination function: protecting public health, ensuring product safety, maintaining local environmental standards. Extraction function: protecting constituent producers from interprovincial competition through certification delays, proprietary testing requirements, origin labeling mandates. Both functions are real and active.
constraint_indexing:constraint_classification(interprovincial_trade_friction, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INTERPROVINCIAL TRADE COMMISSION (PITON) — Formal dispute-resolution body for trade friction exists with established protocols and arbitration rules. Theater ratio high: commission reviews complaints, issues rulings, provinces nominally comply, barriers persist. Functional authority has degraded — provinces find workarounds (tighter local standards, extended certification periods) that technically comply with rulings while maintaining barriers. Theater_ratio reflects ritualized compliance.
constraint_indexing:constraint_classification(interprovincial_trade_friction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW FRAME (MOUNTAIN) — From a civilizational perspective, some regulatory fragmentation is inherent to federal systems: provinces have constitutional authority over trade within their borders, and safety/environmental standards legitimately vary by local conditions. This perspective naturalizes trade friction as an unavoidable feature of federalism. However, the structural data reveals this as a false summit — the extraction mechanisms (strategic regulatory tightening, certification delays) are contingent institutional choices, not constitutional necessities.
constraint_indexing:constraint_classification(interprovincial_trade_friction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(interprovincial_trade_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(interprovincial_trade_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(interprovincial_trade_friction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(interprovincial_trade_friction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(interprovincial_trade_friction, TR),
    TR >= 0.70.

:- end_tests(interprovincial_trade_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high, with upward trend. Base value 0.38 at t=0 reflects that barriers existed but were less sophisticated (explicit tariffs, transparent quotas). Modern barriers are predominantly regulatory complexity: multi-stage certification for out-of-province products, origin-of-materials verification, duplication of testing already completed in origin province. Extractiveness rises as provinces become more sophisticated in concealing protectionism behind legitimate regulatory intent. Trend from 0.38 to 0.52 shows accumulation of barriers over time, reflecting both growing protectionist intent and institutional learning about regulatory design. Suppression (0.48): Moderate. Out-of-province producers face high barriers but not absolute prohibition — some cross-border trade occurs, though constrained. Exit options exist but are costly (relocating to target province, abandoning product line, paying for duplicate certifications). Suppression reflects the cost structure of the barriers rather than their absoluteness. Theater ratio (0.55): Moderate-high. Interprovincial Trade Commission exists, holds hearings, issues rulings that provinces ostensibly comply with. But compliance is nominal — provinces find regulatory workarounds that technically comply while maintaining barriers. Theater has risen as formal dispute mechanisms have proliferated without reducing actual barriers, creating an appearance of accountability. The theater is not performative ritual with zero function (piton-level) but rather formalized process that generates compliance theater while permitting continued extraction.
 *
 * PERSPECTIVAL GAP:
 *   The fundamental gap is between incumbent producers' rope perception and out-of-province producers' snare perception. Both are structurally justified: the incumbent genuinely experiences coordination value from regulatory fragmentation, while the competitor genuinely experiences systematic extraction. The gap reveals that the constraint's function depends entirely on observer position. The regulatory agency's tangled_rope classification is the institution's native view — they execute both coordination (safety) and extraction (protection) simultaneously and intentionally. The trade commission's piton classification reveals the institutional decay: dispute mechanisms have become performative theater while barriers persist. The mountain classification from the analytical observer is a false summit: federalism's structural decentralization is claimed as natural law, concealing the active protectionist regulatory design that accumulates over time.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position relative to extraction flow. Out-of-province producers are victims with trapped exit (d ≈ 0.95) — maximum extraction. Incumbent producers are beneficiaries with arbitrage exit (d ≈ 0.15) — negative extraction. Regulatory agencies occupy the complex middle: they are both beneficiaries (protected constituency) and constraint-enforcing institutions (d ≈ 0.40-0.50). Interprovincial consumers are trapped between beneficiary position (safety standards) and victim position (reduced choice/higher prices), creating d ≈ 0.50 (symmetric). The analytical observer at civilizational scope occupies the canonical d ≈ 0.72 for analytical position, derived from the false summit tendency (inability to see that naturalized federalism conceals active protectionist choices).
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through the institutional decomposition: the constraint contains genuine coordination (safety, environmental standards) plus genuine extraction (protectionist barriers). It is not 'is this a coordination mechanism?' but 'at what cost does the coordination function operate, and who bears that cost?' The incumbent producer's rope perspective reflects the coordination value they genuinely experience. The competitor's snare perspective reflects the extraction they genuinely experience. The regulatory agency's tangled_rope perspective reflects the dual function they intentionally execute. The consumer's tangled_rope perspective reflects the genuine safety benefit plus genuine cost (reduced choice/higher prices). The trade commission's piton perspective reveals institutional decay — the dispute mechanism has become theater as provinces learn to maintain barriers while technically complying with rulings. The analytical observer's mountain is a false summit: naturalizing federalism as constitutional necessity obscures the active protectionist regulatory design that accumulates barriers over time. Mandatrophy is resolved: this is genuinely a tangled rope with strong piton elements and a false summit naturalizing it as constitutional inevitability.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    regulatory_legitimacy_vs_protectionism,
    'What proportion of provincial non-tariff barriers reflects genuine regulatory diversity vs deliberate protectionist design?',
    'Comparative analysis: regulatory stringency in provinces with competitive local producers vs provinces without; correlation between regulatory tightness and import volume; testimony from regulatory agencies about barrier intent',
    'If majority is genuine diversity: constraint moves toward Rope (coordination). If majority is strategic: constraint remains Tangled Rope or Snare depending on implementation cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_legitimacy_vs_protectionism, empirical, 'Proportion of regulatory barriers that are protectionist vs legitimately diverse').

omega_variable(
    certification_cost_structure,
    'Are certification and testing cost barriers imposed equally on interprovincial and interprovincial producers, or are out-of-province producers systematically charged higher rates or required to repeat already-completed tests?',
    'Cost audit: compare certification fees and duplication burden for producers at different interstate positions; interview regulatory agencies about cost allocation methodology',
    'If equal treatment: barriers are legitimate coordination costs. If discriminatory: barriers constitute direct extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(certification_cost_structure, empirical, 'Whether certification costs are applied equally or discriminatorily').

omega_variable(
    constitutional_trade_mandate_enforceability,
    'Does the constitutional framework (e.g., Canadian Charter of Rights and Freedoms Section 6, mobility of goods clauses) create an enforceable ceiling on provincial trade restrictions, or is enforcement so weak that the ceiling functions only symbolically?',
    'Legal analysis of rulings and compliance rates; comparison of trade commission rulings vs actual barrier persistence; estimation of enforcement cost relative to barrier benefit',
    'If enforceable: mountain classification is false (constitutional mandate overrides local interest). If unenforced: provinces face no meaningful constraint (snare persists). If partially enforced: tangled rope holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(constitutional_trade_mandate_enforceability, empirical, 'Enforceability of constitutional trade mandates').

omega_variable(
    consumer_awareness_feedback,
    'Do consumers recognize interprovincial trade barriers as deliberate protectionism, or do they interpret price and choice differences as natural market variation or quality differences?',
    'Consumer survey and focus groups in provinces with significant trade barriers; measurement of brand recognition and price sensitivity for interprovincial vs domestic alternatives; analysis of consumer complaint patterns',
    'If recognized: political pressure for barrier reduction likely. If unrecognized: constraint persists through invisibility (theater mask sustains extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_awareness_feedback, empirical, 'Whether consumers perceive trade barriers as deliberate protectionism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(interprovincial_trade_friction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iptf_tr_t0, interprovincial_trade_friction, theater_ratio, 0, 0.42).
narrative_ontology:measurement(iptf_tr_t10, interprovincial_trade_friction, theater_ratio, 10, 0.48).
narrative_ontology:measurement(iptf_tr_t20, interprovincial_trade_friction, theater_ratio, 20, 0.55).
narrative_ontology:measurement(iptf_tr_t5, interprovincial_trade_friction, theater_ratio, 5, 0.45).

% Extraction over time
narrative_ontology:measurement(iptf_be_t0, interprovincial_trade_friction, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(iptf_be_t10, interprovincial_trade_friction, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(iptf_be_t20, interprovincial_trade_friction, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(iptf_be_t5, interprovincial_trade_friction, base_extractiveness, 5, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(interprovincial_trade_friction, resource_allocation).
narrative_ontology:affects_constraint(interprovincial_trade_friction, regulatory_capture_provincial_agencies).
narrative_ontology:affects_constraint(interprovincial_trade_friction, supply_chain_fragmentation_domestic).

% DUAL FORMULATION NOTE:
% Interprovincial trade friction is distinct from pure regulatory capture (which would focus on agency-industry alignment) and supply-chain fragmentation (which would focus on logistics complexity). This story focuses on the structural tension between constitutional decentralization and market integration, where regulatory authority is used to maintain market fragmentation. Upstream stories would address specific barrier mechanisms (certification requirements, labeling mandates); downstream stories would address economic consequences (price differentials, innovation drag).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(interprovincial_trade_friction, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
