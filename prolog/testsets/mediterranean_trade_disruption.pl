% ============================================================================
% CONSTRAINT STORY: mediterranean_trade_disruption
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mediterranean_trade_disruption, []).

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
 *   constraint_id: mediterranean_trade_disruption
 *   human_readable: Mediterranean Trade Disruption and Geopolitical Extraction
 *   domain: economic_policy/geopolitics/maritime_trade
 *
 * SUMMARY:
 *   Mediterranean trade disruption creates a structural constraint where
 *   geography (the region's bottleneck role in global trade) combines with
 *   political-military enforcement to extract value from traders dependent on
 *   the route. The constraint exhibits characteristics of Tangled Rope
 *   because it simultaneously solves a coordination problem (rerouting global
 *   supply chains, establishing safe passage protocols) while extracting rent
 *   from those who cannot exit (small traders, importers of critical goods).
 *   The constraint's extractiveness has increased from 0.35 (low enforcement)
 *   to 0.58 (high enforcement and cost multiplication) over a four-year
 *   interval, indicating that enforcement intensity is rising while theater
 *   (performative aspects like official risk assessments, humanitarian
 *   corridors) remains moderate. This gap suggests the underlying mechanism
 *   is increasingly coercive rather than coordinative, though the veneer of
 *   coordination (negotiated passage, insurance, rerouting agreements)
 *   persists.
 *
 * KEY AGENTS:
 *   - Small Mediterranean Traders: Primary victims (powerless/trapped) — lack capital for rerouting, face 300-500% insurance premiums, cannot exit except by abandoning Mediterranean trade
 *   - Global Supply Chains: Secondary victims (moderate/constrained) — bear deadweight loss from rerouting, inventory buildup, demand destruction; some exit capacity through modal shift (rail, pipeline) but at high cost
 *   - Hegemonic Maritime Powers: Primary beneficiaries (institutional/arbitrage) — enforce chokepoint control, collect transit fees, benefit from reduced competition; experience constraint as coordination mechanism
 *   - Insurance and Risk Intermediaries: Secondary beneficiaries (institutional/arbitrage) — profit from elevated premiums (3-5x baseline), provide genuine risk pooling service while extracting rents
 *   - Alternative Route Coalition: Organized agents (organized/mobile) — port authorities, logistics operators, rail companies; have capacity to build alternatives but face coordination barriers
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks treating geographic constraint as immutable natural law, missing the political-military enforcement component
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mediterranean_trade_disruption, 0.58).
domain_priors:suppression_score(mediterranean_trade_disruption, 0.68).
domain_priors:theater_ratio(mediterranean_trade_disruption, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mediterranean_trade_disruption, extractiveness, 0.58).
narrative_ontology:constraint_metric(mediterranean_trade_disruption, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(mediterranean_trade_disruption, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mediterranean_trade_disruption, tangled_rope).
narrative_ontology:human_readable(mediterranean_trade_disruption, "Mediterranean Trade Disruption and Geopolitical Extraction").
narrative_ontology:topic_domain(mediterranean_trade_disruption, "economic_policy/geopolitics/maritime_trade").

domain_priors:requires_active_enforcement(mediterranean_trade_disruption).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mediterranean_trade_disruption, hegemonic_maritime_powers).
narrative_ontology:constraint_beneficiary(mediterranean_trade_disruption, land_trade_operators).
narrative_ontology:constraint_beneficiary(mediterranean_trade_disruption, insurance_intermediaries).
narrative_ontology:constraint_victim(mediterranean_trade_disruption, small_mediterranean_traders).
narrative_ontology:constraint_victim(mediterranean_trade_disruption, global_supply_chains).
narrative_ontology:constraint_victim(mediterranean_trade_disruption, energy_importers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL MEDITERRANEAN TRADER (SNARE) — Trapped by geographic dependence on the route. Cannot reroute cargo through land infrastructure (does not exist at scale). Faces extreme insurance costs (300-500% premium), piracy/military risk, and no viable exit except abandoning Mediterranean trade entirely. Bears maximum extraction with zero alternatives.
constraint_indexing:constraint_classification(mediterranean_trade_disruption, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: GLOBAL SUPPLY CHAIN (TANGLED ROPE) — Moderate power with constrained exit. The disruption creates genuine coordination problem (rerouting logistics, inventory management, demand smoothing) that requires active enforcement of new protocols. Simultaneously extracts from supply chains through forced inefficiency, cost multiplication, and deadweight loss. Both coordination and extraction are structural to the constraint.
constraint_indexing:constraint_classification(mediterranean_trade_disruption, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: HEGEMONIC MARITIME POWER (ROPE) — Institutional actor with arbitrage options. Experiences the disruption as coordination mechanism: enforces chokepoint control, collects transit fees/tribute, redirects traffic through allied ports. Benefits from disruption while coordinating information flow about safe passage. Net beneficiary with genuine coordination function.
constraint_indexing:constraint_classification(mediterranean_trade_disruption, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INSURANCE INTERMEDIARIES (ROPE) — Institutional actors with arbitrage options. Benefit from elevated risk premiums (3-5x normal rates), while providing genuine coordination service (risk pooling, claims processing). The disruption increases their revenue and their functional value simultaneously. Pure beneficiary-coordination relationship.
constraint_indexing:constraint_classification(mediterranean_trade_disruption, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: ALTERNATIVE ROUTE COALITION (TANGLED ROPE) — Organized agents (port authorities, logistics firms, rail operators) with mobile exit options mobilizing alternative infrastructure (Suez alternatives, rail corridors, land routes). The constraint creates genuine coordination problem (multi-modal logistics, customs harmonization, route pricing). Simultaneously, the coalition benefits from disruption-driven demand for alternatives. Extraction and coordination both present; coalition has agency to exit the primary constraint by building alternatives.
constraint_indexing:constraint_classification(mediterranean_trade_disruption, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From civilizational scale, geographic bottlenecks are immutable: the Mediterranean is a fixed chokepoint with no alternative deep-water routes for bulk trade. Any actor controlling the region can extract by virtue of geography alone. This perspective risks naturalizing what is actually a political-military constraint. The mountain classification will be flagged as a false summit by the engine — geography creates vulnerability, but extraction requires enforcement.
constraint_indexing:constraint_classification(mediterranean_trade_disruption, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mediterranean_trade_disruption_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mediterranean_trade_disruption, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mediterranean_trade_disruption, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mediterranean_trade_disruption, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mediterranean_trade_disruption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint extracts through multiple mechanisms: elevated insurance costs (300-500% premium multiplier), forced rerouting (added logistics cost, time delays), loss of volume (demand destruction), and tribute/fee collection. However, extractiveness is not maximal (0.66+) because (a) some legitimate coordination costs exist (supply chain rebalancing is genuinely difficult), (b) alternatives are technically feasible (not impossible, only expensive), and (c) the constraint is enforced but not total (some trade continues, not all is blocked). The trajectory from 0.35 to 0.58 indicates increasing enforcement intensity — early phase had modest impact, recent phase shows rising coercion. Suppression (0.68): High. Traders face multiple barriers to exit: geographic dependence, capital constraints preventing large-scale rerouting, asymmetric enforcement (military vs commercial actors), and information asymmetry (enforcement rules can change without notice). Suppression is not total (0.90+) because maritime alternatives technically exist and some traders have successfully rerouted. Theater ratio (0.55): Moderate. The constraint includes performative elements (humanitarian corridors, official passage notifications, insurance frameworks, customs protocols) but these are not the primary enforcement mechanism. Unlike pure Pitons (theater >> 0.70), this constraint's theater is close to 50/50 with genuine coercion, indicating the veneer of coordination is still functional but increasingly strained.
 *
 * PERSPECTIVAL GAP:
 *   The gap between beneficiaries and victims is stark. Maritime powers and insurance intermediaries see Rope (coordination mechanism generating mutual benefit) or even higher-order benefit (revenue generation with minimal cost). Small traders see Snare (extraction with no alternative). Global supply chains see Tangled Rope (both coordination problem and extraction). The analytical observer who naturalizes geography sees Mountain (immutable constraint), while the enforcement-aware observer sees Snare (political-military imposition). This perspectival range (from Mountain to Snare) is characteristic of constraints where enforcement legitimacy is contested. Beneficiaries frame the constraint as natural law or necessary coordination; victims frame it as political extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) derives from the agent's structural position relative to extraction flow. Small traders are full targets (d ≈ 0.95): they bear costs, have no exit, trapped by geography and capital. Global supply chains are partial targets (d ≈ 0.70): they bear costs but have some exit capacity through alternatives (higher cost but possible). Maritime powers are beneficiaries (d ≈ 0.05-0.15): they collect rents and have arbitrage options (enforce or reduce enforcement). Insurance intermediaries are beneficiaries (d ≈ 0.10): they profit from elevated risk premiums. Alternative coalition has mixed positioning (d ≈ 0.55): they bear some costs (have to compete for traffic) but benefit from new demand for alternatives. The analytical observer at civilizational scale (d ≈ 0.72): attempts to observe the constraint from outside but is positioned ambiguously relative to it — neither full target nor full beneficiary, but analytically vulnerable to naturalizing enforcement as immutable geography.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PRESENT: The constraint is classified as Tangled Rope (not Snare) because beneficiaries legitimately coordinate traffic flow and risk management, not purely extract. However, the extractiveness value (0.58) is elevated because the coordination benefit is asymmetrically distributed — most benefit flows to beneficiaries, most cost flows to victims. The mandatrophy resolution requires showing that (a) genuine coordination function exists (rerouting, passage protocols, risk pooling), (b) asymmetric extraction also exists (rent collection, forced premium payment), (c) the ratio between them justifies Tangled Rope rather than Snare. The trajectory from 0.35 to 0.58 suggests increasing risk that the constraint tips toward pure Snare — if extractiveness continues to rise and coordination function weakens, the classification would shift. Monitoring theater ratio (currently 0.55) is key: if theater drops below 0.40, the coordinative veneer is dissolving and reclassification toward Snare becomes warranted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    enforcement_mechanism_sustainability,
    'Is the disruption maintained by active military/political enforcement or by passive geographic constraint?',
    'Scenario analysis: removal of enforcement actors while geography remains constant. If trade resumes, enforcement was primary; if disruption persists, geography was primary.',
    'If enforcement: classification tilts toward Snare (power-driven extraction). If geography: classification tilts toward Mountain (immutable). Current evidence suggests 70% enforcement / 30% geography.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_mechanism_sustainability, empirical, 'Whether disruption is enforcement-driven or geographically immutable').

omega_variable(
    alternative_route_viability_threshold,
    'At what cost multiplier do alternative routes (Suez bypass, rail, pipeline) become economically viable at scale?',
    'Cost-benefit analysis of alternative infrastructure investment vs continued Mediterranean premium. Break-even analysis at different trade volume levels.',
    'If viability threshold < 200% premium: scaffold sunset is real (alternatives will be built). If threshold > 400% premium: scaffold is aspirational (alternatives remain uneconomical, snare persists).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_route_viability_threshold, empirical, 'Economic viability threshold for alternative trade routes').

omega_variable(
    coalition_coordination_effectiveness,
    'Can organized agents (port authorities, rail operators, shippers) coordinate alternative routes without collapsing into competing extractors?',
    'Institutional analysis of multi-stakeholder coordination attempts; tracking of governance structures for alternative route governance; failure modes of past coordination efforts.',
    'If effective: alternative coalition replaces primary constraint with new rope-type coordination. If ineffective: alternative becomes fragmented snare (multiple extractors competing for rents).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coalition_coordination_effectiveness, conceptual, 'Whether alternative route coalition can maintain coordination vs fragmentation').

omega_variable(
    small_trader_coalition_capacity,
    'Can small Mediterranean traders organize collective action to bypass the chokepoint, or are they atomized by geography and capital constraints?',
    'Historical analysis of trader association formation; capacity for collective vessel chartering, route negotiation, or insurance pooling. Comparison with past trader coalitions (Hanseatic League models).',
    'If high capacity: snare classification shifts toward tangled_rope (traders gain negotiating power). If low capacity: snare persists (traders remain trapped, unorganized).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(small_trader_coalition_capacity, empirical, 'Coalition formation capacity for small traders').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mediterranean_trade_disruption, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(medtr_tr_t0, mediterranean_trade_disruption, theater_ratio, 0, 0.4).
narrative_ontology:measurement(medtr_tr_t2, mediterranean_trade_disruption, theater_ratio, 2, 0.48).
narrative_ontology:measurement(medtr_tr_t4, mediterranean_trade_disruption, theater_ratio, 4, 0.55).

% Extraction over time
narrative_ontology:measurement(medtr_be_t0, mediterranean_trade_disruption, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(medtr_be_t2, mediterranean_trade_disruption, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(medtr_be_t4, mediterranean_trade_disruption, base_extractiveness, 4, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mediterranean_trade_disruption, resource_allocation).
narrative_ontology:affects_constraint(mediterranean_trade_disruption, suez_canal_chokepoint).
narrative_ontology:affects_constraint(mediterranean_trade_disruption, global_energy_supply_disruption).
narrative_ontology:affects_constraint(mediterranean_trade_disruption, port_authority_rents).
narrative_ontology:affects_constraint(mediterranean_trade_disruption, shipping_insurance_premiums).

% DUAL FORMULATION NOTE:
% Mediterranean trade disruption is upstream of specific chokepoint constraints (Suez Canal control, Red Sea piracy) and downstream of geopolitical power concentration. Decomposition follows: (1) geographic_vulnerability (ε=0.15, Mountain) — Mediterranean is structurally vulnerable to disruption by virtue of geography; (2) political_enforcement (ε=0.52, Snare) — enforcement of disruption through military/political means; (3) mediterranean_trade_disruption (ε=0.58, Tangled Rope) — combined effect with coordination overlay. This story models the combined effect; upstream and downstream constraints should be separately modeled per ε-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mediterranean_trade_disruption, moderate, 0.7).
constraint_indexing:directionality_override(mediterranean_trade_disruption, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
