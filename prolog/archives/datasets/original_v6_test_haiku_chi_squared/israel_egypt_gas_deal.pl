% ============================================================================
% CONSTRAINT STORY: israel_egypt_gas_deal
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_israel_egypt_gas_deal, []).

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
 *   constraint_id: israel_egypt_gas_deal
 *   human_readable: Geopolitical Gas Supply Agreement between Israel and Egypt
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The Israel-Egypt gas supply agreement represents a hybrid
 *   coordination-extraction structure that functions simultaneously as energy
 *   market integration, geopolitical normalization mechanism, and structural
 *   trap for weaker actors. Signed in 2020-2021, the agreement allows Israel
 *   to export Eastern Mediterranean gas reserves (Leviathan and Tamar fields)
 *   to Egypt for domestic consumption and LNG export, providing Egypt with
 *   foreign currency revenue during a debt crisis while giving Israel
 *   strategic economic leverage. The constraint exhibits strong perspectival
 *   divergence: Israeli energy ministry sees pure coordination (Rope);
 *   Egyptian government sees mixed benefit/constraint (Tangled Rope);
 *   Palestinian territories see pure extraction (Snare); organized regional
 *   actors see coordination with monopolistic features (Tangled Rope);
 *   renewable-energy advocates see a temporary structure with sunset
 *   (Scaffold); the boycott regime experiences institutional degradation
 *   (Piton); and a thermodynamic observer risks naturalizing the arrangement
 *   as inevitable (false Mountain). The theater_ratio (0.58) reflects
 *   substantial performative framing: both Israeli and Egyptian governments
 *   describe the agreement as 'Palestinian benefit' and 'regional stability,'
 *   while actual Palestinian energy access remains constrained and the
 *   arrangement functions primarily as normalization leverage.
 *
 * KEY AGENTS:
 *   - Israeli Energy Ministry: Primary beneficiary (institutional/arbitrage) — monetizes gas discoveries, establishes regional economic integration, captures strategic leverage
 *   - Egyptian Government: Constrained beneficiary-victim (moderate/constrained) — receives foreign currency revenue during IMF crisis but locks in long-term dependency
 *   - Palestinian Territories: Primary victim (powerless/trapped) — excluded from negotiations, lacks independent energy access, structurally dependent on external actors
 *   - Regional Energy Market Coalition: Organized actor (organized/constrained) — Gulf states, Turkey, EU energy security planners; see both coordination (market) and extraction (monopoly)
 *   - Eastern Mediterranean Renewable Coalition: Organized actor (organized/mobile) — climate advocates, renewable energy investors; see constraint as temporary with sunset
 *   - Arab League Boycott Framework: Institutional actor (institutional/arbitrage) — formal enforcement mechanism experiencing functional degradation via performative compliance
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing political arrangement as thermodynamic inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(israel_egypt_gas_deal, 0.52).
domain_priors:suppression_score(israel_egypt_gas_deal, 0.68).
domain_priors:theater_ratio(israel_egypt_gas_deal, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(israel_egypt_gas_deal, extractiveness, 0.52).
narrative_ontology:constraint_metric(israel_egypt_gas_deal, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(israel_egypt_gas_deal, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(israel_egypt_gas_deal, tangled_rope).
narrative_ontology:human_readable(israel_egypt_gas_deal, "Geopolitical Gas Supply Agreement between Israel and Egypt").
narrative_ontology:topic_domain(israel_egypt_gas_deal, "geopolitical/economic").

domain_priors:requires_active_enforcement(israel_egypt_gas_deal).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(israel_egypt_gas_deal, israeli_energy_sector).
narrative_ontology:constraint_beneficiary(israel_egypt_gas_deal, egyptian_government_budget).
narrative_ontology:constraint_victim(israel_egypt_gas_deal, palestinian_territories).
narrative_ontology:constraint_victim(israel_egypt_gas_deal, egyptian_renewable_energy_capacity).
narrative_ontology:constraint_victim(israel_egypt_gas_deal, regional_energy_independence).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PALESTINIAN TERRITORIES (SNARE) — Structurally excluded from energy agreement benefits despite geographical proximity and resource claims. Trapped in asymmetric dependency on external suppliers; no seat at negotiation table. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.69.
constraint_indexing:constraint_classification(israel_egypt_gas_deal, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: EGYPTIAN GOVERNMENT (TANGLED ROPE) — Constrained by foreign currency debt and energy crisis; benefits from gas supply revenue and IMF-mandated fiscal discipline. Also victim of long-term energy dependency. d≈0.62, f(d)≈0.85, σ=0.9 → χ≈0.39.
constraint_indexing:constraint_classification(israel_egypt_gas_deal, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ISRAELI ENERGY MINISTRY (ROPE) — Primary beneficiary; monetizes discoveries while establishing regional economic integration. Pure coordination from extraction perspective: gas flows, revenue accrues, diplomatic leverage increases. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05.
constraint_indexing:constraint_classification(israel_egypt_gas_deal, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL ENERGY MARKET COALITION (TANGLED ROPE) — Organized actors (Gulf states, Turkey, EU energy security planners) see both coordination (market stabilization, LNG export infrastructure) and extraction (Israeli/Egyptian monopoly on Eastern Mediterranean supply). d≈0.58, f(d)≈0.78, σ=1.1 → χ≈0.47.
constraint_indexing:constraint_classification(israel_egypt_gas_deal, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: HISTORICAL ARAB BOYCOTT FRAMEWORK (PITON) — Formal Arab League boycott of Israeli goods is theoretically active but functionally degraded; the gas deal represents performative compliance (reframed as 'Palestinian benefit' in rhetoric) while violating the spirit of the original prohibition. theater_ratio=0.58 reflects substantial performative negotiation frames. The boycott persists through institutional inertia despite structural violations.
constraint_indexing:constraint_classification(israel_egypt_gas_deal, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: EASTERN MEDITERRANEAN RENEWABLE TRANSITION (SCAFFOLD) — Organized actors (EU climate mandates, regional decarbonization initiatives) see the gas deal as temporary infrastructure with sunset: solar/wind capacity deployment in Egypt and Israel is building alternative energy pathways. Current gas agreement has implicit sunset as renewable costs decline. d≈0.35, f(d)≈0.32, σ=0.9 → χ≈0.17. Suppression remains moderate because the renewable exit is feasible within 15-20 years.
constraint_indexing:constraint_classification(israel_egypt_gas_deal, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / THERMODYNAMIC VIEW (MOUNTAIN) — From civilizational/universal perspective, energy flow follows economic gradients; gas from lowest-cost producer to highest-value user is thermodynamically inevitable. This perspective risks naturalizing what is actually a contingent political arrangement (territorial claims, sovereignty recognition, IMF conditionality). The engine will detect this as false summit.
constraint_indexing:constraint_classification(israel_egypt_gas_deal, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(israel_egypt_gas_deal_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(israel_egypt_gas_deal, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(israel_egypt_gas_deal, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(israel_egypt_gas_deal, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(israel_egypt_gas_deal, TR),
    TR >= 0.70.

:- end_tests(israel_egypt_gas_deal_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The agreement does transfer value to Israel (gas revenue, strategic leverage) and Egypt (foreign currency, debt service) but creates structural dependency for weaker actors (Palestinians, future Egypt vis-à-vis Israeli supply). The extractiveness is not as high as pure monopoly pricing (0.70+) because there is genuine benefit flow to Egypt and some coordination function (market stabilization). The rising trajectory (0.35→0.52 over 6 years) reflects increasing lock-in as infrastructure investment deepens. Suppression (0.68): High. Significant coercive features include: Egypt's debt crisis removing negotiating power, Palestinian exclusion from table, geopolitical preconditions for Arab recognition, and lack of alternative suppliers at equivalent cost/timeline. But suppression is not total (0.80+) because Egypt retains formal sovereignty and renewable alternatives are technically possible. Theater ratio (0.58): Moderate-high. Substantial performative elements: both governments frame the deal as 'regional stability,' 'Palestinian benefit,' and 'energy cooperation,' while the actual mechanism is Israeli leverage over Egyptian currency flows and normalization. The performative content increased over time as public justifications diverged from structural mechanics.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates why indexed classification is necessary: the same institutional arrangement produces six different observed types across observer positions. Israeli energy ministry sees Rope (coordination solving energy security + market access). Egyptian government oscillates between Rope (revenue benefit) and Snare (dependency trap) depending on whether it focuses on immediate debt relief or 20-year lock-in. Palestinian territories experience Snare (pure extraction, no exit). Regional energy market sees Tangled Rope (both market stability and monopolistic gatekeeping). Renewable advocates see Scaffold (temporary, sunset via decarbonization). Boycott regime sees Piton (formal prohibition degraded to performative compliance). Thermodynamic observer risks Mountain (naturalizes as inevitable) — but the structural data reveals this as false summit. The perspectival divergence is not measurement error; it is the legitimate experience of actors at different structural positions relative to the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Israeli Energy Ministry: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary with strong institutional leverage. Egyptian Government: Beneficiary + constrained → d≈0.62, f(d)≈0.85. Receives gas revenue but cannot exit due to debt crisis and geopolitical pressure. Palestinian Territories: Victim + trapped → d≈0.92, f(d)≈1.38. No negotiating position, no alternative suppliers, structurally dependent. Regional actors: Mixed + constrained → d≈0.58, f(d)≈0.78. Both benefit from market stability and harmed by monopoly pricing. Renewable coalition: Organized + mobile → d≈0.35, f(d)≈0.32. Can exit via investment in renewables; trajectory is improving. Boycott framework: Institutional + arbitrage → d≈0.08, f(d)≈-0.10. Piton classification derives from theater gate, not directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   CRITICAL CASE FOR MANDATROPHY: The gas deal demonstrates why distinguishing Tangled Rope from Snare is non-trivial. The structural data (ε=0.52, suppression=0.68, requires_active_enforcement=true, beneficiaries AND victims present) satisfies Tangled Rope gates. But from the Palestinian perspective, the same constraint is a pure Snare (ε→higher, suppression→0.85+, victims only). The mandatrophy resolution: the constraint is genuinely Tangled Rope from the bilateral Israel-Egypt perspective (coordination function + extraction from third parties) but becomes Snare when you expand the scope to include Palestinian territories. This is not measurement ambiguity — it is scope-dependent classification. The Egyptian perspective is critical: if Egypt is experiencing genuine fiscal relief (coordination) rather than debt-trap deepening (extraction), the Tangled Rope is accurate. If Egypt is trapped in long-term energy dependency with revenue primarily servicing IMF loans, the constraint becomes a Snare targeting both Palestinians and Egypt. The measurements track this: rising extractiveness (0.35→0.52) suggests increasing lock-in; rising theater (0.42→0.58) suggests performative framing diverging from structural reality. The constraint remains Tangled Rope if Egyptian government retains meaningful agency and exit options; it degrades toward Snare if renewable alternatives become unavailable or if debt dependency deepens.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    palestinian_revenue_distribution,
    'Do actual revenue flows to Palestinian territories match public commitments, or are they performative/tokenized?',
    'Audit of fund flows from Egyptian government to PA; comparison of stated vs actual per-capita energy cost reduction for Palestinian consumers',
    'If genuine flows: constraint is more Rope than Snare for Palestinians. If tokenized: snare classification confirmed; extraction mechanism is stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_revenue_distribution, empirical, 'Whether Palestinian revenue commitments are genuine or performative').

omega_variable(
    egyptian_debt_trap_mechanism,
    'Is the gas deal structured to trap Egypt in long-term dependency (debt-service via gas payments), or does it create genuine fiscal relief?',
    'Long-term debt trajectory analysis with/without gas revenue; comparison of deal terms to IMF loan conditions; counterfactual renewable-energy investment scenarios',
    'If trap: Egyptian government is secondary victim (Snare classification strengthens). If relief: Egyptian Tangled Rope is accurate (mixed benefit).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(egyptian_debt_trap_mechanism, empirical, 'Whether gas deal creates debt dependency or fiscal relief for Egypt').

omega_variable(
    renewable_transition_feasibility,
    'Can Eastern Mediterranean renewable capacity actually replace natural gas within 15-20 year sunset window, or is the scaffold sunset illusory?',
    'Technical cost modeling of solar/wind deployment at regional scale; grid integration feasibility studies; capital availability for infrastructure transition',
    'If feasible: scaffold perspective is structural (sunset is real). If infeasible: scaffold is aspirational; constraint remains extraction (Snare/Tangled Rope) indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_transition_feasibility, empirical, 'Whether renewable transition can realistically replace gas supply').

omega_variable(
    normalization_extraction_coupling,
    'Is the gas deal primarily a coordination mechanism (mutual benefit) or a mechanism to lock Egypt into normalization (extraction via political dependency)?',
    'Comparative analysis of normalization timelines in other peace/trade frameworks; examination of whether gas deal was conditional on Egyptian recognition moves',
    'If coordination-primary: Rope classification strengthens. If normalization-primary: extractive component (Snare) increases; strategic asymmetry becomes dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(normalization_extraction_coupling, conceptual, 'Whether gas deal is coordination or normalization extraction').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(israel_egypt_gas_deal, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(iegd_tr_t0, israel_egypt_gas_deal, theater_ratio, 0, 0.42).
narrative_ontology:measurement(iegd_tr_t3, israel_egypt_gas_deal, theater_ratio, 3, 0.5).
narrative_ontology:measurement(iegd_tr_t6, israel_egypt_gas_deal, theater_ratio, 6, 0.58).

% Extraction over time
narrative_ontology:measurement(iegd_be_t0, israel_egypt_gas_deal, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(iegd_be_t3, israel_egypt_gas_deal, base_extractiveness, 3, 0.44).
narrative_ontology:measurement(iegd_be_t6, israel_egypt_gas_deal, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(israel_egypt_gas_deal, resource_allocation).
narrative_ontology:affects_constraint(israel_egypt_gas_deal, eastern_mediterranean_lng_export).
narrative_ontology:affects_constraint(israel_egypt_gas_deal, egyptian_renewable_energy_transition).
narrative_ontology:affects_constraint(israel_egypt_gas_deal, arab_league_normalization_constraint).
narrative_ontology:affects_constraint(israel_egypt_gas_deal, palestinian_energy_sovereignty).

% DUAL FORMULATION NOTE:
% The gas deal should be decomposed into two related constraints: (1) resource_allocation (ε≈0.45, Tangled Rope) — bilateral coordination between Israel and Egypt for gas supply, and (2) geopolitical_extraction (ε≈0.68, Snare) — use of gas deal as mechanism to lock Egypt into normalization and exclude Palestinian participation. The current story integrates both; separate stories could model them distinctly. The affects_constraints links identify upstream empirical claims (LNG market data) and downstream institutional effects (normalization, renewable transition).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(israel_egypt_gas_deal, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
