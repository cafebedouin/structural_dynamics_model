% ============================================================================
% CONSTRAINT STORY: ice_safe_departure
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ice_safe_departure, []).

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
 *   constraint_id: ice_safe_departure
 *   human_readable: ICE Safe Departure Program
 *   domain: political/immigration
 *
 * SUMMARY:
 *   The ICE Safe Departure Program creates a structural tension between
 *   humanitarian framing and coercive extraction. Immigrants with final
 *   deportation orders are offered financial incentives ($500-$1,500) to
 *   depart 'voluntarily' rather than face mandatory deportation after
 *   contested legal proceedings. The program reduces ICE's enforcement costs
 *   and accelerates case closure while using humanitarian language ('safe
 *   departure,' 'dignity,' 'choice') to convert mandatory expulsion into
 *   apparent individual decision-making. This transformation is functionally
 *   essential: reframing extraction as voluntary reduces political
 *   resistance, legitimacy costs, and the level of overt suppression
 *   required. The constraint exhibits tangled rope structure: genuine
 *   coordination benefit (reduced family trauma, faster closure, lower
 *   detention costs) is inseparable from asymmetric extraction (permanent
 *   separation, foregone income, foreclosed legal remedies). The theater
 *   ratio (0.62) reflects that the program's primary function is rhetorical —
 *   it changes how deportation is named and experienced, not fundamentally
 *   how it operates. Base extractiveness has risen over the measurement
 *   interval (0.45 → 0.58) as the program's rhetoric has matured and been
 *   integrated into official enforcement strategy, indicating that the naming
 *   change has successfully normalized what was previously contested.
 *
 * KEY AGENTS:
 *   - Undocumented Immigrants: Primary victims (powerless/trapped) — face final deportation orders with no meaningful legal remedy; program offers illusory choice between mandatory expulsion or 'voluntary' expulsion with financial incentive
 *   - ICE Administrative Apparatus: Primary beneficiary (institutional/arbitrage) — captures cost reduction (detention, courtroom time, resistance management) without enforcement expenditure; rebrands extraction as coordination
 *   - Family Units Left Behind: Secondary victims (moderate/constrained) — separated from deporting members; lose income, face social/legal status degradation; some limited benefits from managed vs contested departure
 *   - Immigration Advocacy Organizations: Secondary beneficiary (powerful/arbitrage) — gain negotiation leverage over program terms; benefit from case-by-case humanitarian improvements; also used to legitimize underlying deportation apparatus
 *   - Civil Society Coalition: Organized actors (organized/constrained) — see program as potential stepping stone toward earned legalization and family unity frameworks; constrained by lack of political power to force broader reform
 *   - Deportation Legal Framework: Institutional actor (institutional/arbitrage) — maintains performative modification to mandatory deportation law; final orders remain legally uncontested; program exists within unchanged legal structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ice_safe_departure, 0.58).
domain_priors:suppression_score(ice_safe_departure, 0.68).
domain_priors:theater_ratio(ice_safe_departure, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ice_safe_departure, extractiveness, 0.58).
narrative_ontology:constraint_metric(ice_safe_departure, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(ice_safe_departure, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ice_safe_departure, tangled_rope).
narrative_ontology:human_readable(ice_safe_departure, "ICE Safe Departure Program").
narrative_ontology:topic_domain(ice_safe_departure, "political/immigration").

domain_priors:requires_active_enforcement(ice_safe_departure).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ice_safe_departure, ice_agency_resource_optimization).
narrative_ontology:constraint_beneficiary(ice_safe_departure, deporting_nation_state).
narrative_ontology:constraint_victim(ice_safe_departure, undocumented_immigrants).
narrative_ontology:constraint_victim(ice_safe_departure, family_units_separated).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNDOCUMENTED IMMIGRANT (SNARE) — Final deportation order with no meaningful exit alternatives. Program offers 'voluntary departure' with financial incentive, but departure means permanent separation from family, employment, and established community. Choice is illusory: deportation is mandatory; 'safe departure' merely packages expulsion as consent. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(ice_safe_departure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FAMILY UNITS LEFT BEHIND (TANGLED ROPE) — Separated from deporting member; coordination function (program enables some managed transitions rather than sudden detention), but extraction dominates: family faces financial burden, lost income, social/legal status degradation, long-term separation. d≈0.78, f(d)≈1.12, σ=1.0 → χ≈0.65.
constraint_indexing:constraint_classification(ice_safe_departure, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ICE ADMINISTRATIVE APPARATUS (ROPE) — Program provides pure coordination benefit: voluntary departure reduces detention costs, accelerates case processing, reduces courtroom burden, minimizes deportation resistance. Agency captures resource optimization without coercion expenditure on resistant subjects. d≈0.08, f(d)≈-0.11, σ=1.0 → χ≈-0.06. Net beneficiary; low suppression cost.
constraint_indexing:constraint_classification(ice_safe_departure, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE DEPORTATION LEGAL FRAMEWORK (PITON) — The Safe Departure Program is a performative modification to mandatory deportation law. Theater_ratio=0.62: program language frames coercive expulsion as 'safe,' 'voluntary,' and 'dignified' departure, creating appearance of humanitarian process while the underlying extraction mechanism (final deportation orders, no meaningful legal remedy) remains unchanged. The framing ritual persists despite minimal functional change to actual enforcement outcomes.
constraint_indexing:constraint_classification(ice_safe_departure, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: IMMIGRATION ADVOCACY ORGANIZATIONS (TANGLED ROPE) — Coordination function: negotiating program terms to reduce deportation trauma and preserve some family continuity. But extraction from system: program is used as policy cover ('see, ICE offers humane options') to legitimize underlying deportation apparatus and reduce public pressure for broader reform. Advocates benefit from program negotiation leverage; public benefits from slightly humaner process. But the constraint's existence enables more aggressive deportation elsewhere. d≈0.45, f(d)≈0.42, σ=1.0 → χ≈0.24.
constraint_indexing:constraint_classification(ice_safe_departure, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: CIVIL SOCIETY COALITION (SCAFFOLD) — Sees Safe Departure as temporary scaffolding toward broader immigration reform: voluntary departure incentives could transition into earned legalization pathways, family unity provisions, and ultimate sunset of deportation-first enforcement. Organized coalition (legal advocates, immigrant organizations, religious groups) uses program as concrete institution around which to build alternative framework. Theater_ratio potentially declining as legal alternatives gain legitimacy. d≈0.38, f(d)≈0.38, σ=1.0 → χ≈0.22.
constraint_indexing:constraint_classification(ice_safe_departure, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — Structured as tangled rope from civilizational scale: the program coordinates between state deportation authority and undocumented population, lowering mutual enforcement costs while extracting from the powerless via coercive expulsion. The 'safe' framing is essential to the extraction mechanism — it converts mandatory deportation into apparent individual choice, reducing resistance and legitimacy costs. Suppression (0.68) reflects that alternatives (immigration reform, legalization pathways, family unity provisions) are actively suppressed; the program's existence enables their suppression by appearing responsive. d≈0.58, f(d)≈0.75, σ=1.0 → χ≈0.43.
constraint_indexing:constraint_classification(ice_safe_departure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ice_safe_departure_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ice_safe_departure, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ice_safe_departure, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ice_safe_departure, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(ice_safe_departure, TR),
    TR >= 0.70.

:- end_tests(ice_safe_departure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The program extracts permanent separation, foregone legal remedies, and implicit coercion (choice between two forms of mandatory expulsion). The extraction is not total — some immigrants genuinely prefer faster departure to prolonged detention, and the financial incentive provides real (if modest) material benefit. The increase from 0.45 to 0.58 over the interval reflects that the program's rhetorical maturity has normalized the extraction mechanism. Suppression (0.68): High. Significant barriers to alternatives include: final deportation orders provide no legal escape route; financial incentive is coercive relative to detained alternative; immigrant legal status makes resistance risky; civil society alternatives (earned legalization, family unity) are actively suppressed by enforcement-first policy; no independent verification of 'voluntary' participation occurs. Theater ratio (0.62): Moderate-high. The program is substantially performative: the naming change ('safe,' 'voluntary,' 'dignified') does real political work in legitimizing the underlying coercive extraction, but minimal functional change to enforcement outcomes occurs. The framing ritual (bureaucratic efficiency language, humanitarian rhetoric) persists and has increased over the interval.
 *
 * PERSPECTIVAL GAP:
 *   This constraint shows stark perspectival divergence. The powerless immigrant sees pure extraction (Snare) — final deportation order with illusory choice. The institutional beneficiary (ICE) sees pure coordination (Rope) — resource optimization without coercion. The moderate victim (separated family) sees mixed extraction and coordination (Tangled Rope) — some benefits from managed process, but extraction dominates. The advocacy organization sees opportunity for leverage (Tangled Rope) — negotiating program terms within a constrained landscape. The civil society coalition sees a scaffold (Temporary Support) — a concrete institution that could transition toward broader reform. The analytical observer, from civilizational scope, sees tangled rope with suppression of alternatives — the program's extraction mechanism is inseparable from its coordination framing, and the coordination benefit is partly achieved by suppressing alternative pathways (earned legalization, family unity). The false summit risk: one could naturalize this constraint as an 'inherent tradeoff' between deportation and humane process, missing that the apparent tradeoff is socially constructed — family unity provisions and earned legalization are structurally possible, but actively suppressed.
 *
 * DIRECTIONALITY LOGIC:
 *   Undocumented immigrants: Victim + trapped → d≈0.92, f(d)≈1.38. Final deportation order with no exit; 'voluntary' incentive is coercive relative to detained alternative. Maximum structural extraction. ICE apparatus: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary; low suppression cost due to voluntary framing reducing resistance. Family units: Victim + constrained → d≈0.78, f(d)≈1.12. Separated by deportation; some limited benefits from managed process (reduced detention trauma), but extraction dominates (lost income, permanent separation). Advocacy organizations: Mixed beneficiary/victim + arbitrage → d≈0.45, f(d)≈0.42. Benefit from increased leverage in negotiating program terms; also used to legitimize underlying system. Civil society coalition: Organized + constrained → d≈0.38, f(d)≈0.38. Have agency through coalition; see exit path (reform trajectory); constrained by enforcement-first policy framework. Analytical observer: Derived from structural analysis → d≈0.58, f(d)≈0.75. Sees suppression of alternatives; tangled rope reflects inseparability of coordination and extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by distinguishing the program's stated function (coordination/humanitarian) from its structural function (extraction with coordination framing). The program is legitimately tangled rope, not pure rope, because: (1) genuine coordination benefit exists (reduced family trauma vs contested deportation, faster closure), (2) asymmetric extraction is inseparable (permanent separation, foregone legal remedies, implicit coercion), (3) active enforcement of the framing is required (rhetoric must maintain 'voluntary' appearance). The false summit (natural law mountain: 'deportation inherently requires some trauma') is correctly identified because alternatives exist: family unity provisions, earned legalization pathways, and deportation defense mechanisms could reduce extraction without eliminating coordination. The program's extraction persists not because it's inevitable, but because suppression of alternatives is structurally enforced. Theater ratio (0.62) captures that the program's primary work is rhetorical — converting mandatory expulsion into apparent choice — rather than functional (changing the actual enforcement outcome). The piton perspective (degraded ritual) reflects that the underlying deportation legal framework maintains its coercive structure while the program's ritual modification sustains it through inertia and rhetorical legitimacy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    voluntary_departure_coercion_threshold,
    'At what point does a ''voluntary'' departure incentive become functionally coercive given the alternative is indefinite detention?',
    'Comparative analysis of immigrant decision-making: survey of program participants on perceived alternatives and choice quality; comparison with genuine voluntary migration vs deportation-adjacent incentives',
    'If financial incentive is perceived as genuine choice: program is primarily coordination (Rope dominant). If perceived as coercion: program is primarily extraction (Snare dominant).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(voluntary_departure_coercion_threshold, empirical, 'Threshold between voluntary incentive and functional coercion').

omega_variable(
    family_separation_permanence,
    'Does the Safe Departure incentive structure actually reduce family separation permanence compared to contested deportation, or does it merely accelerate separation?',
    'Longitudinal tracking of family reunification rates post-departure; comparison of program participants vs non-participants on ability to petition for family sponsorship or maintain contact',
    'If reunification rates are higher: program has genuine coordination function. If equivalent or lower: program is extraction masked by coordination framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_separation_permanence, empirical, 'Whether program reduces long-term family separation vs merely accelerating it').

omega_variable(
    policy_spillover_legitimacy,
    'Does the existence of Safe Departure Program provide political cover for more aggressive deportation enforcement elsewhere in the system?',
    'Policy analysis of enforcement priorities pre- and post-program; analysis of rhetorical use of program by agency and Congress; comparison of total deportation rates and enforcement intensity',
    'If enforcement intensity increases post-program while program participation is highlighted publicly: program is suppressing alternatives and enabling increased overall extraction. If enforcement is unchanged or decreases: program is not providing legitimacy cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(policy_spillover_legitimacy, conceptual, 'Whether program legitimacy spillover enables increased enforcement elsewhere').

omega_variable(
    reform_pathway_viability,
    'Is the Safe Departure Program a genuine stepping stone toward earned legalization/family unity frameworks, or a terminal endpoint that forecloses broader reform?',
    'Historical institutional analysis of similar programs; legislative trajectory post-program; stakeholder interviews on reform prospects; comparison with countries that use departure incentives as transition mechanisms vs endpoints',
    'If stepping stone: scaffold perspective is structural. If terminal: program is snare disguised as temporary, and suppression is higher than measured.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_pathway_viability, conceptual, 'Whether program is stepping stone to reform or terminal endpoint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ice_safe_departure, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ice_sd_tr_t0, ice_safe_departure, theater_ratio, 0, 0.48).
narrative_ontology:measurement(ice_sd_tr_t3, ice_safe_departure, theater_ratio, 3, 0.56).
narrative_ontology:measurement(ice_sd_tr_t6, ice_safe_departure, theater_ratio, 6, 0.62).

% Extraction over time
narrative_ontology:measurement(ice_sd_be_t0, ice_safe_departure, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ice_sd_be_t3, ice_safe_departure, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(ice_sd_be_t6, ice_safe_departure, base_extractiveness, 6, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ice_safe_departure, enforcement_mechanism).
narrative_ontology:affects_constraint(ice_safe_departure, deportation_finality_doctrine).
narrative_ontology:affects_constraint(ice_safe_departure, family_separation_enforcement).
narrative_ontology:affects_constraint(ice_safe_departure, undocumented_labor_extraction).

% DUAL FORMULATION NOTE:
% The Safe Departure Program exists in a constraint family with the underlying deportation authority structure. This program's ε=0.58 reflects the specific extraction mechanism of 'voluntary' incentives; the parent constraint (deportation authority itself) has ε≈0.65 reflecting the foundational coercive structure. The program is downstream of the deportation finality doctrine (final orders can only be challenged through limited appellate routes) and affects family separation enforcement (program becomes the mechanism through which families are separated).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(ice_safe_departure, institutional, 0.1).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
