% ============================================================================
% CONSTRAINT STORY: derivative_labor_extraction
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_labor_extraction, []).

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
 *   constraint_id: derivative_labor_extraction
 *   human_readable: Derivative Labor Extraction through Value Chain Fragmentation
 *   domain: economic/labor/global_supply_chains
 *
 * SUMMARY:
 *   Derivative labor extraction refers to the structural capture of labor
 *   value through fragmented supply chains where lead firms (brand owners,
 *   retailers, manufacturers) outsource production to networks of suppliers
 *   in lower-wage regions. Workers bear suppressed wages, precarious
 *   conditions, and monopsony control; lead firms capture the differential
 *   between global labor costs and prices charged in high-wage markets. The
 *   constraint exhibits all six DR types from different perspectives,
 *   revealing the gap between how the extraction appears to beneficiaries
 *   (coordination), how it is experienced by victims (snare), and how
 *   development institutions naturalize it (mountain). The theater ratio
 *   (0.58) reflects the mid-level performativity of CSR audits and labor
 *   certifications — these create appearance of monitoring without capturing
 *   the extraction flow. Base extractiveness has increased from 0.45 to 0.68
 *   over the interval as globalization deepened, supply chains fragmented
 *   further, and labor-organizing capacity eroded relative to capital
 *   mobility.
 *
 * KEY AGENTS:
 *   - Peripheral Workers: Primary victim (powerless/trapped) — wage-earners with no alternative employment, visa restrictions, and geographic immobility. Bear full extraction cost through wage suppression and condition control.
 *   - Lead Firms: Primary beneficiary (institutional/arbitrage) — capture monopsony rents through price-setting power over suppliers and cost arbitrage across geographies. Have exit capacity (location shifting, supplier switching).
 *   - Intermediate Suppliers: Secondary actors (moderate/constrained) — face capital requirements and customer concentration. Experience tangled rope: genuine coordination function (synchronizing global production) alongside extraction.
 *   - Labor Standard Coalition: Organized agents (organized/constrained) — ILO, unions, NGO networks attempting to impose minimum floors and transparency. See constraint as temporary and solvable through organizing.
 *   - Development Organizations: Institutional actors (institutional/identity_locked) — identity-fused with export-growth logic; facilitating capital inflow while extractive wages subsidize profits. Structurally mobile but identity prevents recognizing extraction as mutable.
 *   - CSR Apparatus: Institutional maintenance system (institutional/arbitrage) — audit and reporting mechanisms that create appearance of verification without eliminating extraction. Piton classification reflects high theater and low functional verification.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing contingent institutional arrangements (comparative advantage, capital mobility, labor cost differentials) as immutable economic law.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_labor_extraction, 0.68).
domain_priors:suppression_score(derivative_labor_extraction, 0.72).
domain_priors:theater_ratio(derivative_labor_extraction, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_labor_extraction, extractiveness, 0.68).
narrative_ontology:constraint_metric(derivative_labor_extraction, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(derivative_labor_extraction, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_labor_extraction, snare).
narrative_ontology:human_readable(derivative_labor_extraction, "Derivative Labor Extraction through Value Chain Fragmentation").
narrative_ontology:topic_domain(derivative_labor_extraction, "economic/labor/global_supply_chains").

domain_priors:requires_active_enforcement(derivative_labor_extraction).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_labor_extraction, lead_firms).
narrative_ontology:constraint_beneficiary(derivative_labor_extraction, capital_holders).
narrative_ontology:constraint_victim(derivative_labor_extraction, peripheral_workers).
narrative_ontology:constraint_victim(derivative_labor_extraction, labor_epistemic_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PERIPHERAL WORKER (SNARE) — Trapped by geographic immobility, visa restrictions, debt dependency, and lack of alternative employment. Bears full extraction cost: wages set by monopsony power, working conditions determined by lead firm specifications, no collective bargaining power. Maximum experienced extraction — no exit capacity.
constraint_indexing:constraint_classification(derivative_labor_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: INTERMEDIATE SUPPLIER (TANGLED ROPE) — Constrained by capital requirements, technology lock-in, and customer concentration (single lead firm may represent 40-60% of revenue). Benefits from guaranteed orders and technology transfer; also subject to price compression and just-in-time demands. Genuine coordination function (synchronizing production across supply chain) embedded within asymmetric extraction.
constraint_indexing:constraint_classification(derivative_labor_extraction, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: LEAD FIRM (ROPE) — Experiences constraint as pure coordination: managing dispersed production, quality standards, and delivery schedules across multiple suppliers. Net beneficiary — extraction flows inward. Arbitrage options available (shift production location, switch suppliers, vertically integrate if needed). Immediate time horizon reflects that pricing decisions and restructuring are quarterly executive functions.
constraint_indexing:constraint_classification(derivative_labor_extraction, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR STANDARD COALITION (SCAFFOLD) — Organized actors (ILO, union networks, NGO accountability schemes) attempting to impose minimum labor standards on supply chains. See the constraint as temporary: union organizing, certification programs (Fair Trade, SA8000), and supply chain transparency initiatives are building alternative verification pathways. Low effective extraction for this perspective because coalition has agency and sees an exit path (enforceable labor floors replacing market extraction).
constraint_indexing:constraint_classification(derivative_labor_extraction, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: CORPORATE SOCIAL RESPONSIBILITY (PITON) — The CSR/sustainability reporting system is substantially performative: audit procedures create appearance of verification without capturing the actual extraction flow. Annual reports, third-party certifications, and stakeholder engagement are largely theatrical — designed to manage reputation risk rather than eliminate the underlying extraction. Theater persists through institutional inertia and reputational necessity despite low functional verification of labor conditions.
constraint_indexing:constraint_classification(derivative_labor_extraction, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CAPTURED DEVELOPMENT ORGANIZATION (TANGLED ROPE) — Development agencies, trade associations, and export-focused government bodies are identity-locked into supply chain paradigms: their institutional identity is constituted through facilitating foreign investment and export growth. Structurally mobile (could redirect investment toward domestic labor protections) but identity-fused with growth-at-any-cost logic. Sees both genuine coordination function (linking developing economies to capital and markets) and asymmetric extraction (workers subsidize profits through suppressed wages). Identity lock prevents recognizing the extraction as mutable.
constraint_indexing:constraint_classification(derivative_labor_extraction, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational/universal perspective, wage compression in low-cost regions is presented as immutable economic law: comparative advantage naturally produces labor cost differentials; capital flows to lowest-cost jurisdictions; firms must extract maximum value to remain competitive. This perspective naturalizes what is actually a contingent institutional arrangement (weak labor protections + capital mobility + monopsony power). The engine's false summit detector will identify this as naturalization of extractive incentive structures.
constraint_indexing:constraint_classification(derivative_labor_extraction, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_labor_extraction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(derivative_labor_extraction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(derivative_labor_extraction, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_labor_extraction, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(derivative_labor_extraction, TR),
    TR >= 0.70.

:- end_tests(derivative_labor_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The original research on global supply chains documents wage suppression of 60-75% relative to equivalent manufacturing in high-wage regions, controlling for labor productivity. The extraction has grown because globalization deepened monopsony power (fewer large buyers controlling more of global supply) while labor organizing capacity eroded. The 0.68 value reflects the scale of extraction flow but is constrained below 0.90+ because some legitimate coordination costs exist (quality control, just-in-time logistics) that are not pure extraction. Suppression (0.72): High. Multiple structural barriers prevent exit: geographic immobility (cost and risk of relocation), visa restrictions (tied to specific employers in many systems), debt dependency (advance wages / recruitment bonds create liability), skill lock-in (supply chain work doesn't transfer to other sectors), and the absence of alternative employment at comparable scale. Suppression increased with just-in-time logistics that made worker organizing more difficult. Theater ratio (0.58): Moderate-high. CSR audits, third-party certifications, and transparency initiatives create substantial performative content — annual audits check compliance with written policies, but actual working conditions and wage calculation often differ from audited representations. However, theater is not dominant (unlike piton's >0.70) because some real monitoring occurs and some workers do report improvements from audits.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the full range of perspectival interpretation. The lead firm sees pure coordination (Rope) — solving the genuine problem of synchronizing dispersed production, managing quality, allocating supply-chain risks. The intermediate supplier sees mixed coordination and extraction (Tangled Rope) — they coordinate with lead firms but also experience price compression and just-in-time demands that compress their margins. The peripheral worker sees pure extraction (Snare) — monopsony wages, no exit, no coordination benefit to them (they don't control timing or design). The labor coalition sees a temporary problem with solutions (Scaffold) — union organizing, sectoral bargaining, and supply-chain transparency can build alternative pathways. The CSR apparatus sees its own degraded ritual (Piton) — audits and reports persist through reputational necessity despite low functional verification. The development organization sees both coordination and extraction (Tangled Rope) but is identity-locked: their professional identity is constituted through facilitating capital inflow, making them unable to recognize the extraction as mutable. The civilizational analytical observer risks seeing comparative advantage and capital mobility as immutable laws (Mountain) — but the structural data reveals these are contingent institutional arrangements (weak labor regulation, unequal bargaining power, capital-favorable migration policy).
 *
 * DIRECTIONALITY LOGIC:
 *   Lead firms occupy the beneficiary position: they capture the spread between global production costs and market prices. Their directionality (d ≈ 0.05) reflects arbitrage-enabled exit — they can shift production, modify supply chains, or vertically integrate if pressure increases. This low d produces negative effective extraction for them (they experience the constraint as pure coordination with benefits). Peripheral workers occupy the victim position: they face trapped exit options with no alternative employment at comparable wage. Their directionality (d ≈ 0.95) produces maximum effective extraction. Intermediate suppliers occupy a middle position (d ≈ 0.50-0.60): they are partly victims of lead-firm price compression but also partly beneficiaries of guaranteed orders and technology transfer. Constrained exit options (capital lock-in) raise their d above a beneficiary level but below a fully-trapped level. Development organizations are institutional beneficiaries in terms of structural position (d ≈ 0.15) — they benefit from capital inflow and export growth — but are identity-locked, which modulates their classification from Rope (which they would experience if not captured) to Tangled Rope (mixed coordination and extraction) at the biographical time horizon.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED through perspectival decomposition. The mandatrophy is NOT 'is derivative labor extraction snare or rope?' but rather 'whose perspective are you adopting?' The answer is: it is genuinely snare from the peripheral worker's perspective (trapped, no exit, pure extraction), genuinely rope from the lead firm's perspective (pure coordination with benefits), and genuinely tangled rope from intermediate positions. The mandatrophy is resolved by observing that the presheaf of perspectives over the observation site is consistent — each agent's classification is accurate for their structural position. The false summit (mountain/natural law) appears when analysts naturalize the institutional arrangements (weak labor regulation, capital-mobility privilege, visa restrictions) as inevitable rather than contingent. Development organizations demonstrate regulatory capture through identity lock — they are structurally capable of redirecting investment toward labor-protective policies but cannot do so because their institutional identity is constituted through growth maximization. This is a case where the classification system reveals the capture mechanism: comparing the identity-locked institutional perspective to the analytical perspective exposes that development logic has been subsumed by capital accumulation logic.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_vs_internalized_suppression,
    'What proportion of measured suppression is structural (visa restrictions, capital requirements, geographic immobility) versus internalized (normalized wage exploitation, acceptance of conditions as inevitable)?',
    'Longitudinal tracking of exit behavior post-constraint removal; comparison of suppression persistence in workers who escape supply chain vs those who remain; cognitive frame analysis via exit interviews',
    'If predominantly structural: constraint can be reduced through policy (visa reform, capital access). If predominantly internalized: constraint carries through after structural barriers are removed — workers remain trapped by identity fusion with low-wage identity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_vs_internalized_suppression, empirical, 'Structural vs internalized suppression mechanism in derivative labor extraction').

omega_variable(
    counterfactual_wage_distribution,
    'What would wage distribution across supply chain be under equivalent coordination with symmetric bargaining power?',
    'Comparison with industries where labor has organizational power (unionized manufacturing, public sector); simulation of wage outcomes under different governance models (cooperatives, profit-sharing, sectoral bargaining)',
    'If counterfactual wages >> observed wages: high extraction is confirmed, legitimating snare classification. If counterfactual wages ≈ observed wages: extraction is lower than model suggests (coordination costs are substantial), weakening snare case.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(counterfactual_wage_distribution, conceptual, 'Counterfactual wage under symmetric bargaining').

omega_variable(
    alternative_coordination_feasibility,
    'Can supply chain coordination function (quality control, delivery timing, innovation diffusion) be sustained without monopsony extraction? Could democratized governance models preserve coordination benefits while reducing extraction?',
    'Analysis of existing alternative models (worker cooperatives in supply chains, sectoral bargaining outcomes in Nordic manufacturing, platform cooperatives with standardized interfaces)',
    'If feasible: constraint is snare legitimately, and scaffold perspective is structurally grounded (alternatives exist). If infeasible: constraint is tangled rope (extraction is coordination cost), not pure snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_coordination_feasibility, empirical, 'Whether alternative coordination models can preserve supply chain function').

omega_variable(
    lead_firm_competitive_necessity,
    'Do lead firms extract maximum value from suppliers because it is competitively necessary (would competitors undercut them if they didn''t) or because it is profit-maximizing (could afford to pay more and remain profitable)?',
    'Comparative analysis of profit margins and wage costs across competing lead firms; historical analysis of labor cost dynamics during periods of high competition vs consolidation; simulation of wage elasticity of demand',
    'If competitively necessary: extraction is structurally coerced (rope or tangled rope). If profit-maximizing choice: extraction is pure rent-seeking (snare). Classification depends on degree of necessity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(lead_firm_competitive_necessity, empirical, 'Whether lead firm extraction is competitively necessary or profit-maximizing').

omega_variable(
    labor_epistemic_commons_degradation,
    'To what degree does derivative labor extraction degrade the epistemic commons (knowledge about working conditions, labor standards, supply chain reality) that would enable worker organization and policy response?',
    'Analysis of information asymmetry between workers, firms, and regulators; tracking of labor organizing effectiveness correlated with information access; measurement of audit/transparency reach',
    'If epistemic commons significantly degraded: suppression is higher than structural measures alone suggest (information barriers prevent coordination). If epistemic commons partially intact: workers have organizing potential and suppression is lower.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(labor_epistemic_commons_degradation, empirical, 'Epistemic commons degradation through opacity and information asymmetry').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_labor_extraction, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deriv_tr_t0, derivative_labor_extraction, theater_ratio, 0, 0.35).
narrative_ontology:measurement(deriv_tr_t10, derivative_labor_extraction, theater_ratio, 10, 0.5).
narrative_ontology:measurement(deriv_tr_t20, derivative_labor_extraction, theater_ratio, 20, 0.58).
narrative_ontology:measurement(deriv_tr_t5, derivative_labor_extraction, theater_ratio, 5, 0.42).

% Extraction over time
narrative_ontology:measurement(deriv_be_t0, derivative_labor_extraction, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(deriv_be_t10, derivative_labor_extraction, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(deriv_be_t20, derivative_labor_extraction, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(deriv_be_t5, derivative_labor_extraction, base_extractiveness, 5, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_labor_extraction, resource_allocation).
narrative_ontology:affects_constraint(derivative_labor_extraction, global_wage_compression).
narrative_ontology:affects_constraint(derivative_labor_extraction, union_organizing_barriers).
narrative_ontology:affects_constraint(derivative_labor_extraction, monopsony_buyer_power).

% DUAL FORMULATION NOTE:
% Derivative labor extraction is downstream of global capital mobility and upstream of worker organizing constraints. The high extractiveness (0.68) reflects the structural coupling of supply-chain fragmentation (enabling monopsony) with weak labor regulation in peripheral economies. This constraint family includes: global_wage_compression (ε=0.55, Tangled Rope — coordination of wage levels across regions alongside extraction), union_organizing_barriers (ε=0.72, Snare — direct suppression of collective action), and monopsony_buyer_power (ε=0.65, Tangled Rope — genuine supply coordination alongside asymmetric extraction). All three stories share the same beneficiaries (lead firms, capital) and victims (peripheral workers) but have different mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(derivative_labor_extraction, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
