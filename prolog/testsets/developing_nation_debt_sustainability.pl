% ============================================================================
% CONSTRAINT STORY: developing_nation_debt_sustainability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_developing_nation_debt_sustainability, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: developing_nation_debt_sustainability
 *   human_readable: Developing Nation Debt Sustainability Trap
 *   domain: economic_policy/development_finance/geopolitics
 *
 * SUMMARY:
 *   The developing nation debt sustainability constraint operates across
 *   multiple structural levels: macroeconomic (capital scarcity, productivity
 *   gaps, debt-to-GDP ratios), institutional (IMF conditionality, structural
 *   adjustment, creditor governance), and geopolitical (sovereignty limits,
 *   capital flight risk, sanctions vulnerability). The constraint exhibits
 *   genuine ambiguity: it is partly legitimate coordination (ensuring capital
 *   allocation and fiscal discipline), partly extractive (extracting policy
 *   concessions and resource flows), and partly degraded institutional legacy
 *   (vestigial colonial governance structures maintained through inertia).
 *   The empirical extractiveness has increased over the 40-year interval from
 *   0.48 to 0.68, indicating rent-seeking layering onto the original
 *   coordination function. Theater ratio has increased from 0.35 to 0.55,
 *   indicating that performative policy documentation and donor coordination
 *   have become a larger share of actual policy implementation. The
 *   constraint is currently a Snare from the perspective of trapped/powerless
 *   agents, but alternative financing mechanisms (Belt and Road, regional
 *   development banks) are creating exit pathways that could transform it
 *   into a Scaffold with sunset dynamics.
 *
 * KEY AGENTS:
 *   - Developing Nation Populations: Primary victims (powerless/trapped) — bear extraction through austerity, reduced public services, and lost opportunity for autonomous development pathways
 *   - Developing Nation Governments: Secondary victims (moderate/constrained) — sovereign in principle but constrained by debt obligations and conditionality; limited exit options
 *   - Creditor Nations: Primary beneficiaries (institutional/arbitrage) — capture capital returns, political leverage, and policy influence over debtor economies
 *   - International Financial Institutions (IMF, World Bank): Primary beneficiaries (institutional/arbitrage) — administer conditionality, control finance flows, and maintain structural dependence
 *   - Multinational Corporations: Secondary beneficiaries (powerful/arbitrage) — benefit from privatization policies, resource extraction rights, and market access secured through debt-driven liberalization
 *   - Domestic Opposition Coalition: Secondary victims (organized/constrained) — labor, civil society, and regional governments opposing austerity; face political risk from organized dissent
 *   - Alternative Creditors: Emerging beneficiaries (organized/mobile) — China, regional banks creating alternative financing with different conditionality structure; generating exit options for debtors
 *   - Analytical Observer: Sees natural economic law (analytical/analytical) — risks treating contingent institutional dependence as immutable capital scarcity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(developing_nation_debt_sustainability, 0.68).
domain_priors:suppression_score(developing_nation_debt_sustainability, 0.72).
domain_priors:theater_ratio(developing_nation_debt_sustainability, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(developing_nation_debt_sustainability, extractiveness, 0.68).
narrative_ontology:constraint_metric(developing_nation_debt_sustainability, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(developing_nation_debt_sustainability, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(developing_nation_debt_sustainability, snare).
narrative_ontology:human_readable(developing_nation_debt_sustainability, "Developing Nation Debt Sustainability Trap").
narrative_ontology:topic_domain(developing_nation_debt_sustainability, "economic_policy/development_finance/geopolitics").

domain_priors:requires_active_enforcement(developing_nation_debt_sustainability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(developing_nation_debt_sustainability, creditor_nations).
narrative_ontology:constraint_beneficiary(developing_nation_debt_sustainability, international_financial_institutions).
narrative_ontology:constraint_beneficiary(developing_nation_debt_sustainability, multinational_corporations).
narrative_ontology:constraint_victim(developing_nation_debt_sustainability, developing_nation_populations).
narrative_ontology:constraint_victim(developing_nation_debt_sustainability, domestic_social_programs).
narrative_ontology:constraint_victim(developing_nation_debt_sustainability, long_term_economic_sovereignty).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBT-TRAPPED POPULATION (SNARE) — Citizens of developing nations bear extraction through austerity, reduced healthcare and education, and structural adjustment constraints. No exit option: the debt is national and cannot be shed by internal population movement. Maximum suppression from external conditions (creditor requirements) and internal scarcity (resources allocated to debt service). Powerless agents with no voice in debt terms or restructuring decisions.
constraint_indexing:constraint_classification(developing_nation_debt_sustainability, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING NATION GOVERNMENT (SNARE) — While nominally sovereign, the government is constrained by debt servicing requirements and creditor conditionality. Exit options exist in principle (default, capital controls, debt restructuring) but carry catastrophic costs (capital flight, economic isolation, currency collapse). High extraction masked as 'structural adjustment' and 'fiscal discipline.' Suppression operates through both external conditionality and internal political vulnerability (creditor nations and IFIs can withdraw support, triggering crisis).
constraint_indexing:constraint_classification(developing_nation_debt_sustainability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDITOR INSTITUTION (ROPE) — From the institutional beneficiary's perspective, debt service is coordination: it aligns the debtor's fiscal policy with creditor preferences and enables capital flows. Institutions like the IMF and World Bank see themselves as solving a collective action problem (how to ensure capital allocation to high-return projects). They experience minimal extraction costs — the conditionality they impose extracts value from debtors, not from the institutions themselves. High arbitrage options: they can reallocate capital if one debtor fails, or renegotiate terms at will.
constraint_indexing:constraint_classification(developing_nation_debt_sustainability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: DOMESTIC OPPOSITION COALITION (TANGLED ROPE) — Organized agents within developing nations (labor unions, civil society, regional governments) see the constraint as both extraction and coordination. They benefit from some IMF-imposed discipline (e.g., reduced corruption, improved fiscal transparency) while bearing costs (austerity, privatization). They have constrained exit options — they can organize dissent or support default, but at political risk. Embedded within the snare but with some agency and some real coordinating function.
constraint_indexing:constraint_classification(developing_nation_debt_sustainability, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ALTERNATIVE CREDITOR COALITION (SCAFFOLD) — China's Belt and Road Initiative, regional development banks, and South-South lending represent an alternative verification pathway and exit mechanism. These creditors have different conditionality structures and longer repayment horizons, reducing the immediate extraction of Western IMF/World Bank arrangements. This perspective sees the traditional debt constraint as temporary, with sunset driven by emerging alternative financing mechanisms. Low theater — infrastructure investment has direct measurable outputs, avoiding the performative auditing of structural adjustment.
constraint_indexing:constraint_classification(developing_nation_debt_sustainability, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: COLONIAL INSTITUTIONAL LEGACY (PITON) — From a civilizational perspective, debt-sustainability frameworks are partially vestigial colonial governance structures. Nations remain institutionally dependent on external creditor oversight through mechanisms (structural adjustment, poverty reduction strategy papers, donor coordination groups) that echo colonial administration. The primary function — ensuring capital flows and development outcomes — has partially degraded into theater: lengthy policy documents, donor consultations, and performance monitoring metrics that perform legitimacy without ensuring actual development outcomes. Theater ratio reflects that much of the 'dialogue' between creditors and debtors is performative compliance, not substantive policy co-design.
constraint_indexing:constraint_classification(developing_nation_debt_sustainability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / STRUCTURAL ECONOMICS (MOUNTAIN) — From a civilizational/universal perspective, the capital scarcity in developing nations appears as a natural economic law: capital flows to highest returns; nations with weak institutional capacity attract lower volumes at higher cost; this creates a sustainable debt trajectory only with high productivity growth that low-capacity nations cannot achieve. The 'debt trap' appears structural and immutable. However, this perspective risks naturalizing what is contingent institutional arrangement (capital controls are possible, debt forgiveness is politically feasible, alternative financing is emerging). Engine analysis: false summit — reveals naturalization of extractive institution as economic law.
constraint_indexing:constraint_classification(developing_nation_debt_sustainability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(developing_nation_debt_sustainability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(developing_nation_debt_sustainability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(developing_nation_debt_sustainability, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(developing_nation_debt_sustainability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(developing_nation_debt_sustainability, TR),
    TR >= 0.70.

:- end_tests(developing_nation_debt_sustainability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High, and increasing. The base measurement began at 0.48 (20-30 years ago) when structural adjustment was newer and less contested. Current value of 0.68 reflects that conditionality has metastasized: IMF programs now control not just macroeconomic aggregates but specific policy sectors (healthcare privatization, education user fees, pension reform, labor law), extracting policy concessions far beyond what is needed for debt service sustainability. The increasing trajectory indicates rent-seeking dynamics — creditors continuously layer new conditions onto the debt constraint, extracting more than necessary for capital allocation. Suppression (0.72): High. Multiple suppression mechanisms: (1) material scarcity of alternative capital; (2) reputational cost of default (capital flight, sovereign wealth fund seizures); (3) conditionality structured to make exit economically catastrophic in short term; (4) geopolitical isolation risk for non-aligned debtor governments; (5) internal political volatility (creditor can withdraw support to trigger domestic crisis). Theater ratio (0.55): Moderate-high. Significant performative components: lengthy Poverty Reduction Strategy Papers with limited implementation, quarterly donor coordination meetings that validate predetermined decisions, performance monitoring frameworks that measure compliance rather than outcomes, policy dialogue that appears consultative but follows predetermined IMF templates. Real policy implementation happens, but substantial institutional effort goes to performance legitimacy rather than problem-solving.
 *
 * PERSPECTIVAL GAP:
 *   Critical asymmetry: creditors perceive coordination (Rope) from arbitrage position, while debtors perceive extraction (Snare) from trapped position. Both are structurally accurate — they are in fundamentally different relationships to the same constraint. This is not a disagreement to be resolved but a structural feature indicating asymmetric extraction. The scaffold perspective reveals that exit options are emerging (alternative financing), which would transform the snare into a temporary constraint with sunset dynamics. The piton perspective shows that institutional inertia maintains the constraint even as its primary function has degraded — policy documentation and donor coordination rituals consume resources without proportional development impact.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derivation flows from beneficiary/victim declarations and exit options. Creditor institutions are beneficiaries with arbitrage options (low d ≈ 0.10-0.20) → negative or low χ → perceive Rope. Developing nation populations are victims with trapped exit (high d ≈ 0.95) → high f(d) → high χ → perceive Snare. Developing nation governments are victims with constrained exit (d ≈ 0.80-0.85) → high f(d) → high χ → perceive Snare despite nominal sovereignty. Alternative creditors are emerging beneficiaries with mobile exit (d ≈ 0.30-0.40) → moderate f(d) → moderate χ → perceive Scaffold (coordination function + exit pathway). The domestic opposition coalition occupies intermediate position: both benefits and bears costs → d ≈ 0.50-0.55 → moderate f(d) → perceive Tangled Rope. Directionality overrides are not needed — the structural beneficiary/victim declarations sufficiently capture directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION (extractiveness > 0.70): This constraint definitively resolves the mandatrophy by showing that high extraction does NOT entail that coordination is absent. The debt constraint DOES solve a real coordination problem (capital allocation, policy discipline), yet ALSO extracts massive value asymmetrically. The classical logic 'if high extraction, then must be pure extraction (snare)' fails. The constraint is genuinely tangled: without the debt mechanism, capital would not flow to developing nations at all (dead coordination problem). Yet the mechanism as designed extracts far more than necessary for basic coordination, and has become a tool for imposing external governance. The mandatrophy is resolved by rejecting the false dichotomy: the snare classification is appropriate BECAUSE the constraint solves coordination while extracting asymmetrically. The solution is not to claim the constraint is 'really' rope or 'really' snare, but to recognize that snare is exactly the right type for a mechanism that combines genuine coordination function with severe extraction. The increasing extractiveness over time (0.48 → 0.68) indicates that coordination is being subordinated to extraction — rent-seeking has layered onto the original coordination function. This is the hallmark of snare degradation or tangled rope asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    productivity_growth_counterfactual,
    'If developing nations achieved productivity growth rates equal to East Asian tigers, would debt sustainability improve sufficiently to escape the snare?',
    'Causal analysis of growth determinants; comparison of debt-to-GDP trajectories in high-growth vs low-growth periods within the same nations; econometric modeling of growth conditional on institutional factors',
    'If yes: debt trap is primarily a coordination problem (Rope from more institutional perspectives). If no: extraction persists regardless of growth (Snare confirmed). If conditional on capital account liberalization: reveals that conditionality itself is extraction mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(productivity_growth_counterfactual, empirical, 'Whether productivity growth alone resolves debt sustainability').

omega_variable(
    creditor_enforcement_cost_asymmetry,
    'Do creditors bear costs proportional to debtors for debt restructuring failures, or are creditor costs externalized to debtors and third parties?',
    'Comparative analysis of creditor losses in default scenarios vs debtor losses; examination of IMF bailout mechanisms that shield creditors from full write-down; assessment of who bears contagion costs',
    'If symmetric: debt relationship is true coordination (Rope). If asymmetric: confirms extraction mechanism (Snare/Tangled Rope). Asymmetry directly supports snare classification.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(creditor_enforcement_cost_asymmetry, empirical, 'Cost asymmetry between creditors and debtors in restructuring').

omega_variable(
    alternative_financing_sufficiency,
    'Can Belt and Road and regional development bank financing fully substitute for IMF/World Bank lending, eliminating the structural dependence on Western institutions?',
    'Longitudinal comparison of financing volumes, conditionality stringency, and development outcomes under alternative creditors vs traditional IFIs; assessment of political will among alternative creditors to scale capacity',
    'If sufficient: scaffold sunset is real and constraint will degrade to Piton over time. If insufficient: alternative financing remains niche and traditional snare persists. Critical for mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_financing_sufficiency, empirical, 'Whether alternative creditors can fully substitute for Western IFIs').

omega_variable(
    odious_debt_doctrine_enforceability,
    'Could the odious debt doctrine (repudiation of debts incurred by non-representative regimes) become enforceable if codified in international law?',
    'Legal analysis of constitutional obstacles in creditor nations; assessment of political feasibility of universal odious debt framework; case studies of partial implementations',
    'If enforceable: entire debt constraint could be reframed as contingent on creditor consent to legitimacy principle. Transforms snare into Scaffold (with sunset driven by legal reform). If unenforceable: snare persists due to creditor power asymmetry.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(odious_debt_doctrine_enforceability, conceptual, 'Whether odious debt doctrine could become binding principle').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(developing_nation_debt_sustainability, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ddns_tr_t0, developing_nation_debt_sustainability, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ddns_tr_t15, developing_nation_debt_sustainability, theater_ratio, 15, 0.45).
narrative_ontology:measurement(ddns_tr_t30, developing_nation_debt_sustainability, theater_ratio, 30, 0.55).
narrative_ontology:measurement(ddns_tr_t40, developing_nation_debt_sustainability, theater_ratio, 40, 0.62).

% Extraction over time
narrative_ontology:measurement(ddns_be_t0, developing_nation_debt_sustainability, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(ddns_be_t15, developing_nation_debt_sustainability, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(ddns_be_t30, developing_nation_debt_sustainability, base_extractiveness, 30, 0.68).
narrative_ontology:measurement(ddns_be_t40, developing_nation_debt_sustainability, base_extractiveness, 40, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(developing_nation_debt_sustainability, resource_allocation).
narrative_ontology:boltzmann_floor_override(developing_nation_debt_sustainability, 0.2).
narrative_ontology:affects_constraint(developing_nation_debt_sustainability, capital_flight_constraint).
narrative_ontology:affects_constraint(developing_nation_debt_sustainability, structural_adjustment_policy_constraint).
narrative_ontology:affects_constraint(developing_nation_debt_sustainability, sovereign_debt_default_cost).

% DUAL FORMULATION NOTE:
% Developing nation debt sustainability decomposes into three structurally related constraints: (1) capital allocation coordination (the core resource allocation function); (2) structural adjustment governance (the policy extraction mechanism); (3) default cost deterrence (the suppression mechanism). This story covers the aggregate constraint. Each component could have its own story with distinct ε values: capital allocation might be Rope (ε ≤ 0.30), structural adjustment might be Tangled Rope (ε ≈ 0.50-0.60), default deterrence might be Snare (ε ≥ 0.70). The aggregate story uses ε = 0.68 to reflect that extraction dominates the system currently, though coordination function persists.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(developing_nation_debt_sustainability, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
