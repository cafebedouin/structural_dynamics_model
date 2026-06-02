% ============================================================================
% CONSTRAINT STORY: lgfv_debt_trap
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_lgfv_debt_trap, []).

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
 *   constraint_id: lgfv_debt_trap
 *   human_readable: Local Government Financing Vehicle Debt Trap
 *   domain: political_economy/development_economics/fiscal_systems
 *
 * SUMMARY:
 *   The LGFV debt trap emerged from China's 2008-2009 stimulus response, when
 *   local governments were prohibited from direct borrowing but required to
 *   fund infrastructure investment. The solution was Local Government
 *   Financing Vehicles — off-balance-sheet entities that borrowed from
 *   state-owned banks using land as collateral, with revenue from land sales
 *   to developers funding debt service. This created a self-reinforcing
 *   cycle: infrastructure investment raised land values, enabling more
 *   borrowing, funding more infrastructure. The mechanism worked as long as
 *   land sales revenue grew faster than debt accumulation. Post-2021, the
 *   cycle broke: real estate crisis (Evergrande default), demographic decline
 *   (homebuying cohort shrinking), and regulatory crackdowns (three red lines
 *   policy) caused land sales to collapse 40-60% in many jurisdictions. Local
 *   governments now face a structural trap: debt service obligations are
 *   fixed, but the revenue source has permanently contracted. The constraint
 *   exhibits genuine coordination function (infrastructure investment solved
 *   real development needs) alongside asymmetric extraction (future taxpayers
 *   bear costs of current consumption, local governments trapped by central
 *   fiscal design).
 *
 * KEY AGENTS:
 *   - Local Government (Prefecture/County): Primary victim (powerless/trapped) — cannot exit the debt structure, cannot default without central permission, cannot raise alternative revenue without tax reform authority. Bears maximum extraction.
 *   - Provincial Government: Secondary victim (moderate/constrained) — has more fiscal capacity and political leverage than prefecture, but still trapped by the same land finance dependency. Experiences mixed coordination and extraction.
 *   - Central Government: Primary beneficiary (institutional/arbitrage) — achieved political stability through infrastructure-led growth without direct fiscal burden; can exit by allowing defaults or providing bailouts; controls the rules. Experiences the constraint as coordination.
 *   - State-Owned Banks: Institutional actor (institutional/constrained) — required to lend to LGFVs by policy directive; hold NPLs that cannot be recognized without triggering crisis; benefit from interest income but bear credit risk. Mixed position.
 *   - Future Taxpayers: Victim (powerless/trapped) — will bear the fiscal cost of debt resolution through reduced services, higher taxes, or inflation; have no voice in current policy; cannot exit. Generational extraction.
 *   - Analytical Observer: Sees the full structure (analytical/analytical) — genuine coordination function (infrastructure investment) embedded in extraction mechanism (fiscal burden shifted to future, local governments trapped by central design).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(lgfv_debt_trap, 0.58).
domain_priors:suppression_score(lgfv_debt_trap, 0.72).
domain_priors:theater_ratio(lgfv_debt_trap, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(lgfv_debt_trap, extractiveness, 0.58).
narrative_ontology:constraint_metric(lgfv_debt_trap, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(lgfv_debt_trap, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(lgfv_debt_trap, tangled_rope).
narrative_ontology:human_readable(lgfv_debt_trap, "Local Government Financing Vehicle Debt Trap").
narrative_ontology:topic_domain(lgfv_debt_trap, "political_economy/development_economics/fiscal_systems").

domain_priors:requires_active_enforcement(lgfv_debt_trap).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(lgfv_debt_trap, central_government_political_stability).
narrative_ontology:constraint_beneficiary(lgfv_debt_trap, state_owned_banks).
narrative_ontology:constraint_beneficiary(lgfv_debt_trap, construction_sector_employment).
narrative_ontology:constraint_victim(lgfv_debt_trap, local_government_fiscal_capacity).
narrative_ontology:constraint_victim(lgfv_debt_trap, household_wealth_allocation).
narrative_ontology:constraint_victim(lgfv_debt_trap, future_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

constraint_indexing:constraint_classification(lgfv_debt_trap, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

constraint_indexing:constraint_classification(lgfv_debt_trap, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(lgfv_debt_trap, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

constraint_indexing:constraint_classification(lgfv_debt_trap, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

constraint_indexing:constraint_classification(lgfv_debt_trap, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

constraint_indexing:constraint_classification(lgfv_debt_trap, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(lgfv_debt_trap_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(lgfv_debt_trap, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(lgfv_debt_trap, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(lgfv_debt_trap, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(lgfv_debt_trap_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts from local fiscal capacity and future taxpayers while benefiting central political stability and current-period growth. The extraction is substantial but not maximal — some infrastructure investment had genuine returns, and the coordination function was real during the growth phase. The value reflects that roughly 40-60% of LGFV debt represents extraction (unproductive investment, corruption, debt service burden exceeding asset value) while 40-60% represents legitimate coordination cost. Suppression (0.72): High. Local governments face severe barriers to exit: cannot default without central permission (Budget Law Article 35), cannot raise alternative revenue without tax authority, cannot reduce expenditure below mandated service levels, face career punishment for fiscal crisis. The suppression is structural and enforced. Theater ratio (0.65): Moderate-high. Significant performative content includes: loan classification games (evergreening NPLs through refinancing), asset valuation fiction (land collateral marked at peak prices), fiscal reporting theater (off-balance-sheet debt not counted in official statistics), and regulatory forbearance (banks not required to recognize losses). The theater has increased as the gap between reported and actual fiscal health has widened.
 *
 * PERSPECTIVAL GAP:
 *   The central government sees coordination (Rope) — the LGFV system solved the legitimate problem of financing infrastructure investment during rapid urbanization, and the political stability benefits justify the fiscal costs. Local governments see extraction (Snare) — they are trapped in a debt structure designed by the center, with obligations they cannot meet and no exit path. Provincial governments see mixed coordination and extraction (Tangled Rope) — the system enabled real development but at unsustainable fiscal cost. State-owned banks see tangled rope — required to lend by policy but bearing credit risk. Future taxpayers see pure extraction (Snare) — they bear the fiscal burden of current consumption with no voice. The analytical observer sees tangled rope — genuine coordination function (infrastructure investment) embedded in extraction mechanism (fiscal burden shifted intertemporally and hierarchically). The perspectival gap reveals how fiscal federalism design can appear as coordination from above and extraction from below.
 *
 * DIRECTIONALITY LOGIC:
 *   Central government is the primary beneficiary: achieved infrastructure-led growth and political stability without direct fiscal burden, retains policy control, can exit by allowing defaults or providing bailouts. Derives low d (beneficiary + arbitrage exit) → low/negative chi → experiences constraint as coordination. Local governments are primary victims: trapped by debt obligations with collapsed revenue source, cannot exit without central permission, bear the fiscal burden. Derives high d (victim + trapped exit) → high chi → experiences constraint as extraction/snare. Provincial governments occupy middle position: more fiscal capacity than prefecture but still dependent on land finance, some political leverage but constrained by central rules. Derives moderate d (victim + constrained exit) → moderate chi → experiences mixed coordination and extraction (tangled_rope). State-owned banks are institutional victims with constrained exit: required to lend by policy directive, hold NPLs that cannot be recognized, benefit from interest income but bear credit risk. Derives moderate d (mixed position + constrained exit) → moderate chi → tangled_rope. Future taxpayers are powerless victims: will bear fiscal cost through reduced services or higher taxes, have no voice in current policy, cannot exit. Derives high d (victim + trapped exit) → high chi → snare.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that the LGFV system has BOTH genuine coordination function (infrastructure investment during rapid urbanization) AND asymmetric extraction (fiscal burden shifted to local governments and future taxpayers). The coordination function was real: China built world-class infrastructure in two decades, enabling productivity growth and poverty reduction. The extraction is also real: local governments are trapped in unsustainable debt, future taxpayers will bear the cost, and much LGFV investment was unproductive (ghost cities, redundant infrastructure). The tangled_rope classification captures this duality. The constraint is NOT pure extraction (snare) because the infrastructure investment had genuine returns during the growth phase. It is NOT pure coordination (rope) because the fiscal burden is asymmetrically distributed and the debt trap is structural. The mandatrophy resolution is that both functions coexist — the system coordinated infrastructure investment while extracting from local fiscal capacity.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    central_bailout_threshold,
    'At what debt-to-GDP threshold does the central government''s implicit guarantee become an explicit bailout, converting the constraint from tangled_rope to rope for local governments?',
    'Historical analysis of central intervention patterns; comparison with 1990s SOE debt restructuring; monitoring of State Council directives on local debt resolution',
    'If threshold is low (bailout occurs at 80-100% local debt-to-revenue): constraint becomes coordination mechanism (Rope from local perspective). If threshold is high (no bailout until systemic crisis): constraint remains extraction mechanism (Snare persists).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(central_bailout_threshold, empirical, 'Central government bailout threshold for local debt').

omega_variable(
    land_finance_substitution,
    'Can property tax reform or central transfer increases substitute for land sales revenue at sufficient scale to break the debt trap?',
    'Pilot program outcomes in Chongqing and Shanghai property tax zones; analysis of central transfer elasticity; political economy constraints on tax reform',
    'If substitution is feasible: constraint has sunset (Scaffold from reform perspective). If substitution is blocked by political constraints: extraction mechanism persists indefinitely.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(land_finance_substitution, conceptual, 'Feasibility of land finance revenue substitution').

omega_variable(
    npl_recognition_timing,
    'When will non-performing LGFV loans be recognized on bank balance sheets, and will recognition trigger systemic crisis or managed resolution?',
    'Bank stress test results; regulatory forbearance policy changes; comparison with Japanese NPL recognition timeline (1990s); monitoring of loan classification standards',
    'If recognition is delayed indefinitely: theater_ratio remains high, extraction continues hidden. If recognition triggers crisis: constraint converts to acute political problem requiring central intervention.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(npl_recognition_timing, empirical, 'Timing and consequences of NPL recognition').

omega_variable(
    demographic_revenue_floor,
    'Does demographic decline establish a permanent floor below which land sales revenue cannot recover, making the debt trap structurally irreversible?',
    'Cohort analysis of homebuying population; urbanization saturation modeling; comparison with Japanese post-1990 land price trajectory',
    'If demographic floor exists and is below debt service requirements: constraint becomes Mountain (irreversible structural limit). If floor is above requirements or demographics stabilize: constraint remains policy-contingent.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(demographic_revenue_floor, empirical, 'Whether demographic decline creates irreversible revenue floor').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(lgfv_debt_trap, 2008, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lgfv_tr_t0, lgfv_debt_trap, theater_ratio, 0, 0.35).
narrative_ontology:measurement(lgfv_tr_t7, lgfv_debt_trap, theater_ratio, 7, 0.5).
narrative_ontology:measurement(lgfv_tr_t14, lgfv_debt_trap, theater_ratio, 14, 0.65).

% Extraction over time
narrative_ontology:measurement(lgfv_be_t0, lgfv_debt_trap, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lgfv_be_t7, lgfv_debt_trap, base_extractiveness, 7, 0.51).
narrative_ontology:measurement(lgfv_be_t14, lgfv_debt_trap, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(lgfv_debt_trap, resource_allocation).
narrative_ontology:affects_constraint(lgfv_debt_trap, real_estate_ponzi_collapse).
narrative_ontology:affects_constraint(lgfv_debt_trap, local_government_service_degradation).

% DUAL FORMULATION NOTE:
% The LGFV debt trap is downstream of two mountain constraints: marginal_product_of_capital_collapse (investment returns declining below debt service cost) and demographic_irreversibility (homebuying cohort shrinking permanently). These upstream constraints establish structural limits that convert the LGFV mechanism from self-reinforcing growth cycle to debt trap. The LGFV constraint has its own extractiveness (0.58) reflecting the fiscal burden distribution; the upstream constraints have their own extractiveness reflecting the irreversibility of the underlying economic conditions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(lgfv_debt_trap, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
