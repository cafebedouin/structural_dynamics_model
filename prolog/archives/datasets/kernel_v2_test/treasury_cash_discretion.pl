% ============================================================================
% CONSTRAINT STORY: treasury_cash_discretion
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-09
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_treasury_cash_discretion, []).

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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: treasury_cash_discretion
 *   human_readable: Treasury Cash Management Discretion Under Debt Ceiling
 *   domain: public_finance/constitutional_law/political_economy
 *
 * SUMMARY:
 *   Treasury's discretion over cash management timing under the debt ceiling
 *   creates a structural tradeoff: drawing down cash reserves earlier brings
 *   the X Date (debt ceiling exhaustion) closer but maintains higher
 *   operational flexibility; building reserves extends the X Date but reduces
 *   immediate liquidity. This discretion is downstream of the statutory
 *   ceiling constraint (mountain) but represents a distinct coordination
 *   mechanism. The constraint coordinates multiple objectives: ensuring
 *   timely federal payments, maintaining market stability, managing debt
 *   issuance schedules, and coordinating with Federal Reserve monetary
 *   operations. The discretion is genuine — Treasury must make real
 *   operational decisions about TGA levels, bill issuance timing, and cash
 *   flow management that cannot be fully specified in advance by legislation.
 *   The constraint exhibits low extraction because the discretion serves
 *   legitimate operational needs rather than systematic rent capture. Theater
 *   ratio is low because Treasury's cash management decisions have real
 *   operational consequences — TGA levels directly affect payment capacity
 *   and market liquidity. Suppression is moderate because Treasury operates
 *   within statutory constraints (the debt ceiling itself) and faces market
 *   discipline (erratic cash management would disrupt Treasury bill markets),
 *   but has substantial operational autonomy within those bounds.
 *
 * KEY AGENTS:
 *   - Treasury Department: Primary beneficiary (institutional/arbitrage) — operational discretion to manage cash flows and coordinate with markets
 *   - Financial Markets: Beneficiary (institutional/mobile) — predictable Treasury behavior coordinates expectations and debt issuance schedules
 *   - Congressional Budget Committees: Delegating authority (powerful/constrained) — benefit from avoiding operational micromanagement while retaining statutory control
 *   - Federal Payment Recipients: Dependent actors (moderate/constrained) — rely on Treasury discretion to ensure timely payments
 *   - Federal Reserve: Coordinating institution (organized/mobile) — coordinates monetary operations with Treasury cash management
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination mechanism with low extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(treasury_cash_discretion, 0.18).
domain_priors:suppression_score(treasury_cash_discretion, 0.22).
domain_priors:theater_ratio(treasury_cash_discretion, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(treasury_cash_discretion, extractiveness, 0.18).
narrative_ontology:constraint_metric(treasury_cash_discretion, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(treasury_cash_discretion, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(treasury_cash_discretion, rope).
narrative_ontology:human_readable(treasury_cash_discretion, "Treasury Cash Management Discretion Under Debt Ceiling").
narrative_ontology:topic_domain(treasury_cash_discretion, "public_finance/constitutional_law/political_economy").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(treasury_cash_discretion, treasury_department).
narrative_ontology:constraint_beneficiary(treasury_cash_discretion, financial_markets).
narrative_ontology:constraint_beneficiary(treasury_cash_discretion, executive_branch).
narrative_ontology:constraint_vindicates(treasury_cash_discretion, executive_operational_flexibility_doctrine).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TREASURY DEPARTMENT (ROPE) — Primary beneficiary with arbitrage exit. Treasury has operational discretion to manage cash balances within statutory constraints. The timing tradeoff (drawdown vs reserve building) is a genuine coordination mechanism: Treasury signals market conditions, manages payment flows, and coordinates with Federal Reserve operations. Low extraction — the discretion serves legitimate operational needs.
constraint_indexing:constraint_classification(treasury_cash_discretion, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 2: FINANCIAL MARKETS (ROPE) — Beneficiary with mobile exit. Markets benefit from Treasury's predictable cash management patterns and advance signaling of debt issuance schedules. The constraint coordinates expectations around TGA levels and bill issuance timing. Markets can exit to alternative instruments if Treasury behavior becomes erratic, but generally experience this as coordination rather than extraction.
constraint_indexing:constraint_classification(treasury_cash_discretion, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: CONGRESSIONAL BUDGET COMMITTEES (ROPE) — Constrained but not trapped. Congress delegates operational discretion to Treasury while retaining statutory authority over the debt ceiling itself. The timing discretion is a coordination mechanism that allows Treasury to manage payment flows without constant legislative micromanagement. Extraction is low because the delegation serves Congress's own interest in avoiding operational chaos.
constraint_indexing:constraint_classification(treasury_cash_discretion, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FEDERAL PAYMENT RECIPIENTS (ROPE) — Moderate power, constrained exit. Social Security recipients, federal contractors, and bondholders depend on Treasury's cash management to ensure timely payments. The discretion coordinates payment timing across diverse obligations. Low extraction — recipients benefit from Treasury's ability to smooth payment flows and avoid disruption.
constraint_indexing:constraint_classification(treasury_cash_discretion, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL RESERVE (ROPE) — Organized institutional actor with mobile exit. The Fed coordinates with Treasury on TGA management to avoid unintended monetary policy effects from large cash balance swings. The discretion is a coordination mechanism between fiscal and monetary authorities. Low extraction — the Fed benefits from predictable Treasury behavior and can adjust reserve management in response.
constraint_indexing:constraint_classification(treasury_cash_discretion, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — The timing discretion is a genuine coordination mechanism solving a real collective-action problem: how to manage government cash flows under a binding statutory constraint without constant legislative intervention. The discretion allows Treasury to optimize across multiple objectives (payment reliability, market stability, debt management efficiency) that would be impossible to specify ex ante in legislation. Extraction is low because no party is systematically disadvantaged — the discretion serves operational efficiency rather than rent extraction.
constraint_indexing:constraint_classification(treasury_cash_discretion, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(treasury_cash_discretion_tests).
:- end_tests(treasury_cash_discretion_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. Treasury's timing discretion creates modest asymmetries — Treasury has information advantages about cash flow projections and can influence the X Date within a range — but these asymmetries serve operational coordination rather than systematic rent extraction. The slight upward drift (0.12 → 0.18) reflects increasing political salience of debt ceiling episodes, which creates temptation for strategic timing manipulation, but extraction remains low because Treasury's primary incentive is operational stability rather than political leverage. Suppression (0.22): Low-moderate. Treasury operates within statutory constraints (the debt ceiling itself, appropriations law, payment prioritization restrictions) and faces market discipline, but has substantial operational autonomy within those bounds. The modest increase over time reflects growing political pressure during debt ceiling episodes, which constrains Treasury's operational flexibility. Theater ratio (0.15): Very low. Treasury's cash management decisions have direct operational consequences — TGA drawdowns affect payment capacity, debt issuance timing affects market liquidity, reserve levels affect X Date projections. The slight increase reflects growing performative aspects of X Date projections during political negotiations, but the core function remains operational rather than theatrical.
 *
 * PERSPECTIVAL GAP:
 *   All perspectives classify as rope, which is unusual but structurally accurate for this constraint. The uniformity reflects that Treasury's cash management discretion genuinely solves a coordination problem (managing complex payment flows under statutory constraints) without creating systematic extraction. The perspectival variation appears in effective extraction (chi) rather than classification type: Treasury experiences near-zero or negative extraction (the discretion is pure operational benefit), markets experience low extraction (benefit from coordination with modest information asymmetry), Congress experiences modest extraction (delegates authority but retains ultimate control), payment recipients experience modest extraction (depend on Treasury but benefit from smoothed flows), and the Fed experiences low extraction (coordinates with Treasury as peer institution). The analytical observer confirms rope classification because the discretion serves genuine operational needs, alternatives would be worse (constant legislative micromanagement or rigid cash management rules), and no party is systematically disadvantaged. The constraint's low extraction and suppression distinguish it from the upstream statutory ceiling constraint (mountain) — the ceiling itself is presented as immutable law, while the cash management discretion within the ceiling is operational coordination.
 *
 * DIRECTIONALITY LOGIC:
 *   Treasury Department is the primary beneficiary with arbitrage exit — derives d near 0.0 (full beneficiary). Treasury's operational discretion directly benefits the institution by providing flexibility to manage complex cash flows without constant legislative intervention. Financial markets are beneficiaries with mobile exit — derive d near 0.1-0.2 (strong beneficiary). Markets benefit from predictable Treasury behavior and advance signaling, and can exit to alternative instruments if Treasury becomes erratic. Congressional Budget Committees are powerful actors with constrained exit — derive d near 0.3 (modest beneficiary). Congress benefits from delegating operational details while retaining statutory authority, but is constrained by the political costs of debt ceiling crises. Federal payment recipients are moderate actors with constrained exit — derive d near 0.3-0.4 (modest beneficiary to symmetric). Recipients benefit from Treasury's ability to smooth payment flows but are constrained by dependence on federal payments. Federal Reserve is organized with mobile exit — derives d near 0.2 (beneficiary). The Fed benefits from coordinated fiscal-monetary operations and can adjust reserve management in response to Treasury actions. No agent is systematically victimized — the discretion serves coordination rather than extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that rope classification can be uniform across perspectives when the coordination function is genuine and extraction is low. The mandate (Treasury's operational discretion to manage cash flows) has not outlived its function — the discretion continues to serve legitimate operational needs. The constraint is not a false summit (mountain naturalization) because it does not claim immutability — Treasury's discretion is explicitly statutory delegation that Congress could revoke or constrain. The constraint is not tangled rope because there are no systematic victims — all parties benefit from or accept the coordination mechanism. The constraint is not scaffold because there is no sunset logic — the operational need for cash management discretion persists as long as the debt ceiling exists. The constraint is not piton because the function is not atrophied — Treasury's cash management decisions have real operational consequences. The constraint is not snare because there is no systematic extraction from trapped agents. The rope classification holds across perspectives because the structural data (low extraction, low suppression, low theater, clear beneficiaries, no victims) consistently supports coordination rather than extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    x_date_manipulation_risk,
    'Does Treasury''s discretion over cash drawdown timing create opportunities for strategic manipulation of the X Date to influence debt ceiling negotiations?',
    'Historical analysis of TGA drawdown patterns during debt ceiling episodes; correlation between Treasury cash management decisions and political negotiation timelines; comparison of X Date projections vs actual exhaustion dates',
    'If manipulation is systematic: extractiveness increases substantially and classification shifts toward tangled_rope (coordination function exists but is contaminated by strategic timing games). If manipulation is rare or absent: rope classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(x_date_manipulation_risk, empirical, 'Whether Treasury strategically manipulates X Date timing for political leverage').

omega_variable(
    extraordinary_measures_boundary,
    'Where is the boundary between legitimate cash management discretion and ''extraordinary measures'' that require explicit statutory authorization?',
    'Legal analysis of Treasury''s statutory authority; historical precedent for extraordinary measures; judicial review of Treasury actions during debt ceiling crises',
    'If boundary is clear and Treasury stays within it: rope classification holds. If boundary is ambiguous and Treasury regularly operates in gray areas: extractiveness increases as discretion becomes a source of constitutional tension.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extraordinary_measures_boundary, conceptual, 'Legal boundary between routine discretion and extraordinary measures').

omega_variable(
    market_stability_vs_political_pressure,
    'When Treasury''s operational interest in market stability conflicts with political pressure to extend the X Date, which objective dominates Treasury decision-making?',
    'Case studies of debt ceiling episodes; interviews with Treasury officials; analysis of TGA management decisions under different political conditions',
    'If market stability consistently dominates: rope classification confirmed (Treasury acts as neutral coordinator). If political pressure systematically influences timing: extractiveness increases as discretion becomes a tool of executive branch political strategy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(market_stability_vs_political_pressure, empirical, 'Whether political pressure contaminates operational cash management').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(treasury_cash_discretion, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcd_tr_t0, treasury_cash_discretion, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tcd_tr_t3, treasury_cash_discretion, theater_ratio, 3, 0.12).
narrative_ontology:measurement(tcd_tr_t6, treasury_cash_discretion, theater_ratio, 6, 0.14).
narrative_ontology:measurement(tcd_tr_t9, treasury_cash_discretion, theater_ratio, 9, 0.15).

% Extraction over time
narrative_ontology:measurement(tcd_extract_baseline, treasury_cash_discretion, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(tcd_be_t3, treasury_cash_discretion, base_extractiveness, 3, 0.14).
narrative_ontology:measurement(tcd_be_t6, treasury_cash_discretion, base_extractiveness, 6, 0.16).
narrative_ontology:measurement(tcd_be_t9, treasury_cash_discretion, base_extractiveness, 9, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(tcd_su_t0, treasury_cash_discretion, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(tcd_su_t3, treasury_cash_discretion, suppression_requirement, 3, 0.2).
narrative_ontology:measurement(tcd_su_t6, treasury_cash_discretion, suppression_requirement, 6, 0.21).
narrative_ontology:measurement(tcd_su_t9, treasury_cash_discretion, suppression_requirement, 9, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(treasury_cash_discretion, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of statutory_ceiling_vs_suspension (mountain) but represents a distinct coordination mechanism. The upstream constraint (the debt ceiling itself) is presented as immutable statutory law; the downstream constraint (Treasury's cash management discretion within the ceiling) is operational coordination. The two constraints have different extractiveness values and different structural relationships to their respective agent sets.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
