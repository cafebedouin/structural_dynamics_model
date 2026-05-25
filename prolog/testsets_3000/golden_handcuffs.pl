% ============================================================================
% CONSTRAINT STORY: golden_handcuffs
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_golden_handcuffs, []).

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
 *   constraint_id: golden_handcuffs
 *   human_readable: Golden Handcuffs (Vesting-Based Retention)
 *   domain: economic/social
 *
 * SUMMARY:
 *   Golden handcuffs represent a fundamental tension in modern corporate
 *   compensation: they genuinely solve a coordination problem (aligning
 *   employee and firm incentives, reducing costly turnover) while
 *   simultaneously extracting value from employees through immobility and
 *   wage suppression. The constraint exhibits classic tangled rope structure:
 *   a coordination mechanism with asymmetric extraction. Employees in the
 *   vesting window face reduced mobility and cannot negotiate effectively
 *   during cliff events; firms capture retention benefits without reciprocal
 *   obligation. The constraint's extractiveness (0.52) reflects that
 *   suppression is significant (0.58) — employees cannot exit without
 *   forfeiting unvested compensation — but the coordination function is real:
 *   equity grants do align incentives and do reduce turnover. Theater ratio
 *   (0.48) indicates moderate performative content: equity compensation
 *   marketing ('you're building wealth') amplifies what is often a wage
 *   reduction (equity replaces cash) that becomes visible only in retrospect.
 *   The biographical time horizon (4-10 year vesting) creates a demographic
 *   trap: employees entering at age 25-30 are locked in until 35-40,
 *   precisely when career alternatives are richest. Vesting schedules
 *   frequently reset on promotion, creating perpetual traps across entire
 *   career tenure.
 *
 * KEY AGENTS:
 *   - Trapped Employee: Primary victim (powerless/trapped) — bears full opportunity cost of immobility; cannot negotiate during vesting window; forfeits unvested equity on exit
 *   - Employer Firm: Primary beneficiary (institutional/arbitrage) — captures retention benefit, predictable tenure, reduced hiring/training costs; can adjust equity schedules and timing
 *   - Mid-Career Professional: Secondary actor (moderate/constrained) — has some mobility but faces meaningful exit cost; experiences both coordination benefit and extraction
 *   - Labor Market: Organized actor (organized/constrained) — collective effect of handcuffs reduces overall labor mobility, suppresses wages during vesting windows, shifts risk to workers
 *   - HR/Compensation System: Institutional actor (institutional/arbitrage) — designs and administers vesting mechanics; maintains theater through equity narrative
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing vesting as inherent to equity compensation or capital structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(golden_handcuffs, 0.52).
domain_priors:suppression_score(golden_handcuffs, 0.58).
domain_priors:theater_ratio(golden_handcuffs, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(golden_handcuffs, extractiveness, 0.52).
narrative_ontology:constraint_metric(golden_handcuffs, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(golden_handcuffs, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(golden_handcuffs, tangled_rope).
narrative_ontology:human_readable(golden_handcuffs, "Golden Handcuffs (Vesting-Based Retention)").
narrative_ontology:topic_domain(golden_handcuffs, "economic/social").

domain_priors:requires_active_enforcement(golden_handcuffs).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(golden_handcuffs, employer_firm).
narrative_ontology:constraint_beneficiary(golden_handcuffs, executive_leadership).
narrative_ontology:constraint_victim(golden_handcuffs, trapped_employee).
narrative_ontology:constraint_victim(golden_handcuffs, labor_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VESTED EMPLOYEE (SNARE) — Trapped by unvested equity. Cannot exit without forfeiting substantial deferred compensation. Faces asymmetric penalty for departure. Maximum extraction: employee bears full opportunity cost of immobility; firm captures retention benefit without reciprocal obligation. Zero degrees of freedom during vesting window.
constraint_indexing:constraint_classification(golden_handcuffs, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: MID-CAREER PROFESSIONAL (TANGLED ROPE) — Constrained but not trapped. Vesting creates genuine incentive alignment (coordination function): employee shares in firm success, firm gains predictable tenure. But asymmetric extraction: firm can adjust equity grants, change vesting schedules, or engineer termination near cliff dates. Employee has some negotiating power but exits carry meaningful cost. Balanced mix of coordination benefit and extraction.
constraint_indexing:constraint_classification(golden_handcuffs, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EMPLOYER FIRM (ROPE) — Pure coordination function from firm perspective. Vesting aligns incentives, reduces costly turnover, enables long-term planning. Firm experiences the constraint as solving a collective action problem: without vesting, high-talent employees would exit post-project. Firm has full arbitrage options: can adjust grants, time vesting schedules, use equity as strategic tool. Net beneficiary — extraction flows toward firm, not from it.
constraint_indexing:constraint_classification(golden_handcuffs, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LABOR MARKET (TANGLED ROPE) — Organized perspective across multiple firms and sectors. Golden handcuffs solve coordination: firms collectively achieve lower churn and higher retention for key talent. But generates systematic extraction: labor mobility decreases, workers accept lower wages during vesting periods, aggregate bargaining power declines. Labor cannot exit the system; must navigate vesting schedules across job transitions. Active enforcement by equity compensation practices and cliff-vesting mechanics.
constraint_indexing:constraint_classification(golden_handcuffs, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STOCK OPTION TRADITION (PITON) — Historical perspective. Golden handcuffs emerged as genuine coordination mechanism in 1980s-90s startup culture: align engineers with firm success when cash was scarce. Functionality has atrophied as equity grants became standard compensation theater. Vestings are often reset on promotion, creating perpetual traps. Theater ratio (0.48) reflects that vesting now serves performative role — 'equity story' in recruitment — alongside coordination. Tradition persists through inertia despite rising dysfunction.
constraint_indexing:constraint_classification(golden_handcuffs, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — Risk of false summit. Temptation to naturalize vesting as inherent to equity compensation, labor economics, or capital structure ('equity vesting is just how incentive alignment works'). But vesting is a contingent institutional design choice, not a law of nature. Other mechanisms exist: cash retention bonuses, profit-sharing, equity with immediate vesting plus malus clauses. The analytical frame reveals vesting as constructed constraint, not natural law. Engine false-summit detector will flag this.
constraint_indexing:constraint_classification(golden_handcuffs, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(golden_handcuffs_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(golden_handcuffs, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(golden_handcuffs, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(golden_handcuffs, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(golden_handcuffs, TR),
    TR >= 0.70.

:- end_tests(golden_handcuffs_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. Vesting genuinely solves coordination (alignment, reduced turnover) but the extraction is real and measurable: (1) employees accept lower wages during vesting periods (equity replaces 20-30% of cash); (2) employees cannot exit to better opportunities without forfeiting earned equity; (3) firms engineer terminations near cliff dates (e.g., layoffs 2 months before 4-year cliff) to avoid vesting payments. The value is not as high as pure snares (0.66+) because the coordination function is genuine and employees do ultimately benefit if they stay. Suppression (0.58): Moderate-high. Significant barriers to mobility: (1) unvested equity is forfeited on exit; (2) cash wages are suppressed during vesting; (3) vesting schedules are standardized (4-year cliff common), reducing employee negotiating power; (4) cliff events create strategic termination vulnerabilities. Suppression is not absolute (employees can still exit, taking the loss) but is substantial. Theater ratio (0.48): Moderate. Performative content includes: (1) 'equity story' in recruitment that obscures wage suppression; (2) annual vesting ceremonies framed as 'earning wealth' (performance theater); (3) RSU marketing that emphasizes upside potential while minimizing downside risk transfer. Theater has increased as equity compensation has become standardized industry practice — fewer employees now perceive equity grants as genuinely extraordinary retention mechanisms and more as wage structure.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a clear and measurable perspectival gap driven by power and exit options. The trapped employee (powerless/trapped) sees pure snare: they cannot exit, face penalties for exit, and lack negotiating power. The mid-career professional (moderate/constrained) sees tangled rope: they have some exit options but face costs; they experience both coordination benefit (equity does align) and extraction (suppressed wages, reduced mobility). The employer firm (institutional/arbitrage) sees rope: vesting solves a genuine coordination problem from their perspective, and they have full arbitrage options (adjust grants, time schedules, engineer terminations). The labor market (organized/constrained) sees tangled rope at systemic level: collective effect of handcuffs reduces labor mobility and aggregate bargaining power while solving firm-level coordination. The stock option tradition (institutional/arbitrage) sees degraded piton: vesting once served genuine coordination function; now serves performative role. The analytical observer risks seeing mountain (naturalizing vesting as inherent to equity compensation), but the structural data reveals it as constructed institutional mechanism.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is determined by structural position within the vesting constraint. Beneficiaries (firm leadership, HR) have low directionality (d ~0.10): they benefit from reduced turnover, retain flexibility to adjust vesting, and have full arbitrage options. They experience negative or minimal effective extraction. Victims (trapped employees) have high directionality (d ~0.90): they bear full opportunity cost of immobility, lose unveiled equity on exit, cannot negotiate during vesting windows. They experience maximum extraction. Moderate actors (mid-career professionals) have middle directionality (d ~0.55): they experience both coordination benefit and extraction, with meaningful but not absolute exit constraints. The organized labor market has directionality around d ~0.60: systemic effect of handcuffs reduces aggregate mobility and bargaining power, but some actors (institutional agents) benefit. The engine's derivation chain produces these d values from the beneficiary/victim declarations and exit options, generating perspectival gap in effective extractiveness (chi).
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE GATE VERIFICATION: The constraint satisfies all three canonical tangled rope gates: (1) beneficiaries declared (employer firm, executive leadership) with coordination function (incentive alignment, reduced turnover); (2) victims declared (trapped employee, labor mobility) with asymmetric extraction (forfeited equity, wage suppression, immobility); (3) active enforcement required (true: vesting schedules are designed and administered, cliff events are enforced, equity is forfeited on termination). The mandatrophy resolves by showing that golden handcuffs are genuinely hybrid: they are NOT pure extraction (snare) because the coordination function is real and measurable; they are NOT pure coordination (rope) because the extraction is real and asymmetric. The tangled rope classification prevents the false summit of naturalizing vesting as 'just how equity incentive alignment works' while also preventing the false minimization of saying 'it's just employee choice to accept equity grants.' The constraint is structural, hybrid, and properly classified only by tangled rope. The theater ratio (0.48) and its increase over time reflect that vesting's performative component has grown as equity compensation has become industry standard, but the functional component remains.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    equity_grant_reset_cycle,
    'Do equity grant resets on promotion create perpetual trap (snare dynamics) or legitimate refresh coordination cycle (rope dynamics)?',
    'Longitudinal career tracking: compare employee exit rates and wage progression for roles with reset schedules vs. flat vesting; analysis of promotional cliff timing relative to vesting windows',
    'If resets create perpetual traps: extractiveness rises to 0.65+, classification becomes snare from employee perspective. If resets are coordinated refreshes: extractiveness stays ~0.50, tangled rope holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equity_grant_reset_cycle, empirical, 'Whether equity resets create perpetual traps or legitimate coordination cycles').

omega_variable(
    alternative_retention_mechanisms,
    'Are vesting-based handcuffs necessary for retention, or would cash retention bonuses or performance-based payouts achieve same coordination with lower suppression?',
    'Comparative analysis of firms using vesting vs. cash retention: churn rates, employee satisfaction, cost to firm, wage suppression effects; randomized policy experiments where feasible',
    'If alternatives are equally effective: vesting is pure extraction mechanism (snare, extractiveness 0.65+). If vesting is necessary: coordination function is real (tangled rope sustained).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_retention_mechanisms, empirical, 'Whether vesting is necessary or substitutable by alternative mechanisms').

omega_variable(
    equity_volatility_transfer,
    'Does equity-based vesting effectively transfer firm-risk-of-failure to employee as hidden extraction cost?',
    'Analysis of employee wealth loss in equity compensation during downturns vs. equivalent periods for non-equity workers; comparison of wealth accumulation in equity-heavy vs. salary-heavy roles across full business cycles',
    'If transfer is substantial: hidden extractiveness adds 0.15-0.20 to observed rates (true extractiveness ~0.70). If transfer is modest: extractiveness estimate is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(equity_volatility_transfer, empirical, 'Magnitude of hidden extraction via equity risk transfer to employee').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(golden_handcuffs, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gh_tr_t0, golden_handcuffs, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gh_tr_t2, golden_handcuffs, theater_ratio, 2, 0.38).
narrative_ontology:measurement(gh_tr_t4, golden_handcuffs, theater_ratio, 4, 0.48).

% Extraction over time
narrative_ontology:measurement(gh_be_t0, golden_handcuffs, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(gh_be_t2, golden_handcuffs, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(gh_be_t4, golden_handcuffs, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(golden_handcuffs, resource_allocation).
narrative_ontology:affects_constraint(golden_handcuffs, executive_compensation_asymmetry).
narrative_ontology:affects_constraint(golden_handcuffs, labor_market_wage_suppression).
narrative_ontology:affects_constraint(golden_handcuffs, employee_geographic_immobility).

% DUAL FORMULATION NOTE:
% Golden handcuffs is the retention mechanism constraint; it is downstream of firm-level compensation strategy decisions and upstream of labor market sorting effects. The constraint's extractiveness is contingent on whether alternative retention mechanisms (cash bonuses, performance payouts) are feasible — this creates empirical uncertainty in the omega variables.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
