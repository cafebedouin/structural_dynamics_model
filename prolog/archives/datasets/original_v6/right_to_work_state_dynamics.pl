% ============================================================================
% CONSTRAINT STORY: right_to_work_state_dynamics
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_right_to_work_state_dynamics, []).

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
 *   constraint_id: right_to_work_state_dynamics
 *   human_readable: Right-to-Work State Labor Constraint Dynamics
 *   domain: labor_law/political_economy
 *
 * SUMMARY:
 *   Right-to-work (RTW) laws prohibit union security agreements that require
 *   workers to pay union dues as a condition of employment. In RTW states,
 *   all workers benefit from union contract negotiations (wages, benefits,
 *   working conditions) but can legally opt out of dues payments. This
 *   creates a free-rider problem: rational non-union workers capture union
 *   gains without supporting union operations, which weakens union finances
 *   and political power. The constraint exhibits genuine coordination
 *   (enabling labor mobility and reducing hiring rigidity) alongside
 *   asymmetric extraction (employers and anti-union coalitions benefit from
 *   union weakening; non-union workers free-ride on union gains while facing
 *   suppressed wages from weakened collective bargaining). The constraint
 *   requires active enforcement: RTW must be continuously defended through
 *   litigation against union challenges, legislative action against pro-union
 *   efforts, and political messaging ('right to work' freedom narrative).
 *   Theater ratio remains below 0.70 because enforcement is substantive
 *   (legal, political, economic consequences) rather than purely
 *   performative. Extractiveness has increased over the 20-year interval from
 *   0.38 to 0.58 as RTW has spread to more states and as union density has
 *   declined in RTW jurisdictions, making non-union workers more vulnerable
 *   to wage suppression.
 *
 * KEY AGENTS:
 *   - Non-Union Workers in RTW States (powerless/trapped): Free-ride on union benefits but face suppressed wages from weakened collective bargaining; cannot exit labor market without relocation
 *   - Organized Labor Movement (moderate/constrained): Constrained by RTW legal framework; retains organizing capacity but suffers revenue and membership loss from free-riding
 *   - Employers in RTW States (powerful/mobile): Benefit from union weakening (lower wage floors, reduced organizing risk); have invested in RTW infrastructure but can mobile if needed
 *   - Anti-Union Political Coalition (powerful/arbitrage): Coordinates RTW expansion; can shift focus between states; benefits from union political defeat
 *   - Pro-Union Democratic Coalition (organized/constrained): Coordinates defensive effort across states; constrained by federalism and electoral cycles
 *   - Analytical Observer (analytical/analytical): Risks naturalizing RTW as inherent to labor economics rather than contingent political arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(right_to_work_state_dynamics, 0.58).
domain_priors:suppression_score(right_to_work_state_dynamics, 0.65).
domain_priors:theater_ratio(right_to_work_state_dynamics, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(right_to_work_state_dynamics, extractiveness, 0.58).
narrative_ontology:constraint_metric(right_to_work_state_dynamics, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(right_to_work_state_dynamics, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(right_to_work_state_dynamics, tangled_rope).
narrative_ontology:human_readable(right_to_work_state_dynamics, "Right-to-Work State Labor Constraint Dynamics").
narrative_ontology:topic_domain(right_to_work_state_dynamics, "labor_law/political_economy").

domain_priors:requires_active_enforcement(right_to_work_state_dynamics).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(right_to_work_state_dynamics, employers_low_wage_sectors).
narrative_ontology:constraint_beneficiary(right_to_work_state_dynamics, union_avoidant_management).
narrative_ontology:constraint_victim(right_to_work_state_dynamics, organized_labor).
narrative_ontology:constraint_victim(right_to_work_state_dynamics, non_union_workers_benefit_recipients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-UNION WORKER (SNARE) — Trapped in labor market with suppressed wages (RTW prevents collective bargaining scale) and no exit beyond relocation. Benefits from union contract negotiations are available but worker cannot force cost-sharing through union membership. Experiences pure extraction: free-ride incentive structure combines with wage suppression from weakened collective bargaining. Maximum directional extraction.
constraint_indexing:constraint_classification(right_to_work_state_dynamics, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ORGANIZED LABOR (ROPE) — Constrained by RTW legal framework but retains organizing power and voice capacity. Sees the constraint as coordination problem (how to sustain union formation when free-riding is legal). Some agency; cannot exit without abandoning members. Moderate extraction — union suffers revenue and membership loss but can still function and negotiate.
constraint_indexing:constraint_classification(right_to_work_state_dynamics, rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EMPLOYERS (TANGLED ROPE) — Genuine coordination function (RTW enables talent mobility, reduces hiring rigidity). Also asymmetric extraction benefit: wage suppression from weakened unions, reduced strike risk, lower labor organizing costs. Can relocate but have invested in RTW infrastructure; mobile exit but constrained by sunk capital. Both coordination and extraction present.
constraint_indexing:constraint_classification(right_to_work_state_dynamics, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANTI-UNION POLITICAL COALITION (TANGLED ROPE) — Coordinates political strategy (state-by-state RTW adoption) while extracting union political power and funding capacity. Arbitrage exit: can shift policy focus between states. Coordination function (RTW enables labor market fluidity narrative) plus asymmetric extraction (union political defeat and revenue loss). Active enforcement: requires sustained legislative and judicial effort.
constraint_indexing:constraint_classification(right_to_work_state_dynamics, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PRO-UNION COALITION (ROPE) — Coordinates labor advocacy across state lines. Sees RTW as pure coordination problem: how to rebuild union capacity given legal constraints. Lower extraction than union's direct view because coalition has political resources and long time horizon. Constrained by federalism and electoral cycles.
constraint_indexing:constraint_classification(right_to_work_state_dynamics, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (FALSE MOUNTAIN) — Risk perspective that naturalizes RTW as inevitable consequence of labor mobility theory or property rights ('workers should be free to not join unions'). This perspective treats the constraint as immutable law of economics. Engine detects false summit: the base properties show active enforcement requirement, beneficiaries, victims, and theater_ratio < 0.70. The 'naturalness' is ideological, not structural.
constraint_indexing:constraint_classification(right_to_work_state_dynamics, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(right_to_work_state_dynamics_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(right_to_work_state_dynamics, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(right_to_work_state_dynamics, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(right_to_work_state_dynamics, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(right_to_work_state_dynamics_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The constraint creates measurable extraction: union members subsidize non-members through contract negotiations (union labor costliness increases employer wage offers); non-members capture benefits without paying dues; employers gain negotiating leverage from union weakness. The value reflects that extraction is substantial but not total — genuine coordination benefits (mobility, hiring flexibility) exist, and unions retain some bargaining power even in RTW states. The upward trend from 0.38 to 0.58 reflects increasing RTW adoption and declining union density, which has amplified the extraction mechanism. Suppression (0.65): Moderate-high. Workers in RTW states face barriers to union formation (free-rider logic reduces organizing feasibility), limited collective bargaining coverage, and reduced worker voice. But suppression is not total — organizing remains possible and some RTW states maintain pockets of union strength. Theater ratio (0.48): Below piton threshold. Enforcement is substantive (legal prohibition has real consequences) rather than purely performative. The ratio has increased slightly as political rhetoric around 'right to work' freedom has become more theatrical, but the enforcement mechanism remains grounded in law and political power rather than empty ritual.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap is between the non-union worker (who sees pure extraction—Snare) and the employer (who sees mixed coordination and extraction—Tangled Rope). Both agree on the facts: non-union workers free-ride on union gains while facing wage suppression. But the non-union worker emphasizes the suppression (maximum experienced extraction), while the employer emphasizes the coordination benefit (labor market efficiency). This gap is diagnostic: it reveals that the constraint genuinely coordinates labor mobility while also extracting from organized labor and non-union workers. The gap between pro-union coalition (Rope) and organized labor (Rope) is subtle but important: both see coordination, but the coalition has more political distance and longer time horizon, producing lower experienced extraction. The analytical observer's false mountain is the greatest gap: naturalizing RTW as inevitable law of economics rather than recognizing it as enforced political choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from structural position. Non-union workers are victims with trapped exit options (high d → maximum f(d)). Organized labor is victimized but has organizing capacity and political resources (moderate d). Employers are beneficiaries with mobile exit options (low d → negative f(d)). Anti-union coalition is beneficiary with arbitrage exit (very low d). Pro-union coalition has political resources but is constrained by federalism (moderate d). The analytical observer at civilizational scope has analytical exit and sees all positions (d ≈ 0.72, high f(d), but context-dependent). Beneficiary/victim declarations drive the directionality computation: employers and anti-union coalition are identified as beneficiaries; organized labor and non-union workers as victims. The engine derives that beneficiaries experience low/negative χ while victims experience high χ.
 *
 * MANDATROPHY ANALYSIS:
 *   TANGLED ROPE RESOLUTION: The constraint satisfies all three required gates for Tangled Rope classification. (1) Beneficiaries declared: employers_low_wage_sectors and union_avoidant_management. (2) Victims declared: organized_labor and non_union_workers_benefit_recipients. (3) Requires active enforcement: true (RTW must be continuously defended through litigation, legislation, and political mobilization). The mandatrophy is resolved by recognizing that the constraint is neither pure coordination (Rope) nor pure extraction (Snare) but a hybrid. The coordination function (labor mobility, hiring flexibility) is genuine but asymmetrically distributed — employers gain mobility benefits while non-union workers face mobility barriers (wage suppression increases cost of geographic relocation). The extraction function (union weakening, wage suppression) is genuine but not absolute — unions retain organizing capacity and political power in some RTW states. The active enforcement requirement indicates that the hybrid equilibrium requires sustained political and legal effort to maintain; without enforcement, the constraint would shift toward either Rope (if employers retained mobility gains) or collapse (if union movement regained power). The theater ratio remains low because enforcement is substantive, not theatrical. If theater ratio were to exceed 0.70, the constraint would reclassify as Piton (degraded Tangled Rope maintained through institutional inertia).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    free_riding_equilibrium_stability,
    'Is the non-union free-rider equilibrium stable, or does it collapse without active enforcement?',
    'Historical analysis of RTW state union density trajectory post-adoption; comparison with pre-RTW baseline and control states; measurement of enforcement mechanisms (right-to-work litigation, legislative renewal frequency)',
    'If equilibrium is self-sustaining: RTW reduces to Rope (coordination around market mechanism). If equilibrium requires sustained enforcement: RTW is Snare or Tangled Rope. Current evidence suggests ongoing enforcement via political advocacy and litigation is necessary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(free_riding_equilibrium_stability, empirical, 'Stability of free-rider equilibrium without active enforcement').

omega_variable(
    wage_suppression_causation_chain,
    'Is wage suppression in RTW states caused by RTW law itself, or by confounding factors (regional economic structure, cost of living, industry composition)?',
    'Difference-in-differences analysis comparing wage trajectories in adopting vs non-adopting border counties; instrumental variable estimation using historical union density; matching analysis controlling for industry and regional factors',
    'If RTW is causal driver: extractiveness > 0.55 is justified. If RTW is spurious (regional economics drive both RTW adoption and wages): extractiveness should be lower (0.35-0.45). Affects beneficiary/victim classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_suppression_causation_chain, empirical, 'Causal attribution of wage suppression to RTW law').

omega_variable(
    coordination_gain_magnitude_claim,
    'How much genuine labor market coordination benefit does RTW actually generate (vs political mythology)?',
    'Measurement of labor mobility rates, job-switching costs, hiring velocity, and skill-matching efficiency in RTW vs union-majority states; analysis of whether coordination gains accrue to workers or solely to employers',
    'If genuine coordination gains are large: Tangled Rope classification is validated. If gains are mythological: constraint should classify as pure extraction (Snare) from non-union worker perspective. Current evidence suggests modest mobility gains concentrated at firm level, not worker level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_gain_magnitude_claim, empirical, 'Magnitude and incidence of RTW coordination benefits').

omega_variable(
    enforcement_sustainability,
    'Can RTW enforcement be sustained indefinitely without escalating political costs, or does the constraint trend toward revision?',
    'Tracking of pro-union political movements in RTW states; measurement of union organizing success rates post-RTW; analysis of whether younger cohorts show different preference patterns; longitudinal survey of worker attitudes toward RTW and unionism',
    'If enforcement costs escalate: scaffold sunset may apply. If political entrenchment is durable: Tangled Rope or Snare persists. If worker preferences shift: constraint may face reclassification within 2-3 decades.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_sustainability, empirical, 'Long-term sustainability of RTW political enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(right_to_work_state_dynamics, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rtw_tr_t0, right_to_work_state_dynamics, theater_ratio, 0, 0.35).
narrative_ontology:measurement(rtw_tr_t10, right_to_work_state_dynamics, theater_ratio, 10, 0.42).
narrative_ontology:measurement(rtw_tr_t20, right_to_work_state_dynamics, theater_ratio, 20, 0.48).
narrative_ontology:measurement(rtw_tr_t5, right_to_work_state_dynamics, theater_ratio, 5, 0.38).

% Extraction over time
narrative_ontology:measurement(rtw_be_t0, right_to_work_state_dynamics, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(rtw_be_t10, right_to_work_state_dynamics, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(rtw_be_t20, right_to_work_state_dynamics, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(rtw_be_t5, right_to_work_state_dynamics, base_extractiveness, 5, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(right_to_work_state_dynamics, resource_allocation).
narrative_ontology:affects_constraint(right_to_work_state_dynamics, union_density_decline_mechanism).
narrative_ontology:affects_constraint(right_to_work_state_dynamics, wage_suppression_regional_dynamics).

% DUAL FORMULATION NOTE:
% RTW state dynamics is downstream of the free-rider problem in collective bargaining (which has its own constraint story with different ε values reflecting theoretical vs empirical free-rider severity). RTW is also upstream of specific sectoral wage suppression effects and regional labor mobility patterns. This story focuses on the institutional enforcement mechanism and perspectival structure; downstream stories address specific sectoral and regional impacts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(right_to_work_state_dynamics, powerful, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
