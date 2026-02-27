% ============================================================================
% CONSTRAINT STORY: golden_handcuffs
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
    constraint_indexing:directionality_override/3,
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
 *   Golden handcuffs represent a structural constraint that combines
 *   legitimate coordination (aligning employee and employer interests around
 *   long-term value creation) with asymmetric extraction (suppressing
 *   employee mobility and bargaining power through forfeiture risk). The
 *   constraint operates by deferring a portion of compensation and
 *   conditioning its receipt on continued employment. From the employer's
 *   perspective, vesting solves the canonical hold-up problem: employees
 *   would otherwise have incentive to extract rents or depart after critical
 *   projects conclude. From the employee's perspective, vesting suppresses
 *   exit options and creates path-dependent lock-in, especially for
 *   career-constrained workers with limited alternative opportunities. The
 *   constraint exhibits a perspectival range spanning all six DR types: it
 *   appears as pure extraction (Snare) to powerless employees, as a degraded
 *   coordination ritual (Piton) from a civilizational narrative view, as
 *   hybrid coordination-extraction (Tangled Rope) to mobile high-skill
 *   workers, and as pure coordination (Rope) to capital allocators. The
 *   temporal trajectory shows increasing theater ratio (from 0.42 to 0.58) as
 *   equity compensation becomes normalized and the performative
 *   wealth-narrative dominates over actual control; extractiveness has risen
 *   from 0.35 to 0.52 as vesting schedules have extended and cliff structures
 *   have tightened.
 *
 * KEY AGENTS:
 *   - Vested Employee: Primary victim (powerless/trapped) — bears forfeiture risk; structurally locked into employment relationship
 *   - Career-Constrained Cohort: Secondary victim (moderate/constrained) — geographic, family, or skill-specific immobility compounds suppression
 *   - High-Demand Technical Specialist: Secondary beneficiary-victim (powerful/mobile) — experiences genuine coordination but also extraction through vesting acceleration and cliff mechanisms
 *   - Employer / Capital Allocator: Primary beneficiary (institutional/arbitrage) — solves alignment and hold-up problems; captures sustained labor without full upfront compensation
 *   - Senior Management: Tertiary beneficiary (powerful/arbitrage) — designs vesting structures that benefit themselves while suppressing cohorts below them
 *   - Labor Collective / Union: Organized secondary actor (organized/constrained) — can negotiate vesting structures but individual lock-in reduces strike capacity
 *   - Equity Narrative Ecosystem: Institutional observer (institutional/analytical) — perpetuates performative narratives of wealth-building and prestige
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(golden_handcuffs, 0.52).
domain_priors:suppression_score(golden_handcuffs, 0.65).
domain_priors:theater_ratio(golden_handcuffs, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(golden_handcuffs, extractiveness, 0.52).
narrative_ontology:constraint_metric(golden_handcuffs, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(golden_handcuffs, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(golden_handcuffs, tangled_rope).
narrative_ontology:human_readable(golden_handcuffs, "Golden Handcuffs (Vesting-Based Retention)").
narrative_ontology:topic_domain(golden_handcuffs, "economic/social").

domain_priors:requires_active_enforcement(golden_handcuffs).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(golden_handcuffs, employer_shareholder_interests).
narrative_ontology:constraint_beneficiary(golden_handcuffs, senior_management).
narrative_ontology:constraint_victim(golden_handcuffs, vested_employee).
narrative_ontology:constraint_victim(golden_handcuffs, labor_mobility).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE VESTED EMPLOYEE (SNARE) — Employee is locked into employment relationship by unvested equity. Departure forfeits years of deferred compensation. Exit costs are catastrophic relative to marginal wage. d≈0.92, f(d)≈1.38, σ=0.9 → χ≈0.64. No alternative paths to retain compensation; suppression is structural.
constraint_indexing:constraint_classification(golden_handcuffs, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE CAREER-CONSTRAINED COHORT (SNARE) — Employees with family obligations, geographic immobility, or limited alternative opportunities experience the vesting constraint as near-total suppression of exit options. Career mobility is theoretically available but practically constrained by economic circumstances. d≈0.85, f(d)≈1.15, σ=0.9 → χ≈0.56. Effective extraction is severe but slightly lower than trapped cohort due to marginal exit paths.
constraint_indexing:constraint_classification(golden_handcuffs, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: THE HIGH-DEMAND TECHNICAL SPECIALIST (TANGLED ROPE) — Senior engineers, scientists, and executives with strong labor market alternatives experience both the vesting constraint (suppression of full compensation package during tenure) and the genuine coordination benefit (alignment of employee and company interests reduces agency costs, enables long-term project planning). Exit is mobile but costly. d≈0.48, f(d)≈0.60, σ=1.1 → χ≈0.34. Mixed: coordination function is real (shared upside), but extraction occurs through vesting acceleration and cliff structures.
constraint_indexing:constraint_classification(golden_handcuffs, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 4: THE EMPLOYER / CAPITAL ALLOCATION PERSPECTIVE (ROPE) — Employer experiences vesting as pure coordination: aligning employee incentives with long-term shareholder value, reducing quit risk during critical projects, and deferring cash outlay. No extraction from the employer's structural position; vesting is a legitimate coordination mechanism that solves hold-up problems. d≈0.08, f(d)≈-0.10, σ=1.1 → χ≈-0.06. Net beneficiary; negative effective extraction.
constraint_indexing:constraint_classification(golden_handcuffs, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: THE LABOR COLLECTIVE / UNION PERSPECTIVE (TANGLED ROPE) — Organized labor sees vesting as both a benefit (deferred compensation structure can improve total compensation packages when negotiated) and a mechanism suppressing collective bargaining power (workers are individually locked in, reducing strike capacity and wage negotiation leverage during vesting periods). d≈0.62, f(d)≈0.82, σ=0.9 → χ≈0.39. Moderate extraction due to organized exit paths and negotiating capacity.
constraint_indexing:constraint_classification(golden_handcuffs, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: THE EQUITY-AS-NARRATIVE OBSERVER (PITON) — The cultural narrative of 'golden handcuffs' as prestigious compensation is largely performative: it creates the perception of alignment and wealth-building while functioning as a labor lock-in mechanism. The theater ratio reflects this — vesting ceremonies, equity-grant announcements, and wealth-on-paper displays substitute for actual control over deferred compensation. theater_ratio=0.58 indicates substantial performative content. The equity narrative persists through institutional inertia despite eroding function as stock-based compensation becomes standard market practice. d≈0.65, f(d)≈1.00, σ=1.1 → χ≈0.58. Piton gate satisfied; underlying function (coordination) has atrophied into theatrical maintenance.
constraint_indexing:constraint_classification(golden_handcuffs, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(golden_handcuffs_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(golden_handcuffs, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(golden_handcuffs, TypeOther, context(agent_power(powerful), _, _, _)),
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
 *   Extractiveness (0.52): Moderate-high. The constraint extracts between 35-52% of deferred compensation value through forfeiture risk and suppressed exit options. The initial value (0.35) reflects the legitimate coordination function — vesting does solve real alignment problems. The increase to 0.52 reflects that as vesting schedules have extended (4-6 year cycles replacing older 2-3 year cycles) and cliff structures have tightened, the extraction component has grown relative to the coordination component. Suppression (0.65): High. Employees cannot retain vested compensation if they depart; cliff structures create discontinuities that force stay-or-lose decisions; lack of portable benefits (health insurance, 401k matching that continues vesting-locked), and information asymmetry about vesting mechanics mean many employees do not fully understand their lock-in until deployed. Theater ratio (0.58): Moderate-high and rising. The performative component has grown over the interval as equity compensation has become standard market practice. Vesting grant ceremonies, stock price updates, and 'wealth on paper' narratives substitute for actual control. Early in the constraint's lifecycle (0.42), vesting was more novel and the coordination function was more salient. As equity becomes expected (theater rising to 0.58), the narrative work intensifies to maintain perceived legitimacy of the lock-in.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximal perspectival divergence. The powerless employee (Snare, d≈0.92) sees pure extraction: their only choice is stay-locked-in or forfeit. The powerful specialist (Tangled Rope, d≈0.48) sees a genuine trade-off: vesting does align incentives and enables projects they care about, but they also experience suppression of their mobility premium. The employer (Rope, d≈0.08) sees pure coordination: vesting solves the critical problem of employee hold-up and turnover during projects. The narrativist observer (Piton, d≈0.65) sees a degraded ritual: vesting persists as performative wealth-building narrative despite atrophied coordination function. This gap is not a classification error — it reflects genuine structural differences in how different agents experience the same constraint. The gap is widest between powerless and institutional perspectives (d: 0.92 vs 0.08, Δ=0.84), indicating high classification divergence and potential for conflict.
 *
 * DIRECTIONALITY LOGIC:
 *   Vested Employee: Victim + trapped → d≈0.92, f(d)≈1.38. Forfeiture is existential; no marginal exit capacity. Career-Constrained Cohort: Victim + constrained → d≈0.85, f(d)≈1.15. Economic exits exist but are costly relative to household resources. High-Demand Specialist: Beneficiary (from alignment) + victim (from extraction) + mobile → d≈0.48, f(d)≈0.60. Mixed relationship: the constraint both enables and suppresses their position. Employer: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Structural position is net positive; vesting solves coordination problems with zero extraction to employer. Senior Management: Beneficiary + arbitrage, but with control over structure → d≈0.05, f(d)≈-0.12. Even stronger net beneficiary; can design cliffs and acceleration schedules that favor their cohort. Labor Collective: Organized + constrained → d≈0.62, f(d)≈0.82. Can negotiate but individual lock-in reduces collective leverage. The engine derives d from beneficiary/victim declarations and exit options; the override for Senior Management reflects that institutional actors designing the constraint have lower d than the base employer position.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The constraint's claimed_type is Tangled Rope, but the perspectival range (Snare → Rope) and the rising theater ratio (0.42 → 0.58) create ambiguity about whether the constraint is genuinely hybrid or whether the coordination function is atrophying into pure extraction. The mandatrophy cannot be resolved without additional empirical data on: (1) whether vesting-driven retention produces welfare improvements for locked-in employees (e.g., project completion, career development) or merely suppresses exit, and (2) whether the rising theater ratio reflects dilution of coordination function or normalization of equity markets. Three candidate resolutions: (A) If coordination function is real and welfare-improving: constraint remains Tangled Rope, and rising theater ratio reflects market maturation, not function degradation. (B) If coordination function has atrophied but vesting persists through institutional inertia: constraint should reclassify as Piton (theater ≥ 0.70 possible at interval end). (C) If vesting serves pure extraction with no genuine coordination: constraint is Snare from all perspectives, and the beneficiary/employer perspective is self-serving rationalization. The JSON declares mandatrophy_resolved: false to flag this ambiguity for downstream analysis. The Open Science Coalition structure from the verification_bottleneck example shows one resolution path: if vesting alternatives (portable equity, transparent vesting, employee-directed compensation) emerge and mature, the constraint could become Scaffold (sunset logic). This is not the current state — vesting is entrenching, not declining.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    vesting_cliff_equivalence,
    'Are cliff-vesting and continuous vesting structurally equivalent extraction mechanisms, or does cliff structure fundamentally change the constraint type?',
    'Comparative analysis of employee exit behavior at cliff vs. graded vesting schedules; measurement of post-cliff departure rates; assessment of bargaining power shift at cliff milestones',
    'If equivalent: constraint remains Tangled Rope across vesting structures. If cliffs are extraction-amplifying: cliff structures should be decomposed into separate constraint (ε≈0.68, Snare at cliffs, Tangled Rope between cliffs).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vesting_cliff_equivalence, empirical, 'Whether cliff-vesting is structurally distinct from continuous vesting').

omega_variable(
    volatility_devaluation_risk,
    'Does stock price volatility convert vesting from shared upside (Rope) to asymmetric downside (Snare) when employee cannot hedge or diversify?',
    'Analysis of volatility-adjusted effective compensation; comparison of vesting value realization vs. grant-date expectations; measurement of employee portfolio concentration and hedging capacity',
    'If high volatility + low hedge capacity: constraint becomes Snare (uninsurable risk transfer to employee). If employees can diversify: Rope/Tangled Rope classification stands.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(volatility_devaluation_risk, empirical, 'Whether stock volatility converts shared upside to asymmetric downside').

omega_variable(
    equity_standard_inevitability,
    'Is stock-based compensation becoming a structural inevitability in certain sectors (tech, biotech) such that rejection of vesting no longer constitutes a viable ''exit'' for even mobile employees?',
    'Longitudinal tracking of compensation structures across cohorts and sectors; measurement of prevalence of stock-based vs. cash compensation in adjacent firms; qualitative data on job search constraints when candidates reject equity offers',
    'If inevitable: constraint becomes Mountain (irreducible structural feature of labor market in affected sectors) for even high-demand workers. If alternatives exist: Tangled Rope/Snare classification persists.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(equity_standard_inevitability, empirical, 'Whether equity compensation has become unavoidable in certain labor markets').

omega_variable(
    retention_vs_extraction_balance,
    'What ratio of vesting-driven retention (genuine coordination) to vesting-driven suppression (extraction) characterizes the true function of the mechanism?',
    'Causal analysis of departure rates with vesting vs. without; measurement of project completion rates and continuity in vesting-locked vs. freely-mobile cohorts; assessment of whether departures prevented by vesting are welfare-improving or welfare-reducing for employee',
    'If retention benefit > suppression cost: Rope or Scaffold (justified coordination). If extraction > retention benefit: Snare or Tangled Rope (unjustified lock-in).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(retention_vs_extraction_balance, preference, 'Whether vesting''s retention function outweighs its lock-in function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(golden_handcuffs, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ghc_tr_t0, golden_handcuffs, theater_ratio, 0, 0.42).
narrative_ontology:measurement(ghc_tr_t2, golden_handcuffs, theater_ratio, 2, 0.5).
narrative_ontology:measurement(ghc_tr_t4, golden_handcuffs, theater_ratio, 4, 0.58).

% Extraction over time
narrative_ontology:measurement(ghc_be_t0, golden_handcuffs, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ghc_be_t2, golden_handcuffs, base_extractiveness, 2, 0.45).
narrative_ontology:measurement(ghc_be_t4, golden_handcuffs, base_extractiveness, 4, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(golden_handcuffs, resource_allocation).
narrative_ontology:affects_constraint(golden_handcuffs, equity_compensation_standard).
narrative_ontology:affects_constraint(golden_handcuffs, labor_mobility_suppression).
narrative_ontology:affects_constraint(golden_handcuffs, executive_compensation_escalation).

% DUAL FORMULATION NOTE:
% Golden handcuffs is the hybrid (Tangled Rope) formulation combining coordination and extraction. The upstream constraint 'equity_compensation_standard' (ε≈0.15, Rope) captures the pure coordination logic of aligning incentives. The downstream constraint 'labor_mobility_suppression' (ε≈0.68, Snare) captures the extraction component in isolation. The constraint family shows how a coordination mechanism (equity alignment) generates an extraction apparatus (vesting lock-in) as a side effect. The extraction component becomes salient and exploitable once the mechanism is established, creating a Mandatrophy state where the label 'coordination' masks the extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(golden_handcuffs, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
