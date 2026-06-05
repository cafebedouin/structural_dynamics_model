% ============================================================================
% CONSTRAINT STORY: nasa_faster_better_cheaper
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_nasa_faster_better_cheaper, []).

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
 *   constraint_id: nasa_faster_better_cheaper
 *   human_readable: The "Faster, Better, Cheaper" (FBC) Management Paradigm
 *   domain: political/economic/technological
 *
 * SUMMARY:
 *   The 'Faster, Better, Cheaper' (FBC) management paradigm was introduced by
 *   NASA administrator Daniel Goldin in 1992 as a response to budget
 *   constraints and Cold War-era congressional pressure to demonstrate
 *   government efficiency. The philosophy aimed to increase mission frequency
 *   and cut costs by accepting elevated technical risk and compressed
 *   development schedules. For mission teams and junior scientists, FBC
 *   created a structural trap: the only path to aerospace career advancement
 *   required accepting impossible timelines and insufficient budgets. For
 *   budget-constrained administrations and Congress, FBC was a coordination
 *   solution that enabled maintaining NASA's mission tempo without budget
 *   increases. The paradigm exhibits all features of a Snare: high extraction
 *   (schedule and risk pushed downstream to engineers), high suppression
 *   (career dependence on NASA contracts creates lock-in), and significant
 *   theater (the 'faster, better, cheaper' messaging obscured the true cost
 *   redistribution). The epistemic commons bore costs through premature
 *   mission launches and high-profile failures (Mars Polar Lander 1999, Mars
 *   Climate Orbiter 1999) that cascaded through planetary science research.
 *   By 1999-2000, FBC had been substantially abandoned after two major
 *   mission failures within months of each other, revealing the constraint's
 *   instability when failures became undeniable.
 *
 * KEY AGENTS:
 *   - Mission Team Engineers: Primary victims (powerless/trapped) — face compressed schedules, elevated risk, and no exit option without leaving aerospace sector.
 *   - Junior Scientists: Secondary victims (moderate/constrained) — early-career researchers dependent on mission success; schedule pressure undermines thorough analysis.
 *   - Scientific Integrity / Epistemic Commons: Victim (powerless/trapped) — abstract collective good bearing cost of premature claims and mission failures.
 *   - Budget-Constrained Political Administration: Primary beneficiary (institutional/arbitrage) — solves political problem of 'more with less' without cost to political system.
 *   - Congress / Budget Gatekeepers: Secondary beneficiary (institutional/arbitrage) — can claim government efficiency while maintaining NASA's public mission profile.
 *   - Aerospace Contractors: Organized victims (organized/constrained) — forced to accept FBC terms through competitive bidding; cost pressure flows to subcontractors and labor.
 *   - Analytical Observer: Observes Tangled Rope structure — real coordination function (political budget constraint solution) combined with real extraction (time and risk redistribution).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(nasa_faster_better_cheaper, 0.58).
domain_priors:suppression_score(nasa_faster_better_cheaper, 0.68).
domain_priors:theater_ratio(nasa_faster_better_cheaper, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(nasa_faster_better_cheaper, extractiveness, 0.58).
narrative_ontology:constraint_metric(nasa_faster_better_cheaper, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(nasa_faster_better_cheaper, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(nasa_faster_better_cheaper, snare).
narrative_ontology:human_readable(nasa_faster_better_cheaper, "The \"Faster, Better, Cheaper\" (FBC) Management Paradigm").
narrative_ontology:topic_domain(nasa_faster_better_cheaper, "political/economic/technological").

domain_priors:requires_active_enforcement(nasa_faster_better_cheaper).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(nasa_faster_better_cheaper, nasa_budget_gatekeepers).
narrative_ontology:constraint_beneficiary(nasa_faster_better_cheaper, political_administrations).
narrative_ontology:constraint_victim(nasa_faster_better_cheaper, mission_teams).
narrative_ontology:constraint_victim(nasa_faster_better_cheaper, scientific_integrity).
narrative_ontology:constraint_victim(nasa_faster_better_cheaper, junior_engineers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MISSION TEAM ENGINEER (SNARE) — Trapped by career and institutional dependence on NASA funding. Must accept compressed timelines, reduced budgets, and elevated technical risk or exit the aerospace sector entirely. No collective bargaining power. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.81. Pure extraction: compressed schedules extract labor without corresponding compensation; elevated risk without risk premium.
constraint_indexing:constraint_classification(nasa_faster_better_cheaper, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: JUNIOR SCIENTIST (SNARE) — Constrained by early-career dependence on mission success for publication and reputation. FBC pressure to launch quickly undermines thorough analysis and validation. Career cost of speaking up about schedule inadequacy is high (appears risk-averse, not team-oriented). d≈0.78, f(d)≈1.08, σ=1.0 → χ≈0.63. Extraction: time pressure converts to credential extraction without compensation.
constraint_indexing:constraint_classification(nasa_faster_better_cheaper, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SCIENTIFIC INTEGRITY / EPISTEMIC COMMONS (SNARE) — Cannot exit; bears cost of premature claims, insufficient validation, and mission failures attributed to 'pilot errors' rather than systemic schedule pressure. Mars Polar Lander and Climate Orbiter failures cascaded through the research community. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.82. Maximum extraction: abstract collective good bears full cost; no compensation mechanism.
constraint_indexing:constraint_classification(nasa_faster_better_cheaper, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: BUDGET-CONSTRAINED POLITICAL ADMINISTRATION (ROPE) — Experiences FBC as a coordination solution to the political problem: how to maintain NASA's mission frequency and public visibility within flat or declining budgets. FBC enables politicians to claim 'getting more done with less' — a pure coordination function. Exit via arbitrage: can defund NASA entirely if FBC fails, shifting resources elsewhere. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.06. Net beneficiary; negative effective extraction means the constraint subsidizes this agent.
constraint_indexing:constraint_classification(nasa_faster_better_cheaper, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: AEROSPACE CONTRACTOR ECOSYSTEM (SNARE) — Organizational level: constrained by government contract structure and competition pressure. Individual contractors bidding for missions must accept FBC terms to remain competitive; those who refuse are excluded. Cost reduction pressure flows downstream to subcontractors and labor. d≈0.70, f(d)≈1.02, σ=1.0 → χ≈0.59. Extraction mechanism: competitive bidding under FBC terms forces cost externalization (schedule compression, quality shortcuts, labor absorption).
constraint_indexing:constraint_classification(nasa_faster_better_cheaper, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / CIVILIZATIONAL VIEW (TANGLED ROPE) — FBC exhibits genuine coordination function (enabling high-frequency missions on constrained budgets) AND asymmetric extraction (time and risk pushed downstream to engineers and scientific validation). The constraint is not pure extraction (snare) because it *does* solve a real political-budgetary problem. It is not pure coordination (rope) because solving that problem requires extractive redistribution. From a civilizational perspective, FBC is revealed as Tangled Rope: the coordination benefit flows to budget-constrained administrations; the extraction cost flows to engineers, junior scientists, and epistemic reliability. χ depends heavily on observer's power position.
constraint_indexing:constraint_classification(nasa_faster_better_cheaper, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nasa_faster_better_cheaper_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nasa_faster_better_cheaper, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nasa_faster_better_cheaper, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(nasa_faster_better_cheaper, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(nasa_faster_better_cheaper_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High. FBC systematically extracted schedule compression and technical risk acceptance from mission teams without corresponding compensation or risk premium. The 1992-1999 period shows accelerating extraction: early FBC missions (1992-1995) had more schedule slack; by 1997-1999, timelines were severely compressed (Mars Polar Lander: 3-year development cycle). The metric reflects that 'faster' was the binding constraint—not achieved through innovation but through reduction in validation time, contingency buffers, and testing cycles. Suppression (0.68): High. Career lock-in (leaving aerospace requires restarting in a new field at mid-career), institutional dependence on NASA contracts, and competitive pressure to accept FBC terms created severe barriers to exit or resistance. Individual engineers had no leverage to reject schedules; collective resistance was suppressed through management culture emphasizing 'can-do' attitude and team loyalty. Theater ratio (0.64): Moderate-high. The 'faster, better, cheaper' messaging created theater by naturalizing cost redistribution as management innovation. The paradigm presented a false choice: either accept FBC or lose NASA funding entirely. In reality, Congressional budget constraints were political choices, not immutable laws. The theater obscured that 'cheaper' meant cheaper *to the government*, not cheaper overall—costs were externalized to labor time and mission risk.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a sharp perspectival divide. Mission-level actors (engineers, junior scientists, spacecraft teams) perceive FBC as a Snare: trapped, impossible constraints, elevated risk without compensation. Budget-level actors (administrations, Congress, NASA leadership) perceive FBC as a Rope or even a positive coordination mechanism: a way to maintain mission frequency and public engagement within political budget constraints. The epistemic commons has no voice in the perspectival gap—it is purely a victim with no perspective to declare. The analytical observer (perspective 6) resolves the gap by recognizing that both perceptions are structurally accurate: FBC *is* a solution to the political coordination problem (Rope function) *and* an extraction mechanism that shifts time and risk downstream (Snare structure). The gap is not a classification disagreement but a revelation that the constraint genuinely is Tangled Rope: solving one problem (political budgets) by creating another (team capacity and risk). The Mars Polar Lander and Climate Orbiter failures made the extraction visible: the manifest failures were crew errors or design flaws attributed at the time, but post-failure analysis revealed systematic shortcuts in validation and testing driven by FBC schedules.
 *
 * DIRECTIONALITY LOGIC:
 *   Budget-constrained administration: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Solves their primary problem (maintaining mission frequency within budget); can exit via defunding NASA entirely. Net beneficiary. Mission team engineers: Victim + trapped → d≈0.92, f(d)≈1.40. Locked in by career dependence; absorb all schedule and risk extraction; no exit without sector departure. Maximum extraction experience. Junior scientists: Victim + constrained → d≈0.78, f(d)≈1.08. Early-career lock-in; can exit by changing fields but at significant career cost. Moderate-high extraction. Epistemic commons: Victim + trapped → d≈0.95, f(d)≈1.42. Permanently trapped; bears cost of validation shortcuts and premature claims; no compensation mechanism. Maximum extraction. Aerospace contractors: Organized + constrained → d≈0.70, f(d)≈1.02. Constrained by competitive bidding under FBC terms; can exit by not bidding, but exit means business loss. Moderate-high extraction. The directionality structure reveals why FBC persisted despite engineer dissent: the beneficiaries (political agents) had sufficient power and exit options to maintain the constraint despite engineer complaints, until mission failures made suppression unsustainable.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY UNRESOLVED AT ε=0.58: The constraint exhibits genuine Tangled Rope structure—it solves a real political coordination problem (maintaining NASA's mission frequency under Congressional budget constraints) while simultaneously imposing asymmetric extraction costs (schedule compression, risk elevation, labor time absorption). The mandatrophy appears at first glance to be a classification ambiguity: is FBC 'really' a Snare (pure extraction) or a Tangled Rope (mixed coordination/extraction)? The resolution is structural: FBC IS Tangled Rope because the beneficiary (budget-constrained administration) genuinely benefits from the coordination function (frequency-on-budget), and the victims genuinely bear extraction costs (time, risk, epistemic reliability). This is not an observation-dependent artifact; it is a feature of the constraint's design. The false choice—'either FBC or no missions'—was itself part of the extraction mechanism: it prevented asking whether other budget/frequency tradeoffs were possible. A true Snare (pure extraction) would have no coordination function; FBC has one. A true Rope (pure coordination) would have no asymmetric victims; FBC creates clear victims. The constraint persisted until failure cascades made suppression unsustainable (Mars Polar Lander, Climate Orbiter, both 1999). At that point, the mandatrophy shifted: the public and Congress could no longer accept the claimed benefit (more missions) without acknowledging the extraction cost (mission failures from schedule pressure). The paradigm was subsequently abandoned, not because engineers convinced management it was a Snare, but because mission failures made the Tangled Rope structure undeniable. Modern NASA has not resolved the underlying political constraint (Congressional budget pressure) but has returned to longer development cycles and more testing—a Rope-like solution that trades mission frequency for reduced extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    schedule_compression_necessity,
    'Was the FBC schedule compression technologically necessary or a political choice to demonstrate ''more with less'' to Congress?',
    'Comparative analysis: mission timelines under FBC vs pre-FBC equivalent missions; analysis of which schedule elements were genuinely constrained by physics vs administrative choice.',
    'If necessary: FBC is coordination (Rope) trying to balance unavoidable constraints. If political choice: FBC is pure extraction (Snare) disguised as efficiency.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(schedule_compression_necessity, empirical, 'Whether schedule compression was technologically necessary').

omega_variable(
    cost_avoidance_vs_cost_reduction,
    'Did FBC achieve true cost reduction or merely defer/externalize costs (schedule pressure absorbed by labor, premature launches increasing later mission costs)?',
    'Total-cost analysis: direct mission costs + cost of failures + rework + labor cost externalization over 10-year period; comparison to counterfactual budgets without FBC pressure.',
    'If true reduction: FBC exhibits some efficiency gain. If cost deferral: FBC is pure rent-seeking (Snare) extracting labor time without production gain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_avoidance_vs_cost_reduction, empirical, 'Whether FBC achieved actual cost reduction').

omega_variable(
    mission_failure_causation,
    'To what degree were Mars Polar Lander and Climate Orbiter failures causally attributable to FBC schedule/budget pressure vs other technical/organizational factors?',
    'Forensic analysis of failure investigation reports; timeline reconstruction; interviews with project leadership about schedule decision-making; comparison of risk assessment practices under FBC vs conventional management.',
    'High causal attribution: FBC is a snare with measurable victim harms. Low causal attribution: FBC schedule pressure is a background factor but not the dominant failure mode.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mission_failure_causation, empirical, 'Causal contribution of FBC pressure to high-profile mission failures').

omega_variable(
    labor_compensation_equivalence,
    'Were engineers and scientists working under FBC compensated at rates equivalent to the time/stress/risk absorbed, or did the paradigm extract labor value?',
    'Wage analysis: FBC-era aerospace engineer salaries vs pre-FBC and contemporary non-FBC sectors; adjustment for hours worked and stress-related health impacts; comparison of promotion velocities and career outcomes.',
    'If equivalently compensated: extraction may be lower than χ=0.81 suggests. If undercompensated: extraction is confirmed; labor extraction is the primary mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_compensation_equivalence, empirical, 'Whether labor was compensated at rates equivalent to demands').

omega_variable(
    political_sustainability_threshold,
    'What level of mission failure or public scandal forces even a budget-constrained administration to abandon FBC-style paradigms?',
    'Historical analysis: failure rates vs political response; analysis of NASA administrator and Congressional turnover correlated with mission losses; tracking of budget pressure before/after high-visibility failures.',
    'If threshold is low: FBC is unstable (Scaffold with very short sunset). If threshold is high: FBC persists despite failures (Snare with strong institutional suppression).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_sustainability_threshold, empirical, 'Political sustainability threshold for FBC-style paradigms').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nasa_faster_better_cheaper, 1992, 1999).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fbc_tr_t0, nasa_faster_better_cheaper, theater_ratio, 0, 0.45).
narrative_ontology:measurement(fbc_tr_t4, nasa_faster_better_cheaper, theater_ratio, 4, 0.58).
narrative_ontology:measurement(fbc_tr_t7, nasa_faster_better_cheaper, theater_ratio, 7, 0.64).

% Extraction over time
narrative_ontology:measurement(fbc_be_t0, nasa_faster_better_cheaper, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fbc_be_t4, nasa_faster_better_cheaper, base_extractiveness, 4, 0.48).
narrative_ontology:measurement(fbc_be_t7, nasa_faster_better_cheaper, base_extractiveness, 7, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(nasa_faster_better_cheaper, resource_allocation).
narrative_ontology:affects_constraint(nasa_faster_better_cheaper, aerospace_schedule_estimation).
narrative_ontology:affects_constraint(nasa_faster_better_cheaper, nasa_risk_management_culture).
narrative_ontology:affects_constraint(nasa_faster_better_cheaper, government_efficiency_theater).

% DUAL FORMULATION NOTE:
% FBC is structurally distinct from its downstream effects (mission failures, erosion of NASA's risk management culture). The paradigm ε=0.58 reflects the schedule/risk extraction mechanism. Upstream constraint (Congressional budget politics) is a separate story that would have different ε. Downstream constraints (cascade effects on planetary science mission planning) inherit FBC's directionality structure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(nasa_faster_better_cheaper, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
