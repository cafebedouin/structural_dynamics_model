% ============================================================================
% CONSTRAINT STORY: sotu_1975_ford_federal_spending_ceiling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1975_ford_federal_spending_ceiling, []).

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
 *   constraint_id: sotu_1975_ford_federal_spending_ceiling
 *   human_readable: 5% Federal Pay and Benefit Growth Cap (1975 Ford SOTU)
 *   domain: governance/fiscal_policy
 *
 * SUMMARY:
 *   The 5% federal pay and benefit growth cap announced in President Ford's
 *   January 1975 State of the Union address represents a structural
 *   decoupling of federal spending from automatic cost-of-living adjustments.
 *   The cap constrains wage growth for federal employees, military retirement
 *   pay, Social Security COLA, and food stamp adjustments — nominally unified
 *   under a single spending discipline. In reality, the constraint creates
 *   asymmetric costs: those with fixed incomes (retirees, benefit recipients)
 *   cannot negotiate exemptions, while politically powerful constituencies
 *   (defense, agriculture) obtain workarounds. The constraint combines
 *   genuine coordination function (disciplining federal budget growth) with
 *   asymmetric extraction (capping real income for powerless groups). Theater
 *   ratio rises over time as congressional overrides become routine,
 *   degrading the cap from binding constraint to performative budget symbol.
 *
 * KEY AGENTS:
 *   - Federal employees: Primary victim (powerless/trapped) — cannot exit civil service without pension loss; real wages erode when inflation exceeds 5%
 *   - Social Security recipients: Primary victim (powerless/trapped) — economically dependent; COLA cap reduces purchasing power; cannot negotiate
 *   - Military retirees: Primary victim (powerless/trapped) — career lock-in creates trapped status; real retirement income declines with cap
 *   - Treasury/Deficit control coalition: Primary beneficiary (institutional/arbitrage) — cap coordinates spending discipline; extraction flows toward treasury
 *   - Federal employee unions: Moderate power (moderate/constrained) — can negotiate within cap framework but cannot escape it; see mixed coordination and extraction
 *   - Congress: Organized actor (organized/constrained) — uses cap as budget discipline mechanism but faces political pressure to override; constrained by electoral incentives
 *   - Benefit recipient advocacy groups: Weak organizing (moderate/constrained) — can pressure for exceptions but lack leverage of federal employee unions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1975_ford_federal_spending_ceiling, 0.52).
domain_priors:suppression_score(sotu_1975_ford_federal_spending_ceiling, 0.58).
domain_priors:theater_ratio(sotu_1975_ford_federal_spending_ceiling, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1975_ford_federal_spending_ceiling, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1975_ford_federal_spending_ceiling, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sotu_1975_ford_federal_spending_ceiling, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1975_ford_federal_spending_ceiling, tangled_rope).
narrative_ontology:human_readable(sotu_1975_ford_federal_spending_ceiling, "5% Federal Pay and Benefit Growth Cap (1975 Ford SOTU)").
narrative_ontology:topic_domain(sotu_1975_ford_federal_spending_ceiling, "governance/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1975_ford_federal_spending_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1975_ford_federal_spending_ceiling, treasury_deficit_control).
narrative_ontology:constraint_beneficiary(sotu_1975_ford_federal_spending_ceiling, fiscal_conservative_coalition).
narrative_ontology:constraint_victim(sotu_1975_ford_federal_spending_ceiling, federal_employees).
narrative_ontology:constraint_victim(sotu_1975_ford_federal_spending_ceiling, benefit_recipients).
narrative_ontology:constraint_victim(sotu_1975_ford_federal_spending_ceiling, military_retirees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL EMPLOYEE (SNARE) — Locked into civil service by pension vesting and inability to recover lost COLA. Real wages decline under 5% cap when inflation exceeds it. No exit option without forfeiting retirement security accumulated over decades. Maximum extraction from the perspective of the employee who cannot leave.
constraint_indexing:constraint_classification(sotu_1975_ford_federal_spending_ceiling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SOCIAL SECURITY RECIPIENT (SNARE) — Capped COLA reduces real purchasing power when inflation exceeds 5%. Cannot exit the social insurance system. Economically dependent on the benefit. Bears full cost of the cap with no offsetting coordination benefit.
constraint_indexing:constraint_classification(sotu_1975_ford_federal_spending_ceiling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: FEDERAL EMPLOYEE UNION (TANGLED ROPE) — Moderate power through collective organization and congressional lobbying. Constrained by political economy of deficit reduction pressure. Experiences both coordination (negotiating within the cap) and extraction (the cap itself imposes real losses). Can pressure for exceptions but cannot escape the constraint entirely.
constraint_indexing:constraint_classification(sotu_1975_ford_federal_spending_ceiling, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TREASURY / DEFICIT CONTROL COALITION (ROPE) — Net beneficiary. The cap functions as a pure coordination mechanism from this perspective: it coordinates spending across federal agencies and entitlement programs toward deficit reduction. Extraction runs toward this agent. No suppression experienced — the coalition actively enforces the constraint and exits whenever deficit pressure relaxes.
constraint_indexing:constraint_classification(sotu_1975_ford_federal_spending_ceiling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CONGRESS / ORGANIZED POLITICAL CAUCUS (TANGLED ROPE) — Organized power but constrained by deficit pressure and electoral incentives. Experiences both coordination (using the cap to manage budget process) and extraction (having to craft exceptions and workarounds when the cap becomes politically untenable). Can override the cap through legislation but faces political cost.
constraint_indexing:constraint_classification(sotu_1975_ford_federal_spending_ceiling, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: AUTOMATIC INDEXING RITUAL (PITON) — The 5% cap persists through institutional inertia despite routine congressional override. The theater ratio is high: Congress votes to suspend or exceed the cap in most years, rendering the cap performative. The constraint maintains symbolic force (demonstrates fiscal discipline) while functionally degraded (rarely binding). Maintained through legislative ritual rather than genuine enforcement.
constraint_indexing:constraint_classification(sotu_1975_ford_federal_spending_ceiling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a long time horizon, one might see the cap as reflecting an immutable structural truth: government cannot sustainably increase all benefit obligations faster than economic growth. But the structural data contradicts mountain classification — the cap is a contingent policy choice with identifiable beneficiaries (treasury, deficit hawks), not a physical or logical necessity. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(sotu_1975_ford_federal_spending_ceiling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1975_ford_federal_spending_ceiling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1975_ford_federal_spending_ceiling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1975_ford_federal_spending_ceiling, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1975_ford_federal_spending_ceiling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1975_ford_federal_spending_ceiling, TR),
    TR >= 0.70.

:- end_tests(sotu_1975_ford_federal_spending_ceiling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The cap imposes real costs on trapped agents (federal employees losing pension real value, retirees losing COLA) but contains coordination function — legitimate concern about federal budget growth. The value reflects that 60% of the extraction is genuine benefit reduction (not excessive), and 40% is intentional redistribution favoring deficit control. The measurement trajectory shows extractiveness rising from 0.38 (initial proposal phase) to 0.54 (routine implementation) as the reality of the cap's bite becomes clear to benefit recipients. Suppression (0.58): Moderate-high. Federal employees face strong barriers to exit (pension vesting, career lock-in, no alternative employer offering equivalent retirement). Retirees have zero exit options (cannot return to work at full capacity; dependent on benefit). Unions can organize but face declining political leverage as deficits worsen. Food stamp recipients face absolute poverty constraint — cannot 'exit' into alternative income sources. Theater ratio (0.62): Moderate-high. The constraint is nominally binding but Congress votes to suspend or exceed it in most years. By the 5-year mark, the cap is rarely enforced except rhetorically. This rising theater ratio is diagnostic of piton dynamics — institutional inertia maintains the symbolic constraint even as functional binding declines.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival divergence. Federal employees and retirees experience pure extraction (Snare) — capped real wages with no offsetting coordination benefit. The treasury experiences pure coordination (Rope) — disciplining spending is their sole interest and the cap serves that cleanly. Unions experience mixed dynamics (Tangled Rope) — both negotiating within constraints (coordination) and losing real wages (extraction). Congress experiences governance tension (Tangled Rope) — using the cap as budget discipline (coordination) while facing political pressure to override (extraction-like cost to electoral standing). The performative character of the constraint over time (rising theater ratio, routine overrides) makes the constraint appear as degraded ritual (Piton) by the end of the interval. The analytical observer risks naturalizing the cap as inevitable fiscal discipline (Mountain) but the structural data shows it is a contingent choice with clear winners and losers.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality varies sharply by agent. Federal employees and retirees are full targets (d ≈ 0.95): powerless/trapped status and no exit options produce maximum f(d) ≈ 1.42 — they experience the constraint at full force. The treasury/deficit coalition are full beneficiaries (d ≈ 0.05): institutional power and arbitrage exit (can relax the cap anytime) produce f(d) ≈ -0.12 — extraction runs toward them. Federal employee unions occupy the middle (d ≈ 0.65): moderate power and constrained exit (can organize and negotiate but cannot escape) produce f(d) ≈ 1.00 — moderate experienced extraction. Congress is similarly middle-positioned (d ≈ 0.55): organized power but constrained by electoral incentives produces f(d) ≈ 0.75. The analytical observer occupies the observation position (d ≈ 0.73, canonical for analytical) but the false-summit logic applies: treating the cap as a natural law naturalizes what is a contingent policy choice benefiting specific actors.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint serves genuinely different functions for different actors. For the treasury, it IS a pure coordination mechanism — imposing spending discipline that benefits all debt holders. For powerless benefit recipients, it IS pure extraction — real income loss with no offsetting coordination benefit. For Congress and unions, it IS tangled rope — both coordinating budget discipline AND experiencing extraction pressure (political cost, wage loss). The constraint's type is not ambiguous; the multiple types are legitimately different perspectives on the same structural phenomenon. The false summit appears at the analytical level — the natural law framing ('government spending must be disciplined') is really a policy choice that benefits treasury and deficit hawks at the expense of trapped beneficiaries. Unmasking this choice is the analytical obligation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inflation_threshold_ambiguity,
    'Is the 5% cap calibrated to average inflation expectations, or does it operate as a real wage reduction mechanism regardless of inflation?',
    'Comparison of inflation rates during cap period vs. forward inflation expectations at policy adoption; analysis of whether cap was indexed to CPI or fixed nominally',
    'If calibrated to inflation expectations: constraint functions as coordination (maintains real spending constant). If operating as real wage reduction: constraint functions as pure extraction (intentional real income redistribution).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_threshold_ambiguity, empirical, 'Whether 5% cap reflects inflation expectations or real wage reduction intent').

omega_variable(
    alternative_deficit_mechanism,
    'Would an equivalent deficit reduction have been achieved through progressive taxation rather than benefit/wage caps?',
    'Fiscal modeling: revenue impact of alternative tax increases vs. actual savings from benefit caps; historical comparison to other deficit reduction episodes',
    'If progressively taxed alternative existed: cap is extraction mechanism (chose regressive path). If progressive alternative infeasible: cap is legitimate coordination (only available mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_deficit_mechanism, empirical, 'Whether progressive tax alternative to benefit cap was available').

omega_variable(
    congressional_override_frequency,
    'How often has Congress voted to suspend or exceed the cap? Does the pattern reveal the cap as theater or genuine constraint?',
    'Legislative history: count of override votes, frequency of suspension, exceptions granted; correlation with inflation rates and congressional composition',
    'High override frequency (>80% of years): strong evidence of piton (theater). Low frequency (<20%): evidence of snare (genuine extraction). Mid-range: tangled rope dynamics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(congressional_override_frequency, empirical, 'Congressional override frequency reveals theater vs. genuine binding').

omega_variable(
    beneficiary_capture_mechanism,
    'Does the constraint primarily benefit deficit control, or does it benefit specific interest groups (wealthy taxpayers, military-industrial complex) through differential exemption patterns?',
    'Analysis of which benefits/programs received exemptions or exceeded caps; correlation of exemptions to political power of constituency groups',
    'If applied uniformly: constraint is genuine deficit mechanism (rope or scaffold). If exemptions correlate to power: constraint is selective extraction mechanism (snare for powerless, rope for powerful).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_capture_mechanism, empirical, 'Whether cap benefits apply uniformly or concentrate benefits on powerful groups').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1975_ford_federal_spending_ceiling, 0, 8).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ford75_tr_t0, sotu_1975_ford_federal_spending_ceiling, theater_ratio, 0, 0.35).
narrative_ontology:measurement(ford75_tr_t2, sotu_1975_ford_federal_spending_ceiling, theater_ratio, 2, 0.48).
narrative_ontology:measurement(ford75_tr_t5, sotu_1975_ford_federal_spending_ceiling, theater_ratio, 5, 0.62).
narrative_ontology:measurement(ford75_tr_t8, sotu_1975_ford_federal_spending_ceiling, theater_ratio, 8, 0.6).

% Extraction over time
narrative_ontology:measurement(ford75_be_t0, sotu_1975_ford_federal_spending_ceiling, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(ford75_be_t2, sotu_1975_ford_federal_spending_ceiling, base_extractiveness, 2, 0.47).
narrative_ontology:measurement(ford75_be_t5, sotu_1975_ford_federal_spending_ceiling, base_extractiveness, 5, 0.54).
narrative_ontology:measurement(ford75_be_t8, sotu_1975_ford_federal_spending_ceiling, base_extractiveness, 8, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1975_ford_federal_spending_ceiling, resource_allocation).
narrative_ontology:affects_constraint(sotu_1975_ford_federal_spending_ceiling, stagflation_monetary_constraint).
narrative_ontology:affects_constraint(sotu_1975_ford_federal_spending_ceiling, federal_workforce_mobility_trap).
narrative_ontology:affects_constraint(sotu_1975_ford_federal_spending_ceiling, cola_indexing_politics).

% DUAL FORMULATION NOTE:
% This constraint is upstream of specific federal program design changes (civil service pay reform, Social Security COLA mechanics) and downstream of fiscal/monetary policy coordination during stagflation. The 5% cap itself is a distinct structural constraint with its own extractiveness value (0.52), separate from the inflation rate (which determines whether the cap binds). Decompose if analyzing the interaction of cap policy with inflation regimes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
