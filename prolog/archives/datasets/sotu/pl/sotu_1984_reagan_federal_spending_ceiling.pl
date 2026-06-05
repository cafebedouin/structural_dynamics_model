% ============================================================================
% CONSTRAINT STORY: sotu_1984_reagan_federal_spending_ceiling
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1984_reagan_federal_spending_ceiling, []).

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
 *   constraint_id: sotu_1984_reagan_federal_spending_ceiling
 *   human_readable: Federal Spending Limited to Federal Revenue (Balanced Budget Constraint)
 *   domain: governance/fiscal_policy
 *
 * SUMMARY:
 *   The balanced budget constraint represents a structural ceiling on federal
 *   deficit spending, introduced prominently in Ronald Reagan's 1984 State of
 *   the Union address and formalized through successive balanced budget
 *   constitutional amendment proposals and statutory constraints
 *   (Gramm-Rudman-Hollings, Budget Control Act). The constraint creates a
 *   genuine coordination problem (maintaining fiscal discipline prevents
 *   runaway deficits and inflation) alongside asymmetric extraction (the
 *   benefits of deficit reduction flow disproportionately to creditors and
 *   inflation hawks, while the costs of reduced spending fall on those
 *   dependent on counter-cyclical stabilization). The constraint exhibits
 *   rising theater_ratio over time (0.42 → 0.68) as the gap between the
 *   formal rule and actual budget execution widens through accounting
 *   mechanisms, emergency exemptions, and optimistic revenue scoring. The
 *   extractiveness metric rises (0.35 → 0.58) reflecting increasing burden
 *   concentration on program beneficiaries as the rule tightens. The
 *   constraint is best classified as Tangled Rope: it performs genuine
 *   coordination (enforces fiscal discipline) while enabling systematic
 *   extraction (prevents counter-cyclical spending exactly when need is
 *   highest). However, powerless beneficiaries of social programs perceive it
 *   as pure Snare — they cannot exit dependence on programs while those
 *   programs are constrained by the spending ceiling.
 *
 * KEY AGENTS:
 *   - Deficit Reduction Coalition: Institutional beneficiaries (powerful/arbitrage) — bondholders, inflation hawks, fiscal conservatives, credit rating agencies. Benefits from reduced borrowing and lower interest rates.
 *   - Congress/Fiscal Authority: Institutional actor (powerful/constrained) — experiences both coordination benefits (spending discipline) and extraction costs (lost policy flexibility). Constrained by political cost of constitutional amendment.
 *   - Social Program Beneficiaries: Primary victims (powerless/trapped) — Medicare recipients, SNAP participants, housing assistance users, disabled persons. Trapped in dependence; cannot exit programs or economic cycles.
 *   - Cyclical Downturn Victims: Primary victims (powerless/trapped) — unemployed and economically displaced persons. Face simultaneous job loss and reduced government support during recessions.
 *   - Progressive Coalition & Economists: Organized agents (organized/mobile) — advocate for counter-cyclical exemptions and automatic stabilizers. See the constraint as a temporary problem with a sunset path.
 *   - Budget Process: Institutional actor (institutional/arbitrage) — maintains theatrical compliance through off-budget accounts, supplemental appropriations, and optimistic scoring. The constraint persists through inertia while evasion mechanisms grow.
 *   - Analytical Observer: Civilizational horizon (analytical/analytical) — risks naturalizing the institutional choice to forbid deficit spending as a mathematical necessity rather than a policy decision.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1984_reagan_federal_spending_ceiling, 0.58).
domain_priors:suppression_score(sotu_1984_reagan_federal_spending_ceiling, 0.65).
domain_priors:theater_ratio(sotu_1984_reagan_federal_spending_ceiling, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1984_reagan_federal_spending_ceiling, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1984_reagan_federal_spending_ceiling, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(sotu_1984_reagan_federal_spending_ceiling, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1984_reagan_federal_spending_ceiling, tangled_rope).
narrative_ontology:human_readable(sotu_1984_reagan_federal_spending_ceiling, "Federal Spending Limited to Federal Revenue (Balanced Budget Constraint)").
narrative_ontology:topic_domain(sotu_1984_reagan_federal_spending_ceiling, "governance/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1984_reagan_federal_spending_ceiling).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1984_reagan_federal_spending_ceiling, deficit_reduction_advocates).
narrative_ontology:constraint_beneficiary(sotu_1984_reagan_federal_spending_ceiling, bondholders).
narrative_ontology:constraint_beneficiary(sotu_1984_reagan_federal_spending_ceiling, inflation_hawk_economists).
narrative_ontology:constraint_beneficiary(sotu_1984_reagan_federal_spending_ceiling, future_taxpayers).
narrative_ontology:constraint_victim(sotu_1984_reagan_federal_spending_ceiling, counter_cyclical_fiscal_policy_capacity).
narrative_ontology:constraint_victim(sotu_1984_reagan_federal_spending_ceiling, social_safety_net_programs).
narrative_ontology:constraint_victim(sotu_1984_reagan_federal_spending_ceiling, discretionary_spending_beneficiaries).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CYCLICAL DOWNTURN VICTIMS (SNARE) — During recessions, the balanced budget ceiling prevents counter-cyclical spending when unemployment is highest and demand is lowest. Powerless agents (unemployed, economically displaced) face both job loss AND reduced government support simultaneously. No exit option: cannot opt out of economic cycles or abandon dependence on programs. Maximum extraction.
constraint_indexing:constraint_classification(sotu_1984_reagan_federal_spending_ceiling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SOCIAL PROGRAM BENEFICIARIES (SNARE) — Budget constraints force means-testing, eligibility restrictions, and benefit reductions for Medicare, SNAP, housing assistance, and education programs. Trapped beneficiaries (elderly, low-income families, disabled persons) cannot exit dependence on these programs and face pro-cyclical benefit cuts exactly when need is greatest. The constraint produces systematic extraction: reducing spending during downturns raises relative burden on those who cannot exit.
constraint_indexing:constraint_classification(sotu_1984_reagan_federal_spending_ceiling, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CONGRESS / FISCAL AUTHORITY (TANGLED ROPE) — Congress experiences both genuine coordination and asymmetric extraction. Coordination: the ceiling enforces spending discipline and prevents runaway deficits, which benefits macroeconomic stability Congress depends on. Extraction: the ceiling removes fiscal flexibility Congress would otherwise deploy to fund priorities, respond to crises, or smooth cycles. Congress is constrained (can exit via constitutional amendment but at prohibitive political cost) and experiences both benefits (deficit control) and costs (policy flexibility lost). Moderate experienced extraction.
constraint_indexing:constraint_classification(sotu_1984_reagan_federal_spending_ceiling, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: DEFICIT REDUCTION COALITION (ROPE) — Bondholders, inflation hawks, fiscal conservatives, credit rating agencies, and foreign governments benefit from the spending ceiling through reduced borrowing, lower interest rates, and reduced inflation expectations. These institutional actors experience the constraint as pure coordination: spending discipline aligns with their interests. High exit option (capital mobility, bond market access) combined with beneficiary status produces low or negative chi. They see the constraint as legitimate governance.
constraint_indexing:constraint_classification(sotu_1984_reagan_federal_spending_ceiling, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: PROGRESSIVE COALITION & KEYNESIAN ECONOMISTS (SCAFFOLD) — This organized group views the balanced budget rule as a temporary constraint that should sunset during recessions and crises. They advocate for automatic stabilizers (counter-cyclical spending that triggers without legislative action) and crisis exemptions that make the constraint conditional rather than absolute. For this perspective, the ceiling is a temporary problem with a defined exit path — automatic stabilizers and counter-cyclical exemptions represent the sunset mechanism. If implemented, extraction declines as the constraint becomes conditional.
constraint_indexing:constraint_classification(sotu_1984_reagan_federal_spending_ceiling, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: BUDGET PROCESS THEATRICALITY (PITON) — The constraint persists formally while being systematically evaded through accounting tricks (off-budget accounts, trust fund borrowing, supplemental appropriations for 'emergencies'), dynamic scoring assumptions, and multi-year averaging. The formal constraint has theatrical compliance — budget resolutions nominally enforce balanced budget but rely on optimistic revenue projections and creative accounting. Theater ratio is high (0.68) because the gap between the stated rule and actual budget execution has widened over time. The constraint's real enforcement has atrophied while the ritual persists through institutional inertia.
constraint_indexing:constraint_classification(sotu_1984_reagan_federal_spending_ceiling, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: NATURAL LAW VIEW (MOUNTAIN) — From a civilizational horizon, federal spending cannot exceed federal revenue as a pure accounting identity if deficits are not permitted. This perspective sees the balanced budget constraint as an immutable law: if you forbid borrowing, you must match inflows to outflows. However, this naturalizes a policy choice (the prohibition on borrowing) as mathematical necessity. The constraint is not a natural law but a human-created institutional rule — the analytical perspective risks falsely summiting by presenting the 'law' of spending ≤ revenue as intrinsic to government rather than contingent on the prohibition of deficit financing.
constraint_indexing:constraint_classification(sotu_1984_reagan_federal_spending_ceiling, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1984_reagan_federal_spending_ceiling_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1984_reagan_federal_spending_ceiling, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1984_reagan_federal_spending_ceiling, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1984_reagan_federal_spending_ceiling, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1984_reagan_federal_spending_ceiling, TR),
    TR >= 0.70.

:- end_tests(sotu_1984_reagan_federal_spending_ceiling_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High-moderate. The balanced budget constraint produces measurable extraction primarily during recessions and downturns. During expansions, the constraint is less binding (revenues naturally rise, creating room for spending). During recessions, the constraint forces pro-cyclical spending cuts exactly when counter-cyclical support is most needed — creating concentrated extraction on those unable to exit dependence on programs. The rising trajectory (0.35 → 0.58) reflects the historical pattern: as budget pressures accumulate and deficits become politically salient, enforcement of the constraint tightens and beneficiaries face larger cuts. Suppression (0.65): High. The constraint suppresses fiscal policy alternatives through formal rule and political culture. Congress cannot easily deploy counter-cyclical spending without violating the rule. Beneficiaries cannot exit dependence on programs constrained by the ceiling. The suppression is structural and institutional, not merely high-cost exit. Theater ratio (0.68): Moderate-high and rising. The formal rule is extensively evaded through: (1) off-budget financing (trust funds, credit reform scores), (2) supplemental appropriations labeled as 'emergency,' (3) optimistic baseline revenue projections that claim 'deficit reduction' through growth assumptions rather than policy changes, (4) dynamic scoring assumptions. The gap between the nominal balanced budget and actual deficit execution has widened over the interval, indicating rising theater. However, the theater is not complete — some budget discipline is enforced, meaning the constraint has both real and performative components.
 *
 * PERSPECTIVAL GAP:
 *   The biggest gap is between the powerless agent (Snare) and the beneficiary (Rope). The beneficiary sees coordination and net benefit. The powerless victim sees pure extraction and synchronized harm during downturns. This gap reflects a genuine structural divergence: the constraint benefits creditors and inflation hawks (who want lower borrowing) while extracting from those dependent on counter-cyclical support. Congress's tangled-rope view is instructive — Congress genuinely experiences both coordination (discipline prevents runaway deficits) and extraction (lost policy flexibility). But Congress's constraints are different from beneficiaries' constraints: Congress can theoretically exit via constitutional amendment, while beneficiaries cannot exit dependence on programs. The gap between powerless and institutional agents reflects the asymmetry in exit options and power.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation chain: beneficiaries (deficit reduction agents) with institutional power and arbitrage-level exit options produce low d (they benefit, they can exit if constrained). Victims (program beneficiaries, trapped agents) with powerless status and trapped exit options produce high d (they bear costs, they cannot exit). Congress with institutional power but constrained exit (amendment requires supermajority) occupies middle ground. The analytical observer at civilizational horizon risks false-summit classification — treating the rule as a natural law rather than a contingent institutional choice. The chi formula χ = ε × f(d) × σ(S) means that the same constraint produces very different experienced extractiveness across perspectives. For the beneficiary with low d, chi is low or negative (they experience it as net benefit). For the trapped victim with high d, chi is high (they experience maximum extraction). For Congress with moderate d, chi is moderate. For the analytical observer, the classification itself becomes the question — is this a mountain (natural law) or a tangled rope (disguised extraction)?
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy through perspectival multiplicity: the same rule is genuinely coordination from the deficit reduction perspective, genuine extraction from the powerless victim's perspective, and mixed tangled rope from Congress's perspective. There is no single 'correct' classification — the constraint is coordination + extraction simultaneously. The mandatrophy resolves when you recognize that 'is this coordination or extraction?' is not a binary question but a function of observer position. The constraint coordinates deficit reduction (genuine collective action problem solved). The constraint extracts from program beneficiaries (genuine asymmetric harm during downturns). Both are true. The theater ratio (0.68) and rising trajectory (0.42 → 0.68) indicate that the nominal rule increasingly diverges from actual enforcement, which suggests the constraint's real function (preventing deficits) is becoming harder to achieve while the performative function (claiming fiscal discipline) is becoming easier to maintain. This is the classic mandatrophy pattern: as the extractive function accumulates, the coordinate function becomes harder to perceive, and theater rises to maintain political viability. If theater reaches 0.80+, the constraint would flip toward Piton classification (institutional inertia overwhelming coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counter_cyclical_necessity_threshold,
    'At what unemployment/recession severity does counter-cyclical spending become economically necessary vs. merely advisable?',
    'Empirical comparison of unemployment-to-benefit ratios in recession vs. expansion; econometric analysis of stimulus multipliers during downturns; historical comparison of outcomes in balanced vs. deficit-enabled fiscal policy during recessions (2008, 2001, 1991, 1981-82 cycles).',
    'If threshold is low (2-3% unemployment): balanced budget rule produces significant welfare loss. If threshold is high (6%+ unemployment): rule''s constraints are acceptable during normal downturns.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counter_cyclical_necessity_threshold, empirical, 'Whether counter-cyclical spending is necessary or merely convenient').

omega_variable(
    long_term_growth_extraction,
    'Does the balanced budget constraint accelerate long-term growth (by reducing crowding out and inflation) or reduce it (by preventing counter-cyclical investment and human capital maintenance)?',
    'Cross-national econometric analysis: countries with hard balanced budget rules vs. soft rules, controlling for baseline growth rates, debt levels, and inflation environments. Long-run (20+ year) growth trajectories and productivity measures.',
    'If growth accelerates: constraint produces net long-term benefit despite cyclical cost. If growth decelerates: constraint extracts growth as the cost of fiscal discipline.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_growth_extraction, empirical, 'Long-term growth impact of balanced budget constraints').

omega_variable(
    distributional_incidence_ambiguity,
    'What portion of the burden from balanced-budget-induced spending cuts falls on powerless vs. powerful agents, and does this distribution change predictably with political cycles?',
    'Distributional analysis of budget cuts: comparing cuts to means-tested benefits vs. tax expenditures vs. military spending vs. interest payments across budget cycles. Time-series analysis of benefit-to-burden ratio by income quintile during deficit-reduction vs. deficit-expansion periods.',
    'If burden is uniform: constraint is a neutral fiscal rule. If burden concentrates on powerless agents: constraint functions as regressive extraction mechanism with political-cycle dependence.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_incidence_ambiguity, empirical, 'Whether balanced budget constraint incidence is regressive').

omega_variable(
    sovereignty_monetary_transmission,
    'In a fiat currency system with independent central bank, does a balanced budget constraint on the fiscal authority represent genuine resource constraint or institutional choice?',
    'Institutional comparison: countries with balanced budget rules and independent central banks vs. those without. Analysis of actual sectoral crowding out (whether government borrowing raises private sector borrowing costs). Examination of whether central bank can accommodate fiscal spending without loss of price stability.',
    'If constraint is genuine resource limit: balanced budget reflects real trade-offs. If constraint is institutional choice: balanced budget rule is contingent on central bank independence and could be relaxed without inflation if demand is below productive capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_monetary_transmission, conceptual, 'Whether balanced budget constraint is natural limit or institutional choice').

omega_variable(
    emergency_exception_scope_drift,
    'How broadly do ''emergency'' exceptions to the balanced budget rule expand over time, and does the definition of emergency become progressively looser?',
    'Historical analysis of emergency appropriations and supplemental spending designations: tracking what counts as ''emergency'' over decades. Comparison of supplemental vs. regular appropriations as share of budget. Analysis of whether crises claimed as justifying exceptions were foreseeable vs. truly unpredictable.',
    'If exceptions remain narrow: rule maintains real constraint. If exceptions expand: rule becomes increasingly theatrical (high theater ratio). Theater-ratio measurement (0.68) suggests exceptions are already significant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(emergency_exception_scope_drift, empirical, 'Whether emergency exceptions erode the balanced budget constraint').

omega_variable(
    redistributive_intent_masking,
    'Is the balanced budget constraint genuinely a neutral fiscal rule, or does it serve as a mask for redistributive preference (favoring bond holders over program beneficiaries)?',
    'Political economy analysis: comparing who benefits from balanced budget vs. deficit spending (creditor vs. debtor, wealthy vs. poor, older vs. younger cohorts). Historical voting patterns on budget resolutions by representative income/interest alignment. Comparative international analysis of which groups advocate for balanced budget rules in different countries.',
    'If neutral: constraint deserves its framing as macroeconomic discipline. If redistributive: constraint is a disguised transfer mechanism that should be transparent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redistributive_intent_masking, preference, 'Whether balanced budget rule masks redistributive intent').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1984_reagan_federal_spending_ceiling, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_1984_reagan_federal_spending_ceiling, theater_ratio, 0, 0.42).
narrative_ontology:measurement(sotu_tr_t5, sotu_1984_reagan_federal_spending_ceiling, theater_ratio, 5, 0.58).
narrative_ontology:measurement(sotu_tr_t10, sotu_1984_reagan_federal_spending_ceiling, theater_ratio, 10, 0.68).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_1984_reagan_federal_spending_ceiling, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(sotu_be_t5, sotu_1984_reagan_federal_spending_ceiling, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(sotu_be_t10, sotu_1984_reagan_federal_spending_ceiling, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(sotu_be_t2, sotu_1984_reagan_federal_spending_ceiling, base_extractiveness, 2, 0.42).
narrative_ontology:measurement(sotu_be_t8, sotu_1984_reagan_federal_spending_ceiling, base_extractiveness, 8, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1984_reagan_federal_spending_ceiling, resource_allocation).
narrative_ontology:affects_constraint(sotu_1984_reagan_federal_spending_ceiling, automatic_stabilizer_suppression).
narrative_ontology:affects_constraint(sotu_1984_reagan_federal_spending_ceiling, countercyclical_fiscal_capacity).
narrative_ontology:affects_constraint(sotu_1984_reagan_federal_spending_ceiling, austerity_enforcement_mechanism).
narrative_ontology:affects_constraint(sotu_1984_reagan_federal_spending_ceiling, monetary_dominance_fiscal_subordination).

% DUAL FORMULATION NOTE:
% The balanced budget constraint is upstream of multiple derived constraints. It constrains automatic stabilizers (welfare expansion during downturns), counter-cyclical fiscal capacity (recession spending), and austerity enforcement (mandatory spending cuts during deficits). The constraint also interacts with monetary dominance dynamics — where central bank independence + fiscal constraint = fiscal subordination to monetary policy objectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1984_reagan_federal_spending_ceiling, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
