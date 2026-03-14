% ============================================================================
% CONSTRAINT STORY: debt_enforcement_against_incapacity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_debt_enforcement_against_incapacity, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: debt_enforcement_against_incapacity
 *   human_readable: Debt Enforcement Against Incapacity
 *   domain: economic/legal/social
 *
 * SUMMARY:
 *   Debt enforcement against incapacity represents a structural constraint
 *   where creditor institutions extract value from debtors who lack income or
 *   capacity to service debt. The constraint operates across multiple
 *   domains: medical debt, consumer credit, mortgages, and student loans. It
 *   exhibits snare characteristics — high extraction (χ ≈ 0.68), high
 *   suppression (0.72), and minimal coordination benefit for the target
 *   population. The constraint persists through legal mechanisms (wage
 *   garnishment, asset seizure, credit reporting, eviction) that are
 *   activated despite demonstrated debtor incapacity. The extractiveness
 *   value (0.68) reflects that enforcement mechanisms transfer wealth from
 *   incapacitated debtors to creditors with high coercive force and minimal
 *   due process for hardship claims. The low theater ratio (0.38) indicates
 *   that enforcement mechanisms function operationally — they do extract
 *   value efficiently — but lack coordinative legitimacy; the extraction is
 *   naked rather than clothed in procedural theater. The measurement
 *   trajectory shows rising extractiveness over the 30-year interval as
 *   enforcement mechanisms have been strengthened (garnishment laws expanded,
 *   credit reporting systemized, asset seizure made automatic) while
 *   incapacity exemptions have been restricted. The rising theater ratio
 *   suggests recent attempts to justify enforcement through procedural
 *   legitimacy (hardship hearings, payment plans) — adding theater to an
 *   inherently extractive mechanism.
 *
 * KEY AGENTS:
 *   - Incapacitated Debtors: Primary victims (powerless/trapped) — bear full extraction; cannot exit through bankruptcy (inaccessible), cannot renegotiate terms (creditor has no incentive), cannot escape obligations (credit system integration). Trapped exit = maximum experienced extraction.
 *   - Creditor Institutions: Primary beneficiaries (institutional/arbitrage) — access enforcement mechanisms with minimal cost; have multiple arbitrage options (securitization, portfolio rebalancing, debt sale); perceive enforcement as legitimate coordination tool. Arbitrage exit = low/negative experienced extraction.
 *   - Household Economy: Secondary victim (moderate/constrained) — coordination function (incentives for productive activity) exists alongside asymmetric extraction (budget constraints, reduced intergenerational mobility). Constrained exit = moderate experienced extraction.
 *   - Bankruptcy System: Institutional actor (organized/constrained) — designed as temporary relief (sunset logic) but has become inaccessible to target population; persists through inertia (piton). Constrained exit = moderate experienced extraction, high theater (procedural legitimacy masking failure).
 *   - Debt Relief Coalition: Organized agents (organized/constrained) — building alternative pathways (jubilees, income-based repayment, cancellation); positioned as temporary interventions with eventual sunset. Constrained exit = perception of solvability; scaffold logic.
 *   - Analytical Observer: Civilizational view (analytical/analytical) — identifies snare structure across all perspectives except creditor institutional context; recognizes that constraint persists through institutional naturalization rather than coordinative necessity.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(debt_enforcement_against_incapacity, 0.68).
domain_priors:suppression_score(debt_enforcement_against_incapacity, 0.72).
domain_priors:theater_ratio(debt_enforcement_against_incapacity, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(debt_enforcement_against_incapacity, extractiveness, 0.68).
narrative_ontology:constraint_metric(debt_enforcement_against_incapacity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(debt_enforcement_against_incapacity, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(debt_enforcement_against_incapacity, snare).
narrative_ontology:human_readable(debt_enforcement_against_incapacity, "Debt Enforcement Against Incapacity").
narrative_ontology:topic_domain(debt_enforcement_against_incapacity, "economic/legal/social").

domain_priors:requires_active_enforcement(debt_enforcement_against_incapacity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(debt_enforcement_against_incapacity, creditor_institutions).
narrative_ontology:constraint_victim(debt_enforcement_against_incapacity, incapacitated_debtors).
narrative_ontology:constraint_victim(debt_enforcement_against_incapacity, household_economic_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INCAPACITATED DEBTOR (SNARE) — Faces structural inability to service debt due to illness, disability, unemployment, or age-related income loss. Legal mechanisms (wage garnishment, asset seizure, court proceedings) enforce extraction despite the debtor's demonstrated inability to pay. Exit is impossible: cannot declare bankruptcy without losing home or critical assets; cannot escape obligations through relocation due to credit system integration; cannot negotiate terms because creditor has no incentive to modify debt structure. The debtor perceives this as pure extraction with maximum coercion and zero coordination benefit.
constraint_indexing:constraint_classification(debt_enforcement_against_incapacity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CREDITOR INSTITUTION (ROPE) — Experiences debt enforcement as a coordination mechanism: the legal framework establishes predictable recovery pathways that enable lending markets to function. Institutional actors have arbitrage options (securitization, sale of debt, portfolio rebalancing) and perceive the constraint as a legitimate coordination tool. From this perspective, enforcement mechanisms solve the collective action problem of maintaining loan performance incentives. Net beneficiary position with high exit optionality.
constraint_indexing:constraint_classification(debt_enforcement_against_incapacity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: HOUSEHOLD ECONOMY (TANGLED ROPE) — Mixed coordination and extraction. Debt enforcement coordinates household financial planning and incentivizes productive activity (work, asset accumulation). But enforcement against incapacitated debtors creates asymmetric extraction: household budgets are permanently constrained; essentials (food, medicine, housing) are subordinated to debt service; intergenerational mobility is blocked. Constrained exit: moving to no-enforcement jurisdictions is economically infeasible; renegotiation requires proving hardship through invasive means.
constraint_indexing:constraint_classification(debt_enforcement_against_incapacity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: BANKRUPTCY SYSTEM (PITON) — Designed as a temporary relief mechanism for incapacitated debtors (sunset clause: discharge after 7-10 years). But the system has become largely performative. Bankruptcy filing requires expensive legal representation, trustee fees, and asset seizure, making it inaccessible to debtors with minimal assets. Most incapacitated debtors cannot exit through the intended mechanism. The theater ratio (0.38) reflects that bankruptcy processes maintain procedural legitimacy while substantively failing to provide relief. The coordination function (fresh start after hardship) has atrophied; institutional inertia maintains the constraint despite degraded purpose.
constraint_indexing:constraint_classification(debt_enforcement_against_incapacity, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEBT RELIEF COALITION (SCAFFOLD) — Community organizations, policy advocates, and mutual aid networks (organized agents with constrained exit) are building alternative exit pathways: debt jubilees, income-based repayment, debt cancellation programs. These interventions have sunset logic — they are positioned as temporary responses to specific hardships (medical debt, student loan crises, pandemic disruption). As these programs mature and normalize, the enforcement constraint's extractive force declines. The coalition sees enforcement against incapacity as a temporary coordination failure that can be resolved through policy intervention.
constraint_indexing:constraint_classification(debt_enforcement_against_incapacity, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a civilizational view, debt enforcement against incapacity reveals a fundamental structural conflict: no agent perceives genuine mutual coordination benefit. The creditor benefits from extraction; the debtor bears costs; the household economy is stratified into payers (constrained) and extractors (beneficiary). The constraint persists not because it solves a collective action problem but because enforcement mechanisms have been institutionalized and naturalized as 'how credit markets work.' The analytical observer classifies this as snare across all contexts except the creditor's immediate institutional view (where it appears as rope).
constraint_indexing:constraint_classification(debt_enforcement_against_incapacity, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(debt_enforcement_against_incapacity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(debt_enforcement_against_incapacity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(debt_enforcement_against_incapacity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(debt_enforcement_against_incapacity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(debt_enforcement_against_incapacity, TR),
    TR >= 0.70.

:- end_tests(debt_enforcement_against_incapacity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The constraint transfers substantial value from debtors to creditors. Empirical measures: (1) income diverted to debt service despite incapacity (average 30-40% of household income in incapacity cases); (2) asset seizure and forced liquidation at below-market rates; (3) creditor recovery rates remain high (70-85%) despite debtor incapacity claims. The value reflects sustained, non-consensual value extraction. Suppression (0.72): High. Multiple mechanisms prevent exit: (a) legal barriers (bankruptcy inaccessible due to filing costs and asset seizure requirements); (b) economic barriers (debt consolidation or relocation infeasible); (c) social barriers (credit system integration makes default detection inevitable); (d) informational barriers (debtors lack knowledge of potential defenses). Suppression is structural, not performative. Theater ratio (0.38): Low-moderate. Enforcement mechanisms function operationally — they extract value efficiently — but lack extensive procedural theater. Courts do not typically adjudicate hardship; enforcement is largely automatic (wage garnishment, credit reporting, asset seizure by rule). Recent trends show rising theater (hardship hearings, payment plan negotiations) as legitimacy challenges mount. The measurement trajectory reflects this: theater rises from 0.28 to 0.42 as enforcement systems add procedural elements to justify continued extraction.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits a stark perspectival gap between the creditor institutional view (Rope — sees enforcement as legitimate coordination mechanism) and all other perspectives (Snare or Tangled Rope). The incapacitated debtor perceives pure extraction with maximum coercion; the household economy perceives mixed coordination-extraction; the analytical observer perceives snare across civilizational time. The creditor's rope classification rests on the assumption that enforcement enables lending markets to function — that is, it solves a coordination problem. But this assumes the creditor's perspective IS a genuine coordination solution. From the debtor's perspective, enforcement is not coordinating anything; it is extracting despite incapacity. The perspectival gap reveals that 'coordination' and 'extraction' are not observer-independent properties — they depend on who benefits and who bears costs. The engine's mandatrophy resolution identifies this: the creditor's rope and the debtor's snare are both structurally accurate from their respective positions. The constraint is snare when measured from the target's perspective and rope when measured from the beneficiary's perspective. No single type is 'correct' — the presheaf over the indexical space contains the full picture.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from beneficiary/victim declarations and exit options. Incapacitated debtors: full victim status + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → experienced χ ≈ 0.97 (maximum extraction). Creditor institutions: full beneficiary status + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → experienced χ ≈ -0.08 (negative extraction, i.e., benefit). Household economy: mixed victim status + constrained exit → d ≈ 0.65 → f(d) ≈ 1.00 → experienced χ ≈ 0.68 (high extraction). Bankruptcy system: institutional beneficiary with constrained exit + mixed victim/beneficiary status → d ≈ 0.40 → f(d) ≈ 0.40 → experienced χ ≈ 0.27 (moderate extraction). Debt relief coalition: organized agent perceiving solvability → d ≈ 0.35 → f(d) ≈ 0.28 → perceived χ ≈ 0.19 (low extraction because exit path is perceived as real). Scope modifier σ(S) = 1.0 (national scope). The directionality logic shows why extraction is experienced radically differently across perspectives: the creditor's arbitrage options (securitization, portfolio rebalancing, default pricing) de-couple them from the incapacitated debtor's specific default; the debtor has no such options and experiences the full weight of enforcement.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED. The mandatrophy is resolved by recognizing that the constraint is snare from the debtor's structural perspective but rope from the creditor's structural perspective. The classification difference is not a measurement error or perspective bias — it reflects genuine structural differences: (1) The creditor has multiple exit options (arbitrage); the debtor has none (trapped). (2) The creditor experiences enforcement as enabling market function; the debtor experiences it as pure extraction. (3) The coordination function (predictable recovery mechanisms) serves the creditor's interest in lending markets; it does not serve the debtor's interest in avoiding bankruptcy. The mandatrophy resolves by accepting that 'this is a snare when measured from the debtor's perspective' and 'this is a rope when measured from the creditor's perspective' are both true. The constraint is not secretly one or the other — it is genuinely both, and the difference reveals the asymmetry. The analytical observer's snare classification (from the civilizational/analytical perspective) correctly identifies that the underlying extraction mechanism serves only creditor interests, not mutual coordination. The creditor's rope is a truth about their experience; the debtor's snare is a truth about the constraint's structural function. No mislabeling occurs when both truths are preserved in the perspectival analysis.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    incapacity_definition_boundary,
    'What constitutes demonstrated incapacity that should trigger enforcement suspension?',
    'Comparative legal analysis of disability standards across jurisdictions; empirical assessment of debtor income trajectories post-enforcement action; medical certification protocols.',
    'If incapacity is defined narrowly (severe disability only): most debtors classified as capable despite genuine inability to pay; extraction rates remain high. If defined broadly (any income < debt service): system becomes welfare mechanism rather than debt recovery tool; lender incentives collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incapacity_definition_boundary, conceptual, 'Definition boundary for incapacity that justifies enforcement suspension').

omega_variable(
    extraction_versus_coordination_fungibility,
    'Are debt enforcement mechanisms primarily extractive (serving creditor interests) or coordinative (establishing predictable incentives for both parties)?',
    'Comparative institutional analysis: jurisdictions with strong incapacity exemptions vs strict enforcement; measurement of debt recovery rates, lending volume, and default costs under each regime; debtor mobility patterns.',
    'If coordinative dominates: classification shifts toward Tangled Rope across all perspectives; enforcement is justified by mutual benefit. If extractive dominates: classification is Snare; enforcement persists to transfer wealth from incapacitated to creditors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_versus_coordination_fungibility, empirical, 'Whether enforcement serves coordination or extraction function').

omega_variable(
    identity_lock_in_debt_obligation,
    'Do incapacitated debtors accept enforcement as legitimate (identity-locked) or perceive it as unjust extraction but face material barriers to exit?',
    'Qualitative research: debtor self-narratives, acceptance vs resentment of debt obligations post-incapacity; psychological assessment of internalized shame vs external constraint; comparative study of debtors in jurisdictions with and without enforcement-against-incapacity mechanisms.',
    'If identity-locked dominates: the binding mechanism is cognitive (debtor has internalized obligation as moral debt despite incapacity); exit would require identity reconstruction. If material barriers dominate: debtors recognize extraction but cannot escape due to structural constraints; different intervention logic applies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_in_debt_obligation, empirical, 'Whether suppression operates through identity lock or material barriers').

omega_variable(
    debt_jubilee_sustainability,
    'Can alternative exit mechanisms (debt jubilee, income-based repayment, debt cancellation) permanently replace enforcement against incapacity, or do they require periodic reinvention?',
    'Longitudinal tracking of debt relief programs (medical bankruptcy, student loan forgiveness, jubilee programs); measurement of re-accumulation of unsustainable debt; analysis of whether debtors re-enter enforcement cycles.',
    'If sustainable replacement: scaffold perspective is correct; sunset is real; enforcement constraint can be permanently decomposed. If cyclical: relief programs are temporary pressure valves; underlying extraction mechanism persists; scaffold is aspirational rather than structural.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(debt_jubilee_sustainability, empirical, 'Whether alternative mechanisms can sustainably replace enforcement').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(debt_enforcement_against_incapacity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(debt_enf_tr_t0, debt_enforcement_against_incapacity, theater_ratio, 0, 0.28).
narrative_ontology:measurement(debt_enf_tr_t10, debt_enforcement_against_incapacity, theater_ratio, 10, 0.35).
narrative_ontology:measurement(debt_enf_tr_t20, debt_enforcement_against_incapacity, theater_ratio, 20, 0.38).
narrative_ontology:measurement(debt_enf_tr_t30, debt_enforcement_against_incapacity, theater_ratio, 30, 0.42).

% Extraction over time
narrative_ontology:measurement(debt_enf_be_t0, debt_enforcement_against_incapacity, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(debt_enf_be_t10, debt_enforcement_against_incapacity, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(debt_enf_be_t20, debt_enforcement_against_incapacity, base_extractiveness, 20, 0.68).
narrative_ontology:measurement(debt_enf_be_t30, debt_enforcement_against_incapacity, base_extractiveness, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(debt_enforcement_against_incapacity, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(debt_enforcement_against_incapacity, 0.12).
narrative_ontology:affects_constraint(debt_enforcement_against_incapacity, medical_debt_accumulation).
narrative_ontology:affects_constraint(debt_enforcement_against_incapacity, wage_stagnation_trap).
narrative_ontology:affects_constraint(debt_enforcement_against_incapacity, eviction_risk_cascade).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
