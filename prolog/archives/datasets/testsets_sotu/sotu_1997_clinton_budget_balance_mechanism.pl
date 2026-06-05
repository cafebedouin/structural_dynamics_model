% ============================================================================
% CONSTRAINT STORY: sotu_1997_clinton_budget_balance_mechanism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1997_clinton_budget_balance_mechanism, []).

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
 *   constraint_id: sotu_1997_clinton_budget_balance_mechanism
 *   human_readable: Deficit Reduction via Multi-Year Balanced Budget Target (2002 Deadline)
 *   domain: fiscal_policy/economic_governance
 *
 * SUMMARY:
 *   The Balanced Budget Act of 1997, culminating in Clinton's State of the
 *   Union commitment, established a statutory mechanism requiring elimination
 *   of the federal deficit by 2002. The constraint combined genuine
 *   coordination (simplifying budget prioritization through binding rules)
 *   with significant extraction (concentrating adjustment burden on
 *   unprotected discretionary spending while sheltering Medicare, Medicaid,
 *   education, and environmental spending). The mechanism exhibits all six DR
 *   types depending on the observer's structural position: future bond
 *   markets see coordination (Rope), discretionary program beneficiaries see
 *   pure extraction (Snare), Congress experiences hybrid
 *   coordination-coercion (Tangled Rope), entitlement reformers see temporary
 *   scaffolding (Scaffold), CBO accounting systems embody theatrical
 *   measurement (Piton), and the civilizational analytical observer risks
 *   naturalizing what is actually a contingent political choice (Mountain).
 *   The mechanism's extractiveness rises over the interval (0.35 → 0.62) as
 *   the gap between the nominal target and underlying fiscal drift widens,
 *   while theater_ratio increases modestly (0.25 → 0.41) as accounting
 *   adjustments and baseline shifts accumulate.
 *
 * KEY AGENTS:
 *   - Future Fiscal Stability / Bond Markets: Primary beneficiary (institutional/arbitrage) — capture coordination benefits from credible deficit-reduction signal; can exit by withdrawing credit if commitment fails
 *   - Discretionary Spending Beneficiaries: Primary victim (powerless/trapped) — agencies, workers in non-protected programs face mandatory contraction with no exit or alternative funding source
 *   - Congress: Secondary actor (moderate/constrained) — constrained by target commitment but benefits from simplified budgeting rules; can exit only by reneging on publicly stated goal
 *   - Low-Income Transfer Recipients: Secondary victim (moderate/constrained) — benefit from Medicaid carve-out (protection) but face extraction through cuts to other safety-net programs
 *   - Entitlement Reform Coalition: Organized beneficiary (organized/constrained) — see constraint as temporary scaffolding toward permanent structural reform; exit path is successful entitlement restructuring
 *   - CBO/OMB Measurement Systems: Institutional actor (institutional/arbitrage) — maintain accounting frameworks that can be adjusted to show 'balance' while underlying trajectory drifts; arbitrage between different measurement baselines
 *   - Analytical Observer: Civilizational position (analytical/analytical) — risks naturalizing contingent political choice (carve-out selection, specific 2002 date, discretionary-first sequencing) as fiscal inevitability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1997_clinton_budget_balance_mechanism, 0.58).
domain_priors:suppression_score(sotu_1997_clinton_budget_balance_mechanism, 0.52).
domain_priors:theater_ratio(sotu_1997_clinton_budget_balance_mechanism, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1997_clinton_budget_balance_mechanism, extractiveness, 0.58).
narrative_ontology:constraint_metric(sotu_1997_clinton_budget_balance_mechanism, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(sotu_1997_clinton_budget_balance_mechanism, theater_ratio, 0.38).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1997_clinton_budget_balance_mechanism, tangled_rope).
narrative_ontology:human_readable(sotu_1997_clinton_budget_balance_mechanism, "Deficit Reduction via Multi-Year Balanced Budget Target (2002 Deadline)").
narrative_ontology:topic_domain(sotu_1997_clinton_budget_balance_mechanism, "fiscal_policy/economic_governance").

domain_priors:requires_active_enforcement(sotu_1997_clinton_budget_balance_mechanism).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1997_clinton_budget_balance_mechanism, future_fiscal_stability).
narrative_ontology:constraint_beneficiary(sotu_1997_clinton_budget_balance_mechanism, bond_market_investors).
narrative_ontology:constraint_beneficiary(sotu_1997_clinton_budget_balance_mechanism, intergenerational_equity).
narrative_ontology:constraint_beneficiary(sotu_1997_clinton_budget_balance_mechanism, long_term_growth_actors).
narrative_ontology:constraint_victim(sotu_1997_clinton_budget_balance_mechanism, discretionary_spending_beneficiaries).
narrative_ontology:constraint_victim(sotu_1997_clinton_budget_balance_mechanism, near_term_workers).
narrative_ontology:constraint_victim(sotu_1997_clinton_budget_balance_mechanism, low_income_transfer_recipients).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISCRETIONARY SPENDING BENEFICIARIES (SNARE) — Agencies, programs, and workers dependent on non-protected federal discretionary spending face mandatory contraction with no exit. The carve-outs (Medicare, Medicaid, education, environment) protect favored constituencies while discretionary programs absorb the full adjustment burden. Trapped agents experience maximum extraction via forced reduction of services or employment.
constraint_indexing:constraint_classification(sotu_1997_clinton_budget_balance_mechanism, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONGRESS (TANGLED ROPE) — Constrained by the binding deficit target and political commitment to balance by 2002, but also benefits from the coordination function: the target simplifies legislative prioritization, clarifies constraints, and provides political cover ('we had to cut discretionary spending'). High suppression (cannot redefine the target easily) combined with genuine coordination benefit (clear rules reduce ambiguity). Hybrid mechanism with both coercive discipline and collaborative rule-setting.
constraint_indexing:constraint_classification(sotu_1997_clinton_budget_balance_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FUTURE FISCAL STABILITY (ROPE) — The balanced budget mechanism coordinates expectations: markets interpret the 2002 deadline as a credible commitment to fiscal discipline, reducing borrowing costs and stabilizing long-term interest rates. The constraint functions as pure coordination from this agent's perspective — enabling future actors to plan on lower debt burdens. Beneficiary with arbitrage exit (can invest in markets that reward fiscal discipline or exit if commitment fails).
constraint_indexing:constraint_classification(sotu_1997_clinton_budget_balance_mechanism, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ENTITLEMENT REFORM COALITION (SCAFFOLD) — Organized actors (think tanks, deficit hawks, budget reformers) see the mechanism as temporary scaffolding toward deeper entitlement restructuring. The protected carve-outs for Medicare and Medicaid are treated as interim political compromises; the 2002 deadline creates urgency for permanent reform. Exit path: entitlement reform resolves the constraint by redefining the protected items. Sunset logic: once Medicare/Medicaid efficiency reforms take root, the balance requirement becomes achievable without discretionary pain.
constraint_indexing:constraint_classification(sotu_1997_clinton_budget_balance_mechanism, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CBO/OMB ACCOUNTING SYSTEMS (PITON) — The baseline scoring and deficit measurement mechanisms are largely performative: various accounting gimmicks (timing shifts, off-budget items, dynamic scoring debates) allow the deficit target to be declared 'met' even when underlying fiscal trajectory is unchanged. By 2002, the constraint is maintained through institutional inertia and accounting theater rather than actual fiscal discipline. The measurement ritual persists because alternatives haven't fully replaced the political commitment to 'balance.'
constraint_indexing:constraint_classification(sotu_1997_clinton_budget_balance_mechanism, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: LOW-INCOME TRANSFER RECIPIENTS (TANGLED ROPE) — Near-term workers and low-income households benefit from the protected Medicaid carve-out (genuine coordination), but face extraction through discretionary program cuts (TANF reform, job training, housing assistance). Mixed experience: protected from some cuts but exposed to others; constrained by the political economy of carve-out negotiation.
constraint_indexing:constraint_classification(sotu_1997_clinton_budget_balance_mechanism, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a fiscal/economic theory perspective, the balanced budget mechanism appears as a natural constraint: federal borrowing is fundamentally limited by the economy's ability to absorb debt, and eventually fiscal imbalance forces adjustment. From this view, the 2002 deadline is merely mechanizing what markets would force anyway. However, this naturalizes what is actually a contingent political choice (the carve-outs, the specific 2002 date, the discretionary-first adjustment path are all constructed, not inherited from fiscal physics).
constraint_indexing:constraint_classification(sotu_1997_clinton_budget_balance_mechanism, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1997_clinton_budget_balance_mechanism_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1997_clinton_budget_balance_mechanism, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1997_clinton_budget_balance_mechanism, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1997_clinton_budget_balance_mechanism, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1997_clinton_budget_balance_mechanism, TR),
    TR >= 0.70.

:- end_tests(sotu_1997_clinton_budget_balance_mechanism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The mechanism concentrates adjustment burden on unprotected discretionary spending (non-defense, non-Medicaid, non-education) while protecting politically favored categories. The carve-outs themselves represent a form of extraction — the protected programs grow at GDP rates while discretionary spending must bear the full adjustment to reach balance. The metric rises over the interval as the gap between nominal target and underlying fiscal drift widens, forcing progressively deeper discretionary cuts. By year 5, the extraction is visible: nominal balance is achieved through accounting adjustments and baseline scoring, while discretionary programs bear genuine contraction. Suppression (0.52): Moderate. The constraint is statutory (high formal suppression — requires congressional action to undo) but subject to negotiation (baseline revisions, scoring assumptions, carve-out redefinition provide escape valves). Discretionary program beneficiaries face high suppression (cannot redefine the target or carve-outs without broad coalition). Congress faces moderate suppression (bound by political commitment but retains discretion over implementation). Theater ratio (0.38): Moderate. The constraint is implemented through genuine fiscal actions (real cuts to discretionary programs) but relies on increasing baseline adjustments and scoring assumptions to claim progress toward the target. The ratio rises toward 0.41 by year 5 as the gap widens and accounting adjustments accumulate. The CBO's measurement role introduces theater — the same fiscal outcome can be labeled 'balanced' or 'in deficit' depending on baseline assumptions.
 *
 * PERSPECTIVAL GAP:
 *   The analytical observer's mountain classification reveals a false summit: the constraint naturalizes a constructed choice (carve-out selection, discretionary-first sequencing) as fiscal inevitability. This is the key mandatrophy issue — the mechanism is presented as 'restoring fiscal responsibility' (natural law framing) when it actually represents a specific distributional choice (discretionary vs. entitlements; near-term vs. long-term; protected vs. exposed constituencies). The snare classification from the powerless perspective is the structural reality: discretionary program beneficiaries have no exit and no offsetting benefit. The rope classification from bond market perspective is also structurally accurate: they coordinate expectations and reduce uncertainty. Both are true simultaneously — the constraint performs genuine coordination for some agents while extracting from others.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from who benefits and who bears costs. Bond market investors benefit from the credible deficit-reduction signal — low d (~0.10), negative χ (they experience coordination benefit). Future taxpayers and stable-economy beneficiaries also benefit — low d. Discretionary program beneficiaries bear costs with no offsetting benefit — high d (~0.85), high χ (experience maximum extraction). Congress is mixed: they benefit from simplified prioritization but are constrained by the target — moderate d (~0.55). The carve-out structure drives directionality: Medicare and Medicaid beneficiaries are protected (low d relative to discretionary beneficiaries), but this protection is not unconditional — if the target proves unachievable, entitlements face reform pressure (moderate d). Low-income transfer recipients benefit from Medicaid protection but face extraction through other program cuts (mixed d, moderate χ). The CBO/OMB systems have low d (benefit from flexibility to declare targets met) and arbitrage exit options. The mechanism produces a directionality vector that concentrates extraction on the powerless (discretionary beneficiaries, low-income workers) while distributing benefits toward future/institutional actors.
 *
 * MANDATROPHY ANALYSIS:
 *   CONSTRAINT FAMILY MEMBER: This story links to entitlement_reform_politics (downstream consequence) and monetary_policy_coordination (parallel mechanism with similar false-summit risk). The mandatrophy is partially resolved by recognizing the distributional asymmetry: the mechanism is simultaneously Rope (coordination for bond markets) and Snare (extraction for discretionary beneficiaries). No single type is incorrect — the classification space is correctly indexical. However, the mountain classification from the analytical perspective is a false summit: fiscal balance is not a law of nature but a political choice about which agents bear costs. The mechanism preserves the illusion of technical inevitability while actually encoding distributional preferences. This is diagnostic of false-summit mandatrophy: natural-law framing is applied to justify constructed constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    carveout_extractive_shield,
    'Do the protected carve-outs (Medicare, Medicaid, education, environment) represent genuine intergenerational equity priorities, or do they shield politically favored constituencies from fiscal discipline while extracting from discretionary programs?',
    'Counterfactual analysis: compare deficit reduction pathways with and without carve-outs; assess whether carve-outs reflect population preferences for spending priorities or political negotiation outcomes; evaluate efficiency losses from protecting inefficient programs while cutting efficient ones.',
    'If carve-outs are defensible: constraint is Tangled Rope (coordination + targeted extraction justified by priorities). If carve-outs are political extraction: constraint is Snare for discretionary beneficiaries and false-summit mountain for the analytical view.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(carveout_extractive_shield, preference, 'Whether carve-outs represent equity priorities or political protection').

omega_variable(
    baseline_scoring_gaming,
    'To what extent does the 2002 balance target rely on baseline adjustments, accounting gimmicks, or dynamic scoring assumptions that don''t reflect actual fiscal improvement?',
    'Post-2002 audit: compare projected vs. actual debt trajectory; analyze CBO baseline revisions between 1997 and 2002; identify timing shifts, off-budget reclassifications, and dynamic scoring adjustments that were counted toward ''balance.''',
    'If gaming is material (>30% of claimed improvement): theater_ratio should be higher (0.65+) and the piton perspective is validated. If minimal (<10%): constraint shows genuine fiscal discipline and theater_ratio remains ~0.38.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(baseline_scoring_gaming, empirical, 'Extent of baseline gaming in achieving 2002 target').

omega_variable(
    entitlement_sustainability_linkage,
    'Does the balanced budget mechanism actually drive entitlement reform (per scaffold perspective), or do protected carve-outs allow indefinite entitlement spending growth to continue unabated?',
    'Historical analysis post-2002: track whether the constraint spurred Medicare/Medicaid structural reforms or merely shifted the fiscal pressure to other years; assess whether ''balancing by 2002'' postponed entitlement crisis rather than resolving it.',
    'If reform is spurred: scaffold sunset logic is real (constraint creates conditions for deeper reform). If postponed: constraint is theater that defers reckoning (piton classification strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entitlement_sustainability_linkage, empirical, 'Whether mechanism spurs entitlement reform or defers reckoning').

omega_variable(
    discretionary_cliff_distributional_impact,
    'Does the first-discretionary-adjusted-then-entitlements sequencing create regressive distributional consequences (near-term pain on discretionary beneficiaries, deferred pain on entitlements)?',
    'Distributional analysis: track which income groups bear cuts in discretionary vs. entitlement spending; assess whether the timing asymmetry (immediate discretionary cuts, postponed entitlement reform) disproportionately burdens lower-income households in the near term.',
    'If regressive: snare classification for discretionary beneficiaries is strengthened; false-summit mountain (naturalizes what is actually a constructed distributional choice) is validated. If distribution is progressive: some of the extractiveness attribution becomes less defensible.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(discretionary_cliff_distributional_impact, empirical, 'Distributional impact of discretionary-first adjustment sequencing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1997_clinton_budget_balance_mechanism, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bbudget_tr_t0, sotu_1997_clinton_budget_balance_mechanism, theater_ratio, 0, 0.25).
narrative_ontology:measurement(bbudget_tr_t2, sotu_1997_clinton_budget_balance_mechanism, theater_ratio, 2, 0.32).
narrative_ontology:measurement(bbudget_tr_t4, sotu_1997_clinton_budget_balance_mechanism, theater_ratio, 4, 0.38).
narrative_ontology:measurement(bbudget_tr_t5, sotu_1997_clinton_budget_balance_mechanism, theater_ratio, 5, 0.41).

% Extraction over time
narrative_ontology:measurement(bbudget_be_t0, sotu_1997_clinton_budget_balance_mechanism, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bbudget_be_t2, sotu_1997_clinton_budget_balance_mechanism, base_extractiveness, 2, 0.48).
narrative_ontology:measurement(bbudget_be_t4, sotu_1997_clinton_budget_balance_mechanism, base_extractiveness, 4, 0.58).
narrative_ontology:measurement(bbudget_be_t5, sotu_1997_clinton_budget_balance_mechanism, base_extractiveness, 5, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1997_clinton_budget_balance_mechanism, resource_allocation).
narrative_ontology:affects_constraint(sotu_1997_clinton_budget_balance_mechanism, entitlement_reform_political_economy).
narrative_ontology:affects_constraint(sotu_1997_clinton_budget_balance_mechanism, discretionary_spending_vulnerability).
narrative_ontology:affects_constraint(sotu_1997_clinton_budget_balance_mechanism, monetary_policy_federal_debt_ceiling).

% DUAL FORMULATION NOTE:
% The balanced budget mechanism is upstream of specific entitlement and discretionary policy constraints. The carve-out structure determines how pressure propagates downstream — protected programs (Medicare, Medicaid) remain free to grow, while unprotected programs face contraction. This constraint family decomposition reflects two structurally distinct mechanisms: (1) the nominal target (year 2002, balance), and (2) the distributional mechanism (carve-outs, discretionary-first). A more precise decomposition would separate these into distinct ε values, but they are analyzed together here because they are legislatively unified and politically inseparable.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1997_clinton_budget_balance_mechanism, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
