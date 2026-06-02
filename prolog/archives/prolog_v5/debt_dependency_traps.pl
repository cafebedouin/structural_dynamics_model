% ============================================================================
% CONSTRAINT STORY: debt_dependency_traps
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_debt_dependency_traps, []).

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
 *   constraint_id: debt_dependency_traps
 *   human_readable: Debt Dependency Traps in Consumer and Development Finance
 *   domain: economic/political/social
 *
 * SUMMARY:
 *   Debt dependency traps represent a structural constraint where borrowers
 *   become locked into repayment obligations that extract wealth
 *   systematically while preventing exit through material barriers (legal
 *   enforcement, credit destruction, wage garnishment) and psychological
 *   internalization (shame, self-blame, normalized indebtedness). The
 *   constraint operates across consumer finance (predatory lending, payday
 *   loans, credit card debt spirals), development finance (IMF structural
 *   adjustment programs, sovereign debt traps), and student debt systems. The
 *   increasing extractiveness over the measurement interval (0.45 → 0.68)
 *   reflects the accumulation of interest, penalties, and compounding
 *   effects, as well as the progressive narrowing of exit options as debt
 *   burden grows. The low theater ratio (0.35) indicates that debt extraction
 *   is relatively direct and mechanically enforced — unlike performative
 *   review systems or vestigial institutions, debt collection is not
 *   primarily maintained through ritual; it is maintained through hard
 *   material enforcement (seizure, garnishment) and legal instruments. The
 *   constraint satisfies all snare gates: base extractiveness ≥ 0.46,
 *   suppression ≥ 0.60, χ ≥ 0.66. The constraint's mandatrophy is resolved by
 *   recognizing that debt serves a genuine coordination function (capital
 *   allocation) at institutional scale but is captured by extractive
 *   rent-seeking mechanisms that transform coordination into subordination.
 *
 * KEY AGENTS:
 *   - Debt-Trapped Borrowers: Primary victims (powerless/trapped) — legal obligation to repay, no path to exit without severe consequences; bear full extraction burden
 *   - Working Borrowers: Secondary victims (moderate/constrained) — high but surmountable costs to exit; experience both genuine credit access benefits and asymmetric extraction
 *   - Intergenerational Debt Carriers: Tertiary victims (powerless/identity_locked) — structurally mobile but identity-fused with debt narratives; shame and internalized responsibility prevent exit even when barriers are removed
 *   - Creditors and Financial Intermediaries: Primary beneficiaries (institutional/arbitrage) — capture interest spread, fees, and risk premium; experience debt dependency as pure coordination mechanism enabling profitable capital allocation
 *   - Debt Collection Apparatus: Institutional enforcer (institutional/arbitrage) — maintains suppression through legal and social machinery; operates largely through threatened enforcement rather than full execution
 *   - Analytical Observer: System-level view (analytical/analytical) — identifies aggregate wealth transfer from debtors to creditors; snare classification confirmed across timeframes and populations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(debt_dependency_traps, 0.68).
domain_priors:suppression_score(debt_dependency_traps, 0.72).
domain_priors:theater_ratio(debt_dependency_traps, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(debt_dependency_traps, extractiveness, 0.68).
narrative_ontology:constraint_metric(debt_dependency_traps, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(debt_dependency_traps, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(debt_dependency_traps, snare).
narrative_ontology:human_readable(debt_dependency_traps, "Debt Dependency Traps in Consumer and Development Finance").
narrative_ontology:topic_domain(debt_dependency_traps, "economic/political/social").

domain_priors:requires_active_enforcement(debt_dependency_traps).
% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(debt_dependency_traps, creditors).
narrative_ontology:constraint_beneficiary(debt_dependency_traps, financial_intermediaries).
narrative_ontology:constraint_victim(debt_dependency_traps, debt_trapped_borrowers).
narrative_ontology:constraint_victim(debt_dependency_traps, post_default_debtors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DEBT-TRAPPED BORROWER (SNARE) — Faces material barriers to exit: legal obligation to repay, wage garnishment, asset seizure, credit destruction that blocks future borrowing on better terms. Interest compounding and penalty fees escalate the trap. The borrower perceives no legitimate path to escape; exit requires default with severe collateral consequences. Maximum experienced extraction due to structural immobility and asymmetric power.
constraint_indexing:constraint_classification(debt_dependency_traps, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: INTERGENERATIONAL DEBT CARRIER (SNARE, identity_locked variant) — At biographical time, constrained by current debt burden. At generational time, identity-locked through internalized debt narratives: borrowers frame themselves as personally responsible for systemic debt patterns, adopt scarcity mindset, transmit financial despair to children. The identity lock (self-blame, shame, normalization of indebtedness) persists even when structural barriers are removed. This perspective shows how structural extraction becomes internalized as personal failing.
constraint_indexing:constraint_classification(debt_dependency_traps, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: FINANCIAL INTERMEDIARY (ROPE) — Benefits from debt dependency as coordination mechanism: lenders create access to capital that borrowers could not otherwise obtain. The constraint coordinates a legitimate economic function — capital allocation from savers to borrowers. Lenders experience the mechanism as pure coordination (solving the matching problem), with minimal perception of extraction. Net beneficiary through interest spread, fees, and risk management.
constraint_indexing:constraint_classification(debt_dependency_traps, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: WORKING BORROWER (TANGLED ROPE) — Has constrained exit options: can in principle restructure debt or relocate, but at high cost (damage to credit, loss of collateral, relocation expenses, income disruption). Experiences both genuine coordination (access to capital enables housing/education/business investment) and asymmetric extraction (interest rates, fees, terms that punish default). Both coordination and extraction are real — the borrower both benefits from credit access and bears hidden costs through debt servicing overhead.
constraint_indexing:constraint_classification(debt_dependency_traps, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DEBT COLLECTION APPARATUS (PITON) — The legal and social machinery for enforcing debt claims (courts, garnishment, credit bureaus, collection agencies) persists largely through institutional inertia. Much debt enforcement is performative: creditors use collection threats that would be expensive to execute in full, relying on psychological pressure and credit-score intimidation rather than actual seizure. The apparatus maintains extraction potential without fully exercising it. Theater ratio high relative to actual enforcement; constraint maintained through threat rather than consistent execution.
constraint_indexing:constraint_classification(debt_dependency_traps, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a systems perspective, debt dependency is a pure extraction mechanism when measured across populations: aggregate borrower interest payments exceed the coordination cost of capital allocation. The constraint systematically transfers wealth from debtors to creditors, concentrated at scale. Low coordination value relative to extraction; compounding mechanisms ensure trap deepens over time. The engine classifies this as snare with high confidence across timeframes.
constraint_indexing:constraint_classification(debt_dependency_traps, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(debt_dependency_traps_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(debt_dependency_traps, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(debt_dependency_traps, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(debt_dependency_traps, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(debt_dependency_traps, TR),
    TR >= 0.70.

:- end_tests(debt_dependency_traps_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High and increasing. The initial extractiveness (0.45) reflects legitimate coordination cost for capital allocation, but the trajectory reveals accumulating extraction. Interest compounding, penalty fees, and refinancing traps deepen the burden over time. By T=10, the extractiveness reaches 0.68 — well into snare territory. The mechanism is not theater (performative) but mechanically enforced through legal instruments and credit system consequences. Suppression (0.72): Very high. Multiple barriers prevent exit: (1) Legal obligation enforced through garnishment, asset seizure, court judgments. (2) Credit system destruction — default damages credit score for 7-10 years, blocking access to better terms and future credit access. (3) Psychological suppression — shame, internalized responsibility, belief that escape is impossible. (4) Systemic barriers — debt-trapped borrowers often have reduced income, geographic mobility constraints, and lack of alternative capital sources. Theater ratio (0.35): Low and stable. This distinguishes debt traps from degraded institutions (Piton). Debt collection is primarily direct enforcement (actual consequence execution) rather than threat-based theater. The machinery works mechanically — failure to pay produces tangible consequences (garnished wages, seized assets, destroyed credit). The low theater indicates pure extraction, not inertial performance.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates pure extraction (Snare) from the trapped borrower's perspective and pure coordination (Rope) from the creditor's perspective — the same mechanism appears as prison to one and profit to the other. The working borrower perceives both (Tangled Rope) because they have partial agency and partial benefit. The identity-locked borrower perceives internalized necessity — the constraint feels natural, not imposed. The collection apparatus perceives its own degradation (Piton) — the machinery for enforcement persists but is increasingly performative, with threats replacing actual seizure. The analytical observer recognizes snare (pure extraction at population level) and mandatrophy (the coordination function is real but has been captured by extractive mechanisms that transform beneficial borrowing into debt servitude).
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (creditors, financial intermediaries) derive low d from arbitrage exit options and beneficiary status. The directionality pipeline computes d ≈ 0.05-0.15, producing f(d) in the range [-0.12, 0.02]. For institutional power atoms with these d values, effective extraction (χ) becomes negative or near-zero — they are subsidized by the constraint, not taxed by it. Victims (trapped borrowers) derive high d from trapped exit options. The pipeline computes d ≈ 0.95, producing f(d) ≈ 1.42. For powerless atoms with this d, χ = 0.68 × 1.42 × 1.0 ≈ 0.96 — maximum experienced extraction. Working borrowers with constrained exit derive d ≈ 0.65, producing f(d) ≈ 1.00 and χ ≈ 0.68. The directionality derivation explains why the same base extractiveness (0.68) produces different classifications: beneficiaries perceive < 0.35 effective extraction (Rope), moderate victims perceive 0.68 effective extraction (Tangled Rope), powerless victims perceive 0.96 effective extraction (Snare). No directionality overrides are needed; the structural derivation captures the reality.
 *
 * MANDATROPHY ANALYSIS:
 *   SNARE CLASSIFICATION CONFIRMED WITH MANDATROPHY RESOLVED. The mandate would conflict only if debt dependency were primarily a coordination mechanism (Rope). However, extractiveness (0.68) exceeds the coordination floor (≈0.35), suppression (0.72) exceeds pure-coordination norms, and the perspectival gap between beneficiaries (who see Rope) and victims (who see Snare) reveals the extraction mechanism. The coordination function (capital allocation from savers to borrowers) is real but has been captured by extractive rate-setting, penalty structures, and refinancing traps that transform the beneficiary role. Mandatrophy is resolved by recognizing: (1) Debt dependency serves genuine coordination at institutional scale (matching capital with borrowers). (2) At victim scale, the same mechanism is pure extraction — interest and penalties exceed coordination cost. (3) The gap between these truths (same mechanism, different outcomes) is not a classification error but evidence that the constraint operates asymmetrically: beneficiaries experience genuine coordination, victims experience pure extraction. The Snare classification captures this asymmetry. The extractiveness trajectory (0.45 → 0.68) shows that the mechanism began as mixed coordination-extraction and degraded toward pure extraction as compounding and penalty structures accumulated. This is not contradiction — it is the natural trajectory of unregulated debt systems where margin maximization replaces coordination efficiency.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    coordination_vs_extraction_boundary,
    'What interest rate threshold distinguishes legitimate capital-allocation coordination from extractive rent-seeking?',
    'Comparative analysis: risk-adjusted cost of capital in competitive markets vs actual interest rates charged to trapped borrowers; decomposition of interest into risk premium + lender overhead + economic rent',
    'If threshold is high (> 8%): many legitimate debts misclassified as extraction. If threshold is low (< 3%): most consumer debt appears extractive, shifting institutional perspective classification from Rope to Tangled Rope or Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_vs_extraction_boundary, empirical, 'Interest rate threshold separating legitimate coordination from extraction').

omega_variable(
    debt_trap_triggering_conditions,
    'What combination of income volatility, interest rates, and initial debt burden creates irreversible debt dependency?',
    'Mathematical modeling of debt service sustainability; identification of bifurcation points where borrowers transition from constrained (high-cost exit possible) to trapped (no viable exit); longitudinal tracking of borrower trajectories',
    'If triggering conditions are rare: most borrowers retain constrained classification (Tangled Rope). If triggering conditions are common: trapped classification (Snare) applies to larger populations, revealing systemic extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_trap_triggering_conditions, empirical, 'Conditions that convert constrained debt to irreversible debt dependency').

omega_variable(
    identity_lock_persistence_post_exit,
    'Does the internalized debt identity (self-blame, scarcity mindset) persist after structural debt is resolved?',
    'Longitudinal follow-up studies of borrowers post-default or post-bankruptcy; measurement of financial behavior, risk tolerance, and self-narratives among those who achieved debt exit vs those who did not',
    'If identity lock persists: the constraint has internalized the victim beyond material bondage. Post-exit behavioral patterns would be shaped by residual identity lock, affecting re-borrowing, financial risk-taking, and intergenerational transmission. Indicates the extractive mechanism runs deeper than formal debt obligation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence_post_exit, empirical, 'Whether debt identity narratives persist after material debt resolution').

omega_variable(
    intergenerational_transmission_mechanism,
    'Does debt dependency transmit across generations through economic causation (inherited poverty limiting opportunity), cultural transmission (normalized indebtedness), or identity fusion (children adopt parent''s shame)?',
    'Decomposition analysis comparing: intergenerational economic mobility in high-debt vs low-debt cohorts; cultural narratives about debt in families with multi-generational debt history; psychological assessment of inherited guilt/shame in children of debt-trapped parents',
    'If economic: intergenerational trap is primarily resource limitation, potentially breakable through income increase. If cultural/identity: the trap is self-sustaining through belief systems even when material conditions improve. Different mechanisms require different interventions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_transmission_mechanism, empirical, 'Transmission mechanism for debt dependency across generations').

omega_variable(
    debt_enforcement_actual_vs_threatened,
    'How much of debt-trap suppression comes from actual enforcement (seizure, garnishment, default consequences) vs psychological enforcement (threat, shame, credit-score fear)?',
    'Audit of debt collection practices; comparison of stated enforcement mechanisms with actual execution rates; analysis of borrower perception vs creditor capacity',
    'If actual enforcement is low: suppression is partially illusory, maintained through theater. Borrower belief that exit is impossible may exceed actual legal barriers. If actual enforcement is high: suppression is structural, not theatrical — different policy intervention required.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_enforcement_actual_vs_threatened, empirical, 'Actual vs threatened enforcement in debt collection').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(debt_dependency_traps, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(debt_tr_t0, debt_dependency_traps, theater_ratio, 0, 0.28).
narrative_ontology:measurement(debt_tr_t5, debt_dependency_traps, theater_ratio, 5, 0.32).
narrative_ontology:measurement(debt_tr_t10, debt_dependency_traps, theater_ratio, 10, 0.35).
narrative_ontology:measurement(debt_tr_t15, debt_dependency_traps, theater_ratio, 15, 0.38).

% Extraction over time
narrative_ontology:measurement(debt_be_t0, debt_dependency_traps, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(debt_be_t5, debt_dependency_traps, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(debt_be_t10, debt_dependency_traps, base_extractiveness, 10, 0.68).
narrative_ontology:measurement(debt_be_t15, debt_dependency_traps, base_extractiveness, 15, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(debt_dependency_traps, resource_allocation).
narrative_ontology:affects_constraint(debt_dependency_traps, wage_stagnation).
narrative_ontology:affects_constraint(debt_dependency_traps, predatory_lending_structures).
narrative_ontology:affects_constraint(debt_dependency_traps, credit_bureau_data_asymmetry).
narrative_ontology:affects_constraint(debt_dependency_traps, sovereign_debt_dependency).

% DUAL FORMULATION NOTE:
% Debt dependency traps are upstream of multiple domain-specific constraints: predatory lending structures (higher ε, snare-specific), credit bureau systems (information asymmetry enabling extraction), and sovereign debt mechanisms (geopolitical scaling of debt trap logic). Each domain has distinct extractiveness and suppression profiles but shares the fundamental debt-dependency mechanism. Stories in this family should link bidirectionally.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
