% ============================================================================
% CONSTRAINT STORY: 1986_reagan_balanced_budget_amendment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1986_reagan_balanced_budget_amendment, []).

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
 *   constraint_id: 1986_reagan_balanced_budget_amendment
 *   human_readable: Constitutional Balanced Budget Amendment (1986 Reagan Proposal)
 *   domain: governance/fiscal_policy
 *
 * SUMMARY:
 *   The balanced budget amendment represents a proposed constitutional
 *   constraint on federal fiscal discretion: revenues must equal expenditures
 *   except during declared national emergencies. Reagan and deficit hawks
 *   argue this 'hard lock' would prevent deficit spending by removing
 *   discretionary choice. The constraint exhibits genuine coordination
 *   function (solving intergenerational commons problem of deficit
 *   externalization) alongside significant asymmetric extraction (austerity
 *   imposed on agencies and welfare populations). The theater ratio is
 *   moderate (0.58) because while balanced-budget rhetoric is substantial,
 *   accounting loopholes and emergency overrides provide escape routes that
 *   the amendment drafters did not anticipate. The constraint has not been
 *   ratified; it remains in the piton phase — repeated proposal and debate
 *   without functional lock-in. If ratified, it would become tangled rope or
 *   snare depending on interpretation of emergency clause and accounting
 *   practices. The extractiveness value (0.52) reflects moderate severity:
 *   the constraint would bite during recessions (worst possible timing) but
 *   emergency override and accounting tricks provide partial relief.
 *   Suppression (0.65) is substantial because federal agencies face mandatory
 *   cuts with limited exit options, but not total because the constraint is
 *   conditional on revenue shortfalls and can be circumvented politically.
 *
 * KEY AGENTS:
 *   - Federal Agencies: Primary victim (powerless/trapped) — bear mandatory spending cuts; no exit or negotiation power
 *   - Welfare-Contingent Populations: Primary victim (powerless/trapped) — face simultaneous income loss and service cuts during recessions; depend on discretionary spending
 *   - Future Taxpayers and Creditors: Primary beneficiary (institutional/arbitrage) — benefit from reduced deficit and inflation risk; can exit via currency arbitrage
 *   - Congress and Political Leadership: Mixed actor (moderate/constrained) — benefits from political cover but loses fiscal flexibility; can exit via amendment revision
 *   - Financial Markets: Secondary beneficiary (powerful/arbitrage) — extract fiscal rents through higher government borrowing costs while gaining inflation protection
 *   - Deficit-Hawk Coalition: Beneficiary ideological group (institutional/arbitrage) — achieves size-of-government reduction objective; can exit by supporting amendment repeal
 *   - Constitutional Amendment Process: Institutional form (institutional/arbitrage) — persists as political ritual without functional ratification (piton phase)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1986_reagan_balanced_budget_amendment, 0.52).
domain_priors:suppression_score(1986_reagan_balanced_budget_amendment, 0.65).
domain_priors:theater_ratio(1986_reagan_balanced_budget_amendment, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1986_reagan_balanced_budget_amendment, extractiveness, 0.52).
narrative_ontology:constraint_metric(1986_reagan_balanced_budget_amendment, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(1986_reagan_balanced_budget_amendment, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1986_reagan_balanced_budget_amendment, tangled_rope).
narrative_ontology:human_readable(1986_reagan_balanced_budget_amendment, "Constitutional Balanced Budget Amendment (1986 Reagan Proposal)").
narrative_ontology:topic_domain(1986_reagan_balanced_budget_amendment, "governance/fiscal_policy").

domain_priors:requires_active_enforcement(1986_reagan_balanced_budget_amendment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1986_reagan_balanced_budget_amendment, future_taxpayers).
narrative_ontology:constraint_beneficiary(1986_reagan_balanced_budget_amendment, inflation_concerned_creditors).
narrative_ontology:constraint_beneficiary(1986_reagan_balanced_budget_amendment, balanced_budget_ideological_coalition).
narrative_ontology:constraint_victim(1986_reagan_balanced_budget_amendment, federal_agencies).
narrative_ontology:constraint_victim(1986_reagan_balanced_budget_amendment, cyclical_stabilization_capacity).
narrative_ontology:constraint_victim(1986_reagan_balanced_budget_amendment, welfare_contingent_populations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FEDERAL AGENCIES (SNARE) — Cannot exit the constraint; face mandatory spending reductions if revenue declines or emergencies exhaust reserves. No negotiating power. Extraction is maximal and coercive: forced austerity regardless of operational need. Emergency loophole provides minimal relief (requires formal declaration, legislative process, political friction). Bears full cost of fiscal discipline imposed from above.
constraint_indexing:constraint_classification(1986_reagan_balanced_budget_amendment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WELFARE-CONTINGENT POPULATIONS (SNARE) — Trapped by dependency on discretionary federal spending (education, healthcare, infrastructure, social services). If budget constraint binds during recession (when need is highest), these populations face simultaneous income loss and service cuts — the worst scenario. No exit option; cannot organize effectively against constitutional amendment. Extraction intensifies during economic downturns when extraction is most damaging.
constraint_indexing:constraint_classification(1986_reagan_balanced_budget_amendment, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: CONGRESS (TANGLED ROPE) — Genuinely constrained by the balanced budget requirement, but also benefits from the coordination function: the amendment provides political cover for difficult budget-balancing decisions ('we had no choice'). Experiences mixed coordination (solves the commons problem of unlimited spending incentives) and extraction (loses discretionary fiscal flexibility). Suppression is real but not total — Congress retains emergency override and can redefine 'balanced' through accounting practices. Can exit via constitutional amendment (very costly but possible).
constraint_indexing:constraint_classification(1986_reagan_balanced_budget_amendment, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: FUTURE TAXPAYERS AND CREDITORS (ROPE) — Primary beneficiaries. The constraint coordinates behavior toward deficit reduction, preventing intergenerational debt transfer. Benefits from reduced inflation risk and lower future tax burdens. Has arbitrage exit: can exit by holding assets in non-dollar denominations or other countries if deficit discipline fails. Experiences minimal suppression — constraint aligns with their preferences. The constraint solves a genuine coordination problem (present generations externalizing costs to future ones).
constraint_indexing:constraint_classification(1986_reagan_balanced_budget_amendment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: FINANCIAL MARKETS (TANGLED ROPE) — Benefit from credible deficit constraint (lower inflation risk, stable bond values). But also extract through the constraint: markets gain pricing power in a capital-scarce environment. Government must bid higher for funds; private investors receive higher returns. The constraint coordinates fiscal discipline AND extracts fiscal rents. Exit option: arbitrage (shift investments if US fiscal credibility declines). Theater component: 'balanced budget' can be gamed through accounting (off-budget items, trust fund raids) — constraint is partially performative.
constraint_indexing:constraint_classification(1986_reagan_balanced_budget_amendment, tangled_rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: CONSTITUTIONAL AMENDMENT PROCESS (PITON) — The amendment machinery persists as an institutional form, but 1986 onward shows repeated failed passage attempts (1995, 1997, 2011, etc.). The constraint exists as a persistent political theater — invoked repeatedly, never actually ratified. The amendment remains a symbol of fiscal discipline without functional lock-in. Theater ratio high because the ritual of proposing/debating the amendment performs 'fiscal responsibility' while the actual mechanism never materializes. If the amendment were actually ratified and triggered, it would shift to Snare or Tangled Rope; but in unreified form, it is primarily performative institutional gesture.
constraint_indexing:constraint_classification(1986_reagan_balanced_budget_amendment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: FISCAL REFORM ADVOCATES (SCAFFOLD) — Organized agents (Brookings, progressive economists, deficit-hawk think tanks) see the balanced budget constraint as a temporary mechanism to solve a real coordination problem: runaway spending incentives. But many scaffold advocates recognize a sunset: as budget processes mature and political norms around fiscal responsibility strengthen, the constitutional straitjacket becomes unnecessary. The constraint provides temporary coordination at the cost of lost flexibility, with expectation of eventual replacement by internalized fiscal norms. Suppression is real but not permanent — advocates see a path to lower suppression over time.
constraint_indexing:constraint_classification(1986_reagan_balanced_budget_amendment, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational lens, some observers argue that government budgets must obey conservation laws: spending cannot exceed revenue indefinitely. The constraint appears as an immutable law of fiscal physics — 'you cannot spend money you don't have' — just as families cannot. This perspective naturalizes the constraint as inherent to economics rather than a contingent policy choice. However, structural data contradicts this: the constraint benefits specific actors (creditors, future-focused taxpayers), harms others (welfare-contingent populations, stabilization capacity), and is subject to emergency override. This is a FALSE SUMMIT: naturalization of a value-laden policy choice as natural law.
constraint_indexing:constraint_classification(1986_reagan_balanced_budget_amendment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1986_reagan_balanced_budget_amendment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1986_reagan_balanced_budget_amendment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1986_reagan_balanced_budget_amendment, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(1986_reagan_balanced_budget_amendment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(1986_reagan_balanced_budget_amendment, TR),
    TR >= 0.70.

:- end_tests(1986_reagan_balanced_budget_amendment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint would force federal agencies and welfare populations into austerity, but the severity is mitigated by three factors: (1) the constraint only binds if revenues decline or spending exceeds revenue (not automatically triggered), (2) emergency override provides pressure relief valve for acute crises, and (3) accounting gimmicks and trust fund manipulation offer partial circumvention. Without these escape routes, extractiveness would be 0.70+. The initial value (0.35) reflects pre-Reagan deficit conditions where the constraint was not yet a serious proposal; the trajectory toward 0.52 reflects growing political traction after 1980s. Suppression (0.65): Substantial but not total. Federal agencies face binding constraints on discretionary spending with limited negotiation capacity. But suppression is not as severe as a true snare (0.60+) would require because (1) emergency override exists, (2) Congress can redefine the balanced budget through accounting, (3) revenue increases remain an option (though politically harder than spending cuts). Theater ratio (0.58): Moderate. The amendment's reality is that it has been proposed and defeated repeatedly without ratification. The political debate around it (performative display of fiscal responsibility) is substantial, but the actual constraint machinery is not yet operative. The theater value reflects this gap between symbolic commitment and functional constraint. If the amendment were ratified and enforced strictly, theater would drop to 0.35-0.40. If it were ratified but accounting loopholes dominated, theater would rise to 0.75+.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. Federal agencies see coercive austerity with no exit (Snare). Welfare populations see synchronized economic vulnerability (Snare). Congress sees a coordination mechanism with political cover but also inflexibility (Tangled Rope). Future taxpayers see deficit prevention (Rope). Financial markets see profitable capital scarcity (Tangled Rope — benefit plus extraction). The amendment itself, unreified in debate since 1986, appears theatrical (Piton). Reform advocates see temporary coordination with eventual sunset (Scaffold). The analytical observer risks seeing immutable economic law (Mountain). These six readings of the same structural phenomenon illustrate why perspectival indexing is essential: a constraint does not have a single classification; it has a presheaf of classifications across observation sites.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from the agent's structural position relative to extraction flow. Federal agencies are trapped with no exit — d ≈ 0.95, producing maximum experienced extractiveness. Welfare populations are trapped but lack organizational power to resist — d ≈ 0.90. Congress is moderate-power constrained — they face the rule but can circumvent through accounting or amend the constitution; d ≈ 0.55-0.60. Future taxpayers and creditors are institutional beneficiaries with arbitrage exit (can exit by holding foreign assets or inflation-indexed securities) — d ≈ 0.10-0.15. Financial markets are powerful and arbitrage-capable — d ≈ 0.35 (extract rents but not trapped). The False Summit perspective uses analytical observer position: from a natural-law framing, budgets 'must' balance just as household accounts must. But the structural beneficiaries (deficit hawks, inflation-concerned creditors) reveal this as naturalization of a contingent policy choice that benefits specific groups. The analytical observer's mountain is a false summit; the engine will detect this via FSM and reclassify.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the balanced budget amendment is NOT PURE COORDINATION (not Rope) and NOT PURE EXTRACTION (not Snare). It is genuinely hybrid. The constraint solves the real problem of intergenerational deficit externalization (coordination function present) while imposing asymmetric costs on current federal agencies and welfare-contingent populations (extraction present). The constraint requires ACTIVE ENFORCEMENT (Congress must pass it, states must ratify it, courts must interpret emergency override narrowly) — satisfying the tangled rope gate. The beneficiary group (future taxpayers, inflation-concerned creditors) is clearly identified and gains specific advantages (lower future taxes, inflation protection). The victim group (federal agencies, welfare populations) clearly loses (austerity, service cuts). The constraint exhibits SUPPRESSION ≥ 0.40 (agencies cannot negotiate, welfare populations cannot exit). These properties confirm tangled rope classification. The false summit risk is that the 'natural law' framing (budgets must balance) disguises the policy choice. The engine will flag this via FSM and require omega analysis of whether the constraint is genuinely natural or constructed to benefit specific interests.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    emergency_override_scope,
    'How broadly or narrowly will the ''national emergency'' override be interpreted in practice? Does inflation count? Recession? Major pandemic?',
    'Historical analysis of emergency declarations; litigation establishing scope of executive/congressional power to invoke override; empirical comparison to other constitutional emergency clauses (e.g., suspension of habeas corpus, war powers)',
    'Narrow interpretation: constraint binds severely, extracting from agencies and welfare populations, enabling snare dynamics. Broad interpretation: constraint becomes largely performative, enabling agencies to escape austerity via creative emergency declarations, shifting classification toward piton (theatrical).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(emergency_override_scope, conceptual, 'Scope of national emergency override in practice').

omega_variable(
    accounting_gimmicks_persistence,
    'Can Congress circumvent the balanced budget requirement through off-budget accounting, trust fund manipulation, or creative revenue definitions?',
    'Auditing practices; legal challenges; international comparison to other countries with constitutional fiscal constraints (e.g., Germany) and their accounting drift over time',
    'If accounting loopholes are extensive: constraint is primarily theater, shifting toward piton. If accounting is tightly controlled: constraint binds hard, intensifying snare dynamics for federal agencies and welfare populations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accounting_gimmicks_persistence, empirical, 'Durability of accounting against circumvention').

omega_variable(
    fiscal_multiplier_lock_in,
    'If the constraint binds during a recession, forcing pro-cyclical austerity, what is the net effect on GDP, unemployment, and tax revenue? Does austerity make the deficit worse?',
    'Econometric analysis of countries with fiscal rules during downturns; simulation of balanced-budget mechanics under various recessionary scenarios; comparison to automatic stabilizer alternatives',
    'If multipliers are high (austerity deepens recession): constraint extracts via GDP loss, unemployment, and reduced tax base — victims multiply, extraction intensifies, snare deepens. If multipliers are low: constraint is mildly painful but not catastrophic, tangled rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_multiplier_lock_in, empirical, 'Fiscal multiplier effects of pro-cyclical austerity constraint').

omega_variable(
    ideological_lock_in_vs_instrumental_choice,
    'Is the balanced budget amendment primarily an instrumental policy choice (solves a real coordination problem) or primarily an ideological commitment (expresses a worldview about government size)?',
    'Historical analysis of amendment proponents'' stated rationales; comparison of austerity intensity across countries with and without balanced budget rules; econometric comparison to countries that abandon the rule after adopting it',
    'If instrumental: the constraint is genuine rope or tangled rope addressing a real problem. If ideological: the constraint naturalizes a value preference as natural law (false summit) and enables extraction of minority welfare interests in service of majority ideology. Classification hinges on whether the beneficiary group (deficit-hawk coalition) is solving a coordination problem or imposing a preference.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ideological_lock_in_vs_instrumental_choice, conceptual, 'Instrumental vs ideological nature of balanced budget requirement').

omega_variable(
    generational_extraction_asymmetry,
    'Does the amendment transfer intergenerational welfare upward (older taxpayers avoid current fiscal costs) or downward (future generations avoid inheriting debt)? Net direction of extraction?',
    'Overlapping-generations fiscal accounting; lifetime tax and benefit analysis by cohort; comparison of present-value lifetime taxes paid vs present-value lifetime benefits received, controlling for the balanced-budget constraint',
    'If extraction is upward: older voters benefit at expense of younger/unborn — amendment is intergenerational snare. If extraction is downward: constraint prevents intergenerational theft, functioning as rope. If symmetric: constraint is neutral coordination mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generational_extraction_asymmetry, empirical, 'Intergenerational distribution of constraint costs and benefits').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1986_reagan_balanced_budget_amendment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bba_tr_t0, 1986_reagan_balanced_budget_amendment, theater_ratio, 0, 0.42).
narrative_ontology:measurement(bba_tr_t5, 1986_reagan_balanced_budget_amendment, theater_ratio, 5, 0.52).
narrative_ontology:measurement(bba_tr_t10, 1986_reagan_balanced_budget_amendment, theater_ratio, 10, 0.58).

% Extraction over time
narrative_ontology:measurement(bba_be_t0, 1986_reagan_balanced_budget_amendment, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bba_be_t5, 1986_reagan_balanced_budget_amendment, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(bba_be_t10, 1986_reagan_balanced_budget_amendment, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1986_reagan_balanced_budget_amendment, resource_allocation).
narrative_ontology:affects_constraint(1986_reagan_balanced_budget_amendment, fiscal_commons_problem).
narrative_ontology:affects_constraint(1986_reagan_balanced_budget_amendment, pro_cyclical_austerity_trap).
narrative_ontology:affects_constraint(1986_reagan_balanced_budget_amendment, intergenerational_burden_shifting).

% DUAL FORMULATION NOTE:
% The balanced budget amendment is downstream of the fiscal commons problem (multiple actors incentivized to spend beyond their means in aggregate) and upstream of specific austerity mechanisms (mandatory spending cuts, welfare reductions) that would be triggered if the amendment were ratified. The amendment's extractiveness derives from how it would force resolution of the commons problem via involuntary austerity rather than voluntary fiscal discipline.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(1986_reagan_balanced_budget_amendment, institutional, 0.12).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
