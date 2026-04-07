% ============================================================================
% CONSTRAINT STORY: sotu_1999_clinton_medicare_trust_fund_surplus_allocation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1999_clinton_medicare_trust_fund_surplus_allocation, []).

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
 *   constraint_id: sotu_1999_clinton_medicare_trust_fund_surplus_allocation
 *   human_readable: Medicare Trust Fund Solvency via Budget Surplus Dedication (1999 Clinton Proposal)
 *   domain: healthcare/fiscal_policy
 *
 * SUMMARY:
 *   The Clinton administration's 1999 proposal to dedicate approximately
 *   one-sixth of the projected 10-year budget surplus to extend Medicare
 *   Trust Fund solvency and add prescription drug coverage represents a
 *   hybrid coordination-extraction mechanism that locks in fiscal commitments
 *   contingent on macroeconomic conditions. The constraint establishes a
 *   structural tie between transient fiscal surpluses and permanent
 *   entitlement obligations, creating intergenerational transfer dynamics.
 *   Elderly Americans benefit immediately from expanded coverage and
 *   guaranteed solvency through 2020. Pharmaceutical manufacturers benefit
 *   from an expanded insured customer base. Future taxpayers face extraction
 *   through generational burden-shifting when surpluses evaporate. The
 *   constraint exhibits all six classification types depending on observation
 *   position: a natural law (demographic inevitability of aging), a temporary
 *   scaffold (reform-enabling mechanism with a 15-year sunset), a degraded
 *   piton (theater-driven crisis response without structural reform), pure
 *   extraction for future workers, and hybrid coordination-extraction for
 *   organized institutional actors.
 *
 * KEY AGENTS:
 *   - Elderly Americans: Primary beneficiary (powerful/arbitrage) — immediate access to prescription drug coverage and Trust Fund solvency guarantee
 *   - Pharmaceutical Industry: Secondary beneficiary (organized/constrained) — expanded customer base through government insurance mechanism
 *   - Medicare Administration: Institutional actor (institutional/constrained) — executes the commitment; both benefits (solvency) and bears costs (administrative burden and future obligation)
 *   - Future Taxpayers (post-2014): Primary victim (powerless/trapped) — locked into Trust Fund obligation when surpluses reverse; faces extraction through mandatory contributions or benefit reductions
 *   - Non-Elderly Public Goods: Secondary victim (powerless/trapped) — competing discretionary spending priorities (infrastructure, education, defense) lose allocation share to entitlement commitment
 *   - Congress—Reform-Oriented Bloc: Organized agents (organized/constrained) — see the constraint as temporary scaffolding enabling structural entitlement reform, but constrained by political economy
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing demographic aging as justification for contingent institutional choices
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, 0.38).
domain_priors:suppression_score(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, 0.42).
domain_priors:theater_ratio(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, tangled_rope).
narrative_ontology:human_readable(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, "Medicare Trust Fund Solvency via Budget Surplus Dedication (1999 Clinton Proposal)").
narrative_ontology:topic_domain(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, "healthcare/fiscal_policy").

domain_priors:requires_active_enforcement(sotu_1999_clinton_medicare_trust_fund_surplus_allocation).
narrative_ontology:has_sunset_clause(sotu_1999_clinton_medicare_trust_fund_surplus_allocation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, elderly_americans).
narrative_ontology:constraint_beneficiary(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, pharmaceutical_manufacturers).
narrative_ontology:constraint_beneficiary(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, medicare_administration).
narrative_ontology:constraint_victim(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, future_taxpayers).
narrative_ontology:constraint_victim(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, non_elderly_public_goods).
narrative_ontology:constraint_victim(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, discretionary_spending_priorities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE TAXPAYER (SNARE) — Faces the structural entrapment of having committed future revenue to a backward-looking obligation. The 15-year window does not extend to their prime earning years, but the Trust Fund solvency mechanism locks in a revenue commitment that persists beyond 2014. If surpluses evaporate, the extraction mechanism (mandatory Trust Fund contributions) remains, shifting burden to younger workers. Maximum experienced extraction — no exit option, generational time horizon reveals the intergenerational transfer structure.
constraint_indexing:constraint_classification(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ELDERLY AMERICANS (ROPE) — Primary beneficiary group. Experiences the constraint as pure coordination: prescription drug coverage solves a genuine collective action problem (seniors facing high OOP costs). The 15-year horizon means most of the benefit accrues within their remaining lifetime. Powerful exit option (arbitrage) because the benefit is non-excludable across age cohort — they cannot be forced to forgo the benefit. Net beneficiary — extraction runs toward this agent through the commitment mechanism.
constraint_indexing:constraint_classification(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: PHARMACEUTICAL INDUSTRY (TANGLED ROPE) — Secondary beneficiary with constrained exit. The proposal expands the eligible customer base (seniors with insurance-backed drug purchasing power) and establishes government as a major purchaser. Both coordination function (public insurance expands market) and asymmetric extraction (price negotiations constrained by coverage mandates). Constrained exit because companies remain dependent on Medicare market access but face regulatory oversight on pricing. Some agency through lobbying but ultimately subordinate to political allocation decisions.
constraint_indexing:constraint_classification(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MEDICARE ADMINISTRATION (TANGLED ROPE) — Organizational beneficiary and victim simultaneously. Benefits from explicit Trust Fund supplementation (removes immediate solvency crisis); extracted from because the 15-year commitment locks in an obligation that may constrain administrative flexibility. Constrained exit because the organization is the administration vehicle — cannot exit the commitment without Congressional action. Active enforcement required (annual Trust Fund accounting, prescription drug benefit implementation) creates ongoing administrative burden classified as extraction cost.
constraint_indexing:constraint_classification(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: CONGRESS—REFORM-ORIENTED BLOC (SCAFFOLD) — Organized agents (deficit hawks, fiscal conservatives, long-term planning advocates) see the surplus allocation as a temporary coordination mechanism with sunset logic: use a temporary fiscal surplus to address a specific crisis (Trust Fund insolvency through 2020), then transition to structural reform (payroll tax adjustment, benefit restructuring, or means-testing). Low effective extraction because this perspective has agency and explicitly envisions an exit path beyond the 15-year window. The constraint is perceived as scaffolding for a transition to sustainable entitlement structure.
constraint_indexing:constraint_classification(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: THE FISCAL SURPLUS THEATER (PITON) — At civilizational/global scale, the constraint reveals itself as largely performative. The 1999 surplus is a temporary phenomenon (driven by demographic bulge, tech bubble, one-time revenues). Dedicating it to Trust Fund extension performs fiscal responsibility without addressing structural entitlement mathematics. The theater ratio (0.55) reflects that the mechanism substitutes announcement of solvency extension for structural reform. The surplus allocation buys political capital through apparent crisis resolution while the underlying actuarial problem persists. Post-2001, when surpluses evaporate, the mechanism degrades to institutional inertia — a rule nominally in place but decoupled from fiscal reality.
constraint_indexing:constraint_classification(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational scale, the aging of the Baby Boom is a demographic given. The Trust Fund solvency crisis is therefore treated as an immutable constraint: the ratio of workers to retirees will decline; the cost of benefits will rise; some mechanism (taxation, benefit reduction, or surplus reallocation) must equilibrate the system. This perspective risks naturalizing what is actually a contingent institutional choice — the trust fund structure itself, the benefit formula, the financing mechanism are all constructed. Declaring beneficiaries below reveals this as a false summit candidate.
constraint_indexing:constraint_classification(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1999_clinton_medicare_trust_fund_surplus_allocation_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, TR),
    TR >= 0.70.

:- end_tests(sotu_1999_clinton_medicare_trust_fund_surplus_allocation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The mechanism provides genuine benefits (drug coverage) to elderly Americans and coordinates a solution to an immediate Trust Fund crisis. However, it also commits future revenue in a way that shifts burden to cohorts who don't benefit proportionately. The trajectory shows extractiveness rising from 0.22 to 0.52 over 15 years as surpluses eventually evaporate (post-2001) and the commitment persists as an unfunded obligation. Suppression (0.42): Moderate. Constraints on alternatives include the political difficulty of raising payroll taxes immediately, the electoral salience of Medicare, and the apparent availability of surplus revenue. However, suppression is not total — structural reform options (benefit restructuring, means-testing, payroll tax increases) remain formally available; they are politically constrained rather than technically impossible. Theater ratio (0.55): Moderate-high. The mechanism performs fiscal responsibility and crisis resolution (announcement that Trust Fund is now solvent until 2020) without addressing the underlying actuarial problem (aging of Baby Boom, benefit growth outpacing revenue growth). The theater serves a genuine coordination function (calming beneficiary anxiety) but masks the structural obligation. Post-2001, theater ratio rises as the mechanism persists as a rule despite fundamental fiscal conditions changing, exemplifying Piton degradation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    surplus_persistence_assumption,
    'Will the fiscal surplus persist at levels sufficient to fund the 15-year commitment?',
    'Historical tracking of actual budget surpluses 1999-2014 against projections; correlation with economic cycles, demographic changes, and discretionary spending patterns',
    'If surplus persists: constraint functions as intended (Rope). If surplus evaporates: constraint degrades to unfunded obligation (Snare from future-taxpayer perspective).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(surplus_persistence_assumption, empirical, 'Fiscal surplus persistence over 15-year window').

omega_variable(
    entitlement_obligation_priority,
    'Is the Trust Fund commitment truly discretionary (suspension possible if surpluses end) or de facto mandatory (political cost of interruption makes it functionally binding)?',
    'Analysis of Congressional behavior 2001-2005 when surpluses reversed; examination of whether Trust Fund supplements continued despite budget deficits',
    'If discretionary: constraint is a Scaffold with genuine sunset. If de facto mandatory: constraint is Tangled Rope with hidden extraction — commits future revenue despite fiscal reversal.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entitlement_obligation_priority, empirical, 'Whether the surplus commitment is truly discretionary or de facto mandatory').

omega_variable(
    structural_entitlement_reform_likelihood,
    'Does the temporary surplus buffer actually enable structural entitlement reform, or does it postpone reform indefinitely by eliminating the political urgency?',
    'Post-2014 analysis: did the Trust Fund extension enable policy space for payroll tax increases or benefit restructuring, or did it create a false sense of crisis resolved that prevented reform?',
    'If enables reform: Scaffold classification is validated. If postpones: constraint is revealed as Piton (performative, theater-driven, ultimately dysfunctional).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_entitlement_reform_likelihood, empirical, 'Whether temporary surplus buffer enables or postpones structural entitlement reform').

omega_variable(
    prescription_drug_benefit_cost_trajectory,
    'What proportion of the budget allocation actually funds expanded prescription drug coverage vs. administrative costs vs. Trust Fund interest?',
    'CMS accounting data post-2000 on pharmaceutical spending as percentage of total Trust Fund allocation; comparison against original projections',
    'If majority funds drug coverage: genuine coordination function (Rope elements valid). If majority diverts to interest/administration: extraction hidden in administrative overhead (Snare elements dominate).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(prescription_drug_benefit_cost_trajectory, empirical, 'Allocation between drug coverage, administration, and interest accumulation').

omega_variable(
    intergenerational_burden_distribution,
    'How does the burden of extending Trust Fund solvency distribute across age cohorts? Who ultimately bears the cost — current retirees, working-age beneficiaries, future workers, or future beneficiaries?',
    'Generational accounting analysis; comparison of lifetime tax contributions vs. lifetime benefits by cohort; projection of post-2020 Trust Fund deficits',
    'If burden shifts to future workers: Snare classification from their perspective is validated. If burden distributed across cohorts: Tangled Rope captures the hybrid coordination-extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_burden_distribution, empirical, 'Intergenerational distribution of Trust Fund solvency costs').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(medicare_surplus_tr_t0, sotu_1999_clinton_medicare_trust_fund_surplus_allocation, theater_ratio, 0, 0.48).
narrative_ontology:measurement(medicare_surplus_tr_t5, sotu_1999_clinton_medicare_trust_fund_surplus_allocation, theater_ratio, 5, 0.52).
narrative_ontology:measurement(medicare_surplus_tr_t10, sotu_1999_clinton_medicare_trust_fund_surplus_allocation, theater_ratio, 10, 0.58).
narrative_ontology:measurement(medicare_surplus_tr_t15, sotu_1999_clinton_medicare_trust_fund_surplus_allocation, theater_ratio, 15, 0.65).

% Extraction over time
narrative_ontology:measurement(medicare_surplus_be_t0, sotu_1999_clinton_medicare_trust_fund_surplus_allocation, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(medicare_surplus_be_t5, sotu_1999_clinton_medicare_trust_fund_surplus_allocation, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(medicare_surplus_be_t10, sotu_1999_clinton_medicare_trust_fund_surplus_allocation, base_extractiveness, 10, 0.38).
narrative_ontology:measurement(medicare_surplus_be_t15, sotu_1999_clinton_medicare_trust_fund_surplus_allocation, base_extractiveness, 15, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, resource_allocation).
narrative_ontology:affects_constraint(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, medicare_payroll_tax_structure).
narrative_ontology:affects_constraint(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, entitlement_solvency_crisis_dynamic).

% DUAL FORMULATION NOTE:
% This constraint is downstream of the structural entitlement mathematics (aging demographics, benefit formulas, financing mechanisms) and upstream of actual policy implementation (pharmaceutical pricing, benefit design, coverage thresholds). The surplus allocation mechanism couples these by making entitlement expansion conditional on transient fiscal conditions rather than dedicated revenue or structural reform.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1999_clinton_medicare_trust_fund_surplus_allocation, powerless, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
