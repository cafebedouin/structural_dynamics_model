% ============================================================================
% CONSTRAINT STORY: 1961_kennedy_fiscal_restraint_budget_discipline
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_1961_kennedy_fiscal_restraint_budget_discipline, []).

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
 *   constraint_id: 1961_kennedy_fiscal_restraint_budget_discipline
 *   human_readable: Fiscal Restraint and Budget Discipline (JFK Era)
 *   domain: governance/fiscal_policy
 *
 * SUMMARY:
 *   The fiscal restraint constraint operating in the early 1960s created an
 *   institutional gate preventing discretionary spending growth despite
 *   legitimate program demands. President Kennedy enforced this discipline
 *   through messaging, Congressional coordination, and procedural mechanisms
 *   (pay-as-you-go postal financing, mandatory tax loophole closure before
 *   new spending). The constraint solved a real problem — preventing currency
 *   crises and international loss of confidence in the dollar — while
 *   imposing costs on programs that could not be funded and constituencies
 *   unable to access new federal initiatives. The constraint exhibits tangled
 *   rope structure: it coordinates international creditor confidence and
 *   domestic macroeconomic stability (genuine coordination function) while
 *   extracting by denying programs and deferring public investment
 *   (asymmetric costs to powerless constituencies). The theater ratio
 *   increased during the interval as the 'discipline' became increasingly
 *   rhetorical (presidential speeches about sacrifice) rather than structural
 *   (actual spending ceilings). By the late 1960s, enforcement had degraded
 *   into performance of restraint rather than execution of actual limits.
 *
 * KEY AGENTS:
 *   - International Creditors and Currency Markets: Primary beneficiary (institutional/arbitrage) — gain stable Treasury instruments and prevent currency crisis; maintain arbitrage exit option to redirect capital
 *   - Unmet Program Constituencies: Primary victim (powerless/trapped) — denied access to new federal programs; cannot exit national fiscal framework; maximum suppression and extraction
 *   - Congressional Discretionary Spenders: Secondary agent (moderate/constrained) — experience mixed benefits (authority to spend within bounds) and costs (popular programs denied); face career risk from both sides (fiscal hawks and spending advocates)
 *   - Fiscal Reform Coalition: Organized actors (organized/mobile) — budget hawks and efficiency advocates who see the crude constraint as temporary, to be replaced by structural tax reform and spending efficiency improvements
 *   - Budget Enforcement Bureaucracy: Institutional routine-maintainers (institutional/constrained) — Treasury, OMB, Congressional Budget Office maintain the enforcement procedures; see own role as degraded (performative) over time
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing the politically-contingent constraint as inevitable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(1961_kennedy_fiscal_restraint_budget_discipline, 0.52).
domain_priors:suppression_score(1961_kennedy_fiscal_restraint_budget_discipline, 0.58).
domain_priors:theater_ratio(1961_kennedy_fiscal_restraint_budget_discipline, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(1961_kennedy_fiscal_restraint_budget_discipline, extractiveness, 0.52).
narrative_ontology:constraint_metric(1961_kennedy_fiscal_restraint_budget_discipline, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(1961_kennedy_fiscal_restraint_budget_discipline, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(1961_kennedy_fiscal_restraint_budget_discipline, tangled_rope).
narrative_ontology:human_readable(1961_kennedy_fiscal_restraint_budget_discipline, "Fiscal Restraint and Budget Discipline (JFK Era)").
narrative_ontology:topic_domain(1961_kennedy_fiscal_restraint_budget_discipline, "governance/fiscal_policy").

domain_priors:requires_active_enforcement(1961_kennedy_fiscal_restraint_budget_discipline).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(1961_kennedy_fiscal_restraint_budget_discipline, international_creditors).
narrative_ontology:constraint_beneficiary(1961_kennedy_fiscal_restraint_budget_discipline, long_term_debt_sustainability).
narrative_ontology:constraint_beneficiary(1961_kennedy_fiscal_restraint_budget_discipline, currency_stability_advocates).
narrative_ontology:constraint_victim(1961_kennedy_fiscal_restraint_budget_discipline, new_program_constituencies).
narrative_ontology:constraint_victim(1961_kennedy_fiscal_restraint_budget_discipline, discretionary_spending_advocates).
narrative_ontology:constraint_victim(1961_kennedy_fiscal_restraint_budget_discipline, deferred_public_investment).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNMET PROGRAM CONSTITUENCY (SNARE) — Trapped agents with no exit from the budget constraint. Citizens needing social services, infrastructure investment, or education programs cannot exit the national fiscal framework. The constraint extracts by denying programs that would exist in an unconstrained budget. Maximum experienced suppression: these agents cannot bypass federal budget discipline without relocating.
constraint_indexing:constraint_classification(1961_kennedy_fiscal_restraint_budget_discipline, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONGRESSIONAL DISCRETIONARY SPENDERS (TANGLED ROPE) — Face constrained exit: politically costly to vote against constituent demands for spending, but cannot freely appropriate without triggering fiscal responsibility backlash. The constraint both enables (provides spending authority within fiscal bounds) and extracts (prevents spending growth that would be electorally popular). Moderately experienced extraction with mixed benefits.
constraint_indexing:constraint_classification(1961_kennedy_fiscal_restraint_budget_discipline, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNATIONAL CREDITORS AND CURRENCY MARKETS (ROPE) — Primary beneficiary. Experiences the constraint as pure coordination mechanism: fiscal discipline enables stable bond markets, maintains dollar credibility, and prevents currency crises. Exit option is arbitrage — they can redirect capital elsewhere, but U.S. Treasury instruments remain preferred. Net beneficiary with high exit capacity.
constraint_indexing:constraint_classification(1961_kennedy_fiscal_restraint_budget_discipline, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REFORMIST BUDGET COALITION (SCAFFOLD) — Organized agents (budget hawks, fiscal conservatives, deficit hawks) see the constraint as a temporary solution with sunset logic. They advocate for structural tax reform, spending efficiency audits, and budget process changes that would replace the crude 'no new programs' rule with more sophisticated allocation mechanisms. Sunset horizon: 10-15 years if structural tax reform succeeds.
constraint_indexing:constraint_classification(1961_kennedy_fiscal_restraint_budget_discipline, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: BUDGET ENFORCEMENT RITUAL (PITON) — The enforcement of fiscal restraint through Congressional procedures, presidential messaging, and Treasury Department coordination is increasingly performative. Theater ratio elevated: much of the 'discipline' is rhetorical (speeches about fiscal responsibility, symbolic vetoes) rather than structural (actual spending reductions). The constraint persists through institutional inertia — agencies continue pay-as-you-go postal service financing and tax loophole closure procedures, but the backbone of restraint (real constraint on total appropriations) has degraded as emergency and mandatory spending have grown.
constraint_indexing:constraint_classification(1961_kennedy_fiscal_restraint_budget_discipline, piton,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational analytical perspective, some fiscal restraint appears to be a natural economic law: governments that spend beyond their productive capacity face currency debasement, inflation, and capital flight. The constraint appears immutable — no nation can indefinitely spend without discipline. However, the structural data reveals this as a false summit: fiscal space is partly a product of institutional choices (tax policy, central bank coordination, international reserve currency status), not pure economic law. Different fiscal constraints apply to the U.S., Japan, and emerging-market nations because of structural political-economic differences.
constraint_indexing:constraint_classification(1961_kennedy_fiscal_restraint_budget_discipline, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(1961_kennedy_fiscal_restraint_budget_discipline_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(1961_kennedy_fiscal_restraint_budget_discipline, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(1961_kennedy_fiscal_restraint_budget_discipline, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(1961_kennedy_fiscal_restraint_budget_discipline, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(1961_kennedy_fiscal_restraint_budget_discipline, TR),
    TR >= 0.70.

:- end_tests(1961_kennedy_fiscal_restraint_budget_discipline_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderately high. The constraint denies legitimate program spending and defers public investment, creating real costs for unmet constituencies. However, the extraction is not as severe as a pure snare (ε ≥ 0.66) because the constraint also genuinely solves a coordination problem — preventing fiscal crisis that would harm all parties. The moderate value reflects the hybrid nature: real benefits to macroeconomic stability coexist with real costs to program access. Suppression (0.58): Moderately high. The barrier to exit is substantial — Congressional members face political cost for voting against constituent demands, yet also face fiscal responsibility norms and international creditor pressure. Citizens needing programs cannot exit the federal framework. But suppression is not total — emergency spending (war, disaster) has historically overridden the constraint. Theater ratio (0.61): Moderately high. The enforcement mechanism has become increasingly performative over the interval — speeches about fiscal responsibility, symbolic gestures toward tax reform and postal efficiency, rhetorical vetoes. By the terminal time point, the actual enforcement is more ritual than mechanism, though the norm still constrains some spending.
 *
 * PERSPECTIVAL GAP:
 *   The original research group sees pure coordination (rope) — fiscal discipline enables stable markets. Congressional spenders see mixed coordination and extraction (tangled rope) — they can spend within bounds but popular programs are denied. Unmet constituencies see pure extraction (snare) — they bear costs with no benefits. Budget reformers see a temporary solution with sunset (scaffold) — structural tax and efficiency reforms will replace the crude constraint. The enforcement bureaucracy sees its own process as degraded (piton) — procedures persist through inertia despite reduced functional verification of actual spending limits. The analytical observer risks seeing natural economic law (mountain) — fiscal discipline appears inevitable — but the structural data reveals this as contingent on specific institutional choices (Fed independence, reserve currency status, tax administration, international creditor coordination).
 *
 * DIRECTIONALITY LOGIC:
 *   International creditors benefit from the constraint and have arbitrage exit options (they can move capital elsewhere but prefer U.S. Treasuries). Their directionality d is low (~0.15), producing negative or minimal effective extraction chi — they experience the constraint as pure coordination. Congressional spenders face constrained exit — politically costly to ignore constituent demands but also costly to defy fiscal norms — so d is moderate (~0.50-0.55), producing moderate chi and mixed perspective. Unmet constituencies face trapped exit — they cannot access the federal budget mechanism — so d is high (~0.90), producing high chi and snare perspective. The constraint's asymmetry is structural: some agents (international creditors) benefit and can exit; others (program constituencies) lose and cannot exit.
 *
 * MANDATROPHY ANALYSIS:
 *   The fiscal restraint constraint resolves the mandatrophy by demonstrating genuine hybrid structure: it is simultaneously a coordination mechanism (solves creditor confidence problem) and an extraction mechanism (denies programs to powerless constituencies). No single classification is 'correct' — the presheaf over agent positions contains rope, tangled_rope, snare, and scaffold. The constraint is NOT a false choice between 'pure coordination' and 'pure extraction.' It is both. This reflects a deep political economy insight: macroeconomic constraints that benefit mobile creditors are often enforced as discipline on immobile constituencies. The mandatrophy resolves by mapping each agent's perspectival classification to their structural position — the target sees snare, the beneficiary sees rope, the analytical observer must avoid naturalizing the politically-contingent arrangement as inevitable law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_space_endogeneity,
    'Is fiscal space a property of economic fundamentals (natural law), or is it endogenously produced by institutional arrangements and political choices?',
    'Comparative analysis: fiscal sustainability outcomes across nations with similar debt-to-GDP ratios but different institutional structures (central bank independence, reserve currency status, tax administration capacity). Historical analysis of deficit reversals and their mechanisms.',
    'If endogenous: the constraint is institutional (Tangled Rope or Scaffold), not natural law. Fiscal discipline could be temporarily relaxed without crisis during emergencies. If exogenous: the constraint is closer to Mountain, and relaxation always triggers crisis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_space_endogeneity, empirical, 'Whether fiscal space is natural economic law or institutional product').

omega_variable(
    internalization_of_discipline,
    'Do Congressional actors internalize fiscal discipline as an identity-fused commitment to fiscal responsibility, or do they experience it as external political constraint they would evade if possible?',
    'Analysis of voting patterns during fiscal crises (emergency spending votes, war funding, pandemic relief). If actors consistently vote for fiscal discipline even when facing partisan pressure for spending, internalization is high. If they vote for spending when political cost is low, external constraint is operative.',
    'If internalized: the constraint has cognitive-behavioral capture (identity_locked elements), making it stable across generations even if formal rules are relaxed. If external: the constraint is fragile and will degrade when political pressure or crisis creates exit opportunities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalization_of_discipline, empirical, 'Whether fiscal discipline is internalized institutional identity or external political constraint').

omega_variable(
    monetary_financing_prohibition,
    'How binding is the implicit constraint against Federal Reserve monetization of deficits, and does it constitute a separate structural limit on fiscal space, or is it derivative of the fiscal discipline constraint?',
    'Historical analysis of Fed independence and Treasury coordination during high-deficit periods. Institutional analysis of Fed charter constraints and political pressure dynamics. Comparison with nations where monetary financing is explicitly permitted.',
    'If binding and independent: there is a second constraint (monetary policy autonomy) that enforces the fiscal constraint. If derivative: the monetary constraint is secondary and could be overridden, making the fiscal constraint less stable.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(monetary_financing_prohibition, empirical, 'Whether monetary financing prohibition is independent or derivative constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(1961_kennedy_fiscal_restraint_budget_discipline, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fiscal_tr_t0, 1961_kennedy_fiscal_restraint_budget_discipline, theater_ratio, 0, 0.42).
narrative_ontology:measurement(fiscal_tr_t3, 1961_kennedy_fiscal_restraint_budget_discipline, theater_ratio, 3, 0.54).
narrative_ontology:measurement(fiscal_tr_t6, 1961_kennedy_fiscal_restraint_budget_discipline, theater_ratio, 6, 0.61).

% Extraction over time
narrative_ontology:measurement(fiscal_be_t0, 1961_kennedy_fiscal_restraint_budget_discipline, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(fiscal_be_t3, 1961_kennedy_fiscal_restraint_budget_discipline, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(fiscal_be_t6, 1961_kennedy_fiscal_restraint_budget_discipline, base_extractiveness, 6, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(1961_kennedy_fiscal_restraint_budget_discipline, resource_allocation).
narrative_ontology:affects_constraint(1961_kennedy_fiscal_restraint_budget_discipline, dollar_hegemony_maintenance).
narrative_ontology:affects_constraint(1961_kennedy_fiscal_restraint_budget_discipline, congressional_appropriations_process).
narrative_ontology:affects_constraint(1961_kennedy_fiscal_restraint_budget_discipline, tax_expenditure_growth).

% DUAL FORMULATION NOTE:
% The fiscal restraint constraint decomposes into multiple structurally distinct constraints: (1) international creditor coordination (keeping dollar stable), (2) domestic discretionary spending gate (preventing program growth), (3) enforcement mechanism (pay-as-you-go, tax loophole closure, procedural controls). The story treats these as aspects of a single constraint for pedagogical clarity, but empirical analysis might separate them into distinct stories with different ε values and beneficiary/victim structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
