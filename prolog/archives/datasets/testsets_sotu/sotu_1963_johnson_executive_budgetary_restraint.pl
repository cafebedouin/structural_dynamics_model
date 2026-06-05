% ============================================================================
% CONSTRAINT STORY: sotu_1963_johnson_executive_budgetary_restraint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1963_johnson_executive_budgetary_restraint, []).

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
 *   constraint_id: sotu_1963_johnson_executive_budgetary_restraint
 *   human_readable: Executive Branch Fiscal Discipline and Budgetary Restraint (LBJ 1963)
 *   domain: governance/budgetary_policy
 *
 * SUMMARY:
 *   President Johnson's 1963 State of the Union pledge to fiscal prudence
 *   establishes an executive governance constraint requiring thrift and
 *   frugality in government spending while maintaining commitments to
 *   substantive missions. The constraint operates at multiple structural
 *   levels: it coordinates legitimate taxpayer preferences for fiscal
 *   discipline with executive action; it extracts from federal agencies and
 *   program expansions by imposing budget ceilings; it provides political
 *   theater that enhances the president's reputation for responsible
 *   governance; and it naturalizes a contingent political choice (which
 *   agencies bear cost) as an immutable principle of good governance. The
 *   constraint benefits taxpayers, fiscal conservatives, and the presidential
 *   office while imposing costs on agencies seeking to expand programs
 *   addressing unfilled needs. The theater ratio (0.65) reflects that much of
 *   the budgetary review process is performative: line-item scrutiny is
 *   conducted without deep technical knowledge, and budget meetings function
 *   as legitimacy theater demonstrating fiscal discipline rather than genuine
 *   optimization. The extractiveness (0.38) reflects moderate asymmetry:
 *   agencies do experience real constraints and must choose between
 *   efficiency and expansion, but the constraint is not totalizing—agencies
 *   with political support can negotiate exceptions, and the constraint
 *   preserves substantive mission delivery rather than eliminating it.
 *
 * KEY AGENTS:
 *   - Federal Agencies: Primary victims (organized/constrained) — bear costs of restraint through reduced operational budgets and deferred program expansion
 *   - Unfunded Missions: Secondary victims (powerless/trapped) — poverty programs, infrastructure, research initiatives remain inadequately funded; cannot advocate directly
 *   - Taxpayers and Fiscal Conservatives: Primary beneficiaries (powerful/mobile) — experience fiscal discipline as aligned with their values; can exit through political participation
 *   - Executive Office and Presidential Reputation: Institutional beneficiary (institutional/arbitrage) — gains legitimacy and political credit for fiscal responsibility
 *   - Bureau of the Budget and Oversight Apparatus: Institutional actor (institutional/arbitrage) — maintains performative review ritual; gatekeeps budget exceptions
 *   - Great Society Reform Coalition: Organized agents (organized/constrained) — experience scaffold dynamics; constrain current spending to enable future priority programs
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing political choice as governance law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1963_johnson_executive_budgetary_restraint, 0.38).
domain_priors:suppression_score(sotu_1963_johnson_executive_budgetary_restraint, 0.42).
domain_priors:theater_ratio(sotu_1963_johnson_executive_budgetary_restraint, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1963_johnson_executive_budgetary_restraint, extractiveness, 0.38).
narrative_ontology:constraint_metric(sotu_1963_johnson_executive_budgetary_restraint, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(sotu_1963_johnson_executive_budgetary_restraint, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1963_johnson_executive_budgetary_restraint, tangled_rope).
narrative_ontology:human_readable(sotu_1963_johnson_executive_budgetary_restraint, "Executive Branch Fiscal Discipline and Budgetary Restraint (LBJ 1963)").
narrative_ontology:topic_domain(sotu_1963_johnson_executive_budgetary_restraint, "governance/budgetary_policy").

domain_priors:requires_active_enforcement(sotu_1963_johnson_executive_budgetary_restraint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1963_johnson_executive_budgetary_restraint, taxpayers).
narrative_ontology:constraint_beneficiary(sotu_1963_johnson_executive_budgetary_restraint, fiscal_conservatives).
narrative_ontology:constraint_beneficiary(sotu_1963_johnson_executive_budgetary_restraint, presidential_reputation).
narrative_ontology:constraint_victim(sotu_1963_johnson_executive_budgetary_restraint, federal_agencies).
narrative_ontology:constraint_victim(sotu_1963_johnson_executive_budgetary_restraint, program_expansion).
narrative_ontology:constraint_victim(sotu_1963_johnson_executive_budgetary_restraint, unfunded_missions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNFUNDED MISSIONS (SNARE) — Federal agencies and programs cannot exit the fiscal constraint. They bear the full cost of restraint without compensation. Unfilled needs (poverty, infrastructure, research) remain unmet while existing operations are squeezed. The agency administrator cannot appeal to higher political authority without risking budget cuts. Maximum extraction from this perspective: commitment persists, costs are real, exit is structurally closed.
constraint_indexing:constraint_classification(sotu_1963_johnson_executive_budgetary_restraint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: FEDERAL AGENCY (TANGLED ROPE) — Cabinet departments and agencies experience both coordination and extraction. The constraint solves a genuine problem: preventing wasteful duplication and ensuring accountability for public funds. But the constraint also extracts from agency operational flexibility. Agencies benefit from the legitimacy that fiscal discipline provides (public trust, political support) while being constrained in what they can accomplish. Exit is costly but possible: agencies that can demonstrate exceptional need or prove high value-per-dollar can negotiate exceptions.
constraint_indexing:constraint_classification(sotu_1963_johnson_executive_budgetary_restraint, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: EXECUTIVE OFFICE (ROPE) — The presidency benefits from the fiscal restraint constraint. It provides political cover against accusations of waste, aligns with conservative fiscal rhetoric, and strengthens the president's personal reputation for discipline. The executive can arbitrage the constraint: crediting themselves for thriftiness while agencies absorb the costs of implementation. The constraint solves the coordination problem of demonstrating fiscal responsibility without requiring the president to sacrifice substantive accomplishments. This is pure coordination from the executive perspective: the constraint is performative political advantage.
constraint_indexing:constraint_classification(sotu_1963_johnson_executive_budgetary_restraint, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: TAXPAYERS AND FISCAL CONSERVATIVES (ROPE) — These agents are primary beneficiaries. The constraint coordinates their preferences for fiscal discipline with executive action. They experience the constraint as beneficial governance: their tax dollars are spent efficiently, waste is reduced, and their fiscal values are honored. Exit options are mobile (they can support or withdraw support for the administration based on fiscal performance) and arbitrage is available (they can exit through political participation or migration). Low experienced extraction because these agents genuinely benefit and retain exit options.
constraint_indexing:constraint_classification(sotu_1963_johnson_executive_budgetary_restraint, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: BUDGETARY OVERSIGHT APPARATUS (PITON) — The Bureau of the Budget and presidential budgetary review mechanisms become institutionalized through this constraint. However, much of their actual function is performative: they review line items but lack deep technical knowledge to evaluate all claims. Budget meetings become theater for demonstrating fiscal discipline rather than genuine optimization. The oversight persists because it legitimates the presidential fiscal narrative, not because it reliably identifies waste. Over time, the ritual hardens while the functional verification decays — a classic piton trajectory.
constraint_indexing:constraint_classification(sotu_1963_johnson_executive_budgetary_restraint, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: GREAT SOCIETY REFORM COALITION (SCAFFOLD) — From a longer timescale, this constraint is temporary scaffolding that directs resources toward specific reforms (poverty programs, education, health) by enforcing discipline elsewhere. The constraint has a sunset: as programs prove their value and constituencies mobilize, budget restraint relaxes for favored initiatives. The coalition experiences constraint but sees an exit path: demonstrate success, build political support, and the fiscal discipline requirement becomes negotiable. This is structure with agency, hence scaffold rather than snare.
constraint_indexing:constraint_classification(sotu_1963_johnson_executive_budgetary_restraint, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, fiscal discipline appears as an immutable law of governance: all organizations must manage resources within constraints, and the principle of value-for-money is universal and unchangeable. However, this perspective naturalizes what is actually a contingent political choice about which agents bear the costs of discipline. The 'natural law' frame obscures that the constraint is selective: some agencies receive exceptions while others do not, and selection follows political power, not objective budget efficiency. The engine will identify this as a false summit.
constraint_indexing:constraint_classification(sotu_1963_johnson_executive_budgetary_restraint, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1963_johnson_executive_budgetary_restraint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1963_johnson_executive_budgetary_restraint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1963_johnson_executive_budgetary_restraint, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1963_johnson_executive_budgetary_restraint, TR),
    TR >= 0.70.

:- end_tests(sotu_1963_johnson_executive_budgetary_restraint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint does impose real costs on agencies and deferred programs, but extraction is not as severe as pure Snare because: (1) agencies retain some exit options—those demonstrating exceptional value can negotiate exceptions; (2) the constraint is selective, not uniform—politically favored agencies receive different treatment; (3) the stated purpose (eliminate waste) is partly genuine, not purely predatory. The initial extractiveness (0.22) reflects that the constraint begins as a rhetorical commitment with loose enforcement. It rises to 0.38 as implementation mechanisms (Bureau of Budget review, budget ceilings, exception approval processes) harden and agencies must make real operational choices. Suppression (0.42): Moderate. Agencies face meaningful barriers to budget expansion—they must justify requests, compete with other agencies, and prove value-for-money. But suppression is not total—high-priority agencies can still grow, and the constraint does not eliminate opposition or exit mechanisms, only raise their costs. Theater ratio (0.65): Moderate-high, increasing over time. Initial budget review (0.48) is conducted with some genuine effort to identify waste. Over time, the review becomes more ritualistic (0.65): reviewers focus on justification narratives and political alignments rather than technical efficiency analysis. The theater persists because it legitimates the fiscal discipline narrative, not because it accurately identifies waste.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival diversity across the six types. The unfunded mission sees pure extraction (Snare)—it bears costs with no agency or compensation. The federal agency sees mixed coordination and extraction (Tangled Rope)—the constraint both legitimates their work (fiscal discipline enhances public trust) and constrains their autonomy. The executive office sees pure coordination (Rope)—the constraint solves the political problem of appearing fiscally responsible. Taxpayers and fiscal conservatives see beneficial coordination (Rope)—their values are honored and their interests are served. The budgetary review apparatus sees a degraded ritual (Piton)—the review persists through institutional inertia and legitimacy maintenance rather than through functional verification. The Great Society coalition sees temporary scaffolding (Scaffold)—they experience constraint but perceive a sunset as political mobilization succeeds. The analytical observer risks seeing immutable governance law (Mountain)—that all organizations must manage resources within constraints—but the structural data reveals this as naturalization of a selective political choice. The perspectival gap is not an error—it is the core diagnostic: the constraint is legitimate from the beneficiary perspective, exploitative from the victim perspective, and politically contingent from the analytical perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality operates through three distinct mechanisms. First, for taxpayers and fiscal conservatives: beneficiary status + mobile exit options → low d → negative χ. These agents genuinely want the constraint and can leave if dissatisfied, so they experience low extraction. Second, for federal agencies: victim status + constrained exit options → moderate d → moderate χ. Agencies bear costs but can negotiate exceptions and can argue that the constraint harms their missions. Third, for the executive office: beneficiary status + arbitrage options → very low d → negative χ. The presidency gains political capital from the constraint and can selectively enforce it to favor preferred agencies, so the executive experiences no extraction—only benefit. The directionality gap (beneficiaries experience negative extraction while victims experience positive extraction) is the structural signature of Tangled Rope: genuine coordination function (fiscal discipline is valuable) combined with asymmetric extraction (costs are concentrated on low-power agencies while benefits flow to high-power beneficiaries).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by acknowledging that Tangled Rope classification is the analytical truth: the constraint simultaneously provides genuine coordination (fiscal discipline) and genuine extraction (cost concentration on low-power agencies). The temptation to see it as pure Rope (all coordination, no extraction) would require ignoring the real agency costs and assuming uniform benefit distribution—which the structural data contradicts. The temptation to see it as pure Snare (all extraction, no coordination) would require denying that fiscal discipline has genuine value—which the beneficiary perspective contradicts. The three-type perspectival distribution (Rope from beneficiary, Tangled Rope from agency, Snare from unfunded missions) shows that classification depends on structural position, and all three positions are real. The Tangled Rope classification from the organized agency perspective is stable and defensible because it reflects the genuine hybrid: the constraint does coordinate some value (legitimacy, efficiency pressure) while extracting asymmetric cost (budget constraints disproportionately affecting low-priority agencies). The mandatrophy is resolved when the analyst accepts that all three perspectives are valid readings of the same constraint, and the Tangled Rope classification is the 'neutral' analytical position that acknowledges both dimensions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    fiscal_discipline_vs_political_theater,
    'Is the executive budgetary restraint constraint a genuine mechanism for eliminating waste, or primarily a political performance device that signals fiscal responsibility while actual expenditures remain largely unchanged?',
    'Comparative analysis of budgets before/after the constraint; measurement of actual cost reduction vs. deferred spending; analysis of which agencies receive exemptions and whether exemptions correlate with political priorities rather than objective efficiency gains',
    'If genuine efficiency: constraint is primarily Rope from agency perspective. If primarily theater: constraint is primarily Piton or Snare from agency perspective, with extraction masked by legitimacy language.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fiscal_discipline_vs_political_theater, empirical, 'Genuine fiscal efficiency vs. political theater').

omega_variable(
    distributional_asymmetry_of_restraint,
    'Does fiscal restraint apply uniformly across all agencies, or do politically favored programs (defense, presidential initiatives) receive exemptions while others (domestic agencies, welfare programs) bear the full brunt of cost control?',
    'Historical budget data showing growth rates by agency category; analysis of exception patterns; correlation of agency budget discipline with political alignment and presidential priority',
    'If uniform: constraint is coordination mechanism. If asymmetric: constraint is extractive mechanism targeting low-power agencies, confirming Snare classification for victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(distributional_asymmetry_of_restraint, empirical, 'Whether fiscal restraint applies uniformly or targets specific agencies').

omega_variable(
    unfunded_mandate_accumulation,
    'Do deferred program expansions and unfunded missions accumulate as debt-like obligations that must be addressed later, or do they genuinely get abandoned and the need genuinely disappears?',
    'Tracking of unfunded initiatives over time; analysis of whether deferred programs eventually get funded (possibly at higher cost after delay); measurement of social/economic costs of delayed implementation',
    'If deferred: constraint is temporary transfer of cost, not elimination. If abandoned: genuine fiscal discipline. If accumulated: constraint creates future extraction burden on successor administrations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(unfunded_mandate_accumulation, empirical, 'Whether deferred programs accumulate as future obligations').

omega_variable(
    agency_innovation_suppression,
    'Does fiscal restraint suppress agency innovation and efficiency improvements that would require upfront investment, creating false economies where short-term savings mask long-term costs?',
    'Measurement of research and development spending by agencies; tracking of process improvements deferred due to budget constraints; analysis of whether post-constraint periods show catch-up spending and accelerated change',
    'If innovation suppressed: restraint is extractive from long-term efficiency perspective. If innovation proceeds: restraint successfully eliminates only true waste.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_innovation_suppression, empirical, 'Whether fiscal restraint suppresses beneficial agency innovation').

omega_variable(
    beneficiary_identity_salience,
    'Are taxpayers and fiscal conservatives genuine beneficiaries of this constraint, or is the constraint primarily a mechanism for concentrating power and discretion in the executive office at the expense of agency autonomy?',
    'Measurement of actual tax burden reduction; analysis of whether fiscal discipline translates to lower taxes or higher deficit spending; examination of whether constraining agencies reduces public service quality that taxpayers value',
    'If genuine benefit: constraint is Rope from beneficiary perspective. If primary effect is executive power concentration: beneficiaries are illusory and constraint is Snare for all parties except the presidency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_identity_salience, conceptual, 'Whether stated beneficiaries genuinely benefit from the constraint').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1963_johnson_executive_budgetary_restraint, 0, 4).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_tr_t0, sotu_1963_johnson_executive_budgetary_restraint, theater_ratio, 0, 0.48).
narrative_ontology:measurement(sotu_tr_t2, sotu_1963_johnson_executive_budgetary_restraint, theater_ratio, 2, 0.58).
narrative_ontology:measurement(sotu_tr_t4, sotu_1963_johnson_executive_budgetary_restraint, theater_ratio, 4, 0.65).

% Extraction over time
narrative_ontology:measurement(sotu_be_t0, sotu_1963_johnson_executive_budgetary_restraint, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sotu_be_t2, sotu_1963_johnson_executive_budgetary_restraint, base_extractiveness, 2, 0.35).
narrative_ontology:measurement(sotu_be_t4, sotu_1963_johnson_executive_budgetary_restraint, base_extractiveness, 4, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1963_johnson_executive_budgetary_restraint, resource_allocation).
narrative_ontology:affects_constraint(sotu_1963_johnson_executive_budgetary_restraint, federal_agency_autonomy).
narrative_ontology:affects_constraint(sotu_1963_johnson_executive_budgetary_restraint, great_society_program_funding).

% DUAL FORMULATION NOTE:
% This constraint is upstream of specific program funding decisions (Great Society initiatives, agency expansion projects). The fiscal discipline constraint sets the budgetary envelope within which those downstream decisions are made. The constraint itself is not about any specific program but about the governance mechanism (thrift and frugality) that allocates scarce resources across all agencies.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1963_johnson_executive_budgetary_restraint, institutional, 0.08).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
