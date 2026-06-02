% ============================================================================
% CONSTRAINT STORY: legitimation_through_objectivity
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-02
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimation_through_objectivity, []).

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
 *   constraint_id: legitimation_through_objectivity
 *   human_readable: Legitimation Through Objectivity in Debt Obligation Measurement
 *   domain: political_economy/labor_relations/debt_systems
 *
 * SUMMARY:
 *   The legitimation-through-objectivity constraint operates across debt
 *   systems, labor relations, and fiscal policy by converting political
 *   relationships into technical measurements. When a sovereign debt crisis
 *   is framed as a matter of 'unsustainable debt-to-GDP ratios' rather than a
 *   conflict over who bears adjustment costs, the precision of the numbers
 *   obscures the discretion embedded in their construction. GDP measurement
 *   excludes unpaid care work, environmental degradation, and commons-based
 *   production. Debt classification treats odious debt and productive
 *   investment identically. Sustainability thresholds embed assumptions about
 *   growth rates, interest rates, and primary surplus targets that
 *   systematically favor creditors. The constraint has intensified over the
 *   1980-2010 interval as financial globalization, central bank independence,
 *   and fiscal rules have expanded the domain of technocratic measurement
 *   while narrowing the space for democratic contestation. The theater ratio
 *   (0.78) reflects that much of the precision is performative: decimal-place
 *   accuracy in debt sustainability analyses coexists with order-of-magnitude
 *   uncertainty in the underlying economic models. The extractiveness (0.68)
 *   reflects that the measurement frameworks systematically transfer
 *   resources and authority from obligation bearers to legitimation
 *   performers.
 *
 * KEY AGENTS:
 *   - Obligation Bearer: Primary victim (powerless/trapped) — individual debtor trapped by obligations quantified through uncontestable measurement frameworks; maximum extraction through naturalized precision
 *   - Debtor Populations: Secondary victim (moderate/constrained) — collective debtors (sovereign states, municipalities, student loan borrowers) constrained by measurement frameworks that naturalize creditor priorities; can organize but face technocratic suppression
 *   - Labor Under Austerity: Tertiary victim (organized/constrained) — labor movements constrained by fiscal frameworks that quantify obligations to creditors with precision while treating obligations to workers as aspirational; asymmetric extraction through measurement design
 *   - Creditor Institutions: Primary beneficiary (institutional/arbitrage) — banks, bondholders, international financial institutions that benefit from measurement precision enforcing obligations while obscuring discretion; arbitrage exit across jurisdictions
 *   - Policy Technocrats: Secondary beneficiary (institutional/arbitrage) — central bankers, finance ministry officials, IMF staff who benefit from objectivity framing that insulates decisions from democratic contestation; professional authority through measurement expertise
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing measurement precision as inherent to economic coordination rather than recognizing it as discretionary choice serving specific interests
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimation_through_objectivity, 0.68).
domain_priors:suppression_score(legitimation_through_objectivity, 0.72).
domain_priors:theater_ratio(legitimation_through_objectivity, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimation_through_objectivity, extractiveness, 0.68).
narrative_ontology:constraint_metric(legitimation_through_objectivity, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(legitimation_through_objectivity, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimation_through_objectivity, snare).
narrative_ontology:human_readable(legitimation_through_objectivity, "Legitimation Through Objectivity in Debt Obligation Measurement").
narrative_ontology:topic_domain(legitimation_through_objectivity, "political_economy/labor_relations/debt_systems").

domain_priors:requires_active_enforcement(legitimation_through_objectivity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimation_through_objectivity, legitimation_performer).
narrative_ontology:constraint_beneficiary(legitimation_through_objectivity, creditor_institutions).
narrative_ontology:constraint_beneficiary(legitimation_through_objectivity, policy_technocrats).
narrative_ontology:constraint_victim(legitimation_through_objectivity, obligation_bearer).
narrative_ontology:constraint_victim(legitimation_through_objectivity, debtor_populations).
narrative_ontology:constraint_victim(legitimation_through_objectivity, labor_under_austerity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: OBLIGATION BEARER (SNARE) — Trapped by debt obligations quantified through measurement frameworks they cannot contest. The precision of the numbers (interest rates to basis points, repayment schedules to the day, inflation adjustments to decimal places) creates an aura of objectivity that forecloses political negotiation. Cannot exit the measurement regime; bears maximum extraction through obligations presented as mathematical necessity rather than political choice.
constraint_indexing:constraint_classification(legitimation_through_objectivity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: DEBTOR POPULATION (SNARE) — Constrained by collective debt burdens (sovereign debt, municipal bonds, student loans) measured through frameworks that naturalize creditor priorities. Can organize politically but face suppression through technocratic framing: challenging the measurement design is portrayed as innumeracy or irresponsibility. The precision of GDP-to-debt ratios, debt service coverage ratios, and credit scores obscures the discretionary choices in what counts as productive activity, what discount rates apply, and what risk premiums are justified.
constraint_indexing:constraint_classification(legitimation_through_objectivity, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CREDITOR INSTITUTION (ROPE) — Benefits from measurement precision that enforces obligations while obscuring discretion. Experiences the constraint as coordination: standardized metrics enable capital allocation, risk assessment, and contract enforcement across jurisdictions. The objectivity theater is functional for this agent — it converts political relationships (who owes whom, under what terms, with what recourse) into technical relationships (numbers in a ledger, legally enforceable). Net beneficiary with arbitrage exit — can shift capital to jurisdictions with more favorable measurement regimes.
constraint_indexing:constraint_classification(legitimation_through_objectivity, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: POLICY TECHNOCRAT (ROPE) — Benefits from objectivity framing that insulates policy decisions from democratic contestation. Experiences the constraint as coordination: precise metrics enable 'evidence-based' policymaking, international comparisons, and bureaucratic legitimacy. The measurement frameworks (inflation targeting, structural balance rules, debt sustainability analyses) provide professional authority and career stability. Arbitrage exit through international mobility in technocratic labor markets (IMF, World Bank, central banks, finance ministries).
constraint_indexing:constraint_classification(legitimation_through_objectivity, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: LABOR MOVEMENT UNDER AUSTERITY (TANGLED ROPE) — Constrained by fiscal frameworks that quantify government obligations (debt service, pension liabilities, deficit targets) with precision while treating social obligations (full employment, wage growth, public services) as aspirational. Benefits from some coordination functions (transparent budget accounting, predictable fiscal rules) but bears asymmetric extraction when measurement design systematically undercounts labor's claims. Can organize to contest austerity but faces suppression through objectivity theater: 'the numbers don't lie,' 'we can't afford it,' 'the markets demand discipline.'
constraint_indexing:constraint_classification(legitimation_through_objectivity, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: SOVEREIGN DEBTOR STATE (TANGLED ROPE) — Constrained by international debt measurement frameworks (IMF debt sustainability analyses, credit rating methodologies, Maastricht criteria) that embed creditor-favorable assumptions while claiming technical neutrality. Benefits from coordination functions (access to capital markets, international reserves, trade finance) but bears extraction through measurement designs that naturalize austerity. Can contest specific metrics but faces suppression through market discipline and institutional pressure. The precision of debt-to-GDP thresholds obscures discretionary choices in GDP measurement, debt classification, and sustainability criteria.
constraint_indexing:constraint_classification(legitimation_through_objectivity, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN / FALSE SUMMIT) — From a civilizational perspective, some measurement precision appears inherent to complex economic coordination: you need numbers to track obligations, assess creditworthiness, and allocate resources. This perspective risks naturalizing the constraint as an immutable requirement of modern finance. However, the structural data contradicts the mountain classification — the engine will compute this as a false summit, revealing that 'objectivity requires precision' naturalizes what is actually a discretionary choice about whose claims get quantified with what rigor. Alternative measurement regimes (participatory budgeting, social auditing, commons-based accounting) demonstrate that precision is not the same as objectivity, and that the choice of what to measure precisely is itself political.
constraint_indexing:constraint_classification(legitimation_through_objectivity, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimation_through_objectivity_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(legitimation_through_objectivity, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(legitimation_through_objectivity, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimation_through_objectivity, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimation_through_objectivity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The measurement frameworks systematically transfer resources from debtors to creditors and authority from democratic institutions to technocratic ones. The extraction operates through naturalization: political choices about parameter settings (discount rates, risk premiums, inflation baskets, GDP definitions) are presented as technical necessities. The value reflects that the extraction is substantial but not total — some democratic contestation persists, and alternative measurement regimes exist at the margins. Suppression (0.72): High. Multiple mechanisms suppress alternatives: technocratic framing portrays measurement challenges as innumeracy; market discipline punishes states that deviate from orthodox metrics; professional gatekeeping excludes heterodox measurement approaches from policy institutions; media coverage amplifies precision theater while ignoring parameter-setting discretion. But suppression is not absolute — social movements, heterodox economists, and some states do contest the frameworks. Theater ratio (0.78): Very high. Much of the precision is performative: debt sustainability analyses report results to decimal places while using models with order-of-magnitude parameter uncertainty; credit ratings claim scientific rigor while embedding subjective judgments; inflation targeting treats 2.00% as meaningfully different from 2.25% despite measurement error exceeding the difference. The theater has increased as measurement complexity has outpaced democratic comprehension, creating space for discretion to hide behind precision.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates extreme perspectival divergence. Creditor institutions and policy technocrats see coordination (Rope) — the measurement frameworks enable capital allocation, risk assessment, and evidence-based policy. They experience the precision as functional objectivity. Labor movements and sovereign debtor states see mixed coordination and extraction (Tangled Rope) — they benefit from some transparency and predictability but bear asymmetric costs when measurement design naturalizes creditor priorities. Obligation bearers and debtor populations see pure extraction (Snare) — the precision forecloses political negotiation by presenting obligations as mathematical necessity. The analytical observer risks seeing natural law (Mountain) — measurement precision appears inherent to economic coordination — but the structural data reveals this as a false summit: alternative regimes (participatory budgeting, social auditing, commons accounting) demonstrate that precision is not the same as objectivity, and that the choice of what to measure precisely is itself political. The gap between the creditor's rope and the debtor's snare is the core of the constraint's extractive function.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality values reflect each agent's structural relationship to the extraction flow. Obligation bearers are full victims with trapped exit — they experience maximum extraction (d ≈ 0.95). Debtor populations are victims with constrained exit — they can organize politically but face technocratic suppression (d ≈ 0.85). Labor movements are victims with constrained exit but also benefit from some coordination functions like transparent budget accounting (d ≈ 0.65). Creditor institutions are full beneficiaries with arbitrage exit — they experience negative effective extraction, capturing rents through naturalized measurement (d ≈ 0.05). Policy technocrats are beneficiaries with arbitrage exit — they capture professional authority and career stability through objectivity framing (d ≈ 0.15). The analytical observer at civilizational scope risks seeing the constraint as a mountain (inherent coordination requirement) when it is actually a snare (extraction through naturalized discretion).
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLUTION REQUIRED (extractiveness > 0.70 threshold not met, but analysis provided for structural clarity): The constraint resolves the mandatrophy by revealing that 'objectivity' and 'extraction' are not mutually exclusive. The measurement frameworks are genuinely precise (the numbers are calculated correctly) AND genuinely extractive (the choice of what to measure, how to measure it, and what precision to demand systematically favors creditors over debtors). The coordination function (enabling capital allocation, contract enforcement, international comparison) coexists with the extraction function (naturalizing creditor priorities, suppressing democratic contestation, transferring authority to technocrats). The tangled_rope classification from labor and sovereign debtor perspectives captures this duality. The snare classification from obligation bearer perspectives captures the experience of those with no exit and no benefit from the coordination function. The rope classification from creditor and technocrat perspectives captures the experience of those who benefit from both the coordination and the extraction. The false summit at the analytical perspective captures the risk of naturalizing discretionary measurement choices as technical necessities. The constraint is not 'really' coordination or 'really' extraction — it is both, experienced differently depending on structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    measurement_precision_necessity,
    'Is numerical precision inherent to economic coordination, or is it a discretionary choice that serves specific interests?',
    'Comparative analysis of alternative measurement regimes (participatory budgeting, social auditing, commons accounting) and their coordination effectiveness; historical analysis of pre-quantification debt systems and their stability',
    'If precision is inherent: mountain classification from more perspectives, constraint is coordination cost. If precision is discretionary: snare classification from more perspectives, constraint is extraction mechanism disguised as technical necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(measurement_precision_necessity, conceptual, 'Whether numerical precision is inherent to coordination or serves specific interests').

omega_variable(
    parameter_setting_visibility,
    'What level of public visibility into parameter-setting discretion would shift the constraint from snare to tangled_rope?',
    'Experimental transparency interventions: publish discount rate assumptions, risk premium justifications, GDP measurement choices, inflation basket compositions. Measure whether visibility enables political contestation or merely adds theater.',
    'If visibility enables contestation: constraint becomes tangled_rope (coordination with acknowledged extraction). If visibility is absorbed as more theater: constraint remains snare (extraction obscured by complexity rather than secrecy).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(parameter_setting_visibility, empirical, 'Whether transparency in parameter-setting enables political contestation').

omega_variable(
    technocratic_capture_reversibility,
    'Can democratic institutions reclaim discretion over measurement design, or has technocratic authority become structurally irreversible?',
    'Analysis of successful democratic challenges to measurement frameworks (Iceland post-2008, participatory budgeting in Porto Alegre, community wealth building in Preston UK); identification of structural conditions enabling reclamation vs those preventing it',
    'If reversible: scaffold perspective gains validity (temporary technocratic dominance with democratic sunset). If irreversible: snare perspective confirmed (permanent extraction through naturalized measurement authority).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technocratic_capture_reversibility, empirical, 'Whether democratic institutions can reclaim measurement discretion from technocrats').

omega_variable(
    creditor_measurement_asymmetry,
    'Why are creditor claims quantified with precision (interest to basis points, repayment schedules to the day) while debtor claims are treated as aspirational (full employment, wage growth, public services)?',
    'Historical analysis of measurement regime evolution; identification of institutional mechanisms that enforce precision for creditor claims while resisting precision for debtor claims; comparison with alternative regimes that quantify social obligations with equal rigor',
    'If asymmetry is inherent to credit relationships: mountain classification gains support. If asymmetry is contingent on power relationships: snare classification confirmed, revealing that ''objectivity'' systematically favors creditors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(creditor_measurement_asymmetry, conceptual, 'Why measurement precision is asymmetric between creditor and debtor claims').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimation_through_objectivity, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(theater_1980, legitimation_through_objectivity, theater_ratio, 0, 0.55).
narrative_ontology:measurement(theater_1995, legitimation_through_objectivity, theater_ratio, 15, 0.68).
narrative_ontology:measurement(theater_2010, legitimation_through_objectivity, theater_ratio, 30, 0.78).

% Extraction over time
narrative_ontology:measurement(extract_1980, legitimation_through_objectivity, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(extract_1995, legitimation_through_objectivity, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(extract_2010, legitimation_through_objectivity, base_extractiveness, 30, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimation_through_objectivity, resource_allocation).

% DUAL FORMULATION NOTE:
% This constraint is downstream of unilateral_condition_control (the upstream constraint that establishes creditor authority to set terms). The legitimation-through-objectivity constraint is the mechanism by which unilateral condition control is naturalized and rendered politically uncontestable. The upstream constraint has its own extractiveness reflecting the power asymmetry in condition-setting; this constraint has its own extractiveness reflecting the additional extraction enabled by objectivity theater. The two constraints are structurally linked but analytically distinct per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legitimation_through_objectivity, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
