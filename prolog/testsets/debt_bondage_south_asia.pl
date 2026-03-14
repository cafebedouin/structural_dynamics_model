% ============================================================================
% CONSTRAINT STORY: debt_bondage_south_asia
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_debt_bondage_south_asia, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: debt_bondage_south_asia
 *   human_readable: Debt Bondage in South Asian Labor Markets
 *   domain: economic/labor/social
 *
 * SUMMARY:
 *   Debt bondage in South Asia represents one of the world's most persistent
 *   forms of coercive labor extraction, affecting an estimated 10-20 million
 *   people across India, Nepal, Bangladesh, and Pakistan. The constraint
 *   operates through the entanglement of three mechanisms: (1) initial debt
 *   obligation, typically incurred through advance wages, ceremonial
 *   expenses, or survival needs; (2) systematic interest rate and accounting
 *   practices that ensure the debt never diminishes and grows
 *   intergenerationally; (3) caste-based occupational assignment and social
 *   stigma that prevents bonded laborers from seeking alternative employment
 *   even when technically possible. The constraint is structurally pure
 *   extraction with minimal genuine coordination function — it exists to
 *   transfer labor value from powerless agents to landlords and creditors
 *   while suppressing exit options through legal invisibility, caste
 *   enforcement, and economic desperation. The formal legal prohibition of
 *   debt bondage across South Asian nations (India 1976, Nepal 2000,
 *   Bangladesh 2006) persists as performative law while the underlying
 *   practice continues through informal enforcement and intergenerational
 *   inheritance. The extractiveness has increased slightly over the 50-year
 *   measurement interval (0.68 to 0.78) reflecting intensification through
 *   compounding interest and caste-based occupational lock-in, while theater
 *   ratio remains low because the constraint operates through direct coercion
 *   rather than institutional performance.
 *
 * KEY AGENTS:
 *   - Bonded Laborers: Primary victims (powerless/trapped) — structurally trapped by debt obligation with no exit path; income consumed entirely by debt service; bear full extraction force.
 *   - Next Generation: Secondary victims (powerless/trapped) — debt transmits intergenerationally through inherited obligation and caste-based occupational assignment; born into bondage.
 *   - Creditor Landlords: Primary beneficiaries (institutional/arbitrage) — capture stable labor supply, wealth transfer through interest accumulation, occupational monopoly; experience constraint as coordination mechanism.
 *   - Non-Bonded Laborers: Tertiary actors (moderate/constrained) — benefit from labor suppression (higher wages for free laborers) but also harmed by reduced occupational mobility and community pressure to maintain caste boundaries.
 *   - Formal Legal System: Institutional actor (institutional/arbitrage) — prohibition exists as theater while informal enforcement persists; maintains legitimacy of formal state while allowing de facto continuation through caste mechanisms.
 *   - Analytical Observer: Civilizational position (analytical/analytical) — views constraint as pure extraction mechanism with structural dependence on caste enforcement and intergenerational inheritance lock.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(debt_bondage_south_asia, 0.78).
domain_priors:suppression_score(debt_bondage_south_asia, 0.85).
domain_priors:theater_ratio(debt_bondage_south_asia, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(debt_bondage_south_asia, extractiveness, 0.78).
narrative_ontology:constraint_metric(debt_bondage_south_asia, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(debt_bondage_south_asia, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(debt_bondage_south_asia, snare).
narrative_ontology:human_readable(debt_bondage_south_asia, "Debt Bondage in South Asian Labor Markets").
narrative_ontology:topic_domain(debt_bondage_south_asia, "economic/labor/social").

domain_priors:requires_active_enforcement(debt_bondage_south_asia).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(debt_bondage_south_asia, creditor_landlords).
narrative_ontology:constraint_beneficiary(debt_bondage_south_asia, employing_households).
narrative_ontology:constraint_beneficiary(debt_bondage_south_asia, merchant_creditors).
narrative_ontology:constraint_victim(debt_bondage_south_asia, bonded_laborers).
narrative_ontology:constraint_victim(debt_bondage_south_asia, next_generation_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BONDED LABORER (SNARE) — Structurally trapped by debt obligation with no material exit path. Income is consumed entirely by debt service, interest compounding ensures the debt never diminishes, and legal/social barriers prevent escape. Maximum extraction experienced — the constraint exists to extract labor value while preventing exit.
constraint_indexing:constraint_classification(debt_bondage_south_asia, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NEXT GENERATION (SNARE) — Debt bondage transmits across generations through inherited obligation, caste-based occupational assignment, and feudal tenure structures. Children born into bonded families begin life already trapped. The constraint extracts across the full generational horizon with minimal possibility of mobility. Intergenerational structure intensifies the snare classification.
constraint_indexing:constraint_classification(debt_bondage_south_asia, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 3: CREDITOR LANDLORD (ROPE) — From the creditor's view, debt bondage solves a coordination problem: how to secure reliable labor supply in agricultural regions with seasonal labor shortage and high mobility. The formal creditor relationship coordinates labor availability with subsistence provision. Net beneficiary — the constraint provides stable labor access and transfers wealth to the creditor.
constraint_indexing:constraint_classification(debt_bondage_south_asia, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: SMALL FARMER / NON-BONDED LABORER (TANGLED ROPE) — Structurally similar to bonded laborers but with constrained rather than trapped exit: high cost to leave, social pressure from community, credit dependency, but some degree of mobility. Experiences the constraint as both a coordination mechanism (labor is reliably available in the village) and extraction (labor costs are suppressed by bonded labor availability). Mixed experience — benefit and burden both present.
constraint_indexing:constraint_classification(debt_bondage_south_asia, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FORMAL LEGAL SYSTEM (PITON) — Debt bondage is explicitly prohibited by Indian law (Bonded Labor System Abolition Act 1976, Bangladesh Labor Act 2006, Nepal's Kamaiya abolition 2000), yet persists structurally. The formal legal framework is theater — it exists on paper while the underlying practice continues through informal enforcement and caste-based occupational assignment. Theater ratio is low (the legal fiction of prohibition is thin), but the law persists through institutional inertia rather than function.
constraint_indexing:constraint_classification(debt_bondage_south_asia, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SNARE) — From a systemic view, debt bondage is a pure extraction mechanism with minimal coordination function. The binding is not economic necessity but active suppression: interest rates are set to ensure non-repayment, caste stigma prevents alternative employment, legal remedies are inaccessible, and intergenerational inheritance perpetuates the trap. The constraint's primary function is wealth transfer from powerless to powerful, sustained by embedded suppression.
constraint_indexing:constraint_classification(debt_bondage_south_asia, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(debt_bondage_south_asia_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(debt_bondage_south_asia, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(debt_bondage_south_asia, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(debt_bondage_south_asia, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(debt_bondage_south_asia, TR),
    TR >= 0.70.

:- end_tests(debt_bondage_south_asia_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78): Very high. The constraint systematically transfers labor value from bonded workers to creditors through: (a) below-market wage rates (bonded workers earn 30-50% less than non-bonded agricultural laborers); (b) compounding interest that ensures non-repayment (interest rates 20-40% annually, sometimes higher); (c) intergenerational transmission that creates permanent debt classes. The high extractiveness reflects that the constraint's primary function is wealth transfer, not coordination. Suppression (0.85): Very high. Multiple reinforcing suppression mechanisms operate: (a) structural (occupational heredity, lack of alternative employment due to caste stigma); (b) legal (informal debt contracts unenforceable in court, so bonded laborers have no legal recourse); (c) informational (workers often illiterate, unaware of rights or alternatives); (d) social (family/community pressure to maintain occupational role); (e) economic (complete income dependency leaves no capital for exit). Theater ratio (0.35): Moderate-low. The constraint operates primarily through direct coercion (occupational assignment, debt obligation, wage suppression) rather than through institutional performance or legitimating ritual. The low theater reflects that the mechanism is economically direct — workers understand they are trapped and creditors understand they are extracting. The formal legal prohibition exists as theater (performative law) but the underlying practice is bare coercion, not hidden institutional ritual.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is severe and reveals the constraint's pure extractive nature. Bonded laborers see absolute snare (no exit, maximum extraction). The next generation sees snare perpetuated across their lifetime. Non-bonded laborers see tangled rope (benefit from labor suppression but also harmed by reduced mobility). Creditor landlords see pure rope (coordination of labor supply, stable benefit, no experienced extraction). The formal legal system sees piton (law on books, practice in reality, institutional inertia sustaining both). The analytical observer sees snare with active structural enforcement. Critically, there is no perspective from which this appears as genuine coordination — even the beneficiary (institutional) perspective records rope (legitimate coordination), but the structural data contradicts this. The creditor 'coordinates' labor supply, but the mechanism is coercion, not mutual benefit. The perspectival analysis reveals that the beneficiary's 'coordination' framing naturalizes what is actually extraction dependent on suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from structural position in the extraction flow. Bonded laborers: trapped exit + victim status → d ≈ 0.95 → f(d) ≈ 1.40 → maximum experienced extraction. Next generation: trapped exit + inherited victim status → d ≈ 0.95 → same maximum extraction. Non-bonded laborers: constrained exit + mixed status (harmed by occupational lock but benefit from labor suppression) → d ≈ 0.60 → f(d) ≈ 0.80 → moderate extraction. Creditor landlords: arbitrage exit + beneficiary status → d ≈ 0.10 → f(d) ≈ -0.05 → negative extraction (they benefit). Scope modifier for regional scale (σ=0.9) dampens chi slightly compared to global scale, but the high base extractiveness and high f(d) for victims still produce χ ≈ 0.66+ for the powerless perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY RESOLVED: This constraint clearly classifies as snare and remains snare across all perspectives except the beneficiary (which views rope). The mandatrophy is resolved by recognizing that the beneficiary's 'rope' classification is FALSE — the beneficiary experiences benefit but the constraint does not coordinate in the sense of providing mutual advantage. The coordination function (labor supply) is genuine, but it is achieved through suppression, not reciprocity. The constraint's true type is snare because: (1) effective extraction χ ≥ 0.66 for the primary victims; (2) base extraction ε ≥ 0.46; (3) suppression ≥ 0.60 and actively maintained; (4) the beneficiary's experience of 'coordination' is enabled by the victims' suppression, not by genuine reciprocal benefit. The analytical observer correctly identifies snare because the observational position is not captured by the beneficiary's framing — from outside the extraction relationship, the mechanism is visible as pure coercion. The formal legal system's piton classification confirms the degradation: the law prohibits what the practice continues, maintaining both legitimacy and continuation through institutional theater.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    debt_vs_caste_mechanism,
    'Is debt bondage primarily a debt mechanism or a caste-based occupational lock mechanism using debt as a cover story?',
    'Analysis of cases where bonded laborers escape debt: do they gain occupational mobility and exit the system, or does caste stigma and occupational assignment persist despite debt resolution? Compare regions with caste enforcement (India, Nepal) vs regions without (parts of Middle East with migrant bonded laborers).',
    'If primarily debt: constraint is snare with economic extraction mechanism (could be partially solved by debt forgiveness alone). If primarily caste: constraint is snare with structural identity-lock (debt forgiveness alone insufficient; requires occupational desegregation and social mobility pathways).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(debt_vs_caste_mechanism, empirical, 'Whether debt bondage is debt-driven or caste-driven').

omega_variable(
    intergenerational_transmission_mechanism,
    'What mechanism locks the next generation into bondage — legal doctrine of inherited debt, occupational heredity norms, or absence of alternative economic pathways?',
    'Ethnographic and legal analysis: compare intergenerational transmission rates across regions with different inheritance doctrines. Examine cases where occupational alternatives became available (education access, urban migration) and track intergenerational escape rates.',
    'If legal doctrine: land reform and debt cancellation directly address mechanism. If occupational heredity: requires education/mobility intervention. If economic pathway absence: requires regional labor diversification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_transmission_mechanism, empirical, 'Mechanism of intergenerational debt bondage transmission').

omega_variable(
    credit_alternative_availability,
    'Do bonded laborers have access to formal credit alternatives (microfinance, bank loans, government credit schemes) that would eliminate debt bondage logic?',
    'Comparative analysis: credit access rates among bonded vs non-bonded laborers; terms offered; actual uptake when alternatives are made available. Historical analysis of regions where formal credit access expanded.',
    'If alternatives exist and unused: extraction requires suppression mechanism (agents refuse alternatives due to social pressure or lack of information). If alternatives absent: creditor has genuine coordination monopoly (snare but with economic logic). If alternatives available and used: constraint may be shifting toward piton (formal prohibition working, informal practice declining).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(credit_alternative_availability, empirical, 'Whether formal credit alternatives to bonded labor exist').

omega_variable(
    interest_rate_sustainability,
    'Are interest rates set to be inherently non-repayable (ensuring perpetual bondage) or are they set at economically rational levels that happen to trap borrowers due to income insufficiency?',
    'Economic analysis of interest rates on bonded labor advances vs market rates for unsecured rural credit. Determine whether the gap reflects risk adjustment or intentional trap-setting. Examine historical variation in rates and debt durations.',
    'If intentionally non-repayable: extraction mechanism is deliberate, confirming snare classification. If economically rational but income-insufficient: suggests coordinating mechanism (credit gap) that becomes extractive due to asymmetric power, suggesting tangled_rope or constrained snare rather than pure snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interest_rate_sustainability, empirical, 'Whether interest rates are intentionally non-repayable').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(debt_bondage_south_asia, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(debt_bond_tr_t0, debt_bondage_south_asia, theater_ratio, 0, 0.25).
narrative_ontology:measurement(debt_bond_tr_t15, debt_bondage_south_asia, theater_ratio, 15, 0.3).
narrative_ontology:measurement(debt_bond_tr_t30, debt_bondage_south_asia, theater_ratio, 30, 0.35).
narrative_ontology:measurement(debt_bond_tr_t45, debt_bondage_south_asia, theater_ratio, 45, 0.35).

% Extraction over time
narrative_ontology:measurement(debt_bond_be_t0, debt_bondage_south_asia, base_extractiveness, 0, 0.68).
narrative_ontology:measurement(debt_bond_be_t15, debt_bondage_south_asia, base_extractiveness, 15, 0.75).
narrative_ontology:measurement(debt_bond_be_t30, debt_bondage_south_asia, base_extractiveness, 30, 0.78).
narrative_ontology:measurement(debt_bond_be_t45, debt_bondage_south_asia, base_extractiveness, 45, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(debt_bondage_south_asia, resource_allocation).
narrative_ontology:boltzmann_floor_override(debt_bondage_south_asia, 0.08).
narrative_ontology:affects_constraint(debt_bondage_south_asia, caste_based_occupational_assignment).
narrative_ontology:affects_constraint(debt_bondage_south_asia, informal_labor_contract_enforcement).
narrative_ontology:affects_constraint(debt_bondage_south_asia, intergenerational_poverty_transmission).

% DUAL FORMULATION NOTE:
% Debt bondage decomposes structurally into three linked constraints: (1) debt_bondage_mechanism (this story, ε=0.78, snare) — the immediate extraction through interest and non-repayment; (2) caste_based_occupational_assignment (upstream, ε=0.72, tangled_rope) — the occupational lock mechanism that makes debt bondage possible; (3) intergenerational_transmission (downstream, ε=0.80, snare) — the inheritance of bondage status. The ε values differ because they address different structural mechanisms: immediate debt dynamics, occupational heritage, and intergenerational reproduction. All three are required for the full constraint pattern.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(debt_bondage_south_asia, moderate, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
