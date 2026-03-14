% ============================================================================
% CONSTRAINT STORY: employer_sponsored_healthcare
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_employer_sponsored_healthcare, []).

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
 *   constraint_id: employer_sponsored_healthcare
 *   human_readable: Employer-Sponsored Healthcare System
 *   domain: economic/healthcare/labor
 *
 * SUMMARY:
 *   Employer-sponsored healthcare in the United States represents a
 *   historically contingent institutional arrangement that locks coordination
 *   and extraction into a single mechanism. Originating from WWII wage
 *   controls that made healthcare benefits a way to compete for workers
 *   without raising nominal wages, the system has become self-perpetuating
 *   through tax incentives ($0.30+ trillion annual foregone revenue),
 *   regulatory design, and cognitive capture. The constraint exhibits all
 *   characteristics of tangled rope: genuine coordination function (risk
 *   pooling across employer populations achieves real efficiency), active
 *   enforcement mechanism (tax subsidies, regulatory barriers to
 *   alternatives), and asymmetric extraction (workers cannot negotiate away
 *   benefits; employers gain leverage; unemployed are excluded; low-wage
 *   workers bear disproportionate burden). The extractiveness trajectory
 *   (0.35 → 0.58 over 50 years) reflects accumulating costs and expanding
 *   scope of leverage as healthcare becomes increasingly expensive and
 *   central to worker wellbeing. The theater ratio increasing from 0.42 to
 *   0.58 indicates that performative elements (employer 'generosity'
 *   narratives, tax-subsidy justifications, benefits administration as status
 *   marker) have grown relative to genuine coordination function.
 *
 * KEY AGENTS:
 *   - Low-wage workers: Primary victims (powerless/trapped) — healthcare dependency forces acceptance of unfavorable wage/condition terms; no exit options
 *   - Unemployed uninsured: Primary victims (powerless/trapped) — excluded from system entirely; pure extraction via denial of access
 *   - Large employers: Primary beneficiaries (powerful/arbitrage) — gain wage suppression leverage, risk pooling benefits, and escape from direct healthcare provision responsibility
 *   - Insurance companies: Institutional beneficiaries (institutional/arbitrage) — guaranteed pooling of populations, risk stratification advantages, employer-intermediated enrollment
 *   - High-wage professionals: Mixed beneficiaries/identity-locked (moderate/identity_locked) — benefit from employer subsidies and risk pooling but structurally mobile and psychologically trapped in professional identity constituted through employer affiliation
 *   - Small business owners: Moderate victims (moderate/constrained) — face high individual premium costs but also benefit from any employee pooling; experience both coordination function and extraction pressure
 *   - Labor unions: Organized moderators (organized/constrained) — reduce extraction through collective bargaining but remain constrained within system; health benefits as negotiated benefit rather than unilateral extraction
 *   - Tax expenditure infrastructure: Institutional maintenance mechanism (institutional/arbitrage) — performs function of justifying system continuation through fiscal theater
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(employer_sponsored_healthcare, 0.58).
domain_priors:suppression_score(employer_sponsored_healthcare, 0.62).
domain_priors:theater_ratio(employer_sponsored_healthcare, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(employer_sponsored_healthcare, extractiveness, 0.58).
narrative_ontology:constraint_metric(employer_sponsored_healthcare, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(employer_sponsored_healthcare, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(employer_sponsored_healthcare, tangled_rope).
narrative_ontology:human_readable(employer_sponsored_healthcare, "Employer-Sponsored Healthcare System").
narrative_ontology:topic_domain(employer_sponsored_healthcare, "economic/healthcare/labor").

domain_priors:requires_active_enforcement(employer_sponsored_healthcare).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(employer_sponsored_healthcare, employers).
narrative_ontology:constraint_beneficiary(employer_sponsored_healthcare, insurance_companies).
narrative_ontology:constraint_beneficiary(employer_sponsored_healthcare, high_wage_workers).
narrative_ontology:constraint_victim(employer_sponsored_healthcare, low_wage_workers).
narrative_ontology:constraint_victim(employer_sponsored_healthcare, unemployed_uninsured).
narrative_ontology:constraint_victim(employer_sponsored_healthcare, small_business_owners).
narrative_ontology:constraint_victim(employer_sponsored_healthcare, healthcare_cost_bearers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-WAGE WORKER (SNARE) — Trapped by dependency on employer health coverage; cannot exit without losing healthcare access. Employment becomes non-voluntary constraint: worker cannot negotiate wages or job conditions because health insurance is bundled. No alternatives for uninsured population without premium income. Maximum suppression and extraction.
constraint_indexing:constraint_classification(employer_sponsored_healthcare, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNEMPLOYED UNINSURED (SNARE) — Completely trapped. No employer sponsorship available; healthcare access locked behind employment. Bears full cost of health risks without coordination benefit. Pure extraction via exclusion from the system.
constraint_indexing:constraint_classification(employer_sponsored_healthcare, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: SMALL BUSINESS OWNER (TANGLED ROPE) — Constrained by high individual premium costs and lack of group bargaining power. Faces genuine coordination problem: pooling risk across employee population reduces per-capita costs (rope function). Simultaneously extracts from employees through cost-shifting (employer share subsidized by wage suppression). Both coordination and asymmetric extraction coexist.
constraint_indexing:constraint_classification(employer_sponsored_healthcare, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: LARGE EMPLOYER (ROPE) — Benefits from risk pooling (genuine coordination function) and wage suppression (workers cannot negotiate away benefits). Has exit options: self-insure, move to alternative systems, or relocate. Experiences constraint as coordination mechanism that solves their risk management problem efficiently.
constraint_indexing:constraint_classification(employer_sponsored_healthcare, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: INSURANCE COMPANY (ROPE) — Benefits from guaranteed pooling of employer-employee populations and risk stratification. Experiences the constraint as coordination: employers aggregate workers for underwriting. Has maximal exit options (regulatory arbitrage, market selection). Net beneficiary.
constraint_indexing:constraint_classification(employer_sponsored_healthcare, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: HIGH-WAGE PROFESSIONAL (TANGLED ROPE, IDENTITY-LOCKED) — Structurally mobile (could exit to individual market or alternative systems) but identity-locked into corporate employment ecosystem. Professional identity constituted through employer affiliation; benefits package is marker of status and belonging. Experiences the constraint as identity-defining rather than coercive, making exit cognitively difficult despite structural mobility. Benefits from coordination and employer subsidy but cannot perceive exit as thinkable from within professional identity frame.
constraint_indexing:constraint_classification(employer_sponsored_healthcare, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 7: LABOR UNION (TANGLED ROPE) — Organized bargaining power constrains extraction but does not eliminate it. Health benefits become negotiated benefit rather than employer-controlled extraction mechanism. Reduces suppression through collective exit threat. Still experiences both coordination (pooling function) and asymmetric extraction (employer controls program and uses it as leverage). Sunset-like quality: universal healthcare would eliminate the constraint entirely, making union-negotiated benefits redundant.
constraint_indexing:constraint_classification(employer_sponsored_healthcare, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: TAX-SUBSIDY INFRASTRUCTURE (PITON) — The employer healthcare system is maintained largely through tax deductibility of employer contributions (theater value: $0.30+ trillion in foregone federal revenue annually). The mechanism is performative: it persists through institutional inertia and path dependency (WWII wage controls created the initial lock-in), not because it solves problems better than alternatives. The tax expenditure is a theatrical justification rather than a functional necessity. Piton classification reflects high theater ratio and decaying original function.
constraint_indexing:constraint_classification(employer_sponsored_healthcare, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational scope, the constraint is a hybrid: genuine coordination problem (risk pooling) layered with asymmetric extraction (access gating, wage suppression, power asymmetry). The analytically identifiable constraint is both features simultaneously. Comparison with other high-income countries reveals the extraction component: universal systems achieve equivalent health outcomes at lower total cost without the employment coupling, demonstrating the extraction is policy choice, not structural necessity.
constraint_indexing:constraint_classification(employer_sponsored_healthcare, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(employer_sponsored_healthcare_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(employer_sponsored_healthcare, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(employer_sponsored_healthcare, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(employer_sponsored_healthcare, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(employer_sponsored_healthcare, TR),
    TR >= 0.70.

:- end_tests(employer_sponsored_healthcare_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Elevated but not maximal. The constraint coordinates genuine problem (healthcare risk pooling) but enables significant wage-suppression extraction. The 0.58 value reflects the empirical reality that employers capture measurable quasi-rents through healthcare coupling without complete wage suppression — worker utility includes healthcare benefits, so complete wage adjustment doesn't occur. Suppression (0.62): Moderate-high. Structural barriers include lack of affordable individual insurance alternatives (pre-ACA this was near-total), regulatory design favoring employer plans, and tax incentives that make employer coverage cheaper than individual. Internalized suppression includes identity fusion (professional identity) and normalized assumption that healthcare comes via employer. Theater ratio (0.58): Moderate-high. Employer generosity narratives, benefits administration theater, and tax-subsidy justifications perform significant functions in maintaining system legitimacy. The theater is not pure deception — it reflects genuine (if modest) coordination benefit — but it obscures the extraction component. The trajectory from 0.42 to 0.58 reflects increasing proportion of system devoted to benefits communication and administrative overhead relative to actual healthcare provision.
 *
 * PERSPECTIVAL GAP:
 *   The most dramatic gap appears between the powerless/trapped low-wage worker (snare: χ=ε×1.36×1.0=0.79) and the institutional beneficiary (rope: χ=ε×(-0.12)×1.0=negative/near-zero). The same constraint produces experienced extractiveness of 0.79 for the worker and effective subsidization for the employer. High-wage professionals occupy an analytically instructive middle ground: they are structurally mobile (could exit to self-employment, individual market, or different system) but psychologically trapped (identity-lock: professional identity constituted through employer affiliation). This creates a perspectival gap between their own perception (rope/coordination) and the analytical observer's assessment (tangled rope/extraction). The union perspective shows how organized power transforms snare into negotiated tangled rope by converting healthcare from employer-controlled extraction into jointly-determined benefit. The tax-subsidy piton perspective reveals the institutional maintenance mechanism: the system is sustained through fiscal theater rather than functional necessity — the coordination benefit could be achieved through alternative mechanisms (small-group exchanges, public option, mutual aid) at lower cost and without employment coupling.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values are derived from structural relationships: low-wage workers (victims + trapped) produce high d (~0.92) and experience maximum f(d) ≈ 1.36 effective extraction multiplier; employers (beneficiaries + arbitrage) produce low d (~0.08) and experience f(d) ≈ -0.12 or negative effective extraction (constraint subsidizes them). High-wage professionals occupy intermediate position: structurally mobile (would suggest d~0.65, f(d)~1.00) but identity-locked (the psychological binding reduces effective exit capacity), creating d~0.72 with f(d)~1.15. The identity-lock generates a perspectival gap: the professional perceives the constraint as rope (identity-affirming coordination) while an outside observer sees tangled rope or snare (extraction hidden by identity fusion). Small business owners occupy genuine middle position (d~0.60): they are partly victims of high individual insurance costs and partly beneficiaries if they can pool employees. Scope modifier σ(S) = 1.0 (national) because the constraint operates uniformly across US labor market; no local/regional variation that would change the extractiveness scaling.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating genuine hybridity: tangled rope classification is correct because BOTH coordination function and asymmetric extraction are structurally necessary features, not optional add-ons. The coordination problem (healthcare risk pooling) is real and non-trivial — individual insurance markets fail due to adverse selection and lack of pooling economies. The extraction component (wage suppression, access gating, excluded unemployment uninsured) is also real and non-separable under current institutional design. However, the constraint is NOT inherently tangled rope. Comparative institutional analysis shows that other countries solve the coordination problem (risk pooling) through alternative mechanisms (national insurance, public option, cooperative structures) without the extraction component. This proves the extraction is not a necessary feature of healthcare coordination — it is a policy choice enabled by the current institutional design. The mandatrophy resolves through recognition that the constraint is contingent tangled rope, not essential. The analytical observer sees that the system's persistence depends on suppression mechanisms (tax incentives, regulatory barriers, identity-lock) rather than on technical necessity. Policy change that creates effective alternative pooling mechanisms (Medicare for All, public option with employer buyout, cooperative exchanges) would convert tangled rope into scaffold with sunset clause (the constraint would phase out as workers accessed superior alternatives) or into rope (pure coordination without extraction).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    identity_lock_persistence,
    'Is the high-wage professional''s identity-lock to employer healthcare a persistent cognitive binding or temporary path dependence that dissolves when alternatives become salient?',
    'Natural experiments comparing professional identity stability pre/post transition to universal healthcare systems (e.g., surveying US expat professionals in countries with universal coverage); measurement of professional identity reconstruction after job loss or forced system transition',
    'If persistent identity lock: constraint''s suppression is underestimated (workers'' own psychology reinforces employment dependence). If temporary: analytical observer perspective underestimates how quickly the constraint would dissolve if institutional frame changed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_persistence, empirical, 'Whether professional identity-lock to employer healthcare persists or dissolves with alternatives').

omega_variable(
    coordination_function_sufficiency,
    'What portion of the coordination benefit achieved by employer pooling could be achieved through alternative institutional structures (small-group purchasing cooperatives, state exchanges, public option) without the extraction component?',
    'Comparative cost analysis of risk pooling mechanisms; health economics literature on administrative costs of alternative coordination structures; historical data on pre-employer-healthcare mutual aid societies and their cost structures',
    'If alternatives achieve 80%+ of coordination benefit: the tangled rope classification is correct and the extraction component is separable from coordination. If alternatives achieve <60%: the extractive and coordination components may be structurally interdependent, suggesting the constraint is more rope-like than tangled.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(coordination_function_sufficiency, empirical, 'Whether coordination benefits require employment coupling or are separable from extraction').

omega_variable(
    suppression_mechanism_decomposition,
    'What percentage of measured suppression (0.62) is structural (no alternative financing, legal barriers) versus internalized (belief that employer healthcare is normal, identity fusion, lack of salience of alternatives)?',
    'Survey design testing salience of alternatives and perceived mobility; longitudinal tracking of suppression measures in jurisdictions transitioning to universal coverage; international comparison of self-reported healthcare access constraints in workers with and without employment-tied systems',
    'If suppression is >50% structural: exit barriers are objective (remains true even with education/awareness campaigns). If >50% internalized: cognitive capture is the binding mechanism; suppression could be dramatically reduced through frame-shifting without policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_decomposition, empirical, 'Structural vs. internalized suppression mechanisms').

omega_variable(
    extraction_magnitude_vs_alternatives,
    'What is the actual economic extraction flowing to employers from wage suppression enabled by healthcare bundling, net of employer healthcare costs?',
    'Econometric analysis comparing wage trajectories for equivalent workers in universal healthcare systems vs. US employer-sponsored systems; quantification of employer-captured quasi-rents from healthcare leverage in wage negotiations; calculation of counterfactual wage distributions absent healthcare coupling',
    'If net extraction is low (<5% of worker income): constraint is primarily coordination with minor asymmetry (rope classification more appropriate). If net extraction is high (>15% of worker income): tangled_rope or snare classification is correct. Empirical answer determines classification accuracy.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(extraction_magnitude_vs_alternatives, empirical, 'Magnitude of employer extraction from healthcare coupling').

omega_variable(
    labor_union_coalition_threshold,
    'At what percentage of unionized workforce does collective bargaining power flip the constraint from snare to negotiated tangled rope or scaffold-like sunset?',
    'Historical analysis of unionization rates correlated with health benefit quality and worker agency; simulation of coalition dynamics; comparison of healthcare outcomes and worker satisfaction across high-unionization vs. low-unionization industries',
    'If threshold is <20% unionization: easily achievable organizing targets exist; organized labor could reshape constraint type system-wide. If threshold is >50%: systemic change requires majority unionization (currently 10% private sector), suggesting constraint is structurally stable absent policy change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_union_coalition_threshold, empirical, 'Coalition threshold for organized labor to transform constraint type').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(employer_sponsored_healthcare, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(esh_tr_t0, employer_sponsored_healthcare, theater_ratio, 0, 0.42).
narrative_ontology:measurement(esh_tr_t25, employer_sponsored_healthcare, theater_ratio, 25, 0.52).
narrative_ontology:measurement(esh_tr_t50, employer_sponsored_healthcare, theater_ratio, 50, 0.58).

% Extraction over time
narrative_ontology:measurement(esh_be_t0, employer_sponsored_healthcare, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(esh_be_t25, employer_sponsored_healthcare, base_extractiveness, 25, 0.48).
narrative_ontology:measurement(esh_be_t50, employer_sponsored_healthcare, base_extractiveness, 50, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(employer_sponsored_healthcare, resource_allocation).
narrative_ontology:boltzmann_floor_override(employer_sponsored_healthcare, 0.18).
narrative_ontology:affects_constraint(employer_sponsored_healthcare, medical_bankruptcy).
narrative_ontology:affects_constraint(employer_sponsored_healthcare, wage_stagnation_unskilled_labor).
narrative_ontology:affects_constraint(employer_sponsored_healthcare, healthcare_access_inequality).
narrative_ontology:affects_constraint(employer_sponsored_healthcare, labor_market_mobility_lock).
narrative_ontology:affects_constraint(employer_sponsored_healthcare, small_business_formation_barriers).

% DUAL FORMULATION NOTE:
% Employer-sponsored healthcare is upstream of multiple healthcare, labor, and inequality constraints. Medical bankruptcy is a direct downstream effect (when employer coverage is lost, catastrophic costs ensue). Wage stagnation for low-wage workers is downstream: employers can suppress wage growth by expanding benefits rather than increasing wages. Healthcare access inequality is downstream: employment-tiered access creates stratification. Labor market mobility lock is downstream: workers cannot change jobs without risking healthcare access loss. Small business formation is downstream: inability to self-insure or access affordable group rates constrains entrepreneurship. The upstream constraint is the tax/regulatory framework that privileges employer-sponsored coverage — this could be separated into its own story if analysis focuses on fiscal policy rather than labor relations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(employer_sponsored_healthcare, moderate, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
