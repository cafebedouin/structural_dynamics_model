% ============================================================================
% CONSTRAINT STORY: dwp_carers_allowance_cliff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dwp_carers_allowance_cliff, []).

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
 *   constraint_id: dwp_carers_allowance_cliff
 *   human_readable: UK DWP Carer's Allowance Earnings Cliff
 *   domain: economic/political
 *
 * SUMMARY:
 *   The UK's Carer's Allowance provides £68.15 per week (as of 2026) to
 *   individuals providing at least 35 hours of unpaid care. The allowance
 *   contains a sharp earnings cliff: any earnings above £123 per week result
 *   in £1 reduction in allowance per £1 earned, creating an effective
 *   marginal tax rate of 100% (or higher if combined with other means-tested
 *   benefits). This constraint exemplifies how welfare design can embed
 *   extraction mechanisms within ostensibly supportive coordination systems.
 *   The same policy structure simultaneously enables care provision
 *   (coordination benefit) and punishes work effort (extraction mechanism),
 *   creating a snare specifically for carers attempting to supplement
 *   inadequate allowance through market work. The constraint's theater ratio
 *   is lower than typical pitons because the cliff's operation is transparent
 *   and mechanically enforced—there is minimal performative overhead, just
 *   direct financial punishment at the boundary.
 *
 * KEY AGENTS:
 *   - Working Carers (powerless/trapped): Primary victims—face 100%+ marginal tax rate at the cliff; cannot exit care obligation or work option without severe cost
 *   - Care-Dependent Household (moderate/constrained): Organized enough to understand welfare rules but constrained by care obligations and benefit eligibility; household internally divided (dependent benefits from care funding, carer bears work disincentive)
 *   - DWP Budget Administration (institutional/arbitrage): Primary beneficiary—experiences cliff as efficient resource targeting; has arbitrage options (adjust threshold, taper rate, redistribute budget) at low political cost
 *   - State Exchequer (institutional/arbitrage): Secondary beneficiary—cliff reduces welfare spending by deterring earnings-based care supplementation
 *   - Care-Providing Sector (organized/constrained): Partially organized (professional care providers, family networks) but constrained by care-dependent population size and funding availability; competes with DWP for carers via wage offers
 *   - Policy Reform Coalition (organized/constrained): Charities, think tanks, disabled rights organizations advocating taper-based reform; organized but constrained by political bandwidth and budget opposition; see the cliff as technical error rather than intentional mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dwp_carers_allowance_cliff, 0.58).
domain_priors:suppression_score(dwp_carers_allowance_cliff, 0.72).
domain_priors:theater_ratio(dwp_carers_allowance_cliff, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dwp_carers_allowance_cliff, extractiveness, 0.58).
narrative_ontology:constraint_metric(dwp_carers_allowance_cliff, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dwp_carers_allowance_cliff, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dwp_carers_allowance_cliff, tangled_rope).
narrative_ontology:human_readable(dwp_carers_allowance_cliff, "UK DWP Carer's Allowance Earnings Cliff").
narrative_ontology:topic_domain(dwp_carers_allowance_cliff, "economic/political").

domain_priors:requires_active_enforcement(dwp_carers_allowance_cliff).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dwp_carers_allowance_cliff, state_welfare_budget).
narrative_ontology:constraint_beneficiary(dwp_carers_allowance_cliff, care_dependent_household).
narrative_ontology:constraint_victim(dwp_carers_allowance_cliff, working_carers).
narrative_ontology:constraint_victim(dwp_carers_allowance_cliff, labor_force_participation).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% A carer earning £120/week faces total loss of £68.15/week allowance if earnings exceed £123. Effective marginal tax rate exceeds 100% across the boundary. Trapped by care obligations (cannot abandon dependent) and by benefit cliff (cannot work additional hours without total allowance loss). Experiences pure extraction: the state extracts all marginal earning above the threshold while the carer bears full care cost. No exit option; no recourse.
constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% A carer with earning capacity between £0-£123/week experiences coordination (the allowance enables care) and extraction (the cliff penalizes work). Partially mobile: can reduce hours, seek informal care-sharing, or exit formal employment entirely, but each option carries cost (income loss, care quality reduction, social isolation). The constraint both enables care provision (coordination benefit) and prevents advancement (extraction cost). Constrained exit option reflects real but costly alternatives.
constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% From the state's administrative perspective, the cliff is a coordination mechanism: it solves the problem of directing limited welfare resources to households genuinely unable to supplement care costs through work. The state has arbitrage options (adjust the threshold, taper the withdrawal, redirect budget). The cliff extracts from carers but at no cost to the state—it is experienced as efficient resource targeting. The state benefits from low uptake of allowance at higher earnings thresholds.
constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% The household (carer + dependent) experiences both coordination and extraction. The allowance funds essential care; work disincentives at the cliff constrain household income. The household is organized enough to navigate welfare systems but constrained by care obligations and benefit design. Multiple agents within the household experience the constraint differently: the dependent benefits from care funding, the carer bears the extraction cost. Household's exit option (move region, change family structure, institutionalize dependent) is constrained by cost and care quality.
constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% The cliff persists as a technical artifact of legacy benefit design (administered through SERPS/Pension Credit systems, now Universal Credit). Policy discussion treats the cliff as an unfortunate technical byproduct ('we would taper, but the system was not designed for it') rather than a deliberate extraction mechanism. The theater ratio reflects this: substantial policy attention to 'fixing' the cliff (reviews, proposed reforms) produces minimal structural change. The constraint endures through institutional inertia and budget constraints, not through intentional enforcement. Piton classification: χ ≤ 0.25, theater ≥ 0.70.
constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% From a civilizational analytical perspective, care work and market labor are fundamentally incommensurable: the household must choose between unpaid essential care and paid income. This incommensurability could be framed as a natural law — a structural feature of how human care systems work — but empirical analysis reveals it is contingent on welfare system design, not immutable. The 'naturalness' of the cliff is a false summit: other systems (taper withdrawal, negative income tax, care stipends) reduce the trade-off without eliminating care. The mountain classification reveals the sophistry in naturalizing policy choices.
constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dwp_carers_allowance_cliff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dwp_carers_allowance_cliff, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dwp_carers_allowance_cliff, TR),
    TR >= 0.70.

:- end_tests(dwp_carers_allowance_cliff_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): The constraint extracts from carers by preventing earnings accumulation above the threshold. The extraction is not total (carers can earn up to £123 without penalty, the allowance itself is a transfer) but severe in the marginal zone—the 100% effective tax rate removes all marginal income from work within the cliff zone, forcing carers to choose between care and income. The value reflects that the extraction is targeted (only high-earning carers) but severe (complete marginal elimination). Historical increase from 0.35 to 0.58 reflects real-wage decline in allowance relative to work opportunities, making the cliff increasingly binding. Suppression (0.72): High. Carers face multiple suppression mechanisms: care obligation (cannot exit care), institutional (must navigate means-test eligibility), market (wage offers insufficient to justify care reduction), and benefit design itself (the cliff is the suppression mechanism). Suppression reflects that carers have few genuine exit options—they cannot work, institutionalize care, or claim they are not carers without severe cost. Theater Ratio (0.48): Moderate-low. Unlike many welfare policies, the cliff operates mechanically with transparent financial incentives—there is minimal ritual or performative compliance. Policy reviews and reform discussions (theater) are frequent but produce minimal structural change (mechanical operation persists). The ratio reflects that while policy discussion is theatrical, the actual extraction mechanism is straightforward and unperformed.
 *
 * PERSPECTIVAL GAP:
 *   The carer at the cliff edge (powerless/trapped) sees a snare—the constraint extracts without coordinating benefit. The working carer (moderate/constrained) sees tangled rope—the allowance enables care but prevents income advancement. The DWP (institutional/arbitrage) sees rope—an efficient coordination mechanism for targeting resources. The care-dependent household (organized/constrained) sees the same tangled rope as the moderate carer but with internal heterogeneity (dependent sees coordination, carer sees extraction). The policy reform coalition (organized/constrained) sees piton—a degraded policy mechanism that persists through inertia despite known problems. The analytical observer risks seeing mountain—the care-work trade-off as an inevitable feature of human obligations—but empirical analysis reveals the cliff is a contingent policy choice, not a law of nature. The perspectival gap is substantial: beneficiaries experience coordination, victims experience extraction, reformers experience inertia, and the observer risks naturalizing contingency.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation chain yields high d (directionality toward target) for carers because: (1) they are declared victims (bear earnings loss), (2) they are powerless in the institutional frame (cannot negotiate benefit terms), and (3) they have trapped exit options (care obligation makes exit costly). f(d) for a trapped powerless victim is ~1.42, producing high experienced extractiveness χ. Conversely, the DWP administration has low d (directionality toward beneficiary) because: (1) they are declared beneficiary (budget saved), (2) they are institutional (administrative capacity to set and enforce terms), and (3) they have arbitrage options (can adjust threshold, taper rate, or redirect budget). The beneficiary's d is ~0.00, f(d) ≈ -0.12, producing negative experienced χ (the constraint subsidizes the administrator). The care-dependent household has moderate d (both benefits from allowance and bears carer's earnings disincentive) because it is internally divided: the dependent benefits, the carer bears extraction. Scope modifier σ(national) = 1.0 (baseline); no amplification or dampening from scope.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLUTION: The constraint is a genuine tangled rope, not a mislabeled snare or rope. It exhibits both coordination (the allowance funds essential care) and asymmetric extraction (the cliff penalizes work). The mandatrophy is resolved by recognizing that the two functions are structurally coupled: the allowance is means-tested because the state wants to concentrate resources on households unable to supplement care costs through work, which is rational coordination. The cliff is the mechanism that enforces this targeting, which is rational extraction of the administrative variety (preventing 'welfare abuse'). The constraint is not a snare masquerading as coordination because carers below the cliff genuinely do receive the allowance without penalty; it is not pure coordination because the cliff mechanism extracts from those who attempt to work. The extractiveness (0.58) reflects that the coordination benefit is moderate (£68.15/week is below minimum viable care cost in most regions) and the extraction is severe (100% marginal rate), but the constraint's purpose is dual—coordination of resources to care provision and prevention of means-test 'abuse.' The theater ratio is low (0.48) because the cliff is mechanically transparent; the policy discussion around reform is theatrical, but the extraction mechanism itself requires no performative maintenance. The mandatrophy is resolved: this is a structurally coherent tangled rope where both functions are authentic and coupled.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cliff_causation_vs_correlation,
    'Does the cliff cause reduced earnings, or do carers with lower earning capacity self-select into the region below the threshold?',
    'Difference-in-differences analysis comparing carers in regions with different cliff thresholds (if such variation existed historically); quasi-experimental variation from policy changes; comparison with equivalent carers in systems without cliffs (e.g., Scottish Carer''s Allowance under different tapers)',
    'If causal: the cliff is an active extraction mechanism, extractiveness remains ~0.58. If selection: the low observed earnings reflect carer characteristics, not cliff disincentive—extractiveness might be lower if carers below the cliff are simply unable to work more hours due to care obligations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cliff_causation_vs_correlation, empirical, 'Whether the cliff causally reduces earnings or reflects self-selection').

omega_variable(
    alternative_design_feasibility,
    'Is the cliff an inevitable feature of means-tested welfare, or does alternative design (gradual taper, negative income tax, dual-income support) eliminate it without destroying the care-work trade-off?',
    'Cross-national comparative analysis (Scottish Carer''s Allowance taper design, equivalent systems in other nations); cost-benefit analysis of alternative tapers against current cliff design; microsimulation of labor supply response to alternative withdrawal rates',
    'If feasible alternatives exist: the cliff is a chosen policy mechanism, extractiveness remains high, and the constraint is a snare/tangled_rope. If alternatives are infeasible (too costly, create worse perverse incentives): the cliff is a tragic binding constraint, possibly approaching mountain status.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_design_feasibility, empirical, 'Whether alternative welfare designs can reduce the cliff without eliminating care-work trade-off').

omega_variable(
    deadweight_loss_asymmetry,
    'Does the state''s budget saving from the cliff (carers working less, lower allowance payouts) exceed the deadweight loss (reduced labor supply, lower tax revenue, reduced economic output)?',
    'Administrative data on allowance savings from the cliff; labor supply elasticity estimation for affected carers; tax revenue impact analysis; comparison with estimated deadweight loss from equivalent marginal tax rates in other policy contexts',
    'If savings > deadweight loss: the cliff is economically rational from the state''s narrow budget perspective, extractiveness justified as coordination mechanism. If savings < deadweight loss: the cliff is Pareto-inferior (makes everyone worse off including the state), suggesting it persists through institutional inertia (piton) rather than rational design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deadweight_loss_asymmetry, empirical, 'Whether state budget savings from the cliff exceed total deadweight loss').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dwp_carers_allowance_cliff, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dwp_ca_tr_t0, dwp_carers_allowance_cliff, theater_ratio, 0, 0.52).
narrative_ontology:measurement(dwp_ca_tr_t10, dwp_carers_allowance_cliff, theater_ratio, 10, 0.5).
narrative_ontology:measurement(dwp_ca_tr_t20, dwp_carers_allowance_cliff, theater_ratio, 20, 0.48).

% Extraction over time
narrative_ontology:measurement(dwp_ca_be_t0, dwp_carers_allowance_cliff, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(dwp_ca_be_t10, dwp_carers_allowance_cliff, base_extractiveness, 10, 0.52).
narrative_ontology:measurement(dwp_ca_be_t20, dwp_carers_allowance_cliff, base_extractiveness, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dwp_carers_allowance_cliff, resource_allocation).
narrative_ontology:affects_constraint(dwp_carers_allowance_cliff, universal_credit_taper_cliff).
narrative_ontology:affects_constraint(dwp_carers_allowance_cliff, disability_work_allowance_threshold).
narrative_ontology:affects_constraint(dwp_carers_allowance_cliff, uk_unpaid_care_labor_trap).

% DUAL FORMULATION NOTE:
% The Carer's Allowance cliff is part of a larger constraint family around means-tested benefit cliffs in UK welfare. The cliff is downstream of the broader resource allocation problem (how to target welfare to households in need) but structurally distinct from related constraints. Universal Credit taper (different parameters, gradual rather than sharp) affects working-age households generally. Disability Work Allowance has similar cliff structure for disability-related care workers. The unpaid care labor trap is the upstream structural constraint (care work is economically irrational relative to market work); the allowance cliff is a downstream manifestation of that trap under means-tested welfare design.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
