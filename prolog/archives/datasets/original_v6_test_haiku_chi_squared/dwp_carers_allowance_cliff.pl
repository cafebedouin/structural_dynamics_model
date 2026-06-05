% ============================================================================
% CONSTRAINT STORY: dwp_carers_allowance_cliff
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
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
 *   The UK Carer's Allowance system creates an earnings cliff whereby carers
 *   lose their entire £76.35 weekly allowance if they earn more than £132 per
 *   week (or £168 if they have looked after the care recipient for 8+ weeks).
 *   This threshold has remained largely static since 2013 (indexation to
 *   earnings only from 2024). The constraint exemplifies how means-tested
 *   welfare systems embed both genuine coordination (subsidizing family care
 *   as a public good) and asymmetric extraction (through behavioral control
 *   via sharp thresholds and suppressed exit options). A working carer with a
 *   care-dependent relative faces a binary choice: stay below the earnings
 *   threshold and receive the allowance, or exceed it and lose it entirely.
 *   The loss exceeds the earnings gain for most wage rates, creating a
 *   negative incentive to work or earn above the threshold. The constraint is
 *   downstream of the broader means-tested welfare architecture, which uses
 *   sharp thresholds as administrative simplifications and behavioral
 *   controls. From the carer's perspective, it is a snare: they are trapped
 *   between care obligation and earnings suppression. From the DWP's
 *   perspective, it is a coordination mechanism that efficiently subsidizes
 *   family care. From a reform coalition perspective, it is a temporary
 *   scaffold pending taper implementation. From the piton perspective, it is
 *   a degraded feature of institutional inertia.
 *
 * KEY AGENTS:
 *   - Working Carers: Primary victims (powerless/trapped) — face binary choice to stay below threshold or lose allowance entirely
 *   - Care-Dependent Households: Primary beneficiaries of allowance but secondarily victimized (moderate/constrained) — carer's income constraint limits household economic mobility
 *   - Department for Work and Pensions: Institutional beneficiary (institutional/arbitrage) — controls budget through sharp threshold; views constraint as coordination mechanism
 *   - Care Reform Coalition: Organized agents (charities, unions, advocates) (organized/constrained) — building momentum for taper implementation; see temporary scaffold with sunset
 *   - Means-Tested Welfare System: Institutional actor (institutional/analytical) — maintains sharp cliff through architectural inertia (piton perspective)
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — sees hybrid coordination-extraction structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dwp_carers_allowance_cliff, 0.58).
domain_priors:suppression_score(dwp_carers_allowance_cliff, 0.72).
domain_priors:theater_ratio(dwp_carers_allowance_cliff, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dwp_carers_allowance_cliff, extractiveness, 0.58).
narrative_ontology:constraint_metric(dwp_carers_allowance_cliff, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(dwp_carers_allowance_cliff, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dwp_carers_allowance_cliff, snare).
narrative_ontology:human_readable(dwp_carers_allowance_cliff, "UK DWP Carer's Allowance Earnings Cliff").
narrative_ontology:topic_domain(dwp_carers_allowance_cliff, "economic/political").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dwp_carers_allowance_cliff, state_social_care_budget).
narrative_ontology:constraint_beneficiary(dwp_carers_allowance_cliff, care_dependent_households).
narrative_ontology:constraint_victim(dwp_carers_allowance_cliff, working_carers).
narrative_ontology:constraint_victim(dwp_carers_allowance_cliff, care_work_viability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WORKING CARER (SNARE) — Trapped between care obligation (dependent relative) and earnings threshold (£132/week). Earning £1 above threshold loses entire £76.35/week allowance. Cannot exit care responsibility; cannot freely earn. d≈0.92, f(d)≈1.38, σ=1.0 → χ≈0.80.
constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CARE-DEPENDENT HOUSEHOLD (SNARE) — Benefits from carer's allowance enabling family care, but trapped in precarious arrangement. Carer's constrained earnings (unable to work above threshold) limits household economic mobility. Extraction takes form of enforced reliance on means-tested allowance. d≈0.70, f(d)≈1.05, σ=1.0 → χ≈0.61.
constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: DEPARTMENT FOR WORK AND PENSIONS (ROPE) — Sees earnings cliff as a coordination mechanism: by setting a sharp threshold, DWP coordinates carer labor supply against formal care market. The cliff is efficient from state budgeting perspective (clear threshold, low administrative overhead). DWP has substantial exit optionality (can modify threshold, adjust allowance rate, implement taper). d≈0.15, f(d)≈0.02, σ=1.0 → χ≈0.01. Minimal effective extraction from DWP's perspective; it experiences this as pure coordination.
constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CARE REFORM COALITION (SCAFFOLD) — Organized actors (charities, care sector unions, policy advocates) see the cliff as a temporary failure with a sunset: rising care costs, social care green paper proposals, and cross-party recognition of carer poverty are building momentum for reform. Taper mechanisms (replacing cliff with gradual withdrawal) are technically available and politically viable. This is explicitly framed as transitional policy pending comprehensive social care reform. d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.28.
constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MEANS-TESTED WELFARE SYSTEM (PITON) — The earnings cliff is a vestigial feature of the broader means-tested welfare architecture (Universal Credit, housing benefit, council tax support all have their own thresholds and tapers). The sharp cliff persists through institutional inertia despite its known perverse incentives. Theater ratio reflects that the system performs budget containment symbolically (strict threshold signals stringency) more than functionally. theater_ratio=0.35 reflects moderate performance: the cliff does actually reduce some claims but also creates administrative complexity, appeals, and compliance verification overhead. The system maintains the cliff because alternatives (taper, negative income tax, basic income) threaten the entire means-tested architecture.
constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint exhibits both genuine coordination (state subsidizing family care is economically rational compared to commercial care market) AND asymmetric extraction (sharp cliff creates efficiency loss, traps carers, generates deadweight loss). The constraint is neither a natural law nor pure extraction but a hybrid institutional arrangement: the coordination function (subsidizing family care) is genuine, but it is embedded in a means-tested logic that extracts behavioral control from carers. ε=0.58 and suppression=0.72 confirm tangled structure. d≈0.65, f(d)≈0.95, σ=1.0 → χ≈0.55.
constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dwp_carers_allowance_cliff_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dwp_carers_allowance_cliff, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.58): The constraint exhibits significant extraction through suppressed earnings and behavioral control. The carer loses £76.35 for each pound of earnings above £132 (an implicit 100% marginal tax rate), creating strong disincentive. However, the extraction is not maximal (like a Snare at ε=0.70+) because the allowance itself is a genuine transfer to carers, and the state benefits from family care substitution for commercial care. The moderate-high value reflects that extraction operates through threshold discontinuity rather than continuous rent-taking. Suppression (0.72): High. Carers face multiple barriers: care obligations create time constraint; welfare rules eliminate earnings above threshold; alternative care arrangements are expensive or unavailable; social stigma surrounds means-tested benefits; employment discrimination affects carers. However, suppression is not total (0.95) because some carers work within the constraint or exit care (though at substantial cost). Theater ratio (0.35): Low-moderate. The constraint is functional, not primarily performative. The threshold actually performs budget containment and carer sorting; it is not a ritual. However, some performative content exists: the specific threshold of £132 is somewhat arbitrary and has symbolic function (appears stringent), and administrative overhead in monitoring slightly exceeds pure efficiency.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a sharp perspectival gap between the powerless carer (snare) and the institutional DWP (rope). The carer experiences binary choice and behavioral extraction. The DWP experiences coordination: the allowance efficiently subsidizes family care at lower cost than commercial alternatives. The care-dependent household occupies an intermediate position: it benefits from the allowance but is secondarily trapped by the carer's constrained earnings. The reform coalition sees this as temporary (scaffold), while the piton perspective reveals institutional inertia: the sharp cliff persists because the entire means-tested welfare system depends on sharp thresholds and administrative simplification. The analytical observer integrates these: the constraint is genuinely tangled (coordination + extraction), not reducible to either pole.
 *
 * DIRECTIONALITY LOGIC:
 *   Working Carer: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction experience. Cannot exit care obligation; cannot exit earnings constraint without losing allowance. Care-dependent household: Both beneficiary (receives allowance) and victim (trapped in reliance, carer constrained) + constrained → d≈0.70, f(d)≈1.05. Mixed experience; benefits from allowance but secondarily trapped. DWP: Beneficiary (budget control) + arbitrage (can modify rules) → d≈0.15, f(d)≈0.02. Low extraction from institutional perspective; views as coordination. Reform Coalition: Organized + constrained → d≈0.45, f(d)≈0.48. Moderate effective extraction; coalition has agency and perceives sunset path. Welfare System Architecture: Institutional + analytical → piton classification from theater gate, not directionality.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint resolves mandatrophy by demonstrating that the six types are legitimate readings from different structural positions. The working carer's snare is not 'wrong' — it is their actual structural experience. The DWP's rope is not 'wrong' — the allowance genuinely coordinates family care provision. The contradiction dissolves when indexed to power and exit options. A snare for a powerless agent with no exit can be a rope for an institutional actor with substantial exit optionality, operating on the same underlying ε and suppression values. The scaffold perspective is empirically verifiable: taper mechanisms exist (Scotland, Wales), policy proposals abound, and political consensus is building. If the cliff were replaced by a taper, the snare would degrade toward tangled rope. If the cliff persists beyond 10 years despite reform proposals, the scaffold perspective would become aspirational (false sunset), and the snare classification would be confirmed. The piton perspective reveals that institutional inertia — the cliff persists because sharp thresholds are load-bearing for the entire means-tested welfare architecture, not because the cliff itself is necessary. This is not false naturalization but accurate diagnosis of institutional constraint.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    cliff_replacement_timeline,
    'When will the sharp earnings cliff be replaced by a gradual taper mechanism?',
    'Policy change documentation; legislative amendments to Social Security Contributions and Benefits Act 1992; DWP official guidance revisions',
    'If replaced within 5 years: scaffold perspective confirmed, constraint is genuinely temporary. If persists beyond 10 years: scaffold is aspirational, constraint becomes structural feature of welfare system (snare dominates).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(cliff_replacement_timeline, empirical, 'Timeline for replacement of sharp cliff with taper mechanism').

omega_variable(
    administrative_cost_justification,
    'Do the administrative costs of monitoring the £132 threshold justify the modest savings from excluding carers above the threshold?',
    'DWP cost-benefit analysis; auditor office assessment; comparison of administration costs to prevented claims',
    'If administrative costs exceed savings: cliff is pure extraction (snare confirmed). If savings justify costs: cliff is legitimate coordination (rope from beneficiary perspective strengthened).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(administrative_cost_justification, empirical, 'Whether administrative costs of earnings monitoring justify threshold savings').

omega_variable(
    care_market_substitution_effect,
    'How much of the carer''s allowance effect reflects genuine substitution between family care and commercial care versus simple income transfer?',
    'Econometric analysis comparing care arrangements before/after allowance eligibility; causal inference from policy changes; household survey data on care source',
    'If substitution effect large (carers genuinely replace commercial care): coordination function is primary (rope/tangled rope). If substitution effect small: allowance is mostly transfer with behavioral extraction (snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(care_market_substitution_effect, empirical, 'Magnitude of substitution between family care and commercial care').

omega_variable(
    taper_implementation_feasibility,
    'Are technical and budgetary constraints preventing taper implementation, or is the sharp cliff maintained for ideological reasons?',
    'Institutional history; interviews with DWP policy designers; comparison to Scotland and Wales tapering policies; budget impact modeling',
    'If technical/budgetary: constraint is structural, taper unlikely (snare endures). If ideological: constraint is contingent on political will, taper is readily available (scaffold framework robust).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(taper_implementation_feasibility, conceptual, 'Whether taper implementation is technically feasible or ideologically resisted').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dwp_carers_allowance_cliff, 0, 16).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(carer_tr_t0, dwp_carers_allowance_cliff, theater_ratio, 0, 0.28).
narrative_ontology:measurement(carer_tr_t8, dwp_carers_allowance_cliff, theater_ratio, 8, 0.31).
narrative_ontology:measurement(carer_tr_t16, dwp_carers_allowance_cliff, theater_ratio, 16, 0.35).

% Extraction over time
narrative_ontology:measurement(carer_be_t0, dwp_carers_allowance_cliff, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(carer_be_t8, dwp_carers_allowance_cliff, base_extractiveness, 8, 0.5).
narrative_ontology:measurement(carer_be_t16, dwp_carers_allowance_cliff, base_extractiveness, 16, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dwp_carers_allowance_cliff, resource_allocation).
narrative_ontology:affects_constraint(dwp_carers_allowance_cliff, means_tested_welfare_cliff_architecture).
narrative_ontology:affects_constraint(dwp_carers_allowance_cliff, care_work_formal_market_substitution).
narrative_ontology:affects_constraint(dwp_carers_allowance_cliff, informal_care_dependency_trap).

% DUAL FORMULATION NOTE:
% The carer's allowance cliff is downstream of the broader means-tested welfare system architecture (Universal Credit, housing benefit, council tax support all use similar thresholds). It is also a specific instantiation of the care work viability constraint: the low allowance rate (£76.35/week as of 2024) combined with the sharp cliff makes formal care work unviable for many carers. Separate constraint stories exist for the broader welfare cliff architecture and for care market dynamics; this story focuses specifically on the allowance threshold mechanism and its behavioral extraction.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
