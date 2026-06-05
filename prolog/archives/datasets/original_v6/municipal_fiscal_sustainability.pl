% ============================================================================
% CONSTRAINT STORY: municipal_fiscal_sustainability
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_municipal_fiscal_sustainability, []).

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
 *   constraint_id: municipal_fiscal_sustainability
 *   human_readable: Municipal Fiscal Sustainability Constraint
 *   domain: public_finance/local_governance
 *
 * SUMMARY:
 *   Municipal fiscal sustainability constraints represent a fundamental
 *   tension in federal systems: local governments must provide essential
 *   services (education, public safety, infrastructure, social services)
 *   while dependent on limited, often regressive revenue bases and subject to
 *   state and federal fiscal rules, cost-shifting mandates, and borrowing
 *   restrictions. This constraint exhibits multiple structural forms
 *   simultaneously from different positions. For residents and service
 *   workers, it appears as an extractive snare where fiscal pressure
 *   translates directly to service cuts and wage constraints. For creditors,
 *   it appears as coordination enabling predictable municipal bond markets.
 *   For central governments, it combines coordination (preventing cascading
 *   fiscal crises) with extraction (consolidating power over local resource
 *   allocation). For the technical compliance regime, it has become largely
 *   performative — metrics gaming masks underlying structural deficits. The
 *   constraint's extractiveness (0.58) and theater ratio (0.68) have risen
 *   over the measurement interval, reflecting both growing fiscal pressure
 *   and increasing reliance on accounting techniques rather than structural
 *   solutions. The suppression level (0.65) reflects substantial barriers to
 *   exit (residents trapped by place-boundedness, workers by labor market
 *   constraints) and limits on solution options (service cuts vs. tax
 *   increases vs. debt accumulation).
 *
 * KEY AGENTS:
 *   - Municipal Residents: Primary victims (powerless/trapped) — place-bound; bear full burden through service cuts, infrastructure deterioration, tax pressure
 *   - Public Service Workers: Secondary victims (moderate/constrained) — dependent on municipal employment; face wage stagnation and pension uncertainty
 *   - Debt Creditors and Financial Institutions: Primary beneficiaries (institutional/arbitrage) — profit from municipal bond issuance and debt service; experience coordination benefit
 *   - Central Government / State Regulators: Secondary beneficiary (organized/constrained) — enforce fiscal discipline; consolidate power over local resource allocation; prevent cascading crises
 *   - Bond Rating Agencies: Institutional actors (institutional/arbitrage) — profit from managing municipal debt risk; maintain technical compliance regime
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing policy-contingent arrangements as fiscal arithmetic
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(municipal_fiscal_sustainability, 0.58).
domain_priors:suppression_score(municipal_fiscal_sustainability, 0.65).
domain_priors:theater_ratio(municipal_fiscal_sustainability, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(municipal_fiscal_sustainability, extractiveness, 0.58).
narrative_ontology:constraint_metric(municipal_fiscal_sustainability, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(municipal_fiscal_sustainability, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(municipal_fiscal_sustainability, tangled_rope).
narrative_ontology:human_readable(municipal_fiscal_sustainability, "Municipal Fiscal Sustainability Constraint").
narrative_ontology:topic_domain(municipal_fiscal_sustainability, "public_finance/local_governance").

domain_priors:requires_active_enforcement(municipal_fiscal_sustainability).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(municipal_fiscal_sustainability, debt_creditors).
narrative_ontology:constraint_beneficiary(municipal_fiscal_sustainability, central_government_regulators).
narrative_ontology:constraint_beneficiary(municipal_fiscal_sustainability, upper_tier_institutional_actors).
narrative_ontology:constraint_victim(municipal_fiscal_sustainability, municipal_residents).
narrative_ontology:constraint_victim(municipal_fiscal_sustainability, public_service_workers).
narrative_ontology:constraint_victim(municipal_fiscal_sustainability, future_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESIDENTS (SNARE) — Trapped within municipal jurisdiction; cannot exit without bearing relocation costs. Bear full burden of fiscal constraints (service cuts, tax increases, deteriorating infrastructure). Extracted from through structural necessity of place-boundedness. No meaningful coordination benefit — the constraint exists to extract, not to coordinate public goods provision.
constraint_indexing:constraint_classification(municipal_fiscal_sustainability, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: PUBLIC SERVICE WORKERS (TANGLED ROPE) — Constrained by labor market conditions and municipal employment dependency. The constraint provides genuine coordination: stable employment funding, pension coordination, service delivery infrastructure. But it also extracts: wage stagnation relative to private sector, pension obligation uncertainty, job insecurity driven by fiscal cycles. Mixed extraction and coordination — significant exit costs but some real benefit.
constraint_indexing:constraint_classification(municipal_fiscal_sustainability, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: DEBT CREDITORS (ROPE) — Municipal bonds and loans generate predictable returns; financial institutions benefit from municipal debt issuance. Experience the constraint as coordination: municipal debt markets function smoothly when fiscal rules are enforced. Arbitrage available (diversify into other markets). Net beneficiary — extraction runs toward creditors, not away. Low effective extraction from their perspective.
constraint_indexing:constraint_classification(municipal_fiscal_sustainability, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CENTRAL GOVERNMENT REGULATORS (TANGLED ROPE) — Organized power (institutional); constrained by political economy of local autonomy but exerts active enforcement through fiscal rules, audit requirements, and intervention triggers. Genuine coordination function: prevents fiscal crises that cascade to national level. But also extracts: consolidates power over local resource allocation, enforces austerity constraints on service delivery. Mixed experience — real coordination benefit plus asymmetric control.
constraint_indexing:constraint_classification(municipal_fiscal_sustainability, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RATING AGENCIES / TECHNICAL COMPLIANCE (PITON) — The formal fiscal sustainability metrics (debt-to-revenue ratios, fund balances, coverage ratios) have become largely performative. Municipalities manage to metrics rather than underlying fiscal health — budget gimmicks, accounting shifts, and deferred maintenance maintain compliance while structural deficits persist. The technical regime persists through inertia and institutional entrenchment, not because it effectively enforces solvency. Theater ratio dominates — the measurements matter more than what they measure.
constraint_indexing:constraint_classification(municipal_fiscal_sustainability, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FISCAL ARITHMETIC (MOUNTAIN) — From a civilizational/universal perspective, the constraint may appear as natural law: municipalities cannot persistently spend beyond revenue without borrowing constraints, and debt service eventually crowds out service delivery — fundamental arithmetic that cannot be negotiated away. However, the structural data reveals this as a false summit: the constraint's form is contingent on institutional design choices (revenue bases, intergovernmental transfers, borrowing rules, pension obligations). Different fiscal federalisms produce different constraints.
constraint_indexing:constraint_classification(municipal_fiscal_sustainability, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(municipal_fiscal_sustainability_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(municipal_fiscal_sustainability, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(municipal_fiscal_sustainability, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(municipal_fiscal_sustainability, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(municipal_fiscal_sustainability, TR),
    TR >= 0.70.

:- end_tests(municipal_fiscal_sustainability_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts substantially through forced service cuts and wage constraints on powerless and moderate-powered agents (residents and workers), but the extraction is not total because some genuine coordination function exists — municipal debt markets and intergovernmental fiscal relationships do require coordination. The increase from 0.32 to 0.58 over the interval reflects accumulated pressure from revenue base stagnation, unfunded mandate growth, and pension obligation accumulation. Suppression (0.65): High. Barriers to exit and solution options are substantial. Residents face place-boundedness costs; workers face labor market constraints; municipalities face legal borrowing limits and revenue base restrictions. The suppression does not reach snare levels (0.60+) alone because some options exist (service reallocation, tax increases, development incentives), but the option space is tightly constrained. Theater ratio (0.68): High. The formal fiscal metrics (debt-to-revenue ratios, fund balance targets, coverage ratios) have decoupled from structural fiscal health. Municipalities achieve metric compliance through one-time revenues, deferred maintenance, pension contribution changes, and accounting adjustments while underlying structural deficits persist. The rise from 0.42 to 0.68 reflects increasing reliance on metrics gaming as genuine fiscal balance becomes unattainable.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Residents experience extraction (snare) driven by absolute fiscal pressure — they have no exit option and bear the full burden. Central government regulators experience coordination (tangled rope) — fiscal rules prevent cascading crises and protect national financial stability, but also extract through power consolidation. Debt creditors experience pure coordination (rope) — municipal debt markets function smoothly when fiscal discipline is maintained, and they benefit without bearing extraction costs. Bond rating agencies experience a performative regime (piton) — the technical metrics persist through institutional inertia despite decoupling from structural reality. The piton classification reveals that institutional actors (creditors, rating agencies, regulators) have vested interest in maintaining the appearance of fiscal discipline even as the underlying mechanisms degrade. The mountain classification is a false summit — naturalizing fiscal constraints as mathematical necessity when they are contingent on institutional design choices (revenue bases, transfer mechanisms, borrowing rules, pension benefit structures).
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value reflects the agent's structural relationship to fiscal pressure. Residents and workers are net targets (d > 0.7) — trapped or constrained with no arbitrage option; extraction flows toward creditors and regulators. Creditors and rating agencies are net beneficiaries (d < 0.2) — arbitrage available; they profit from municipal borrowing without bearing its costs. Central government has mixed directionality (d ≈ 0.5) — genuine coordination function (preventing crises) plus extraction function (consolidating power). The tangled_rope classification requires both beneficiary and victim groups; this constraint meets that criterion: debt creditors and central government benefit; residents, workers, and future taxpayers bear costs. The active enforcement requirement is met through fiscal rules, audit mandates, and intervention triggers. The high theater ratio and increasing trajectory indicate piton-adjacent dynamics — the formal compliance regime is partially degraded, maintained through institutional inertia and creditor interest rather than functional necessity.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is RESOLVED through perspectival decomposition: all six types are legitimate readings of the constraint from different structural positions. No single type is 'correct' — the presheaf over observer positions is the full analysis. The mountain perspective is a false summit (naturalization of policy-contingent fiscal arithmetic). The snare perspective is the resident's genuine structural reality (trapped, extracted from, no coordination benefit). The rope perspective is the creditor's genuine structural reality (benefits from functioning municipal debt markets). The tangled_rope perspective from public workers and central government is the accurate intermediate classification (mixed coordination and extraction). The piton perspective reveals the degradation of formal compliance mechanisms. The constraint resolves the mandatrophy by showing that the same base properties (extractiveness 0.58, suppression 0.65, theater 0.68) produce six different legitimate classifications depending on the observer's power level, time horizon, exit options, and spatial scope. This is not a bug in the classification system — it is the system working correctly to show how the same structural fact (municipal fiscal pressure) is experienced as an immutable law, pure coordination, mixed coordination-extraction, pure extraction, performative ritual, or temporary problem, depending on structural position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    structural_revenue_elasticity,
    'Do municipal revenue bases (property tax, sales tax, income tax) have inherent inadequacy relative to service demand, or is the gap a policy choice?',
    'Comparative fiscal analysis across jurisdictions with similar demographics but different revenue structures; correlation between revenue base composition and service delivery sustainability',
    'If inherent inadequacy: constraint is mountain-adjacent (structural limit). If policy choice: constraint is snare disguised as natural necessity — the extraction mechanism is the assigned revenue base, not fiscal physics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(structural_revenue_elasticity, empirical, 'Whether revenue inadequacy is structural or policy-driven').

omega_variable(
    intergovernmental_transfer_dependency,
    'To what extent does fiscal pressure exist because municipalities lack revenue sources vs. because central governments deliberately constrain transfers or shift costs downward?',
    'Time-series analysis of transfer-to-revenue ratios; comparison with federal devolution policies; identification of cost-shifting mandates (unfunded liability transfers)',
    'If high cost-shifting: constraint is partially a tangled rope at inter-governmental level (central government extracts by forcing local service delivery costs). If structural gap: constraint is more mountain-like.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intergovernmental_transfer_dependency, empirical, 'Role of cost-shifting in fiscal sustainability pressure').

omega_variable(
    pension_obligation_backdoor_extraction,
    'Are unfunded pension liabilities a legitimate deferred compensation claim or a financial entrapment mechanism designed to drain municipal resources?',
    'Historical analysis of pension promise evolution; comparison of pension burden across similar jurisdictions; identification of design choices (calc methods, COLA provisions, assumption changes) that expanded obligations',
    'If legitimate deferred compensation: pension obligations are coordination (satisfying real worker claims). If backdoor extraction: obligations are a snare (workers are victims of overpromising that depletes general fund). Classification shifts from tangled_rope to piton+snare hybrid.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(pension_obligation_backdoor_extraction, empirical, 'Whether pensions represent legitimate compensation or financial entrapment').

omega_variable(
    metrics_gaming_prevalence,
    'What proportion of apparent fiscal sustainability compliance is achieved through genuine structural balance vs. accounting shifts, one-time revenues, or deferred obligations?',
    'Forensic budget analysis across sample of compliant municipalities; tracking of deferred maintenance backlogs, pension contribution patterns, fund balance accounting; correlation between reported compliance and structural indicators (service degradation, infrastructure decay)',
    'If >60% gaming: piton classification confirmed — theater dominates. If <20% gaming: theater_ratio should be lower and tangled_rope classification is empirically sound.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metrics_gaming_prevalence, empirical, 'Extent of metrics gaming in fiscal compliance').

omega_variable(
    resident_exit_mobility,
    'Are residents truly trapped (no feasible exit) or significantly constrained (exit possible at high cost)?',
    'Analysis of migration patterns relative to fiscal stress; cost-of-exit analysis (housing price differentials, relocation burden, job market factors); correlation between municipal fiscal condition and outmigration rates',
    'If truly trapped: snare classification from resident perspective is correct. If constrained: exit_options should be ''constrained'' not ''trapped'', lowering d and effective extraction experienced by residents.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(resident_exit_mobility, empirical, 'Degree of resident exit mobility vs. entrapment').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(municipal_fiscal_sustainability, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mfs_tr_t0, municipal_fiscal_sustainability, theater_ratio, 0, 0.42).
narrative_ontology:measurement(mfs_tr_t7, municipal_fiscal_sustainability, theater_ratio, 7, 0.58).
narrative_ontology:measurement(mfs_tr_t15, municipal_fiscal_sustainability, theater_ratio, 15, 0.68).
narrative_ontology:measurement(mfs_tr_t22, municipal_fiscal_sustainability, theater_ratio, 22, 0.75).

% Extraction over time
narrative_ontology:measurement(mfs_be_t0, municipal_fiscal_sustainability, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(mfs_be_t7, municipal_fiscal_sustainability, base_extractiveness, 7, 0.48).
narrative_ontology:measurement(mfs_be_t15, municipal_fiscal_sustainability, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(mfs_be_t22, municipal_fiscal_sustainability, base_extractiveness, 22, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(municipal_fiscal_sustainability, resource_allocation).
narrative_ontology:affects_constraint(municipal_fiscal_sustainability, public_pension_sustainability).
narrative_ontology:affects_constraint(municipal_fiscal_sustainability, intergovernmental_fiscal_federalism).
narrative_ontology:affects_constraint(municipal_fiscal_sustainability, municipal_infrastructure_decay).

% DUAL FORMULATION NOTE:
% Municipal fiscal sustainability decomposes into three structurally distinct constraints: (1) Revenue base adequacy (whether municipal revenues can match service demand without external transfer — ε ≈ 0.45, rope-like); (2) Pension obligation burden (unfunded liabilities — ε ≈ 0.72, snare-like for workers and residents); (3) Intergovernmental transfer mechanics (whether states/feds cost-shift or fund adequately — ε varies by jurisdiction). This story covers the aggregate constraint; decomposed stories track each mechanism separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(municipal_fiscal_sustainability, institutional, 0.52).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
