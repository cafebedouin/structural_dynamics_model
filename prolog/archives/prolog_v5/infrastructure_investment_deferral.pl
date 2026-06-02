% ============================================================================
% CONSTRAINT STORY: infrastructure_investment_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_infrastructure_investment_deferral, []).

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
 *   constraint_id: infrastructure_investment_deferral
 *   human_readable: Infrastructure Investment Deferral
 *   domain: public_policy/fiscal_management
 *
 * SUMMARY:
 *   Infrastructure investment deferral is a structural constraint on public
 *   budgeting where immediate fiscal pressures create systematic
 *   underinvestment in asset maintenance and replacement, displacing costs to
 *   future periods while preserving short-term budget balances. This
 *   constraint exhibits tangled coordination-extraction properties: there is
 *   a genuine coordination function (managing scarce fiscal resources across
 *   competing priorities), but this is paired with asymmetric extraction
 *   where current budget actors benefit at the expense of future taxpayers
 *   and current users who experience service degradation. The constraint is
 *   enforced through political economy mechanisms: elected officials face
 *   short planning horizons; constituencies reward fiscal restraint in the
 *   present; and liability shifting to future administrations is politically
 *   costless. The extractiveness trajectory shows the characteristic pattern
 *   of deferred maintenance: costs rise exponentially as infrastructure ages
 *   and emergency repairs replace preventive maintenance (initial ε=0.32
 *   rises to ε=0.72 by generation+30). Theater ratio increases as the gap
 *   between acknowledged maintenance needs (condition assessments,
 *   engineering reports) and actual spending widens, creating performative
 *   needs-documentation with minimal functional impact.
 *
 * KEY AGENTS:
 *   - Current Taxpayers: Primary beneficiary (institutional/arbitrage) — immediate tax burden reduction; can shift costs to future administrations
 *   - Future Generations/Taxpayers: Primary victim (powerless/trapped) — inherit deferred costs at 3-5x preventive maintenance expense; no exit option
 *   - Current Infrastructure Users: Secondary victim (moderate/constrained) — benefit from functional systems short-term but face increasing service disruptions, safety risks, congestion; geographically trapped
 *   - Fiscal Conservative Political Faction: Institutional beneficiary (institutional/arbitrage) — deferral aligns with cost-control narrative and enables reallocation to preferred programs
 *   - Public Finance Reform Coalition: Organized agents (organized/constrained) — pursue alternative funding mechanisms (infrastructure banks, dedicated revenue streams, bonds) as exits from the deferral trap
 *   - Infrastructure Maintenance Bureaucracy: Institutional actor (institutional/arbitrage) — produces assessment theater; maintains organizational form despite degraded functional authority
 *   - Construction and Engineering Sectors: Powerful extractors (powerful/mobile) — extract premium pricing during crisis spending while suffering contract reduction during deferral phases; can relocate to better-funded jurisdictions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(infrastructure_investment_deferral, 0.58).
domain_priors:suppression_score(infrastructure_investment_deferral, 0.65).
domain_priors:theater_ratio(infrastructure_investment_deferral, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(infrastructure_investment_deferral, extractiveness, 0.58).
narrative_ontology:constraint_metric(infrastructure_investment_deferral, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(infrastructure_investment_deferral, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(infrastructure_investment_deferral, tangled_rope).
narrative_ontology:human_readable(infrastructure_investment_deferral, "Infrastructure Investment Deferral").
narrative_ontology:topic_domain(infrastructure_investment_deferral, "public_policy/fiscal_management").

domain_priors:requires_active_enforcement(infrastructure_investment_deferral).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(infrastructure_investment_deferral, current_taxpayers).
narrative_ontology:constraint_beneficiary(infrastructure_investment_deferral, fiscal_conservatives).
narrative_ontology:constraint_beneficiary(infrastructure_investment_deferral, short_term_budget_managers).
narrative_ontology:constraint_victim(infrastructure_investment_deferral, future_taxpayers).
narrative_ontology:constraint_victim(infrastructure_investment_deferral, infrastructure_users).
narrative_ontology:constraint_victim(infrastructure_investment_deferral, public_asset_maintenance).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE GENERATIONS (SNARE) — Cannot exit the deferred maintenance obligation; bears compounding costs as infrastructure degrades. Younger cohorts inherit crumbling bridges, aging water systems, and deteriorating transit networks with no alternative but to fund emergency repairs at 3-5x preventive maintenance costs. Maximum extraction through temporal displacement.
constraint_indexing:constraint_classification(infrastructure_investment_deferral, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CURRENT INFRASTRUCTURE USERS (TANGLED ROPE) — Benefit from maintained roads, bridges, and transit systems in the near term while deferral is occurring, but face increasing congestion, safety risks, and service disruptions as infrastructure ages. Constrained by geographic dependency and lack of private alternatives in most contexts. Mixed extraction and coordination.
constraint_indexing:constraint_classification(infrastructure_investment_deferral, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FISCAL CONSERVATIVE POLITICAL FACTION (ROPE) — Experiences deferral as pure coordination: reduces immediate tax burden, aligns with cost-control narrative, enables reallocation to preferred spending categories. Arbitrage exit: can shift blame to future administrations or external shocks. Net beneficiary with genuine political benefit.
constraint_indexing:constraint_classification(infrastructure_investment_deferral, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: PUBLIC FINANCE REFORM COALITION (SCAFFOLD) — Infrastructure banking, dedicated revenue streams, and long-term capital plans represent organized attempts to exit the deferral trap. Infrastructure bonds, public-private partnerships, and bipartisan infrastructure bills (when they occur) create temporary constraints with sunset logic: as alternative funding mechanisms mature, the need for deferral diminishes. High agency despite constraint.
constraint_indexing:constraint_classification(infrastructure_investment_deferral, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: INFRASTRUCTURE MAINTENANCE BUREAUCRACY (PITON) — Performs the ritual of asset management (condition assessments, inspection reports, maintenance schedules) while lacking authority to enforce spending. The bureaucracy persists through institutional inertia, producing theater (extensive needs assessments that are then ignored) despite degraded function. Theater ratio high; actual coordination function minimal.
constraint_indexing:constraint_classification(infrastructure_investment_deferral, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: CONSTRUCTION AND ENGINEERING SECTORS (TANGLED ROPE) — Experience deferral as mixed extraction and benefit. Suffer from reduced maintenance contracts and slower project timelines during deferral, but benefit from the eventual emergency repairs and crisis-driven spending spikes that command premium pricing. Extraction masked as volatility; mobile because they can relocate to better-funded jurisdictions.
constraint_indexing:constraint_classification(infrastructure_investment_deferral, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / ACCOUNTING IDENTITY VIEW (MOUNTAIN) — From a civilizational perspective, infrastructure deferral appears as an immutable accounting law: the costs of deferred maintenance must eventually be paid; they cannot be eliminated, only displaced in time. The present value of infrastructure replacement is fixed; the choice is between distributed preventive investment or concentrated emergency replacement. This perspective risks naturalizing what is actually a political choice (whether to defer) as a physical necessity.
constraint_indexing:constraint_classification(infrastructure_investment_deferral, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(infrastructure_investment_deferral_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(infrastructure_investment_deferral, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(infrastructure_investment_deferral, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(infrastructure_investment_deferral, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(infrastructure_investment_deferral, TR),
    TR >= 0.70.

:- end_tests(infrastructure_investment_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint systematically privileges current budget managers over future budget managers; this is extractive but not maximal because the extraction mechanism is structural (time-shifted costs) rather than wasteful (pure rent capture). The beneficiaries genuinely experience reduced fiscal pressure; the victims genuinely bear future costs. As the 30-year trajectory shows, extractiveness increases as deferred costs compound — what appears as 32% extraction in year 0 becomes 72% extraction by year 30. Suppression (0.65): High. Barriers to exit include: political economy constraints (short election cycles), institutional fragmentation (current and future administrations are structurally separate agents), and epistemic closure (future costs are discounted/invisible in current budgets). Victims lack organized pressure mechanisms. Theater ratio (0.68): Moderate-high. Extensive infrastructure needs assessments, condition reports, and maintenance schedules are produced but systematically ignored in budget allocation. The ritual of needs documentation substitutes for actual spending. As extractiveness rises, theater increases (gap between acknowledged needs and funded work widens), eventually reaching piton-range values.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates a temporal perspectival gap: the same institutional arrangements classify as Rope (beneficial coordination) from the current fiscal manager's position but as Snare (pure extraction) from the future taxpayer's position. The current generation experiences genuine budget coordination benefit (scarce resources allocated across priorities); future generations experience only cost escalation with no offsetting benefit. The analytical observer's Mountain perspective risks naturalizing what is a political choice (when to defer, how much to extract) as an accounting identity (costs must eventually be paid). The construction sector's Tangled Rope reflects their mixed position: extract premium pricing during crises while losing revenue during deferral phases. The infrastructure bureaucracy's Piton reflects degraded function (assessments produce no spending) masked by institutional persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each agent reflects their structural position relative to the deferral mechanism. Current taxpayers and fiscal conservatives have low d (0.10-0.25) — they are beneficiaries with mobile exit options; the constraint extracts FROM future agents toward them. Future taxpayers have high d (0.90-0.95) — they are trapped victims with no exit; the constraint extracts maximum value from them. Current infrastructure users have moderate-high d (0.70-0.80) — they experience service degradation (extraction) while benefiting from functional systems (coordination), placing them in the tangled rope middle. Construction sectors have moderate d (0.50-0.60) — they both extract during crisis spending and lose revenue during deferral, creating ambiguous directionality that resolves through mobile exit options (they can relocate). The bureaucracy has low d (0.20-0.30) as an institutional beneficiary (maintains organizational form through theater) despite degraded function.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate puzzle here is: 'Is deferral a Rope (pure coordination failure) or a Snare (pure extraction mechanism)?' The mandatrophy resolves by showing both are partially correct but perspectival. From the current budget manager's view, deferral IS a coordination mechanism — it solves the immediate problem of scarce fiscal resources by temporal shifting. From the future taxpayer's view, it IS extraction — costs are imposed without benefit or consent. The Tangled Rope classification at the moderate/constrained level captures the hybrid: the constraint both coordinates (allocates scarce resources) and extracts (asymmetrically distributes costs). The mandatrophy is resolved not by choosing one type but by mapping the perspectival landscape: budget actors see coordination; future agents see extraction; the constraint contains both. The theater ratio increase (0.52 → 0.75) signals degradation: as the gap between acknowledged needs and funded work widens, the constraint shifts from functional tangled rope toward piton (performative mechanism with inertial persistence).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deferral_vs_deliberate_disinvestment,
    'Is infrastructure deferral a temporary fiscal constraint (Scaffold-capable exit) or deliberate disinvestment ideology (Snare-locked extraction)?',
    'Analysis of political rhetoric and budget documents: does leadership acknowledge deferral as temporary problem or frame aging infrastructure as acceptable? Do alternative funding mechanisms (bonds, dedicated streams, public-private partnerships) get actively pursued or dismissed?',
    'If temporary constraint: scaffold perspective valid, sunset mechanisms plausible. If ideological: snare and piton perspectives dominate; extraction is self-reinforcing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(deferral_vs_deliberate_disinvestment, conceptual, 'Whether deferral is temporary fiscal constraint or ideological disinvestment').

omega_variable(
    catastrophic_failure_threshold,
    'What level of infrastructure degradation triggers forced spending and breaks the deferral lock?',
    'Empirical tracking of infrastructure condition metrics (bridge sufficiency ratings, water main breaks, road pavement condition) and correlation with budget allocation changes. Identification of crisis-spending thresholds in historical data.',
    'If threshold is low: deferral mechanism becomes unstable (Scaffold with near-term sunset). If threshold is high: generations of cost escalation are possible (Snare deepens).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(catastrophic_failure_threshold, empirical, 'Infrastructure degradation threshold triggering forced spending').

omega_variable(
    extraction_vector_ambiguity,
    'Does deferral primarily extract from future taxpayers (temporal extraction) or from current infrastructure users (service degradation extraction)?',
    'Decomposition of costs: service disruption and safety costs borne now vs. capitalized deferral costs borne by future budgets. Measurement of current service quality decline vs. deferred liability accumulation.',
    'If temporal extraction dominant: Snare classification from future perspective correct. If service degradation dominant: Tangled Rope from current user perspective is primary structural fact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vector_ambiguity, empirical, 'Whether extraction targets future cohorts or current users').

omega_variable(
    political_cycle_entrenchment,
    'Does the 4-year political budget cycle institutionalize deferral as a structural feature independent of fiscal ideology?',
    'Comparative analysis across jurisdictions with different political cultures: do all show deferral pattern regardless of local fiscal ideology? Do jurisdictions with longer-term budget horizons (20+ year planning) show different deferral patterns?',
    'If political cycle dominant: deferral is Piton (institutional inertia, not ideology). If ideological: deferral reflects chosen values. Structural remedies differ sharply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_cycle_entrenchment, empirical, 'Political cycle entrenchment of deferral pattern').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(infrastructure_investment_deferral, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(infra_tr_t0, infrastructure_investment_deferral, theater_ratio, 0, 0.52).
narrative_ontology:measurement(infra_tr_t10, infrastructure_investment_deferral, theater_ratio, 10, 0.61).
narrative_ontology:measurement(infra_tr_t20, infrastructure_investment_deferral, theater_ratio, 20, 0.68).
narrative_ontology:measurement(infra_tr_t30, infrastructure_investment_deferral, theater_ratio, 30, 0.75).

% Extraction over time
narrative_ontology:measurement(infra_be_t0, infrastructure_investment_deferral, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(infra_be_t10, infrastructure_investment_deferral, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(infra_be_t20, infrastructure_investment_deferral, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(infra_be_t30, infrastructure_investment_deferral, base_extractiveness, 30, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(infrastructure_investment_deferral, resource_allocation).
narrative_ontology:affects_constraint(infrastructure_investment_deferral, public_asset_maintenance_crisis).
narrative_ontology:affects_constraint(infrastructure_investment_deferral, intergenerational_fiscal_burden_transfer).
narrative_ontology:affects_constraint(infrastructure_investment_deferral, political_myopia_structural).

% DUAL FORMULATION NOTE:
% Infrastructure deferral is upstream of specific asset-class crises (bridge failures, water system contamination, transit breakdown) but represents a distinct structural constraint at the budget allocation level. Decomposition: treat deferral mechanism (ε=0.58, tangled rope) separately from domain-specific crises (each with their own ε values). All are linked via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(infrastructure_investment_deferral, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
