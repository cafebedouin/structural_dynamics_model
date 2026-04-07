% ============================================================================
% CONSTRAINT STORY: sotu_1962_kennedy_manpower_training_development_act
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1962_kennedy_manpower_training_development_act, []).

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
 *   constraint_id: sotu_1962_kennedy_manpower_training_development_act
 *   human_readable: Manpower Training and Development Act (1962) — Federal Retraining for Automation-Displaced Workers
 *   domain: labor/economic_policy
 *
 * SUMMARY:
 *   The Manpower Training and Development Act (1962) represents a specific
 *   institutional response to technological displacement: treating worker
 *   retraining as a federal public good rather than an individual burden or a
 *   pure private market problem. The constraint embeds labor market
 *   stabilization into the federal government's role, creating a coordination
 *   mechanism that prevents regional economic collapse while simultaneously
 *   extracting subsistence-level costs from workers during retraining and
 *   creating organizational dependencies on federal funding. The program's
 *   classification as Scaffold derives from its genuine coordination function
 *   (preventing unemployment-driven welfare state expansion, stabilizing
 *   regional economies, matching workers to viable labor markets) combined
 *   with explicit sunset logic embedded in its authorization structure.
 *   However, the rising theater_ratio and increasing base_extractiveness over
 *   the measured interval suggest that as automation displacement
 *   accelerates, the program's coordination function may degrade and its
 *   extraction function increase, transitioning toward Tangled Rope or Snare
 *   classification. The constraint is foundational to the 1960s-era federal
 *   response to technological change and sets precedent for how displacement
 *   risk is allocated between individuals, employers, and the state.
 *
 * KEY AGENTS:
 *   - Displaced Mill and Mine Workers: Primary beneficiary (powerless/trapped) — receive retraining and wage support during transition, but face subsistence stipends, geographic lock-in, and uncertain labor market absorption
 *   - Federal Budget Administration: Secondary beneficiary (institutional/arbitrage) — gains labor market stabilization, prevents welfare expansion, maintains consumer demand and regional stability
 *   - Regional Employers and Chambers of Commerce: Secondary beneficiary (institutional/arbitrage) — benefit from stable labor supply, avoid social unrest, receive workforce development subsidy
 *   - Federal Taxpayers: Primary victim (powerless/trapped in taxation system) — bear direct cost of retraining program through tax dollars
 *   - Labor Unions and Worker Organizations: Moderately constrained (moderate/constrained) — recognize retraining's benefits while organizing against underlying automation and capital mobility
 *   - Existing Vocational Education System: Institutional competitor (institutional/arbitrage) — loses prestige and enrollment as federal retraining provides parallel apparatus; degraded to Piton status
 *   - Analytical Observer: Civilizational analyst (analytical/analytical) — sees program as temporary institutional response to acceleration of displacement; expects sunset when displacement outpaces program capacity
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1962_kennedy_manpower_training_development_act, 0.35).
domain_priors:suppression_score(sotu_1962_kennedy_manpower_training_development_act, 0.45).
domain_priors:theater_ratio(sotu_1962_kennedy_manpower_training_development_act, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1962_kennedy_manpower_training_development_act, extractiveness, 0.35).
narrative_ontology:constraint_metric(sotu_1962_kennedy_manpower_training_development_act, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(sotu_1962_kennedy_manpower_training_development_act, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1962_kennedy_manpower_training_development_act, scaffold).
narrative_ontology:human_readable(sotu_1962_kennedy_manpower_training_development_act, "Manpower Training and Development Act (1962) — Federal Retraining for Automation-Displaced Workers").
narrative_ontology:topic_domain(sotu_1962_kennedy_manpower_training_development_act, "labor/economic_policy").

domain_priors:requires_active_enforcement(sotu_1962_kennedy_manpower_training_development_act).
narrative_ontology:has_sunset_clause(sotu_1962_kennedy_manpower_training_development_act).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1962_kennedy_manpower_training_development_act, displaced_workers).
narrative_ontology:constraint_beneficiary(sotu_1962_kennedy_manpower_training_development_act, federal_budget_administrators).
narrative_ontology:constraint_beneficiary(sotu_1962_kennedy_manpower_training_development_act, employers_avoiding_local_labor_crises).
narrative_ontology:constraint_victim(sotu_1962_kennedy_manpower_training_development_act, federal_taxpayers).
narrative_ontology:constraint_victim(sotu_1962_kennedy_manpower_training_development_act, competing_training_programs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISPLACED MILL WORKER (TANGLED ROPE) — Structurally trapped: skills are obsolete, relocation costs are prohibitive, alternative employment in the region is scarce. The retraining program offers genuine coordination (stable re-employment pathway) alongside extraction (retraining occurs at subsistence stipends; opportunity cost of lost wages during training; geographic lock-in to program location). Maximum suppression from structural position, but not a pure snare because the program genuinely solves the coordination problem of matching displaced workers to viable labor markets. Extraction is moderate because the worker retains agency in skill selection and the program addresses a real need.
constraint_indexing:constraint_classification(sotu_1962_kennedy_manpower_training_development_act, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: FEDERAL BUDGET ADMINISTRATION (ROPE) — Benefits from labor market stabilization: retraining prevents welfare-state explosion, maintains consumer demand, avoids regional economic collapse. Experiences the constraint as coordination with minimal extraction cost — the program is a legitimate public investment in social stability. Arbitrage option: can shift focus to other labor policies or reduce program scope. The administrative apparatus itself benefits from budget authority and organizational growth.
constraint_indexing:constraint_classification(sotu_1962_kennedy_manpower_training_development_act, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: REGIONAL EMPLOYERS (ROPE) — Benefit from stable labor supply and avoided social unrest in mill towns and mining regions. The retraining program prevents sudden unemployment shocks that would damage local business ecosystems. Experiences constraint as coordination: public investment in workforce development reduces private training burden. Arbitrage option: can lobby for program expansion or shift to private training contracts.
constraint_indexing:constraint_classification(sotu_1962_kennedy_manpower_training_development_act, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 4: LABOR UNIONS (SCAFFOLD) — Constrained by member displacement but also recognize retraining as temporary solution. See the program as a 1960s-era response to automation that will require more fundamental economic restructuring by next generation. Theater element high: unions must acknowledge retraining's real benefits while organizing against the underlying automation itself. Sunset logic: unions expect the program to mature into either (a) broader income support systems, or (b) stronger automation regulation limiting displacement. The program's value decays as a coordination mechanism when it becomes theater without preventing displacement.
constraint_indexing:constraint_classification(sotu_1962_kennedy_manpower_training_development_act, scaffold,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: EXISTING VOCATIONAL EDUCATION (PITON) — Degraded by the creation of parallel federal retraining apparatus. The MTDA creates separate institutional infrastructure rather than integrating with existing vocational schools, reducing efficiency and creating redundancy. Theater element: both systems must now justify their existence and funding separately. Original function (training young workers for initial entry) is preserved but institutional prestige declines as federal retraining is seen as more responsive to economic change. Maintenance continues through inertia and union protection, not functional necessity.
constraint_indexing:constraint_classification(sotu_1962_kennedy_manpower_training_development_act, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (SCAFFOLD) — From a generational view, the MTDA is a temporary institutional response to the acceleration of technological displacement. It treats automation-driven unemployment as a public responsibility (accurate diagnosis) but relies on retraining as the primary solution mechanism (asymptotically insufficient for accelerating displacement rates). The program has clear sunset logic: it will be superseded by either (a) more fundamental income support systems (UBI, job guarantees), or (b) regulation of automation pacing. Current extractiveness reflects the program's temporary adequacy; as displacement rates rise, the program's coordination function will degrade and extraction will increase. Measured extractiveness (0.35) reflects the program's current efficacy; projections show theater_ratio rising and base_extractiveness increasing as the program becomes saturated.
constraint_indexing:constraint_classification(sotu_1962_kennedy_manpower_training_development_act, scaffold,
    context(agent_power(analytical),
            time_horizon(generational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1962_kennedy_manpower_training_development_act_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1962_kennedy_manpower_training_development_act, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1962_kennedy_manpower_training_development_act, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1962_kennedy_manpower_training_development_act, TR),
    TR >= 0.70.

:- end_tests(sotu_1962_kennedy_manpower_training_development_act_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate, reflecting genuine coordination function alongside measurable extraction. The program genuinely solves the coordination problem of matching displaced workers to viable labor markets and prevents regional economic collapse. However, it extracts costs: retraining occurs at subsistence stipends (workers bear opportunity cost of lost wages during training); geographic lock-in (workers must locate to training sites); labor market absorption is imperfect (workers may accept lower-wage positions or underemployment). The extraction is not overwhelming because the program addresses a real need and offers genuine agency in skill selection. Theater ratio (0.52): Moderate-high, reflecting the program's mixed functional and performative elements. The functional element is genuine labor market matching. The performative element is high: the program must publicly demonstrate effectiveness (graduation rates, placement statistics) and justify ongoing federal budget authority. Suppression (0.45): Moderate, reflecting both structural barriers (displaced workers have few alternative options; retraining is compulsory for federal support eligibility) and the program's genuine reduction of barriers (removes cost obstacles to skills acquisition). The program's sunset clause indicates that suppression is expected to decline as automation rates stabilize or as alternative economic structures mature.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is profound and structurally important. Federal administrators and employers see the program as pure coordination (Rope) — a win-win investment in workforce development and regional stability. Displaced workers see the program as Tangled Rope — genuine coordination of their return to work alongside extraction of subsistence-level costs during retraining and imperfect labor market absorption. Labor unions see it as Scaffold — a temporary coordination mechanism that will be superseded by either stronger automation regulation or broader income support systems. The existing vocational education system sees it as Piton — a degraded, redundant institutional competitor maintained by federal prestige rather than functional necessity. The analytical observer sees it as a Scaffold with critical sunset logic: the program's classification will change as automation displacement rates accelerate. If displacement outpaces program capacity, the analytical perspective will transition to Tangled Rope or Snare, and the extraction mechanism (workers retrain but cannot find work) will become apparent to all perspectives. The gap between beneficiary and victim perspectives here is not about disagreement on facts but about different structural positions in the extraction flow.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective is derived from structural position relative to the extraction flow. Displaced workers are victims (high d): they bear suppression costs and subsistence-level extraction despite being nominal beneficiaries. Federal budget administrators are beneficiaries (low d): they gain labor market stabilization and organizational authority. Regional employers are beneficiaries (low d): they benefit from stable labor supply without bearing direct training costs. Federal taxpayers are victims (high d): they bear direct fiscal cost. Labor unions occupy mixed position (d ≈ 0.50): they benefit from stabilization while opposing the underlying displacement. The analytical observer sees the structural tension (d ≈ 0.60): displacement rate will eventually exceed program capacity, shifting the constraint from coordination to extraction. The directionality derivation is essential for understanding why the program appears as Rope from the federal perspective but as Tangled Rope from the worker perspective — the extraction flow is identical, but its direction relative to each actor differs.
 *
 * MANDATROPHY ANALYSIS:
 *   The MTDA resolves mandatrophy by explicitly embedding sunset logic into its classification. This is a Scaffold, not a Rope, precisely because its coordination function has a known terminal condition: it solves the problem of matching displaced workers to viable labor markets under specific conditions (moderate displacement rates, adequate regional labor market absorption capacity). The program's framers recognized that it was temporary — intended to manage the transition to automation-era labor markets, not to solve permanent technological displacement. The mandatrophy is resolved by measuring the program against its actual coordination function: does it prevent regional economic collapse and match workers to jobs? Yes. Will it continue to do so as displacement rates accelerate? No — at some point (estimated 10-20 years), either displacement rates will stabilize, or the program will be superseded by broader income support systems. The theater_ratio increase (0.40 → 0.52) indicates the program is developing performative elements as justification burden increases with scale. The base_extractiveness increase (0.25 → 0.35) reflects that workers are increasingly bearing subsistence-level costs and facing imperfect labor market absorption. The Scaffold classification holds as long as these metrics remain in range; if they diverge further (theater > 0.70, extractiveness > 0.45), the classification transitions to Tangled Rope or Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    retraining_labor_market_absorption_capacity,
    'What is the actual absorption capacity of regional labor markets for retraining graduates? Does retraining match displaced workers to jobs that exist, or does it create queuing for scarce positions?',
    'Longitudinal employment tracking of retraining graduates; comparison of post-program placement rates to regional job availability; wage trajectory analysis (do graduates re-enter at previous wages or accept lower-paying positions?)',
    'If absorption capacity is high (>75% placement at prior-wage-equivalent jobs): retraining functions as genuine coordination (Rope/Scaffold). If capacity is low (<50% or significant wage loss): retraining becomes credential recycling with moderate extraction (Tangled Rope from worker perspective). If capacity collapses as displacement accelerates: transition to Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(retraining_labor_market_absorption_capacity, empirical, 'Whether regional labor markets can absorb retraining graduates at adequate wages').

omega_variable(
    automation_displacement_rate_trajectory,
    'What is the rate of acceleration of automation-driven displacement relative to retraining program capacity? Will the program scale appropriately or become saturated?',
    'Quantitative analysis of displacement rates (jobs lost per year to automation) vs. program enrollment capacity and graduate absorption. Trend analysis over 5-10 year intervals.',
    'If displacement rate < program capacity: Scaffold classification holds (temporary solution with sunset as displacement stabilizes). If displacement rate > program capacity: program becomes theater-heavy Tangled Rope (workers retrain but still cannot find work). If displacement rate accelerates exponentially: Snare classification (workers internalize failure as personal rather than structural).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(automation_displacement_rate_trajectory, empirical, 'Whether automation displacement rate will outpace retraining program capacity').

omega_variable(
    federal_commitment_duration,
    'Is the sunset clause for MTDA real (program genuinely temporary until displaced) or rhetorical (program will persist indefinitely with shifting justifications)?',
    'Policy analysis of authorization structure (explicit sunset vs. indefinite annual appropriation); congressional debate records; evolution of program scope and eligibility over time.',
    'If real sunset: Scaffold classification confirmed. If rhetorical: program degrades to Piton (maintained by institutional inertia, theater increasingly high). If indefinite with expanded scope: may persist as Rope/Tangled Rope equilibrium.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(federal_commitment_duration, conceptual, 'Whether the sunset clause represents genuine temporary commitment or permanent institutional fixture').

omega_variable(
    automation_versus_structural_deindustrialization_mechanism,
    'Is worker displacement primarily driven by machine automation (technological substitution) or by capital mobility and deindustrialization (mills relocating to lower-cost regions)? These require different remedies.',
    'Comparative analysis of closed mill cases: mills with automation investment vs. mills that relocated entirely. Worker outcome differences between retraining (automation case) and structural relocation assistance (capital mobility case).',
    'If primarily automation: retraining addresses the root problem (Scaffold). If primarily capital flight: retraining is theater — workers cannot compete with globalized labor costs (Snare from worker perspective). If mixed: some workers genuinely need skills update, others need relocation assistance or structural economic transition (Tangled Rope).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(automation_versus_structural_deindustrialization_mechanism, empirical, 'Whether displacement is driven by automation or by capital mobility and deindustrialization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1962_kennedy_manpower_training_development_act, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mtda_tr_t0, sotu_1962_kennedy_manpower_training_development_act, theater_ratio, 0, 0.4).
narrative_ontology:measurement(mtda_tr_t2, sotu_1962_kennedy_manpower_training_development_act, theater_ratio, 2, 0.48).
narrative_ontology:measurement(mtda_tr_t5, sotu_1962_kennedy_manpower_training_development_act, theater_ratio, 5, 0.52).

% Extraction over time
narrative_ontology:measurement(mtda_be_t0, sotu_1962_kennedy_manpower_training_development_act, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(mtda_be_t2, sotu_1962_kennedy_manpower_training_development_act, base_extractiveness, 2, 0.3).
narrative_ontology:measurement(mtda_be_t5, sotu_1962_kennedy_manpower_training_development_act, base_extractiveness, 5, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1962_kennedy_manpower_training_development_act, resource_allocation).
narrative_ontology:affects_constraint(sotu_1962_kennedy_manpower_training_development_act, labor_market_displacement_from_automation).
narrative_ontology:affects_constraint(sotu_1962_kennedy_manpower_training_development_act, federal_budget_labor_policy_burden).
narrative_ontology:affects_constraint(sotu_1962_kennedy_manpower_training_development_act, regional_economic_stabilization_via_federal_investment).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
