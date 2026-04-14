% ============================================================================
% CONSTRAINT STORY: sotu_1947_truman_price_stability_tripartite_responsibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1947_truman_price_stability_tripartite_responsibility, []).

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
 *   constraint_id: sotu_1947_truman_price_stability_tripartite_responsibility
 *   human_readable: Truman's 1947 Tripartite Price Stability Mechanism
 *   domain: economic_policy/inflation_control
 *
 * SUMMARY:
 *   President Truman's 1947 State of the Union address articulates a
 *   tripartite price-stability mechanism allocating anti-inflation
 *   responsibility across labor, industry, and government. The constraint
 *   functions as a coordinated effort to prevent wage-price spirals that
 *   would erode fixed-income purchasing power (particularly for retirees,
 *   civil servants, and low-wage workers), while simultaneously constraining
 *   labor's negotiating power and industry's pricing freedom. The mechanism
 *   is not enforced through law (no price controls exist) but through
 *   sustained political rhetoric framing cooperation as patriotic duty and
 *   mutual interest. Structurally, the constraint exhibits all
 *   characteristics of a tangled_rope: genuine coordination function
 *   (stabilizing inflation expectations, protecting vulnerable groups)
 *   layered with asymmetric extraction (workers bear suppression of wage
 *   growth; marginal industry bears cost pressures; beneficiaries are
 *   fixed-income groups with no exit option). The constraint's theater_ratio
 *   rises over time (0.52 to 0.65) as inflation accelerates and compliance
 *   becomes increasingly rhetorical rather than substantive — by 1948-1950,
 *   when inflation resurges above 5% annually, the tripartite mechanism
 *   collapses as labor refuses further restraint.
 *
 * KEY AGENTS:
 *   - Wage Laborers (especially unionized industrial workers): Primary victims (powerless/trapped) — constrained from demanding wage increases despite inflation; trapped by employment dependence; no coordination benefit
 *   - Labor Union Leadership: Secondary victims (moderate/constrained) — face pressure from government tripartite appeals and from membership wage demands; career risk if they fail to extract concessions; some negotiating power retained
 *   - Fixed-Income Earners (retirees, civil servants): Primary beneficiaries (powerless/trapped) — benefit from prevented inflation; purchasing power protected; trapped in nominal contracts but experience no extraction
 *   - Large Industrial Firms (with government procurement access): Beneficiaries (powerful/arbitrage) — can recover costs through government contracts, tax incentives, exemptions; experience low extraction
 *   - Small-to-Medium Industrial Firms (without government access): Victims (powerful/arbitrage) — forced to absorb cost pressures while holding prices; arbitrage options exist but are unreliable
 *   - Government Executive (Truman Administration): Institutional enforcer (institutional/arbitrage) — exhorts compliance; lacks legal enforcement tools; relies on voluntary patriotic cooperation
 *   - Congressional Opposition (Republican fiscal conservatives): Organized constraint skeptics (organized/constrained) — see mechanism as temporary wartime vestige; frame sunset through monetary discipline
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1947_truman_price_stability_tripartite_responsibility, 0.52).
domain_priors:suppression_score(sotu_1947_truman_price_stability_tripartite_responsibility, 0.68).
domain_priors:theater_ratio(sotu_1947_truman_price_stability_tripartite_responsibility, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1947_truman_price_stability_tripartite_responsibility, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1947_truman_price_stability_tripartite_responsibility, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(sotu_1947_truman_price_stability_tripartite_responsibility, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1947_truman_price_stability_tripartite_responsibility, tangled_rope).
narrative_ontology:human_readable(sotu_1947_truman_price_stability_tripartite_responsibility, "Truman's 1947 Tripartite Price Stability Mechanism").
narrative_ontology:topic_domain(sotu_1947_truman_price_stability_tripartite_responsibility, "economic_policy/inflation_control").

domain_priors:requires_active_enforcement(sotu_1947_truman_price_stability_tripartite_responsibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1947_truman_price_stability_tripartite_responsibility, fixed_income_earners).
narrative_ontology:constraint_beneficiary(sotu_1947_truman_price_stability_tripartite_responsibility, wage_workers_general).
narrative_ontology:constraint_victim(sotu_1947_truman_price_stability_tripartite_responsibility, business_profit_margins).
narrative_ontology:constraint_victim(sotu_1947_truman_price_stability_tripartite_responsibility, labor_negotiating_power).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: WAGE LABORER (SNARE) — Trapped in the constraint mechanism. Pressured to refrain from wage increases despite inflation erosion. Cannot exit without losing employment or accepting lower purchasing power. Government rhetoric frames wage restraint as patriotic duty while industry preserves profit margins. Maximum suppression: workers cannot strike for inflation-indexed raises without being labeled 'inflationary.' No coordination benefit accrues to them — the benefit flows to fixed-income groups and capital.
constraint_indexing:constraint_classification(sotu_1947_truman_price_stability_tripartite_responsibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: LABOR UNION LEADERSHIP (TANGLED ROPE) — Constrained by both the tripartite framework (pressure to endorse wage restraint) and membership pressure (workers demand nominal wage increases). Union leadership faces career risk (replacement by militant rank-and-file) if they fully comply with government appeals. Moderate extraction because unions retain negotiating power and can extract concessions in non-wage terms (benefits, work rules). Coordination function exists: unions help stabilize labor markets, preventing bidding wars that would accelerate inflation. Active enforcement required: government must consistently pressure unions while industry threatens closure if wage demands persist.
constraint_indexing:constraint_classification(sotu_1947_truman_price_stability_tripartite_responsibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INDUSTRIAL SECTOR / NOMINAL COMPLIANCE (ROPE) — From the position of a firm that can achieve high-volume production, the constraint functions as coordination. Government commitment to high-volume procurement and tax incentives reduces uncertainty; labor restraint prevents wage spirals that would force price increases. Beneficiary: arbitrage options exist (can pass through costs to consumers via government procurement, can exit through conversion to civilian production). Low experienced extraction because the constraint aligns with profit-maximizing strategy during postwar demand boom. Theater here is low — industrial compliance with price-holding is genuine functional coordination.
constraint_indexing:constraint_classification(sotu_1947_truman_price_stability_tripartite_responsibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INDUSTRIAL SECTOR / MARGIN CONSTRAINT (SNARE) — From the position of a firm with limited profit margins or facing real cost pressures, the constraint is extractive. Required to hold prices despite wage inflation and input cost increases. Government exhorts restraint but does not guarantee cost recovery. Exit option (arbitrage) exists for large firms through government channels (lobbying, procurement negotiations, cost-plus contracting exceptions), but is unavailable to small-to-medium producers. These firms experience the constraint as pure extraction: suppressed pricing while costs rise. Effective extraction χ is high for this cohort because they have power to organize (trade associations) but limited individual exit — trapped at the group level despite powerful status.
constraint_indexing:constraint_classification(sotu_1947_truman_price_stability_tripartite_responsibility, snare,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FIXED-INCOME EARNER / RETIREE (ROPE) — Trapped in nominal income contracts. Benefits directly from the constraint: prevented inflation protects purchasing power. No exit option (cannot renegotiate pension) but also experiences no extraction — the constraint subsidizes this agent by forcing others to bear inflation costs. This perspective shows the core coordination function: distributes inflation burden from powerless fixed-income groups to capital and labor negotiators. The mechanism works as pure coordination from this viewpoint because the beneficiary cannot defect.
constraint_indexing:constraint_classification(sotu_1947_truman_price_stability_tripartite_responsibility, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 6: FISCAL CONSERVATIVE COALITION (SCAFFOLD) — Organized political agents (Eisenhower Republicans, deficit hawks) see the tripartite mechanism as a temporary fix with a designed sunset. If inflation stays low and productivity gains justify modest wage growth, the constraint loses its necessity — workers can gain wages from productivity, not from nominal wage demands; industry can invest instead of defending margins. The sunset is 5-10 years: either inflation stabilizes and the constraint becomes unnecessary, or inflation returns and the mechanism fails (labor revolts, capital demands price freedom). Theater is moderate (61%) because the mechanism requires continuous political legitimation — if the public stops believing the tripartite appeals, enforcement collapses.
constraint_indexing:constraint_classification(sotu_1947_truman_price_stability_tripartite_responsibility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: DEMOCRATIC CONSENSUS FRAMEWORK (PITON) — By civilizational timescale, the tripartite mechanism is a degraded vestige of wartime price controls. The functional coordination role (stabilizing inflation expectations) could be achieved through monetary policy, fiscal discipline, or coordinated wage-price agreements with explicit cost-of-living adjusters. Instead, the mechanism persists through rhetorical appeals to patriotic duty and tripartite cooperation. Theater is high (61%): most of the constraint's force comes from public legitimacy narratives rather than structural enforcement (no legal price controls exist; compliance is voluntary with social pressure). Once inflation returns or labor solidarity breaks, the theater collapses and the mechanism is revealed as inertial — maintained because alternatives haven't fully displaced it, not because it functions.
constraint_indexing:constraint_classification(sotu_1947_truman_price_stability_tripartite_responsibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER (TANGLED ROPE) — Structural decomposition reveals genuine coordination (distributing inflation risk, stabilizing expectations) layered with asymmetric extraction (workers bear disproportionate restraint cost; small-to-medium capital forced to absorb cost pressure). The mechanism is not a natural law (could use monetary policy instead) and not pure coordination (benefits concentrate on fixed-income groups while labor bears suppression). The tripartite framing obscures that the mechanism works only if labor accepts restraint indefinitely — at the first moment when inflation accelerates or labor power increases, the system ruptures. Analytical classification is tangled_rope because both coordination and extraction are genuine structural features, not rhetorical overlays.
constraint_indexing:constraint_classification(sotu_1947_truman_price_stability_tripartite_responsibility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1947_truman_price_stability_tripartite_responsibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1947_truman_price_stability_tripartite_responsibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1947_truman_price_stability_tripartite_responsibility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1947_truman_price_stability_tripartite_responsibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1947_truman_price_stability_tripartite_responsibility, TR),
    TR >= 0.70.

:- end_tests(sotu_1947_truman_price_stability_tripartite_responsibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint imposes material costs on labor (suppressed wages during inflationary period) and on marginal industry (price-holding despite rising costs) while providing benefits to fixed-income groups (inflation protection) and large capital (stable expectations, government support). The value reflects that extraction is genuine but not total — labor retains some bargaining power through union coordination; industry can lobby for exemptions; government can claim this is mutual sacrifice. Rising from 0.35 to 0.58 over the interval reflects that as inflation accelerates (and hidden inflation erodes real wages despite nominal restraint), workers bear increasing suppression cost while the coordination benefit (keeping inflation moderate) becomes less certain. Suppression (0.68): Moderately high. Labor faces suppression from: (1) government patriotic appeals framing wage demands as inflationary treason, (2) employer threats to accelerate labor-shedding if wages rise, (3) union leadership internal pressure to maintain tripartite credibility, (4) social stigma against 'selfish' wage demands. But suppression is not total because unions retain strike capacity and can extract concessions in non-wage terms. Industry faces suppression through government moral suasion and threat of investigation or procurement penalty. Theater ratio (0.61): Moderate-high, rising from 0.52 to 0.65. Tripartite appeals are partially performative — most of the constraint's force comes from rhetoric about 'shared sacrifice' and 'national duty' rather than from structural enforcement mechanisms. Government has no legal price controls (unlike wartime OPA) and relies on voluntary compliance. As inflation persists, the theater becomes more visible: firms claim price-holding while quietly restructuring labor; workers comply with union restraint while experiencing real wage erosion; government exhorts sacrifice while maintaining deficit spending. The rising theater_ratio reflects that by 1948-1950, the mechanism is maintained increasingly through ritual incantation rather than material enforcement.
 *
 * PERSPECTIVAL GAP:
 *   This constraint produces stark perspectival divergence. The fixed-income earner sees pure coordination (rope) — the constraint protects them without imposing costs. The wage laborer sees pure extraction (snare) — wage suppression without coordination benefit accrues to them. The large industrial firm sees coordination (rope) — the constraint aligns with profit maximization during demand boom. The marginal firm sees extraction (snare) — forced cost absorption without government support. Labor union leadership sees tangled_rope (mixed coordination and extraction) — they coordinate labor supply (preventing bids-down of wages) while extracting costs from members (wage growth suppression). The analytical observer sees tangled_rope because both coordination (inflation stabilization) and extraction (asymmetric burden allocation) are genuine structural features. The Democratic consensus framework sees piton (degraded ritual) because the mechanism relies on voluntary patriotic compliance that erodes as inflation persists. This divergence reflects that the mechanism works only as long as all parties perceive it as beneficial — the moment labor power increases or inflation resurges, the perspectival alignment breaks and the constraint ruptures.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) is derived from beneficiary/victim status and exit options. Fixed-income earners are pure beneficiaries with trapped exit (cannot renegotiate pensions) — derives to d ≈ 0.05 (full beneficiary), producing f(d) ≈ -0.12, negative χ (constraint subsidizes them). Wage laborers are pure victims with trapped exit — derives to d ≈ 0.95 (full target), producing f(d) ≈ 1.42 (powerless), high χ. Large industrial beneficiaries with arbitrage exit derive to d ≈ 0.15 (beneficiary with options), producing f(d) ≈ -0.01, near-zero χ (low experienced extraction). Marginal industrial victims with constrained arbitrage exit derive to d ≈ 0.70 (victim with limited options), producing f(d) ≈ 1.05 (moderate), elevated χ. Labor union leadership occupies d ≈ 0.50 (symmetric — both victims and local beneficiaries as coordinators), producing f(d) ≈ 0.65 (moderate), the basis for tangled_rope. Government institutional power derives to d ≈ 0.48 (symmetric institutional position), producing f(d) ≈ 0.60 (institutional canonical). The scope modifier σ(S) = 1.0 for national scope (baseline). Suppression (0.68) is unscaled — it is a raw structural property of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through perspectival alignment analysis. The base properties (ε=0.52, χ at moderate levels across most perspectives) do not uniquely determine type — the constraint exhibits characteristics of tangled_rope (coordination + extraction), snare (from labor perspective), rope (from fixed-income perspective), and scaffold (sunset logic). Mandatrophy is resolved by recognizing that this is NOT a false disambiguation problem where multiple types are all equally correct. Instead, it is a perspectival fission problem: the constraint is stable as tangled_rope ONLY IF all parties perceive mixed benefit (coordination offsetting extraction). Once labor experiences successive years of real wage erosion despite nominal wage restraint (inflation acceleration without wage growth), the labor perspective shifts from tangled_rope to snare (extraction without coordination benefit). This shift is not reclassification within a single perspective — it is detection of the constraint's true underlying structure: the coordination function (inflation stabilization) depends on voluntary labor restraint, and when labor no longer perceives this as coordinating (because inflation returns despite restraint), the constraint ruptures. The mandatrophy is resolved by noting that the base_properties claim tangled_rope, but this is perspectivally contingent on labor perception. The analytical observer's tangled_rope classification is robust (sees both functions regardless of what labor perceives), so tangled_rope is the stable type. The snare classification (from labor at biographical horizon) predicts the constraint's collapse when labor perception shifts, which is exactly what happens in 1948-1950.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    productivity_wage_decoupling_threshold,
    'At what productivity growth rate does wage restraint become unjustifiable even under tripartite logic?',
    'Historical comparison of productivity gains vs nominal wage growth year-over-year; stakeholder surveys on perceived fairness thresholds',
    'If productivity exceeds 2% annually but wages are constrained to <1%: labor perspective shifts from rope to snare by biographical horizon. If productivity equals wage growth: mechanism can persist. This determines whether the scaffold sunset is realistic or wishful.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(productivity_wage_decoupling_threshold, empirical, 'Productivity threshold for wage restraint legitimacy').

omega_variable(
    capital_profit_preservation_mechanism,
    'Is industry price-holding genuinely reflecting restraint or disguised profit-preservation through cost-cutting that shifts burden to suppliers and workers?',
    'Longitudinal accounting data: profit margins by firm size; labor intensity changes; supply chain pressure metrics; ratio of price-holding to labor-shedding',
    'If price-holding masks labor rationalization: extraction is higher than measured (workers bear both wage suppression AND employment risk). If price-holding involves genuine margin compression: mechanism is more equitable than apparent. This determines whether the industrial sector''s snare classification is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capital_profit_preservation_mechanism, empirical, 'Whether price-holding masks labor rationalization').

omega_variable(
    labor_movement_coalition_stability,
    'What is the critical inflation threshold at which labor coalition coherence breaks and wildcat strikes override union leadership tripartite commitments?',
    'Historical strike data correlation with inflation rates; union leadership turnover frequency during periods of constraint; rank-and-file mobilization patterns',
    'If threshold is <3% annual inflation: constraint likely persists. If threshold is >5%: mechanism is unstable and collapse is probable within 2-3 years of higher inflation. This determines the actual sunset horizon for the scaffold perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_movement_coalition_stability, empirical, 'Inflation threshold for labor coalition collapse').

omega_variable(
    government_enforcement_capacity_limits,
    'What enforcement mechanisms does Truman possess (short of wage-price controls) to sustain tripartite compliance, and how durable are they?',
    'Inventory of executive tools: procurement pressure, tax incentives, public exhortation, threat of investigation. Assess durability as political winds shift (Republican Congress, Cold War demands).',
    'If enforcement relies primarily on voluntary compliance and patriotic framing: mechanism collapses when political legitimacy erodes (which it did by 1948-1950 when inflation returned). If legal tools exist: more durable. This determines whether piton classification (degraded ritual) is premature or prescient.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(government_enforcement_capacity_limits, empirical, 'Government enforcement mechanisms and their durability').

omega_variable(
    false_summit_risk_natural_inflation,
    'Is the tripartite mechanism presented as a response to inherent economic forces (immutable inflation), or as a contingent policy choice?',
    'Discourse analysis of Truman rhetoric: frequency of naturalizing language (''inflation is inevitable,'' ''price pressures are beyond control'') vs. political-choice language (''we must decide together''). Comparison to monetary policy alternatives available but not pursued.',
    'If framing is naturalized: workers more likely to accept restraint as natural law. If framing is political: resistance is more likely. This determines whether any observer is at risk of false summit misclassification (seeing policy choice as natural law).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(false_summit_risk_natural_inflation, conceptual, 'Naturalization of inflation as inevitable vs. policy choice').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1947_truman_price_stability_tripartite_responsibility, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(truman_1947_theater_t0, sotu_1947_truman_price_stability_tripartite_responsibility, theater_ratio, 0, 0.52).
narrative_ontology:measurement(truman_1947_theater_t1, sotu_1947_truman_price_stability_tripartite_responsibility, theater_ratio, 1, 0.57).
narrative_ontology:measurement(truman_1947_theater_t2, sotu_1947_truman_price_stability_tripartite_responsibility, theater_ratio, 2, 0.61).
narrative_ontology:measurement(truman_1947_theater_t3, sotu_1947_truman_price_stability_tripartite_responsibility, theater_ratio, 3, 0.65).

% Extraction over time
narrative_ontology:measurement(truman_1947_extract_t0, sotu_1947_truman_price_stability_tripartite_responsibility, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(truman_1947_extract_t1, sotu_1947_truman_price_stability_tripartite_responsibility, base_extractiveness, 1, 0.48).
narrative_ontology:measurement(truman_1947_extract_t2, sotu_1947_truman_price_stability_tripartite_responsibility, base_extractiveness, 2, 0.52).
narrative_ontology:measurement(truman_1947_extract_t3, sotu_1947_truman_price_stability_tripartite_responsibility, base_extractiveness, 3, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1947_truman_price_stability_tripartite_responsibility, resource_allocation).
narrative_ontology:boltzmann_floor_override(sotu_1947_truman_price_stability_tripartite_responsibility, 0.12).
narrative_ontology:affects_constraint(sotu_1947_truman_price_stability_tripartite_responsibility, taft_hartley_labor_restriction).
narrative_ontology:affects_constraint(sotu_1947_truman_price_stability_tripartite_responsibility, postwar_price_control_dissolution).
narrative_ontology:affects_constraint(sotu_1947_truman_price_stability_tripartite_responsibility, federal_reserve_inflation_mandate).

% DUAL FORMULATION NOTE:
% The tripartite mechanism is downstream of wartime price control infrastructure (OPA) and upstream of explicit legislative labor restriction (Taft-Hartley 1947). The mechanism also structures the implicit bargain with Federal Reserve independence: labor and capital accept inflation restraint through moral suasion in exchange for Fed maintaining low-rate environment. These stories together form an institutional response to postwar inflation expectations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1947_truman_price_stability_tripartite_responsibility, powerful, 0.7).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
