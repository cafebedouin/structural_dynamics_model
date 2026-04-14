% ============================================================================
% CONSTRAINT STORY: sotu_1957_eisenhower_wage_productivity_alignment
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1957_eisenhower_wage_productivity_alignment, []).

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
 *   constraint_id: sotu_1957_eisenhower_wage_productivity_alignment
 *   human_readable: Wage-Productivity Alignment Norm (Eisenhower 1957)
 *   domain: labor/macroeconomic_policy
 *
 * SUMMARY:
 *   The wage-productivity alignment norm, articulated by President Eisenhower
 *   in 1957, institutionalized a constraint on labor-market wage-setting by
 *   framing wage increases as legitimate only when coupled to productivity
 *   improvements. This constraint functioned simultaneously as a coordination
 *   mechanism (aligning labor incentives with technological improvement), a
 *   consumption protection mechanism (restraining cost-push inflation), and
 *   an extraction mechanism (preventing labor from capturing inflation
 *   premiums or technological rent-sharing). The constraint exemplifies the
 *   DR framework's core insight: the same structural phenomenon appears as
 *   pure coordination (Rope) from capital's perspective, temporary compromise
 *   (Scaffold) from Cold War consensus view, extraction (Snare) from
 *   unorganized labor's perspective, and immutable law (Mountain) from
 *   naturalized economic theory. The extractiveness has grown over the
 *   measurement interval (0.28 → 0.52) as actual wage-productivity decoupling
 *   has increased while the norm persists. Theater ratio has risen (0.45 →
 *   0.64) as the performative content of the constraint has increased: policy
 *   institutions maintain the rhetoric of wage-productivity alignment despite
 *   decoupling in practice, indicating Piton degradation.
 *
 * KEY AGENTS:
 *   - Organized Labor Unions: Primary victim (organized/constrained) — coordinating function for workers but facing asymmetric extraction through wage discipline; identity-fused with post-war compromise framework
 *   - Unorganized Workers: Secondary victim (powerless/trapped) — face wage pressure without collective voice; cannot arbitrage or exit labor market; maximum suppression
 *   - Capital and Business Management: Primary beneficiary (institutional/arbitrage) — wage discipline enables profit capture, price flexibility, and productivity-gain distribution to capital returns
 *   - Consumers and Retirees: Secondary beneficiary (powerful/mobile) — benefit from price stability and purchasing power protection; aggregated interest aligns with macroeconomic policy
 *   - Federal Reserve and Macroeconomic Policy Establishment: Institutional enforcer (institutional/arbitrage) — maintains norm through labor statistics, rhetoric, and interest rate policy; operates with performative component (Piton observation)
 *   - Cold War Consensus Institutions: Temporary mediator (organized/constrained) — built temporary labor-capital accord with full employment commitment as quid pro quo; sunset activated in 1970s as productivity growth slowed
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1957_eisenhower_wage_productivity_alignment, 0.52).
domain_priors:suppression_score(sotu_1957_eisenhower_wage_productivity_alignment, 0.58).
domain_priors:theater_ratio(sotu_1957_eisenhower_wage_productivity_alignment, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1957_eisenhower_wage_productivity_alignment, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1957_eisenhower_wage_productivity_alignment, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(sotu_1957_eisenhower_wage_productivity_alignment, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1957_eisenhower_wage_productivity_alignment, tangled_rope).
narrative_ontology:human_readable(sotu_1957_eisenhower_wage_productivity_alignment, "Wage-Productivity Alignment Norm (Eisenhower 1957)").
narrative_ontology:topic_domain(sotu_1957_eisenhower_wage_productivity_alignment, "labor/macroeconomic_policy").

domain_priors:requires_active_enforcement(sotu_1957_eisenhower_wage_productivity_alignment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1957_eisenhower_wage_productivity_alignment, consumers).
narrative_ontology:constraint_beneficiary(sotu_1957_eisenhower_wage_productivity_alignment, retirees_on_fixed_income).
narrative_ontology:constraint_beneficiary(sotu_1957_eisenhower_wage_productivity_alignment, business_capital_owners).
narrative_ontology:constraint_victim(sotu_1957_eisenhower_wage_productivity_alignment, organized_labor).
narrative_ontology:constraint_victim(sotu_1957_eisenhower_wage_productivity_alignment, wage_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNORGANIZED WORKER (SNARE) — No collective bargaining power; faces wage discipline enforced through employer coordination and inflation pressure. Cannot exit labor market without destitution. Productivity gains are extracted from labor toward capital and consumer prices. Maximum suppression: individual workers cannot coordinate, cannot arbitrage, cannot escape.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_wage_productivity_alignment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: ORGANIZED UNION (TANGLED ROPE) — Genuine coordination function: wage negotiations link labor compensation to technological progress, align incentives for workers to support productivity improvements. Asymmetric extraction: union coordinates for workers' benefit but faces constraint that limits wage growth to productivity gains only, preventing capture of inflation premium or rent extraction. High suppression through political rhetoric and employer coordination against 'wage-driven inflation' claims.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_wage_productivity_alignment, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CAPITAL AND BUSINESS MANAGEMENT (ROPE) — Primary beneficiary. Wages are disciplined; productivity gains can be distributed to capital returns, dividend increases, or price reductions without labor capturing inflation adjustment. Experiences the constraint as coordination: it aligns labor incentives with efficiency improvements. Net beneficiary through arbitrage: can shift between wage concessions, price increases, and profit distribution.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_wage_productivity_alignment, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: CONSUMERS AND RETIREES (ROPE) — Beneficiaries of price stability. Wage restraint prevents cost-push inflation from eroding purchasing power and fixed-income pensions. Experiences the constraint as coordination: it protects the macroeconomic commons. Powerful because aggregated consumer interest aligns with state monetary policy; mobile because can migrate purchasing to alternative goods/services if inflation occurs.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_wage_productivity_alignment, rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: MACROECONOMIC POLICY ESTABLISHMENT (PITON) — Maintains wage-productivity alignment norm through Federal Reserve rhetoric and labor statistics publication. Theater ratio reflects that the 'alignment' is largely performative: actual wage-productivity coupling decouples in practice (wages stagnate while productivity grows), yet the norm persists through policy language and union acceptance. The constraint is degraded — it no longer functionally achieves price stability or fair distribution, yet institutional inertia maintains the framing.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_wage_productivity_alignment, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: POST-WAR CONSENSUS SCAFFOLD (SCAFFOLD) — The wage-productivity alignment norm emerges from Cold War consensus-building: labor accepts wage discipline in exchange for full employment commitment, welfare expansion, and corporate tax burden. This is a temporary coordination mechanism with a sunset: it relied on sustained productivity growth, industrial unionism, and tacit capital-labor accord. As productivity growth slowed (1970s) and globalization fragmented the manufacturing base, the sunset clause activated. The scaffold has low effective extraction because it is explicitly temporary and both labor and capital experienced it as a negotiated compromise.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_wage_productivity_alignment, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURALIZED VIEW (MOUNTAIN) — From a civilizational perspective, the wage-productivity link appears as an immutable feature of rational economics: if wages rise faster than productivity, inflation must follow; inflation erodes real gains. The constraint appears as a law of macroeconomic arithmetic. However, structural data contradicts this: the norm has explicit beneficiaries (consumers, capital), explicit victims (organized labor), and explicit enforcement mechanisms (employer coordination, political rhetoric). The engine detects this as a false summit — naturalizing a contingent institutional arrangement as economic law.
constraint_indexing:constraint_classification(sotu_1957_eisenhower_wage_productivity_alignment, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1957_eisenhower_wage_productivity_alignment_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1957_eisenhower_wage_productivity_alignment, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1957_eisenhower_wage_productivity_alignment, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1957_eisenhower_wage_productivity_alignment, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1957_eisenhower_wage_productivity_alignment, TR),
    TR >= 0.70.

:- end_tests(sotu_1957_eisenhower_wage_productivity_alignment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The constraint redistributes productivity gains away from labor toward capital and consumers. Base value reflects that the norm has genuine coordination function (aligns incentives) alongside extraction (restricts wage growth to productivity only, excluding inflation adjustment or rent-sharing). The value reflects early-1970s dynamics when decoupling begins but norm persists. Growth trajectory (0.28 → 0.52) indicates increasing extractiveness as actual wage-productivity gap widens while constraint remains enforced. Suppression (0.58): Moderate-high. Organized labor faces political pressure, employer coordination against 'wage-driven inflation' rhetoric, and macroeconomic policy headwinds. Unorganized workers face market-mediated suppression. However, suppression is not total: unions retain some bargaining power, sectoral variation exists, and mobility (sectoral or international) provides partial exits for some workers. Theater ratio (0.64): Moderate-high. The constraint increasingly operates as performative theater: policy institutions maintain wage-productivity rhetoric while actual decoupling is visible in data. Unions accept the frame despite growing evidence of asymmetry. Theater growth (0.45 → 0.64) reflects degradation over time as the constraint's functional coordination purpose (align labor to productivity) has been replaced by extractive theater (constrain labor while capital gains accumulate).
 *
 * PERSPECTIVAL GAP:
 *   The constraint produces maximum perspectival divergence across the observation site. Capital and management see Rope: wage-productivity alignment is a coordination mechanism that aligns labor incentives with efficiency and enables price stability. Consumers see Rope: the constraint protects their purchasing power. The Cold War consensus infrastructure sees Scaffold: the constraint is a temporary compromise with a negotiated sunset (full employment as quid pro quo). Unorganized workers see Snare: wage discipline is extraction with no exit and no compensation. Organized labor sees Tangled Rope: the constraint has genuine coordination function (aligns labor to technological progress) but asymmetric extraction (prevents inflation adjustment, prevents profit-sharing, prevents rent capture). The macroeconomic policy establishment sees Piton: the constraint persists through institutional inertia despite decoupling in practice. The analytical observer naturalize it as Mountain: wage-productivity alignment is an economic law. This perspectival range demonstrates the full spectrum of the constraint classification system and instantiates the DR framework's core claim that no single type is 'correct' — the presheaf of perspectives IS the constraint's structural reality.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from beneficiary/victim declarations and exit modulation. Capital (beneficiary + arbitrage exit) derives d ≈ 0.15 → f(d) ≈ -0.02 → negative or near-zero χ; they experience negative effective extraction (the constraint benefits them). Consumers (beneficiary + mobile exit) derive d ≈ 0.35 → f(d) ≈ 0.25 → low positive χ; they experience the constraint as net beneficial. Organized labor (victim + constrained exit) derives d ≈ 0.75 → f(d) ≈ 1.08 → high χ; they experience high effective extraction. Unorganized workers (victim + trapped exit) derive d ≈ 0.90 → f(d) ≈ 1.32 → maximum χ; they experience maximum extraction. The directionality asymmetry is structural: capital can choose measurement methods, sectoral strategies, and timing, while labor cannot choose to ignore wage-productivity constraint without violating the political-economic framework. The scope modifier σ(S) = 1.0 for national scope (baseline). No directionality overrides are required; the derivation chain captures the asymmetry directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the constraint's type varies legitimately with perspective, and no single type is 'the truth.' The resolution is achieved through three insights: (1) The constraint has genuine coordination function (alignment of labor incentives to productivity improvement) — this justifies Rope from capital's perspective and Scaffold from post-war consensus view. (2) The constraint has asymmetric extraction (prevents labor from capturing inflation premiums or technological rents) — this justifies Tangled Rope from organized labor's perspective and Snare from unorganized workers' perspective. (3) The constraint is increasingly performative (rhetoric persists despite decoupling evidence) — this justifies Piton classification from the macroeconomic policy establishment. (4) The mountain classification reveals false-summit naturalization: the constraint is not an economic law but a negotiated institutional arrangement with identifiable beneficiaries and a contingent sunset clause. The mandatrophy resolution is the constraint's perspectival spectrum itself: all types are correct from their respective structural positions. The analytical task is to understand why the same constraint produces divergent classifications — the answer lies in directionality asymmetry, beneficiary/victim asymmetry, and exit-option asymmetry.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    productivity_measurement_ambiguity,
    'How should productivity be measured: output per worker, output per hour, total factor productivity, or sector-specific metrics? Do different measures yield different wage baselines?',
    'Comparative analysis of wage outcomes under different productivity metrics; regression analysis of wage settlement outcomes against productivity measure choice',
    'If total factor productivity is used: capital gains are included in ''productivity,'' reducing wage-share baseline. If output per hour is used: wage claims are higher. Measurement choice determines how much extraction is hidden inside accounting definitions.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(productivity_measurement_ambiguity, empirical, 'Productivity measurement methodology determines wage-share baseline').

omega_variable(
    capital_gains_distribution_ambiguity,
    'Are productivity gains from capital investment (automation, tooling, capacity expansion) legitimately excluded from the wage-productivity link, or does this constitute hidden extraction of labor''s share?',
    'Historical decomposition of productivity growth into labor-contributed vs. capital-contributed components; comparison to pre-1930s labor bargaining frameworks that included capital gains',
    'If capital-driven productivity gains are legitimately excluded: wage-productivity alignment is fair coordination (Rope). If they are hidden extraction: the constraint is more extractive than classified (Snare), and beneficiaries include not just consumers but also capital accumulation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capital_gains_distribution_ambiguity, conceptual, 'Whether capital-driven productivity gains should be included in wage-share calculations').

omega_variable(
    full_employment_commitment_enforcement,
    'Was the full employment commitment (the scaffold''s quid pro quo for labor''s wage discipline) actually enforced, or did it serve as rhetorical cover for wage extraction?',
    'Longitudinal comparison of unemployment rates, job creation, and wage growth across periods; analysis of whether full employment was pursued when conflicts with price stability',
    'If full employment was maintained: scaffold perspective is accurate, and the constraint had genuine coordination function. If full employment was abandoned when inflation fears arose: the scaffold''s sunset was engineered by policy reversal, and the constraint degraded to pure extraction (Snare from labor perspective).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(full_employment_commitment_enforcement, empirical, 'Whether full employment commitment was enforced as negotiated quid pro quo').

omega_variable(
    inflation_causation_attribution,
    'What portion of post-1957 inflation was actually caused by wage pressure (''wage-driven inflation'') vs. other sources (oil shocks, monetary expansion, import competition, demand-pull inflation)?',
    'Econometric decomposition of inflation causes; comparison of wage share to inflation correlation across OECD countries; analysis of wage lag vs. price lag in historical data',
    'If wage-driven inflation is primary cause: constraint is justified and functional (Rope). If other factors dominate: wage-productivity alignment is theater for extracting labor without addressing actual inflation sources (Piton degradation or Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(inflation_causation_attribution, empirical, 'Attribution of inflation causation to wage pressure vs. other sources').

omega_variable(
    identity_lock_on_organized_labor,
    'Do organized labor unions maintain the wage-productivity norm despite growing evidence of its asymmetric extraction because their institutional identity is fused with the compromise framework itself?',
    'Institutional history analysis; interviews and statements from union leadership; comparison of union positions on alternative wage-setting mechanisms (living-wage floors, sectoral bargaining, profit-sharing) vs. wage-productivity attachment',
    'If identity-locked: unions are constrained by cognitive capture, not material barriers. They could exit (demand full cost-of-living adjustment, sectoral bargaining, worker ownership stakes) but identity fusion with the post-war compromise prevents them from seeing alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_on_organized_labor, conceptual, 'Identity fusion of organized labor with post-war compromise framework').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1957_eisenhower_wage_productivity_alignment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wageprod_tr_t0, sotu_1957_eisenhower_wage_productivity_alignment, theater_ratio, 0, 0.45).
narrative_ontology:measurement(wageprod_tr_t5, sotu_1957_eisenhower_wage_productivity_alignment, theater_ratio, 5, 0.55).
narrative_ontology:measurement(wageprod_tr_t10, sotu_1957_eisenhower_wage_productivity_alignment, theater_ratio, 10, 0.64).

% Extraction over time
narrative_ontology:measurement(wageprod_be_t0, sotu_1957_eisenhower_wage_productivity_alignment, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(wageprod_be_t5, sotu_1957_eisenhower_wage_productivity_alignment, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(wageprod_be_t10, sotu_1957_eisenhower_wage_productivity_alignment, base_extractiveness, 10, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1957_eisenhower_wage_productivity_alignment, resource_allocation).
narrative_ontology:affects_constraint(sotu_1957_eisenhower_wage_productivity_alignment, phillips_curve_empirical_expectations).
narrative_ontology:affects_constraint(sotu_1957_eisenhower_wage_productivity_alignment, post_war_labor_capital_accord).
narrative_ontology:affects_constraint(sotu_1957_eisenhower_wage_productivity_alignment, inflation_targeting_regime).

% DUAL FORMULATION NOTE:
% The wage-productivity alignment norm is downstream of the post-war labor-capital accord and upstream of inflation-targeting regimes. The post-war accord (ε ≈ 0.30, Scaffold) established the full employment quid pro quo; the wage-productivity norm operationalized one side of that bargain. Later inflation-targeting frameworks (ε ≈ 0.68, Snare) abandoned the full employment commitment but retained the wage-discipline rhetoric, degrading the constraint from Scaffold to Snare. These stories form a family showing institutional evolution: temporary consensus (Scaffold) → degraded consensus (Piton) → pure extraction under new framework (Snare).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1957_eisenhower_wage_productivity_alignment, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
