% ============================================================================
% CONSTRAINT STORY: us_labor_mobility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_labor_mobility, []).

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
 *   constraint_id: us_labor_mobility
 *   human_readable: US Geographic and Professional Labor Mobility
 *   domain: economic/technological
 *
 * SUMMARY:
 *   US labor mobility — the ability of workers to relocate for economic
 *   opportunity across geographic and professional boundaries — appears as a
 *   fundamentally different constraint depending on who is observing. For
 *   high-skill workers in tech hubs, it is primarily a coordination mechanism
 *   enabling efficient labor markets and knowledge spillovers (Rope). For
 *   place-bound workers in declining regions, it is a snare: they are trapped
 *   by housing debt, family ties, and credential devaluation, forced to watch
 *   opportunity migrate elsewhere while their local economies deteriorate.
 *   The constraint exhibits all six DR types, revealing how the same
 *   phenomenon (geographic labor reallocation) can be functional or
 *   extractive depending on the observer's structural position. The core
 *   tension is between efficient labor reallocation (which requires mobility)
 *   and human rootedness (which resists it). Over the past 40 years,
 *   extractiveness has increased from 0.38 to 0.58, driven by widening wage
 *   gaps between opportunity zones (tech hubs, financial centers) and
 *   declining regions, rising housing costs in opportunity zones, fragmented
 *   occupational licensing, and family constraints. Theater ratio has
 *   increased from 0.35 to 0.52, reflecting the performative nature of
 *   mobility-promotion rhetoric that ignores structural barriers. Remote work
 *   infrastructure (post-2020) represents a potential sunset mechanism for
 *   the constraint — it could decouple earnings potential from geographic
 *   location, allowing workers to stay rooted while accessing high-wage labor
 *   markets — but this sunset is contingent on sustained employer commitment
 *   and broadband infrastructure investment.
 *
 * KEY AGENTS:
 *   - High-skill workers: Primary beneficiary (institutional/arbitrage) — can relocate to opportunity zones, capture wage premiums, participate in high-productivity clusters
 *   - Tech hub employers and VC ecosystem: Primary beneficiary (institutional/arbitrage) — benefit from geographic concentration of talent, knowledge networks, and capital
 *   - Place-bound workers: Primary victim (powerless/trapped) — locked in declining regions by housing debt, family ties, and declining local labor markets; experience extraction
 *   - Declining regional economies: Structural victim (powerless/trapped) — lose human capital and economic base to geographic concentration; no exit option
 *   - Low-skill workers: Structural victim (moderate/constrained) — limited mobility due to family constraints and lack of portable credentials; face both place-binding and skill-binding
 *   - State licensing and credentialing systems: Institutional actor (institutional/constrained) — maintain interstate barriers through occupational licensing; perform gatekeeping function with degraded functional necessity (Piton)
 *   - Regional development coalitions: Organized actors (organized/constrained) — building alternative pathways (remote work infrastructure, distributed talent networks) with sunset logic
 *   - Housing market owners and developers: Institutional beneficiary (institutional/arbitrage) — capture rents through geographic concentration; benefit from supply constraints in opportunity zones
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_labor_mobility, 0.58).
domain_priors:suppression_score(us_labor_mobility, 0.68).
domain_priors:theater_ratio(us_labor_mobility, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_labor_mobility, extractiveness, 0.58).
narrative_ontology:constraint_metric(us_labor_mobility, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(us_labor_mobility, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_labor_mobility, tangled_rope).
narrative_ontology:human_readable(us_labor_mobility, "US Geographic and Professional Labor Mobility").
narrative_ontology:topic_domain(us_labor_mobility, "economic/technological").

domain_priors:requires_active_enforcement(us_labor_mobility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_labor_mobility, high_skill_workers).
narrative_ontology:constraint_beneficiary(us_labor_mobility, tech_hub_employers).
narrative_ontology:constraint_beneficiary(us_labor_mobility, venture_capital_ecosystem).
narrative_ontology:constraint_victim(us_labor_mobility, place_bound_workers).
narrative_ontology:constraint_victim(us_labor_mobility, declining_regional_economies).
narrative_ontology:constraint_victim(us_labor_mobility, low_skill_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PLACE-BOUND WORKER (SNARE) — Cannot relocate due to family ties, housing debt, limited savings, or regional credential devaluation. Trapped in declining labor markets. High extraction, high suppression. d≈0.92, f(d)≈1.40, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(us_labor_mobility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: MOBILE BUT CONSTRAINED WORKER (TANGLED ROPE) — Nominally can relocate but faces barriers: housing market costs in opportunity zones, credential licensing across states, family constraints, student debt burden. Experiences mixed coordination (labor market efficiency) and extraction (captured by housing/licensing regimes). d≈0.68, f(d)≈1.05, σ=1.0 → χ≈0.58.
constraint_indexing:constraint_classification(us_labor_mobility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: TECH HUB EMPLOYER / VC ECOSYSTEM (ROPE) — Benefits from geographic concentration and worker mobility. Labor migration solves collective action problem: enables skill clustering, network density, knowledge spillovers. Can arbitrage across regions. d≈0.10, f(d)≈0.05, σ=1.2 → χ≈0.04. Net beneficiary; constraint experiences as coordination.
constraint_indexing:constraint_classification(us_labor_mobility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: REGIONAL DEVELOPMENT COALITION (SCAFFOLD) — Organized actors (midwest tech councils, sunbelt workforce programs, remote-work advocates) see labor mobility as a temporary problem being solved by telework infrastructure, distributed engineering talent, and revitalized downtowns. Sunset clause: as remote work matures and regional job markets strengthen, geographic mobility pressure declines. d≈0.45, f(d)≈0.55, σ=1.0 → χ≈0.32.
constraint_indexing:constraint_classification(us_labor_mobility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE LICENSING / CREDENTIALING SYSTEMS (PITON) — Occupational licensing (medical, legal, engineering) blocks interstate mobility for legitimate safety reasons but persists beyond functional necessity. Theater ratio 0.63 reflects performative gatekeeping. Original function (prevent quackery) degraded into protectionism. Maintained by professional guilds through inertia, not safety improvements.
constraint_indexing:constraint_classification(us_labor_mobility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational view, some geographic immobility is inherent: family ties, cultural rootedness, asset lock-in (housing) are irreducible constraints on human relocation. However, base properties (ε=0.58, suppression=0.68) contradict mountain classification — institutional arrangements (housing policy, licensing, credential portability) are contingent, not immutable. False summit: naturalizes policy choices as laws of nature.
constraint_indexing:constraint_classification(us_labor_mobility, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_labor_mobility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(us_labor_mobility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(us_labor_mobility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(us_labor_mobility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(us_labor_mobility, TR),
    TR >= 0.70.

:- end_tests(us_labor_mobility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing trend. The constraint extracts from place-bound workers and declining regions through wage gaps (0-40% premium for relocation to hubs over interval), housing market rents (opportunity zone housing costs increased 2-3x faster than median wages), and opportunity concentration. The extraction is active and growing but not total — some workers successfully relocate, some declining regions diversify. Suppression (0.68): Substantial but not absolute. Multiple barriers: occupational licensing blocks ~10% of professionals from interstate practice; housing costs in opportunity zones create 7-10 year savings barrier for entry-level workers; family constraints (childcare, elderly parent care) reduce exit options for 25-30% of working-age population; credential valuation varies by state (engineer/teacher licensing particularly restrictive). Theater ratio (0.52): Moderate. Considerable performative content in mobility narrative: 'move for opportunity' rhetoric ignores structural barriers; 'skills shortage' claims ignore credential portability barriers; 'geographic arbitrage' narratives omit housing cost feedback loops. But theater is not dominant — genuine wage premiums and skill clustering effects are real, just asymmetrically distributed.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximum perspectival divergence. Tech hub employers see Rope (labor market coordination). High-skill arbitrageurs see Rope or Scaffold (they are solving the problem via relocation or remote work). Place-bound workers see Snare (extraction, no exit). Declining regions see Snare (systematic extraction of human capital). Moderate workers with some mobility see Tangled Rope (mixed coordination and extraction: they benefit from efficient labor matching but suffer extraction through housing markets and credential barriers). State licensing systems see Piton (their gatekeeping function is performative — safety arguments no longer justify interstate restrictions). Remote-work advocates see Scaffold (distributed infrastructure is building a sunset for geographic mobility requirement). The analytical observer at civilizational scale risks seeing Mountain (human rootedness is an immutable feature of human nature) — but this is a false summit: the constraint is substantially policy-contingent (zoning, licensing, credential portability, housing finance).
 *
 * DIRECTIONALITY LOGIC:
 *   High-skill workers (institutional, arbitrage, beneficiary): d≈0.10, f(d)≈0.05. Net beneficiary. They earn premiums and experience mobility as freedom. Tech hub employers (institutional, arbitrage, beneficiary): d≈0.08, f(d)≈-0.08. Net beneficiary. They benefit from geographic concentration and skill clustering. Place-bound workers (powerless, trapped, victim): d≈0.92, f(d)≈1.40. Maximal extraction. No exit capacity, trapped by housing debt and family ties, experience wage suppression. Declining regions (powerless, trapped, victim): d≈0.93, f(d)≈1.41. Maximal extraction. Systematic human capital extraction with no alternative. Low-skill workers (moderate, constrained, victim): d≈0.70, f(d)≈1.08. High extraction. Mobility constrained by family burden and credential portability barriers; also trapped by lack of specialized skills. State licensing systems (institutional, constrained): d≈0.35, f(d)≈0.30. Despite benefit to professional guilds (beneficiary logic), the constrained exit options and loss of original function justify piton classification over rope. Regional development coalitions (organized, constrained): d≈0.45, f(d)≈0.55. Moderate extractiveness; they have agency and see pathway forward (remote work, distributed talent). Housing market (institutional, arbitrage): implicit beneficiary — d≈0.05, f(d)≈-0.12. Net beneficiary through rent capture.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that geographic mobility is fundamentally a **coordination problem with extractive capture**. The pure-coordination reading (Rope: everyone benefits from efficient labor reallocation) is true but incomplete — it describes the beneficiary's experience. The pure-extraction reading (Snare: workers are trapped, regions are drained) is true but incomplete — it describes the victim's experience. The constraint is tangled because: (1) there is a real coordination function (labor market efficiency, knowledge spillovers, productivity gains) that benefits the overall economy; (2) there is active extraction (housing rents, wage gaps, credential rents) that benefits specific actors and harms others; (3) the extraction is normalized through market rhetoric ('efficient allocation,' 'vote with your feet') that obscures its coercive components (family ties cannot be liquidated, housing debt is a collateral-based trap, state licensing is a barrier backed by state power). The constraint is not 'pure coordination disguised as extraction' (that would be a Snare falsely labeled Rope) nor 'pure extraction disguised as coordination' (that would be Rope falsely labeled Snare). It is genuinely hybrid: the efficiency gains are real, the extraction is real, and they are structurally intertwined. The Tangled Rope classification captures this: active enforcement (licensing, housing finance regulations) is required to maintain the system; multiple perspectives classify differently (confirming asymmetric extraction); beneficiaries and victims are distinct; and both coordination function and extraction mechanism are structurally necessary to the constraint's operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    housing_market_extraction_vs_coordination,
    'Is the relationship between housing prices and mobility a coordination problem (efficient price signal) or an extraction mechanism (landlord/developer rent capture)?',
    'Cross-national comparison of housing price-to-income ratios in high-mobility vs low-mobility economies; correlation analysis between housing policy (zoning, supply constraints) and wage growth trajectory',
    'If coordination: mobility decline reflects efficient rational choice. If extraction: housing policy is a snare disguised as market efficiency, requiring policy intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(housing_market_extraction_vs_coordination, empirical, 'Housing market role in mobility constraint').

omega_variable(
    occupational_licensing_gatekeeping_scope,
    'What fraction of interstate mobility loss is due to legitimate occupational safety (physicians, engineers) vs protectionist guild gatekeeping (hair braiding, interior design)?',
    'Licensing reciprocity analysis by profession; comparison of consumer outcomes in full-reciprocity states vs restricted states; international comparison of licensing regimes and mobility rates',
    'If legitimate: 5-10% of mobility loss justified. If protectionist: 30-50% is pure extraction, enabling targeted reciprocity reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(occupational_licensing_gatekeeping_scope, empirical, 'Occupational licensing contribution to mobility barrier').

omega_variable(
    remote_work_structural_permanence,
    'Is the post-pandemic remote work infrastructure permanent enough to structurally decouple earnings potential from geographic location?',
    'Longitudinal tracking of remote work adoption by industry through 2030; analysis of wage convergence between traditional hubs and distributed locations; employer return-to-office commitment tracking',
    'If permanent: scaffold sunset is real — geographic mobility constraint declines by 30-40% within 10 years. If temporary: remote infrastructure fades, mobility constraint persists at current level.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remote_work_structural_permanence, empirical, 'Permanence of remote work as mobility constraint reliever').

omega_variable(
    family_rootedness_vs_economic_necessity,
    'How much of geographic immobility is due to rational family preference for staying put vs economic desperation that makes mobility feel impossible?',
    'Willingness-to-pay surveys; analysis of mobility patterns in response to extreme opportunity gaps (>50% wage premium); comparison of voluntary non-movers vs involuntary trapped workers',
    'If preference-driven: immobility is legitimate choice, not extraction. If desperation-driven: trapped workers'' low exit capacity is structural, snare classification confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(family_rootedness_vs_economic_necessity, preference, 'Family ties vs economic desperation in mobility immobility').

omega_variable(
    skill_credential_portability_enforcement,
    'Can standardized competency testing (instead of state-by-state licensing) provide equivalent consumer protection with near-zero mobility friction?',
    'Pilot programs for multi-state credentials; comparative safety outcomes in occupations with high reciprocity vs restrictive licensing; regulatory impact analysis',
    'If viable: licensing barrier to mobility can be nearly eliminated, reducing suppression from 0.68 to ~0.40 and reclassifying constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(skill_credential_portability_enforcement, empirical, 'Feasibility of standardized credentials to replace state licensing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_labor_mobility, 1980, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(uslm_tr_t0, us_labor_mobility, theater_ratio, 0, 0.35).
narrative_ontology:measurement(uslm_tr_t20, us_labor_mobility, theater_ratio, 20, 0.44).
narrative_ontology:measurement(uslm_tr_t40, us_labor_mobility, theater_ratio, 40, 0.52).

% Extraction over time
narrative_ontology:measurement(uslm_be_t0, us_labor_mobility, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(uslm_be_t20, us_labor_mobility, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(uslm_be_t40, us_labor_mobility, base_extractiveness, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_labor_mobility, resource_allocation).
narrative_ontology:affects_constraint(us_labor_mobility, occupational_licensing_fragmentation).
narrative_ontology:affects_constraint(us_labor_mobility, us_housing_market_constraint).
narrative_ontology:affects_constraint(us_labor_mobility, regional_economic_divergence).
narrative_ontology:affects_constraint(us_labor_mobility, remote_work_infrastructure).

% DUAL FORMULATION NOTE:
% Labor mobility is downstream of housing market, occupational licensing, and regional divergence constraints. Each has its own epsilon reflecting local structural factors, but labor mobility integrates them into a single experience for workers. The upstream constraints have lower epsilon values (more natural/necessary) while labor mobility exhibits higher extractiveness (more policy-contingent) because it is the site where multiple policy choices converge. Decomposition follows ε-invariance: housing (ε≈0.35, resource constraint), licensing (ε≈0.42, gatekeeping), regional divergence (ε≈0.65, extractive outcome) are distinct constraints linked through labor mobility (ε≈0.58, coordinated outcome).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(us_labor_mobility, institutional, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
