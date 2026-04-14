% ============================================================================
% CONSTRAINT STORY: boom_bust_path_dependency
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_boom_bust_path_dependency, []).

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
 *   constraint_id: boom_bust_path_dependency
 *   human_readable: The Heritage Fund Piton (Fiscal Volatility Path)
 *   domain: economic/policy
 *
 * SUMMARY:
 *   Alberta's boom-bust fiscal cycle is a piton: a degraded institutional
 *   constraint maintained through political inertia and theatrical rhetoric
 *   despite ample evidence of its dysfunction. The constraint emerges from a
 *   specific set of policy choices—keeping resource royalties among the
 *   lowest in North America, rejecting a Norway-style Heritage Fund,
 *   resisting progressive taxation—that create structural fiscal volatility.
 *   Rather than address the root causes, political actors (particularly
 *   sovereigntist movements) externalize blame onto federal transfers and
 *   'Ottawa capture,' perpetuating a narrative that the volatility is
 *   exogenous and inevitable. This framing naturalizes what is actually a
 *   contingent policy regime. The piton's theater ratio (0.78) reflects the
 *   performative debates about 'fixing the budget' through austerity while
 *   avoiding the core policy levers. Healthcare workers, public servants, and
 *   future taxpayers bear the extraction costs; resource extractors and
 *   low-tax advocates benefit from the regime. The constraint exhibits path
 *   dependency: early choices (low royalties, no savings fund) created
 *   institutional and political lock-in that makes reform costly and
 *   politically fraught, even as the original rationale has degraded.
 *
 * KEY AGENTS:
 *   - Healthcare Workers and Public Service Employees: Primary victims (powerless/trapped) — subject to perpetual wage freezes, layoffs, and service cuts; no exit from provincial employment
 *   - Resource Extractors and Energy Companies: Primary beneficiaries (institutional/arbitrage) — benefit from low, stable royalty rates and predictable tax regime; can shift operations across jurisdictions
 *   - Rural and Regional Communities: Secondary victims (moderate/constrained) — benefit from boom-time infrastructure but face severe service collapses during busts; high cost of exit
 *   - Future Taxpayers: Secondary victims (moderate/constrained) — bear intergenerational costs of capital underinvestment and deferred maintenance; trapped by demographic and geographic constraints
 *   - Provincial Government: Institutional actor maintaining the piton (institutional/arbitrage) — sustains low-tax/low-royalty regime through political theater despite recognition of volatility problem
 *   - Sovereigntist Political Movement: Institutional amplifier (institutional/arbitrage) — benefits from external blame narrative ('Ottawa's fault'); reinforces piton by externalizing causality
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(boom_bust_path_dependency, 0.38).
domain_priors:suppression_score(boom_bust_path_dependency, 0.68).
domain_priors:theater_ratio(boom_bust_path_dependency, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(boom_bust_path_dependency, extractiveness, 0.38).
narrative_ontology:constraint_metric(boom_bust_path_dependency, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(boom_bust_path_dependency, theater_ratio, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(boom_bust_path_dependency, piton).
narrative_ontology:human_readable(boom_bust_path_dependency, "The Heritage Fund Piton (Fiscal Volatility Path)").
narrative_ontology:topic_domain(boom_bust_path_dependency, "economic/policy").

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(boom_bust_path_dependency, resource_extractors).
narrative_ontology:constraint_beneficiary(boom_bust_path_dependency, low_tax_advocates).
narrative_ontology:constraint_victim(boom_bust_path_dependency, healthcare_workers).
narrative_ontology:constraint_victim(boom_bust_path_dependency, public_service_workers).
narrative_ontology:constraint_victim(boom_bust_path_dependency, future_taxpayers).
narrative_ontology:constraint_victim(boom_bust_path_dependency, rural_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: HEALTHCARE WORKERS (SNARE) — Trapped in a system of perpetual austerity cycles driven by boom-bust commodity volatility. No exit from provincial employment; subject to wage freezes, layoffs, and service cuts during busts. Extract maximum experienced cost with no agency or compensation mechanism. The constraint is enforced through budget discipline and political blame-shifting, not through voluntary coordination.
constraint_indexing:constraint_classification(boom_bust_path_dependency, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: RURAL COMMUNITIES (TANGLED ROPE) — Benefit from periodic oil-driven infrastructure investment during booms (roads, schools, utilities) but face service collapses during busts. Constrained exit: costly to relocate; depend on provincial services. Experience both coordination (needed for infrastructure scale) and extraction (unequal burden of austerity). Active enforcement through provincial budget cycles and federal transfer negotiations.
constraint_indexing:constraint_classification(boom_bust_path_dependency, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: RESOURCE EXTRACTORS (ROPE) — Benefit from low and stable royalty rates regardless of commodity cycle. Arbitrage options: can shift operations across jurisdictions or adjust production volume. Experience the constraint primarily as coordination: predictable tax/royalty rates enable long-term investment planning. Net beneficiary — the low royalty regime persists because political actors externalize volatility onto public services rather than stabilizing via resource rents.
constraint_indexing:constraint_classification(boom_bust_path_dependency, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PROVINCIAL GOVERNMENT (PITON) — The core piton: maintains a low-tax/low-royalty regime and narrative ('Alberta advantage') despite ample historical evidence that this strategy creates fiscal volatility. The narrative persists through institutional inertia and political theater despite degraded function. The government experiences the volatility problem as externally imposed (blame Ottawa, blame markets) rather than as a consequence of policy choices. Active enforcement through perpetual rhetoric ('we can't raise taxes because the private sector will leave') maintains the piton even as its stated purpose—attracting investment and ensuring prosperity—has degraded. Theater ratio high (0.78): performative debates about 'fixing the budget' while avoiding the core policy levers (royalty rates, Heritage Fund contribution mandates, progressive taxation).
constraint_indexing:constraint_classification(boom_bust_path_dependency, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: FUTURE TAXPAYERS (SNARE) — Trapped by path dependency: today's low-revenue, high-volatility regime constrains tomorrow's fiscal capacity. Capital underinvestment in education, infrastructure, and health systems during austerity cycles compounds intergenerational cost. Cannot exit; will bear increased debt service, deferred maintenance, and limited public investment. No compensation mechanism for the extraction of present prosperity at future expense.
constraint_indexing:constraint_classification(boom_bust_path_dependency, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER — COMMODITY CURSE (FALSE MOUNTAIN) — From an analytical/civilizational view, commodity-dependent fiscal structures inevitably create volatility: this is a natural law of resource economies. However, this naturalizes what is actually a policy choice: Norway stabilized through a sovereign wealth fund, Chile through countercyclical spending rules, Canada through transfer programs. The mountain classification is a false summit masking contingent institutional design. Alberta's volatility is not inherent to oil; it is inherent to the choice to keep royalties low, avoid wealth fund accumulation, and shift risk onto public employment and service provision.
constraint_indexing:constraint_classification(boom_bust_path_dependency, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(boom_bust_path_dependency_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(boom_bust_path_dependency, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(boom_bust_path_dependency, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(boom_bust_path_dependency, TR),
    TR >= 0.70.

:- end_tests(boom_bust_path_dependency_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The constraint extracts from public service workers and future taxpayers through austerity cycles, but extraction is not as severe as a snare (ε > 0.46) because it is partly voluntarily maintained through political choice rather than structural necessity. The beneficiaries (resource extractors, low-tax advocates) capture value, but the regime's reproduction relies on political rhetoric and institutional inertia, not pure coercion. Suppression (0.68): High. Barriers to exit include: (1) geographic immobility (cannot easily leave provincial employment), (2) political capture (low-tax ideology dominates discourse), (3) institutional lock-in (decades of low-royalty contracts limit revenue capacity), (4) blame-shifting (external attribution to federal transfers prevents internal policy reform). These are suppressed alternatives: progressive taxation, higher royalties, and Heritage Fund mandates are politically radioactive despite strong comparative evidence. Theater ratio (0.78): High. The constraint is maintained substantially through theatrical activity: political debates about 'belt-tightening' and 'fiscal responsibility' that avoid the core policy choices (royalty rates, taxation structure, savings fund requirements). The theater has increased over time as the original rationale (attracting investment) has degraded—Alberta's oil industry is now subject to global commodity markets and climate policy regardless of tax rates, yet the low-tax narrative persists.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals how the same constraint structure produces divergent classifications. Resource extractors see Rope (pure coordination of predictable fiscal regime enabling investment). The provincial government sees Piton (recognizes volatility as a problem but maintains the regime through political inertia). Healthcare workers see Snare (trapped in austerity with no exit). Rural communities see Tangled Rope (both coordinated infrastructure provision and extraction through bust-time service cuts). Future taxpayers see Snare (pure extraction via deferred investment and accumulated debt). The analytical observer risks seeing Mountain (commodity volatility as inevitable law of resource economies) but structural analysis reveals this as a false summit: Norway, Chile, and Canada demonstrate that policy design (not commodity curse) determines fiscal stability. The gap between beneficiary (Rope) and victim (Snare) perspectives is maximal, indicating strong asymmetric extraction masked by coordination rhetoric.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values derive from agents' structural positions relative to the fiscal volatility constraint. Resource extractors (beneficiaries, arbitrage options) experience low d ≈ 0.10-0.15: they benefit from predictable low royalties and can shift production volume to manage revenue risk. The provincial government (institutional beneficiary, arbitrage through political rhetoric) experiences d ≈ 0.20-0.30: they benefit from the low-tax narrative and can avoid internal policy reform through external blame. Public service workers and healthcare employees (trapped, victims) experience high d ≈ 0.90-0.95: they bear volatility costs with no exit option. Rural communities (constrained, mixed victims/beneficiaries) experience moderate d ≈ 0.55-0.65: they benefit from boom-time infrastructure but face severe bust-time service cuts. Future taxpayers (constrained, pure victims) experience high d ≈ 0.80-0.85: they inherit fiscal constraints with no agency in creating them. The engine's directionality derivation automatically computes these from the beneficiary/victim declarations and exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by exposing the piton classification itself: the constraint is NOT a mountain of economic necessity, but a piton of institutional inertia. The provincial government maintains the low-tax/low-royalty regime while simultaneously claiming that volatility is an external problem ('Ottawa's fault,' 'commodity markets'). This is the canonical piton pattern: maintaining a defunct institutional arrangement through theater while externalizing causality. The mandatrophy is resolved by recognizing that 'fiscal volatility' is not an irreducible constraint but a consequence of specific policy choices that persist through political narrative rather than structural necessity. The false mountain (commodity curse) naturalizes what is actually institutional design. The true structure is a piton maintained by sovereigntist narrative that externalizes blame, preventing reform to royalty rates, tax progressivity, or Heritage Fund mandates. The constraint persists not because volatility is unavoidable but because reform is politically costly and the cost is externalized onto powerless agents (healthcare workers, future taxpayers).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    counterfactual_heritage_fund,
    'Would a Norway-style Heritage Fund mandate (e.g., 50% of resource revenues at consistent commodity prices) have stabilized fiscal volatility, or would political pressure have eroded the mandate during booms?',
    'Historical analysis of failed provincial savings fund mandates (e.g., Alberta Heritage Fund 1976-1987 contribution patterns); comparative study of Norway''s governance structures that sustained the fund vs political dynamics in resource-dependent regions',
    'If mandate would have held: the piton is a choice masquerading as necessity (false mountain confirmed). If mandate would have been eroded: volatility is a political economy problem requiring stronger institutions, not merely better policy design.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(counterfactual_heritage_fund, empirical, 'Whether Heritage Fund mandate could have stabilized volatility').

omega_variable(
    attribution_of_austerity_burden,
    'What share of Alberta''s public service austerity and healthcare cutbacks since 2015 is attributable to federal transfer reductions vs provincial revenue volatility from low royalties?',
    'Fiscal accounting: decompose provincial budget deficits by source (lower commodity prices, lower royalty take, lower federal transfers, policy choices); comparative analysis of peer provinces with different revenue structures',
    'If federal transfers dominate: sovereigntist narrative gains traction (piton persists through blaming Ottawa). If low royalties dominate: self-inflicted narrative gains traction (piton exposed as choice, not necessity).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(attribution_of_austerity_burden, empirical, 'Attribution of austerity to federal transfers vs provincial revenue choices').

omega_variable(
    political_sustainability_of_reform,
    'If Alberta moved to Scandinavian-level royalty rates (40-50% of resource value) and a mandatory Heritage Fund contribution, would the political coalition supporting low-tax ideology collapse, or would the distributional benefits (reduced volatility, increased public investment) stabilize the reform?',
    'Comparative political economy: study outcomes of similar reform attempts in resource-dependent regions (Alaska, Canada, Chile); scenario modeling of Alberta fiscal stability under higher royalty regimes; polling and political alignment analysis',
    'If reform would collapse: path dependency is locked in by political economy (piton is entrenched). If reform would stabilize: the piton is maintained by myopia and elite capture rather than structural necessity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_sustainability_of_reform, preference, 'Political sustainability of moving to higher royalty rates').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(boom_bust_path_dependency, 1980, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bbpd_tr_t0, boom_bust_path_dependency, theater_ratio, 0, 0.65).
narrative_ontology:measurement(bbpd_tr_t10, boom_bust_path_dependency, theater_ratio, 10, 0.72).
narrative_ontology:measurement(bbpd_tr_t20, boom_bust_path_dependency, theater_ratio, 20, 0.78).

% Extraction over time
narrative_ontology:measurement(bbpd_be_t0, boom_bust_path_dependency, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(bbpd_be_t10, boom_bust_path_dependency, base_extractiveness, 10, 0.33).
narrative_ontology:measurement(bbpd_be_t20, boom_bust_path_dependency, base_extractiveness, 20, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(boom_bust_path_dependency, resource_allocation).
narrative_ontology:affects_constraint(boom_bust_path_dependency, canadian_transfer_payment_capture).
narrative_ontology:affects_constraint(boom_bust_path_dependency, resource_curse_narrative).

% DUAL FORMULATION NOTE:
% The boom-bust fiscal path dependency decomposes into two distinct constraint stories: (1) boom_bust_path_dependency (this story, ε=0.38, piton) — the institutional choice to maintain low-tax/low-royalty regime despite recognized volatility costs; (2) commodity_price_transmission (ε=0.08, rope) — the structural necessity for fiscal policy to respond to commodity prices given a resource-dependent revenue base. The piton is downstream of the commodity price transmission constraint but represents a distinct institutional failure to adopt stabilization mechanisms (Heritage Fund, progressive taxation, countercyclical spending) that would convert rope into managed coordination.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(boom_bust_path_dependency, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
