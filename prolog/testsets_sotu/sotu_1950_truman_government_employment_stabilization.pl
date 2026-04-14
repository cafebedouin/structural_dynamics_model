% ============================================================================
% CONSTRAINT STORY: sotu_1950_truman_government_employment_stabilization
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sotu_1950_truman_government_employment_stabilization, []).

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
 *   constraint_id: sotu_1950_truman_government_employment_stabilization
 *   human_readable: Government Employment Stabilization via Countercyclical Fiscal Policy (1950 Truman Doctrine)
 *   domain: economics/macroeconomic_policy/employment
 *
 * SUMMARY:
 *   The Truman administration's defense of government employment and
 *   purchasing-power stabilization programs represents a foundational claim
 *   for the postwar mixed economy: that the state must institutionally commit
 *   to countercyclical fiscal intervention to prevent the cycle of demand
 *   collapse, mass unemployment, and social devastation that characterized
 *   the 1930s. This constraint operates across multiple institutional levels
 *   — aggregate demand management, labor market policy, and federal budget
 *   discipline — creating a complex perspectival landscape. The constraint's
 *   extractiveness rises over the measurement interval (0.28 → 0.66) as the
 *   original countercyclical programs become institutionalized and political
 *   lock-in prevents timely contraction during expansions, shifting the
 *   mechanism from stabilization (rope) toward permanent transfer and
 *   extraction (snare). The theater ratio similarly rises (0.35 → 0.75) as
 *   the bureaucratic apparatus acquires self-sustaining constituencies
 *   independent of actual countercyclical function. By the 1970s, critics
 *   across the political spectrum (conservatives attacking deficits,
 *   progressives attacking insufficient universalism) perceive the programs
 *   as performative rather than functional. The constraint exemplifies how a
 *   genuinely innovative coordination mechanism (Keynesian demand management)
 *   degrades into institutional inertia and sectoral redistribution over
 *   generational timescales.
 *
 * KEY AGENTS:
 *   - Employed Workers & Business Sector: Primary beneficiaries (moderate-to-powerful/constrained-to-arbitrage) — capture demand stabilization, employment security, and profit protection during downturns
 *   - Future Taxpayers & Creditor Nations: Primary victims (powerless/trapped) — bear debt service, inflation risk, and fiscal constraints on alternative public investment; born into regime with no exit
 *   - Non-Participating Labor Market Entrants: Secondary victim (moderate/constrained) — benefit from tight labor markets but suffer from productivity drag and underinvestment
 *   - Federal Bureaucracy: Institutional actor (institutional/arbitrage) — maintains employment program apparatus through institutional inertia; acquires autonomous constituencies (regional offices, union contracts)
 *   - Market-Based Reform Coalition: Organized agents (organized/mobile) — advocate transition from discretionary programs to automatic stabilizers; see sunset pathway
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent Keynesian framework as immutable economic law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sotu_1950_truman_government_employment_stabilization, 0.52).
domain_priors:suppression_score(sotu_1950_truman_government_employment_stabilization, 0.48).
domain_priors:theater_ratio(sotu_1950_truman_government_employment_stabilization, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sotu_1950_truman_government_employment_stabilization, extractiveness, 0.52).
narrative_ontology:constraint_metric(sotu_1950_truman_government_employment_stabilization, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(sotu_1950_truman_government_employment_stabilization, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sotu_1950_truman_government_employment_stabilization, tangled_rope).
narrative_ontology:human_readable(sotu_1950_truman_government_employment_stabilization, "Government Employment Stabilization via Countercyclical Fiscal Policy (1950 Truman Doctrine)").
narrative_ontology:topic_domain(sotu_1950_truman_government_employment_stabilization, "economics/macroeconomic_policy/employment").

domain_priors:requires_active_enforcement(sotu_1950_truman_government_employment_stabilization).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sotu_1950_truman_government_employment_stabilization, employed_workers).
narrative_ontology:constraint_beneficiary(sotu_1950_truman_government_employment_stabilization, business_sector).
narrative_ontology:constraint_beneficiary(sotu_1950_truman_government_employment_stabilization, incumbent_government).
narrative_ontology:constraint_victim(sotu_1950_truman_government_employment_stabilization, future_taxpayers).
narrative_ontology:constraint_victim(sotu_1950_truman_government_employment_stabilization, deficit_creditors).
narrative_ontology:constraint_victim(sotu_1950_truman_government_employment_stabilization, non_participating_labor).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EMPLOYED WORKER — ROPE (IMMEDIATE HORIZON) — Benefits directly from sustained employment and purchasing power during recession. Government programs provide income floor and demand stability. Constrained exit: can theoretically refuse public employment but faces severe income loss. Experiences the constraint primarily as coordination: the system solves the collective action problem of aggregate demand collapse. Theater is low from this perspective — the safety net is functionally real, not performative.
constraint_indexing:constraint_classification(sotu_1950_truman_government_employment_stabilization, rope,
    context(agent_power(moderate),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 2: BUSINESS SECTOR — TANGLED ROPE — Benefits from sustained consumer demand and access to credit markets at favorable rates (government-backed bonds). Also benefits from reduced wage pressure during downturns (government employment absorbs slack labor). But also bears costs: taxation to finance programs, labor power drawn away by public employment, reduced monopsony power. Exit option is arbitrage — can relocate internationally, shift to non-cyclical sectors, or lobby to change the program structure. Mixed extraction and coordination: the constraint benefits business through demand stabilization but also extracts through taxation and labor market discipline.
constraint_indexing:constraint_classification(sotu_1950_truman_government_employment_stabilization, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: FUTURE TAXPAYER — SNARE — Bears the extraction of debt service, reduced public investment, and inflation risk from sustained deficit spending. Has no exit: born into the fiscal structure. No vote on whether to accept the intergenerational transfer. Experiences this as pure extraction disguised as stabilization — the benefit flows to current workers and businesses; the cost flows to future creditors and younger cohorts. Trapped because the fiscal regime is locked in by political economy (no politician dismantles popular employment programs; Truman's doctrine becomes irreversible).
constraint_indexing:constraint_classification(sotu_1950_truman_government_employment_stabilization, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 4: NON-PARTICIPATING LABOR MARKET ENTRANT — TANGLED ROPE (GENERATIONAL) — Benefits from tighter labor markets created by government employment absorption of slack (entry-level wages rise). But also bears costs: if government programs are financed through inflation or reduced private investment, human capital formation and productivity growth suffer. Constrained exit — can delay labor force entry, emigrate, or pursue education, but faces real barriers. Benefits are real but indirect; costs are real but diffuse. Generational horizon: wage premium from tight labor market vs. productivity drag from underinvestment.
constraint_indexing:constraint_classification(sotu_1950_truman_government_employment_stabilization, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: FEDERAL BUREAUCRACY — PITON (INSTITUTIONAL/ARBITRAGE) — The apparatus of public employment programs becomes self-sustaining through institutional inertia. Originally created as countercyclical tools (functional), the bureaucracy persists as a performance of economic management regardless of actual countercyclical timing or effectiveness. By the 1960s-1980s, many programs continued during expansions (not recessions), suggesting the managerial theater has decoupled from the original stabilization function. Theater ratio elevated because agencies acquire stakeholders (regional offices, union contracts, congressional constituencies) independent of program efficacy. Arbitrage option: bureaucracy can shift resources between programs, expand mandates, or defend budgets politically without solving the original coordination problem.
constraint_indexing:constraint_classification(sotu_1950_truman_government_employment_stabilization, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: MARKET-BASED REFORM COALITION — SCAFFOLD — Organized economists and policymakers see government employment programs as a temporary solution being replaced by built-in automatic stabilizers (progressive taxation, unemployment insurance, SNAP). The scaffold has a genuine sunset: as automatic stabilizers mature and become embedded in tax code and social programs, explicit public employment programs become redundant. Mobile exit: coalition can advocate for policy transition without abandoning stabilization altogether. Low effective extraction because reformers see an exit path and the coalition has agency. Theater is moderate: transitioning from discretionary public works to automatic mechanisms requires political theater but solving a real coordination problem.
constraint_indexing:constraint_classification(sotu_1950_truman_government_employment_stabilization, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER — MOUNTAIN (CIVILIZATIONAL/UNIVERSAL) — From civilizational scope, the constraint appears as a natural law: capitalist economies are inherently prone to demand collapse without countercyclical fiscal intervention; the Keynesian revolution revealed an immutable property of market economies (falling wages do not restore equilibrium; demand must be sustained exogenously). This perspective naturalizes the constraint as a feature of economic reality rather than a political choice. However, the structural data contradicts mountain classification — identifiable beneficiaries (current workers, incumbent government), identifiable victims (future taxpayers), and significant suppression (political lock-in of programs) suggest the 'natural law' framing is a false summit. Austrian economics, real business cycle theory, and market-clearing alternatives offer competing naturalizations, each claiming to reveal the true natural law. The mountain classification masks the contingency of the Keynesian framing.
constraint_indexing:constraint_classification(sotu_1950_truman_government_employment_stabilization, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sotu_1950_truman_government_employment_stabilization_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sotu_1950_truman_government_employment_stabilization, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sotu_1950_truman_government_employment_stabilization, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(sotu_1950_truman_government_employment_stabilization, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(sotu_1950_truman_government_employment_stabilization, TR),
    TR >= 0.70.

:- end_tests(sotu_1950_truman_government_employment_stabilization_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high at present, rising from 0.28 at inception. The original 1950 program design was genuinely stabilizing — countercyclical by intent, with explicit sunset when recessions ended. By the 1970s, programs persist through expansions, suggesting the stabilization function has decoupled from the original rationale. The rising extractiveness reflects political lock-in: no constituency supports contraction during expansions (workers benefit from employment, politicians benefit from patronage, bureaucrats expand mandates), so the constraint converts from stabilization tool to permanent transfer mechanism. Suppression (0.48): Moderate. Barriers to exit include political entrenchment (rolling back programs invites electoral backlash), constituency dependencies (regional economies depend on program spending), and ideological capture (Keynesianism becomes the establishment orthodoxy, preventing serious challenge). But suppression is not total — neoclassical and Austrian critics maintain alternative frameworks, and the 1980s conservative shift demonstrates partial reversibility. Theater ratio (0.58): Moderate, rising to 0.75. The original programs had low theater — direct job creation with real public works outcomes (dams, roads, schools). By the 1960s-1980s, program expansion continues through recessions and expansions alike, suggesting theater has decoupled from functional stabilization. The bureaucratic apparatus performs demand management rather than implementing it.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single institutional mechanism appears radically differently across structural positions. The employed worker and business sector see coordination (rope) — the system solves the collective action problem of aggregate demand collapse and enables investment planning. The future taxpayer and deficit creditor see pure extraction (snare) — they bear costs they did not choose and cannot escape. The non-participating labor market entrant sees mixed effects (tangled_rope) — benefits from tight labor markets but suffers from productivity drag and fiscal constraints on education/infrastructure. The federal bureaucracy sees its own inertia (piton) — the apparatus persists through institutional momentum rather than functional necessity. The reform coalition sees a temporary problem (scaffold) — automatic stabilizers and monetary policy are building alternative mechanisms with lower institutional overhead. The analytical observer risks seeing immutable natural law (mountain) — demand collapse is inherent to market economies — but this naturalizes what is actually a political choice about fiscal regime design. The perspectival gaps widen over the measurement interval as the constraint drifts from functional stabilization (rope) toward institutional inertia (piton) and extractive transfer (snare).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim declarations and exit options. Employed workers and businesses are structural beneficiaries — they capture demand-side stimulus and employment protection. Their d values are low (beneficiary status → low extraction from their perspective). Future taxpayers are structural victims with no exit (trapped → high d → high f(d) → high experienced extraction). The piton perspective (federal bureaucracy) has high arbitrage capacity — can shift programs, expand budgets, adjust implementation without solving the original coordination problem — so d is moderate. The market reform coalition has mobile exit — can advocate for policy transitions without abandoning stabilization — so d is also moderate. The analytical observer at civilizational scope risks d = 0.72 (canonical analytical value) while misrecognizing contingent institutional arrangements as natural law. The false summit detector will flag the mountain perspective as problematic: the beneficiary and victim data contradict the natural law framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved through temporal decomposition: the constraint is legitimately rope/scaffold at inception (genuine coordination function, sunset clause implicit in countercyclical design) but degrades into tangled_rope/piton/snare over generational timescale as political lock-in prevents contraction and bureaucratic inertia sustains programs independent of stabilization function. The classification is not 'which type is correct?' but 'which era are we measuring?' 1950: rope (functional countercyclical stabilization). 1965: tangled_rope (programs expanded during expansion, mixed extraction and coordination). 1975: piton (programs persist via institutional inertia, theater ratio elevated, functional stabilization questionable). The mandate-atrophy signature captures exactly this drift: the original stabilization mandate (preventing 1930s-style collapse) atrophies into performative fiscal policy and sectoral redistribution. The theater ratio rising from 0.35 to 0.75 documents the mandate atrophy empirically. The resolution is not to reclassify from rope to snare (both are correct for different eras) but to recognize that the constraint's evolutionary trajectory is documented in the measurements and to adjust policy expectations accordingly: programs effective for recession management in 1950 become institutional ballast by 1975.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    countercyclical_timing_effectiveness,
    'Do government employment programs actually implement countercyclical timing (expand during recessions, contract during expansions), or do they persistently expand during both cycles due to political lock-in?',
    'Time-series analysis of spending relative to business cycle; comparison of program expansion rates during recessions vs expansions; audit of program lifecycle across 1945-1980 period',
    'If countercyclical: programs are functional stabilizers (rope/scaffold). If acyclical or procyclical: programs become permanent transfer mechanisms disguised as stabilization (piton or tangled_rope with elevated extraction).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(countercyclical_timing_effectiveness, empirical, 'Whether programs implement countercyclical timing or are persistently acyclical').

omega_variable(
    debt_sustainability_threshold,
    'At what cumulative debt-to-GDP ratio does deficit financing for employment stabilization become unsustainable and shift from coordination mechanism to extractive transfer?',
    'Long-term fiscal modeling; historical comparison with pre-Keynesian economies and post-Keynesian debt accumulation patterns; identification of inflation breakpoint and creditor confidence collapse',
    'If threshold is distant (>100% debt-to-GDP): programs remain coordinate stabilizers for extended period (rope). If threshold is near (<60% debt-to-GDP): even moderate programs become extractive (snare). Ambiguity affects generational perspective: is extraction immediate (snare) or deferred (future taxpayer trap)?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(debt_sustainability_threshold, empirical, 'Debt threshold for shift from coordination to unsustainable extraction').

omega_variable(
    alternative_stabilization_mechanisms,
    'Do automatic stabilizers (progressive taxation, unemployment insurance, income-contingent transfers) deliver equivalent demand stabilization without requiring discretionary government employment programs?',
    'Comparative analysis of demand response to automatic vs discretionary mechanisms; counterfactual modeling of post-1950 recessions with automatic-stabilizers-only vs Truman-style programs; econometric decomposition of demand support sources',
    'If automatic stabilizers sufficient: scaffold sunset is real and inevitable (programs transition to modern form). If insufficient: programs remain structurally necessary (rope or tangled_rope). If automatic stabilizers create their own extraction (welfare dependency, moral hazard): the constraint reframes rather than sunset.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_stabilization_mechanisms, empirical, 'Whether automatic stabilizers can replace discretionary employment programs').

omega_variable(
    political_irreversibility,
    'Is the Keynesian employment program regime politically irreversible, or can major political shifts (conservative ascendance, fiscal crisis) dismantle the constraint?',
    'Political economy analysis: constituency mapping for program beneficiaries, union power, regional distribution; historical comparison with pre-New Deal roll-backs and post-2008 austerity politics; identification of tipping points for policy reversal',
    'If irreversible: the constraint is effectively trapped (mountain-like immutability via political path dependence, not natural law). If reversible: the constraint is contingent (snare with potential exit via political upheaval). Affects future taxpayer perspective: is the debt load permanent (snare) or contestable (tangled_rope)?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_irreversibility, empirical, 'Political reversibility of Keynesian employment regime').

omega_variable(
    employment_program_wage_effect,
    'Do government employment programs sustain nominal wages through demand-side effects, or do they suppress private-sector wages by increasing labor supply (substitution effect)?',
    'Wage dynamics analysis: regression of nominal and real wages on public employment levels; identification of displacement vs demand effects; comparative analysis with periods of private-sector hiring',
    'If demand effect dominates: programs benefit workers across sectors (rope). If substitution effect dominates: programs extract from non-participating workers and future creditors (snare). Determines whether the constraint is genuinely coordination or disguised extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(employment_program_wage_effect, empirical, 'Net wage effect of public employment programs').

omega_variable(
    manufacturing_vs_service_sector_asymmetry,
    'Do government employment programs disproportionately stabilize manufacturing employment vs service sectors, creating sectoral redistribution and productivity drag?',
    'Sectoral decomposition of employment stabilization; tracking of program distribution across manufacturing-intensive vs service-intensive regions; long-term productivity trends by sector; identification of resource lock-in effects',
    'If manufacturing-heavy: programs suppress sectoral reallocation and long-term productivity (extraction from dynamic sectors, benefit to declining industries). If balanced: programs support genuine stabilization across sectors (rope). Affects piton diagnosis: is bureaucratic inertia locking in obsolete sectors?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(manufacturing_vs_service_sector_asymmetry, empirical, 'Sectoral asymmetry in employment stabilization').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sotu_1950_truman_government_employment_stabilization, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sotu_truman_tr_t0, sotu_1950_truman_government_employment_stabilization, theater_ratio, 0, 0.35).
narrative_ontology:measurement(sotu_truman_tr_t8, sotu_1950_truman_government_employment_stabilization, theater_ratio, 8, 0.48).
narrative_ontology:measurement(sotu_truman_tr_t16, sotu_1950_truman_government_employment_stabilization, theater_ratio, 16, 0.62).
narrative_ontology:measurement(sotu_truman_tr_t24, sotu_1950_truman_government_employment_stabilization, theater_ratio, 24, 0.75).

% Extraction over time
narrative_ontology:measurement(sotu_truman_be_t0, sotu_1950_truman_government_employment_stabilization, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(sotu_truman_be_t8, sotu_1950_truman_government_employment_stabilization, base_extractiveness, 8, 0.42).
narrative_ontology:measurement(sotu_truman_be_t16, sotu_1950_truman_government_employment_stabilization, base_extractiveness, 16, 0.58).
narrative_ontology:measurement(sotu_truman_be_t24, sotu_1950_truman_government_employment_stabilization, base_extractiveness, 24, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sotu_1950_truman_government_employment_stabilization, resource_allocation).
narrative_ontology:affects_constraint(sotu_1950_truman_government_employment_stabilization, federal_deficit_accumulation).
narrative_ontology:affects_constraint(sotu_1950_truman_government_employment_stabilization, inflation_wage_spiral).
narrative_ontology:affects_constraint(sotu_1950_truman_government_employment_stabilization, crowding_out_private_investment).
narrative_ontology:affects_constraint(sotu_1950_truman_government_employment_stabilization, union_power_entrenchment).
narrative_ontology:affects_constraint(sotu_1950_truman_government_employment_stabilization, regional_subsidy_dependency).

% DUAL FORMULATION NOTE:
% Government employment stabilization decomposes into three structurally distinct constraints: (1) countercyclical demand management (coordination problem solved by fiscal policy), (2) intergenerational fiscal burden (extraction from future creditors), (3) bureaucratic institutional inertia (degradation of original function). Each has its own ε value and network position. The unified 'employment stabilization' label conflates these three distinct mechanisms. Network decomposition enables tracking of how the original coordination mechanism (1) generates downstream extraction dynamics (2, 3) over generational timescales.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sotu_1950_truman_government_employment_stabilization, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
