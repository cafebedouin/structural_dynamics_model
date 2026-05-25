% ============================================================================
% CONSTRAINT STORY: startup_capital_barrier
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_startup_capital_barrier, []).

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
 *   constraint_id: startup_capital_barrier
 *   human_readable: Startup Capital Barrier and Entrepreneurial Access
 *   domain: economic/venture_capital
 *
 * SUMMARY:
 *   The startup capital barrier represents a structural constraint on
 *   entrepreneurial access where capital scarcity, information asymmetry, and
 *   network gatekeeping combine to extract rents from founders seeking to
 *   scale ventures beyond bootstrap capacity. This constraint exhibits the
 *   full range of DR classification depending on observer position. For
 *   capital-starved founders, the barrier functions as a snare — they face
 *   suppression through minimum viable scale thresholds, geographic distance
 *   from capital sources, and lack of network access, with no meaningful exit
 *   option. For well-connected founders, the constraint functions as tangled
 *   rope — they benefit from coordination (matching capital with execution)
 *   while bearing asymmetric extraction through dilution and investor
 *   control. For venture capital funds, the constraint is pure rope — it
 *   solves a genuine matching problem while restricting supply and justifying
 *   premium returns. The constraint exhibits modest theater (0.35) because
 *   the fundamental coordination function (matching capital with ventures) is
 *   real, unlike purely extractive systems. However, extractiveness has
 *   drifted upward (0.48 to 0.58) over 14 years as information asymmetry has
 *   increased and geographic concentration has deepened, suggesting
 *   rent-seeking layering onto the coordination function.
 *
 * KEY AGENTS:
 *   - Capital-Constrained Founders: Primary victims (powerless/trapped) — face structural impossibility of scaling without venture access; geographically distant or from underrepresented demographics; no alternative funding pathways reach sufficient magnitude
 *   - Well-Connected Founders: Secondary victims/beneficiaries (moderate/constrained) — access capital networks but negotiate from weaker position than investors; benefit from insider knowledge but extract disproportionate value through network gatekeeping
 *   - Venture Capital Funds: Primary beneficiaries (institutional/arbitrage) — extract rents from capital scarcity and information asymmetry; capture disproportionate returns through dilution and control mechanisms; have full exit optionality
 *   - Angel Investors and Syndicates: Secondary beneficiaries (powerful/mobile) — operate within capital networks; some gatekeeping power but less concentrated than institutional funds; moderate extraction
 *   - Alternative Finance Coalition: Organized agents (organized/constrained) — crowdfunding platforms, community capital, revenue-based financing; building parallel mechanisms but also locked into ecosystem dynamics; moderate extraction with genuine coordination function
 *   - Legacy Banking System: Institutional actor (institutional/arbitrage) — theoretically could provide capital but has largely exited market; maintains theater through small business programs and lending divisions while capital flows elsewhere; piton classification reflects degraded function masked by institutional inertia
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing venture capital as the inevitable matching mechanism for high-growth ventures rather than seeing it as one contingent institutional arrangement among alternatives
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(startup_capital_barrier, 0.58).
domain_priors:suppression_score(startup_capital_barrier, 0.65).
domain_priors:theater_ratio(startup_capital_barrier, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(startup_capital_barrier, extractiveness, 0.58).
narrative_ontology:constraint_metric(startup_capital_barrier, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(startup_capital_barrier, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(startup_capital_barrier, tangled_rope).
narrative_ontology:human_readable(startup_capital_barrier, "Startup Capital Barrier and Entrepreneurial Access").
narrative_ontology:topic_domain(startup_capital_barrier, "economic/venture_capital").

domain_priors:requires_active_enforcement(startup_capital_barrier).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(startup_capital_barrier, established_venture_capitalists).
narrative_ontology:constraint_beneficiary(startup_capital_barrier, institutional_investors).
narrative_ontology:constraint_beneficiary(startup_capital_barrier, well_connected_entrepreneurs).
narrative_ontology:constraint_victim(startup_capital_barrier, capital_constrained_founders).
narrative_ontology:constraint_victim(startup_capital_barrier, geographically_distant_entrepreneurs).
narrative_ontology:constraint_victim(startup_capital_barrier, underrepresented_demographic_founders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CAPITAL-STARVED FOUNDER (SNARE) — Faces structural impossibility of bootstrapping to scale without venture capital access. No alternative funding pathways reach sufficient magnitude. Trapped by minimum viable scale threshold and geographic/network distance from capital sources. High extraction: captures disproportionate dilution, unfavorable terms, and survival dependence on investor requirements. Suppression near total — cannot exit or negotiate meaningfully.
constraint_indexing:constraint_classification(startup_capital_barrier, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: WELL-CONNECTED FOUNDER (TANGLED ROPE) — Benefits from insider access to capital networks while also bearing extraction through unfavorable equity distribution and investor control. Constrained by competitive pressure and market timing but has real negotiating power. Experiences genuine coordination: capital providers match with execution teams. Asymmetric extraction exists but is moderate — the constraint partially solves a real matching problem while extracting rents through information asymmetry and gatekeeping.
constraint_indexing:constraint_classification(startup_capital_barrier, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: VENTURE CAPITAL FUND (ROPE) — Benefits directly from the capital barrier: restricted supply of viable investment opportunities justifies premium returns. Experiences the constraint as pure coordination: matching capital with founders solves a genuine market problem. Low suppression from this perspective — can exit by diversifying into other asset classes. Net beneficiary with functional coordination logic underlying the relationship.
constraint_indexing:constraint_classification(startup_capital_barrier, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: ALTERNATIVE FINANCE COALITION (TANGLED ROPE) — Crowdfunding platforms, angel syndicates, and community financing mechanisms coordinate alternative capital pathways while also creating parallel extraction mechanisms (platform fees, syndicate dilution). Organized agents with constrained exit options: building alternatives while simultaneously locked into ecosystem dynamics. Moderate extraction with genuine coordination function — the constraint is being actively decomposed but remains hybrid.
constraint_indexing:constraint_classification(startup_capital_barrier, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: LEGACY BANKING SYSTEM (PITON) — Traditional bank lending to startups has atrophied despite theoretical capacity. Theater persists: small business lending programs exist and are promoted as solutions, but represent minimal actual capital flow to early-stage ventures. The constraint maintains through institutional inertia — banks retain lending divisions and small business programs for regulatory and optics purposes while capital flows to venture funds. High theater ratio reflects degraded function masked by institutional theater.
constraint_indexing:constraint_classification(startup_capital_barrier, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, capital scarcity and information asymmetry between funders and founders appear as immutable economic constraints: matching high-risk ventures with capital inevitably requires verification mechanisms, risk aggregation, and return extraction. This perspective naturalizes what is actually a contingent institutional arrangement (venture capital as the primary scaling mechanism) as an inherent feature of entrepreneurship. The engine's false summit detector will flag this as naturalization of a contingent structure.
constraint_indexing:constraint_classification(startup_capital_barrier, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(startup_capital_barrier_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(startup_capital_barrier, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(startup_capital_barrier, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(startup_capital_barrier, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(startup_capital_barrier, TR),
    TR >= 0.70.

:- end_tests(startup_capital_barrier_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high and increasing. The constraint extracts through three mechanisms: (1) capital scarcity driving dilution and unfavorable terms, (2) information asymmetry enabling investor selection and control, and (3) network gatekeeping limiting access to capital. The increase from 0.48 to 0.58 reflects growing geographic concentration and rise of mega-funds with outsized bargaining power. The value is not higher because genuine coordination function exists — venture capital does solve the matching problem of connecting capital with execution capability, and some founders do achieve outsized returns. Suppression (0.65): Moderate-high. Barriers to alternative funding pathways, minimum viable scale thresholds, geographic concentration of capital, and demographic gatekeeping create significant but not total suppression. Some founders can bootstrap, pursue alternative funding, or build differently. Bootstrapped companies and revenue-based financing represent suppression reduction mechanisms, though remaining marginal at ecosystem scale. Theater ratio (0.35): Moderate-low. The constraint's coordination function is substantially real — matching capital with ventures solves a genuine information and risk aggregation problem. Theater exists in investor pitching rituals and due diligence theater, but is lower than purely extractive constraints. The legacy banking system's theater (maintained through small business programs) is higher, but the venture ecosystem's theater is moderate.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates stark perspectival divergence. The venture capital fund sees pure coordination (Rope) — they are solving the matching problem of connecting capital with founders. The capital-constrained founder sees pure extraction (Snare) — they have no exit and face maximum extraction through dilution and control. The well-connected founder sees mixed coordination and extraction (Tangled Rope) — the system enables their growth while extracting through asymmetric terms. The alternative finance coalition sees a problem being solved (Tangled Rope at generational scale) — new mechanisms are emerging that reduce the constraint but also create parallel extraction. The legacy banking system sees its own degraded function (Piton) — small business lending persists through institutional inertia despite minimal capital actually flowing to early-stage ventures. The civilizational observer risks seeing natural law (Mountain) — venture capital as the inevitable matching mechanism — but the structural data reveals this as naturalization of a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values flow from structural position. Capital-constrained founders with trapped exit options and victim status derive high d (0.92 empirically) — they experience maximum extraction. Well-connected founders with constrained exit but beneficiary status derive moderate d (0.55) — they benefit from network access but face extraction through asymmetric terms. Venture capital funds with arbitrage exit and beneficiary status derive low d (0.08) — extraction flows toward them. The analytical observer at civilizational scope derives d approximately 0.72 — they see the system but are not embedded in its extraction mechanism. These directional differences explain why the same constraint produces different chi values across perspectives: high chi for trapped agents (f(d) ≈ 1.42), moderate chi for constrained agents (f(d) ≈ 0.90), negative chi for arbitrage beneficiaries (f(d) ≈ -0.12).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by showing that venture capital serves both genuine coordination and extractive functions simultaneously. The coordination function is real: matching capital with founders under uncertainty is hard, and institutional structures for aggregating capital and evaluating ventures do provide value. The extraction is real: the barrier restricts supply, enables gatekeeping, and concentrates returns toward investors and well-connected founders. The constraint cannot be classified as pure rope (no genuine coordination, all theater) because scaling ventures beyond bootstrap truly requires capital and evaluation capability. It cannot be classified as pure snare (all extraction, no coordination) because the matching problem is genuine and solutions exist. It is tangled rope — the same structure that solves the matching problem also enables extraction through gatekeeping. The mandatrophy resolution is that both functions are structural and neither can be eliminated without destroying the other: solving the matching problem requires concentrated capital and evaluation capacity, which naturally creates gatekeeping positions and extraction opportunities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_funding_sufficiency,
    'Can alternative funding mechanisms (grants, revenue-based financing, community capital, public investment) reach sufficient scale and speed to compete with venture capital for funding viable high-growth startups?',
    '10-year comparative analysis of capital deployed through alternative mechanisms vs. venture; tracking of founding outcome rates stratified by funding source; assessment of geographic and demographic access parity',
    'If alternative mechanisms reach parity: suppression drops significantly, constraint reclassifies toward Rope from multiple perspectives. If they remain marginal: suppression remains high, constraint remains Snare/Tangled Rope across perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_funding_sufficiency, empirical, 'Whether alternative funding mechanisms can achieve scale parity with venture capital').

omega_variable(
    information_asymmetry_mechanism,
    'Is the primary extraction mechanism information asymmetry (investors have better diligence capacity), network gatekeeping (capital flows through closed networks), or fundamental capital scarcity (insufficient capital for all viable ventures)?',
    'Comparative study of outcomes for founders with equivalent access to diligence information vs. those without; analysis of network composition of successful vs. failed capital raises; measurement of actual vs. theoretical capital supply needed for viable ventures',
    'If primarily information asymmetry: could be addressed through transparency mechanisms, reducing suppression. If primarily gatekeeping: requires network expansion interventions. If capital scarcity: structure is inherently extractive and difficult to reform without macro change.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(information_asymmetry_mechanism, empirical, 'Primary mechanism driving capital barrier extraction').

omega_variable(
    geographic_capital_concentration_inevitability,
    'Is geographic concentration of venture capital (San Francisco, Boston, Beijing) an inevitable feature of capital market efficiency or a contingent institutional arrangement maintained by network effects and regulatory path dependence?',
    'Historical analysis of capital concentration over time; comparative study of distributed vs. concentrated capital ecosystems; assessment of whether geographic distribution increases or decreases capital allocation efficiency and startup success rates',
    'If inevitable: geographic extraction is a structural feature of the constraint. If contingent: distributed capital mechanisms could reshape the constraint''s spatial scope properties and suppress the regional/national level extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geographic_capital_concentration_inevitability, conceptual, 'Whether geographic capital concentration is inevitable or contingent').

omega_variable(
    founder_identity_lock_versus_exit_cost,
    'For capital-constrained founders, is the binding mechanism primarily material (no alternative path to scale) or identity-locked (internalized belief that venture capital is the only legitimate scaling pathway)?',
    'Qualitative analysis of founder narratives and aspirations; study of founders who exit startup formation entirely vs. those who pursue alternative models; assessment of whether identity-lock persists post-exit or dissolves when material barriers are removed',
    'If primarily material: exit_options classification as ''trapped'' is correct. If partially identity-locked: some founders experience the constraint at ''identity_locked'' exit level, changing classification from Snare to Rope at biographical time horizon.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founder_identity_lock_versus_exit_cost, empirical, 'Whether founder barriers are material or identity-based').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(startup_capital_barrier, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(startup_tr_t0, startup_capital_barrier, theater_ratio, 0, 0.28).
narrative_ontology:measurement(startup_tr_t7, startup_capital_barrier, theater_ratio, 7, 0.32).
narrative_ontology:measurement(startup_tr_t14, startup_capital_barrier, theater_ratio, 14, 0.35).

% Extraction over time
narrative_ontology:measurement(startup_be_t0, startup_capital_barrier, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(startup_be_t7, startup_capital_barrier, base_extractiveness, 7, 0.55).
narrative_ontology:measurement(startup_be_t14, startup_capital_barrier, base_extractiveness, 14, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(startup_capital_barrier, resource_allocation).
narrative_ontology:affects_constraint(startup_capital_barrier, geographic_inequality_divergence).
narrative_ontology:affects_constraint(startup_capital_barrier, venture_capital_return_concentration).
narrative_ontology:affects_constraint(startup_capital_barrier, founder_demographic_representation).

% DUAL FORMULATION NOTE:
% The startup capital barrier decomposes into multiple structurally distinct constraints: capital scarcity (information/matching problem), network gatekeeping (social structure problem), and geographic concentration (spatial distribution problem). Each has different ε values and different intervention points. This story captures the aggregate constraint; downstream stories address specific mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(startup_capital_barrier, powerful, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
