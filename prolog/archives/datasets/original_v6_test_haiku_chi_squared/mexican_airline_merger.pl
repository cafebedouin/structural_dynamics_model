% ============================================================================
% CONSTRAINT STORY: mexican_airline_merger
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mexican_airline_merger, []).

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
 *   constraint_id: mexican_airline_merger
 *   human_readable: Mexican Airline Merger Antitrust Exemption
 *   domain: economic/aviation_regulation
 *
 * SUMMARY:
 *   Mexico's antitrust exemption for airline consolidation creates a
 *   structural hybrid of coordination necessity and extraction mechanism. The
 *   ostensible rationale — preserving Mexicana de Aviación as a national
 *   carrier and avoiding bankruptcy — provides a genuine coordination
 *   function: maintaining domestic air connectivity, preventing route
 *   abandonment in remote areas, and retaining a Mexican-controlled airline
 *   in strategic corridors. However, the exemption simultaneously enables the
 *   consolidated carrier to raise prices, reduce service quality, and
 *   suppress smaller competitors without antitrust constraint. The constraint
 *   exhibits the full range of Deferential Realism types depending on
 *   observer position: passengers experience pure extraction (Snare),
 *   competitors face structural disadvantage (Snare), the consolidated
 *   carrier sees rational consolidation (Rope), the government pursues dual
 *   interests (Tangled Rope), the regulatory authority becomes theatrical
 *   (Piton), and the analytical observer sees the coordination-extraction
 *   hybrid clearly (Tangled Rope). The theater ratio (0.58) reflects that
 *   regulatory review processes continue — COFECE maintains merger review
 *   authority — but the predetermined exemption removes the substantive
 *   enforcement mechanism. The constraint's extractiveness (0.52) and
 *   suppression (0.65) place it squarely in the Tangled Rope range, requiring
 *   both coordination function and asymmetric extraction to be present.
 *
 * KEY AGENTS:
 *   - Consolidated Airline Group (Mexicana + merger partner): Primary beneficiary (institutional/arbitrage) — gains monopoly pricing power, route consolidation advantages, and government support; experiences constraint as coordination
 *   - Mexican Air Passengers: Primary victim (powerless/trapped) — bear full cost of price increases and reduced service; cannot exit (geography and transportation alternatives inferior)
 *   - Smaller Competitor Airlines: Secondary victim (moderate/constrained) — face structural disadvantage in competing on exempted routes; exit constrained by capital and regulatory barriers
 *   - Mexican Government: Dual actor (organized/constrained) — coordination role (preserve national carrier, connectivity) and extraction role (control aviation sector, industrial policy); constrained by budget and political legitimacy concerns
 *   - Mexican Antitrust Authority (COFECE): Institutional observer (institutional/arbitrage) — maintains nominal authority but exemption degrades enforcement function to theater; piton perspective reflects degraded constraint
 *   - Analytical Observer: Global competition law perspective (analytical/analytical) — sees tangled coordination-extraction hybrid; can distinguish genuine efficiency gains from rent protection
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mexican_airline_merger, 0.52).
domain_priors:suppression_score(mexican_airline_merger, 0.65).
domain_priors:theater_ratio(mexican_airline_merger, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mexican_airline_merger, extractiveness, 0.52).
narrative_ontology:constraint_metric(mexican_airline_merger, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(mexican_airline_merger, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mexican_airline_merger, tangled_rope).
narrative_ontology:human_readable(mexican_airline_merger, "Mexican Airline Merger Antitrust Exemption").
narrative_ontology:topic_domain(mexican_airline_merger, "economic/aviation_regulation").

domain_priors:requires_active_enforcement(mexican_airline_merger).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mexican_airline_merger, consolidated_airline_group).
narrative_ontology:constraint_beneficiary(mexican_airline_merger, mexican_government_revenue).
narrative_ontology:constraint_victim(mexican_airline_merger, mexican_air_passengers).
narrative_ontology:constraint_victim(mexican_airline_merger, smaller_competitor_airlines).
narrative_ontology:constraint_victim(mexican_airline_merger, market_competition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEXICAN AIR PASSENGERS (SNARE) — Domestic travelers have no exit: air is the primary intercity transport for Mexico's geography. Post-merger pricing power is concentrated; alternatives (bus, car) are inferior for long-distance routes. Suppression: regulatory exemption eliminates competitive discipline. d≈0.92, f(d)≈1.40, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(mexican_airline_merger, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: SMALLER COMPETITOR AIRLINES (SNARE) — Face structural disadvantage post-merger: consolidated carrier gains route density advantage, economies of scale, and regulatory favor. Exit is constrained (capital-intensive industry, regulatory barriers). Cannot match merged entity's pricing on shared routes. d≈0.88, f(d)≈1.35, σ=1.0 → χ≈0.70.
constraint_indexing:constraint_classification(mexican_airline_merger, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: CONSOLIDATED AIRLINE GROUP (ROPE) — Experiences merger as a coordination mechanism: eliminating redundant routes, consolidating hubs, reducing operating costs. Antitrust exemption enables rational route consolidation. Benefits from network effects and scale. Government support de-risks restructuring. d≈0.08, f(d)≈-0.10, σ=1.0 → χ≈-0.05. Net beneficiary.
constraint_indexing:constraint_classification(mexican_airline_merger, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: MEXICAN GOVERNMENT (TANGLED ROPE) — Coordination function: consolidating a national champion airline reduces bankruptcy risk, preserves strategic aviation capacity, and maintains air connectivity (Mexicana serves underserved routes). Extraction function: uses exemption to extract regulatory compliance from competitors, avoid antitrust enforcement costs, and signal industrial policy willingness. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.34.
constraint_indexing:constraint_classification(mexican_airline_merger, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: MEXICAN ANTITRUST AUTHORITY / COFECE (PITON) — Formally independent regulator, but merger exemption granted through political pressure (government direction to rescue Mexicana). COFECE's nominal enforcement power is real, but actual enforcement is theater: the exemption removes the legal basis for challenge. Authority persists (COFECE exists, reviews mergers) but core function (preventing anticompetitive consolidation) is degraded. theater_ratio≈0.58 reflects that regulatory review rituals continue but with predetermined outcome. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.03.
constraint_indexing:constraint_classification(mexican_airline_merger, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a global competition law standpoint, this merger exhibits both coordination and extraction. Coordination: avoiding airline bankruptcy and maintaining connectivity services (genuine public good). Extraction: using government power to override competitive discipline and suppress prices/service quality adjustments through market. The exemption is not a law of nature (mountain) but a deliberate policy choice that redistributes welfare from passengers and competitors to the consolidated carrier and government. d≈0.70, f(d)≈1.08, σ=1.0 → χ≈0.56.
constraint_indexing:constraint_classification(mexican_airline_merger, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mexican_airline_merger_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(mexican_airline_merger, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mexican_airline_merger, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(mexican_airline_merger, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(mexican_airline_merger, TR),
    TR >= 0.70.

:- end_tests(mexican_airline_merger_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The exemption directly enables price-fixing capacity through consolidation and regulatory prohibition of competitive discipline. However, extractiveness is not maximal (0.66+) because the consolidated carrier may provide real operational efficiencies, maintain service obligations, and faces some residual competition from international carriers and ground transport. The measurement trajectory (0.28→0.52 over 5 years) reflects that initial post-merger adjustments appear as efficiency gains, but prices stabilize at elevated levels while capacity remains constrained, revealing the extraction mechanism. Suppression (0.65): High. Multiple reinforcing barriers prevent competitive response: antitrust exemption removes legal recourse; regulatory authority (COFECE) is politically constrained; high capital barriers for new entrants; government support of consolidated carrier creates asymmetric competitive advantage. Only residual suppression (<1.0) reflects that international flights and ground alternatives provide some competitive pressure. Theater ratio (0.58): Moderate. COFECE review processes, merger approval hearings, and ongoing regulatory oversight maintain performative components. However, the predetermined exemption removes substantive uncertainty — the theater is visible and acknowledged. Theater is not as high (≥0.70) as in truly degraded systems because regulatory authority still exists and future enforcement remains possible if political conditions change.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap reveals the constraint's hybrid nature. The consolidated airline sees legitimate coordination (rope): eliminating redundant routes, consolidating hubs, reducing bankruptcy risk — all genuine operational benefits. The government sees mixed coordination and extraction (tangled rope): preserving strategic capacity (coordination) while using exemption to control competition and signal industrial policy (extraction). Passengers see pure extraction (snare): higher prices, reduced capacity, trapped exit. Competitors see structural extraction (snare): regulatory prohibition of competitive response. The regulatory authority sees theater (piton): maintaining review processes while enforcement is predetermined. The analytical observer sees the full tangled rope: both coordination and extraction are real, neither cancels the other. The gap between the airline/government view and the passenger/competitor view is irreducible — they experience structurally different constraints from the same policy.
 *
 * DIRECTIONALITY LOGIC:
 *   Consolidated airline group: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.10. Net beneficiary with low extraction pressure. Mexican government: Beneficiary + constrained (political legitimacy constraint) → d≈0.50, f(d)≈0.65. Symmetric position; benefits from coordination, costs from passenger dissent and international scrutiny. Passengers: Victim + trapped (no alternative transport) → d≈0.92, f(d)≈1.40. Maximum extraction pressure; cannot exit. Smaller competitors: Victim + constrained (capital barriers, regulatory capture) → d≈0.88, f(d)≈1.35. High extraction pressure; some exit possible but costly. COFECE (antitrust authority): Institutional + arbitrage (can exit through formal exemption application) → d≈0.05, f(d)≈-0.12. Appears as beneficiary of exemption because burden is removed, but actually degraded as constraint actor. Analytical observer: analytical → d≈0.70, f(d)≈1.08. High perceived extraction because observer sees inefficiency (monopoly deadweight loss) clearly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by distinguishing GENUINE COORDINATION (maintaining connectivity, preventing route abandonment) from OPPORTUNISTIC EXTRACTION (price-fixing, capacity constraints). The consolidated airline benefits both from legitimate efficiency gains AND from regulatory prohibition of competitive response. The Tangled Rope classification holds both simultaneously: the merger may be structurally necessary (coordination), but the exemption enables anti-competitive behavior (extraction). No single type misses the reality. Mountain (false summit test): 'Airline consolidation is a law of nature' — rejected. The exemption is a deliberate policy choice, not an immutable law. Rope (coordination-only): Rejected for passengers and competitors perspective; rejected at global level where exemption appears as rent-seeking. Snare (extraction-only): Rejected for airline and government perspective; reductionist if it denies real operational benefits. The Tangled Rope holds because BOTH coordination and extraction occur, benefitting different agents. The classification prevents confusion: exemption is not 'good policy for efficiency' (which would be Rope) and not 'pure monopoly rent' (which would be Snare). It's explicitly hybrid.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    merger_efficiency_claims,
    'Do the claimed operational efficiencies from consolidation materialize, or is the exemption purely extractive rent protection?',
    'Post-merger cost analysis: fuel efficiency gains, labor consolidation, route optimization vs actual price increases and service reductions. Comparison with pre-exemption baseline.',
    'If real efficiencies: classification shifts toward Rope (legitimate coordination gain). If absent: classification solidifies as Snare (pure extraction with no coordination benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(merger_efficiency_claims, empirical, 'Whether merger generates operational efficiencies or is pure rent extraction').

omega_variable(
    government_subsidy_contingency,
    'Is the exemption contingent on government financial bailout/subsidy, or does the merged carrier stand alone?',
    'Financial audit: track government capital injection, implicit guarantees, favorable debt restructuring, subsidy contracts for underserved routes. Compare actual government outlay to stated rescue mission.',
    'If subsidized: exemption is extraction mechanism for government-backed monopoly (Snare for passengers). If standalone: exemption is genuine coordination investment (Rope from efficiency view).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(government_subsidy_contingency, empirical, 'Whether airline independence or government subsidy contingency determines exemption justification').

omega_variable(
    competitive_reentry_feasibility,
    'After consolidation, how realistic is market reentry for new carriers or reentry of dissolved competitors?',
    'Regulatory analysis: slot allocation transparency, gate access requirements, airport capacity bottlenecks. Comparative analysis with other aviation markets (US, EU) post-merger.',
    'If reentry blocked: exemption creates permanent extraction window (Snare). If reentry remains realistic: exemption is temporary coordination mechanism (Scaffold).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitive_reentry_feasibility, empirical, 'Whether regulatory barriers prevent competitive reentry after consolidation').

omega_variable(
    unserved_route_expansion,
    'Does the merged carrier expand service to previously unserved routes, or does it consolidate existing profitable routes?',
    'Route network analysis: map all routes pre-merger and post-merger; measure expansion to low-income or remote areas vs optimization of high-margin hub-to-hub corridors.',
    'If expansion: coordination function is real (connectivity gain justifies exemption). If consolidation: exemption enables cream-skimming (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unserved_route_expansion, empirical, 'Whether merger expands unserved routes or consolidates profitable routes').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mexican_airline_merger, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mxam_tr_t0, mexican_airline_merger, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mxam_tr_t2, mexican_airline_merger, theater_ratio, 2, 0.48).
narrative_ontology:measurement(mxam_tr_t5, mexican_airline_merger, theater_ratio, 5, 0.58).

% Extraction over time
narrative_ontology:measurement(mxam_be_t0, mexican_airline_merger, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mxam_be_t2, mexican_airline_merger, base_extractiveness, 2, 0.38).
narrative_ontology:measurement(mxam_be_t5, mexican_airline_merger, base_extractiveness, 5, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mexican_airline_merger, resource_allocation).
narrative_ontology:affects_constraint(mexican_airline_merger, mexican_airline_bankruptcy_prevention).
narrative_ontology:affects_constraint(mexican_airline_merger, latin_american_aviation_consolidation).

% DUAL FORMULATION NOTE:
% The merger exemption is downstream of bankruptcy risk (upstream constraint: Mexicana financial distress) but represents a distinct structural choice. Alternative constraints include labor cost restructuring without consolidation, route subsidization without merger, and international codeshare without exemption. The exemption family includes both the consolidation decision and the competitive outcome.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(mexican_airline_merger, institutional, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
