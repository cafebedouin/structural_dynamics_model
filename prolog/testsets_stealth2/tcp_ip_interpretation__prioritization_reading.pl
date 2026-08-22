% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__prioritization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tcp_ip_interpretation__prioritization_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tcp_ip_interpretation__prioritization_reading
 *   human_readable: Paid Prioritization as Network Management (Prioritization Reading of TCP/IP)
 *   domain: technological/legal/regulatory
 *
 * SUMMARY:
 *   This story instantiates the PRIORITIZATION READING of the TCP/IP
 *   interpretation kernel: the claim that the architecture's best-effort
 *   service model permits differentiated service quality as legitimate
 *   network management, and therefore that ISPs may sell transmission
 *   priority. Under this reading the standing arrangement is a regime in
 *   which incumbent broadband operators offer managed quality tiers and
 *   premium interconnection, large edge platforms purchase priority, and edge
 *   services without fast-lane budgets compete on degraded best-effort terms.
 *   The reading's own expected structural delta names both faces of the
 *   arrangement: network investment is said to be incentivized, and unfunded
 *   edge services are conceded to be disadvantaged. Epsilon's referent is
 *   fixed to that standing arrangement — the paid-prioritization regime as it
 *   actually operates — and the value is indexed to THIS reading's lights:
 *   the reading treats most of the fee flow as legitimate management and
 *   investment funding while conceding the disadvantage to unfunded edge
 *   services, yielding a moderate rather than low epsilon. A neutrality
 *   reading assessing the identical referent would author a far higher
 *   epsilon; that divergence across readings over one fixed referent is the
 *   point of the kernel decomposition, not an inconsistency. KEY AGENTS (by
 *   structural relationship): See commentary.key_agents. The arrangement's
 *   parties are the incumbent operators who administer it, the large
 *   platforms who purchase position within it, the unfunded edge services and
 *   end users who bear its performance costs, the municipal networks excluded
 *   as an alternative path, the regulator adjudicating which reading governs,
 *   and the advocacy coalition pressing the rival reading from outside the
 *   rulemaking majority.
 *
 * KEY AGENTS:
 *   - - incumbent_broadband_isps: Agenda-setter (institutional/arbitrage) — administers the prioritization regime, collects fast-lane and interconnection fees directly, and funds the litigation and lobbying that defend the permissive interpretation
 *   - - prioritized_large_edge_platforms: Beneficiary (powerful/mobile) — purchases priority and premium interconnection, gaining measurable performance advantage over unfunded rivals; pays into the regime they benefit from
 *   - - unfunded_edge_services: Payer (moderate/constrained) — startups and small services reaching users over the same last-mile plants without fast-lane budgets, absorbing degraded peak-hour performance relative to funded competitors
 *   - - best_effort_end_users: Payer with secondary beneficiary position (organized/constrained) — households absorbing congestion-driven quality variance while indirectly sharing in network expansion funded by tier revenue
 *   - - municipal_broadband_networks: Excluded (moderate/trapped) — city-owned networks that could offer an alternative access path but are barred or discouraged by state statutes aligned with incumbent interests
 *   - - telecom_regulator: Observer (institutional/analytical) — adjudicates which reading governs; currently holds the permissive line after reclassification and successful court defense of repeal, retaining authority to reverse
 *   - - net_neutrality_advocates: Excluded (organized/constrained) — public-interest groups, academics, and edge-company coalitions pressing the neutrality reading through comments, litigation, and state initiatives, dependent on the very networks whose governance they contest
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, 0.52).
domain_priors:suppression_score(tcp_ip_interpretation__prioritization_reading, 0.66).
domain_priors:theater_ratio(tcp_ip_interpretation__prioritization_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__prioritization_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__prioritization_reading, "Paid Prioritization as Network Management (Prioritization Reading of TCP/IP)").
narrative_ontology:topic_domain(tcp_ip_interpretation__prioritization_reading, "technological/legal/regulatory").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__prioritization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__prioritization_reading, '3d28e9b8-eb20-40cf-9e36-c3f3238907d5').
narrative_ontology:cs_kernel_codification('3d28e9b8-eb20-40cf-9e36-c3f3238907d5', fixed_text).
narrative_ontology:cs_authority_grounding('3d28e9b8-eb20-40cf-9e36-c3f3238907d5', extraction).
narrative_ontology:cs_interpretation_layer_present('3d28e9b8-eb20-40cf-9e36-c3f3238907d5').
narrative_ontology:cs_reading_relation('3d28e9b8-eb20-40cf-9e36-c3f3238907d5', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('3d28e9b8-eb20-40cf-9e36-c3f3238907d5', tcp_ip_interpretation__zero_rating_reading, influences).
narrative_ontology:cs_axiom('3d28e9b8-eb20-40cf-9e36-c3f3238907d5', foundational, best_effort_is_floor_not_ceiling).
narrative_ontology:cs_axiom_status(best_effort_is_floor_not_ceiling, holdable).
narrative_ontology:cs_axiom_grounding('3d28e9b8-eb20-40cf-9e36-c3f3238907d5', best_effort_is_floor_not_ceiling, conventional).
narrative_ontology:cs_axiom('3d28e9b8-eb20-40cf-9e36-c3f3238907d5', foundational, service_differentiation_is_legitimate_network_management).
narrative_ontology:cs_axiom_status(service_differentiation_is_legitimate_network_management, holdable).
narrative_ontology:cs_axiom_grounding('3d28e9b8-eb20-40cf-9e36-c3f3238907d5', service_differentiation_is_legitimate_network_management, instrumental).
narrative_ontology:cs_reference_frame('3d28e9b8-eb20-40cf-9e36-c3f3238907d5', permitted_differentiation_over_best_effort_core).
narrative_ontology:cs_drift_state('3d28e9b8-eb20-40cf-9e36-c3f3238907d5', post_repeal_paid_fast_lane_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('3d28e9b8-eb20-40cf-9e36-c3f3238907d5', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, incumbent_broadband_isps).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, prioritized_large_edge_platforms).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, unfunded_edge_services).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, best_effort_end_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, municipal_broadband_networks).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, best_effort_end_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate the last-mile networks over which all edge traffic reaches households. Implement managed quality-of-service classes and sell fast-lane and premium-interconnection arrangements to edge providers, collecting the associated fees directly. Fund trade associations, commissioned studies, litigation, and lobbying that defend the permissive interpretation in regulatory proceedings and courts. Because they own the physical plant, they can restructure or reprice the arrangement at will and remain profitable under either reading.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, incumbent_broadband_isps, agenda_setter,
    institutional, generational, arbitrage, national).

% Large video, streaming, cloud, and gaming firms that pay for transmission priority or premium interconnection. The purchased priority delivers measurably better peak-hour performance than unfunded rivals obtain on the same networks, converting an expense into competitive advantage. They can shift traffic across content delivery networks, negotiate private interconnection, and absorb the fees at scale; their payments are a major revenue component of the arrangement they sit inside.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, prioritized_large_edge_platforms, beneficiary,
    powerful, biographical, mobile, global).

% Startups and small services — telehealth, independent streaming, multiplayer games, education tools — that reach users over the same last-mile plants without fast-lane budgets. During congestion their traffic queues behind prioritized flows, degrading the experience they can offer relative to funded competitors. Content delivery networks and dedicated transit reduce but do not eliminate the gap, and virtually no alternative reaches the incumbent's installed subscriber base. They coordinate episodically through joint rulemaking comments but lack durable structural leverage.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, unfunded_edge_services, payer,
    moderate, biographical, constrained, global).

% Households buying broadband on best-effort terms. When capacity is allocated to prioritized traffic, their unprioritized applications absorb the resulting delay and jitter, concentrated in evening peaks. They also share indirectly in network expansion that tier revenue helps fund, and they express preference through consumer complaints, public-comment campaigns, and switching where a second provider exists — which in most markets is rare. Their bills do not itemize which traffic arrived late or why.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, best_effort_end_users, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, best_effort_end_users, beneficiary).

% City- and community-owned networks that could offer an additional access path and compete on open, undifferentiated service terms. State statutes — lobbied for by incumbent-aligned coalitions — bar or heavily condition municipal deployment in much of the country. Their exclusion removes the principal structural alternative to incumbent last-mile control, which is why they sit behind barriers they did not build and cannot vote away locally.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, municipal_broadband_networks, excluded,
    moderate, generational, trapped, local).

% Adjudicates which reading of the architecture governs domestic traffic management. After reclassifying broadband and repealing the non-discrimination rules, it successfully defended the permissive line in court and now presides over a regime of case-by-case review. It takes testimony from every other seat, commissions economic and engineering studies, and retains legal authority to re-impose non-discrimination rules — the pivot on which the arrangement's continuation turns.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, telecom_regulator, observer,
    institutional, generational, analytical, national).

% Public-interest organizations, academic network researchers, and edge-company coalitions pressing the neutrality reading: that the end-to-end principle requires non-discriminatory forwarding. They operate through rulemaking comments, litigation, state-legislative initiatives, and public mobilization rather than through formal seats in the current rulemaking majority. They depend for their own operations and reach on the very networks whose governance they contest, which bounds their leverage.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, net_neutrality_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__prioritization_reading, incumbent_broadband_isps).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__prioritization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages scarce, shared last-minute capacity among heterogeneous traffic flows — latency-sensitive interactive traffic versus bulk transfers — and channels a revenue stream toward continuous capacity expansion, without a central allocator deciding which flows deserve quality.
% TRANSFER_FUNCTION: Moves money from edge services (directly, via fast-lane and interconnection fees) and from end users (via subscription prices that embed network costs) to incumbent broadband operators; moves transmission priority toward paying origins and residual best-effort performance toward everyone else.
% ABSENT_VOICES: Net-neutrality advocates, unfunded-edge startup founders, and municipal broadband operators would object to the arrangement's terms and are largely outside the current rulemaking conversation, which is dominated by incumbent operators and the large platforms able to pay for position. State-level dissent survives in statute and litigation but holds no seat in federal proceedings.
% DISAPPEARANCE_RATIONALE: If the permissive reading lost force overnight and strict non-discrimination governed instead, fast-lane and premium-interconnection revenue would vanish, edge-provider cost structures and CDN interconnection markets would reprice, ISP investment plans and product tiers would be rebuilt around flat service, state preemption fights and litigation postures would invert, and the traffic-economics layer of the commercial internet would reorganize around undifferentiated delivery.
% FOUNDING_PROBLEM: Packet-switched networks share finite links among competing flows with no central allocator: TCP/IP's designers specified best-effort delivery and pushed quality assurance upward to endpoints and applications. The recurring problem this reading addresses is how to allocate scarce peak-hour capacity among latency-sensitive and bulk traffic, and how to finance continuous capacity expansion, in that decentralized design.
% FOUNDING_PROBLEM_CORROBORATION: The underlying scarcity-management problem is corroborated from outside the benefiting parties: the network-engineering literature on queueing and active queue management, the IETF's differentiated-services standardization work that predates the commercial fight, and academic telecommunications economics on peak-load costs all attest it, and none of those sources sits inside the ISP beneficiary set. The further claim that PAID fast lanes specifically — rather than need-based quality classes — are required to solve it is attested almost exclusively by the incumbent operators and their trade associations; that portion of the genealogy is self-attested, and the investment_causality omega exists to test it.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__prioritization_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__prioritization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__prioritization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tcp_ip_interpretation__prioritization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__prioritization_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tcp_ip_interpretation__prioritization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tcp_ip_interpretation__prioritization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.52: moderate by this reading's own lights over the fixed referent. The reading holds that differentiated delivery solves a real problem (peak-hour congestion among heterogeneous flows) and that tier revenue plausibly funds capacity, but it concedes — in its own expected structural delta — that unfunded edge services are disadvantaged and that some fee flow prices market position rather than marginal cost. Suppression is 0.66 and is authored as a RAW STRUCTURAL PROPERTY, unscaled by power or scope: the regime's persistence depends on winning interpretive contests (litigation defending repeal, preemption of state non-discrimination statutes, statutory barriers to municipal alternatives), not on participant preference. Theater ratio is 0.40: queue engineering and interconnection management are real, but a growing share of 'network management' activity is product-tier marketing and lobbying. Accessibility collapse is 0.50 — alternatives (CDN multi-homing, dedicated transit, satellite and municipal access) partially survive once the regime is understood, but each is costly and none fully escapes incumbent last-mile control. Resistance is 0.65 — the neutrality movement, state legislatures, and edge-coalition litigation constitute sustained, organized pushback.
 *   
 *   The measurement series runs on ONE SHARED TIME GRID (2005, 2010, 2015, 2018, 2021, 2025) with every tracked metric authored at every point. The series is deliberately non-monotonic: base_extractiveness dips at 2015 (0.29) because the Title II reclassification temporarily banned paid prioritization, contracting the arrangement's operation, then rebounds after the 2018 repeal as commercial fast-lane products and premium interconnection normalize. This is a political cycle driven by an external regulatory shock — a single visible cycle, not intermittent reinforcement; the oscillation is a side effect of jurisdictional politics, not itself the mechanism of burden. Suppression_requirement rises monotonically across the same grid because each neutrality surge forced the permissive coalition to build more durable defenses (court victories, state preemption, municipal-barrier statutes) — enforcement infrastructure maturing and hardening over the interval, which is why suppression_requirement rather than suppression alone traces the dynamic. Endpoint values match the base_properties scalars by construction.
 *   
 *   Coalition note (coalition check): the payer seats are fragmented but not without latent coalition power — joint rulemaking comments by startup coalitions and edge-company filings have moved proceedings before; their power atom ('moderate', 'organized') reflects episodic coordination rather than durable structure.
 *
 * PERSPECTIVAL GAP:
 *   Four seats experience one structure four ways, and the engine computes per-seat classifications from the structural data rather than from this claim. From the incumbent operator's seat the arrangement is infrastructure it built and manages: differentiation is engineering, fees are cost recovery, and the neutrality reading is an outsider's misreading of the architecture. From the unfunded edge service's seat the same structure is a toll gate on market entry: identical packets, divergent delivery, priced by budget. From the large platform's seat it is a purchasable moat — an expense, but one that converts into competitive advantage over smaller rivals, which is why a nominally paying seat computes beneficiary-side. From the household's seat it is variable quality with a bill that never itemizes the cause. The regulator and advocate seats see the whole contest and differ on which reading the text supports. No authored claim adjudicates these divergences; the directionalities and exits do.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations drive the derivation. Incumbent_broadband_isps sit nearest the beneficiary pole (they collect the fees and control the mechanism; their arbitrage-grade exit means they profit under either reading). Prioritized_large_edge_platforms are declared beneficiaries despite paying: the derivation reads their declaration, not their invoice, because the advantage they purchase exceeds the fee they pay relative to unfunded rivals. Unfunded_edge_services derive near the target pole: declared victims, constrained exit, moderate power. Best_effort_end_users carry a dual declaration (payer with secondary beneficiary), which the derivation alone would land near symmetric (~0.5); the explicit override corrects this to 0.72 because the two sides of their position are not symmetrical in kind — the degradation is immediate, certain, and concentrated in peak hours, while the investment upside is diffuse, contingent, and unverifiable at the household level. Municipal_broadband_networks derive near the target pole through their victim declaration and trapped exit: the regime's enforcement coalition maintains the statutory barriers they sit behind. Directionality overrides are used ONLY where the derivation would err (the dual-role household seat); everywhere else the structural data speaks.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — allocating scarce peak-hour capacity among heterogeneous flows and financing continuous capacity expansion in a decentralized packet network — remains live: congestion recurs every evening peak and capacity costs recur every upgrade cycle. Accordingly, mandatrophy_resolved is NOT declared, and the R5 interview records status 'live'. The classification discipline cuts both ways here. The genuine coordination function (real queue management, real congestion, real funding needs) blocks a snare mislabel: the coordination story is not cover, and authoring this as pure extraction would erase the engineering substance that a neutrality regime must still perform somehow. Conversely, the conceded asymmetric burden — named payers, disadvantaged unfunded edge, suppressed municipal alternatives — blocks a rope mislabel: authoring this as pure coordination would launder the position-selling component as cost recovery. Tangled_rope is the claim that honors both halves. The omega variables carry the drift conditions: adverse resolution of investment_causality or management_discrimination_boundary pushes the computed type toward snare; negligible edge_handicap_magnitude pushes it toward rope. The engine owns that computation; this story supplies the structure it computes from.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of kernel tcp_ip_interpretation: does the architecture''s best-effort design PERMIT differentiated service as network management (this reading), REQUIRE non-discrimination (neutrality_reading), or permit selective sponsored exemptions (zero_rating_reading)?',
    'Architectural analysis of what the RFC 791/793 service model commits to versus leaves open, combined with comparative outcomes across jurisdictions that have adopted each reading (e.g., regimes that permit traffic management but ban paid tiers).',
    'Under the neutrality reading the same referent loses its coordination cover and computes toward pure extraction with a markedly higher epsilon; under the zero-rating reading the victim set shifts toward non-sponsored content. This story''s classification holds only within the prioritization reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: this story instantiates one of three declared readings of the TCP/IP interpretation kernel.').

omega_variable(
    investment_causality,
    'Does fast-lane and premium-interconnection revenue cause incremental network investment, or does it substitute for investment that subscriber growth and ordinary capital planning would fund anyway?',
    'Panel studies of ISP capital expenditure against prioritization-related revenue, controlling for subscriber growth, technology upgrade cycles, and universal-service subsidies.',
    'If substitution dominates, the coordination justification thins and effective extraction rises sharply, drifting the computed type toward snare; if causal, a substantial share of the measured burden is genuine funding cost and the tangled_rope reading is stabilized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(investment_causality, empirical, 'Whether prioritization revenue funds new capacity or replaces it.').

omega_variable(
    management_discrimination_boundary,
    'Where does need-based congestion management (queue discipline applied by traffic class) end and ability-to-pay discrimination begin, and does the operative regime stay on the management side?',
    'Disclosure and audit of deployed queueing policies and interconnection agreements: whether priority keys to application need (latency sensitivity, congestion state) or simply to payment.',
    'If priority keys to payment rather than need, the coordination function becomes cover and the constraint recomputes toward snare; if keyed to need, the tangled_rope reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(management_discrimination_boundary, conceptual, 'Boundary between legitimate QoS management and paid discrimination.').

omega_variable(
    edge_handicap_magnitude,
    'How large is the realized performance and market-entry handicap on unfunded edge services, as opposed to the hypothetical disadvantage asserted in advocacy on both sides?',
    'Longitudinal measurement studies comparing latency, throughput, and startup survival for paying versus non-paying origins on the same last-mile plants.',
    'A negligible realized handicap would thin the victim set and drift the computed type toward rope; a severe one raises effective extraction for the payer seats and hardens the tangled_rope or worse verdict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(edge_handicap_magnitude, empirical, 'Magnitude of the disadvantage borne by edge services without fast-lane budgets.').

omega_variable(
    authority_grounding_framing,
    'Is the authority enforcing this reading grounded in engineering expertise (an independent professional consensus that differentiation is sound practice) or in commercial interest (incumbents funding the defense of the permissive line because they profit from it)?',
    'Trace the provenance of the technical justifications offered in rulemakings and litigation: independent engineering-body endorsement versus industry-commissioned studies and trade-association filings.',
    'An expertise framing yields a different commitment-system classification than the extraction framing authored here; the cs_pattern verdict flips with the framing, so this omega carries the framing under-determination explicitly rather than leaving it implicit in the declared values.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'Under-determination between expertise-grounded and extraction-grounded framings of the same authority structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__prioritization_reading, 2005, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t2005, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2005, 0.15).
narrative_ontology:measurement(tcp__tr_t2010, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(tcp__tr_t2015, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2015, 0.24).
narrative_ontology:measurement(tcp__tr_t2018, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2018, 0.32).
narrative_ontology:measurement(tcp__tr_t2021, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2021, 0.36).
narrative_ontology:measurement(tcp__tr_t2025, tcp_ip_interpretation__prioritization_reading, theater_ratio, 2025, 0.4).

% Extraction over time
narrative_ontology:measurement(tcp__be_t2005, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2005, 0.26).
narrative_ontology:measurement(tcp__be_t2010, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2010, 0.33).
narrative_ontology:measurement(tcp__be_t2015, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2015, 0.29).
narrative_ontology:measurement(tcp__be_t2018, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2018, 0.41).
narrative_ontology:measurement(tcp__be_t2021, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2021, 0.47).
narrative_ontology:measurement(tcp__be_t2025, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 2025, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t2005, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2005, 0.34).
narrative_ontology:measurement(tcp__su_t2010, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2010, 0.43).
narrative_ontology:measurement(tcp__su_t2015, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2015, 0.52).
narrative_ontology:measurement(tcp__su_t2018, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2018, 0.6).
narrative_ontology:measurement(tcp__su_t2021, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2021, 0.63).
narrative_ontology:measurement(tcp__su_t2025, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 2025, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__prioritization_reading, resource_allocation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'TCP/IP interpretation' per the epsilon-invariance principle. The label conflates three structurally distinct claims — mandated non-discrimination (neutrality_reading), permitted differentiation as management (this story), and selective sponsored exemptions (zero_rating_reading) — each with its own epsilon, beneficiary/victim structure, and classification. They are linked as a family via affects_constraints. Upstream/downstream structure: the neutrality reading historically supplied the foil against which this reading's defensive enforcement machinery was built (its rulemaking victories triggered the suppression buildup traced in the measurements), and this reading's commercial normalization creates downstream legitimacy pressure on the zero-rating reading (shared industry playbook), which is why the reading_relations differ by sibling. All three readings assess the SAME standing referent — the paid-prioritization arrangement — with reading-indexed epsilon values; the divergence in their authored epsilons over one fixed referent is the designed measurement, not an inconsistency.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tcp_ip_interpretation__prioritization_reading, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
