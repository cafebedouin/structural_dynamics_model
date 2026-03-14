% ============================================================================
% CONSTRAINT STORY: startup_ecosystem_accessibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_startup_ecosystem_accessibility, []).

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
 *   constraint_id: startup_ecosystem_accessibility
 *   human_readable: Startup Ecosystem Accessibility Barrier
 *   domain: economic/entrepreneurship
 *
 * SUMMARY:
 *   The startup ecosystem accessibility constraint describes the barrier
 *   structure that concentrates startup capital, founder networks, and
 *   success outcomes in geographic clusters (primarily Silicon Valley, New
 *   York, Boston) while excluding geographically dispersed and
 *   demographically underrepresented founders. This constraint exhibits
 *   features of both coordination (capital-allocation efficiency, knowledge
 *   spillovers, investor networks) and extraction (geographic gatekeeping,
 *   demographic filtering, network favoritism). The constraint is dynamic:
 *   distributed capital models, remote-work normalization, and regulatory
 *   attention are creating parallel pathways that provide a partial sunset
 *   mechanism, yet the core barrier persists and intensifies through
 *   mechanisms like narrative persistence (the 'startup myth'), implicit
 *   bias, and capital concentration. The theater ratio (0.65) reflects
 *   significant performative elements: startup media celebrates diversity
 *   narratives while capital flows remain concentrated; tech conferences hold
 *   panels on access while deal flow remains gatekept; regulatory agencies
 *   create founder diversity initiatives that operate at the margins of
 *   capital allocation.
 *
 * KEY AGENTS:
 *   - Geographically Isolated Entrepreneurs: Primary victim (powerless/trapped) — face material barriers (capital scarcity, lack of local investors, cost of relocation) with no exit option except geographic arbitrage (high cost)
 *   - Underrepresented Founder Demographics: Primary victim (powerless/trapped or identity_locked) — face implicit bias, network exclusion, and internalized suppression; exit requires identity assimilation or accepting lower access
 *   - Tier-1 Venture Capital: Primary beneficiary (institutional/arbitrage) — benefits from concentrated deal flow, founder selectivity, network effects; experiences gatekeeping as efficient coordination
 *   - Regional Startup Ecosystems: Mixed actor (moderate/constrained) — benefits from local coordination but loses top founders and capital to coastal hubs; suppression through narrative of 'necessity to go coastal'
 *   - Distributed Capital Models: Emerging organized actor (organized/mobile) — remote VCs, syndicates, micro-funds creating parallel pathways with sunset logic
 *   - Founder Myth Narrative: Institutional mechanism (institutional/arbitrage) — maintains constraint through cultural persistence; perpetuates archetype expectations
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees the constraint as contingent institutional arrangement, not natural law, but notes that decomposing into network necessity vs extraction, geographic concentration necessity, and bias persistence will be required for policy intervention
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(startup_ecosystem_accessibility, 0.58).
domain_priors:suppression_score(startup_ecosystem_accessibility, 0.62).
domain_priors:theater_ratio(startup_ecosystem_accessibility, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(startup_ecosystem_accessibility, extractiveness, 0.58).
narrative_ontology:constraint_metric(startup_ecosystem_accessibility, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(startup_ecosystem_accessibility, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(startup_ecosystem_accessibility, tangled_rope).
narrative_ontology:human_readable(startup_ecosystem_accessibility, "Startup Ecosystem Accessibility Barrier").
narrative_ontology:topic_domain(startup_ecosystem_accessibility, "economic/entrepreneurship").

domain_priors:requires_active_enforcement(startup_ecosystem_accessibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(startup_ecosystem_accessibility, established_venture_capital).
narrative_ontology:constraint_beneficiary(startup_ecosystem_accessibility, network_connected_founders).
narrative_ontology:constraint_beneficiary(startup_ecosystem_accessibility, elite_university_networks).
narrative_ontology:constraint_victim(startup_ecosystem_accessibility, geographically_isolated_entrepreneurs).
narrative_ontology:constraint_victim(startup_ecosystem_accessibility, underrepresented_founder_demographics).
narrative_ontology:constraint_victim(startup_ecosystem_accessibility, capital_constrained_regions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: GEOGRAPHIC OUTSIDER (SNARE) — Founders outside major tech hubs face structural barriers: VC capital is concentrated in Silicon Valley, New York, and Boston; network access requires physical presence or elite credentials; local capital sources are scarce. Exit requires relocation (high cost), which functions as a material trap. The constraint extracts through geographic gatekeeping.
constraint_indexing:constraint_classification(startup_ecosystem_accessibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: UNDERREPRESENTED FOUNDER (SNARE) — Founders from underrepresented demographics (women, racial minorities, non-English-speaking backgrounds) face compounded barriers: implicit bias in investor evaluation, exclusion from informal deal networks, higher due diligence friction, stereotype threat in pitching contexts. Exit requires either assimilating to dominant founder profile (identity cost) or accepting lower access to capital. Maximum experienced extraction.
constraint_indexing:constraint_classification(startup_ecosystem_accessibility, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: REGIONAL STARTUP ECOSYSTEM (TANGLED ROPE) — Smaller regional ecosystems (Austin, Denver, Toronto) experience both coordination benefits (shared local knowledge, regional investor networks, startup communities) and extraction (capital flows disproportionately to coastal hubs; second-tier founders exit region if funding succeeds; brain drain to dominant centers). High suppression due to narrative that 'serious startups must go to Silicon Valley' limits local capital formation.
constraint_indexing:constraint_classification(startup_ecosystem_accessibility, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: TIER-1 VENTURE CAPITAL (ROPE) — Elite VC firms benefit from the concentrated ecosystem through network effects, deal flow concentration, and founder selectivity. The barrier to outside founders actually functions as a coordination mechanism for them: concentrated deal flow in known networks reduces search costs and concentrates their capital efficiently. They experience the constraint as pure coordination, not extraction, because their position enables them to arbitrage away from the barrier.
constraint_indexing:constraint_classification(startup_ecosystem_accessibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: REMOTE-FIRST AND DISTRIBUTED CAPITAL (SCAFFOLD) — New funding models (remote-first VCs, micro-funds, syndicates, Republic.co, SeedInvest) are creating parallel pathways that bypass geography and network gatekeeping. As distributed capital matures, it provides a sunset mechanism: founders no longer absolutely require coastal hub access. Suppression is declining through technological and operational innovations (video pitching, distributed due diligence, founder networks online). Theater is moderate: the distributed model has real functional verification (portfolio tracking, outcome data) but also marketing theater ('democratizing access').
constraint_indexing:constraint_classification(startup_ecosystem_accessibility, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: FOUNDER MYTH NARRATIVE (PITON) — The cultural narrative of 'startup = young person in Silicon Valley garage → unicorn' is largely theatrical persistence of a 1990s-2000s pattern. Modern successful founders are distributed, older, from diverse backgrounds, and often don't fit the archetype. Yet the narrative persists in startup media, pitch books, and founder self-conception, constraining perceived possibilities even as empirical founder demographics have broadened. The constraint is maintained through cultural inertia and narrative repetition, not through active gatekeeping.
constraint_indexing:constraint_classification(startup_ecosystem_accessibility, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational view, the startup ecosystem serves a genuine coordination function (capital allocation to innovation, founder community formation, knowledge spillovers) while simultaneously extracting through gatekeeping, network favoritism, and geographical concentration. The constraint is not naturalizable — it is a contingent institutional arrangement (venture capital fund structures, accredited investor rules, urban real estate costs) that both enables and restricts value creation. The suppression is enforceable (legal restrictions, capital location, information asymmetry) but also eroding through technology and regulatory change.
constraint_indexing:constraint_classification(startup_ecosystem_accessibility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(startup_ecosystem_accessibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(startup_ecosystem_accessibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(startup_ecosystem_accessibility, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(startup_ecosystem_accessibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(startup_ecosystem_accessibility, TR),
    TR >= 0.70.

:- end_tests(startup_ecosystem_accessibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The startup ecosystem extracts through capital concentration (1-2% of founders access tier-1 VC, controlling 80%+ of capital), geographic gatekeeping (VC is concentrated in 3 metros), and demographic filtering (women receive 2% of venture funding, non-white founders 4%). However, extraction is not maximal because: (a) distributed capital alternatives exist and are growing, (b) some founders succeed outside the gated ecosystem through bootstrap/angel capital, (c) the extraction flow is not perfectly enforced. Suppression (0.62): Moderate-high. Barriers include capital scarcity outside major hubs (material), network exclusion (social), implicit bias in evaluation (cognitive), relocation costs (economic), visa restrictions for international founders (legal). Suppression is significant but not absolute — some founders find workarounds, and suppression is declining as distributed capital grows. Theater ratio (0.65): Moderate-high. Substantial performative activity includes: startup media celebrating diversity while capital remains gatekept; founder diversity initiatives operating at margins of capital allocation; pitch books and conferences emphasizing innovation while perpetuating founder archetype expectations; 'mission-driven' VCs claiming impact while maintaining tier-1 gatekeeping. Theater has increased over the measurement interval as narrative emphasis on diversity has grown while capital concentration has persisted (moral licencing effect).
 *
 * PERSPECTIVAL GAP:
 *   Geographic outsiders and underrepresented founders perceive the ecosystem as Snare (pure extraction, no exit option, maximum suppression). Tier-1 VC perceives it as Rope (coordination, low extraction from their position). Regional ecosystems perceive Tangled Rope (mixed coordination and extraction). Distributed capital proponents perceive Scaffold (temporary problem with sunset as remote-first and syndicates mature). The founder myth narrative community perceives Piton (outdated but persistent cultural expectation). The analytical observer perceives Tangled Rope overall, noting that the gap between Snare (outsider experience) and Rope (insider experience) is the signature of a genuinely extractive hybrid constraint. The constraint is NOT a mountain because the concentration is contingent on policies (capital location, visa restrictions, real estate costs), not on natural law. The constraint IS tangled rope at the systemic level because genuine coordination (capital allocation to innovation, founder networks) coexists with genuine extraction (gatekeeping, demographic filtering).
 *
 * DIRECTIONALITY LOGIC:
 *   Tier-1 VC (institutional/arbitrage) derives low d from beneficiary status + arbitrage exit: they can arbitrage away from the barrier if it becomes less useful. Geographic outsiders (powerless/trapped) derive high d from victim status + trapped exit: they cannot arbitrage away without paying full relocation/identity cost. Underrepresented founders may derive high d from victim + identity_locked exit if their exclusion is internalized (they believe they don't belong) or from victim + trapped exit if the barriers are material (they can't access capital). Regional ecosystems derive mixed d values: local coordination gives them some beneficiary status (d ≈ 0.40) but capital outflow gives them victim status (d ≈ 0.70), averaging to constrained position. The analytical observer derives d ≈ 0.72 (observer position in the sigmoid) because they see both coordination and extraction and cannot arbitrage either away — they must analyze the system as-is.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy is resolved by recognizing that the startup ecosystem is neither pure coordination (Rope) nor pure extraction (Snare), but a genuine hybrid (Tangled Rope) where both functions are required for the classification. The coordination function is capital allocation to innovation, founder community formation, and knowledge spillovers — without this function, startups would lack funding and networks. The extraction function is gatekeeping that prevents 99% of potential founders from accessing tier-1 capital, concentrating wealth and opportunity in geographic clusters and demographic profiles. The constraint persists because the gatekeeping (extraction) is claimed to be necessary for capital allocation efficiency (coordination), but this claim is partially true and partially extractive. The scaffolding perspective reveals that distributed capital is providing the coordination function with lower extraction, suggesting that the gatekeeping was contingent, not necessary. The analytical classification as Tangled Rope is justified by the presence of both beneficiaries (tier-1 VCs, network-connected founders) and victims (geographically/demographically excluded founders) with different exit options and power levels, combined with measurable suppression (barriers to access) and enforcement (capital concentration, implicit bias). The false summit risk is the claim that geographic/demographic concentration is a natural law of startup economics — structural data shows it is a contingent institutional arrangement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_necessity_vs_gatekeeping,
    'How much of the VC network gatekeeping is functionally necessary for capital allocation (reducing adverse selection) versus extractive gatekeeper rent-seeking?',
    'Comparative analysis of investor returns from network-referred deals vs. open-application deals; measurement of selection quality vs. deal flow concentration benefits; counterfactual modeling of distributed capital performance',
    'If mostly functional: constraint reclassifies as Rope from more perspectives (coordination is real). If mostly extractive: constraint reclassifies as Snare from more perspectives (gatekeeping is primary mechanism).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_necessity_vs_gatekeeping, empirical, 'Proportion of network gatekeeping that is functionally necessary versus rent-seeking').

omega_variable(
    geographic_concentration_necessity,
    'Is startup geographic concentration in major hubs a natural consequence of knowledge spillover and agglomeration benefits, or a contingent outcome of capital/visa/real estate policies?',
    'Historical analysis of pre-concentration startup ecosystems; cross-country comparison of capital distribution and startup success rates; measurement of agglomeration benefits in dispersed vs concentrated ecosystems',
    'If natural agglomeration: concentration is mountain-adjacent (immutable efficiency). If policy-contingent: concentration is snare (extractive arrangement that could be redesigned).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(geographic_concentration_necessity, empirical, 'Whether geographic concentration is natural agglomeration or policy-contingent').

omega_variable(
    implicit_bias_persistence,
    'Is implicit bias against underrepresented founders in VC evaluation systematic and persistent, or declining as VCs professionalize evaluation processes?',
    'Matched-sample analysis of pitcher demographics vs funding outcomes; longitudinal trend analysis of founder diversity in funded vs rejected cohorts; controlled pitch experiment (identical pitch, varied presenter demographics)',
    'If persistent and systematic: underrepresented founder snare is structural. If declining: suppression metric will decline and constraint type may shift toward rope/scaffold.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implicit_bias_persistence, empirical, 'Magnitude and trajectory of implicit bias against underrepresented founders').

omega_variable(
    distributed_capital_sufficiency,
    'Can distributed capital models (remote VCs, syndicates, micro-funds) actually provide sufficient capital depth and deal expertise to match tier-1 VC, or are they perpetually limited to early-stage/secondary-market deals?',
    'Comparative analysis of capital availability by stage, company geography, and founder background; tracking of distributed-capital-backed companies to Series A/B success rates; measurement of knowledge/network value versus capital provision',
    'If sufficient: scaffold sunset is real and constraint will decline. If limited: distributed capital is supplementary but not replacive, and snare/tangled rope will persist for ambitious founders.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(distributed_capital_sufficiency, empirical, 'Whether distributed capital models can provide sufficient depth to replace tier-1 VC').

omega_variable(
    suppression_internalization,
    'How much of the measured suppression is external (capital/location barriers) versus internalized (founders self-select out, believe they don''t belong, don''t attempt to compete)?',
    'Cohort analysis of founder application/rejection patterns; interview data on founder decision-making about ecosystem access; measurement of aspiration gap (desired vs attempted ventures by founder background)',
    'If highly internalized: suppression metric understates constraint severity for underrepresented founders (constraint is identity-locked, not just trapped). If mostly external: suppression is material barrier with potential policy fixes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Proportion of suppression that is external versus internalized').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(startup_ecosystem_accessibility, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(startup_access_tr_t0, startup_ecosystem_accessibility, theater_ratio, 0, 0.52).
narrative_ontology:measurement(startup_access_tr_t7, startup_ecosystem_accessibility, theater_ratio, 7, 0.62).
narrative_ontology:measurement(startup_access_tr_t15, startup_ecosystem_accessibility, theater_ratio, 15, 0.65).
narrative_ontology:measurement(startup_access_tr_t22, startup_ecosystem_accessibility, theater_ratio, 22, 0.64).

% Extraction over time
narrative_ontology:measurement(startup_access_be_t0, startup_ecosystem_accessibility, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(startup_access_be_t7, startup_ecosystem_accessibility, base_extractiveness, 7, 0.55).
narrative_ontology:measurement(startup_access_be_t15, startup_ecosystem_accessibility, base_extractiveness, 15, 0.58).
narrative_ontology:measurement(startup_access_be_t22, startup_ecosystem_accessibility, base_extractiveness, 22, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(startup_ecosystem_accessibility, resource_allocation).
narrative_ontology:boltzmann_floor_override(startup_ecosystem_accessibility, 0.18).
narrative_ontology:affects_constraint(startup_ecosystem_accessibility, wealth_concentration_dynamics).
narrative_ontology:affects_constraint(startup_ecosystem_accessibility, geographic_inequality_persistence).
narrative_ontology:affects_constraint(startup_ecosystem_accessibility, demographic_representation_inequality).

% DUAL FORMULATION NOTE:
% The startup ecosystem accessibility constraint decomposes into structurally distinct sub-constraints: (1) geographic_concentration (capital location + visa/real estate policies); (2) demographic_filtering (implicit bias + network exclusion); (3) capital_allocation_efficiency (information asymmetry + adverse selection). Each sub-constraint has different ε and different perspectives, but they reinforce each other in the combined ecosystem barrier. The family link indicates that changes to any one sub-constraint (e.g., distributed capital for geographic distribution, bias reduction for demographic filtering, improved signaling for capital allocation) will affect the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(startup_ecosystem_accessibility, powerful, 0.18).
constraint_indexing:directionality_override(startup_ecosystem_accessibility, moderate, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
