% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__prioritization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tcp_ip_interpretation__prioritization_reading
 *   human_readable: TCP/IP Differentiated Service Quality: Prioritization Reading
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   The prioritization reading of TCP/IP interprets the protocol as
 *   permitting and legitimizing differentiated service quality, where network
 *   operators (ISPs) can implement paid fast lanes, traffic prioritization,
 *   and quality-of-service mechanisms in exchange for revenue that
 *   incentivizes infrastructure investment. This reading grounds itself in
 *   the economic imperative for network expansion: TCP/IP does not specify
 *   absolute equality of packet treatment, routers implement queueing
 *   disciplines, and scarce backbone capacity requires allocation mechanisms.
 *   Under this reading, permitting commercial prioritization aligns
 *   incentives — edge services that generate congestion pay for the capacity
 *   they consume; ISPs invest revenue in backbone expansion; all users
 *   benefit from better infrastructure. This reading directly contests two
 *   sibling readings: the neutrality_reading (all lawful packets receive
 *   equal treatment regardless of source, destination, or content) and the
 *   zero_rating_reading (ISPs cannot discriminate based on data origin or
 *   application type, including zero-rating schemes). The prioritization
 *   reading is currently instantiated in US policy (post-2017 FCC order
 *   classifying broadband as non-common-carrier, permitting prioritization
 *   absent specific harm) and in portions of European policy (permitted under
 *   specific conditions in BEREC guidelines). The constraint exhibits
 *   tangled_rope classification: genuine coordination function (solving
 *   network congestion, incentivizing investment) coexists with asymmetric
 *   extraction (unfunded services disadvantaged, access equity degraded,
 *   startup ecosystem compressed).
 *
 * KEY AGENTS:
 *   - Incumbent ISPs (institutional/arbitrage): Primary beneficiaries — capture fast-lane revenue, incentivized infrastructure investment, reduced regulatory burden
 *   - Premium Content Providers (institutional/arbitrage): Secondary beneficiaries — can purchase priority, guaranteed quality, competitive advantage over unfunded services
 *   - Unfunded Edge Innovators (powerless/trapped): Primary victims — startups, nonprofits, public interest services cannot afford prioritization; experience degraded service as cost
 *   - Public Interest Services (moderate/constrained): Secondary victims — health systems, libraries, community initiatives forced into negotiation; constrained by budget and political will
 *   - Broadband Access Equity (powerless/trapped): Tertiary victim (abstract collective) — universal affordable access degraded; rural and low-income users bear costs
 *   - Regulatory Bodies (organized/constrained): Governance actors — attempt to set boundary conditions; constrained by ISP political power and technical complexity
 *   - Network Equipment Manufacturers (powerful/constrained): Beneficiary conditional on regulatory stability — profit from ISP investment cycles; constrained by policy uncertainty
 *   - Analytical Observer (analytical/analytical): Civilizational perspective that risks naturalizing the prioritization reading as inherent to TCP/IP architecture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, 0.58).
domain_priors:suppression_score(tcp_ip_interpretation__prioritization_reading, 0.62).
domain_priors:theater_ratio(tcp_ip_interpretation__prioritization_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__prioritization_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__prioritization_reading, "TCP/IP Differentiated Service Quality: Prioritization Reading").
narrative_ontology:topic_domain(tcp_ip_interpretation__prioritization_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__prioritization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__prioritization_reading, '1a0307c4-871c-4c4d-9d6b-4ed56f492cc4').
narrative_ontology:cs_kernel_codification('1a0307c4-871c-4c4d-9d6b-4ed56f492cc4', formalized).
narrative_ontology:cs_authority_grounding('1a0307c4-871c-4c4d-9d6b-4ed56f492cc4', extraction).
narrative_ontology:cs_interpretation_layer_present('1a0307c4-871c-4c4d-9d6b-4ed56f492cc4').
narrative_ontology:cs_reading_relation('1a0307c4-871c-4c4d-9d6b-4ed56f492cc4', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('1a0307c4-871c-4c4d-9d6b-4ed56f492cc4', tcp_ip_interpretation__zero_rating_reading, influences).
narrative_ontology:cs_axiom('1a0307c4-871c-4c4d-9d6b-4ed56f492cc4', foundational, network_investment_incentive_necessary).
narrative_ontology:cs_axiom_status(network_investment_incentive_necessary, holdable).
narrative_ontology:cs_axiom_grounding('1a0307c4-871c-4c4d-9d6b-4ed56f492cc4', network_investment_incentive_necessary, empirically_contingent).
narrative_ontology:cs_axiom('1a0307c4-871c-4c4d-9d6b-4ed56f492cc4', foundational, edge_service_affordability_secondary).
narrative_ontology:cs_axiom_status(edge_service_affordability_secondary, holdable).
narrative_ontology:cs_axiom_grounding('1a0307c4-871c-4c4d-9d6b-4ed56f492cc4', edge_service_affordability_secondary, instrumental).
narrative_ontology:cs_reference_frame('1a0307c4-871c-4c4d-9d6b-4ed56f492cc4', isp_network_investment_priority).
narrative_ontology:cs_drift_state('1a0307c4-871c-4c4d-9d6b-4ed56f492cc4', contemporary_post_neutrality_uncertainty, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1a0307c4-871c-4c4d-9d6b-4ed56f492cc4', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, incumbent_isps).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, premium_edge_services).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, unfunded_edge_innovators).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, public_interest_services).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, broadband_access_equity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNFUNDED EDGE INNOVATORS (SNARE) — Cannot exit the prioritization scheme. Startup services, public health platforms, educational nonprofits lack capital to purchase fast-lane access. Trapped by resource asymmetry; bear full cost of degraded service quality. No coordination benefit perceived — only extraction.
constraint_indexing:constraint_classification(tcp_ip_interpretation__prioritization_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: PUBLIC INTEREST SERVICES (TANGLED ROPE) — Government health systems, public libraries, community broadband initiatives. Constrained by budget and political will to negotiate with ISPs. Experience mixed coordination (the prioritization system enables ISP network investment that benefits all users) and extraction (forced negotiation for quality that should be universal). Moderate experienced extraction due to some agency through regulatory appeal.
constraint_indexing:constraint_classification(tcp_ip_interpretation__prioritization_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PREMIUM CONTENT PROVIDERS (ROPE) — Netflix, Apple, Google, other deep-pocket services. Can afford fast lanes and benefit from guaranteed quality. Experience the constraint as coordination: paying for priority solves the mutual problem of congestion management and service quality assurance. Net beneficiaries with exit options (can relocate infrastructure, negotiate directly with ISPs globally). Negative effective extraction — the constraint benefits them.
constraint_indexing:constraint_classification(tcp_ip_interpretation__prioritization_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INCUMBENT ISPs (ROPE) — Comcast, Verizon, AT&T, others with fiber/spectrum assets. See prioritization as legitimate network management that incentivizes infrastructure investment. Extract revenue from premium services but also experience coordination: tiered pricing solves the collective-action problem of funding network expansion. Generate investment in backbone capacity that benefits all users. Net beneficiaries — negative effective extraction.
constraint_indexing:constraint_classification(tcp_ip_interpretation__prioritization_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: NETWORK EQUIPMENT MANUFACTURERS (TANGLED ROPE) — Cisco, Nokia, Juniper. Benefit from ISP investment incentives (new equipment sales for priority queuing, DPI, traffic shaping). Constrained by regulatory uncertainty (regulations could ban prioritization, eliminating the upgrade cycle). Mixed position: coordinating ISP investment (beneficial) while being constrained by policy volatility.
constraint_indexing:constraint_classification(tcp_ip_interpretation__prioritization_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: BROADBAND ACCESS EQUITY (SNARE) — The abstract structural good of universal affordable access. Trapped: prioritization shifts costs to those least able to pay for premium tiers. Rural users, low-income households, developing-world access initiatives bear full extraction with no voice. No beneficiary among individuals — only an aggregate collective good that cannot organize.
constraint_indexing:constraint_classification(tcp_ip_interpretation__prioritization_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 7: REGULATORY BODIES & CONSUMER ADVOCATES (TANGLED ROPE) — Organized agents (FCC, BEREC, consumer advocacy groups) attempt to govern prioritization through rules (net neutrality regulations, zero-rating bans, interconnection requirements). Constrained by ISP political power and technical complexity. Mixed function: protecting access equity (coordination) while negotiating extraction boundaries (enforcement). Experience moderate extraction due to organizational agency despite resource asymmetry.
constraint_indexing:constraint_classification(tcp_ip_interpretation__prioritization_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a technical/universal perspective, some form of prioritization appears inherent to packet networks: finite bandwidth always requires allocation; congestion always requires discrimination. The TCP/IP architecture itself contains no mechanism for absolute equality — routers implement queuing disciplines. Scarcity appears immutable. However, the structural data reveals this as a false summit: choices about queue discipline (FIFO, weighted fair queuing, priority) are architectural decisions, not laws of nature. The 'inevitability' of commercial prioritization naturalizes what is actually a contingent policy reading.
constraint_indexing:constraint_classification(tcp_ip_interpretation__prioritization_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tcp_ip_interpretation__prioritization_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tcp_ip_interpretation__prioritization_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tcp_ip_interpretation__prioritization_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tcp_ip_interpretation__prioritization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. ISPs extract substantial revenue from premium services and edge innovators; however, the extraction is not maximal (0.72+) because genuine network investment benefits exist and some edge services can afford prioritization. The trajectory over the interval (0.38 → 0.58) reflects accumulation: early prioritization schemes were limited and experimental (theater ratio high, ~0.52); as implementation matured and regulatory uncertainty decreased, ISPs expanded the scope, theater dropped (faster, simpler implementation), and extractiveness increased. Suppression (0.62): Moderate-high. Significant barriers exist to avoiding prioritization: edge services have no technical alternative to ISP networks for last-mile delivery; regulatory appeal is available but costly and slow; architectural alternatives (mesh, satellite, municipal fiber) are nascent. But suppression is not total (≥ 0.85) because organized players can negotiate, some jurisdictions have banned prioritization, and technical workarounds exist (application-level optimization). Theater ratio (0.48): Moderate-low. Implementation is relatively straightforward (DPI, queue priority, traffic shaping) and functional — the theaters are policy frames ('network investment incentives,' 'congestion management'), not implementation rituals. This differentiates prioritization_reading from piton classification. Claimed type (tangled_rope) reflects both genuine coordination (network capacity is scarce, allocation is necessary, incentivizing investment benefits users) and asymmetric extraction (costs fall disproportionately on powerless actors).
 *
 * PERSPECTIVAL GAP:
 *   The prioritization reading produces stark perspectival divergence. Incumbent ISPs and premium content providers see rope (legitimate coordination solving congestion) with negative effective extraction. Unfunded innovators and access equity see snare (pure extraction with no benefit). Public interest services see tangled_rope (mixed coordination and extraction). Regulatory bodies see tangled_rope with constrained agency (trying to govern extraction while acknowledging investment coordination). The analytical observer risks seeing mountain (inevitable technical necessity) despite the structural evidence of policy choice. The perspectival gap reveals that the prioritization reading's legitimacy claim (network investment requires prioritization incentives) is contested precisely because its distribution is asymmetric — benefits accrue to those with capital, costs to those without. The kernel ambiguity is whether this asymmetry is a feature of the architecture (inevitable) or a choice about which architectural option to instantiate (contingent policy reading).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from the structural position of each agent relative to prioritization. ISPs and premium content providers have low d (~0.10-0.20): they benefit from prioritization, have arbitrage exit options (can negotiate globally, relocate services), and are institutional. Unfunded innovators have high d (~0.85-0.95): they are victims, lack exit options (no alternative last-mile delivery), and are powerless. Public interest services have moderate d (~0.55-0.65): they are victims but have some agency through regulatory appeal and political mobilization. The sigmoid f(d) then scales these based on power and exit: high d + powerless → f(d) ≈ 1.42 (maximum experienced extraction); low d + institutional → f(d) ≈ -0.12 (negative experienced extraction). Scope modifier σ(S) adds a global amplification (1.2×) reflecting that prioritization systems scale: what begins as national ISP policy becomes architectural asymmetry at global scope, making edge of the affected services universally vulnerable. The resulting chi values range from negative (beneficiaries) to 0.95+ (trapped victims), confirming tangled_rope classification at multiple perspectives.
 *
 * MANDATROPHY ANALYSIS:
 *   The prioritization reading avoids mandatrophy by explicitly modeling both coordination (network investment, congestion management) and extraction (revenue capture, startup disadvantage, access equity degradation). No single agent perceives this as pure coordination or pure extraction — the snare perception from unfunded innovators is balanced against the rope perception from ISPs. The tangled_rope classification at multiple perspectives (public services, regulatory bodies, equipment manufacturers) confirms that asymmetric extraction coexists with genuine coordination function. The false-summit mountain classification in the analytical perspective is a diagnostic signal: the 'inevitable technical necessity' framing naturalizes what is actually a policy choice among architecturally feasible alternatives (FIFO, weighted fair queuing, round-robin, priority). Unmasking this false summit requires recognizing that the prioritization reading is one kernel interpretation, not a natural law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_ambiguity,
    'Is this reading of TCP/IP prioritization grounded in technical architecture (routers must implement queueing) or in policy choice (which queueing discipline is legitimate)?',
    'Comparative analysis: queuing discipline taxonomy (FIFO, weighted fair queuing, round-robin, priority, virtual clock) showing that prioritization is one choice among many architecturally feasible alternatives, each with different extractiveness profiles. Determine whether the TCP/IP specification mandates commercial prioritization or merely permits it.',
    'If architectural imperative: mountain classification is legitimate. If policy choice: mountain is false summit, and this reading instantiates a specific policy kernel distinct from neutrality_reading or zero_rating_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether prioritization is architectural necessity or policy choice').

omega_variable(
    congestion_proportionality,
    'Are the economic costs of network congestion actually distributed proportionally across edge services, or does the prioritization scheme extract from those who created congestion least?',
    'Network traffic analysis: identify congestion sources; correlate with beneficiaries and victims of fast-lane arrangements. Test whether prioritization charges accumulate on services proportional to their contribution to congestion.',
    'If proportional: tangled_rope classification holds; extraction is reasoned as congestion management. If disproportional: snare classification strengthens; extraction is revealed as rent-seeking using congestion as pretext.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(congestion_proportionality, empirical, 'Whether prioritization costs are proportional to congestion contribution').

omega_variable(
    infrastructure_investment_causality,
    'Do prioritization schemes and fast-lane revenue actually cause ISP network investment, or do they primarily redirect profit without increasing total backbone capacity?',
    'Historical analysis: correlate ISP capital expenditure with introduction of prioritization; compare expansion rates before/after fast lanes; control for regulatory and competitive pressure. Separate causation from correlation.',
    'If causal (investment increase): rope and tangled_rope perspectives confirmed — genuine coordination benefit. If uncausal (profit redirection): extraction mechanism is revealed; rope perspective reflects false beneficiary claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_investment_causality, empirical, 'Whether prioritization drives actual network expansion').

omega_variable(
    architectural_exit_path,
    'Can alternative architectures (mesh routing, satellite, fixed wireless, municipal fiber, blockchain-based incentive layers) reduce dependence on ISP prioritization, and what timeline?',
    'Technical feasibility analysis of alternative last-mile and backbone architectures; cost-benefit modeling. Test whether escape from ISP prioritization authority is structurally possible within 5/10/20 year horizons.',
    'If exit possible: scaffold perspective applies (temporary constraint); constraining agents can anticipate exit. If exit blocked: snare and piton perspectives strengthen (lock-in is structural).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(architectural_exit_path, empirical, 'Feasibility of architectural exit from ISP prioritization control').

omega_variable(
    sibling_reading_kernel_unity,
    'Are the neutrality_reading and zero_rating_reading genuine alternatives grounded in the same contested TCP/IP kernel, or are they structurally separate constraints with separate kernels?',
    'Comparative constraint analysis: extract the core kernel claim each reading instantiates. If kernel is identical (TCP/IP permits/forbids/regulates service differentiation) and reading-specific axioms conflict, they are siblings. If kernels differ, they are separate constraint families.',
    'If unified kernel: reading_relations (forecloses/coexists_with/influences) apply. If separate kernels: network.affects_constraints links are causal/dependency edges, not reading relations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_kernel_unity, conceptual, 'Whether sibling readings share a single contested kernel').

omega_variable(
    regulatory_foreclosure_dynamics,
    'If jurisdictions adopt zero-rating_reading (banning prioritization), does this reading''s core legitimacy claim (ISPs need prioritization incentives for investment) become empirically unfalsifiable within that jurisdiction, thereby functionally foreclosing this reading despite its logical persistence?',
    'Comparative policy analysis: jurisdictions with net neutrality regulations + strong network investment vs. those with prioritization permitted but weak investment. Test whether foreclosure is logical (one reading''s axiom contradicts the other) or practical (regulatory context makes one reading unlivable).',
    'If logical foreclosure: reading_relations entry is forecloses. If practical foreclosure: reading is holdable but overridden in some jurisdictions; axiom status becomes jurisdiction-dependent, requiring cs_structure decomposition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(regulatory_foreclosure_dynamics, conceptual, 'Whether regulatory context logically or practically forecloses this reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__prioritization_reading, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcpip_prio_tr_t0, tcp_ip_interpretation__prioritization_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(tcpip_prio_tr_t8, tcp_ip_interpretation__prioritization_reading, theater_ratio, 8, 0.5).
narrative_ontology:measurement(tcpip_prio_tr_t15, tcp_ip_interpretation__prioritization_reading, theater_ratio, 15, 0.48).

% Extraction over time
narrative_ontology:measurement(tcpip_prio_be_t0, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(tcpip_prio_be_t8, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 8, 0.52).
narrative_ontology:measurement(tcpip_prio_be_t15, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 15, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(tcpip_prio_su_t0, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement(tcpip_prio_su_t8, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 8, 0.58).
narrative_ontology:measurement(tcpip_prio_su_t15, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 15, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__prioritization_reading, resource_allocation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__zero_rating_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, isp_interconnection_rent_extraction).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, edge_service_startup_moat).

% DUAL FORMULATION NOTE:
% The TCP/IP kernel ('What packet prioritization is legitimate?') contains three structurally distinct constraint readings with different ε values, beneficiary/victim distributions, and perspectives. This file instantiates the prioritization_reading (ε ≈ 0.58, Tangled Rope). Sibling readings (neutrality and zero-rating) are separate constraint stories with distinct extractiveness profiles and are linked via network.affects_constraints. The upstream constraint (isp_interconnection_rent_extraction) shows how prioritization mechanisms feed into larger structural extraction patterns. The downstream constraint (edge_service_startup_moat) models the compressed innovation ecosystem resulting from prioritization costs.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tcp_ip_interpretation__prioritization_reading, institutional, 0.12).
constraint_indexing:directionality_override(tcp_ip_interpretation__prioritization_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
