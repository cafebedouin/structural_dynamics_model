% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__zero_rating_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tcp_ip_interpretation__zero_rating_reading, []).

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
 *   constraint_id: tcp_ip_interpretation__zero_rating_reading
 *   human_readable: TCP/IP Zero-Rating Exemption Authorization (ISP Selective Data Cap Waiver)
 *   domain: telecommunications_law/internet_policy/network_governance
 *
 * SUMMARY:
 *   The zero-rating reading interprets TCP/IP's technical architecture to
 *   authorize ISP selective exemptions from data caps for sponsored content.
 *   This reading permits Internet Service Providers to offer zero-rated
 *   (data-cap-exempt) access to content partners in exchange for partnership
 *   fees, arguing that such arrangements are technically neutral
 *   optimizations that enable better service delivery in
 *   bandwidth-constrained networks. This is ONE reading of a contested kernel
 *   (tcp_ip_interpretation). Sibling readings include the neutrality_reading
 *   (TCP/IP mandates equal treatment regardless of content origin or sponsor)
 *   and the prioritization_reading (TCP/IP authorizes ISPs to prioritize
 *   based on technical criteria). The three readings coexist as competing
 *   policy interpretations of the same technical standard. The zero-rating
 *   reading is the subject of this constraint story. It exhibits classic
 *   tangled-rope structure: genuine coordination function (ISP-platform
 *   partnerships enable service delivery) combined with asymmetric extraction
 *   (incumbent platforms gain competitive moat, startup entrants face raised
 *   barriers). Over the measurement interval (approximately 2015-2021 in
 *   telecom regulatory time), extractiveness accumulated as zero-rating
 *   exemptions became standard ISP practice, suppression increased as
 *   competitive alternatives were foreclosed, and theater ratio declined
 *   slightly (the justification shifted from technical necessity toward
 *   explicit business partnership, reducing performative content).
 *
 * KEY AGENTS:
 *   - ISP Network Operators: Primary beneficiary (institutional/arbitrage) — capture partnership fees and traffic management coordination benefits
 *   - Incumbent Content Platforms (Netflix, Meta, Google, Apple): Primary beneficiary (powerful/arbitrage) — gain data-cap exemptions that competitors cannot match; raised competitive moat
 *   - Startup Content Providers: Primary victim (moderate/constrained) — cannot negotiate exemptions; face user pressure toward exempted platforms; raised entry barriers
 *   - Bandwidth-Constrained Users: Primary victim (powerless/trapped) — in single-ISP markets, zero-rating forces platform choice; no exit option
 *   - Net Neutrality Regulatory Principle: Structural victim (institutional/constrained) — original coordination function (open access guarantee) degraded to theater
 *   - Alternative Ecosystem Providers: Organized secondary actors (organized/mobile) — community networks, municipal broadband, satellite operators providing bypass pathways
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, 0.58).
domain_priors:suppression_score(tcp_ip_interpretation__zero_rating_reading, 0.64).
domain_priors:theater_ratio(tcp_ip_interpretation__zero_rating_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__zero_rating_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "TCP/IP Zero-Rating Exemption Authorization (ISP Selective Data Cap Waiver)").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "telecommunications_law/internet_policy/network_governance").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, '362fa0ba-9c3e-422a-a620-d5239a9fbf8a').
narrative_ontology:cs_kernel_codification('362fa0ba-9c3e-422a-a620-d5239a9fbf8a', formalized).
narrative_ontology:cs_authority_grounding('362fa0ba-9c3e-422a-a620-d5239a9fbf8a', extraction).
narrative_ontology:cs_interpretation_layer_present('362fa0ba-9c3e-422a-a620-d5239a9fbf8a').
narrative_ontology:cs_reading_relation('362fa0ba-9c3e-422a-a620-d5239a9fbf8a', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('362fa0ba-9c3e-422a-a620-d5239a9fbf8a', tcp_ip_interpretation__prioritization_reading, influences).
narrative_ontology:cs_axiom('362fa0ba-9c3e-422a-a620-d5239a9fbf8a', foundational, network_operator_exemption_authority).
narrative_ontology:cs_axiom_status(network_operator_exemption_authority, holdable).
narrative_ontology:cs_axiom_grounding('362fa0ba-9c3e-422a-a620-d5239a9fbf8a', network_operator_exemption_authority, instrumental).
narrative_ontology:cs_axiom('362fa0ba-9c3e-422a-a620-d5239a9fbf8a', foundational, technical_optimization_permits_discrimination).
narrative_ontology:cs_axiom_status(technical_optimization_permits_discrimination, holdable).
narrative_ontology:cs_axiom_grounding('362fa0ba-9c3e-422a-a620-d5239a9fbf8a', technical_optimization_permits_discrimination, empirically_contingent).
narrative_ontology:cs_reference_frame('362fa0ba-9c3e-422a-a620-d5239a9fbf8a', open_internet_with_isp_optimization_authority).
narrative_ontology:cs_drift_state('362fa0ba-9c3e-422a-a620-d5239a9fbf8a', contemporary_platform_concentration_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('362fa0ba-9c3e-422a-a620-d5239a9fbf8a', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, incumbent_content_platforms).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, isps_with_zero_rating_partnerships).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, sponsored_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, competitive_entry_startups).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, bandwidth_constrained_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, net_neutrality_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: BANDWIDTH-CONSTRAINED USER (SNARE) — Data-capped users in regions with limited ISP competition have no meaningful choice. Zero-rating exemptions for incumbent platforms force them toward those platforms regardless of preference. Unable to exit or organize; bears full cost of reduced choice and entrenchment.
constraint_indexing:constraint_classification(tcp_ip_interpretation__zero_rating_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: STARTUP CONTENT PROVIDER (SNARE) — Faces asymmetric barriers: cannot negotiate zero-rating deals (lacks scale and negotiating power) while competitors gain data-cap exemptions. Users are pushed toward exempted platforms by bandwidth economics. Exit is high-cost (relocate to exempted platform at loss of differentiation). Structurally unable to compete on equivalent terms.
constraint_indexing:constraint_classification(tcp_ip_interpretation__zero_rating_reading, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INCUMBENT PLATFORM (TANGLED ROPE) — Genuine coordination function: ISP partnerships solve the last-mile congestion problem and enable platforms to serve users in bandwidth-constrained regions. But also extracts: platform's exemption status creates moat against competitors and raises competitive entry barriers. Coordination benefit exists alongside asymmetric extraction.
constraint_indexing:constraint_classification(tcp_ip_interpretation__zero_rating_reading, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ISP NETWORK OPERATOR (ROPE) — Pure coordination from this view: zero-rating enables traffic management (prioritizing important services) and partnerships solve genuine congestion. ISP experiences the exemption framework as a coordination mechanism enabling business partnerships. Low suppression; agent has exit and agency (can choose partners, set terms).
constraint_indexing:constraint_classification(tcp_ip_interpretation__zero_rating_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: NET NEUTRALITY PRINCIPLE (PITON) — The net neutrality principle once functioned as a coordination mechanism ensuring open access. Zero-rating exemptions have degraded this principle into a theater: the regulatory framework (Telecommunications Act interpretation, FCC guidance) persists as a binding constraint but no longer serves its original coordination function. Maintained through institutional inertia and path dependence rather than functional necessity. Theater ratio high because enforcement decouples from stated principle.
constraint_indexing:constraint_classification(tcp_ip_interpretation__zero_rating_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ALTERNATIVE ECOSYSTEM (SCAFFOLD) — Community networks, municipal broadband, satellite internet (Starlink, Kuiper) and mesh protocols offer bypass pathways around ISP gatekeeping. These represent temporary scaffolding with sunset logic: as alternatives mature (5-10 year horizon), ISP gatekeeping through zero-rating loses leverage. Organized agents pursuing mobile exit paths.
constraint_indexing:constraint_classification(tcp_ip_interpretation__zero_rating_reading, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From engineering first-principles, ISP bandwidth management and content optimization partnerships are immutable features of network economics: data prioritization is inevitable given resource scarcity. This perspective risks naturalizing zero-rating exemptions as inherent to TCP/IP's own logic. However, the structural data reveals this is a false summit: the exemption framework is a policy reading of TCP/IP, not an immutable property of the protocol itself.
constraint_indexing:constraint_classification(tcp_ip_interpretation__zero_rating_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tcp_ip_interpretation__zero_rating_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(tcp_ip_interpretation__zero_rating_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(tcp_ip_interpretation__zero_rating_reading, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(tcp_ip_interpretation__zero_rating_reading, TR),
    TR >= 0.70.

:- end_tests(tcp_ip_interpretation__zero_rating_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, rising. Base extraction reflects asymmetric platform advantage (incumbents exempted, startups not) and user steering toward exempted content. Measurement trajectory shows accumulation: 0.35→0.48→0.58 across 6-year interval, indicating that as zero-rating became industry standard, the extraction mechanism solidified. By year 6, most major ISPs operated zero-rating partnerships, raising barriers materially. Suppression (0.64): High and rising. Barriers include: (1) startup inability to negotiate exemptions without massive scale, (2) user behavioral steering by bandwidth economics, (3) ISP market concentration reducing competitive pressure for neutrality, (4) lack of open-source or community alternative platforms at comparable scale. The trajectory 0.48→0.60→0.64 reflects increasing foreclosure of competitive pathways. Theater ratio (0.48): Low and declining. Unlike performative systems (e.g., CSR, token diversity initiatives), zero-rating justifications shifted from 'technical optimization' framing toward explicit 'business partnership' language, reducing rhetorical theater. The functional claim (data-cap exemption enables service) is substantive and testable, though contested.
 *
 * PERSPECTIVAL GAP:
 *   This reading instantiates deep perspectival divergence. The ISP sees Rope (coordination enabling partnership revenue). The incumbent platform sees Tangled Rope (coordination benefit + moat protection). The startup sees Snare (unable to compete). The bandwidth-constrained user sees Snare (forced choice). The net neutrality principle sees Piton (degraded regulatory function). The alternative ecosystem sees Scaffold (sunset pathway). The analytical observer risks seeing Mountain (network optimization as immutable). The most diagnostic gap: the ISP's rope vs. the startup's snare derive from identical base_properties, revealing that the classification depends entirely on structural position (beneficiary vs. victim, arbitrage vs. constrained). This gap is the core explanatory work of the zero-rating reading.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) map to power and exit options. ISP operators (institutional/arbitrage) have low d (~0.15): beneficiaries with exit options → low extraction against them. Incumbent platforms (powerful/arbitrage) have moderate-low d (~0.25): beneficiaries but with competitive pressures → moderate extraction. Startups (moderate/constrained) have high d (~0.78): victims without significant exit → high extraction. Users in single-ISP markets (powerless/trapped) have maximum d (~0.95): victims with no exit → maximum extraction. The sigmoid f(d) transforms these d values into experienced extractiveness chi. The piton perspective for net neutrality principle uses d~0.72 (observer position) but low chi because the principle itself is not an agent making choices — the piton classification comes from theater_ratio exceeding 0.7, not from high effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The zero-rating reading resolves mandatrophy by acknowledging that the same structural constraint (ISP exemption authority) exhibits tangled-rope characteristics: genuine coordination (partnerships enable service, traffic management) coexists with asymmetric extraction (incumbent moat, startup barriers). The mandatrophy question — is this coordination or extraction? — has a tangled-rope answer: both. The constraint is not misclassified as a snare (that would erase coordination). It is correctly classified as tangled rope because: (1) beneficiaries (ISPs, platforms) and coordination function exist, (2) victims (startups, users) and asymmetric extraction exist, (3) active enforcement required (FCC regulations, ISP contracts, technical implementations). The perspectival gap (ISP rope, startup snare, user snare) shows that tangled rope is the global constraint type — it contains local snare and rope regions depending on position.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_equivalent_access,
    'What constitutes ''equivalent access'' to content under zero-rating exemptions? Is differential data-cap treatment equivalent to discrimination?',
    'Comparative analysis of user experience: content speed, responsiveness, and practical usability with and without exemption; economic analysis of threshold where exemption becomes material competitive disadvantage',
    'If exemption threshold is low: reading treats zero-rating as technical traffic management (coordination). If threshold is high: reading treats exemption as economic gatekeeping (extraction). This distinction determines whether the constraint is Rope or Tangled Rope at the ISP perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_equivalent_access, empirical, 'Whether differential data-cap treatment constitutes material discrimination').

omega_variable(
    protocol_vs_policy_distinction,
    'Is zero-rating authorization an interpretation of TCP/IP protocol itself, or a policy layer imposed above protocol-agnostic infrastructure?',
    'Historical analysis of TCP/IP specification documents, RFC standards, and early Internet governance decisions. Determination of whether prioritization was ever contemplated as core protocol feature vs. implemented as later ISP policy choice.',
    'If protocol-intrinsic: zero-rating is Mountain (immutable). If policy-layer: zero-rating is a contingent institutional reading (Tangled Rope/Snare). This omega resolves whether the mountain perspective instantiates true natural law or false summit.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(protocol_vs_policy_distinction, conceptual, 'Boundary between TCP/IP protocol design and ISP policy interpretation').

omega_variable(
    competitive_moat_materiality,
    'What percentage of users in competition-constrained regions (single ISP availability) are materially influenced toward exempted platforms by zero-rating incentives?',
    'Market research: user data consumption patterns before/after zero-rating implementation; correlation between exemption availability and platform market share in single-ISP markets vs. multi-ISP markets',
    'If >40% influence threshold crossed: extraction mechanism is material (Snare/Tangled Rope confirmed). If <20%: effect may be negligible (reading moves toward Rope). This empirical measure determines whether the startup victim perspective reflects real structural disadvantage or theoretical concern.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(competitive_moat_materiality, empirical, 'Materiality of zero-rating moat effect on competitive entry').

omega_variable(
    kernel_reading_ambiguity_zero_rating_vs_neutrality,
    'This reading (zero-rating) and the sibling neutrality_reading both interpret the same TCP/IP kernel but authorize opposite exemptions. Does the zero-rating reading''s core premise foreclose the neutrality reading, or do both remain live policy options?',
    'Examination of whether zero-rating and neutrality can coexist in a single regulatory framework (e.g., zero-rating permitted for essential services but prohibited for competitive advantage) vs. whether they are logically incompatible (zero-rating necessarily violates neutrality principle)',
    'If coexist-capable: readings are coexists_with (live policy contest). If mutually exclusive: zero-rating forecloses neutrality or vice versa. This determines the cs_structure.reading_relations field for the neutrality sibling.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity_zero_rating_vs_neutrality, conceptual, 'Logical compatibility of zero-rating and neutrality readings of TCP/IP kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__zero_rating_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(zero_rating_tr_t0, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0, 0.52).
narrative_ontology:measurement(zero_rating_tr_t3, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 3, 0.5).
narrative_ontology:measurement(zero_rating_tr_t6, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 6, 0.48).

% Extraction over time
narrative_ontology:measurement(zero_rating_be_t0, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(zero_rating_be_t3, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 3, 0.48).
narrative_ontology:measurement(zero_rating_be_t6, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 6, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(zero_rating_su_t0, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(zero_rating_su_t3, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 3, 0.6).
narrative_ontology:measurement(zero_rating_su_t6, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 6, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__zero_rating_reading, resource_allocation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__prioritization_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, platform_competitive_moat).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, isp_market_concentration).

% DUAL FORMULATION NOTE:
% The zero-rating reading is one of three readings of the tcp_ip_interpretation kernel. The neutrality and prioritization readings are separate constraint stories with their own epsilon values, beneficiary/victim structures, and perspectives. The three are linked by network.affects_constraints and share the same kernel_id but different reading_id values. Decomposition is required because each reading authorizes different technical practices and produces different economic structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tcp_ip_interpretation__zero_rating_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
