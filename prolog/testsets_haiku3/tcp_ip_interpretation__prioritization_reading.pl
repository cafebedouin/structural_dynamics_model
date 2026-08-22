% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__prioritization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   human_readable: TCP/IP Differentiated Service Quality (Prioritization Reading)
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the TCP/IP kernel: the
 *   interpretation that the protocol permits ISPs to implement differentiated
 *   service quality as a legitimate network management and investment
 *   mechanism. Under this reading, ISPs gain authority to create paid
 *   priority lanes; content providers with capital benefit from guaranteed
 *   performance; unfunded edge services and innovation-stage startups are
 *   disadvantaged by deprioritization. The constraint is claimed as tangled
 *   rope because it solves a genuine coordination problem (infrastructure
 *   investment incentives, congestion management) but does so asymmetrically,
 *   extracting from those without capital to invest in prioritization. The
 *   alternative neutrality_reading claims TCP/IP embodies an end-to-end
 *   principle that forbids such discrimination; the zero_rating_reading
 *   claims exemptions for sponsored content are authorized. These are not
 *   observables of the same constraint—they are three different constraints
 *   rooted in three different readings of the TCP/IP kernel. This story
 *   authors only the prioritization reading.
 *
 * KEY AGENTS:
 *   - ISP operators (agenda-setter, institutional power): control last-mile routing, interpret TCP/IP permissively, capture prioritization revenue.
 *   - Content providers with capital (beneficiary, powerful): can afford fast lanes, gain competitive advantage over unfunded rivals.
 *   - Edge service providers (payer, moderate power): smaller content, academic, and health networks bear deprioritization cost.
 *   - Independent startups (payer, powerless/identity-locked): face structural disadvantage; founding identity locked to free end-to-end routing assumption.
 *   - Unfunded public services (payer, powerless/trapped): public health, emergency, research networks cannot pay for prioritization.
 *   - Network engineers (observer, organized): divided on whether differentiation violates the original protocol design intent.
 *   - Regulatory authorities (observer, institutional): interpret whether the reading is permitted and can impose prohibitions.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, 0.68).
domain_priors:suppression_score(tcp_ip_interpretation__prioritization_reading, 0.52).
domain_priors:theater_ratio(tcp_ip_interpretation__prioritization_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__prioritization_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__prioritization_reading, "TCP/IP Differentiated Service Quality (Prioritization Reading)").
narrative_ontology:topic_domain(tcp_ip_interpretation__prioritization_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__prioritization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__prioritization_reading, '47f94a19-7429-44cc-8a0b-2aeb7fc8c967').
narrative_ontology:cs_kernel_codification('47f94a19-7429-44cc-8a0b-2aeb7fc8c967', distributed).
narrative_ontology:cs_authority_grounding('47f94a19-7429-44cc-8a0b-2aeb7fc8c967', extraction).
narrative_ontology:cs_interpretation_layer_present('47f94a19-7429-44cc-8a0b-2aeb7fc8c967').
narrative_ontology:cs_reading_relation('47f94a19-7429-44cc-8a0b-2aeb7fc8c967', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('47f94a19-7429-44cc-8a0b-2aeb7fc8c967', tcp_ip_interpretation__zero_rating_reading, influences).
narrative_ontology:cs_axiom('47f94a19-7429-44cc-8a0b-2aeb7fc8c967', foundational, isp_operational_discretion_over_routing).
narrative_ontology:cs_axiom_status(isp_operational_discretion_over_routing, holdable).
narrative_ontology:cs_axiom_grounding('47f94a19-7429-44cc-8a0b-2aeb7fc8c967', isp_operational_discretion_over_routing, conventional).
narrative_ontology:cs_axiom('47f94a19-7429-44cc-8a0b-2aeb7fc8c967', foundational, price_signals_efficient_resource_allocation).
narrative_ontology:cs_axiom_status(price_signals_efficient_resource_allocation, holdable).
narrative_ontology:cs_axiom_grounding('47f94a19-7429-44cc-8a0b-2aeb7fc8c967', price_signals_efficient_resource_allocation, empirically_contingent).
narrative_ontology:cs_reference_frame('47f94a19-7429-44cc-8a0b-2aeb7fc8c967', isp_network_management_authority).
narrative_ontology:cs_drift_state('47f94a19-7429-44cc-8a0b-2aeb7fc8c967', contemporary_platform_dominance_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('47f94a19-7429-44cc-8a0b-2aeb7fc8c967', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, isp_operators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, content_providers_with_capital).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, edge_service_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, independent_startups).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, unfunded_public_services).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, users).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__prioritization_reading, network_investment_efficiency).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__prioritization_reading, isp_operational_discretion).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Control the last-mile routing infrastructure and interpret TCP/IP as permitting differentiated QoS management. Under this reading, they can offer prioritized lanes to high-paying content providers, fund infrastructure upgrade through paid prioritization, and manage congestion by service class. They argue this reading is justified by network engineering necessity and enables sustained infrastructure investment.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, isp_operators, agenda_setter,
    institutional, generational, arbitrage, global).

% Large content platforms (streaming, CDN operators, major cloud services) can afford paid fast lanes and benefit from priority routing without degrading when network is congested. They gain competitive advantage over unfunded competitors. Can negotiate volume discounts and integration with ISP infrastructure.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, content_providers_with_capital, beneficiary,
    powerful, generational, arbitrage, global).

% Smaller content providers, academic institutions, health networks, public broadcasters bear the cost of deprioritization without fast-lane subscription. Their services experience degraded performance during congestion unless they pay for prioritization. Exit options are limited: they cannot build alternative last-mile infrastructure, reroute around ISP networks, or simply absorb the cost indefinitely.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, edge_service_providers, payer,
    moderate, biographical, constrained, global).

% New services and innovation-stage companies cannot afford paid prioritization during development. They face a structural disadvantage: their growth trajectory depends on user experience, which is constrained by deprioritization, but they lack revenue to pay for fast lanes. The business-model dependency on free end-to-end routing is part of their founding identity.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, independent_startups, payer,
    powerless, biographical, identity_locked, global).

% Public health networks, emergency services, academic research platforms, and public media services cannot compete for paid prioritization. They lack commercial revenue sources and are structurally barred from the fast-lane market. They depend entirely on best-effort service and bear congestion costs that ISP operators can now deliberately allocate to them.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, unfunded_public_services, payer,
    powerless, generational, trapped, global).

% The technical community that originally designed TCP/IP and maintains routing protocols. They provide testimony on what the protocol does and does not authorize, and whether differentiation aligns with or violates the protocol's original design intent. Divided: some argue differentiation is legitimate management, others argue it violates end-to-end principle.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, network_engineers, observer,
    organized, biographical, analytical, global).

% National telecom/internet regulators (FCC in US, Ofcom in UK, national telecoms authorities elsewhere) interpret whether TCP/IP as written permits or prohibits paid prioritization. Their decisions frame what ISPs are allowed to do and can impose mandates or prohibitions that alter the constraint's enforcement.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, regulatory_authorities, observer,
    institutional, generational, analytical, national).

% End consumers benefit from ISP infrastructure investment and network stability that paid prioritization funds. They also bear the cost indirectly: services they use either pay fast-lane fees (which raise consumer prices) or degrade in quality (if they cannot afford fast lanes). Choice is constrained by ISP monopolies in most last-mile markets.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, users, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__prioritization_reading, users, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__prioritization_reading, isp_operators).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__prioritization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allocates scarce network capacity through price signals: ISPs manage congestion and fund infrastructure upgrade by offering differentiated QoS; content providers can guarantee performance to users by purchasing priority lanes.
% TRANSFER_FUNCTION: Moves payment from capital-rich content providers to ISP operators, and implicitly transfers service quality from unfunded edge providers to funded ones, concentrating bandwidth on paid priority lanes during congestion.
% ABSENT_VOICES: Edge providers who lack capital, public institutions, and innovation-stage startups are structurally excluded from the negotiation: they cannot afford fast lanes and have no seat at the table where ISPs and large platforms set prioritization terms. Their objection would be that the reading transforms a public good (open network) into a privatized commodity.
% DISAPPEARANCE_RATIONALE: If paid prioritization were prohibited tomorrow, ISPs would lose a revenue stream and would need to fund infrastructure upgrade through other mechanisms (regulated rates, public investment, or accepting lower investment). Content platforms would compete on equal footing with new entrants. Services would operate under best-effort guarantees again. Innovation incentives and service deployment patterns would shift.
% FOUNDING_PROBLEM: Network capacity is finite and congestion creates performance degradation. ISP operators need incentives to build infrastructure at the scale and speed the Internet demands. Unprioritized networks create tragedy-of-the-commons incentives for under-investment.
% FOUNDING_PROBLEM_CORROBORATION: ISP operators and major content providers attest the infrastructure-funding problem is live and severe. Network engineers and regulatory authorities dispute the diagnosis: they attest that congestion is manageable under properly-engineered best-effort networks and that the 'investment' argument is sometimes a cover for rent extraction. Public testimony and technical literature from outside the ISP-operator coalition supports the disputed status.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__prioritization_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__prioritization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__prioritization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tcp_ip_interpretation__prioritization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__prioritization_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness is moderate-to-high (0.68 end-state) because ISPs extract prioritization fees that are decoupled from marginal cost of service, and because the reading authorizes them to degrade service to non-payers. Suppression is moderate (0.52) because the reading's persistence depends partly on active enforcement (technical implementation of priority queues, degradation of best-effort routes, regulatory justification) and partly on path-dependent lock-in (services that were built under the free end-to-end assumption cannot easily exit). Theater ratio is moderate-rising (0.25→0.41): the infrastructure-investment narrative is partly functional (real capital needs for network upgrade) and partly performative (justifying rent extraction). Accessibility_collapse is moderate (0.62): alternatives exist (regulatory mandate for open routing, public funding of ISP infrastructure, network-slicing without paid priority) but are politically difficult. Resistance is high (0.71) because unfunded services and the open-internet advocacy community actively oppose this reading. Measurement series show extractiveness rising as paid prioritization becomes operationalized and normalized, theater ratio rising as infrastructure justifications accumulate alongside deprioritization practice, and suppression rising as enforcement infrastructure hardens.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter and beneficiary seats experience tangled_rope coordination + subsidy; the payer seats experience tangled_rope as enforced extraction. From the ISP operator and large-platform perspective, differentiation solves a real coordination problem (fund infrastructure, manage congestion) and creates a stable equilibrium. From the edge-provider and startup perspective, the same constraint is coercive: they are forced to choose between paying extraction fees or accepting deprioritized service—there is no escape. The engine computes this gap from the structural data (beneficiary vs. victim roles, exit options, power asymmetry): the same reading produces different effective extraction and suppression profiles for different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   ISP operators: d ≈ 0.1–0.2 (beneficiary end; they set the rules and capture the revenue). Content providers with capital: d ≈ 0.25–0.35 (beneficiary, though slightly constrained; they pay for speed but receive net benefit and choice). Edge service providers: d ≈ 0.65–0.75 (target end; they pay involuntarily for deprioritization, have moderate power but constrained exit). Independent startups: d ≈ 0.85–0.95 (full target end; they bear extraction, have no power, and identity-locked exit makes d approach 1.0—exit means abandoning the founding business model). Unfunded public services: d ≈ 0.90–1.0 (full target; they are trapped and bear indefinite deprioritization cost). Regulatory authorities: d ≈ 0.5 (analytical observer; they pay no direct cost and receive no direct benefit, but their interpretation determines the constraint's persistence). Users sit near 0.55 (symmetric): genuine infrastructure benefit but also indirect cost through rising consumer prices or service degradation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is 'infrastructure investment needs and congestion management'—a real coordination challenge. The founding_problem_status is contested because ISPs claim the problem is live and severe, while the regulatory/technical community disputes the diagnosis: best-effort networks with proper engineering can manage congestion, and the 'investment gap' is sometimes a cover story for rent extraction. If the founding problem is truly dead (infrastructure is built out, best-effort networks are sufficient), then the reading persists as pure extraction—a snare, not a tangled rope. The mandatrophy risk: the prioritization reading will persist even after infrastructure is no longer a binding constraint, because ISPs benefit from the extraction and lack incentive to rescind it. The theater_ratio rising (functional infrastructure justification → performative defense of prioritization) is an early signal of this drift. A tangled_rope that loses its coordination function becomes a snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    infrastructure_investment_necessity,
    'Is differentiated service quality genuinely necessary to fund network infrastructure upgrade, or is it a rent-extraction mechanism layered onto real coordination needs?',
    'Comparative analysis of deployment patterns and infrastructure investment rates across jurisdictions with and without paid prioritization; longitudinal data on ISP capex as a function of whether paid prioritization is permitted.',
    'If differentiation is necessary (bandwidth scarcity, real upgrade costs), the reading is justified coordination and the extraction is legitimate cost-recovery. If infrastructure can be funded without differentiation (public funding, regulated rates, or ISP profits are sufficient), the reading is rent extraction and should be classified as snare, not tangled rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_investment_necessity, empirical, 'Whether infrastructure investment gap is real or cover story.').

omega_variable(
    reading_logical_status,
    'Does the prioritization reading coexist with the neutrality reading in a single regulatory framework, or does one logically foreclose the other?',
    'Formal analysis of the TCP/IP specification and its design intent; review of authoritative technical documentation and the protocol''s original design papers; examination of whether the end-to-end principle is a logically binding constraint on the protocol or a design recommendation that permits exceptions.',
    'If the readings logically foreclose each other, classifying both as ''live'' simultaneously is incoherent—the constraint family is over-determined. If the readings coexist (the protocol is ambiguous and different parties legitimately read it differently), the three-reading decomposition is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_logical_status, conceptual, 'Whether the prioritization and neutrality readings are logically incompatible or genuinely coexistent.').

omega_variable(
    startup_identity_lock_mechanism,
    'Is the identity-locking of independent startups to ''free end-to-end'' routing a structural feature of the reading, or is it a consequence of historical accident and business-model dependency?',
    'Post-shift qualitative research: if regulation mandated open prioritization and startups shifted to paid-priority business models, would the identity lock persist or dissolve? If identity persists even after constraints change, the lock is deeper than the reading itself.',
    'If the lock is reading-constitutive, it is part of the structural description and d for startups should approach 1.0. If the lock is historically contingent and would dissolve with constraint change, d could shift downward if the constraint changed—the reading is not the source of the lock.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(startup_identity_lock_mechanism, empirical, 'Whether startup identity-lock to free routing is structural or contingent on business-model history.').

omega_variable(
    suppression_internalization_depth,
    'Is the suppression of edge providers and startups structural (economic dependency on ISP last-mile, no alternative routing) or partly internalized (they have accepted the deprioritization expectation as normal)?',
    'Post-regulatory-change observation: if neutrality or open-routing were mandated and suppression mechanisms removed, would resistance emerge quickly (structural suppression) or would normalization persist (internalized suppression)? Historical comparison with past periods of open-routing norms.',
    'If suppression is largely internalized, the constraint''s effective suppression may be lower than the structural measure suggests—but also, removal of the constraint would not immediately resolve the subjective expectation. If suppression is structural, removal would quickly restore alternatives and resistance.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_depth, empirical, 'Whether measured suppression is structural barriers or internalized acceptance.').

omega_variable(
    reading_vs_regulation_mutual_constitution,
    'Does the prioritization reading emerge from technical reality (what TCP/IP can do), or from regulatory permission (what regulators allow ISPs to do), or are these mutually constitutive?',
    'Historical analysis of how regulatory decisions shaped ISP interpretation of the protocol, and how technical advocates shaped regulatory understanding. Examine whether ISPs could implement prioritization without regulatory permission and whether regulators could permit it without technical feasibility.',
    'If the reading is purely regulatory, prohibition would dissolve the constraint immediately. If the reading is technically emergent, prohibition would merely suppress expression while the underlying capability persists. If mutual, the reading is stable only as long as both elements hold—technical feasibility AND regulatory permission.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_regulation_mutual_constitution, conceptual, 'Whether the prioritization reading is grounded in technical reality, regulatory interpretation, or their mutual constitution.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__prioritization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__prioritization_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(tcp__tr_t0, observed).
narrative_ontology:measurement(tcp__tr_t4, tcp_ip_interpretation__prioritization_reading, theater_ratio, 4, 0.29).
narrative_ontology:measurement_basis(tcp__tr_t4, observed).
narrative_ontology:measurement(tcp__tr_t8, tcp_ip_interpretation__prioritization_reading, theater_ratio, 8, 0.33).
narrative_ontology:measurement_basis(tcp__tr_t8, observed).
narrative_ontology:measurement(tcp__tr_t12, tcp_ip_interpretation__prioritization_reading, theater_ratio, 12, 0.37).
narrative_ontology:measurement_basis(tcp__tr_t12, observed).
narrative_ontology:measurement(tcp__tr_t18, tcp_ip_interpretation__prioritization_reading, theater_ratio, 18, 0.4).
narrative_ontology:measurement_basis(tcp__tr_t18, observed).
narrative_ontology:measurement(tcp__tr_t25, tcp_ip_interpretation__prioritization_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(tcp__tr_t25, observed).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(tcp__be_t0, observed).
narrative_ontology:measurement(tcp__be_t4, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement_basis(tcp__be_t4, observed).
narrative_ontology:measurement(tcp__be_t8, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement_basis(tcp__be_t8, observed).
narrative_ontology:measurement(tcp__be_t12, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement_basis(tcp__be_t12, observed).
narrative_ontology:measurement(tcp__be_t18, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 18, 0.66).
narrative_ontology:measurement_basis(tcp__be_t18, observed).
narrative_ontology:measurement(tcp__be_t25, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(tcp__be_t25, observed).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement_basis(tcp__su_t0, observed).
narrative_ontology:measurement(tcp__su_t4, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 4, 0.42).
narrative_ontology:measurement_basis(tcp__su_t4, observed).
narrative_ontology:measurement(tcp__su_t8, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement_basis(tcp__su_t8, observed).
narrative_ontology:measurement(tcp__su_t12, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 12, 0.49).
narrative_ontology:measurement_basis(tcp__su_t12, observed).
narrative_ontology:measurement(tcp__su_t18, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 18, 0.51).
narrative_ontology:measurement_basis(tcp__su_t18, observed).
narrative_ontology:measurement(tcp__su_t25, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 25, 0.52).
narrative_ontology:measurement_basis(tcp__su_t25, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__prioritization_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__prioritization_reading, 0.18).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% The TCP/IP kernel is interpreted differently by three readings, each producing a distinct constraint with its own beneficiary/victim structure and extraction profile. The prioritization_reading (this file) authorizes differentiated service and paid lanes. The neutrality_reading forbids discrimination. The zero_rating_reading permits exemptions for sponsored content. These are not alternative measurements of the same constraint—they are three separate constraints rooted in one contested kernel. Prioritization_reading affects both siblings through resource-reallocation pressure: if prioritization becomes operationalized, it constrains the feasibility of the neutrality reading (which requires equal treatment) and influences the zero_rating reading (which would operate within a prioritized landscape). All three stories are linked via network.affects_constraints for family tracking.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
