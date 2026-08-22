% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__optimization_latitude_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_tcp_specification__optimization_latitude_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
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
 *   constraint_id: rfc9293_tcp_specification__optimization_latitude_reading
 *   human_readable: RFC 9293 TCP Specification — Optimization-Latitude Reading (Outcome Contract, Open Means)
 *   domain: technological/distributed_systems_coordination
 *
 * SUMMARY:
 *   RFC 9293 specifies what a TCP implementation must DO — deliver a
 *   reliable, ordered byte stream, manage connection state, respond to
 *   congestion — while leaving HOW almost entirely to the implementer:
 *   congestion control algorithm, timer granularity, buffer management,
 *   pacing, and loss recovery strategy are all implementation latitude so
 *   long as the observable contract holds. This story instantiates the
 *   optimization-latitude reading of that specification; the
 *   strict-invariance and middlebox-realism readings are separate constraint
 *   files linked via network.affects_constraints. The arrangement's
 *   beneficiaries are broad (anyone who builds on or rides the transport),
 *   its costs are concentrated on stack maintainers who carry conformance
 *   burden, and its one genuine exposure is the unpriced externality when a
 *   powerful implementer's optimized variant degrades competing flows on a
 *   shared bottleneck. KEY AGENTS (by structural relationship): -
 *   ietf_tcpm_working_group: agenda setter (institutional/mobile) — maintains
 *   the specification, collects no rents -
 *   high_performance_transport_implementers: primary beneficiary
 *   (institutional/arbitrage) — converts latitude into fleet-scale
 *   performance - os_kernel_stack_maintainers: principal payer
 *   (institutional/constrained) — carries the conformance burden -
 *   loss_based_competing_flows: exposed third party (powerless/trapped) —
 *   bears unpriced externality risk - application_developers and
 *   internet_end_users: distributed beneficiaries (organized,
 *   powerless/mobile) - network_operators: dual-positioned beneficiary-payer
 *   (institutional/constrained) - middlebox_vendors: excluded shaper
 *   (institutional/constrained) — narrows latitude in deployment without a
 *   seat - protocol_researchers: analytical observer (organized/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__optimization_latitude_reading, 0.15).
domain_priors:suppression_score(rfc9293_tcp_specification__optimization_latitude_reading, 0.1).
domain_priors:theater_ratio(rfc9293_tcp_specification__optimization_latitude_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, resistance, 0.18).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__optimization_latitude_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__optimization_latitude_reading, "RFC 9293 TCP Specification — Optimization-Latitude Reading (Outcome Contract, Open Means)").
narrative_ontology:topic_domain(rfc9293_tcp_specification__optimization_latitude_reading, "technological/distributed_systems_coordination").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__optimization_latitude_reading, '3e234e85-3999-41a6-8077-80ce59b9942c').
narrative_ontology:cs_kernel_codification('3e234e85-3999-41a6-8077-80ce59b9942c', formalized).
narrative_ontology:cs_authority_grounding('3e234e85-3999-41a6-8077-80ce59b9942c', expertise).
narrative_ontology:cs_interpretation_layer_present('3e234e85-3999-41a6-8077-80ce59b9942c').
narrative_ontology:cs_reading_relation('3e234e85-3999-41a6-8077-80ce59b9942c', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e234e85-3999-41a6-8077-80ce59b9942c', rfc9293_tcp_specification__middlebox_realism_reading, influences).
narrative_ontology:cs_axiom('3e234e85-3999-41a6-8077-80ce59b9942c', foundational, normative_force_extends_to_observable_behavior_only).
narrative_ontology:cs_axiom_status(normative_force_extends_to_observable_behavior_only, holdable).
narrative_ontology:cs_axiom_grounding('3e234e85-3999-41a6-8077-80ce59b9942c', normative_force_extends_to_observable_behavior_only, conventional).
narrative_ontology:cs_axiom('3e234e85-3999-41a6-8077-80ce59b9942c', secondary, performance_latitude_legitimate_within_semantic_bounds).
narrative_ontology:cs_axiom_status(performance_latitude_legitimate_within_semantic_bounds, holdable).
narrative_ontology:cs_axiom_grounding('3e234e85-3999-41a6-8077-80ce59b9942c', performance_latitude_legitimate_within_semantic_bounds, instrumental).
narrative_ontology:cs_reference_frame('3e234e85-3999-41a6-8077-80ce59b9942c', outcome_contract_open_implementation).
narrative_ontology:cs_drift_state('3e234e85-3999-41a6-8077-80ce59b9942c', contemporary_post_ossification, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('3e234e85-3999-41a6-8077-80ce59b9942c', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, application_developers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, high_performance_transport_implementers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, network_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, internet_end_users).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__optimization_latitude_reading, loss_based_competing_flows).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, os_kernel_stack_maintainers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__optimization_latitude_reading, high_performance_transport_implementers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__optimization_latitude_reading, os_kernel_stack_maintainers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__optimization_latitude_reading, network_operators).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__optimization_latitude_reading, end_to_end_argument).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__optimization_latitude_reading, robustness_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains and updates the TCP specification through the IETF standards process: drafts revisions, adjudicates errata, publishes extensions and applicability statements. Collects no fees; its return is the continued functioning of the interoperable Internet its document describes. Exit would mean handing maintenance to another body or letting the document stagnate while implementations drift.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, ietf_tcpm_working_group, agenda_setter,
    institutional, generational, mobile, global).

% Operate large fleets (search, video, cloud) where transport efficiency translates directly into cost and user experience. They invest in advanced congestion control, pacing, and buffer management inside the semantic contract, deploy variants fleet-wide, and publish results. Their scale lets them route around the standard if it stops serving them — they built QUIC when TCP's evolution stalled.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, high_performance_transport_implementers, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__optimization_latitude_reading, high_performance_transport_implementers, payer).

% Write and maintain the TCP implementations everyone else inherits: Linux, Windows, BSD stacks. They carry the full conformance burden — state machine, timers, header handling, decades of accumulated edge cases — and absorb every extension the community ratifies. Leaving is not available: their stacks must speak to the installed base of the Internet.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, os_kernel_stack_maintainers, payer,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__optimization_latitude_reading, os_kernel_stack_maintainers, beneficiary).

% Build applications against the reliable byte-stream interface and expect any conformant stack anywhere to carry their traffic. They neither run nor maintain the protocol; they consume its guarantees. Exit is real but costly: moving to QUIC or raw UDP means rebuilding transport properties the interface gave them.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, application_developers, beneficiary,
    organized, biographical, mobile, global).

% Run the links and routers the traffic crosses. They benefit from predictable, interoperable endpoint behavior and bear the operational cost of accommodating whatever senders' optimized algorithms do to their queues — sudden throughput shifts, buffer pressure, new failure modes. Their leverage over endpoint code is limited to configuration and peering policy.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, network_operators, beneficiary,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__optimization_latitude_reading, network_operators, payer).

% Experience the arrangement only as things working: pages load, calls connect, downloads complete. They bear latency and fairness effects of others' optimization choices with no representation in the process and no practical exit except switching services.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, internet_end_users, beneficiary,
    powerless, biographical, mobile, global).

% Individual transfers sharing a bottleneck with a fleet running an aggressively optimized algorithm. When the optimizer misjudges fairness, these flows see throughput collapse for the duration of contention. They have no seat in any standards conversation and cannot leave the shared link mid-transfer.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, loss_based_competing_flows, payer,
    powerless, immediate, trapped, regional).

% Sell firewalls, NATs, load balancers, and WAN optimizers deployed along the paths the specification governs. Their products silently reshape what endpoint latitude can exercise — stripping options, rewriting headers, freezing sequence assumptions — yet they are not party to the standards process that must accommodate them.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_vendors, excluded,
    institutional, biographical, constrained, global).

% Study congestion control, transport performance, and protocol evolution; propose variants, measure deployed behavior, and feed results back into the standards process. They hold the analytical seat: affected by the arrangement's direction, invested in none of its revenue.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, protocol_researchers, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__optimization_latitude_reading, diffuse).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__optimization_latitude_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of reliable transport interoperability across independently developed stacks and administratively disjoint networks: one behavioral contract (ordered reliable byte stream, connection semantics, an endpoint obligation to control congestion) lets N implementers interoperate without central planning, while leaving implementation means open so performance innovation continues.
% TRANSFER_FUNCTION: Moves conformance obligations to every implementer (build the whole semantic contract, absorb every ratified extension) and interoperability reach to every conformant stack; moves optimization freedom to implementers, priced at preservation of the byte-stream contract; and transfers congestion-management responsibility from the network core to the endpoints.
% ABSENT_VOICES: Middlebox vendors shape what latitude can exercise in deployment but hold no seat in the process that must accommodate them. End users are represented only derivatively, through application developers. Flows sharing a bottleneck with an optimized sender — the parties exposed to fairness externalities — have no seat at all; their interests enter only when researchers measure and publish the harm.
% DISAPPEARANCE_RATIONALE: Existing stacks would keep talking to each other for a while, but every new stack, operating system, extension, and performance technique would lose its common reference point. Interoperability would fragment into de facto clusters around whichever implementations had market weight, and transport evolution would collapse into bilateral negotiation among the largest fleets. The IETF's capacity to evolve transport at all would evaporate with the shared document.
% FOUNDING_PROBLEM: Heterogeneous 1970s-80s networks needed a common host-to-host reliable byte stream so arbitrarily different hosts and networks could interoperate without bespoke pairwise adaptation; the TCP specification lineage (793 through 9293) was built to make transport a solved, shared substrate.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: the IETF QUIC working group's charter documents attest the problem persists — a rival transport effort that nonetheless reproduced the same reliability contract rather than abandoning it — and newly written OS network stacks continue to target TCP conformance as a launch requirement. No party with standing claims the interoperability problem is permanently solved.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__optimization_latitude_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__optimization_latitude_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__optimization_latitude_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 0.15, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rfc9293_tcp_specification__optimization_latitude_reading_tests).
:- end_tests(rfc9293_tcp_specification__optimization_latitude_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.15) because the specification takes only the semantic contract and prices it at the cost of the coordination itself; the 2016 bump in the series marks the BBRv1 deployment, when one implementer's latitude briefly imposed measurable unfairness on loss-based competing flows before algorithm revision and community pressure corrected it. Suppression (0.10) is a raw structural property, unscaled by power or scope: nothing coerces participation — conformance is self-interested, and the exits are real (QUIC, SCTP, raw UDP), which is why suppression sits far below enforcement-bearing arrangements. Theater ratio (0.12) reflects mostly-functional activity with a small ceremonial residue: legacy features retained for conformance box-ticking (urgent pointer, push-flag semantics) that no longer carry load. Accessibility collapse (0.40) is rope-range: alternatives do not vanish once the constraint is understood — QUIC demonstrates a worked exit — but universal reachability over the installed Internet still pulls most implementers back to the contract. Resistance (0.18) is low: implementers comply because interoperability is what they want; the occasional deviation is a bug report, not a revolt. Claimed type is rope on structural grounds — genuine coordination function, net-benefit participation, no suppressed alternatives — authored independently of the metrics; if the engine computes divergence from any seat, that divergence is the datum. All temporal series share one grid (1981/1990/1998/2006/2014/2016/2022/2025) so no metric is sampled against another metric's end-state.
 *
 * PERSPECTIVAL GAP:
 *   Four seats inhabit the same text differently. From the high-performance implementer's seat the arrangement is freedom: the contract tells them what must hold and leaves the rest theirs, and their arbitrage-grade exit makes the relationship voluntary. From the kernel maintainer's seat it is obligation: the full semantic contract plus every ratified extension lands in their code, with no exit from the installed base. From the competing flow's seat it is hazard: another party's latitude is the weather their transfer dies in. From the working group's seat it is stewardship: a document to keep coherent. The engine computes these per-seat classifications from the structural data; the authored claim adjudicates none of them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: application developers, end users, and network operators derive net subsidy from the arrangement, and high-performance implementers sit nearest the beneficiary pole — they collect the largest share of the latitude's value and hold arbitrage exit besides. Victim declaration drives high directionality: loss_based_competing_flows are trapped, powerless, and bear costs they never agreed to, putting their seat nearest the full-target end despite the arrangement's low base extraction. Kernel stack maintainers derive high d as payers with constrained exit — they fund the coordination with conformance labor. No directionality overrides are used: the derivation from declared roles, power, and exit options captures every seat's relationship without correction, and an override keyed to a power atom would misapply across the three differently-positioned institutional seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — heterogeneous networks needing a shared reliable transport contract — is live, not dead: it recurs with every new device, stack, and network, and the QUIC effort's decision to rebuild the same contract rather than discard it is outside corroboration that the problem never went away. Theater is low and the function is fully performed, so the receipt surface (diffuse gains, prohibitive fixing cost) must not be read as the piton cell: that cell flags atrophied function kept alive by inertia, whereas here prohibitive fix-cost is the ordinary signature of durable, healthy infrastructure no one should want to fix. The classification prevents two mislabelings at once: a strict-invariance holder would read latitude as abdication (under-specified chaos inviting fragmentation), and a fairness-harmed flow holder would read it as extraction (freedom for the powerful at the trapped's expense); the structural data shows instead a genuine coordination arrangement with a bounded, self-corrected externality — the 2016 spike and its correction are visible in the measurement series precisely so the drift detector sees the immune response, not just the infection. No sunset clause is authored: nothing about this arrangement is transitional.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This story instantiates the optimization-latitude reading of the rfc9293_tcp_specification kernel; the same text under the strict_invariance_reading (full state machine normative) or the middlebox_realism_reading (deployed practice normative) constitutes a different constraint with a different epsilon and beneficiary structure — which reading''s classification does the corpus ultimately certify?',
    'Compile all three reading-stories and compare computed types, epsilon, and per-seat divergences; the disagreement is located in the scope of normative force — observable behavior only (this reading), the full state machine (strict invariance), or deployed practice (middlebox realism).',
    'Under strict_invariance, epsilon rises (universal conformance burden, variant experimentation suppressed) and the type likely shifts toward tangled_rope; under middlebox_realism the agenda_setter relocates from the IETF to the deployed middlebox population and the working-group seat becomes an observer of a constraint it does not administer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: one kernel, three readings; this file authors only the latitude reading.').

omega_variable(
    optimization_externality_pricing,
    'Does implementation latitude systematically impose unpriced costs on competing flows sharing bottlenecks (the BBRv1-versus-loss-based pattern), and is the correction loop — community measurement pressure plus algorithm revision — adequate to keep the externality bounded?',
    'Deployment-era throughput and fairness studies of optimized variants against mixed congestion-control populations; track whether successive revisions close the measured unfairness gap.',
    'If externalities are systematic and uncorrected, the latitude regime leaks extraction onto non-consenting third parties and the arrangement tilts toward tangled_rope with loss_based_competing_flows as the paying seat; if corrections hold, the costs remain bounded coordination friction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(optimization_externality_pricing, empirical, 'Whether the latitude''s fairness externality is self-correcting or accumulating.').

omega_variable(
    semantic_boundary_erosion,
    'Can the semantic bounds themselves be eroded incrementally by optimizations that change observable behavior while claiming mere performance improvement — handshake-modifying extensions, new option semantics, sequence-space reinterpretations?',
    'Track standards-process disputes over proposed extensions: do boundary-changing proposals get rejected as semantic violations, or absorbed as latitude?',
    'If the bounds erode, latitude becomes a vehicle for unilateral protocol change by powerful implementers — an extraction channel invisible to per-seat metrics until interoperability breaks.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(semantic_boundary_erosion, conceptual, 'Whether the boundary that contains the latitude is itself stable.').

omega_variable(
    wire_latitude_ossification,
    'Middlebox ossification has narrowed wire-visible latitude (stripped options, frozen sequence assumptions); does the remaining latitude — internal algorithms plus encrypted extension surfaces — still deliver the reading''s promised benefit?',
    'Compare deployability of new wire-visible TCP features before and after roughly 2005, and against extension velocity on encrypted transports.',
    'If wire latitude keeps collapsing, the reading''s benefit contracts toward internal-only optimization and the practical contest migrates toward the middlebox_realism reading''s terrain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wire_latitude_ossification, empirical, 'Whether deployment reality preserves enough latitude for the reading to hold.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__optimization_latitude_reading, 1981, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9293_optlat_tr_t1981, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 1981, 0.05).
narrative_ontology:measurement_basis(rfc9293_optlat_tr_t1981, observed).
narrative_ontology:measurement(rfc9293_optlat_tr_t1990, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 1990, 0.06).
narrative_ontology:measurement_basis(rfc9293_optlat_tr_t1990, observed).
narrative_ontology:measurement(rfc9293_optlat_tr_t1998, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 1998, 0.08).
narrative_ontology:measurement_basis(rfc9293_optlat_tr_t1998, observed).
narrative_ontology:measurement(rfc9293_optlat_tr_t2006, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2006, 0.09).
narrative_ontology:measurement_basis(rfc9293_optlat_tr_t2006, observed).
narrative_ontology:measurement(rfc9293_optlat_tr_t2014, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2014, 0.11).
narrative_ontology:measurement_basis(rfc9293_optlat_tr_t2014, observed).
narrative_ontology:measurement(rfc9293_optlat_tr_t2016, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2016, 0.11).
narrative_ontology:measurement_basis(rfc9293_optlat_tr_t2016, observed).
narrative_ontology:measurement(rfc9293_optlat_tr_t2022, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2022, 0.12).
narrative_ontology:measurement_basis(rfc9293_optlat_tr_t2022, observed).
narrative_ontology:measurement(rfc9293_optlat_tr_t2025, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 2025, 0.12).
narrative_ontology:measurement_basis(rfc9293_optlat_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(rfc9293_optlat_be_t1981, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 1981, 0.1).
narrative_ontology:measurement_basis(rfc9293_optlat_be_t1981, observed).
narrative_ontology:measurement(rfc9293_optlat_be_t1990, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 1990, 0.12).
narrative_ontology:measurement_basis(rfc9293_optlat_be_t1990, observed).
narrative_ontology:measurement(rfc9293_optlat_be_t1998, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 1998, 0.13).
narrative_ontology:measurement_basis(rfc9293_optlat_be_t1998, observed).
narrative_ontology:measurement(rfc9293_optlat_be_t2006, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2006, 0.14).
narrative_ontology:measurement_basis(rfc9293_optlat_be_t2006, observed).
narrative_ontology:measurement(rfc9293_optlat_be_t2014, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2014, 0.15).
narrative_ontology:measurement_basis(rfc9293_optlat_be_t2014, observed).
narrative_ontology:measurement(rfc9293_optlat_be_t2016, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2016, 0.19).
narrative_ontology:measurement_basis(rfc9293_optlat_be_t2016, observed).
narrative_ontology:measurement(rfc9293_optlat_be_t2022, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2022, 0.16).
narrative_ontology:measurement_basis(rfc9293_optlat_be_t2022, observed).
narrative_ontology:measurement(rfc9293_optlat_be_t2025, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 2025, 0.15).
narrative_ontology:measurement_basis(rfc9293_optlat_be_t2025, observed).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(rfc9293_tcp_specification__optimization_latitude_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__optimization_latitude_reading, information_standard).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification__middlebox_realism_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: 'the TCP specification' is one colloquial label covering three structurally distinct claims about where normative force resides, each with its own epsilon, beneficiary structure, and type. This file authors the optimization-latitude reading (outcome contract, open means; low extraction). The strict-invariance reading (exact state-machine replication) is a separate story with higher conformance burden on implementers; the middlebox-realism reading (deployed practice as the operative spec) relocates the agenda-setter entirely. Family edges: the latitude reading presupposes a stable semantic core — the invariance reading's object — and its demonstrated deployability (BBR, DCTCP surviving contact with real networks) creates structural pressure on the middlebox-realism claim that the network dictates protocol evolution.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
