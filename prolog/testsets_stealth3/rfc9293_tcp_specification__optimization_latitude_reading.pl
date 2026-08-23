% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__optimization_latitude_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: RFC 9293 TCP Specification — Optimization-Latitude Reading
 *   domain: technological/distributed-systems-coordination
 *
 * SUMMARY:
 *   RFC 9293 governs the Transmission Control Protocol as a semantic
 *   contract: it specifies what a conforming implementation must do on the
 *   wire — establish and tear down connections, sequence and acknowledge
 *   bytes, retransmit lost segments, send in a network-safe manner — and it
 *   deliberately does not specify how an implementation achieves those
 *   outcomes internally. This story authors that arrangement as the
 *   optimization-latitude reading sees it: a coordination device binding
 *   behavior, not design, whose compliance costs are modest and symmetric and
 *   whose latitude is genuinely exercisable (the BBR and DCTCP lineages
 *   deployed inside mainstream stacks without breaking interoperability). The
 *   epsilon referent is the standing arrangement — the
 *   specification-as-deployed-governor of TCP implementations — assessed by
 *   this reading's own lights; it is not the rival arrangements other
 *   readings would endorse. Per the epsilon-invariance principle, the sibling
 *   readings of this kernel are separate constraint stories, not hedges
 *   inside this one; they appear here only in kernel_context, the omegas, and
 *   the network block. KEY AGENTS (by structural relationship): see
 *   key_agents.
 *
 * KEY AGENTS:
 *   - os_kernel_tcp_maintainers: Primary coordinated implementer (institutional/constrained) — bears the conformance cost, collects the interoperability payoff
 *   - ietf_transport_working_groups: Agenda setter (institutional/constrained) — authors and revises the semantic contract through the RFC series
 *   - transport_performance_engineers: Latitude exercisers (organized/mobile) — redesign beneath the outcome contract (BBR, DCTCP lineages)
 *   - application_developers: Downstream beneficiary (powerful/mobile) — consumes the byte-stream guarantee; QUIC exit available and increasingly used
 *   - internet_end_users: Diffuse beneficiary (powerless/constrained) — experience only the working result; choose neither transport nor settings
 *   - middlebox_operators: Path-side cost bearer outside the contract's protected surface (organized/trapped) — excluded from the standardization conversation, absorbs disruption when latitude is exercised
 *   - alternative_transport_designers: Analytical observer (institutional/analytical) — builds the exit that disciplines the arrangement from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__optimization_latitude_reading, 0.11).
domain_priors:suppression_score(rfc9293_tcp_specification__optimization_latitude_reading, 0.08).
domain_priors:theater_ratio(rfc9293_tcp_specification__optimization_latitude_reading, 0.06).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, extractiveness, 0.11).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0.08).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0.06).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__optimization_latitude_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__optimization_latitude_reading, rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__optimization_latitude_reading, "RFC 9293 TCP Specification — Optimization-Latitude Reading").
narrative_ontology:topic_domain(rfc9293_tcp_specification__optimization_latitude_reading, "technological/distributed-systems-coordination").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__optimization_latitude_reading, '5f87275d-e630-4e24-82d7-3897d8fafe9f').
narrative_ontology:cs_kernel_codification('5f87275d-e630-4e24-82d7-3897d8fafe9f', formalized).
narrative_ontology:cs_authority_grounding('5f87275d-e630-4e24-82d7-3897d8fafe9f', expertise).
narrative_ontology:cs_interpretation_layer_present('5f87275d-e630-4e24-82d7-3897d8fafe9f').
narrative_ontology:cs_reading_relation('5f87275d-e630-4e24-82d7-3897d8fafe9f', rfc9293_tcp_specification__strict_invariance_reading, forecloses).
narrative_ontology:cs_reading_relation('5f87275d-e630-4e24-82d7-3897d8fafe9f', rfc9293_tcp_specification__middlebox_realism_reading, influences).
narrative_ontology:cs_axiom('5f87275d-e630-4e24-82d7-3897d8fafe9f', foundational, behavioral_contract_not_prescriptive_path).
narrative_ontology:cs_axiom_status(behavioral_contract_not_prescriptive_path, holdable).
narrative_ontology:cs_axiom_grounding('5f87275d-e630-4e24-82d7-3897d8fafe9f', behavioral_contract_not_prescriptive_path, conventional).
narrative_ontology:cs_axiom('5f87275d-e630-4e24-82d7-3897d8fafe9f', secondary, optimization_within_semantic_bounds_permitted).
narrative_ontology:cs_axiom_status(optimization_within_semantic_bounds_permitted, holdable).
narrative_ontology:cs_axiom_grounding('5f87275d-e630-4e24-82d7-3897d8fafe9f', optimization_within_semantic_bounds_permitted, instrumental).
narrative_ontology:cs_reference_frame('5f87275d-e630-4e24-82d7-3897d8fafe9f', semantic_outcome_contract).
narrative_ontology:cs_drift_state('5f87275d-e630-4e24-82d7-3897d8fafe9f', post_quic_ossification_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5f87275d-e630-4e24-82d7-3897d8fafe9f', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__optimization_latitude_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, os_kernel_tcp_maintainers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, application_developers).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, internet_end_users).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__optimization_latitude_reading, transport_performance_engineers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_operators).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Write and maintain the TCP code in general-purpose operating systems (Linux, FreeBSD, Windows, macOS). They implement the wire-visible semantics the specification defines — header processing, state transitions, acknowledgment and retransmission behavior — and choose internal algorithms (congestion control, buffering, timers) from whatever performs best on their hardware. Their recurring cost is conformance: regression testing against every peer stack, tracking errata and updates. Their gain is that every other conforming stack speaks to theirs without bilateral negotiation. Leaving the arrangement would mean shipping an operating system whose transport cannot reach most of the internet.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, os_kernel_tcp_maintainers, beneficiary,
    institutional, generational, constrained, global).

% Author and revise the specification through the RFC series (TCPM working group, Transport Area directorate). They consolidate errata, tighten requirements language, and periodically restructure the document (RFC 793 became RFC 9293). Their authority rests on demonstrated competence and rough consensus; nothing compels anyone to follow their output. Revision is possible but slow, bounded by the installed base's expectations and by the working-group process itself.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, ietf_transport_working_groups, agenda_setter,
    institutional, generational, constrained, global).

% Build software on top of the reliable byte stream the specification guarantees, without implementing retransmission, ordering, or flow control themselves. They gain a stable target that works across every operating system and network path. Where the guarantee fits poorly — real-time media, loss-tolerant streams — they route around it with UDP-based designs rather than petitioning for changes. Exit is real and increasingly exercised: several large application operators have moved substantial traffic to QUIC.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, application_developers, beneficiary,
    powerful, biographical, mobile, global).

% Experience the arrangement only as things working: pages loading, transfers completing, connections surviving path changes. They choose neither the transport nor its settings; applications and operating systems select on their behalf. Their alternative is switching applications or networks, not transports.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, internet_end_users, beneficiary,
    powerless, immediate, constrained, global).

% Research groups and industry teams in the congestion-control and datacenter-transport lineages (BBR, DCTCP and successors) who treat the specification's outcome guarantees as a fixed target and redesign everything beneath it — pacing, loss response, window dynamics — to move more data faster. Their work deploys inside mainstream stacks, which is possible only because the specification binds behavior rather than design. They bear review and deployment risk; they gain a worldwide measurable canvas.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, transport_performance_engineers, beneficiary,
    organized, biographical, mobile, global).

% Vendors and operators of firewalls, NATs, load balancers, and WAN optimizers sitting on network paths. Their products inspect and reshape TCP traffic, and many were built against common implementation habits rather than the specification's guarantees alone. When stacks exercise their latitude — new congestion control, new options, altered handshake behavior — some of these products misclassify or degrade flows, and their owners absorb upgrade costs. They hold no seat in the endpoint-standardization process where the latitude is defined; their channel of influence is failure reports and interoperability pressure after deployment.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_operators, excluded,
    organized, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_operators, payer).

% The QUIC working group and related designers building transport that deliberately bypasses the deployed TCP ecosystem. They study the specification and its deployment history as design input — what to keep (the outcome guarantees), what to escape (wire-format ossification, in-path interference). They neither depend on the specification's persistence nor pay into it; their seat with respect to it is analytical, though their success changes the option space available to every other party.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__optimization_latitude_reading, alternative_transport_designers, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__optimization_latitude_reading, diffuse).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__optimization_latitude_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the bilateral-interoperability problem for independently developed transport stacks: connection establishment and teardown, sequencing, acknowledgment, retransmission semantics, flow- and congestion-control signals are defined once, centrally, so any two conforming implementations exchange a reliable ordered byte stream without prior negotiation. Under this reading the specification coordinates on outcomes — the byte-stream guarantee and network-safe sending behavior — and leaves the means open.
% TRANSFER_FUNCTION: Moves no material resource between parties. What moves is certainty: each implementer surrenders freedom on wire-visible semantics (header formats, flag meanings, state transitions) and receives guaranteed interpretability with every other conforming stack. A second, thinner transfer runs onto senders collectively: the obligation to implement congestion control, surrendered by every sender, received by every sender as a network that does not collapse.
% ABSENT_VOICES: Middlebox operators and firewall vendors were historically absent from the endpoint-standardization conversation; the NAT and firewall industries made deployment-shaping choices the specification never anticipated, and they would have argued for explicit extension points and tunability had they been in the room. Application domains with needs the reliability mandate serves poorly (real-time, loss-tolerant) likewise never shaped the text — they exited via UDP instead. Both absences are commentary-grade: they explain the shape of later friction, not the classification.
% DISAPPEARANCE_RATIONALE: If the semantic contract vanished overnight, independently maintained stacks would diverge within months; every application would need per-peer adaptation or a replacement protocol; the performance-engineering lineage loses its shared deployment target; and traffic would fragment into incompatible transport islands until a successor standard (something QUIC-like) reconstructed the same guarantees. Arrangements across the entire application economy depend on it.
% FOUNDING_PROBLEM: Late-1970s internetworking: heterogeneous networks (ARPANET, packet radio, satellite, early Ethernet) needed one host-to-host protocol giving applications a reliable byte stream regardless of underlying network, and the multiple diverging TCP versions circulating by 1979-1980 had stopped interoperating — the specification was written to force convergence on a single semantic contract.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the historical record of the ARPANET TCP version conflicts and the January 1981 flag-day transition (contemporaneous Cerf memoranda and the RFC series) attests the founding convergence problem; QUIC's designers — not TCP beneficiaries — attest the interoperability problem remains real by reconstructing equivalent guarantees in RFC 9000's rationale; interop bake-off reports and conformance test suites attest the problem recurs with every newly written stack.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__optimization_latitude_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__optimization_latitude_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__optimization_latitude_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__optimization_latitude_reading, 0.11, 'stealth/ox-alpha', 'none', direct).

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
 *   Claimed type is rope on structural grounds independent of the metrics: the arrangement solves a real collective-action problem (mutual interpretability of independently written stacks), participants are net beneficiaries, no seat collects rents from its operation, and alternatives are not suppressed — indeed the main alternative (QUIC) was designed, standardized, and deployed by parties with no stake in the specification's persistence. Metrics are authored as descriptive facts. Extractiveness 0.11: conformance testing, errata tracking, and edge-case handling are real recurring costs, but they buy the interoperability that is the implementers' own payoff, and no beneficiary seat captures another seat's payment. Suppression 0.08: adherence is self-enforcing through interoperability rather than coercively enforced — a non-conforming stack simply fails to communicate — and exits exist (SCTP niches, QUIC at scale, raw UDP designs). Theater 0.06: the specification's functions are almost entirely functional; the small residual is working-group ceremony and document-maintenance ritual. Accessibility_collapse 0.30: alternatives persist and thrive, but network-effect gravity keeps TCP the default path, so alternatives are viable rather than effortless. Resistance 0.10: no organized resistance to the contract itself; friction is localized in specific mechanism debates (initial-window sizing, congestion-control politics). The suppression_requirement series is authored deliberately: this story's enforcement dynamic is the decay of lock-in as exits matured — from a period with no alternative transport (t=0) through SCTP's niche arrival, to QUIC's standardized, browser-deployed exit — so the suppressive force of the arrangement is the traced quantity, not a static backdrop. All three series share one time grid (t = 0, 8, 16, 24, 32, 40, 44; years since 1981) so every metric is authored at every examined point; the mid-interval bump in extractiveness (t=24) marks the ossification era, when latitude remained textually free but practically risky — the arrangement's effective grip tightened without any change to its text.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from identical structural data. From the implementer seat the arrangement is nearly pure coordination: a modest recurring conformance cost against a large interoperability payoff, with algorithmic freedom preserved. From the middlebox-operator seat the same latitude reads as destabilization — deployed inspection assumptions break when optimizations ship — so effective extraction from that vantage is materially higher than the story-level 0.11. From the agenda-setter seat it is stewardship: reputational and epistemic payoff for maintaining a functioning commons. From the end-user seat it is invisible infrastructure. The engine derives these per-seat classifications from power, exit, and role data; the divergence between the implementer and middlebox seats is the story's principal perspectival fact.
 *
 * DIRECTIONALITY LOGIC:
 *   Four seats are declared beneficiaries (implementers, application developers, end users, performance engineers) and derive low directionality — the arrangement subsidizes them relative to its costs. The middlebox-operator seat carries excluded (secondary payer) roles but sits outside the contract's protected surface: the specification never promised path devices stability, so their losses when latitude is exercised are opportunity costs of overfitting to implementation habits, not extracted payments. No directionality_overrides are authored: overrides key on power atoms, and this story's two organized-power seats need opposite corrections (performance engineers are low-d beneficiaries; middlebox operators are mid-d incidental bearers), so a single per-atom override would misfire on one of them. The attribution question — whether path-device disruption counts as extraction at all — is routed to the middlebox_cost_attribution omega instead of being forced through the override mechanism.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — forcing convergence among divergent implementations — is still live: every newly written stack re-runs it, which is why conformance suites and interop events persist. Nothing here is mandatrophy-resolved; there is no sunset because the function has not atrophied. The classification guards against two opposite mislabelings. The corpus's habitual failure is dressing extraction as coordination; here the live risks are inverted: (1) reading the specification's real compliance costs as extraction (a false snare verdict that ignores that the payer seats are also the payoff seats), and (2) attributing ossification-era rigidity to the text itself, inflating epsilon for constraints the document never imposed. On the receipt surface, gains are affirmatively diffuse — every named seat was checked and none captures the arrangement's minimal extraction — and fixing is prohibitive. That prohibitive-plus-diffuse cell pattern-matches the piton signature, and it is flagged here deliberately: downstream consumers should weigh it against the theater_ratio (0.06, far from performative maintenance), the live founding problem, and the flat extractiveness trajectory before reading the cell mechanically. Load-bearing coordination and vestigial performance can share a receipt cell; the theater and drift data are what distinguish them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_membership,
    'This story instantiates one reading (optimization_latitude_reading) of the kernel rfc9293_tcp_specification; how would the classification shift if the sibling readings were adopted instead?',
    'Compile and classify the sibling stories (strict_invariance_reading, middlebox_realism_reading) and compare per-seat outputs; the divergence between computed classifications localizes the disagreement to the boundary-location element described in kernel_context.',
    'Under strict_invariance_reading, mandated exact replication converts conformance latitude into obligation, raising epsilon and suppression for every implementer seat and suppressing the BBR/DCTCP variant lineage. Under middlebox_realism_reading, authority relocates from the document to deployed-path behavior, making epsilon path-dependent and only partly authorable from any single text. This story''s rope classification is conditional on the latitude premise holding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_membership, conceptual, 'Committer structure: this is one of three readings of the TCP-specification kernel; siblings are separate constraints.').

omega_variable(
    latitude_boundary_location,
    'Where exactly does the semantic contract end and implementation latitude begin — are pacing, initial-window sizing, new TCP options, and handshake extensions inside the protected surface or outside it?',
    'Conformance-test adjudication and interop bake-off outcomes; deployment telemetry on specific optimizations (TCP Fast Open traversal rates, MPTCP option survival across path populations, initial-window controversy resolutions).',
    'A narrower boundary raises effective epsilon (more behavior becomes obligatory for every implementer); a wider one lowers it. The authored 0.11 assumes the boundary sits at wire-visible semantics with algorithm choice free; resolving the boundary either way moves the metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(latitude_boundary_location, empirical, 'The exact extent of the latitude this reading grants is contestable at specific mechanisms.').

omega_variable(
    middlebox_cost_attribution,
    'When exercising latitude disrupts middlebox processing (BBR versus buffer-shaped queues, TFO versus stateful firewalls), is that a cost the arrangement imposes on a governed party (epsilon-relevant) or a cost borne by actors outside the contract''s protected surface (not epsilon-relevant)?',
    'Test whether the specification ever extended protection to in-path equipment: if the semantic contract nowhere promises path devices stability, their losses are opportunity costs of overfitting to implementation habits rather than extraction, and the payer-flavored residue on the middlebox seat is incidental.',
    'Attributing those costs to the arrangement would raise epsilon toward tangled-rope territory and convert the middlebox seat into a genuine victim; excluding them keeps the rope classification clean. This is the principal epsilon-invariance risk for this reading and the reason no directionality override was authored for the middlebox seat.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_cost_attribution, conceptual, 'Whether path-device disruption counts as extraction by the specification or as external cost.').

omega_variable(
    ossification_feedback_loop,
    'Does middlebox-driven wire ossification narrow the exercisable latitude below what the text grants, and if so does this reading''s epsilon understate the operative constraint?',
    'Measure TCP option-bit survival rates and extension deployment success across path populations over time; compare textual latitude against exercised latitude. QUIC''s encrypted-transport design is the running natural experiment: it prices the gap between textual and exercisable latitude.',
    'If implicit path constraints bind harder than the text, the operative constraint is stricter than the authored one and measured extraction and rigidity should be revised upward — the t=24 extractiveness bump in the measurement series is the historical trace of exactly this effect. If BBR-class deployments show algorithmic latitude remains fully exercisable, the reading''s low epsilon stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(ossification_feedback_loop, empirical, 'Whether deployed-path behavior silently amends the latitude the text grants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__optimization_latitude_reading, 0, 44).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9293_optlat_tr_t0, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(rfc9293_optlat_tr_t8, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 8, 0.05).
narrative_ontology:measurement(rfc9293_optlat_tr_t16, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 16, 0.06).
narrative_ontology:measurement(rfc9293_optlat_tr_t24, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 24, 0.08).
narrative_ontology:measurement(rfc9293_optlat_tr_t32, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 32, 0.08).
narrative_ontology:measurement(rfc9293_optlat_tr_t40, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 40, 0.07).
narrative_ontology:measurement(rfc9293_optlat_tr_t44, rfc9293_tcp_specification__optimization_latitude_reading, theater_ratio, 44, 0.06).

% Extraction over time
narrative_ontology:measurement(rfc9293_optlat_be_t0, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 0, 0.14).
narrative_ontology:measurement(rfc9293_optlat_be_t8, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 8, 0.12).
narrative_ontology:measurement(rfc9293_optlat_be_t16, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 16, 0.1).
narrative_ontology:measurement(rfc9293_optlat_be_t24, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 24, 0.13).
narrative_ontology:measurement(rfc9293_optlat_be_t32, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 32, 0.12).
narrative_ontology:measurement(rfc9293_optlat_be_t40, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 40, 0.11).
narrative_ontology:measurement(rfc9293_optlat_be_t44, rfc9293_tcp_specification__optimization_latitude_reading, base_extractiveness, 44, 0.11).

% Suppression requirement over time
narrative_ontology:measurement(rfc9293_optlat_su_t0, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(rfc9293_optlat_su_t8, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 8, 0.19).
narrative_ontology:measurement(rfc9293_optlat_su_t16, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 16, 0.18).
narrative_ontology:measurement(rfc9293_optlat_su_t24, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 24, 0.16).
narrative_ontology:measurement(rfc9293_optlat_su_t32, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 32, 0.13).
narrative_ontology:measurement(rfc9293_optlat_su_t40, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 40, 0.09).
narrative_ontology:measurement(rfc9293_optlat_su_t44, rfc9293_tcp_specification__optimization_latitude_reading, suppression_requirement, 44, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__optimization_latitude_reading, information_standard).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__optimization_latitude_reading, middlebox_realism_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'the TCP specification'. The label conflates three structurally distinct claims with different epsilon values, beneficiary structures, and failure modes: (1) strict_invariance_reading — an invariant state machine mandating exact replication; (2) optimization_latitude_reading (this file) — a semantic outcome contract granting means-latitude, low extraction, self-enforcing; (3) middlebox_realism_reading — a path-dependent de facto protocol whose operative text is deployed behavior. Each is authored as its own story with its own stable epsilon; this file authors only (2). The upstream/downstream structure runs from this reading to the realism reading: latitude exercise generates the empirical record the realism reading cites. Family membership is expressed via network.affects_constraints in all three files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
