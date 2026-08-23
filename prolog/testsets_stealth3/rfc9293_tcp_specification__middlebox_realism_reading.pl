% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__middlebox_realism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_rfc9293_tcp_specification__middlebox_realism_reading, []).

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
 *   constraint_id: rfc9293_tcp_specification__middlebox_realism_reading
 *   human_readable: TCP Middlebox Realism Reading — Deployed Path Behavior as Operative Standard
 *   domain: technological/network-protocol-coordination
 *
 * SUMMARY:
 *   RFC 9293 describes how TCP endpoints ought to behave; the traffic
 *   actually carried obeys a protocol jointly produced by endpoints and the
 *   population of path-resident equipment — address translators, stateful
 *   firewalls, shapers, load balancers, inspection appliances — that
 *   rewrites, splits, delays, resets, and observes flows. Under this reading,
 *   the specification's authority is real but subordinate: features it
 *   standardizes succeed or fail according to what deployed equipment
 *   tolerates, and the operative definition of the protocol is what the
 *   network does. The arrangement has a genuine coordination core (address
 *   conservation, perimeter policy, load distribution) and a substantial
 *   extraction superstructure (surveillance, unilateral control over packet
 *   fate, ossification of protocol evolution) delivered through the same
 *   boxes. This file instantiates ONE reading of the contested kernel
 *   rfc9293_tcp_specification — the middlebox_realism_reading; the
 *   strict-invariance and optimization-latitude readings are separate
 *   constraints in separate files, linked through
 *   network.affects_constraints, and are deliberately not averaged into this
 *   story's epsilon. The claim/metric relationship is kept independent:
 *   claimed_type is authored from structural analysis (both coordination and
 *   asymmetric extraction present, active enforcement required), while the
 *   metrics are authored as descriptive estimates of actual operation.
 *
 * KEY AGENTS:
 *   - isp_operators: primary agenda-setter and principal recipient of gains (institutional/constrained) — administers the path equipment population, collects address-economics and visibility advantages
 *   - enterprise_security_teams: agenda-setter with incidental benefit (institutional/constrained) — sets perimeter policy, pays maintenance and breakage costs
 *   - middlebox_vendor_industry: commercial beneficiary (powerful/arbitrage) — sells the equipment, pivots as traffic encrypts
 *   - state_surveillance_agencies: beneficiary with mandate-setting power (institutional/mobile) — collects from on-path vantage points, shapes interception requirements
 *   - cloud_load_balancer_operators: beneficiary-agenda-setter hybrid (institutional/arbitrage) — steers and terminates flows at scale
 *   - endpoint_applications: primary target (moderate/trapped) — bears rewriting, resetting, and throttling; discovers path behavior only through failure
 *   - residential_internet_users: primary target (powerless/trapped) — bears surveillance and interference invisibly; provider switching does not escape
 *   - transport_protocol_innovators: target with identity-locked exit (moderate/identity_locked) — careers bound to evolving a protocol the path freezes
 *   - ietf_standards_community: nominal agenda-setter whose writ is subordinate (institutional/identity_locked) — institutional identity fused with specification authorship
 *   - censored_jurisdiction_users: excluded voice (powerless/trapped) — governed by path policy with no seat in any governance venue
 *   - measurement_research_community: analytical observer (analytical/analytical) — maps path behavior from outside, no enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.62).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.55).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "TCP Middlebox Realism Reading — Deployed Path Behavior as Operative Standard").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "technological/network-protocol-coordination").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, '1f6cccca-e50a-48f0-9123-1731678043c2').
narrative_ontology:cs_kernel_codification('1f6cccca-e50a-48f0-9123-1731678043c2', formalized).
narrative_ontology:cs_authority_grounding('1f6cccca-e50a-48f0-9123-1731678043c2', practice).
narrative_ontology:cs_interpretation_layer_present('1f6cccca-e50a-48f0-9123-1731678043c2').
narrative_ontology:cs_reading_relation('1f6cccca-e50a-48f0-9123-1731678043c2', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('1f6cccca-e50a-48f0-9123-1731678043c2', rfc9293_tcp_specification__optimization_latitude_reading, influences).
narrative_ontology:cs_axiom('1f6cccca-e50a-48f0-9123-1731678043c2', foundational, wire_behavior_overrides_specification).
narrative_ontology:cs_axiom_status(wire_behavior_overrides_specification, holdable).
narrative_ontology:cs_axiom_grounding('1f6cccca-e50a-48f0-9123-1731678043c2', wire_behavior_overrides_specification, empirically_contingent).
narrative_ontology:cs_axiom('1f6cccca-e50a-48f0-9123-1731678043c2', foundational, interoperability_emerges_from_middlebox_tolerance).
narrative_ontology:cs_axiom_status(interoperability_emerges_from_middlebox_tolerance, holdable).
narrative_ontology:cs_axiom_grounding('1f6cccca-e50a-48f0-9123-1731678043c2', interoperability_emerges_from_middlebox_tolerance, empirically_contingent).
narrative_ontology:cs_reference_frame('1f6cccca-e50a-48f0-9123-1731678043c2', deployed_network_primacy).
narrative_ontology:cs_drift_state('1f6cccca-e50a-48f0-9123-1731678043c2', contemporary_encrypted_transport_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('1f6cccca-e50a-48f0-9123-1731678043c2', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, isp_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_security_teams).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_vendor_industry).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_agencies).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, cloud_load_balancer_operators).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_applications).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, residential_internet_users).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, transport_protocol_innovators).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, censored_jurisdiction_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate access and transit networks carrying consumer and business traffic. They deploy and configure the path equipment — address translators, stateful filters, traffic shapers, inspection appliances — that stands between endpoints. They gain address-space economics from translation, policy control over what traverses their links, and commercially valuable visibility into flows. They bear equipment costs, continuous maintenance as traffic encrypts, and periodic regulatory scrutiny. Stepping back from the practice would mean rearchitecting address plans and security posture simultaneously.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, isp_operators, agenda_setter,
    institutional, generational, constrained, continental).

% Run perimeter inspection and filtering for organizations. They decide what crosses the organizational boundary, log flows for audit, and block protocol behavior they do not recognize. They gain centralized policy control and audit trails; they pay for appliance fleets, tuning labor, and the breakage that follows when endpoints adopt newer transport features. Their alternative — endpoint-native security — would redistribute their budget and headcount.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_security_teams, agenda_setter,
    institutional, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_security_teams, beneficiary).

% Sells the inspection, filtering, and optimization appliances and virtual machines that operators deploy. Revenue scales with the complexity of what runs on the path. They invest in keeping their products relevant as traffic encrypts, and can pivot product lines toward new inspection surfaces faster than the operators who bought the previous generation.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, middlebox_vendor_industry, beneficiary,
    powerful, biographical, arbitrage, global).

% Collect communications metadata and, where legally empowered or covertly positioned, content from on-path vantage points. They benefit from the legibility of legacy protocol design and press for retention of interceptable interfaces; they also mandate interception-capability requirements that shape what operators deploy. Their position is jurisdictionally anchored and durable across political turnover.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_agencies, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_agencies, agenda_setter).

% Operate large-scale front-end proxies and load balancers that terminate and re-originate connections for hosted services. They gain traffic steering, volumetric attack absorption, and session control; they also impose their own connection behaviors on client flows. They can shift workloads between regions and protocol stacks with relative ease compared to fixed-line access operators.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, cloud_load_balancer_operators, beneficiary,
    institutional, biographical, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, cloud_load_balancer_operators, agenda_setter).

% Send and receive byte streams assuming the path preserves segment integrity, ordering, and timing semantics. In practice their connections are rewritten, split, delayed, reset, or throttled by equipment they cannot see or negotiate with. They discover path behavior only through failures, and respond with defensive workarounds — fallback ports, conservative timers, disabling newer extensions — that cost performance and delay feature adoption.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, endpoint_applications, payer,
    moderate, biographical, trapped, global).

% Use applications over access networks whose operators inspect, prioritize, and record their traffic. They experience the arrangement as slow pages, blocked services, and privacy exposure, without visibility into which device on the path did what. Switching providers rarely changes the treatment, since similar equipment dominates every access market they can reach.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, residential_internet_users, payer,
    powerless, immediate, trapped, global).

% Researchers and engineers extending the transport layer — multipath transfer, faster connection setup, explicit congestion notification, improved loss recovery. Their proposals repeatedly stall at deployment: intermediate equipment drops or mangles what it does not recognize. Careers, grants, and publication records are staked on evolving this protocol, so leaving the research program means abandoning accumulated expertise and communities.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, transport_protocol_innovators, payer,
    moderate, generational, identity_locked, global).

% Authors and maintains the transport specification, runs interoperability events, and registers behavioral expectations. Its documents define the protocol on paper, yet its writ extends only as far as deployed equipment tolerates: features it standardizes ship disabled or broken, and it responds by documenting path realities in companion memos rather than by compelling conformance. The organization's identity and process are bound to specification authorship itself.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, ietf_standards_community, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Live under national path-level filtering regimes that block, throttle, or rewrite specific services. They have no seat in operator procurement, standards deliberations, or vendor design; their objection to path-level content control is registered only as circumvention traffic, which the filtering regime treats as a signal to tighten further.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, censored_jurisdiction_users, excluded,
    powerless, immediate, trapped, national).

% Academic and industry groups that fingerprint path equipment from the outside, publishing maps of what intermediate devices do to flows. They hold no enforcement power; their analyses inform standards debate, regulatory inquiries, and endpoint defenses, and their measurement techniques are in a constant race with equipment that adapts to evade fingerprinting.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, measurement_research_community, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__middlebox_realism_reading, isp_operators).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__middlebox_realism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Path-resident equipment solves real problems the endpoint-centric specification never addressed: address translation conserves a depleted address space and lets edge networks grow without globally routable prefixes; perimeter filtering concentrates security policy at trust boundaries; load balancers and caches distribute demand across server pools; on-path control points give operators of huge networks a manageable place to steer traffic.
% TRANSFER_FUNCTION: Moves control and information rather than goods: packet-fate decisions move from endpoints to path operators; flow metadata and often payload visibility move from users to operators, vendors, and third-party collectors; the cost of protocol evolution moves onto innovators whose extensions the path discards; security labor is concentrated at perimeters, offloading endpoints.
% ABSENT_VOICES: Users under national filtering regimes have no seat anywhere in the arrangement's governance — not in operator procurement, not in vendor design, not in standards deliberation. Application developers encounter path behavior only as post-deployment breakage reports. Future users inherit the frozen protocol surface that ossification leaves them, and are represented by no one at deployment time.
% DISAPPEARANCE_RATIONALE: If path-level mediation vanished overnight, address translation alone would strand billions of devices without usable connectivity under the existing address plan; enterprise perimeters would fail closed; content delivery steering and attack absorption would collapse; and the endpoint stack would need wholesale re-engineering around globally routable addressing and endpoint-native security before traffic flowed reliably. The arrangements of nearly every named party depend on the current structure.
% FOUNDING_PROBLEM: The early internet assumed a flat, trusted topology with globally routable addresses and cooperative endpoints. Commercialization broke each assumption: the version-four address space began exhausting in the early 1990s, organizational boundaries became hostile, and operators of rapidly growing networks demanded workable control points. Path-resident equipment was deployed to solve address scarcity, perimeter defense, and operational manageability — problems the endpoint-focused specification never took on.
% FOUNDING_PROBLEM_CORROBORATION: Regional internet registry exhaustion data and address-allocation statistics attest the addressing problem was and remains real; the peer-reviewed measurement literature (path-interference and equipment-fingerprinting studies) attests from outside the benefiting parties that path behavior diverges from the specification and that extension deployment fails at rate; endpoint developer incident reports corroborate the same picture from the paying side. None of these corroborating sources collects from the arrangement.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__middlebox_realism_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__middlebox_realism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rfc9293_tcp_specification__middlebox_realism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(rfc9293_tcp_specification__middlebox_realism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.62: substantial, but discounted below snare-range because the same equipment renders real services — translation keeps the depleted address space viable, filtering enforces genuine policy, balancing distributes load. Suppression is authored at 0.55 as a raw structural property (per the framework, suppression is NOT scaled by power or scope — only extractiveness is scaled, by the engine, through directionality and scope): the mechanism is structural first — unrecognized packets are dropped or rewritten regardless of anyone's beliefs — with a smaller internalized component in which developers pre-emptively design for the middlebox, a learned caution that persists even on clean paths. Alternatives partially exist (encrypted transports, overlay tunnels, address-space transition), which caps accessibility_collapse at 0.45 rather than mountain-range values. Resistance is high at 0.6: a sustained standards-and-encryption counter-campaign, a measurement literature that documents interference publicly, and endpoint workarounds. Theater_ratio at 0.38 reflects a growing share of path activity that is compliance logging, checkbox security, and surveillance dressed as hygiene, atop a functional core. The temporal series run on ONE shared grid (points 0, 5, 10, 15, 20, 25, 30 of a 1995–2025 mapping) with all three tracked metrics authored at every point, so no metric row borrows another's end-state values. Trajectories are monotonic rather than cyclical: extraction and enforcement intensity ratchet upward as inspection capability diffuses, with no observed relaxation phase — the oscillation-driven intermittent-reinforcement pattern does not apply here.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute differently from identical wires. From the operator seat, the arrangement is infrastructure they built, maintain, and legitimately govern: address scarcity is real, perimeters are where their accountability lives, and the specification is one input among many to running a network. From the endpoint seats, the same structure is unilateral, opaque modification of their traffic by parties they cannot identify or negotiate with — the specification promised them semantics the path does not honor. The innovator seat adds a temporal dimension: what operators experience as prudent conservatism, innovators experience as a ratchet that kills each successive generation of protocol work. The engine derives these per-seat classifications from the authored structural data; this story's claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real recipients: operators gain address economics, policy control, and monetizable visibility; vendors gain sales; surveillance agencies gain collection; cloud operators gain steering and termination control; enterprise teams gain centralized policy. These sit near the beneficiary end of directionality, with vendor and cloud seats pushed further toward it by their arbitrage-grade exit. Victim declarations map to the paying side: endpoints and residential users bear interference and surveillance with trapped exit, placing them near the full-target end; innovators are pulled toward it doubly by identity lock — their professional selves are constituted by the evolution project the path defeats. The censored-user seat is the extreme target: total extraction of communication autonomy with zero exit and no voice. The standards community is the structurally odd seat: a nominal agenda-setter whose agenda does not govern — it neither collects meaningfully nor controls outcomes, and its identity lock keeps it in the game as the arrangement's designated aspirational author. No directionality overrides are authored: the derivation chain from beneficiary/victim declarations plus power and exit atoms reproduces these relationships without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — address exhaustion, hostile perimeters, unmanageable-scale operation — is still live, corroborated from outside the benefiting parties by registry exhaustion data and the independent measurement literature. Mandatrophy is therefore NOT resolved: the arrangement has not outlived its function, and no sunset clause exists or should. What the temporal series shows instead is accretion: a functional coordination core accumulating an extraction and theater layer on top, with theater_ratio climbing from 0.20 to 0.38 across the interval. The classification discipline matters in both directions here: labeling the arrangement pure capture would erase the real coordination that keeps the depleted-address-space internet running; labeling it pure coordination would erase the documented surveillance, unilateral control, and ossification that ride the same boxes. The tangled-rope claim holds both truths in one structure. On the mismatch consumer: founding_problem_status=live crossed with disappearance_verdict=world_rearranges is a consistent pairing — no zombie flag fires, correctly, because the function and the persistence genuinely still align.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing_underdetermination,
    'Is the operative constraining force of the TCP specification best characterized by this middlebox-realism reading (deployed path behavior governs; specification authority is subordinate), or by the sibling strict-invariance or optimization-latitude readings?',
    'Compile all three reading-stories of the kernel and compare authored epsilon, victim sets, and computed per-seat classifications against observed deployment outcomes (transport-extension failure rates, path-interference measurement literature). The reading whose structural data predicts the observed wire is vindicated.',
    'The sibling readings locate epsilon in the text''s constraining force rather than in the standing middlebox-mediated arrangement, yielding lower-extraction, coordination-flavored verdicts; this reading yields the tangled-rope profile with endpoint autonomy as the victim set. The disagreement is located in what counts as the authoritative definition of the protocol: the document or the deployed network.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing_underdetermination, conceptual, 'Committer-frame omega: this story instantiates the middlebox_realism_reading of kernel rfc9293_tcp_specification; sibling readings are separate constraints with different epsilon referents.').

omega_variable(
    coordination_extraction_separability,
    'Are the genuine coordination services path equipment provides (address conservation, perimeter filtering, load distribution) structurally separable from the surveillance and control extraction performed by the same boxes, or does one box necessarily do both?',
    'Natural experiments: IPv6-only deployments that remove address-translation necessity; jurisdictions with and without mandated inspection capability; compare whether control and collection behavior persists when the coordination function is engineered away.',
    'If separable, the extraction component is a pure-capture layer riding on real coordination and the victim set narrows accordingly; if inseparable, part of the measured extraction is the irreducible price of the coordination itself and the tangled-rope classification is stabilized.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_extraction_separability, empirical, 'Whether the arrangement''s coordination and extraction components can be disentangled.').

omega_variable(
    encryption_erosion_trajectory,
    'Will endpoint-side encryption (authenticated encryption expansion, encrypted transport migration, encrypted client hello) restore endpoint autonomy over packet fate, or will path operators reassert control through encrypted-traffic analysis and selective blocking of the new transports?',
    'Longitudinal measurement of new-transport success rates across access networks; diffusion tracking of encrypted-traffic-analysis product capability; regulatory actions mandating or forbidding inspection of encrypted flows.',
    'If erosion wins, this arrangement decays toward vestigial status for legacy traffic while a successor arrangement forms around the encrypted transports; if reassertion wins, suppression rises above the authored scalar and the enforcement-intensification trajectory continues.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(encryption_erosion_trajectory, empirical, 'Direction of the arms race between endpoint encryption and path-level control.').

omega_variable(
    ossification_reversibility,
    'Is transport-layer ossification a reversible condition (coordinated equipment refresh, mandatory-to-implement updates, certification programs) or a one-way ratchet in which each unrecognized extension permanently poisons the path for its successors?',
    'Track deployment success rates of post-2010 transport extensions against measured middlebox failure rates over time; compare networks that underwent deliberate equipment refresh against those that did not.',
    'Reversibility lowers long-run extraction and supports a coordination-dominant reading; a confirmed ratchet supports drift toward pure capture and strengthens the case that the specification''s authority loss is permanent rather than cyclical.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ossification_reversibility, empirical, 'Whether the freezing of protocol evolution on the wire can be undone.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9293_mb_realism_tr_t0, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(rfc9293_mb_realism_tr_t5, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement(rfc9293_mb_realism_tr_t10, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 10, 0.28).
narrative_ontology:measurement(rfc9293_mb_realism_tr_t15, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 15, 0.32).
narrative_ontology:measurement(rfc9293_mb_realism_tr_t20, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 20, 0.34).
narrative_ontology:measurement(rfc9293_mb_realism_tr_t25, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 25, 0.36).
narrative_ontology:measurement(rfc9293_mb_realism_tr_t30, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 30, 0.38).

% Extraction over time
narrative_ontology:measurement(rfc9293_mb_realism_be_t0, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(rfc9293_mb_realism_be_t5, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(rfc9293_mb_realism_be_t10, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(rfc9293_mb_realism_be_t15, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 15, 0.54).
narrative_ontology:measurement(rfc9293_mb_realism_be_t20, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(rfc9293_mb_realism_be_t25, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(rfc9293_mb_realism_be_t30, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(rfc9293_mb_realism_su_t0, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(rfc9293_mb_realism_su_t5, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 5, 0.36).
narrative_ontology:measurement(rfc9293_mb_realism_su_t10, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement(rfc9293_mb_realism_su_t15, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement(rfc9293_mb_realism_su_t20, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(rfc9293_mb_realism_su_t25, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 25, 0.53).
narrative_ontology:measurement(rfc9293_mb_realism_su_t30, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 30, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, resource_allocation).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__optimization_latitude_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, ipv6_deployment_transition).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, quic_encrypted_transport_migration).

% DUAL FORMULATION NOTE:
% The colloquial label 'the TCP specification' decomposes into three structurally distinct constraints — three readings of one kernel. The strict-invariance reading treats the text as a binding state machine (low epsilon, text-referent); the optimization-latitude reading treats it as an outcome contract permitting bounded implementation freedom (low-to-moderate epsilon, text-referent); this middlebox-realism reading treats the deployed path population as the operative standard with the specification subordinate (highest epsilon of the family, referent is the standing middlebox-mediated arrangement). Family edges run upstream-textual to downstream-deployed because specification language is routinely cited as cover for path practices. Each member carries its own epsilon, beneficiaries, and victims per the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
