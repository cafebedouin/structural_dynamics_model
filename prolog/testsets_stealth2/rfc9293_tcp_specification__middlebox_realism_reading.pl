% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__middlebox_realism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: rfc9293_tcp_specification__middlebox_realism_reading
 *   human_readable: TCP Middlebox Realism Regime (Path-Subordinate Specification Reading)
 *   domain: technological/network-standards
 *
 * SUMMARY:
 *   On real Internet paths, TCP behavior is determined jointly by endpoint
 *   stacks and the deployed middlebox population: carrier-grade NAT rewrites
 *   addressing and terminates connections, stateful firewalls drop unfamiliar
 *   traffic, deep-packet-inspection systems read and classify flows, and
 *   packets that violate accumulated path expectations are silently discarded
 *   or reset. Under this reading, RFC 9293 functions as a description of
 *   ideal endpoint behavior whose authority is subordinate to what deployed
 *   path equipment permits; the operative protocol is path-dependent, and the
 *   specification community governs only where the path declines to
 *   intervene. The epsilon referent is the standing de facto arrangement —
 *   middlebox-mediated TCP as it actually operates — assessed by this
 *   reading's own lights, never the hypothetical fully spec-governed network
 *   this reading's critics prefer. The claimed type and the metrics below are
 *   independent authored facts: the claim states the structure I believe true
 *   (genuine coordination function plus asymmetric transfer of control,
 *   actively enforced); the metrics state what I believe descriptively
 *   accurate of the arrangement's operation. KEY AGENTS (by structural
 *   relationship): - isp_nat_dpi_operators: Primary agenda-setter and
 *   collector (institutional/arbitrage) — administers the de facto standard
 *   through equipment policy - state_traffic_inspection_agencies: Primary
 *   beneficiary of visibility (institutional/mobile) — collects intelligence
 *   from path position without administering anything -
 *   enterprise_firewall_administrators: Secondary beneficiary and local
 *   enforcer (organized/constrained) — enforces perimeter policy, collects
 *   security value - hyperscale_endpoint_operators: Target with partial exit
 *   (powerful/mobile) — bears ossification, engineers around the path, builds
 *   encrypted bypass - independent_application_developers: Target
 *   (moderate/constrained) — bears breakage and workaround burden without
 *   leverage - ietf_specification_authors: Target of authority displacement
 *   (institutional/constrained) — specification becomes aspiration at the
 *   middlebox line - end_user_privacy_bearers: Diffuse target
 *   (powerless/constrained) — bears visibility and alteration without consent
 *   or audit - application_layer_innovators: Excluded voice
 *   (moderate/trapped) — locked out by path expectations before deploying -
 *   network_measurement_researchers: Analytical observer
 *   (analytical/analytical) — documents the gap between documented and
 *   deployed behavior
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.66).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.7).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "TCP Middlebox Realism Regime (Path-Subordinate Specification Reading)").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "technological/network-standards").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, '74985e53-533a-4de1-8174-be9db2324e17').
narrative_ontology:cs_kernel_codification('74985e53-533a-4de1-8174-be9db2324e17', formalized).
narrative_ontology:cs_authority_grounding('74985e53-533a-4de1-8174-be9db2324e17', expertise).
narrative_ontology:cs_interpretation_layer_present('74985e53-533a-4de1-8174-be9db2324e17').
narrative_ontology:cs_reading_relation('74985e53-533a-4de1-8174-be9db2324e17', rfc9293_tcp_specification__strict_invariance_reading, forecloses).
narrative_ontology:cs_reading_relation('74985e53-533a-4de1-8174-be9db2324e17', rfc9293_tcp_specification__optimization_latitude_reading, influences).
narrative_ontology:cs_axiom('74985e53-533a-4de1-8174-be9db2324e17', foundational, path_population_is_protocol_authority).
narrative_ontology:cs_axiom_status(path_population_is_protocol_authority, holdable).
narrative_ontology:cs_axiom_grounding('74985e53-533a-4de1-8174-be9db2324e17', path_population_is_protocol_authority, empirically_contingent).
narrative_ontology:cs_axiom('74985e53-533a-4de1-8174-be9db2324e17', foundational, endpoint_autonomy_subordinate_to_path_policy).
narrative_ontology:cs_axiom_status(endpoint_autonomy_subordinate_to_path_policy, holdable).
narrative_ontology:cs_axiom_grounding('74985e53-533a-4de1-8174-be9db2324e17', endpoint_autonomy_subordinate_to_path_policy, instrumental).
narrative_ontology:cs_reference_frame('74985e53-533a-4de1-8174-be9db2324e17', spec_as_endpoint_idealization).
narrative_ontology:cs_drift_state('74985e53-533a-4de1-8174-be9db2324e17', post_encrypted_transport_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('74985e53-533a-4de1-8174-be9db2324e17', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, isp_nat_dpi_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_firewall_administrators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, state_traffic_inspection_agencies).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, hyperscale_endpoint_operators).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, independent_application_developers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, ietf_specification_authors).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, end_user_privacy_bearers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, hyperscale_endpoint_operators).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__middlebox_realism_reading, deployed_base_primacy_doctrine).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__middlebox_realism_reading, nat_address_scarcity_management).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate carrier-grade NAT and deep-packet-inspection equipment across access networks. Their forwarding decisions — rewriting addresses, terminating and re-originating connections, resetting unexpected traffic, prioritizing or throttling flows — collectively define what TCP behavior survives on real paths. They save address-space costs through multiplexing, sell traffic-management and inspection capability, and face little pressure to honor endpoint expectations their equipment does not recognize. Exit is easy in the relevant sense: they refresh equipment on procurement cycles and reconfigure policy at will.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, isp_nat_dpi_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, isp_nat_dpi_operators, beneficiary).

% Run stateful firewalls, proxies, and policy enforcement points at organizational boundaries. They collect breach-prevention and compliance value from inspecting and filtering traffic crossing their perimeter, and they configure the boxes that drop unfamiliar protocols and ports. Their professional identity is built around perimeter defense; replacing it would mean re-architecting security around zero-trust models at a cost their organizations resist.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_firewall_administrators, beneficiary,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_firewall_administrators, agenda_setter).

% Position themselves on the path through lawful-intercept mandates, compelled-operator assistance, or passive collection, and gather traffic metadata and, where encryption permits, content. They bear none of the administration burden and collect intelligence continuously. Their collection points can move as network topology changes.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, state_traffic_inspection_agencies, beneficiary,
    institutional, civilizational, mobile, national).

% Serve traffic at planetary scale and feel path interference acutely: new transport features fail to deploy through middlebox populations, connections break on restrictive networks, and they maintain large engineering teams writing fallback and workaround logic. They also benefit from universal TCP interoperability, which is why they remain on the protocol. Their scale funds a partial exit — encrypted transports that hide traffic from intermediate inspection — which they have begun building.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, hyperscale_endpoint_operators, payer,
    powerful, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, hyperscale_endpoint_operators, beneficiary).

% Ship applications that must traverse networks they do not control. Connections fail on restrictive paths, nonstandard ports are blocked, and their practical remedy is to confine themselves to vanilla TCP on well-known ports. They have no leverage to negotiate with network operators and no budget to duplicate the workarounds larger firms maintain.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, independent_application_developers, payer,
    moderate, immediate, constrained, global).

% Publish and maintain the TCP specification through a rough-consensus process. Their documents describe behavior that endpoint implementations largely follow but that the path population freely overrides; extensions they standardize routinely die in deployment. They cannot exit — specifying is their mandate — and their authority stops where the middleboxes begin.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, ietf_specification_authors, payer,
    institutional, generational, constrained, global).

% Generate the traffic that intermediate systems inspect, prioritize, and sometimes alter. They cannot audit what happens to their packets in transit, did not consent to the inspection, and their partial escapes — TLS everywhere, paid VPNs — cover some but not all of the exposure at real cost.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, end_user_privacy_bearers, payer,
    powerless, immediate, constrained, global).

% Want to deploy new application transports or run services outside the well-worn port and protocol grooves. The path's accumulated expectations exclude them before they start: their packets are dropped or degraded by equipment tuned to legacy TCP. They hold no seat in operator procurement or in the processes that set what the path tolerates.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, application_layer_innovators, excluded,
    moderate, biographical, trapped, global).

% Probe and document what the path actually does to traffic — which options survive, which get stripped, where resets originate. Their measurement literature is the main public record of the gap between documented and deployed behavior. They enforce nothing and collect nothing from the arrangement.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, network_measurement_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__middlebox_realism_reading, isp_nat_dpi_operators).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__middlebox_realism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The middlebox population solves real problems: NAT multiplexes many hosts onto scarce IPv4 addresses; stateful firewalls enforce administrator security policy at trust boundaries; path equipment manages congestion and link heterogeneity. Stated without evaluation of how these functions are financed or extended.
% TRANSFER_FUNCTION: Moves behavioral control over connections and visibility into traffic content from endpoints and users to path operators; moves engineering burden (fallback logic, workaround maintenance) from the network to endpoint implementers; moves traffic metadata, and content where encryption permits, from users to operators and inspection agencies.
% ABSENT_VOICES: Application-layer innovators and would-be new-transport deployers are structurally excluded — the path's expectations are set by operator procurement and legacy tuning without their participation, and their exclusion is maintained by the same equipment that enforces the arrangement. End users affected by inspection hold no seat in operator procurement decisions; their only channel is the specification process that this reading shows to be subordinate.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, the rearrangement would be immediate and severe: billions of devices behind carrier-grade NAT lose routable connectivity until addresses are renumbered or IPv6 completes; enterprise interiors lose their perimeter model with nothing pre-built to replace it; inspection agencies lose their collection geometry; and the frozen surface of transport innovation unfreezes, as endpoints begin deploying behaviors the path previously punished. Nothing about the current network survives the removal intact.
% FOUNDING_PROBLEM: During the commercial Internet's explosive mid-1990s growth, IPv4 address space was running out faster than renumbering could cope, and organizations connecting to a hostile global network needed a defensible boundary. NAT addressed the first; the stateful firewall addressed the second. The middlebox population is the accumulated answer to those two problems.
% FOUNDING_PROBLEM_CORROBORATION: Regional internet registry exhaustion records (RIPE NCC, APNIC, ARIN) independently document continued IPv4 scarcity decades past forecast; academic measurement programs (CAIDA, IMC literature) corroborate the prevalence and behavior of the deployed middlebox population from outside the operator set; incident-response and cyber-insurance reporting corroborates that boundary threats remain material. None of these sources is a beneficiary of the arrangement, and all attest the founding problems persist — the dispute among parties concerns whether the arrangement's current scope still serves those problems, not whether the problems exist.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__middlebox_realism_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__middlebox_realism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is 0.66 because the arrangement transfers durable value — behavioral control, traffic visibility, deployment veto power — from endpoints and the specification community to path operators, while leaving the genuine service legs (address multiplexing, perimeter filtering) intact; it is high but not extreme because much of what the path does would be done anyway under any multi-administrator network. Suppression is 0.70 as a raw structural property, unscaled by power or scope: the arrangement persists through active packet-level force (drops, resets, rewrites, port blocks), not through participant preference, and alternatives are met with escalating interference rather than accommodation. Theater ratio is 0.42: conformance testing, specification citations, and compliance language continue as ceremony while the binding rules live in equipment configurations; the specification's governing role is roughly half performance at interval end. Accessibility collapse is 0.52 — alternatives exist (encrypted transports, VPNs, private links) but are costly, partial, and themselves met with path-side adaptation, so alternatives degrade without vanishing. Resistance is 0.60: the encrypted-transport movement is a deliberate, well-resourced countermeasure, and the measurement community continuously documents interference. The temporal series run on one shared six-point grid (1994–2022) with all three metrics authored at every point; the rising suppression_requirement series is authored deliberately because this story specifically tracks enforcement-capacity build-up — from sparse packet filters to ubiquitous stateful inspection and active reset injection — not merely shifting extraction.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently from the same structure. From the operator seat the arrangement is infrastructure it built and legitimately administers: address conservation and perimeter defense are its job, and endpoint complaints read as customers demanding exceptions. From the endpoint developer seats the same equipment is an unaccountable veto player that breaks connections and forbids innovation. From the specification seat it is a slow usurpation — authority draining from published documents to procurement catalogs. From the user seat it is invisible: inspection leaves no trace in the interface. Same-level lateral divergence matters among the targets: hyperscale endpoint operators and independent developers occupy the same nominal side with opposite exits — the former fund encrypted transports that route around inspection, the latter are confined to vanilla TCP on well-known ports, so identical path behavior produces mobility for one and entrapment for the other. Identity-lock operates on the enterprise administrator seat: the perimeter-defense self-concept (castle-and-moat professionalism) fuses the agent to the arrangement; if that frame broke — if the profession accepted that perimeters are obsolete — the administrator seat would flip from defender of the arrangement to its critic, and the classification of that seat would move accordingly.
 *
 * DIRECTIONALITY LOGIC:
 *   The engine derives directionality from the declared beneficiary/victim structure plus exit atoms; no overrides are needed because the declarations alone differentiate every seat. The three beneficiary groups sit near the beneficiary end: operators collect cost savings and traffic-management revenue while controlling the enforcement machinery (arbitrage exit pushes them further toward subsidy); inspection agencies collect intelligence continuously from a passive position; enterprise administrators collect security value. The four victim groups sit near the target end: specification authors bear full authority displacement with no exit (their mandate is to specify); independent developers and privacy bearers are constrained; hyperscale operators are pulled back toward symmetry by their partial encrypted-transport exit, which is exactly why their effective burden reads lower than their nominal exposure. The measurement-researcher seat is analytical and feeds no directionality. Scope amplification applies mildly: the arrangement is global, so verification of what any given path actually does is hard, which the engine reflects in effective extraction for the target seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled-rope classification is what prevents both mislabelings. Calling this a snare would erase the genuine coordination legs — address multiplexing and perimeter filtering solve real, externally corroborated problems — and would mispredict remedies, since abolishing the arrangement outright strands address allocation for billions of devices and exposes enterprise interiors. Calling it a rope would erase the asymmetric transfer: control and visibility move one way, engineering burden moves the other, and protocol evolution is suppressed by the same structure that performs the coordination. Holding both facts together is the point. On genealogy: the founding problem (IPv4 scarcity, perimeter defense during the commercial Internet's growth) remains live per external corroboration, so no mandatrophy is declared and the arrangement is not a zombie — but the live founding problem is precisely what launders the scope growth (surveillance capability, deployment veto) that the founding problem never demanded. The risk signature to watch is not dead-problem persistence but live-problem expansion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_authority_reading_contest,
    'Which reading of kernel rfc9293_tcp_specification correctly characterizes the specification''s authority relation to deployed behavior: path-subordinate (this story), exactly-replicated invariant state machine (strict_invariance_reading), or outcome-fixing with implementation latitude (optimization_latitude_reading)?',
    'Comparative compilation of all three sibling stories, each with its own epsilon and victim structure; convergence or divergence of computed types across readings locates where the disagreement is resolvable by evidence versus irreducibly conceptual.',
    'The sibling readings instantiate different constraints: strict invariance makes implementation fidelity the victim set, optimization latitude makes semantic-bound disputes the contested surface, and this reading makes endpoint autonomy the victim set. Resolving the contest shifts which story carries the operative classification of TCP governance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_authority_reading_contest, conceptual, 'This story is one reading of the RFC 9293 kernel; the sibling readings are separate constraints, and the contest among them is routed here rather than folded into this story''s classification.').

omega_variable(
    dpi_function_composition,
    'What fraction of deep-packet-inspection deployment is genuinely motivated by security and traffic management versus surveillance and monetization?',
    'Operator procurement records, feature-activation telemetry across jurisdictions with different surveillance law, and disclosure under regulatory inquiry.',
    'A high security share supports the genuine-coordination leg and stabilizes the tangled-rope classification; a high surveillance share shifts the balance toward pure extraction riding on a shrinking coordination cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dpi_function_composition, empirical, 'Composition of the inspection function between defensive coordination and surveillance extraction.').

omega_variable(
    ossification_attribution,
    'Is the observed failure of new transport features to deploy attributable to middlebox interference specifically, or jointly to endpoint software conservatism and incentive gaps?',
    'Controlled deployment experiments comparing rollout curves for encrypted transports versus plaintext extensions across matched path populations.',
    'If middleboxes are the binding constraint, the extraction attribution to path operators strengthens and the victim assignment holds; if endpoint conservatism dominates, part of the measured suppression belongs to a different constraint and this story''s victim set narrows.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ossification_attribution, empirical, 'Causal attribution of protocol ossification between path equipment and endpoint inertia.').

omega_variable(
    middlebox_prevalence_contingency,
    'Is pervasive middlebox deployment a contingent artifact of IPv4 scarcity and particular regulatory choices, or a structural inevitability of networks spanning multiple administrative domains?',
    'Counterfactual analysis of IPv6-complete deployments and zero-trust architectures at scale: does equivalent path intervention re-emerge under abundance?',
    'If contingent, the arrangement is remediable policy and transitional framings gain force; if structural, the arrangement approaches natural-law permanence and this reading hardens toward treating the path population as an immovable term of the protocol environment.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(middlebox_prevalence_contingency, conceptual, 'Whether the middlebox population is a removable construction or a durable feature of multi-administrator networking.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 1994, 2022).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t1994, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 1994, 0.15).
narrative_ontology:measurement_basis(rfc9_tr_t1994, observed).
narrative_ontology:measurement(rfc9_tr_t2000, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement_basis(rfc9_tr_t2000, observed).
narrative_ontology:measurement(rfc9_tr_t2006, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2006, 0.3).
narrative_ontology:measurement_basis(rfc9_tr_t2006, observed).
narrative_ontology:measurement(rfc9_tr_t2011, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2011, 0.35).
narrative_ontology:measurement_basis(rfc9_tr_t2011, observed).
narrative_ontology:measurement(rfc9_tr_t2016, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2016, 0.39).
narrative_ontology:measurement_basis(rfc9_tr_t2016, observed).
narrative_ontology:measurement(rfc9_tr_t2022, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2022, 0.42).
narrative_ontology:measurement_basis(rfc9_tr_t2022, observed).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t1994, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 1994, 0.35).
narrative_ontology:measurement_basis(rfc9_be_t1994, observed).
narrative_ontology:measurement(rfc9_be_t2000, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement_basis(rfc9_be_t2000, observed).
narrative_ontology:measurement(rfc9_be_t2006, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2006, 0.55).
narrative_ontology:measurement_basis(rfc9_be_t2006, observed).
narrative_ontology:measurement(rfc9_be_t2011, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2011, 0.6).
narrative_ontology:measurement_basis(rfc9_be_t2011, observed).
narrative_ontology:measurement(rfc9_be_t2016, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2016, 0.64).
narrative_ontology:measurement_basis(rfc9_be_t2016, observed).
narrative_ontology:measurement(rfc9_be_t2022, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2022, 0.66).
narrative_ontology:measurement_basis(rfc9_be_t2022, observed).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t1994, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 1994, 0.25).
narrative_ontology:measurement_basis(rfc9_su_t1994, observed).
narrative_ontology:measurement(rfc9_su_t2000, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2000, 0.38).
narrative_ontology:measurement_basis(rfc9_su_t2000, observed).
narrative_ontology:measurement(rfc9_su_t2006, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2006, 0.52).
narrative_ontology:measurement_basis(rfc9_su_t2006, observed).
narrative_ontology:measurement(rfc9_su_t2011, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2011, 0.62).
narrative_ontology:measurement_basis(rfc9_su_t2011, observed).
narrative_ontology:measurement(rfc9_su_t2016, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2016, 0.68).
narrative_ontology:measurement_basis(rfc9_su_t2016, observed).
narrative_ontology:measurement(rfc9_su_t2022, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2022, 0.7).
narrative_ontology:measurement_basis(rfc9_su_t2022, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, resource_allocation).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__optimization_latitude_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of kernel rfc9293_tcp_specification. The colloquial label 'the TCP specification' covers three structurally distinct claims about what the document is: an exactly-replicated invariant state machine (strict_invariance_reading), an outcome contract with implementation latitude (optimization_latitude_reading), and an idealized endpoint description subordinate to the deployed path (this story). Each is a separate file with its own epsilon, beneficiary/victim structure, and classification; they are linked here because the upstream readings are cited as authority in disputes that this reading adjudicates empirically. This story sits downstream: measurement evidence of path-dependent behavior is what pressures the other two readings' premises, which is why the edges run from this story to its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
