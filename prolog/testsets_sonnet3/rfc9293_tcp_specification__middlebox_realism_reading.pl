% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__middlebox_realism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
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
 *   constraint_id: rfc9293_tcp_specification__middlebox_realism_reading
 *   human_readable: RFC 9293 TCP Specification Under Middlebox Realism
 *   domain: network_protocol_engineering/internet_standards
 *
 * SUMMARY:
 *   This story reads RFC 9293 not as an authoritative description of TCP but
 *   as an aspirational text whose real-world force is subordinate to the
 *   deployed population of middleboxes — NAT gateways, firewalls, DPI
 *   appliances, and state-operated interception infrastructure — that
 *   actually determine what packets survive a path and in what shape. Under
 *   this reading, the specification's coordination promise (predictable
 *   interoperable endpoint behavior) has been substantially captured: the
 *   entities that control the physical path extract policy-enforcement,
 *   surveillance, and rent-like control over protocol evolution from their
 *   position, while the costs (broken extensibility, connection failures,
 *   foreclosed privacy, defensive engineering burden) land on implementers,
 *   application developers, and end users who have no seat in middlebox
 *   deployment decisions. This is a Tangled Rope, not a pure Snare: the
 *   middlebox population does provide genuine, non-fake coordination
 *   functions (NAT genuinely conserves IPv4 address space; enterprise
 *   firewalls genuinely reduce certain attack surface; some interception is
 *   lawful-intercept infrastructure under judicial process) — the
 *   coordination is real, but it rides alongside asymmetric extraction of
 *   control from endpoints who never agreed to it and often cannot detect it.
 *
 * KEY AGENTS:
 *   - isp_traffic_management_operators: primary beneficiary, institutional power, arbitrage exit — controls the path
 *   - state_surveillance_agencies: primary beneficiary, institutional power, arbitrage exit — extracts intelligence value from path position
 *   - protocol_implementers: primary payer, moderate power, constrained exit — must defensively code around unknown middlebox behavior
 *   - privacy_seeking_endpoints: primary victim, powerless, trapped exit — bears the sharpest cost with least recourse
 *   - ietf_tcpm_working_group: analytical observer — documents the gap but cannot enforce against it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.68).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.71).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "RFC 9293 TCP Specification Under Middlebox Realism").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "network_protocol_engineering/internet_standards").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, 'dbdbb842-9f23-4986-b8ad-2ad19d8c0e59').
narrative_ontology:cs_kernel_codification('dbdbb842-9f23-4986-b8ad-2ad19d8c0e59', fixed_text).
narrative_ontology:cs_authority_grounding('dbdbb842-9f23-4986-b8ad-2ad19d8c0e59', practice).
narrative_ontology:cs_interpretation_layer_present('dbdbb842-9f23-4986-b8ad-2ad19d8c0e59').
narrative_ontology:cs_reading_relation('dbdbb842-9f23-4986-b8ad-2ad19d8c0e59', rfc9293_tcp_specification__strict_invariance_reading, coexists_with).
narrative_ontology:cs_reading_relation('dbdbb842-9f23-4986-b8ad-2ad19d8c0e59', rfc9293_tcp_specification__optimization_latitude_reading, influences).
narrative_ontology:cs_axiom('dbdbb842-9f23-4986-b8ad-2ad19d8c0e59', foundational, deployed_practice_supersedes_specified_text).
narrative_ontology:cs_axiom_status(deployed_practice_supersedes_specified_text, holdable).
narrative_ontology:cs_axiom_grounding('dbdbb842-9f23-4986-b8ad-2ad19d8c0e59', deployed_practice_supersedes_specified_text, empirically_contingent).
narrative_ontology:cs_axiom('dbdbb842-9f23-4986-b8ad-2ad19d8c0e59', secondary, path_control_constitutes_de_facto_protocol_authority).
narrative_ontology:cs_axiom_status(path_control_constitutes_de_facto_protocol_authority, holdable).
narrative_ontology:cs_axiom_grounding('dbdbb842-9f23-4986-b8ad-2ad19d8c0e59', path_control_constitutes_de_facto_protocol_authority, empirically_contingent).
narrative_ontology:cs_reference_frame('dbdbb842-9f23-4986-b8ad-2ad19d8c0e59', endpoint_specified_state_machine_authority).
narrative_ontology:cs_drift_state('dbdbb842-9f23-4986-b8ad-2ad19d8c0e59', post_ossification_measurement_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('dbdbb842-9f23-4986-b8ad-2ad19d8c0e59', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, isp_traffic_management_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_firewall_administrators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_agencies).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, carrier_grade_nat_operators).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, protocol_implementers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, end_to_end_application_developers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, privacy_seeking_endpoints).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, novel_transport_extension_designers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy middleboxes (NAT, deep packet inspection, traffic shaping, TCP normalization) inline on subscriber paths. They rewrite sequence numbers, strip unrecognized options, reset connections that don't match expected state-machine shapes, and throttle flows that look anomalous. Their deployed behavior effectively overrides RFC 9293's endpoint-to-endpoint semantics for anyone who traverses their network, and they bear none of the compatibility costs their boxes impose on others.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, isp_traffic_management_operators, beneficiary,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, isp_traffic_management_operators, agenda_setter).

% Operate stateful firewalls and proxies that terminate, inspect, and re-originate TCP connections for security policy enforcement. They benefit from the ambiguity RFC 9293 leaves in undefined edge-case behavior, since their boxes' idiosyncratic interpretations become the de facto standard inside their networks. They can update or remove their boxes largely at will.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_firewall_administrators, beneficiary,
    organized, biographical, arbitrage, regional).

% Compel or co-locate with carrier infrastructure to perform traffic injection, connection resets, and fingerprinting keyed to TCP option ordering and timing behavior. They rely on the gap between specified endpoint behavior and actual on-path behavior to justify interception as 'network management' rather than interference, and face essentially no exit constraint of their own.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_agencies, beneficiary,
    institutional, civilizational, arbitrage, national).

% Multiplex many subscriber endpoints behind shared address/port pools, rewriting connection state at scale to manage IPv4 exhaustion. Their boxes impose connection-count limits, timeout policies, and port-reuse behaviors invisible to the specification but binding on every endpoint that transits them.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, carrier_grade_nat_operators, beneficiary,
    institutional, generational, arbitrage, national).

% Write TCP stacks that must work across the actual deployed internet, not the RFC's idealized state machine. They discover through field failures — silently dropped options, mangled window scaling, spuriously reset connections — which middlebox behaviors they must defensively code around. They cannot negotiate with the middleboxes; they can only guess and patch.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, protocol_implementers, payer,
    moderate, biographical, constrained, global).

% Build applications assuming RFC-conformant transport and then debug production incidents caused by an ossified path: connections that hang because ECN or window scaling was stripped, NAT timeout-induced drops for idle long-lived connections, or fingerprint-based throttling of protocols that don't resemble expected traffic shapes. They pay in engineering time and degraded user experience for a specification the network does not actually honor.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, end_to_end_application_developers, payer,
    moderate, biographical, constrained, global).

% Individuals and organizations attempting confidential or censorship-resistant communication find that on-path middleboxes can reset, throttle, or flag connections based on TCP-level fingerprinting regardless of payload encryption. Their only real recourse — tunneling, obfuscation, alternate transports — is itself a cost imposed by the middlebox population's control over the wire format, and in many jurisdictions there is no accessible alternate path at all.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, privacy_seeking_endpoints, payer,
    powerless, immediate, trapped, global).

% Propose new TCP options or extensions (e.g., new congestion signaling, multipath, novel option kinds) and find that a substantial fraction of paths silently strip or mishandle unrecognized options. Ossification caused by the deployed middlebox population means the specification's extensibility provisions are effectively dead on arrival for large parts of the internet, regardless of what RFC 9293 authorizes.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, novel_transport_extension_designers, payer,
    moderate, generational, constrained, global).

% Maintains and revises the specification text, documents known middlebox interference patterns (e.g., in RFC 3234 and follow-on ossification literature), and issues guidance, but has no enforcement mechanism over deployed hardware. It can describe what middleboxes do; it cannot compel them to stop.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, ietf_tcpm_working_group, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(rfc9293_tcp_specification__middlebox_realism_reading, diffuse).
narrative_ontology:fixing_cost_class(rfc9293_tcp_specification__middlebox_realism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The specification text nominally coordinates endpoint implementers around one predictable state machine, so that any two RFC-conformant hosts can interoperate without prior negotiation about connection semantics.
% TRANSFER_FUNCTION: Moves effective control over what 'TCP' means — over connection lifetime, option acceptance, timing, and permissible extension — from the two communicating endpoints and the specification's authors to the operators of on-path middleboxes, who extract policy-enforcement and surveillance capability from their position in the path at the cost of endpoint autonomy and protocol evolvability.
% ABSENT_VOICES: The IETF working group that authored RFC 9293 has no seat in any individual middlebox deployment decision; individual end users whose connections are reset or throttled have no visibility into which box did it or why; would-be designers of new transport mechanisms are not represented in the ISP/enterprise/state procurement decisions that determine whether their extensions will actually traverse the deployed internet.
% DISAPPEARANCE_RATIONALE: If the middlebox population's de facto authority over TCP semantics vanished overnight — if every path became RFC-conformant end-to-end — implementers would drop enormous quantities of defensive workaround code, previously stalled extensions (new options, alternate congestion signaling) would become deployable, censorship and traffic-shaping regimes keyed to TCP fingerprinting would lose their mechanism, and NAT/firewall-driven connection-count and timeout constraints that currently shape application design would disappear. The shape of practical internet engineering would change substantially.
% FOUNDING_PROBLEM: TCP needed a single, precisely specified state machine so that independently implemented endpoints across an unbounded number of networks could interoperate reliably without out-of-band coordination — this is the problem RFC 9293 (and its predecessor RFC 793) was built to solve.
% FOUNDING_PROBLEM_CORROBORATION: The IETF tcpm working group and independent network-measurement researchers (e.g., studies on TCP option stripping and NAT/firewall interference published outside any single vendor's interest) attest that the interoperability problem the specification was built to solve is now only partially addressed by the text itself, because deployed middlebox behavior — not the RFC — determines what actually interoperates on most real paths. ISPs and enterprise security vendors, the parties who benefit from this state, characterize their middlebox behavior as necessary network management rather than as a deviation from the founding interoperability goal, which is why the status is contested rather than settled as dead.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__middlebox_realism_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__middlebox_realism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_sonnet3', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 0.68, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness (0.68) and suppression (0.71) are both substantial and rising over the measured interval because the middlebox population's control over the wire format has hardened from occasional NAT boxes in the 1990s to a nearly universal, multi-layered interception and shaping infrastructure by the 2020s. Theater ratio (0.42) reflects that a meaningful share of activity attributed to 'network management' (traffic normalization, security filtering) increasingly functions as policy enforcement and surveillance rather than the narrower technical purpose it is publicly justified by — this is not pure theater (some functions are real) but a rising share is performative cover for control extraction. Accessibility collapse (0.6) is moderate-high: alternatives exist (VPNs, obfuscated transports, QUIC-over-UDP migration) but are themselves costly and increasingly targeted by the same middlebox population. Resistance (0.55) reflects active engineering pushback — protocol ossification research, encrypted transport design, QUIC's deliberate move to UDP to escape TCP-specific middlebox interference — but this resistance operates around the constraint rather than dismantling it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (ISPs, enterprise firewall operators, state agencies, CGN operators) all sit near institutional power with arbitrage-grade exit: they can modify, replace, or route around their own middleboxes at will, and they capture the coordination value (address conservation, security posture, intelligence access) while imposing costs elsewhere. Victims (implementers, application developers, privacy-seeking endpoints, extension designers) have constrained-to-trapped exit: an implementer cannot refuse to handle middlebox interference and still ship a working stack; a privacy-seeking individual in a jurisdiction with pervasive interception has no path-level alternative at all. This asymmetry — arbitrage exit for those who control the path versus trapped/constrained exit for those who merely traverse it — is exactly the directionality pattern the engine should read as high effective extraction for the payer seats and low/negative extraction (net subsidy) for the beneficiary seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (endpoint interoperability without out-of-band coordination) is only partially live: for the narrow case of two well-behaved endpoints on a clean path, RFC 9293 still functions as intended. But for a large and growing share of real internet traffic, the specification's authority has been superseded by the deployed middlebox population's de facto rules, and the beneficiaries of that supersession (ISPs, enterprises, states) have strong incentive to characterize their interference as necessary network management rather than as a founding-problem substitution. Classifying this as Tangled Rope rather than Snare avoids two mislabeling errors: treating the whole arrangement as pure malicious extraction (ignoring the real coordination functions middleboxes do perform) and treating it as untroubled coordination (ignoring the asymmetric, non-consensual capture of control from endpoints). The Tangled Rope label holds both the genuine coordination function and the extraction in view simultaneously.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    middlebox_coordination_vs_capture_boundary,
    'How much of middlebox behavior is genuinely necessary coordination (NAT for address conservation, firewalling for real security threats) versus opportunistic capture of path-control for surveillance, rent extraction, or anti-competitive traffic shaping?',
    'Comparative measurement studies isolating middlebox behaviors that have no plausible technical justification (e.g., stripping of options with no security implication, resets targeting specific application fingerprints unrelated to any policy) from behaviors serving a documented, narrow technical purpose.',
    'If most middlebox interference maps to defensible technical necessity, this reading overstates extraction and the arrangement is closer to ordinary Tangled Rope with modest asymmetry; if a large share is opportunistic and undocumented, this reading understates the case for reclassifying toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_coordination_vs_capture_boundary, empirical, 'Whether measured middlebox interference is predominantly functional or predominantly extractive.').

omega_variable(
    specification_authority_displacement_degree,
    'To what extent has RFC 9293''s specified state machine been effectively displaced by middlebox-mediated behavior across the actual internet, versus remaining authoritative for some significant, identifiable subset of paths?',
    'Longitudinal internet-measurement studies (active probing, passive traffic analysis) quantifying the fraction of paths exhibiting RFC-conformant versus middlebox-modified TCP behavior over time.',
    'A high displacement fraction supports treating this reading as the operative structural reality for most practical purposes; a low fraction would suggest the strict_invariance_reading remains descriptively accurate for a meaningful core of the internet and this reading applies only to a bounded, if significant, subset of paths.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(specification_authority_displacement_degree, empirical, 'Empirical extent of middlebox-driven displacement of specified endpoint behavior.').

omega_variable(
    reading_selection_and_kernel_ambiguity,
    'Is the choice among the three readings of RFC 9293 (strict invariance, optimization latitude, middlebox realism) itself a matter of which observable one privileges — the text, the endpoint implementation space, or the on-path traffic — or is one reading simply more descriptively accurate of the internet as deployed?',
    'This is a conceptual/framing question rather than one resolvable by a single measurement: it depends on whether ''TCP'' is defined as the specification text, as endpoint-implemented behavior, or as end-to-end observed outcomes across real paths. The framing choice is documented here rather than folded into the ε value, per the ε-invariance principle.',
    'Different framing choices are exactly why this concept was decomposed into three separate constraint stories rather than one story with an internal observable parameter; each reading''s ε, beneficiaries, and victims are authored independently and linked via the kernel.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_selection_and_kernel_ambiguity, conceptual, 'Whether reading selection reflects observable choice or descriptive accuracy, and how the decomposition into three stories addresses this.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 1981, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t1981, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 1981, 0.05).
narrative_ontology:measurement(rfc9_tr_t1993, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 1993, 0.12).
narrative_ontology:measurement(rfc9_tr_t2001, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2001, 0.22).
narrative_ontology:measurement(rfc9_tr_t2009, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2009, 0.32).
narrative_ontology:measurement(rfc9_tr_t2016, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2016, 0.38).
narrative_ontology:measurement(rfc9_tr_t2024, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t1981, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 1981, 0.1).
narrative_ontology:measurement(rfc9_be_t1993, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 1993, 0.28).
narrative_ontology:measurement(rfc9_be_t2001, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2001, 0.42).
narrative_ontology:measurement(rfc9_be_t2009, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2009, 0.55).
narrative_ontology:measurement(rfc9_be_t2016, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2016, 0.62).
narrative_ontology:measurement(rfc9_be_t2024, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t1981, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 1981, 0.05).
narrative_ontology:measurement(rfc9_su_t1993, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 1993, 0.2).
narrative_ontology:measurement(rfc9_su_t2001, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2001, 0.4).
narrative_ontology:measurement(rfc9_su_t2009, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2009, 0.55).
narrative_ontology:measurement(rfc9_su_t2016, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2016, 0.65).
narrative_ontology:measurement(rfc9_su_t2024, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 2024, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__middlebox_realism_reading, 0.12).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, optimization_latitude_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the rfc9293_tcp_specification kernel. strict_invariance_reading treats the specified state machine as binding and exactly replicated (closer to Mountain/Rope, low extraction, coordination-dominant). optimization_latitude_reading treats the spec as outcome-bound with implementation freedom (Rope-leaning, moderate extraction from competitive optimization pressure). This story, middlebox_realism_reading, treats specification authority as empirically subordinate to deployed on-path infrastructure and authors substantially higher extraction and suppression because it identifies concentrated beneficiaries (ISPs, enterprises, state agencies) extracting control from a comparatively powerless and often trapped victim set (privacy-seeking endpoints, extension designers). The three stories share the same kernel text and interoperability history but diverge in which observable each treats as authoritative — this divergence in ε and classification across the three files is the intended structure per the ε-invariance decomposition principle, not an inconsistency to reconcile.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
