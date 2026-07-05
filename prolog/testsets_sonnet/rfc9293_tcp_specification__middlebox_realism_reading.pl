% ============================================================================
% CONSTRAINT STORY: rfc9293_tcp_specification__middlebox_realism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: TCP Specification Subordinated to Deployed Middlebox Population (Middlebox Realism Reading)
 *   domain: network_protocol_engineering/internet_standards
 *
 * SUMMARY:
 *   This story instantiates the middlebox-realism reading of the RFC 9293
 *   kernel: the specification's endpoint state machine is treated as
 *   aspirational text whose actual authority over wire behavior is
 *   subordinate to the deployed middlebox population. Under this reading the
 *   constraint is a Tangled Rope — the RFC genuinely coordinates a shared
 *   vocabulary that lets independent implementers build interoperable stacks,
 *   but the path between those endpoints is administered by ISPs, enterprise
 *   firewalls, CDN vendors, and state surveillance operators who extract
 *   control, visibility, and enforcement capacity through the same wire
 *   format the specification describes, at the direct cost of implementers'
 *   engineering effort, users' feature access, and privacy-seeking endpoints'
 *   metadata assumptions. This is a sibling of, not identical to, the
 *   strict_invariance_reading (which treats the state machine as a hard
 *   interoperability invariant implementers must replicate exactly) and the
 *   optimization_latitude_reading (which treats the specification as
 *   permitting implementation freedom within semantic bounds for
 *   performance). Where those readings locate the interesting structure
 *   inside the specification-implementer relationship, this reading locates
 *   it in the specification-versus-path relationship: what matters here is
 *   not whether an implementer is faithful to the RFC, but whether the
 *   network path between two faithful implementers preserves what the RFC
 *   promised. The extractiveness values in this story are authored
 *   specifically for that relationship and are not commensurable with ε
 *   values authored for the sibling readings — each reading is its own
 *   constraint per the ε-invariance principle.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rfc9293_tcp_specification__middlebox_realism_reading, 0.61).
domain_priors:suppression_score(rfc9293_tcp_specification__middlebox_realism_reading, 0.58).
domain_priors:theater_ratio(rfc9293_tcp_specification__middlebox_realism_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(rfc9293_tcp_specification__middlebox_realism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(rfc9293_tcp_specification__middlebox_realism_reading, tangled_rope).
narrative_ontology:human_readable(rfc9293_tcp_specification__middlebox_realism_reading, "TCP Specification Subordinated to Deployed Middlebox Population (Middlebox Realism Reading)").
narrative_ontology:topic_domain(rfc9293_tcp_specification__middlebox_realism_reading, "network_protocol_engineering/internet_standards").

domain_priors:requires_active_enforcement(rfc9293_tcp_specification__middlebox_realism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(rfc9293_tcp_specification__middlebox_realism_reading, '51bfa3d2-d3a0-4d04-8b5a-712a98ca7c29').
narrative_ontology:cs_kernel_codification('51bfa3d2-d3a0-4d04-8b5a-712a98ca7c29', formalized).
narrative_ontology:cs_authority_grounding('51bfa3d2-d3a0-4d04-8b5a-712a98ca7c29', distributed).
narrative_ontology:cs_reading_relation('51bfa3d2-d3a0-4d04-8b5a-712a98ca7c29', rfc9293_tcp_specification__strict_invariance_reading, influences).
narrative_ontology:cs_reading_relation('51bfa3d2-d3a0-4d04-8b5a-712a98ca7c29', rfc9293_tcp_specification__optimization_latitude_reading, coexists_with).
narrative_ontology:cs_axiom('51bfa3d2-d3a0-4d04-8b5a-712a98ca7c29', foundational, path_administration_overrides_endpoint_specification).
narrative_ontology:cs_axiom_status(path_administration_overrides_endpoint_specification, holdable).
narrative_ontology:cs_axiom_grounding('51bfa3d2-d3a0-4d04-8b5a-712a98ca7c29', path_administration_overrides_endpoint_specification, empirically_contingent).
narrative_ontology:cs_axiom('51bfa3d2-d3a0-4d04-8b5a-712a98ca7c29', secondary, deployed_infrastructure_is_the_de_facto_standard).
narrative_ontology:cs_axiom_status(deployed_infrastructure_is_the_de_facto_standard, holdable).
narrative_ontology:cs_axiom_grounding('51bfa3d2-d3a0-4d04-8b5a-712a98ca7c29', deployed_infrastructure_is_the_de_facto_standard, conventional).
narrative_ontology:cs_reference_frame('51bfa3d2-d3a0-4d04-8b5a-712a98ca7c29', end_to_end_principle_endpoint_supremacy).
narrative_ontology:cs_drift_state('51bfa3d2-d3a0-4d04-8b5a-712a98ca7c29', contemporary_middlebox_saturated_internet, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('51bfa3d2-d3a0-4d04-8b5a-712a98ca7c29', '').
narrative_ontology:cs_kernel_id(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, isp_network_operators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_firewall_administrators).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_agencies).
narrative_ontology:constraint_beneficiary(rfc9293_tcp_specification__middlebox_realism_reading, cdn_and_load_balancer_vendors).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, protocol_implementers).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, end_users_seeking_new_transport_features).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, privacy_seeking_endpoints).
narrative_ontology:constraint_victim(rfc9293_tcp_specification__middlebox_realism_reading, research_and_experimental_protocol_developers).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__middlebox_realism_reading, robustness_principle_of_deployed_networks).
narrative_ontology:constraint_vindicates(rfc9293_tcp_specification__middlebox_realism_reading, de_facto_standard_supersedes_de_jure_text).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Deploy NAT gateways, stateful firewalls, and traffic-shaping middleboxes along transit paths that rewrite sequence numbers, strip options, reset connections outside expected window bounds, and enforce policy invisibly to the endpoints. They administer the actual behavior the network exhibits and can change it, but bear none of the compatibility cost their rewriting imposes on implementers elsewhere. RFC 9293's endpoint state machine is aspirational text from their operational vantage; what they configure IS the protocol as experienced end to end.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, isp_network_operators, agenda_setter,
    institutional, generational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, isp_network_operators, beneficiary).

% Operate deep packet inspection appliances and application-layer gateways that terminate, inspect, and sometimes silently drop TCP options or extensions not on an approved allowlist, justified as security policy. They gain visibility and control over traffic crossing their perimeter, at the cost of breaking connections that use legitimate but unrecognized TCP extensions.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_firewall_administrators, agenda_setter,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(rfc9293_tcp_specification__middlebox_realism_reading, enterprise_firewall_administrators, beneficiary).

% Compel or operate middleboxes at national chokepoints that fingerprint, throttle, or reset connections based on observed TCP behavior, using the very path-dependence the specification cannot prevent as a surveillance and control surface. They benefit from the gap between specified endpoint privacy expectations and what the path actually permits.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, state_surveillance_agencies, beneficiary,
    institutional, civilizational, arbitrage, national).

% Build proprietary connection-splicing, TCP fast-open workarounds, and anycast failover logic that assumes and exploits specific middlebox behaviors observed empirically across deployed networks, converting the instability into a competitive product feature (better real-world traversal) rather than a shared burden.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, cdn_and_load_balancer_vendors, beneficiary,
    powerful, generational, mobile, global).

% Write TCP stacks and must add defensive workarounds, option-fallback logic, and empirical probing (PMTU black-hole detection, MSS clamping compensation, ECN bit stripping tolerance) that RFC 9293 never specifies, because deployed middleboxes violate the RFC's endpoint contract routinely. They cannot opt out of the messy reality without breaking connectivity for real users; compliance with the RFC alone yields a stack that fails in the field.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, protocol_implementers, payer,
    moderate, biographical, trapped, global).

% Experience silent failures, degraded performance, or feature ossification (new TCP options routinely stripped in transit) whenever an application tries to use a legitimate but uncommon extension. They have no visibility into which middlebox on their path caused the failure and no direct recourse; the specification offers no protection against path-level interference.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, end_users_seeking_new_transport_features, payer,
    powerless, immediate, trapped, global).

% Rely on the specified protocol behaving as documented to reason about what metadata leaks to the network; middlebox fingerprinting and forced protocol downgrade (e.g., stripping encryption-adjacent extensions) undermine that assumption, exposing behavior the endpoint never intended to reveal, with no mechanism to detect or resist the interference at the endpoint.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, privacy_seeking_endpoints, payer,
    powerless, biographical, constrained, national).

% Attempt to deploy new congestion-control or extension mechanisms through the standards process, only to find deployed middlebox populations silently break or discriminate against unrecognized wire formats, forcing years of incremental, ossification-avoiding workarounds (e.g., tunneling new semantics inside already-tolerated fields) rather than clean extension.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, research_and_experimental_protocol_developers, payer,
    moderate, generational, constrained, global).

% Maintains and revises the RFC 9293 text, documents known middlebox interference patterns in companion RFCs and errata, and mediates disputes about what 'correct' behavior means when the deployed network disagrees with the specification. Can revise text but cannot compel middlebox operators to conform.
narrative_ontology:constraint_stakeholder(rfc9293_tcp_specification__middlebox_realism_reading, ietf_tcpm_working_group, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The specification text still coordinates a shared vocabulary for describing endpoint state transitions, allowing interoperable stack implementations to be built and reasoned about in the absence of path interference — a genuine baseline that new implementers can start from.
% TRANSFER_FUNCTION: Moves de facto control over what the protocol actually does away from the endpoints and the specification body and toward whoever administers the path — ISPs, enterprise gateways, and state chokepoints — who capture visibility, policy enforcement capacity, and in some cases surveillance leverage at the cost of protocol implementers' engineering effort and end users' feature access and privacy assumptions.
% ABSENT_VOICES: End users whose connections silently fail or are fingerprinted have no seat in IETF process and typically never learn a middlebox was responsible; research protocol developers attempting new extensions raise the ossification problem repeatedly in working-group discussion but lack the leverage to compel middlebox vendors or operators to change deployed behavior.
% DISAPPEARANCE_RATIONALE: If the middlebox population's authority over actual TCP behavior vanished overnight and only the specification governed, protocol extension would become dramatically easier, end-to-end privacy assumptions would hold again, and a substantial portion of current defensive-implementation engineering effort industry-wide would become unnecessary — but ISPs, enterprises, and surveillance operators would lose a major control and visibility surface, which is precisely why the middlebox population persists despite specification text saying otherwise.
% FOUNDING_PROBLEM: Middleboxes were originally deployed to solve real problems the base specification did not address: address scarcity (NAT), basic perimeter security (stateful firewalls), and performance (load balancing, connection splicing) — genuine gaps in what pure end-to-end TCP could provide on its own.
% FOUNDING_PROBLEM_CORROBORATION: IETF working-group documents (e.g., RFC 3234 on middlebox taxonomy, and ongoing tcpm errata) attest that some original problems (address exhaustion, basic connection tracking) remain partially live, corroborated by network operators themselves. However, independent measurement studies (academic middlebox census work, e.g. active-probing surveys of TCP option stripping) conducted outside both the operator and surveillance-beneficiary communities find that a large share of current interference is either legacy inertia or policy/surveillance enforcement unrelated to the original address-scarcity or basic-security rationale — supporting a 'dead-original-purpose, repurposed-for-control' reading for a substantial subset of deployed middleboxes.
narrative_ontology:disappearance_verdict(rfc9293_tcp_specification__middlebox_realism_reading, world_rearranges).
narrative_ontology:founding_problem_status(rfc9293_tcp_specification__middlebox_realism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(rfc9293_tcp_specification__middlebox_realism_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(rfc9293_tcp_specification__middlebox_realism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(rfc9293_tcp_specification__middlebox_realism_reading, 0.61, 'claude-sonnet-5', 'none', direct).

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
 *   Extractiveness rises across the interval (0.32 to 0.61) tracking the historical growth of middlebox deployment density and sophistication — from simple NAT/basic-firewall middleboxes in the protocol's early widespread-deployment era to today's DPI appliances, carrier-grade NAT, and surveillance-capable traffic-shaping infrastructure. Theater ratio also rises (0.18 to 0.44): a growing share of 'security' and 'optimization' middlebox activity is justified rhetorically while its actual function has shifted toward policy enforcement and visibility capture unrelated to the original address-scarcity or basic-security rationale. Suppression tracks the hardening of this interference into normalized, expected network behavior that implementers must now defensively code around as a baseline assumption rather than treating as an exceptional path condition — the enforcement infrastructure (widespread DPI, carrier NAT, national filtering) matured and hardened over the interval. Accessibility collapse is moderate (0.5): implementers CAN work around most interference with sufficient defensive engineering, so alternatives are not fully foreclosed, but the cost of doing so is now a routine, unavoidable tax on protocol work. Resistance is moderate-high (0.55): the IETF tcpm working group and researchers actively document, protest, and design around middlebox interference (ossification-avoidance work, encrypted transport design partly motivated by middlebox evasion), but cannot compel change in deployed infrastructure they do not control.
 *
 * DIRECTIONALITY LOGIC:
 *   ISPs, enterprise firewall administrators, state surveillance agencies, and CDN/load-balancer vendors are declared beneficiaries: they administer the actual path behavior, capture visibility or control, and bear minimal compatibility cost themselves — the derivation should place them near the beneficiary end of directionality, particularly given their institutional power and arbitrage-grade exit (they can change their own configurations at will without waiting on standards processes). Protocol implementers, ordinary end users, privacy-seeking endpoints, and research protocol developers are declared victims: they bear the engineering cost, feature loss, privacy erosion, and ossification cost respectively, with constrained-to-trapped exit options since they cannot simply route around infrastructure they do not control. The IETF tcpm working group sits in an analytical observer position — it documents and mediates but neither collects the extraction nor bears its cost directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (address scarcity, basic perimeter security, connection-tracking gaps in pure end-to-end TCP) was genuinely live when middleboxes were first widely deployed, which is why this reading is Tangled Rope rather than pure Snare — there IS a real coordination function underneath. But the founding-problem status is authored as contested, not dead, because independent measurement work shows a substantial and apparently growing fraction of current interference serves surveillance and policy-enforcement functions that were never part of the original rationale. Classifying this as Tangled Rope rather than collapsing it into either 'pure natural evolution of infrastructure' (which would hide the extraction) or 'pure Snare' (which would erase the genuine coordination NAT and firewalls still provide) is exactly the distinction this reading is built to preserve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    middlebox_coordination_vs_capture_boundary,
    'For any given deployed middlebox (a specific NAT gateway, a specific enterprise DPI appliance, a specific national filtering system), is its current behavior still serving the original coordination problem it was deployed to solve, or has it been repurposed primarily for policy enforcement, surveillance, or rent extraction unrelated to that original problem?',
    'Longitudinal independent measurement studies (active probing, passive traffic analysis conducted by academic or civil-society researchers outside both ISP and surveillance-agency interests) that track what specific interference behaviors persist after the original justifying condition (e.g., IPv4 address scarcity, given IPv6 deployment) is substantially relieved.',
    'If interference persists or grows despite the original justifying condition receding, the coordination story is largely cover and the constraint should be read closer to Snare for the affected middlebox subpopulation; if interference recedes proportionally, the Tangled Rope reading holds as authored.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(middlebox_coordination_vs_capture_boundary, empirical, 'Whether specific deployed middleboxes still serve their founding coordination function or have been repurposed toward pure extraction.').

omega_variable(
    kernel_reading_selection_ambiguity,
    'Is the middlebox-realism framing the dominant lived reality of TCP''s operation for most connections, or is it a boundary case (specific paths, specific regions, specific adversarial contexts) while most traffic experiences something closer to the strict-invariance or optimization-latitude readings undisturbed?',
    'Global measurement studies comparing the incidence and severity of middlebox interference across diverse network paths (residential broadband, enterprise, mobile carrier, transit backbone, censored national networks) to establish what fraction of real-world TCP traffic experiences path-dependent behavior significant enough to justify this reading as the operative one for a given connection.',
    'If middlebox interference is concentrated in a minority of paths (e.g., specific national networks or specific enterprise perimeters), the middlebox-realism reading is the correct kernel reading only for that subset, and most global TCP traffic should be evaluated under the sibling readings instead — narrowing this constraint''s applicable scope without changing its ε for the paths where it does apply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_ambiguity, conceptual, 'Whether the middlebox-realism reading is the globally dominant account of TCP''s operation or a scoped exception applicable to specific paths.').

omega_variable(
    surveillance_beneficiary_naturalization,
    'Does treating network path administration as an inevitable, natural feature of internet infrastructure (rather than a series of specific, contestable deployment and policy choices by identifiable operators) function to naturalize what is actually a constructed extraction arrangement?',
    'Compare jurisdictions and network architectures with materially different middlebox deployment norms (e.g., networks with strong net-neutrality and privacy regulation versus networks with extensive state-mandated filtering) to establish whether the level of path interference is a policy-contingent variable rather than a technical inevitability.',
    'If interference levels vary substantially by policy regime rather than technical necessity, this strengthens the case that beneficiary capture (not technical necessity) drives the observed extraction, supporting the Tangled Rope-toward-Snare direction of the drift already shown in the temporal measurements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(surveillance_beneficiary_naturalization, conceptual, 'Whether framing path interference as infrastructural inevitability obscures its status as a constructed, policy-contingent extraction arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(rfc9293_tcp_specification__middlebox_realism_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(rfc9_tr_t0, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement(rfc9_tr_t5, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(rfc9_tr_t10, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 10, 0.27).
narrative_ontology:measurement(rfc9_tr_t15, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 15, 0.33).
narrative_ontology:measurement(rfc9_tr_t20, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(rfc9_tr_t25, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 25, 0.42).
narrative_ontology:measurement(rfc9_tr_t30, rfc9293_tcp_specification__middlebox_realism_reading, theater_ratio, 30, 0.44).

% Extraction over time
narrative_ontology:measurement(rfc9_be_t0, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(rfc9_be_t5, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 5, 0.4).
narrative_ontology:measurement(rfc9_be_t10, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 10, 0.47).
narrative_ontology:measurement(rfc9_be_t15, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 15, 0.53).
narrative_ontology:measurement(rfc9_be_t20, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 20, 0.57).
narrative_ontology:measurement(rfc9_be_t25, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement(rfc9_be_t30, rfc9293_tcp_specification__middlebox_realism_reading, base_extractiveness, 30, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(rfc9_su_t0, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(rfc9_su_t5, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 5, 0.41).
narrative_ontology:measurement(rfc9_su_t10, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 10, 0.46).
narrative_ontology:measurement(rfc9_su_t15, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 15, 0.5).
narrative_ontology:measurement(rfc9_su_t20, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement(rfc9_su_t25, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 25, 0.56).
narrative_ontology:measurement(rfc9_su_t30, rfc9293_tcp_specification__middlebox_realism_reading, suppression_requirement, 30, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(rfc9293_tcp_specification__middlebox_realism_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(rfc9293_tcp_specification__middlebox_realism_reading, 0.12).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__strict_invariance_reading).
narrative_ontology:affects_constraint(rfc9293_tcp_specification__middlebox_realism_reading, rfc9293_tcp_specification__optimization_latitude_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the rfc9293_tcp_specification kernel, decomposed per the ε-invariance principle because the natural-language label 'what RFC 9293 actually requires' covers structurally distinct claims with different ε values: strict_invariance_reading (specification-vs-implementer fidelity claim, low extraction, near-Mountain/Rope), optimization_latitude_reading (specification-vs-implementer freedom claim, low-to-moderate extraction, likely Rope), and this middlebox_realism_reading (specification-vs-path authority claim, substantially extractive, Tangled Rope). All three share the same kernel text (RFC 9293) but diverge on what the relevant authority relationship is and who the parties to it are. Each carries its own beneficiary/victim structure and its own claimed_type; they are linked here rather than merged because merging them would violate ε-invariance — measuring 'the RFC' via implementer-fidelity yields a very different extraction picture than measuring it via path-administration.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
