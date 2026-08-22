% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__prioritization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: tcp_ip_interpretation__prioritization_reading
 *   human_readable: TCP/IP Prioritization Reading â Paid Fast Lane Network Management
 *   domain: technology governance / internet policy / telecommunications law
 *
 * SUMMARY:
 *   This constraint instantiates the prioritization_reading of the
 *   tcp_ip_interpretation kernel, which holds that TCP/IP's architecture â
 *   particularly the Type of Service field (RFC 791), the Differentiated
 *   Services framework (RFC 2474), and the layered end-to-end design â
 *   permits and structurally anticipates differentiated treatment of traffic
 *   for network management and commercial prioritization. Under this reading,
 *   ISPs are authorized to implement paid fast lanes, unfunded edge services
 *   are structurally disadvantaged, and network investment is incentivized by
 *   monetizing forwarding priority. Sibling readings include the
 *   neutrality_reading (end-to-end non-discrimination as the protocol's
 *   binding normative structure) and the zero_rating_reading (selective
 *   exemption from data caps as permissible under the same permissive
 *   interpretive frame).
 *
 * KEY AGENTS:
 *   - tier_1_isps: agenda-setter (institutional/arbitrage) â operates infrastructure, sets fast-lane terms, collects priority fees
 *   - large_content_providers: beneficiary (powerful/constrained) â purchases assured delivery, gains competitive moat against unfunded entrants
 *   - unfunded_edge_services: payer (powerless/trapped) â bears degraded reach and performance without resources to buy priority
 *   - small_content_providers: payer (moderate/constrained) â faces best-effort degradation under congestion, lacks ISP negotiating leverage
 *   - telecommunications_regulators: observer (institutional/analytical) â adjudicates authorization but does not administer contracts
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, 0.55).
domain_priors:suppression_score(tcp_ip_interpretation__prioritization_reading, 0.5).
domain_priors:theater_ratio(tcp_ip_interpretation__prioritization_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__prioritization_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__prioritization_reading, "TCP/IP Prioritization Reading â Paid Fast Lane Network Management").
narrative_ontology:topic_domain(tcp_ip_interpretation__prioritization_reading, "technology governance / internet policy / telecommunications law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__prioritization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__prioritization_reading, '21f80755-a0cd-474b-b353-f63dc1fcece5').
narrative_ontology:cs_kernel_codification('21f80755-a0cd-474b-b353-f63dc1fcece5', formalized).
narrative_ontology:cs_authority_grounding('21f80755-a0cd-474b-b353-f63dc1fcece5', distributed).
narrative_ontology:cs_reading_relation('21f80755-a0cd-474b-b353-f63dc1fcece5', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('21f80755-a0cd-474b-b353-f63dc1fcece5', tcp_ip_interpretation__zero_rating_reading, influences).
narrative_ontology:cs_axiom('21f80755-a0cd-474b-b353-f63dc1fcece5', foundational, differentiated_treatment_intrinsic_to_ip_design).
narrative_ontology:cs_axiom_status(differentiated_treatment_intrinsic_to_ip_design, holdable).
narrative_ontology:cs_axiom_grounding('21f80755-a0cd-474b-b353-f63dc1fcece5', differentiated_treatment_intrinsic_to_ip_design, conventional).
narrative_ontology:cs_axiom('21f80755-a0cd-474b-b353-f63dc1fcece5', foundational, commercial_prioritization_incentivizes_investment).
narrative_ontology:cs_axiom_status(commercial_prioritization_incentivizes_investment, holdable).
narrative_ontology:cs_axiom_grounding('21f80755-a0cd-474b-b353-f63dc1fcece5', commercial_prioritization_incentivizes_investment, instrumental).
narrative_ontology:cs_reference_frame('21f80755-a0cd-474b-b353-f63dc1fcece5', permissive_protocol_design).
narrative_ontology:cs_drift_state('21f80755-a0cd-474b-b353-f63dc1fcece5', commercial_internet_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('21f80755-a0cd-474b-b353-f63dc1fcece5', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, tier_1_isps).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, large_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, unfunded_edge_services).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, small_content_providers).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__prioritization_reading, differentiated_services_doctrine).
narrative_ontology:constraint_vindicates(tcp_ip_interpretation__prioritization_reading, infrastructure_investment_incentive).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Operate backbone and last-mile infrastructure; implement traffic differentiation through deep packet inspection, QoS policies, and paid peering or prioritization agreements. Set the technical and commercial terms for fast lanes and collect revenue from content providers for prioritized delivery.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, tier_1_isps, agenda_setter,
    institutional, generational, arbitrage, global).

% Contract with ISPs for assured delivery and prioritized bandwidth for streaming, gaming, and real-time services. Benefit from reduced competition from unfunded services that cannot afford priority fees, while paying for preferential treatment that raises barriers to entry.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, large_content_providers, beneficiary,
    powerful, biographical, constrained, global).

% Develop innovative applications without capital to purchase prioritization or direct peering. Face degraded user experience relative to prioritized competitors, reduced discoverability, and structural disadvantage in markets where latency determines success. No viable alternative to reaching users except through ISP-controlled pipes.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, unfunded_edge_services, payer,
    powerless, immediate, trapped, global).

% Operate regional or niche platforms that lack negotiating leverage with ISPs. Cannot afford paid prioritization fees at scale; traffic is delivered on best-effort terms that are effectively degraded when networks are congested and priority traffic absorbs available capacity.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, small_content_providers, payer,
    moderate, biographical, constrained, national).

% Adjudicate between net neutrality mandates and ISP network-management claims. Their rulings determine whether prioritization is legally authorized or prohibited, but they do not directly administer the prioritization contracts and are subject to lobbying pressure from the ISP industry.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, telecommunications_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__prioritization_reading, tier_1_isps).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__prioritization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides differentiated quality-of-service to manage heterogeneous application requirements over shared infrastructure, aiming to deliver latency-sensitive traffic reliably and incentivize capital investment in network capacity by allowing monetization of forwarding priority.
% TRANSFER_FUNCTION: Moves competitive advantage and user reach from unfunded edge services and small content providers to tier-1 ISPs (via paid prioritization revenue) and large content providers (via assured delivery and competitive moats against unfunded entrants).
% ABSENT_VOICES: Unfunded edge innovators without legal representation in national ISP regulatory proceedings; end-users in jurisdictions where regulatory proceedings are dominated by ISP technical expertise and investment-impact models; public-interest advocates structurally sidelined from technical standard-setting bodies.
% DISAPPEARANCE_RATIONALE: If paid prioritization rules vanished and all traffic were treated equally, ISP revenue models would shift away from fast-lane fees, content-provider competitive dynamics would flatten as unfunded services regained reach parity, and network investment incentives would restructure toward capacity expansion rather than prioritization monetization.
% FOUNDING_PROBLEM: Network congestion and the need to deliver varying application requirements â latency-sensitive voice and video versus bulk file transfer â over a shared best-effort packet infrastructure without collapse under peak load.
% FOUNDING_PROBLEM_CORROBORATION: ISPs and network engineering bodies attest that congestion management and heterogeneous QoS remain live technical problems. Net neutrality advocates and academic network researchers attest that capacity expansion, edge caching, and over-provisioning have substantially addressed the founding problem, and that prioritization has migrated to a rent-seeking function; regulatory economic analyses from outside the ISP industry support the shifted-function reading.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__prioritization_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__prioritization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__prioritization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(tcp_ip_interpretation__prioritization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__prioritization_reading, 0.55, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness (0.55) is moderate-high because the arrangement systematically disadvantages unfunded services by making reach contingent on payments that extract scarce capital from the edge. Suppression (0.5) is moderate: where authorized the constraint is enforced via DPI and contractual terms, but jurisdictional fragmentation and encryption alternatives keep alternatives partly open. Theater ratio (0.3) reflects that QoS is a genuine technical function, yet an increasing share of justification rhetoric frames commercially unnecessary prioritization as congestion management. Resistance (0.55) captures sustained net-neutrality advocacy and regulatory oscillation. The temporal series show extraction rising as the reading shifted from technical DiffServ to commercial fast lanes, then plateauing as regulatory pushback emerged.
 *
 * PERSPECTIVAL GAP:
 *   The ISP seat experiences the constraint as legitimate network management and investment recovery; the unfunded edge service seat experiences the same technical behavior as a competitive barrier that extracts viability. The regulator seat experiences a contested policy choice rather than a natural operational necessity. These divergences are structurally determined by who controls the pipes, who can pay for priority, and who is locked out of both capital and negotiation.
 *
 * DIRECTIONALITY LOGIC:
 *   ISPs and large content providers are on the beneficiary side: ISPs monetize priority directly, and large content providers gain QoS assurance plus a competitive barrier against unfunded rivals. Their directionality is near the beneficiary end. Unfunded edge services and small content providers are the targets: they bear the cost of degraded best-effort delivery in a system where priority traffic absorbs capacity and user expectations shift toward low-latency performance. Their directionality is near the target end. Consumers are not separately seated here but would sit near symmetric, receiving some QoS benefit while paying diffuse costs in reduced innovation diversity.
 *
 * MANDATROPHY ANALYSIS:
 *   Without the Tangled Rope classification, this arrangement could be misread as pure Rope (QoS coordination) because latency management is a real technical need, or as pure Snare (commercial extraction) because unfunded services are disadvantaged. The Tangled Rope gate requires both genuine coordination (heterogeneous application requirements, investment incentives) and asymmetric extraction (identifiable victims paying through the same structure). The presence of both elements â verified by the beneficiaries and victims declarations plus active enforcement â prevents either mislabeling. If the coordination function were dead and only extraction remained, the measurements would show rising theater_ratio and the classification would migrate toward Snare; if extraction evaporated, it would approach Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    prioritization_reading_kernel_location,
    'Is the prioritization reading a structural feature of TCP/IP protocol design, or a post-hoc regulatory-commercial framing that reinterprets the kernel to authorize extraction?',
    'Historical protocol archaeology of RFC 791 and RFC 2474 to determine whether differentiated service was intended for inter-provider technical congestion management or intra-provider commercial prioritization; comparative regulatory anthropology across jurisdictions.',
    'If the reading is found to be a post-hoc framing, the constraint''s claimed coordination function weakens and extraction dominates, shifting computed classification toward snare; if genuinely embedded in protocol design, tangled_rope or rope classification is supported.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(prioritization_reading_kernel_location, conceptual, 'Whether prioritization is intrinsic to protocol design or a retrospective commercial framing.').

omega_variable(
    qos_vs_rent_extraction,
    'Does paid prioritization solve a genuine coordination problem (heterogeneous congestion, investment shortfalls) or create artificial scarcity to extract rents from the edge?',
    'Empirical comparison of network performance, investment levels, and edge-innovation rates in jurisdictions with and without paid prioritization, controlling for baseline infrastructure, demand, and income levels.',
    'If performance and investment do not significantly improve relative to capacity-expansion regimes, the coordination story is cover for extraction and the effective extractiveness is higher than the coordination-adjusted measure suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(qos_vs_rent_extraction, empirical, 'Whether paid prioritization is genuine coordination or artificial scarcity.').

omega_variable(
    sibling_jurisdictional_exclusivity,
    'Does the prioritization reading foreclose the neutrality reading within a single regulatory framework, or can both coexist as live policy positions without logical contradiction?',
    'Analysis of regulatory regimes: the US FCC''s Restoring Internet Freedom Order adopted the prioritization reading while the EU Open Internet Regulation adopts the neutrality reading. They coexist globally, but within any single jurisdiction the operational framework must choose one mode.',
    'If the readings are jurisdictionally mutually exclusive, they foreclose each other at the implementation level even if they coexist as interpretive positions globally; this affects whether the kernel is modeled as distributed authority or contested lineage with zero-sum regulatory capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_jurisdictional_exclusivity, conceptual, 'Whether prioritization and neutrality readings are mutually exclusive in practice.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__prioritization_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__prioritization_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tcp__tr_t5, tcp_ip_interpretation__prioritization_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement(tcp__tr_t10, tcp_ip_interpretation__prioritization_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(tcp__tr_t15, tcp_ip_interpretation__prioritization_reading, theater_ratio, 15, 0.28).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__prioritization_reading, theater_ratio, 20, 0.3).
narrative_ontology:measurement(tcp__tr_t25, tcp_ip_interpretation__prioritization_reading, theater_ratio, 25, 0.3).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(tcp__be_t5, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 5, 0.3).
narrative_ontology:measurement(tcp__be_t10, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 10, 0.4).
narrative_ontology:measurement(tcp__be_t15, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 15, 0.52).
narrative_ontology:measurement(tcp__be_t20, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(tcp__be_t25, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 25, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(tcp__su_t5, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 5, 0.35).
narrative_ontology:measurement(tcp__su_t10, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 10, 0.45).
narrative_ontology:measurement(tcp__su_t15, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 15, 0.55).
narrative_ontology:measurement(tcp__su_t20, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 20, 0.52).
narrative_ontology:measurement(tcp__su_t25, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 25, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__prioritization_reading, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
