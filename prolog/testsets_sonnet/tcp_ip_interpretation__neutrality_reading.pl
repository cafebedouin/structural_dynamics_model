% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tcp_ip_interpretation__neutrality_reading, []).

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
 *   constraint_id: tcp_ip_interpretation__neutrality_reading
 *   human_readable: TCP/IP End-to-End Principle — Neutrality Reading
 *   domain: technology governance/internet policy/telecommunications law
 *
 * SUMMARY:
 *   TCP/IP's end-to-end design is invoked as the technical basis for network
 *   neutrality regulation, which prohibits ISPs from discriminating against
 *   traffic by application or content type. This story instantiates ONLY the
 *   neutrality reading of the underlying TCP/IP-interpretation kernel: it
 *   treats the protocol's design as committing carriers to non-discriminatory
 *   packet handling as a matter of structural principle, requiring active
 *   regulatory enforcement to hold against carrier incentives to monetize
 *   prioritization. Two sibling constraints exist under the same kernel — a
 *   prioritization reading (differentiated service quality as legitimate
 *   network management) and a zero-rating reading (selective
 *   sponsored-content exemptions) — each with a different beneficiary/victim
 *   structure and a different ε. Those are separate constraint files, not
 *   alternate measurements of this one; this story does not average across
 *   them or hedge its extraction value against theirs.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, 0.28).
domain_priors:suppression_score(tcp_ip_interpretation__neutrality_reading, 0.42).
domain_priors:theater_ratio(tcp_ip_interpretation__neutrality_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__neutrality_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__neutrality_reading, "TCP/IP End-to-End Principle — Neutrality Reading").
narrative_ontology:topic_domain(tcp_ip_interpretation__neutrality_reading, "technology governance/internet policy/telecommunications law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__neutrality_reading, 'b72909a4-6657-47db-bff5-c822b264b1a2').
narrative_ontology:cs_kernel_codification('b72909a4-6657-47db-bff5-c822b264b1a2', distributed).
narrative_ontology:cs_authority_grounding('b72909a4-6657-47db-bff5-c822b264b1a2', distributed).
narrative_ontology:cs_reading_relation('b72909a4-6657-47db-bff5-c822b264b1a2', tcp_ip_interpretation__prioritization_reading, forecloses).
narrative_ontology:cs_reading_relation('b72909a4-6657-47db-bff5-c822b264b1a2', tcp_ip_interpretation__zero_rating_reading, influences).
narrative_ontology:cs_axiom('b72909a4-6657-47db-bff5-c822b264b1a2', foundational, packet_treatment_must_be_content_blind).
narrative_ontology:cs_axiom_status(packet_treatment_must_be_content_blind, holdable).
narrative_ontology:cs_axiom_grounding('b72909a4-6657-47db-bff5-c822b264b1a2', packet_treatment_must_be_content_blind, conventional).
narrative_ontology:cs_axiom('b72909a4-6657-47db-bff5-c822b264b1a2', secondary, edge_innovation_requires_permissionless_access).
narrative_ontology:cs_axiom_status(edge_innovation_requires_permissionless_access, holdable).
narrative_ontology:cs_axiom_grounding('b72909a4-6657-47db-bff5-c822b264b1a2', edge_innovation_requires_permissionless_access, instrumental).
narrative_ontology:cs_reference_frame('b72909a4-6657-47db-bff5-c822b264b1a2', original_end_to_end_design_intent).
narrative_ontology:cs_drift_state('b72909a4-6657-47db-bff5-c822b264b1a2', post_streaming_and_5g_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b72909a4-6657-47db-bff5-c822b264b1a2', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, edge_application_developers).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, internet_users).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, content_startups).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, last_mile_isps).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, network_management_teams).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Adopts and enforces the reading that TCP/IP's design commits carriers to non-discriminatory packet handling regardless of source, destination, or application. Issues rules, investigates complaints, and can penalize ISPs found throttling or blocking traffic by content type. Frames the reading as a technical fact about the protocol's original design intent, not a policy choice.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, regulatory_agencies, agenda_setter,
    institutional, generational, analytical, national).

% Owns the physical last-mile infrastructure and bears the capital cost of scaling capacity for high-bandwidth applications (video, gaming) without being permitted to charge those applications differentially or prioritize paying traffic. Cannot easily exit the jurisdiction; can lobby, litigate, or slow-walk compliance, but remains bound by whichever kernel reading the regulator adopts.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, last_mile_isps, payer,
    powerful, biographical, constrained, national).

% Builds and ships applications on the assumption that packets are treated identically regardless of source, letting a small team compete for bandwidth on equal technical footing with an incumbent. Depends entirely on the neutrality reading holding; would need to renegotiate distribution deals with every carrier if the prioritization or zero-rating readings prevailed instead.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, edge_application_developers, beneficiary,
    moderate, biographical, mobile, global).

% Receives service where no application is technically privileged or degraded by the carrier, preserving the ability to reach any lawful service equally. Exit is bounded by local ISP competition (often thin), so the protection this reading provides is largely dependent on the regulator's continued adoption of it rather than on the user's own market power.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, internet_users, beneficiary,
    organized, biographical, constrained, national).

% Operates congestion-management and quality-of-service tooling that must be justified as content-agnostic even where differentiating by application type would be the most technically efficient way to manage real-time-sensitive traffic during peak load. Bears operational cost of proving non-discriminatory intent for any traffic-shaping decision.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, network_management_teams, payer,
    moderate, immediate, trapped, national).

% Depends on being unable to be priced out of reaching users by incumbents that could otherwise pay for fast-lane treatment. Has essentially no bargaining leverage with carriers and relies entirely on the neutrality reading's prohibition on paid prioritization to survive against better-capitalized rivals.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, content_startups, beneficiary,
    powerless, biographical, mobile, global).

% Would prefer the prioritization or zero-rating readings, under which their scale advantage could be converted into paid fast-lane or sponsored-data arrangements with carriers. Under the neutrality reading, this option is foreclosed; their objection is rarely solicited directly in neutrality rulemakings, which frame the question as carrier-versus-user rather than incumbent-versus-challenger.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, incumbent_platform_operators, excluded,
    powerful, generational, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__neutrality_reading, diffuse).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__neutrality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates a shared expectation across every application developer, carrier, and user that a packet is handled the same way regardless of what it carries, letting anyone build on the network without needing individual carriage agreements.
% TRANSFER_FUNCTION: Prevents ISPs from converting scarce last-mile bandwidth into a revenue stream by charging edge providers for preferential treatment, effectively transferring the option value of that potential revenue to edge developers and content startups as a free input.
% ABSENT_VOICES: Incumbent platform operators who would benefit from paid prioritization or sponsored-data deals are structurally excluded from the neutrality framing, which is posed as protecting ordinary users and small developers rather than adjudicating between two classes of powerful commercial actors (carriers vs. platforms).
% DISAPPEARANCE_RATIONALE: If the neutrality reading were abandoned in favor of a sibling reading, ISPs would begin building paid-prioritization and sponsored-data products within a fiscal cycle, incumbent platforms would negotiate carriage deals unavailable to smaller developers, and the current assumption of uniform reachability for new entrants would end — the competitive landscape for internet-native businesses would reorganize around carrier relationships rather than technical merit alone.
% FOUNDING_PROBLEM: Early internetworking needed a design where the network layer would not need to understand or adjudicate among applications, so that innovation could happen at the edges without requiring carrier permission for each new use case.
% FOUNDING_PROBLEM_CORROBORATION: Original protocol architects and academic network engineers outside the regulatory and carrier interest groups attest that the end-to-end design principle was a genuine architectural choice for extensibility, not an explicit non-discrimination mandate; carriers dispute that the design compels the specific policy conclusion regulators now derive from it, arguing the technical design underdetermines the neutrality reading.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__neutrality_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__neutrality_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(tcp_ip_interpretation__neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__neutrality_reading, 0.28, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tcp_ip_interpretation__neutrality_reading_tests).
:- end_tests(tcp_ip_interpretation__neutrality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-low (0.28) because the coordination function — uniform packet treatment enabling permissionless innovation — is genuine and substantial; the extraction component is the constrained revenue option value taken from ISPs and, secondarily, the compliance burden placed on network management teams who must justify traffic-shaping decisions as content-agnostic even when application-aware shaping would be more efficient. Suppression (0.42) reflects the active enforcement apparatus required to hold the line against carrier incentives to discriminate, not any coercion of end users. Theater ratio is low (0.2) — the enforcement substantially performs its stated function, though a growing share of compliance activity (documentation, audits) is procedural rather than functional as the doctrine matures.
 *
 * DIRECTIONALITY LOGIC:
 *   Edge application developers, content startups, and internet users are structural beneficiaries — the neutrality reading subsidizes their access to bandwidth without requiring carriage negotiation, so their directionality sits near the beneficiary end. Last-mile ISPs and network management teams are targets: the reading forecloses a revenue and efficiency option they would otherwise exercise, placing them nearer the full-target end despite ISPs' considerable power — their power does not translate into exit, since they remain bound by whichever kernel reading the regulator adopts and cannot unilaterally reinterpret the protocol.
 *
 * MANDATROPHY ANALYSIS:
 *   The neutrality reading's original founding problem — enabling edge innovation without requiring carrier permission — remains partially live (new entrants still depend on it) but is contested by carriers who argue the technical justification has been stretched to cover a policy preference the protocol design does not itself mandate. Classifying this as tangled_rope rather than a pure rope prevents mislabeling the arrangement as costless coordination: it is a genuine coordination mechanism for edge innovation that simultaneously extracts a real economic option from carriers, and both facts must be held together rather than resolved into a single verdict.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    protocol_design_vs_policy_mandate,
    'Does TCP/IP''s technical architecture actually entail the neutrality reading''s non-discrimination requirement, or is the reading a policy choice retrospectively justified by appeal to protocol design?',
    'Historical analysis of RFC discussions and original architects'' stated design intent (e.g., Saltzer, Reed, Clark''s end-to-end arguments paper) compared against the specific regulatory non-discrimination rules derived from it; assess whether the technical design underdetermines the policy conclusion.',
    'If the technical design underdetermines the neutrality reading, this constraint is better understood as a policy choice wearing a technical-necessity framing — which would not change this story''s ε (that is fixed for this reading) but would reduce confidence in the mountain-adjacent legitimacy claims sometimes made on the neutrality reading''s behalf.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(protocol_design_vs_policy_mandate, conceptual, 'Whether TCP/IP''s design technically mandates the neutrality reading or merely permits it.').

omega_variable(
    kernel_reading_selection_mechanism,
    'What determines which of the three readings (neutrality, prioritization, zero_rating) a given regulatory jurisdiction adopts, and is that selection mechanism itself contestable?',
    'Comparative study of jurisdictions that have adopted each reading (EU/India for neutrality-leaning rules, other markets permitting zero-rating) tracing the political and lobbying processes that produced divergent readings of the same underlying protocol.',
    'If reading-selection tracks political power of incumbent carriers versus edge developers more than any feature of the protocol itself, all three sibling constraints in this kernel family should be understood as contested policy outcomes rather than technical readings — reinforcing the need to keep them as separate, non-averaged constraint files.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_selection_mechanism, empirical, 'Whether jurisdictional reading-selection is driven by political economy rather than technical interpretation.').

omega_variable(
    enforcement_durability,
    'Does the neutrality reading persist as active law, or does it oscillate with regulatory administration changes, making its ''requires_active_enforcement'' status intermittent rather than stable?',
    'Track regulatory rulemaking history (adoption, repeal, re-adoption cycles) in jurisdictions with documented back-and-forth neutrality rulemaking.',
    'If enforcement is intermittent, the effective suppression this reading imposes on ISPs is lower on average than the point-in-time metrics suggest, and edge beneficiaries face more uncertainty than a stable-tangled-rope classification implies.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_durability, empirical, 'Whether neutrality enforcement is a stable regime or a cyclically contested one.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__neutrality_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__neutrality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tcp__tr_t4, tcp_ip_interpretation__neutrality_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(tcp__tr_t8, tcp_ip_interpretation__neutrality_reading, theater_ratio, 8, 0.14).
narrative_ontology:measurement(tcp__tr_t12, tcp_ip_interpretation__neutrality_reading, theater_ratio, 12, 0.16).
narrative_ontology:measurement(tcp__tr_t16, tcp_ip_interpretation__neutrality_reading, theater_ratio, 16, 0.17).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__neutrality_reading, theater_ratio, 20, 0.19).
narrative_ontology:measurement(tcp__tr_t24, tcp_ip_interpretation__neutrality_reading, theater_ratio, 24, 0.2).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(tcp__be_t4, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 4, 0.2).
narrative_ontology:measurement(tcp__be_t8, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 8, 0.22).
narrative_ontology:measurement(tcp__be_t12, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 12, 0.24).
narrative_ontology:measurement(tcp__be_t16, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 16, 0.26).
narrative_ontology:measurement(tcp__be_t20, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 20, 0.27).
narrative_ontology:measurement(tcp__be_t24, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 24, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(tcp__su_t4, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 4, 0.33).
narrative_ontology:measurement(tcp__su_t8, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 8, 0.36).
narrative_ontology:measurement(tcp__su_t12, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 12, 0.38).
narrative_ontology:measurement(tcp__su_t16, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 16, 0.4).
narrative_ontology:measurement(tcp__su_t20, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 20, 0.41).
narrative_ontology:measurement(tcp__su_t24, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 24, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__neutrality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__neutrality_reading, 0.12).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__prioritization_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three sibling readings of the tcp_ip_interpretation kernel, each authored as a separate constraint story per the ε-invariance principle: neutrality_reading (this file, tangled_rope, moderate-low ε favoring edge developers/users), prioritization_reading (favoring ISPs' differentiated-QoS revenue model), and zero_rating_reading (favoring platforms able to subsidize sponsored data). The three do not share an ε value — each reading produces a structurally distinct beneficiary/victim configuration and is linked here via network edges rather than folded into one story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
