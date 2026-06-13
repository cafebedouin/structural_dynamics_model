% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: tcp_ip_interpretation__neutrality_reading
 *   human_readable: TCP/IP End-to-End Principle: Neutrality Reading (Non-Discrimination Mandate)
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   The TCP/IP protocol stack embodies the 'end-to-end principle': network
 *   operators should not make content- or application-aware decisions about
 *   traffic; they should provide dumb pipes and let intelligence live at the
 *   edges. This reading interprets that principle as a non-discrimination
 *   mandate: ISPs are prohibited from prioritizing, degrading, or blocking
 *   traffic by application type, content origin, or commercial relationship.
 *   Edge platforms and innovators benefit (no gatekeeping); ISPs are
 *   constrained in revenue optimization; regulators enforce via
 *   common-carrier classification and net neutrality rules. The neutrality
 *   reading has competed with a prioritization reading (ISPs can manage
 *   quality-of-service by traffic class) and a zero-rating reading (ISPs can
 *   exempt sponsored content) since at least 2010. This story models the
 *   neutrality reading as a single coherent constraint and routes the contest
 *   to the committer structure (cs_structure and omega variables). The
 *   authored extractiveness, suppression, and theater metrics reflect the
 *   neutrality reading's actual enforcement state (regulatory volatility,
 *   contested authority, theatrical compliance); the claim is rope
 *   (coordination around open innovation), and the metrics describe
 *   extractive enforcement overhead — the divergence is intentional and is
 *   the gap the engine measures.
 *
 * KEY AGENTS:
 *   - isps_tier_1: Institutional power, constrained exit. Bear the cost of the non-discrimination mandate; argue it prevents revenue optimization.
 *   - edge_innovators: Moderate power, mobile exit. Benefit from guaranteed non-discriminatory access; can compete without ISP gatekeeping.
 *   - content_platforms_not_tier_1: Organized power, arbitrage exit. Benefit from non-discrimination; have secondary exit via private interconnection.
 *   - residential_users: Powerless, trapped exit. Protected from ISP-mediated gatekeeping; benefit from diverse service availability.
 *   - internet_standards_bodies: Institutional power, analytical position. Anchor the technical interpretation of the end-to-end principle.
 *   - telecommunications_regulators: Institutional power, analytical position. Enforce non-discrimination through policy and litigation.
 *   - courts: Institutional power, analytical position. Arbitrate whether regulators have authority to mandate non-discrimination.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, 0.38).
domain_priors:suppression_score(tcp_ip_interpretation__neutrality_reading, 0.52).
domain_priors:theater_ratio(tcp_ip_interpretation__neutrality_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0.52).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, accessibility_collapse, 0.67).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__neutrality_reading, rope).
narrative_ontology:human_readable(tcp_ip_interpretation__neutrality_reading, "TCP/IP End-to-End Principle: Neutrality Reading (Non-Discrimination Mandate)").
narrative_ontology:topic_domain(tcp_ip_interpretation__neutrality_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__neutrality_reading, '8d494c71-cbf6-492a-aac0-b638da455b2a').
narrative_ontology:cs_kernel_codification('8d494c71-cbf6-492a-aac0-b638da455b2a', fixed_text).
narrative_ontology:cs_authority_grounding('8d494c71-cbf6-492a-aac0-b638da455b2a', lineage).
narrative_ontology:cs_interpretation_layer_present('8d494c71-cbf6-492a-aac0-b638da455b2a').
narrative_ontology:cs_reading_relation('8d494c71-cbf6-492a-aac0-b638da455b2a', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_reading_relation('8d494c71-cbf6-492a-aac0-b638da455b2a', tcp_ip_interpretation__zero_rating_reading, coexists_with).
narrative_ontology:cs_axiom('8d494c71-cbf6-492a-aac0-b638da455b2a', foundational, network_operator_non_discrimination_requirement).
narrative_ontology:cs_axiom_status(network_operator_non_discrimination_requirement, holdable).
narrative_ontology:cs_axiom_grounding('8d494c71-cbf6-492a-aac0-b638da455b2a', network_operator_non_discrimination_requirement, deontological).
narrative_ontology:cs_axiom('8d494c71-cbf6-492a-aac0-b638da455b2a', foundational, edge_innovation_protection_via_open_access).
narrative_ontology:cs_axiom_status(edge_innovation_protection_via_open_access, holdable).
narrative_ontology:cs_axiom_grounding('8d494c71-cbf6-492a-aac0-b638da455b2a', edge_innovation_protection_via_open_access, instrumental).
narrative_ontology:cs_reference_frame('8d494c71-cbf6-492a-aac0-b638da455b2a', end_to_end_principle_as_non_discrimination).
narrative_ontology:cs_drift_state('8d494c71-cbf6-492a-aac0-b638da455b2a', contemporary_regulatory_fragmentation_2024, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('8d494c71-cbf6-492a-aac0-b638da455b2a', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, edge_innovators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, content_platforms_not_tier_1).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, residential_users).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, democratic_information_access).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__neutrality_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(tcp_ip_interpretation__neutrality_reading, 'none', 1).

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
 *   The neutrality reading claims rope (genuine coordination problem: prevent strategic gatekeeping; enable open innovation). The metrics are moderate: extractiveness 0.38 because suppression and enforcement overhead are substantial relative to the coordination benefit actually achieved (regulators have flipped the rule multiple times; ISPs comply under duress and exit via lobbying). Suppression is high (0.52) because the rule is actively enforced via regulatory threat, litigation, and classification disputes; ISPs face fines and reclassifications if caught discriminating. Theater is moderate (0.41) because compliance mechanisms are theatrical: ISPs report zero prioritization but engage in subtle degradation and zero-rating partnerships that stay below regulatory visibility thresholds. The measurement series tracks the constraint's operational history: extractiveness grew from near-zero in 1995 (no commercial ISP gatekeeping threat yet) to 0.38 by 2024 (mature enforcement, routine violations, regulatory capture attempts). Suppression requirement grew from 0.18 to 0.52 as ISP resistance hardened and enforcement became more legalistic. Theater ratio grew from 0.08 to 0.41 as the constraint shifted from natural coordination (everyone wanted open networks in the mid-90s) to defended regime (ISPs now defend against it through regulatory capture and technical evasion). All metrics are authored on a single shared time grid (1995, 2005, 2010, 2015, 2020, 2024) so temporal analysis is continuous.
 *
 * PERSPECTIVAL GAP:
 *   ISPs see this as a rope constraint they were forced into: they built network infrastructure at cost and regulatory approval explicitly permitted service differentiation; the retrospective non-discrimination mandate violates property rights and competitive fairness. From the ISP seat, this is snare-like extraction enforced by regulators who captured the agenda. Edge platforms and users see this as genuine rope: it solved the gatekeeping problem and enables competition. From those seats, the constraint is legitimate coordination. Courts and regulators see administrative authority and public-interest framing: the constraint is justified to prevent monopoly abuse. The engine computes per-seat classifications from the structural data (power, exit, beneficiary/victim); the divergence in how the constraint is experienced across seats is the point — it is a rope with extractive enforcement overhead, experienced differently by institutional and edge-level actors.
 *
 * DIRECTIONALITY LOGIC:
 *   ISPs (institutional power, constrained exit, payer role) compute d near 1.0 (full target): the constraint directly limits their revenue optimization and is enforced against their interests. Edge innovators and platforms (moderate/organized power, mobile/arbitrage exit, beneficiary role) compute d near 0.0 (full beneficiary): the constraint subsidizes their market entry and competitive position. Residential users (powerless, trapped exit, beneficiary role) compute d near 0.1 (substantial subsidy): they benefit from protection against gatekeeping but have no practical exit if the constraint fails. Courts and regulators (institutional power, analytical exit, agenda_setter role) compute d near 0.5 (symmetric): they administer the constraint and bear equal stakes in maintaining its legitimacy and managing its implementation costs.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (prevent ISP gatekeeping) remains live in the neutrality reading, but the mandate has partially atrophied in enforcement. Regulatory classifications have flip-flopped (US FCC reclassified ISPs as information services in 2020, then back to common carriers in 2024); ISPs have found legal workarounds (zero-rating is technically not blocking, just differential pricing); and enforcement is patchy (EU net neutrality rules are stricter than US post-2020). The theater_ratio rising from 0.08 to 0.41 indicates increasing performative compliance: ISPs publish neutrality commitments but engage in subtle degradation that is technically hard to prove. The constraint has not failed (founding_problem_status = contested, not dead), but enforcement decay is visible. The mandatrophy is partial and contested rather than terminal.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    end_to_end_principle_empirical_content,
    'Is the end-to-end principle a technical claim about efficient network design, a moral claim about ISP behavior, or a binding architectural rule for TCP/IP itself?',
    'Literature survey of RFC 1958 (Saltzer, Reed, Clark) and subsequent protocol specifications. Technical analysis of whether TCP/IP actually requires end-to-end operation or merely recommends it. Historian analysis of how the principle was framed in different eras.',
    'If the principle is only a design recommendation (technical preference, not a binding rule), then interpreting it as a non-discrimination mandate is a normative addition, not a discovery — the reading is constructive policy, not protocol interpretation. If the principle is a binding architectural requirement, the neutrality reading is technically sound. Ambiguity here generates a committer-frame ambiguity: the ISP position is that end-to-end is historical context that does not dictate current network management; the neutrality position is that it does. The type classification is stable (rope under the neutrality reading); the ambiguity is whether the reading itself is justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(end_to_end_principle_empirical_content, conceptual, 'Whether end-to-end is a binding architectural rule or a design preference.').

omega_variable(
    network_management_vs_discrimination,
    'Can an ISP distinguish traffic by quality-of-service class for legitimate network management without crossing into discriminatory prioritization?',
    'Operational evidence from ISPs that manage congestion: do QoS classes correlate with application type (Netflix, Zoom, gaming) or with technical congestion signal (packet loss, latency)? Can ISPs implement neutral QoS without learning what application is running? What do network engineers at tier-1 ISPs and content platforms say about feasibility?',
    'If legitimate QoS is possible without content awareness, the neutrality reading is sustainable: discriminate by technical signal, not application. If QoS always requires content inspection, the neutrality reading forces ISPs into a binary choice (manage congestion via neutral means, or allow degradation). This omega resolves part of the prioritization/neutrality contest: if QoS is feasible neutrally, prioritization reading loses empirical ground.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_management_vs_discrimination, empirical, 'Whether network management can be implemented without content discrimination.').

omega_variable(
    regulatory_authority_for_protocol_interpretation,
    'Do telecommunications regulators have authority to mandate how TCP/IP is interpreted and deployed in commercial networks, or is protocol specification the sole domain of IETF and technical standards bodies?',
    'Constitutional and administrative-law analysis in jurisdictions where the question is contested (US courts, EU regulatory bodies). Does the principle of net neutrality depend on regulatory interpretation of a technical standard, or on technical fact?',
    'If regulators lack authority to interpret TCP/IP, the neutrality reading is a policy choice (legitimate but not binding on all parties); if they do have authority, the reading is a regulatory fact. This affects the commitment-system structure: is the kernel (end-to-end principle) anchored in technical standards bodies (IETF) or regulatory bodies (FCC/Ofcom)? The type classification is stable; the authority ambiguity is structural.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(regulatory_authority_for_protocol_interpretation, conceptual, 'Who has authority to interpret TCP/IP''s end-to-end principle for regulatory purposes?').

omega_variable(
    zero_rating_as_discrimination,
    'Is ISP exemption of zero-rated content from data caps a form of content discrimination (prioritizing some applications) or a legitimate service tier (differential pricing without degradation)?',
    'Technical evidence: does zero-rating change bits-on-the-wire behavior, or only change billing? Economic evidence: does zero-rating reduce demand for competing services? Regulatory precedent from EU, FCC, and other jurisdictions.',
    'If zero-rating is pure pricing discrimination (not technical), the neutrality reading could tolerate it (non-discrimination in bits, not billing). If zero-rating is effective prioritization (makes selected applications faster in user perception), the neutrality reading forbids it. This omega directly affects how the zero_rating_reading sister constraint is classified and whether it coexists with or forecloses the neutrality reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_rating_as_discrimination, empirical, 'Whether zero-rating is technical discrimination or pricing differentiation.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression (0.52) structural (regulatory threat, litigation risk, commercial penalties for violations) or partially internalized (ISP compliance culture, professional norms, executive risk aversion)?',
    'Post-deregulation natural experiment: if neutrality rules are repealed, do ISPs immediately violate or do they continue compliance due to internalized norms? Interview evidence from ISP engineers and executives about compliance drivers.',
    'If suppression is entirely structural (regulatory), removing the rule would trigger immediate violation. If partially internalized, some compliance persists even without enforcement. This affects the type stability: a high-inertia rope with internalized suppression is more resilient than one dependent on active enforcement. Theater_ratio rising to 0.41 suggests some internalization is present (theatrical compliance suggests norms, not pure fear); but the ambiguity remains.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether ISP compliance is structural coercion or internalized norm.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__neutrality_reading, 1995, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t1995, tcp_ip_interpretation__neutrality_reading, theater_ratio, 1995, 0.08).
narrative_ontology:measurement(tcp__tr_t2005, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2005, 0.14).
narrative_ontology:measurement(tcp__tr_t2010, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2010, 0.22).
narrative_ontology:measurement(tcp__tr_t2015, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2015, 0.32).
narrative_ontology:measurement(tcp__tr_t2020, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2020, 0.39).
narrative_ontology:measurement(tcp__tr_t2024, tcp_ip_interpretation__neutrality_reading, theater_ratio, 2024, 0.41).

% Extraction over time
narrative_ontology:measurement(tcp__be_t1995, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 1995, 0.15).
narrative_ontology:measurement(tcp__be_t2005, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2005, 0.22).
narrative_ontology:measurement(tcp__be_t2010, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2010, 0.28).
narrative_ontology:measurement(tcp__be_t2015, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement(tcp__be_t2020, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2020, 0.37).
narrative_ontology:measurement(tcp__be_t2024, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 2024, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t1995, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 1995, 0.18).
narrative_ontology:measurement(tcp__su_t2005, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2005, 0.31).
narrative_ontology:measurement(tcp__su_t2010, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2010, 0.41).
narrative_ontology:measurement(tcp__su_t2015, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2015, 0.48).
narrative_ontology:measurement(tcp__su_t2020, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2020, 0.5).
narrative_ontology:measurement(tcp__su_t2024, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 2024, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__neutrality_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(tcp_ip_interpretation__neutrality_reading, 0.12).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__prioritization_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation__zero_rating_reading).

% DUAL FORMULATION NOTE:
% tcp_ip_interpretation is a contested kernel with three readings: neutrality_reading (this file), prioritization_reading, and zero_rating_reading. The readings are structurally distinct constraints with different ε values, beneficiary/victim structures, and founding problems. They coexist or influence each other (not mutual foreclosure) because the kernel (end-to-end principle in TCP/IP) is ambiguous and subject to competing interpretations by different parties. Neutrality emphasizes non-discrimination and edge protection; prioritization emphasizes ISP autonomy and network management; zero-rating emphasizes selective market access. All three are live positions in regulatory and policy discourse. Each story models its reading as a standalone constraint; the network edges track logical and structural relationships. The three readings together form a constraint family; understanding any one requires reference to its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(tcp_ip_interpretation__neutrality_reading, institutional, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
