% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__neutrality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
 *   constraint_id: tcp_ip_interpretation__neutrality_reading
 *   human_readable: TCP/IP End-to-End Principle: Network Neutrality Reading
 *   domain: Technology Governance / Internet Policy / Telecommunications Law
 *
 * SUMMARY:
 *   This constraint story instantiates the 'neutrality_reading' of the
 *   broader 'tcp_ip_interpretation' kernel. It posits that the fundamental
 *   design of TCP/IP, particularly the end-to-end principle, inherently
 *   requires internet service providers (ISPs) to treat all data packets
 *   equally, without discrimination based on content, application, source, or
 *   destination. This reading aims to prevent ISPs from leveraging their
 *   control over internet access to extract rents from content providers or
 *   users through discriminatory practices like throttling, blocking, or paid
 *   prioritization. The constraint is actively enforced through regulatory
 *   frameworks and public advocacy, constantly challenged by ISPs seeking
 *   greater control over traffic management.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__neutrality_reading, 0.15).
domain_priors:suppression_score(tcp_ip_interpretation__neutrality_reading, 0.75).
domain_priors:theater_ratio(tcp_ip_interpretation__neutrality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(tcp_ip_interpretation__neutrality_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__neutrality_reading, rope).
narrative_ontology:human_readable(tcp_ip_interpretation__neutrality_reading, "TCP/IP End-to-End Principle: Network Neutrality Reading").
narrative_ontology:topic_domain(tcp_ip_interpretation__neutrality_reading, "Technology Governance / Internet Policy / Telecommunications Law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__neutrality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__neutrality_reading, '4ba8f0a3-8477-4fc8-a1e2-867cf1766d44').
narrative_ontology:cs_kernel_codification('4ba8f0a3-8477-4fc8-a1e2-867cf1766d44', implicit).
narrative_ontology:cs_authority_grounding('4ba8f0a3-8477-4fc8-a1e2-867cf1766d44', practice).
narrative_ontology:cs_interpretation_layer_present('4ba8f0a3-8477-4fc8-a1e2-867cf1766d44').
narrative_ontology:cs_reading_relation('4ba8f0a3-8477-4fc8-a1e2-867cf1766d44', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_reading_relation('4ba8f0a3-8477-4fc8-a1e2-867cf1766d44', tcp_ip_interpretation__zero_rating_reading, coexists_with).
narrative_ontology:cs_axiom('4ba8f0a3-8477-4fc8-a1e2-867cf1766d44', foundational, network_neutrality_is_foundational).
narrative_ontology:cs_axiom_status(network_neutrality_is_foundational, holdable).
narrative_ontology:cs_axiom_grounding('4ba8f0a3-8477-4fc8-a1e2-867cf1766d44', network_neutrality_is_foundational, deontological).
narrative_ontology:cs_axiom('4ba8f0a3-8477-4fc8-a1e2-867cf1766d44', secondary, no_content_discrimination).
narrative_ontology:cs_axiom_status(no_content_discrimination, holdable).
narrative_ontology:cs_axiom_grounding('4ba8f0a3-8477-4fc8-a1e2-867cf1766d44', no_content_discrimination, empirically_contingent).
narrative_ontology:cs_reference_frame('4ba8f0a3-8477-4fc8-a1e2-867cf1766d44', open_internet_design_philosophy).
narrative_ontology:cs_drift_state('4ba8f0a3-8477-4fc8-a1e2-867cf1766d44', contemporary_regulatory_challenges, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4ba8f0a3-8477-4fc8-a1e2-867cf1766d44', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__neutrality_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, internet_users).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, edge_innovators).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__neutrality_reading, content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__neutrality_reading, internet_service_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from open access to all content and applications without discrimination or throttling by their ISP. They bear indirect costs if ISPs pass on compliance costs, but primarily benefit from choice and innovation.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, internet_users, beneficiary,
    moderate, biographical, constrained, global).

% Can launch new services and applications on the internet without needing permission or paying extra fees to ISPs for prioritized access. Their ability to innovate and compete depends on a level playing field.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, edge_innovators, beneficiary,
    moderate, biographical, mobile, global).

% Can reach their audience without fear of their content being blocked, throttled, or subjected to fast lanes by ISPs. This ensures fair competition and broad distribution for their services.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, content_providers, beneficiary,
    powerful, biographical, mobile, global).

% Are prohibited from discriminating against internet traffic based on source, destination, or content. This constrains their ability to optimize revenue through differentiated services or charging content providers for priority access, which they view as a burden.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, internet_service_providers, payer,
    institutional, generational, constrained, national).

% Are tasked with defining and enforcing network neutrality rules, often against strong lobbying from ISPs. Their role is to balance innovation, consumer protection, and infrastructure investment.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, telecom_regulators, agenda_setter,
    institutional, generational, analytical, national).

% Understand the technical architecture of TCP/IP and the implications of various traffic management practices. They often advocate for the end-to-end principle as fundamental to the internet's design and functionality.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__neutrality_reading, network_engineers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(tcp_ip_interpretation__neutrality_reading, diffuse).
narrative_ontology:fixing_cost_class(tcp_ip_interpretation__neutrality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ensures a neutral, non-discriminatory platform for all internet traffic, fostering innovation at the 'edge' of the network and maximizing user choice by preventing ISPs from acting as gatekeepers.
% TRANSFER_FUNCTION: Prevents the transfer of value from edge innovators and internet users to internet service providers, which would occur if ISPs could charge for prioritized access or block content.
% ABSENT_VOICES: ISPs advocating for prioritization, zero-rating, or other discriminatory practices are often present in policy debates, but their arguments are structurally opposed to this reading's core tenets. They would argue for greater network management flexibility and revenue optimization.
% DISAPPEARANCE_RATIONALE: If the end-to-end principle and its non-discrimination requirement vanished overnight, ISPs would rapidly implement tiered services, fast lanes, and content blocking, fragmenting the internet, stifling edge innovation, and fundamentally altering the digital economy.
% FOUNDING_PROBLEM: Preventing network operators from controlling or discriminating against content and applications, ensuring an open and innovative internet where innovation occurs at the 'edge' rather than being dictated by the 'core' network providers.
% FOUNDING_PROBLEM_CORROBORATION: Internet pioneers, academic researchers, and consumer advocacy groups consistently corroborate the ongoing relevance and necessity of this principle, citing continuous threats from ISPs seeking to monetize traffic discrimination. Legislative hearings and independent technical analyses also support this view.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__neutrality_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__neutrality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__neutrality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(tcp_ip_interpretation__neutrality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__neutrality_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The `extractiveness` is low (0.15) because this reading's primary function is to *prevent* extraction by ISPs, ensuring a level playing field. `suppression` is high (0.75) because it actively suppresses ISP business models that rely on discriminatory traffic management, requiring constant regulatory and technical enforcement. `resistance` is also high (0.8) due to continuous lobbying and legal challenges from ISPs. `theater_ratio` is low (0.1) as the principle is genuinely functional, though its enforcement often involves public debate and legal battles. `accessibility_collapse` is low (0.2) because the principle ensures that alternatives (new edge services) are not collapsed by ISP gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of internet users and edge innovators, this constraint is a vital 'Rope' that coordinates an open and innovative internet. From the perspective of ISPs, it is a 'Snare' that prevents them from optimizing their networks and revenue streams through differentiated services, forcing them to bear costs without commensurate benefits. The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Internet users, edge innovators, and content providers are the primary beneficiaries, as they gain from an open, non-discriminatory internet. Internet service providers are the payers, as they bear the cost of foregone revenue from discriminatory practices. Telecom regulators act as agenda-setters, responsible for upholding and enforcing the principle. Network engineers serve as observers, providing technical insights into the principle's implications.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_context,
    'Is this constraint a genuine interpretation of TCP/IP''s inherent design, or a policy choice layered onto a flexible technical standard?',
    'Consensus among internet architects and engineers regarding the ''end-to-end principle'' as a foundational design philosophy, versus legal/economic arguments for network management flexibility.',
    'If a genuine interpretation, it strengthens the ''Rope'' classification by grounding it in technical necessity. If a policy choice, it highlights the ''enforcement_mechanism'' aspect and the contestability of its ''naturalness''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_context, conceptual, 'Ambiguity between technical necessity and policy choice in interpreting TCP/IP.').

omega_variable(
    definition_of_discrimination,
    'What constitutes ''discrimination'' in a technically complex and dynamic network environment?',
    'Development of clear, measurable technical standards for identifying discriminatory traffic management practices, agreed upon by engineers and regulators.',
    'A clear definition would strengthen enforcement and reduce ISP resistance by removing ambiguity. An ambiguous definition allows ISPs to exploit loopholes, increasing effective extraction and reducing the constraint''s ''Rope'' function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_discrimination, empirical, 'Ambiguity in defining and measuring network discrimination.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (regulatory barriers, legal penalties) or internalized (ISPs accepting the principle as legitimate)?',
    'Post-deregulation ISP behavior: if discriminatory practices immediately resume, suppression is structural. If some ISPs voluntarily maintain neutrality, partial internalization is present.',
    'If primarily structural, the constraint''s persistence depends entirely on active enforcement. If partly internalized, the constraint has a stronger normative grounding beyond pure coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for ISPs.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__neutrality_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__neutrality_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tcp__tr_t8, tcp_ip_interpretation__neutrality_reading, theater_ratio, 8, 0.07).
narrative_ontology:measurement(tcp__tr_t16, tcp_ip_interpretation__neutrality_reading, theater_ratio, 16, 0.08).
narrative_ontology:measurement(tcp__tr_t24, tcp_ip_interpretation__neutrality_reading, theater_ratio, 24, 0.09).
narrative_ontology:measurement(tcp__tr_t32, tcp_ip_interpretation__neutrality_reading, theater_ratio, 32, 0.1).
narrative_ontology:measurement(tcp__tr_t40, tcp_ip_interpretation__neutrality_reading, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(tcp__be_t8, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 8, 0.12).
narrative_ontology:measurement(tcp__be_t16, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 16, 0.13).
narrative_ontology:measurement(tcp__be_t24, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 24, 0.14).
narrative_ontology:measurement(tcp__be_t32, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 32, 0.15).
narrative_ontology:measurement(tcp__be_t40, tcp_ip_interpretation__neutrality_reading, base_extractiveness, 40, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(tcp__su_t8, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 8, 0.65).
narrative_ontology:measurement(tcp__su_t16, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 16, 0.7).
narrative_ontology:measurement(tcp__su_t24, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 24, 0.73).
narrative_ontology:measurement(tcp__su_t32, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 32, 0.75).
narrative_ontology:measurement(tcp__su_t40, tcp_ip_interpretation__neutrality_reading, suppression_requirement, 40, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__neutrality_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, internet_innovation_ecosystem).
narrative_ontology:affects_constraint(tcp_ip_interpretation__neutrality_reading, digital_economy_competition).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'tcp_ip_interpretation' kernel, focusing on non-discrimination. Other readings (prioritization, zero-rating) represent alternative interpretations of TCP/IP's implications for network governance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
