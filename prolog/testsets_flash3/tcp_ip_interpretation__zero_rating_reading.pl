% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__zero_rating_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tcp_ip_interpretation__zero_rating_reading, []).

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
 *   constraint_id: tcp_ip_interpretation__zero_rating_reading
 *   human_readable: TCP/IP Interpretation: Zero-Rating as Permissible
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   This constraint represents the interpretation of TCP/IP and internet
 *   policy that permits 'zero-rating' – where Internet Service Providers
 *   (ISPs) exempt specific content or applications from counting against a
 *   user's data cap, often in partnership with content providers. This
 *   reading allows ISPs to leverage their control over network access to
 *   create tiered access to content, benefiting incumbent content providers
 *   and ISPs at the expense of independent content creators and, indirectly,
 *   user choice. The claimed type is 'tangled_rope' because it offers a
 *   coordination function (perceived free data for users) alongside
 *   significant asymmetric extraction.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, 0.68).
domain_priors:suppression_score(tcp_ip_interpretation__zero_rating_reading, 0.75).
domain_priors:theater_ratio(tcp_ip_interpretation__zero_rating_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__zero_rating_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "TCP/IP Interpretation: Zero-Rating as Permissible").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, 'f6bc2b7c-3508-4a96-a996-9ef69373799f').
narrative_ontology:cs_kernel_codification('f6bc2b7c-3508-4a96-a996-9ef69373799f', fixed_text).
narrative_ontology:cs_authority_grounding('f6bc2b7c-3508-4a96-a996-9ef69373799f', extraction).
narrative_ontology:cs_interpretation_layer_present('f6bc2b7c-3508-4a96-a996-9ef69373799f').
narrative_ontology:cs_reading_relation('f6bc2b7c-3508-4a96-a996-9ef69373799f', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('f6bc2b7c-3508-4a96-a996-9ef69373799f', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_axiom('f6bc2b7c-3508-4a96-a996-9ef69373799f', foundational, network_management_includes_commercial_differentiation).
narrative_ontology:cs_axiom_status(network_management_includes_commercial_differentiation, holdable).
narrative_ontology:cs_axiom_grounding('f6bc2b7c-3508-4a96-a996-9ef69373799f', network_management_includes_commercial_differentiation, conventional).
narrative_ontology:cs_axiom('f6bc2b7c-3508-4a96-a996-9ef69373799f', secondary, consumer_choice_is_enhanced_by_sponsored_data).
narrative_ontology:cs_axiom_status(consumer_choice_is_enhanced_by_sponsored_data, holdable).
narrative_ontology:cs_axiom_grounding('f6bc2b7c-3508-4a96-a996-9ef69373799f', consumer_choice_is_enhanced_by_sponsored_data, empirically_contingent).
narrative_ontology:cs_reference_frame('f6bc2b7c-3508-4a96-a996-9ef69373799f', commercial_flexibility_framework).
narrative_ontology:cs_drift_state('f6bc2b7c-3508-4a96-a996-9ef69373799f', contemporary_regulatory_scrutiny, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f6bc2b7c-3508-4a96-a996-9ef69373799f', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, internet_service_providers).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, sponsored_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, independent_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, internet_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, internet_users).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% ISPs interpret TCP/IP as allowing them to offer 'zero-rated' services, where data used by specific applications or content does not count against a subscriber's data cap. This allows them to form partnerships with content providers, creating new revenue streams and customer lock-in. They actively enforce these policies through billing systems and network management.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, internet_service_providers, agenda_setter,
    institutional, generational, arbitrage, national).

% These are typically large, incumbent content companies (e.g., major streaming services, social media platforms) that partner with ISPs to have their content zero-rated. They benefit from increased user engagement and reduced data costs for their users, which translates to a competitive advantage over non-sponsored content.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, sponsored_content_providers, beneficiary,
    powerful, biographical, mobile, global).

% Smaller or newer content providers who cannot afford or are not offered zero-rating partnerships. Their content consumes users' data allowances, putting them at a disadvantage against zero-rated competitors. They bear the cost of reduced discoverability and user engagement due to data cap concerns.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, independent_content_providers, payer,
    moderate, biographical, constrained, global).

% Users benefit from accessing zero-rated content without it counting against their data caps, which can be a perceived cost saving. However, they are subtly steered towards specific content and may face higher effective costs for non-zero-rated content, limiting their choice and potentially stifling innovation. Their exit options are limited by available ISP choices in their region.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, internet_users, payer,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, internet_users, beneficiary).

% Government bodies tasked with overseeing telecommunications and internet policy. They investigate zero-rating practices for potential anti-competitive effects or violations of net neutrality principles, often facing pressure from both ISPs and consumer advocacy groups. Their actions can alter the regulatory landscape for zero-rating.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, telecom_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows ISPs and content providers to coordinate on business models that offer consumers perceived value (free data for certain services), potentially expanding internet access or usage for specific applications.
% TRANSFER_FUNCTION: Transfers economic value from non-sponsored content providers (who face a disadvantage) and, indirectly, from internet users (who are steered towards specific content) to ISPs and sponsored content providers, in the form of increased revenue and market share.
% ABSENT_VOICES: Advocates for a strict interpretation of net neutrality, who argue that zero-rating inherently violates the principle of non-discrimination, are often excluded from direct policy-making processes where zero-rating is permitted. They would argue for a level playing field for all content.
% DISAPPEARANCE_RATIONALE: If zero-rating vanished overnight, ISPs would lose a significant revenue stream and a tool for customer acquisition/retention. Sponsored content providers would lose their competitive advantage, leading to a re-evaluation of their distribution strategies. Internet users would face uniform data charges, potentially shifting their consumption patterns. The market for internet content and services would rebalance, likely increasing competition for non-incumbent providers.
% FOUNDING_PROBLEM: ISPs sought new revenue streams and ways to differentiate services in a competitive market, while content providers sought guaranteed reach to users, especially those with data caps.
% FOUNDING_PROBLEM_CORROBORATION: ISPs and sponsored content providers attest that zero-rating addresses ongoing market needs for innovation and consumer choice. Independent content providers and net neutrality advocates, however, argue that the 'problem' zero-rating solves is primarily a business model challenge for incumbents, not a genuine market failure, and that it creates new problems for competition and user choice.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__zero_rating_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__zero_rating_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__zero_rating_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(tcp_ip_interpretation__zero_rating_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__zero_rating_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tcp_ip_interpretation__zero_rating_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(tcp_ip_interpretation__zero_rating_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because zero-rating creates an uneven playing field, disadvantaging non-sponsored content and effectively charging users for data that would otherwise be 'free' if all content were treated equally. Suppression (0.75) is also high, as ISPs actively enforce these policies through network management and contractual agreements, effectively suppressing alternative business models for content delivery. The theater ratio is low (0.20) because the zero-rating mechanism is genuinely functional in delivering its intended benefit (free data for specific content), even if the underlying justification is contested.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ISPs and sponsored content providers, zero-rating is a legitimate business innovation that benefits consumers. From the perspective of independent content providers and net neutrality advocates, it is a form of anti-competitive behavior that distorts the internet market. The engine's classification will reflect this divergence based on the structural roles and metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   ISPs and sponsored content providers are clear beneficiaries, gaining revenue and market share. Independent content providers and internet users are payers, bearing the costs of reduced competition and constrained choice. Telecom regulators act as observers, evaluating the impact of these practices.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zero_rating_impact_on_competition,
    'To what extent does zero-rating genuinely stifle competition and innovation among content providers, versus merely shifting market dynamics?',
    'Longitudinal studies comparing market entry and growth rates of independent content providers in jurisdictions with and without zero-rating, controlling for other market factors.',
    'If competition is significantly stifled, it strengthens the argument for zero-rating as a snare or highly extractive tangled rope. If market dynamics merely shift without stifling innovation, it might lean towards a more benign tangled rope or even a rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_rating_impact_on_competition, empirical, 'Empirical assessment of zero-rating''s effect on market competition.').

omega_variable(
    consumer_benefit_vs_choice_distortion,
    'Does the perceived benefit of ''free data'' for zero-rated content outweigh the potential distortion of consumer choice and the hidden costs of a less open internet?',
    'Consumer surveys and behavioral economics studies measuring willingness-to-pay for non-zero-rated content, and the impact of zero-rating on content discovery and consumption patterns.',
    'If choice distortion is significant and unacknowledged by consumers, it increases the effective extractiveness and suppression. If consumers genuinely value the zero-rated offerings and understand the trade-offs, it could reduce the perceived extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consumer_benefit_vs_choice_distortion, preference, 'Balancing consumer perceived benefit against market distortion.').

omega_variable(
    kernel_reading_ambiguity,
    'Is this ''zero_rating_reading'' a legitimate interpretation of TCP/IP''s design principles, or a policy choice that leverages technical ambiguities for commercial gain?',
    'Historical analysis of TCP/IP''s original design intent and subsequent architectural evolution, alongside legal and policy debates on network neutrality. Resolution would involve a conceptual re-framing of the ''spirit'' of the internet''s foundational protocols.',
    'If deemed a legitimate interpretation, it reinforces the ''tangled_rope'' classification as a policy choice. If deemed a leveraging of ambiguity, it pushes towards a ''snare'' classification, implying a more deliberate and less justifiable extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Ambiguity regarding the foundational principles of TCP/IP and their application to zero-rating.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__zero_rating_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(tcp__tr_t5, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 5, 0.12).
narrative_ontology:measurement(tcp__tr_t10, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(tcp__tr_t15, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 15, 0.18).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(tcp__be_t5, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(tcp__be_t10, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(tcp__be_t15, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 15, 0.66).
narrative_ontology:measurement(tcp__be_t20, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(tcp__su_t5, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 5, 0.65).
narrative_ontology:measurement(tcp__su_t10, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(tcp__su_t15, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 15, 0.73).
narrative_ontology:measurement(tcp__su_t20, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 20, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__zero_rating_reading, resource_allocation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, net_neutrality_regulation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, digital_market_competition).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('zero_rating_reading') of the 'tcp_ip_interpretation' kernel. It coexists with 'neutrality_reading' and 'prioritization_reading', which offer alternative interpretations of TCP/IP's principles regarding network traffic management.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
