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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: tcp_ip_interpretation__zero_rating_reading
 *   human_readable: TCP/IP Interpretation: Zero-Rating Exemptions
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   This constraint represents the interpretation of TCP/IP and internet
 *   policy that permits 'zero-rating,' where Internet Service Providers
 *   (ISPs) exempt certain sponsored content from users' data caps. This
 *   reading allows ISPs to partner with content providers, creating new
 *   revenue streams and influencing user behavior. It is a 'tangled rope'
 *   because it offers a coordination function (making some content more
 *   accessible) but also involves significant asymmetric extraction and
 *   suppression of alternatives for unaffiliated content providers and,
 *   indirectly, for users.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__zero_rating_reading, 0.65).
domain_priors:suppression_score(tcp_ip_interpretation__zero_rating_reading, 0.7).
domain_priors:theater_ratio(tcp_ip_interpretation__zero_rating_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(tcp_ip_interpretation__zero_rating_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__zero_rating_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "TCP/IP Interpretation: Zero-Rating Exemptions").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, '2f7c41ae-9572-41e7-aebb-7fdfb2bc4d34').
narrative_ontology:cs_kernel_codification('2f7c41ae-9572-41e7-aebb-7fdfb2bc4d34', fixed_text).
narrative_ontology:cs_authority_grounding('2f7c41ae-9572-41e7-aebb-7fdfb2bc4d34', extraction).
narrative_ontology:cs_interpretation_layer_present('2f7c41ae-9572-41e7-aebb-7fdfb2bc4d34').
narrative_ontology:cs_reading_relation('2f7c41ae-9572-41e7-aebb-7fdfb2bc4d34', tcp_ip_interpretation__neutrality_reading, influences).
narrative_ontology:cs_reading_relation('2f7c41ae-9572-41e7-aebb-7fdfb2bc4d34', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_axiom('2f7c41ae-9572-41e7-aebb-7fdfb2bc4d34', foundational, network_operator_flexibility_for_innovation).
narrative_ontology:cs_axiom_status(network_operator_flexibility_for_innovation, holdable).
narrative_ontology:cs_axiom_grounding('2f7c41ae-9572-41e7-aebb-7fdfb2bc4d34', network_operator_flexibility_for_innovation, instrumental).
narrative_ontology:cs_axiom('2f7c41ae-9572-41e7-aebb-7fdfb2bc4d34', secondary, differentiated_pricing_as_market_mechanism).
narrative_ontology:cs_axiom_status(differentiated_pricing_as_market_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('2f7c41ae-9572-41e7-aebb-7fdfb2bc4d34', differentiated_pricing_as_market_mechanism, conventional).
narrative_ontology:cs_reference_frame('2f7c41ae-9572-41e7-aebb-7fdfb2bc4d34', market_driven_network_management).
narrative_ontology:cs_drift_state('2f7c41ae-9572-41e7-aebb-7fdfb2bc4d34', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2f7c41ae-9572-41e7-aebb-7fdfb2bc4d34', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, internet_service_providers).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__zero_rating_reading, sponsored_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__zero_rating_reading, unaffiliated_content_providers).
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

% ISPs interpret TCP/IP to allow them to offer 'zero-rated' data, where certain content does not count against a user's data cap. This allows them to form partnerships with content providers, creating new revenue streams and increasing subscriber lock-in. They actively enforce these exemptions.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, internet_service_providers, agenda_setter,
    institutional, generational, arbitrage, national).

% These providers benefit from increased user engagement and reduced data costs for their content, as it doesn't count against user data caps. This gives them a competitive advantage over non-sponsored content, especially in markets with strict data limits.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, sponsored_content_providers, beneficiary,
    powerful, biographical, mobile, global).

% These providers' content counts against user data caps, putting them at a disadvantage compared to sponsored content. They face higher barriers to entry and growth, as users may prioritize zero-rated content. Their options are to seek sponsorship (if possible) or compete on an uneven playing field.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, unaffiliated_content_providers, payer,
    moderate, biographical, constrained, global).

% Users benefit from access to certain content without consuming their data allowance, which can be attractive. However, this incentivizes them to consume sponsored content, potentially limiting their choice and making it more expensive to access non-sponsored alternatives. Their 'choice' is shaped by the zero-rating offers.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, internet_users, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, internet_users, beneficiary).

% These bodies are tasked with interpreting telecommunications law and internet policy. They observe the market effects of zero-rating and may intervene to either permit, restrict, or ban the practice based on their interpretation of fair competition and consumer welfare.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, telecom_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows ISPs to coordinate with content providers to offer differentiated data plans, potentially making certain content more accessible to users and creating new business models for network operators.
% TRANSFER_FUNCTION: Transfers user attention and market share from unaffiliated content providers to sponsored content providers, and revenue from sponsored content providers to ISPs (through partnership agreements or increased market power).
% ABSENT_VOICES: Advocates for strict network neutrality, who argue that all data should be treated equally, are often excluded from the direct negotiation between ISPs and content providers. They would argue that zero-rating distorts competition and consumer choice.
% DISAPPEARANCE_RATIONALE: If zero-rating exemptions vanished, ISPs would lose a significant revenue stream and a tool for subscriber retention. Sponsored content providers would lose their competitive advantage, leading to a reshuffling of market share and potentially higher data consumption costs for users, or a shift in content consumption patterns.
% FOUNDING_PROBLEM: ISPs sought new revenue streams and ways to differentiate their services in competitive markets, while content providers sought ways to reach users more effectively, especially those with limited data plans.
% FOUNDING_PROBLEM_CORROBORATION: ISPs and sponsored content providers continue to assert the need for flexible business models and consumer choice. Critics (unaffiliated content providers, consumer advocacy groups, some regulators) argue that the 'problem' is now primarily about leveraging market power, not genuine innovation, and that the benefits are outweighed by anti-competitive effects.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__zero_rating_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__zero_rating_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__zero_rating_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(tcp_ip_interpretation__zero_rating_reading, 'none', 1).

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
 *   Extractiveness is high (0.65) because ISPs leverage their control over network access to generate revenue from content providers, and users are subtly steered towards sponsored content. Suppression is also high (0.7) as it actively disadvantages non-sponsored content and limits user choice, requiring active enforcement by ISPs to maintain these exemptions. The theater ratio is low (0.2) because the practice is genuinely functional for the beneficiaries, even if its benefits are unevenly distributed.
 *
 * PERSPECTIVAL GAP:
 *   ISPs and sponsored content providers view zero-rating as a beneficial innovation that enhances consumer choice and creates new business models. Unaffiliated content providers and many internet users, however, experience it as a form of discrimination that distorts competition and limits genuine choice. Telecom regulators often find themselves mediating these conflicting perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   ISPs and sponsored content providers are clear beneficiaries (d near 0.0), gaining revenue and market access. Unaffiliated content providers and internet users are targets (d near 1.0), bearing the costs of reduced visibility, distorted choice, and an unlevel playing field. Telecom regulators are analytical observers (d near 0.5), tasked with assessing the overall impact.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (facilitating content access, creating new business models) is still 'live' for its beneficiaries, but its status is 'contested' by victims who argue its primary function has shifted to rent extraction. The classification as a Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring any coordination function). The rising extractiveness and suppression over time indicate a drift towards greater extraction, even if a coordination function persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zero_rating_impact_on_competition,
    'To what extent does zero-rating genuinely enhance consumer welfare and competition, versus creating an unlevel playing field that entrenches incumbents and stifles innovation?',
    'Longitudinal studies comparing market dynamics in jurisdictions with and without zero-rating, focusing on new market entry, content diversity, and consumer switching behavior.',
    'If zero-rating is found to significantly stifle competition and innovation, it would strengthen arguments for regulatory intervention, potentially reclassifying the constraint closer to a Snare. If it genuinely expands access without undue harm, it would reinforce its Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_rating_impact_on_competition, empirical, 'Empirical assessment of zero-rating''s competitive effects.').

omega_variable(
    tcp_ip_original_intent,
    'Was the original design of TCP/IP intended to permit or prohibit such forms of content differentiation and economic leveraging by network operators?',
    'Historical analysis of RFCs, design documents, and early internet governance discussions, interpreted by network architects and legal scholars.',
    'If original intent strongly supports strict neutrality, the zero-rating reading would be seen as a significant departure, potentially weakening its legitimacy. If the original design was ambiguous or silent, it would support the ''coexists_with'' relationship with other readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tcp_ip_original_intent, conceptual, 'Ambiguity regarding TCP/IP''s foundational principles concerning network neutrality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__zero_rating_reading, 2007, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t2007, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 2007, 0.1).
narrative_ontology:measurement(tcp__tr_t2012, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 2012, 0.15).
narrative_ontology:measurement(tcp__tr_t2018, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 2018, 0.18).
narrative_ontology:measurement(tcp__tr_t2024, tcp_ip_interpretation__zero_rating_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(tcp__be_t2007, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 2007, 0.4).
narrative_ontology:measurement(tcp__be_t2012, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 2012, 0.5).
narrative_ontology:measurement(tcp__be_t2018, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 2018, 0.6).
narrative_ontology:measurement(tcp__be_t2024, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t2007, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 2007, 0.5).
narrative_ontology:measurement(tcp__su_t2012, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 2012, 0.6).
narrative_ontology:measurement(tcp__su_t2018, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 2018, 0.65).
narrative_ontology:measurement(tcp__su_t2024, tcp_ip_interpretation__zero_rating_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__zero_rating_reading, resource_allocation).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__neutrality_reading).
narrative_ontology:affects_constraint(tcp_ip_interpretation__zero_rating_reading, tcp_ip_interpretation__prioritization_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'tcp_ip_interpretation' kernel, alongside 'neutrality_reading' and 'prioritization_reading'. Each reading represents a distinct interpretation of TCP/IP's principles with different structural consequences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
