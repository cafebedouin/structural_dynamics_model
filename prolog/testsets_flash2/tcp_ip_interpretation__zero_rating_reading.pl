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
 *   human_readable: TCP/IP Interpretation: Zero-Rating Exemptions for Sponsored Content
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   This constraint represents the 'zero-rating' reading of TCP/IP, where
 *   Internet Service Providers (ISPs) are permitted to exempt certain
 *   sponsored content from users' data caps. This interpretation allows ISPs
 *   to form partnerships with content providers, creating a tiered internet
 *   where some content is 'free' (data-wise) and other content counts against
 *   caps. This reading is contested by those advocating for network
 *   neutrality, who argue that TCP/IP embodies an end-to-end principle
 *   requiring non-discrimination.
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
narrative_ontology:human_readable(tcp_ip_interpretation__zero_rating_reading, "TCP/IP Interpretation: Zero-Rating Exemptions for Sponsored Content").
narrative_ontology:topic_domain(tcp_ip_interpretation__zero_rating_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__zero_rating_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__zero_rating_reading, '78199d79-c366-4dae-bb01-d7fbac764459').
narrative_ontology:cs_kernel_codification('78199d79-c366-4dae-bb01-d7fbac764459', fixed_text).
narrative_ontology:cs_authority_grounding('78199d79-c366-4dae-bb01-d7fbac764459', extraction).
narrative_ontology:cs_interpretation_layer_present('78199d79-c366-4dae-bb01-d7fbac764459').
narrative_ontology:cs_reading_relation('78199d79-c366-4dae-bb01-d7fbac764459', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('78199d79-c366-4dae-bb01-d7fbac764459', tcp_ip_interpretation__prioritization_reading, coexists_with).
narrative_ontology:cs_axiom('78199d79-c366-4dae-bb01-d7fbac764459', foundational, network_management_includes_commercial_differentiation).
narrative_ontology:cs_axiom_status(network_management_includes_commercial_differentiation, holdable).
narrative_ontology:cs_axiom_grounding('78199d79-c366-4dae-bb01-d7fbac764459', network_management_includes_commercial_differentiation, conventional).
narrative_ontology:cs_axiom('78199d79-c366-4dae-bb01-d7fbac764459', secondary, data_cap_exemptions_benefit_consumers).
narrative_ontology:cs_axiom_status(data_cap_exemptions_benefit_consumers, holdable).
narrative_ontology:cs_axiom_grounding('78199d79-c366-4dae-bb01-d7fbac764459', data_cap_exemptions_benefit_consumers, instrumental).
narrative_ontology:cs_reference_frame('78199d79-c366-4dae-bb01-d7fbac764459', commercial_network_optimization).
narrative_ontology:cs_drift_state('78199d79-c366-4dae-bb01-d7fbac764459', contemporary_regulatory_challenges, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('78199d79-c366-4dae-bb01-d7fbac764459', '').
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

% ISPs interpret TCP/IP to allow them to partner with content providers, exempting sponsored content from data caps. They benefit from new revenue streams and increased subscriber loyalty to their preferred content. They actively enforce these exemptions through network configuration and billing practices.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, internet_service_providers, agenda_setter,
    institutional, generational, arbitrage, national).

% Large content companies (e.g., streaming services, social media platforms) pay ISPs for zero-rating, gaining a competitive advantage by offering their services without consuming users' data allowances. This increases their market share and user engagement.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, sponsored_content_providers, beneficiary,
    powerful, biographical, mobile, global).

% Smaller content creators and startups cannot afford zero-rating agreements, placing them at a significant disadvantage. Their content consumes user data, making it less attractive compared to zero-rated alternatives, hindering their ability to compete and innovate.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, independent_content_providers, payer,
    moderate, biographical, constrained, global).

% Users benefit from 'free' access to sponsored content without hitting data caps, which can be attractive. However, this incentivizes them to consume specific content, limiting their choice and potentially leading to higher overall data costs for non-sponsored content. Their internet experience is shaped by ISP partnerships.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, internet_users, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(tcp_ip_interpretation__zero_rating_reading, internet_users, beneficiary).

% Government agencies tasked with overseeing telecommunications policy. They investigate complaints, conduct market analyses, and issue rulings on whether zero-rating practices violate principles of open internet or fair competition. Their actions can alter the enforcement landscape.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__zero_rating_reading, telecom_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows ISPs and content providers to coordinate on business models that offer users 'free' data for specific services, potentially expanding internet access for certain applications and creating new revenue streams for network maintenance.
% TRANSFER_FUNCTION: Transfers market advantage and user attention from non-sponsored content to sponsored content, and revenue from sponsored content providers to ISPs, in exchange for data cap exemptions.
% ABSENT_VOICES: New, innovative content providers who cannot afford zero-rating agreements are effectively excluded from fair competition. They would argue for a level playing field where all content is treated equally, fostering genuine innovation.
% DISAPPEARANCE_RATIONALE: If zero-rating exemptions vanished overnight, ISPs would lose a significant revenue stream, sponsored content providers would lose their competitive edge, and users would face data charges for all content. The internet content ecosystem would rebalance, potentially fostering more diverse content but requiring new business models for ISPs.
% FOUNDING_PROBLEM: ISPs sought new revenue models to fund network upgrades and manage increasing data traffic, while content providers sought ways to reach users more effectively and reduce perceived data costs for their services.
% FOUNDING_PROBLEM_CORROBORATION: ISPs and sponsored content providers attest that the problem of network funding and user engagement is still live. Independent economists and consumer advocates, however, argue that while the problems are real, zero-rating is an anti-competitive solution that distorts the market rather than solving underlying issues fairly.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__zero_rating_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__zero_rating_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__zero_rating_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.68) is substantial because zero-rating distorts competition, favoring incumbent content providers who can afford to pay ISPs, and limiting user choice by incentivizing consumption of 'free' content. Suppression (0.75) is high as ISPs actively enforce these exemptions through network policies and contractual agreements, effectively suppressing alternative business models for independent content providers. The theater ratio (0.20) is low, as the mechanism is primarily functional in its intent to generate revenue and manage traffic, though its 'benefit to users' framing can be performative.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of ISPs and sponsored content providers, zero-rating is a legitimate business innovation that benefits users and funds network development. From the perspective of independent content providers and network neutrality advocates, it is an anti-competitive practice that undermines the open internet. The engine's classification will reflect the structural asymmetry of extraction and suppression, likely diverging from the beneficiaries' 'rope' framing.
 *
 * DIRECTIONALITY LOGIC:
 *   ISPs are clear beneficiaries and agenda-setters, gaining revenue and control. Sponsored content providers are also beneficiaries, gaining market share. Independent content providers and internet users are payers, bearing the costs of distorted competition and limited choice, respectively. Telecom regulators act as observers, evaluating the impact and potential for intervention.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    zero_rating_impact_on_innovation,
    'Does zero-rating genuinely foster innovation by enabling new business models, or does it stifle innovation by creating insurmountable barriers to entry for startups?',
    'Longitudinal studies comparing innovation rates and market entry success in jurisdictions with and without zero-rating policies, controlling for other market factors.',
    'If zero-rating stifles innovation, its extractive nature is amplified by foreclosing future alternatives, strengthening its classification as a Snare. If it genuinely fosters new models, its coordination function is more robust, pushing it towards Tangled Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(zero_rating_impact_on_innovation, empirical, 'Empirical impact of zero-rating on competitive innovation.').

omega_variable(
    tcp_ip_original_intent,
    'Was the original design of TCP/IP intended to be neutral and non-discriminatory, or did it implicitly allow for network management practices like zero-rating?',
    'Historical analysis of founding documents, early RFCs, and developer correspondence, alongside expert testimony from original architects of the internet.',
    'If original intent strongly supports neutrality, this zero-rating reading would be seen as a later, extractive reinterpretation, weakening its legitimacy. If the intent was ambiguous or allowed for such practices, this reading gains conceptual grounding.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(tcp_ip_original_intent, conceptual, 'Ambiguity in TCP/IP''s foundational design regarding network neutrality.').


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
narrative_ontology:measurement(tcp__be_t10, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 10, 0.65).
narrative_ontology:measurement(tcp__be_t15, tcp_ip_interpretation__zero_rating_reading, base_extractiveness, 15, 0.67).
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

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'tcp_ip_interpretation' kernel. It focuses on the permissibility of zero-rating. The 'neutrality_reading' emphasizes non-discrimination, and the 'prioritization_reading' focuses on differentiated service quality. All three are distinct interpretations of the same underlying technical and policy kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
