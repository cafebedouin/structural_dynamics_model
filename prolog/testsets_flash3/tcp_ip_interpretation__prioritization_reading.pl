% ============================================================================
% CONSTRAINT STORY: tcp_ip_interpretation__prioritization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: tcp_ip_interpretation__prioritization_reading
 *   human_readable: TCP/IP Interpretation: Prioritization as Network Management
 *   domain: technology_governance/internet_policy/telecommunications_law
 *
 * SUMMARY:
 *   This constraint represents the interpretation of TCP/IP protocols that
 *   permits Internet Service Providers (ISPs) to offer differentiated service
 *   quality, including 'fast lanes' for content providers who pay for
 *   prioritized traffic. This reading is often framed by ISPs as essential
 *   for network management and incentivizing infrastructure investment.
 *   However, it leads to asymmetric extraction from smaller content providers
 *   and internet users, who experience a tiered internet. The claimed type is
 *   'tangled_rope' because it offers a coordination function (network
 *   management, investment incentives) but is coupled with significant
 *   asymmetric extraction and requires active enforcement to maintain.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tcp_ip_interpretation__prioritization_reading, 0.65).
domain_priors:suppression_score(tcp_ip_interpretation__prioritization_reading, 0.7).
domain_priors:theater_ratio(tcp_ip_interpretation__prioritization_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(tcp_ip_interpretation__prioritization_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tcp_ip_interpretation__prioritization_reading, tangled_rope).
narrative_ontology:human_readable(tcp_ip_interpretation__prioritization_reading, "TCP/IP Interpretation: Prioritization as Network Management").
narrative_ontology:topic_domain(tcp_ip_interpretation__prioritization_reading, "technology_governance/internet_policy/telecommunications_law").

domain_priors:requires_active_enforcement(tcp_ip_interpretation__prioritization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tcp_ip_interpretation__prioritization_reading, '3cc6c61b-bf2f-45aa-8c4e-cb374a90251b').
narrative_ontology:cs_kernel_codification('3cc6c61b-bf2f-45aa-8c4e-cb374a90251b', fixed_text).
narrative_ontology:cs_authority_grounding('3cc6c61b-bf2f-45aa-8c4e-cb374a90251b', extraction).
narrative_ontology:cs_interpretation_layer_present('3cc6c61b-bf2f-45aa-8c4e-cb374a90251b').
narrative_ontology:cs_reading_relation('3cc6c61b-bf2f-45aa-8c4e-cb374a90251b', tcp_ip_interpretation__neutrality_reading, coexists_with).
narrative_ontology:cs_reading_relation('3cc6c61b-bf2f-45aa-8c4e-cb374a90251b', tcp_ip_interpretation__zero_rating_reading, coexists_with).
narrative_ontology:cs_axiom('3cc6c61b-bf2f-45aa-8c4e-cb374a90251b', foundational, network_management_requires_flexibility).
narrative_ontology:cs_axiom_status(network_management_requires_flexibility, holdable).
narrative_ontology:cs_axiom_grounding('3cc6c61b-bf2f-45aa-8c4e-cb374a90251b', network_management_requires_flexibility, instrumental).
narrative_ontology:cs_axiom('3cc6c61b-bf2f-45aa-8c4e-cb374a90251b', foundational, market_forces_optimize_resource_allocation).
narrative_ontology:cs_axiom_status(market_forces_optimize_resource_allocation, holdable).
narrative_ontology:cs_axiom_grounding('3cc6c61b-bf2f-45aa-8c4e-cb374a90251b', market_forces_optimize_resource_allocation, empirically_contingent).
narrative_ontology:cs_reference_frame('3cc6c61b-bf2f-45aa-8c4e-cb374a90251b', commercial_network_management_paradigm).
narrative_ontology:cs_drift_state('3cc6c61b-bf2f-45aa-8c4e-cb374a90251b', contemporary_regulatory_contestation, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3cc6c61b-bf2f-45aa-8c4e-cb374a90251b', '').
narrative_ontology:cs_kernel_id(tcp_ip_interpretation__prioritization_reading, tcp_ip_interpretation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, internet_service_providers).
narrative_ontology:constraint_beneficiary(tcp_ip_interpretation__prioritization_reading, large_content_providers).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, small_startups).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, internet_users).
narrative_ontology:constraint_victim(tcp_ip_interpretation__prioritization_reading, neutrality_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% ISPs interpret TCP/IP to allow them to offer differentiated service tiers, including 'fast lanes' for content providers willing to pay. They benefit from new revenue streams and argue this incentivizes network investment. They actively enforce these policies through traffic management.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, internet_service_providers, agenda_setter,
    institutional, generational, arbitrage, national).

% These providers pay for prioritized access to ensure their content reaches users quickly and reliably, gaining a competitive advantage. They benefit from guaranteed quality of service and expanded reach, passing costs to consumers or absorbing them.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, large_content_providers, beneficiary,
    powerful, biographical, mobile, global).

% These entities cannot afford prioritization fees, placing them at a disadvantage against larger competitors. Their services may be slower or less reliable, hindering growth and market entry. Their exit options are limited to niche markets or acquisition.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, small_startups, payer,
    moderate, immediate, constrained, regional).

% Users experience a tiered internet, where access to certain content may be faster or slower depending on provider agreements. They pay for internet access and may indirectly pay higher prices for prioritized content. Their ability to switch ISPs is often limited by local monopolies.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, internet_users, payer,
    organized, biographical, constrained, national).

% These groups argue that TCP/IP's original design implies an open, non-discriminatory network. They are excluded from direct policy-making but engage in public discourse and lobbying efforts to restore network neutrality. Their identity is tied to the principle of an open internet.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, neutrality_advocates, excluded,
    organized, generational, identity_locked, global).

% Government bodies tasked with overseeing telecommunications policy. They analyze the economic and social impacts of differentiated service, balancing consumer protection, competition, and infrastructure investment. Their decisions can alter the legal framework for this constraint.
narrative_ontology:constraint_stakeholder(tcp_ip_interpretation__prioritization_reading, telecom_regulators, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Allows ISPs to manage network congestion and allocate bandwidth efficiently by prioritizing certain traffic, theoretically ensuring critical services or paid-for content maintain performance even under heavy load.
% TRANSFER_FUNCTION: Moves revenue from content providers (and indirectly, users) to ISPs in exchange for prioritized network access, and transfers competitive advantage to large content providers over smaller ones.
% ABSENT_VOICES: The original architects of TCP/IP and early internet pioneers, who envisioned an end-to-end principle of non-discrimination, are largely absent from contemporary regulatory debates, or their original intent is reinterpreted to fit current commercial models.
% DISAPPEARANCE_RATIONALE: If the ability to differentiate service quality vanished overnight, ISPs would lose a significant revenue stream, potentially impacting network investment models. Content providers would compete solely on content quality and user experience, and the internet's economic structure would shift towards a more uniform, 'best-effort' service model.
% FOUNDING_PROBLEM: The problem of funding network infrastructure upgrades and managing increasing internet traffic demand, particularly for high-bandwidth applications, while maintaining service quality.
% FOUNDING_PROBLEM_CORROBORATION: ISPs and their industry associations consistently attest that network investment requires flexible revenue models, including differentiated services. Some economists and policy analysts corroborate the need for investment incentives, though they may dispute the specific mechanisms or the extent of differentiation.
narrative_ontology:disappearance_verdict(tcp_ip_interpretation__prioritization_reading, world_rearranges).
narrative_ontology:founding_problem_status(tcp_ip_interpretation__prioritization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(tcp_ip_interpretation__prioritization_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(tcp_ip_interpretation__prioritization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(tcp_ip_interpretation__prioritization_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high because the prioritization model allows ISPs to charge for access to a fundamental utility, creating a two-sided market where one side (content providers) pays for preferential treatment. Suppression is also high because ISPs control the 'last mile' of internet access, limiting alternatives for both content providers and end-users. The theater ratio is low, as the network management function is genuinely performed, though its primary justification is increasingly seen as a cover for revenue generation. Accessibility collapse is moderate, as alternatives (like VPNs or alternative ISPs) exist but are often costly or limited. Resistance is moderate-high due to ongoing public and regulatory debates.
 *
 * PERSPECTIVAL GAP:
 *   ISPs and large content providers perceive this as a necessary and efficient market mechanism for network management and investment. Small startups and internet users, however, experience it as an extractive barrier to entry and a degradation of the open internet principle. The engine's classification will highlight this divergence, showing a 'tangled_rope' from the perspective of payers and a 'rope' or even 'mountain' from the perspective of beneficiaries who see it as a natural market outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   ISPs are clear beneficiaries and agenda-setters, directly profiting from prioritization fees. Large content providers are also beneficiaries, gaining a competitive edge. Small startups and internet users are payers, bearing the costs of a tiered internet. Neutrality advocates are excluded, as their core principle is directly challenged by this interpretation. Telecom regulators act as observers, weighing the various impacts.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (funding network investment, managing traffic) is still live, but its function has drifted. What began as a technical interpretation for network efficiency has accumulated extractive layers, making it a 'tangled_rope' rather than a pure 'rope'. The classification prevents mislabeling it as a pure 'snare' by acknowledging the genuine (though often overstated) coordination function, while also preventing it from being seen as a pure 'rope' by highlighting the asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_investment_causality,
    'Does the ability to implement differentiated service quality genuinely incentivize network infrastructure investment, or is investment primarily driven by other market forces and regulatory mandates?',
    'Empirical studies comparing network investment rates in jurisdictions with and without strong network neutrality regulations over a multi-year period, controlling for market size and competition.',
    'If prioritization does not significantly boost investment, the primary justification for the constraint weakens, increasing its effective extractiveness and pushing it closer to a ''snare''. If it does, the coordination function is strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(network_investment_causality, empirical, 'Whether prioritization truly drives network investment.').

omega_variable(
    technical_necessity_vs_commercial_choice,
    'To what extent is differentiated service quality a technical necessity for efficient network operation (e.g., managing congestion for critical services), versus a commercial choice to create tiered markets?',
    'Technical analysis by independent network engineers and computer scientists, modeling network performance under various traffic management regimes, including those without paid prioritization.',
    'If differentiation is primarily a commercial choice, the ''network management'' coordination story becomes more theatrical, increasing the constraint''s theater_ratio and extractiveness. If technically necessary, it reinforces the coordination aspect.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_necessity_vs_commercial_choice, empirical, 'Technical vs. commercial drivers of service differentiation.').

omega_variable(
    framing_of_tcp_ip_intent,
    'Is the ''prioritization_reading'' a faithful interpretation of TCP/IP''s original design principles and intent, or a reinterpretation driven by commercial interests?',
    'Historical analysis of RFCs, design documents, and statements from the original internet architects, alongside legal and policy interpretations over time. This is a conceptual rather than purely empirical question.',
    'If it''s a reinterpretation, the legitimacy of the ''prioritization_reading'' as a ''natural'' outcome of the protocol is undermined, strengthening arguments for regulatory intervention based on original intent. If faithful, it reinforces the ''mountain'' aspect of the technical layer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(framing_of_tcp_ip_intent, conceptual, 'Original intent of TCP/IP regarding service differentiation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tcp_ip_interpretation__prioritization_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tcp__tr_t0, tcp_ip_interpretation__prioritization_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(tcp__tr_t5, tcp_ip_interpretation__prioritization_reading, theater_ratio, 5, 0.07).
narrative_ontology:measurement(tcp__tr_t10, tcp_ip_interpretation__prioritization_reading, theater_ratio, 10, 0.08).
narrative_ontology:measurement(tcp__tr_t15, tcp_ip_interpretation__prioritization_reading, theater_ratio, 15, 0.09).
narrative_ontology:measurement(tcp__tr_t20, tcp_ip_interpretation__prioritization_reading, theater_ratio, 20, 0.1).

% Extraction over time
narrative_ontology:measurement(tcp__be_t0, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(tcp__be_t5, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(tcp__be_t10, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 10, 0.6).
narrative_ontology:measurement(tcp__be_t15, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement(tcp__be_t20, tcp_ip_interpretation__prioritization_reading, base_extractiveness, 20, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(tcp__su_t0, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(tcp__su_t5, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(tcp__su_t10, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(tcp__su_t15, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement(tcp__su_t20, tcp_ip_interpretation__prioritization_reading, suppression_requirement, 20, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tcp_ip_interpretation__prioritization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, net_neutrality_regulations).
narrative_ontology:affects_constraint(tcp_ip_interpretation__prioritization_reading, digital_divide_policies).

% DUAL FORMULATION NOTE:
% This constraint is the 'prioritization_reading' of the 'tcp_ip_interpretation' kernel. It is one of three sibling readings, alongside 'neutrality_reading' and 'zero_rating_reading', each representing a distinct interpretation of TCP/IP's implications for network policy.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
